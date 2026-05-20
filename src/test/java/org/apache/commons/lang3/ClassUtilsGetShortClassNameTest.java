/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.commons.lang3;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * {@link ClassUtils#getShortClassName(Class)} can throw {@link NoClassDefFoundError} when the supplied class is an inner class whose enclosing (outer) class
 * has been removed from the classpath.
 *
 * <p>
 * The code path lives in the {@code while (x != null)} loop of {@code getShortClassName(Class)}: it calls {@code x.getSimpleName()} and
 * {@code x.getDeclaringClass()}. Both of those JDK methods resolve the {@code InnerClasses} attribute of the inner class' bytecode, and when the enclosing
 * class is missing the JVM throws {@link NoClassDefFoundError} from inside {@code Class#getDeclaringClass0}/{@code getSimpleBinaryName}.
 * </p>
 * <p>
 * The test compiles two classes ({@code F030Outer} and {@code F030Outer$Inner}) with the in-process {@code javax.tools.JavaCompiler}, deletes the outer class
 * file, loads the inner class via a fresh {@link URLClassLoader}, and then invokes {@link ClassUtils#getShortClassName(Class)}.
 * </p>
 *
 * <p>
 * At baseline (commit {@code 8538458e7}) the call propagates {@link NoClassDefFoundError}. After the fix the {@code if/else} block around
 * {@code isAnonymousClass}/{@code isLocalClass} is wrapped in a {@code try / catch (NoClassDefFoundError)} that falls back to
 * {@link ClassUtils#getShortClassName(String)} on the binary name.
 * </p>
 */
public class ClassUtilsGetShortClassNameTest {

    // @formatter:off
    private static final String OUTER_SRC = ""
        + "package f030;\n"
        + "public class F030Outer {\n"
        + "    public static class Inner {\n"
        + "        public int value;\n"
        + "    }\n"
        + "}\n";
    // @formatter:on

    /**
     * Compiles {@code f030.F030Outer} (and its nested {@code Inner}) into the supplied directory using the in-process Java compiler.
     *
     * @return {@code true} on success, {@code false} if the compiler is not available (running on a JRE rather than a JDK).
     */
    private static boolean compileOuterWithInner(final Path classesDir) throws IOException {
        final JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        assertNotNull("JDK compiler missing");
        final Path srcDir = classesDir.resolve("..").resolve("src").normalize();
        Files.createDirectories(srcDir);
        final Path outerJava = srcDir.resolve("F030Outer.java");
        Files.write(outerJava, OUTER_SRC.getBytes(StandardCharsets.UTF_8));
        Files.createDirectories(classesDir);
        return compiler.run(null, null, null, "-d", classesDir.toString(), outerJava.toString()) == 0;
    }

    /**
     * Creates a child {@link URLClassLoader} that can resolve only the inner class file, the outer class file is deliberately omitted from the directory it
     * points at, simulating a torn deployment / shaded JAR.
     */
    private static URLClassLoader innerOnlyLoader(final Path innerOnlyDir) throws IOException {
        final URL url = innerOnlyDir.toUri().toURL();
        return new URLClassLoader(new URL[] { url }, ClassLoader.getSystemClassLoader().getParent());
    }

    /**
     * Anonymous and local classes follow a separate code path that calls {@code getShortClassName(c.getName())} directly. The compiler-generated ordinal (fpr
     * example, {@code $1}) is preserved by the legacy contract, so the short name ends with {@code ".<digits>"}.
     */
    @Test
    public void testAnonymousAndLocalClassesUseSeparatePath() {
        final Runnable anon = new Runnable() {

            @Override
            public void run() {
                /* no-op */
            }
        };
        final String shortName = ClassUtils.getShortClassName(anon.getClass());
        assertNotNull(shortName);
        assertTrue(shortName.matches(".*\\.[0-9].*"), "Anonymous class short name should preserve the compiler ordinal: " + shortName);
    }

    @Test
    public void testGetShortClassNameDoesNotThrowOnNormalClasses() {
        assertEquals("String", ClassUtils.getShortClassName(String.class));
        assertEquals("Map.Entry", ClassUtils.getShortClassName(Map.Entry.class));
        assertEquals("int[]", ClassUtils.getShortClassName(int[].class));
    }

    @Test
    public void testGetShortClassNameOnInnerClassWithMissingOuter(@TempDir final Path tempDir) throws Exception {
        final Path classesDir = tempDir.resolve("classes");
        assumeTrue(compileOuterWithInner(classesDir));
        final Path outerClass = classesDir.resolve("f030").resolve("F030Outer.class");
        final Path innerClass = classesDir.resolve("f030").resolve("F030Outer$Inner.class");
        assumeTrue(Files.exists(outerClass) && Files.exists(innerClass), "Expected compiled class files to exist");
        final Path innerOnly = tempDir.resolve("inner-only");
        Files.createDirectories(innerOnly.resolve("f030"));
        Files.copy(innerClass, innerOnly.resolve("f030").resolve("F030Outer$Inner.class"));
        try (URLClassLoader cl = innerOnlyLoader(innerOnly)) {
            final Class<?> inner = Class.forName("f030.F030Outer$Inner", false, cl);
            assertEquals("f030.F030Outer$Inner", inner.getName());
            // Sanity: at the JDK layer, both getSimpleName and getDeclaringClass
            // throw NoClassDefFoundError for this inner class on the current JVM.
            assertThrows(NoClassDefFoundError.class, inner::getSimpleName);
            assertThrows(NoClassDefFoundError.class, inner::getDeclaringClass);
            // Post-fix: ClassUtils.getShortClassName(Class) catches NoClassDefFoundError
            // thrown anywhere inside the if (isAnonymous || isLocal){...} else {...} block
            // and falls back to string-parsing the binary name "f030.F030Outer$Inner"
            // which yields "F030Outer.Inner" (per the documented contract of
            // getShortClassName(String): the '$' separator is replaced with '.').
            assertEquals("F030Outer.Inner", ClassUtils.getShortClassName(inner));
        }
    }

    @Test
    public void testGetShortClassNameReturnsNonNullForStandardClasses() {
        assertNotNull(ClassUtils.getShortClassName(String.class));
        assertNotNull(ClassUtils.getShortClassName(Integer.class));
    }
}
