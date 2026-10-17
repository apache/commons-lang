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

import static org.apache.commons.lang3.LangAssertions.assertNullPointerException;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;

import org.junit.jupiter.api.Test;

/**
 */
class ClassPathUtilsTest extends AbstractLangTest {

    private static Package unnamedPackage() {
        return new ClassLoader() {
            Package defineUnnamedPackage() {
                return definePackage("", null, null, null, null, null, null, null);
            }
        }.defineUnnamedPackage();
    }

    @Test
    void testConstructor() {
        assertNotNull(new ClassPathUtils());
        final Constructor<?>[] cons = ClassPathUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertTrue(Modifier.isPublic(cons[0].getModifiers()));
        assertTrue(Modifier.isPublic(ClassPathUtils.class.getModifiers()));
        assertFalse(Modifier.isFinal(ClassPathUtils.class.getModifiers()));
    }

    @Test
    void testPackageToPath() {
        assertEquals("a", ClassPathUtils.packageToPath("a"));
        assertEquals("a/b", ClassPathUtils.packageToPath("a.b"));
        assertEquals("a/b/c", ClassPathUtils.packageToPath("a.b.c"));
    }

    @Test
    void testPathToPackage() {
        assertEquals("a", ClassPathUtils.pathToPackage("a"));
        assertEquals("a.b", ClassPathUtils.pathToPackage("a/b"));
        assertEquals("a.b.c", ClassPathUtils.pathToPackage("a/b/c"));
    }

    @Test
    void testToFullyQualifiedNameArrayAndPrimitiveClass() throws ClassNotFoundException {
        assertNullPointerException(() -> ClassPathUtils.toFullyQualifiedName(String[].class, "Test.properties"));
        assertNullPointerException(() -> ClassPathUtils.toFullyQualifiedName(int.class, "Test.properties"));
        assertNullPointerException(() -> ClassPathUtils.toFullyQualifiedName(void.class, "Test.properties"));
        assertNullPointerException(() -> ClassPathUtils.toFullyQualifiedName(int[].class, "Test.properties"));
        final Class<?> unnamedPackageArray = Class.forName("[LClassPathUtilsDefaultPackage;");
        assertNullPointerException(() -> ClassPathUtils.toFullyQualifiedName(unnamedPackageArray, "Test.properties"));
    }

    @Test
    void testToFullyQualifiedNameClassNull() {
        assertNullPointerException(() -> ClassPathUtils.toFullyQualifiedName(ClassPathUtils.class, null));
    }

    @Test
    void testToFullyQualifiedNameClassString() {
        final String expected = "org.apache.commons.lang3.Test.properties";
        final String actual = ClassPathUtils.toFullyQualifiedName(ClassPathUtils.class, "Test.properties");

        assertEquals(expected, actual);
    }

    @Test
    void testToFullyQualifiedNameNullClassString() {
        assertNullPointerException(
                () -> ClassPathUtils.toFullyQualifiedName((Class<?>) null, "Test.properties"));
    }

    @Test
    void testToFullyQualifiedNameNullPackageString() {
        assertNullPointerException(
                () -> ClassPathUtils.toFullyQualifiedName((Package) null, "Test.properties"));
    }

    @Test
    void testToFullyQualifiedNamePackageNull() {
        assertNullPointerException(
                () -> ClassPathUtils.toFullyQualifiedName(ClassPathUtils.class.getPackage(), null));
    }

    @Test
    void testToFullyQualifiedNamePackageString() {
        final String expected = "org.apache.commons.lang3.Test.properties";
        final String actual = ClassPathUtils.toFullyQualifiedName(ClassPathUtils.class.getPackage(), "Test.properties");

        assertEquals(expected, actual);
    }

    @Test
    void testToFullyQualifiedNameUnnamedPackage() {
        assertEquals("Test.properties", ClassPathUtils.toFullyQualifiedName(unnamedPackage(), "Test.properties"));
    }

    @Test
    void testToFullyQualifiedNameUnnamedPackageClass() throws ClassNotFoundException {
        assertEquals("Test.properties", ClassPathUtils.toFullyQualifiedName(
                Class.forName("ClassPathUtilsDefaultPackage"), "Test.properties"));
    }

    @Test
    void testToFullyQualifiedNameUnnamedPackageClassEmptyResource() throws ClassNotFoundException {
        assertEquals("", ClassPathUtils.toFullyQualifiedName(Class.forName("ClassPathUtilsDefaultPackage"), ""));
    }

    @Test
    void testToFullyQualifiedNameUnnamedPackageClassNullResource() throws ClassNotFoundException {
        final Class<?> context = Class.forName("ClassPathUtilsDefaultPackage");
        assertNullPointerException(() -> ClassPathUtils.toFullyQualifiedName(context, null));
    }

    @Test
    void testToFullyQualifiedNameUnnamedPackageEmptyResource() {
        assertEquals("", ClassPathUtils.toFullyQualifiedName(unnamedPackage(), ""));
    }

    @Test
    void testToFullyQualifiedNameUnnamedPackageNullResource() {
        assertNullPointerException(() -> ClassPathUtils.toFullyQualifiedName(unnamedPackage(), null));
    }

    @Test
    void testToFullyQualifiedPathArrayAndPrimitiveClass() throws ClassNotFoundException {
        assertNullPointerException(() -> ClassPathUtils.toFullyQualifiedPath(String[].class, "Test.properties"));
        assertNullPointerException(() -> ClassPathUtils.toFullyQualifiedPath(int.class, "Test.properties"));
        assertNullPointerException(() -> ClassPathUtils.toFullyQualifiedPath(void.class, "Test.properties"));
        assertNullPointerException(() -> ClassPathUtils.toFullyQualifiedPath(int[].class, "Test.properties"));
        final Class<?> unnamedPackageArray = Class.forName("[LClassPathUtilsDefaultPackage;");
        assertNullPointerException(() -> ClassPathUtils.toFullyQualifiedPath(unnamedPackageArray, "Test.properties"));
    }

    @Test
    void testToFullyQualifiedPathClass() {
        final String expected = "org/apache/commons/lang3/Test.properties";
        final String actual = ClassPathUtils.toFullyQualifiedPath(ClassPathUtils.class, "Test.properties");

        assertEquals(expected, actual);
    }

    @Test
    void testToFullyQualifiedPathClassNull() {
        assertNullPointerException(() -> ClassPathUtils.toFullyQualifiedPath(ClassPathUtils.class, null));
    }

    @Test
    void testToFullyQualifiedPathClassNullString() {
        assertNullPointerException(
                () -> ClassPathUtils.toFullyQualifiedPath((Class<?>) null, "Test.properties"));
    }

    @Test
    void testToFullyQualifiedPathPackage() {
        final String expected = "org/apache/commons/lang3/Test.properties";
        final String actual = ClassPathUtils.toFullyQualifiedPath(ClassPathUtils.class.getPackage(), "Test.properties");

        assertEquals(expected, actual);
    }

    @Test
    void testToFullyQualifiedPathPackageNull() {
        assertNullPointerException(
                () -> ClassPathUtils.toFullyQualifiedPath(ClassPathUtils.class.getPackage(), null));
    }

    @Test
    void testToFullyQualifiedPathPackageNullString() {
        assertNullPointerException(
                () -> ClassPathUtils.toFullyQualifiedPath((Package) null, "Test.properties"));
    }

    @Test
    void testToFullyQualifiedPathUnnamedPackage() {
        assertEquals("Test.properties", ClassPathUtils.toFullyQualifiedPath(unnamedPackage(), "Test.properties"));
    }

    @Test
    void testToFullyQualifiedPathUnnamedPackageClass() throws ClassNotFoundException {
        assertEquals("Test.properties", ClassPathUtils.toFullyQualifiedPath(
                Class.forName("ClassPathUtilsDefaultPackage"), "Test.properties"));
    }

    @Test
    void testToFullyQualifiedPathUnnamedPackageClassEmptyResource() throws ClassNotFoundException {
        assertEquals("", ClassPathUtils.toFullyQualifiedPath(Class.forName("ClassPathUtilsDefaultPackage"), ""));
    }

    @Test
    void testToFullyQualifiedPathUnnamedPackageClassNullResource() throws ClassNotFoundException {
        final Class<?> context = Class.forName("ClassPathUtilsDefaultPackage");
        assertNullPointerException(() -> ClassPathUtils.toFullyQualifiedPath(context, null));
    }

    @Test
    void testToFullyQualifiedPathUnnamedPackageEmptyResource() {
        assertEquals("", ClassPathUtils.toFullyQualifiedPath(unnamedPackage(), ""));
    }

    @Test
    void testToFullyQualifiedPathUnnamedPackageNullResource() {
        assertNullPointerException(() -> ClassPathUtils.toFullyQualifiedPath(unnamedPackage(), null));
    }
}
