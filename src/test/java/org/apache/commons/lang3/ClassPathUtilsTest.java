/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang3;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;

import org.junit.jupiter.api.Test;

/**
 */
public class ClassPathUtilsTest extends AbstractLangTest {

    @Test
    public void testConstructor() {
        assertNotNull(new ClassPathUtils());
        final Constructor<?>[] cons = ClassPathUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertTrue(Modifier.isPublic(cons[0].getModifiers()));
        assertTrue(Modifier.isPublic(ClassPathUtils.class.getModifiers()));
        assertFalse(Modifier.isFinal(ClassPathUtils.class.getModifiers()));
    }

    @Test
    public void testPackageToPath() {
        assertEquals("a", ClassPathUtils.packageToPath("a"));
        assertEquals("a/b", ClassPathUtils.packageToPath("a.b"));
        assertEquals("a/b/c", ClassPathUtils.packageToPath("a.b.c"));
    }

    @Test
    public void testPathToPackage() {
        assertEquals("a", ClassPathUtils.pathToPackage("a"));
        assertEquals("a.b", ClassPathUtils.pathToPackage("a/b"));
        assertEquals("a.b.c", ClassPathUtils.pathToPackage("a/b/c"));
    }

    @Test
    public void testToFullyQualifiedNameNullClassString() {
        assertThrows(NullPointerException.class,
                () -> ClassPathUtils.toFullyQualifiedName((Class<?>) null, "Test.properties"));
    }

    @Test
    public void testToFullyQualifiedNameClassNull() {
        assertThrows(NullPointerException.class, () -> ClassPathUtils.toFullyQualifiedName(ClassPathUtils.class, null));
    }

    @Test
    public void testToFullyQualifiedNameClassString() {
        final String expected = "org.apache.commons.lang3.Test.properties";
        final String actual = ClassPathUtils.toFullyQualifiedName(ClassPathUtils.class, "Test.properties");

        assertEquals(expected, actual);
    }

    @Test
    public void testToFullyQualifiedNameNullPackageString() {
        assertThrows(NullPointerException.class,
                () -> ClassPathUtils.toFullyQualifiedName((Package) null, "Test.properties"));
    }

    @Test
    public void testToFullyQualifiedNamePackageNull() {
        assertThrows(NullPointerException.class,
                () -> ClassPathUtils.toFullyQualifiedName(ClassPathUtils.class.getPackage(), null));
    }

    @Test
    public void testToFullyQualifiedNamePackageString() {
        final String expected = "org.apache.commons.lang3.Test.properties";
        final String actual = ClassPathUtils.toFullyQualifiedName(ClassPathUtils.class.getPackage(), "Test.properties");

        assertEquals(expected, actual);
    }

    @Test
    public void testToFullyQualifiedPathClassNullString() {
        assertThrows(NullPointerException.class,
                () -> ClassPathUtils.toFullyQualifiedPath((Class<?>) null, "Test.properties"));
    }

    @Test
    public void testToFullyQualifiedPathClassNull() {
        assertThrows(NullPointerException.class, () -> ClassPathUtils.toFullyQualifiedPath(ClassPathUtils.class, null));
    }

    @Test
    public void testToFullyQualifiedPathClass() {
        final String expected = "org/apache/commons/lang3/Test.properties";
        final String actual = ClassPathUtils.toFullyQualifiedPath(ClassPathUtils.class, "Test.properties");

        assertEquals(expected, actual);
    }

    @Test
    public void testToFullyQualifiedPathPackageNullString() {
        assertThrows(NullPointerException.class,
                () -> ClassPathUtils.toFullyQualifiedPath((Package) null, "Test.properties"));
    }

    @Test
    public void testToFullyQualifiedPathPackageNull() {
        assertThrows(NullPointerException.class,
                () -> ClassPathUtils.toFullyQualifiedPath(ClassPathUtils.class.getPackage(), null));
    }

    @Test
    public void testToFullyQualifiedPathPackage() {
        final String expected = "org/apache/commons/lang3/Test.properties";
        final String actual = ClassPathUtils.toFullyQualifiedPath(ClassPathUtils.class.getPackage(), "Test.properties");

        assertEquals(expected, actual);
    }
}
