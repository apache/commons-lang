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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;

import org.junit.Test;

/**
 */
public class ClassPathUtilsTest {

    @Test
    public void testConstructor() {
        assertNotNull(new ClassPathUtils());
        final Constructor<?>[] cons = ClassPathUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertTrue(Modifier.isPublic(cons[0].getModifiers()));
        assertTrue(Modifier.isPublic(ClassPathUtils.class.getModifiers()));
        assertFalse(Modifier.isFinal(ClassPathUtils.class.getModifiers()));
    }

    @Test(expected = NullPointerException.class)
    public void testToFullyQualifiedNameNullClassString() throws Exception {
        ClassPathUtils.toFullyQualifiedName((Class<?>) null, "Test.properties");
    }

    @Test(expected = NullPointerException.class)
    public void testToFullyQualifiedNameClassNull() throws Exception {
        ClassPathUtils.toFullyQualifiedName(ClassPathUtils.class, null);
    }

    @Test
    public void testToFullyQualifiedNameClassString() throws Exception {
        final String expected = "org.apache.commons.lang3.Test.properties";
        final String actual = ClassPathUtils.toFullyQualifiedName(ClassPathUtils.class, "Test.properties");

        assertEquals(expected, actual);
    }

    @Test(expected = NullPointerException.class)
    public void testToFullyQualifiedNameNullPackageString() throws Exception {
        ClassPathUtils.toFullyQualifiedName((Package) null, "Test.properties");
    }

    @Test(expected = NullPointerException.class)
    public void testToFullyQualifiedNamePackageNull() throws Exception {
        ClassPathUtils.toFullyQualifiedName(ClassPathUtils.class.getPackage(), null);
    }

    @Test
    public void testToFullyQualifiedNamePackageString() throws Exception {
        final String expected = "org.apache.commons.lang3.Test.properties";
        final String actual = ClassPathUtils.toFullyQualifiedName(ClassPathUtils.class.getPackage(), "Test.properties");

        assertEquals(expected, actual);
    }

    @Test(expected = NullPointerException.class)
    public void testToFullyQualifiedPathClassNullString() throws Exception {
        ClassPathUtils.toFullyQualifiedPath((Class<?>) null, "Test.properties");
    }

    @Test(expected = NullPointerException.class)
    public void testToFullyQualifiedPathClassNull() throws Exception {
        ClassPathUtils.toFullyQualifiedPath(ClassPathUtils.class, null);
    }

    @Test
    public void testToFullyQualifiedPathClass() throws Exception {
        final String expected = "org/apache/commons/lang3/Test.properties";
        final String actual = ClassPathUtils.toFullyQualifiedPath(ClassPathUtils.class, "Test.properties");

        assertEquals(expected, actual);
    }

    @Test(expected = NullPointerException.class)
    public void testToFullyQualifiedPathPackageNullString() throws Exception {
        ClassPathUtils.toFullyQualifiedPath((Package) null, "Test.properties");
    }

    @Test(expected = NullPointerException.class)
    public void testToFullyQualifiedPathPackageNull() throws Exception {
        ClassPathUtils.toFullyQualifiedPath(ClassPathUtils.class.getPackage(), null);
    }

    @Test
    public void testToFullyQualifiedPathPackage() throws Exception {
        final String expected = "org/apache/commons/lang3/Test.properties";
        final String actual = ClassPathUtils.toFullyQualifiedPath(ClassPathUtils.class.getPackage(), "Test.properties");

        assertEquals(expected, actual);
    }
}
