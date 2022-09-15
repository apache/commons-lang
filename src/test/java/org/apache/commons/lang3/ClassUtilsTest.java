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

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.apache.commons.lang3.ClassUtils.Interfaces;
import org.apache.commons.lang3.reflect.testbed.GenericConsumer;
import org.apache.commons.lang3.reflect.testbed.GenericParent;
import org.apache.commons.lang3.reflect.testbed.StringParameterizedChild;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

/**
 * Unit tests {@link org.apache.commons.lang3.ClassUtils}.
 */
@SuppressWarnings("boxing") // JUnit4 does not support primitive equality testing apart from long
public class ClassUtilsTest extends AbstractLangTest {

    private static final String OBJECT_CANONICAL_NAME = "java.lang.Object";

    private static class CX implements IB, IA, IE {
        // empty
    }

    @SuppressWarnings("unused") // IB is redundant but what a test checks
    private static class CY extends CX implements IB, IC {
        // empty
    }

    private interface IA {
        // empty
    }

    private interface IB {
        // empty
    }

    private interface IC extends ID, IE {
        // empty
    }

    private interface ID {
        // empty
    }

    private interface IE extends IF {
        // empty
    }

    private interface IF {
        // empty
    }

    private static class Inner {
        private class DeeplyNested {
            // empty
        }
    }

    private void assertGetClassReturnsClass(final Class<?> c) throws Exception {
        assertEquals(c, ClassUtils.getClass(c.getName()));
    }

    private void assertGetClassThrowsClassNotFound(final String className) {
        assertGetClassThrowsException(className, ClassNotFoundException.class);
    }

    private void assertGetClassThrowsException(final String className, final Class<? extends Exception> exceptionType) {
        assertThrows(exceptionType, () -> ClassUtils.getClass(className),
            "ClassUtils.getClass() should fail with an exception of type " + exceptionType.getName() + " when given class name \"" + className + "\".");
    }

    private void assertGetClassThrowsNullPointerException(final String className) {
        assertGetClassThrowsException(className, NullPointerException.class);
    }

    @Test
    public void test_convertClassesToClassNames_List() {
        final List<Class<?>> list = new ArrayList<>();
        List<String> result = ClassUtils.convertClassesToClassNames(list);
        assertEquals(0, result.size());

        list.add(String.class);
        list.add(null);
        list.add(Object.class);
        result = ClassUtils.convertClassesToClassNames(list);
        assertEquals(3, result.size());
        assertEquals("java.lang.String", result.get(0));
        assertNull(result.get(1));
        assertEquals(OBJECT_CANONICAL_NAME, result.get(2));

        @SuppressWarnings("unchecked") // test what happens when non-generic code adds wrong type of element
        final List<Object> olist = (List<Object>) (List<?>) list;
        olist.add(new Object());
        assertThrows(ClassCastException.class, () -> ClassUtils.convertClassesToClassNames(list), "Should not have been able to convert list");
        assertNull(ClassUtils.convertClassesToClassNames(null));
    }

    @Test
    public void test_convertClassNamesToClasses_List() {
        final List<String> list = new ArrayList<>();
        List<Class<?>> result = ClassUtils.convertClassNamesToClasses(list);
        assertEquals(0, result.size());

        list.add("java.lang.String");
        list.add("java.lang.xxx");
        list.add(OBJECT_CANONICAL_NAME);
        result = ClassUtils.convertClassNamesToClasses(list);
        assertEquals(3, result.size());
        assertEquals(String.class, result.get(0));
        assertNull(result.get(1));
        assertEquals(Object.class, result.get(2));

        @SuppressWarnings("unchecked") // test what happens when non-generic code adds wrong type of element
        final List<Object> olist = (List<Object>) (List<?>) list;
        olist.add(new Object());
        assertThrows(ClassCastException.class, () -> ClassUtils.convertClassNamesToClasses(list), "Should not have been able to convert list");
        assertNull(ClassUtils.convertClassNamesToClasses(null));
    }

    @Test
    public void test_getAbbreviatedName_Class() {
        assertEquals("", ClassUtils.getAbbreviatedName((Class<?>) null, 1));
        assertEquals("j.l.String", ClassUtils.getAbbreviatedName(String.class, 1));
        assertEquals("j.l.String", ClassUtils.getAbbreviatedName(String.class, 5));
        assertEquals("o.a.c.l.ClassUtils", ClassUtils.getAbbreviatedName(ClassUtils.class, 18));
        assertEquals("j.lang.String", ClassUtils.getAbbreviatedName(String.class, 13));
        assertEquals("j.lang.String", ClassUtils.getAbbreviatedName(String.class, 15));
        assertEquals("java.lang.String", ClassUtils.getAbbreviatedName(String.class, 20));
    }

    @Test
    @DisplayName("When the desired length is negative then exception is thrown")
    public void test_getAbbreviatedName_Class_NegativeLen() {
        assertThrows(IllegalArgumentException.class, () -> ClassUtils.getAbbreviatedName(String.class, -10));
    }

    @Test
    @DisplayName("When the desired length is zero then exception is thrown")
    public void test_getAbbreviatedName_Class_ZeroLen() {
        assertThrows(IllegalArgumentException.class, () -> ClassUtils.getAbbreviatedName(String.class, 0));
    }

    @Test
    public void test_getAbbreviatedName_String() {
        assertEquals("", ClassUtils.getAbbreviatedName((String) null, 1));
        assertEquals("", ClassUtils.getAbbreviatedName("", 1));
        assertEquals("WithoutPackage", ClassUtils.getAbbreviatedName("WithoutPackage", 1));
        assertEquals("j.l.String", ClassUtils.getAbbreviatedName("java.lang.String", 1));
        assertEquals("o.a.c.l.ClassUtils", ClassUtils.getAbbreviatedName("org.apache.commons.lang3.ClassUtils", 18));
        assertEquals("org.apache.commons.lang3.ClassUtils",
            ClassUtils.getAbbreviatedName("org.apache.commons.lang3.ClassUtils", "org.apache.commons.lang3.ClassUtils".length()));
        assertEquals("o.a.c.l.ClassUtils", ClassUtils.getAbbreviatedName("o.a.c.l.ClassUtils", 18));
        assertEquals("o..c.l.ClassUtils", ClassUtils.getAbbreviatedName("o..c.l.ClassUtils", 18));
        assertEquals(".", ClassUtils.getAbbreviatedName(".", 18));
        assertEquals(".", ClassUtils.getAbbreviatedName(".", 1));
        assertEquals("..", ClassUtils.getAbbreviatedName("..", 1));
        assertEquals("...", ClassUtils.getAbbreviatedName("...", 2));
        assertEquals("...", ClassUtils.getAbbreviatedName("...", 3));
        assertEquals("java.lang.String", ClassUtils.getAbbreviatedName("java.lang.String", Integer.MAX_VALUE));
        assertEquals("j.lang.String", ClassUtils.getAbbreviatedName("java.lang.String", "j.lang.String".length()));
        assertEquals("j.l.String", ClassUtils.getAbbreviatedName("java.lang.String", "j.lang.String".length() - 1));
        assertEquals("j.l.String", ClassUtils.getAbbreviatedName("java.lang.String", "j.l.String".length()));
        assertEquals("j.l.String", ClassUtils.getAbbreviatedName("java.lang.String", "j.l.String".length() - 1));
    }

    /**
     * Test that in case the required length is larger than the name and thus there is no need for any shortening then the
     * returned string object is the same as the one passed as argument. Note, however, that this is tested as an internal
     * implementation detail, but it is not a guaranteed feature of the implementation.
     */
    @Test
    @DisplayName("When the length hint is longer than the actual length then the same String object is returned")
    public void test_getAbbreviatedName_TooLongHint() {
        final String className = "java.lang.String";
        Assertions.assertSame(className, ClassUtils.getAbbreviatedName(className, className.length() + 1));
        Assertions.assertSame(className, ClassUtils.getAbbreviatedName(className, className.length()));
    }

    @Test
    public void test_getAllInterfaces_Class() {
        final List<?> list = ClassUtils.getAllInterfaces(CY.class);
        assertEquals(6, list.size());
        assertEquals(IB.class, list.get(0));
        assertEquals(IC.class, list.get(1));
        assertEquals(ID.class, list.get(2));
        assertEquals(IE.class, list.get(3));
        assertEquals(IF.class, list.get(4));
        assertEquals(IA.class, list.get(5));

        assertNull(ClassUtils.getAllInterfaces(null));
    }

    @Test
    public void test_getAllSuperclasses_Class() {
        final List<?> list = ClassUtils.getAllSuperclasses(CY.class);
        assertEquals(2, list.size());
        assertEquals(CX.class, list.get(0));
        assertEquals(Object.class, list.get(1));

        assertNull(ClassUtils.getAllSuperclasses(null));
    }

    @Test
    public void test_getCanonicalName_Class() {
        assertEquals("org.apache.commons.lang3.ClassUtils", ClassUtils.getCanonicalName(ClassUtils.class));
        assertEquals("java.util.Map.Entry", ClassUtils.getCanonicalName(Map.Entry.class));
        assertEquals("", ClassUtils.getCanonicalName((Class<?>) null));

        assertEquals("java.lang.String[]", ClassUtils.getCanonicalName(String[].class));
        assertEquals("java.util.Map.Entry[]", ClassUtils.getCanonicalName(Map.Entry[].class));

        // Primitives
        assertEquals("boolean", ClassUtils.getCanonicalName(boolean.class));
        assertEquals("byte", ClassUtils.getCanonicalName(byte.class));
        assertEquals("char", ClassUtils.getCanonicalName(char.class));
        assertEquals("short", ClassUtils.getCanonicalName(short.class));
        assertEquals("int", ClassUtils.getCanonicalName(int.class));
        assertEquals("long", ClassUtils.getCanonicalName(long.class));
        assertEquals("float", ClassUtils.getCanonicalName(float.class));
        assertEquals("double", ClassUtils.getCanonicalName(double.class));

        // Primitive Arrays
        assertEquals("boolean[]", ClassUtils.getCanonicalName(boolean[].class));
        assertEquals("byte[]", ClassUtils.getCanonicalName(byte[].class));
        assertEquals("char[]", ClassUtils.getCanonicalName(char[].class));
        assertEquals("short[]", ClassUtils.getCanonicalName(short[].class));
        assertEquals("int[]", ClassUtils.getCanonicalName(int[].class));
        assertEquals("long[]", ClassUtils.getCanonicalName(long[].class));
        assertEquals("float[]", ClassUtils.getCanonicalName(float[].class));
        assertEquals("double[]", ClassUtils.getCanonicalName(double[].class));

        // Arrays of arrays of ...
        assertEquals("java.lang.String[][]", ClassUtils.getCanonicalName(String[][].class));
        assertEquals("java.lang.String[][][]", ClassUtils.getCanonicalName(String[][][].class));
        assertEquals("java.lang.String[][][][]", ClassUtils.getCanonicalName(String[][][][].class));

        // Inner types
        class Named {
            // empty
        }
        assertEquals(StringUtils.EMPTY, ClassUtils.getCanonicalName(new Object() {
            // empty
        }.getClass()));
        assertEquals(StringUtils.EMPTY, ClassUtils.getCanonicalName(Named.class));
        assertEquals("org.apache.commons.lang3.ClassUtilsTest.Inner", ClassUtils.getCanonicalName(Inner.class));
    }

    @Test
    public void test_getCanonicalName_Class_String() {
        assertEquals("org.apache.commons.lang3.ClassUtils", ClassUtils.getCanonicalName(ClassUtils.class, "X"));
        assertEquals("java.util.Map.Entry", ClassUtils.getCanonicalName(Map.Entry.class, "X"));
        assertEquals("X", ClassUtils.getCanonicalName((Class<?>) null, "X"));

        assertEquals("java.lang.String[]", ClassUtils.getCanonicalName(String[].class, "X"));
        assertEquals("java.util.Map.Entry[]", ClassUtils.getCanonicalName(Map.Entry[].class, "X"));

        // Primitives
        assertEquals("boolean", ClassUtils.getCanonicalName(boolean.class, "X"));
        assertEquals("byte", ClassUtils.getCanonicalName(byte.class, "X"));
        assertEquals("char", ClassUtils.getCanonicalName(char.class, "X"));
        assertEquals("short", ClassUtils.getCanonicalName(short.class, "X"));
        assertEquals("int", ClassUtils.getCanonicalName(int.class, "X"));
        assertEquals("long", ClassUtils.getCanonicalName(long.class, "X"));
        assertEquals("float", ClassUtils.getCanonicalName(float.class, "X"));
        assertEquals("double", ClassUtils.getCanonicalName(double.class, "X"));

        // Primitive Arrays
        assertEquals("boolean[]", ClassUtils.getCanonicalName(boolean[].class, "X"));
        assertEquals("byte[]", ClassUtils.getCanonicalName(byte[].class, "X"));
        assertEquals("char[]", ClassUtils.getCanonicalName(char[].class, "X"));
        assertEquals("short[]", ClassUtils.getCanonicalName(short[].class, "X"));
        assertEquals("int[]", ClassUtils.getCanonicalName(int[].class, "X"));
        assertEquals("long[]", ClassUtils.getCanonicalName(long[].class, "X"));
        assertEquals("float[]", ClassUtils.getCanonicalName(float[].class, "X"));
        assertEquals("double[]", ClassUtils.getCanonicalName(double[].class, "X"));

        // Arrays of arrays of ...
        assertEquals("java.lang.String[][]", ClassUtils.getCanonicalName(String[][].class, "X"));
        assertEquals("java.lang.String[][][]", ClassUtils.getCanonicalName(String[][][].class, "X"));
        assertEquals("java.lang.String[][][][]", ClassUtils.getCanonicalName(String[][][][].class, "X"));

        // Inner types
        class Named {
            // empty
        }
        assertEquals("X", ClassUtils.getCanonicalName(new Object() {
            // empty
        }.getClass(), "X"));
        assertEquals("X", ClassUtils.getCanonicalName(Named.class, "X"));
        assertEquals("org.apache.commons.lang3.ClassUtilsTest.Inner", ClassUtils.getCanonicalName(Inner.class, "X"));
        assertEquals("X", ClassUtils.getCanonicalName((Object) null, "X"));
        assertEquals(OBJECT_CANONICAL_NAME, ClassUtils.getCanonicalName(new Object()));
    }

    @Test
    public void test_getClass() {
       // assertEquals("org.apache.commons.lang3.ClassUtils", ClassUtils.getName(ClassLoader.class, "@"));
    }

    @Test
    public void test_getName_Class() {
        assertEquals("org.apache.commons.lang3.ClassUtils", ClassUtils.getName(ClassUtils.class));
        assertEquals("java.util.Map$Entry", ClassUtils.getName(Map.Entry.class));
        assertEquals("", ClassUtils.getName((Class<?>) null));

        assertEquals("[Ljava.lang.String;", ClassUtils.getName(String[].class));
        assertEquals("[Ljava.util.Map$Entry;", ClassUtils.getName(Map.Entry[].class));

        // Primitives
        assertEquals("boolean", ClassUtils.getName(boolean.class));
        assertEquals("byte", ClassUtils.getName(byte.class));
        assertEquals("char", ClassUtils.getName(char.class));
        assertEquals("short", ClassUtils.getName(short.class));
        assertEquals("int", ClassUtils.getName(int.class));
        assertEquals("long", ClassUtils.getName(long.class));
        assertEquals("float", ClassUtils.getName(float.class));
        assertEquals("double", ClassUtils.getName(double.class));

        // Primitive Arrays
        assertEquals("[Z", ClassUtils.getName(boolean[].class));
        assertEquals("[B", ClassUtils.getName(byte[].class));
        assertEquals("[C", ClassUtils.getName(char[].class));
        assertEquals("[S", ClassUtils.getName(short[].class));
        assertEquals("[I", ClassUtils.getName(int[].class));
        assertEquals("[J", ClassUtils.getName(long[].class));
        assertEquals("[F", ClassUtils.getName(float[].class));
        assertEquals("[D", ClassUtils.getName(double[].class));

        // Arrays of arrays of ...
        assertEquals("[[Ljava.lang.String;", ClassUtils.getName(String[][].class));
        assertEquals("[[[Ljava.lang.String;", ClassUtils.getName(String[][][].class));
        assertEquals("[[[[Ljava.lang.String;", ClassUtils.getName(String[][][][].class));

        // Inner types
        class Named {
            // empty
        }
        assertEquals("org.apache.commons.lang3.ClassUtilsTest$3", ClassUtils.getName(new Object() {
            // empty
        }.getClass()));
        assertEquals("org.apache.commons.lang3.ClassUtilsTest$3Named", ClassUtils.getName(Named.class));
        assertEquals("org.apache.commons.lang3.ClassUtilsTest$Inner", ClassUtils.getName(Inner.class));
        assertEquals(OBJECT_CANONICAL_NAME, ClassUtils.getName(new Object()));
    }

    @Test
    public void test_getName_Object() {
        assertEquals("org.apache.commons.lang3.ClassUtils", ClassUtils.getName(new ClassUtils(), "<null>"));
        assertEquals("org.apache.commons.lang3.ClassUtilsTest$Inner", ClassUtils.getName(new Inner(), "<null>"));
        assertEquals("java.lang.String", ClassUtils.getName("hello", "<null>"));
        assertEquals("<null>", ClassUtils.getName(null, "<null>"));

        // Inner types
        class Named {
            // empty
        }
        assertEquals("org.apache.commons.lang3.ClassUtilsTest$4", ClassUtils.getName(new Object() {
            // empty
        }, "<null>"));
        assertEquals("org.apache.commons.lang3.ClassUtilsTest$4Named", ClassUtils.getName(new Named(), "<null>"));
        assertEquals("org.apache.commons.lang3.ClassUtilsTest$Inner", ClassUtils.getName(new Inner(), "<null>"));
    }

    @Test
    public void test_getPackageCanonicalName_Class() {
        assertEquals("org.apache.commons.lang3", ClassUtils.getPackageCanonicalName(ClassUtils.class));
        assertEquals("org.apache.commons.lang3", ClassUtils.getPackageCanonicalName(ClassUtils[].class));
        assertEquals("org.apache.commons.lang3", ClassUtils.getPackageCanonicalName(ClassUtils[][].class));
        assertEquals("", ClassUtils.getPackageCanonicalName(int[].class));
        assertEquals("", ClassUtils.getPackageCanonicalName(int[][].class));

        // Inner types
        class Named {
            // empty
        }
        assertEquals("org.apache.commons.lang3", ClassUtils.getPackageCanonicalName(new Object() {
            // empty
        }.getClass()));
        assertEquals("org.apache.commons.lang3", ClassUtils.getPackageCanonicalName(Named.class));
        assertEquals("org.apache.commons.lang3", ClassUtils.getPackageCanonicalName(Inner.class));
        assertEquals(StringUtils.EMPTY, ClassUtils.getPackageCanonicalName((Class<?>) null));
    }

    @Test
    public void test_getPackageCanonicalName_Object() {
        assertEquals("<null>", ClassUtils.getPackageCanonicalName(null, "<null>"));
        assertEquals("org.apache.commons.lang3", ClassUtils.getPackageCanonicalName(new ClassUtils(), "<null>"));
        assertEquals("org.apache.commons.lang3", ClassUtils.getPackageCanonicalName(new ClassUtils[0], "<null>"));
        assertEquals("org.apache.commons.lang3", ClassUtils.getPackageCanonicalName(new ClassUtils[0][0], "<null>"));
        assertEquals("", ClassUtils.getPackageCanonicalName(new int[0], "<null>"));
        assertEquals("", ClassUtils.getPackageCanonicalName(new int[0][0], "<null>"));

        // Inner types
        class Named {
            // empty
        }
        assertEquals("org.apache.commons.lang3", ClassUtils.getPackageCanonicalName(new Object() {
            // empty
        }, "<null>"));
        assertEquals("org.apache.commons.lang3", ClassUtils.getPackageCanonicalName(new Named(), "<null>"));
        assertEquals("org.apache.commons.lang3", ClassUtils.getPackageCanonicalName(new Inner(), "<null>"));
    }

    @Test
    public void test_getPackageCanonicalName_String() {
        assertEquals("org.apache.commons.lang3", ClassUtils.getPackageCanonicalName("org.apache.commons.lang3.ClassUtils"));
        assertEquals("org.apache.commons.lang3", ClassUtils.getPackageCanonicalName("[Lorg.apache.commons.lang3.ClassUtils;"));
        assertEquals("org.apache.commons.lang3", ClassUtils.getPackageCanonicalName("[[Lorg.apache.commons.lang3.ClassUtils;"));
        assertEquals("org.apache.commons.lang3", ClassUtils.getPackageCanonicalName("org.apache.commons.lang3.ClassUtils[]"));
        assertEquals("org.apache.commons.lang3", ClassUtils.getPackageCanonicalName("org.apache.commons.lang3.ClassUtils[][]"));
        assertEquals("", ClassUtils.getPackageCanonicalName("[I"));
        assertEquals("", ClassUtils.getPackageCanonicalName("[[I"));
        assertEquals("", ClassUtils.getPackageCanonicalName("int[]"));
        assertEquals("", ClassUtils.getPackageCanonicalName("int[][]"));

        // Inner types
        assertEquals("org.apache.commons.lang3", ClassUtils.getPackageCanonicalName("org.apache.commons.lang3.ClassUtilsTest$6"));
        assertEquals("org.apache.commons.lang3", ClassUtils.getPackageCanonicalName("org.apache.commons.lang3.ClassUtilsTest$5Named"));
        assertEquals("org.apache.commons.lang3", ClassUtils.getPackageCanonicalName("org.apache.commons.lang3.ClassUtilsTest$Inner"));
    }

    @Test
    public void test_getPackageName_Class() {
        assertEquals("java.lang", ClassUtils.getPackageName(String.class));
        assertEquals("java.util", ClassUtils.getPackageName(Map.Entry.class));
        assertEquals("", ClassUtils.getPackageName((Class<?>) null));

        // LANG-535
        assertEquals("java.lang", ClassUtils.getPackageName(String[].class));

        // Primitive Arrays
        assertEquals("", ClassUtils.getPackageName(boolean[].class));
        assertEquals("", ClassUtils.getPackageName(byte[].class));
        assertEquals("", ClassUtils.getPackageName(char[].class));
        assertEquals("", ClassUtils.getPackageName(short[].class));
        assertEquals("", ClassUtils.getPackageName(int[].class));
        assertEquals("", ClassUtils.getPackageName(long[].class));
        assertEquals("", ClassUtils.getPackageName(float[].class));
        assertEquals("", ClassUtils.getPackageName(double[].class));

        // Arrays of arrays of ...
        assertEquals("java.lang", ClassUtils.getPackageName(String[][].class));
        assertEquals("java.lang", ClassUtils.getPackageName(String[][][].class));
        assertEquals("java.lang", ClassUtils.getPackageName(String[][][][].class));

        // On-the-fly types
        class Named {
            // empty
        }
        assertEquals("org.apache.commons.lang3", ClassUtils.getPackageName(new Object() {
            // empty
        }.getClass()));
        assertEquals("org.apache.commons.lang3", ClassUtils.getPackageName(Named.class));
    }

    @Test
    public void test_getPackageName_Object() {
        assertEquals("org.apache.commons.lang3", ClassUtils.getPackageName(new ClassUtils(), "<null>"));
        assertEquals("org.apache.commons.lang3", ClassUtils.getPackageName(new Inner(), "<null>"));
        assertEquals("<null>", ClassUtils.getPackageName(null, "<null>"));
    }

    @Test
    public void test_getPackageName_String() {
        assertEquals("org.apache.commons.lang3", ClassUtils.getPackageName(ClassUtils.class.getName()));
        assertEquals("java.util", ClassUtils.getPackageName(Map.Entry.class.getName()));
        assertEquals("", ClassUtils.getPackageName((String) null));
        assertEquals("", ClassUtils.getPackageName(""));
    }

    @Test
    public void test_getShortCanonicalName_Class() {
        assertEquals("ClassUtils", ClassUtils.getShortCanonicalName(ClassUtils.class));
        assertEquals("ClassUtils[]", ClassUtils.getShortCanonicalName(ClassUtils[].class));
        assertEquals("ClassUtils[][]", ClassUtils.getShortCanonicalName(ClassUtils[][].class));
        assertEquals("int[]", ClassUtils.getShortCanonicalName(int[].class));
        assertEquals("int[][]", ClassUtils.getShortCanonicalName(int[][].class));

        // Inner types
        class Named {
            // empty
        }
        assertEquals("", ClassUtils.getShortCanonicalName(new Object() {
            // empty
        }.getClass()));
        // WARNING: this is fragile, implementation may change, naming is not guaranteed
        assertEquals("", ClassUtils.getShortCanonicalName(Named.class));
        assertEquals("Inner", ClassUtils.getShortCanonicalName(Inner.class));
        assertEquals(StringUtils.EMPTY, ClassUtils.getShortCanonicalName((Class<?>) null));
    }

    @Test
    public void test_getShortCanonicalName_Object() {
        assertEquals("<null>", ClassUtils.getShortCanonicalName(null, "<null>"));
        assertEquals("ClassUtils", ClassUtils.getShortCanonicalName(new ClassUtils(), "<null>"));
        assertEquals("ClassUtils[]", ClassUtils.getShortCanonicalName(new ClassUtils[0], "<null>"));
        assertEquals("ClassUtils[][]", ClassUtils.getShortCanonicalName(new ClassUtils[0][0], "<null>"));
        assertEquals("int[]", ClassUtils.getShortCanonicalName(new int[0], "<null>"));
        assertEquals("int[][]", ClassUtils.getShortCanonicalName(new int[0][0], "<null>"));

        // Inner types
        class Named {
            // empty
        }
        assertEquals("", ClassUtils.getShortCanonicalName(new Object() {
            // empty
        }, "<null>"));
        assertEquals("", ClassUtils.getShortCanonicalName(new Named(), "<null>"));
        assertEquals("Inner", ClassUtils.getShortCanonicalName(new Inner(), "<null>"));
    }

    @Test
    public void test_getShortCanonicalName_String() {
        assertEquals("", ClassUtils.getShortCanonicalName((String) null));
        assertEquals("Map.Entry", ClassUtils.getShortCanonicalName(java.util.Map.Entry.class.getName()));
        assertEquals("Entry", ClassUtils.getShortCanonicalName(java.util.Map.Entry.class.getCanonicalName()));
        assertEquals("ClassUtils", ClassUtils.getShortCanonicalName("org.apache.commons.lang3.ClassUtils"));
        assertEquals("ClassUtils[]", ClassUtils.getShortCanonicalName("[Lorg.apache.commons.lang3.ClassUtils;"));
        assertEquals("ClassUtils[][]", ClassUtils.getShortCanonicalName("[[Lorg.apache.commons.lang3.ClassUtils;"));
        assertEquals("ClassUtils[]", ClassUtils.getShortCanonicalName("org.apache.commons.lang3.ClassUtils[]"));
        assertEquals("ClassUtils[][]", ClassUtils.getShortCanonicalName("org.apache.commons.lang3.ClassUtils[][]"));
        assertEquals("int[]", ClassUtils.getShortCanonicalName("[I"));
        assertEquals("int[]", ClassUtils.getShortCanonicalName(int[].class.getCanonicalName()));
        assertEquals("int[]", ClassUtils.getShortCanonicalName(int[].class.getName()));
        assertEquals("int[][]", ClassUtils.getShortCanonicalName("[[I"));
        assertEquals("int[]", ClassUtils.getShortCanonicalName("int[]"));
        assertEquals("int[][]", ClassUtils.getShortCanonicalName("int[][]"));

        // this is to demonstrate that the documentation and the naming of the methods
        // uses the class name and canonical name totally mixed up, which cannot be
        // fixed without backward compatibility break
        assertEquals("int[]", int[].class.getCanonicalName());
        assertEquals("[I", int[].class.getName());

        // Inner types... the problem is that these are not canonical names, classes with this name do not even have canonical
        // name
        // WARNING: this is fragile, implementation may change, naming is not guaranteed
        assertEquals("ClassUtilsTest.6", ClassUtils.getShortCanonicalName("org.apache.commons.lang3.ClassUtilsTest$6"));
        // WARNING: this is fragile, implementation may change, naming is not guaranteed
        assertEquals("ClassUtilsTest.5Named", ClassUtils.getShortCanonicalName("org.apache.commons.lang3.ClassUtilsTest$5Named"));
        assertEquals("ClassUtilsTest.Inner", ClassUtils.getShortCanonicalName("org.apache.commons.lang3.ClassUtilsTest$Inner"));
        // demonstrating what a canonical name is... it is a bigger issue to clean this up
        assertEquals("org.apache.commons.lang3.ClassUtilsTest$10", new org.apache.commons.lang3.ClassUtilsTest() {
        }.getClass().getName());
        assertNull(new org.apache.commons.lang3.ClassUtilsTest() {
        }.getClass().getCanonicalName());
    }

    @Test
    public void test_getShortClassName_Class() {
        assertEquals("ClassUtils", ClassUtils.getShortClassName(ClassUtils.class));
        assertEquals("Map.Entry", ClassUtils.getShortClassName(Map.Entry.class));
        assertEquals("", ClassUtils.getShortClassName((Class<?>) null));

        // LANG-535
        assertEquals("String[]", ClassUtils.getShortClassName(String[].class));
        assertEquals("Map.Entry[]", ClassUtils.getShortClassName(Map.Entry[].class));

        // Primitives
        assertEquals("boolean", ClassUtils.getShortClassName(boolean.class));
        assertEquals("byte", ClassUtils.getShortClassName(byte.class));
        assertEquals("char", ClassUtils.getShortClassName(char.class));
        assertEquals("short", ClassUtils.getShortClassName(short.class));
        assertEquals("int", ClassUtils.getShortClassName(int.class));
        assertEquals("long", ClassUtils.getShortClassName(long.class));
        assertEquals("float", ClassUtils.getShortClassName(float.class));
        assertEquals("double", ClassUtils.getShortClassName(double.class));

        // Primitive Arrays
        assertEquals("boolean[]", ClassUtils.getShortClassName(boolean[].class));
        assertEquals("byte[]", ClassUtils.getShortClassName(byte[].class));
        assertEquals("char[]", ClassUtils.getShortClassName(char[].class));
        assertEquals("short[]", ClassUtils.getShortClassName(short[].class));
        assertEquals("int[]", ClassUtils.getShortClassName(int[].class));
        assertEquals("long[]", ClassUtils.getShortClassName(long[].class));
        assertEquals("float[]", ClassUtils.getShortClassName(float[].class));
        assertEquals("double[]", ClassUtils.getShortClassName(double[].class));

        // Arrays of arrays of ...
        assertEquals("String[][]", ClassUtils.getShortClassName(String[][].class));
        assertEquals("String[][][]", ClassUtils.getShortClassName(String[][][].class));
        assertEquals("String[][][][]", ClassUtils.getShortClassName(String[][][][].class));

        // Inner types
        class Named {
            // empty
        }
        // WARNING: this is fragile, implementation may change, naming is not guaranteed
        assertEquals("ClassUtilsTest.12", ClassUtils.getShortClassName(new Object() {
            // empty
        }.getClass()));
        // WARNING: this is fragile, implementation may change, naming is not guaranteed
        assertEquals("ClassUtilsTest.10Named", ClassUtils.getShortClassName(Named.class));
        assertEquals("ClassUtilsTest.Inner", ClassUtils.getShortClassName(Inner.class));
    }

    @Test
    public void test_getShortClassName_Object() {
        assertEquals("ClassUtils", ClassUtils.getShortClassName(new ClassUtils(), "<null>"));
        assertEquals("ClassUtilsTest.Inner", ClassUtils.getShortClassName(new Inner(), "<null>"));
        assertEquals("String", ClassUtils.getShortClassName("hello", "<null>"));
        assertEquals("<null>", ClassUtils.getShortClassName(null, "<null>"));

        // Inner types
        class Named {
            // empty
        }
        // WARNING: this is fragile, implementation may change, naming is not guaranteed
        assertEquals("ClassUtilsTest.13", ClassUtils.getShortClassName(new Object() {
            // empty
        }, "<null>"));
        // WARNING: this is fragile, implementation may change, naming is not guaranteed
        assertEquals("ClassUtilsTest.11Named", ClassUtils.getShortClassName(new Named(), "<null>"));
        assertEquals("ClassUtilsTest.Inner", ClassUtils.getShortClassName(new Inner(), "<null>"));
    }

    @Test
    public void test_getShortClassName_String() {
        assertEquals("ClassUtils", ClassUtils.getShortClassName(ClassUtils.class.getName()));
        assertEquals("Map.Entry", ClassUtils.getShortClassName(Map.Entry.class.getName()));
        assertEquals("", ClassUtils.getShortClassName((String) null));
        assertEquals("", ClassUtils.getShortClassName(""));
    }

    @Test
    public void test_getSimpleName_Class() {
        assertEquals("ClassUtils", ClassUtils.getSimpleName(ClassUtils.class));
        assertEquals("Entry", ClassUtils.getSimpleName(Map.Entry.class));
        assertEquals("", ClassUtils.getSimpleName(null));

        // LANG-535
        assertEquals("String[]", ClassUtils.getSimpleName(String[].class));
        assertEquals("Entry[]", ClassUtils.getSimpleName(Map.Entry[].class));

        // Primitives
        assertEquals("boolean", ClassUtils.getSimpleName(boolean.class));
        assertEquals("byte", ClassUtils.getSimpleName(byte.class));
        assertEquals("char", ClassUtils.getSimpleName(char.class));
        assertEquals("short", ClassUtils.getSimpleName(short.class));
        assertEquals("int", ClassUtils.getSimpleName(int.class));
        assertEquals("long", ClassUtils.getSimpleName(long.class));
        assertEquals("float", ClassUtils.getSimpleName(float.class));
        assertEquals("double", ClassUtils.getSimpleName(double.class));

        // Primitive Arrays
        assertEquals("boolean[]", ClassUtils.getSimpleName(boolean[].class));
        assertEquals("byte[]", ClassUtils.getSimpleName(byte[].class));
        assertEquals("char[]", ClassUtils.getSimpleName(char[].class));
        assertEquals("short[]", ClassUtils.getSimpleName(short[].class));
        assertEquals("int[]", ClassUtils.getSimpleName(int[].class));
        assertEquals("long[]", ClassUtils.getSimpleName(long[].class));
        assertEquals("float[]", ClassUtils.getSimpleName(float[].class));
        assertEquals("double[]", ClassUtils.getSimpleName(double[].class));

        // Arrays of arrays of ...
        assertEquals("String[][]", ClassUtils.getSimpleName(String[][].class));
        assertEquals("String[][][]", ClassUtils.getSimpleName(String[][][].class));
        assertEquals("String[][][][]", ClassUtils.getSimpleName(String[][][][].class));

        // On-the-fly types
        class Named {
            // empty
        }
        assertEquals("", ClassUtils.getSimpleName(new Object() {
            // empty
        }.getClass()));
        assertEquals("Named", ClassUtils.getSimpleName(Named.class));
    }

    @Test
    public void test_getSimpleName_Object() {
        assertEquals("ClassUtils", ClassUtils.getSimpleName(new ClassUtils()));
        assertEquals("Inner", ClassUtils.getSimpleName(new Inner()));
        assertEquals("String", ClassUtils.getSimpleName("hello"));
        assertEquals(StringUtils.EMPTY, ClassUtils.getSimpleName(null));
        assertEquals(StringUtils.EMPTY, ClassUtils.getSimpleName(null));
    }

    @Test
    public void test_getSimpleName_Object_String() {
        assertEquals("ClassUtils", ClassUtils.getSimpleName(new ClassUtils(), "<null>"));
        assertEquals("Inner", ClassUtils.getSimpleName(new Inner(), "<null>"));
        assertEquals("String", ClassUtils.getSimpleName("hello", "<null>"));
        assertEquals("<null>", ClassUtils.getSimpleName(null, "<null>"));
        assertNull(ClassUtils.getSimpleName(null, null));
    }

    @Test
    public void test_isAssignable() {
        assertFalse(ClassUtils.isAssignable((Class<?>) null, null));
        assertFalse(ClassUtils.isAssignable(String.class, null));

        assertTrue(ClassUtils.isAssignable(null, Object.class));
        assertTrue(ClassUtils.isAssignable(null, Integer.class));
        assertFalse(ClassUtils.isAssignable(null, Integer.TYPE));
        assertTrue(ClassUtils.isAssignable(String.class, Object.class));
        assertTrue(ClassUtils.isAssignable(String.class, String.class));
        assertFalse(ClassUtils.isAssignable(Object.class, String.class));

        assertTrue(ClassUtils.isAssignable(Integer.TYPE, Integer.class));
        assertTrue(ClassUtils.isAssignable(Integer.TYPE, Object.class));
        assertTrue(ClassUtils.isAssignable(Integer.class, Integer.TYPE));
        assertTrue(ClassUtils.isAssignable(Integer.class, Object.class));
        assertTrue(ClassUtils.isAssignable(Integer.TYPE, Integer.TYPE));
        assertTrue(ClassUtils.isAssignable(Integer.class, Integer.class));
        assertTrue(ClassUtils.isAssignable(Boolean.TYPE, Boolean.class));
        assertTrue(ClassUtils.isAssignable(Boolean.TYPE, Object.class));
        assertTrue(ClassUtils.isAssignable(Boolean.class, Boolean.TYPE));
        assertTrue(ClassUtils.isAssignable(Boolean.class, Object.class));
        assertTrue(ClassUtils.isAssignable(Boolean.TYPE, Boolean.TYPE));
        assertTrue(ClassUtils.isAssignable(Boolean.class, Boolean.class));
    }

    @Test
    public void test_isAssignable_Autoboxing() {
        assertFalse(ClassUtils.isAssignable((Class<?>) null, null, true));
        assertFalse(ClassUtils.isAssignable(String.class, null, true));

        assertTrue(ClassUtils.isAssignable(null, Object.class, true));
        assertTrue(ClassUtils.isAssignable(null, Integer.class, true));
        assertFalse(ClassUtils.isAssignable(null, Integer.TYPE, true));
        assertTrue(ClassUtils.isAssignable(String.class, Object.class, true));
        assertTrue(ClassUtils.isAssignable(String.class, String.class, true));
        assertFalse(ClassUtils.isAssignable(Object.class, String.class, true));
        assertTrue(ClassUtils.isAssignable(Integer.TYPE, Integer.class, true));
        assertTrue(ClassUtils.isAssignable(Integer.TYPE, Object.class, true));
        assertTrue(ClassUtils.isAssignable(Integer.class, Integer.TYPE, true));
        assertTrue(ClassUtils.isAssignable(Integer.class, Object.class, true));
        assertTrue(ClassUtils.isAssignable(Integer.TYPE, Integer.TYPE, true));
        assertTrue(ClassUtils.isAssignable(Integer.class, Integer.class, true));
        assertTrue(ClassUtils.isAssignable(Boolean.TYPE, Boolean.class, true));
        assertTrue(ClassUtils.isAssignable(Boolean.class, Boolean.TYPE, true));
        assertTrue(ClassUtils.isAssignable(Boolean.class, Object.class, true));
        assertTrue(ClassUtils.isAssignable(Boolean.TYPE, Boolean.TYPE, true));
        assertTrue(ClassUtils.isAssignable(Boolean.class, Boolean.class, true));
    }

    @Test
    public void test_isAssignable_ClassArray_ClassArray() {
        final Class<?>[] array2 = new Class[] {Object.class, Object.class};
        final Class<?>[] array1 = new Class[] {Object.class};
        final Class<?>[] array1s = new Class[] {String.class};
        final Class<?>[] array0 = new Class[] {};
        final Class<?>[] arrayPrimitives = {Integer.TYPE, Boolean.TYPE};
        final Class<?>[] arrayWrappers = {Integer.class, Boolean.class};

        assertFalse(ClassUtils.isAssignable(array1, array2));
        assertFalse(ClassUtils.isAssignable(null, array2));
        assertTrue(ClassUtils.isAssignable(null, array0));
        assertTrue(ClassUtils.isAssignable(array0, array0));
        assertTrue(ClassUtils.isAssignable(array0, (Class<?>[]) null)); // explicit cast to avoid warning
        assertTrue(ClassUtils.isAssignable(null, (Class<?>[]) null)); // explicit cast to avoid warning

        assertFalse(ClassUtils.isAssignable(array1, array1s));
        assertTrue(ClassUtils.isAssignable(array1s, array1s));
        assertTrue(ClassUtils.isAssignable(array1s, array1));

        assertTrue(ClassUtils.isAssignable(arrayPrimitives, arrayWrappers));
        assertTrue(ClassUtils.isAssignable(arrayWrappers, arrayPrimitives));
        assertFalse(ClassUtils.isAssignable(arrayPrimitives, array1));
        assertFalse(ClassUtils.isAssignable(arrayWrappers, array1));
        assertTrue(ClassUtils.isAssignable(arrayPrimitives, array2));
        assertTrue(ClassUtils.isAssignable(arrayWrappers, array2));
    }

    @Test
    public void test_isAssignable_ClassArray_ClassArray_Autoboxing() {
        final Class<?>[] array2 = new Class[] {Object.class, Object.class};
        final Class<?>[] array1 = new Class[] {Object.class};
        final Class<?>[] array1s = new Class[] {String.class};
        final Class<?>[] array0 = new Class[] {};
        final Class<?>[] arrayPrimitives = {Integer.TYPE, Boolean.TYPE};
        final Class<?>[] arrayWrappers = {Integer.class, Boolean.class};

        assertFalse(ClassUtils.isAssignable(array1, array2, true));
        assertFalse(ClassUtils.isAssignable(null, array2, true));
        assertTrue(ClassUtils.isAssignable(null, array0, true));
        assertTrue(ClassUtils.isAssignable(array0, array0, true));
        assertTrue(ClassUtils.isAssignable(array0, null, true));
        assertTrue(ClassUtils.isAssignable((Class[]) null, null, true));

        assertFalse(ClassUtils.isAssignable(array1, array1s, true));
        assertTrue(ClassUtils.isAssignable(array1s, array1s, true));
        assertTrue(ClassUtils.isAssignable(array1s, array1, true));

        assertTrue(ClassUtils.isAssignable(arrayPrimitives, arrayWrappers, true));
        assertTrue(ClassUtils.isAssignable(arrayWrappers, arrayPrimitives, true));
        assertFalse(ClassUtils.isAssignable(arrayPrimitives, array1, true));
        assertFalse(ClassUtils.isAssignable(arrayWrappers, array1, true));
        assertTrue(ClassUtils.isAssignable(arrayPrimitives, array2, true));
        assertTrue(ClassUtils.isAssignable(arrayWrappers, array2, true));
    }

    @Test
    public void test_isAssignable_ClassArray_ClassArray_NoAutoboxing() {
        final Class<?>[] array2 = new Class[] {Object.class, Object.class};
        final Class<?>[] array1 = new Class[] {Object.class};
        final Class<?>[] array1s = new Class[] {String.class};
        final Class<?>[] array0 = new Class[] {};
        final Class<?>[] arrayPrimitives = {Integer.TYPE, Boolean.TYPE};
        final Class<?>[] arrayWrappers = {Integer.class, Boolean.class};

        assertFalse(ClassUtils.isAssignable(array1, array2, false));
        assertFalse(ClassUtils.isAssignable(null, array2, false));
        assertTrue(ClassUtils.isAssignable(null, array0, false));
        assertTrue(ClassUtils.isAssignable(array0, array0, false));
        assertTrue(ClassUtils.isAssignable(array0, null, false));
        assertTrue(ClassUtils.isAssignable((Class[]) null, null, false));

        assertFalse(ClassUtils.isAssignable(array1, array1s, false));
        assertTrue(ClassUtils.isAssignable(array1s, array1s, false));
        assertTrue(ClassUtils.isAssignable(array1s, array1, false));

        assertFalse(ClassUtils.isAssignable(arrayPrimitives, arrayWrappers, false));
        assertFalse(ClassUtils.isAssignable(arrayWrappers, arrayPrimitives, false));
        assertFalse(ClassUtils.isAssignable(arrayPrimitives, array1, false));
        assertFalse(ClassUtils.isAssignable(arrayWrappers, array1, false));
        assertTrue(ClassUtils.isAssignable(arrayWrappers, array2, false));
        assertFalse(ClassUtils.isAssignable(arrayPrimitives, array2, false));
    }

    @Test
    public void test_isAssignable_DefaultUnboxing_Widening() {
        // test byte conversions
        assertFalse(ClassUtils.isAssignable(Byte.class, Character.TYPE), "byte -> char");
        assertTrue(ClassUtils.isAssignable(Byte.class, Byte.TYPE), "byte -> byte");
        assertTrue(ClassUtils.isAssignable(Byte.class, Short.TYPE), "byte -> short");
        assertTrue(ClassUtils.isAssignable(Byte.class, Integer.TYPE), "byte -> int");
        assertTrue(ClassUtils.isAssignable(Byte.class, Long.TYPE), "byte -> long");
        assertTrue(ClassUtils.isAssignable(Byte.class, Float.TYPE), "byte -> float");
        assertTrue(ClassUtils.isAssignable(Byte.class, Double.TYPE), "byte -> double");
        assertFalse(ClassUtils.isAssignable(Byte.class, Boolean.TYPE), "byte -> boolean");

        // test short conversions
        assertFalse(ClassUtils.isAssignable(Short.class, Character.TYPE), "short -> char");
        assertFalse(ClassUtils.isAssignable(Short.class, Byte.TYPE), "short -> byte");
        assertTrue(ClassUtils.isAssignable(Short.class, Short.TYPE), "short -> short");
        assertTrue(ClassUtils.isAssignable(Short.class, Integer.TYPE), "short -> int");
        assertTrue(ClassUtils.isAssignable(Short.class, Long.TYPE), "short -> long");
        assertTrue(ClassUtils.isAssignable(Short.class, Float.TYPE), "short -> float");
        assertTrue(ClassUtils.isAssignable(Short.class, Double.TYPE), "short -> double");
        assertFalse(ClassUtils.isAssignable(Short.class, Boolean.TYPE), "short -> boolean");

        // test char conversions
        assertTrue(ClassUtils.isAssignable(Character.class, Character.TYPE), "char -> char");
        assertFalse(ClassUtils.isAssignable(Character.class, Byte.TYPE), "char -> byte");
        assertFalse(ClassUtils.isAssignable(Character.class, Short.TYPE), "char -> short");
        assertTrue(ClassUtils.isAssignable(Character.class, Integer.TYPE), "char -> int");
        assertTrue(ClassUtils.isAssignable(Character.class, Long.TYPE), "char -> long");
        assertTrue(ClassUtils.isAssignable(Character.class, Float.TYPE), "char -> float");
        assertTrue(ClassUtils.isAssignable(Character.class, Double.TYPE), "char -> double");
        assertFalse(ClassUtils.isAssignable(Character.class, Boolean.TYPE), "char -> boolean");

        // test int conversions
        assertFalse(ClassUtils.isAssignable(Integer.class, Character.TYPE), "int -> char");
        assertFalse(ClassUtils.isAssignable(Integer.class, Byte.TYPE), "int -> byte");
        assertFalse(ClassUtils.isAssignable(Integer.class, Short.TYPE), "int -> short");
        assertTrue(ClassUtils.isAssignable(Integer.class, Integer.TYPE), "int -> int");
        assertTrue(ClassUtils.isAssignable(Integer.class, Long.TYPE), "int -> long");
        assertTrue(ClassUtils.isAssignable(Integer.class, Float.TYPE), "int -> float");
        assertTrue(ClassUtils.isAssignable(Integer.class, Double.TYPE), "int -> double");
        assertFalse(ClassUtils.isAssignable(Integer.class, Boolean.TYPE), "int -> boolean");

        // test long conversions
        assertFalse(ClassUtils.isAssignable(Long.class, Character.TYPE), "long -> char");
        assertFalse(ClassUtils.isAssignable(Long.class, Byte.TYPE), "long -> byte");
        assertFalse(ClassUtils.isAssignable(Long.class, Short.TYPE), "long -> short");
        assertFalse(ClassUtils.isAssignable(Long.class, Integer.TYPE), "long -> int");
        assertTrue(ClassUtils.isAssignable(Long.class, Long.TYPE), "long -> long");
        assertTrue(ClassUtils.isAssignable(Long.class, Float.TYPE), "long -> float");
        assertTrue(ClassUtils.isAssignable(Long.class, Double.TYPE), "long -> double");
        assertFalse(ClassUtils.isAssignable(Long.class, Boolean.TYPE), "long -> boolean");

        // test float conversions
        assertFalse(ClassUtils.isAssignable(Float.class, Character.TYPE), "float -> char");
        assertFalse(ClassUtils.isAssignable(Float.class, Byte.TYPE), "float -> byte");
        assertFalse(ClassUtils.isAssignable(Float.class, Short.TYPE), "float -> short");
        assertFalse(ClassUtils.isAssignable(Float.class, Integer.TYPE), "float -> int");
        assertFalse(ClassUtils.isAssignable(Float.class, Long.TYPE), "float -> long");
        assertTrue(ClassUtils.isAssignable(Float.class, Float.TYPE), "float -> float");
        assertTrue(ClassUtils.isAssignable(Float.class, Double.TYPE), "float -> double");
        assertFalse(ClassUtils.isAssignable(Float.class, Boolean.TYPE), "float -> boolean");

        // test double conversions
        assertFalse(ClassUtils.isAssignable(Double.class, Character.TYPE), "double -> char");
        assertFalse(ClassUtils.isAssignable(Double.class, Byte.TYPE), "double -> byte");
        assertFalse(ClassUtils.isAssignable(Double.class, Short.TYPE), "double -> short");
        assertFalse(ClassUtils.isAssignable(Double.class, Integer.TYPE), "double -> int");
        assertFalse(ClassUtils.isAssignable(Double.class, Long.TYPE), "double -> long");
        assertFalse(ClassUtils.isAssignable(Double.class, Float.TYPE), "double -> float");
        assertTrue(ClassUtils.isAssignable(Double.class, Double.TYPE), "double -> double");
        assertFalse(ClassUtils.isAssignable(Double.class, Boolean.TYPE), "double -> boolean");

        // test boolean conversions
        assertFalse(ClassUtils.isAssignable(Boolean.class, Character.TYPE), "boolean -> char");
        assertFalse(ClassUtils.isAssignable(Boolean.class, Byte.TYPE), "boolean -> byte");
        assertFalse(ClassUtils.isAssignable(Boolean.class, Short.TYPE), "boolean -> short");
        assertFalse(ClassUtils.isAssignable(Boolean.class, Integer.TYPE), "boolean -> int");
        assertFalse(ClassUtils.isAssignable(Boolean.class, Long.TYPE), "boolean -> long");
        assertFalse(ClassUtils.isAssignable(Boolean.class, Float.TYPE), "boolean -> float");
        assertFalse(ClassUtils.isAssignable(Boolean.class, Double.TYPE), "boolean -> double");
        assertTrue(ClassUtils.isAssignable(Boolean.class, Boolean.TYPE), "boolean -> boolean");
    }

    @Test
    public void test_isAssignable_NoAutoboxing() {
        assertFalse(ClassUtils.isAssignable((Class<?>) null, null, false));
        assertFalse(ClassUtils.isAssignable(String.class, null, false));

        assertTrue(ClassUtils.isAssignable(null, Object.class, false));
        assertTrue(ClassUtils.isAssignable(null, Integer.class, false));
        assertFalse(ClassUtils.isAssignable(null, Integer.TYPE, false));
        assertTrue(ClassUtils.isAssignable(String.class, Object.class, false));
        assertTrue(ClassUtils.isAssignable(String.class, String.class, false));
        assertFalse(ClassUtils.isAssignable(Object.class, String.class, false));
        assertFalse(ClassUtils.isAssignable(Integer.TYPE, Integer.class, false));
        assertFalse(ClassUtils.isAssignable(Integer.TYPE, Object.class, false));
        assertFalse(ClassUtils.isAssignable(Integer.class, Integer.TYPE, false));
        assertTrue(ClassUtils.isAssignable(Integer.TYPE, Integer.TYPE, false));
        assertTrue(ClassUtils.isAssignable(Integer.class, Integer.class, false));
        assertFalse(ClassUtils.isAssignable(Boolean.TYPE, Boolean.class, false));
        assertFalse(ClassUtils.isAssignable(Boolean.TYPE, Object.class, false));
        assertFalse(ClassUtils.isAssignable(Boolean.class, Boolean.TYPE, false));
        assertTrue(ClassUtils.isAssignable(Boolean.class, Object.class, false));
        assertTrue(ClassUtils.isAssignable(Boolean.TYPE, Boolean.TYPE, false));
        assertTrue(ClassUtils.isAssignable(Boolean.class, Boolean.class, false));
    }

    @Test
    public void test_isAssignable_Unboxing_Widening() {
        // test byte conversions
        assertFalse(ClassUtils.isAssignable(Byte.class, Character.TYPE, true), "byte -> char");
        assertTrue(ClassUtils.isAssignable(Byte.class, Byte.TYPE, true), "byte -> byte");
        assertTrue(ClassUtils.isAssignable(Byte.class, Short.TYPE, true), "byte -> short");
        assertTrue(ClassUtils.isAssignable(Byte.class, Integer.TYPE, true), "byte -> int");
        assertTrue(ClassUtils.isAssignable(Byte.class, Long.TYPE, true), "byte -> long");
        assertTrue(ClassUtils.isAssignable(Byte.class, Float.TYPE, true), "byte -> float");
        assertTrue(ClassUtils.isAssignable(Byte.class, Double.TYPE, true), "byte -> double");
        assertFalse(ClassUtils.isAssignable(Byte.class, Boolean.TYPE, true), "byte -> boolean");

        // test short conversions
        assertFalse(ClassUtils.isAssignable(Short.class, Character.TYPE, true), "short -> char");
        assertFalse(ClassUtils.isAssignable(Short.class, Byte.TYPE, true), "short -> byte");
        assertTrue(ClassUtils.isAssignable(Short.class, Short.TYPE, true), "short -> short");
        assertTrue(ClassUtils.isAssignable(Short.class, Integer.TYPE, true), "short -> int");
        assertTrue(ClassUtils.isAssignable(Short.class, Long.TYPE, true), "short -> long");
        assertTrue(ClassUtils.isAssignable(Short.class, Float.TYPE, true), "short -> float");
        assertTrue(ClassUtils.isAssignable(Short.class, Double.TYPE, true), "short -> double");
        assertFalse(ClassUtils.isAssignable(Short.class, Boolean.TYPE, true), "short -> boolean");

        // test char conversions
        assertTrue(ClassUtils.isAssignable(Character.class, Character.TYPE, true), "char -> char");
        assertFalse(ClassUtils.isAssignable(Character.class, Byte.TYPE, true), "char -> byte");
        assertFalse(ClassUtils.isAssignable(Character.class, Short.TYPE, true), "char -> short");
        assertTrue(ClassUtils.isAssignable(Character.class, Integer.TYPE, true), "char -> int");
        assertTrue(ClassUtils.isAssignable(Character.class, Long.TYPE, true), "char -> long");
        assertTrue(ClassUtils.isAssignable(Character.class, Float.TYPE, true), "char -> float");
        assertTrue(ClassUtils.isAssignable(Character.class, Double.TYPE, true), "char -> double");
        assertFalse(ClassUtils.isAssignable(Character.class, Boolean.TYPE, true), "char -> boolean");

        // test int conversions
        assertFalse(ClassUtils.isAssignable(Integer.class, Character.TYPE, true), "int -> char");
        assertFalse(ClassUtils.isAssignable(Integer.class, Byte.TYPE, true), "int -> byte");
        assertFalse(ClassUtils.isAssignable(Integer.class, Short.TYPE, true), "int -> short");
        assertTrue(ClassUtils.isAssignable(Integer.class, Integer.TYPE, true), "int -> int");
        assertTrue(ClassUtils.isAssignable(Integer.class, Long.TYPE, true), "int -> long");
        assertTrue(ClassUtils.isAssignable(Integer.class, Float.TYPE, true), "int -> float");
        assertTrue(ClassUtils.isAssignable(Integer.class, Double.TYPE, true), "int -> double");
        assertFalse(ClassUtils.isAssignable(Integer.class, Boolean.TYPE, true), "int -> boolean");

        // test long conversions
        assertFalse(ClassUtils.isAssignable(Long.class, Character.TYPE, true), "long -> char");
        assertFalse(ClassUtils.isAssignable(Long.class, Byte.TYPE, true), "long -> byte");
        assertFalse(ClassUtils.isAssignable(Long.class, Short.TYPE, true), "long -> short");
        assertFalse(ClassUtils.isAssignable(Long.class, Integer.TYPE, true), "long -> int");
        assertTrue(ClassUtils.isAssignable(Long.class, Long.TYPE, true), "long -> long");
        assertTrue(ClassUtils.isAssignable(Long.class, Float.TYPE, true), "long -> float");
        assertTrue(ClassUtils.isAssignable(Long.class, Double.TYPE, true), "long -> double");
        assertFalse(ClassUtils.isAssignable(Long.class, Boolean.TYPE, true), "long -> boolean");

        // test float conversions
        assertFalse(ClassUtils.isAssignable(Float.class, Character.TYPE, true), "float -> char");
        assertFalse(ClassUtils.isAssignable(Float.class, Byte.TYPE, true), "float -> byte");
        assertFalse(ClassUtils.isAssignable(Float.class, Short.TYPE, true), "float -> short");
        assertFalse(ClassUtils.isAssignable(Float.class, Integer.TYPE, true), "float -> int");
        assertFalse(ClassUtils.isAssignable(Float.class, Long.TYPE, true), "float -> long");
        assertTrue(ClassUtils.isAssignable(Float.class, Float.TYPE, true), "float -> float");
        assertTrue(ClassUtils.isAssignable(Float.class, Double.TYPE, true), "float -> double");
        assertFalse(ClassUtils.isAssignable(Float.class, Boolean.TYPE, true), "float -> boolean");

        // test double conversions
        assertFalse(ClassUtils.isAssignable(Double.class, Character.TYPE, true), "double -> char");
        assertFalse(ClassUtils.isAssignable(Double.class, Byte.TYPE, true), "double -> byte");
        assertFalse(ClassUtils.isAssignable(Double.class, Short.TYPE, true), "double -> short");
        assertFalse(ClassUtils.isAssignable(Double.class, Integer.TYPE, true), "double -> int");
        assertFalse(ClassUtils.isAssignable(Double.class, Long.TYPE, true), "double -> long");
        assertFalse(ClassUtils.isAssignable(Double.class, Float.TYPE, true), "double -> float");
        assertTrue(ClassUtils.isAssignable(Double.class, Double.TYPE, true), "double -> double");
        assertFalse(ClassUtils.isAssignable(Double.class, Boolean.TYPE, true), "double -> boolean");

        // test boolean conversions
        assertFalse(ClassUtils.isAssignable(Boolean.class, Character.TYPE, true), "boolean -> char");
        assertFalse(ClassUtils.isAssignable(Boolean.class, Byte.TYPE, true), "boolean -> byte");
        assertFalse(ClassUtils.isAssignable(Boolean.class, Short.TYPE, true), "boolean -> short");
        assertFalse(ClassUtils.isAssignable(Boolean.class, Integer.TYPE, true), "boolean -> int");
        assertFalse(ClassUtils.isAssignable(Boolean.class, Long.TYPE, true), "boolean -> long");
        assertFalse(ClassUtils.isAssignable(Boolean.class, Float.TYPE, true), "boolean -> float");
        assertFalse(ClassUtils.isAssignable(Boolean.class, Double.TYPE, true), "boolean -> double");
        assertTrue(ClassUtils.isAssignable(Boolean.class, Boolean.TYPE, true), "boolean -> boolean");
    }

    @Test
    public void test_isAssignable_Widening() {
        // test byte conversions
        assertFalse(ClassUtils.isAssignable(Byte.TYPE, Character.TYPE), "byte -> char");
        assertTrue(ClassUtils.isAssignable(Byte.TYPE, Byte.TYPE), "byte -> byte");
        assertTrue(ClassUtils.isAssignable(Byte.TYPE, Short.TYPE), "byte -> short");
        assertTrue(ClassUtils.isAssignable(Byte.TYPE, Integer.TYPE), "byte -> int");
        assertTrue(ClassUtils.isAssignable(Byte.TYPE, Long.TYPE), "byte -> long");
        assertTrue(ClassUtils.isAssignable(Byte.TYPE, Float.TYPE), "byte -> float");
        assertTrue(ClassUtils.isAssignable(Byte.TYPE, Double.TYPE), "byte -> double");
        assertFalse(ClassUtils.isAssignable(Byte.TYPE, Boolean.TYPE), "byte -> boolean");

        // test short conversions
        assertFalse(ClassUtils.isAssignable(Short.TYPE, Character.TYPE), "short -> char");
        assertFalse(ClassUtils.isAssignable(Short.TYPE, Byte.TYPE), "short -> byte");
        assertTrue(ClassUtils.isAssignable(Short.TYPE, Short.TYPE), "short -> short");
        assertTrue(ClassUtils.isAssignable(Short.TYPE, Integer.TYPE), "short -> int");
        assertTrue(ClassUtils.isAssignable(Short.TYPE, Long.TYPE), "short -> long");
        assertTrue(ClassUtils.isAssignable(Short.TYPE, Float.TYPE), "short -> float");
        assertTrue(ClassUtils.isAssignable(Short.TYPE, Double.TYPE), "short -> double");
        assertFalse(ClassUtils.isAssignable(Short.TYPE, Boolean.TYPE), "short -> boolean");

        // test char conversions
        assertTrue(ClassUtils.isAssignable(Character.TYPE, Character.TYPE), "char -> char");
        assertFalse(ClassUtils.isAssignable(Character.TYPE, Byte.TYPE), "char -> byte");
        assertFalse(ClassUtils.isAssignable(Character.TYPE, Short.TYPE), "char -> short");
        assertTrue(ClassUtils.isAssignable(Character.TYPE, Integer.TYPE), "char -> int");
        assertTrue(ClassUtils.isAssignable(Character.TYPE, Long.TYPE), "char -> long");
        assertTrue(ClassUtils.isAssignable(Character.TYPE, Float.TYPE), "char -> float");
        assertTrue(ClassUtils.isAssignable(Character.TYPE, Double.TYPE), "char -> double");
        assertFalse(ClassUtils.isAssignable(Character.TYPE, Boolean.TYPE), "char -> boolean");

        // test int conversions
        assertFalse(ClassUtils.isAssignable(Integer.TYPE, Character.TYPE), "int -> char");
        assertFalse(ClassUtils.isAssignable(Integer.TYPE, Byte.TYPE), "int -> byte");
        assertFalse(ClassUtils.isAssignable(Integer.TYPE, Short.TYPE), "int -> short");
        assertTrue(ClassUtils.isAssignable(Integer.TYPE, Integer.TYPE), "int -> int");
        assertTrue(ClassUtils.isAssignable(Integer.TYPE, Long.TYPE), "int -> long");
        assertTrue(ClassUtils.isAssignable(Integer.TYPE, Float.TYPE), "int -> float");
        assertTrue(ClassUtils.isAssignable(Integer.TYPE, Double.TYPE), "int -> double");
        assertFalse(ClassUtils.isAssignable(Integer.TYPE, Boolean.TYPE), "int -> boolean");

        // test long conversions
        assertFalse(ClassUtils.isAssignable(Long.TYPE, Character.TYPE), "long -> char");
        assertFalse(ClassUtils.isAssignable(Long.TYPE, Byte.TYPE), "long -> byte");
        assertFalse(ClassUtils.isAssignable(Long.TYPE, Short.TYPE), "long -> short");
        assertFalse(ClassUtils.isAssignable(Long.TYPE, Integer.TYPE), "long -> int");
        assertTrue(ClassUtils.isAssignable(Long.TYPE, Long.TYPE), "long -> long");
        assertTrue(ClassUtils.isAssignable(Long.TYPE, Float.TYPE), "long -> float");
        assertTrue(ClassUtils.isAssignable(Long.TYPE, Double.TYPE), "long -> double");
        assertFalse(ClassUtils.isAssignable(Long.TYPE, Boolean.TYPE), "long -> boolean");

        // test float conversions
        assertFalse(ClassUtils.isAssignable(Float.TYPE, Character.TYPE), "float -> char");
        assertFalse(ClassUtils.isAssignable(Float.TYPE, Byte.TYPE), "float -> byte");
        assertFalse(ClassUtils.isAssignable(Float.TYPE, Short.TYPE), "float -> short");
        assertFalse(ClassUtils.isAssignable(Float.TYPE, Integer.TYPE), "float -> int");
        assertFalse(ClassUtils.isAssignable(Float.TYPE, Long.TYPE), "float -> long");
        assertTrue(ClassUtils.isAssignable(Float.TYPE, Float.TYPE), "float -> float");
        assertTrue(ClassUtils.isAssignable(Float.TYPE, Double.TYPE), "float -> double");
        assertFalse(ClassUtils.isAssignable(Float.TYPE, Boolean.TYPE), "float -> boolean");

        // test double conversions
        assertFalse(ClassUtils.isAssignable(Double.TYPE, Character.TYPE), "double -> char");
        assertFalse(ClassUtils.isAssignable(Double.TYPE, Byte.TYPE), "double -> byte");
        assertFalse(ClassUtils.isAssignable(Double.TYPE, Short.TYPE), "double -> short");
        assertFalse(ClassUtils.isAssignable(Double.TYPE, Integer.TYPE), "double -> int");
        assertFalse(ClassUtils.isAssignable(Double.TYPE, Long.TYPE), "double -> long");
        assertFalse(ClassUtils.isAssignable(Double.TYPE, Float.TYPE), "double -> float");
        assertTrue(ClassUtils.isAssignable(Double.TYPE, Double.TYPE), "double -> double");
        assertFalse(ClassUtils.isAssignable(Double.TYPE, Boolean.TYPE), "double -> boolean");

        // test boolean conversions
        assertFalse(ClassUtils.isAssignable(Boolean.TYPE, Character.TYPE), "boolean -> char");
        assertFalse(ClassUtils.isAssignable(Boolean.TYPE, Byte.TYPE), "boolean -> byte");
        assertFalse(ClassUtils.isAssignable(Boolean.TYPE, Short.TYPE), "boolean -> short");
        assertFalse(ClassUtils.isAssignable(Boolean.TYPE, Integer.TYPE), "boolean -> int");
        assertFalse(ClassUtils.isAssignable(Boolean.TYPE, Long.TYPE), "boolean -> long");
        assertFalse(ClassUtils.isAssignable(Boolean.TYPE, Float.TYPE), "boolean -> float");
        assertFalse(ClassUtils.isAssignable(Boolean.TYPE, Double.TYPE), "boolean -> double");
        assertTrue(ClassUtils.isAssignable(Boolean.TYPE, Boolean.TYPE), "boolean -> boolean");
    }

    @Test
    public void test_isInnerClass_Class() {
        assertTrue(ClassUtils.isInnerClass(Inner.class));
        assertTrue(ClassUtils.isInnerClass(Map.Entry.class));
        assertTrue(ClassUtils.isInnerClass(new Cloneable() {
            // empty
        }.getClass()));
        assertFalse(ClassUtils.isInnerClass(this.getClass()));
        assertFalse(ClassUtils.isInnerClass(String.class));
        assertFalse(ClassUtils.isInnerClass(null));
    }

    @Test
    public void testComparable() {
        final TreeMap<Class<?>, String> map = new TreeMap<>(ClassUtils.comparator());
        map.put(String.class, "lastEntry");
        map.toString();
        map.put(Character.class, "firstEntry");
        map.toString();
        assertEquals("firstEntry", map.firstEntry().getValue());
        assertEquals(Character.class, map.firstEntry().getKey());
        //
        assertEquals("lastEntry", map.lastEntry().getValue());
        assertEquals(String.class, map.lastEntry().getKey());
        //
        map.put(null, "null");
        map.toString();
        assertEquals("null", map.get(null));
    }

    @Test
    public void testConstructor() {
        assertNotNull(new ClassUtils());
        final Constructor<?>[] cons = ClassUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertTrue(Modifier.isPublic(cons[0].getModifiers()));
        assertTrue(Modifier.isPublic(ClassUtils.class.getModifiers()));
        assertFalse(Modifier.isFinal(ClassUtils.class.getModifiers()));
    }

    @Test
    public void testGetClassByNormalNameArrays() throws ClassNotFoundException {
        assertEquals(int[].class, ClassUtils.getClass("int[]"));
        assertEquals(long[].class, ClassUtils.getClass("long[]"));
        assertEquals(short[].class, ClassUtils.getClass("short[]"));
        assertEquals(byte[].class, ClassUtils.getClass("byte[]"));
        assertEquals(char[].class, ClassUtils.getClass("char[]"));
        assertEquals(float[].class, ClassUtils.getClass("float[]"));
        assertEquals(double[].class, ClassUtils.getClass("double[]"));
        assertEquals(boolean[].class, ClassUtils.getClass("boolean[]"));
        assertEquals(String[].class, ClassUtils.getClass("java.lang.String[]"));
        assertEquals(java.util.Map.Entry[].class, ClassUtils.getClass("java.util.Map.Entry[]"));
        assertEquals(java.util.Map.Entry[].class, ClassUtils.getClass("java.util.Map$Entry[]"));
        assertEquals(java.util.Map.Entry[].class, ClassUtils.getClass("[Ljava.util.Map.Entry;"));
        assertEquals(java.util.Map.Entry[].class, ClassUtils.getClass("[Ljava.util.Map$Entry;"));
    }

    @Test
    public void testGetClassByNormalNameArrays2D() throws ClassNotFoundException {
        assertEquals(int[][].class, ClassUtils.getClass("int[][]"));
        assertEquals(long[][].class, ClassUtils.getClass("long[][]"));
        assertEquals(short[][].class, ClassUtils.getClass("short[][]"));
        assertEquals(byte[][].class, ClassUtils.getClass("byte[][]"));
        assertEquals(char[][].class, ClassUtils.getClass("char[][]"));
        assertEquals(float[][].class, ClassUtils.getClass("float[][]"));
        assertEquals(double[][].class, ClassUtils.getClass("double[][]"));
        assertEquals(boolean[][].class, ClassUtils.getClass("boolean[][]"));
        assertEquals(String[][].class, ClassUtils.getClass("java.lang.String[][]"));
    }

    @Test
    public void testGetClassClassNotFound() throws Exception {
        assertGetClassThrowsClassNotFound("bool");
        assertGetClassThrowsClassNotFound("bool[]");
        assertGetClassThrowsClassNotFound("integer[]");
    }

    @Test
    public void testGetClassInvalidArguments() throws Exception {
        assertGetClassThrowsNullPointerException(null);
        assertGetClassThrowsClassNotFound("[][][]");
        assertGetClassThrowsClassNotFound("[[]");
        assertGetClassThrowsClassNotFound("[");
        assertGetClassThrowsClassNotFound("java.lang.String][");
        assertGetClassThrowsClassNotFound(".hello.world");
        assertGetClassThrowsClassNotFound("hello..world");
    }

    @Test
    public void testGetClassRawPrimitives() throws ClassNotFoundException {
        assertEquals(int.class, ClassUtils.getClass("int"));
        assertEquals(long.class, ClassUtils.getClass("long"));
        assertEquals(short.class, ClassUtils.getClass("short"));
        assertEquals(byte.class, ClassUtils.getClass("byte"));
        assertEquals(char.class, ClassUtils.getClass("char"));
        assertEquals(float.class, ClassUtils.getClass("float"));
        assertEquals(double.class, ClassUtils.getClass("double"));
        assertEquals(boolean.class, ClassUtils.getClass("boolean"));
        assertEquals(void.class, ClassUtils.getClass("void"));
    }

    @Test
    public void testGetClassWithArrayClasses() throws Exception {
        assertGetClassReturnsClass(String[].class);
        assertGetClassReturnsClass(int[].class);
        assertGetClassReturnsClass(long[].class);
        assertGetClassReturnsClass(short[].class);
        assertGetClassReturnsClass(byte[].class);
        assertGetClassReturnsClass(char[].class);
        assertGetClassReturnsClass(float[].class);
        assertGetClassReturnsClass(double[].class);
        assertGetClassReturnsClass(boolean[].class);
    }

    @Test
    public void testGetClassWithArrayClasses2D() throws Exception {
        assertGetClassReturnsClass(String[][].class);
        assertGetClassReturnsClass(int[][].class);
        assertGetClassReturnsClass(long[][].class);
        assertGetClassReturnsClass(short[][].class);
        assertGetClassReturnsClass(byte[][].class);
        assertGetClassReturnsClass(char[][].class);
        assertGetClassReturnsClass(float[][].class);
        assertGetClassReturnsClass(double[][].class);
        assertGetClassReturnsClass(boolean[][].class);
    }

    @Test
    public void testGetComponentType() {
        final CX[] newArray = {};
        @SuppressWarnings("unchecked")
        final Class<CX[]> classCxArray = (Class<CX[]>) newArray.getClass();
        // No type-cast required.
        final Class<CX> componentType = ClassUtils.getComponentType(classCxArray);
        assertEquals(CX.class, componentType);
        assertNull(ClassUtils.getComponentType(null));
    }

    @Test
    public void testGetInnerClass() throws ClassNotFoundException {
        assertEquals(Inner.DeeplyNested.class, ClassUtils.getClass("org.apache.commons.lang3.ClassUtilsTest.Inner.DeeplyNested"));
        assertEquals(Inner.DeeplyNested.class, ClassUtils.getClass("org.apache.commons.lang3.ClassUtilsTest.Inner$DeeplyNested"));
        assertEquals(Inner.DeeplyNested.class, ClassUtils.getClass("org.apache.commons.lang3.ClassUtilsTest$Inner$DeeplyNested"));
        assertEquals(Inner.DeeplyNested.class, ClassUtils.getClass("org.apache.commons.lang3.ClassUtilsTest$Inner.DeeplyNested"));
        //
        assertEquals(Inner.DeeplyNested.class, ClassUtils.getClass("org.apache.commons.lang3.ClassUtilsTest.Inner.DeeplyNested", true));
        assertEquals(Inner.DeeplyNested.class, ClassUtils.getClass("org.apache.commons.lang3.ClassUtilsTest.Inner$DeeplyNested", true));
        assertEquals(Inner.DeeplyNested.class, ClassUtils.getClass("org.apache.commons.lang3.ClassUtilsTest$Inner$DeeplyNested", true));
        assertEquals(Inner.DeeplyNested.class, ClassUtils.getClass("org.apache.commons.lang3.ClassUtilsTest$Inner.DeeplyNested", true));
        //
        final ClassLoader classLoader = Inner.DeeplyNested.class.getClassLoader();
        assertEquals(Inner.DeeplyNested.class, ClassUtils.getClass(classLoader, "org.apache.commons.lang3.ClassUtilsTest.Inner.DeeplyNested"));
        assertEquals(Inner.DeeplyNested.class, ClassUtils.getClass(classLoader, "org.apache.commons.lang3.ClassUtilsTest.Inner$DeeplyNested"));
        assertEquals(Inner.DeeplyNested.class, ClassUtils.getClass(classLoader, "org.apache.commons.lang3.ClassUtilsTest$Inner$DeeplyNested"));
        assertEquals(Inner.DeeplyNested.class, ClassUtils.getClass(classLoader, "org.apache.commons.lang3.ClassUtilsTest$Inner.DeeplyNested"));
        //
    }

    @Test
    public void testGetPublicMethod() throws Exception {
        // Tests with Collections$UnmodifiableSet
        final Set<?> set = Collections.unmodifiableSet(new HashSet<>());
        final Method isEmptyMethod = ClassUtils.getPublicMethod(set.getClass(), "isEmpty");
        assertTrue(Modifier.isPublic(isEmptyMethod.getDeclaringClass().getModifiers()));
        assertTrue((Boolean) isEmptyMethod.invoke(set));

        // Tests with a public Class
        final Method toStringMethod = ClassUtils.getPublicMethod(Object.class, "toString");
        assertEquals(Object.class.getMethod("toString"), toStringMethod);
    }

    @Test
    public void testHierarchyExcludingInterfaces() {
        final Iterator<Class<?>> iter = ClassUtils.hierarchy(StringParameterizedChild.class).iterator();
        assertEquals(StringParameterizedChild.class, iter.next());
        assertEquals(GenericParent.class, iter.next());
        assertEquals(Object.class, iter.next());
        assertFalse(iter.hasNext());
    }

    @Test
    public void testHierarchyIncludingInterfaces() {
        final Iterator<Class<?>> iter = ClassUtils.hierarchy(StringParameterizedChild.class, Interfaces.INCLUDE).iterator();
        assertEquals(StringParameterizedChild.class, iter.next());
        assertEquals(GenericParent.class, iter.next());
        assertEquals(GenericConsumer.class, iter.next());
        assertEquals(Object.class, iter.next());
        assertFalse(iter.hasNext());
    }

    @Test
    public void testIsPrimitiveOrWrapper() {

        // test primitive wrapper classes
        assertTrue(ClassUtils.isPrimitiveOrWrapper(Boolean.class), "Boolean.class");
        assertTrue(ClassUtils.isPrimitiveOrWrapper(Byte.class), "Byte.class");
        assertTrue(ClassUtils.isPrimitiveOrWrapper(Character.class), "Character.class");
        assertTrue(ClassUtils.isPrimitiveOrWrapper(Short.class), "Short.class");
        assertTrue(ClassUtils.isPrimitiveOrWrapper(Integer.class), "Integer.class");
        assertTrue(ClassUtils.isPrimitiveOrWrapper(Long.class), "Long.class");
        assertTrue(ClassUtils.isPrimitiveOrWrapper(Double.class), "Double.class");
        assertTrue(ClassUtils.isPrimitiveOrWrapper(Float.class), "Float.class");

        // test primitive classes
        assertTrue(ClassUtils.isPrimitiveOrWrapper(Boolean.TYPE), "boolean");
        assertTrue(ClassUtils.isPrimitiveOrWrapper(Byte.TYPE), "byte");
        assertTrue(ClassUtils.isPrimitiveOrWrapper(Character.TYPE), "char");
        assertTrue(ClassUtils.isPrimitiveOrWrapper(Short.TYPE), "short");
        assertTrue(ClassUtils.isPrimitiveOrWrapper(Integer.TYPE), "int");
        assertTrue(ClassUtils.isPrimitiveOrWrapper(Long.TYPE), "long");
        assertTrue(ClassUtils.isPrimitiveOrWrapper(Double.TYPE), "double");
        assertTrue(ClassUtils.isPrimitiveOrWrapper(Float.TYPE), "float");
        assertTrue(ClassUtils.isPrimitiveOrWrapper(Void.TYPE), "Void.TYPE");

        // others
        assertFalse(ClassUtils.isPrimitiveOrWrapper(null), "null");
        assertFalse(ClassUtils.isPrimitiveOrWrapper(Void.class), "Void.class");
        assertFalse(ClassUtils.isPrimitiveOrWrapper(String.class), "String.class");
        assertFalse(ClassUtils.isPrimitiveOrWrapper(this.getClass()), "this.getClass()");
    }

    @Test
    public void testIsPrimitiveWrapper() {

        // test primitive wrapper classes
        assertTrue(ClassUtils.isPrimitiveWrapper(Boolean.class), "Boolean.class");
        assertTrue(ClassUtils.isPrimitiveWrapper(Byte.class), "Byte.class");
        assertTrue(ClassUtils.isPrimitiveWrapper(Character.class), "Character.class");
        assertTrue(ClassUtils.isPrimitiveWrapper(Short.class), "Short.class");
        assertTrue(ClassUtils.isPrimitiveWrapper(Integer.class), "Integer.class");
        assertTrue(ClassUtils.isPrimitiveWrapper(Long.class), "Long.class");
        assertTrue(ClassUtils.isPrimitiveWrapper(Double.class), "Double.class");
        assertTrue(ClassUtils.isPrimitiveWrapper(Float.class), "Float.class");

        // test primitive classes
        assertFalse(ClassUtils.isPrimitiveWrapper(Boolean.TYPE), "boolean");
        assertFalse(ClassUtils.isPrimitiveWrapper(Byte.TYPE), "byte");
        assertFalse(ClassUtils.isPrimitiveWrapper(Character.TYPE), "char");
        assertFalse(ClassUtils.isPrimitiveWrapper(Short.TYPE), "short");
        assertFalse(ClassUtils.isPrimitiveWrapper(Integer.TYPE), "int");
        assertFalse(ClassUtils.isPrimitiveWrapper(Long.TYPE), "long");
        assertFalse(ClassUtils.isPrimitiveWrapper(Double.TYPE), "double");
        assertFalse(ClassUtils.isPrimitiveWrapper(Float.TYPE), "float");

        // others
        assertFalse(ClassUtils.isPrimitiveWrapper(null), "null");
        assertFalse(ClassUtils.isPrimitiveWrapper(Void.class), "Void.class");
        assertFalse(ClassUtils.isPrimitiveWrapper(Void.TYPE), "Void.TYPE");
        assertFalse(ClassUtils.isPrimitiveWrapper(String.class), "String.class");
        assertFalse(ClassUtils.isPrimitiveWrapper(this.getClass()), "this.getClass()");
    }

    @Test
    public void testPrimitivesToWrappers() {
        // test null
//        assertNull("null -> null", ClassUtils.primitivesToWrappers(null)); // generates warning
        assertNull(ClassUtils.primitivesToWrappers((Class<?>[]) null), "null -> null"); // equivalent cast to avoid warning
        // Other possible casts for null
        assertArrayEquals(ArrayUtils.EMPTY_CLASS_ARRAY, ClassUtils.primitivesToWrappers(), "empty -> empty");
        final Class<?>[] castNull = ClassUtils.primitivesToWrappers((Class<?>) null); // == new Class<?>[]{null}
        assertArrayEquals(new Class<?>[] {null}, castNull, "(Class<?>) null -> [null]");
        // test empty array is returned unchanged
        assertArrayEquals(ArrayUtils.EMPTY_CLASS_ARRAY, ClassUtils.primitivesToWrappers(ArrayUtils.EMPTY_CLASS_ARRAY), "empty -> empty");

        // test an array of various classes
        final Class<?>[] primitives = new Class[] {Boolean.TYPE, Byte.TYPE, Character.TYPE, Short.TYPE, Integer.TYPE, Long.TYPE, Double.TYPE, Float.TYPE,
            String.class, ClassUtils.class};
        final Class<?>[] wrappers = ClassUtils.primitivesToWrappers(primitives);

        for (int i = 0; i < primitives.length; i++) {
            // test each returned wrapper
            final Class<?> primitive = primitives[i];
            final Class<?> expectedWrapper = ClassUtils.primitiveToWrapper(primitive);

            assertEquals(expectedWrapper, wrappers[i], primitive + " -> " + expectedWrapper);
        }

        // test an array of no primitive classes
        final Class<?>[] noPrimitives = new Class[] {String.class, ClassUtils.class, Void.TYPE};
        // This used to return the exact same array, but no longer does.
        assertNotSame(noPrimitives, ClassUtils.primitivesToWrappers(noPrimitives), "unmodified");
    }

    @Test
    public void testPrimitiveToWrapper() {

        // test primitive classes
        assertEquals(Boolean.class, ClassUtils.primitiveToWrapper(Boolean.TYPE), "boolean -> Boolean.class");
        assertEquals(Byte.class, ClassUtils.primitiveToWrapper(Byte.TYPE), "byte -> Byte.class");
        assertEquals(Character.class, ClassUtils.primitiveToWrapper(Character.TYPE), "char -> Character.class");
        assertEquals(Short.class, ClassUtils.primitiveToWrapper(Short.TYPE), "short -> Short.class");
        assertEquals(Integer.class, ClassUtils.primitiveToWrapper(Integer.TYPE), "int -> Integer.class");
        assertEquals(Long.class, ClassUtils.primitiveToWrapper(Long.TYPE), "long -> Long.class");
        assertEquals(Double.class, ClassUtils.primitiveToWrapper(Double.TYPE), "double -> Double.class");
        assertEquals(Float.class, ClassUtils.primitiveToWrapper(Float.TYPE), "float -> Float.class");

        // test a few other classes
        assertEquals(String.class, ClassUtils.primitiveToWrapper(String.class), "String.class -> String.class");
        assertEquals(ClassUtils.class, ClassUtils.primitiveToWrapper(ClassUtils.class), "ClassUtils.class -> ClassUtils.class");
        assertEquals(Void.TYPE, ClassUtils.primitiveToWrapper(Void.TYPE), "Void.TYPE -> Void.TYPE");

        // test null
        assertNull(ClassUtils.primitiveToWrapper(null), "null -> null");
    }

    // Show the Java bug: https://bugs.java.com/bugdatabase/view_bug.do?bug_id=4071957
    // We may have to delete this if a JDK fixes the bug.
    @Test
    public void testShowJavaBug() throws Exception {
        // Tests with Collections$UnmodifiableSet
        final Set<?> set = Collections.unmodifiableSet(new HashSet<>());
        final Method isEmptyMethod = set.getClass().getMethod("isEmpty");
        assertThrows(IllegalAccessException.class, () -> isEmptyMethod.invoke(set));
    }

    @Test
    public void testToClass_object() {
//        assertNull(ClassUtils.toClass(null)); // generates warning
        assertNull(ClassUtils.toClass((Object[]) null)); // equivalent explicit cast

        // Additional varargs tests
        assertArrayEquals(ArrayUtils.EMPTY_CLASS_ARRAY, ClassUtils.toClass(), "empty -> empty");
        final Class<?>[] castNull = ClassUtils.toClass((Object) null); // == new Object[]{null}
        assertArrayEquals(new Object[] {null}, castNull, "(Object) null -> [null]");

        assertSame(ArrayUtils.EMPTY_CLASS_ARRAY, ClassUtils.toClass(ArrayUtils.EMPTY_OBJECT_ARRAY));

        assertArrayEquals(new Class[] {String.class, Integer.class, Double.class}, ClassUtils.toClass("Test", Integer.valueOf(1), Double.valueOf(99d)));

        assertArrayEquals(new Class[] {String.class, null, Double.class}, ClassUtils.toClass("Test", null, Double.valueOf(99d)));
    }

    @Test
    public void testWithInterleavingWhitespace() throws ClassNotFoundException {
        assertEquals(int[].class, ClassUtils.getClass(" int [ ] "));
        assertEquals(long[].class, ClassUtils.getClass("\rlong\t[\n]\r"));
        assertEquals(short[].class, ClassUtils.getClass("\tshort                \t\t[]"));
        assertEquals(byte[].class, ClassUtils.getClass("byte[\t\t\n\r]   "));
    }

    @Test
    public void testWrappersToPrimitives() {
        // an array with classes to test
        final Class<?>[] classes = {Boolean.class, Byte.class, Character.class, Short.class, Integer.class, Long.class, Float.class, Double.class, String.class,
            ClassUtils.class, null};

        final Class<?>[] primitives = ClassUtils.wrappersToPrimitives(classes);
        // now test the result
        assertEquals(classes.length, primitives.length, "Wrong length of result array");
        for (int i = 0; i < classes.length; i++) {
            final Class<?> expectedPrimitive = ClassUtils.wrapperToPrimitive(classes[i]);
            assertEquals(expectedPrimitive, primitives[i], classes[i] + " -> " + expectedPrimitive);
        }
    }

    @Test
    public void testWrappersToPrimitivesEmpty() {
        final Class<?>[] empty = new Class[0];
        assertArrayEquals(empty, ClassUtils.wrappersToPrimitives(empty), "Wrong result for empty input");
    }

    @Test
    public void testWrappersToPrimitivesNull() {
//        assertNull("Wrong result for null input", ClassUtils.wrappersToPrimitives(null)); // generates warning
        assertNull(ClassUtils.wrappersToPrimitives((Class<?>[]) null), "Wrong result for null input"); // equivalent cast
        // Other possible casts for null
        assertArrayEquals(ArrayUtils.EMPTY_CLASS_ARRAY, ClassUtils.wrappersToPrimitives(), "empty -> empty");
        final Class<?>[] castNull = ClassUtils.wrappersToPrimitives((Class<?>) null); // == new Class<?>[]{null}
        assertArrayEquals(new Class<?>[] {null}, castNull, "(Class<?>) null -> [null]");
    }

    @Test
    public void testWrapperToPrimitive() {
        // an array with classes to convert
        final Class<?>[] primitives = {Boolean.TYPE, Byte.TYPE, Character.TYPE, Short.TYPE, Integer.TYPE, Long.TYPE, Float.TYPE, Double.TYPE};
        for (final Class<?> primitive : primitives) {
            final Class<?> wrapperCls = ClassUtils.primitiveToWrapper(primitive);
            assertFalse(wrapperCls.isPrimitive(), "Still primitive");
            assertEquals(primitive, ClassUtils.wrapperToPrimitive(wrapperCls), wrapperCls + " -> " + primitive);
        }
    }

    @Test
    public void testWrapperToPrimitiveNoWrapper() {
        assertNull(ClassUtils.wrapperToPrimitive(String.class), "Wrong result for non wrapper class");
    }

    @Test
    public void testWrapperToPrimitiveNull() {
        assertNull(ClassUtils.wrapperToPrimitive(null), "Wrong result for null class");
    }
}
