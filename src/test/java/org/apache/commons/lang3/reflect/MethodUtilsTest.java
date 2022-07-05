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
package org.apache.commons.lang3.reflect;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItemInArray;
import static org.hamcrest.Matchers.hasItems;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.awt.Color;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.ClassUtils;
import org.apache.commons.lang3.ClassUtils.Interfaces;
import org.apache.commons.lang3.math.NumberUtils;
import org.apache.commons.lang3.mutable.Mutable;
import org.apache.commons.lang3.mutable.MutableObject;
import org.apache.commons.lang3.reflect.testbed.Annotated;
import org.apache.commons.lang3.reflect.testbed.GenericConsumer;
import org.apache.commons.lang3.reflect.testbed.GenericParent;
import org.apache.commons.lang3.reflect.testbed.PublicChild;
import org.apache.commons.lang3.reflect.testbed.StringParameterizedChild;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * Unit tests MethodUtils
 */
public class MethodUtilsTest extends AbstractLangTest {

    private interface PrivateInterface {
    }

    static class TestBeanWithInterfaces implements PrivateInterface {
        public String foo() {
            return "foo()";
        }
    }

    public static class TestBean {

        public static String bar() {
            return "bar()";
        }

        public static String bar(final int i) {
            return "bar(int)";
        }

        public static String bar(final Integer i) {
            return "bar(Integer)";
        }

        public static String bar(final double d) {
            return "bar(double)";
        }

        public static String bar(final String s) {
            return "bar(String)";
        }

        public static String bar(final Object o) {
            return "bar(Object)";
        }

        public static String bar(final String... s) {
            return "bar(String...)";
        }

        public static String bar(final long... s) {
            return "bar(long...)";
        }

        public static String bar(final Integer i, final String... s) {
            return "bar(int, String...)";
        }

        public static void oneParameterStatic(final String s) {
            // empty
        }

        @SuppressWarnings("unused")
        private void privateStuff() {
        }

        @SuppressWarnings("unused")
        private String privateStringStuff() {
            return "privateStringStuff()";
        }

        @SuppressWarnings("unused")
        private String privateStringStuff(final int i) {
            return "privateStringStuff(int)";
        }

        @SuppressWarnings("unused")
        private String privateStringStuff(final Integer i) {
            return "privateStringStuff(Integer)";
        }

        @SuppressWarnings("unused")
        private String privateStringStuff(final double d) {
            return "privateStringStuff(double)";
        }

        @SuppressWarnings("unused")
        private String privateStringStuff(final String s) {
            return "privateStringStuff(String)";
        }

        @SuppressWarnings("unused")
        private String privateStringStuff(final Object s) {
            return "privateStringStuff(Object)";
        }

        public String foo() {
            return "foo()";
        }

        public String foo(final int i) {
            return "foo(int)";
        }

        public String foo(final Integer i) {
            return "foo(Integer)";
        }

        public String foo(final double d) {
            return "foo(double)";
        }

        public String foo(final long l) {
            return "foo(long)";
        }

        public String foo(final String s) {
            return "foo(String)";
        }

        public String foo(final Object o) {
            return "foo(Object)";
        }

        public String foo(final String... s) {
            return "foo(String...)";
        }

        public String foo(final long... l) {
            return "foo(long...)";
        }

        public String foo(final Integer i, final String... s) {
            return "foo(int, String...)";
        }

        public void oneParameter(final String s) {
            // empty
        }

        public String foo(final Object... s) {
            return "foo(Object...)";
        }

        public int[] unboxing(final int... values) {
            return values;
        }

        // This method is overloaded for the wrapper class for every primitive type, plus the common supertypes
        // Number and Object. This is an acid test since it easily leads to ambiguous methods.
        public static String varOverload(final Byte... args) {
            return "Byte...";
        }

        public static String varOverload(final Character... args) {
            return "Character...";
        }

        public static String varOverload(final Short... args) {
            return "Short...";
        }

        public static String varOverload(final Boolean... args) {
            return "Boolean...";
        }

        public static String varOverload(final Float... args) {
            return "Float...";
        }

        public static String varOverload(final Double... args) {
            return "Double...";
        }

        public static String varOverload(final Integer... args) {
            return "Integer...";
        }

        public static String varOverload(final Long... args) {
            return "Long...";
        }

        public static String varOverload(final Number... args) {
            return "Number...";
        }

        public static String varOverload(final Object... args) {
            return "Object...";
        }

        public static String varOverload(final String... args) {
            return "String...";
        }

        // This method is overloaded for the wrapper class for every numeric primitive type, plus the common
        // supertype Number
        public static String numOverload(final Byte... args) {
            return "Byte...";
        }

        public static String numOverload(final Short... args) {
            return "Short...";
        }

        public static String numOverload(final Float... args) {
            return "Float...";
        }

        public static String numOverload(final Double... args) {
            return "Double...";
        }

        public static String numOverload(final Integer... args) {
            return "Integer...";
        }

        public static String numOverload(final Long... args) {
            return "Long...";
        }

        public static String numOverload(final Number... args) {
            return "Number...";
        }

        // These varOverloadEcho and varOverloadEchoStatic methods are designed to verify that
        // not only is the correct overloaded variant invoked, but that the varags arguments
        // are also delivered correctly to the method.
        public ImmutablePair<String, Object[]> varOverloadEcho(final String... args) {
            return new ImmutablePair<>("String...", args);
        }

        public ImmutablePair<String, Object[]> varOverloadEcho(final Number... args) {
            return new ImmutablePair<>("Number...", args);
        }

        public static ImmutablePair<String, Object[]> varOverloadEchoStatic(final String... args) {
            return new ImmutablePair<>("String...", args);
        }

        public static ImmutablePair<String, Object[]> varOverloadEchoStatic(final Number... args) {
            return new ImmutablePair<>("Number...", args);
        }

        static void verify(final ImmutablePair<String, Object[]> a, final ImmutablePair<String, Object[]> b) {
            assertEquals(a.getLeft(), b.getLeft());
            assertArrayEquals(a.getRight(), b.getRight());
        }

        static void verify(final ImmutablePair<String, Object[]> a, final Object obj) {
            @SuppressWarnings("unchecked")
            final ImmutablePair<String, Object[]> pair = (ImmutablePair<String, Object[]>) obj;
            verify(a, pair);
        }

    }

    private static class TestMutable implements Mutable<Object> {
        @Override
        public Object getValue() {
            return null;
        }

        @Override
        public void setValue(final Object value) {
        }
    }

    private TestBean testBean;
    private final Map<Class<?>, Class<?>[]> classCache = new HashMap<>();

    @BeforeEach
    public void setUp() {
        testBean = new TestBean();
        classCache.clear();
    }

    @Test
    public void testConstructor() throws Exception {
        assertNotNull(MethodUtils.class.newInstance());
    }

    @Test
    public void verifyJavaVarargsOverloadingResolution() {
        // This code is not a test of MethodUtils.
        // Rather it makes explicit the behavior of the Java specification for
        // various cases of overload resolution.
        assertEquals("Byte...", TestBean.varOverload((byte) 1, (byte) 2));
        assertEquals("Short...", TestBean.varOverload((short) 1, (short) 2));
        assertEquals("Integer...", TestBean.varOverload(1, 2));
        assertEquals("Long...", TestBean.varOverload(1L, 2L));
        assertEquals("Float...", TestBean.varOverload(1f, 2f));
        assertEquals("Double...", TestBean.varOverload(1d, 2d));
        assertEquals("Character...", TestBean.varOverload('a', 'b'));
        assertEquals("String...", TestBean.varOverload("a", "b"));
        assertEquals("Boolean...", TestBean.varOverload(true, false));

        assertEquals("Object...", TestBean.varOverload(1, "s"));
        assertEquals("Object...", TestBean.varOverload(1, true));
        assertEquals("Object...", TestBean.varOverload(1.1, true));
        assertEquals("Object...", TestBean.varOverload('c', true));
        assertEquals("Number...", TestBean.varOverload(1, 1.1));
        assertEquals("Number...", TestBean.varOverload(1, 1L));
        assertEquals("Number...", TestBean.varOverload(1d, 1f));
        assertEquals("Number...", TestBean.varOverload((short) 1, (byte) 1));
        assertEquals("Object...", TestBean.varOverload(1, 'c'));
        assertEquals("Object...", TestBean.varOverload('c', "s"));
    }

    @Test
    public void testInvokeJavaVarargsOverloadingResolution() throws Exception {
        assertEquals("Byte...", MethodUtils.invokeStaticMethod(TestBean.class,
                "varOverload", (byte) 1, (byte) 2));
        assertEquals("Short...", MethodUtils.invokeStaticMethod(TestBean.class,
                "varOverload", (short) 1, (short) 2));
        assertEquals("Integer...", MethodUtils.invokeStaticMethod(TestBean.class,
                "varOverload", 1, 2));
        assertEquals("Long...", MethodUtils.invokeStaticMethod(TestBean.class,
                "varOverload", 1L, 2L));
        assertEquals("Float...", MethodUtils.invokeStaticMethod(TestBean.class,
                "varOverload", 1f, 2f));
        assertEquals("Double...", MethodUtils.invokeStaticMethod(TestBean.class,
                "varOverload", 1d, 2d));
        assertEquals("Character...", MethodUtils.invokeStaticMethod(TestBean.class,
                "varOverload", 'a', 'b'));
        assertEquals("String...", MethodUtils.invokeStaticMethod(TestBean.class,
                "varOverload", "a", "b"));
        assertEquals("Boolean...", MethodUtils.invokeStaticMethod(TestBean.class,
                "varOverload", true, false));

        assertEquals("Object...", MethodUtils.invokeStaticMethod(TestBean.class,
                "varOverload", 1, "s"));
        assertEquals("Object...", MethodUtils.invokeStaticMethod(TestBean.class,
                "varOverload", 1, true));
        assertEquals("Object...", MethodUtils.invokeStaticMethod(TestBean.class,
                "varOverload", 1.1, true));
        assertEquals("Object...", MethodUtils.invokeStaticMethod(TestBean.class,
                "varOverload", 'c', true));
        assertEquals("Number...", MethodUtils.invokeStaticMethod(TestBean.class,
                "varOverload", 1, 1.1));
        assertEquals("Number...", MethodUtils.invokeStaticMethod(TestBean.class,
                "varOverload", 1, 1L));
        assertEquals("Number...", MethodUtils.invokeStaticMethod(TestBean.class,
                "varOverload", 1d, 1f));
        assertEquals("Number...", MethodUtils.invokeStaticMethod(TestBean.class,
                "varOverload", (short) 1, (byte) 1));
        assertEquals("Object...", MethodUtils.invokeStaticMethod(TestBean.class,
                "varOverload", 1, 'c'));
        assertEquals("Object...", MethodUtils.invokeStaticMethod(TestBean.class,
                "varOverload", 'c', "s"));

        assertEquals("Object...", MethodUtils.invokeStaticMethod(TestBean.class, "varOverload",
                (Object[]) ArrayUtils.EMPTY_CLASS_ARRAY));
        assertEquals("Number...", MethodUtils.invokeStaticMethod(TestBean.class, "numOverload",
                (Object[]) ArrayUtils.EMPTY_CLASS_ARRAY));
    }

    @Test
    public void testInvokeMethod() throws Exception {
        assertEquals("foo()", MethodUtils.invokeMethod(testBean, "foo",
                (Object[]) ArrayUtils.EMPTY_CLASS_ARRAY));
        assertEquals("foo()", MethodUtils.invokeMethod(testBean, "foo"));
        assertEquals("foo()", MethodUtils.invokeMethod(testBean, "foo",
                (Object[]) null));
        assertEquals("foo()", MethodUtils.invokeMethod(testBean, "foo",
                null, null));
        assertEquals("foo(String)", MethodUtils.invokeMethod(testBean, "foo",
                ""));
        assertEquals("foo(Object)", MethodUtils.invokeMethod(testBean, "foo",
                new Object()));
        assertEquals("foo(Object)", MethodUtils.invokeMethod(testBean, "foo",
                Boolean.TRUE));
        assertEquals("foo(Integer)", MethodUtils.invokeMethod(testBean, "foo",
                NumberUtils.INTEGER_ONE));
        assertEquals("foo(int)", MethodUtils.invokeMethod(testBean, "foo",
                NumberUtils.BYTE_ONE));
        assertEquals("foo(long)", MethodUtils.invokeMethod(testBean, "foo",
                NumberUtils.LONG_ONE));
        assertEquals("foo(double)", MethodUtils.invokeMethod(testBean, "foo",
                NumberUtils.DOUBLE_ONE));
        assertEquals("foo(String...)", MethodUtils.invokeMethod(testBean, "foo",
                "a", "b", "c"));
        assertEquals("foo(String...)", MethodUtils.invokeMethod(testBean, "foo",
                "a", "b", "c"));
        assertEquals("foo(int, String...)", MethodUtils.invokeMethod(testBean, "foo",
                5, "a", "b", "c"));
        assertEquals("foo(long...)", MethodUtils.invokeMethod(testBean, "foo",
                1L, 2L));

        assertThrows(NoSuchMethodException.class, () -> MethodUtils.invokeMethod(testBean, "foo", 1, 2));

        TestBean.verify(new ImmutablePair<>("String...", new String[]{"x", "y"}),
                MethodUtils.invokeMethod(testBean, "varOverloadEcho", "x", "y"));
        TestBean.verify(new ImmutablePair<>("Number...", new Number[]{17, 23, 42}),
                MethodUtils.invokeMethod(testBean, "varOverloadEcho", 17, 23, 42));
        TestBean.verify(new ImmutablePair<>("String...", new String[]{"x", "y"}),
                MethodUtils.invokeMethod(testBean, "varOverloadEcho", "x", "y"));
        TestBean.verify(new ImmutablePair<>("Number...", new Number[]{17, 23, 42}),
                MethodUtils.invokeMethod(testBean, "varOverloadEcho", 17, 23, 42));
    }

    @Test
    public void testInvokeMethod_VarArgsWithNullValues() throws Exception {
        assertEquals("String...", MethodUtils.invokeMethod(testBean, "varOverload",
                "a", null, "c"));
        assertEquals("String...", MethodUtils.invokeMethod(testBean, "varOverload",
                                                                "a", "b", null));
    }

    @Test
    public void testInvokeMethod_VarArgsNotUniqueResolvable() throws Exception {
      assertEquals("Boolean...", MethodUtils.invokeMethod(testBean, "varOverload",
                                                         new Object[] {null}));
      assertEquals("Object...", MethodUtils.invokeMethod(testBean, "varOverload",
                                                         (Object[]) null));
    }

    @Test
    public void testInvokeExactMethod() throws Exception {
        assertEquals("foo()", MethodUtils.invokeExactMethod(testBean, "foo",
                (Object[]) ArrayUtils.EMPTY_CLASS_ARRAY));
        assertEquals("foo()", MethodUtils.invokeExactMethod(testBean, "foo"));
        assertEquals("foo()", MethodUtils.invokeExactMethod(testBean, "foo",
                (Object[]) null));
        assertEquals("foo()", MethodUtils.invokeExactMethod(testBean, "foo",
                null, null));
        assertEquals("foo(String)", MethodUtils.invokeExactMethod(testBean,
                "foo", ""));
        assertEquals("foo(Object)", MethodUtils.invokeExactMethod(testBean,
                "foo", new Object()));
        assertEquals("foo(Integer)", MethodUtils.invokeExactMethod(testBean,
                "foo", NumberUtils.INTEGER_ONE));
        assertEquals("foo(double)", MethodUtils.invokeExactMethod(testBean,
                "foo", new Object[]{NumberUtils.DOUBLE_ONE},
                new Class[]{Double.TYPE}));

        assertThrows(
                NoSuchMethodException.class,
                () -> MethodUtils.invokeExactMethod(testBean, "foo", NumberUtils.BYTE_ONE));

        assertThrows(
                NoSuchMethodException.class,
                () -> MethodUtils.invokeExactMethod(testBean, "foo", NumberUtils.LONG_ONE));
        assertThrows(NoSuchMethodException.class, () -> MethodUtils.invokeExactMethod(testBean, "foo", Boolean.TRUE));
    }

    @Test
    public void testInvokeStaticMethod() throws Exception {
        assertEquals("bar()", MethodUtils.invokeStaticMethod(TestBean.class,
                "bar", (Object[]) ArrayUtils.EMPTY_CLASS_ARRAY));
        assertEquals("bar()", MethodUtils.invokeStaticMethod(TestBean.class,
                "bar", (Object[]) null));
        assertEquals("bar()", MethodUtils.invokeStaticMethod(TestBean.class,
                "bar", null, null));
        assertEquals("bar(String)", MethodUtils.invokeStaticMethod(
                TestBean.class, "bar", ""));
        assertEquals("bar(Object)", MethodUtils.invokeStaticMethod(
                TestBean.class, "bar", new Object()));
        assertEquals("bar(Object)", MethodUtils.invokeStaticMethod(
                TestBean.class, "bar", Boolean.TRUE));
        assertEquals("bar(Integer)", MethodUtils.invokeStaticMethod(
                TestBean.class, "bar", NumberUtils.INTEGER_ONE));
        assertEquals("bar(int)", MethodUtils.invokeStaticMethod(TestBean.class,
                "bar", NumberUtils.BYTE_ONE));
        assertEquals("bar(double)", MethodUtils.invokeStaticMethod(
                TestBean.class, "bar", NumberUtils.DOUBLE_ONE));
        assertEquals("bar(String...)", MethodUtils.invokeStaticMethod(
                TestBean.class, "bar", "a", "b"));
        assertEquals("bar(long...)", MethodUtils.invokeStaticMethod(
                TestBean.class, "bar", 1L, 2L));
        assertEquals("bar(int, String...)", MethodUtils.invokeStaticMethod(
                TestBean.class, "bar", NumberUtils.INTEGER_ONE, "a", "b"));

        TestBean.verify(new ImmutablePair<>("String...", new String[]{"x", "y"}),
                MethodUtils.invokeStaticMethod(TestBean.class, "varOverloadEchoStatic", "x", "y"));
        TestBean.verify(new ImmutablePair<>("Number...", new Number[]{17, 23, 42}),
                MethodUtils.invokeStaticMethod(TestBean.class, "varOverloadEchoStatic", 17, 23, 42));
        TestBean.verify(new ImmutablePair<>("String...", new String[]{"x", "y"}),
                MethodUtils.invokeStaticMethod(TestBean.class, "varOverloadEchoStatic", "x", "y"));
        TestBean.verify(new ImmutablePair<>("Number...", new Number[]{17, 23, 42}),
                MethodUtils.invokeStaticMethod(TestBean.class, "varOverloadEchoStatic", 17, 23, 42));

        assertThrows(
                NoSuchMethodException.class, () -> MethodUtils.invokeStaticMethod(TestBean.class, "does_not_exist"));
    }

    @Test
    public void testInvokeExactStaticMethod() throws Exception {
        assertEquals("bar()", MethodUtils.invokeExactStaticMethod(TestBean.class,
                "bar", (Object[]) ArrayUtils.EMPTY_CLASS_ARRAY));
        assertEquals("bar()", MethodUtils.invokeExactStaticMethod(TestBean.class,
                "bar", (Object[]) null));
        assertEquals("bar()", MethodUtils.invokeExactStaticMethod(TestBean.class,
                "bar", null, null));
        assertEquals("bar(String)", MethodUtils.invokeExactStaticMethod(
                TestBean.class, "bar", ""));
        assertEquals("bar(Object)", MethodUtils.invokeExactStaticMethod(
                TestBean.class, "bar", new Object()));
        assertEquals("bar(Integer)", MethodUtils.invokeExactStaticMethod(
                TestBean.class, "bar", NumberUtils.INTEGER_ONE));
        assertEquals("bar(double)", MethodUtils.invokeExactStaticMethod(
                TestBean.class, "bar", new Object[]{NumberUtils.DOUBLE_ONE},
                new Class[]{Double.TYPE}));

        assertThrows(
                NoSuchMethodException.class,
                () -> MethodUtils.invokeExactStaticMethod(TestBean.class, "bar", NumberUtils.BYTE_ONE));
        assertThrows(
                NoSuchMethodException.class,
                () -> MethodUtils.invokeExactStaticMethod(TestBean.class, "bar", NumberUtils.LONG_ONE));
        assertThrows(
                NoSuchMethodException.class,
                () -> MethodUtils.invokeExactStaticMethod(TestBean.class, "bar", Boolean.TRUE));
    }

    @Test
    public void testGetAccessibleInterfaceMethod() throws Exception {
        final Class<?>[][] p = {ArrayUtils.EMPTY_CLASS_ARRAY, null};
        for (final Class<?>[] element : p) {
            final Method method = TestMutable.class.getMethod("getValue", element);
            final Method accessibleMethod = MethodUtils.getAccessibleMethod(method);
            assertNotSame(accessibleMethod, method);
            assertSame(Mutable.class, accessibleMethod.getDeclaringClass());
        }
    }

    @Test
    public void testGetAccessibleMethodPrivateInterface() throws Exception {
        final Method expected = TestBeanWithInterfaces.class.getMethod("foo");
        assertNotNull(expected);
        final Method actual = MethodUtils.getAccessibleMethod(TestBeanWithInterfaces.class, "foo");
        assertNull(actual);
    }

    @Test
    public void testGetAccessibleInterfaceMethodFromDescription() {
        final Class<?>[][] p = {ArrayUtils.EMPTY_CLASS_ARRAY, null};
        for (final Class<?>[] element : p) {
            final Method accessibleMethod = MethodUtils.getAccessibleMethod(
                    TestMutable.class, "getValue", element);
            assertSame(Mutable.class, accessibleMethod.getDeclaringClass());
        }
    }

    @Test
    public void testGetAccessiblePublicMethod() throws Exception {
        assertSame(MutableObject.class, MethodUtils.getAccessibleMethod(
                MutableObject.class.getMethod("getValue",
                        ArrayUtils.EMPTY_CLASS_ARRAY)).getDeclaringClass());
    }

    @Test
    public void testGetAccessiblePublicMethodFromDescription() {
        assertSame(MutableObject.class, MethodUtils.getAccessibleMethod(
                MutableObject.class, "getValue", ArrayUtils.EMPTY_CLASS_ARRAY)
                .getDeclaringClass());
    }

    @Test
    public void testGetAccessibleMethodInaccessible() throws Exception {
        final Method expected = TestBean.class.getDeclaredMethod("privateStuff");
        final Method actual = MethodUtils.getAccessibleMethod(expected);
        assertNull(actual);
    }

    @Test
    public void testGetMatchingAccessibleMethod() {
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo",
                ArrayUtils.EMPTY_CLASS_ARRAY, ArrayUtils.EMPTY_CLASS_ARRAY);
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo",
                null, ArrayUtils.EMPTY_CLASS_ARRAY);
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo",
                singletonArray(String.class), singletonArray(String.class));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo",
                singletonArray(Object.class), singletonArray(Object.class));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo",
                singletonArray(Boolean.class), singletonArray(Object.class));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo",
                singletonArray(Byte.class), singletonArray(Integer.TYPE));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo",
                singletonArray(Byte.TYPE), singletonArray(Integer.TYPE));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo",
                singletonArray(Short.class), singletonArray(Integer.TYPE));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo",
                singletonArray(Short.TYPE), singletonArray(Integer.TYPE));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo",
                singletonArray(Character.class), singletonArray(Integer.TYPE));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo",
                singletonArray(Character.TYPE), singletonArray(Integer.TYPE));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo",
                singletonArray(Integer.class), singletonArray(Integer.class));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo",
                singletonArray(Integer.TYPE), singletonArray(Integer.TYPE));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo",
                singletonArray(Long.class), singletonArray(Long.TYPE));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo",
                singletonArray(Long.TYPE), singletonArray(Long.TYPE));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo",
                singletonArray(Float.class), singletonArray(Double.TYPE));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo",
                singletonArray(Float.TYPE), singletonArray(Double.TYPE));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo",
                singletonArray(Double.class), singletonArray(Double.TYPE));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo",
                singletonArray(Double.TYPE), singletonArray(Double.TYPE));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo",
                singletonArray(Double.TYPE), singletonArray(Double.TYPE));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo",
                new Class[]{String.class, String.class}, new Class[]{String[].class});
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo",
                new Class[]{Integer.TYPE, String.class, String.class}, new Class[]{Integer.class, String[].class});
        expectMatchingAccessibleMethodParameterTypes(InheritanceBean.class, "testOne",
                singletonArray(ParentObject.class), singletonArray(ParentObject.class));
        expectMatchingAccessibleMethodParameterTypes(InheritanceBean.class, "testOne",
                singletonArray(ChildObject.class), singletonArray(ParentObject.class));
        expectMatchingAccessibleMethodParameterTypes(InheritanceBean.class, "testTwo",
                singletonArray(ParentObject.class), singletonArray(GrandParentObject.class));
        expectMatchingAccessibleMethodParameterTypes(InheritanceBean.class, "testTwo",
                singletonArray(ChildObject.class), singletonArray(ChildInterface.class));
    }

    @Test
    public void testNullArgument() {
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "oneParameter",
                singletonArray(null), singletonArray(String.class));
    }

    @Test
    public void testGetOverrideHierarchyIncludingInterfaces() {
        final Method method = MethodUtils.getAccessibleMethod(StringParameterizedChild.class, "consume", String.class);
        final Iterator<MethodDescriptor> expected =
                Arrays.asList(new MethodDescriptor(StringParameterizedChild.class, "consume", String.class),
                        new MethodDescriptor(GenericParent.class, "consume", GenericParent.class.getTypeParameters()[0]),
                        new MethodDescriptor(GenericConsumer.class, "consume", GenericConsumer.class.getTypeParameters()[0]))
                        .iterator();
        for (final Method m : MethodUtils.getOverrideHierarchy(method, Interfaces.INCLUDE)) {
            assertTrue(expected.hasNext());
            final MethodDescriptor md = expected.next();
            assertEquals(md.declaringClass, m.getDeclaringClass());
            assertEquals(md.name, m.getName());
            assertEquals(md.parameterTypes.length, m.getParameterTypes().length);
            for (int i = 0; i < md.parameterTypes.length; i++) {
                assertTrue(TypeUtils.equals(md.parameterTypes[i], m.getGenericParameterTypes()[i]));
            }
        }
        assertFalse(expected.hasNext());
    }

    @Test
    public void testGetOverrideHierarchyExcludingInterfaces() {
        final Method method = MethodUtils.getAccessibleMethod(StringParameterizedChild.class, "consume", String.class);
        final Iterator<MethodDescriptor> expected =
                Arrays.asList(new MethodDescriptor(StringParameterizedChild.class, "consume", String.class),
                        new MethodDescriptor(GenericParent.class, "consume", GenericParent.class.getTypeParameters()[0]))
                        .iterator();
        for (final Method m : MethodUtils.getOverrideHierarchy(method, Interfaces.EXCLUDE)) {
            assertTrue(expected.hasNext());
            final MethodDescriptor md = expected.next();
            assertEquals(md.declaringClass, m.getDeclaringClass());
            assertEquals(md.name, m.getName());
            assertEquals(md.parameterTypes.length, m.getParameterTypes().length);
            for (int i = 0; i < md.parameterTypes.length; i++) {
                assertTrue(TypeUtils.equals(md.parameterTypes[i], m.getGenericParameterTypes()[i]));
            }
        }
        assertFalse(expected.hasNext());
    }

    @Test
    @Annotated
    public void testGetMethodsWithAnnotation() throws NoSuchMethodException {
        assertArrayEquals(new Method[0], MethodUtils.getMethodsWithAnnotation(Object.class, Annotated.class));

        final Method[] methodsWithAnnotation = MethodUtils.getMethodsWithAnnotation(MethodUtilsTest.class, Annotated.class);
        assertEquals(2, methodsWithAnnotation.length);
        assertThat(methodsWithAnnotation, hasItemInArray(MethodUtilsTest.class.getMethod("testGetMethodsWithAnnotation")));
        assertThat(methodsWithAnnotation, hasItemInArray(MethodUtilsTest.class.getMethod("testGetMethodsListWithAnnotation")));
    }

    @Test
    public void testGetMethodsWithAnnotationSearchSupersAndIgnoreAccess() {
        assertArrayEquals(new Method[0], MethodUtils.getMethodsWithAnnotation(Object.class, Annotated.class,
                true, true));

        final Method[] methodsWithAnnotation = MethodUtils.getMethodsWithAnnotation(PublicChild.class, Annotated.class,
                true, true);
        assertEquals(4, methodsWithAnnotation.length);
        assertEquals("PublicChild", methodsWithAnnotation[0].getDeclaringClass().getSimpleName());
        assertEquals("PublicChild", methodsWithAnnotation[1].getDeclaringClass().getSimpleName());
        assertTrue(methodsWithAnnotation[0].getName().endsWith("AnnotatedMethod"));
        assertTrue(methodsWithAnnotation[1].getName().endsWith("AnnotatedMethod"));
        assertEquals("Foo.doIt",
                methodsWithAnnotation[2].getDeclaringClass().getSimpleName() + '.' +
                        methodsWithAnnotation[2].getName());
        assertEquals("Parent.parentProtectedAnnotatedMethod",
                methodsWithAnnotation[3].getDeclaringClass().getSimpleName() + '.' +
                        methodsWithAnnotation[3].getName());
    }

    @Test
    public void testGetMethodsWithAnnotationNotSearchSupersButIgnoreAccess() {
        assertArrayEquals(new Method[0], MethodUtils.getMethodsWithAnnotation(Object.class, Annotated.class,
                false, true));

        final Method[] methodsWithAnnotation = MethodUtils.getMethodsWithAnnotation(PublicChild.class, Annotated.class,
                false, true);
        assertEquals(2, methodsWithAnnotation.length);
        assertEquals("PublicChild", methodsWithAnnotation[0].getDeclaringClass().getSimpleName());
        assertEquals("PublicChild", methodsWithAnnotation[1].getDeclaringClass().getSimpleName());
        assertTrue(methodsWithAnnotation[0].getName().endsWith("AnnotatedMethod"));
        assertTrue(methodsWithAnnotation[1].getName().endsWith("AnnotatedMethod"));
    }

    @Test
    public void testGetMethodsWithAnnotationSearchSupersButNotIgnoreAccess() {
        assertArrayEquals(new Method[0], MethodUtils.getMethodsWithAnnotation(Object.class, Annotated.class,
                true, false));

        final Method[] methodsWithAnnotation = MethodUtils.getMethodsWithAnnotation(PublicChild.class, Annotated.class,
                true, false);
        assertEquals(2, methodsWithAnnotation.length);
        assertEquals("PublicChild.publicAnnotatedMethod",
                methodsWithAnnotation[0].getDeclaringClass().getSimpleName() + '.' +
                        methodsWithAnnotation[0].getName());
        assertEquals("Foo.doIt",
                methodsWithAnnotation[1].getDeclaringClass().getSimpleName() + '.' +
                        methodsWithAnnotation[1].getName());
    }

    @Test
    public void testGetMethodsWithAnnotationNotSearchSupersAndNotIgnoreAccess() {
        assertArrayEquals(new Method[0], MethodUtils.getMethodsWithAnnotation(Object.class, Annotated.class,
                false, false));

        final Method[] methodsWithAnnotation = MethodUtils.getMethodsWithAnnotation(PublicChild.class, Annotated.class,
                false, false);
        assertEquals(1, methodsWithAnnotation.length);
        assertEquals("PublicChild.publicAnnotatedMethod",
                methodsWithAnnotation[0].getDeclaringClass().getSimpleName() + '.' +
                        methodsWithAnnotation[0].getName());
    }

    @Test
    public void testGetAnnotationSearchSupersAndIgnoreAccess() throws NoSuchMethodException {
        assertNull(MethodUtils.getAnnotation(PublicChild.class.getMethod("parentNotAnnotatedMethod"),
                Annotated.class, true, true));
        assertNotNull(MethodUtils.getAnnotation(PublicChild.class.getMethod("doIt"), Annotated.class,
                true, true));
        assertNotNull(MethodUtils.getAnnotation(PublicChild.class.getMethod("parentProtectedAnnotatedMethod"),
                Annotated.class, true, true));
        assertNotNull(MethodUtils.getAnnotation(PublicChild.class.getDeclaredMethod("privateAnnotatedMethod"),
                Annotated.class, true, true));
        assertNotNull(MethodUtils.getAnnotation(PublicChild.class.getMethod("publicAnnotatedMethod"),
                Annotated.class, true, true));

        assertNull(MethodUtils.getAnnotation(StringParameterizedChild.class.getMethod("parentNotAnnotatedMethod", String.class),
                Annotated.class, true, true));
        assertNotNull(MethodUtils.getAnnotation(StringParameterizedChild.class.getMethod("parentProtectedAnnotatedMethod", String.class),
                Annotated.class, true, true));
        assertNotNull(MethodUtils.getAnnotation(StringParameterizedChild.class.getDeclaredMethod("privateAnnotatedMethod", String.class),
                Annotated.class, true, true));
        assertNotNull(MethodUtils.getAnnotation(StringParameterizedChild.class.getMethod("publicAnnotatedMethod", String.class),
                Annotated.class, true, true));
    }

    @Test
    public void testGetAnnotationNotSearchSupersButIgnoreAccess() throws NoSuchMethodException {
        assertNull(MethodUtils.getAnnotation(PublicChild.class.getMethod("parentNotAnnotatedMethod"),
                Annotated.class, false, true));
        assertNull(MethodUtils.getAnnotation(PublicChild.class.getMethod("doIt"), Annotated.class,
                false, true));
        assertNull(MethodUtils.getAnnotation(PublicChild.class.getMethod("parentProtectedAnnotatedMethod"),
                Annotated.class, false, true));
        assertNotNull(MethodUtils.getAnnotation(PublicChild.class.getDeclaredMethod("privateAnnotatedMethod"),
                Annotated.class, false, true));
        assertNotNull(MethodUtils.getAnnotation(PublicChild.class.getMethod("publicAnnotatedMethod"),
                Annotated.class, false, true));
    }

    @Test
    public void testGetAnnotationSearchSupersButNotIgnoreAccess() throws NoSuchMethodException {
        assertNull(MethodUtils.getAnnotation(PublicChild.class.getMethod("parentNotAnnotatedMethod"),
                Annotated.class, true, false));
        assertNull(MethodUtils.getAnnotation(PublicChild.class.getMethod("doIt"), Annotated.class,
                true, false));
        assertNull(MethodUtils.getAnnotation(PublicChild.class.getMethod("parentProtectedAnnotatedMethod"),
                Annotated.class, true, false));
        assertNull(MethodUtils.getAnnotation(PublicChild.class.getDeclaredMethod("privateAnnotatedMethod"),
                Annotated.class, true, false));
        assertNotNull(MethodUtils.getAnnotation(PublicChild.class.getMethod("publicAnnotatedMethod"),
                Annotated.class, true, false));

        assertNull(MethodUtils.getAnnotation(StringParameterizedChild.class.getMethod("parentNotAnnotatedMethod", String.class),
                Annotated.class, true, false));
        assertNull(MethodUtils.getAnnotation(StringParameterizedChild.class.getMethod("parentProtectedAnnotatedMethod", String.class),
                Annotated.class, true, false));
        assertNull(MethodUtils.getAnnotation(StringParameterizedChild.class.getDeclaredMethod("privateAnnotatedMethod", String.class),
                Annotated.class, true, false));
        assertNotNull(MethodUtils.getAnnotation(StringParameterizedChild.class.getMethod("publicAnnotatedMethod", String.class),
                Annotated.class, true, false));
    }

    @Test
    public void testGetAnnotationNotSearchSupersAndNotIgnoreAccess() throws NoSuchMethodException {
        assertNull(MethodUtils.getAnnotation(PublicChild.class.getMethod("parentNotAnnotatedMethod"),
                Annotated.class, false, false));
        assertNull(MethodUtils.getAnnotation(PublicChild.class.getMethod("doIt"), Annotated.class,
                false, false));
        assertNull(MethodUtils.getAnnotation(PublicChild.class.getMethod("parentProtectedAnnotatedMethod"),
                Annotated.class, false, false));
        assertNull(MethodUtils.getAnnotation(PublicChild.class.getDeclaredMethod("privateAnnotatedMethod"),
                Annotated.class, false, false));
        assertNotNull(MethodUtils.getAnnotation(PublicChild.class.getMethod("publicAnnotatedMethod"),
                Annotated.class, false, false));
    }

    @Test
    public void testGetMethodsWithAnnotationIllegalArgumentException1() {
        assertThrows(NullPointerException.class, () -> MethodUtils.getMethodsWithAnnotation(FieldUtilsTest.class, null));
    }

    @Test
    public void testGetMethodsWithAnnotationIllegalArgumentException2() {
        assertThrows(NullPointerException.class, () -> MethodUtils.getMethodsWithAnnotation(null, Annotated.class));
    }

    @Test
    public void testGetMethodsWithAnnotationIllegalArgumentException3() {
        assertThrows(NullPointerException.class, () -> MethodUtils.getMethodsWithAnnotation(null, null));
    }

    @Test
    @Annotated
    public void testGetMethodsListWithAnnotation() throws NoSuchMethodException {
        assertEquals(0, MethodUtils.getMethodsListWithAnnotation(Object.class, Annotated.class).size());

        final List<Method> methodWithAnnotation = MethodUtils.getMethodsListWithAnnotation(MethodUtilsTest.class, Annotated.class);
        assertEquals(2, methodWithAnnotation.size());
        assertThat(methodWithAnnotation, hasItems(
                MethodUtilsTest.class.getMethod("testGetMethodsWithAnnotation"),
                MethodUtilsTest.class.getMethod("testGetMethodsListWithAnnotation")
        ));
    }

    @Test
    public void testGetMethodsListWithAnnotationIllegalArgumentException1() {
        assertThrows(NullPointerException.class, () -> MethodUtils.getMethodsListWithAnnotation(FieldUtilsTest.class, null));
    }

    @Test
    public void testGetMethodsListWithAnnotationIllegalArgumentException2() {
        assertThrows(NullPointerException.class, () -> MethodUtils.getMethodsListWithAnnotation(null, Annotated.class));
    }

    @Test
    public void testGetMethodsListWithAnnotationIllegalArgumentException3() {
        assertThrows(NullPointerException.class, () -> MethodUtils.getMethodsListWithAnnotation(null, null));
    }

    @Test
    public void testGetAnnotationIllegalArgumentException1() {
        assertThrows(NullPointerException.class,
                () -> MethodUtils.getAnnotation(FieldUtilsTest.class.getDeclaredMethods()[0], null, true, true));
    }

    @Test
    public void testGetAnnotationIllegalArgumentException2() {
        assertThrows(NullPointerException.class, () -> MethodUtils.getAnnotation(null, Annotated.class, true, true));
    }

    @Test
    public void testGetAnnotationIllegalArgumentException3() {
        assertThrows(NullPointerException.class, () -> MethodUtils.getAnnotation(null, null, true, true));
    }

    private void expectMatchingAccessibleMethodParameterTypes(final Class<?> cls,
                                                              final String methodName, final Class<?>[] requestTypes, final Class<?>[] actualTypes) {
        final Method m = MethodUtils.getMatchingAccessibleMethod(cls, methodName,
                requestTypes);
        assertNotNull(m, "could not find any matches for " + methodName
                + " (" + (requestTypes == null ? null : toString(requestTypes)) + ")");
        assertArrayEquals(actualTypes, m.getParameterTypes(), toString(m.getParameterTypes()) + " not equals " + toString(actualTypes));
    }

    private String toString(final Class<?>[] c) {
        return Arrays.asList(c).toString();
    }

    private Class<?>[] singletonArray(final Class<?> c) {
        Class<?>[] result = classCache.get(c);
        if (result == null) {
            result = new Class[]{c};
            classCache.put(c, result);
        }
        return result;
    }

    public static class InheritanceBean {
        public void testOne(final Object obj) {
        }

        public void testOne(final GrandParentObject obj) {
        }

        public void testOne(final ParentObject obj) {
        }

        public void testTwo(final Object obj) {
        }

        public void testTwo(final GrandParentObject obj) {
        }

        public void testTwo(final ChildInterface obj) {
        }
    }

    interface ChildInterface {
    }

    public static class GrandParentObject {
    }

    public static class ParentObject extends GrandParentObject {
    }

    public static class ChildObject extends ParentObject implements ChildInterface {
    }

    private static class MethodDescriptor {
        final Class<?> declaringClass;
        final String name;
        final Type[] parameterTypes;

        MethodDescriptor(final Class<?> declaringClass, final String name, final Type... parameterTypes) {
            this.declaringClass = declaringClass;
            this.name = name;
            this.parameterTypes = parameterTypes;
        }
    }

    @Test
    public void testVarArgsUnboxing() throws Exception {
        final TestBean testBean = new TestBean();
        final int[] actual = (int[]) MethodUtils.invokeMethod(testBean, "unboxing", Integer.valueOf(1), Integer.valueOf(2));
        assertArrayEquals(new int[]{1, 2}, actual);
    }

    @Test
    public void testInvokeMethodForceAccessNoArgs() throws Exception {
        assertEquals("privateStringStuff()", MethodUtils.invokeMethod(testBean, true, "privateStringStuff"));
    }

    @Test
    public void testInvokeMethodForceAccessWithArgs() throws Exception {
        assertEquals("privateStringStuff(Integer)", MethodUtils.invokeMethod(testBean, true, "privateStringStuff", 5));
        assertEquals("privateStringStuff(double)", MethodUtils.invokeMethod(testBean, true, "privateStringStuff", 5.0d));
        assertEquals("privateStringStuff(String)", MethodUtils.invokeMethod(testBean, true, "privateStringStuff", "Hi There"));
        assertEquals("privateStringStuff(Object)", MethodUtils.invokeMethod(testBean, true, "privateStringStuff", new Date()));
    }

    @Test
    public void testDistance() throws Exception {
        final Method distanceMethod = MethodUtils.getMatchingMethod(MethodUtils.class, "distance", Class[].class, Class[].class);
        distanceMethod.setAccessible(true);

        assertEquals(-1, distanceMethod.invoke(null, new Class[]{String.class}, new Class[]{Date.class}));
        assertEquals(0, distanceMethod.invoke(null, new Class[]{Date.class}, new Class[]{Date.class}));
        assertEquals(1, distanceMethod.invoke(null, new Class[]{Integer.class}, new Class[]{ClassUtils.wrapperToPrimitive(Integer.class)}));
        assertEquals(2, distanceMethod.invoke(null, new Class[]{Integer.class}, new Class[]{Object.class}));

        distanceMethod.setAccessible(false);
    }

    @Test
    public void testGetMatchingMethod() throws NoSuchMethodException {
        assertEquals(MethodUtils.getMatchingMethod(GetMatchingMethodClass.class, "testMethod"),
                GetMatchingMethodClass.class.getMethod("testMethod"));

        assertEquals(MethodUtils.getMatchingMethod(GetMatchingMethodClass.class, "testMethod", Long.TYPE),
                GetMatchingMethodClass.class.getMethod("testMethod", Long.TYPE));

        assertEquals(MethodUtils.getMatchingMethod(GetMatchingMethodClass.class, "testMethod", Long.class),
                GetMatchingMethodClass.class.getMethod("testMethod", Long.class));

        assertEquals(MethodUtils.getMatchingMethod(GetMatchingMethodClass.class, "testMethod", (Class<?>) null),
                GetMatchingMethodClass.class.getMethod("testMethod", Long.class));

        assertThrows(IllegalStateException.class,
                () -> MethodUtils.getMatchingMethod(GetMatchingMethodClass.class, "testMethod2", (Class<?>) null));

        assertEquals(MethodUtils.getMatchingMethod(GetMatchingMethodClass.class, "testMethod3", Long.TYPE, Long.class),
                GetMatchingMethodClass.class.getMethod("testMethod3", Long.TYPE, Long.class));

        assertEquals(MethodUtils.getMatchingMethod(GetMatchingMethodClass.class, "testMethod3", Long.class, Long.TYPE),
                GetMatchingMethodClass.class.getMethod("testMethod3", Long.class, Long.TYPE));

        assertEquals(MethodUtils.getMatchingMethod(GetMatchingMethodClass.class, "testMethod3", null, Long.TYPE),
                GetMatchingMethodClass.class.getMethod("testMethod3", Long.class, Long.TYPE));

        assertEquals(MethodUtils.getMatchingMethod(GetMatchingMethodClass.class, "testMethod3", Long.TYPE, null),
                GetMatchingMethodClass.class.getMethod("testMethod3", Long.TYPE, Long.class));

        assertThrows(IllegalStateException.class,
                () -> MethodUtils.getMatchingMethod(GetMatchingMethodClass.class, "testMethod4", null, null));
    }

    private static final class GetMatchingMethodClass {
        public void testMethod() {
        }

        public void testMethod(final Long aLong) {
        }

        public void testMethod(final long aLong) {
        }

        public void testMethod2(final Long aLong) {
        }

        public void testMethod2(final Color aColor) {
        }

        public void testMethod2(final long aLong) {
        }

        public void testMethod3(final long aLong, final Long anotherLong) {
        }

        public void testMethod3(final Long aLong, final long anotherLong) {
        }

        public void testMethod3(final Long aLong, final Long anotherLong) {
        }

        public void testMethod4(final Long aLong, final Long anotherLong) {
        }

        public void testMethod4(final Color aColor1, final Color aColor2) {
        }
    }
}
