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
package org.apache.commons.lang3.reflect;

import static org.apache.commons.lang3.LangAssertions.assertNullPointerException;
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
import java.lang.reflect.Modifier;
import java.lang.reflect.Type;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
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
import org.apache.commons.lang3.reflect.testbed.PublicSubBeanOtherPackage;
import org.apache.commons.lang3.reflect.testbed.StringParameterizedChild;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * {@link Tests MethodUtils}.
 */
class MethodUtilsTest extends AbstractLangTest {

    protected abstract static class AbstractGetMatchingMethod implements InterfaceGetMatchingMethod {
        public abstract void testMethod5(Exception exception);
    }

    protected abstract static class AbstractGetMatchingMethod2 implements InterfaceGetMatchingMethod {
        @Override
        public void testMethod6() { }
    }

    public static class ChildObject extends ParentObject implements PackagePrivateEmptyInterface {
    }

    private static final class ConcreteGetMatchingMethod2 extends AbstractGetMatchingMethod2 { }

    private static final class ConcreteGetMatchingMethod22 extends AbstractGetMatchingMethod2 {
        @Override
        public void testMethod6() { }
    }

    private static final class GetMatchingMethodClass {

        public void testMethod() {
        }

        public void testMethod(final long aLong) {
        }

        public void testMethod(final Long aLong) {
        }

        public void testMethod2(final Color aColor) {
        }

        public void testMethod2(final long aLong) {
        }

        public void testMethod2(final Long aLong) {
        }

        public void testMethod3(final long aLong, final Long anotherLong) {
        }

        public void testMethod3(final Long aLong, final long anotherLong) {
        }

        public void testMethod3(final Long aLong, final Long anotherLong) {
        }

        public void testMethod4(final Color aColor1, final Color aColor2) {
        }

        public void testMethod4(final Long aLong, final Long anotherLong) {
        }
    }

    private static final class GetMatchingMethodImpl extends AbstractGetMatchingMethod {
        @Override
        public void testMethod5(final Exception exception) {
        }
    }

    public static class GrandParentObject {
    }
    public static class InheritanceBean {
        public void testOne(final GrandParentObject obj) {
        }

        public void testOne(final Object obj) {
        }

        public void testOne(final ParentObject obj) {
        }

        public void testTwo(final GrandParentObject obj) {
        }

        public void testTwo(final Object obj) {
        }

        public void testTwo(final PackagePrivateEmptyInterface obj) {
        }
    }

    interface InterfaceGetMatchingMethod {
        default void testMethod6() {
        }
    }
    private static final class MethodDescriptor {
        final Class<?> declaringClass;
        final String name;
        final Type[] parameterTypes;

        MethodDescriptor(final Class<?> declaringClass, final String name, final Type... parameterTypes) {
            this.declaringClass = declaringClass;
            this.name = name;
            this.parameterTypes = parameterTypes;
        }
    }

    interface PackagePrivateEmptyInterface {
        // empty
    }

    public static class ParentObject extends GrandParentObject {
        // empty
    }

    private interface PrivateEmptyInterface {
        // empty
    }

    public static class PublicImpl1OfPackagePrivateEmptyInterface implements PackagePrivateEmptyInterface {
        // empty
    }

    public static class PublicImpl2OfPackagePrivateEmptyInterface implements PackagePrivateEmptyInterface {
        // empty
    }

    public static class TestBean {

        public static String bar() {
            return "bar()";
        }

        public static String bar(final double d) {
            return "bar(double)";
        }

        public static String bar(final int i) {
            return "bar(int)";
        }

        public static String bar(final Integer i) {
            return "bar(Integer)";
        }

        public static String bar(final Integer i, final String... s) {
            return "bar(Integer, String...)";
        }

        public static String bar(final long... s) {
            return "bar(long...)";
        }

        public static String bar(final Object o) {
            return "bar(Object)";
        }

        public static String bar(final String s) {
            return "bar(String)";
        }

        public static String bar(final String... s) {
            return "bar(String...)";
        }

        // This method is overloaded for the wrapper class for every numeric primitive type, plus the common
        // supertype Number
        public static String numOverload(final Byte... args) {
            return "Byte...";
        }

        public static String numOverload(final Double... args) {
            return "Double...";
        }

        public static String numOverload(final Float... args) {
            return "Float...";
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

        public static String numOverload(final Short... args) {
            return "Short...";
        }

        public static void oneParameterStatic(final String s) {
            // empty
        }

        public static String staticInt(final int intArg) {
            return "static int";
        }

        public static String staticIntIntVarArg(final int intArg, final int... args) {
            return "static int, int...";
        }

        public static String staticIntLongVarArg(final int intArg, final long... args) {
            return "static int, long...";
        }

        public static String staticIntStringVarArg(final int intArg, final String... args) {
            return "static int, String...";
        }

        public static String staticPackagePrivateEmptyInterface(final PackagePrivateEmptyInterface... args) {
            return "static PackagePrivateEmptyInterface...";
        }

        public static String varOverload(final Boolean... args) {
            return "Boolean...";
        }

        // This method is overloaded for the wrapper class for every primitive type, plus the common supertypes
        // Number and Object. This is an acid test since it easily leads to ambiguous methods.
        public static String varOverload(final Byte... args) {
            return "Byte...";
        }

        public static String varOverload(final Character... args) {
            return "Character...";
        }

        public static String varOverload(final Double... args) {
            return "Double...";
        }

        public static String varOverload(final Float... args) {
            return "Float...";
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

        public static String varOverload(final Short... args) {
            return "Short...";
        }

        public static String varOverload(final String... args) {
            return "String...";
        }

        public static ImmutablePair<String, Object[]> varOverloadEchoStatic(final Number... args) {
            return new ImmutablePair<>("Number...", args);
        }

        public static ImmutablePair<String, Object[]> varOverloadEchoStatic(final String... args) {
            return new ImmutablePair<>("String...", args);
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

        boolean unboxBooleanArray;

        boolean unboxByteArray;

        boolean unboxCharArray;

        boolean unboxDoubleArray;

        boolean unboxFloatArray;

        boolean unboxIntArray;

        boolean unboxLongArray;

        boolean unboxShortArray;

        public String foo() {
            return "foo()";
        }

        public String foo(final double d) {
            return "foo(double)";
        }

        public String foo(final int i) {
            return "foo(int)";
        }

        public String foo(final Integer i) {
            return "foo(Integer)";
        }

        public String foo(final Integer i, final String... s) {
            return "foo(int, String...)";
        }

        public String foo(final long l) {
            return "foo(long)";
        }

        public String foo(final long... l) {
            return "foo(long...)";
        }

        public String foo(final Object o) {
            return "foo(Object)";
        }

        public String foo(final Object... s) {
            return "foo(Object...)";
        }

        public String foo(final String s) {
            return "foo(String)";
        }

        public String foo(final String... s) {
            return "foo(String...)";
        }

        public String intIntVarArg(final int intArg, final int... args) {
            return "int, int...";
        }

        public String intLongVarArg(final int intArg, final long... args) {
            return "int, long...";
        }

        public String intStringVarArg(final int intArg, final String... args) {
            return "int, String...";
        }

        public void oneParameter(final String s) {
            // empty
        }

        public String packagePrivateEmptyInterface(final PackagePrivateEmptyInterface... args) {
            return "PackagePrivateEmptyInterface...";
        }

        @SuppressWarnings("unused")
        private String privateStringStuff() {
            return "privateStringStuff()";
        }

        @SuppressWarnings("unused")
        private String privateStringStuff(final double d) {
            return "privateStringStuff(double)";
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
        private String privateStringStuff(final Object s) {
            return "privateStringStuff(Object)";
        }

        @SuppressWarnings("unused")
        private String privateStringStuff(final String s) {
            return "privateStringStuff(String)";
        }

        @SuppressWarnings("unused")
        private void privateStuff() {
        }

        public boolean[] unboxing(final boolean... values) {
            unboxBooleanArray = true;
            return values;
        }

        public byte[] unboxing(final byte... values) {
            unboxByteArray = true;
            return values;
        }

        public char[] unboxing(final char... values) {
            unboxCharArray = true;
            return values;
        }

        public double[] unboxing(final double... values) {
            unboxDoubleArray = true;
            return values;
        }

        public float[] unboxing(final float... values) {
            unboxFloatArray = true;
            return values;
        }

        public int[] unboxing(final int... values) {
            unboxIntArray = true;
            return values;
        }

        public long[] unboxing(final long... values) {
            unboxLongArray = true;
            return values;
        }

        public short[] unboxing(final short... values) {
            unboxShortArray = true;
            return values;
        }

        public ImmutablePair<String, Object[]> varOverloadEcho(final Number... args) {
            return new ImmutablePair<>("Number...", args);
        }

        // These varOverloadEcho and varOverloadEchoStatic methods are designed to verify that
        // not only is the correct overloaded variant invoked, but that the varargs arguments
        // are also delivered correctly to the method.
        public ImmutablePair<String, Object[]> varOverloadEcho(final String... args) {
            return new ImmutablePair<>("String...", args);
        }

    }

    static class TestBeanSubclass extends TestBean {
    }

    static class TestBeanWithInterfaces implements PrivateEmptyInterface {
        public String foo() {
            return "foo()";
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

    private static final class TestMutableSubclass extends TestMutable {
        // empty
    }

    private final Map<Class<?>, Class<?>[]> classCache = new HashMap<>();

    private TestBean testBean;

    private void expectMatchingAccessibleMethodParameterTypes(final Class<?> cls, final String methodName, final Class<?>[] requestTypes,
            final Class<?>[] actualTypes) {
        final Method m = MethodUtils.getMatchingAccessibleMethod(cls, methodName, requestTypes);
        assertNotNull(m, "could not find any matches for " + methodName + " (" + (requestTypes == null ? null : toString(requestTypes)) + ")");
        assertArrayEquals(actualTypes, m.getParameterTypes(), toString(m.getParameterTypes()) + " not equals " + toString(actualTypes));
    }

    @BeforeEach
    public void setUp() {
        testBean = new TestBean();
        classCache.clear();
    }

    private Class<?>[] singletonArray(final Class<?> c) {
        Class<?>[] result = classCache.get(c);
        if (result == null) {
            result = new Class[]{c};
            classCache.put(c, result);
        }
        return result;
    }

    @Test
    void testConstructor() throws Exception {
        assertNotNull(MethodUtils.class.getConstructor().newInstance());
    }

    @Test
    void testDistance() throws Exception {
        final Method distanceMethod = MethodUtils.getMatchingMethod(MethodUtils.class, "distance", Class[].class, Class[].class);
        distanceMethod.setAccessible(true);
        assertEquals(-1, distanceMethod.invoke(null, new Class[] { String.class }, new Class[] { Date.class }));
        assertEquals(0, distanceMethod.invoke(null, new Class[] { Date.class }, new Class[] { Date.class }));
        assertEquals(1, distanceMethod.invoke(null, new Class[] { Integer.class }, new Class[] { ClassUtils.wrapperToPrimitive(Integer.class) }));
        assertEquals(2, distanceMethod.invoke(null, new Class[] { Integer.class }, new Class[] { Object.class }));
        distanceMethod.setAccessible(false);
    }

    @ParameterizedTest
    @ValueSource(classes = {TestMutable.class, TestMutableSubclass.class})
    void testGetAccessibleInterfaceMethod(final Class<?> clazz) throws Exception {
        final Class<?>[][] p = {ArrayUtils.EMPTY_CLASS_ARRAY, null};
        for (final Class<?>[] element : p) {
            final Method method = clazz.getMethod("getValue", element);
            final Method accessibleMethod = MethodUtils.getAccessibleMethod(method);
            assertNotSame(accessibleMethod, method);
            assertSame(Mutable.class, accessibleMethod.getDeclaringClass());
            final Method accessibleMethod2 = MethodUtils.getAccessibleMethod(clazz, method);
            assertNotSame(accessibleMethod2, method);
            assertSame(Mutable.class, accessibleMethod2.getDeclaringClass());
        }
    }

    @ParameterizedTest
    @ValueSource(classes = {TestMutable.class, TestMutableSubclass.class})
    void testGetAccessibleInterfaceMethodFromDescription(final Class<?> clazz) {
        final Class<?>[][] p = { ArrayUtils.EMPTY_CLASS_ARRAY, null };
        for (final Class<?>[] element : p) {
            final Method accessibleMethod = MethodUtils.getAccessibleMethod(clazz, "getValue", element);
            assertSame(Mutable.class, accessibleMethod.getDeclaringClass());
        }
    }

    @Test
    void testGetAccessibleMethodInaccessible() throws Exception {
        assertNull(MethodUtils.getAccessibleMethod(TestBean.class.getDeclaredMethod("privateStuff")));
        assertNull(MethodUtils.getAccessibleMethod(TestBean.class, TestBean.class.getDeclaredMethod("privateStuff")));
        assertNull(MethodUtils.getAccessibleMethod(TestBeanSubclass.class, TestBean.class.getDeclaredMethod("privateStuff")));
    }

    @Test
    void testGetAccessibleMethodPrivateInterface() throws Exception {
        final Method expected = TestBeanWithInterfaces.class.getMethod("foo");
        assertNotNull(expected);
        final Method actual = MethodUtils.getAccessibleMethod(TestBeanWithInterfaces.class, "foo");
        assertNull(actual);
    }

    @Test
    void testGetAccessibleMethodPublicSub() throws Exception {
        // PackageBean class is package-private
        final int modifiers = PackageBean.class.getModifiers();
        assertFalse(Modifier.isPrivate(modifiers));
        assertFalse(Modifier.isProtected(modifiers));
        assertFalse(Modifier.isPublic(modifiers));
        // make sure that bean does what it should: compile
        new PublicSubBean().setBar("");
        // make sure that bean does what it should
        final PublicSubBean bean = new PublicSubBean();
        assertEquals(bean.getFoo(), "This is foo", "Start value (foo)");
        assertEquals(bean.getBar(), "This is bar", "Start value (bar)");
        bean.setFoo("new foo");
        bean.setBar("new bar");
        assertEquals(bean.getFoo(), "new foo", "Set value (foo)");
        assertEquals(bean.getBar(), "new bar", "Set value (bar)");
        // see if we can access public methods in a default access superclass
        // from a public access subclass instance
        MethodUtils.invokeExactMethod(bean, "setFoo", "alpha");
        assertEquals(bean.getFoo(), "alpha", "Set value (foo:2)");
        MethodUtils.invokeExactMethod(bean, "setBar", "beta");
        assertEquals(bean.getBar(), "beta", "Set value (bar:2)");
        // PublicSubBean.setFoo(String)
        Method method = MethodUtils.getAccessibleMethod(PublicSubBean.class, "setFoo", String.class);
        assertNotNull(method, "getAccessibleMethod() setFoo is Null");
        method.invoke(bean, "1111");
        assertEquals("1111", bean.getFoo(), "Set value (foo:3)");
        // PublicSubBean.setBar(String)
        method = MethodUtils.getAccessibleMethod(PublicSubBean.class, "setBar", String.class);
        assertNotNull(method, "getAccessibleMethod() setBar is Null");
        method.invoke(bean, "2222");
        assertEquals("2222", bean.getBar(), "Set value (bar:3)");
    }

    @Test
    void testGetAccessibleMethodPublicSubOtherPackage() throws Exception {
        // PackageBeanOtherPackage class is package-private
        final int modifiers = Class.forName("org.apache.commons.lang3.reflect.testbed.PackageBeanOtherPackage").getModifiers();
        assertFalse(Modifier.isPrivate(modifiers));
        assertFalse(Modifier.isProtected(modifiers));
        assertFalse(Modifier.isPublic(modifiers));
        // make sure that bean does what it should: compile
        new PublicSubBeanOtherPackage().setBar("");
        // make sure that bean does what it should
        final PublicSubBeanOtherPackage bean = new PublicSubBeanOtherPackage();
        assertEquals(bean.getFoo(), "This is foo", "Start value (foo)");
        assertEquals(bean.getBar(), "This is bar", "Start value (bar)");
        bean.setFoo("new foo");
        bean.setBar("new bar");
        assertEquals(bean.getFoo(), "new foo", "Set value (foo)");
        assertEquals(bean.getBar(), "new bar", "Set value (bar)");
        // see if we can access public methods in a default access superclass
        // from a public access subclass instance
        MethodUtils.invokeExactMethod(bean, "setFoo", "alpha");
        assertEquals(bean.getFoo(), "alpha", "Set value (foo:2)");
        MethodUtils.invokeExactMethod(bean, "setBar", "beta");
        assertEquals(bean.getBar(), "beta", "Set value (bar:2)");
        // PublicSubBean.setFoo(String)
        Method method = MethodUtils.getAccessibleMethod(PublicSubBeanOtherPackage.class, "setFoo", String.class);
        assertNotNull(method, "getAccessibleMethod() setFoo is Null");
        method.invoke(bean, "1111");
        assertEquals("1111", bean.getFoo(), "Set value (foo:3)");
        // PublicSubBean.setBar(String)
        method = MethodUtils.getAccessibleMethod(PublicSubBeanOtherPackage.class, "setBar", String.class);
        assertNotNull(method, "getAccessibleMethod() setBar is Null");
        method.invoke(bean, "2222");
        assertEquals("2222", bean.getBar(), "Set value (bar:3)");
    }

    @Test
    void testGetAccessiblePublicMethod() throws Exception {
        assertSame(MutableObject.class,
                MethodUtils.getAccessibleMethod(MutableObject.class.getMethod("getValue", ArrayUtils.EMPTY_CLASS_ARRAY)).getDeclaringClass());
        assertSame(MutableObject.class, MethodUtils
                .getAccessibleMethod(MutableObject.class, MutableObject.class.getMethod("getValue", ArrayUtils.EMPTY_CLASS_ARRAY)).getDeclaringClass());
    }

    @Test
    void testGetAccessiblePublicMethodFromDescription() {
        assertSame(MutableObject.class, MethodUtils.getAccessibleMethod(MutableObject.class, "getValue", ArrayUtils.EMPTY_CLASS_ARRAY).getDeclaringClass());
    }

    @Test
    void testGetAnnotationIllegalArgumentException1() {
        assertNullPointerException(() -> MethodUtils.getAnnotation(FieldUtilsTest.class.getDeclaredMethods()[0], null, true, true));
    }

    @Test
    void testGetAnnotationIllegalArgumentException2() {
        assertNullPointerException(() -> MethodUtils.getAnnotation(null, Annotated.class, true, true));
    }

    @Test
    void testGetAnnotationIllegalArgumentException3() {
        assertNullPointerException(() -> MethodUtils.getAnnotation(null, null, true, true));
    }

    @Test
    void testGetAnnotationNotSearchSupersAndNotIgnoreAccess() throws NoSuchMethodException {
        assertNull(MethodUtils.getAnnotation(PublicChild.class.getMethod("parentNotAnnotatedMethod"), Annotated.class, false, false));
        assertNull(MethodUtils.getAnnotation(PublicChild.class.getMethod("doIt"), Annotated.class, false, false));
        assertNull(MethodUtils.getAnnotation(PublicChild.class.getMethod("parentProtectedAnnotatedMethod"), Annotated.class, false, false));
        assertNull(MethodUtils.getAnnotation(PublicChild.class.getDeclaredMethod("privateAnnotatedMethod"), Annotated.class, false, false));
        assertNotNull(MethodUtils.getAnnotation(PublicChild.class.getMethod("publicAnnotatedMethod"), Annotated.class, false, false));
    }

    @Test
    void testGetAnnotationNotSearchSupersButIgnoreAccess() throws NoSuchMethodException {
        assertNull(MethodUtils.getAnnotation(PublicChild.class.getMethod("parentNotAnnotatedMethod"), Annotated.class, false, true));
        assertNull(MethodUtils.getAnnotation(PublicChild.class.getMethod("doIt"), Annotated.class, false, true));
        assertNull(MethodUtils.getAnnotation(PublicChild.class.getMethod("parentProtectedAnnotatedMethod"), Annotated.class, false, true));
        assertNotNull(MethodUtils.getAnnotation(PublicChild.class.getDeclaredMethod("privateAnnotatedMethod"), Annotated.class, false, true));
        assertNotNull(MethodUtils.getAnnotation(PublicChild.class.getMethod("publicAnnotatedMethod"), Annotated.class, false, true));
    }

    @Test
    void testGetAnnotationSearchSupersAndIgnoreAccess() throws NoSuchMethodException {
        assertNull(MethodUtils.getAnnotation(PublicChild.class.getMethod("parentNotAnnotatedMethod"), Annotated.class, true, true));
        assertNotNull(MethodUtils.getAnnotation(PublicChild.class.getMethod("doIt"), Annotated.class, true, true));
        assertNotNull(MethodUtils.getAnnotation(PublicChild.class.getMethod("parentProtectedAnnotatedMethod"), Annotated.class, true, true));
        assertNotNull(MethodUtils.getAnnotation(PublicChild.class.getDeclaredMethod("privateAnnotatedMethod"), Annotated.class, true, true));
        assertNotNull(MethodUtils.getAnnotation(PublicChild.class.getMethod("publicAnnotatedMethod"), Annotated.class, true, true));
        assertNull(MethodUtils.getAnnotation(StringParameterizedChild.class.getMethod("parentNotAnnotatedMethod", String.class), Annotated.class, true, true));
        assertNotNull(MethodUtils.getAnnotation(StringParameterizedChild.class.getMethod("parentProtectedAnnotatedMethod", String.class), Annotated.class, true,
                true));
        assertNotNull(MethodUtils.getAnnotation(StringParameterizedChild.class.getDeclaredMethod("privateAnnotatedMethod", String.class), Annotated.class, true,
                true));
        assertNotNull(MethodUtils.getAnnotation(StringParameterizedChild.class.getMethod("publicAnnotatedMethod", String.class), Annotated.class, true, true));
    }

    @Test
    void testGetAnnotationSearchSupersButNotIgnoreAccess() throws NoSuchMethodException {
        assertNull(MethodUtils.getAnnotation(PublicChild.class.getMethod("parentNotAnnotatedMethod"), Annotated.class, true, false));
        assertNull(MethodUtils.getAnnotation(PublicChild.class.getMethod("doIt"), Annotated.class, true, false));
        assertNull(MethodUtils.getAnnotation(PublicChild.class.getMethod("parentProtectedAnnotatedMethod"), Annotated.class, true, false));
        assertNull(MethodUtils.getAnnotation(PublicChild.class.getDeclaredMethod("privateAnnotatedMethod"), Annotated.class, true, false));
        assertNotNull(MethodUtils.getAnnotation(PublicChild.class.getMethod("publicAnnotatedMethod"), Annotated.class, true, false));
        assertNull(MethodUtils.getAnnotation(StringParameterizedChild.class.getMethod("parentNotAnnotatedMethod", String.class), Annotated.class, true, false));
        assertNull(MethodUtils.getAnnotation(StringParameterizedChild.class.getMethod("parentProtectedAnnotatedMethod", String.class), Annotated.class, true,
                false));
        assertNull(MethodUtils.getAnnotation(StringParameterizedChild.class.getDeclaredMethod("privateAnnotatedMethod", String.class), Annotated.class, true,
                false));
        assertNotNull(MethodUtils.getAnnotation(StringParameterizedChild.class.getMethod("publicAnnotatedMethod", String.class), Annotated.class, true, false));
    }

    @Test
    void testGetMatchingAccessibleMethod() {
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo", ArrayUtils.EMPTY_CLASS_ARRAY, ArrayUtils.EMPTY_CLASS_ARRAY);
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo", null, ArrayUtils.EMPTY_CLASS_ARRAY);
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo", singletonArray(String.class), singletonArray(String.class));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo", singletonArray(Object.class), singletonArray(Object.class));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo", singletonArray(Boolean.class), singletonArray(Object.class));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo", singletonArray(Byte.class), singletonArray(Integer.TYPE));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo", singletonArray(Byte.TYPE), singletonArray(Integer.TYPE));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo", singletonArray(Short.class), singletonArray(Integer.TYPE));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo", singletonArray(Short.TYPE), singletonArray(Integer.TYPE));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo", singletonArray(Character.class), singletonArray(Integer.TYPE));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo", singletonArray(Character.TYPE), singletonArray(Integer.TYPE));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo", singletonArray(Integer.class), singletonArray(Integer.class));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo", singletonArray(Integer.TYPE), singletonArray(Integer.TYPE));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo", singletonArray(Long.class), singletonArray(Long.TYPE));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo", singletonArray(Long.TYPE), singletonArray(Long.TYPE));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo", singletonArray(Float.class), singletonArray(Double.TYPE));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo", singletonArray(Float.TYPE), singletonArray(Double.TYPE));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo", singletonArray(Double.class), singletonArray(Double.TYPE));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo", singletonArray(Double.TYPE), singletonArray(Double.TYPE));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo", singletonArray(Double.TYPE), singletonArray(Double.TYPE));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo", new Class[] { String.class, String.class }, new Class[] { String[].class });
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo", new Class[] { Integer.TYPE, String.class, String.class },
                new Class[] { Integer.class, String[].class });
        expectMatchingAccessibleMethodParameterTypes(InheritanceBean.class, "testOne", singletonArray(ParentObject.class), singletonArray(ParentObject.class));
        expectMatchingAccessibleMethodParameterTypes(InheritanceBean.class, "testOne", singletonArray(ChildObject.class), singletonArray(ParentObject.class));
        expectMatchingAccessibleMethodParameterTypes(InheritanceBean.class, "testTwo", singletonArray(ParentObject.class),
                singletonArray(GrandParentObject.class));
        expectMatchingAccessibleMethodParameterTypes(InheritanceBean.class, "testTwo", singletonArray(ChildObject.class), singletonArray(PackagePrivateEmptyInterface.class));
        // LANG-1757
        expectMatchingAccessibleMethodParameterTypes(Files.class, "exists", singletonArray(Path.class), new Class[] { Path.class, LinkOption[].class });
    }

    @Test
    void testGetMatchingMethod() throws NoSuchMethodException {
        assertEquals(MethodUtils.getMatchingMethod(GetMatchingMethodClass.class, "testMethod"), GetMatchingMethodClass.class.getMethod("testMethod"));
        assertEquals(MethodUtils.getMatchingMethod(GetMatchingMethodClass.class, "testMethod", Long.TYPE),
                GetMatchingMethodClass.class.getMethod("testMethod", Long.TYPE));
        assertEquals(MethodUtils.getMatchingMethod(GetMatchingMethodClass.class, "testMethod", Long.class),
                GetMatchingMethodClass.class.getMethod("testMethod", Long.class));
        assertEquals(MethodUtils.getMatchingMethod(GetMatchingMethodClass.class, "testMethod", (Class<?>) null),
                GetMatchingMethodClass.class.getMethod("testMethod", Long.class));
        assertThrows(IllegalStateException.class, () -> MethodUtils.getMatchingMethod(GetMatchingMethodClass.class, "testMethod2", (Class<?>) null));
        assertEquals(MethodUtils.getMatchingMethod(GetMatchingMethodClass.class, "testMethod3", Long.TYPE, Long.class),
                GetMatchingMethodClass.class.getMethod("testMethod3", Long.TYPE, Long.class));
        assertEquals(MethodUtils.getMatchingMethod(GetMatchingMethodClass.class, "testMethod3", Long.class, Long.TYPE),
                GetMatchingMethodClass.class.getMethod("testMethod3", Long.class, Long.TYPE));
        assertEquals(MethodUtils.getMatchingMethod(GetMatchingMethodClass.class, "testMethod3", null, Long.TYPE),
                GetMatchingMethodClass.class.getMethod("testMethod3", Long.class, Long.TYPE));
        assertEquals(MethodUtils.getMatchingMethod(GetMatchingMethodClass.class, "testMethod3", Long.TYPE, null),
                GetMatchingMethodClass.class.getMethod("testMethod3", Long.TYPE, Long.class));
        assertThrows(IllegalStateException.class, () -> MethodUtils.getMatchingMethod(GetMatchingMethodClass.class, "testMethod4", null, null));
        assertEquals(MethodUtils.getMatchingMethod(GetMatchingMethodImpl.class, "testMethod5", RuntimeException.class),
                GetMatchingMethodImpl.class.getMethod("testMethod5", Exception.class));
        assertEquals(GetMatchingMethodImpl.class.getMethod("testMethod6"), MethodUtils.getMatchingMethod(GetMatchingMethodImpl.class, "testMethod6"));
        assertNullPointerException(() -> MethodUtils.getMatchingMethod(null, "testMethod5", RuntimeException.class));
        Method testMethod6 = MethodUtils.getMatchingMethod(ConcreteGetMatchingMethod2.class, "testMethod6");
        assertNotNull(testMethod6);
        assertEquals(AbstractGetMatchingMethod2.class, testMethod6.getDeclaringClass());
        testMethod6 = MethodUtils.getMatchingMethod(ConcreteGetMatchingMethod22.class, "testMethod6");
        assertNotNull(testMethod6);
        assertEquals(ConcreteGetMatchingMethod22.class, testMethod6.getDeclaringClass());
    }

    @Test
    void testGetMethodObject() throws Exception {
        assertEquals(MutableObject.class.getMethod("getValue", ArrayUtils.EMPTY_CLASS_ARRAY),
                MethodUtils.getMethodObject(MutableObject.class, "getValue", ArrayUtils.EMPTY_CLASS_ARRAY));
        assertNull(MethodUtils.getMethodObject(MutableObject.class, "does not exist, at all", ArrayUtils.EMPTY_CLASS_ARRAY));
        assertNull(MethodUtils.getMethodObject(null, "does not exist, at all", ArrayUtils.EMPTY_CLASS_ARRAY));
        assertNull(MethodUtils.getMethodObject(null, null, ArrayUtils.EMPTY_CLASS_ARRAY));
        assertNull(MethodUtils.getMethodObject(MutableObject.class, null, ArrayUtils.EMPTY_CLASS_ARRAY));
        // 0 args
        assertNull(MethodUtils.getMethodObject(MutableObject.class, "getValue", new Class[] { null }));
        // 1 args
        assertNull(MethodUtils.getMethodObject(MutableObject.class, "equals", new Class[] { null }));
        assertNull(MethodUtils.getMethodObject(MutableObject.class, "equals", new Class[] { String.class, null, String.class }));
    }

    /**
     * Tests a {@code public} method.
     */
    @Test
    @Annotated
    public void testGetMethodsListWithAnnotation() throws NoSuchMethodException {
        assertEquals(0, MethodUtils.getMethodsListWithAnnotation(Object.class, Annotated.class).size());
        final List<Method> methodWithAnnotation = MethodUtils.getMethodsListWithAnnotation(MethodUtilsTest.class, Annotated.class);
        assertEquals(2, methodWithAnnotation.size());
        assertTrue(methodWithAnnotation.contains(MethodUtilsTest.class.getMethod("testGetMethodsWithAnnotation")));
        assertTrue(methodWithAnnotation.contains(MethodUtilsTest.class.getMethod("testGetMethodsListWithAnnotation")));
    }

    @Test
    void testGetMethodsListWithAnnotationNullPointerException1() {
        assertNullPointerException(() -> MethodUtils.getMethodsListWithAnnotation(FieldUtilsTest.class, null));
    }

    @Test
    void testGetMethodsListWithAnnotationNullPointerException2() {
        assertNullPointerException(() -> MethodUtils.getMethodsListWithAnnotation(null, Annotated.class));
    }

    @Test
    void testGetMethodsListWithAnnotationNullPointerException3() {
        assertNullPointerException(() -> MethodUtils.getMethodsListWithAnnotation(null, null));
    }

    @Test
    @Annotated
    public void testGetMethodsWithAnnotation() throws NoSuchMethodException {
        assertArrayEquals(new Method[0], MethodUtils.getMethodsWithAnnotation(Object.class, Annotated.class));
        final Method[] methodsWithAnnotation = MethodUtils.getMethodsWithAnnotation(MethodUtilsTest.class, Annotated.class);
        assertEquals(2, methodsWithAnnotation.length);
        assertTrue(ArrayUtils.contains(methodsWithAnnotation, MethodUtilsTest.class.getMethod("testGetMethodsWithAnnotation")));
        assertTrue(ArrayUtils.contains(methodsWithAnnotation, MethodUtilsTest.class.getMethod("testGetMethodsListWithAnnotation")));
    }

    @Test
    void testGetMethodsWithAnnotationIllegalArgumentException1() {
        assertNullPointerException(() -> MethodUtils.getMethodsWithAnnotation(FieldUtilsTest.class, null));
    }

    @Test
    void testGetMethodsWithAnnotationIllegalArgumentException2() {
        assertNullPointerException(() -> MethodUtils.getMethodsWithAnnotation(null, Annotated.class));
    }

    @Test
    void testGetMethodsWithAnnotationIllegalArgumentException3() {
        assertNullPointerException(() -> MethodUtils.getMethodsWithAnnotation(null, null));
    }

    @Test
    void testGetMethodsWithAnnotationNotSearchSupersAndNotIgnoreAccess() {
        assertArrayEquals(new Method[0], MethodUtils.getMethodsWithAnnotation(Object.class, Annotated.class, false, false));
        final Method[] methodsWithAnnotation = MethodUtils.getMethodsWithAnnotation(PublicChild.class, Annotated.class, false, false);
        assertEquals(1, methodsWithAnnotation.length);
        assertEquals("PublicChild.publicAnnotatedMethod",
                methodsWithAnnotation[0].getDeclaringClass().getSimpleName() + '.' + methodsWithAnnotation[0].getName());
    }

    @Test
    void testGetMethodsWithAnnotationNotSearchSupersButIgnoreAccess() {
        assertArrayEquals(new Method[0], MethodUtils.getMethodsWithAnnotation(Object.class, Annotated.class, false, true));
        final Method[] methodsWithAnnotation = MethodUtils.getMethodsWithAnnotation(PublicChild.class, Annotated.class, false, true);
        assertEquals(2, methodsWithAnnotation.length);
        assertEquals("PublicChild", methodsWithAnnotation[0].getDeclaringClass().getSimpleName());
        assertEquals("PublicChild", methodsWithAnnotation[1].getDeclaringClass().getSimpleName());
        assertTrue(methodsWithAnnotation[0].getName().endsWith("AnnotatedMethod"));
        assertTrue(methodsWithAnnotation[1].getName().endsWith("AnnotatedMethod"));
    }

    @Test
    void testGetMethodsWithAnnotationSearchSupersAndIgnoreAccess() {
        assertArrayEquals(new Method[0], MethodUtils.getMethodsWithAnnotation(Object.class, Annotated.class, true, true));
        final Method[] methodsWithAnnotation = MethodUtils.getMethodsWithAnnotation(PublicChild.class, Annotated.class, true, true);
        assertEquals(4, methodsWithAnnotation.length);
        assertEquals("PublicChild", methodsWithAnnotation[0].getDeclaringClass().getSimpleName());
        assertEquals("PublicChild", methodsWithAnnotation[1].getDeclaringClass().getSimpleName());
        assertTrue(methodsWithAnnotation[0].getName().endsWith("AnnotatedMethod"));
        assertTrue(methodsWithAnnotation[1].getName().endsWith("AnnotatedMethod"));
        assertEquals("Foo.doIt", methodsWithAnnotation[2].getDeclaringClass().getSimpleName() + '.' + methodsWithAnnotation[2].getName());
        assertEquals("Parent.parentProtectedAnnotatedMethod",
                methodsWithAnnotation[3].getDeclaringClass().getSimpleName() + '.' + methodsWithAnnotation[3].getName());
    }

    @Test
    void testGetMethodsWithAnnotationSearchSupersButNotIgnoreAccess() {
        assertArrayEquals(new Method[0], MethodUtils.getMethodsWithAnnotation(Object.class, Annotated.class, true, false));
        final Method[] methodsWithAnnotation = MethodUtils.getMethodsWithAnnotation(PublicChild.class, Annotated.class, true, false);
        assertEquals(2, methodsWithAnnotation.length);
        assertEquals("PublicChild.publicAnnotatedMethod",
                methodsWithAnnotation[0].getDeclaringClass().getSimpleName() + '.' + methodsWithAnnotation[0].getName());
        assertEquals("Foo.doIt", methodsWithAnnotation[1].getDeclaringClass().getSimpleName() + '.' + methodsWithAnnotation[1].getName());
    }

    @Test
    void testGetOverrideHierarchyExcludingInterfaces() {
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
    void testGetOverrideHierarchyIncludingInterfaces() {
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
    void testInvokeExactMethod() throws Exception {
        assertEquals("foo()", MethodUtils.invokeExactMethod(testBean, "foo", (Object[]) ArrayUtils.EMPTY_CLASS_ARRAY));
        assertEquals("foo()", MethodUtils.invokeExactMethod(testBean, "foo"));
        assertEquals("foo()", MethodUtils.invokeExactMethod(testBean, "foo", (Object[]) null));
        assertEquals("foo()", MethodUtils.invokeExactMethod(testBean, "foo", null, null));
        assertEquals("foo(String)", MethodUtils.invokeExactMethod(testBean, "foo", ""));
        assertEquals("foo(Object)", MethodUtils.invokeExactMethod(testBean, "foo", new Object()));
        assertEquals("foo(Integer)", MethodUtils.invokeExactMethod(testBean, "foo", NumberUtils.INTEGER_ONE));
        assertEquals("foo(double)", MethodUtils.invokeExactMethod(testBean, "foo", new Object[] { NumberUtils.DOUBLE_ONE }, new Class[] { Double.TYPE }));
        assertThrows(NoSuchMethodException.class, () -> MethodUtils.invokeExactMethod(testBean, "foo", NumberUtils.BYTE_ONE));
        assertThrows(NoSuchMethodException.class, () -> MethodUtils.invokeExactMethod(testBean, "foo", NumberUtils.LONG_ONE));
        assertThrows(NoSuchMethodException.class, () -> MethodUtils.invokeExactMethod(testBean, "foo", Boolean.TRUE));
        assertThrows(NullPointerException.class, () -> MethodUtils.invokeExactMethod(null, "foo", NumberUtils.BYTE_ONE));
        assertThrows(NoSuchMethodException.class, () -> MethodUtils.invokeExactMethod(testBean, null, NumberUtils.BYTE_ONE));
        assertThrows(NullPointerException.class,
                () -> MethodUtils.invokeExactMethod(null, "foo", new Object[] { NumberUtils.DOUBLE_ONE }, new Class[] { Double.TYPE }));
        assertThrows(NoSuchMethodException.class,
                () -> MethodUtils.invokeExactMethod(testBean, null, new Object[] { NumberUtils.DOUBLE_ONE }, new Class[] { Double.TYPE }));
    }

    @Test
    void testInvokeExactStaticMethod() throws Exception {
        assertEquals("bar()", MethodUtils.invokeExactStaticMethod(TestBean.class, "bar", (Object[]) ArrayUtils.EMPTY_CLASS_ARRAY));
        assertEquals("bar()", MethodUtils.invokeExactStaticMethod(TestBean.class, "bar", (Object[]) null));
        assertEquals("bar()", MethodUtils.invokeExactStaticMethod(TestBean.class, "bar", null, null));
        assertEquals("bar(String)", MethodUtils.invokeExactStaticMethod(TestBean.class, "bar", ""));
        assertEquals("bar(Object)", MethodUtils.invokeExactStaticMethod(TestBean.class, "bar", new Object()));
        assertEquals("bar(Integer)", MethodUtils.invokeExactStaticMethod(TestBean.class, "bar", NumberUtils.INTEGER_ONE));
        assertEquals("bar(double)",
                MethodUtils.invokeExactStaticMethod(TestBean.class, "bar", new Object[] { NumberUtils.DOUBLE_ONE }, new Class[] { Double.TYPE }));
        assertThrows(NoSuchMethodException.class, () -> MethodUtils.invokeExactStaticMethod(TestBean.class, "bar", NumberUtils.BYTE_ONE));
        assertThrows(NoSuchMethodException.class, () -> MethodUtils.invokeExactStaticMethod(TestBean.class, "bar", NumberUtils.LONG_ONE));
        assertThrows(NoSuchMethodException.class, () -> MethodUtils.invokeExactStaticMethod(TestBean.class, "bar", Boolean.TRUE));
    }

    @Test
    void testInvokeJavaVarArgsOverloadingResolution() throws Exception {
        // Primitive wrappers
        assertEquals("Byte...", MethodUtils.invokeStaticMethod(TestBean.class, "varOverload", (byte) 1, (byte) 2));
        assertEquals("Short...", MethodUtils.invokeStaticMethod(TestBean.class, "varOverload", (short) 1, (short) 2));
        assertEquals("Integer...", MethodUtils.invokeStaticMethod(TestBean.class, "varOverload", 1, 2));
        assertEquals("Long...", MethodUtils.invokeStaticMethod(TestBean.class, "varOverload", 1L, 2L));
        assertEquals("Float...", MethodUtils.invokeStaticMethod(TestBean.class, "varOverload", 1f, 2f));
        assertEquals("Double...", MethodUtils.invokeStaticMethod(TestBean.class, "varOverload", 1d, 2d));
        assertEquals("Boolean...", MethodUtils.invokeStaticMethod(TestBean.class, "varOverload", true, false));
        // Number
        assertEquals("Number...", MethodUtils.invokeStaticMethod(TestBean.class, "varOverload", 1, 1.1));
        assertEquals("Number...", MethodUtils.invokeStaticMethod(TestBean.class, "varOverload", 1, 1L));
        assertEquals("Number...", MethodUtils.invokeStaticMethod(TestBean.class, "varOverload", 1d, 1f));
        assertEquals("Number...", MethodUtils.invokeStaticMethod(TestBean.class, "varOverload", (short) 1, (byte) 1));
        assertEquals("Number...", MethodUtils.invokeStaticMethod(TestBean.class, "numOverload", ArrayUtils.EMPTY_OBJECT_ARRAY));
        assertEquals("Number...", MethodUtils.invokeStaticMethod(TestBean.class, "numOverload", (Object[]) ArrayUtils.EMPTY_CLASS_ARRAY));
        assertEquals("Number...", MethodUtils.invokeStaticMethod(TestBean.class, "numOverload", (Object[]) ArrayUtils.EMPTY_INTEGER_OBJECT_ARRAY));
        // Object
        assertEquals("Object...", MethodUtils.invokeStaticMethod(TestBean.class, "varOverload", 1, "s"));
        assertEquals("Object...", MethodUtils.invokeStaticMethod(TestBean.class, "varOverload", 1, true));
        assertEquals("Object...", MethodUtils.invokeStaticMethod(TestBean.class, "varOverload", 1.1, true));
        assertEquals("Object...", MethodUtils.invokeStaticMethod(TestBean.class, "varOverload", 'c', true));
        assertEquals("Object...", MethodUtils.invokeStaticMethod(TestBean.class, "varOverload", 1, 'c'));
        assertEquals("Object...", MethodUtils.invokeStaticMethod(TestBean.class, "varOverload", 'c', "s"));
        assertEquals("Object...", MethodUtils.invokeStaticMethod(TestBean.class, "varOverload", ArrayUtils.EMPTY_OBJECT_ARRAY));
        assertEquals("Object...", MethodUtils.invokeStaticMethod(TestBean.class, "varOverload", (Object[]) ArrayUtils.EMPTY_CLASS_ARRAY));
        assertEquals("Object...", MethodUtils.invokeStaticMethod(TestBean.class, "varOverload", (Object[]) ArrayUtils.EMPTY_INTEGER_OBJECT_ARRAY));
        assertEquals("Object...", MethodUtils.invokeStaticMethod(TestBean.class, "varOverload"));
        // Other
        assertEquals("String...", MethodUtils.invokeStaticMethod(TestBean.class, "varOverload", "a", "b"));
        assertEquals("Character...", MethodUtils.invokeStaticMethod(TestBean.class, "varOverload", 'a', 'b'));
    }

    @Test
    void testInvokeMethod() throws Exception {
        assertEquals("foo()", MethodUtils.invokeMethod(testBean, "foo", (Object[]) ArrayUtils.EMPTY_CLASS_ARRAY));
        assertEquals("foo()", MethodUtils.invokeMethod(testBean, "foo"));
        assertEquals("foo()", MethodUtils.invokeMethod(testBean, "foo", (Object[]) null));
        assertEquals("foo()", MethodUtils.invokeMethod(testBean, "foo", null, null));
        assertEquals("foo(String)", MethodUtils.invokeMethod(testBean, "foo", ""));
        assertEquals("foo(Object)", MethodUtils.invokeMethod(testBean, "foo", new Object()));
        assertEquals("foo(Object)", MethodUtils.invokeMethod(testBean, "foo", Boolean.TRUE));
        assertEquals("foo(Integer)", MethodUtils.invokeMethod(testBean, "foo", NumberUtils.INTEGER_ONE));
        assertEquals("foo(int)", MethodUtils.invokeMethod(testBean, "foo", NumberUtils.BYTE_ONE));
        assertEquals("foo(long)", MethodUtils.invokeMethod(testBean, "foo", NumberUtils.LONG_ONE));
        assertEquals("foo(double)", MethodUtils.invokeMethod(testBean, "foo", NumberUtils.DOUBLE_ONE));
        assertEquals("foo(String...)", MethodUtils.invokeMethod(testBean, "foo", "a", "b", "c"));
        assertEquals("foo(String...)", MethodUtils.invokeMethod(testBean, "foo", "a", "b", "c"));
        assertEquals("foo(int, String...)", MethodUtils.invokeMethod(testBean, "foo", 5, "a", "b", "c"));
        assertEquals("foo(long...)", MethodUtils.invokeMethod(testBean, "foo", 1L, 2L));
        assertEquals("foo(long...)", MethodUtils.invokeMethod(testBean, "foo", 1, 2));
        assertEquals("foo(long...)", MethodUtils.invokeMethod(testBean, "foo", (byte) 1, (byte) 2)); // widen
        assertEquals("foo(long...)", MethodUtils.invokeMethod(testBean, "foo", (short) 1, (short) 2)); // widen
        assertEquals("foo(long...)", MethodUtils.invokeMethod(testBean, "foo", (char) 1, (char) 2)); // widen
        TestBean.verify(new ImmutablePair<>("String...", new String[] { "x", "y" }), MethodUtils.invokeMethod(testBean, "varOverloadEcho", "x", "y"));
        TestBean.verify(new ImmutablePair<>("Number...", new Number[] { 17, 23, 42 }), MethodUtils.invokeMethod(testBean, "varOverloadEcho", 17, 23, 42));
        TestBean.verify(new ImmutablePair<>("String...", new String[] { "x", "y" }), MethodUtils.invokeMethod(testBean, "varOverloadEcho", "x", "y"));
        TestBean.verify(new ImmutablePair<>("Number...", new Number[] { 17, 23, 42 }), MethodUtils.invokeMethod(testBean, "varOverloadEcho", 17, 23, 42));
        assertNullPointerException(() -> MethodUtils.invokeMethod(null, "foo", 1, 2));
        assertThrows(NoSuchMethodException.class, () -> MethodUtils.invokeMethod(testBean, null, 1, 2));
        assertThrows(NoSuchMethodException.class, () -> MethodUtils.invokeMethod(new Object(), "nonExistent", new Object[] { "val" }, new Class<?>[] { null }));
    }

    @Test
    void testInvokeMethod_VarArgsWithNullValues() throws Exception {
        assertEquals("String...", MethodUtils.invokeMethod(testBean, "varOverload", "a", null, "c"));
        assertEquals("String...", MethodUtils.invokeMethod(testBean, "varOverload", "a", "b", null));
        assertEquals("String...", MethodUtils.invokeMethod(testBean, "varOverload", new String[] { "a" }, new Class<?>[] { String.class }));
        assertThrows(NoSuchMethodException.class,
                () -> assertEquals("String...", MethodUtils.invokeMethod(testBean, "doesn't exist", new String[] { "a" }, new Class<?>[] { null })));
    }

    @Test
    void testInvokeMethod1PlusVarArgs() throws Exception {
        // intStringVarArg
        assertEquals("int, String...", MethodUtils.invokeMethod(testBean, "intStringVarArg", 1));
        assertEquals("int, String...", MethodUtils.invokeMethod(testBean, "intStringVarArg", 1, "s"));
        assertEquals("int, String...", MethodUtils.invokeMethod(testBean, "intStringVarArg", 1, "s1", "s2"));
        assertThrows(NoSuchMethodException.class, () -> MethodUtils.invokeMethod(testBean, "intStringVarArg", 1, "s1", 5));
        // intLongVarArg
        assertEquals("int, long...", MethodUtils.invokeMethod(testBean, "intLongVarArg", 1));
        assertEquals("int, long...", MethodUtils.invokeMethod(testBean, "intLongVarArg", 1, 2L));
        assertEquals("int, long...", MethodUtils.invokeMethod(testBean, "intLongVarArg", 1, 2L, 3L));
        assertThrows(NoSuchMethodException.class, () -> MethodUtils.invokeMethod(testBean, "intLongVarArg", 1, "s1", 5));
        // intIntVarArg
        assertEquals("int, int...", MethodUtils.invokeMethod(testBean, "intIntVarArg", 1));
        assertEquals("int, int...", MethodUtils.invokeMethod(testBean, "intIntVarArg", 1, 2));
        assertEquals("int, int...", MethodUtils.invokeMethod(testBean, "intIntVarArg", 1, 2, 3));
        assertThrows(NoSuchMethodException.class, () -> MethodUtils.invokeMethod(testBean, "intLongVarArg", 1, "s1", 5));
    }

    @Test
    void testInvokeMethodForceAccessNoArgs() throws Exception {
        assertEquals("privateStringStuff()", MethodUtils.invokeMethod(testBean, true, "privateStringStuff"));
    }

    @Test
    void testInvokeMethodForceAccessWithArgs() throws Exception {
        assertEquals("privateStringStuff(Integer)", MethodUtils.invokeMethod(testBean, true, "privateStringStuff", 5));
        assertEquals("privateStringStuff(double)", MethodUtils.invokeMethod(testBean, true, "privateStringStuff", 5.0d));
        assertEquals("privateStringStuff(String)", MethodUtils.invokeMethod(testBean, true, "privateStringStuff", "Hi There"));
        assertEquals("privateStringStuff(Object)", MethodUtils.invokeMethod(testBean, true, "privateStringStuff", new Date()));
        assertNullPointerException(() -> MethodUtils.invokeMethod(null, true, "privateStringStuff", "Hi There"));
        assertNullPointerException(() -> MethodUtils.invokeMethod(testBean, true, null, "Hi There"));
    }

    @Test
    void testInvokeMethodVarArgsNotUniqueResolvable() throws Exception {
        assertEquals("Boolean...", MethodUtils.invokeMethod(testBean, "varOverload", new Object[] { null }));
        assertEquals("Object...", MethodUtils.invokeMethod(testBean, "varOverload", (Object[]) null));
    }

    @Test
    void testInvokeMethodVarArgsOfInterface() throws Exception {
        // packagePrivateEmptyInterface
        assertEquals("PackagePrivateEmptyInterface...", MethodUtils.invokeMethod(testBean, "packagePrivateEmptyInterface",
                new PublicImpl1OfPackagePrivateEmptyInterface(), new PublicImpl2OfPackagePrivateEmptyInterface()));
        assertEquals("PackagePrivateEmptyInterface...", MethodUtils.invokeMethod(testBean, "packagePrivateEmptyInterface", new PackagePrivateEmptyInterface() {
            // empty
        }, new PackagePrivateEmptyInterface() {
            // empty
        }));
    }

    @Test
    void testInvokeMethodVarArgsUnboxingBooleanArray() throws Exception {
        final TestBean testBean = new TestBean();
        final boolean[] actual = (boolean[]) MethodUtils.invokeMethod(testBean, "unboxing", Boolean.TRUE, Boolean.FALSE);
        assertArrayEquals(new boolean[] { true, false }, actual);
        assertTrue(testBean.unboxBooleanArray);
    }

    @Test
    void testInvokeMethodVarArgsUnboxingByteArray() throws Exception {
        final TestBean testBean = new TestBean();
        final byte[] actual = (byte[]) MethodUtils.invokeMethod(testBean, "unboxing", Byte.valueOf((byte) 1), Byte.valueOf((byte) 2));
        assertArrayEquals(new byte[] { 1, 2 }, actual);
        assertTrue(testBean.unboxByteArray);
    }

    @Test
    void testInvokeMethodVarArgsUnboxingCharArray() throws Exception {
        final TestBean testBean = new TestBean();
        final char[] actual = (char[]) MethodUtils.invokeMethod(testBean, "unboxing", Character.valueOf((char) 1), Character.valueOf((char) 2));
        assertArrayEquals(new char[] { 1, 2 }, actual);
        assertTrue(testBean.unboxCharArray);
    }

    @Test
    void testInvokeMethodVarArgsUnboxingDoubleArray() throws Exception {
        final TestBean testBean = new TestBean();
        final double[] actual = (double[]) MethodUtils.invokeMethod(testBean, "unboxing", Double.valueOf(1), Double.valueOf(2));
        assertArrayEquals(new double[] { 1, 2 }, actual);
        assertTrue(testBean.unboxDoubleArray);
    }

    @Test
    void testInvokeMethodVarArgsUnboxingFloatArray() throws Exception {
        final TestBean testBean = new TestBean();
        final float[] actual = (float[]) MethodUtils.invokeMethod(testBean, "unboxing", Float.valueOf(1), Float.valueOf(2));
        assertArrayEquals(new float[] { 1, 2 }, actual);
        assertTrue(testBean.unboxFloatArray);
    }

    @Test
    void testInvokeMethodVarArgsUnboxingIntArray() throws Exception {
        final TestBean testBean = new TestBean();
        final int[] actual = (int[]) MethodUtils.invokeMethod(testBean, "unboxing", Integer.valueOf(1), Integer.valueOf(2));
        assertArrayEquals(new int[] { 1, 2 }, actual);
        assertTrue(testBean.unboxIntArray);
    }

    @Test
    void testInvokeMethodVarArgsUnboxingLongArray() throws Exception {
        final TestBean testBean = new TestBean();
        final long[] actual = (long[]) MethodUtils.invokeMethod(testBean, "unboxing", Long.valueOf(1), Long.valueOf(2));
        assertArrayEquals(new long[] { 1, 2 }, actual);
        assertTrue(testBean.unboxLongArray);
    }

    @Test
    void testInvokeMethodVarArgsUnboxingShortArray() throws Exception {
        final TestBean testBean = new TestBean();
        final short[] actual = (short[]) MethodUtils.invokeMethod(testBean, "unboxing", Short.valueOf((short) 1), Short.valueOf((short) 2));
        assertArrayEquals(new short[] { 1, 2 }, actual);
        assertTrue(testBean.unboxShortArray);
    }

    @Test
    void testInvokeStaticMethod() throws Exception {
        assertEquals("bar()", MethodUtils.invokeStaticMethod(TestBean.class, "bar"));
        assertEquals("bar()", MethodUtils.invokeStaticMethod(TestBean.class, "bar", (Object[]) ArrayUtils.EMPTY_CLASS_ARRAY));
        assertEquals("bar()", MethodUtils.invokeStaticMethod(TestBean.class, "bar", (Object[]) null));
        assertEquals("bar()", MethodUtils.invokeStaticMethod(TestBean.class, "bar", null, null));
        assertEquals("bar(String)", MethodUtils.invokeStaticMethod(TestBean.class, "bar", ""));
        assertEquals("bar(Object)", MethodUtils.invokeStaticMethod(TestBean.class, "bar", new Object()));
        assertEquals("bar(Object)", MethodUtils.invokeStaticMethod(TestBean.class, "bar", Boolean.TRUE));
        assertEquals("bar(Integer)", MethodUtils.invokeStaticMethod(TestBean.class, "bar", NumberUtils.INTEGER_ONE));
        assertEquals("bar(int)", MethodUtils.invokeStaticMethod(TestBean.class, "bar", NumberUtils.BYTE_ONE));
        assertEquals("static int", MethodUtils.invokeStaticMethod(TestBean.class, "staticInt", NumberUtils.BYTE_ONE));
        assertEquals("static int", MethodUtils.invokeStaticMethod(TestBean.class, "staticInt", NumberUtils.SHORT_ONE));
        assertEquals("static int", MethodUtils.invokeStaticMethod(TestBean.class, "staticInt", NumberUtils.INTEGER_ONE));
        assertEquals("static int", MethodUtils.invokeStaticMethod(TestBean.class, "staticInt", 'a'));
        assertEquals("bar(double)", MethodUtils.invokeStaticMethod(TestBean.class, "bar", NumberUtils.DOUBLE_ONE));
        assertEquals("bar(String...)", MethodUtils.invokeStaticMethod(TestBean.class, "bar", "a", "b"));
        assertEquals("bar(long...)", MethodUtils.invokeStaticMethod(TestBean.class, "bar", 1L, 2L));
        assertEquals("bar(long...)", MethodUtils.invokeStaticMethod(TestBean.class, "bar", (byte) 1, (byte) 2)); // widen
        assertEquals("bar(long...)", MethodUtils.invokeStaticMethod(TestBean.class, "bar", (short) 1, (short) 2)); // widen
        assertEquals("bar(long...)", MethodUtils.invokeStaticMethod(TestBean.class, "bar", 1, 2)); // widen
        assertEquals("bar(Integer, String...)", MethodUtils.invokeStaticMethod(TestBean.class, "bar", NumberUtils.INTEGER_ONE, "a", "b"));
        // You cannot widen a Short to an Integer in Java source, but you can a short to an int but this API declares an Integer, not an int.
        assertThrows(NoSuchMethodException.class,
                () -> assertEquals("bar(Integer, String...)", MethodUtils.invokeStaticMethod(TestBean.class, "bar", NumberUtils.SHORT_ONE, "a", "b"))); // widen
        TestBean.verify(new ImmutablePair<>("String...", new String[] { "x", "y" }),
                MethodUtils.invokeStaticMethod(TestBean.class, "varOverloadEchoStatic", "x", "y"));
        TestBean.verify(new ImmutablePair<>("Number...", new Number[] { 17, 23, 42 }),
                MethodUtils.invokeStaticMethod(TestBean.class, "varOverloadEchoStatic", 17, 23, 42));
        TestBean.verify(new ImmutablePair<>("String...", new String[] { "x", "y" }),
                MethodUtils.invokeStaticMethod(TestBean.class, "varOverloadEchoStatic", "x", "y"));
        TestBean.verify(new ImmutablePair<>("Number...", new Number[] { 17, 23, 42 }),
                MethodUtils.invokeStaticMethod(TestBean.class, "varOverloadEchoStatic", 17, 23, 42));
        assertThrows(NoSuchMethodException.class, () -> MethodUtils.invokeStaticMethod(TestBean.class, "does_not_exist"));
    }

    @Test
    void testInvokeStaticMethod1PlusVarArgs() throws Exception {
        // staticIntStringVarArg
        assertEquals("static int, String...", MethodUtils.invokeStaticMethod(TestBean.class, "staticIntStringVarArg", 1));
        assertEquals("static int, String...", MethodUtils.invokeStaticMethod(TestBean.class, "staticIntStringVarArg", 1, "s"));
        assertEquals("static int, String...", MethodUtils.invokeStaticMethod(TestBean.class, "staticIntStringVarArg", 1, "s1", "s2"));
        assertThrows(NoSuchMethodException.class, () -> MethodUtils.invokeStaticMethod(TestBean.class, "staticIntStringVarArg", 1, "s1", 5));
        // staticIntLongVarArg
        assertEquals("static int, long...", MethodUtils.invokeMethod(testBean, "staticIntLongVarArg", 1));
        assertEquals("static int, long...", MethodUtils.invokeMethod(testBean, "staticIntLongVarArg", 1, 2L));
        assertEquals("static int, long...", MethodUtils.invokeMethod(testBean, "staticIntLongVarArg", 1, 2L, 3L));
        assertThrows(NoSuchMethodException.class, () -> MethodUtils.invokeMethod(testBean, "staticIntLongVarArg", 1, "s1", 5));
        // staticIntIntVarArg
        assertEquals("static int, int...", MethodUtils.invokeMethod(testBean, "staticIntIntVarArg", 1));
        assertEquals("static int, int...", MethodUtils.invokeMethod(testBean, "staticIntIntVarArg", 1, 2));
        assertEquals("static int, int...", MethodUtils.invokeMethod(testBean, "staticIntIntVarArg", 1, 2, 3));
        assertThrows(NoSuchMethodException.class, () -> MethodUtils.invokeMethod(testBean, "staticIntIntVarArg", 1, "s1", 5));
    }

    @Test
    void testInvokeStaticMethodVarArgsOfInterface() throws Exception {
        // staticPackagePrivateEmptyInterface
        assertEquals("static PackagePrivateEmptyInterface...", MethodUtils.invokeStaticMethod(TestBean.class, "staticPackagePrivateEmptyInterface",
                new PublicImpl1OfPackagePrivateEmptyInterface(), new PublicImpl2OfPackagePrivateEmptyInterface()));
        assertEquals("static PackagePrivateEmptyInterface...",
                MethodUtils.invokeStaticMethod(TestBean.class, "staticPackagePrivateEmptyInterface", new PackagePrivateEmptyInterface() {
                    // empty
                }, new PackagePrivateEmptyInterface() {
                    // empty
                }));
    }

    @Test
    void testNullArgument() {
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "oneParameter", singletonArray(null), singletonArray(String.class));
    }

    @Test
    void testVarargsOverloadingResolution() {
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

    private String toString(final Class<?>[] c) {
        return Arrays.asList(c).toString();
    }
}
