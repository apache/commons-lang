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

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.math.NumberUtils;
import org.apache.commons.lang3.mutable.Mutable;
import org.apache.commons.lang3.mutable.MutableObject;
import org.apache.commons.lang3.ClassUtils.Interfaces;
import org.apache.commons.lang3.reflect.testbed.Annotated;
import org.apache.commons.lang3.reflect.testbed.GenericConsumer;
import org.apache.commons.lang3.reflect.testbed.GenericParent;
import org.apache.commons.lang3.reflect.testbed.StringParameterizedChild;
import org.junit.Before;
import org.junit.Test;

/**
 * Unit tests MethodUtils
 * @version $Id$
 */
public class MethodUtilsTest {
  
    private static interface PrivateInterface {}
    
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
        
        public static void oneParameterStatic(final String s) {
            // empty
        }

        @SuppressWarnings("unused")
        private void privateStuff() {
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

        public String foo(final String s) {
            return "foo(String)";
        }

        public String foo(final Object o) {
            return "foo(Object)";
        }
        
        public void oneParameter(final String s) {
            // empty
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
    private final Map<Class<?>, Class<?>[]> classCache = new HashMap<Class<?>, Class<?>[]>();

    @Before
    public void setUp() throws Exception {
        testBean = new TestBean();
        classCache.clear();
    }

    @Test
    public void testConstructor() throws Exception {
        assertNotNull(MethodUtils.class.newInstance());
    }

    @Test
    public void testInvokeMethod() throws Exception {
        assertEquals("foo()", MethodUtils.invokeMethod(testBean, "foo",
                (Object[]) ArrayUtils.EMPTY_CLASS_ARRAY));
        assertEquals("foo()", MethodUtils.invokeMethod(testBean, "foo",
                (Object[]) null));
        assertEquals("foo()", MethodUtils.invokeMethod(testBean, "foo", 
                (Object[]) null, (Class<?>[]) null));
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
        assertEquals("foo(double)", MethodUtils.invokeMethod(testBean, "foo",
                NumberUtils.LONG_ONE));
        assertEquals("foo(double)", MethodUtils.invokeMethod(testBean, "foo",
                NumberUtils.DOUBLE_ONE));
    }

    @Test
    public void testInvokeExactMethod() throws Exception {
        assertEquals("foo()", MethodUtils.invokeExactMethod(testBean, "foo",
                (Object[]) ArrayUtils.EMPTY_CLASS_ARRAY));
        assertEquals("foo()", MethodUtils.invokeExactMethod(testBean, "foo",
                (Object[]) null));
        assertEquals("foo()", MethodUtils.invokeExactMethod(testBean, "foo", 
                (Object[]) null, (Class<?>[]) null));
        assertEquals("foo(String)", MethodUtils.invokeExactMethod(testBean,
                "foo", ""));
        assertEquals("foo(Object)", MethodUtils.invokeExactMethod(testBean,
                "foo", new Object()));
        assertEquals("foo(Integer)", MethodUtils.invokeExactMethod(testBean,
                "foo", NumberUtils.INTEGER_ONE));
        assertEquals("foo(double)", MethodUtils.invokeExactMethod(testBean,
                "foo", new Object[] { NumberUtils.DOUBLE_ONE },
                new Class[] { Double.TYPE }));

        try {
            MethodUtils
                    .invokeExactMethod(testBean, "foo", NumberUtils.BYTE_ONE);
            fail("should throw NoSuchMethodException");
        } catch (final NoSuchMethodException e) {
        }
        try {
            MethodUtils
                    .invokeExactMethod(testBean, "foo", NumberUtils.LONG_ONE);
            fail("should throw NoSuchMethodException");
        } catch (final NoSuchMethodException e) {
        }
        try {
            MethodUtils.invokeExactMethod(testBean, "foo", Boolean.TRUE);
            fail("should throw NoSuchMethodException");
        } catch (final NoSuchMethodException e) {
        }
    }

    @Test
    public void testInvokeStaticMethod() throws Exception {
        assertEquals("bar()", MethodUtils.invokeStaticMethod(TestBean.class,
                "bar", (Object[]) ArrayUtils.EMPTY_CLASS_ARRAY));
        assertEquals("bar()", MethodUtils.invokeStaticMethod(TestBean.class,
                "bar", (Object[]) null));
        assertEquals("bar()", MethodUtils.invokeStaticMethod(TestBean.class,
                "bar", (Object[]) null, (Class<?>[]) null));
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
                TestBean.class, "bar", NumberUtils.LONG_ONE));
        assertEquals("bar(double)", MethodUtils.invokeStaticMethod(
                TestBean.class, "bar", NumberUtils.DOUBLE_ONE));
        
        try {
            MethodUtils.invokeStaticMethod(TestBean.class, "does_not_exist");
            fail("should throw NoSuchMethodException");
        } catch (final NoSuchMethodException e) {
        }
    }

    @Test
    public void testInvokeExactStaticMethod() throws Exception {
        assertEquals("bar()", MethodUtils.invokeExactStaticMethod(TestBean.class,
                "bar", (Object[]) ArrayUtils.EMPTY_CLASS_ARRAY));
        assertEquals("bar()", MethodUtils.invokeExactStaticMethod(TestBean.class,
                "bar", (Object[]) null));
        assertEquals("bar()", MethodUtils.invokeExactStaticMethod(TestBean.class,
                "bar", (Object[]) null, (Class<?>[]) null));
        assertEquals("bar(String)", MethodUtils.invokeExactStaticMethod(
                TestBean.class, "bar", ""));
        assertEquals("bar(Object)", MethodUtils.invokeExactStaticMethod(
                TestBean.class, "bar", new Object()));
        assertEquals("bar(Integer)", MethodUtils.invokeExactStaticMethod(
                TestBean.class, "bar", NumberUtils.INTEGER_ONE));
        assertEquals("bar(double)", MethodUtils.invokeExactStaticMethod(
                TestBean.class, "bar", new Object[] { NumberUtils.DOUBLE_ONE },
                new Class[] { Double.TYPE }));

        try {
            MethodUtils.invokeExactStaticMethod(TestBean.class, "bar",
                    NumberUtils.BYTE_ONE);
            fail("should throw NoSuchMethodException");
        } catch (final NoSuchMethodException e) {
        }
        try {
            MethodUtils.invokeExactStaticMethod(TestBean.class, "bar",
                    NumberUtils.LONG_ONE);
            fail("should throw NoSuchMethodException");
        } catch (final NoSuchMethodException e) {
        }
        try {
            MethodUtils.invokeExactStaticMethod(TestBean.class, "bar",
                    Boolean.TRUE);
            fail("should throw NoSuchMethodException");
        } catch (final NoSuchMethodException e) {
        }
    }

    @Test
    public void testGetAccessibleInterfaceMethod() throws Exception {
        final Class<?>[][] p = { ArrayUtils.EMPTY_CLASS_ARRAY, null };
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
    public void testGetAccessibleInterfaceMethodFromDescription()
            throws Exception {
        final Class<?>[][] p = { ArrayUtils.EMPTY_CLASS_ARRAY, null };
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
    public void testGetAccessiblePublicMethodFromDescription() throws Exception {
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
   public void testGetMatchingAccessibleMethod() throws Exception {
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
                singletonArray(Long.class), singletonArray(Double.TYPE));
        expectMatchingAccessibleMethodParameterTypes(TestBean.class, "foo",
                singletonArray(Long.TYPE), singletonArray(Double.TYPE));
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
        final Method[] annotatedMethods = new Method[]{
                MethodUtilsTest.class.getMethod("testGetMethodsWithAnnotation"),
                MethodUtilsTest.class.getMethod("testGetMethodsListWithAnnotation")
        };
        assertArrayEquals(annotatedMethods, MethodUtils.getMethodsWithAnnotation(MethodUtilsTest.class, Annotated.class));
    }

    @Test(expected = IllegalArgumentException.class)
    public void testGetMethodsWithAnnotationIllegalArgumentException1() {
        MethodUtils.getMethodsWithAnnotation(FieldUtilsTest.class, null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testGetMethodsWithAnnotationIllegalArgumentException2() {
        MethodUtils.getMethodsWithAnnotation(null, Annotated.class);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testGetMethodsWithAnnotationIllegalArgumentException3() {
        MethodUtils.getMethodsWithAnnotation(null, null);
    }

    @Test
    @Annotated
    public void testGetMethodsListWithAnnotation() throws NoSuchMethodException {
        assertEquals(0, MethodUtils.getMethodsListWithAnnotation(Object.class, Annotated.class).size());
        final List<Method> annotatedMethods = Arrays.asList(
                MethodUtilsTest.class.getMethod("testGetMethodsWithAnnotation"),
                MethodUtilsTest.class.getMethod("testGetMethodsListWithAnnotation")
        );
        final List<Method> methodUtilsTestAnnotatedFields = MethodUtils.getMethodsListWithAnnotation(MethodUtilsTest.class, Annotated.class);
        assertEquals(annotatedMethods.size(), methodUtilsTestAnnotatedFields.size());
        assertTrue(methodUtilsTestAnnotatedFields.contains(annotatedMethods.get(0)));
        assertTrue(methodUtilsTestAnnotatedFields.contains(annotatedMethods.get(1)));
    }

    @Test(expected = IllegalArgumentException.class)
    public void testGetMethodsListWithAnnotationIllegalArgumentException1() {
        MethodUtils.getMethodsListWithAnnotation(FieldUtilsTest.class, null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testGetMethodsListWithAnnotationIllegalArgumentException2() {
        MethodUtils.getMethodsListWithAnnotation(null, Annotated.class);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testGetMethodsListWithAnnotationIllegalArgumentException3() {
        MethodUtils.getMethodsListWithAnnotation(null, null);
    }
    
    private void expectMatchingAccessibleMethodParameterTypes(final Class<?> cls,
            final String methodName, final Class<?>[] requestTypes, final Class<?>[] actualTypes) {
        final Method m = MethodUtils.getMatchingAccessibleMethod(cls, methodName,
                requestTypes);
        assertTrue(toString(m.getParameterTypes()) + " not equals "
                + toString(actualTypes), Arrays.equals(actualTypes, m
                .getParameterTypes()));
    }

    private String toString(final Class<?>[] c) {
        return Arrays.asList(c).toString();
    }

    private Class<?>[] singletonArray(final Class<?> c) {
        Class<?>[] result = classCache.get(c);
        if (result == null) {
            result = new Class[] { c };
            classCache.put(c, result);
        }
        return result;
    }

    public static class InheritanceBean {
        public void testOne(final Object obj) {}
        public void testOne(final GrandParentObject obj) {}
        public void testOne(final ParentObject obj) {}
        public void testTwo(final Object obj) {}
        public void testTwo(final GrandParentObject obj) {}
        public void testTwo(final ChildInterface obj) {}
    }
    
    interface ChildInterface {}    
    public static class GrandParentObject {}
    public static class ParentObject extends GrandParentObject {}
    public static class ChildObject extends ParentObject implements ChildInterface {}

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
}
