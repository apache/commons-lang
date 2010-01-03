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

import java.lang.reflect.Constructor;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import junit.framework.TestCase;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.math.NumberUtils;

/**
 * Unit tests ConstructorUtils
 * @author mbenson
 * @version $Id$
 */
public class ConstructorUtilsTest extends TestCase {
    public static class TestBean {
        private String toString;

        public TestBean() {
            toString = "()";
        }

        public TestBean(int i) {
            toString = "(int)";
        }

        public TestBean(Integer i) {
            toString = "(Integer)";
        }

        public TestBean(double d) {
            toString = "(double)";
        }

        public TestBean(String s) {
            toString = "(String)";
        }

        public TestBean(Object o) {
            toString = "(Object)";
        }

        @Override
        public String toString() {
            return toString;
        }
    }

    private static class PrivateClass {
        @SuppressWarnings("unused")
        public PrivateClass() {
        }
    }

    private Map<Class<?>, Class<?>[]> classCache;

    public ConstructorUtilsTest(String name) {
        super(name);
        classCache = new HashMap<Class<?>, Class<?>[]>();
    }

    @Override
    protected void setUp() throws Exception {
        super.setUp();
        classCache.clear();
    }

    public void testConstructor() throws Exception {
        assertNotNull(MethodUtils.class.newInstance());
    }

    public void testInvokeConstructor() throws Exception {
        assertEquals("()", ConstructorUtils.invokeConstructor(TestBean.class,
                ArrayUtils.EMPTY_CLASS_ARRAY).toString());
        assertEquals("()", ConstructorUtils.invokeConstructor(TestBean.class,
                (Class[]) null).toString());
        assertEquals("(String)", ConstructorUtils.invokeConstructor(
                TestBean.class, "").toString());
        assertEquals("(Object)", ConstructorUtils.invokeConstructor(
                TestBean.class, new Object()).toString());
        assertEquals("(Object)", ConstructorUtils.invokeConstructor(
                TestBean.class, Boolean.TRUE).toString());
        assertEquals("(Integer)", ConstructorUtils.invokeConstructor(
                TestBean.class, NumberUtils.INTEGER_ONE).toString());
        assertEquals("(int)", ConstructorUtils.invokeConstructor(
                TestBean.class, NumberUtils.BYTE_ONE).toString());
        assertEquals("(double)", ConstructorUtils.invokeConstructor(
                TestBean.class, NumberUtils.LONG_ONE).toString());
        assertEquals("(double)", ConstructorUtils.invokeConstructor(
                TestBean.class, NumberUtils.DOUBLE_ONE).toString());
    }

    public void testInvokeExactConstructor() throws Exception {
        assertEquals("()", ConstructorUtils.invokeExactConstructor(
                TestBean.class, ArrayUtils.EMPTY_CLASS_ARRAY).toString());
        assertEquals("()", ConstructorUtils.invokeExactConstructor(
                TestBean.class, (Class[]) null).toString());
        assertEquals("(String)", ConstructorUtils.invokeExactConstructor(
                TestBean.class, "").toString());
        assertEquals("(Object)", ConstructorUtils.invokeExactConstructor(
                TestBean.class, new Object()).toString());
        assertEquals("(Integer)", ConstructorUtils.invokeExactConstructor(
                TestBean.class, NumberUtils.INTEGER_ONE).toString());
        assertEquals("(double)", ConstructorUtils.invokeExactConstructor(
                TestBean.class, new Object[] { NumberUtils.DOUBLE_ONE },
                new Class[] { Double.TYPE }).toString());

        try {
            ConstructorUtils.invokeExactConstructor(TestBean.class,
                    NumberUtils.BYTE_ONE);
            fail("should throw NoSuchMethodException");
        } catch (NoSuchMethodException e) {
        }
        try {
            ConstructorUtils.invokeExactConstructor(TestBean.class,
                    NumberUtils.LONG_ONE);
            fail("should throw NoSuchMethodException");
        } catch (NoSuchMethodException e) {
        }
        try {
            ConstructorUtils.invokeExactConstructor(TestBean.class,
                    Boolean.TRUE);
            fail("should throw NoSuchMethodException");
        } catch (NoSuchMethodException e) {
        }
    }

    public void testGetAccessibleConstructor() throws Exception {
        assertNotNull(ConstructorUtils.getAccessibleConstructor(Object.class
                .getConstructor(ArrayUtils.EMPTY_CLASS_ARRAY)));
        assertNull(ConstructorUtils.getAccessibleConstructor(PrivateClass.class
                .getConstructor(ArrayUtils.EMPTY_CLASS_ARRAY)));
    }

    public void testGetAccessibleConstructorFromDescription() throws Exception {
        assertNotNull(ConstructorUtils.getAccessibleConstructor(Object.class,
                ArrayUtils.EMPTY_CLASS_ARRAY));
        assertNull(ConstructorUtils.getAccessibleConstructor(
                PrivateClass.class, ArrayUtils.EMPTY_CLASS_ARRAY));
    }

    public void testGetMatchingAccessibleMethod() throws Exception {
        expectMatchingAccessibleConstructorParameterTypes(TestBean.class,
                ArrayUtils.EMPTY_CLASS_ARRAY, ArrayUtils.EMPTY_CLASS_ARRAY);
        expectMatchingAccessibleConstructorParameterTypes(TestBean.class, null,
                ArrayUtils.EMPTY_CLASS_ARRAY);
        expectMatchingAccessibleConstructorParameterTypes(TestBean.class,
                singletonArray(String.class), singletonArray(String.class));
        expectMatchingAccessibleConstructorParameterTypes(TestBean.class,
                singletonArray(Object.class), singletonArray(Object.class));
        expectMatchingAccessibleConstructorParameterTypes(TestBean.class,
                singletonArray(Boolean.class), singletonArray(Object.class));
        expectMatchingAccessibleConstructorParameterTypes(TestBean.class,
                singletonArray(Byte.class), singletonArray(Integer.TYPE));
        expectMatchingAccessibleConstructorParameterTypes(TestBean.class,
                singletonArray(Byte.TYPE), singletonArray(Integer.TYPE));
        expectMatchingAccessibleConstructorParameterTypes(TestBean.class,
                singletonArray(Short.class), singletonArray(Integer.TYPE));
        expectMatchingAccessibleConstructorParameterTypes(TestBean.class,
                singletonArray(Short.TYPE), singletonArray(Integer.TYPE));
        expectMatchingAccessibleConstructorParameterTypes(TestBean.class,
                singletonArray(Character.class), singletonArray(Integer.TYPE));
        expectMatchingAccessibleConstructorParameterTypes(TestBean.class,
                singletonArray(Character.TYPE), singletonArray(Integer.TYPE));
        expectMatchingAccessibleConstructorParameterTypes(TestBean.class,
                singletonArray(Integer.class), singletonArray(Integer.class));
        expectMatchingAccessibleConstructorParameterTypes(TestBean.class,
                singletonArray(Integer.TYPE), singletonArray(Integer.TYPE));
        expectMatchingAccessibleConstructorParameterTypes(TestBean.class,
                singletonArray(Long.class), singletonArray(Double.TYPE));
        expectMatchingAccessibleConstructorParameterTypes(TestBean.class,
                singletonArray(Long.TYPE), singletonArray(Double.TYPE));
        expectMatchingAccessibleConstructorParameterTypes(TestBean.class,
                singletonArray(Float.class), singletonArray(Double.TYPE));
        expectMatchingAccessibleConstructorParameterTypes(TestBean.class,
                singletonArray(Float.TYPE), singletonArray(Double.TYPE));
        expectMatchingAccessibleConstructorParameterTypes(TestBean.class,
                singletonArray(Double.class), singletonArray(Double.TYPE));
        expectMatchingAccessibleConstructorParameterTypes(TestBean.class,
                singletonArray(Double.TYPE), singletonArray(Double.TYPE));
    }

    private void expectMatchingAccessibleConstructorParameterTypes(Class<?> cls,
            Class<?>[] requestTypes, Class<?>[] actualTypes) {
        Constructor<?> c = ConstructorUtils.getMatchingAccessibleConstructor(cls,
                requestTypes);
        assertTrue(toString(c.getParameterTypes()) + " not equals "
                + toString(actualTypes), Arrays.equals(actualTypes, c
                .getParameterTypes()));
    }

    private String toString(Class<?>[] c) {
        return Arrays.asList(c).toString();
    }

    private Class<?>[] singletonArray(Class<?> c) {
        Class<?>[] result = classCache.get(c);
        if (result == null) {
            result = new Class[] { c };
            classCache.put(c, result);
        }
        return result;
    }

}
