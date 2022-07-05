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

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.lang.reflect.Constructor;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.math.NumberUtils;
import org.apache.commons.lang3.mutable.MutableObject;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * Unit tests ConstructorUtils
 */
public class ConstructorUtilsTest extends AbstractLangTest {
    public static class TestBean {
        private final String toString;
        final String[] varArgs;

        public TestBean() {
            toString = "()";
            varArgs = null;
        }

        public TestBean(final int i) {
            toString = "(int)";
            varArgs = null;
        }

        public TestBean(final Integer i) {
            toString = "(Integer)";
            varArgs = null;
        }

        public TestBean(final double d) {
            toString = "(double)";
            varArgs = null;
        }

        public TestBean(final String s) {
            toString = "(String)";
            varArgs = null;
        }

        public TestBean(final Object o) {
            toString = "(Object)";
            varArgs = null;
        }

        public TestBean(final String... s) {
            toString = "(String...)";
            varArgs = s;
        }

        public TestBean(final BaseClass bc, final String... s) {
            toString = "(BaseClass, String...)";
            varArgs = s;
        }

        public TestBean(final Integer i, final String... s) {
            toString = "(Integer, String...)";
            varArgs = s;
        }

        public TestBean(final Integer first, final int... args) {
            toString = "(Integer, String...)";
            varArgs = new String[args.length];
            for (int i = 0; i < args.length; ++i) {
                varArgs[i] = Integer.toString(args[i]);
            }
        }

        @Override
        public String toString() {
            return toString;
        }

        void verify(final String str, final String[] args) {
          assertEquals(str, toString);
          assertArrayEquals(args, varArgs);
        }
    }

    private static class BaseClass {}

    private static class SubClass extends BaseClass {}

    static class PrivateClass {
        @SuppressWarnings("unused")
        public PrivateClass() {
        }

        @SuppressWarnings("unused")
        public static class PublicInnerClass {
            public PublicInnerClass() {
            }
        }
    }

    private final Map<Class<?>, Class<?>[]> classCache;

    public ConstructorUtilsTest() {
        classCache = new HashMap<>();
    }


    @BeforeEach
    public void setUp() {
        classCache.clear();
    }

    @Test
    public void testConstructor() throws Exception {
        assertNotNull(MethodUtils.class.newInstance());
    }

    @Test
    public void testInvokeConstructor() throws Exception {
        assertEquals("()", ConstructorUtils.invokeConstructor(TestBean.class,
                (Object[]) ArrayUtils.EMPTY_CLASS_ARRAY).toString());
        assertEquals("()", ConstructorUtils.invokeConstructor(TestBean.class,
                (Object[]) null).toString());
        assertEquals("()", ConstructorUtils.invokeConstructor(TestBean.class).toString());
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
        ConstructorUtils.invokeConstructor(TestBean.class, NumberUtils.INTEGER_ONE)
          .verify("(Integer)", null);
        ConstructorUtils.invokeConstructor(TestBean.class, "a", "b")
          .verify("(String...)", new String[]{"a", "b"});
        ConstructorUtils.invokeConstructor(TestBean.class, NumberUtils.INTEGER_ONE, "a", "b")
          .verify("(Integer, String...)", new String[]{"a", "b"});
        ConstructorUtils.invokeConstructor(TestBean.class, new SubClass(), new String[]{"a", "b"})
          .verify("(BaseClass, String...)", new String[]{"a", "b"});
    }

    @Test
    public void testInvokeExactConstructor() throws Exception {
        assertEquals("()", ConstructorUtils.invokeExactConstructor(
                TestBean.class, (Object[]) ArrayUtils.EMPTY_CLASS_ARRAY).toString());
        assertEquals("()", ConstructorUtils.invokeExactConstructor(
                TestBean.class, (Object[]) null).toString());
        assertEquals("(String)", ConstructorUtils.invokeExactConstructor(
                TestBean.class, "").toString());
        assertEquals("(Object)", ConstructorUtils.invokeExactConstructor(
                TestBean.class, new Object()).toString());
        assertEquals("(Integer)", ConstructorUtils.invokeExactConstructor(
                TestBean.class, NumberUtils.INTEGER_ONE).toString());
        assertEquals("(double)", ConstructorUtils.invokeExactConstructor(
                TestBean.class, new Object[] { NumberUtils.DOUBLE_ONE },
                new Class[] { Double.TYPE }).toString());

        assertThrows(
                NoSuchMethodException.class,
                () -> ConstructorUtils.invokeExactConstructor(TestBean.class, NumberUtils.BYTE_ONE));
        assertThrows(
                NoSuchMethodException.class,
                () -> ConstructorUtils.invokeExactConstructor(TestBean.class, NumberUtils.LONG_ONE));
        assertThrows(
                NoSuchMethodException.class,
                () -> ConstructorUtils.invokeExactConstructor(TestBean.class, Boolean.TRUE));
    }

    @Test
    public void testGetAccessibleConstructor() throws Exception {
        assertNotNull(ConstructorUtils.getAccessibleConstructor(Object.class
                .getConstructor(ArrayUtils.EMPTY_CLASS_ARRAY)));
        assertNull(ConstructorUtils.getAccessibleConstructor(PrivateClass.class
                .getConstructor(ArrayUtils.EMPTY_CLASS_ARRAY)));
        assertNull(ConstructorUtils.getAccessibleConstructor(PrivateClass.PublicInnerClass.class));
    }

    @Test
    public void testGetAccessibleConstructorFromDescription() {
        assertNotNull(ConstructorUtils.getAccessibleConstructor(Object.class,
                ArrayUtils.EMPTY_CLASS_ARRAY));
        assertNull(ConstructorUtils.getAccessibleConstructor(
                PrivateClass.class, ArrayUtils.EMPTY_CLASS_ARRAY));
    }

    @Test
    public void testGetMatchingAccessibleMethod() {
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
        expectMatchingAccessibleConstructorParameterTypes(TestBean.class,
                new Class<?>[]{SubClass.class, String[].class},
                new Class<?>[]{BaseClass.class, String[].class});
    }

    @Test
    public void testNullArgument() {
        expectMatchingAccessibleConstructorParameterTypes(MutableObject.class,
                singletonArray(null), singletonArray(Object.class));
    }

    private void expectMatchingAccessibleConstructorParameterTypes(final Class<?> cls,
            final Class<?>[] requestTypes, final Class<?>[] actualTypes) {
        final Constructor<?> c = ConstructorUtils.getMatchingAccessibleConstructor(cls,
                requestTypes);
        assertArrayEquals(actualTypes, c.getParameterTypes(), toString(c.getParameterTypes()) + " not equals " + toString(actualTypes));
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

    @Test
    public void testVarArgsUnboxing() throws Exception {
        final TestBean testBean = ConstructorUtils.invokeConstructor(
                TestBean.class, Integer.valueOf(1), Integer.valueOf(2), Integer.valueOf(3));

        assertArrayEquals(new String[]{"2", "3"}, testBean.varArgs);
    }

}
