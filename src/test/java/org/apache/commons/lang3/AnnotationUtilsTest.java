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
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTimeoutPreemptively;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Proxy;
import java.time.Duration;
import java.util.Collection;
import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 */
public class AnnotationUtilsTest extends AbstractLangTest {
    @Retention(RetentionPolicy.RUNTIME)
    public @interface NestAnnotation {
        boolean booleanValue();
        boolean[] booleanValues();
        byte byteValue();
        byte[] byteValues();
        char charValue();
        char[] charValues();
        double doubleValue();
        double[] doubleValues();
        float floatValue();
        float[] floatValues();
        int intValue();
        int[] intValues();
        long longValue();
        long[] longValues();
        short shortValue();
        short[] shortValues();
        Stooge stooge();
        Stooge[] stooges();
        String string();
        String[] strings();
        Class<?> type();
        Class<?>[] types();
    }

    public enum Stooge {
        MOE, LARRY, CURLY, JOE, SHEMP
    }

    @Target(ElementType.FIELD)
    @Retention(RetentionPolicy.RUNTIME)
    public @interface TestAnnotation {
        boolean booleanValue();
        boolean[] booleanValues();
        byte byteValue();
        byte[] byteValues();
        char charValue();
        char[] charValues();
        double doubleValue();
        double[] doubleValues();
        float floatValue();
        float[] floatValues();
        int intValue();
        int[] intValues();
        long longValue();
        long[] longValues();
        NestAnnotation nest();
        NestAnnotation[] nests();
        short shortValue();
        short[] shortValues();
        Stooge stooge();
        Stooge[] stooges();
        String string();
        String[] strings();
        Class<?> type();
        Class<?>[] types();
    }

    @Retention(RetentionPolicy.RUNTIME)
    @Target({ElementType.METHOD})
    public @interface TestMethodAnnotation {
        final class None extends Throwable {

            private static final long serialVersionUID = 1L;
        }

        Class<? extends Throwable> expected() default None.class;

        long timeout() default 0L;
    }

    @TestAnnotation(
            booleanValue = false,
            booleanValues = { false },
            byteValue = 0,
            byteValues = { 0 },
            charValue = 0,
            charValues = { 0 },
            doubleValue = 0,
            doubleValues = { 0 },
            floatValue = 0,
            floatValues = { 0 },
            intValue = 0,
            intValues = { 0 },
            longValue = 0,
            longValues = { 0 },
            nest = @NestAnnotation(
                    booleanValue = false,
                    booleanValues = { false },
                    byteValue = 0,
                    byteValues = { 0 },
                    charValue = 0,
                    charValues = { 0 },
                    doubleValue = 0,
                    doubleValues = { 0 },
                    floatValue = 0,
                    floatValues = { 0 },
                    intValue = 0,
                    intValues = { 0 },
                    longValue = 0,
                    longValues = { 0 },
                    shortValue = 0,
                    shortValues = { 0 },
                    stooge = Stooge.CURLY,
                    stooges = { Stooge.MOE, Stooge.LARRY, Stooge.SHEMP },
                    string = "",
                    strings = { "" },
                    type = Object.class,
                    types = { Object.class }
            ),
            nests = {
                @NestAnnotation(
                        booleanValue = false,
                        booleanValues = { false },
                        byteValue = 0,
                        byteValues = { 0 },
                        charValue = 0,
                        charValues = { 0 },
                        doubleValue = 0,
                        doubleValues = { 0 },
                        floatValue = 0,
                        floatValues = { 0 },
                        intValue = 0,
                        intValues = { 0 },
                        longValue = 0,
                        longValues = { 0 },
                        shortValue = 0,
                        shortValues = { 0 },
                        stooge = Stooge.CURLY,
                        stooges = { Stooge.MOE, Stooge.LARRY, Stooge.SHEMP },
                        string = "",
                        strings = { "" },
                        type = Object[].class,
                        types = { Object[].class }
                )
            },
            shortValue = 0,
            shortValues = { 0 },
            stooge = Stooge.SHEMP,
            stooges = { Stooge.MOE, Stooge.LARRY, Stooge.CURLY },
            string = "",
            strings = { "" },
            type = Object.class,
            types = { Object.class }
    )
    public Object dummy1;

    @TestAnnotation(
            booleanValue = false,
            booleanValues = { false },
            byteValue = 0,
            byteValues = { 0 },
            charValue = 0,
            charValues = { 0 },
            doubleValue = 0,
            doubleValues = { 0 },
            floatValue = 0,
            floatValues = { 0 },
            intValue = 0,
            intValues = { 0 },
            longValue = 0,
            longValues = { 0 },
            nest = @NestAnnotation(
                    booleanValue = false,
                    booleanValues = { false },
                    byteValue = 0,
                    byteValues = { 0 },
                    charValue = 0,
                    charValues = { 0 },
                    doubleValue = 0,
                    doubleValues = { 0 },
                    floatValue = 0,
                    floatValues = { 0 },
                    intValue = 0,
                    intValues = { 0 },
                    longValue = 0,
                    longValues = { 0 },
                    shortValue = 0,
                    shortValues = { 0 },
                    stooge = Stooge.CURLY,
                    stooges = { Stooge.MOE, Stooge.LARRY, Stooge.SHEMP },
                    string = "",
                    strings = { "" },
                    type = Object.class,
                    types = { Object.class }
            ),
            nests = {
                @NestAnnotation(
                        booleanValue = false,
                        booleanValues = { false },
                        byteValue = 0,
                        byteValues = { 0 },
                        charValue = 0,
                        charValues = { 0 },
                        doubleValue = 0,
                        doubleValues = { 0 },
                        floatValue = 0,
                        floatValues = { 0 },
                        intValue = 0,
                        intValues = { 0 },
                        longValue = 0,
                        longValues = { 0 },
                        shortValue = 0,
                        shortValues = { 0 },
                        stooge = Stooge.CURLY,
                        stooges = { Stooge.MOE, Stooge.LARRY, Stooge.SHEMP },
                        string = "",
                        strings = { "" },
                        type = Object[].class,
                        types = { Object[].class }
                )
            },
            shortValue = 0,
            shortValues = { 0 },
            stooge = Stooge.SHEMP,
            stooges = { Stooge.MOE, Stooge.LARRY, Stooge.CURLY },
            string = "",
            strings = { "" },
            type = Object.class,
            types = { Object.class }
    )
    public Object dummy2;

    @TestAnnotation(
            booleanValue = false,
            booleanValues = { false },
            byteValue = 0,
            byteValues = { 0 },
            charValue = 0,
            charValues = { 0 },
            doubleValue = 0,
            doubleValues = { 0 },
            floatValue = 0,
            floatValues = { 0 },
            intValue = 0,
            intValues = { 0 },
            longValue = 0,
            longValues = { 0 },
            nest = @NestAnnotation(
                    booleanValue = false,
                    booleanValues = { false },
                    byteValue = 0,
                    byteValues = { 0 },
                    charValue = 0,
                    charValues = { 0 },
                    doubleValue = 0,
                    doubleValues = { 0 },
                    floatValue = 0,
                    floatValues = { 0 },
                    intValue = 0,
                    intValues = { 0 },
                    longValue = 0,
                    longValues = { 0 },
                    shortValue = 0,
                    shortValues = { 0 },
                    stooge = Stooge.CURLY,
                    stooges = { Stooge.MOE, Stooge.LARRY, Stooge.SHEMP },
                    string = "",
                    strings = { "" },
                    type = Object.class,
                    types = { Object.class }
            ),
            nests = {
                @NestAnnotation(
                        booleanValue = false,
                        booleanValues = { false },
                        byteValue = 0,
                        byteValues = { 0 },
                        charValue = 0,
                        charValues = { 0 },
                        doubleValue = 0,
                        doubleValues = { 0 },
                        floatValue = 0,
                        floatValues = { 0 },
                        intValue = 0,
                        intValues = { 0 },
                        longValue = 0,
                        longValues = { 0 },
                        shortValue = 0,
                        shortValues = { 0 },
                        stooge = Stooge.CURLY,
                        stooges = { Stooge.MOE, Stooge.LARRY, Stooge.SHEMP },
                        string = "",
                        strings = { "" },
                        type = Object[].class,
                        types = { Object[].class }
                ),
                //add a second NestAnnotation to break equality:
                @NestAnnotation(
                        booleanValue = false,
                        booleanValues = { false },
                        byteValue = 0,
                        byteValues = { 0 },
                        charValue = 0,
                        charValues = { 0 },
                        doubleValue = 0,
                        doubleValues = { 0 },
                        floatValue = 0,
                        floatValues = { 0 },
                        intValue = 0,
                        intValues = { 0 },
                        longValue = 0,
                        longValues = { 0 },
                        shortValue = 0,
                        shortValues = { 0 },
                        stooge = Stooge.CURLY,
                        stooges = { Stooge.MOE, Stooge.LARRY, Stooge.SHEMP },
                        string = "",
                        strings = { "" },
                        type = Object[].class,
                        types = { Object[].class }
                )
            },
            shortValue = 0,
            shortValues = { 0 },
            stooge = Stooge.SHEMP,
            stooges = { Stooge.MOE, Stooge.LARRY, Stooge.CURLY },
            string = "",
            strings = { "" },
            type = Object.class,
            types = { Object.class }
    )
    public Object dummy3;

    @NestAnnotation(
            booleanValue = false,
            booleanValues = { false },
            byteValue = 0,
            byteValues = { 0 },
            charValue = 0,
            charValues = { 0 },
            doubleValue = 0,
            doubleValues = { 0 },
            floatValue = 0,
            floatValues = { 0 },
            intValue = 0,
            intValues = { 0 },
            longValue = 0,
            longValues = { 0 },
            shortValue = 0,
            shortValues = { 0 },
            stooge = Stooge.CURLY,
            stooges = { Stooge.MOE, Stooge.LARRY, Stooge.SHEMP },
            string = "",
            strings = { "" },
            type = Object[].class,
            types = { Object[].class }
    )
    public Object dummy4;

    private Field field1;
    private Field field2;
    private Field field3;
    private Field field4;

    @BeforeEach
    public void setup() throws Exception {
        field1 = getClass().getDeclaredField("dummy1");
        field2 = getClass().getDeclaredField("dummy2");
        field3 = getClass().getDeclaredField("dummy3");
        field4 = getClass().getDeclaredField("dummy4");
    }

    @Test
    public void testAnnotationsOfDifferingTypes() {
        assertFalse(AnnotationUtils.equals(field1.getAnnotation(TestAnnotation.class), field4.getAnnotation(NestAnnotation.class)));
        assertFalse(AnnotationUtils.equals(field4.getAnnotation(NestAnnotation.class), field1.getAnnotation(TestAnnotation.class)));
    }

    @Test
    public void testBothArgsNull() {
        assertTrue(AnnotationUtils.equals(null, null));
    }

    @Test
    public void testEquivalence() {
        assertTrue(AnnotationUtils.equals(field1.getAnnotation(TestAnnotation.class), field2.getAnnotation(TestAnnotation.class)));
        assertTrue(AnnotationUtils.equals(field2.getAnnotation(TestAnnotation.class), field1.getAnnotation(TestAnnotation.class)));
    }

    @Test
    public void testGeneratedAnnotationEquivalentToRealAnnotation() {
        assertTimeoutPreemptively(Duration.ofSeconds(666L), () -> {
            final Test real = getClass().getDeclaredMethod(
                    "testGeneratedAnnotationEquivalentToRealAnnotation").getAnnotation(Test.class);

            final InvocationHandler generatedTestInvocationHandler = (proxy, method, args) -> {
                if ("equals".equals(method.getName()) && method.getParameterTypes().length == 1) {
                    return Boolean.valueOf(proxy == args[0]);
                }
                if ("hashCode".equals(method.getName()) && method.getParameterTypes().length == 0) {
                    return Integer.valueOf(System.identityHashCode(proxy));
                }
                if ("toString".equals(method.getName()) && method.getParameterTypes().length == 0) {
                    return "Test proxy";
                }
                return method.invoke(real, args);
            };

            final Test generated = (Test) Proxy.newProxyInstance(Thread.currentThread()
                            .getContextClassLoader(), new Class[]{Test.class},
                    generatedTestInvocationHandler);
            assertEquals(real, generated);
            assertNotEquals(generated, real);
            assertTrue(AnnotationUtils.equals(generated, real));
            assertTrue(AnnotationUtils.equals(real, generated));

            final Test generated2 = (Test) Proxy.newProxyInstance(Thread.currentThread()
                            .getContextClassLoader(), new Class[]{Test.class},
                    generatedTestInvocationHandler);
            assertNotEquals(generated, generated2);
            assertNotEquals(generated2, generated);
            assertTrue(AnnotationUtils.equals(generated, generated2));
            assertTrue(AnnotationUtils.equals(generated2, generated));
        });
    }

    @Test
    public void testHashCode() {
        assertTimeoutPreemptively(Duration.ofSeconds(666L), () -> {
            final Test test = getClass().getDeclaredMethod("testHashCode").getAnnotation(Test.class);
            assertEquals(test.hashCode(), AnnotationUtils.hashCode(test));
            final TestAnnotation testAnnotation1 = field1.getAnnotation(TestAnnotation.class);
            assertEquals(testAnnotation1.hashCode(), AnnotationUtils.hashCode(testAnnotation1));
            final TestAnnotation testAnnotation3 = field3.getAnnotation(TestAnnotation.class);
            assertEquals(testAnnotation3.hashCode(), AnnotationUtils.hashCode(testAnnotation3));
        });
    }

    @Test
    public void testIsValidAnnotationMemberType() {
        for (final Class<?> type : new Class[] { byte.class, short.class, int.class, char.class,
                long.class, float.class, double.class, boolean.class, String.class, Class.class,
                NestAnnotation.class, TestAnnotation.class, Stooge.class, ElementType.class }) {
            assertTrue(AnnotationUtils.isValidAnnotationMemberType(type));
            assertTrue(AnnotationUtils.isValidAnnotationMemberType(Array.newInstance(type, 0)
                    .getClass()));
        }
        for (final Class<?> type : new Class[] { Object.class, Map.class, Collection.class }) {
            assertFalse(AnnotationUtils.isValidAnnotationMemberType(type));
            assertFalse(AnnotationUtils.isValidAnnotationMemberType(Array.newInstance(type, 0)
                    .getClass()));
        }
    }

    @Test
    public void testNonEquivalentAnnotationsOfSameType() {
        assertFalse(AnnotationUtils.equals(field1.getAnnotation(TestAnnotation.class), field3.getAnnotation(TestAnnotation.class)));
        assertFalse(AnnotationUtils.equals(field3.getAnnotation(TestAnnotation.class), field1.getAnnotation(TestAnnotation.class)));
    }

    @Test
    public void testOneArgNull() {
        assertFalse(AnnotationUtils.equals(field1.getAnnotation(TestAnnotation.class), null));
        assertFalse(AnnotationUtils.equals(null, field1.getAnnotation(TestAnnotation.class)));
    }

    @Test
    public void testSameInstance() {
        assertTrue(AnnotationUtils.equals(field1.getAnnotation(TestAnnotation.class), field1.getAnnotation(TestAnnotation.class)));
    }

    @Test
    @TestMethodAnnotation(timeout = 666000)
    public void testToString() {
        assertTimeoutPreemptively(Duration.ofSeconds(666L), () -> {
            final TestMethodAnnotation testAnnotation =
                    getClass().getDeclaredMethod("testToString").getAnnotation(TestMethodAnnotation.class);

            final String annotationString = AnnotationUtils.toString(testAnnotation);
            assertTrue(annotationString.startsWith("@org.apache.commons.lang3.AnnotationUtilsTest$TestMethodAnnotation("));
            assertTrue(annotationString.endsWith(")"));
            assertTrue(annotationString.contains("expected=class org.apache.commons.lang3.AnnotationUtilsTest$TestMethodAnnotation$None"));
            assertTrue(annotationString.contains("timeout=666000"));
            assertTrue(annotationString.contains(", "));
        });
    }

}
