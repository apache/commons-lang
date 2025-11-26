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
package org.apache.commons.lang3.builder;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertFalse;

import java.math.BigDecimal;
import java.math.BigInteger;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link ReflectionDiffBuilder}.
 */
class ReflectionDiffBuilderTest extends AbstractLangTest {

    @SuppressWarnings("unused")
    private static final class TypeTestChildClass extends TypeTestClass {
        String field = "a";
    }

    @SuppressWarnings("unused")
    private static class TypeTestClass implements Diffable<TypeTestClass> {

        private static int staticField;
        private final ToStringStyle style = SHORT_STYLE;
        private final boolean booleanField = true;
        private final boolean[] booleanArrayField = { true };
        private final byte byteField = (byte) 0xFF;
        private final byte[] byteArrayField = { (byte) 0xFF };
        private char charField = 'a';
        private char[] charArrayField = { 'a' };
        private final double doubleField = 1.0;
        private final double[] doubleArrayField = { 1.0 };
        private final float floatField = 1.0f;
        private final float[] floatArrayField = { 1.0f };
        int intField = 1;
        private final int[] intArrayField = { 1 };
        private final long longField = 1L;
        private final long[] longArrayField = { 1L };
        private final short shortField = 1;
        private final short[] shortArrayField = { 1 };
        private final Object objectField = null;
        private final Object[] objectArrayField = { null };
        private transient String transientField;
        private BigDecimal bigDecimal = BigDecimal.valueOf(20, 1);
        private BigInteger bigInteger = BigInteger.valueOf(2);
        @DiffExclude
        private String annotatedField = "a";
        private String excludedField = "a";

        @Override
        public DiffResult<TypeTestClass> diff(final TypeTestClass obj) {
            // @formatter:off
            return ReflectionDiffBuilder.<TypeTestClass>builder()
                    .setDiffBuilder(diffBuilder(obj))
                    .setExcludeFieldNames("excludedField")
                    .build()
                    .build();
            // @formatter:on
        }

        DiffBuilder<TypeTestClass> diffBuilder(final TypeTestClass obj) {
            // @formatter:off
            return DiffBuilder.<TypeTestClass>builder()
                    .setLeft(this)
                    .setRight(obj)
                    .setStyle(style)
                    .build();
            // @formatter:on
        }

        public DiffResult<TypeTestClass> diffDeprecated(final TypeTestClass obj) {
            return new ReflectionDiffBuilder<>(this, obj, style).setExcludeFieldNames("excludedField").build();
        }

        @Override
        public boolean equals(final Object obj) {
            return EqualsBuilder.reflectionEquals(this, obj, false);
        }

        @Override
        public int hashCode() {
            return HashCodeBuilder.reflectionHashCode(this, false);
        }
    }

    private static final ToStringStyle SHORT_STYLE = ToStringStyle.SHORT_PREFIX_STYLE;



    @Test
    void testArrayDifference() {
        final TypeTestClass firstObject = new TypeTestClass();
        firstObject.charArrayField = new char[] { 'c' };
        final TypeTestClass secondObject = new TypeTestClass();
        // Normal builder
        DiffResult<TypeTestClass> list = firstObject.diff(secondObject);
        assertEquals(1, list.getNumberOfDiffs());
        // Deprecated constructor
        list = firstObject.diffDeprecated(secondObject);
        assertEquals(1, list.getNumberOfDiffs());
    }

    @Test
    void testBigDecimalDifference() {
        final TypeTestClass firstObject = new TypeTestClass();
        // 2.0 is not equal to 2.00, see BigDecimal#equals()
        firstObject.bigDecimal = BigDecimal.valueOf(200, 2);
        final TypeTestClass secondObject = new TypeTestClass();
        final DiffResult<TypeTestClass> list = firstObject.diff(secondObject);
        assertEquals(1, list.getNumberOfDiffs());
    }

    @Test
    void testBigIntegerDifference() {
        final TypeTestClass firstObject = new TypeTestClass();
        firstObject.bigInteger = BigInteger.valueOf(100);
        final TypeTestClass secondObject = new TypeTestClass();

        final DiffResult<TypeTestClass> list = firstObject.diff(secondObject);
        assertEquals(1, list.getNumberOfDiffs());
    }

    @Test
    void testDifferenceInInherited_field() {
        final TypeTestChildClass firstObject = new TypeTestChildClass();
        firstObject.intField = 99;
        final TypeTestChildClass secondObject = new TypeTestChildClass();

        final DiffResult<TypeTestClass> list = firstObject.diff(secondObject);
        assertEquals(1, list.getNumberOfDiffs());
    }



    @Test
    void testTransientFieldDifference() {
        final TypeTestClass firstObject = new TypeTestClass();
        firstObject.transientField = "a";
        final TypeTestClass secondObject = new TypeTestClass();
        secondObject.transientField = "b";
        DiffResult<TypeTestClass> list = firstObject.diff(secondObject);
        assertEquals(0, list.getNumberOfDiffs());
        list = firstObject.diffDeprecated(secondObject);
        assertEquals(0, list.getNumberOfDiffs());
    }



    @Test
    void testAlwaysDiffFlag() {

        class MyDirect {
            float value;
            MyDirect(float v) { this.value = v; }
        }

        MyDirect a = new MyDirect(10f);
        MyDirect b = new MyDirect(10f);

        // Case 1: alwaysDiff = false
        ReflectionDiffBuilder<MyDirect> builder1 =
                ReflectionDiffBuilder.<MyDirect>builder()
                        .setDiffBuilder(
                                DiffBuilder.<MyDirect>builder()
                                        .setLeft(a)
                                        .setRight(b)
                                        .setStyle(ToStringStyle.SHORT_PREFIX_STYLE)
                                        .build()
                        )
                        .build(); // ? ReflectionDiffBuilder

        DiffResult<MyDirect> result = builder1.build(); // ? DiffResult
        assertTrue(result.getDiffs().isEmpty());

        // Case 2: alwaysDiff = true
        ReflectionDiffBuilder<MyDirect> builder2 =
                ReflectionDiffBuilder.<MyDirect>builder()
                        .setDiffBuilder(
                                DiffBuilder.<MyDirect>builder()
                                        .setLeft(a)
                                        .setRight(b)
                                        .setStyle(ToStringStyle.SHORT_PREFIX_STYLE)
                                        .build()
                        )
                        .build(); // ? ReflectionDiffBuilder

        builder2.setAlwaysDiff(true); //call it here
        DiffResult<MyDirect> forced = builder2.build();
        assertFalse(forced.getDiffs().isEmpty());
    }

}
