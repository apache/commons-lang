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

import static org.apache.commons.lang3.LangAssertions.assertNullPointerException;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.ArrayUtils;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link DiffBuilder}.
 */
class DiffBuilderTest extends AbstractLangTest {

    /**
     * Test fixture.
     */
    private static class NestedTypeTestClass implements Diffable<NestedTypeTestClass> {

        private final ToStringStyle style = SHORT_STYLE;
        private boolean booleanField = true;

        @Override
        public DiffResult<NestedTypeTestClass> diff(final NestedTypeTestClass obj) {
            // @formatter:off
            return new DiffBuilder<>(this, obj, style)
                    .append("boolean", booleanField, obj.booleanField)
                    .build();
            // @formatter:on
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

    /**
     * Test fixture.
     */
    private static final class TypeTestClass implements Diffable<TypeTestClass> {

        private ToStringStyle style = SHORT_STYLE;
        private boolean booleanField = true;
        private boolean[] booleanArrayField = { true };
        private byte byteField = (byte) 0xFF;
        private byte[] byteArrayField = { (byte) 0xFF };
        private char charField = 'a';
        private char[] charArrayField = { 'a' };
        private double doubleField = 1.0;
        private double[] doubleArrayField = { 1.0 };
        private float floatField = 1.0f;
        private float[] floatArrayField = { 1.0f };
        private int intField = 1;
        private int[] intArrayField = { 1 };
        private long longField = 1L;
        private long[] longArrayField = { 1L };
        private short shortField = 1;
        private short[] shortArrayField = { 1 };
        private Object objectField;
        private Object[] objectArrayField = { null };
        private final NestedTypeTestClass nestedDiffableField = new NestedTypeTestClass();

        @Override
        public DiffResult<TypeTestClass> diff(final TypeTestClass obj) {
            // @formatter:off
            return new DiffBuilder<>(this, obj, style)
                    .append("boolean", booleanField, obj.booleanField)
                    .append("booleanArray", booleanArrayField, obj.booleanArrayField)
                    .append("byte", byteField, obj.byteField)
                    .append("byteArray", byteArrayField, obj.byteArrayField)
                    .append("char", charField, obj.charField)
                    .append("charArray", charArrayField, obj.charArrayField)
                    .append("double", doubleField, obj.doubleField)
                    .append("doubleArray", doubleArrayField, obj.doubleArrayField)
                    .append("float", floatField, obj.floatField)
                    .append("floatArray", floatArrayField, obj.floatArrayField)
                    .append("int", intField, obj.intField)
                    .append("intArray", intArrayField, obj.intArrayField)
                    .append("long", longField, obj.longField)
                    .append("longArray", longArrayField, obj.longArrayField)
                    .append("short", shortField, obj.shortField)
                    .append("shortArray", shortArrayField, obj.shortArrayField)
                    .append("objectField", objectField, obj.objectField)
                    .append("objectArrayField", objectArrayField, obj.objectArrayField)
                    .append("nestedDiffableField", nestedDiffableField.diff(obj.nestedDiffableField))
                    .build();
            // @formatter:on
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
    void testBoolean() {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.booleanField = false;
        final DiffResult<TypeTestClass> list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertEquals(Boolean.class, diff.getType(), () -> diff.getType().getClass().toString());
        assertEquals(Boolean.TRUE, diff.getLeft());
        assertEquals(Boolean.FALSE, diff.getRight());
    }

    @Test
    void testBooleanArray() {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.booleanArrayField = new boolean[] { false, false };
        final DiffResult<TypeTestClass> list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertArrayEquals(ArrayUtils.toObject(class1.booleanArrayField), (Object[]) diff.getLeft());
        assertArrayEquals(ArrayUtils.toObject(class2.booleanArrayField), (Object[]) diff.getRight());
    }

    @Test
    void testByte() {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.byteField = 0x01;
        final DiffResult<TypeTestClass> list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertEquals(Byte.class, diff.getType(), () -> diff.getType().getClass().toString());
        assertEquals(Byte.valueOf(class1.byteField), diff.getLeft());
        assertEquals(Byte.valueOf(class2.byteField), diff.getRight());
    }

    @Test
    void testByteArray() {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.byteArrayField = new byte[] { 0x01, 0x02 };
        final DiffResult<TypeTestClass> list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertArrayEquals(ArrayUtils.toObject(class1.byteArrayField), (Object[]) diff.getLeft());
        assertArrayEquals(ArrayUtils.toObject(class2.byteArrayField), (Object[]) diff.getRight());
    }

    @Test
    void testByteArrayEqualAsObject1() {
        // @formatter:off
        final DiffResult<String> list = DiffBuilder.<String>builder().setLeft("String1").setRight("String2").setStyle(SHORT_STYLE).build()
                .append("foo", new boolean[] { false }, new boolean[] { false })
                .append("foo", new byte[] { 0x01 }, new byte[] { 0x01 })
                .append("foo", new char[] { 'a' }, new char[] { 'a' })
                .append("foo", new double[] { 1.0 }, new double[] { 1.0 })
                .append("foo", new float[] { 1.0F }, new float[] { 1.0F })
                .append("foo", new int[] { 1 }, new int[] { 1 })
                .append("foo", new long[] { 1L }, new long[] { 1L })
                .append("foo", new short[] { 1 }, new short[] { 1 })
                .append("foo", new Object[] { 1, "two" }, new Object[] { 1, "two" })
                .build();
        // @formatter:on
        assertEquals(0, list.getNumberOfDiffs());
    }

    @Test
    void testByteArrayEqualAsObjectDeprecated() {
        // @formatter:off
        final DiffResult<String> list = new DiffBuilder<>("String1", "String2", SHORT_STYLE)
                .append("foo", new boolean[] { false }, new boolean[] { false })
                .append("foo", new byte[] { 0x01 }, new byte[] { 0x01 })
                .append("foo", new char[] { 'a' }, new char[] { 'a' })
                .append("foo", new double[] { 1.0 }, new double[] { 1.0 })
                .append("foo", new float[] { 1.0F }, new float[] { 1.0F })
                .append("foo", new int[] { 1 }, new int[] { 1 })
                .append("foo", new long[] { 1L }, new long[] { 1L })
                .append("foo", new short[] { 1 }, new short[] { 1 })
                .append("foo", new Object[] { 1, "two" }, new Object[] { 1, "two" })
                .build();
        // @formatter:on
        assertEquals(0, list.getNumberOfDiffs());
    }

    @Test
    void testChar() {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.charField = 'z';
        final DiffResult<TypeTestClass> list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertEquals(Character.valueOf(class1.charField), diff.getLeft());
        assertEquals(Character.valueOf(class2.charField), diff.getRight());
    }

    @Test
    void testCharArray() {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.charArrayField = new char[] { 'f', 'o', 'o' };
        final DiffResult<TypeTestClass> list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertArrayEquals(ArrayUtils.toObject(class1.charArrayField), (Object[]) diff.getLeft());
        assertArrayEquals(ArrayUtils.toObject(class2.charArrayField), (Object[]) diff.getRight());
    }

    @Test
    void testDiffResult() {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.intField = 2;

        final DiffResult<TypeTestClass> list = new DiffBuilder<>(class1, class2, SHORT_STYLE).append("prop1", class1.diff(class2)).build();
        assertEquals(1, list.getNumberOfDiffs());
        assertEquals("prop1.int", list.getDiffs().get(0).getFieldName());
    }

    @Test
    void testDiffResultEquals() {
        final TypeTestClass class1 = new TypeTestClass();
        class1.intField = 2;

        final DiffResult<TypeTestClass> list = new DiffBuilder<>(class1, class1, SHORT_STYLE).append("prop1", class1.diff(class1)).build();
        assertEquals(0, list.getNumberOfDiffs());
        assertTrue(list.getDiffs().isEmpty());
    }

    @Test
    void testDouble() {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.doubleField = 99.99;
        final DiffResult<TypeTestClass> list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertEquals(Double.valueOf(class1.doubleField), diff.getLeft());
        assertEquals(Double.valueOf(class2.doubleField), diff.getRight());
    }

    @Test
    void testDoubleArray() {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.doubleArrayField = new double[] { 3.0, 2.9, 2.8 };
        final DiffResult<TypeTestClass> list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertArrayEquals(ArrayUtils.toObject(class1.doubleArrayField), (Object[]) diff.getLeft());
        assertArrayEquals(ArrayUtils.toObject(class2.doubleArrayField), (Object[]) diff.getRight());
    }

    @Test
    void testFloat() {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.floatField = 99.99F;
        final DiffResult<TypeTestClass> list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertEquals(Float.valueOf(class1.floatField), diff.getLeft());
        assertEquals(Float.valueOf(class2.floatField), diff.getRight());
    }

    @Test
    void testFloatArray() {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.floatArrayField = new float[] { 3.0F, 2.9F, 2.8F };
        final DiffResult<TypeTestClass> list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertArrayEquals(ArrayUtils.toObject(class1.floatArrayField), (Object[]) diff.getLeft());
        assertArrayEquals(ArrayUtils.toObject(class2.floatArrayField), (Object[]) diff.getRight());
    }

    @Test
    void testInt() {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.intField = 42;
        final DiffResult<TypeTestClass> list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertEquals(Integer.valueOf(class1.intField), diff.getLeft());
        assertEquals(Integer.valueOf(class2.intField), diff.getRight());
    }

    @Test
    void testIntArray() {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.intArrayField = new int[] { 3, 2, 1 };
        final DiffResult<TypeTestClass> list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertArrayEquals(ArrayUtils.toObject(class1.intArrayField), (Object[]) diff.getLeft());
        assertArrayEquals(ArrayUtils.toObject(class2.intArrayField), (Object[]) diff.getRight());
    }

    @Test
    void testLong() {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.longField = 42L;
        final DiffResult<TypeTestClass> list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertEquals(Long.valueOf(class1.longField), diff.getLeft());
        assertEquals(Long.valueOf(class2.longField), diff.getRight());
    }

    @Test
    void testLongArray() {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.longArrayField = new long[] { 3L, 2L, 1L };
        final DiffResult<TypeTestClass> list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertArrayEquals(ArrayUtils.toObject(class1.longArrayField), (Object[]) diff.getLeft());
        assertArrayEquals(ArrayUtils.toObject(class2.longArrayField), (Object[]) diff.getRight());
    }

    @Test
    void testNestedDiffableNo() {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        final DiffResult<TypeTestClass> list = class1.diff(class2);
        assertEquals(0, list.getNumberOfDiffs());
        final List<Diff<?>> diff = list.getDiffs();
        assertTrue(diff.isEmpty());
    }

    @Test
    void testNestedDiffableYesNestedNot() {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.intField = 9;
        final DiffResult<TypeTestClass> list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertEquals(Integer.class, diff.getType());
        assertEquals(1, diff.getLeft());
        assertEquals(9, diff.getRight());
    }

    @Test
    void testNestedDiffableYesNestedOnly() {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.nestedDiffableField.booleanField = false;
        final DiffResult<TypeTestClass> list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertEquals(Object.class, diff.getType());
        assertEquals(Boolean.TRUE, diff.getLeft());
        assertEquals(Boolean.FALSE, diff.getRight());
    }

    @Test
    void testNestedDiffableYesNestedYes() {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.intField = 9;
        class2.nestedDiffableField.booleanField = false;
        final DiffResult<TypeTestClass> list = class1.diff(class2);
        assertEquals(2, list.getNumberOfDiffs());
        final Diff<?> diff0 = list.getDiffs().get(0);
        assertEquals(Integer.class, diff0.getType());
        assertEquals(1, diff0.getLeft());
        assertEquals(9, diff0.getRight());
        final Diff<?> diff1 = list.getDiffs().get(1);
        assertEquals(Object.class, diff1.getType());
        assertEquals(Boolean.TRUE, diff1.getLeft());
        assertEquals(Boolean.FALSE, diff1.getRight());
    }

    @Test
    void testNullLhs() {
        assertNullPointerException(() -> new DiffBuilder<>(null, this, ToStringStyle.DEFAULT_STYLE));
    }

    @Test
    void testNullLhs_4args() {
        assertNullPointerException(() -> new DiffBuilder<>(null, this, ToStringStyle.DEFAULT_STYLE, true));
    }

    @Test
    void testNullRhs() {
        assertNullPointerException(() -> new DiffBuilder<>(this, null, ToStringStyle.DEFAULT_STYLE));
    }

    @Test
    void testNullRhs_4args() {
        assertNullPointerException(() -> new DiffBuilder<>(this, null, ToStringStyle.DEFAULT_STYLE, true));
    }

    @Test
    void testObject() {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.objectField = "Some string";
        final DiffResult<TypeTestClass> list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertEquals(class1.objectField, diff.getLeft());
        assertEquals(class2.objectField, diff.getRight());
    }

    @Test
    void testObjectArray() {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.objectArrayField = new Object[] { "string", 1, 2 };
        final DiffResult<TypeTestClass> list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertArrayEquals(class1.objectArrayField, (Object[]) diff.getLeft());
        assertArrayEquals(class2.objectArrayField, (Object[]) diff.getRight());
    }

    @Test
    void testObjectArrayEqual() {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class1.objectArrayField = new Object[] { "string", 1, 2 };
        class2.objectArrayField = new Object[] { "string", 1, 2 };
        final DiffResult<TypeTestClass> list = class1.diff(class2);
        assertEquals(0, list.getNumberOfDiffs());
    }

    /**
     * Test that "left" and "right" are the same instance but are equal.
     */
    @Test
    void testObjectsNotSameButEqual() {
        final TypeTestClass left = new TypeTestClass();
        left.objectField = Integer.valueOf(1000);
        final TypeTestClass right = new TypeTestClass();
        right.objectField = Integer.valueOf(1000);
        assertNotSame(left.objectField, right.objectField);
        assertEquals(left.objectField, right.objectField);

        final DiffResult<TypeTestClass> list = left.diff(right);
        assertEquals(0, list.getNumberOfDiffs());
    }

    /**
     * Test that "left" and "right" are not the same instance and are not equal.
     */
    @Test
    void testObjectsNotSameNorEqual() {
        final TypeTestClass left = new TypeTestClass();
        left.objectField = 4;
        final TypeTestClass right = new TypeTestClass();
        right.objectField = 100;
        assertNotSame(left.objectField, right.objectField);
        assertNotEquals(left.objectField, right.objectField);

        final DiffResult<TypeTestClass> list = left.diff(right);
        assertEquals(1, list.getNumberOfDiffs());
    }

    /**
     * Test that "left" and "right" are the same instance and are equal.
     */
    @Test
    void testObjectsSameAndEqual() {
        final Integer sameObject = 1;
        final TypeTestClass left = new TypeTestClass();
        left.objectField = sameObject;
        final TypeTestClass right = new TypeTestClass();
        right.objectField = sameObject;
        assertSame(left.objectField, right.objectField);
        assertEquals(left.objectField, right.objectField);

        final DiffResult<TypeTestClass> list = left.diff(right);
        assertEquals(0, list.getNumberOfDiffs());
    }

    @Test
    void testSameObjectIgnoresAppends() {
        final TypeTestClass testClass = new TypeTestClass();
        final DiffResult<TypeTestClass> list = new DiffBuilder<>(testClass, testClass, SHORT_STYLE).append("ignored", false, true).build();
        assertEquals(0, list.getNumberOfDiffs());
    }

    @Test
    void testShort() {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.shortField = 42;
        final DiffResult<TypeTestClass> list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertEquals(Short.valueOf(class1.shortField), diff.getLeft());
        assertEquals(Short.valueOf(class2.shortField), diff.getRight());
    }

    @Test
    void testShortArray() {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.shortArrayField = new short[] { 3, 2, 1 };
        final DiffResult<TypeTestClass> list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertArrayEquals(ArrayUtils.toObject(class1.shortArrayField), (Object[]) diff.getLeft());
        assertArrayEquals(ArrayUtils.toObject(class2.shortArrayField), (Object[]) diff.getRight());
    }

    @Test
    void testSimilarObjectIgnoresAppends() {
        final TypeTestClass testClass1 = new TypeTestClass();
        final TypeTestClass testClass2 = new TypeTestClass();
        final DiffResult<TypeTestClass> list = new DiffBuilder<>(testClass1, testClass2, SHORT_STYLE).append("ignored", false, true).build();
        assertEquals(0, list.getNumberOfDiffs());
    }

    @Test
    void testStylePassedToDiffResult() {
        final TypeTestClass class1 = new TypeTestClass();
        DiffResult<TypeTestClass> list = class1.diff(class1);
        assertEquals(SHORT_STYLE, list.getToStringStyle());

        class1.style = ToStringStyle.MULTI_LINE_STYLE;
        list = class1.diff(class1);
        assertEquals(ToStringStyle.MULTI_LINE_STYLE, list.getToStringStyle());
    }

    @Test
    void testTriviallyEqualTestDisabled() {
        // Constructor's arguments are not trivially equal, but not testing for that.
        final DiffBuilder<Integer> explicitTestAndNotEqual1 = new DiffBuilder<>(1, 2, null, false);
        explicitTestAndNotEqual1.append("letter", "X", "Y");
        assertEquals(1, explicitTestAndNotEqual1.build().getNumberOfDiffs());

        // Constructor's arguments are trivially equal, but not testing for that.
        final DiffBuilder<Integer> explicitTestAndNotEqual2 = new DiffBuilder<>(1, 1, null, false);
        // This append(f, l, r) will not abort early.
        explicitTestAndNotEqual2.append("letter", "X", "Y");
        assertEquals(1, explicitTestAndNotEqual2.build().getNumberOfDiffs());
    }

    @Test
    void testTriviallyEqualTestEnabled() {
        // The option to test if trivially equal is enabled by default.
        final DiffBuilder<Integer> implicitTestAndEqual = new DiffBuilder<>(1, 1, null);
        // This append(f, l, r) will abort without creating a Diff for letter.
        implicitTestAndEqual.append("letter", "X", "Y");
        assertEquals(0, implicitTestAndEqual.build().getNumberOfDiffs());

        final DiffBuilder<Integer> implicitTestAndNotEqual = new DiffBuilder<>(1, 2, null);
        // This append(f, l, r) will not abort early
        // because the constructor's arguments were not trivially equal.
        implicitTestAndNotEqual.append("letter", "X", "Y");
        assertEquals(1, implicitTestAndNotEqual.build().getNumberOfDiffs());

        // This is explicitly enabling the trivially equal test.
        final DiffBuilder<Integer> explicitTestAndEqual = new DiffBuilder<>(1, 1, null, true);
        explicitTestAndEqual.append("letter", "X", "Y");
        assertEquals(0, explicitTestAndEqual.build().getNumberOfDiffs());
    }

}
