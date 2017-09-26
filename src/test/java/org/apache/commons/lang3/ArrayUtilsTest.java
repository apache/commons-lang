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

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Date;
import java.util.Map;

import org.junit.Assert;
import org.junit.Test;

/**
 * Unit tests {@link org.apache.commons.lang3.ArrayUtils}.
 */
@SuppressWarnings("deprecation") // deliberate use of deprecated code
public class ArrayUtilsTest {

    //-----------------------------------------------------------------------
    @Test
    public void testConstructor() {
        assertNotNull(new ArrayUtils());
        final Constructor<?>[] cons = ArrayUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertTrue(Modifier.isPublic(cons[0].getModifiers()));
        assertTrue(Modifier.isPublic(ArrayUtils.class.getModifiers()));
        assertFalse(Modifier.isFinal(ArrayUtils.class.getModifiers()));
    }

    //-----------------------------------------------------------------------
    @Test
    public void testToString() {
        assertEquals("{}", ArrayUtils.toString(null));
        assertEquals("{}", ArrayUtils.toString(new Object[0]));
        assertEquals("{}", ArrayUtils.toString(new String[0]));
        assertEquals("{<null>}", ArrayUtils.toString(new String[]{null}));
        assertEquals("{pink,blue}", ArrayUtils.toString(new String[]{"pink", "blue"}));

        assertEquals("<empty>", ArrayUtils.toString(null, "<empty>"));
        assertEquals("{}", ArrayUtils.toString(new Object[0], "<empty>"));
        assertEquals("{}", ArrayUtils.toString(new String[0], "<empty>"));
        assertEquals("{<null>}", ArrayUtils.toString(new String[]{null}, "<empty>"));
        assertEquals("{pink,blue}", ArrayUtils.toString(new String[]{"pink", "blue"}, "<empty>"));
    }

    //-----------------------------------------------------------------------
    @Test
    public void testHashCode() {
        final long[][] array1 = new long[][]{{2, 5}, {4, 5}};
        final long[][] array2 = new long[][]{{2, 5}, {4, 6}};
        assertTrue(ArrayUtils.hashCode(array1) == ArrayUtils.hashCode(array1));
        assertFalse(ArrayUtils.hashCode(array1) == ArrayUtils.hashCode(array2));

        final Object[] array3 = new Object[]{new String(new char[]{'A', 'B'})};
        final Object[] array4 = new Object[]{"AB"};
        assertTrue(ArrayUtils.hashCode(array3) == ArrayUtils.hashCode(array3));
        assertTrue(ArrayUtils.hashCode(array3) == ArrayUtils.hashCode(array4));

        final Object[] arrayA = new Object[]{new boolean[]{true, false}, new int[]{6, 7}};
        final Object[] arrayB = new Object[]{new boolean[]{true, false}, new int[]{6, 7}};
        assertTrue(ArrayUtils.hashCode(arrayB) == ArrayUtils.hashCode(arrayA));
    }

    //-----------------------------------------------------------------------
    private void assertIsEquals(final Object array1, final Object array2, final Object array3) {
        assertTrue(ArrayUtils.isEquals(array1, array1));
        assertTrue(ArrayUtils.isEquals(array2, array2));
        assertTrue(ArrayUtils.isEquals(array3, array3));
        assertFalse(ArrayUtils.isEquals(array1, array2));
        assertFalse(ArrayUtils.isEquals(array2, array1));
        assertFalse(ArrayUtils.isEquals(array1, array3));
        assertFalse(ArrayUtils.isEquals(array3, array1));
        assertFalse(ArrayUtils.isEquals(array1, array2));
        assertFalse(ArrayUtils.isEquals(array2, array1));
    }

    @Test
    public void testIsEquals() {
        final long[][] larray1 = new long[][]{{2, 5}, {4, 5}};
        final long[][] larray2 = new long[][]{{2, 5}, {4, 6}};
        final long[] larray3 = new long[]{2, 5};
        this.assertIsEquals(larray1, larray2, larray3);

        final int[][] iarray1 = new int[][]{{2, 5}, {4, 5}};
        final int[][] iarray2 = new int[][]{{2, 5}, {4, 6}};
        final int[] iarray3 = new int[]{2, 5};
        this.assertIsEquals(iarray1, iarray2, iarray3);

        final short[][] sarray1 = new short[][]{{2, 5}, {4, 5}};
        final short[][] sarray2 = new short[][]{{2, 5}, {4, 6}};
        final short[] sarray3 = new short[]{2, 5};
        this.assertIsEquals(sarray1, sarray2, sarray3);

        final float[][] farray1 = new float[][]{{2, 5}, {4, 5}};
        final float[][] farray2 = new float[][]{{2, 5}, {4, 6}};
        final float[] farray3 = new float[]{2, 5};
        this.assertIsEquals(farray1, farray2, farray3);

        final double[][] darray1 = new double[][]{{2, 5}, {4, 5}};
        final double[][] darray2 = new double[][]{{2, 5}, {4, 6}};
        final double[] darray3 = new double[]{2, 5};
        this.assertIsEquals(darray1, darray2, darray3);

        final byte[][] byteArray1 = new byte[][]{{2, 5}, {4, 5}};
        final byte[][] byteArray2 = new byte[][]{{2, 5}, {4, 6}};
        final byte[] byteArray3 = new byte[]{2, 5};
        this.assertIsEquals(byteArray1, byteArray2, byteArray3);

        final char[][] charArray1 = new char[][]{{2, 5}, {4, 5}};
        final char[][] charArray2 = new char[][]{{2, 5}, {4, 6}};
        final char[] charArray3 = new char[]{2, 5};
        this.assertIsEquals(charArray1, charArray2, charArray3);

        final boolean[][] barray1 = new boolean[][]{{true, false}, {true, true}};
        final boolean[][] barray2 = new boolean[][]{{true, false}, {true, false}};
        final boolean[] barray3 = new boolean[]{false, true};
        this.assertIsEquals(barray1, barray2, barray3);

        final Object[] array3 = new Object[]{new String(new char[]{'A', 'B'})};
        final Object[] array4 = new Object[]{"AB"};
        assertTrue(ArrayUtils.isEquals(array3, array3));
        assertTrue(ArrayUtils.isEquals(array3, array4));

        assertTrue(ArrayUtils.isEquals(null, null));
        assertFalse(ArrayUtils.isEquals(null, array4));
    }

    //-----------------------------------------------------------------------
    /**
     * Tests generic array creation with parameters of same type.
     */
    @Test
    public void testArrayCreation() {
        final String[] array = ArrayUtils.toArray("foo", "bar");
        assertEquals(2, array.length);
        assertEquals("foo", array[0]);
        assertEquals("bar", array[1]);
    }

    /**
     * Tests generic array creation with general return type.
     */
    @Test
    public void testArrayCreationWithGeneralReturnType() {
        final Object obj = ArrayUtils.toArray("foo", "bar");
        assertTrue(obj instanceof String[]);
    }

    /**
     * Tests generic array creation with parameters of common base type.
     */
    @Test
    public void testArrayCreationWithDifferentTypes() {
        final Number[] array = ArrayUtils.<Number>toArray(Integer.valueOf(42), Double.valueOf(Math.PI));
        assertEquals(2, array.length);
        assertEquals(Integer.valueOf(42), array[0]);
        assertEquals(Double.valueOf(Math.PI), array[1]);
    }

    /**
     * Tests generic array creation with generic type.
     */
    @Test
    public void testIndirectArrayCreation() {
        final String[] array = toArrayPropagatingType("foo", "bar");
        assertEquals(2, array.length);
        assertEquals("foo", array[0]);
        assertEquals("bar", array[1]);
    }

    /**
     * Tests generic empty array creation with generic type.
     */
    @Test
    public void testEmptyArrayCreation() {
        final String[] array = ArrayUtils.<String>toArray();
        assertEquals(0, array.length);
    }

    /**
     * Tests indirect generic empty array creation with generic type.
     */
    @Test
    public void testIndirectEmptyArrayCreation() {
        final String[] array = ArrayUtilsTest.<String>toArrayPropagatingType();
        assertEquals(0, array.length);
    }

    @SafeVarargs
    private static <T> T[] toArrayPropagatingType(final T... items) {
        return ArrayUtils.toArray(items);
    }

    //-----------------------------------------------------------------------
    @Test
    public void testToMap() {
        Map<?, ?> map = ArrayUtils.toMap(new String[][]{{"foo", "bar"}, {"hello", "world"}});

        assertEquals("bar", map.get("foo"));
        assertEquals("world", map.get("hello"));

        assertEquals(null, ArrayUtils.toMap(null));
        try {
            ArrayUtils.toMap(new String[][]{{"foo", "bar"}, {"short"}});
            fail("exception expected");
        } catch (final IllegalArgumentException ex) {
        }
        try {
            ArrayUtils.toMap(new Object[]{new Object[]{"foo", "bar"}, "illegal type"});
            fail("exception expected");
        } catch (final IllegalArgumentException ex) {
        }
        try {
            ArrayUtils.toMap(new Object[]{new Object[]{"foo", "bar"}, null});
            fail("exception expected");
        } catch (final IllegalArgumentException ex) {
        }

        map = ArrayUtils.toMap(new Object[]{new Map.Entry<Object, Object>() {
            @Override
            public Object getKey() {
                return "foo";
            }

            @Override
            public Object getValue() {
                return "bar";
            }

            @Override
            public Object setValue(final Object value) {
                throw new UnsupportedOperationException();
            }

            @Override
            public boolean equals(final Object o) {
                throw new UnsupportedOperationException();
            }

            @Override
            public int hashCode() {
                throw new UnsupportedOperationException();
            }
        }});
        assertEquals("bar", map.get("foo"));
    }

    //-----------------------------------------------------------------------
    @Test
    public void testClone() {
        assertArrayEquals(null, ArrayUtils.clone((Object[]) null));
        Object[] original1 = new Object[0];
        Object[] cloned1 = ArrayUtils.clone(original1);
        assertTrue(Arrays.equals(original1, cloned1));
        assertTrue(original1 != cloned1);

        final StringBuilder builder = new StringBuilder("pick");
        original1 = new Object[]{builder, "a", new String[]{"stick"}};
        cloned1 = ArrayUtils.clone(original1);
        assertTrue(Arrays.equals(original1, cloned1));
        assertTrue(original1 != cloned1);
        assertSame(original1[0], cloned1[0]);
        assertSame(original1[1], cloned1[1]);
        assertSame(original1[2], cloned1[2]);
    }

    @Test
    public void testCloneBoolean() {
        assertEquals(null, ArrayUtils.clone((boolean[]) null));
        final boolean[] original = new boolean[]{true, false};
        final boolean[] cloned = ArrayUtils.clone(original);
        assertTrue(Arrays.equals(original, cloned));
        assertTrue(original != cloned);
    }

    @Test
    public void testCloneLong() {
        assertEquals(null, ArrayUtils.clone((long[]) null));
        final long[] original = new long[]{0L, 1L};
        final long[] cloned = ArrayUtils.clone(original);
        assertTrue(Arrays.equals(original, cloned));
        assertTrue(original != cloned);
    }

    @Test
    public void testCloneInt() {
        assertEquals(null, ArrayUtils.clone((int[]) null));
        final int[] original = new int[]{5, 8};
        final int[] cloned = ArrayUtils.clone(original);
        assertTrue(Arrays.equals(original, cloned));
        assertTrue(original != cloned);
    }

    @Test
    public void testCloneShort() {
        assertEquals(null, ArrayUtils.clone((short[]) null));
        final short[] original = new short[]{1, 4};
        final short[] cloned = ArrayUtils.clone(original);
        assertTrue(Arrays.equals(original, cloned));
        assertTrue(original != cloned);
    }

    @Test
    public void testCloneChar() {
        assertEquals(null, ArrayUtils.clone((char[]) null));
        final char[] original = new char[]{'a', '4'};
        final char[] cloned = ArrayUtils.clone(original);
        assertTrue(Arrays.equals(original, cloned));
        assertTrue(original != cloned);
    }

    @Test
    public void testCloneByte() {
        assertEquals(null, ArrayUtils.clone((byte[]) null));
        final byte[] original = new byte[]{1, 6};
        final byte[] cloned = ArrayUtils.clone(original);
        assertTrue(Arrays.equals(original, cloned));
        assertTrue(original != cloned);
    }

    @Test
    public void testCloneDouble() {
        assertEquals(null, ArrayUtils.clone((double[]) null));
        final double[] original = new double[]{2.4d, 5.7d};
        final double[] cloned = ArrayUtils.clone(original);
        assertTrue(Arrays.equals(original, cloned));
        assertTrue(original != cloned);
    }

    @Test
    public void testCloneFloat() {
        assertEquals(null, ArrayUtils.clone((float[]) null));
        final float[] original = new float[]{2.6f, 6.4f};
        final float[] cloned = ArrayUtils.clone(original);
        assertTrue(Arrays.equals(original, cloned));
        assertTrue(original != cloned);
    }

    //-----------------------------------------------------------------------

    private class TestClass {
    }

    @Test
    public void testNullToEmptyGenericNull() {
        final TestClass[] output = ArrayUtils.nullToEmpty(null, TestClass[].class);

        assertTrue(output != null);
        assertTrue(output.length == 0);
    }

    @Test
    public void testNullToEmptyGenericEmpty() {
        final TestClass[] input = new TestClass[]{};
        final TestClass[] output = ArrayUtils.nullToEmpty(input, TestClass[].class);

        assertSame(input, output);
    }

    @Test
    public void testNullToEmptyGeneric() {
        final TestClass[] input = new TestClass[]{new TestClass(), new TestClass()};
        final TestClass[] output = ArrayUtils.nullToEmpty(input, TestClass[].class);

        assertSame(input, output);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testNullToEmptyGenericNullType() {
        final TestClass[] input = new TestClass[]{};
        ArrayUtils.nullToEmpty(input, null);
    }

    @Test
    public void testNullToEmptyBooleanNull() throws Exception {
        assertEquals(ArrayUtils.EMPTY_BOOLEAN_ARRAY, ArrayUtils.nullToEmpty((boolean[]) null));
    }

    @Test
    public void testNullToEmptyBooleanEmptyArray() throws Exception {
        final boolean[] empty = new boolean[]{};
        final boolean[] result = ArrayUtils.nullToEmpty(empty);
        assertEquals(ArrayUtils.EMPTY_BOOLEAN_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyBoolean() {
        final boolean[] original = new boolean[]{true, false};
        assertEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyLongNull() throws Exception {
        assertEquals(ArrayUtils.EMPTY_LONG_ARRAY, ArrayUtils.nullToEmpty((long[]) null));
    }

    @Test
    public void testNullToEmptyLongEmptyArray() throws Exception {
        final long[] empty = new long[]{};
        final long[] result = ArrayUtils.nullToEmpty(empty);
        assertEquals(ArrayUtils.EMPTY_LONG_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyLong() {
        final long[] original = new long[]{1L, 2L};
        assertEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyIntNull() throws Exception {
        assertEquals(ArrayUtils.EMPTY_INT_ARRAY, ArrayUtils.nullToEmpty((int[]) null));
    }

    @Test
    public void testNullToEmptyIntEmptyArray() throws Exception {
        final int[] empty = new int[]{};
        final int[] result = ArrayUtils.nullToEmpty(empty);
        assertEquals(ArrayUtils.EMPTY_INT_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyInt() {
        final int[] original = new int[]{1, 2};
        assertEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyShortNull() throws Exception {
        assertEquals(ArrayUtils.EMPTY_SHORT_ARRAY, ArrayUtils.nullToEmpty((short[]) null));
    }

    @Test
    public void testNullToEmptyShortEmptyArray() throws Exception {
        final short[] empty = new short[]{};
        final short[] result = ArrayUtils.nullToEmpty(empty);
        assertEquals(ArrayUtils.EMPTY_SHORT_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyShort() {
        final short[] original = new short[]{1, 2};
        assertEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyCharNull() throws Exception {
        assertEquals(ArrayUtils.EMPTY_CHAR_ARRAY, ArrayUtils.nullToEmpty((char[]) null));
    }

    @Test
    public void testNullToEmptyCharEmptyArray() throws Exception {
        final char[] empty = new char[]{};
        final char[] result = ArrayUtils.nullToEmpty(empty);
        assertEquals(ArrayUtils.EMPTY_CHAR_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyChar() {
        final char[] original = new char[]{'a', 'b'};
        assertEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyByteNull() throws Exception {
        assertEquals(ArrayUtils.EMPTY_BYTE_ARRAY, ArrayUtils.nullToEmpty((byte[]) null));
    }

    @Test
    public void testNullToEmptyByteEmptyArray() throws Exception {
        final byte[] empty = new byte[]{};
        final byte[] result = ArrayUtils.nullToEmpty(empty);
        assertEquals(ArrayUtils.EMPTY_BYTE_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyByte() {
        final byte[] original = new byte[]{0x0F, 0x0E};
        assertEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyDoubleNull() throws Exception {
        assertEquals(ArrayUtils.EMPTY_DOUBLE_ARRAY, ArrayUtils.nullToEmpty((double[]) null));
    }

    @Test
    public void testNullToEmptyDoubleEmptyArray() throws Exception {
        final double[] empty = new double[]{};
        final double[] result = ArrayUtils.nullToEmpty(empty);
        assertEquals(ArrayUtils.EMPTY_DOUBLE_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyDouble() {
        final double[] original = new double[]{1L, 2L};
        assertEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyFloatNull() throws Exception {
        assertEquals(ArrayUtils.EMPTY_FLOAT_ARRAY, ArrayUtils.nullToEmpty((float[]) null));
    }

    @Test
    public void testNullToEmptyFloatEmptyArray() throws Exception {
        final float[] empty = new float[]{};
        final float[] result = ArrayUtils.nullToEmpty(empty);
        assertEquals(ArrayUtils.EMPTY_FLOAT_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyFloat() {
        final float[] original = new float[]{2.6f, 3.8f};
        assertEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyObjectNull() throws Exception {
        assertArrayEquals(ArrayUtils.EMPTY_OBJECT_ARRAY, ArrayUtils.nullToEmpty((Object[]) null));
    }

    @Test
    public void testNullToEmptyObjectEmptyArray() throws Exception {
        final Object[] empty = new Object[]{};
        final Object[] result = ArrayUtils.nullToEmpty(empty);
        assertArrayEquals(ArrayUtils.EMPTY_OBJECT_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyObject() {
        final Object[] original = new Object[]{Boolean.TRUE, Boolean.FALSE};
        assertArrayEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyClassNull() throws Exception {
        assertArrayEquals(ArrayUtils.EMPTY_CLASS_ARRAY, ArrayUtils.nullToEmpty((Class<?>[]) null));
    }

    @Test
    public void testNullToEmptyClassEmptyArray() throws Exception {
        final Class<?>[] empty = {};
        final Class<?>[] result = ArrayUtils.nullToEmpty(empty);
        assertArrayEquals(ArrayUtils.EMPTY_CLASS_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyClass() {
        final Class<?>[] original = {Object.class, String.class};
        assertArrayEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyStringNull() throws Exception {
        assertArrayEquals(ArrayUtils.EMPTY_STRING_ARRAY, ArrayUtils.nullToEmpty((String[]) null));
    }

    @Test
    public void testNullToEmptyStringEmptyArray() throws Exception {
        final String[] empty = new String[]{};
        final String[] result = ArrayUtils.nullToEmpty(empty);
        assertArrayEquals(ArrayUtils.EMPTY_STRING_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyString() {
        final String[] original = new String[]{"abc", "def"};
        assertArrayEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyBooleanObjectNull() throws Exception {
        assertArrayEquals(ArrayUtils.EMPTY_BOOLEAN_OBJECT_ARRAY, ArrayUtils.nullToEmpty((Boolean[]) null));
    }

    @Test
    public void testNullToEmptyBooleanObjectEmptyArray() throws Exception {
        final Boolean[] empty = new Boolean[]{};
        final Boolean[] result = ArrayUtils.nullToEmpty(empty);
        assertArrayEquals(ArrayUtils.EMPTY_BOOLEAN_OBJECT_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyBooleanObject() {
        final Boolean[] original = new Boolean[]{Boolean.TRUE, Boolean.FALSE};
        assertArrayEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyLongObjectNull() throws Exception {
        assertArrayEquals(ArrayUtils.EMPTY_LONG_OBJECT_ARRAY, ArrayUtils.nullToEmpty((Long[]) null));
    }

    @Test
    public void testNullToEmptyLongObjectEmptyArray() throws Exception {
        final Long[] empty = new Long[]{};
        final Long[] result = ArrayUtils.nullToEmpty(empty);
        assertArrayEquals(ArrayUtils.EMPTY_LONG_OBJECT_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyLongObject() {
        @SuppressWarnings("boxing") final Long[] original = new Long[]{1L, 2L};
        assertArrayEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyIntObjectNull() throws Exception {
        assertArrayEquals(ArrayUtils.EMPTY_INTEGER_OBJECT_ARRAY, ArrayUtils.nullToEmpty((Integer[]) null));
    }

    @Test
    public void testNullToEmptyIntObjectEmptyArray() throws Exception {
        final Integer[] empty = new Integer[]{};
        final Integer[] result = ArrayUtils.nullToEmpty(empty);
        assertArrayEquals(ArrayUtils.EMPTY_INTEGER_OBJECT_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyIntObject() {
        final Integer[] original = new Integer[]{1, 2};
        assertArrayEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyShortObjectNull() throws Exception {
        assertArrayEquals(ArrayUtils.EMPTY_SHORT_OBJECT_ARRAY, ArrayUtils.nullToEmpty((Short[]) null));
    }

    @Test
    public void testNullToEmptyShortObjectEmptyArray() throws Exception {
        final Short[] empty = new Short[]{};
        final Short[] result = ArrayUtils.nullToEmpty(empty);
        assertArrayEquals(ArrayUtils.EMPTY_SHORT_OBJECT_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyShortObject() {
        @SuppressWarnings("boxing") final Short[] original = new Short[]{1, 2};
        assertArrayEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNUllToEmptyCharObjectNull() throws Exception {
        assertArrayEquals(ArrayUtils.EMPTY_CHARACTER_OBJECT_ARRAY, ArrayUtils.nullToEmpty((Character[]) null));
    }

    @Test
    public void testNullToEmptyCharObjectEmptyArray() throws Exception {
        final Character[] empty = new Character[]{};
        final Character[] result = ArrayUtils.nullToEmpty(empty);
        assertArrayEquals(ArrayUtils.EMPTY_CHARACTER_OBJECT_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyCharObject() {
        final Character[] original = new Character[]{'a', 'b'};
        assertArrayEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyByteObjectNull() throws Exception {
        assertArrayEquals(ArrayUtils.EMPTY_BYTE_OBJECT_ARRAY, ArrayUtils.nullToEmpty((Byte[]) null));
    }

    @Test
    public void testNullToEmptyByteObjectEmptyArray() throws Exception {
        final Byte[] empty = new Byte[]{};
        final Byte[] result = ArrayUtils.nullToEmpty(empty);
        assertArrayEquals(ArrayUtils.EMPTY_BYTE_OBJECT_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyByteObject() {
        final Byte[] original = new Byte[]{0x0F, 0x0E};
        assertArrayEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyDoubleObjectNull() throws Exception {
        assertArrayEquals(ArrayUtils.EMPTY_DOUBLE_OBJECT_ARRAY, ArrayUtils.nullToEmpty((Double[]) null));
    }

    @Test
    public void testNullToEmptyDoubleObjectEmptyArray() throws Exception {
        final Double[] empty = new Double[]{};
        final Double[] result = ArrayUtils.nullToEmpty(empty);
        assertArrayEquals(ArrayUtils.EMPTY_DOUBLE_OBJECT_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyDoubleObject() {
        final Double[] original = new Double[]{1D, 2D};
        assertArrayEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyFloatObjectNull() throws Exception {
        assertArrayEquals(ArrayUtils.EMPTY_FLOAT_OBJECT_ARRAY, ArrayUtils.nullToEmpty((Float[]) null));
    }

    @Test
    public void testNullToEmptyFloatObjectEmptyArray() throws Exception {
        final Float[] empty = new Float[]{};
        final Float[] result = ArrayUtils.nullToEmpty(empty);
        assertArrayEquals(ArrayUtils.EMPTY_FLOAT_OBJECT_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyFloatObject() {
        final Float[] original = new Float[]{2.6f, 3.8f};
        assertArrayEquals(original, ArrayUtils.nullToEmpty(original));
    }

    //-----------------------------------------------------------------------

    @Test
    public void testSubarrayObject() {
        final Object[] nullArray = null;
        final Object[] objectArray = {"a", "b", "c", "d", "e", "f"};

        assertEquals("0 start, mid end", "abcd",
                StringUtils.join(ArrayUtils.subarray(objectArray, 0, 4)));
        assertEquals("0 start, length end", "abcdef",
                StringUtils.join(ArrayUtils.subarray(objectArray, 0, objectArray.length)));
        assertEquals("mid start, mid end", "bcd",
                StringUtils.join(ArrayUtils.subarray(objectArray, 1, 4)));
        assertEquals("mid start, length end", "bcdef",
                StringUtils.join(ArrayUtils.subarray(objectArray, 1, objectArray.length)));

        assertNull("null input", ArrayUtils.subarray(nullArray, 0, 3));
        assertEquals("empty array", "",
                StringUtils.join(ArrayUtils.subarray(ArrayUtils.EMPTY_OBJECT_ARRAY, 1, 2)));
        assertEquals("start > end", "",
                StringUtils.join(ArrayUtils.subarray(objectArray, 4, 2)));
        assertEquals("start == end", "",
                StringUtils.join(ArrayUtils.subarray(objectArray, 3, 3)));
        assertEquals("start undershoot, normal end", "abcd",
                StringUtils.join(ArrayUtils.subarray(objectArray, -2, 4)));
        assertEquals("start overshoot, any end", "",
                StringUtils.join(ArrayUtils.subarray(objectArray, 33, 4)));
        assertEquals("normal start, end overshoot", "cdef",
                StringUtils.join(ArrayUtils.subarray(objectArray, 2, 33)));
        assertEquals("start undershoot, end overshoot", "abcdef",
                StringUtils.join(ArrayUtils.subarray(objectArray, -2, 12)));

        // array type tests
        final Date[] dateArray = {new java.sql.Date(new Date().getTime()),
                new Date(), new Date(), new Date(), new Date()};

        assertSame("Object type", Object.class,
                ArrayUtils.subarray(objectArray, 2, 4).getClass().getComponentType());
        assertSame("java.util.Date type", java.util.Date.class,
                ArrayUtils.subarray(dateArray, 1, 4).getClass().getComponentType());
        assertNotSame("java.sql.Date type", java.sql.Date.class,
                ArrayUtils.subarray(dateArray, 1, 4).getClass().getComponentType());
        try {
            @SuppressWarnings("unused") final java.sql.Date[] dummy = (java.sql.Date[]) ArrayUtils.subarray(dateArray, 1, 3);
            fail("Invalid downcast");
        } catch (final ClassCastException e) {
        }
    }

    @Test
    public void testSubarrayLong() {
        final long[] nullArray = null;
        final long[] array = {999910, 999911, 999912, 999913, 999914, 999915};
        final long[] leftSubarray = {999910, 999911, 999912, 999913};
        final long[] midSubarray = {999911, 999912, 999913, 999914};
        final long[] rightSubarray = {999912, 999913, 999914, 999915};

        assertTrue("0 start, mid end",
                ArrayUtils.isEquals(leftSubarray,
                        ArrayUtils.subarray(array, 0, 4)));

        assertTrue("0 start, length end",
                ArrayUtils.isEquals(array,
                        ArrayUtils.subarray(array, 0, array.length)));

        assertTrue("mid start, mid end",
                ArrayUtils.isEquals(midSubarray,
                        ArrayUtils.subarray(array, 1, 5)));

        assertTrue("mid start, length end",
                ArrayUtils.isEquals(rightSubarray,
                        ArrayUtils.subarray(array, 2, array.length)));


        assertNull("null input", ArrayUtils.subarray(nullArray, 0, 3));

        assertEquals("empty array", ArrayUtils.EMPTY_LONG_ARRAY,
                ArrayUtils.subarray(ArrayUtils.EMPTY_LONG_ARRAY, 1, 2));

        assertEquals("start > end", ArrayUtils.EMPTY_LONG_ARRAY,
                ArrayUtils.subarray(array, 4, 2));

        assertEquals("start == end", ArrayUtils.EMPTY_LONG_ARRAY,
                ArrayUtils.subarray(array, 3, 3));

        assertTrue("start undershoot, normal end",
                ArrayUtils.isEquals(leftSubarray,
                        ArrayUtils.subarray(array, -2, 4)));

        assertEquals("start overshoot, any end",
                ArrayUtils.EMPTY_LONG_ARRAY,
                ArrayUtils.subarray(array, 33, 4));

        assertTrue("normal start, end overshoot",
                ArrayUtils.isEquals(rightSubarray,
                        ArrayUtils.subarray(array, 2, 33)));

        assertTrue("start undershoot, end overshoot",
                ArrayUtils.isEquals(array,
                        ArrayUtils.subarray(array, -2, 12)));

        // empty-return tests

        assertSame("empty array, object test",
                ArrayUtils.EMPTY_LONG_ARRAY,
                ArrayUtils.subarray(ArrayUtils.EMPTY_LONG_ARRAY, 1, 2));

        assertSame("start > end, object test",
                ArrayUtils.EMPTY_LONG_ARRAY,
                ArrayUtils.subarray(array, 4, 1));

        assertSame("start == end, object test",
                ArrayUtils.EMPTY_LONG_ARRAY,
                ArrayUtils.subarray(array, 3, 3));

        assertSame("start overshoot, any end, object test",
                ArrayUtils.EMPTY_LONG_ARRAY,
                ArrayUtils.subarray(array, 8733, 4));

        // array type tests

        assertSame("long type", long.class,
                ArrayUtils.subarray(array, 2, 4).getClass().getComponentType());

    }

    @Test
    public void testSubarrayInt() {
        final int[] nullArray = null;
        final int[] array = {10, 11, 12, 13, 14, 15};
        final int[] leftSubarray = {10, 11, 12, 13};
        final int[] midSubarray = {11, 12, 13, 14};
        final int[] rightSubarray = {12, 13, 14, 15};


        assertTrue("0 start, mid end",
                ArrayUtils.isEquals(leftSubarray,
                        ArrayUtils.subarray(array, 0, 4)));

        assertTrue("0 start, length end",
                ArrayUtils.isEquals(array,
                        ArrayUtils.subarray(array, 0, array.length)));

        assertTrue("mid start, mid end",
                ArrayUtils.isEquals(midSubarray,
                        ArrayUtils.subarray(array, 1, 5)));

        assertTrue("mid start, length end",
                ArrayUtils.isEquals(rightSubarray,
                        ArrayUtils.subarray(array, 2, array.length)));


        assertNull("null input", ArrayUtils.subarray(nullArray, 0, 3));

        assertEquals("empty array", ArrayUtils.EMPTY_INT_ARRAY,
                ArrayUtils.subarray(ArrayUtils.EMPTY_INT_ARRAY, 1, 2));

        assertEquals("start > end", ArrayUtils.EMPTY_INT_ARRAY,
                ArrayUtils.subarray(array, 4, 2));

        assertEquals("start == end", ArrayUtils.EMPTY_INT_ARRAY,
                ArrayUtils.subarray(array, 3, 3));

        assertTrue("start undershoot, normal end",
                ArrayUtils.isEquals(leftSubarray,
                        ArrayUtils.subarray(array, -2, 4)));

        assertEquals("start overshoot, any end",
                ArrayUtils.EMPTY_INT_ARRAY,
                ArrayUtils.subarray(array, 33, 4));

        assertTrue("normal start, end overshoot",
                ArrayUtils.isEquals(rightSubarray,
                        ArrayUtils.subarray(array, 2, 33)));

        assertTrue("start undershoot, end overshoot",
                ArrayUtils.isEquals(array,
                        ArrayUtils.subarray(array, -2, 12)));

        // empty-return tests

        assertSame("empty array, object test",
                ArrayUtils.EMPTY_INT_ARRAY,
                ArrayUtils.subarray(ArrayUtils.EMPTY_INT_ARRAY, 1, 2));

        assertSame("start > end, object test",
                ArrayUtils.EMPTY_INT_ARRAY,
                ArrayUtils.subarray(array, 4, 1));

        assertSame("start == end, object test",
                ArrayUtils.EMPTY_INT_ARRAY,
                ArrayUtils.subarray(array, 3, 3));

        assertSame("start overshoot, any end, object test",
                ArrayUtils.EMPTY_INT_ARRAY,
                ArrayUtils.subarray(array, 8733, 4));

        // array type tests

        assertSame("int type", int.class,
                ArrayUtils.subarray(array, 2, 4).getClass().getComponentType());

    }

    @Test
    public void testSubarrayShort() {
        final short[] nullArray = null;
        final short[] array = {10, 11, 12, 13, 14, 15};
        final short[] leftSubarray = {10, 11, 12, 13};
        final short[] midSubarray = {11, 12, 13, 14};
        final short[] rightSubarray = {12, 13, 14, 15};


        assertTrue("0 start, mid end",
                ArrayUtils.isEquals(leftSubarray,
                        ArrayUtils.subarray(array, 0, 4)));

        assertTrue("0 start, length end",
                ArrayUtils.isEquals(array,
                        ArrayUtils.subarray(array, 0, array.length)));

        assertTrue("mid start, mid end",
                ArrayUtils.isEquals(midSubarray,
                        ArrayUtils.subarray(array, 1, 5)));

        assertTrue("mid start, length end",
                ArrayUtils.isEquals(rightSubarray,
                        ArrayUtils.subarray(array, 2, array.length)));


        assertNull("null input", ArrayUtils.subarray(nullArray, 0, 3));

        assertEquals("empty array", ArrayUtils.EMPTY_SHORT_ARRAY,
                ArrayUtils.subarray(ArrayUtils.EMPTY_SHORT_ARRAY, 1, 2));

        assertEquals("start > end", ArrayUtils.EMPTY_SHORT_ARRAY,
                ArrayUtils.subarray(array, 4, 2));

        assertEquals("start == end", ArrayUtils.EMPTY_SHORT_ARRAY,
                ArrayUtils.subarray(array, 3, 3));

        assertTrue("start undershoot, normal end",
                ArrayUtils.isEquals(leftSubarray,
                        ArrayUtils.subarray(array, -2, 4)));

        assertEquals("start overshoot, any end",
                ArrayUtils.EMPTY_SHORT_ARRAY,
                ArrayUtils.subarray(array, 33, 4));

        assertTrue("normal start, end overshoot",
                ArrayUtils.isEquals(rightSubarray,
                        ArrayUtils.subarray(array, 2, 33)));

        assertTrue("start undershoot, end overshoot",
                ArrayUtils.isEquals(array,
                        ArrayUtils.subarray(array, -2, 12)));

        // empty-return tests

        assertSame("empty array, object test",
                ArrayUtils.EMPTY_SHORT_ARRAY,
                ArrayUtils.subarray(ArrayUtils.EMPTY_SHORT_ARRAY, 1, 2));

        assertSame("start > end, object test",
                ArrayUtils.EMPTY_SHORT_ARRAY,
                ArrayUtils.subarray(array, 4, 1));

        assertSame("start == end, object test",
                ArrayUtils.EMPTY_SHORT_ARRAY,
                ArrayUtils.subarray(array, 3, 3));

        assertSame("start overshoot, any end, object test",
                ArrayUtils.EMPTY_SHORT_ARRAY,
                ArrayUtils.subarray(array, 8733, 4));

        // array type tests

        assertSame("short type", short.class,
                ArrayUtils.subarray(array, 2, 4).getClass().getComponentType());

    }

    @Test
    public void testSubarrChar() {
        final char[] nullArray = null;
        final char[] array = {'a', 'b', 'c', 'd', 'e', 'f'};
        final char[] leftSubarray = {'a', 'b', 'c', 'd',};
        final char[] midSubarray = {'b', 'c', 'd', 'e',};
        final char[] rightSubarray = {'c', 'd', 'e', 'f',};


        assertTrue("0 start, mid end",
                ArrayUtils.isEquals(leftSubarray,
                        ArrayUtils.subarray(array, 0, 4)));

        assertTrue("0 start, length end",
                ArrayUtils.isEquals(array,
                        ArrayUtils.subarray(array, 0, array.length)));

        assertTrue("mid start, mid end",
                ArrayUtils.isEquals(midSubarray,
                        ArrayUtils.subarray(array, 1, 5)));

        assertTrue("mid start, length end",
                ArrayUtils.isEquals(rightSubarray,
                        ArrayUtils.subarray(array, 2, array.length)));


        assertNull("null input", ArrayUtils.subarray(nullArray, 0, 3));

        assertEquals("empty array", ArrayUtils.EMPTY_CHAR_ARRAY,
                ArrayUtils.subarray(ArrayUtils.EMPTY_CHAR_ARRAY, 1, 2));

        assertEquals("start > end", ArrayUtils.EMPTY_CHAR_ARRAY,
                ArrayUtils.subarray(array, 4, 2));

        assertEquals("start == end", ArrayUtils.EMPTY_CHAR_ARRAY,
                ArrayUtils.subarray(array, 3, 3));

        assertTrue("start undershoot, normal end",
                ArrayUtils.isEquals(leftSubarray,
                        ArrayUtils.subarray(array, -2, 4)));

        assertEquals("start overshoot, any end",
                ArrayUtils.EMPTY_CHAR_ARRAY,
                ArrayUtils.subarray(array, 33, 4));

        assertTrue("normal start, end overshoot",
                ArrayUtils.isEquals(rightSubarray,
                        ArrayUtils.subarray(array, 2, 33)));

        assertTrue("start undershoot, end overshoot",
                ArrayUtils.isEquals(array,
                        ArrayUtils.subarray(array, -2, 12)));

        // empty-return tests

        assertSame("empty array, object test",
                ArrayUtils.EMPTY_CHAR_ARRAY,
                ArrayUtils.subarray(ArrayUtils.EMPTY_CHAR_ARRAY, 1, 2));

        assertSame("start > end, object test",
                ArrayUtils.EMPTY_CHAR_ARRAY,
                ArrayUtils.subarray(array, 4, 1));

        assertSame("start == end, object test",
                ArrayUtils.EMPTY_CHAR_ARRAY,
                ArrayUtils.subarray(array, 3, 3));

        assertSame("start overshoot, any end, object test",
                ArrayUtils.EMPTY_CHAR_ARRAY,
                ArrayUtils.subarray(array, 8733, 4));

        // array type tests

        assertSame("char type", char.class,
                ArrayUtils.subarray(array, 2, 4).getClass().getComponentType());

    }

    @Test
    public void testSubarrayByte() {
        final byte[] nullArray = null;
        final byte[] array = {10, 11, 12, 13, 14, 15};
        final byte[] leftSubarray = {10, 11, 12, 13};
        final byte[] midSubarray = {11, 12, 13, 14};
        final byte[] rightSubarray = {12, 13, 14, 15};


        assertTrue("0 start, mid end",
                ArrayUtils.isEquals(leftSubarray,
                        ArrayUtils.subarray(array, 0, 4)));

        assertTrue("0 start, length end",
                ArrayUtils.isEquals(array,
                        ArrayUtils.subarray(array, 0, array.length)));

        assertTrue("mid start, mid end",
                ArrayUtils.isEquals(midSubarray,
                        ArrayUtils.subarray(array, 1, 5)));

        assertTrue("mid start, length end",
                ArrayUtils.isEquals(rightSubarray,
                        ArrayUtils.subarray(array, 2, array.length)));


        assertNull("null input", ArrayUtils.subarray(nullArray, 0, 3));

        assertEquals("empty array", ArrayUtils.EMPTY_BYTE_ARRAY,
                ArrayUtils.subarray(ArrayUtils.EMPTY_BYTE_ARRAY, 1, 2));

        assertEquals("start > end", ArrayUtils.EMPTY_BYTE_ARRAY,
                ArrayUtils.subarray(array, 4, 2));

        assertEquals("start == end", ArrayUtils.EMPTY_BYTE_ARRAY,
                ArrayUtils.subarray(array, 3, 3));

        assertTrue("start undershoot, normal end",
                ArrayUtils.isEquals(leftSubarray,
                        ArrayUtils.subarray(array, -2, 4)));

        assertEquals("start overshoot, any end",
                ArrayUtils.EMPTY_BYTE_ARRAY,
                ArrayUtils.subarray(array, 33, 4));

        assertTrue("normal start, end overshoot",
                ArrayUtils.isEquals(rightSubarray,
                        ArrayUtils.subarray(array, 2, 33)));

        assertTrue("start undershoot, end overshoot",
                ArrayUtils.isEquals(array,
                        ArrayUtils.subarray(array, -2, 12)));

        // empty-return tests

        assertSame("empty array, object test",
                ArrayUtils.EMPTY_BYTE_ARRAY,
                ArrayUtils.subarray(ArrayUtils.EMPTY_BYTE_ARRAY, 1, 2));

        assertSame("start > end, object test",
                ArrayUtils.EMPTY_BYTE_ARRAY,
                ArrayUtils.subarray(array, 4, 1));

        assertSame("start == end, object test",
                ArrayUtils.EMPTY_BYTE_ARRAY,
                ArrayUtils.subarray(array, 3, 3));

        assertSame("start overshoot, any end, object test",
                ArrayUtils.EMPTY_BYTE_ARRAY,
                ArrayUtils.subarray(array, 8733, 4));

        // array type tests

        assertSame("byte type", byte.class,
                ArrayUtils.subarray(array, 2, 4).getClass().getComponentType());

    }

    @Test
    public void testSubarrayDouble() {
        final double[] nullArray = null;
        final double[] array = {10.123, 11.234, 12.345, 13.456, 14.567, 15.678};
        final double[] leftSubarray = {10.123, 11.234, 12.345, 13.456,};
        final double[] midSubarray = {11.234, 12.345, 13.456, 14.567,};
        final double[] rightSubarray = {12.345, 13.456, 14.567, 15.678};


        assertTrue("0 start, mid end",
                ArrayUtils.isEquals(leftSubarray,
                        ArrayUtils.subarray(array, 0, 4)));

        assertTrue("0 start, length end",
                ArrayUtils.isEquals(array,
                        ArrayUtils.subarray(array, 0, array.length)));

        assertTrue("mid start, mid end",
                ArrayUtils.isEquals(midSubarray,
                        ArrayUtils.subarray(array, 1, 5)));

        assertTrue("mid start, length end",
                ArrayUtils.isEquals(rightSubarray,
                        ArrayUtils.subarray(array, 2, array.length)));


        assertNull("null input", ArrayUtils.subarray(nullArray, 0, 3));

        assertEquals("empty array", ArrayUtils.EMPTY_DOUBLE_ARRAY,
                ArrayUtils.subarray(ArrayUtils.EMPTY_DOUBLE_ARRAY, 1, 2));

        assertEquals("start > end", ArrayUtils.EMPTY_DOUBLE_ARRAY,
                ArrayUtils.subarray(array, 4, 2));

        assertEquals("start == end", ArrayUtils.EMPTY_DOUBLE_ARRAY,
                ArrayUtils.subarray(array, 3, 3));

        assertTrue("start undershoot, normal end",
                ArrayUtils.isEquals(leftSubarray,
                        ArrayUtils.subarray(array, -2, 4)));

        assertEquals("start overshoot, any end",
                ArrayUtils.EMPTY_DOUBLE_ARRAY,
                ArrayUtils.subarray(array, 33, 4));

        assertTrue("normal start, end overshoot",
                ArrayUtils.isEquals(rightSubarray,
                        ArrayUtils.subarray(array, 2, 33)));

        assertTrue("start undershoot, end overshoot",
                ArrayUtils.isEquals(array,
                        ArrayUtils.subarray(array, -2, 12)));

        // empty-return tests

        assertSame("empty array, object test",
                ArrayUtils.EMPTY_DOUBLE_ARRAY,
                ArrayUtils.subarray(ArrayUtils.EMPTY_DOUBLE_ARRAY, 1, 2));

        assertSame("start > end, object test",
                ArrayUtils.EMPTY_DOUBLE_ARRAY,
                ArrayUtils.subarray(array, 4, 1));

        assertSame("start == end, object test",
                ArrayUtils.EMPTY_DOUBLE_ARRAY,
                ArrayUtils.subarray(array, 3, 3));

        assertSame("start overshoot, any end, object test",
                ArrayUtils.EMPTY_DOUBLE_ARRAY,
                ArrayUtils.subarray(array, 8733, 4));

        // array type tests

        assertSame("double type", double.class,
                ArrayUtils.subarray(array, 2, 4).getClass().getComponentType());

    }

    @Test
    public void testSubarrayFloat() {
        final float[] nullArray = null;
        final float[] array = {10, 11, 12, 13, 14, 15};
        final float[] leftSubarray = {10, 11, 12, 13};
        final float[] midSubarray = {11, 12, 13, 14};
        final float[] rightSubarray = {12, 13, 14, 15};


        assertTrue("0 start, mid end",
                ArrayUtils.isEquals(leftSubarray,
                        ArrayUtils.subarray(array, 0, 4)));

        assertTrue("0 start, length end",
                ArrayUtils.isEquals(array,
                        ArrayUtils.subarray(array, 0, array.length)));

        assertTrue("mid start, mid end",
                ArrayUtils.isEquals(midSubarray,
                        ArrayUtils.subarray(array, 1, 5)));

        assertTrue("mid start, length end",
                ArrayUtils.isEquals(rightSubarray,
                        ArrayUtils.subarray(array, 2, array.length)));


        assertNull("null input", ArrayUtils.subarray(nullArray, 0, 3));

        assertEquals("empty array", ArrayUtils.EMPTY_FLOAT_ARRAY,
                ArrayUtils.subarray(ArrayUtils.EMPTY_FLOAT_ARRAY, 1, 2));

        assertEquals("start > end", ArrayUtils.EMPTY_FLOAT_ARRAY,
                ArrayUtils.subarray(array, 4, 2));

        assertEquals("start == end", ArrayUtils.EMPTY_FLOAT_ARRAY,
                ArrayUtils.subarray(array, 3, 3));

        assertTrue("start undershoot, normal end",
                ArrayUtils.isEquals(leftSubarray,
                        ArrayUtils.subarray(array, -2, 4)));

        assertEquals("start overshoot, any end",
                ArrayUtils.EMPTY_FLOAT_ARRAY,
                ArrayUtils.subarray(array, 33, 4));

        assertTrue("normal start, end overshoot",
                ArrayUtils.isEquals(rightSubarray,
                        ArrayUtils.subarray(array, 2, 33)));

        assertTrue("start undershoot, end overshoot",
                ArrayUtils.isEquals(array,
                        ArrayUtils.subarray(array, -2, 12)));

        // empty-return tests

        assertSame("empty array, object test",
                ArrayUtils.EMPTY_FLOAT_ARRAY,
                ArrayUtils.subarray(ArrayUtils.EMPTY_FLOAT_ARRAY, 1, 2));

        assertSame("start > end, object test",
                ArrayUtils.EMPTY_FLOAT_ARRAY,
                ArrayUtils.subarray(array, 4, 1));

        assertSame("start == end, object test",
                ArrayUtils.EMPTY_FLOAT_ARRAY,
                ArrayUtils.subarray(array, 3, 3));

        assertSame("start overshoot, any end, object test",
                ArrayUtils.EMPTY_FLOAT_ARRAY,
                ArrayUtils.subarray(array, 8733, 4));

        // array type tests

        assertSame("float type", float.class,
                ArrayUtils.subarray(array, 2, 4).getClass().getComponentType());

    }

    @Test
    public void testSubarrayBoolean() {
        final boolean[] nullArray = null;
        final boolean[] array = {true, true, false, true, false, true};
        final boolean[] leftSubarray = {true, true, false, true};
        final boolean[] midSubarray = {true, false, true, false};
        final boolean[] rightSubarray = {false, true, false, true};


        assertTrue("0 start, mid end",
                ArrayUtils.isEquals(leftSubarray,
                        ArrayUtils.subarray(array, 0, 4)));

        assertTrue("0 start, length end",
                ArrayUtils.isEquals(array,
                        ArrayUtils.subarray(array, 0, array.length)));

        assertTrue("mid start, mid end",
                ArrayUtils.isEquals(midSubarray,
                        ArrayUtils.subarray(array, 1, 5)));

        assertTrue("mid start, length end",
                ArrayUtils.isEquals(rightSubarray,
                        ArrayUtils.subarray(array, 2, array.length)));


        assertNull("null input", ArrayUtils.subarray(nullArray, 0, 3));

        assertEquals("empty array", ArrayUtils.EMPTY_BOOLEAN_ARRAY,
                ArrayUtils.subarray(ArrayUtils.EMPTY_BOOLEAN_ARRAY, 1, 2));

        assertEquals("start > end", ArrayUtils.EMPTY_BOOLEAN_ARRAY,
                ArrayUtils.subarray(array, 4, 2));

        assertEquals("start == end", ArrayUtils.EMPTY_BOOLEAN_ARRAY,
                ArrayUtils.subarray(array, 3, 3));

        assertTrue("start undershoot, normal end",
                ArrayUtils.isEquals(leftSubarray,
                        ArrayUtils.subarray(array, -2, 4)));

        assertEquals("start overshoot, any end",
                ArrayUtils.EMPTY_BOOLEAN_ARRAY,
                ArrayUtils.subarray(array, 33, 4));

        assertTrue("normal start, end overshoot",
                ArrayUtils.isEquals(rightSubarray,
                        ArrayUtils.subarray(array, 2, 33)));

        assertTrue("start undershoot, end overshoot",
                ArrayUtils.isEquals(array,
                        ArrayUtils.subarray(array, -2, 12)));

        // empty-return tests

        assertSame("empty array, object test",
                ArrayUtils.EMPTY_BOOLEAN_ARRAY,
                ArrayUtils.subarray(ArrayUtils.EMPTY_BOOLEAN_ARRAY, 1, 2));

        assertSame("start > end, object test",
                ArrayUtils.EMPTY_BOOLEAN_ARRAY,
                ArrayUtils.subarray(array, 4, 1));

        assertSame("start == end, object test",
                ArrayUtils.EMPTY_BOOLEAN_ARRAY,
                ArrayUtils.subarray(array, 3, 3));

        assertSame("start overshoot, any end, object test",
                ArrayUtils.EMPTY_BOOLEAN_ARRAY,
                ArrayUtils.subarray(array, 8733, 4));

        // array type tests

        assertSame("boolean type", boolean.class,
                ArrayUtils.subarray(array, 2, 4).getClass().getComponentType());

    }

    //-----------------------------------------------------------------------
    @Test
    public void testSameLength() {
        final Object[] nullArray = null;
        final Object[] emptyArray = new Object[0];
        final Object[] oneArray = new Object[]{"pick"};
        final Object[] twoArray = new Object[]{"pick", "stick"};

        assertTrue(ArrayUtils.isSameLength(nullArray, nullArray));
        assertTrue(ArrayUtils.isSameLength(nullArray, emptyArray));
        assertFalse(ArrayUtils.isSameLength(nullArray, oneArray));
        assertFalse(ArrayUtils.isSameLength(nullArray, twoArray));

        assertTrue(ArrayUtils.isSameLength(emptyArray, nullArray));
        assertTrue(ArrayUtils.isSameLength(emptyArray, emptyArray));
        assertFalse(ArrayUtils.isSameLength(emptyArray, oneArray));
        assertFalse(ArrayUtils.isSameLength(emptyArray, twoArray));

        assertFalse(ArrayUtils.isSameLength(oneArray, nullArray));
        assertFalse(ArrayUtils.isSameLength(oneArray, emptyArray));
        assertTrue(ArrayUtils.isSameLength(oneArray, oneArray));
        assertFalse(ArrayUtils.isSameLength(oneArray, twoArray));

        assertFalse(ArrayUtils.isSameLength(twoArray, nullArray));
        assertFalse(ArrayUtils.isSameLength(twoArray, emptyArray));
        assertFalse(ArrayUtils.isSameLength(twoArray, oneArray));
        assertTrue(ArrayUtils.isSameLength(twoArray, twoArray));
    }

    @Test
    public void testSameLengthBoolean() {
        final boolean[] nullArray = null;
        final boolean[] emptyArray = new boolean[0];
        final boolean[] oneArray = new boolean[]{true};
        final boolean[] twoArray = new boolean[]{true, false};

        assertTrue(ArrayUtils.isSameLength(nullArray, nullArray));
        assertTrue(ArrayUtils.isSameLength(nullArray, emptyArray));
        assertFalse(ArrayUtils.isSameLength(nullArray, oneArray));
        assertFalse(ArrayUtils.isSameLength(nullArray, twoArray));

        assertTrue(ArrayUtils.isSameLength(emptyArray, nullArray));
        assertTrue(ArrayUtils.isSameLength(emptyArray, emptyArray));
        assertFalse(ArrayUtils.isSameLength(emptyArray, oneArray));
        assertFalse(ArrayUtils.isSameLength(emptyArray, twoArray));

        assertFalse(ArrayUtils.isSameLength(oneArray, nullArray));
        assertFalse(ArrayUtils.isSameLength(oneArray, emptyArray));
        assertTrue(ArrayUtils.isSameLength(oneArray, oneArray));
        assertFalse(ArrayUtils.isSameLength(oneArray, twoArray));

        assertFalse(ArrayUtils.isSameLength(twoArray, nullArray));
        assertFalse(ArrayUtils.isSameLength(twoArray, emptyArray));
        assertFalse(ArrayUtils.isSameLength(twoArray, oneArray));
        assertTrue(ArrayUtils.isSameLength(twoArray, twoArray));
    }

    @Test
    public void testSameLengthLong() {
        final long[] nullArray = null;
        final long[] emptyArray = new long[0];
        final long[] oneArray = new long[]{0L};
        final long[] twoArray = new long[]{0L, 76L};

        assertTrue(ArrayUtils.isSameLength(nullArray, nullArray));
        assertTrue(ArrayUtils.isSameLength(nullArray, emptyArray));
        assertFalse(ArrayUtils.isSameLength(nullArray, oneArray));
        assertFalse(ArrayUtils.isSameLength(nullArray, twoArray));

        assertTrue(ArrayUtils.isSameLength(emptyArray, nullArray));
        assertTrue(ArrayUtils.isSameLength(emptyArray, emptyArray));
        assertFalse(ArrayUtils.isSameLength(emptyArray, oneArray));
        assertFalse(ArrayUtils.isSameLength(emptyArray, twoArray));

        assertFalse(ArrayUtils.isSameLength(oneArray, nullArray));
        assertFalse(ArrayUtils.isSameLength(oneArray, emptyArray));
        assertTrue(ArrayUtils.isSameLength(oneArray, oneArray));
        assertFalse(ArrayUtils.isSameLength(oneArray, twoArray));

        assertFalse(ArrayUtils.isSameLength(twoArray, nullArray));
        assertFalse(ArrayUtils.isSameLength(twoArray, emptyArray));
        assertFalse(ArrayUtils.isSameLength(twoArray, oneArray));
        assertTrue(ArrayUtils.isSameLength(twoArray, twoArray));
    }

    @Test
    public void testSameLengthInt() {
        final int[] nullArray = null;
        final int[] emptyArray = new int[0];
        final int[] oneArray = new int[]{4};
        final int[] twoArray = new int[]{5, 7};

        assertTrue(ArrayUtils.isSameLength(nullArray, nullArray));
        assertTrue(ArrayUtils.isSameLength(nullArray, emptyArray));
        assertFalse(ArrayUtils.isSameLength(nullArray, oneArray));
        assertFalse(ArrayUtils.isSameLength(nullArray, twoArray));

        assertTrue(ArrayUtils.isSameLength(emptyArray, nullArray));
        assertTrue(ArrayUtils.isSameLength(emptyArray, emptyArray));
        assertFalse(ArrayUtils.isSameLength(emptyArray, oneArray));
        assertFalse(ArrayUtils.isSameLength(emptyArray, twoArray));

        assertFalse(ArrayUtils.isSameLength(oneArray, nullArray));
        assertFalse(ArrayUtils.isSameLength(oneArray, emptyArray));
        assertTrue(ArrayUtils.isSameLength(oneArray, oneArray));
        assertFalse(ArrayUtils.isSameLength(oneArray, twoArray));

        assertFalse(ArrayUtils.isSameLength(twoArray, nullArray));
        assertFalse(ArrayUtils.isSameLength(twoArray, emptyArray));
        assertFalse(ArrayUtils.isSameLength(twoArray, oneArray));
        assertTrue(ArrayUtils.isSameLength(twoArray, twoArray));
    }

    @Test
    public void testSameLengthShort() {
        final short[] nullArray = null;
        final short[] emptyArray = new short[0];
        final short[] oneArray = new short[]{4};
        final short[] twoArray = new short[]{6, 8};

        assertTrue(ArrayUtils.isSameLength(nullArray, nullArray));
        assertTrue(ArrayUtils.isSameLength(nullArray, emptyArray));
        assertFalse(ArrayUtils.isSameLength(nullArray, oneArray));
        assertFalse(ArrayUtils.isSameLength(nullArray, twoArray));

        assertTrue(ArrayUtils.isSameLength(emptyArray, nullArray));
        assertTrue(ArrayUtils.isSameLength(emptyArray, emptyArray));
        assertFalse(ArrayUtils.isSameLength(emptyArray, oneArray));
        assertFalse(ArrayUtils.isSameLength(emptyArray, twoArray));

        assertFalse(ArrayUtils.isSameLength(oneArray, nullArray));
        assertFalse(ArrayUtils.isSameLength(oneArray, emptyArray));
        assertTrue(ArrayUtils.isSameLength(oneArray, oneArray));
        assertFalse(ArrayUtils.isSameLength(oneArray, twoArray));

        assertFalse(ArrayUtils.isSameLength(twoArray, nullArray));
        assertFalse(ArrayUtils.isSameLength(twoArray, emptyArray));
        assertFalse(ArrayUtils.isSameLength(twoArray, oneArray));
        assertTrue(ArrayUtils.isSameLength(twoArray, twoArray));
    }

    @Test
    public void testSameLengthChar() {
        final char[] nullArray = null;
        final char[] emptyArray = new char[0];
        final char[] oneArray = new char[]{'f'};
        final char[] twoArray = new char[]{'d', 't'};

        assertTrue(ArrayUtils.isSameLength(nullArray, nullArray));
        assertTrue(ArrayUtils.isSameLength(nullArray, emptyArray));
        assertFalse(ArrayUtils.isSameLength(nullArray, oneArray));
        assertFalse(ArrayUtils.isSameLength(nullArray, twoArray));

        assertTrue(ArrayUtils.isSameLength(emptyArray, nullArray));
        assertTrue(ArrayUtils.isSameLength(emptyArray, emptyArray));
        assertFalse(ArrayUtils.isSameLength(emptyArray, oneArray));
        assertFalse(ArrayUtils.isSameLength(emptyArray, twoArray));

        assertFalse(ArrayUtils.isSameLength(oneArray, nullArray));
        assertFalse(ArrayUtils.isSameLength(oneArray, emptyArray));
        assertTrue(ArrayUtils.isSameLength(oneArray, oneArray));
        assertFalse(ArrayUtils.isSameLength(oneArray, twoArray));

        assertFalse(ArrayUtils.isSameLength(twoArray, nullArray));
        assertFalse(ArrayUtils.isSameLength(twoArray, emptyArray));
        assertFalse(ArrayUtils.isSameLength(twoArray, oneArray));
        assertTrue(ArrayUtils.isSameLength(twoArray, twoArray));
    }

    @Test
    public void testSameLengthByte() {
        final byte[] nullArray = null;
        final byte[] emptyArray = new byte[0];
        final byte[] oneArray = new byte[]{3};
        final byte[] twoArray = new byte[]{4, 6};

        assertTrue(ArrayUtils.isSameLength(nullArray, nullArray));
        assertTrue(ArrayUtils.isSameLength(nullArray, emptyArray));
        assertFalse(ArrayUtils.isSameLength(nullArray, oneArray));
        assertFalse(ArrayUtils.isSameLength(nullArray, twoArray));

        assertTrue(ArrayUtils.isSameLength(emptyArray, nullArray));
        assertTrue(ArrayUtils.isSameLength(emptyArray, emptyArray));
        assertFalse(ArrayUtils.isSameLength(emptyArray, oneArray));
        assertFalse(ArrayUtils.isSameLength(emptyArray, twoArray));

        assertFalse(ArrayUtils.isSameLength(oneArray, nullArray));
        assertFalse(ArrayUtils.isSameLength(oneArray, emptyArray));
        assertTrue(ArrayUtils.isSameLength(oneArray, oneArray));
        assertFalse(ArrayUtils.isSameLength(oneArray, twoArray));

        assertFalse(ArrayUtils.isSameLength(twoArray, nullArray));
        assertFalse(ArrayUtils.isSameLength(twoArray, emptyArray));
        assertFalse(ArrayUtils.isSameLength(twoArray, oneArray));
        assertTrue(ArrayUtils.isSameLength(twoArray, twoArray));
    }

    @Test
    public void testSameLengthDouble() {
        final double[] nullArray = null;
        final double[] emptyArray = new double[0];
        final double[] oneArray = new double[]{1.3d};
        final double[] twoArray = new double[]{4.5d, 6.3d};

        assertTrue(ArrayUtils.isSameLength(nullArray, nullArray));
        assertTrue(ArrayUtils.isSameLength(nullArray, emptyArray));
        assertFalse(ArrayUtils.isSameLength(nullArray, oneArray));
        assertFalse(ArrayUtils.isSameLength(nullArray, twoArray));

        assertTrue(ArrayUtils.isSameLength(emptyArray, nullArray));
        assertTrue(ArrayUtils.isSameLength(emptyArray, emptyArray));
        assertFalse(ArrayUtils.isSameLength(emptyArray, oneArray));
        assertFalse(ArrayUtils.isSameLength(emptyArray, twoArray));

        assertFalse(ArrayUtils.isSameLength(oneArray, nullArray));
        assertFalse(ArrayUtils.isSameLength(oneArray, emptyArray));
        assertTrue(ArrayUtils.isSameLength(oneArray, oneArray));
        assertFalse(ArrayUtils.isSameLength(oneArray, twoArray));

        assertFalse(ArrayUtils.isSameLength(twoArray, nullArray));
        assertFalse(ArrayUtils.isSameLength(twoArray, emptyArray));
        assertFalse(ArrayUtils.isSameLength(twoArray, oneArray));
        assertTrue(ArrayUtils.isSameLength(twoArray, twoArray));
    }

    @Test
    public void testSameLengthFloat() {
        final float[] nullArray = null;
        final float[] emptyArray = new float[0];
        final float[] oneArray = new float[]{2.5f};
        final float[] twoArray = new float[]{6.4f, 5.8f};

        assertTrue(ArrayUtils.isSameLength(nullArray, nullArray));
        assertTrue(ArrayUtils.isSameLength(nullArray, emptyArray));
        assertFalse(ArrayUtils.isSameLength(nullArray, oneArray));
        assertFalse(ArrayUtils.isSameLength(nullArray, twoArray));

        assertTrue(ArrayUtils.isSameLength(emptyArray, nullArray));
        assertTrue(ArrayUtils.isSameLength(emptyArray, emptyArray));
        assertFalse(ArrayUtils.isSameLength(emptyArray, oneArray));
        assertFalse(ArrayUtils.isSameLength(emptyArray, twoArray));

        assertFalse(ArrayUtils.isSameLength(oneArray, nullArray));
        assertFalse(ArrayUtils.isSameLength(oneArray, emptyArray));
        assertTrue(ArrayUtils.isSameLength(oneArray, oneArray));
        assertFalse(ArrayUtils.isSameLength(oneArray, twoArray));

        assertFalse(ArrayUtils.isSameLength(twoArray, nullArray));
        assertFalse(ArrayUtils.isSameLength(twoArray, emptyArray));
        assertFalse(ArrayUtils.isSameLength(twoArray, oneArray));
        assertTrue(ArrayUtils.isSameLength(twoArray, twoArray));
    }

    //-----------------------------------------------------------------------
    @Test
    public void testSameType() {
        try {
            ArrayUtils.isSameType(null, null);
            fail();
        } catch (final IllegalArgumentException ex) {
        }
        try {
            ArrayUtils.isSameType(null, new Object[0]);
            fail();
        } catch (final IllegalArgumentException ex) {
        }
        try {
            ArrayUtils.isSameType(new Object[0], null);
            fail();
        } catch (final IllegalArgumentException ex) {
        }

        assertTrue(ArrayUtils.isSameType(new Object[0], new Object[0]));
        assertFalse(ArrayUtils.isSameType(new String[0], new Object[0]));
        assertTrue(ArrayUtils.isSameType(new String[0][0], new String[0][0]));
        assertFalse(ArrayUtils.isSameType(new String[0], new String[0][0]));
        assertFalse(ArrayUtils.isSameType(new String[0][0], new String[0]));
    }

    //-----------------------------------------------------------------------
    @Test
    public void testReverse() {
        final StringBuffer str1 = new StringBuffer("pick");
        final String str2 = "a";
        final String[] str3 = new String[]{"stick"};
        final String str4 = "up";

        Object[] array = new Object[]{str1, str2, str3};
        ArrayUtils.reverse(array);
        assertEquals(array[0], str3);
        assertEquals(array[1], str2);
        assertEquals(array[2], str1);

        array = new Object[]{str1, str2, str3, str4};
        ArrayUtils.reverse(array);
        assertEquals(array[0], str4);
        assertEquals(array[1], str3);
        assertEquals(array[2], str2);
        assertEquals(array[3], str1);

        array = null;
        ArrayUtils.reverse(array);
        assertArrayEquals(null, array);
    }

    @Test
    public void testReverseLong() {
        long[] array = new long[]{1L, 2L, 3L};
        ArrayUtils.reverse(array);
        assertEquals(array[0], 3L);
        assertEquals(array[1], 2L);
        assertEquals(array[2], 1L);

        array = null;
        ArrayUtils.reverse(array);
        assertEquals(null, array);
    }

    @Test
    public void testReverseInt() {
        int[] array = new int[]{1, 2, 3};
        ArrayUtils.reverse(array);
        assertEquals(array[0], 3);
        assertEquals(array[1], 2);
        assertEquals(array[2], 1);

        array = null;
        ArrayUtils.reverse(array);
        assertEquals(null, array);
    }

    @Test
    public void testReverseShort() {
        short[] array = new short[]{1, 2, 3};
        ArrayUtils.reverse(array);
        assertEquals(array[0], 3);
        assertEquals(array[1], 2);
        assertEquals(array[2], 1);

        array = null;
        ArrayUtils.reverse(array);
        assertEquals(null, array);
    }

    @Test
    public void testReverseChar() {
        char[] array = new char[]{'a', 'f', 'C'};
        ArrayUtils.reverse(array);
        assertEquals(array[0], 'C');
        assertEquals(array[1], 'f');
        assertEquals(array[2], 'a');

        array = null;
        ArrayUtils.reverse(array);
        assertEquals(null, array);
    }

    @Test
    public void testReverseByte() {
        byte[] array = new byte[]{2, 3, 4};
        ArrayUtils.reverse(array);
        assertEquals(array[0], 4);
        assertEquals(array[1], 3);
        assertEquals(array[2], 2);

        array = null;
        ArrayUtils.reverse(array);
        assertEquals(null, array);
    }

    @Test
    public void testReverseDouble() {
        double[] array = new double[]{0.3d, 0.4d, 0.5d};
        ArrayUtils.reverse(array);
        assertEquals(array[0], 0.5d, 0.0d);
        assertEquals(array[1], 0.4d, 0.0d);
        assertEquals(array[2], 0.3d, 0.0d);

        array = null;
        ArrayUtils.reverse(array);
        assertEquals(null, array);
    }

    @Test
    public void testReverseFloat() {
        float[] array = new float[]{0.3f, 0.4f, 0.5f};
        ArrayUtils.reverse(array);
        assertEquals(array[0], 0.5f, 0.0f);
        assertEquals(array[1], 0.4f, 0.0f);
        assertEquals(array[2], 0.3f, 0.0f);

        array = null;
        ArrayUtils.reverse(array);
        assertEquals(null, array);
    }

    @Test
    public void testReverseBoolean() {
        boolean[] array = new boolean[]{false, false, true};
        ArrayUtils.reverse(array);
        assertTrue(array[0]);
        assertFalse(array[1]);
        assertFalse(array[2]);

        array = null;
        ArrayUtils.reverse(array);
        assertEquals(null, array);
    }

    @Test
    public void testReverseBooleanRange() {
        boolean[] array = new boolean[]{false, false, true};
        // The whole array
        ArrayUtils.reverse(array, 0, 3);
        assertTrue(array[0]);
        assertFalse(array[1]);
        assertFalse(array[2]);
        // a range
        array = new boolean[]{false, false, true};
        ArrayUtils.reverse(array, 0, 2);
        assertFalse(array[0]);
        assertFalse(array[1]);
        assertTrue(array[2]);
        // a range with a negative start
        array = new boolean[]{false, false, true};
        ArrayUtils.reverse(array, -1, 3);
        assertTrue(array[0]);
        assertFalse(array[1]);
        assertFalse(array[2]);
        // a range with a large stop index
        array = new boolean[]{false, false, true};
        ArrayUtils.reverse(array, -1, array.length + 1000);
        assertTrue(array[0]);
        assertFalse(array[1]);
        assertFalse(array[2]);
        // null
        array = null;
        ArrayUtils.reverse(array, 0, 3);
        assertEquals(null, array);
    }

    @Test
    public void testReverseByteRange() {
        byte[] array = new byte[]{1, 2, 3};
        // The whole array
        ArrayUtils.reverse(array, 0, 3);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);
        // a range
        array = new byte[]{1, 2, 3};
        ArrayUtils.reverse(array, 0, 2);
        assertEquals(2, array[0]);
        assertEquals(1, array[1]);
        assertEquals(3, array[2]);
        // a range with a negative start
        array = new byte[]{1, 2, 3};
        ArrayUtils.reverse(array, -1, 3);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);
        // a range with a large stop index
        array = new byte[]{1, 2, 3};
        ArrayUtils.reverse(array, -1, array.length + 1000);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);
        // null
        array = null;
        ArrayUtils.reverse(array, 0, 3);
        assertEquals(null, array);
    }

    @Test
    public void testReverseCharRange() {
        char[] array = new char[]{1, 2, 3};
        // The whole array
        ArrayUtils.reverse(array, 0, 3);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);
        // a range
        array = new char[]{1, 2, 3};
        ArrayUtils.reverse(array, 0, 2);
        assertEquals(2, array[0]);
        assertEquals(1, array[1]);
        assertEquals(3, array[2]);
        // a range with a negative start
        array = new char[]{1, 2, 3};
        ArrayUtils.reverse(array, -1, 3);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);
        // a range with a large stop index
        array = new char[]{1, 2, 3};
        ArrayUtils.reverse(array, -1, array.length + 1000);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);
        // null
        array = null;
        ArrayUtils.reverse(array, 0, 3);
        assertEquals(null, array);
    }

    @Test
    public void testReverseDoubleRange() {
        double[] array = new double[]{1, 2, 3};
        // The whole array
        ArrayUtils.reverse(array, 0, 3);
        assertEquals(3, array[0], 0);
        assertEquals(2, array[1], 0);
        assertEquals(1, array[2], 0);
        // a range
        array = new double[]{1, 2, 3};
        ArrayUtils.reverse(array, 0, 2);
        assertEquals(2, array[0], 0);
        assertEquals(1, array[1], 0);
        assertEquals(3, array[2], 0);
        // a range with a negative start
        array = new double[]{1, 2, 3};
        ArrayUtils.reverse(array, -1, 3);
        assertEquals(3, array[0], 0);
        assertEquals(2, array[1], 0);
        assertEquals(1, array[2], 0);
        // a range with a large stop index
        array = new double[]{1, 2, 3};
        ArrayUtils.reverse(array, -1, array.length + 1000);
        assertEquals(3, array[0], 0);
        assertEquals(2, array[1], 0);
        assertEquals(1, array[2], 0);
        // null
        array = null;
        ArrayUtils.reverse(array, 0, 3);
        assertEquals(null, array);
    }

    @Test
    public void testReverseFloatRange() {
        float[] array = new float[]{1, 2, 3};
        // The whole array
        ArrayUtils.reverse(array, 0, 3);
        assertEquals(3, array[0], 0);
        assertEquals(2, array[1], 0);
        assertEquals(1, array[2], 0);
        // a range
        array = new float[]{1, 2, 3};
        ArrayUtils.reverse(array, 0, 2);
        assertEquals(2, array[0], 0);
        assertEquals(1, array[1], 0);
        assertEquals(3, array[2], 0);
        // a range with a negative start
        array = new float[]{1, 2, 3};
        ArrayUtils.reverse(array, -1, 3);
        assertEquals(3, array[0], 0);
        assertEquals(2, array[1], 0);
        assertEquals(1, array[2], 0);
        // a range with a large stop index
        array = new float[]{1, 2, 3};
        ArrayUtils.reverse(array, -1, array.length + 1000);
        assertEquals(3, array[0], 0);
        assertEquals(2, array[1], 0);
        assertEquals(1, array[2], 0);
        // null
        array = null;
        ArrayUtils.reverse(array, 0, 3);
        assertEquals(null, array);
    }

    @Test
    public void testReverseIntRange() {
        int[] array = new int[]{1, 2, 3};
        // The whole array
        ArrayUtils.reverse(array, 0, 3);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);
        // a range
        array = new int[]{1, 2, 3};
        ArrayUtils.reverse(array, 0, 2);
        assertEquals(2, array[0]);
        assertEquals(1, array[1]);
        assertEquals(3, array[2]);
        // a range with a negative start
        array = new int[]{1, 2, 3};
        ArrayUtils.reverse(array, -1, 3);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);
        // a range with a large stop index
        array = new int[]{1, 2, 3};
        ArrayUtils.reverse(array, -1, array.length + 1000);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);
        // null
        array = null;
        ArrayUtils.reverse(array, 0, 3);
        assertEquals(null, array);
    }

    @Test
    public void testReverseLongRange() {
        long[] array = new long[]{1, 2, 3};
        // The whole array
        ArrayUtils.reverse(array, 0, 3);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);
        // a range
        array = new long[]{1, 2, 3};
        ArrayUtils.reverse(array, 0, 2);
        assertEquals(2, array[0]);
        assertEquals(1, array[1]);
        assertEquals(3, array[2]);
        // a range with a negative start
        array = new long[]{1, 2, 3};
        ArrayUtils.reverse(array, -1, 3);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);
        // a range with a large stop index
        array = new long[]{1, 2, 3};
        ArrayUtils.reverse(array, -1, array.length + 1000);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);
        // null
        array = null;
        ArrayUtils.reverse(array, 0, 3);
        assertEquals(null, array);
    }

    @Test
    public void testReverseShortRange() {
        short[] array = new short[]{1, 2, 3};
        // The whole array
        ArrayUtils.reverse(array, 0, 3);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);
        // a range
        array = new short[]{1, 2, 3};
        ArrayUtils.reverse(array, 0, 2);
        assertEquals(2, array[0]);
        assertEquals(1, array[1]);
        assertEquals(3, array[2]);
        // a range with a negative start
        array = new short[]{1, 2, 3};
        ArrayUtils.reverse(array, -1, 3);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);
        // a range with a large stop index
        array = new short[]{1, 2, 3};
        ArrayUtils.reverse(array, -1, array.length + 1000);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);
        // null
        array = null;
        ArrayUtils.reverse(array, 0, 3);
        assertEquals(null, array);
    }

    @Test
    public void testReverseObjectRange() {
        String[] array = new String[]{"1", "2", "3"};
        // The whole array
        ArrayUtils.reverse(array, 0, 3);
        assertEquals("3", array[0]);
        assertEquals("2", array[1]);
        assertEquals("1", array[2]);
        // a range
        array = new String[]{"1", "2", "3"};
        ArrayUtils.reverse(array, 0, 2);
        assertEquals("2", array[0]);
        assertEquals("1", array[1]);
        assertEquals("3", array[2]);
        // a range with a negative start
        array = new String[]{"1", "2", "3"};
        ArrayUtils.reverse(array, -1, 3);
        assertEquals("3", array[0]);
        assertEquals("2", array[1]);
        assertEquals("1", array[2]);
        // a range with a large stop index
        array = new String[]{"1", "2", "3"};
        ArrayUtils.reverse(array, -1, array.length + 1000);
        assertEquals("3", array[0]);
        assertEquals("2", array[1]);
        assertEquals("1", array[2]);
        // null
        array = null;
        ArrayUtils.reverse(array, 0, 3);
        assertEquals(null, array);
    }

    //-----------------------------------------------------------------------
    @Test
    public void testSwapChar() {
        char[] array = new char[]{1, 2, 3};
        ArrayUtils.swap(array, 0, 2);
        assertArrayEquals(new char[]{3, 2, 1}, array);

        array = new char[]{1, 2, 3};
        ArrayUtils.swap(array, 0, 0);
        assertArrayEquals(new char[]{1, 2, 3}, array);

        array = new char[]{1, 2, 3};
        ArrayUtils.swap(array, 1, 0);
        assertArrayEquals(new char[]{2, 1, 3}, array);
    }

    @Test
    public void testSwapCharRange() {
        char[] array = new char[]{1, 2, 3, 4};
        ArrayUtils.swap(array, 0, 2, 2);
        assertEquals(3, array[0]);
        assertEquals(4, array[1]);
        assertEquals(1, array[2]);
        assertEquals(2, array[3]);

        array = new char[]{1, 2, 3};
        ArrayUtils.swap(array, 0, 3);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);

        array = new char[]{1, 2, 3};
        ArrayUtils.swap(array, 0, 2, 2);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);

        array = new char[]{1, 2, 3};
        ArrayUtils.swap(array, -1, 2, 2);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);

        array = new char[]{1, 2, 3};
        ArrayUtils.swap(array, 0, -1, 2);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);

        array = new char[]{1, 2, 3};
        ArrayUtils.swap(array, -1, -1, 2);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);
    }

    @Test
    public void testSwapByte() {
        final byte[] array = new byte[]{1, 2, 3};
        ArrayUtils.swap(array, 0, 2);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);
    }

    @Test
    public void testSwapNullByteArray() {
        final byte[] array = null;
        ArrayUtils.swap(array, 0, 2);
        assertNull(array);
    }

    @Test
    public void testSwapEmptyByteArray() {
        final byte[] array = new byte[0];
        ArrayUtils.swap(array, 0, 2);
        assertEquals(0, array.length);
    }

    @Test
    public void testSwapByteRange() {
        byte[] array = new byte[]{1, 2, 3, 4};
        ArrayUtils.swap(array, 0, 2, 2);
        assertEquals(3, array[0]);
        assertEquals(4, array[1]);
        assertEquals(1, array[2]);
        assertEquals(2, array[3]);

        array = new byte[]{1, 2, 3};
        ArrayUtils.swap(array, 0, 3);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);

        array = new byte[]{1, 2, 3};
        ArrayUtils.swap(array, 0, 2, 2);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);

        array = new byte[]{1, 2, 3};
        ArrayUtils.swap(array, -1, 2, 2);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);

        array = new byte[]{1, 2, 3};
        ArrayUtils.swap(array, 0, -1, 2);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);

        array = new byte[]{1, 2, 3};
        ArrayUtils.swap(array, -1, -1, 2);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);
    }

    @Test
    public void testSwapFloat() {
        final float[] array = new float[]{1, 2, 3};
        ArrayUtils.swap(array, 0, 2);
        assertEquals(3, array[0], 0);
        assertEquals(2, array[1], 0);
        assertEquals(1, array[2], 0);
    }

    @Test
    public void testSwapNullFloatArray() {
        final float[] array = null;
        ArrayUtils.swap(array, 0, 2);
        assertNull(array);
    }

    @Test
    public void testSwapEmptyFloatArray() {
        final float[] array = new float[0];
        ArrayUtils.swap(array, 0, 2);
        assertEquals(0, array.length);
    }

    @Test
    public void testSwapFloatRange() {
        float[] array = new float[]{1, 2, 3, 4};
        ArrayUtils.swap(array, 0, 2, 2);
        assertEquals(3, array[0], 0);
        assertEquals(4, array[1], 0);
        assertEquals(1, array[2], 0);
        assertEquals(2, array[3], 0);

        array = new float[]{1, 2, 3};
        ArrayUtils.swap(array, 0, 3);
        assertEquals(1, array[0], 0);
        assertEquals(2, array[1], 0);
        assertEquals(3, array[2], 0);

        array = new float[]{1, 2, 3};
        ArrayUtils.swap(array, 0, 2, 2);
        assertEquals(3, array[0], 0);
        assertEquals(2, array[1], 0);
        assertEquals(1, array[2], 0);

        array = new float[]{1, 2, 3};
        ArrayUtils.swap(array, -1, 2, 2);
        assertEquals(3, array[0], 0);
        assertEquals(2, array[1], 0);
        assertEquals(1, array[2], 0);

        array = new float[]{1, 2, 3};
        ArrayUtils.swap(array, 0, -1, 2);
        assertEquals(1, array[0], 0);
        assertEquals(2, array[1], 0);
        assertEquals(3, array[2], 0);

        array = new float[]{1, 2, 3};
        ArrayUtils.swap(array, -1, -1, 2);
        assertEquals(1, array[0], 0);
        assertEquals(2, array[1], 0);
        assertEquals(3, array[2], 0);
    }

    @Test
    public void testSwapDouble() {
        final double[] array = new double[]{1, 2, 3};
        ArrayUtils.swap(array, 0, 2);
        assertEquals(3, array[0], 0);
        assertEquals(2, array[1], 0);
        assertEquals(1, array[2], 0);
    }

    @Test
    public void testSwapNullDoubleArray() {
        final double[] array = null;
        ArrayUtils.swap(array, 0, 2);
        assertNull(array);
    }

    @Test
    public void testSwapEmptyDoubleArray() {
        final double[] array = new double[0];
        ArrayUtils.swap(array, 0, 2);
        assertEquals(0, array.length);
    }

    @Test
    public void testSwapDoubleRange() {
        double[] array = new double[]{1, 2, 3, 4};
        ArrayUtils.swap(array, 0, 2, 2);
        assertEquals(3, array[0], 0);
        assertEquals(4, array[1], 0);
        assertEquals(1, array[2], 0);
        assertEquals(2, array[3], 0);

        array = new double[]{1, 2, 3};
        ArrayUtils.swap(array, 0, 3);
        assertEquals(1, array[0], 0);
        assertEquals(2, array[1], 0);
        assertEquals(3, array[2], 0);

        array = new double[]{1, 2, 3};
        ArrayUtils.swap(array, 0, 2, 2);
        assertEquals(3, array[0], 0);
        assertEquals(2, array[1], 0);
        assertEquals(1, array[2], 0);

        array = new double[]{1, 2, 3};
        ArrayUtils.swap(array, -1, 2, 2);
        assertEquals(3, array[0], 0);
        assertEquals(2, array[1], 0);
        assertEquals(1, array[2], 0);

        array = new double[]{1, 2, 3};
        ArrayUtils.swap(array, 0, -1, 2);
        assertEquals(1, array[0], 0);
        assertEquals(2, array[1], 0);
        assertEquals(3, array[2], 0);

        array = new double[]{1, 2, 3};
        ArrayUtils.swap(array, -1, -1, 2);
        assertEquals(1, array[0], 0);
        assertEquals(2, array[1], 0);
        assertEquals(3, array[2], 0);
    }

    @Test
    public void testSwapInt() {
        final int[] array = new int[]{1, 2, 3};
        ArrayUtils.swap(array, 0, 2);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);
    }

    @Test
    public void testSwapNullIntArray() {
        final int[] array = null;
        ArrayUtils.swap(array, 0, 2);
        assertNull(array);
    }

    @Test
    public void testSwapEmptyIntArray() {
        final int[] array = new int[0];
        ArrayUtils.swap(array, 0, 2);
        assertEquals(0, array.length);
    }

    @Test
    public void testSwapIntRange() {
        int[] array = new int[]{1, 2, 3, 4};
        ArrayUtils.swap(array, 0, 2, 2);
        assertEquals(3, array[0]);
        assertEquals(4, array[1]);
        assertEquals(1, array[2]);
        assertEquals(2, array[3]);

        array = new int[]{1, 2, 3};
        ArrayUtils.swap(array, 3, 0);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);

        array = new int[]{1, 2, 3};
        ArrayUtils.swap(array, 0, 2, 2);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);

        array = new int[]{1, 2, 3};
        ArrayUtils.swap(array, -1, 2, 2);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);

        array = new int[]{1, 2, 3};
        ArrayUtils.swap(array, 0, -1, 2);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);

        array = new int[]{1, 2, 3};
        ArrayUtils.swap(array, -1, -1, 2);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);
    }

    @Test
    public void testSwapIntExchangedOffsets() {
        int[] array;
        array = new int[]{1, 2, 3};
        ArrayUtils.swap(array, 0, 1, 2);
        assertArrayEquals(new int[]{2, 3, 1}, array);

        array = new int[]{1, 2, 3};
        ArrayUtils.swap(array, 1, 0, 2);
        assertArrayEquals(new int[]{2, 3, 1}, array);
    }

    @Test
    public void testSwapShort() {
        final short[] array = new short[]{1, 2, 3};
        ArrayUtils.swap(array, 0, 2);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);
    }

    @Test
    public void testSwapNullShortArray() {
        final short[] array = null;
        ArrayUtils.swap(array, 0, 2);
        assertNull(array);
    }

    @Test
    public void testSwapEmptyShortArray() {
        final short[] array = new short[0];
        ArrayUtils.swap(array, 0, 2);
        assertEquals(0, array.length);
    }

    @Test
    public void testSwapShortRange() {
        short[] array = new short[]{1, 2, 3, 4};
        ArrayUtils.swap(array, 0, 2, 2);
        assertEquals(3, array[0]);
        assertEquals(4, array[1]);
        assertEquals(1, array[2]);
        assertEquals(2, array[3]);

        array = new short[]{1, 2, 3};
        ArrayUtils.swap(array, 3, 0);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);

        array = new short[]{1, 2, 3};
        ArrayUtils.swap(array, 0, 2, 2);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);

        array = new short[]{1, 2, 3};
        ArrayUtils.swap(array, -1, 2, 2);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);

        array = new short[]{1, 2, 3};
        ArrayUtils.swap(array, 0, -1, 2);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);

        array = new short[]{1, 2, 3};
        ArrayUtils.swap(array, -1, -1, 2);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);
    }

    @Test
    public void testSwapNullCharArray() {
        final char[] array = null;
        ArrayUtils.swap(array, 0, 2);
        assertNull(array);
    }

    @Test
    public void testSwapEmptyCharArray() {
        final char[] array = new char[0];
        ArrayUtils.swap(array, 0, 2);
        assertEquals(0, array.length);
    }

    @Test
    public void testSwapLong() {
        final long[] array = new long[]{1, 2, 3};
        ArrayUtils.swap(array, 0, 2);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);
    }

    @Test
    public void testSwapNullLongArray() {
        final long[] array = null;
        ArrayUtils.swap(array, 0, 2);
        assertNull(array);
    }

    @Test
    public void testSwapEmptyLongArray() {
        final long[] array = new long[0];
        ArrayUtils.swap(array, 0, 2);
        assertEquals(0, array.length);
    }

    @Test
    public void testSwapLongRange() {
        long[] array = new long[]{1, 2, 3, 4};
        ArrayUtils.swap(array, 0, 2, 2);
        assertEquals(3, array[0]);
        assertEquals(4, array[1]);
        assertEquals(1, array[2]);
        assertEquals(2, array[3]);

        array = new long[]{1, 2, 3};
        ArrayUtils.swap(array, 0, 3);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);

        array = new long[]{1, 2, 3};
        ArrayUtils.swap(array, 0, 2, 2);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);

        array = new long[]{1, 2, 3};
        ArrayUtils.swap(array, -1, 2, 2);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);

        array = new long[]{1, 2, 3};
        ArrayUtils.swap(array, 0, -1, 2);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);

        array = new long[]{1, 2, 3};
        ArrayUtils.swap(array, -1, -1, 2);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);
    }

    @Test
    public void testSwapBoolean() {
        final boolean[] array = new boolean[]{true, false, false};
        ArrayUtils.swap(array, 0, 2);
        assertFalse(array[0]);
        assertFalse(array[1]);
        assertTrue(array[2]);
    }

    @Test
    public void testSwapNullBooleanArray() {
        final boolean[] array = null;
        ArrayUtils.swap(array, 0, 2);
        assertNull(array);
    }

    @Test
    public void testSwapEmptyBooleanArray() {
        final boolean[] array = new boolean[0];
        ArrayUtils.swap(array, 0, 2);
        assertEquals(0, array.length);
    }

    @Test
    public void testSwapBooleanRange() {
        boolean[] array = new boolean[]{false, false, true, true};
        ArrayUtils.swap(array, 0, 2, 2);
        assertTrue(array[0]);
        assertTrue(array[1]);
        assertFalse(array[2]);
        assertFalse(array[3]);

        array = new boolean[]{false, true, false};
        ArrayUtils.swap(array, 0, 3);
        assertFalse(array[0]);
        assertTrue(array[1]);
        assertFalse(array[2]);

        array = new boolean[]{true, true, false};
        ArrayUtils.swap(array, 0, 2, 2);
        assertFalse(array[0]);
        assertTrue(array[1]);
        assertTrue(array[2]);

        array = new boolean[]{true, true, false};
        ArrayUtils.swap(array, -1, 2, 2);
        assertFalse(array[0]);
        assertTrue(array[1]);
        assertTrue(array[2]);

        array = new boolean[]{true, true, false};
        ArrayUtils.swap(array, 0, -1, 2);
        assertTrue(array[0]);
        assertTrue(array[1]);
        assertFalse(array[2]);

        array = new boolean[]{true, true, false};
        ArrayUtils.swap(array, -1, -1, 2);
        assertTrue(array[0]);
        assertTrue(array[1]);
        assertFalse(array[2]);
    }

    @Test
    public void testSwapObject() {
        final String[] array = new String[]{"1", "2", "3"};
        ArrayUtils.swap(array, 0, 2);
        assertEquals("3", array[0]);
        assertEquals("2", array[1]);
        assertEquals("1", array[2]);
    }

    @Test
    public void testSwapNullObjectArray() {
        final String[] array = null;
        ArrayUtils.swap(array, 0, 2);
        assertNull(array);
    }

    @Test
    public void testSwapEmptyObjectArray() {
        final String[] array = new String[0];
        ArrayUtils.swap(array, 0, 2);
        assertEquals(0, array.length);
    }

    @Test
    public void testSwapObjectRange() {
        String[] array = new String[]{"1", "2", "3", "4"};
        ArrayUtils.swap(array, 0, 2, 2);
        assertEquals("3", array[0]);
        assertEquals("4", array[1]);
        assertEquals("1", array[2]);
        assertEquals("2", array[3]);

        array = new String[]{"1", "2", "3", "4"};
        ArrayUtils.swap(array, -1, 2, 3);
        assertEquals("3", array[0]);
        assertEquals("4", array[1]);
        assertEquals("1", array[2]);
        assertEquals("2", array[3]);

        array = new String[]{"1", "2", "3", "4", "5"};
        ArrayUtils.swap(array, -3, 2, 3);
        assertEquals("3", array[0]);
        assertEquals("4", array[1]);
        assertEquals("5", array[2]);
        assertEquals("2", array[3]);
        assertEquals("1", array[4]);

        array = new String[]{"1", "2", "3", "4", "5"};
        ArrayUtils.swap(array, 2, -2, 3);
        assertEquals("3", array[0]);
        assertEquals("4", array[1]);
        assertEquals("5", array[2]);
        assertEquals("2", array[3]);
        assertEquals("1", array[4]);

        array = new String[0];
        ArrayUtils.swap(array, 0, 2, 2);
        assertEquals(0, array.length);

        array = null;
        ArrayUtils.swap(array, 0, 2, 2);
        assertNull(array);
    }

    //-----------------------------------------------------------------------
    @Test
    public void testShiftDouble() {
        final double[] array = new double[]{1, 2, 3, 4};
        ArrayUtils.shift(array, 1);
        assertEquals(4, array[0], 0);
        assertEquals(1, array[1], 0);
        assertEquals(2, array[2], 0);
        assertEquals(3, array[3], 0);
        ArrayUtils.shift(array, -1);
        assertEquals(1, array[0], 0);
        assertEquals(2, array[1], 0);
        assertEquals(3, array[2], 0);
        assertEquals(4, array[3], 0);
        ArrayUtils.shift(array, 5);
        assertEquals(4, array[0], 0);
        assertEquals(1, array[1], 0);
        assertEquals(2, array[2], 0);
        assertEquals(3, array[3], 0);
        ArrayUtils.shift(array, -3);
        assertEquals(3, array[0], 0);
        assertEquals(4, array[1], 0);
        assertEquals(1, array[2], 0);
        assertEquals(2, array[3], 0);
    }

    @Test
    public void testShiftRangeDouble() {
        final double[] array = new double[]{1, 2, 3, 4, 5};
        ArrayUtils.shift(array, 1, 3, 1);
        assertEquals(1, array[0], 0);
        assertEquals(3, array[1], 0);
        assertEquals(2, array[2], 0);
        assertEquals(4, array[3], 0);
        assertEquals(5, array[4], 0);
        ArrayUtils.shift(array, 1, 4, 2);
        assertEquals(1, array[0], 0);
        assertEquals(2, array[1], 0);
        assertEquals(4, array[2], 0);
        assertEquals(3, array[3], 0);
        assertEquals(5, array[4], 0);
    }

    @Test
    public void testShiftRangeNoElemDouble() {
        final double[] array = new double[]{1, 2, 3, 4};
        ArrayUtils.shift(array, 1, 1, 1);
        assertEquals(1, array[0], 0);
        assertEquals(2, array[1], 0);
        assertEquals(3, array[2], 0);
        assertEquals(4, array[3], 0);
    }

    @Test
    public void testShiftRangeNullDouble() {
        final double[] array = null;
        ArrayUtils.shift(array, 1, 1, 1);
        assertNull(array);
    }

    @Test
    public void testShiftNullDouble() {
        final double[] array = null;

        ArrayUtils.shift(array, 1);
        assertNull(array);
    }

    @Test
    public void testShiftAllDouble() {
        final double[] array = new double[]{1, 2, 3, 4};
        ArrayUtils.shift(array, 4);
        assertEquals(1, array[0], 0);
        assertEquals(2, array[1], 0);
        assertEquals(3, array[2], 0);
        assertEquals(4, array[3], 0);
        ArrayUtils.shift(array, -4);
        assertEquals(1, array[0], 0);
        assertEquals(2, array[1], 0);
        assertEquals(3, array[2], 0);
        assertEquals(4, array[3], 0);
    }

    @Test
    public void testShiftFloat() {
        final float[] array = new float[]{1, 2, 3, 4};
        ArrayUtils.shift(array, 1);
        assertEquals(4, array[0], 0);
        assertEquals(1, array[1], 0);
        assertEquals(2, array[2], 0);
        assertEquals(3, array[3], 0);
        ArrayUtils.shift(array, -1);
        assertEquals(1, array[0], 0);
        assertEquals(2, array[1], 0);
        assertEquals(3, array[2], 0);
        assertEquals(4, array[3], 0);
        ArrayUtils.shift(array, 5);
        assertEquals(4, array[0], 0);
        assertEquals(1, array[1], 0);
        assertEquals(2, array[2], 0);
        assertEquals(3, array[3], 0);
        ArrayUtils.shift(array, -3);
        assertEquals(3, array[0], 0);
        assertEquals(4, array[1], 0);
        assertEquals(1, array[2], 0);
        assertEquals(2, array[3], 0);
    }

    @Test
    public void testShiftRangeFloat() {
        final float[] array = new float[]{1, 2, 3, 4, 5};
        ArrayUtils.shift(array, 1, 3, 1);
        assertEquals(1, array[0], 0);
        assertEquals(3, array[1], 0);
        assertEquals(2, array[2], 0);
        assertEquals(4, array[3], 0);
        assertEquals(5, array[4], 0);
        ArrayUtils.shift(array, 1, 4, 2);
        assertEquals(1, array[0], 0);
        assertEquals(2, array[1], 0);
        assertEquals(4, array[2], 0);
        assertEquals(3, array[3], 0);
        assertEquals(5, array[4], 0);
    }

    @Test
    public void testShiftRangeNoElemFloat() {
        final float[] array = new float[]{1, 2, 3, 4};
        ArrayUtils.shift(array, 1, 1, 1);
        assertEquals(1, array[0], 0);
        assertEquals(2, array[1], 0);
        assertEquals(3, array[2], 0);
        assertEquals(4, array[3], 0);
    }

    @Test
    public void testShiftRangeNullFloat() {
        final float[] array = null;
        ArrayUtils.shift(array, 1, 1, 1);
        assertNull(array);
    }

    @Test
    public void testShiftNullFloat() {
        final float[] array = null;

        ArrayUtils.shift(array, 1);
        assertNull(array);
    }

    @Test
    public void testShiftAllFloat() {
        final float[] array = new float[]{1, 2, 3, 4};
        ArrayUtils.shift(array, 4);
        assertEquals(1, array[0], 0);
        assertEquals(2, array[1], 0);
        assertEquals(3, array[2], 0);
        assertEquals(4, array[3], 0);
        ArrayUtils.shift(array, -4);
        assertEquals(1, array[0], 0);
        assertEquals(2, array[1], 0);
        assertEquals(3, array[2], 0);
        assertEquals(4, array[3], 0);
    }

    @Test
    public void testShiftShort() {
        short[] array = new short[]{1, 2, 3, 4};
        ArrayUtils.shift(array, 1);
        assertEquals(4, array[0]);
        assertEquals(1, array[1]);
        assertEquals(2, array[2]);
        assertEquals(3, array[3]);
        ArrayUtils.shift(array, -1);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);
        assertEquals(4, array[3]);
        ArrayUtils.shift(array, 5);
        assertEquals(4, array[0]);
        assertEquals(1, array[1]);
        assertEquals(2, array[2]);
        assertEquals(3, array[3]);
        ArrayUtils.shift(array, -3);
        assertEquals(3, array[0]);
        assertEquals(4, array[1]);
        assertEquals(1, array[2]);
        assertEquals(2, array[3]);
        array = new short[]{1, 2, 3, 4, 5};
        ArrayUtils.shift(array, 2);
        assertEquals(4, array[0]);
        assertEquals(5, array[1]);
        assertEquals(1, array[2]);
        assertEquals(2, array[3]);
        assertEquals(3, array[4]);
    }

    @Test
    public void testShiftNullShort() {
        final short[] array = null;

        ArrayUtils.shift(array, 1);
        assertNull(array);
    }

    @Test
    public void testShiftRangeShort() {
        final short[] array = new short[]{1, 2, 3, 4, 5};
        ArrayUtils.shift(array, 1, 3, 1);
        assertEquals(1, array[0]);
        assertEquals(3, array[1]);
        assertEquals(2, array[2]);
        assertEquals(4, array[3]);
        assertEquals(5, array[4]);
        ArrayUtils.shift(array, 1, 4, 2);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(4, array[2]);
        assertEquals(3, array[3]);
        assertEquals(5, array[4]);
    }

    @Test
    public void testShiftRangeNoElemShort() {
        final short[] array = new short[]{1, 2, 3, 4};
        ArrayUtils.shift(array, 1, 1, 1);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);
        assertEquals(4, array[3]);
    }

    @Test
    public void testShiftRangeNullShort() {
        final short[] array = null;

        ArrayUtils.shift(array, 1, 1, 1);
        assertNull(array);
    }

    @Test
    public void testShiftAllShort() {
        final short[] array = new short[]{1, 2, 3, 4};
        ArrayUtils.shift(array, 4);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);
        assertEquals(4, array[3]);
        ArrayUtils.shift(array, -4);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);
        assertEquals(4, array[3]);
    }

    @Test
    public void testShiftByte() {
        final byte[] array = new byte[]{1, 2, 3, 4};
        ArrayUtils.shift(array, 1);
        assertEquals(4, array[0]);
        assertEquals(1, array[1]);
        assertEquals(2, array[2]);
        assertEquals(3, array[3]);
        ArrayUtils.shift(array, -1);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);
        assertEquals(4, array[3]);
        ArrayUtils.shift(array, 5);
        assertEquals(4, array[0]);
        assertEquals(1, array[1]);
        assertEquals(2, array[2]);
        assertEquals(3, array[3]);
        ArrayUtils.shift(array, -3);
        assertEquals(3, array[0]);
        assertEquals(4, array[1]);
        assertEquals(1, array[2]);
        assertEquals(2, array[3]);
    }

    @Test
    public void testShiftRangeByte() {
        final byte[] array = new byte[]{1, 2, 3, 4, 5};
        ArrayUtils.shift(array, 1, 3, 1);
        assertEquals(1, array[0]);
        assertEquals(3, array[1]);
        assertEquals(2, array[2]);
        assertEquals(4, array[3]);
        assertEquals(5, array[4]);
        ArrayUtils.shift(array, 1, 4, 2);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(4, array[2]);
        assertEquals(3, array[3]);
        assertEquals(5, array[4]);
    }

    @Test
    public void testShiftRangeNoElemByte() {
        final byte[] array = new byte[]{1, 2, 3, 4};
        ArrayUtils.shift(array, 1, 1, 1);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);
        assertEquals(4, array[3]);
    }

    @Test
    public void testShiftRangeNullByte() {
        final byte[] array = null;
        ArrayUtils.shift(array, 1, 1, 1);
        assertNull(array);
    }

    @Test
    public void testShiftAllByte() {
        final byte[] array = new byte[]{1, 2, 3, 4};
        ArrayUtils.shift(array, 4);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);
        assertEquals(4, array[3]);
        ArrayUtils.shift(array, -4);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);
        assertEquals(4, array[3]);
    }

    @Test
    public void testShiftChar() {
        final char[] array = new char[]{1, 2, 3, 4};
        ArrayUtils.shift(array, 1);
        assertEquals(4, array[0]);
        assertEquals(1, array[1]);
        assertEquals(2, array[2]);
        assertEquals(3, array[3]);
        ArrayUtils.shift(array, -1);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);
        assertEquals(4, array[3]);
        ArrayUtils.shift(array, 5);
        assertEquals(4, array[0]);
        assertEquals(1, array[1]);
        assertEquals(2, array[2]);
        assertEquals(3, array[3]);
        ArrayUtils.shift(array, -3);
        assertEquals(3, array[0]);
        assertEquals(4, array[1]);
        assertEquals(1, array[2]);
        assertEquals(2, array[3]);
    }

    @Test
    public void testShiftRangeChar() {
        final char[] array = new char[]{1, 2, 3, 4, 5};
        ArrayUtils.shift(array, 1, 3, 1);
        assertEquals(1, array[0]);
        assertEquals(3, array[1]);
        assertEquals(2, array[2]);
        assertEquals(4, array[3]);
        assertEquals(5, array[4]);
        ArrayUtils.shift(array, 1, 4, 2);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(4, array[2]);
        assertEquals(3, array[3]);
        assertEquals(5, array[4]);
    }

    @Test
    public void testShiftRangeNoElemChar() {
        final char[] array = new char[]{1, 2, 3, 4};
        ArrayUtils.shift(array, 1, 1, 1);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);
        assertEquals(4, array[3]);
    }

    @Test
    public void testShiftRangeNullChar() {
        final char[] array = null;
        ArrayUtils.shift(array, 1, 1, 1);
        assertNull(array);
    }

    @Test
    public void testShiftAllChar() {
        final char[] array = new char[]{1, 2, 3, 4};
        ArrayUtils.shift(array, 4);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);
        assertEquals(4, array[3]);
        ArrayUtils.shift(array, -4);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);
        assertEquals(4, array[3]);
    }

    @Test
    public void testShiftLong() {
        final long[] array = new long[]{1, 2, 3, 4};
        ArrayUtils.shift(array, 1);
        assertEquals(4, array[0]);
        assertEquals(1, array[1]);
        assertEquals(2, array[2]);
        assertEquals(3, array[3]);
        ArrayUtils.shift(array, -1);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);
        assertEquals(4, array[3]);
        ArrayUtils.shift(array, 5);
        assertEquals(4, array[0]);
        assertEquals(1, array[1]);
        assertEquals(2, array[2]);
        assertEquals(3, array[3]);
        ArrayUtils.shift(array, -3);
        assertEquals(3, array[0]);
        assertEquals(4, array[1]);
        assertEquals(1, array[2]);
        assertEquals(2, array[3]);
    }

    @Test
    public void testShiftRangeLong() {
        final long[] array = new long[]{1, 2, 3, 4, 5};
        ArrayUtils.shift(array, 1, 3, 1);
        assertEquals(1, array[0]);
        assertEquals(3, array[1]);
        assertEquals(2, array[2]);
        assertEquals(4, array[3]);
        assertEquals(5, array[4]);
        ArrayUtils.shift(array, 1, 4, 2);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(4, array[2]);
        assertEquals(3, array[3]);
        assertEquals(5, array[4]);
    }

    @Test
    public void testShiftRangeNoElemLong() {
        final long[] array = new long[]{1, 2, 3, 4};
        ArrayUtils.shift(array, 1, 1, 1);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);
        assertEquals(4, array[3]);
    }

    @Test
    public void testShiftRangeNullLong() {
        final long[] array = null;
        ArrayUtils.shift(array, 1, 1, 1);
        assertNull(array);
    }

    @Test
    public void testShiftNullLong() {
        final long[] array = null;

        ArrayUtils.shift(array, 1);
        assertNull(array);
    }

    @Test
    public void testShiftAllLong() {
        final long[] array = new long[]{1, 2, 3, 4};
        ArrayUtils.shift(array, 4);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);
        assertEquals(4, array[3]);
        ArrayUtils.shift(array, -4);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);
        assertEquals(4, array[3]);
    }

    @Test
    public void testShiftInt() {
        final int[] array = new int[]{1, 2, 3, 4};
        ArrayUtils.shift(array, 1);
        assertEquals(4, array[0]);
        assertEquals(1, array[1]);
        assertEquals(2, array[2]);
        assertEquals(3, array[3]);
        ArrayUtils.shift(array, -1);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);
        assertEquals(4, array[3]);
        ArrayUtils.shift(array, 5);
        assertEquals(4, array[0]);
        assertEquals(1, array[1]);
        assertEquals(2, array[2]);
        assertEquals(3, array[3]);
        ArrayUtils.shift(array, -3);
        assertEquals(3, array[0]);
        assertEquals(4, array[1]);
        assertEquals(1, array[2]);
        assertEquals(2, array[3]);
    }

    @Test
    public void testShiftNullInt() {
        final int[] array = null;

        ArrayUtils.shift(array, 1);
        assertNull(array);
    }

    @Test
    public void testShiftRangeInt() {
        final int[] array = new int[]{1, 2, 3, 4, 5};
        ArrayUtils.shift(array, 1, 3, 1);
        assertEquals(1, array[0]);
        assertEquals(3, array[1]);
        assertEquals(2, array[2]);
        assertEquals(4, array[3]);
        assertEquals(5, array[4]);
        ArrayUtils.shift(array, 1, 4, 2);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(4, array[2]);
        assertEquals(3, array[3]);
        assertEquals(5, array[4]);
    }

    @Test
    public void testShiftRangeNoElemInt() {
        final int[] array = new int[]{1, 2, 3, 4};
        ArrayUtils.shift(array, 1, 1, 1);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);
        assertEquals(4, array[3]);
    }

    @Test
    public void testShiftRangeNullInt() {
        final int[] array = null;
        ArrayUtils.shift(array, 1, 1, 1);
        assertNull(array);
    }

    @Test
    public void testShiftAllInt() {
        final int[] array = new int[]{1, 2, 3, 4};
        ArrayUtils.shift(array, 4);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);
        assertEquals(4, array[3]);
        ArrayUtils.shift(array, -4);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);
        assertEquals(4, array[3]);
    }

    @Test
    public void testShiftObject() {
        final String[] array = new String[]{"1", "2", "3", "4"};
        ArrayUtils.shift(array, 1);
        assertEquals("4", array[0]);
        assertEquals("1", array[1]);
        assertEquals("2", array[2]);
        assertEquals("3", array[3]);
        ArrayUtils.shift(array, -1);
        assertEquals("1", array[0]);
        assertEquals("2", array[1]);
        assertEquals("3", array[2]);
        assertEquals("4", array[3]);
        ArrayUtils.shift(array, 5);
        assertEquals("4", array[0]);
        assertEquals("1", array[1]);
        assertEquals("2", array[2]);
        assertEquals("3", array[3]);
        ArrayUtils.shift(array, -3);
        assertEquals("3", array[0]);
        assertEquals("4", array[1]);
        assertEquals("1", array[2]);
        assertEquals("2", array[3]);
    }

    @Test
    public void testShiftNullObject() {
        final String[] array = null;

        ArrayUtils.shift(array, 1);
        assertNull(array);
    }

    @Test
    public void testShiftRangeObject() {
        final String[] array = new String[]{"1", "2", "3", "4", "5"};
        ArrayUtils.shift(array, 1, 3, 1);
        assertEquals("1", array[0]);
        assertEquals("3", array[1]);
        assertEquals("2", array[2]);
        assertEquals("4", array[3]);
        assertEquals("5", array[4]);
        ArrayUtils.shift(array, 1, 4, 2);
        assertEquals("1", array[0]);
        assertEquals("2", array[1]);
        assertEquals("4", array[2]);
        assertEquals("3", array[3]);
        assertEquals("5", array[4]);
    }

    @Test
    public void testShiftRangeNoElemObject() {
        final String[] array = new String[]{"1", "2", "3", "4"};
        ArrayUtils.shift(array, 1, 1, 1);
        assertEquals("1", array[0]);
        assertEquals("2", array[1]);
        assertEquals("3", array[2]);
        assertEquals("4", array[3]);
    }

    @Test
    public void testShiftRangeNullObject() {
        final String[] array = null;
        ArrayUtils.shift(array, 1, 1, 1);
        assertNull(array);
    }

    @Test
    public void testShiftAllObject() {
        final String[] array = new String[]{"1", "2", "3", "4"};
        ArrayUtils.shift(array, 4);
        assertEquals("1", array[0]);
        assertEquals("2", array[1]);
        assertEquals("3", array[2]);
        assertEquals("4", array[3]);
        ArrayUtils.shift(array, -4);
        assertEquals("1", array[0]);
        assertEquals("2", array[1]);
        assertEquals("3", array[2]);
        assertEquals("4", array[3]);
    }

    @Test
    public void testShiftBoolean() {
        final boolean[] array = new boolean[]{true, true, false, false};

        ArrayUtils.shift(array, 1);
        assertFalse(array[0]);
        assertTrue(array[1]);
        assertTrue(array[2]);
        assertFalse(array[3]);

        ArrayUtils.shift(array, -1);
        assertTrue(array[0]);
        assertTrue(array[1]);
        assertFalse(array[2]);
        assertFalse(array[3]);

        ArrayUtils.shift(array, 5);
        assertFalse(array[0]);
        assertTrue(array[1]);
        assertTrue(array[2]);
        assertFalse(array[3]);

        ArrayUtils.shift(array, -3);
        assertFalse(array[0]);
        assertFalse(array[1]);
        assertTrue(array[2]);
        assertTrue(array[3]);
    }

    @Test
    public void testShiftNullBoolean() {
        final boolean[] array = null;

        ArrayUtils.shift(array, 1);
        assertNull(array);
    }

    //-----------------------------------------------------------------------
    @Test
    public void testIndexOf() {
        final Object[] array = new Object[]{"0", "1", "2", "3", null, "0"};
        assertEquals(-1, ArrayUtils.indexOf(null, null));
        assertEquals(-1, ArrayUtils.indexOf(null, "0"));
        assertEquals(-1, ArrayUtils.indexOf(new Object[0], "0"));
        assertEquals(0, ArrayUtils.indexOf(array, "0"));
        assertEquals(1, ArrayUtils.indexOf(array, "1"));
        assertEquals(2, ArrayUtils.indexOf(array, "2"));
        assertEquals(3, ArrayUtils.indexOf(array, "3"));
        assertEquals(4, ArrayUtils.indexOf(array, null));
        assertEquals(-1, ArrayUtils.indexOf(array, "notInArray"));
    }

    @Test
    public void testIndexOfWithStartIndex() {
        final Object[] array = new Object[]{"0", "1", "2", "3", null, "0"};
        assertEquals(-1, ArrayUtils.indexOf(null, null, 2));
        assertEquals(-1, ArrayUtils.indexOf(new Object[0], "0", 0));
        assertEquals(-1, ArrayUtils.indexOf(null, "0", 2));
        assertEquals(5, ArrayUtils.indexOf(array, "0", 2));
        assertEquals(-1, ArrayUtils.indexOf(array, "1", 2));
        assertEquals(2, ArrayUtils.indexOf(array, "2", 2));
        assertEquals(3, ArrayUtils.indexOf(array, "3", 2));
        assertEquals(4, ArrayUtils.indexOf(array, null, 2));
        assertEquals(-1, ArrayUtils.indexOf(array, "notInArray", 2));

        assertEquals(4, ArrayUtils.indexOf(array, null, -1));
        assertEquals(-1, ArrayUtils.indexOf(array, null, 8));
        assertEquals(-1, ArrayUtils.indexOf(array, "0", 8));
    }

    @Test
    public void testLastIndexOf() {
        final Object[] array = new Object[]{"0", "1", "2", "3", null, "0"};
        assertEquals(-1, ArrayUtils.lastIndexOf(null, null));
        assertEquals(-1, ArrayUtils.lastIndexOf(null, "0"));
        assertEquals(5, ArrayUtils.lastIndexOf(array, "0"));
        assertEquals(1, ArrayUtils.lastIndexOf(array, "1"));
        assertEquals(2, ArrayUtils.lastIndexOf(array, "2"));
        assertEquals(3, ArrayUtils.lastIndexOf(array, "3"));
        assertEquals(4, ArrayUtils.lastIndexOf(array, null));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, "notInArray"));
    }

    @Test
    public void testLastIndexOfWithStartIndex() {
        final Object[] array = new Object[]{"0", "1", "2", "3", null, "0"};
        assertEquals(-1, ArrayUtils.lastIndexOf(null, null, 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(null, "0", 2));
        assertEquals(0, ArrayUtils.lastIndexOf(array, "0", 2));
        assertEquals(1, ArrayUtils.lastIndexOf(array, "1", 2));
        assertEquals(2, ArrayUtils.lastIndexOf(array, "2", 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, "3", 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, "3", -1));
        assertEquals(4, ArrayUtils.lastIndexOf(array, null, 5));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, null, 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, "notInArray", 5));

        assertEquals(-1, ArrayUtils.lastIndexOf(array, null, -1));
        assertEquals(5, ArrayUtils.lastIndexOf(array, "0", 88));
    }

    @Test
    public void testContains() {
        final Object[] array = new Object[]{"0", "1", "2", "3", null, "0"};
        assertFalse(ArrayUtils.contains(null, null));
        assertFalse(ArrayUtils.contains(null, "1"));
        assertTrue(ArrayUtils.contains(array, "0"));
        assertTrue(ArrayUtils.contains(array, "1"));
        assertTrue(ArrayUtils.contains(array, "2"));
        assertTrue(ArrayUtils.contains(array, "3"));
        assertTrue(ArrayUtils.contains(array, null));
        assertFalse(ArrayUtils.contains(array, "notInArray"));
    }

    @Test
    public void testContains_LANG_1261() {
        class LANG1261ParentObject {
            @Override
            public boolean equals(final Object o) {
                return true;
            }
        }
        class LANG1261ChildObject extends LANG1261ParentObject {
        }

        final Object[] array = new LANG1261ChildObject[]{new LANG1261ChildObject()};

        assertTrue(ArrayUtils.contains(array, new LANG1261ParentObject()));
    }

    //-----------------------------------------------------------------------
    @Test
    public void testIndexOfLong() {
        long[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, 0));
        array = new long[]{0, 1, 2, 3, 0};
        assertEquals(0, ArrayUtils.indexOf(array, 0));
        assertEquals(1, ArrayUtils.indexOf(array, 1));
        assertEquals(2, ArrayUtils.indexOf(array, 2));
        assertEquals(3, ArrayUtils.indexOf(array, 3));
        assertEquals(-1, ArrayUtils.indexOf(array, 99));
    }

    @Test
    public void testIndexOfLongWithStartIndex() {
        long[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, 0, 2));
        array = new long[]{0, 1, 2, 3, 0};
        assertEquals(4, ArrayUtils.indexOf(array, 0, 2));
        assertEquals(-1, ArrayUtils.indexOf(array, 1, 2));
        assertEquals(2, ArrayUtils.indexOf(array, 2, 2));
        assertEquals(3, ArrayUtils.indexOf(array, 3, 2));
        assertEquals(3, ArrayUtils.indexOf(array, 3, -1));
        assertEquals(-1, ArrayUtils.indexOf(array, 99, 0));
        assertEquals(-1, ArrayUtils.indexOf(array, 0, 6));
    }

    @Test
    public void testLastIndexOfLong() {
        long[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 0));
        array = new long[]{0, 1, 2, 3, 0};
        assertEquals(4, ArrayUtils.lastIndexOf(array, 0));
        assertEquals(1, ArrayUtils.lastIndexOf(array, 1));
        assertEquals(2, ArrayUtils.lastIndexOf(array, 2));
        assertEquals(3, ArrayUtils.lastIndexOf(array, 3));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 99));
    }

    @Test
    public void testLastIndexOfLongWithStartIndex() {
        long[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 0, 2));
        array = new long[]{0, 1, 2, 3, 0};
        assertEquals(0, ArrayUtils.lastIndexOf(array, 0, 2));
        assertEquals(1, ArrayUtils.lastIndexOf(array, 1, 2));
        assertEquals(2, ArrayUtils.lastIndexOf(array, 2, 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 3, 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 3, -1));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 99, 4));
        assertEquals(4, ArrayUtils.lastIndexOf(array, 0, 88));
    }

    @Test
    public void testContainsLong() {
        long[] array = null;
        assertFalse(ArrayUtils.contains(array, 1));
        array = new long[]{0, 1, 2, 3, 0};
        assertTrue(ArrayUtils.contains(array, 0));
        assertTrue(ArrayUtils.contains(array, 1));
        assertTrue(ArrayUtils.contains(array, 2));
        assertTrue(ArrayUtils.contains(array, 3));
        assertFalse(ArrayUtils.contains(array, 99));
    }

    //-----------------------------------------------------------------------
    @Test
    public void testIndexOfInt() {
        int[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, 0));
        array = new int[]{0, 1, 2, 3, 0};
        assertEquals(0, ArrayUtils.indexOf(array, 0));
        assertEquals(1, ArrayUtils.indexOf(array, 1));
        assertEquals(2, ArrayUtils.indexOf(array, 2));
        assertEquals(3, ArrayUtils.indexOf(array, 3));
        assertEquals(-1, ArrayUtils.indexOf(array, 99));
    }

    @Test
    public void testIndexOfIntWithStartIndex() {
        int[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, 0, 2));
        array = new int[]{0, 1, 2, 3, 0};
        assertEquals(4, ArrayUtils.indexOf(array, 0, 2));
        assertEquals(-1, ArrayUtils.indexOf(array, 1, 2));
        assertEquals(2, ArrayUtils.indexOf(array, 2, 2));
        assertEquals(3, ArrayUtils.indexOf(array, 3, 2));
        assertEquals(3, ArrayUtils.indexOf(array, 3, -1));
        assertEquals(-1, ArrayUtils.indexOf(array, 99, 0));
        assertEquals(-1, ArrayUtils.indexOf(array, 0, 6));
    }

    @Test
    public void testLastIndexOfInt() {
        int[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 0));
        array = new int[]{0, 1, 2, 3, 0};
        assertEquals(4, ArrayUtils.lastIndexOf(array, 0));
        assertEquals(1, ArrayUtils.lastIndexOf(array, 1));
        assertEquals(2, ArrayUtils.lastIndexOf(array, 2));
        assertEquals(3, ArrayUtils.lastIndexOf(array, 3));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 99));
    }

    @Test
    public void testLastIndexOfIntWithStartIndex() {
        int[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 0, 2));
        array = new int[]{0, 1, 2, 3, 0};
        assertEquals(0, ArrayUtils.lastIndexOf(array, 0, 2));
        assertEquals(1, ArrayUtils.lastIndexOf(array, 1, 2));
        assertEquals(2, ArrayUtils.lastIndexOf(array, 2, 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 3, 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 3, -1));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 99));
        assertEquals(4, ArrayUtils.lastIndexOf(array, 0, 88));
    }

    @Test
    public void testContainsInt() {
        int[] array = null;
        assertFalse(ArrayUtils.contains(array, 1));
        array = new int[]{0, 1, 2, 3, 0};
        assertTrue(ArrayUtils.contains(array, 0));
        assertTrue(ArrayUtils.contains(array, 1));
        assertTrue(ArrayUtils.contains(array, 2));
        assertTrue(ArrayUtils.contains(array, 3));
        assertFalse(ArrayUtils.contains(array, 99));
    }

    //-----------------------------------------------------------------------
    @Test
    public void testIndexOfShort() {
        short[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, (short) 0));
        array = new short[]{0, 1, 2, 3, 0};
        assertEquals(0, ArrayUtils.indexOf(array, (short) 0));
        assertEquals(1, ArrayUtils.indexOf(array, (short) 1));
        assertEquals(2, ArrayUtils.indexOf(array, (short) 2));
        assertEquals(3, ArrayUtils.indexOf(array, (short) 3));
        assertEquals(-1, ArrayUtils.indexOf(array, (short) 99));
    }

    @Test
    public void testIndexOfShortWithStartIndex() {
        short[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, (short) 0, 2));
        array = new short[]{0, 1, 2, 3, 0};
        assertEquals(4, ArrayUtils.indexOf(array, (short) 0, 2));
        assertEquals(-1, ArrayUtils.indexOf(array, (short) 1, 2));
        assertEquals(2, ArrayUtils.indexOf(array, (short) 2, 2));
        assertEquals(3, ArrayUtils.indexOf(array, (short) 3, 2));
        assertEquals(3, ArrayUtils.indexOf(array, (short) 3, -1));
        assertEquals(-1, ArrayUtils.indexOf(array, (short) 99, 0));
        assertEquals(-1, ArrayUtils.indexOf(array, (short) 0, 6));
    }

    @Test
    public void testLastIndexOfShort() {
        short[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (short) 0));
        array = new short[]{0, 1, 2, 3, 0};
        assertEquals(4, ArrayUtils.lastIndexOf(array, (short) 0));
        assertEquals(1, ArrayUtils.lastIndexOf(array, (short) 1));
        assertEquals(2, ArrayUtils.lastIndexOf(array, (short) 2));
        assertEquals(3, ArrayUtils.lastIndexOf(array, (short) 3));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (short) 99));
    }

    @Test
    public void testLastIndexOfShortWithStartIndex() {
        short[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (short) 0, 2));
        array = new short[]{0, 1, 2, 3, 0};
        assertEquals(0, ArrayUtils.lastIndexOf(array, (short) 0, 2));
        assertEquals(1, ArrayUtils.lastIndexOf(array, (short) 1, 2));
        assertEquals(2, ArrayUtils.lastIndexOf(array, (short) 2, 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (short) 3, 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (short) 3, -1));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (short) 99));
        assertEquals(4, ArrayUtils.lastIndexOf(array, (short) 0, 88));
    }

    @Test
    public void testContainsShort() {
        short[] array = null;
        assertFalse(ArrayUtils.contains(array, (short) 1));
        array = new short[]{0, 1, 2, 3, 0};
        assertTrue(ArrayUtils.contains(array, (short) 0));
        assertTrue(ArrayUtils.contains(array, (short) 1));
        assertTrue(ArrayUtils.contains(array, (short) 2));
        assertTrue(ArrayUtils.contains(array, (short) 3));
        assertFalse(ArrayUtils.contains(array, (short) 99));
    }

    //-----------------------------------------------------------------------
    @Test
    public void testIndexOfChar() {
        char[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, 'a'));
        array = new char[]{'a', 'b', 'c', 'd', 'a'};
        assertEquals(0, ArrayUtils.indexOf(array, 'a'));
        assertEquals(1, ArrayUtils.indexOf(array, 'b'));
        assertEquals(2, ArrayUtils.indexOf(array, 'c'));
        assertEquals(3, ArrayUtils.indexOf(array, 'd'));
        assertEquals(-1, ArrayUtils.indexOf(array, 'e'));
    }

    @Test
    public void testIndexOfCharWithStartIndex() {
        char[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, 'a', 2));
        array = new char[]{'a', 'b', 'c', 'd', 'a'};
        assertEquals(4, ArrayUtils.indexOf(array, 'a', 2));
        assertEquals(-1, ArrayUtils.indexOf(array, 'b', 2));
        assertEquals(2, ArrayUtils.indexOf(array, 'c', 2));
        assertEquals(3, ArrayUtils.indexOf(array, 'd', 2));
        assertEquals(3, ArrayUtils.indexOf(array, 'd', -1));
        assertEquals(-1, ArrayUtils.indexOf(array, 'e', 0));
        assertEquals(-1, ArrayUtils.indexOf(array, 'a', 6));
    }

    @Test
    public void testLastIndexOfChar() {
        char[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 'a'));
        array = new char[]{'a', 'b', 'c', 'd', 'a'};
        assertEquals(4, ArrayUtils.lastIndexOf(array, 'a'));
        assertEquals(1, ArrayUtils.lastIndexOf(array, 'b'));
        assertEquals(2, ArrayUtils.lastIndexOf(array, 'c'));
        assertEquals(3, ArrayUtils.lastIndexOf(array, 'd'));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 'e'));
    }

    @Test
    public void testLastIndexOfCharWithStartIndex() {
        char[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 'a', 2));
        array = new char[]{'a', 'b', 'c', 'd', 'a'};
        assertEquals(0, ArrayUtils.lastIndexOf(array, 'a', 2));
        assertEquals(1, ArrayUtils.lastIndexOf(array, 'b', 2));
        assertEquals(2, ArrayUtils.lastIndexOf(array, 'c', 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 'd', 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 'd', -1));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 'e'));
        assertEquals(4, ArrayUtils.lastIndexOf(array, 'a', 88));
    }

    @Test
    public void testContainsChar() {
        char[] array = null;
        assertFalse(ArrayUtils.contains(array, 'b'));
        array = new char[]{'a', 'b', 'c', 'd', 'a'};
        assertTrue(ArrayUtils.contains(array, 'a'));
        assertTrue(ArrayUtils.contains(array, 'b'));
        assertTrue(ArrayUtils.contains(array, 'c'));
        assertTrue(ArrayUtils.contains(array, 'd'));
        assertFalse(ArrayUtils.contains(array, 'e'));
    }

    //-----------------------------------------------------------------------
    @Test
    public void testIndexOfByte() {
        byte[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, (byte) 0));
        array = new byte[]{0, 1, 2, 3, 0};
        assertEquals(0, ArrayUtils.indexOf(array, (byte) 0));
        assertEquals(1, ArrayUtils.indexOf(array, (byte) 1));
        assertEquals(2, ArrayUtils.indexOf(array, (byte) 2));
        assertEquals(3, ArrayUtils.indexOf(array, (byte) 3));
        assertEquals(-1, ArrayUtils.indexOf(array, (byte) 99));
    }

    @Test
    public void testIndexOfByteWithStartIndex() {
        byte[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, (byte) 0, 2));
        array = new byte[]{0, 1, 2, 3, 0};
        assertEquals(4, ArrayUtils.indexOf(array, (byte) 0, 2));
        assertEquals(-1, ArrayUtils.indexOf(array, (byte) 1, 2));
        assertEquals(2, ArrayUtils.indexOf(array, (byte) 2, 2));
        assertEquals(3, ArrayUtils.indexOf(array, (byte) 3, 2));
        assertEquals(3, ArrayUtils.indexOf(array, (byte) 3, -1));
        assertEquals(-1, ArrayUtils.indexOf(array, (byte) 99, 0));
        assertEquals(-1, ArrayUtils.indexOf(array, (byte) 0, 6));
    }

    @Test
    public void testLastIndexOfByte() {
        byte[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (byte) 0));
        array = new byte[]{0, 1, 2, 3, 0};
        assertEquals(4, ArrayUtils.lastIndexOf(array, (byte) 0));
        assertEquals(1, ArrayUtils.lastIndexOf(array, (byte) 1));
        assertEquals(2, ArrayUtils.lastIndexOf(array, (byte) 2));
        assertEquals(3, ArrayUtils.lastIndexOf(array, (byte) 3));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (byte) 99));
    }

    @Test
    public void testLastIndexOfByteWithStartIndex() {
        byte[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (byte) 0, 2));
        array = new byte[]{0, 1, 2, 3, 0};
        assertEquals(0, ArrayUtils.lastIndexOf(array, (byte) 0, 2));
        assertEquals(1, ArrayUtils.lastIndexOf(array, (byte) 1, 2));
        assertEquals(2, ArrayUtils.lastIndexOf(array, (byte) 2, 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (byte) 3, 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (byte) 3, -1));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (byte) 99));
        assertEquals(4, ArrayUtils.lastIndexOf(array, (byte) 0, 88));
    }

    @Test
    public void testContainsByte() {
        byte[] array = null;
        assertFalse(ArrayUtils.contains(array, (byte) 1));
        array = new byte[]{0, 1, 2, 3, 0};
        assertTrue(ArrayUtils.contains(array, (byte) 0));
        assertTrue(ArrayUtils.contains(array, (byte) 1));
        assertTrue(ArrayUtils.contains(array, (byte) 2));
        assertTrue(ArrayUtils.contains(array, (byte) 3));
        assertFalse(ArrayUtils.contains(array, (byte) 99));
    }

    //-----------------------------------------------------------------------
    @SuppressWarnings("cast")
    @Test
    public void testIndexOfDouble() {
        double[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, (double) 0));
        array = new double[0];
        assertEquals(-1, ArrayUtils.indexOf(array, (double) 0));
        array = new double[]{0, 1, 2, 3, 0};
        assertEquals(0, ArrayUtils.indexOf(array, (double) 0));
        assertEquals(1, ArrayUtils.indexOf(array, (double) 1));
        assertEquals(2, ArrayUtils.indexOf(array, (double) 2));
        assertEquals(3, ArrayUtils.indexOf(array, (double) 3));
        assertEquals(3, ArrayUtils.indexOf(array, (double) 3, -1));
        assertEquals(-1, ArrayUtils.indexOf(array, (double) 99));
    }

    @SuppressWarnings("cast")
    @Test
    public void testIndexOfDoubleTolerance() {
        double[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, (double) 0, (double) 0));
        array = new double[0];
        assertEquals(-1, ArrayUtils.indexOf(array, (double) 0, (double) 0));
        array = new double[]{0, 1, 2, 3, 0};
        assertEquals(0, ArrayUtils.indexOf(array, (double) 0, 0.3));
        assertEquals(2, ArrayUtils.indexOf(array, 2.2, 0.35));
        assertEquals(3, ArrayUtils.indexOf(array, 4.15, 2.0));
        assertEquals(1, ArrayUtils.indexOf(array, 1.00001324, 0.0001));
    }

    @SuppressWarnings("cast")
    @Test
    public void testIndexOfDoubleWithStartIndex() {
        double[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, (double) 0, 2));
        array = new double[0];
        assertEquals(-1, ArrayUtils.indexOf(array, (double) 0, 2));
        array = new double[]{0, 1, 2, 3, 0};
        assertEquals(4, ArrayUtils.indexOf(array, (double) 0, 2));
        assertEquals(-1, ArrayUtils.indexOf(array, (double) 1, 2));
        assertEquals(2, ArrayUtils.indexOf(array, (double) 2, 2));
        assertEquals(3, ArrayUtils.indexOf(array, (double) 3, 2));
        assertEquals(-1, ArrayUtils.indexOf(array, (double) 99, 0));
        assertEquals(-1, ArrayUtils.indexOf(array, (double) 0, 6));
    }

    @SuppressWarnings("cast")
    @Test
    public void testIndexOfDoubleWithStartIndexTolerance() {
        double[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, (double) 0, 2, (double) 0));
        array = new double[0];
        assertEquals(-1, ArrayUtils.indexOf(array, (double) 0, 2, (double) 0));
        array = new double[]{0, 1, 2, 3, 0};
        assertEquals(-1, ArrayUtils.indexOf(array, (double) 0, 99, 0.3));
        assertEquals(0, ArrayUtils.indexOf(array, (double) 0, 0, 0.3));
        assertEquals(4, ArrayUtils.indexOf(array, (double) 0, 3, 0.3));
        assertEquals(2, ArrayUtils.indexOf(array, 2.2, 0, 0.35));
        assertEquals(3, ArrayUtils.indexOf(array, 4.15, 0, 2.0));
        assertEquals(1, ArrayUtils.indexOf(array, 1.00001324, 0, 0.0001));
        assertEquals(3, ArrayUtils.indexOf(array, 4.15, -1, 2.0));
        assertEquals(1, ArrayUtils.indexOf(array, 1.00001324, -300, 0.0001));
    }

    @SuppressWarnings("cast")
    @Test
    public void testLastIndexOfDouble() {
        double[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (double) 0));
        array = new double[0];
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (double) 0));
        array = new double[]{0, 1, 2, 3, 0};
        assertEquals(4, ArrayUtils.lastIndexOf(array, (double) 0));
        assertEquals(1, ArrayUtils.lastIndexOf(array, (double) 1));
        assertEquals(2, ArrayUtils.lastIndexOf(array, (double) 2));
        assertEquals(3, ArrayUtils.lastIndexOf(array, (double) 3));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (double) 99));
    }

    @SuppressWarnings("cast")
    @Test
    public void testLastIndexOfDoubleTolerance() {
        double[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (double) 0, (double) 0));
        array = new double[0];
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (double) 0, (double) 0));
        array = new double[]{0, 1, 2, 3, 0};
        assertEquals(4, ArrayUtils.lastIndexOf(array, (double) 0, 0.3));
        assertEquals(2, ArrayUtils.lastIndexOf(array, 2.2, 0.35));
        assertEquals(3, ArrayUtils.lastIndexOf(array, 4.15, 2.0));
        assertEquals(1, ArrayUtils.lastIndexOf(array, 1.00001324, 0.0001));
    }

    @SuppressWarnings("cast")
    @Test
    public void testLastIndexOfDoubleWithStartIndex() {
        double[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (double) 0, 2));
        array = new double[0];
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (double) 0, 2));
        array = new double[]{0, 1, 2, 3, 0};
        assertEquals(0, ArrayUtils.lastIndexOf(array, (double) 0, 2));
        assertEquals(1, ArrayUtils.lastIndexOf(array, (double) 1, 2));
        assertEquals(2, ArrayUtils.lastIndexOf(array, (double) 2, 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (double) 3, 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (double) 3, -1));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (double) 99));
        assertEquals(4, ArrayUtils.lastIndexOf(array, (double) 0, 88));
    }

    @SuppressWarnings("cast")
    @Test
    public void testLastIndexOfDoubleWithStartIndexTolerance() {
        double[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (double) 0, 2, (double) 0));
        array = new double[0];
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (double) 0, 2, (double) 0));
        array = new double[]{(double) 3};
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (double) 1, 0, (double) 0));
        array = new double[]{0, 1, 2, 3, 0};
        assertEquals(4, ArrayUtils.lastIndexOf(array, (double) 0, 99, 0.3));
        assertEquals(0, ArrayUtils.lastIndexOf(array, (double) 0, 3, 0.3));
        assertEquals(2, ArrayUtils.lastIndexOf(array, 2.2, 3, 0.35));
        assertEquals(3, ArrayUtils.lastIndexOf(array, 4.15, array.length, 2.0));
        assertEquals(1, ArrayUtils.lastIndexOf(array, 1.00001324, array.length, 0.0001));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 4.15, -200, 2.0));
    }

    @SuppressWarnings("cast")
    @Test
    public void testContainsDouble() {
        double[] array = null;
        assertFalse(ArrayUtils.contains(array, (double) 1));
        array = new double[]{0, 1, 2, 3, 0};
        assertTrue(ArrayUtils.contains(array, (double) 0));
        assertTrue(ArrayUtils.contains(array, (double) 1));
        assertTrue(ArrayUtils.contains(array, (double) 2));
        assertTrue(ArrayUtils.contains(array, (double) 3));
        assertFalse(ArrayUtils.contains(array, (double) 99));
    }

    @SuppressWarnings("cast")
    @Test
    public void testContainsDoubleTolerance() {
        double[] array = null;
        assertFalse(ArrayUtils.contains(array, (double) 1, (double) 0));
        array = new double[]{0, 1, 2, 3, 0};
        assertFalse(ArrayUtils.contains(array, 4.0, 0.33));
        assertFalse(ArrayUtils.contains(array, 2.5, 0.49));
        assertTrue(ArrayUtils.contains(array, 2.5, 0.50));
        assertTrue(ArrayUtils.contains(array, 2.5, 0.51));
    }

    //-----------------------------------------------------------------------
    @SuppressWarnings("cast")
    @Test
    public void testIndexOfFloat() {
        float[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, (float) 0));
        array = new float[0];
        assertEquals(-1, ArrayUtils.indexOf(array, (float) 0));
        array = new float[]{0, 1, 2, 3, 0};
        assertEquals(0, ArrayUtils.indexOf(array, (float) 0));
        assertEquals(1, ArrayUtils.indexOf(array, (float) 1));
        assertEquals(2, ArrayUtils.indexOf(array, (float) 2));
        assertEquals(3, ArrayUtils.indexOf(array, (float) 3));
        assertEquals(-1, ArrayUtils.indexOf(array, (float) 99));
    }

    @SuppressWarnings("cast")
    @Test
    public void testIndexOfFloatWithStartIndex() {
        float[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, (float) 0, 2));
        array = new float[0];
        assertEquals(-1, ArrayUtils.indexOf(array, (float) 0, 2));
        array = new float[]{0, 1, 2, 3, 0};
        assertEquals(4, ArrayUtils.indexOf(array, (float) 0, 2));
        assertEquals(-1, ArrayUtils.indexOf(array, (float) 1, 2));
        assertEquals(2, ArrayUtils.indexOf(array, (float) 2, 2));
        assertEquals(3, ArrayUtils.indexOf(array, (float) 3, 2));
        assertEquals(3, ArrayUtils.indexOf(array, (float) 3, -1));
        assertEquals(-1, ArrayUtils.indexOf(array, (float) 99, 0));
        assertEquals(-1, ArrayUtils.indexOf(array, (float) 0, 6));
    }

    @SuppressWarnings("cast")
    @Test
    public void testLastIndexOfFloat() {
        float[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (float) 0));
        array = new float[0];
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (float) 0));
        array = new float[]{0, 1, 2, 3, 0};
        assertEquals(4, ArrayUtils.lastIndexOf(array, (float) 0));
        assertEquals(1, ArrayUtils.lastIndexOf(array, (float) 1));
        assertEquals(2, ArrayUtils.lastIndexOf(array, (float) 2));
        assertEquals(3, ArrayUtils.lastIndexOf(array, (float) 3));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (float) 99));
    }

    @SuppressWarnings("cast")
    @Test
    public void testLastIndexOfFloatWithStartIndex() {
        float[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (float) 0, 2));
        array = new float[0];
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (float) 0, 2));
        array = new float[]{0, 1, 2, 3, 0};
        assertEquals(0, ArrayUtils.lastIndexOf(array, (float) 0, 2));
        assertEquals(1, ArrayUtils.lastIndexOf(array, (float) 1, 2));
        assertEquals(2, ArrayUtils.lastIndexOf(array, (float) 2, 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (float) 3, 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (float) 3, -1));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (float) 99));
        assertEquals(4, ArrayUtils.lastIndexOf(array, (float) 0, 88));
    }

    @SuppressWarnings("cast")
    @Test
    public void testContainsFloat() {
        float[] array = null;
        assertFalse(ArrayUtils.contains(array, (float) 1));
        array = new float[]{0, 1, 2, 3, 0};
        assertTrue(ArrayUtils.contains(array, (float) 0));
        assertTrue(ArrayUtils.contains(array, (float) 1));
        assertTrue(ArrayUtils.contains(array, (float) 2));
        assertTrue(ArrayUtils.contains(array, (float) 3));
        assertFalse(ArrayUtils.contains(array, (float) 99));
    }

    //-----------------------------------------------------------------------
    @Test
    public void testIndexOfBoolean() {
        boolean[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, true));
        array = new boolean[0];
        assertEquals(-1, ArrayUtils.indexOf(array, true));
        array = new boolean[]{true, false, true};
        assertEquals(0, ArrayUtils.indexOf(array, true));
        assertEquals(1, ArrayUtils.indexOf(array, false));
        array = new boolean[]{true, true};
        assertEquals(-1, ArrayUtils.indexOf(array, false));
    }

    @Test
    public void testIndexOfBooleanWithStartIndex() {
        boolean[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, true, 2));
        array = new boolean[0];
        assertEquals(-1, ArrayUtils.indexOf(array, true, 2));
        array = new boolean[]{true, false, true};
        assertEquals(2, ArrayUtils.indexOf(array, true, 1));
        assertEquals(-1, ArrayUtils.indexOf(array, false, 2));
        assertEquals(1, ArrayUtils.indexOf(array, false, 0));
        assertEquals(1, ArrayUtils.indexOf(array, false, -1));
        array = new boolean[]{true, true};
        assertEquals(-1, ArrayUtils.indexOf(array, false, 0));
        assertEquals(-1, ArrayUtils.indexOf(array, false, -1));
    }

    @Test
    public void testLastIndexOfBoolean() {
        boolean[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, true));
        array = new boolean[0];
        assertEquals(-1, ArrayUtils.lastIndexOf(array, true));
        array = new boolean[]{true, false, true};
        assertEquals(2, ArrayUtils.lastIndexOf(array, true));
        assertEquals(1, ArrayUtils.lastIndexOf(array, false));
        array = new boolean[]{true, true};
        assertEquals(-1, ArrayUtils.lastIndexOf(array, false));
    }

    @Test
    public void testLastIndexOfBooleanWithStartIndex() {
        boolean[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, true, 2));
        array = new boolean[0];
        assertEquals(-1, ArrayUtils.lastIndexOf(array, true, 2));
        array = new boolean[]{true, false, true};
        assertEquals(2, ArrayUtils.lastIndexOf(array, true, 2));
        assertEquals(0, ArrayUtils.lastIndexOf(array, true, 1));
        assertEquals(1, ArrayUtils.lastIndexOf(array, false, 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, true, -1));
        array = new boolean[]{true, true};
        assertEquals(-1, ArrayUtils.lastIndexOf(array, false, 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, true, -1));
    }

    @Test
    public void testContainsBoolean() {
        boolean[] array = null;
        assertFalse(ArrayUtils.contains(array, true));
        array = new boolean[]{true, false, true};
        assertTrue(ArrayUtils.contains(array, true));
        assertTrue(ArrayUtils.contains(array, false));
        array = new boolean[]{true, true};
        assertTrue(ArrayUtils.contains(array, true));
        assertFalse(ArrayUtils.contains(array, false));
    }

    // testToPrimitive/Object for boolean
    //  -----------------------------------------------------------------------
    @Test
    public void testToPrimitive_boolean() {
        final Boolean[] b = null;
        assertEquals(null, ArrayUtils.toPrimitive(b));
        assertSame(ArrayUtils.EMPTY_BOOLEAN_ARRAY, ArrayUtils.toPrimitive(new Boolean[0]));
        assertTrue(Arrays.equals(
                new boolean[]{true, false, true},
                ArrayUtils.toPrimitive(new Boolean[]{Boolean.TRUE, Boolean.FALSE, Boolean.TRUE}))
        );

        try {
            ArrayUtils.toPrimitive(new Boolean[]{Boolean.TRUE, null});
            fail();
        } catch (final NullPointerException ex) {
        }
    }

    @Test
    public void testToPrimitive_boolean_boolean() {
        assertEquals(null, ArrayUtils.toPrimitive(null, false));
        assertSame(ArrayUtils.EMPTY_BOOLEAN_ARRAY, ArrayUtils.toPrimitive(new Boolean[0], false));
        assertTrue(Arrays.equals(
                new boolean[]{true, false, true},
                ArrayUtils.toPrimitive(new Boolean[]{Boolean.TRUE, Boolean.FALSE, Boolean.TRUE}, false))
        );
        assertTrue(Arrays.equals(
                new boolean[]{true, false, false},
                ArrayUtils.toPrimitive(new Boolean[]{Boolean.TRUE, null, Boolean.FALSE}, false))
        );
        assertTrue(Arrays.equals(
                new boolean[]{true, true, false},
                ArrayUtils.toPrimitive(new Boolean[]{Boolean.TRUE, null, Boolean.FALSE}, true))
        );
    }

    @Test
    public void testToObject_boolean() {
        final boolean[] b = null;
        assertArrayEquals(null, ArrayUtils.toObject(b));
        assertSame(ArrayUtils.EMPTY_BOOLEAN_OBJECT_ARRAY, ArrayUtils.toObject(new boolean[0]));
        assertTrue(Arrays.equals(
                new Boolean[]{Boolean.TRUE, Boolean.FALSE, Boolean.TRUE},
                ArrayUtils.toObject(new boolean[]{true, false, true}))
        );
    }

    // testToPrimitive/Object for byte
    //  -----------------------------------------------------------------------
    @Test
    public void testToPrimitive_char() {
        final Character[] b = null;
        assertEquals(null, ArrayUtils.toPrimitive(b));

        assertSame(ArrayUtils.EMPTY_CHAR_ARRAY, ArrayUtils.toPrimitive(new Character[0]));

        assertTrue(Arrays.equals(
                new char[]{Character.MIN_VALUE, Character.MAX_VALUE, '0'},
                ArrayUtils.toPrimitive(new Character[]{new Character(Character.MIN_VALUE),
                        new Character(Character.MAX_VALUE), new Character('0')}))
        );

        try {
            ArrayUtils.toPrimitive(new Character[]{new Character(Character.MIN_VALUE), null});
            fail();
        } catch (final NullPointerException ex) {
        }
    }

    @Test
    public void testToPrimitive_char_char() {
        final Character[] b = null;
        assertEquals(null, ArrayUtils.toPrimitive(b, Character.MIN_VALUE));

        assertSame(ArrayUtils.EMPTY_CHAR_ARRAY,
                ArrayUtils.toPrimitive(new Character[0], (char) 0));

        assertTrue(Arrays.equals(
                new char[]{Character.MIN_VALUE, Character.MAX_VALUE, '0'},
                ArrayUtils.toPrimitive(new Character[]{new Character(Character.MIN_VALUE),
                                new Character(Character.MAX_VALUE), new Character('0')},
                        Character.MIN_VALUE))
        );

        assertTrue(Arrays.equals(
                new char[]{Character.MIN_VALUE, Character.MAX_VALUE, '0'},
                ArrayUtils.toPrimitive(new Character[]{new Character(Character.MIN_VALUE), null,
                        new Character('0')}, Character.MAX_VALUE))
        );
    }

    @Test
    public void testToObject_char() {
        final char[] b = null;
        assertArrayEquals(null, ArrayUtils.toObject(b));

        assertSame(ArrayUtils.EMPTY_CHARACTER_OBJECT_ARRAY,
                ArrayUtils.toObject(new char[0]));

        assertTrue(Arrays.equals(
                new Character[]{new Character(Character.MIN_VALUE),
                        new Character(Character.MAX_VALUE), new Character('0')},
                ArrayUtils.toObject(new char[]{Character.MIN_VALUE, Character.MAX_VALUE,
                        '0'}))
        );
    }

    // testToPrimitive/Object for byte
    //  -----------------------------------------------------------------------
    @Test
    public void testToPrimitive_byte() {
        final Byte[] b = null;
        assertEquals(null, ArrayUtils.toPrimitive(b));

        assertSame(ArrayUtils.EMPTY_BYTE_ARRAY, ArrayUtils.toPrimitive(new Byte[0]));

        assertTrue(Arrays.equals(
                new byte[]{Byte.MIN_VALUE, Byte.MAX_VALUE, (byte) 9999999},
                ArrayUtils.toPrimitive(new Byte[]{Byte.valueOf(Byte.MIN_VALUE),
                        Byte.valueOf(Byte.MAX_VALUE), Byte.valueOf((byte) 9999999)}))
        );

        try {
            ArrayUtils.toPrimitive(new Byte[]{Byte.valueOf(Byte.MIN_VALUE), null});
            fail();
        } catch (final NullPointerException ex) {
        }
    }

    @Test
    public void testToPrimitive_byte_byte() {
        final Byte[] b = null;
        assertEquals(null, ArrayUtils.toPrimitive(b, Byte.MIN_VALUE));

        assertSame(ArrayUtils.EMPTY_BYTE_ARRAY,
                ArrayUtils.toPrimitive(new Byte[0], (byte) 1));

        assertTrue(Arrays.equals(
                new byte[]{Byte.MIN_VALUE, Byte.MAX_VALUE, (byte) 9999999},
                ArrayUtils.toPrimitive(new Byte[]{Byte.valueOf(Byte.MIN_VALUE),
                                Byte.valueOf(Byte.MAX_VALUE), Byte.valueOf((byte) 9999999)},
                        Byte.MIN_VALUE))
        );

        assertTrue(Arrays.equals(
                new byte[]{Byte.MIN_VALUE, Byte.MAX_VALUE, (byte) 9999999},
                ArrayUtils.toPrimitive(new Byte[]{Byte.valueOf(Byte.MIN_VALUE), null,
                        Byte.valueOf((byte) 9999999)}, Byte.MAX_VALUE))
        );
    }

    @Test
    public void testToObject_byte() {
        final byte[] b = null;
        assertArrayEquals(null, ArrayUtils.toObject(b));

        assertSame(ArrayUtils.EMPTY_BYTE_OBJECT_ARRAY,
                ArrayUtils.toObject(new byte[0]));

        assertTrue(Arrays.equals(
                new Byte[]{Byte.valueOf(Byte.MIN_VALUE),
                        Byte.valueOf(Byte.MAX_VALUE), Byte.valueOf((byte) 9999999)},
                ArrayUtils.toObject(new byte[]{Byte.MIN_VALUE, Byte.MAX_VALUE,
                        (byte) 9999999}))
        );
    }

    // testToPrimitive/Object for short
    //  -----------------------------------------------------------------------
    @Test
    public void testToPrimitive_short() {
        final Short[] b = null;
        assertEquals(null, ArrayUtils.toPrimitive(b));

        assertSame(ArrayUtils.EMPTY_SHORT_ARRAY, ArrayUtils.toPrimitive(new Short[0]));

        assertTrue(Arrays.equals(
                new short[]{Short.MIN_VALUE, Short.MAX_VALUE, (short) 9999999},
                ArrayUtils.toPrimitive(new Short[]{Short.valueOf(Short.MIN_VALUE),
                        Short.valueOf(Short.MAX_VALUE), Short.valueOf((short) 9999999)}))
        );

        try {
            ArrayUtils.toPrimitive(new Short[]{Short.valueOf(Short.MIN_VALUE), null});
            fail();
        } catch (final NullPointerException ex) {
        }
    }

    @Test
    public void testToPrimitive_short_short() {
        final Short[] s = null;
        assertEquals(null, ArrayUtils.toPrimitive(s, Short.MIN_VALUE));

        assertSame(ArrayUtils.EMPTY_SHORT_ARRAY, ArrayUtils.toPrimitive(new Short[0],
                Short.MIN_VALUE));

        assertTrue(Arrays.equals(
                new short[]{Short.MIN_VALUE, Short.MAX_VALUE, (short) 9999999},
                ArrayUtils.toPrimitive(new Short[]{Short.valueOf(Short.MIN_VALUE),
                        Short.valueOf(Short.MAX_VALUE), Short.valueOf((short) 9999999)}, Short.MIN_VALUE))
        );

        assertTrue(Arrays.equals(
                new short[]{Short.MIN_VALUE, Short.MAX_VALUE, (short) 9999999},
                ArrayUtils.toPrimitive(new Short[]{Short.valueOf(Short.MIN_VALUE), null,
                        Short.valueOf((short) 9999999)}, Short.MAX_VALUE))
        );
    }

    @Test
    public void testToObject_short() {
        final short[] b = null;
        assertArrayEquals(null, ArrayUtils.toObject(b));

        assertSame(ArrayUtils.EMPTY_SHORT_OBJECT_ARRAY,
                ArrayUtils.toObject(new short[0]));

        assertTrue(Arrays.equals(
                new Short[]{Short.valueOf(Short.MIN_VALUE), Short.valueOf(Short.MAX_VALUE),
                        Short.valueOf((short) 9999999)},
                ArrayUtils.toObject(new short[]{Short.MIN_VALUE, Short.MAX_VALUE,
                        (short) 9999999}))
        );
    }

    //  testToPrimitive/Object for int
    //  -----------------------------------------------------------------------
    @Test
    public void testToPrimitive_int() {
        final Integer[] b = null;
        assertEquals(null, ArrayUtils.toPrimitive(b));
        assertSame(ArrayUtils.EMPTY_INT_ARRAY, ArrayUtils.toPrimitive(new Integer[0]));
        assertTrue(Arrays.equals(
                new int[]{Integer.MIN_VALUE, Integer.MAX_VALUE, 9999999},
                ArrayUtils.toPrimitive(new Integer[]{Integer.valueOf(Integer.MIN_VALUE),
                        Integer.valueOf(Integer.MAX_VALUE), Integer.valueOf(9999999)}))
        );

        try {
            ArrayUtils.toPrimitive(new Integer[]{Integer.valueOf(Integer.MIN_VALUE), null});
            fail();
        } catch (final NullPointerException ex) {
        }
    }

    @Test
    public void testToPrimitive_int_int() {
        final Long[] l = null;
        assertEquals(null, ArrayUtils.toPrimitive(l, Integer.MIN_VALUE));
        assertSame(ArrayUtils.EMPTY_INT_ARRAY,
                ArrayUtils.toPrimitive(new Integer[0], 1));
        assertTrue(Arrays.equals(
                new int[]{Integer.MIN_VALUE, Integer.MAX_VALUE, 9999999},
                ArrayUtils.toPrimitive(new Integer[]{Integer.valueOf(Integer.MIN_VALUE),
                        Integer.valueOf(Integer.MAX_VALUE), Integer.valueOf(9999999)}, 1)));
        assertTrue(Arrays.equals(
                new int[]{Integer.MIN_VALUE, Integer.MAX_VALUE, 9999999},
                ArrayUtils.toPrimitive(new Integer[]{Integer.valueOf(Integer.MIN_VALUE),
                        null, Integer.valueOf(9999999)}, Integer.MAX_VALUE))
        );
    }

    @Test
    public void testToPrimitive_intNull() {
        final Integer[] iArray = null;
        assertEquals(null, ArrayUtils.toPrimitive(iArray, Integer.MIN_VALUE));
    }

    @Test
    public void testToObject_int() {
        final int[] b = null;
        assertArrayEquals(null, ArrayUtils.toObject(b));

        assertSame(
                ArrayUtils.EMPTY_INTEGER_OBJECT_ARRAY,
                ArrayUtils.toObject(new int[0]));

        assertTrue(
                Arrays.equals(
                        new Integer[]{
                                Integer.valueOf(Integer.MIN_VALUE),
                                Integer.valueOf(Integer.MAX_VALUE),
                                Integer.valueOf(9999999)},
                        ArrayUtils.toObject(
                                new int[]{Integer.MIN_VALUE, Integer.MAX_VALUE, 9999999})));
    }

    //  testToPrimitive/Object for long
    //  -----------------------------------------------------------------------
    @Test
    public void testToPrimitive_long() {
        final Long[] b = null;
        assertEquals(null, ArrayUtils.toPrimitive(b));

        assertSame(ArrayUtils.EMPTY_LONG_ARRAY,
                ArrayUtils.toPrimitive(new Long[0]));

        assertTrue(Arrays.equals(
                new long[]{Long.MIN_VALUE, Long.MAX_VALUE, 9999999},
                ArrayUtils.toPrimitive(new Long[]{Long.valueOf(Long.MIN_VALUE),
                        Long.valueOf(Long.MAX_VALUE), Long.valueOf(9999999)}))
        );

        try {
            ArrayUtils.toPrimitive(new Long[]{Long.valueOf(Long.MIN_VALUE), null});
            fail();
        } catch (final NullPointerException ex) {
        }
    }

    @Test
    public void testToPrimitive_long_long() {
        final Long[] l = null;
        assertEquals(null, ArrayUtils.toPrimitive(l, Long.MIN_VALUE));

        assertSame(ArrayUtils.EMPTY_LONG_ARRAY,
                ArrayUtils.toPrimitive(new Long[0], 1));

        assertTrue(Arrays.equals(
                new long[]{Long.MIN_VALUE, Long.MAX_VALUE, 9999999},
                ArrayUtils.toPrimitive(new Long[]{Long.valueOf(Long.MIN_VALUE),
                        Long.valueOf(Long.MAX_VALUE), Long.valueOf(9999999)}, 1)));

        assertTrue(Arrays.equals(
                new long[]{Long.MIN_VALUE, Long.MAX_VALUE, 9999999},
                ArrayUtils.toPrimitive(new Long[]{Long.valueOf(Long.MIN_VALUE),
                        null, Long.valueOf(9999999)}, Long.MAX_VALUE))
        );
    }

    @Test
    public void testToObject_long() {
        final long[] b = null;
        assertArrayEquals(null, ArrayUtils.toObject(b));

        assertSame(
                ArrayUtils.EMPTY_LONG_OBJECT_ARRAY,
                ArrayUtils.toObject(new long[0]));

        assertTrue(
                Arrays.equals(
                        new Long[]{
                                Long.valueOf(Long.MIN_VALUE),
                                Long.valueOf(Long.MAX_VALUE),
                                Long.valueOf(9999999)},
                        ArrayUtils.toObject(
                                new long[]{Long.MIN_VALUE, Long.MAX_VALUE, 9999999})));
    }

    //  testToPrimitive/Object for float
    //  -----------------------------------------------------------------------
    @Test
    public void testToPrimitive_float() {
        final Float[] b = null;
        assertEquals(null, ArrayUtils.toPrimitive(b));

        assertSame(ArrayUtils.EMPTY_FLOAT_ARRAY,
                ArrayUtils.toPrimitive(new Float[0]));

        assertTrue(Arrays.equals(
                new float[]{Float.MIN_VALUE, Float.MAX_VALUE, 9999999},
                ArrayUtils.toPrimitive(new Float[]{Float.valueOf(Float.MIN_VALUE),
                        Float.valueOf(Float.MAX_VALUE), Float.valueOf(9999999)}))
        );

        try {
            ArrayUtils.toPrimitive(new Float[]{Float.valueOf(Float.MIN_VALUE), null});
            fail();
        } catch (final NullPointerException ex) {
        }
    }

    @Test
    public void testToPrimitive_float_float() {
        final Float[] l = null;
        assertEquals(null, ArrayUtils.toPrimitive(l, Float.MIN_VALUE));

        assertSame(ArrayUtils.EMPTY_FLOAT_ARRAY,
                ArrayUtils.toPrimitive(new Float[0], 1));

        assertTrue(Arrays.equals(
                new float[]{Float.MIN_VALUE, Float.MAX_VALUE, 9999999},
                ArrayUtils.toPrimitive(new Float[]{Float.valueOf(Float.MIN_VALUE),
                        Float.valueOf(Float.MAX_VALUE), Float.valueOf(9999999)}, 1)));

        assertTrue(Arrays.equals(
                new float[]{Float.MIN_VALUE, Float.MAX_VALUE, 9999999},
                ArrayUtils.toPrimitive(new Float[]{Float.valueOf(Float.MIN_VALUE),
                        null, Float.valueOf(9999999)}, Float.MAX_VALUE))
        );
    }

    @Test
    public void testToObject_float() {
        final float[] b = null;
        assertArrayEquals(null, ArrayUtils.toObject(b));

        assertSame(
                ArrayUtils.EMPTY_FLOAT_OBJECT_ARRAY,
                ArrayUtils.toObject(new float[0]));

        assertTrue(
                Arrays.equals(
                        new Float[]{
                                Float.valueOf(Float.MIN_VALUE),
                                Float.valueOf(Float.MAX_VALUE),
                                Float.valueOf(9999999)},
                        ArrayUtils.toObject(
                                new float[]{Float.MIN_VALUE, Float.MAX_VALUE, 9999999})));
    }

    //  testToPrimitive/Object for double
    //  -----------------------------------------------------------------------
    @Test
    public void testToPrimitive_double() {
        final Double[] b = null;
        assertEquals(null, ArrayUtils.toPrimitive(b));

        assertSame(ArrayUtils.EMPTY_DOUBLE_ARRAY,
                ArrayUtils.toPrimitive(new Double[0]));

        assertTrue(Arrays.equals(
                new double[]{Double.MIN_VALUE, Double.MAX_VALUE, 9999999},
                ArrayUtils.toPrimitive(new Double[]{Double.valueOf(Double.MIN_VALUE),
                        Double.valueOf(Double.MAX_VALUE), Double.valueOf(9999999)}))
        );

        try {
            ArrayUtils.toPrimitive(new Float[]{Float.valueOf(Float.MIN_VALUE), null});
            fail();
        } catch (final NullPointerException ex) {
        }
    }

    @Test
    public void testToPrimitive_double_double() {
        final Double[] l = null;
        assertEquals(null, ArrayUtils.toPrimitive(l, Double.MIN_VALUE));

        assertSame(ArrayUtils.EMPTY_DOUBLE_ARRAY,
                ArrayUtils.toPrimitive(new Double[0], 1));

        assertTrue(Arrays.equals(
                new double[]{Double.MIN_VALUE, Double.MAX_VALUE, 9999999},
                ArrayUtils.toPrimitive(new Double[]{Double.valueOf(Double.MIN_VALUE),
                        Double.valueOf(Double.MAX_VALUE), Double.valueOf(9999999)}, 1)));

        assertTrue(Arrays.equals(
                new double[]{Double.MIN_VALUE, Double.MAX_VALUE, 9999999},
                ArrayUtils.toPrimitive(new Double[]{Double.valueOf(Double.MIN_VALUE),
                        null, Double.valueOf(9999999)}, Double.MAX_VALUE))
        );
    }

    @Test
    public void testToObject_double() {
        final double[] b = null;
        assertArrayEquals(null, ArrayUtils.toObject(b));

        assertSame(
                ArrayUtils.EMPTY_DOUBLE_OBJECT_ARRAY,
                ArrayUtils.toObject(new double[0]));

        assertTrue(
                Arrays.equals(
                        new Double[]{
                                Double.valueOf(Double.MIN_VALUE),
                                Double.valueOf(Double.MAX_VALUE),
                                Double.valueOf(9999999)},
                        ArrayUtils.toObject(
                                new double[]{Double.MIN_VALUE, Double.MAX_VALUE, 9999999})));
    }

    //-----------------------------------------------------------------------

    /**
     * Test for {@link ArrayUtils#isEmpty(java.lang.Object[])}.
     */
    @Test
    public void testIsEmptyObject() {
        final Object[] emptyArray = new Object[]{};
        final Object[] notEmptyArray = new Object[]{new String("Value")};
        assertTrue(ArrayUtils.isEmpty((Object[]) null));
        assertTrue(ArrayUtils.isEmpty(emptyArray));
        assertFalse(ArrayUtils.isEmpty(notEmptyArray));
    }

    /**
     * Tests for {@link ArrayUtils#isEmpty(long[])},
     * {@link ArrayUtils#isEmpty(int[])},
     * {@link ArrayUtils#isEmpty(short[])},
     * {@link ArrayUtils#isEmpty(char[])},
     * {@link ArrayUtils#isEmpty(byte[])},
     * {@link ArrayUtils#isEmpty(double[])},
     * {@link ArrayUtils#isEmpty(float[])} and
     * {@link ArrayUtils#isEmpty(boolean[])}.
     */
    @Test
    public void testIsEmptyPrimitives() {
        final long[] emptyLongArray = new long[]{};
        final long[] notEmptyLongArray = new long[]{1L};
        assertTrue(ArrayUtils.isEmpty((long[]) null));
        assertTrue(ArrayUtils.isEmpty(emptyLongArray));
        assertFalse(ArrayUtils.isEmpty(notEmptyLongArray));

        final int[] emptyIntArray = new int[]{};
        final int[] notEmptyIntArray = new int[]{1};
        assertTrue(ArrayUtils.isEmpty((int[]) null));
        assertTrue(ArrayUtils.isEmpty(emptyIntArray));
        assertFalse(ArrayUtils.isEmpty(notEmptyIntArray));

        final short[] emptyShortArray = new short[]{};
        final short[] notEmptyShortArray = new short[]{1};
        assertTrue(ArrayUtils.isEmpty((short[]) null));
        assertTrue(ArrayUtils.isEmpty(emptyShortArray));
        assertFalse(ArrayUtils.isEmpty(notEmptyShortArray));

        final char[] emptyCharArray = new char[]{};
        final char[] notEmptyCharArray = new char[]{1};
        assertTrue(ArrayUtils.isEmpty((char[]) null));
        assertTrue(ArrayUtils.isEmpty(emptyCharArray));
        assertFalse(ArrayUtils.isEmpty(notEmptyCharArray));

        final byte[] emptyByteArray = new byte[]{};
        final byte[] notEmptyByteArray = new byte[]{1};
        assertTrue(ArrayUtils.isEmpty((byte[]) null));
        assertTrue(ArrayUtils.isEmpty(emptyByteArray));
        assertFalse(ArrayUtils.isEmpty(notEmptyByteArray));

        final double[] emptyDoubleArray = new double[]{};
        final double[] notEmptyDoubleArray = new double[]{1.0};
        assertTrue(ArrayUtils.isEmpty((double[]) null));
        assertTrue(ArrayUtils.isEmpty(emptyDoubleArray));
        assertFalse(ArrayUtils.isEmpty(notEmptyDoubleArray));

        final float[] emptyFloatArray = new float[]{};
        final float[] notEmptyFloatArray = new float[]{1.0F};
        assertTrue(ArrayUtils.isEmpty((float[]) null));
        assertTrue(ArrayUtils.isEmpty(emptyFloatArray));
        assertFalse(ArrayUtils.isEmpty(notEmptyFloatArray));

        final boolean[] emptyBooleanArray = new boolean[]{};
        final boolean[] notEmptyBooleanArray = new boolean[]{true};
        assertTrue(ArrayUtils.isEmpty((boolean[]) null));
        assertTrue(ArrayUtils.isEmpty(emptyBooleanArray));
        assertFalse(ArrayUtils.isEmpty(notEmptyBooleanArray));
    }

    /**
     * Test for {@link ArrayUtils#isNotEmpty(java.lang.Object[])}.
     */
    @Test
    public void testIsNotEmptyObject() {
        final Object[] emptyArray = new Object[]{};
        final Object[] notEmptyArray = new Object[]{new String("Value")};
        assertFalse(ArrayUtils.isNotEmpty((Object[]) null));
        assertFalse(ArrayUtils.isNotEmpty(emptyArray));
        assertTrue(ArrayUtils.isNotEmpty(notEmptyArray));
    }

    /**
     * Tests for {@link ArrayUtils#isNotEmpty(long[])},
     * {@link ArrayUtils#isNotEmpty(int[])},
     * {@link ArrayUtils#isNotEmpty(short[])},
     * {@link ArrayUtils#isNotEmpty(char[])},
     * {@link ArrayUtils#isNotEmpty(byte[])},
     * {@link ArrayUtils#isNotEmpty(double[])},
     * {@link ArrayUtils#isNotEmpty(float[])} and
     * {@link ArrayUtils#isNotEmpty(boolean[])}.
     */
    @Test
    public void testIsNotEmptyPrimitives() {
        final long[] emptyLongArray = new long[]{};
        final long[] notEmptyLongArray = new long[]{1L};
        assertFalse(ArrayUtils.isNotEmpty((long[]) null));
        assertFalse(ArrayUtils.isNotEmpty(emptyLongArray));
        assertTrue(ArrayUtils.isNotEmpty(notEmptyLongArray));

        final int[] emptyIntArray = new int[]{};
        final int[] notEmptyIntArray = new int[]{1};
        assertFalse(ArrayUtils.isNotEmpty((int[]) null));
        assertFalse(ArrayUtils.isNotEmpty(emptyIntArray));
        assertTrue(ArrayUtils.isNotEmpty(notEmptyIntArray));

        final short[] emptyShortArray = new short[]{};
        final short[] notEmptyShortArray = new short[]{1};
        assertFalse(ArrayUtils.isNotEmpty((short[]) null));
        assertFalse(ArrayUtils.isNotEmpty(emptyShortArray));
        assertTrue(ArrayUtils.isNotEmpty(notEmptyShortArray));

        final char[] emptyCharArray = new char[]{};
        final char[] notEmptyCharArray = new char[]{1};
        assertFalse(ArrayUtils.isNotEmpty((char[]) null));
        assertFalse(ArrayUtils.isNotEmpty(emptyCharArray));
        assertTrue(ArrayUtils.isNotEmpty(notEmptyCharArray));

        final byte[] emptyByteArray = new byte[]{};
        final byte[] notEmptyByteArray = new byte[]{1};
        assertFalse(ArrayUtils.isNotEmpty((byte[]) null));
        assertFalse(ArrayUtils.isNotEmpty(emptyByteArray));
        assertTrue(ArrayUtils.isNotEmpty(notEmptyByteArray));

        final double[] emptyDoubleArray = new double[]{};
        final double[] notEmptyDoubleArray = new double[]{1.0};
        assertFalse(ArrayUtils.isNotEmpty((double[]) null));
        assertFalse(ArrayUtils.isNotEmpty(emptyDoubleArray));
        assertTrue(ArrayUtils.isNotEmpty(notEmptyDoubleArray));

        final float[] emptyFloatArray = new float[]{};
        final float[] notEmptyFloatArray = new float[]{1.0F};
        assertFalse(ArrayUtils.isNotEmpty((float[]) null));
        assertFalse(ArrayUtils.isNotEmpty(emptyFloatArray));
        assertTrue(ArrayUtils.isNotEmpty(notEmptyFloatArray));

        final boolean[] emptyBooleanArray = new boolean[]{};
        final boolean[] notEmptyBooleanArray = new boolean[]{true};
        assertFalse(ArrayUtils.isNotEmpty((boolean[]) null));
        assertFalse(ArrayUtils.isNotEmpty(emptyBooleanArray));
        assertTrue(ArrayUtils.isNotEmpty(notEmptyBooleanArray));
    }

    // ------------------------------------------------------------------------
    @Test
    public void testGetLength() {
        assertEquals(0, ArrayUtils.getLength(null));

        final Object[] emptyObjectArray = new Object[0];
        final Object[] notEmptyObjectArray = new Object[]{"aValue"};
        assertEquals(0, ArrayUtils.getLength(null));
        assertEquals(0, ArrayUtils.getLength(emptyObjectArray));
        assertEquals(1, ArrayUtils.getLength(notEmptyObjectArray));

        final int[] emptyIntArray = new int[]{};
        final int[] notEmptyIntArray = new int[]{1};
        assertEquals(0, ArrayUtils.getLength(null));
        assertEquals(0, ArrayUtils.getLength(emptyIntArray));
        assertEquals(1, ArrayUtils.getLength(notEmptyIntArray));

        final short[] emptyShortArray = new short[]{};
        final short[] notEmptyShortArray = new short[]{1};
        assertEquals(0, ArrayUtils.getLength(null));
        assertEquals(0, ArrayUtils.getLength(emptyShortArray));
        assertEquals(1, ArrayUtils.getLength(notEmptyShortArray));

        final char[] emptyCharArray = new char[]{};
        final char[] notEmptyCharArray = new char[]{1};
        assertEquals(0, ArrayUtils.getLength(null));
        assertEquals(0, ArrayUtils.getLength(emptyCharArray));
        assertEquals(1, ArrayUtils.getLength(notEmptyCharArray));

        final byte[] emptyByteArray = new byte[]{};
        final byte[] notEmptyByteArray = new byte[]{1};
        assertEquals(0, ArrayUtils.getLength(null));
        assertEquals(0, ArrayUtils.getLength(emptyByteArray));
        assertEquals(1, ArrayUtils.getLength(notEmptyByteArray));

        final double[] emptyDoubleArray = new double[]{};
        final double[] notEmptyDoubleArray = new double[]{1.0};
        assertEquals(0, ArrayUtils.getLength(null));
        assertEquals(0, ArrayUtils.getLength(emptyDoubleArray));
        assertEquals(1, ArrayUtils.getLength(notEmptyDoubleArray));

        final float[] emptyFloatArray = new float[]{};
        final float[] notEmptyFloatArray = new float[]{1.0F};
        assertEquals(0, ArrayUtils.getLength(null));
        assertEquals(0, ArrayUtils.getLength(emptyFloatArray));
        assertEquals(1, ArrayUtils.getLength(notEmptyFloatArray));

        final boolean[] emptyBooleanArray = new boolean[]{};
        final boolean[] notEmptyBooleanArray = new boolean[]{true};
        assertEquals(0, ArrayUtils.getLength(null));
        assertEquals(0, ArrayUtils.getLength(emptyBooleanArray));
        assertEquals(1, ArrayUtils.getLength(notEmptyBooleanArray));

        try {
            ArrayUtils.getLength("notAnArray");
            fail("IllegalArgumentException should have been thrown");
        } catch (final IllegalArgumentException e) {
        }
    }

    @Test
    public void testIsSorted() {
        Integer[] array = null;
        assertTrue(ArrayUtils.isSorted(array));

        array = new Integer[]{1};
        assertTrue(ArrayUtils.isSorted(array));

        array = new Integer[]{1, 2, 3};
        assertTrue(ArrayUtils.isSorted(array));

        array = new Integer[]{1, 3, 2};
        assertFalse(ArrayUtils.isSorted(array));
    }

    @Test
    public void testIsSortedComparator() {
        final Comparator<Integer> c = new Comparator<Integer>() {
            @Override
            public int compare(final Integer o1, final Integer o2) {
                return o2.compareTo(o1);
            }
        };

        Integer[] array = null;
        assertTrue(ArrayUtils.isSorted(array, c));

        array = new Integer[]{1};
        assertTrue(ArrayUtils.isSorted(array, c));

        array = new Integer[]{3, 2, 1};
        assertTrue(ArrayUtils.isSorted(array, c));

        array = new Integer[]{1, 3, 2};
        assertFalse(ArrayUtils.isSorted(array, c));
    }

    @Test(expected = IllegalArgumentException.class)
    public void testIsSortedNullComparator() throws Exception {
        ArrayUtils.isSorted(null, null);
    }

    @Test
    public void testIsSortedInt() {
        int[] array = null;
        assertTrue(ArrayUtils.isSorted(array));

        array = new int[]{1};
        assertTrue(ArrayUtils.isSorted(array));

        array = new int[]{1, 2, 3};
        assertTrue(ArrayUtils.isSorted(array));

        array = new int[]{1, 3, 2};
        assertFalse(ArrayUtils.isSorted(array));
    }

    @Test
    public void testIsSortedFloat() {
        float[] array = null;
        assertTrue(ArrayUtils.isSorted(array));

        array = new float[]{0f};
        assertTrue(ArrayUtils.isSorted(array));

        array = new float[]{-1f, 0f, 0.1f, 0.2f};
        assertTrue(ArrayUtils.isSorted(array));

        array = new float[]{-1f, 0.2f, 0.1f, 0f};
        assertFalse(ArrayUtils.isSorted(array));
    }

    @Test
    public void testIsSortedLong() {
        long[] array = null;
        assertTrue(ArrayUtils.isSorted(array));

        array = new long[]{0L};
        assertTrue(ArrayUtils.isSorted(array));

        array = new long[]{-1L, 0L, 1L};
        assertTrue(ArrayUtils.isSorted(array));

        array = new long[]{-1L, 1L, 0L};
        assertFalse(ArrayUtils.isSorted(array));
    }

    @Test
    public void testIsSortedDouble() {
        double[] array = null;
        assertTrue(ArrayUtils.isSorted(array));

        array = new double[]{0.0};
        assertTrue(ArrayUtils.isSorted(array));

        array = new double[]{-1.0, 0.0, 0.1, 0.2};
        assertTrue(ArrayUtils.isSorted(array));

        array = new double[]{-1.0, 0.2, 0.1, 0.0};
        assertFalse(ArrayUtils.isSorted(array));
    }

    @Test
    public void testIsSortedChar() {
        char[] array = null;
        assertTrue(ArrayUtils.isSorted(array));

        array = new char[]{'a'};
        assertTrue(ArrayUtils.isSorted(array));

        array = new char[]{'a', 'b', 'c'};
        assertTrue(ArrayUtils.isSorted(array));

        array = new char[]{'a', 'c', 'b'};
        assertFalse(ArrayUtils.isSorted(array));
    }

    @Test
    public void testIsSortedByte() {
        byte[] array = null;
        assertTrue(ArrayUtils.isSorted(array));

        array = new byte[]{0x10};
        assertTrue(ArrayUtils.isSorted(array));

        array = new byte[]{0x10, 0x20, 0x30};
        assertTrue(ArrayUtils.isSorted(array));

        array = new byte[]{0x10, 0x30, 0x20};
        assertFalse(ArrayUtils.isSorted(array));
    }

    @Test
    public void testIsSortedShort() {
        short[] array = null;
        assertTrue(ArrayUtils.isSorted(array));

        array = new short[]{0};
        assertTrue(ArrayUtils.isSorted(array));

        array = new short[]{-1, 0, 1};
        assertTrue(ArrayUtils.isSorted(array));

        array = new short[]{-1, 1, 0};
        assertFalse(ArrayUtils.isSorted(array));
    }

    @Test
    public void testIsSortedBool() {
        boolean[] array = null;
        assertTrue(ArrayUtils.isSorted(array));

        array = new boolean[]{true};
        assertTrue(ArrayUtils.isSorted(array));

        array = new boolean[]{false, true};
        assertTrue(ArrayUtils.isSorted(array));

        array = new boolean[]{true, false};
        assertFalse(ArrayUtils.isSorted(array));
    }

    @Test
    public void testCreatePrimitiveArray() {
        Assert.assertNull(ArrayUtils.toPrimitive((Object[]) null));
        Assert.assertArrayEquals(new int[]{}, ArrayUtils.toPrimitive(new Integer[]{}));
        Assert.assertArrayEquals(new short[]{2}, ArrayUtils.toPrimitive(new Short[]{2}));
        Assert.assertArrayEquals(new long[]{2, 3}, ArrayUtils.toPrimitive(new Long[]{2L, 3L}));
        Assert.assertArrayEquals(new float[]{3.14f}, ArrayUtils.toPrimitive(new Float[]{3.14f}), 0.1f);
        Assert.assertArrayEquals(new double[]{2.718}, ArrayUtils.toPrimitive(new Double[]{2.718}), 0.1);
    }

    @Test
    public void testToStringArray_array() {
        assertNull(ArrayUtils.toStringArray(null));

        assertArrayEquals(new String[0], ArrayUtils.toStringArray(new Object[0]));

        final Object[] array = new Object[]{1, 2, 3, "array", "test"};
        assertArrayEquals(new String[]{"1", "2", "3", "array", "test"}, ArrayUtils.toStringArray(array));

        try {
            ArrayUtils.toStringArray(new Object[]{null});
            fail("NullPointerException expected!");
        } catch (final NullPointerException expected) {
        }
    }

    @Test
    public void testToStringArray_array_string() {
        assertNull(ArrayUtils.toStringArray(null, ""));

        assertArrayEquals(new String[0], ArrayUtils.toStringArray(new Object[0], ""));

        final Object[] array = new Object[]{1, null, "test"};
        assertArrayEquals(new String[]{"1", "valueForNullElements", "test"},
                ArrayUtils.toStringArray(array, "valueForNullElements"));
    }

    @Test
    public void testShuffle() {
        String[] array1 = new String[]{"1", "2", "3", "4", "5", "6", "7", "8", "9", "10"};
        String[] array2 = ArrayUtils.clone(array1);

        ArrayUtils.shuffle(array1);
        Assert.assertFalse(Arrays.equals(array1, array2));
        for (String element : array2) {
            Assert.assertTrue("Element " + element + " not found", ArrayUtils.contains(array1, element));
        }
    }

    @Test
    public void testShuffleBoolean() {
        boolean[] array1 = new boolean[]{true, false, true, true, false, false, true, false, false, true};
        boolean[] array2 = ArrayUtils.clone(array1);

        ArrayUtils.shuffle(array1);
        Assert.assertFalse(Arrays.equals(array1, array2));
        Assert.assertEquals(5, ArrayUtils.removeAllOccurences(array1, true).length);
    }

    @Test
    public void testShuffleByte() {
        byte[] array1 = new byte[]{1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
        byte[] array2 = ArrayUtils.clone(array1);

        ArrayUtils.shuffle(array1);
        Assert.assertFalse(Arrays.equals(array1, array2));
        for (byte element : array2) {
            Assert.assertTrue("Element " + element + " not found", ArrayUtils.contains(array1, element));
        }
    }

    @Test
    public void testShuffleChar() {
        char[] array1 = new char[]{1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
        char[] array2 = ArrayUtils.clone(array1);

        ArrayUtils.shuffle(array1);
        Assert.assertFalse(Arrays.equals(array1, array2));
        for (char element : array2) {
            Assert.assertTrue("Element " + element + " not found", ArrayUtils.contains(array1, element));
        }
    }

    @Test
    public void testShuffleShort() {
        short[] array1 = new short[]{1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
        short[] array2 = ArrayUtils.clone(array1);

        ArrayUtils.shuffle(array1);
        Assert.assertFalse(Arrays.equals(array1, array2));
        for (short element : array2) {
            Assert.assertTrue("Element " + element + " not found", ArrayUtils.contains(array1, element));
        }
    }

    @Test
    public void testShuffleInt() {
        int[] array1 = new int[]{1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
        int[] array2 = ArrayUtils.clone(array1);

        ArrayUtils.shuffle(array1);
        Assert.assertFalse(Arrays.equals(array1, array2));
        for (int element : array2) {
            Assert.assertTrue("Element " + element + " not found", ArrayUtils.contains(array1, element));
        }
    }

    @Test
    public void testShuffleLong() {
        long[] array1 = new long[]{1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
        long[] array2 = ArrayUtils.clone(array1);

        ArrayUtils.shuffle(array1);
        Assert.assertFalse(Arrays.equals(array1, array2));
        for (long element : array2) {
            Assert.assertTrue("Element " + element + " not found", ArrayUtils.contains(array1, element));
        }
    }

    @Test
    public void testShuffleFloat() {
        float[] array1 = new float[]{1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
        float[] array2 = ArrayUtils.clone(array1);

        ArrayUtils.shuffle(array1);
        Assert.assertFalse(Arrays.equals(array1, array2));
        for (float element : array2) {
            Assert.assertTrue("Element " + element + " not found", ArrayUtils.contains(array1, element));
        }
    }

    @Test
    public void testShuffleDouble() {
        double[] array1 = new double[]{1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
        double[] array2 = ArrayUtils.clone(array1);

        ArrayUtils.shuffle(array1);
        Assert.assertFalse(Arrays.equals(array1, array2));
        for (double element : array2) {
            Assert.assertTrue("Element " + element + " not found", ArrayUtils.contains(array1, element));
        }
    }
}
