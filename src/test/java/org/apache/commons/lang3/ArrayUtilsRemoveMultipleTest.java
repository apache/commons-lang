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
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;

/**
 * Tests ArrayUtils remove and removeElement methods.
 */
public class ArrayUtilsRemoveMultipleTest extends AbstractLangTest {

    @Test
    public void testRemoveAllBooleanArray() {
        boolean[] array;

        array = ArrayUtils.removeAll(new boolean[] { true }, 0);
        assertArrayEquals(ArrayUtils.EMPTY_BOOLEAN_ARRAY, array);
        assertEquals(Boolean.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new boolean[] { true, false }, 0);
        assertArrayEquals(new boolean[]{false}, array);
        assertEquals(Boolean.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new boolean[] { true, false }, 1);
        assertArrayEquals(new boolean[]{true}, array);
        assertEquals(Boolean.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new boolean[] { true, false, true }, 1);
        assertArrayEquals(new boolean[]{true, true}, array);
        assertEquals(Boolean.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new boolean[] { true, false }, 0, 1);
        assertArrayEquals(ArrayUtils.EMPTY_BOOLEAN_ARRAY, array);
        assertEquals(Boolean.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new boolean[] { true, false, false }, 0, 1);
        assertArrayEquals(new boolean[]{false}, array);
        assertEquals(Boolean.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new boolean[] { true, false, false }, 0, 2);
        assertArrayEquals(new boolean[]{false}, array);
        assertEquals(Boolean.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new boolean[] { true, false, false }, 1, 2);
        assertArrayEquals(new boolean[]{true}, array);
        assertEquals(Boolean.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new boolean[] { true, false, true, false, true }, 0, 2, 4);
        assertArrayEquals(new boolean[]{false, false}, array);
        assertEquals(Boolean.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new boolean[] { true, false, true, false, true }, 1, 3);
        assertArrayEquals(new boolean[]{true, true, true}, array);
        assertEquals(Boolean.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new boolean[] { true, false, true, false, true }, 1, 3, 4);
        assertArrayEquals(new boolean[]{true, true}, array);
        assertEquals(Boolean.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new boolean[] { true, false, true, false, true, false, true }, 0, 2, 4, 6);
        assertArrayEquals(new boolean[]{false, false, false}, array);
        assertEquals(Boolean.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new boolean[] { true, false, true, false, true, false, true }, 1, 3, 5);
        assertArrayEquals(new boolean[]{true, true, true, true}, array);
        assertEquals(Boolean.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new boolean[] { true, false, true, false, true, false, true }, 0, 1, 2);
        assertArrayEquals(new boolean[]{false, true, false, true}, array);
        assertEquals(Boolean.TYPE, array.getClass().getComponentType());
    }

    @Test
    public void testRemoveAllBooleanArrayNegativeIndex() {
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.removeAll(new boolean[] { true, false }, -1));
    }

    @Test
    public void testRemoveAllBooleanArrayOutOfBoundsIndex() {
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.removeAll(new boolean[] { true, false }, 2));
    }

    @Test
    public void testRemoveAllBooleanArrayRemoveNone() {
        final boolean[] array1 = { true, false };
        final boolean[] array2 = ArrayUtils.removeAll(array1);
        assertNotSame(array1, array2);
        assertArrayEquals(array1, array2);
        assertEquals(boolean.class, array2.getClass().getComponentType());
    }

    @Test
    public void testRemoveAllByteArray() {
        byte[] array;

        array = ArrayUtils.removeAll(new byte[] { 1 }, 0);
        assertArrayEquals(ArrayUtils.EMPTY_BYTE_ARRAY, array);
        assertEquals(Byte.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new byte[] { 1, 2 }, 0);
        assertArrayEquals(new byte[]{2}, array);
        assertEquals(Byte.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new byte[] { 1, 2 }, 1);
        assertArrayEquals(new byte[]{1}, array);
        assertEquals(Byte.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new byte[] { 1, 2, 1 }, 1);
        assertArrayEquals(new byte[]{1, 1}, array);
        assertEquals(Byte.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new byte[] { 1, 2 }, 0, 1);
        assertArrayEquals(ArrayUtils.EMPTY_BYTE_ARRAY, array);
        assertEquals(Byte.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new byte[] { 1, 2, 3 }, 0, 1);
        assertArrayEquals(new byte[]{3}, array);
        assertEquals(Byte.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new byte[] { 1, 2, 3 }, 1, 2);
        assertArrayEquals(new byte[]{1}, array);
        assertEquals(Byte.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new byte[] { 1, 2, 3 }, 0, 2);
        assertArrayEquals(new byte[]{2}, array);
        assertEquals(Byte.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new byte[] { 1, 2, 3, 4, 5 }, 1, 3);
        assertArrayEquals(new byte[]{1, 3, 5}, array);
        assertEquals(Byte.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new byte[] { 1, 2, 3, 4, 5 }, 0, 2, 4);
        assertArrayEquals(new byte[]{2, 4}, array);
        assertEquals(Byte.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new byte[] { 1, 2, 3, 4, 5, 6, 7 }, 1, 3, 5);
        assertArrayEquals(new byte[]{1, 3, 5, 7}, array);
        assertEquals(Byte.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new byte[] { 1, 2, 3, 4, 5, 6, 7 }, 0, 2, 4, 6);
        assertArrayEquals(new byte[]{2, 4, 6}, array);
        assertEquals(Byte.TYPE, array.getClass().getComponentType());
    }

    @Test
    public void testRemoveAllByteArrayNegativeIndex() {
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.removeAll(new byte[] { 1, 2 }, -1));
    }

    @Test
    public void testRemoveAllByteArrayOutOfBoundsIndex() {
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.removeAll(new byte[] { 1, 2 }, 2));
    }

    @Test
    public void testRemoveAllByteArrayRemoveNone() {
        final byte[] array1 = { 1, 2 };
        final byte[] array2 = ArrayUtils.removeAll(array1);
        assertNotSame(array1, array2);
        assertArrayEquals(array1, array2);
        assertEquals(byte.class, array2.getClass().getComponentType());
    }

    @Test
    public void testRemoveAllCharArray() {
        char[] array;

        array = ArrayUtils.removeAll(new char[] { 'a' }, 0);
        assertArrayEquals(ArrayUtils.EMPTY_CHAR_ARRAY, array);
        assertEquals(Character.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new char[] { 'a', 'b' }, 0);
        assertArrayEquals(new char[]{'b'}, array);
        assertEquals(Character.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new char[] { 'a', 'b' }, 1);
        assertArrayEquals(new char[]{'a'}, array);
        assertEquals(Character.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new char[] { 'a', 'b', 'c' }, 1);
        assertArrayEquals(new char[]{'a', 'c'}, array);
        assertEquals(Character.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new char[] { 'a', 'b' }, 0, 1);
        assertArrayEquals(ArrayUtils.EMPTY_CHAR_ARRAY, array);
        assertEquals(Character.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new char[] { 'a', 'b', 'c' }, 0, 1);
        assertArrayEquals(new char[]{'c'}, array);
        assertEquals(Character.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new char[] { 'a', 'b', 'c' }, 1, 2);
        assertArrayEquals(new char[]{'a'}, array);
        assertEquals(Character.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new char[] { 'a', 'b', 'c' }, 0, 2);
        assertArrayEquals(new char[]{'b'}, array);
        assertEquals(Character.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new char[] { 'a', 'b', 'c', 'd', 'e' }, 1, 3);
        assertArrayEquals(new char[]{'a', 'c', 'e'}, array);
        assertEquals(Character.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new char[] { 'a', 'b', 'c', 'd', 'e' }, 0, 2, 4);
        assertArrayEquals(new char[]{'b', 'd'}, array);
        assertEquals(Character.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new char[] { 'a', 'b', 'c', 'd', 'e', 'f', 'g' }, 1, 3, 5);
        assertArrayEquals(new char[]{'a', 'c', 'e', 'g'}, array);
        assertEquals(Character.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new char[] { 'a', 'b', 'c', 'd', 'e', 'f', 'g' }, 0, 2, 4, 6);
        assertArrayEquals(new char[]{'b', 'd', 'f'}, array);
        assertEquals(Character.TYPE, array.getClass().getComponentType());
    }

    @Test
    public void testRemoveAllCharArrayNegativeIndex() {
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.removeAll(new char[] { 'a', 'b' }, -1));
    }

    @Test
    public void testRemoveAllCharArrayOutOfBoundsIndex() {
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.removeAll(new char[] { 'a', 'b' }, 2));
    }

    @Test
    public void testRemoveAllCharArrayRemoveNone() {
        final char[] array1 = { 'a', 'b' };
        final char[] array2 = ArrayUtils.removeAll(array1);
        assertNotSame(array1, array2);
        assertArrayEquals(array1, array2);
        assertEquals(char.class, array2.getClass().getComponentType());
    }

    @Test
    public void testRemoveAllDoubleArray() {
        double[] array;

        array = ArrayUtils.removeAll(new double[] { 1 }, 0);
        assertArrayEquals(ArrayUtils.EMPTY_DOUBLE_ARRAY, array);
        assertEquals(Double.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new double[] { 1, 2 }, 0);
        assertArrayEquals(new double[]{2}, array);
        assertEquals(Double.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new double[] { 1, 2 }, 1);
        assertArrayEquals(new double[]{1}, array);
        assertEquals(Double.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new double[] { 1, 2, 1 }, 1);
        assertArrayEquals(new double[]{1, 1}, array);
        assertEquals(Double.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new double[] { 1, 2 }, 0, 1);
        assertArrayEquals(ArrayUtils.EMPTY_DOUBLE_ARRAY, array);
        assertEquals(Double.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new double[] { 1, 2, 3 }, 0, 1);
        assertArrayEquals(new double[]{3}, array);
        assertEquals(Double.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new double[] { 1, 2, 3 }, 1, 2);
        assertArrayEquals(new double[]{1}, array);
        assertEquals(Double.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new double[] { 1, 2, 3 }, 0, 2);
        assertArrayEquals(new double[]{2}, array);
        assertEquals(Double.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new double[] { 1, 2, 3, 4, 5 }, 1, 3);
        assertArrayEquals(new double[]{1, 3, 5}, array);
        assertEquals(Double.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new double[] { 1, 2, 3, 4, 5 }, 0, 2, 4);
        assertArrayEquals(new double[]{2, 4}, array);
        assertEquals(Double.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new double[] { 1, 2, 3, 4, 5, 6, 7 }, 1, 3, 5);
        assertArrayEquals(new double[]{1, 3, 5, 7}, array);
        assertEquals(Double.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new double[] { 1, 2, 3, 4, 5, 6, 7 }, 0, 2, 4, 6);
        assertArrayEquals(new double[]{2, 4, 6}, array);
        assertEquals(Double.TYPE, array.getClass().getComponentType());
    }

    @Test
    public void testRemoveAllDoubleArrayNegativeIndex() {
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.removeAll(new double[] { 1, 2 }, -1));
    }

    @Test
    public void testRemoveAllDoubleArrayOutOfBoundsIndex() {
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.removeAll(new double[] { 1, 2 }, 2));
    }

    @Test
    public void testRemoveAllDoubleArrayRemoveNone() {
        final double[] array1 = { 1, 2 };
        final double[] array2 = ArrayUtils.removeAll(array1);
        assertNotSame(array1, array2);
        assertArrayEquals(array1, array2);
        assertEquals(double.class, array2.getClass().getComponentType());
    }

    @Test
    public void testRemoveAllFloatArray() {
        float[] array;

        array = ArrayUtils.removeAll(new float[] { 1 }, 0);
        assertArrayEquals(ArrayUtils.EMPTY_FLOAT_ARRAY, array);
        assertEquals(Float.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new float[] { 1, 2 }, 0);
        assertArrayEquals(new float[]{2}, array);
        assertEquals(Float.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new float[] { 1, 2 }, 1);
        assertArrayEquals(new float[]{1}, array);
        assertEquals(Float.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new float[] { 1, 2, 1 }, 1);
        assertArrayEquals(new float[]{1, 1}, array);
        assertEquals(Float.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new float[] { 1, 2 }, 0, 1);
        assertArrayEquals(ArrayUtils.EMPTY_FLOAT_ARRAY, array);
        assertEquals(Float.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new float[] { 1, 2, 3 }, 0, 1);
        assertArrayEquals(new float[]{3}, array);
        assertEquals(Float.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new float[] { 1, 2, 3 }, 1, 2);
        assertArrayEquals(new float[]{1}, array);
        assertEquals(Float.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new float[] { 1, 2, 3 }, 0, 2);
        assertArrayEquals(new float[]{2}, array);
        assertEquals(Float.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new float[] { 1, 2, 3, 4, 5 }, 1, 3);
        assertArrayEquals(new float[]{1, 3, 5}, array);
        assertEquals(Float.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new float[] { 1, 2, 3, 4, 5 }, 0, 2, 4);
        assertArrayEquals(new float[]{2, 4}, array);
        assertEquals(Float.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new float[] { 1, 2, 3, 4, 5, 6, 7 }, 1, 3, 5);
        assertArrayEquals(new float[]{1, 3, 5, 7}, array);
        assertEquals(Float.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new float[] { 1, 2, 3, 4, 5, 6, 7 }, 0, 2, 4, 6);
        assertArrayEquals(new float[]{2, 4, 6}, array);
        assertEquals(Float.TYPE, array.getClass().getComponentType());
    }

    @Test
    public void testRemoveAllFloatArrayNegativeIndex() {
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.removeAll(new float[] { 1, 2 }, -1));
    }

    @Test
    public void testRemoveAllFloatArrayOutOfBoundsIndex() {
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.removeAll(new float[] { 1, 2 }, 2));
    }

    @Test
    public void testRemoveAllFloatArrayRemoveNone() {
        final float[] array1 = { 1, 2 };
        final float[] array2 = ArrayUtils.removeAll(array1);
        assertNotSame(array1, array2);
        assertArrayEquals(array1, array2);
        assertEquals(float.class, array2.getClass().getComponentType());
    }

    @Test
    public void testRemoveAllIntArray() {
        int[] array;

        array = ArrayUtils.removeAll(ArrayUtils.EMPTY_INT_ARRAY, ArrayUtils.EMPTY_INT_ARRAY);
        assertArrayEquals(ArrayUtils.EMPTY_INT_ARRAY, array);

        array = ArrayUtils.removeAll(new int[] { 1 }, ArrayUtils.EMPTY_INT_ARRAY);
        assertArrayEquals(new int[]{1}, array);

        array = ArrayUtils.removeAll(new int[] { 1 }, 0);
        assertArrayEquals(ArrayUtils.EMPTY_INT_ARRAY, array);
        assertEquals(Integer.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new int[] { 1, 2 }, 0);
        assertArrayEquals(new int[]{2}, array);
        assertEquals(Integer.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new int[] { 1, 2 }, 1);
        assertArrayEquals(new int[]{1}, array);
        assertEquals(Integer.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new int[] { 1, 2, 1 }, 1);
        assertArrayEquals(new int[]{1, 1}, array);
        assertEquals(Integer.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new int[] { 1, 2 }, 0, 1);
        assertArrayEquals(ArrayUtils.EMPTY_INT_ARRAY, array);
        assertEquals(Integer.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new int[] { 1, 2, 3 }, 0, 1);
        assertArrayEquals(new int[]{3}, array);
        assertEquals(Integer.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new int[] { 1, 2, 3 }, 1, 2);
        assertArrayEquals(new int[]{1}, array);
        assertEquals(Integer.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new int[] { 1, 2, 3 }, 0, 2);
        assertArrayEquals(new int[]{2}, array);
        assertEquals(Integer.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new int[] { 1, 2, 3, 4, 5 }, 1, 3);
        assertArrayEquals(new int[]{1, 3, 5}, array);
        assertEquals(Integer.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new int[] { 1, 2, 3, 4, 5 }, 0, 2, 4);
        assertArrayEquals(new int[]{2, 4}, array);
        assertEquals(Integer.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new int[] { 1, 2, 3, 4, 5, 6, 7 }, 1, 3, 5);
        assertArrayEquals(new int[]{1, 3, 5, 7}, array);
        assertEquals(Integer.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new int[] { 1, 2, 3, 4, 5, 6, 7 }, 0, 2, 4, 6);
        assertArrayEquals(new int[]{2, 4, 6}, array);
        assertEquals(Integer.TYPE, array.getClass().getComponentType());
    }

    @Test
    public void testRemoveAllIntArrayNegativeIndex() {
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.removeAll(new int[] { 1, 2 }, -1));
    }

    @Test
    public void testRemoveAllIntArrayOutOfBoundsIndex() {
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.removeAll(new int[] { 1, 2 }, 2));
    }

    @Test
    public void testRemoveAllIntArrayRemoveNone() {
        final int[] array1 = { 1, 2 };
        final int[] array2 = ArrayUtils.removeAll(array1);
        assertNotSame(array1, array2);
        assertArrayEquals(array1, array2);
        assertEquals(int.class, array2.getClass().getComponentType());
    }

    @Test
    public void testRemoveAllLongArray() {
        long[] array;

        array = ArrayUtils.removeAll(new long[] { 1 }, 0);
        assertArrayEquals(ArrayUtils.EMPTY_LONG_ARRAY, array);
        assertEquals(Long.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new long[] { 1, 2 }, 0);
        assertArrayEquals(new long[]{2}, array);
        assertEquals(Long.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new long[] { 1, 2 }, 1);
        assertArrayEquals(new long[]{1}, array);
        assertEquals(Long.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new long[] { 1, 2, 1 }, 1);
        assertArrayEquals(new long[]{1, 1}, array);
        assertEquals(Long.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new long[] { 1, 2 }, 0, 1);
        assertArrayEquals(ArrayUtils.EMPTY_LONG_ARRAY, array);
        assertEquals(Long.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new long[] { 1, 2, 3 }, 0, 1);
        assertArrayEquals(new long[]{3}, array);
        assertEquals(Long.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new long[] { 1, 2, 3 }, 1, 2);
        assertArrayEquals(new long[]{1}, array);
        assertEquals(Long.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new long[] { 1, 2, 3 }, 0, 2);
        assertArrayEquals(new long[]{2}, array);
        assertEquals(Long.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new long[] { 1, 2, 3, 4, 5 }, 1, 3);
        assertArrayEquals(new long[]{1, 3, 5}, array);
        assertEquals(Long.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new long[] { 1, 2, 3, 4, 5 }, 0, 2, 4);
        assertArrayEquals(new long[]{2, 4}, array);
        assertEquals(Long.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new long[] { 1, 2, 3, 4, 5, 6, 7 }, 1, 3, 5);
        assertArrayEquals(new long[]{1, 3, 5, 7}, array);
        assertEquals(Long.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new long[] { 1, 2, 3, 4, 5, 6, 7 }, 0, 2, 4, 6);
        assertArrayEquals(new long[]{2, 4, 6}, array);
        assertEquals(Long.TYPE, array.getClass().getComponentType());
    }

    @Test
    public void testRemoveAllLongArrayNegativeIndex() {
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.removeAll(new long[] { 1, 2 }, -1));
    }

    @Test
    public void testRemoveAllLongArrayOutOfBoundsIndex() {
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.removeAll(new long[] { 1, 2 }, 2));
    }

    @Test
    public void testRemoveAllLongArrayRemoveNone() {
        final long[] array1 = { 1, 2 };
        final long[] array2 = ArrayUtils.removeAll(array1);
        assertNotSame(array1, array2);
        assertArrayEquals(array1, array2);
        assertEquals(long.class, array2.getClass().getComponentType());
    }

    @Test
    public void testRemoveAllNullBooleanArray() {
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.removeAll((boolean[]) null, 0));
    }

    @Test
    public void testRemoveAllNullByteArray() {
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.removeAll((byte[]) null, 0));
    }

    @Test
    public void testRemoveAllNullCharArray() {
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.removeAll((char[]) null, 0));
    }

    @Test
    public void testRemoveAllNullDoubleArray() {
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.removeAll((double[]) null, 0));
    }

    @Test
    public void testRemoveAllNullFloatArray() {
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.removeAll((float[]) null, 0));
    }

    @Test
    public void testRemoveAllNullIntArray() {
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.removeAll((int[]) null, 0));
    }

    @Test
    public void testRemoveAllNullLongArray() {
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.removeAll((long[]) null, 0));
    }

    @Test
    public void testRemoveAllNullObjectArray() {
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.remove((Object[]) null, 0));
    }

    @Test
    public void testRemoveAllNullShortArray() {
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.removeAll((short[]) null, 0));
    }

    @Test
    public void testRemoveAllNumberArray() {
        final Number[] inarray = { Integer.valueOf(1), Long.valueOf(2L), Byte.valueOf((byte) 3) };
        assertEquals(3, inarray.length);
        Number[] outarray;

        outarray = ArrayUtils.removeAll(inarray, 1);
        assertArrayEquals(new Number[] { Integer.valueOf(1), Byte.valueOf((byte) 3) }, outarray);
        assertEquals(Number.class, outarray.getClass().getComponentType());

        outarray = ArrayUtils.removeAll(outarray, 1);
        assertArrayEquals(new Number[] { Integer.valueOf(1) }, outarray);
        assertEquals(Number.class, outarray.getClass().getComponentType());

        outarray = ArrayUtils.removeAll(outarray, 0);
        assertEquals(0, outarray.length);
        assertEquals(Number.class, outarray.getClass().getComponentType());

        outarray = ArrayUtils.removeAll(inarray, 0, 1);
        assertArrayEquals(new Number[] { Byte.valueOf((byte) 3) }, outarray);
        assertEquals(Number.class, outarray.getClass().getComponentType());

        outarray = ArrayUtils.removeAll(inarray, 0, 2);
        assertArrayEquals(new Number[] { Long.valueOf(2L) }, outarray);
        assertEquals(Number.class, outarray.getClass().getComponentType());

        outarray = ArrayUtils.removeAll(inarray, 1, 2);
        assertArrayEquals(new Number[] { Integer.valueOf(1) }, outarray);
        assertEquals(Number.class, outarray.getClass().getComponentType());
    }

    @Test
    public void testRemoveAllObjectArray() {
        Object[] array;

        array = ArrayUtils.removeAll(new Object[] { "a" }, 0);
        assertArrayEquals(ArrayUtils.EMPTY_OBJECT_ARRAY, array);
        assertEquals(Object.class, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new Object[] { "a", "b" }, 0, 1);
        assertArrayEquals(ArrayUtils.EMPTY_OBJECT_ARRAY, array);
        assertEquals(Object.class, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new Object[] { "a", "b", "c" }, 1, 2);
        assertArrayEquals(new Object[] { "a" }, array);
        assertEquals(Object.class, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new Object[] { "a", "b", "c", "d" }, 1, 2);
        assertArrayEquals(new Object[] { "a", "d" }, array);
        assertEquals(Object.class, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new Object[] { "a", "b", "c", "d" }, 0, 3);
        assertArrayEquals(new Object[] { "b", "c" }, array);
        assertEquals(Object.class, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new Object[] { "a", "b", "c", "d" }, 0, 1, 3);
        assertArrayEquals(new Object[] { "c" }, array);
        assertEquals(Object.class, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new Object[] { "a", "b", "c", "d", "e" }, 0, 1, 3);
        assertArrayEquals(new Object[] { "c", "e" }, array);
        assertEquals(Object.class, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new Object[] { "a", "b", "c", "d", "e" }, 0, 2, 4);
        assertArrayEquals(new Object[] { "b", "d" }, array);
        assertEquals(Object.class, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new Object[] { "a", "b", "c", "d" }, 0, 1, 3, 0, 1, 3);
        assertArrayEquals(new Object[] { "c" }, array);
        assertEquals(Object.class, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new Object[] { "a", "b", "c", "d" }, 2, 1, 0, 3);
        assertArrayEquals(ArrayUtils.EMPTY_OBJECT_ARRAY, array);
        assertEquals(Object.class, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new Object[] { "a", "b", "c", "d" }, 2, 0, 1, 3, 0, 2, 1, 3);
        assertArrayEquals(ArrayUtils.EMPTY_OBJECT_ARRAY, array);
        assertEquals(Object.class, array.getClass().getComponentType());
    }

    @Test
    public void testRemoveAllObjectArrayNegativeIndex() {
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.removeAll(new Object[] { "a", "b" }, -1));
    }

    @Test
    public void testRemoveAllObjectArrayOutOfBoundsIndex() {
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.removeAll(new Object[] { "a", "b" }, 2));
    }

    @Test
    public void testRemoveAllObjectArrayRemoveNone() {
        final Object[] array1 = { "foo", "bar", "baz" };
        final Object[] array2 = ArrayUtils.removeAll(array1);
        assertNotSame(array1, array2);
        assertArrayEquals(array1, array2);
        assertEquals(Object.class, array2.getClass().getComponentType());
    }

    @Test
    public void testRemoveAllShortArray() {
        short[] array;

        array = ArrayUtils.removeAll(new short[] { 1 }, 0);
        assertArrayEquals(ArrayUtils.EMPTY_SHORT_ARRAY, array);
        assertEquals(Short.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new short[] { 1, 2 }, 0);
        assertArrayEquals(new short[]{2}, array);
        assertEquals(Short.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new short[] { 1, 2 }, 1);
        assertArrayEquals(new short[]{1}, array);
        assertEquals(Short.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new short[] { 1, 2, 1 }, 1);
        assertArrayEquals(new short[]{1, 1}, array);
        assertEquals(Short.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new short[] { 1, 2 }, 0, 1);
        assertArrayEquals(ArrayUtils.EMPTY_SHORT_ARRAY, array);
        assertEquals(Short.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new short[] { 1, 2, 3 }, 0, 1);
        assertArrayEquals(new short[]{3}, array);
        assertEquals(Short.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new short[] { 1, 2, 3 }, 1, 2);
        assertArrayEquals(new short[]{1}, array);
        assertEquals(Short.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new short[] { 1, 2, 3 }, 0, 2);
        assertArrayEquals(new short[]{2}, array);
        assertEquals(Short.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new short[] { 1, 2, 3, 4, 5 }, 1, 3);
        assertArrayEquals(new short[]{1, 3, 5}, array);
        assertEquals(Short.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new short[] { 1, 2, 3, 4, 5 }, 0, 2, 4);
        assertArrayEquals(new short[]{2, 4}, array);
        assertEquals(Short.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new short[] { 1, 2, 3, 4, 5, 6, 7 }, 1, 3, 5);
        assertArrayEquals(new short[]{1, 3, 5, 7}, array);
        assertEquals(Short.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeAll(new short[] { 1, 2, 3, 4, 5, 6, 7 }, 0, 2, 4, 6);
        assertArrayEquals(new short[]{2, 4, 6}, array);
        assertEquals(Short.TYPE, array.getClass().getComponentType());
    }

    @Test
    public void testRemoveAllShortArrayNegativeIndex() {
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.removeAll(new short[] { 1, 2 }, -1, 0));
    }

    @Test
    public void testRemoveAllShortArrayOutOfBoundsIndex() {
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.removeAll(new short[] { 1, 2 }, 2, 0));
    }

    @Test
    public void testRemoveAllShortArrayRemoveNone() {
        final short[] array1 = { 1, 2 };
        final short[] array2 = ArrayUtils.removeAll(array1);
        assertNotSame(array1, array2);
        assertArrayEquals(array1, array2);
        assertEquals(short.class, array2.getClass().getComponentType());
    }

    @Test
    public void testRemoveElementBooleanArray() {
        boolean[] array;

        array = ArrayUtils.removeElements((boolean[]) null, true);
        assertNull(array);

        array = ArrayUtils.removeElements(ArrayUtils.EMPTY_BOOLEAN_ARRAY, true);
        assertArrayEquals(ArrayUtils.EMPTY_BOOLEAN_ARRAY, array);
        assertEquals(Boolean.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new boolean[] { true }, true);
        assertArrayEquals(ArrayUtils.EMPTY_BOOLEAN_ARRAY, array);
        assertEquals(Boolean.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new boolean[] { true, false }, true);
        assertArrayEquals(new boolean[]{false}, array);
        assertEquals(Boolean.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new boolean[] { true, false, true }, true);
        assertArrayEquals(new boolean[]{false, true}, array);
        assertEquals(Boolean.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements((boolean[]) null, true, false);
        assertNull(array);

        array = ArrayUtils.removeElements(ArrayUtils.EMPTY_BOOLEAN_ARRAY, true, false);
        assertArrayEquals(ArrayUtils.EMPTY_BOOLEAN_ARRAY, array);
        assertEquals(Boolean.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new boolean[] { true }, true, false);
        assertArrayEquals(ArrayUtils.EMPTY_BOOLEAN_ARRAY, array);
        assertEquals(Boolean.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new boolean[] { true, false }, true, false);
        assertArrayEquals(ArrayUtils.EMPTY_BOOLEAN_ARRAY, array);
        assertEquals(Boolean.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new boolean[] { true, false }, true, true);
        assertArrayEquals(new boolean[]{false}, array);
        assertEquals(Boolean.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new boolean[] { true, false, true }, true, false);
        assertArrayEquals(new boolean[]{true}, array);
        assertEquals(Boolean.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new boolean[] { true, false, true }, true, true);
        assertArrayEquals(new boolean[]{false}, array);
        assertEquals(Boolean.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new boolean[] { true, false, true }, true, true, true, true);
        assertArrayEquals(new boolean[]{false}, array);
        assertEquals(Boolean.TYPE, array.getClass().getComponentType());
    }

    @Test
    public void testRemoveElementByteArray() {
        byte[] array;

        array = ArrayUtils.removeElements((byte[]) null, (byte) 1);
        assertNull(array);

        array = ArrayUtils.removeElements(ArrayUtils.EMPTY_BYTE_ARRAY, (byte) 1);
        assertArrayEquals(ArrayUtils.EMPTY_BYTE_ARRAY, array);
        assertEquals(Byte.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new byte[] { 1 }, (byte) 1);
        assertArrayEquals(ArrayUtils.EMPTY_BYTE_ARRAY, array);
        assertEquals(Byte.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new byte[] { 1, 2 }, (byte) 1);
        assertArrayEquals(new byte[]{2}, array);
        assertEquals(Byte.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new byte[] { 1, 2, 1 }, (byte) 1);
        assertArrayEquals(new byte[]{2, 1}, array);
        assertEquals(Byte.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements((byte[]) null, (byte) 1, (byte) 2);
        assertNull(array);

        array = ArrayUtils.removeElements(ArrayUtils.EMPTY_BYTE_ARRAY, (byte) 1, (byte) 2);
        assertArrayEquals(ArrayUtils.EMPTY_BYTE_ARRAY, array);
        assertEquals(Byte.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new byte[] { 1 }, (byte) 1, (byte) 2);
        assertArrayEquals(ArrayUtils.EMPTY_BYTE_ARRAY, array);
        assertEquals(Byte.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new byte[] { 1, 2 }, (byte) 1, (byte) 2);
        assertArrayEquals(ArrayUtils.EMPTY_BYTE_ARRAY, array);
        assertEquals(Byte.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new byte[] { 1, 2 }, (byte) 1, (byte) 1);
        assertArrayEquals(new byte[]{2}, array);
        assertEquals(Byte.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new byte[] { 1, 2, 1 }, (byte) 1, (byte) 2);
        assertArrayEquals(new byte[]{1}, array);
        assertEquals(Byte.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new byte[] { 1, 2, 1 }, (byte) 1, (byte) 1);
        assertArrayEquals(new byte[]{2}, array);
        assertEquals(Byte.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new byte[] { 1, 2, 1 }, (byte) 1, (byte) 1, (byte) 1, (byte) 1);
        assertArrayEquals(new byte[]{2}, array);
        assertEquals(Byte.TYPE, array.getClass().getComponentType());
    }

    @Test
    public void testRemoveElementCharArray() {
        char[] array;

        array = ArrayUtils.removeElements((char[]) null, 'a');
        assertNull(array);

        array = ArrayUtils.removeElements(ArrayUtils.EMPTY_CHAR_ARRAY, 'a');
        assertArrayEquals(ArrayUtils.EMPTY_CHAR_ARRAY, array);
        assertEquals(Character.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new char[] { 'a' }, 'a');
        assertArrayEquals(ArrayUtils.EMPTY_CHAR_ARRAY, array);
        assertEquals(Character.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new char[] { 'a', 'b' }, 'a');
        assertArrayEquals(new char[]{'b'}, array);
        assertEquals(Character.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new char[] { 'a', 'b', 'a' }, 'a');
        assertArrayEquals(new char[]{'b', 'a'}, array);
        assertEquals(Character.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements((char[]) null, 'a', 'b');
        assertNull(array);

        array = ArrayUtils.removeElements(ArrayUtils.EMPTY_CHAR_ARRAY, 'a', 'b');
        assertArrayEquals(ArrayUtils.EMPTY_CHAR_ARRAY, array);
        assertEquals(Character.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new char[] { 'a' }, 'a', 'b');
        assertArrayEquals(ArrayUtils.EMPTY_CHAR_ARRAY, array);
        assertEquals(Character.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new char[] { 'a', 'b' }, 'a', 'b');
        assertArrayEquals(ArrayUtils.EMPTY_CHAR_ARRAY, array);
        assertEquals(Character.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new char[] { 'a', 'b' }, 'a', 'a');
        assertArrayEquals(new char[]{'b'}, array);
        assertEquals(Character.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new char[] { 'a', 'b', 'a' }, 'a', 'b');
        assertArrayEquals(new char[]{'a'}, array);
        assertEquals(Character.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new char[] { 'a', 'b', 'a' }, 'a', 'a');
        assertArrayEquals(new char[]{'b'}, array);
        assertEquals(Character.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new char[] { 'a', 'b', 'a' }, 'a', 'a', 'a', 'a');
        assertArrayEquals(new char[]{'b'}, array);
        assertEquals(Character.TYPE, array.getClass().getComponentType());
    }

    @Test
    @SuppressWarnings("cast")
    public void testRemoveElementDoubleArray() {
        double[] array;

        array = ArrayUtils.removeElements((double[]) null, (double) 1);
        assertNull(array);

        array = ArrayUtils.removeElements(ArrayUtils.EMPTY_DOUBLE_ARRAY, (double) 1);
        assertArrayEquals(ArrayUtils.EMPTY_DOUBLE_ARRAY, array);
        assertEquals(Double.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new double[] { 1 }, (double) 1);
        assertArrayEquals(ArrayUtils.EMPTY_DOUBLE_ARRAY, array);
        assertEquals(Double.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new double[] { 1, 2 }, (double) 1);
        assertArrayEquals(new double[]{2}, array);
        assertEquals(Double.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new double[] { 1, 2, 1 }, (double) 1);
        assertArrayEquals(new double[]{2, 1}, array);
        assertEquals(Double.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements((double[]) null, (double) 1, (double) 2);
        assertNull(array);

        array = ArrayUtils.removeElements(ArrayUtils.EMPTY_DOUBLE_ARRAY, (double) 1, (double) 2);
        assertArrayEquals(ArrayUtils.EMPTY_DOUBLE_ARRAY, array);
        assertEquals(Double.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new double[] { 1 }, (double) 1, (double) 2);
        assertArrayEquals(ArrayUtils.EMPTY_DOUBLE_ARRAY, array);
        assertEquals(Double.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new double[] { 1, 2 }, (double) 1, (double) 2);
        assertArrayEquals(ArrayUtils.EMPTY_DOUBLE_ARRAY, array);
        assertEquals(Double.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new double[] { 1, 2 }, (double) 1, (double) 1);
        assertArrayEquals(new double[]{2}, array);
        assertEquals(Double.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new double[] { 1, 2, 1 }, (double) 1, (double) 2);
        assertArrayEquals(new double[]{1}, array);
        assertEquals(Double.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new double[] { 1, 2, 1 }, (double) 1, (double) 1);
        assertArrayEquals(new double[]{2}, array);
        assertEquals(Double.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new double[] { 1, 2, 1 }, (double) 1, (double) 1, (double) 1, (double) 1);
        assertArrayEquals(new double[]{2}, array);
        assertEquals(Double.TYPE, array.getClass().getComponentType());
    }

    @Test
    @SuppressWarnings("cast")
    public void testRemoveElementFloatArray() {
        float[] array;

        array = ArrayUtils.removeElements((float[]) null, (float) 1);
        assertNull(array);

        array = ArrayUtils.removeElements(ArrayUtils.EMPTY_FLOAT_ARRAY, (float) 1);
        assertArrayEquals(ArrayUtils.EMPTY_FLOAT_ARRAY, array);
        assertEquals(Float.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new float[] { 1 }, (float) 1);
        assertArrayEquals(ArrayUtils.EMPTY_FLOAT_ARRAY, array);
        assertEquals(Float.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new float[] { 1, 2 }, (float) 1);
        assertArrayEquals(new float[]{2}, array);
        assertEquals(Float.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new float[] { 1, 2, 1 }, (float) 1);
        assertArrayEquals(new float[]{2, 1}, array);
        assertEquals(Float.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements((float[]) null, (float) 1, (float) 1);
        assertNull(array);

        array = ArrayUtils.removeElements(ArrayUtils.EMPTY_FLOAT_ARRAY, (float) 1, (float) 1);
        assertArrayEquals(ArrayUtils.EMPTY_FLOAT_ARRAY, array);
        assertEquals(Float.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new float[] { 1 }, (float) 1, (float) 1);
        assertArrayEquals(ArrayUtils.EMPTY_FLOAT_ARRAY, array);
        assertEquals(Float.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new float[] { 1, 2 }, (float) 1, (float) 2);
        assertArrayEquals(ArrayUtils.EMPTY_FLOAT_ARRAY, array);
        assertEquals(Float.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new float[] { 1, 2 }, (float) 1, (float) 1);
        assertArrayEquals(new float[]{2}, array);
        assertEquals(Float.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new float[] { 1, 2, 1 }, (float) 1, (float) 1);
        assertArrayEquals(new float[]{2}, array);
        assertEquals(Float.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new float[] { 1, 2, 1 }, (float) 1, (float) 2);
        assertArrayEquals(new float[]{1}, array);
        assertEquals(Float.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new float[] { 1, 2, 1 }, (float) 1, (float) 1, (float) 1, (float) 1);
        assertArrayEquals(new float[]{2}, array);
        assertEquals(Float.TYPE, array.getClass().getComponentType());
    }

    @Test
    public void testRemoveElementIntArray() {
        int[] array;

        array = ArrayUtils.removeElements((int[]) null, 1);
        assertNull(array);

        array = ArrayUtils.removeElements(ArrayUtils.EMPTY_INT_ARRAY, 1);
        assertArrayEquals(ArrayUtils.EMPTY_INT_ARRAY, array);
        assertEquals(Integer.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new int[] { 1 }, 1);
        assertArrayEquals(ArrayUtils.EMPTY_INT_ARRAY, array);
        assertEquals(Integer.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new int[] { 1, 2 }, 1);
        assertArrayEquals(new int[]{2}, array);
        assertEquals(Integer.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new int[] { 1, 2, 1 }, 1);
        assertArrayEquals(new int[]{2, 1}, array);
        assertEquals(Integer.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements((int[]) null, 1);
        assertNull(array);

        array = ArrayUtils.removeElements(ArrayUtils.EMPTY_INT_ARRAY, 1, 1);
        assertArrayEquals(ArrayUtils.EMPTY_INT_ARRAY, array);
        assertEquals(Integer.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new int[] { 1 }, 1, 1);
        assertArrayEquals(ArrayUtils.EMPTY_INT_ARRAY, array);
        assertEquals(Integer.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new int[] { 1, 2 }, 1, 2);
        assertArrayEquals(ArrayUtils.EMPTY_INT_ARRAY, array);
        assertEquals(Integer.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new int[] { 1, 2 }, 1, 1);
        assertArrayEquals(new int[]{2}, array);
        assertEquals(Integer.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new int[] { 1, 2, 1 }, 1, 2);
        assertArrayEquals(new int[]{1}, array);
        assertEquals(Integer.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new int[] { 1, 2, 1 }, 1, 1);
        assertArrayEquals(new int[]{2}, array);
        assertEquals(Integer.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new int[] { 1, 2, 1 }, 1, 1, 1, 1);
        assertArrayEquals(new int[]{2}, array);
        assertEquals(Integer.TYPE, array.getClass().getComponentType());
    }

    @Test
    public void testRemoveElementLongArray() {
        long[] array;

        array = ArrayUtils.removeElements((long[]) null, 1L);
        assertNull(array);

        array = ArrayUtils.removeElements(ArrayUtils.EMPTY_LONG_ARRAY, 1L);
        assertArrayEquals(ArrayUtils.EMPTY_LONG_ARRAY, array);
        assertEquals(Long.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new long[] { 1 }, 1L);
        assertArrayEquals(ArrayUtils.EMPTY_LONG_ARRAY, array);
        assertEquals(Long.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new long[] { 1, 2 }, 1L);
        assertArrayEquals(new long[]{2}, array);
        assertEquals(Long.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new long[] { 1, 2, 1 }, 1L);
        assertArrayEquals(new long[]{2, 1}, array);
        assertEquals(Long.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements((long[]) null, 1L, 1L);
        assertNull(array);

        array = ArrayUtils.removeElements(ArrayUtils.EMPTY_LONG_ARRAY, 1L, 1L);
        assertArrayEquals(ArrayUtils.EMPTY_LONG_ARRAY, array);
        assertEquals(Long.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new long[] { 1 }, 1L, 1L);
        assertArrayEquals(ArrayUtils.EMPTY_LONG_ARRAY, array);
        assertEquals(Long.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new long[] { 1, 2 }, 1L, 2L);
        assertArrayEquals(ArrayUtils.EMPTY_LONG_ARRAY, array);
        assertEquals(Long.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new long[] { 1, 2 }, 1L, 1L);
        assertArrayEquals(new long[]{2}, array);
        assertEquals(Long.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new long[] { 1, 2, 1 }, 1L, 1L);
        assertArrayEquals(new long[]{2}, array);
        assertEquals(Long.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new long[] { 1, 2, 1 }, 1L, 2L);
        assertArrayEquals(new long[]{1}, array);
        assertEquals(Long.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new long[] { 1, 2, 1 }, 1L, 1L, 1L, 1L);
        assertArrayEquals(new long[]{2}, array);
        assertEquals(Long.TYPE, array.getClass().getComponentType());
    }

    @Test
    public void testRemoveElementShortArray() {
        short[] array;

        array = ArrayUtils.removeElements((short[]) null, (short) 1);
        assertNull(array);

        array = ArrayUtils.removeElements(ArrayUtils.EMPTY_SHORT_ARRAY, (short) 1);
        assertArrayEquals(ArrayUtils.EMPTY_SHORT_ARRAY, array);
        assertEquals(Short.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new short[] { 1 }, (short) 1);
        assertArrayEquals(ArrayUtils.EMPTY_SHORT_ARRAY, array);
        assertEquals(Short.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new short[] { 1, 2 }, (short) 1);
        assertArrayEquals(new short[]{2}, array);
        assertEquals(Short.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new short[] { 1, 2, 1 }, (short) 1);
        assertArrayEquals(new short[]{2, 1}, array);
        assertEquals(Short.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements((short[]) null, (short) 1, (short) 1);
        assertNull(array);

        array = ArrayUtils.removeElements(ArrayUtils.EMPTY_SHORT_ARRAY, (short) 1, (short) 1);
        assertArrayEquals(ArrayUtils.EMPTY_SHORT_ARRAY, array);
        assertEquals(Short.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new short[] { 1 }, (short) 1, (short) 1);
        assertArrayEquals(ArrayUtils.EMPTY_SHORT_ARRAY, array);
        assertEquals(Short.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new short[] { 1, 2 }, (short) 1, (short) 2);
        assertArrayEquals(ArrayUtils.EMPTY_SHORT_ARRAY, array);
        assertEquals(Short.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new short[] { 1, 2 }, (short) 1, (short) 1);
        assertArrayEquals(new short[]{2}, array);
        assertEquals(Short.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new short[] { 1, 2, 1 }, (short) 1, (short) 1);
        assertArrayEquals(new short[]{2}, array);
        assertEquals(Short.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new short[] { 1, 2, 1 }, (short) 1, (short) 2);
        assertArrayEquals(new short[]{1}, array);
        assertEquals(Short.TYPE, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new short[] { 1, 2, 1 }, (short) 1, (short) 1, (short) 1, (short) 1);
        assertArrayEquals(new short[]{2}, array);
        assertEquals(Short.TYPE, array.getClass().getComponentType());
    }

    @Test
    public void testRemoveElementsObjectArray() {
        Object[] array;

        array = ArrayUtils.removeElements((Object[]) null, "a");
        assertNull(array);

        array = ArrayUtils.removeElements(ArrayUtils.EMPTY_OBJECT_ARRAY, "a");
        assertArrayEquals(ArrayUtils.EMPTY_OBJECT_ARRAY, array);
        assertEquals(Object.class, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new Object[] { "a" }, "a");
        assertArrayEquals(ArrayUtils.EMPTY_OBJECT_ARRAY, array);
        assertEquals(Object.class, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new Object[] { "a", "b" }, "a");
        assertArrayEquals(new Object[]{"b"}, array);
        assertEquals(Object.class, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new Object[] { "a", "b", "a" }, "a");
        assertArrayEquals(new Object[]{"b", "a"}, array);
        assertEquals(Object.class, array.getClass().getComponentType());

        array = ArrayUtils.removeElements((Object[]) null, "a", "b");
        assertNull(array);

        array = ArrayUtils.removeElements(ArrayUtils.EMPTY_OBJECT_ARRAY, "a", "b");
        assertArrayEquals(ArrayUtils.EMPTY_OBJECT_ARRAY, array);
        assertEquals(Object.class, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new Object[] { "a" }, "a", "b");
        assertArrayEquals(ArrayUtils.EMPTY_OBJECT_ARRAY, array);
        assertEquals(Object.class, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new Object[] { "a", "b" }, "a", "c");
        assertArrayEquals(new Object[]{"b"}, array);
        assertEquals(Object.class, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new Object[] { "a", "b", "a" }, "a");
        assertArrayEquals(new Object[]{"b", "a"}, array);
        assertEquals(Object.class, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new Object[] { "a", "b", "a" }, "a", "b");
        assertArrayEquals(new Object[]{"a"}, array);
        assertEquals(Object.class, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new Object[] { "a", "b", "a" }, "a", "a");
        assertArrayEquals(new Object[]{"b"}, array);
        assertEquals(Object.class, array.getClass().getComponentType());

        array = ArrayUtils.removeElements(new Object[] { "a", "b", "a" }, "a", "a", "a", "a");
        assertArrayEquals(new Object[]{"b"}, array);
        assertEquals(Object.class, array.getClass().getComponentType());
    }

}
