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

package org.apache.commons.lang3;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;

/**
 * Tests ArrayUtils remove and removeElement methods.
 */
class ArrayUtilsRemoveTest extends AbstractLangTest {

    @Test
    void testRemoveAllBooleanOccurences() {
        boolean[] a = null;
        assertNull(ArrayUtils.removeAllOccurences(a, true));

        a = new boolean[0];
        assertArrayEquals(ArrayUtils.EMPTY_BOOLEAN_ARRAY, ArrayUtils.removeAllOccurences(a, true));

        a = new boolean[] { true };
        assertArrayEquals(ArrayUtils.EMPTY_BOOLEAN_ARRAY, ArrayUtils.removeAllOccurences(a, true));

        a = new boolean[] { true, true };
        assertArrayEquals(ArrayUtils.EMPTY_BOOLEAN_ARRAY, ArrayUtils.removeAllOccurences(a, true));

        a = new boolean[] { false, true, true, false, true };
        assertArrayEquals(new boolean[]{false, false}, ArrayUtils.removeAllOccurences(a, true));

        a = new boolean[] { false, true, true, false, true };
        assertArrayEquals(new boolean[]{true, true, true}, ArrayUtils.removeAllOccurences(a, false));
    }

    @Test
    void testRemoveAllBooleanOccurrences() {
        boolean[] a = null;
        assertNull(ArrayUtils.removeAllOccurrences(a, true));

        a = new boolean[0];
        assertArrayEquals(ArrayUtils.EMPTY_BOOLEAN_ARRAY, ArrayUtils.removeAllOccurrences(a, true));

        a = new boolean[] { true };
        assertArrayEquals(ArrayUtils.EMPTY_BOOLEAN_ARRAY, ArrayUtils.removeAllOccurrences(a, true));

        a = new boolean[] { true, true };
        assertArrayEquals(ArrayUtils.EMPTY_BOOLEAN_ARRAY, ArrayUtils.removeAllOccurrences(a, true));

        a = new boolean[] { false, true, true, false, true };
        assertArrayEquals(new boolean[]{false, false}, ArrayUtils.removeAllOccurrences(a, true));

        a = new boolean[] { false, true, true, false, true };
        assertArrayEquals(new boolean[]{true, true, true}, ArrayUtils.removeAllOccurrences(a, false));
    }

    @Test
    void testRemoveAllByteOccurences() {
        byte[] a = null;
        assertNull(ArrayUtils.removeAllOccurences(a, (byte) 2));

        a = new byte[0];
        assertArrayEquals(ArrayUtils.EMPTY_BYTE_ARRAY, ArrayUtils.removeAllOccurences(a, (byte) 2));

        a = new byte[] { 2 };
        assertArrayEquals(ArrayUtils.EMPTY_BYTE_ARRAY, ArrayUtils.removeAllOccurences(a, (byte) 2));

        a = new byte[] { 2, 2 };
        assertArrayEquals(ArrayUtils.EMPTY_BYTE_ARRAY, ArrayUtils.removeAllOccurences(a, (byte) 2));

        a = new byte[] { 1, 2, 2, 3, 2 };
        assertArrayEquals(new byte[]{1, 3}, ArrayUtils.removeAllOccurences(a, (byte) 2));

        a = new byte[] { 1, 2, 2, 3, 2 };
        assertArrayEquals(new byte[]{1, 2, 2, 3, 2}, ArrayUtils.removeAllOccurences(a, (byte) 4));
    }

    @Test
    void testRemoveAllByteOccurrences() {
        byte[] a = null;
        assertNull(ArrayUtils.removeAllOccurrences(a, (byte) 2));

        a = new byte[0];
        assertArrayEquals(ArrayUtils.EMPTY_BYTE_ARRAY, ArrayUtils.removeAllOccurrences(a, (byte) 2));

        a = new byte[] { 2 };
        assertArrayEquals(ArrayUtils.EMPTY_BYTE_ARRAY, ArrayUtils.removeAllOccurrences(a, (byte) 2));

        a = new byte[] { 2, 2 };
        assertArrayEquals(ArrayUtils.EMPTY_BYTE_ARRAY, ArrayUtils.removeAllOccurrences(a, (byte) 2));

        a = new byte[] { 1, 2, 2, 3, 2 };
        assertArrayEquals(new byte[]{1, 3}, ArrayUtils.removeAllOccurrences(a, (byte) 2));

        a = new byte[] { 1, 2, 2, 3, 2 };
        assertArrayEquals(new byte[]{1, 2, 2, 3, 2}, ArrayUtils.removeAllOccurrences(a, (byte) 4));
    }

    @Test
    void testRemoveAllCharOccurences() {
        char[] a = null;
        assertNull(ArrayUtils.removeAllOccurences(a, '2'));

        a = new char[0];
        assertArrayEquals(ArrayUtils.EMPTY_CHAR_ARRAY, ArrayUtils.removeAllOccurences(a, '2'));

        a = new char[] { '2' };
        assertArrayEquals(ArrayUtils.EMPTY_CHAR_ARRAY, ArrayUtils.removeAllOccurences(a, '2'));

        a = new char[] { '2', '2' };
        assertArrayEquals(ArrayUtils.EMPTY_CHAR_ARRAY, ArrayUtils.removeAllOccurences(a, '2'));

        a = new char[] { '1', '2', '2', '3', '2' };
        assertArrayEquals(new char[]{'1', '3'}, ArrayUtils.removeAllOccurences(a, '2'));

        a = new char[] { '1', '2', '2', '3', '2' };
        assertArrayEquals(new char[]{'1', '2', '2', '3', '2'}, ArrayUtils.removeAllOccurences(a, '4'));
    }

    @Test
    void testRemoveAllCharOccurrences() {
        char[] a = null;
        assertNull(ArrayUtils.removeAllOccurrences(a, '2'));

        a = new char[0];
        assertArrayEquals(ArrayUtils.EMPTY_CHAR_ARRAY, ArrayUtils.removeAllOccurrences(a, '2'));

        a = new char[] { '2' };
        assertArrayEquals(ArrayUtils.EMPTY_CHAR_ARRAY, ArrayUtils.removeAllOccurrences(a, '2'));

        a = new char[] { '2', '2' };
        assertArrayEquals(ArrayUtils.EMPTY_CHAR_ARRAY, ArrayUtils.removeAllOccurrences(a, '2'));

        a = new char[] { '1', '2', '2', '3', '2' };
        assertArrayEquals(new char[]{'1', '3'}, ArrayUtils.removeAllOccurrences(a, '2'));

        a = new char[] { '1', '2', '2', '3', '2' };
        assertArrayEquals(new char[]{'1', '2', '2', '3', '2'}, ArrayUtils.removeAllOccurrences(a, '4'));
    }

    @Test
    void testRemoveAllDoubleOccurences() {
        double[] a = null;
        assertNull(ArrayUtils.removeAllOccurences(a, 2));

        a = new double[0];
        assertArrayEquals(ArrayUtils.EMPTY_DOUBLE_ARRAY, ArrayUtils.removeAllOccurences(a, 2));

        a = new double[] { 2 };
        assertArrayEquals(ArrayUtils.EMPTY_DOUBLE_ARRAY, ArrayUtils.removeAllOccurences(a, 2));

        a = new double[] { 2, 2 };
        assertArrayEquals(ArrayUtils.EMPTY_DOUBLE_ARRAY, ArrayUtils.removeAllOccurences(a, 2));

        a = new double[] { 1, 2, 2, 3, 2 };
        assertArrayEquals(new double[]{1, 3}, ArrayUtils.removeAllOccurences(a, 2));

        a = new double[] { 1, 2, 2, 3, 2 };
        assertArrayEquals(new double[]{1, 2, 2, 3, 2}, ArrayUtils.removeAllOccurences(a, 4));
    }

    @Test
    void testRemoveAllDoubleOccurrences() {
        double[] a = null;
        assertNull(ArrayUtils.removeAllOccurrences(a, 2));

        a = new double[0];
        assertArrayEquals(ArrayUtils.EMPTY_DOUBLE_ARRAY, ArrayUtils.removeAllOccurrences(a, 2));

        a = new double[] { 2 };
        assertArrayEquals(ArrayUtils.EMPTY_DOUBLE_ARRAY, ArrayUtils.removeAllOccurrences(a, 2));

        a = new double[] { 2, 2 };
        assertArrayEquals(ArrayUtils.EMPTY_DOUBLE_ARRAY, ArrayUtils.removeAllOccurrences(a, 2));

        a = new double[] { 1, 2, 2, 3, 2 };
        assertArrayEquals(new double[]{1, 3}, ArrayUtils.removeAllOccurrences(a, 2));

        a = new double[] { 1, 2, 2, 3, 2 };
        assertArrayEquals(new double[]{1, 2, 2, 3, 2}, ArrayUtils.removeAllOccurrences(a, 4));
    }

    @Test
    void testRemoveAllFloatOccurences() {
        float[] a = null;
        assertNull(ArrayUtils.removeAllOccurences(a, 2));

        a = new float[0];
        assertArrayEquals(ArrayUtils.EMPTY_FLOAT_ARRAY, ArrayUtils.removeAllOccurences(a, 2));

        a = new float[] { 2 };
        assertArrayEquals(ArrayUtils.EMPTY_FLOAT_ARRAY, ArrayUtils.removeAllOccurences(a, 2));

        a = new float[] { 2, 2 };
        assertArrayEquals(ArrayUtils.EMPTY_FLOAT_ARRAY, ArrayUtils.removeAllOccurences(a, 2));

        a = new float[] { 1, 2, 2, 3, 2 };
        assertArrayEquals(new float[]{1, 3}, ArrayUtils.removeAllOccurences(a, 2));

        a = new float[] { 1, 2, 2, 3, 2 };
        assertArrayEquals(new float[]{1, 2, 2, 3, 2}, ArrayUtils.removeAllOccurences(a, 4));
    }

    @Test
    void testRemoveAllFloatOccurrences() {
        float[] a = null;
        assertNull(ArrayUtils.removeAllOccurrences(a, 2));

        a = new float[0];
        assertArrayEquals(ArrayUtils.EMPTY_FLOAT_ARRAY, ArrayUtils.removeAllOccurrences(a, 2));

        a = new float[] { 2 };
        assertArrayEquals(ArrayUtils.EMPTY_FLOAT_ARRAY, ArrayUtils.removeAllOccurrences(a, 2));

        a = new float[] { 2, 2 };
        assertArrayEquals(ArrayUtils.EMPTY_FLOAT_ARRAY, ArrayUtils.removeAllOccurrences(a, 2));

        a = new float[] { 1, 2, 2, 3, 2 };
        assertArrayEquals(new float[]{1, 3}, ArrayUtils.removeAllOccurrences(a, 2));

        a = new float[] { 1, 2, 2, 3, 2 };
        assertArrayEquals(new float[]{1, 2, 2, 3, 2}, ArrayUtils.removeAllOccurrences(a, 4));
    }

    @Test
    void testRemoveAllIntOccurences() {
        int[] a = null;
        assertNull(ArrayUtils.removeAllOccurences(a, 2));

        a = new int[0];
        assertArrayEquals(ArrayUtils.EMPTY_INT_ARRAY, ArrayUtils.removeAllOccurences(a, 2));

        a = new int[] { 2 };
        assertArrayEquals(ArrayUtils.EMPTY_INT_ARRAY, ArrayUtils.removeAllOccurences(a, 2));

        a = new int[] { 2, 2 };
        assertArrayEquals(ArrayUtils.EMPTY_INT_ARRAY, ArrayUtils.removeAllOccurences(a, 2));

        a = new int[] { 1, 2, 2, 3, 2 };
        assertArrayEquals(new int[]{1, 3}, ArrayUtils.removeAllOccurences(a, 2));

        a = new int[] { 1, 2, 2, 3, 2 };
        assertArrayEquals(new int[]{1, 2, 2, 3, 2}, ArrayUtils.removeAllOccurences(a, 4));
    }

    @Test
    void testRemoveAllIntOccurrences() {
        int[] a = null;
        assertNull(ArrayUtils.removeAllOccurrences(a, 2));

        a = new int[0];
        assertArrayEquals(ArrayUtils.EMPTY_INT_ARRAY, ArrayUtils.removeAllOccurrences(a, 2));

        a = new int[] { 2 };
        assertArrayEquals(ArrayUtils.EMPTY_INT_ARRAY, ArrayUtils.removeAllOccurrences(a, 2));

        a = new int[] { 2, 2 };
        assertArrayEquals(ArrayUtils.EMPTY_INT_ARRAY, ArrayUtils.removeAllOccurrences(a, 2));

        a = new int[] { 1, 2, 2, 3, 2 };
        assertArrayEquals(new int[]{1, 3}, ArrayUtils.removeAllOccurrences(a, 2));

        a = new int[] { 1, 2, 2, 3, 2 };
        assertArrayEquals(new int[]{1, 2, 2, 3, 2}, ArrayUtils.removeAllOccurrences(a, 4));
    }

    @Test
    void testRemoveAllLongOccurences() {
        long[] a = null;
        assertNull(ArrayUtils.removeAllOccurences(a, 2));

        a = new long[0];
        assertArrayEquals(ArrayUtils.EMPTY_LONG_ARRAY, ArrayUtils.removeAllOccurences(a, 2));

        a = new long[] { 2 };
        assertArrayEquals(ArrayUtils.EMPTY_LONG_ARRAY, ArrayUtils.removeAllOccurences(a, 2));

        a = new long[] { 2, 2 };
        assertArrayEquals(ArrayUtils.EMPTY_LONG_ARRAY, ArrayUtils.removeAllOccurences(a, 2));

        a = new long[] { 1, 2, 2, 3, 2 };
        assertArrayEquals(new long[]{1, 3}, ArrayUtils.removeAllOccurences(a, 2));

        a = new long[] { 1, 2, 2, 3, 2 };
        assertArrayEquals(new long[]{1, 2, 2, 3, 2}, ArrayUtils.removeAllOccurences(a, 4));
    }

    @Test
    void testRemoveAllLongOccurrences() {
        long[] a = null;
        assertNull(ArrayUtils.removeAllOccurrences(a, 2));

        a = new long[0];
        assertArrayEquals(ArrayUtils.EMPTY_LONG_ARRAY, ArrayUtils.removeAllOccurrences(a, 2));

        a = new long[] { 2 };
        assertArrayEquals(ArrayUtils.EMPTY_LONG_ARRAY, ArrayUtils.removeAllOccurrences(a, 2));

        a = new long[] { 2, 2 };
        assertArrayEquals(ArrayUtils.EMPTY_LONG_ARRAY, ArrayUtils.removeAllOccurrences(a, 2));

        a = new long[] { 1, 2, 2, 3, 2 };
        assertArrayEquals(new long[]{1, 3}, ArrayUtils.removeAllOccurrences(a, 2));

        a = new long[] { 1, 2, 2, 3, 2 };
        assertArrayEquals(new long[]{1, 2, 2, 3, 2}, ArrayUtils.removeAllOccurrences(a, 4));
    }

    @Test
    void testRemoveAllObjectOccurences() {
        String[] a = null;
        assertNull(ArrayUtils.removeAllOccurences(a, "2"));

        a = new String[0];
        assertArrayEquals(ArrayUtils.EMPTY_STRING_ARRAY, ArrayUtils.removeAllOccurences(a, "2"));

        a = new String[] { "2" };
        assertArrayEquals(ArrayUtils.EMPTY_STRING_ARRAY, ArrayUtils.removeAllOccurences(a, "2"));

        a = new String[] { "2", "2" };
        assertArrayEquals(ArrayUtils.EMPTY_STRING_ARRAY, ArrayUtils.removeAllOccurences(a, "2"));

        a = new String[] { "1", "2", "2", "3", "2" };
        assertArrayEquals(new String[]{"1", "3"}, ArrayUtils.removeAllOccurences(a, "2"));

        a = new String[] { "1", "2", "2", "3", "2" };
        assertArrayEquals(new String[]{"1", "2", "2", "3", "2"}, ArrayUtils.removeAllOccurences(a, "4"));
    }

    @Test
    void testRemoveAllObjectOccurrences() {
        String[] a = null;
        assertNull(ArrayUtils.removeAllOccurrences(a, "2"));

        a = new String[0];
        assertArrayEquals(ArrayUtils.EMPTY_STRING_ARRAY, ArrayUtils.removeAllOccurrences(a, "2"));

        a = new String[] { "2" };
        assertArrayEquals(ArrayUtils.EMPTY_STRING_ARRAY, ArrayUtils.removeAllOccurrences(a, "2"));

        a = new String[] { "2", "2" };
        assertArrayEquals(ArrayUtils.EMPTY_STRING_ARRAY, ArrayUtils.removeAllOccurrences(a, "2"));

        a = new String[] { "1", "2", "2", "3", "2" };
        assertArrayEquals(new String[]{"1", "3"}, ArrayUtils.removeAllOccurrences(a, "2"));

        a = new String[] { "1", "2", "2", "3", "2" };
        assertArrayEquals(new String[]{"1", "2", "2", "3", "2"}, ArrayUtils.removeAllOccurrences(a, "4"));
    }

    @Test
    void testRemoveAllShortOccurences() {
        short[] a = null;
        assertNull(ArrayUtils.removeAllOccurences(a, (short) 2));

        a = new short[0];
        assertArrayEquals(ArrayUtils.EMPTY_SHORT_ARRAY, ArrayUtils.removeAllOccurences(a, (short) 2));

        a = new short[] { 2 };
        assertArrayEquals(ArrayUtils.EMPTY_SHORT_ARRAY, ArrayUtils.removeAllOccurences(a, (short) 2));

        a = new short[] { 2, 2 };
        assertArrayEquals(ArrayUtils.EMPTY_SHORT_ARRAY, ArrayUtils.removeAllOccurences(a, (short) 2));

        a = new short[] { 1, 2, 2, 3, 2 };
        assertArrayEquals(new short[]{1, 3}, ArrayUtils.removeAllOccurences(a, (short) 2));

        a = new short[] { 1, 2, 2, 3, 2 };
        assertArrayEquals(new short[]{1, 2, 2, 3, 2}, ArrayUtils.removeAllOccurences(a, (short) 4));
    }

    @Test
    void testRemoveAllShortOccurrences() {
        short[] a = null;
        assertNull(ArrayUtils.removeAllOccurrences(a, (short) 2));

        a = new short[0];
        assertArrayEquals(ArrayUtils.EMPTY_SHORT_ARRAY, ArrayUtils.removeAllOccurrences(a, (short) 2));

        a = new short[] { 2 };
        assertArrayEquals(ArrayUtils.EMPTY_SHORT_ARRAY, ArrayUtils.removeAllOccurrences(a, (short) 2));

        a = new short[] { 2, 2 };
        assertArrayEquals(ArrayUtils.EMPTY_SHORT_ARRAY, ArrayUtils.removeAllOccurrences(a, (short) 2));

        a = new short[] { 1, 2, 2, 3, 2 };
        assertArrayEquals(new short[]{1, 3}, ArrayUtils.removeAllOccurrences(a, (short) 2));

        a = new short[] { 1, 2, 2, 3, 2 };
        assertArrayEquals(new short[]{1, 2, 2, 3, 2}, ArrayUtils.removeAllOccurrences(a, (short) 4));
    }

    @Test
    void testRemoveBooleanArray() {
        boolean[] array;
        array = ArrayUtils.remove(new boolean[] {true}, 0);
        assertArrayEquals(ArrayUtils.EMPTY_BOOLEAN_ARRAY, array);
        assertEquals(Boolean.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.remove(new boolean[] {true, false}, 0);
        assertArrayEquals(new boolean[]{false}, array);
        assertEquals(Boolean.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.remove(new boolean[] {true, false}, 1);
        assertArrayEquals(new boolean[]{true}, array);
        assertEquals(Boolean.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.remove(new boolean[] {true, false, true}, 1);
        assertArrayEquals(new boolean[]{true, true}, array);
        assertEquals(Boolean.TYPE, array.getClass().getComponentType());
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.remove(new boolean[] {true, false}, -1));
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.remove(new boolean[] {true, false}, 2));
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.remove((boolean[]) null, 0));
    }

    @Test
    void testRemoveByteArray() {
        byte[] array;
        array = ArrayUtils.remove(new byte[] {1}, 0);
        assertArrayEquals(ArrayUtils.EMPTY_BYTE_ARRAY, array);
        assertEquals(Byte.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.remove(new byte[] {1, 2}, 0);
        assertArrayEquals(new byte[]{2}, array);
        assertEquals(Byte.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.remove(new byte[] {1, 2}, 1);
        assertArrayEquals(new byte[]{1}, array);
        assertEquals(Byte.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.remove(new byte[] {1, 2, 1}, 1);
        assertArrayEquals(new byte[]{1, 1}, array);
        assertEquals(Byte.TYPE, array.getClass().getComponentType());
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.remove(new byte[] {1, 2}, -1));
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.remove(new byte[] {1, 2}, 2));
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.remove((byte[]) null, 0));
    }

    @Test
    void testRemoveCharArray() {
        char[] array;
        array = ArrayUtils.remove(new char[] {'a'}, 0);
        assertArrayEquals(ArrayUtils.EMPTY_CHAR_ARRAY, array);
        assertEquals(Character.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.remove(new char[] {'a', 'b'}, 0);
        assertArrayEquals(new char[]{'b'}, array);
        assertEquals(Character.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.remove(new char[] {'a', 'b'}, 1);
        assertArrayEquals(new char[]{'a'}, array);
        assertEquals(Character.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.remove(new char[] {'a', 'b', 'c'}, 1);
        assertArrayEquals(new char[]{'a', 'c'}, array);
        assertEquals(Character.TYPE, array.getClass().getComponentType());
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.remove(new char[] {'a', 'b'}, -1));
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.remove(new char[] {'a', 'b'}, 2));
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.remove((char[]) null, 0));
    }

    @Test
    void testRemoveDoubleArray() {
        double[] array;
        array = ArrayUtils.remove(new double[] {1}, 0);
        assertArrayEquals(ArrayUtils.EMPTY_DOUBLE_ARRAY, array);
        assertEquals(Double.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.remove(new double[] {1, 2}, 0);
        assertArrayEquals(new double[]{2}, array);
        assertEquals(Double.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.remove(new double[] {1, 2}, 1);
        assertArrayEquals(new double[]{1}, array);
        assertEquals(Double.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.remove(new double[] {1, 2, 1}, 1);
        assertArrayEquals(new double[]{1, 1}, array);
        assertEquals(Double.TYPE, array.getClass().getComponentType());
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.remove(new double[] {1, 2}, -1));
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.remove(new double[] {1, 2}, 2));
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.remove((double[]) null, 0));
    }

    @Test
    void testRemoveElementBooleanArray() {
        boolean[] array;
        array = ArrayUtils.removeElement(null, true);
        assertNull(array);
        array = ArrayUtils.removeElement(ArrayUtils.EMPTY_BOOLEAN_ARRAY, true);
        assertArrayEquals(ArrayUtils.EMPTY_BOOLEAN_ARRAY, array);
        assertEquals(Boolean.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.removeElement(new boolean[] {true}, true);
        assertArrayEquals(ArrayUtils.EMPTY_BOOLEAN_ARRAY, array);
        assertEquals(Boolean.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.removeElement(new boolean[] {true, false}, true);
        assertArrayEquals(new boolean[]{false}, array);
        assertEquals(Boolean.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.removeElement(new boolean[] {true, false, true}, true);
        assertArrayEquals(new boolean[]{false, true}, array);
        assertEquals(Boolean.TYPE, array.getClass().getComponentType());
    }

    @Test
    void testRemoveElementByteArray() {
        byte[] array;
        array = ArrayUtils.removeElement((byte[]) null, (byte) 1);
        assertNull(array);
        array = ArrayUtils.removeElement(ArrayUtils.EMPTY_BYTE_ARRAY, (byte) 1);
        assertArrayEquals(ArrayUtils.EMPTY_BYTE_ARRAY, array);
        assertEquals(Byte.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.removeElement(new byte[] {1}, (byte) 1);
        assertArrayEquals(ArrayUtils.EMPTY_BYTE_ARRAY, array);
        assertEquals(Byte.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.removeElement(new byte[] {1, 2}, (byte) 1);
        assertArrayEquals(new byte[]{2}, array);
        assertEquals(Byte.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.removeElement(new byte[] {1, 2, 1}, (byte) 1);
        assertArrayEquals(new byte[]{2, 1}, array);
        assertEquals(Byte.TYPE, array.getClass().getComponentType());
    }

    @Test
    void testRemoveElementCharArray() {
        char[] array;
        array = ArrayUtils.removeElement((char[]) null, 'a');
        assertNull(array);
        array = ArrayUtils.removeElement(ArrayUtils.EMPTY_CHAR_ARRAY, 'a');
        assertArrayEquals(ArrayUtils.EMPTY_CHAR_ARRAY, array);
        assertEquals(Character.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.removeElement(new char[] {'a'}, 'a');
        assertArrayEquals(ArrayUtils.EMPTY_CHAR_ARRAY, array);
        assertEquals(Character.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.removeElement(new char[] {'a', 'b'}, 'a');
        assertArrayEquals(new char[]{'b'}, array);
        assertEquals(Character.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.removeElement(new char[] {'a', 'b', 'a'}, 'a');
        assertArrayEquals(new char[]{'b', 'a'}, array);
        assertEquals(Character.TYPE, array.getClass().getComponentType());
    }

    @Test
    void testRemoveElementDoubleArray() {
        double[] array;
        array = ArrayUtils.removeElement(null, (double) 1);
        assertNull(array);
        array = ArrayUtils.removeElement(ArrayUtils.EMPTY_DOUBLE_ARRAY, 1);
        assertArrayEquals(ArrayUtils.EMPTY_DOUBLE_ARRAY, array);
        assertEquals(Double.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.removeElement(new double[] {1}, 1);
        assertArrayEquals(ArrayUtils.EMPTY_DOUBLE_ARRAY, array);
        assertEquals(Double.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.removeElement(new double[] {1, 2}, 1);
        assertArrayEquals(new double[]{2}, array);
        assertEquals(Double.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.removeElement(new double[] {1, 2, 1}, 1);
        assertArrayEquals(new double[]{2, 1}, array);
        assertEquals(Double.TYPE, array.getClass().getComponentType());
    }

    @Test
    void testRemoveElementFloatArray() {
        float[] array;
        array = ArrayUtils.removeElement((float[]) null, 1);
        assertNull(array);
        array = ArrayUtils.removeElement(ArrayUtils.EMPTY_FLOAT_ARRAY, 1);
        assertArrayEquals(ArrayUtils.EMPTY_FLOAT_ARRAY, array);
        assertEquals(Float.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.removeElement(new float[] {1}, 1);
        assertArrayEquals(ArrayUtils.EMPTY_FLOAT_ARRAY, array);
        assertEquals(Float.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.removeElement(new float[] {1, 2}, 1);
        assertArrayEquals(new float[]{2}, array);
        assertEquals(Float.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.removeElement(new float[] {1, 2, 1}, 1);
        assertArrayEquals(new float[]{2, 1}, array);
        assertEquals(Float.TYPE, array.getClass().getComponentType());
    }

    @Test
    void testRemoveElementIntArray() {
        int[] array;
        array = ArrayUtils.removeElement((int[]) null, 1);
        assertNull(array);
        array = ArrayUtils.removeElement(ArrayUtils.EMPTY_INT_ARRAY, 1);
        assertArrayEquals(ArrayUtils.EMPTY_INT_ARRAY, array);
        assertEquals(Integer.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.removeElement(new int[] {1}, 1);
        assertArrayEquals(ArrayUtils.EMPTY_INT_ARRAY, array);
        assertEquals(Integer.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.removeElement(new int[] {1, 2}, 1);
        assertArrayEquals(new int[]{2}, array);
        assertEquals(Integer.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.removeElement(new int[] {1, 2, 1}, 1);
        assertArrayEquals(new int[]{2, 1}, array);
        assertEquals(Integer.TYPE, array.getClass().getComponentType());
    }

    @Test
    void testRemoveElementLongArray() {
        long[] array;
        array = ArrayUtils.removeElement((long[]) null, 1L);
        assertNull(array);
        array = ArrayUtils.removeElement(ArrayUtils.EMPTY_LONG_ARRAY, 1L);
        assertArrayEquals(ArrayUtils.EMPTY_LONG_ARRAY, array);
        assertEquals(Long.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.removeElement(new long[] {1}, 1L);
        assertArrayEquals(ArrayUtils.EMPTY_LONG_ARRAY, array);
        assertEquals(Long.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.removeElement(new long[] {1, 2}, 1L);
        assertArrayEquals(new long[]{2}, array);
        assertEquals(Long.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.removeElement(new long[] {1, 2, 1}, 1L);
        assertArrayEquals(new long[]{2, 1}, array);
        assertEquals(Long.TYPE, array.getClass().getComponentType());
    }

    @Test
    void testRemoveElementObjectArray() {
        Object[] array;
        array = ArrayUtils.removeElement(null, "a");
        assertNull(array);
        array = ArrayUtils.removeElement(ArrayUtils.EMPTY_OBJECT_ARRAY, "a");
        assertArrayEquals(ArrayUtils.EMPTY_OBJECT_ARRAY, array);
        assertEquals(Object.class, array.getClass().getComponentType());
        array = ArrayUtils.removeElement(new Object[] {"a"}, "a");
        assertArrayEquals(ArrayUtils.EMPTY_OBJECT_ARRAY, array);
        assertEquals(Object.class, array.getClass().getComponentType());
        array = ArrayUtils.removeElement(new Object[] {"a", "b"}, "a");
        assertArrayEquals(new Object[]{"b"}, array);
        assertEquals(Object.class, array.getClass().getComponentType());
        array = ArrayUtils.removeElement(new Object[] {"a", "b", "a"}, "a");
        assertArrayEquals(new Object[]{"b", "a"}, array);
        assertEquals(Object.class, array.getClass().getComponentType());
    }

    @Test
    void testRemoveElementShortArray() {
        short[] array;
        array = ArrayUtils.removeElement((short[]) null, (short) 1);
        assertNull(array);
        array = ArrayUtils.removeElement(ArrayUtils.EMPTY_SHORT_ARRAY, (short) 1);
        assertArrayEquals(ArrayUtils.EMPTY_SHORT_ARRAY, array);
        assertEquals(Short.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.removeElement(new short[] {1}, (short) 1);
        assertArrayEquals(ArrayUtils.EMPTY_SHORT_ARRAY, array);
        assertEquals(Short.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.removeElement(new short[] {1, 2}, (short) 1);
        assertArrayEquals(new short[]{2}, array);
        assertEquals(Short.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.removeElement(new short[] {1, 2, 1}, (short) 1);
        assertArrayEquals(new short[]{2, 1}, array);
        assertEquals(Short.TYPE, array.getClass().getComponentType());
    }

    @Test
    void testRemoveFloatArray() {
        float[] array;
        array = ArrayUtils.remove(new float[] {1}, 0);
        assertArrayEquals(ArrayUtils.EMPTY_FLOAT_ARRAY, array);
        assertEquals(Float.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.remove(new float[] {1, 2}, 0);
        assertArrayEquals(new float[]{2}, array);
        assertEquals(Float.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.remove(new float[] {1, 2}, 1);
        assertArrayEquals(new float[]{1}, array);
        assertEquals(Float.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.remove(new float[] {1, 2, 1}, 1);
        assertArrayEquals(new float[]{1, 1}, array);
        assertEquals(Float.TYPE, array.getClass().getComponentType());
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.remove(new float[] {1, 2}, -1));
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.remove(new float[] {1, 2}, 2));
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.remove((float[]) null, 0));
    }

    @Test
    void testRemoveIntArray() {
        int[] array;
        array = ArrayUtils.remove(new int[] {1}, 0);
        assertArrayEquals(ArrayUtils.EMPTY_INT_ARRAY, array);
        assertEquals(Integer.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.remove(new int[] {1, 2}, 0);
        assertArrayEquals(new int[]{2}, array);
        assertEquals(Integer.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.remove(new int[] {1, 2}, 1);
        assertArrayEquals(new int[]{1}, array);
        assertEquals(Integer.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.remove(new int[] {1, 2, 1}, 1);
        assertArrayEquals(new int[]{1, 1}, array);
        assertEquals(Integer.TYPE, array.getClass().getComponentType());
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.remove(new int[] {1, 2}, -1));
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.remove(new int[] {1, 2}, 2));
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.remove((int[]) null, 0));
    }

    @Test
    void testRemoveLongArray() {
        long[] array;
        array = ArrayUtils.remove(new long[] {1}, 0);
        assertArrayEquals(ArrayUtils.EMPTY_LONG_ARRAY, array);
        assertEquals(Long.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.remove(new long[] {1, 2}, 0);
        assertArrayEquals(new long[]{2}, array);
        assertEquals(Long.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.remove(new long[] {1, 2}, 1);
        assertArrayEquals(new long[]{1}, array);
        assertEquals(Long.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.remove(new long[] {1, 2, 1}, 1);
        assertArrayEquals(new long[]{1, 1}, array);
        assertEquals(Long.TYPE, array.getClass().getComponentType());
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.remove(new long[] {1, 2}, -1));
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.remove(new long[] {1, 2}, 2));
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.remove((long[]) null, 0));
    }

    @Test
    void testRemoveNumberArray() {
        final Number[] inarray = {Integer.valueOf(1), Long.valueOf(2), Byte.valueOf((byte) 3)};
        assertEquals(3, inarray.length);
        Number[] outarray;
        outarray = ArrayUtils.remove(inarray, 1);
        assertEquals(2, outarray.length);
        assertEquals(Number.class, outarray.getClass().getComponentType());
        outarray = ArrayUtils.remove(outarray, 1);
        assertEquals(1, outarray.length);
        assertEquals(Number.class, outarray.getClass().getComponentType());
        outarray = ArrayUtils.remove(outarray, 0);
        assertEquals(0, outarray.length);
        assertEquals(Number.class, outarray.getClass().getComponentType());
    }

    @Test
    void testRemoveObjectArray() {
        Object[] array;
        array = ArrayUtils.remove(new Object[] {"a"}, 0);
        assertArrayEquals(ArrayUtils.EMPTY_OBJECT_ARRAY, array);
        assertEquals(Object.class, array.getClass().getComponentType());
        array = ArrayUtils.remove(new Object[] {"a", "b"}, 0);
        assertArrayEquals(new Object[]{"b"}, array);
        assertEquals(Object.class, array.getClass().getComponentType());
        array = ArrayUtils.remove(new Object[] {"a", "b"}, 1);
        assertArrayEquals(new Object[]{"a"}, array);
        assertEquals(Object.class, array.getClass().getComponentType());
        array = ArrayUtils.remove(new Object[] {"a", "b", "c"}, 1);
        assertArrayEquals(new Object[]{"a", "c"}, array);
        assertEquals(Object.class, array.getClass().getComponentType());
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.remove(new Object[] {"a", "b"}, -1));
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.remove(new Object[] {"a", "b"}, 2));
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.remove((Object[]) null, 0));
    }

    @Test
    void testRemoveShortArray() {
        short[] array;
        array = ArrayUtils.remove(new short[] {1}, 0);
        assertArrayEquals(ArrayUtils.EMPTY_SHORT_ARRAY, array);
        assertEquals(Short.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.remove(new short[] {1, 2}, 0);
        assertArrayEquals(new short[]{2}, array);
        assertEquals(Short.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.remove(new short[] {1, 2}, 1);
        assertArrayEquals(new short[]{1}, array);
        assertEquals(Short.TYPE, array.getClass().getComponentType());
        array = ArrayUtils.remove(new short[] {1, 2, 1}, 1);
        assertArrayEquals(new short[]{1, 1}, array);
        assertEquals(Short.TYPE, array.getClass().getComponentType());
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.remove(new short[] {1, 2}, -1));
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.remove(new short[] {1, 2}, 2));
        assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.remove((short[]) null, 0));
    }
}
