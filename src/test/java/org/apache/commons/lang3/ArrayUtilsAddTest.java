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
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;

/**
 * Tests ArrayUtils add methods.
 */
public class ArrayUtilsAddTest extends AbstractLangTest {

    @Test
    public void testAddFirstBoolean() {
        boolean[] newArray;
        newArray = ArrayUtils.addFirst(null, false);
        assertArrayEquals(new boolean[]{false}, newArray);
        assertEquals(Boolean.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.addFirst(null, true);
        assertArrayEquals(new boolean[]{true}, newArray);
        assertEquals(Boolean.TYPE, newArray.getClass().getComponentType());
        final boolean[] array1 = {true, false, true};
        newArray = ArrayUtils.addFirst(array1, false);
        assertArrayEquals(new boolean[]{false, true, false, true}, newArray);
        assertEquals(Boolean.TYPE, newArray.getClass().getComponentType());
    }

    @Test
    public void testAddFirstByte() {
        byte[] newArray;
        newArray = ArrayUtils.addFirst((byte[]) null, (byte) 0);
        assertArrayEquals(new byte[]{0}, newArray);
        assertEquals(Byte.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.addFirst((byte[]) null, (byte) 1);
        assertArrayEquals(new byte[]{1}, newArray);
        assertEquals(Byte.TYPE, newArray.getClass().getComponentType());
        final byte[] array1 = {1, 2, 3};
        newArray = ArrayUtils.addFirst(array1, (byte) 0);
        assertArrayEquals(new byte[]{0, 1, 2, 3}, newArray);
        assertEquals(Byte.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.addFirst(array1, (byte) 4);
        assertArrayEquals(new byte[]{4, 1, 2, 3}, newArray);
        assertEquals(Byte.TYPE, newArray.getClass().getComponentType());
    }

    @Test
    public void testAddFirstChar() {
        char[] newArray;
        newArray = ArrayUtils.addFirst((char[]) null, (char) 0);
        assertArrayEquals(new char[]{0}, newArray);
        assertEquals(Character.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.addFirst((char[]) null, (char) 1);
        assertArrayEquals(new char[]{1}, newArray);
        assertEquals(Character.TYPE, newArray.getClass().getComponentType());
        final char[] array1 = {1, 2, 3};
        newArray = ArrayUtils.addFirst(array1, (char) 0);
        assertArrayEquals(new char[]{0, 1, 2, 3}, newArray);
        assertEquals(Character.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.addFirst(array1, (char) 4);
        assertArrayEquals(new char[]{4, 1, 2, 3}, newArray);
        assertEquals(Character.TYPE, newArray.getClass().getComponentType());
    }

    @Test
    public void testAddFirstDouble() {
        double[] newArray;
        newArray = ArrayUtils.addFirst((double[]) null, 0);
        assertArrayEquals(new double[]{0}, newArray);
        assertEquals(Double.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.addFirst((double[]) null, 1);
        assertArrayEquals(new double[]{1}, newArray);
        assertEquals(Double.TYPE, newArray.getClass().getComponentType());
        final double[] array1 = {1, 2, 3};
        newArray = ArrayUtils.addFirst(array1, 0);
        assertArrayEquals(new double[]{0, 1, 2, 3}, newArray);
        assertEquals(Double.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.addFirst(array1, 4);
        assertArrayEquals(new double[]{4, 1, 2, 3}, newArray);
        assertEquals(Double.TYPE, newArray.getClass().getComponentType());
    }

    @Test
    public void testAddFirstFloat() {
        float[] newArray;
        newArray = ArrayUtils.addFirst((float[]) null, 0);
        assertArrayEquals(new float[]{0}, newArray);
        assertEquals(Float.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.addFirst((float[]) null, 1);
        assertArrayEquals(new float[]{1}, newArray);
        assertEquals(Float.TYPE, newArray.getClass().getComponentType());
        final float[] array1 = {1, 2, 3};
        newArray = ArrayUtils.addFirst(array1, 0);
        assertArrayEquals(new float[]{0, 1, 2, 3}, newArray);
        assertEquals(Float.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.addFirst(array1, 4);
        assertArrayEquals(new float[]{4, 1, 2, 3}, newArray);
        assertEquals(Float.TYPE, newArray.getClass().getComponentType());
    }

    @Test
    public void testAddFirstInt() {
        int[] newArray;
        newArray = ArrayUtils.addFirst((int[]) null, 0);
        assertArrayEquals(new int[]{0}, newArray);
        assertEquals(Integer.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.addFirst((int[]) null, 1);
        assertArrayEquals(new int[]{1}, newArray);
        assertEquals(Integer.TYPE, newArray.getClass().getComponentType());
        final int[] array1 = {1, 2, 3};
        newArray = ArrayUtils.addFirst(array1, 0);
        assertArrayEquals(new int[]{0, 1, 2, 3}, newArray);
        assertEquals(Integer.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.addFirst(array1, 4);
        assertArrayEquals(new int[]{4, 1, 2, 3}, newArray);
        assertEquals(Integer.TYPE, newArray.getClass().getComponentType());
    }

    @Test
    public void testAddFirstLong() {
        long[] newArray;
        newArray = ArrayUtils.addFirst((long[]) null, 0);
        assertArrayEquals(new long[]{0}, newArray);
        assertEquals(Long.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.addFirst((long[]) null, 1);
        assertArrayEquals(new long[]{1}, newArray);
        assertEquals(Long.TYPE, newArray.getClass().getComponentType());
        final long[] array1 = {1, 2, 3};
        newArray = ArrayUtils.addFirst(array1, 0);
        assertArrayEquals(new long[]{0, 1, 2, 3}, newArray);
        assertEquals(Long.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.addFirst(array1, 4);
        assertArrayEquals(new long[]{4, 1, 2, 3}, newArray);
        assertEquals(Long.TYPE, newArray.getClass().getComponentType());
    }

    @Test
    public void testAddFirstObject() {
        Object[] newArray;

        //show that not casting is okay
        newArray = ArrayUtils.add((Object[]) null, "a");
        assertArrayEquals(new String[]{"a"}, newArray);
        assertArrayEquals(new Object[]{"a"}, newArray);
        assertEquals(String.class, newArray.getClass().getComponentType());

        //show that not casting to Object[] is okay and will assume String based on "a"
        final String[] newStringArray = ArrayUtils.add(null, "a");
        assertArrayEquals(new String[]{"a"}, newStringArray);
        assertArrayEquals(new Object[]{"a"}, newStringArray);
        assertEquals(String.class, newStringArray.getClass().getComponentType());

        final String[] stringArray1 = { "a", "b", "c" };
        newArray = ArrayUtils.addFirst(stringArray1, null);
        assertArrayEquals(new String[] { null, "a", "b", "c" }, newArray);
        assertEquals(String.class, newArray.getClass().getComponentType());

        newArray = ArrayUtils.addFirst(stringArray1, "d");
        assertArrayEquals(new String[] { "d", "a", "b", "c" }, newArray);
        assertEquals(String.class, newArray.getClass().getComponentType());

        Number[] numberArray1 = { Integer.valueOf(1), Double.valueOf(2) };
        newArray = ArrayUtils.addFirst(numberArray1, Float.valueOf(3));
        assertArrayEquals(new Number[] { Float.valueOf(3), Integer.valueOf(1), Double.valueOf(2) }, newArray);
        assertEquals(Number.class, newArray.getClass().getComponentType());

        numberArray1 = null;
        newArray = ArrayUtils.addFirst(numberArray1, Float.valueOf(3));
        assertArrayEquals(new Float[] { Float.valueOf(3) }, newArray);
        assertEquals(Float.class, newArray.getClass().getComponentType());
    }

    @Test
    public void testAddFirstShort() {
        short[] newArray;
        newArray = ArrayUtils.addFirst((short[]) null, (short) 0);
        assertArrayEquals(new short[]{0}, newArray);
        assertEquals(Short.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.addFirst((short[]) null, (short) 1);
        assertArrayEquals(new short[]{1}, newArray);
        assertEquals(Short.TYPE, newArray.getClass().getComponentType());
        final short[] array1 = {1, 2, 3};
        newArray = ArrayUtils.addFirst(array1, (short) 0);
        assertArrayEquals(new short[]{0, 1, 2, 3}, newArray);
        assertEquals(Short.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.addFirst(array1, (short) 4);
        assertArrayEquals(new short[]{4, 1, 2, 3}, newArray);
        assertEquals(Short.TYPE, newArray.getClass().getComponentType());
    }

    @Test
    public void testAddObjectArrayBoolean() {
        boolean[] newArray;
        newArray = ArrayUtils.add(null, false);
        assertArrayEquals(new boolean[]{false}, newArray);
        assertEquals(Boolean.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add(null, true);
        assertArrayEquals(new boolean[]{true}, newArray);
        assertEquals(Boolean.TYPE, newArray.getClass().getComponentType());
        final boolean[] array1 = {true, false, true};
        newArray = ArrayUtils.add(array1, false);
        assertArrayEquals(new boolean[]{true, false, true, false}, newArray);
        assertEquals(Boolean.TYPE, newArray.getClass().getComponentType());
    }

    @Test
    public void testAddObjectArrayByte() {
        byte[] newArray;
        newArray = ArrayUtils.add((byte[]) null, (byte) 0);
        assertArrayEquals(new byte[]{0}, newArray);
        assertEquals(Byte.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add((byte[]) null, (byte) 1);
        assertArrayEquals(new byte[]{1}, newArray);
        assertEquals(Byte.TYPE, newArray.getClass().getComponentType());
        final byte[] array1 = {1, 2, 3};
        newArray = ArrayUtils.add(array1, (byte) 0);
        assertArrayEquals(new byte[]{1, 2, 3, 0}, newArray);
        assertEquals(Byte.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add(array1, (byte) 4);
        assertArrayEquals(new byte[]{1, 2, 3, 4}, newArray);
        assertEquals(Byte.TYPE, newArray.getClass().getComponentType());
    }

    @Test
    public void testAddObjectArrayChar() {
        char[] newArray;
        newArray = ArrayUtils.add((char[]) null, (char) 0);
        assertArrayEquals(new char[]{0}, newArray);
        assertEquals(Character.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add((char[]) null, (char) 1);
        assertArrayEquals(new char[]{1}, newArray);
        assertEquals(Character.TYPE, newArray.getClass().getComponentType());
        final char[] array1 = {1, 2, 3};
        newArray = ArrayUtils.add(array1, (char) 0);
        assertArrayEquals(new char[]{1, 2, 3, 0}, newArray);
        assertEquals(Character.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add(array1, (char) 4);
        assertArrayEquals(new char[]{1, 2, 3, 4}, newArray);
        assertEquals(Character.TYPE, newArray.getClass().getComponentType());
    }

    @Test
    public void testAddObjectArrayDouble() {
        double[] newArray;
        newArray = ArrayUtils.add((double[]) null, 0);
        assertArrayEquals(new double[]{0}, newArray);
        assertEquals(Double.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add((double[]) null, 1);
        assertArrayEquals(new double[]{1}, newArray);
        assertEquals(Double.TYPE, newArray.getClass().getComponentType());
        final double[] array1 = {1, 2, 3};
        newArray = ArrayUtils.add(array1, 0);
        assertArrayEquals(new double[]{1, 2, 3, 0}, newArray);
        assertEquals(Double.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add(array1, 4);
        assertArrayEquals(new double[]{1, 2, 3, 4}, newArray);
        assertEquals(Double.TYPE, newArray.getClass().getComponentType());
    }

    @Test
    public void testAddObjectArrayFloat() {
        float[] newArray;
        newArray = ArrayUtils.add((float[]) null, 0);
        assertArrayEquals(new float[]{0}, newArray);
        assertEquals(Float.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add((float[]) null, 1);
        assertArrayEquals(new float[]{1}, newArray);
        assertEquals(Float.TYPE, newArray.getClass().getComponentType());
        final float[] array1 = {1, 2, 3};
        newArray = ArrayUtils.add(array1, 0);
        assertArrayEquals(new float[]{1, 2, 3, 0}, newArray);
        assertEquals(Float.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add(array1, 4);
        assertArrayEquals(new float[]{1, 2, 3, 4}, newArray);
        assertEquals(Float.TYPE, newArray.getClass().getComponentType());
    }

    @Test
    public void testAddObjectArrayInt() {
        int[] newArray;
        newArray = ArrayUtils.add((int[]) null, 0);
        assertArrayEquals(new int[]{0}, newArray);
        assertEquals(Integer.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add((int[]) null, 1);
        assertArrayEquals(new int[]{1}, newArray);
        assertEquals(Integer.TYPE, newArray.getClass().getComponentType());
        final int[] array1 = {1, 2, 3};
        newArray = ArrayUtils.add(array1, 0);
        assertArrayEquals(new int[]{1, 2, 3, 0}, newArray);
        assertEquals(Integer.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add(array1, 4);
        assertArrayEquals(new int[]{1, 2, 3, 4}, newArray);
        assertEquals(Integer.TYPE, newArray.getClass().getComponentType());
    }

    @Test
    public void testAddObjectArrayLong() {
        long[] newArray;
        newArray = ArrayUtils.add((long[]) null, 0);
        assertArrayEquals(new long[]{0}, newArray);
        assertEquals(Long.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add((long[]) null, 1);
        assertArrayEquals(new long[]{1}, newArray);
        assertEquals(Long.TYPE, newArray.getClass().getComponentType());
        final long[] array1 = {1, 2, 3};
        newArray = ArrayUtils.add(array1, 0);
        assertArrayEquals(new long[]{1, 2, 3, 0}, newArray);
        assertEquals(Long.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add(array1, 4);
        assertArrayEquals(new long[]{1, 2, 3, 4}, newArray);
        assertEquals(Long.TYPE, newArray.getClass().getComponentType());
    }

    @Test
    public void testAddObjectArrayObject() {
        Object[] newArray;

        //show that not casting is okay
        newArray = ArrayUtils.add((Object[]) null, "a");
        assertArrayEquals(new String[]{"a"}, newArray);
        assertArrayEquals(new Object[]{"a"}, newArray);
        assertEquals(String.class, newArray.getClass().getComponentType());

        //show that not casting to Object[] is okay and will assume String based on "a"
        final String[] newStringArray = ArrayUtils.add(null, "a");
        assertArrayEquals(new String[]{"a"}, newStringArray);
        assertArrayEquals(new Object[]{"a"}, newStringArray);
        assertEquals(String.class, newStringArray.getClass().getComponentType());

        final String[] stringArray1 = {"a", "b", "c"};
        newArray = ArrayUtils.add(stringArray1, null);
        assertArrayEquals(new String[]{"a", "b", "c", null}, newArray);
        assertEquals(String.class, newArray.getClass().getComponentType());

        newArray = ArrayUtils.add(stringArray1, "d");
        assertArrayEquals(new String[]{"a", "b", "c", "d"}, newArray);
        assertEquals(String.class, newArray.getClass().getComponentType());

        Number[] numberArray1 = {Integer.valueOf(1), Double.valueOf(2)};
        newArray = ArrayUtils.add(numberArray1, Float.valueOf(3));
        assertArrayEquals(new Number[]{Integer.valueOf(1), Double.valueOf(2), Float.valueOf(3)}, newArray);
        assertEquals(Number.class, newArray.getClass().getComponentType());

        numberArray1 = null;
        newArray = ArrayUtils.add(numberArray1, Float.valueOf(3));
        assertArrayEquals(new Float[]{Float.valueOf(3)}, newArray);
        assertEquals(Float.class, newArray.getClass().getComponentType());
    }

    @Test
    public void testAddObjectArrayShort() {
        short[] newArray;
        newArray = ArrayUtils.add((short[]) null, (short) 0);
        assertArrayEquals(new short[]{0}, newArray);
        assertEquals(Short.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add((short[]) null, (short) 1);
        assertArrayEquals(new short[]{1}, newArray);
        assertEquals(Short.TYPE, newArray.getClass().getComponentType());
        final short[] array1 = {1, 2, 3};
        newArray = ArrayUtils.add(array1, (short) 0);
        assertArrayEquals(new short[]{1, 2, 3, 0}, newArray);
        assertEquals(Short.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add(array1, (short) 4);
        assertArrayEquals(new short[]{1, 2, 3, 4}, newArray);
        assertEquals(Short.TYPE, newArray.getClass().getComponentType());
    }

    @Test
    public void testAddObjectArrayToObjectArray() {
        assertNull(ArrayUtils.addAll(null, (Object[]) null));
        Object[] newArray;
        final String[] stringArray1 = {"a", "b", "c"};
        final String[] stringArray2 = {"1", "2", "3"};
        newArray = ArrayUtils.addAll(stringArray1, (String[]) null);
        assertNotSame(stringArray1, newArray);
        assertArrayEquals(stringArray1, newArray);
        assertArrayEquals(new String[]{"a", "b", "c"}, newArray);
        assertEquals(String.class, newArray.getClass().getComponentType());
        newArray = ArrayUtils.addAll(null, stringArray2);
        assertNotSame(stringArray2, newArray);
        assertArrayEquals(stringArray2, newArray);
        assertArrayEquals(new String[]{"1", "2", "3"}, newArray);
        assertEquals(String.class, newArray.getClass().getComponentType());
        newArray = ArrayUtils.addAll(stringArray1, stringArray2);
        assertArrayEquals(new String[]{"a", "b", "c", "1", "2", "3"}, newArray);
        assertEquals(String.class, newArray.getClass().getComponentType());
        newArray = ArrayUtils.addAll(ArrayUtils.EMPTY_STRING_ARRAY, (String[]) null);
        assertArrayEquals(ArrayUtils.EMPTY_STRING_ARRAY, newArray);
        assertArrayEquals(new String[]{}, newArray);
        assertEquals(String.class, newArray.getClass().getComponentType());
        newArray = ArrayUtils.addAll(null, ArrayUtils.EMPTY_STRING_ARRAY);
        assertArrayEquals(ArrayUtils.EMPTY_STRING_ARRAY, newArray);
        assertArrayEquals(new String[]{}, newArray);
        assertEquals(String.class, newArray.getClass().getComponentType());
        newArray = ArrayUtils.addAll(ArrayUtils.EMPTY_STRING_ARRAY, ArrayUtils.EMPTY_STRING_ARRAY);
        assertArrayEquals(ArrayUtils.EMPTY_STRING_ARRAY, newArray);
        assertArrayEquals(new String[]{}, newArray);
        assertEquals(String.class, newArray.getClass().getComponentType());
        final String[] stringArrayNull = {null};
        newArray = ArrayUtils.addAll(stringArrayNull, stringArrayNull);
        assertArrayEquals(new String[]{null, null}, newArray);
        assertEquals(String.class, newArray.getClass().getComponentType());

        // boolean
        assertArrayEquals(new boolean[]{true, false, false, true}, ArrayUtils.addAll(new boolean[]{true, false}, false, true));

        assertArrayEquals(new boolean[]{false, true}, ArrayUtils.addAll(null, new boolean[]{false, true}));

        assertArrayEquals(new boolean[]{true, false}, ArrayUtils.addAll(new boolean[]{true, false}, null));

        // char
        assertArrayEquals(new char[]{'a', 'b', 'c', 'd'}, ArrayUtils.addAll(new char[]{'a', 'b'}, 'c', 'd'));

        assertArrayEquals(new char[]{'c', 'd'}, ArrayUtils.addAll(null, new char[]{'c', 'd'}));

        assertArrayEquals(new char[]{'a', 'b'}, ArrayUtils.addAll(new char[]{'a', 'b'}, null));

        // byte
        assertArrayEquals(new byte[]{(byte) 0, (byte) 1, (byte) 2, (byte) 3}, ArrayUtils.addAll(new byte[]{(byte) 0, (byte) 1}, (byte) 2, (byte) 3));

        assertArrayEquals(new byte[]{(byte) 2, (byte) 3}, ArrayUtils.addAll(null, new byte[]{(byte) 2, (byte) 3}));

        assertArrayEquals(new byte[]{(byte) 0, (byte) 1}, ArrayUtils.addAll(new byte[]{(byte) 0, (byte) 1}, null));

        // short
        assertArrayEquals(new short[]{(short) 10, (short) 20, (short) 30, (short) 40}, ArrayUtils.addAll(new short[]{(short) 10, (short) 20}, (short) 30, (short) 40));

        assertArrayEquals(new short[]{(short) 30, (short) 40}, ArrayUtils.addAll(null, new short[]{(short) 30, (short) 40}));

        assertArrayEquals(new short[]{(short) 10, (short) 20}, ArrayUtils.addAll(new short[]{(short) 10, (short) 20}, null));

        // int
        assertArrayEquals(new int[]{1, 1000, -1000, -1}, ArrayUtils.addAll(new int[]{1, 1000}, -1000, -1));

        assertArrayEquals(new int[]{-1000, -1}, ArrayUtils.addAll(null, new int[]{-1000, -1}));

        assertArrayEquals(new int[]{1, 1000}, ArrayUtils.addAll(new int[]{1, 1000}, null));

        // long
        assertArrayEquals(new long[]{1L, -1L, 1000L, -1000L}, ArrayUtils.addAll(new long[]{1L, -1L}, 1000L, -1000L));

        assertArrayEquals(new long[]{1000L, -1000L}, ArrayUtils.addAll(null, new long[]{1000L, -1000L}));

        assertArrayEquals(new long[]{1L, -1L}, ArrayUtils.addAll(new long[]{1L, -1L}, null));

        // float
        assertArrayEquals(new float[]{10.5f, 10.1f, 1.6f, 0.01f}, ArrayUtils.addAll(new float[]{10.5f, 10.1f}, 1.6f, 0.01f));

        assertArrayEquals(new float[]{1.6f, 0.01f}, ArrayUtils.addAll(null, new float[]{1.6f, 0.01f}));

        assertArrayEquals(new float[]{10.5f, 10.1f}, ArrayUtils.addAll(new float[]{10.5f, 10.1f}, null));

        // double
        assertArrayEquals(new double[]{Math.PI, -Math.PI, 0, 9.99}, ArrayUtils.addAll(new double[]{Math.PI, -Math.PI}, 0, 9.99));

        assertArrayEquals(new double[]{0, 9.99}, ArrayUtils.addAll(null, new double[]{0, 9.99}));

        assertArrayEquals(new double[]{Math.PI, -Math.PI}, ArrayUtils.addAll(new double[]{Math.PI, -Math.PI}, null));

    }

    @SuppressWarnings("deprecation")
    @Test
    public void testAddObjectAtIndex() {
        Object[] newArray;
        newArray = ArrayUtils.add((Object[]) null, 0, "a");
        assertArrayEquals(new String[]{"a"}, newArray);
        assertArrayEquals(new Object[]{"a"}, newArray);
        assertEquals(String.class, newArray.getClass().getComponentType());
        final String[] stringArray1 = {"a", "b", "c"};
        newArray = ArrayUtils.add(stringArray1, 0, null);
        assertArrayEquals(new String[]{null, "a", "b", "c"}, newArray);
        assertEquals(String.class, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add(stringArray1, 1, null);
        assertArrayEquals(new String[]{"a", null, "b", "c"}, newArray);
        assertEquals(String.class, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add(stringArray1, 3, null);
        assertArrayEquals(new String[]{"a", "b", "c", null}, newArray);
        assertEquals(String.class, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add(stringArray1, 3, "d");
        assertArrayEquals(new String[]{"a", "b", "c", "d"}, newArray);
        assertEquals(String.class, newArray.getClass().getComponentType());
        assertEquals(String.class, newArray.getClass().getComponentType());

        final Object[] o = {"1", "2", "4"};
        final Object[] result = ArrayUtils.add(o, 2, "3");
        final Object[] result2 = ArrayUtils.add(o, 3, "5");

        assertNotNull(result);
        assertEquals(4, result.length);
        assertEquals("1", result[0]);
        assertEquals("2", result[1]);
        assertEquals("3", result[2]);
        assertEquals("4", result[3]);
        assertNotNull(result2);
        assertEquals(4, result2.length);
        assertEquals("1", result2[0]);
        assertEquals("2", result2[1]);
        assertEquals("4", result2[2]);
        assertEquals("5", result2[3]);

        // boolean tests
        boolean[] booleanArray = ArrayUtils.add( null, 0, true );
        assertArrayEquals(new boolean[]{true}, booleanArray);
        IndexOutOfBoundsException e =
                assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.add( null, -1, true));
        assertEquals("Index: -1, Length: 0", e.getMessage());
        booleanArray = ArrayUtils.add( new boolean[] { true }, 0, false);
        assertArrayEquals(new boolean[]{false, true}, booleanArray);
        booleanArray = ArrayUtils.add( new boolean[] { false }, 1, true);
        assertArrayEquals(new boolean[]{false, true}, booleanArray);
        booleanArray = ArrayUtils.add( new boolean[] { true, false }, 1, true);
        assertArrayEquals(new boolean[]{true, true, false}, booleanArray);
        e = assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.add(new boolean[] { true, false }, 4, true));
        assertEquals("Index: 4, Length: 2", e.getMessage());
        e = assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.add(new boolean[] { true, false }, -1, true));
        assertEquals("Index: -1, Length: 2", e.getMessage());

        // char tests
        char[] charArray = ArrayUtils.add( (char[]) null, 0, 'a' );
        assertArrayEquals(new char[]{'a'}, charArray);
        e = assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.add( (char[]) null, -1, 'a' ));
        assertEquals("Index: -1, Length: 0", e.getMessage());
        charArray = ArrayUtils.add( new char[] { 'a' }, 0, 'b');
        assertArrayEquals(new char[]{'b', 'a'}, charArray);
        charArray = ArrayUtils.add( new char[] { 'a', 'b' }, 0, 'c');
        assertArrayEquals(new char[]{'c', 'a', 'b'}, charArray);
        charArray = ArrayUtils.add( new char[] { 'a', 'b' }, 1, 'k');
        assertArrayEquals(new char[]{'a', 'k', 'b'}, charArray);
        charArray = ArrayUtils.add( new char[] { 'a', 'b', 'c' }, 1, 't');
        assertArrayEquals(new char[]{'a', 't', 'b', 'c'}, charArray);
        e = assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.add( new char[] { 'a', 'b' }, 4, 'c'));
        assertEquals("Index: 4, Length: 2", e.getMessage());
        e = assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.add( new char[] { 'a', 'b' }, -1, 'c'));
        assertEquals("Index: -1, Length: 2", e.getMessage());

        // short tests
        short[] shortArray = ArrayUtils.add( new short[] { 1 }, 0, (short) 2);
        assertArrayEquals(new short[]{2, 1}, shortArray);
        e = assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.add( (short[]) null, -1, (short) 2));
        assertEquals("Index: -1, Length: 0", e.getMessage());
        shortArray = ArrayUtils.add( new short[] { 2, 6 }, 2, (short) 10);
        assertArrayEquals(new short[]{2, 6, 10}, shortArray);
        shortArray = ArrayUtils.add( new short[] { 2, 6 }, 0, (short) -4);
        assertArrayEquals(new short[]{-4, 2, 6}, shortArray);
        shortArray = ArrayUtils.add( new short[] { 2, 6, 3 }, 2, (short) 1);
        assertArrayEquals(new short[]{2, 6, 1, 3}, shortArray);
        e = assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.add( new short[] { 2, 6 }, 4, (short) 10));
        assertEquals("Index: 4, Length: 2", e.getMessage());
        e = assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.add( new short[] { 2, 6 }, -1, (short) 10));
        assertEquals("Index: -1, Length: 2", e.getMessage());

        // byte tests
        byte[] byteArray = ArrayUtils.add( new byte[] { 1 }, 0, (byte) 2);
        assertArrayEquals(new byte[]{2, 1}, byteArray);
        e = assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.add( (byte[]) null, -1, (byte) 2));
        assertEquals("Index: -1, Length: 0", e.getMessage());
        byteArray = ArrayUtils.add( new byte[] { 2, 6 }, 2, (byte) 3);
        assertArrayEquals(new byte[]{2, 6, 3}, byteArray);
        byteArray = ArrayUtils.add( new byte[] { 2, 6 }, 0, (byte) 1);
        assertArrayEquals(new byte[]{1, 2, 6}, byteArray);
        byteArray = ArrayUtils.add( new byte[] { 2, 6, 3 }, 2, (byte) 1);
        assertArrayEquals(new byte[]{2, 6, 1, 3}, byteArray);
        e = assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.add( new byte[] { 2, 6 }, 4, (byte) 3));
        assertEquals("Index: 4, Length: 2", e.getMessage());
        e = assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.add( new byte[] { 2, 6 }, -1, (byte) 3));
        assertEquals("Index: -1, Length: 2", e.getMessage());

        // int tests
        int[] intArray = ArrayUtils.add( new int[] { 1 }, 0, 2);
        assertArrayEquals(new int[]{2, 1}, intArray);
        e = assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.add( (int[]) null, -1, 2));
        assertEquals("Index: -1, Length: 0", e.getMessage());
        intArray = ArrayUtils.add( new int[] { 2, 6 }, 2, 10);
        assertArrayEquals(new int[]{2, 6, 10}, intArray);
        intArray = ArrayUtils.add( new int[] { 2, 6 }, 0, -4);
        assertArrayEquals(new int[]{-4, 2, 6}, intArray);
        intArray = ArrayUtils.add( new int[] { 2, 6, 3 }, 2, 1);
        assertArrayEquals(new int[]{2, 6, 1, 3}, intArray);
        e = assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.add( new int[] { 2, 6 }, 4, 10));
        assertEquals("Index: 4, Length: 2", e.getMessage());
        e = assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.add( new int[] { 2, 6 }, -1, 10));
        assertEquals("Index: -1, Length: 2", e.getMessage());

        // long tests
        long[] longArray = ArrayUtils.add( new long[] { 1L }, 0, 2L);
        assertArrayEquals(new long[]{2L, 1L}, longArray);
        e = assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.add( (long[]) null, -1, 2L));
        assertEquals("Index: -1, Length: 0", e.getMessage());
        longArray = ArrayUtils.add( new long[] { 2L, 6L }, 2, 10L);
        assertArrayEquals(new long[]{2L, 6L, 10L}, longArray);
        longArray = ArrayUtils.add( new long[] { 2L, 6L }, 0, -4L);
        assertArrayEquals(new long[]{-4L, 2L, 6L}, longArray);
        longArray = ArrayUtils.add( new long[] { 2L, 6L, 3L }, 2, 1L);
        assertArrayEquals(new long[]{2L, 6L, 1L, 3L}, longArray);
        e = assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.add( new long[] { 2L, 6L }, 4, 10L));
        assertEquals("Index: 4, Length: 2", e.getMessage());
        e = assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.add( new long[] { 2L, 6L }, -1, 10L));
        assertEquals("Index: -1, Length: 2", e.getMessage());

        // float tests
        float[] floatArray = ArrayUtils.add( new float[] { 1.1f }, 0, 2.2f);
        assertArrayEquals(new float[]{2.2f, 1.1f}, floatArray);
        e = assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.add( (float[]) null, -1, 2.2f));
        assertEquals("Index: -1, Length: 0", e.getMessage());
        floatArray = ArrayUtils.add( new float[] { 2.3f, 6.4f }, 2, 10.5f);
        assertArrayEquals(new float[]{2.3f, 6.4f, 10.5f}, floatArray);
        floatArray = ArrayUtils.add( new float[] { 2.6f, 6.7f }, 0, -4.8f);
        assertArrayEquals(new float[]{-4.8f, 2.6f, 6.7f}, floatArray);
        floatArray = ArrayUtils.add( new float[] { 2.9f, 6.0f, 0.3f }, 2, 1.0f);
        assertArrayEquals(new float[]{2.9f, 6.0f, 1.0f, 0.3f}, floatArray);
        e = assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.add( new float[] { 2.3f, 6.4f }, 4, 10.5f));
        assertEquals("Index: 4, Length: 2", e.getMessage());
        e = assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.add( new float[] { 2.3f, 6.4f }, -1, 10.5f));
        assertEquals("Index: -1, Length: 2", e.getMessage());

        // double tests
        double[] doubleArray = ArrayUtils.add( new double[] { 1.1 }, 0, 2.2);
        assertArrayEquals(new double[]{2.2, 1.1}, doubleArray);
        e = assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.add(null, -1, 2.2));
        assertEquals("Index: -1, Length: 0", e.getMessage());
        doubleArray = ArrayUtils.add( new double[] { 2.3, 6.4 }, 2, 10.5);
        assertArrayEquals(new double[]{2.3, 6.4, 10.5}, doubleArray);
        doubleArray = ArrayUtils.add( new double[] { 2.6, 6.7 }, 0, -4.8);
        assertArrayEquals(new double[]{-4.8, 2.6, 6.7}, doubleArray);
        doubleArray = ArrayUtils.add( new double[] { 2.9, 6.0, 0.3 }, 2, 1.0);
        assertArrayEquals(new double[]{2.9, 6.0, 1.0, 0.3}, doubleArray);
        e = assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.add( new double[] { 2.3, 6.4 }, 4, 10.5));
        assertEquals("Index: 4, Length: 2", e.getMessage());
        e = assertThrows(IndexOutOfBoundsException.class, () -> ArrayUtils.add( new double[] { 2.3, 6.4 }, -1, 10.5));
        assertEquals("Index: -1, Length: 2", e.getMessage());
    }

    @Test
    public void testJira567() {
        final Number[] n;
        // Valid array construction
        n = ArrayUtils.addAll(new Number[]{Integer.valueOf(1)}, new Long[]{Long.valueOf(2)});
        assertEquals(2, n.length);
        assertEquals(Number.class, n.getClass().getComponentType());
        // Invalid - can't store Long in Integer array
        assertThrows(IllegalArgumentException.class,
                () -> ArrayUtils.addAll(new Integer[]{Integer.valueOf(1)}, new Long[]{Long.valueOf(2)}));
    }

    @Test
    @SuppressWarnings("deprecation")
    public void testLANG571() {
        final String[] stringArray=null;
        final String aString=null;
        assertThrows(IllegalArgumentException.class, () -> ArrayUtils.add(stringArray, aString));
        assertThrows(IllegalArgumentException.class, () -> ArrayUtils.add(stringArray, 0, aString));
    }

}
