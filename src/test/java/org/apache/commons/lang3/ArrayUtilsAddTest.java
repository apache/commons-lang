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

import java.util.Arrays;

import junit.framework.TestCase;

/**
 * Tests ArrayUtils add methods.
 *
 * @author Gary D. Gregory
 * @version $Id$
 */
public class ArrayUtilsAddTest extends TestCase {

    public void testJira567(){
        Number[] n;
        // Valid array construction
        n = ArrayUtils.addAll(new Number[]{Integer.valueOf(1)}, new Long[]{Long.valueOf(2)});
        assertEquals(2,n.length);
        assertEquals(Number.class,n.getClass().getComponentType());
        try {
            // Invalid - can't store Long in Integer array
               n = ArrayUtils.addAll(new Integer[]{Integer.valueOf(1)}, new Long[]{Long.valueOf(2)});
               fail("Should have generated IllegalArgumentException");
        } catch (IllegalArgumentException expected) {
        }
    }

    public void testAddObjectArrayBoolean() {
        boolean[] newArray;
        newArray = ArrayUtils.add((boolean[])null, false);
        assertTrue(Arrays.equals(new boolean[]{false}, newArray));
        assertEquals(Boolean.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add((boolean[])null, true);
        assertTrue(Arrays.equals(new boolean[]{true}, newArray));
        assertEquals(Boolean.TYPE, newArray.getClass().getComponentType());
        boolean[] array1 = new boolean[]{true, false, true};
        newArray = ArrayUtils.add(array1, false);
        assertTrue(Arrays.equals(new boolean[]{true, false, true, false}, newArray));
        assertEquals(Boolean.TYPE, newArray.getClass().getComponentType());
    }

    public void testAddObjectArrayByte() {
        byte[] newArray;
        newArray = ArrayUtils.add((byte[])null, (byte)0);
        assertTrue(Arrays.equals(new byte[]{0}, newArray));
        assertEquals(Byte.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add((byte[])null, (byte)1);
        assertTrue(Arrays.equals(new byte[]{1}, newArray));
        assertEquals(Byte.TYPE, newArray.getClass().getComponentType());
        byte[] array1 = new byte[]{1, 2, 3};
        newArray = ArrayUtils.add(array1, (byte)0);
        assertTrue(Arrays.equals(new byte[]{1, 2, 3, 0}, newArray));
        assertEquals(Byte.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add(array1, (byte)4);
        assertTrue(Arrays.equals(new byte[]{1, 2, 3, 4}, newArray));
        assertEquals(Byte.TYPE, newArray.getClass().getComponentType());
    }

    public void testAddObjectArrayChar() {
        char[] newArray;
        newArray = ArrayUtils.add((char[])null, (char)0);
        assertTrue(Arrays.equals(new char[]{0}, newArray));
        assertEquals(Character.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add((char[])null, (char)1);
        assertTrue(Arrays.equals(new char[]{1}, newArray));
        assertEquals(Character.TYPE, newArray.getClass().getComponentType());
        char[] array1 = new char[]{1, 2, 3};
        newArray = ArrayUtils.add(array1, (char)0);
        assertTrue(Arrays.equals(new char[]{1, 2, 3, 0}, newArray));
        assertEquals(Character.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add(array1, (char)4);
        assertTrue(Arrays.equals(new char[]{1, 2, 3, 4}, newArray));
        assertEquals(Character.TYPE, newArray.getClass().getComponentType());
    }

    public void testAddObjectArrayDouble() {
        double[] newArray;
        newArray = ArrayUtils.add((double[])null, 0);
        assertTrue(Arrays.equals(new double[]{0}, newArray));
        assertEquals(Double.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add((double[])null, 1);
        assertTrue(Arrays.equals(new double[]{1}, newArray));
        assertEquals(Double.TYPE, newArray.getClass().getComponentType());
        double[] array1 = new double[]{1, 2, 3};
        newArray = ArrayUtils.add(array1, 0);
        assertTrue(Arrays.equals(new double[]{1, 2, 3, 0}, newArray));
        assertEquals(Double.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add(array1, 4);
        assertTrue(Arrays.equals(new double[]{1, 2, 3, 4}, newArray));
        assertEquals(Double.TYPE, newArray.getClass().getComponentType());
    }

    public void testAddObjectArrayFloat() {
        float[] newArray;
        newArray = ArrayUtils.add((float[])null, 0);
        assertTrue(Arrays.equals(new float[]{0}, newArray));
        assertEquals(Float.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add((float[])null, 1);
        assertTrue(Arrays.equals(new float[]{1}, newArray));
        assertEquals(Float.TYPE, newArray.getClass().getComponentType());
        float[] array1 = new float[]{1, 2, 3};
        newArray = ArrayUtils.add(array1, 0);
        assertTrue(Arrays.equals(new float[]{1, 2, 3, 0}, newArray));
        assertEquals(Float.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add(array1, 4);
        assertTrue(Arrays.equals(new float[]{1, 2, 3, 4}, newArray));
        assertEquals(Float.TYPE, newArray.getClass().getComponentType());
    }

    public void testAddObjectArrayInt() {
        int[] newArray;
        newArray = ArrayUtils.add((int[])null, 0);
        assertTrue(Arrays.equals(new int[]{0}, newArray));
        assertEquals(Integer.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add((int[])null, 1);
        assertTrue(Arrays.equals(new int[]{1}, newArray));
        assertEquals(Integer.TYPE, newArray.getClass().getComponentType());
        int[] array1 = new int[]{1, 2, 3};
        newArray = ArrayUtils.add(array1, 0);
        assertTrue(Arrays.equals(new int[]{1, 2, 3, 0}, newArray));
        assertEquals(Integer.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add(array1, 4);
        assertTrue(Arrays.equals(new int[]{1, 2, 3, 4}, newArray));
        assertEquals(Integer.TYPE, newArray.getClass().getComponentType());
    }

    public void testAddObjectArrayLong() {
        long[] newArray;
        newArray = ArrayUtils.add((long[])null, 0);
        assertTrue(Arrays.equals(new long[]{0}, newArray));
        assertEquals(Long.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add((long[])null, 1);
        assertTrue(Arrays.equals(new long[]{1}, newArray));
        assertEquals(Long.TYPE, newArray.getClass().getComponentType());
        long[] array1 = new long[]{1, 2, 3};
        newArray = ArrayUtils.add(array1, 0);
        assertTrue(Arrays.equals(new long[]{1, 2, 3, 0}, newArray));
        assertEquals(Long.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add(array1, 4);
        assertTrue(Arrays.equals(new long[]{1, 2, 3, 4}, newArray));
        assertEquals(Long.TYPE, newArray.getClass().getComponentType());
    }

    public void testAddObjectArrayShort() {
        short[] newArray;
        newArray = ArrayUtils.add((short[])null, (short)0);
        assertTrue(Arrays.equals(new short[]{0}, newArray));
        assertEquals(Short.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add((short[])null, (short)1);
        assertTrue(Arrays.equals(new short[]{1}, newArray));
        assertEquals(Short.TYPE, newArray.getClass().getComponentType());
        short[] array1 = new short[]{1, 2, 3};
        newArray = ArrayUtils.add(array1, (short)0);
        assertTrue(Arrays.equals(new short[]{1, 2, 3, 0}, newArray));
        assertEquals(Short.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add(array1, (short)4);
        assertTrue(Arrays.equals(new short[]{1, 2, 3, 4}, newArray));
        assertEquals(Short.TYPE, newArray.getClass().getComponentType());
    }

    public void testAddObjectArrayObject() {
        Object[] newArray;

        //show that not casting is okay
        newArray = ArrayUtils.add((Object[])null, "a");
        assertTrue(Arrays.equals((new String[]{"a"}), newArray));
        assertTrue(Arrays.equals((new Object[]{"a"}), newArray));
        assertEquals(String.class, newArray.getClass().getComponentType());

        //show that not casting to Object[] is okay and will assume String based on "a"
        String[] newStringArray = ArrayUtils.add(null, "a");
        assertTrue(Arrays.equals((new String[]{"a"}), newStringArray));
        assertTrue(Arrays.equals((new Object[]{"a"}), newStringArray));
        assertEquals(String.class, newStringArray.getClass().getComponentType());

        String[] stringArray1 = new String[]{"a", "b", "c"};
        newArray = ArrayUtils.add(stringArray1, null);
        assertTrue(Arrays.equals((new String[]{"a", "b", "c", null}), newArray));
        assertEquals(String.class, newArray.getClass().getComponentType());

        newArray = ArrayUtils.add(stringArray1, "d");
        assertTrue(Arrays.equals((new String[]{"a", "b", "c", "d"}), newArray));
        assertEquals(String.class, newArray.getClass().getComponentType());

        Number[] numberArray1 = new Number[]{new Integer(1), new Double(2)};
        newArray = ArrayUtils.add(numberArray1, new Float(3));
        assertTrue(Arrays.equals((new Number[]{new Integer(1), new Double(2), new Float(3)}), newArray));
        assertEquals(Number.class, newArray.getClass().getComponentType());

        numberArray1 = null;
        newArray = ArrayUtils.add(numberArray1, new Float(3));
        assertTrue(Arrays.equals((new Float[]{new Float(3)}), newArray));
        assertEquals(Float.class, newArray.getClass().getComponentType());
    }
    
    public void testLANG571(){
        String[] stringArray=null;
        String aString=null;
        try {
            @SuppressWarnings("unused")
            String[] sa = ArrayUtils.add(stringArray, aString);
            fail("Should have caused IllegalArgumentException");
        } catch (IllegalArgumentException iae){
            //expected
        }
        try {
            @SuppressWarnings("unused")
            String[] sa = ArrayUtils.add(stringArray, 0, aString);
            fail("Should have caused IllegalArgumentException");
        } catch (IllegalArgumentException iae){
            //expected
        }
    }

    public void testAddObjectArrayToObjectArray() {
        assertNull(ArrayUtils.addAll((Object[]) null, (Object[]) null));
        Object[] newArray;
        String[] stringArray1 = new String[]{"a", "b", "c"};
        String[] stringArray2 = new String[]{"1", "2", "3"};
        newArray = ArrayUtils.addAll(stringArray1, (String[]) null);
        assertNotSame(stringArray1, newArray);
        assertTrue(Arrays.equals(stringArray1, newArray));
        assertTrue(Arrays.equals((new String[]{"a", "b", "c"}), newArray));
        assertEquals(String.class, newArray.getClass().getComponentType());
        newArray = ArrayUtils.addAll(null, stringArray2);
        assertNotSame(stringArray2, newArray);
        assertTrue(Arrays.equals(stringArray2, newArray));
        assertTrue(Arrays.equals((new String[]{"1", "2", "3"}), newArray));
        assertEquals(String.class, newArray.getClass().getComponentType());
        newArray = ArrayUtils.addAll(stringArray1, stringArray2);
        assertTrue(Arrays.equals((new String[]{"a", "b", "c", "1", "2", "3"}), newArray));
        assertEquals(String.class, newArray.getClass().getComponentType());
        newArray = ArrayUtils.addAll(ArrayUtils.EMPTY_STRING_ARRAY, (String[]) null);
        assertTrue(Arrays.equals(ArrayUtils.EMPTY_STRING_ARRAY, newArray));
        assertTrue(Arrays.equals((new String[]{}), newArray));
        assertEquals(String.class, newArray.getClass().getComponentType());
        newArray = ArrayUtils.addAll(null, ArrayUtils.EMPTY_STRING_ARRAY);
        assertTrue(Arrays.equals(ArrayUtils.EMPTY_STRING_ARRAY, newArray));
        assertTrue(Arrays.equals((new String[]{}), newArray));
        assertEquals(String.class, newArray.getClass().getComponentType());
        newArray = ArrayUtils.addAll(ArrayUtils.EMPTY_STRING_ARRAY, ArrayUtils.EMPTY_STRING_ARRAY);
        assertTrue(Arrays.equals(ArrayUtils.EMPTY_STRING_ARRAY, newArray));
        assertTrue(Arrays.equals((new String[]{}), newArray));
        assertEquals(String.class, newArray.getClass().getComponentType());
        String[] stringArrayNull = new String []{null};
        newArray = ArrayUtils.addAll(stringArrayNull, stringArrayNull);
        assertTrue(Arrays.equals((new String[]{null, null}), newArray));
        assertEquals(String.class, newArray.getClass().getComponentType());

        // boolean
        assertTrue( Arrays.equals( new boolean[] { true, false, false, true },
            ArrayUtils.addAll( new boolean[] { true, false }, new boolean[] { false, true } ) ) );

        assertTrue( Arrays.equals( new boolean[] { false, true },
            ArrayUtils.addAll( null, new boolean[] { false, true } ) ) );

        assertTrue( Arrays.equals( new boolean[] { true, false },
            ArrayUtils.addAll( new boolean[] { true, false }, null ) ) );

        // char
        assertTrue( Arrays.equals( new char[] { 'a', 'b', 'c', 'd' },
            ArrayUtils.addAll( new char[] { 'a', 'b' }, new char[] { 'c', 'd' } ) ) );

        assertTrue( Arrays.equals( new char[] { 'c', 'd' },
            ArrayUtils.addAll( null, new char[] { 'c', 'd' } ) ) );

        assertTrue( Arrays.equals( new char[] { 'a', 'b' },
            ArrayUtils.addAll( new char[] { 'a', 'b' }, null ) ) );

        // byte
        assertTrue( Arrays.equals( new byte[] { (byte) 0, (byte) 1, (byte) 2, (byte) 3 },
            ArrayUtils.addAll( new byte[] { (byte) 0, (byte) 1 }, new byte[] { (byte) 2, (byte) 3 } ) ) );

        assertTrue( Arrays.equals( new byte[] { (byte) 2, (byte) 3 },
            ArrayUtils.addAll( null, new byte[] { (byte) 2, (byte) 3 } ) ) );

        assertTrue( Arrays.equals( new byte[] { (byte) 0, (byte) 1 },
            ArrayUtils.addAll( new byte[] { (byte) 0, (byte) 1 }, null ) ) );

        // short
        assertTrue( Arrays.equals( new short[] { (short) 10, (short) 20, (short) 30, (short) 40 },
            ArrayUtils.addAll( new short[] { (short) 10, (short) 20 }, new short[] { (short) 30, (short) 40 } ) ) );

        assertTrue( Arrays.equals( new short[] { (short) 30, (short) 40 },
            ArrayUtils.addAll( null, new short[] { (short) 30, (short) 40 } ) ) );

        assertTrue( Arrays.equals( new short[] { (short) 10, (short) 20 },
            ArrayUtils.addAll( new short[] { (short) 10, (short) 20 }, null ) ) );

        // int
        assertTrue( Arrays.equals( new int[] { 1, 1000, -1000, -1 },
            ArrayUtils.addAll( new int[] { 1, 1000 }, new int[] { -1000, -1 } ) ) );

        assertTrue( Arrays.equals( new int[] { -1000, -1 },
            ArrayUtils.addAll( null, new int[] { -1000, -1 } ) ) );

        assertTrue( Arrays.equals( new int[] { 1, 1000 },
            ArrayUtils.addAll( new int[] { 1, 1000 }, null ) ) );

        // long
        assertTrue( Arrays.equals( new long[] { 1L, -1L, 1000L, -1000L },
            ArrayUtils.addAll( new long[] { 1L, -1L }, new long[] { 1000L, -1000L } ) ) );

        assertTrue( Arrays.equals( new long[] { 1000L, -1000L },
            ArrayUtils.addAll( null, new long[] { 1000L, -1000L } ) ) );

        assertTrue( Arrays.equals( new long[] { 1L, -1L },
            ArrayUtils.addAll( new long[] { 1L, -1L }, null ) ) );

        // float
        assertTrue( Arrays.equals( new float[] { 10.5f, 10.1f, 1.6f, 0.01f },
            ArrayUtils.addAll( new float[] { 10.5f, 10.1f }, new float[] { 1.6f, 0.01f } ) ) );

        assertTrue( Arrays.equals( new float[] { 1.6f, 0.01f },
            ArrayUtils.addAll( null, new float[] { 1.6f, 0.01f } ) ) );

        assertTrue( Arrays.equals( new float[] { 10.5f, 10.1f },
            ArrayUtils.addAll( new float[] { 10.5f, 10.1f }, null ) ) );

        // double
        assertTrue( Arrays.equals( new double[] { Math.PI, -Math.PI, 0, 9.99 },
            ArrayUtils.addAll( new double[] { Math.PI, -Math.PI }, new double[] { 0, 9.99 } ) ) );

        assertTrue( Arrays.equals( new double[] { 0, 9.99 },
            ArrayUtils.addAll( null, new double[] { 0, 9.99 } ) ) );

        assertTrue( Arrays.equals( new double[] { Math.PI, -Math.PI },
            ArrayUtils.addAll( new double[] { Math.PI, -Math.PI }, null ) ) );

    }

    public void testAddObjectAtIndex() {
        Object[] newArray;
        newArray = ArrayUtils.add((Object[])null, 0, "a");
        assertTrue(Arrays.equals((new String[]{"a"}), newArray));
        assertTrue(Arrays.equals((new Object[]{"a"}), newArray));
        assertEquals(String.class, newArray.getClass().getComponentType());
        String[] stringArray1 = new String[]{"a", "b", "c"};
        newArray = ArrayUtils.add(stringArray1, 0, null);
        assertTrue(Arrays.equals((new String[]{null, "a", "b", "c"}), newArray));
        assertEquals(String.class, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add(stringArray1, 1, null);
        assertTrue(Arrays.equals((new String[]{"a", null, "b", "c"}), newArray));
        assertEquals(String.class, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add(stringArray1, 3, null);
        assertTrue(Arrays.equals((new String[]{"a", "b", "c", null}), newArray));
        assertEquals(String.class, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add(stringArray1, 3, "d");
        assertTrue(Arrays.equals((new String[]{"a", "b", "c", "d"}), newArray));
        assertEquals(String.class, newArray.getClass().getComponentType());
        assertEquals(String.class, newArray.getClass().getComponentType());

        Object[] o = new Object[] {"1", "2", "4"};
        Object[] result = ArrayUtils.add(o, 2, "3");
        Object[] result2 = ArrayUtils.add(o, 3, "5");

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
        assertTrue( Arrays.equals( new boolean[] { true }, booleanArray ) );
        try {
            booleanArray = ArrayUtils.add( null, -1, true );
        } catch(IndexOutOfBoundsException e) {
            assertEquals("Index: -1, Length: 0", e.getMessage());
        }
        booleanArray = ArrayUtils.add( new boolean[] { true }, 0, false);
        assertTrue( Arrays.equals( new boolean[] { false, true }, booleanArray ) );
        booleanArray = ArrayUtils.add( new boolean[] { false }, 1, true);
        assertTrue( Arrays.equals( new boolean[] { false, true }, booleanArray ) );
        booleanArray = ArrayUtils.add( new boolean[] { true, false }, 1, true);
        assertTrue( Arrays.equals( new boolean[] { true, true, false }, booleanArray ) );
        try {
            booleanArray = ArrayUtils.add( new boolean[] { true, false }, 4, true);
        } catch(IndexOutOfBoundsException e) {
            assertEquals("Index: 4, Length: 2", e.getMessage());
        }
        try {
            booleanArray = ArrayUtils.add( new boolean[] { true, false }, -1, true);
        } catch(IndexOutOfBoundsException e) {
            assertEquals("Index: -1, Length: 2", e.getMessage());
        }

        // char tests
        char[] charArray = ArrayUtils.add( (char[]) null, 0, 'a' );
        assertTrue( Arrays.equals( new char[] { 'a' }, charArray ) );
        try {
            charArray = ArrayUtils.add( (char[]) null, -1, 'a' );
        } catch(IndexOutOfBoundsException e) {
            assertEquals("Index: -1, Length: 0", e.getMessage());
        }
        charArray = ArrayUtils.add( new char[] { 'a' }, 0, 'b');
        assertTrue( Arrays.equals( new char[] { 'b', 'a' }, charArray ) );
        charArray = ArrayUtils.add( new char[] { 'a', 'b' }, 0, 'c');
        assertTrue( Arrays.equals( new char[] { 'c', 'a', 'b' }, charArray ) );
        charArray = ArrayUtils.add( new char[] { 'a', 'b' }, 1, 'k');
        assertTrue( Arrays.equals( new char[] { 'a', 'k', 'b' }, charArray ) );
        charArray = ArrayUtils.add( new char[] { 'a', 'b', 'c' }, 1, 't');
        assertTrue( Arrays.equals( new char[] { 'a', 't', 'b', 'c' }, charArray ) );
        try {
            charArray = ArrayUtils.add( new char[] { 'a', 'b' }, 4, 'c');
        } catch(IndexOutOfBoundsException e) {
            assertEquals("Index: 4, Length: 2", e.getMessage());
        }
        try {
            charArray = ArrayUtils.add( new char[] { 'a', 'b' }, -1, 'c');
        } catch(IndexOutOfBoundsException e) {
            assertEquals("Index: -1, Length: 2", e.getMessage());
        }

        // short tests
        short[] shortArray = ArrayUtils.add( new short[] { 1 }, 0, (short) 2);
        assertTrue( Arrays.equals( new short[] { 2, 1 }, shortArray ) );
        try {
            shortArray = ArrayUtils.add( (short[]) null, -1, (short) 2);
        } catch(IndexOutOfBoundsException e) {
            assertEquals("Index: -1, Length: 0", e.getMessage());
        }
        shortArray = ArrayUtils.add( new short[] { 2, 6 }, 2, (short) 10);
        assertTrue( Arrays.equals( new short[] { 2, 6, 10 }, shortArray ) );
        shortArray = ArrayUtils.add( new short[] { 2, 6 }, 0, (short) -4);
        assertTrue( Arrays.equals( new short[] { -4, 2, 6 }, shortArray ) );
        shortArray = ArrayUtils.add( new short[] { 2, 6, 3 }, 2, (short) 1);
        assertTrue( Arrays.equals( new short[] { 2, 6, 1, 3 }, shortArray ) );
        try {
            shortArray = ArrayUtils.add( new short[] { 2, 6 }, 4, (short) 10);
        } catch(IndexOutOfBoundsException e) {
            assertEquals("Index: 4, Length: 2", e.getMessage());
        }
        try {
            shortArray = ArrayUtils.add( new short[] { 2, 6 }, -1, (short) 10);
        } catch(IndexOutOfBoundsException e) {
            assertEquals("Index: -1, Length: 2", e.getMessage());
        }

        // byte tests
        byte[] byteArray = ArrayUtils.add( new byte[] { 1 }, 0, (byte) 2);
        assertTrue( Arrays.equals( new byte[] { 2, 1 }, byteArray ) );
        try {
            byteArray = ArrayUtils.add( (byte[]) null, -1, (byte) 2);
        } catch(IndexOutOfBoundsException e) {
            assertEquals("Index: -1, Length: 0", e.getMessage());
        }
        byteArray = ArrayUtils.add( new byte[] { 2, 6 }, 2, (byte) 3);
        assertTrue( Arrays.equals( new byte[] { 2, 6, 3 }, byteArray ) );
        byteArray = ArrayUtils.add( new byte[] { 2, 6 }, 0, (byte) 1);
        assertTrue( Arrays.equals( new byte[] { 1, 2, 6 }, byteArray ) );
        byteArray = ArrayUtils.add( new byte[] { 2, 6, 3 }, 2, (byte) 1);
        assertTrue( Arrays.equals( new byte[] { 2, 6, 1, 3 }, byteArray ) );
        try {
            byteArray = ArrayUtils.add( new byte[] { 2, 6 }, 4, (byte) 3);
        } catch(IndexOutOfBoundsException e) {
            assertEquals("Index: 4, Length: 2", e.getMessage());
        }
        try {
            byteArray = ArrayUtils.add( new byte[] { 2, 6 }, -1, (byte) 3);
        } catch(IndexOutOfBoundsException e) {
            assertEquals("Index: -1, Length: 2", e.getMessage());
        }

        // int tests
        int[] intArray = ArrayUtils.add( new int[] { 1 }, 0, 2);
        assertTrue( Arrays.equals( new int[] { 2, 1 }, intArray ) );
        try {
            intArray = ArrayUtils.add( (int[]) null, -1, 2);
        } catch(IndexOutOfBoundsException e) {
            assertEquals("Index: -1, Length: 0", e.getMessage());
        }
        intArray = ArrayUtils.add( new int[] { 2, 6 }, 2, 10);
        assertTrue( Arrays.equals( new int[] { 2, 6, 10 }, intArray ) );
        intArray = ArrayUtils.add( new int[] { 2, 6 }, 0, -4);
        assertTrue( Arrays.equals( new int[] { -4, 2, 6 }, intArray ) );
        intArray = ArrayUtils.add( new int[] { 2, 6, 3 }, 2, 1);
        assertTrue( Arrays.equals( new int[] { 2, 6, 1, 3 }, intArray ) );
        try {
            intArray = ArrayUtils.add( new int[] { 2, 6 }, 4, 10);
        } catch(IndexOutOfBoundsException e) {
            assertEquals("Index: 4, Length: 2", e.getMessage());
        }
        try {
            intArray = ArrayUtils.add( new int[] { 2, 6 }, -1, 10);
        } catch(IndexOutOfBoundsException e) {
            assertEquals("Index: -1, Length: 2", e.getMessage());
        }

        // long tests
        long[] longArray = ArrayUtils.add( new long[] { 1L }, 0, 2L);
        assertTrue( Arrays.equals( new long[] { 2L, 1L }, longArray ) );
        try {
            longArray = ArrayUtils.add( (long[]) null, -1, 2L);
        } catch(IndexOutOfBoundsException e) {
            assertEquals("Index: -1, Length: 0", e.getMessage());
        }
        longArray = ArrayUtils.add( new long[] { 2L, 6L }, 2, 10L);
        assertTrue( Arrays.equals( new long[] { 2L, 6L, 10L }, longArray ) );
        longArray = ArrayUtils.add( new long[] { 2L, 6L }, 0, -4L);
        assertTrue( Arrays.equals( new long[] { -4L, 2L, 6L }, longArray ) );
        longArray = ArrayUtils.add( new long[] { 2L, 6L, 3L }, 2, 1L);
        assertTrue( Arrays.equals( new long[] { 2L, 6L, 1L, 3L }, longArray ) );
        try {
            longArray = ArrayUtils.add( new long[] { 2L, 6L }, 4, 10L);
        } catch(IndexOutOfBoundsException e) {
            assertEquals("Index: 4, Length: 2", e.getMessage());
        }
        try {
            longArray = ArrayUtils.add( new long[] { 2L, 6L }, -1, 10L);
        } catch(IndexOutOfBoundsException e) {
            assertEquals("Index: -1, Length: 2", e.getMessage());
        }

        // float tests
        float[] floatArray = ArrayUtils.add( new float[] { 1.1f }, 0, 2.2f);
        assertTrue( Arrays.equals( new float[] { 2.2f, 1.1f }, floatArray ) );
        try {
            floatArray = ArrayUtils.add( (float[]) null, -1, 2.2f);
        } catch(IndexOutOfBoundsException e) {
            assertEquals("Index: -1, Length: 0", e.getMessage());
        }
        floatArray = ArrayUtils.add( new float[] { 2.3f, 6.4f }, 2, 10.5f);
        assertTrue( Arrays.equals( new float[] { 2.3f, 6.4f, 10.5f }, floatArray ) );
        floatArray = ArrayUtils.add( new float[] { 2.6f, 6.7f }, 0, -4.8f);
        assertTrue( Arrays.equals( new float[] { -4.8f, 2.6f, 6.7f }, floatArray ) );
        floatArray = ArrayUtils.add( new float[] { 2.9f, 6.0f, 0.3f }, 2, 1.0f);
        assertTrue( Arrays.equals( new float[] { 2.9f, 6.0f, 1.0f, 0.3f }, floatArray ) );
        try {
            floatArray = ArrayUtils.add( new float[] { 2.3f, 6.4f }, 4, 10.5f);
        } catch(IndexOutOfBoundsException e) {
            assertEquals("Index: 4, Length: 2", e.getMessage());
        }
        try {
            floatArray = ArrayUtils.add( new float[] { 2.3f, 6.4f }, -1, 10.5f);
        } catch(IndexOutOfBoundsException e) {
            assertEquals("Index: -1, Length: 2", e.getMessage());
        }

        // double tests
        double[] doubleArray = ArrayUtils.add( new double[] { 1.1 }, 0, 2.2);
        assertTrue( Arrays.equals( new double[] { 2.2, 1.1 }, doubleArray ) );
        try {
          doubleArray = ArrayUtils.add( (double[]) null, -1, 2.2);
        } catch(IndexOutOfBoundsException e) {
            assertEquals("Index: -1, Length: 0", e.getMessage());
        }
        doubleArray = ArrayUtils.add( new double[] { 2.3, 6.4 }, 2, 10.5);
        assertTrue( Arrays.equals( new double[] { 2.3, 6.4, 10.5 }, doubleArray ) );
        doubleArray = ArrayUtils.add( new double[] { 2.6, 6.7 }, 0, -4.8);
        assertTrue( Arrays.equals( new double[] { -4.8, 2.6, 6.7 }, doubleArray ) );
        doubleArray = ArrayUtils.add( new double[] { 2.9, 6.0, 0.3 }, 2, 1.0);
        assertTrue( Arrays.equals( new double[] { 2.9, 6.0, 1.0, 0.3 }, doubleArray ) );
        try {
            doubleArray = ArrayUtils.add( new double[] { 2.3, 6.4 }, 4, 10.5);
        } catch(IndexOutOfBoundsException e) {
            assertEquals("Index: 4, Length: 2", e.getMessage());
        }
        try {
            doubleArray = ArrayUtils.add( new double[] { 2.3, 6.4 }, -1, 10.5);
        } catch(IndexOutOfBoundsException e) {
            assertEquals("Index: -1, Length: 2", e.getMessage());
        }
    }

}
