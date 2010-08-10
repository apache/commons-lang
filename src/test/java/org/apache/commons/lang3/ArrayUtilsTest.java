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

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.Date;
import java.util.Map;

import junit.framework.TestCase;

/**
 * Unit tests {@link org.apache.commons.lang3.ArrayUtils}.
 *
 * @author Apache Software Foundation
 * @author Moritz Petersen
 * @author Nikolay Metchev
 * @author Matthew Hawthorne
 * @author Tim O'Brien
 * @author <a href="mailto:equinus100@hotmail.com">Ashwin S</a>
 * @author Fredrik Westermarck
 * @author Gary Gregory
 * @author Maarten Coene
 * @author <a href="mailto:levon@lk.otherinbox.com">Levon Karayan</a>
 * @version $Id$
 */
public class ArrayUtilsTest extends TestCase {

    public ArrayUtilsTest(String name) {
        super(name);
    }

    //-----------------------------------------------------------------------
    public void testConstructor() {
        assertNotNull(new ArrayUtils());
        Constructor<?>[] cons = ArrayUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertEquals(true, Modifier.isPublic(cons[0].getModifiers()));
        assertEquals(true, Modifier.isPublic(ArrayUtils.class.getModifiers()));
        assertEquals(false, Modifier.isFinal(ArrayUtils.class.getModifiers()));
    }
    
    //-----------------------------------------------------------------------
    public void testToString() {
        assertEquals("{}", ArrayUtils.toString(null));
        assertEquals("{}", ArrayUtils.toString(new Object[0]));
        assertEquals("{}", ArrayUtils.toString(new String[0]));
        assertEquals("{<null>}", ArrayUtils.toString(new String[] {null}));
        assertEquals("{pink,blue}", ArrayUtils.toString(new String[] {"pink","blue"}));
        
        assertEquals("<empty>", ArrayUtils.toString(null, "<empty>"));
        assertEquals("{}", ArrayUtils.toString(new Object[0], "<empty>"));
        assertEquals("{}", ArrayUtils.toString(new String[0], "<empty>"));
        assertEquals("{<null>}", ArrayUtils.toString(new String[] {null}, "<empty>"));
        assertEquals("{pink,blue}", ArrayUtils.toString(new String[] {"pink","blue"}, "<empty>"));
    }

    //-----------------------------------------------------------------------
    private void assertIsEquals(Object array1, Object array2, Object array3) {
        assertEquals(true, ArrayUtils.isEquals(array1, array1));
        assertEquals(true, ArrayUtils.isEquals(array2, array2));
        assertEquals(true, ArrayUtils.isEquals(array3, array3));
        assertEquals(false, ArrayUtils.isEquals(array1, array2));
        assertEquals(false, ArrayUtils.isEquals(array2, array1));
        assertEquals(false, ArrayUtils.isEquals(array1, array3));
        assertEquals(false, ArrayUtils.isEquals(array3, array1));
        assertEquals(false, ArrayUtils.isEquals(array1, array2));
        assertEquals(false, ArrayUtils.isEquals(array2, array1));
    }

    public void testIsEquals() {
        long[][] larray1 = new long[][]{{2, 5}, {4, 5}};
        long[][] larray2 = new long[][]{{2, 5}, {4, 6}};
        long[] larray3 = new long[]{2, 5};
        this.assertIsEquals(larray1, larray2, larray3);

        int[][] iarray1 = new int[][]{{2, 5}, {4, 5}};
        int[][] iarray2 = new int[][]{{2, 5}, {4, 6}};
        int[] iarray3 = new int[]{2, 5};
        this.assertIsEquals(iarray1, iarray2, iarray3);

        short[][] sarray1 = new short[][]{{2, 5}, {4, 5}};
        short[][] sarray2 = new short[][]{{2, 5}, {4, 6}};
        short[] sarray3 = new short[]{2, 5};
        this.assertIsEquals(sarray1, sarray2, sarray3);

        float[][] farray1 = new float[][]{{2, 5}, {4, 5}};
        float[][] farray2 = new float[][]{{2, 5}, {4, 6}};
        float[] farray3 = new float[]{2, 5};
        this.assertIsEquals(farray1, farray2, farray3);

        double[][] darray1 = new double[][]{{2, 5}, {4, 5}};
        double[][] darray2 = new double[][]{{2, 5}, {4, 6}};
        double[] darray3 = new double[]{2, 5};
        this.assertIsEquals(darray1, darray2, darray3);

        byte[][] byteArray1 = new byte[][]{{2, 5}, {4, 5}};
        byte[][] byteArray2 = new byte[][]{{2, 5}, {4, 6}};
        byte[] byteArray3 = new byte[]{2, 5};
        this.assertIsEquals(byteArray1, byteArray2, byteArray3);

        char[][] charArray1 = new char[][]{{2, 5}, {4, 5}};
        char[][] charArray2 = new char[][]{{2, 5}, {4, 6}};
        char[] charArray3 = new char[]{2, 5};
        this.assertIsEquals(charArray1, charArray2, charArray3);

        boolean[][] barray1 = new boolean[][]{{true, false}, {true, true}};
        boolean[][] barray2 = new boolean[][]{{true, false}, {true, false}};
        boolean[] barray3 = new boolean[]{false, true};
        this.assertIsEquals(barray1, barray2, barray3);

        Object[] array3 = new Object[]{new String(new char[]{'A', 'B'})};
        Object[] array4 = new Object[]{"AB"};
        assertEquals(true, ArrayUtils.isEquals(array3, array3));
        assertEquals(true, ArrayUtils.isEquals(array3, array4));

        assertEquals(true, ArrayUtils.isEquals(null, null));
        assertEquals(false, ArrayUtils.isEquals(null, array4));
    }
    
    //-----------------------------------------------------------------------
    /**
     * Tests generic array creation with parameters of same type.
     */
    public void testArrayCreation()
    {
        final String[] array = ArrayUtils.toArray("foo", "bar");
        assertEquals(2, array.length);
        assertEquals("foo", array[0]);
        assertEquals("bar", array[1]);
    }

    /**
     * Tests generic array creation with general return type.
     */
    public void testArrayCreationWithGeneralReturnType()
    {
        final Object obj = ArrayUtils.toArray("foo", "bar");
        assertTrue(obj instanceof String[]);
    }

    /**
     * Tests generic array creation with parameters of common base type.
     */
    public void testArrayCreationWithDifferentTypes()
    {
        final Number[] array = ArrayUtils.<Number>toArray(Integer.valueOf(42), Double.valueOf(Math.PI));
        assertEquals(2, array.length);
        assertEquals(Integer.valueOf(42), array[0]);
        assertEquals(Double.valueOf(Math.PI), array[1]);
    }

    /**
     * Tests generic array creation with generic type.
     */
    public void testIndirectArrayCreation()
    {
        final String[] array = toArrayPropagatingType("foo", "bar");
        assertEquals(2, array.length);
        assertEquals("foo", array[0]);
        assertEquals("bar", array[1]);
    }

    /**
     * Tests generic empty array creation with generic type.
     */
    public void testEmptyArrayCreation()
    {
        final String[] array = ArrayUtils.<String>toArray();
        assertEquals(0, array.length);
    }

    /**
     * Tests indirect generic empty array creation with generic type.
     */
    public void testIndirectEmptyArrayCreation()
    {
        final String[] array = ArrayUtilsTest.<String>toArrayPropagatingType();
        assertEquals(0, array.length);
    }

    private static <T> T[] toArrayPropagatingType(final T... items)
    {
        return ArrayUtils.toArray(items);
    }
    
    //-----------------------------------------------------------------------
    public void testToMap() {
        Map<?, ?> map = ArrayUtils.toMap(new String[][] {{"foo", "bar"}, {"hello", "world"}});
        
        assertEquals("bar", map.get("foo"));
        assertEquals("world", map.get("hello"));
        
        assertEquals(null, ArrayUtils.toMap(null));
        try {
            ArrayUtils.toMap(new String[][] {{"foo", "bar"}, {"short"}});
            fail("exception expected");
        } catch (IllegalArgumentException ex) {}
        try {
            ArrayUtils.toMap(new Object[] {new Object[] {"foo", "bar"}, "illegal type"});
            fail("exception expected");
        } catch (IllegalArgumentException ex) {}
        try {
            ArrayUtils.toMap(new Object[] {new Object[] {"foo", "bar"}, null});
            fail("exception expected");
        } catch (IllegalArgumentException ex) {}
        
        map = ArrayUtils.toMap(new Object[] {new Map.Entry<Object, Object>() {
            public Object getKey() {
                return "foo";
            }
            public Object getValue() {
                return "bar";
            }
            public Object setValue(Object value) {
                throw new UnsupportedOperationException();
            }
            @Override
            public boolean equals(Object o) {
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
    public void testClone() {
        assertEquals(null, ArrayUtils.clone((Object[]) null));
        Object[] original1 = new Object[0];
        Object[] cloned1 = ArrayUtils.clone(original1);
        assertTrue(Arrays.equals(original1, cloned1));
        assertTrue(original1 != cloned1);
        
        StringBuffer buf = new StringBuffer("pick");
        original1 = new Object[] {buf, "a", new String[] {"stick"}};
        cloned1 = ArrayUtils.clone(original1);
        assertTrue(Arrays.equals(original1, cloned1));
        assertTrue(original1 != cloned1);
        assertSame(original1[0], cloned1[0]);
        assertSame(original1[1], cloned1[1]);
        assertSame(original1[2], cloned1[2]);
    }

    public void testCloneBoolean() {
        assertEquals(null, ArrayUtils.clone((boolean[]) null));
        boolean[] original = new boolean[] {true, false};
        boolean[] cloned = ArrayUtils.clone(original);
        assertTrue(Arrays.equals(original, cloned));
        assertTrue(original != cloned);
    }
    
    public void testCloneLong() {
        assertEquals(null, ArrayUtils.clone((long[]) null));
        long[] original = new long[] {0L, 1L};
        long[] cloned = ArrayUtils.clone(original);
        assertTrue(Arrays.equals(original, cloned));
        assertTrue(original != cloned);
    }
    
    public void testCloneInt() {
        assertEquals(null, ArrayUtils.clone((int[]) null));
        int[] original = new int[] {5, 8};
        int[] cloned = ArrayUtils.clone(original);
        assertTrue(Arrays.equals(original, cloned));
        assertTrue(original != cloned);
    }
    
    public void testCloneShort() {
        assertEquals(null, ArrayUtils.clone((short[]) null));
        short[] original = new short[] {1, 4};
        short[] cloned = ArrayUtils.clone(original);
        assertTrue(Arrays.equals(original, cloned));
        assertTrue(original != cloned);
    }
    
    public void testCloneChar() {
        assertEquals(null, ArrayUtils.clone((char[]) null));
        char[] original = new char[] {'a', '4'};
        char[] cloned = ArrayUtils.clone(original);
        assertTrue(Arrays.equals(original, cloned));
        assertTrue(original != cloned);
    }
    
    public void testCloneByte() {
        assertEquals(null, ArrayUtils.clone((byte[]) null));
        byte[] original = new byte[] {1, 6};
        byte[] cloned = ArrayUtils.clone(original);
        assertTrue(Arrays.equals(original, cloned));
        assertTrue(original != cloned);
    }
    
    public void testCloneDouble() {
        assertEquals(null, ArrayUtils.clone((double[]) null));
        double[] original = new double[] {2.4d, 5.7d};
        double[] cloned = ArrayUtils.clone(original);
        assertTrue(Arrays.equals(original, cloned));
        assertTrue(original != cloned);
    }
    
    public void testCloneFloat() {
        assertEquals(null, ArrayUtils.clone((float[]) null));
        float[] original = new float[] {2.6f, 6.4f};
        float[] cloned = ArrayUtils.clone(original);
        assertTrue(Arrays.equals(original, cloned));
        assertTrue(original != cloned);
    }

    //-----------------------------------------------------------------------

    public void testNullToEmptyBoolean() {
        // Test null handling
        assertEquals(ArrayUtils.EMPTY_BOOLEAN_ARRAY, ArrayUtils.nullToEmpty((boolean[]) null));
        // Test valid array handling
        boolean[] original = new boolean[] {true, false};
        assertEquals(original, ArrayUtils.nullToEmpty(original));
        // Test empty array handling
        boolean[] empty = new boolean[]{};
        boolean[] result = ArrayUtils.nullToEmpty(empty);
        assertEquals(ArrayUtils.EMPTY_BOOLEAN_ARRAY, result);
        assertTrue(empty != result);
    }
    
    public void testNullToEmptyLong() {
        // Test null handling
        assertEquals(ArrayUtils.EMPTY_LONG_ARRAY, ArrayUtils.nullToEmpty((long[]) null));
        // Test valid array handling
        long[] original = new long[] {1L, 2L};
        assertEquals(original, ArrayUtils.nullToEmpty(original));
        // Test empty array handling
        long[] empty = new long[]{};
        long[] result = ArrayUtils.nullToEmpty(empty);
        assertEquals(ArrayUtils.EMPTY_LONG_ARRAY, result);
        assertTrue(empty != result);
    }
    
    public void testNullToEmptyInt() {
        // Test null handling
        assertEquals(ArrayUtils.EMPTY_INT_ARRAY, ArrayUtils.nullToEmpty((int[]) null));
        // Test valid array handling
        int[] original = new int[] {1, 2};
        assertEquals(original, ArrayUtils.nullToEmpty(original));
        // Test empty array handling
        int[] empty = new int[]{};
        int[] result = ArrayUtils.nullToEmpty(empty);
        assertEquals(ArrayUtils.EMPTY_INT_ARRAY, result);
        assertTrue(empty != result);
    }
    
    public void testNullToEmptyShort() {
        // Test null handling
        assertEquals(ArrayUtils.EMPTY_SHORT_ARRAY, ArrayUtils.nullToEmpty((short[]) null));
        // Test valid array handling
        short[] original = new short[] {1, 2};
        assertEquals(original, ArrayUtils.nullToEmpty(original));
        // Test empty array handling
        short[] empty = new short[]{};
        short[] result = ArrayUtils.nullToEmpty(empty);
        assertEquals(ArrayUtils.EMPTY_SHORT_ARRAY, result);
        assertTrue(empty != result);
    }
    
    public void testNullToEmptyChar() {
        // Test null handling
        assertEquals(ArrayUtils.EMPTY_CHAR_ARRAY, ArrayUtils.nullToEmpty((char[]) null));
        // Test valid array handling
        char[] original = new char[] {'a', 'b'};
        assertEquals(original, ArrayUtils.nullToEmpty(original));
        // Test empty array handling
        char[] empty = new char[]{};
        char[] result = ArrayUtils.nullToEmpty(empty);
        assertEquals(ArrayUtils.EMPTY_CHAR_ARRAY, result);
        assertTrue(empty != result);
    }
    
    public void testNullToEmptyByte() {
        // Test null handling
        assertEquals(ArrayUtils.EMPTY_BYTE_ARRAY, ArrayUtils.nullToEmpty((byte[]) null));
        // Test valid array handling
        byte[] original = new byte[] {0x0F, 0x0E};
        assertEquals(original, ArrayUtils.nullToEmpty(original));
        // Test empty array handling
        byte[] empty = new byte[]{};
        byte[] result = ArrayUtils.nullToEmpty(empty);
        assertEquals(ArrayUtils.EMPTY_BYTE_ARRAY, result);
        assertTrue(empty != result);
    }
    
    public void testNullToEmptyDouble() {
        // Test null handling
        assertEquals(ArrayUtils.EMPTY_DOUBLE_ARRAY, ArrayUtils.nullToEmpty((double[]) null));
        // Test valid array handling
        double[] original = new double[] {1L, 2L};
        assertEquals(original, ArrayUtils.nullToEmpty(original));
        // Test empty array handling
        double[] empty = new double[]{};
        double[] result = ArrayUtils.nullToEmpty(empty);
        assertEquals(ArrayUtils.EMPTY_DOUBLE_ARRAY, result);
        assertTrue(empty != result);
    }
    
    public void testNullToEmptyFloat() {
        // Test null handling
        assertEquals(ArrayUtils.EMPTY_FLOAT_ARRAY, ArrayUtils.nullToEmpty((float[]) null));
        // Test valid array handling
        float[] original = new float[] {2.6f, 3.8f};
        assertEquals(original, ArrayUtils.nullToEmpty(original));
        // Test empty array handling
        float[] empty = new float[]{};
        float[] result = ArrayUtils.nullToEmpty(empty);
        assertEquals(ArrayUtils.EMPTY_FLOAT_ARRAY, result);
        assertTrue(empty != result);
    }

    public void testNullToEmptyObject() {
        // Test null handling
        assertEquals(ArrayUtils.EMPTY_OBJECT_ARRAY, ArrayUtils.nullToEmpty((Object[]) null));
        // Test valid array handling
        Object[] original = new Object[] {true, false};
        assertEquals(original, ArrayUtils.nullToEmpty(original));
        // Test empty array handling
        Object[] empty = new Object[]{};
        Object[] result = ArrayUtils.nullToEmpty(empty);
        assertEquals(ArrayUtils.EMPTY_OBJECT_ARRAY, result);
        assertTrue(empty != result);
    }

    public void testNullToEmptyString() {
        // Test null handling
        assertEquals(ArrayUtils.EMPTY_STRING_ARRAY, ArrayUtils.nullToEmpty((String[]) null));
        // Test valid array handling
        String[] original = new String[] {"abc", "def"};
        assertEquals(original, ArrayUtils.nullToEmpty(original));
        // Test empty array handling
        String[] empty = new String[]{};
        String[] result = ArrayUtils.nullToEmpty(empty);
        assertEquals(ArrayUtils.EMPTY_STRING_ARRAY, result);
        assertTrue(empty != result);
    }

    public void testNullToEmptyBooleanObject() {
        // Test null handling
        assertEquals(ArrayUtils.EMPTY_BOOLEAN_OBJECT_ARRAY, ArrayUtils.nullToEmpty((Boolean[]) null));
        // Test valid array handling
        Boolean[] original = new Boolean[] {Boolean.TRUE, Boolean.FALSE};
        assertEquals(original, ArrayUtils.nullToEmpty(original));
        // Test empty array handling
        Boolean[] empty = new Boolean[]{};
        Boolean[] result = ArrayUtils.nullToEmpty(empty);
        assertEquals(ArrayUtils.EMPTY_BOOLEAN_OBJECT_ARRAY, result);
        assertTrue(empty != result);
    }
    
    public void testNullToEmptyLongObject() {
        // Test null handling
        assertEquals(ArrayUtils.EMPTY_LONG_OBJECT_ARRAY, ArrayUtils.nullToEmpty((Long[]) null));
        // Test valid array handling
        Long[] original = new Long[] {1L, 2L};
        assertEquals(original, ArrayUtils.nullToEmpty(original));
        // Test empty array handling
        Long[] empty = new Long[]{};
        Long[] result = ArrayUtils.nullToEmpty(empty);
        assertEquals(ArrayUtils.EMPTY_LONG_OBJECT_ARRAY, result);
        assertTrue(empty != result);
    }
    
    public void testNullToEmptyIntObject() {
        // Test null handling
        assertEquals(ArrayUtils.EMPTY_INTEGER_OBJECT_ARRAY, ArrayUtils.nullToEmpty((Integer[]) null));
        // Test valid array handling
        Integer[] original = new Integer[] {1, 2};
        assertEquals(original, ArrayUtils.nullToEmpty(original));
        // Test empty array handling
        Integer[] empty = new Integer[]{};
        Integer[] result = ArrayUtils.nullToEmpty(empty);
        assertEquals(ArrayUtils.EMPTY_INTEGER_OBJECT_ARRAY, result);
        assertTrue(empty != result);
    }
    
    public void testNullToEmptyShortObject() {
        // Test null handling
        assertEquals(ArrayUtils.EMPTY_SHORT_OBJECT_ARRAY, ArrayUtils.nullToEmpty((Short[]) null));
        // Test valid array handling
        Short[] original = new Short[] {1, 2};
        assertEquals(original, ArrayUtils.nullToEmpty(original));
        // Test empty array handling
        Short[] empty = new Short[]{};
        Short[] result = ArrayUtils.nullToEmpty(empty);
        assertEquals(ArrayUtils.EMPTY_SHORT_OBJECT_ARRAY, result);
        assertTrue(empty != result);
    }
    
    public void testNullToEmptyCharObject() {
        // Test null handling
        assertEquals(ArrayUtils.EMPTY_CHARACTER_OBJECT_ARRAY, ArrayUtils.nullToEmpty((Character[]) null));
        // Test valid array handling
        Character[] original = new Character[] {'a', 'b'};
        assertEquals(original, ArrayUtils.nullToEmpty(original));
        // Test empty array handling
        Character[] empty = new Character[]{};
        Character[] result = ArrayUtils.nullToEmpty(empty);
        assertEquals(ArrayUtils.EMPTY_CHARACTER_OBJECT_ARRAY, result);
        assertTrue(empty != result);
    }
    
    public void testNullToEmptyByteObject() {
        // Test null handling
        assertEquals(ArrayUtils.EMPTY_BYTE_OBJECT_ARRAY, ArrayUtils.nullToEmpty((Byte[]) null));
        // Test valid array handling
        Byte[] original = new Byte[] {0x0F, 0x0E};
        assertEquals(original, ArrayUtils.nullToEmpty(original));
        // Test empty array handling
        Byte[] empty = new Byte[]{};
        Byte[] result = ArrayUtils.nullToEmpty(empty);
        assertEquals(ArrayUtils.EMPTY_BYTE_OBJECT_ARRAY, result);
        assertTrue(empty != result);
    }
    
    public void testNullToEmptyDoubleObject() {
        // Test null handling
        assertEquals(ArrayUtils.EMPTY_DOUBLE_OBJECT_ARRAY, ArrayUtils.nullToEmpty((Double[]) null));
        // Test valid array handling
        Double[] original = new Double[] {1D, 2D};
        assertEquals(original, ArrayUtils.nullToEmpty(original));
        // Test empty array handling
        Double[] empty = new Double[]{};
        Double[] result = ArrayUtils.nullToEmpty(empty);
        assertEquals(ArrayUtils.EMPTY_DOUBLE_OBJECT_ARRAY, result);
        assertTrue(empty != result);
    }
    
    public void testNullToEmptyFloatObject() {
        // Test null handling
        assertEquals(ArrayUtils.EMPTY_FLOAT_OBJECT_ARRAY, ArrayUtils.nullToEmpty((Float[]) null));
        // Test valid array handling
        Float[] original = new Float[] {2.6f, 3.8f};
        assertEquals(original, ArrayUtils.nullToEmpty(original));
        // Test empty array handling
        Float[] empty = new Float[]{};
        Float[] result = ArrayUtils.nullToEmpty(empty);
        assertEquals(ArrayUtils.EMPTY_FLOAT_OBJECT_ARRAY, result);
        assertTrue(empty != result);
    }

    //-----------------------------------------------------------------------

    public void testSubarrayObject() {
        Object[] nullArray = null;
        Object[] objectArray = { "a", "b", "c", "d", "e", "f"};

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
        Date[] dateArray = { new java.sql.Date(new Date().getTime()),
            new Date(), new Date(), new Date(), new Date() };

        assertSame("Object type", Object.class,
            ArrayUtils.subarray(objectArray, 2, 4).getClass().getComponentType());
        assertSame("java.util.Date type", java.util.Date.class,
            ArrayUtils.subarray(dateArray, 1, 4).getClass().getComponentType());
        assertNotSame("java.sql.Date type", java.sql.Date.class,
            ArrayUtils.subarray(dateArray, 1, 4).getClass().getComponentType());
        try {
            @SuppressWarnings("unused")
            java.sql.Date[] dummy = (java.sql.Date[])ArrayUtils.subarray(dateArray, 1,3);
            fail("Invalid downcast");
        } catch (ClassCastException e) {}
    }

    public void testSubarrayLong() {
        long[] nullArray = null;
        long[] array = { 999910, 999911, 999912, 999913, 999914, 999915 };
        long[] leftSubarray     = { 999910, 999911, 999912, 999913 };
        long[] midSubarray      = { 999911, 999912, 999913, 999914 };
        long[] rightSubarray    = { 999912, 999913, 999914, 999915 };

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

    public void testSubarrayInt() {
        int[] nullArray = null;
        int[] array = { 10, 11, 12, 13, 14, 15 };
        int[] leftSubarray  = { 10, 11, 12, 13 };
        int[] midSubarray   = { 11, 12, 13, 14 };
        int[] rightSubarray = { 12, 13, 14, 15 };


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

    public void testSubarrayShort() {
        short[] nullArray = null;
        short[] array = { 10, 11, 12, 13, 14, 15 };
        short[] leftSubarray    = { 10, 11, 12, 13 };
        short[] midSubarray     = { 11, 12, 13, 14 };
        short[] rightSubarray   = { 12, 13, 14, 15 };


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

    public void testSubarrChar() {
        char[] nullArray = null;
        char[] array = { 'a', 'b', 'c', 'd', 'e', 'f' };
        char[] leftSubarray     = { 'a', 'b', 'c', 'd', };
        char[] midSubarray      = { 'b', 'c', 'd', 'e', };
        char[] rightSubarray    = { 'c', 'd', 'e', 'f', };


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

    public void testSubarrayByte() {
        byte[] nullArray = null;
        byte[] array = { 10, 11, 12, 13, 14, 15 };
        byte[] leftSubarray     = { 10, 11, 12, 13 };
        byte[] midSubarray      = { 11, 12, 13, 14 };
        byte[] rightSubarray = { 12, 13, 14, 15 };


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

    public void testSubarrayDouble() {
        double[] nullArray = null;
        double[] array = { 10.123, 11.234, 12.345, 13.456, 14.567, 15.678 };
        double[] leftSubarray   = { 10.123, 11.234, 12.345, 13.456, };
        double[] midSubarray    = { 11.234, 12.345, 13.456, 14.567, };
        double[] rightSubarray  = { 12.345, 13.456, 14.567, 15.678 };


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

    public void testSubarrayFloat() {
        float[] nullArray = null;
        float[] array = { 10, 11, 12, 13, 14, 15 };
        float[] leftSubarray    = { 10, 11, 12, 13 };
        float[] midSubarray     = { 11, 12, 13, 14 };
        float[] rightSubarray   = { 12, 13, 14, 15 };


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

    public void testSubarrayBoolean() {
        boolean[] nullArray = null;
        boolean[] array = { true, true, false, true, false, true };
        boolean[] leftSubarray  = { true, true, false, true  };
        boolean[] midSubarray   = { true, false, true, false };
        boolean[] rightSubarray = { false, true, false, true };


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
    public void testSameLength() {
        Object[] nullArray = null;
        Object[] emptyArray = new Object[0];
        Object[] oneArray = new Object[] {"pick"};
        Object[] twoArray = new Object[] {"pick", "stick"};
        
        assertEquals(true, ArrayUtils.isSameLength(nullArray, nullArray));
        assertEquals(true, ArrayUtils.isSameLength(nullArray, emptyArray));
        assertEquals(false, ArrayUtils.isSameLength(nullArray, oneArray));
        assertEquals(false, ArrayUtils.isSameLength(nullArray, twoArray));
        
        assertEquals(true, ArrayUtils.isSameLength(emptyArray, nullArray));
        assertEquals(true, ArrayUtils.isSameLength(emptyArray, emptyArray));
        assertEquals(false, ArrayUtils.isSameLength(emptyArray, oneArray));
        assertEquals(false, ArrayUtils.isSameLength(emptyArray, twoArray));
        
        assertEquals(false, ArrayUtils.isSameLength(oneArray, nullArray));
        assertEquals(false, ArrayUtils.isSameLength(oneArray, emptyArray));
        assertEquals(true, ArrayUtils.isSameLength(oneArray, oneArray));
        assertEquals(false, ArrayUtils.isSameLength(oneArray, twoArray));
        
        assertEquals(false, ArrayUtils.isSameLength(twoArray, nullArray));
        assertEquals(false, ArrayUtils.isSameLength(twoArray, emptyArray));
        assertEquals(false, ArrayUtils.isSameLength(twoArray, oneArray));
        assertEquals(true, ArrayUtils.isSameLength(twoArray, twoArray));
    }

    public void testSameLengthBoolean() {
        boolean[] nullArray = null;
        boolean[] emptyArray = new boolean[0];
        boolean[] oneArray = new boolean[] {true};
        boolean[] twoArray = new boolean[] {true, false};
        
        assertEquals(true, ArrayUtils.isSameLength(nullArray, nullArray));
        assertEquals(true, ArrayUtils.isSameLength(nullArray, emptyArray));
        assertEquals(false, ArrayUtils.isSameLength(nullArray, oneArray));
        assertEquals(false, ArrayUtils.isSameLength(nullArray, twoArray));
        
        assertEquals(true, ArrayUtils.isSameLength(emptyArray, nullArray));
        assertEquals(true, ArrayUtils.isSameLength(emptyArray, emptyArray));
        assertEquals(false, ArrayUtils.isSameLength(emptyArray, oneArray));
        assertEquals(false, ArrayUtils.isSameLength(emptyArray, twoArray));
        
        assertEquals(false, ArrayUtils.isSameLength(oneArray, nullArray));
        assertEquals(false, ArrayUtils.isSameLength(oneArray, emptyArray));
        assertEquals(true, ArrayUtils.isSameLength(oneArray, oneArray));
        assertEquals(false, ArrayUtils.isSameLength(oneArray, twoArray));
        
        assertEquals(false, ArrayUtils.isSameLength(twoArray, nullArray));
        assertEquals(false, ArrayUtils.isSameLength(twoArray, emptyArray));
        assertEquals(false, ArrayUtils.isSameLength(twoArray, oneArray));
        assertEquals(true, ArrayUtils.isSameLength(twoArray, twoArray));
    }
    
    public void testSameLengthLong() {
        long[] nullArray = null;
        long[] emptyArray = new long[0];
        long[] oneArray = new long[] {0L};
        long[] twoArray = new long[] {0L, 76L};
        
        assertEquals(true, ArrayUtils.isSameLength(nullArray, nullArray));
        assertEquals(true, ArrayUtils.isSameLength(nullArray, emptyArray));
        assertEquals(false, ArrayUtils.isSameLength(nullArray, oneArray));
        assertEquals(false, ArrayUtils.isSameLength(nullArray, twoArray));
        
        assertEquals(true, ArrayUtils.isSameLength(emptyArray, nullArray));
        assertEquals(true, ArrayUtils.isSameLength(emptyArray, emptyArray));
        assertEquals(false, ArrayUtils.isSameLength(emptyArray, oneArray));
        assertEquals(false, ArrayUtils.isSameLength(emptyArray, twoArray));
        
        assertEquals(false, ArrayUtils.isSameLength(oneArray, nullArray));
        assertEquals(false, ArrayUtils.isSameLength(oneArray, emptyArray));
        assertEquals(true, ArrayUtils.isSameLength(oneArray, oneArray));
        assertEquals(false, ArrayUtils.isSameLength(oneArray, twoArray));
        
        assertEquals(false, ArrayUtils.isSameLength(twoArray, nullArray));
        assertEquals(false, ArrayUtils.isSameLength(twoArray, emptyArray));
        assertEquals(false, ArrayUtils.isSameLength(twoArray, oneArray));
        assertEquals(true, ArrayUtils.isSameLength(twoArray, twoArray));
    }
    
    public void testSameLengthInt() {
        int[] nullArray = null;
        int[] emptyArray = new int[0];
        int[] oneArray = new int[] {4};
        int[] twoArray = new int[] {5, 7};
        
        assertEquals(true, ArrayUtils.isSameLength(nullArray, nullArray));
        assertEquals(true, ArrayUtils.isSameLength(nullArray, emptyArray));
        assertEquals(false, ArrayUtils.isSameLength(nullArray, oneArray));
        assertEquals(false, ArrayUtils.isSameLength(nullArray, twoArray));
        
        assertEquals(true, ArrayUtils.isSameLength(emptyArray, nullArray));
        assertEquals(true, ArrayUtils.isSameLength(emptyArray, emptyArray));
        assertEquals(false, ArrayUtils.isSameLength(emptyArray, oneArray));
        assertEquals(false, ArrayUtils.isSameLength(emptyArray, twoArray));
        
        assertEquals(false, ArrayUtils.isSameLength(oneArray, nullArray));
        assertEquals(false, ArrayUtils.isSameLength(oneArray, emptyArray));
        assertEquals(true, ArrayUtils.isSameLength(oneArray, oneArray));
        assertEquals(false, ArrayUtils.isSameLength(oneArray, twoArray));
        
        assertEquals(false, ArrayUtils.isSameLength(twoArray, nullArray));
        assertEquals(false, ArrayUtils.isSameLength(twoArray, emptyArray));
        assertEquals(false, ArrayUtils.isSameLength(twoArray, oneArray));
        assertEquals(true, ArrayUtils.isSameLength(twoArray, twoArray));
    }
    
    public void testSameLengthShort() {
        short[] nullArray = null;
        short[] emptyArray = new short[0];
        short[] oneArray = new short[] {4};
        short[] twoArray = new short[] {6, 8};
        
        assertEquals(true, ArrayUtils.isSameLength(nullArray, nullArray));
        assertEquals(true, ArrayUtils.isSameLength(nullArray, emptyArray));
        assertEquals(false, ArrayUtils.isSameLength(nullArray, oneArray));
        assertEquals(false, ArrayUtils.isSameLength(nullArray, twoArray));
        
        assertEquals(true, ArrayUtils.isSameLength(emptyArray, nullArray));
        assertEquals(true, ArrayUtils.isSameLength(emptyArray, emptyArray));
        assertEquals(false, ArrayUtils.isSameLength(emptyArray, oneArray));
        assertEquals(false, ArrayUtils.isSameLength(emptyArray, twoArray));
        
        assertEquals(false, ArrayUtils.isSameLength(oneArray, nullArray));
        assertEquals(false, ArrayUtils.isSameLength(oneArray, emptyArray));
        assertEquals(true, ArrayUtils.isSameLength(oneArray, oneArray));
        assertEquals(false, ArrayUtils.isSameLength(oneArray, twoArray));
        
        assertEquals(false, ArrayUtils.isSameLength(twoArray, nullArray));
        assertEquals(false, ArrayUtils.isSameLength(twoArray, emptyArray));
        assertEquals(false, ArrayUtils.isSameLength(twoArray, oneArray));
        assertEquals(true, ArrayUtils.isSameLength(twoArray, twoArray));
    }
    
    public void testSameLengthChar() {
        char[] nullArray = null;
        char[] emptyArray = new char[0];
        char[] oneArray = new char[] {'f'};
        char[] twoArray = new char[] {'d', 't'};
        
        assertEquals(true, ArrayUtils.isSameLength(nullArray, nullArray));
        assertEquals(true, ArrayUtils.isSameLength(nullArray, emptyArray));
        assertEquals(false, ArrayUtils.isSameLength(nullArray, oneArray));
        assertEquals(false, ArrayUtils.isSameLength(nullArray, twoArray));
        
        assertEquals(true, ArrayUtils.isSameLength(emptyArray, nullArray));
        assertEquals(true, ArrayUtils.isSameLength(emptyArray, emptyArray));
        assertEquals(false, ArrayUtils.isSameLength(emptyArray, oneArray));
        assertEquals(false, ArrayUtils.isSameLength(emptyArray, twoArray));
        
        assertEquals(false, ArrayUtils.isSameLength(oneArray, nullArray));
        assertEquals(false, ArrayUtils.isSameLength(oneArray, emptyArray));
        assertEquals(true, ArrayUtils.isSameLength(oneArray, oneArray));
        assertEquals(false, ArrayUtils.isSameLength(oneArray, twoArray));
        
        assertEquals(false, ArrayUtils.isSameLength(twoArray, nullArray));
        assertEquals(false, ArrayUtils.isSameLength(twoArray, emptyArray));
        assertEquals(false, ArrayUtils.isSameLength(twoArray, oneArray));
        assertEquals(true, ArrayUtils.isSameLength(twoArray, twoArray));
    }
    
    public void testSameLengthByte() {
        byte[] nullArray = null;
        byte[] emptyArray = new byte[0];
        byte[] oneArray = new byte[] {3};
        byte[] twoArray = new byte[] {4, 6};
        
        assertEquals(true, ArrayUtils.isSameLength(nullArray, nullArray));
        assertEquals(true, ArrayUtils.isSameLength(nullArray, emptyArray));
        assertEquals(false, ArrayUtils.isSameLength(nullArray, oneArray));
        assertEquals(false, ArrayUtils.isSameLength(nullArray, twoArray));
        
        assertEquals(true, ArrayUtils.isSameLength(emptyArray, nullArray));
        assertEquals(true, ArrayUtils.isSameLength(emptyArray, emptyArray));
        assertEquals(false, ArrayUtils.isSameLength(emptyArray, oneArray));
        assertEquals(false, ArrayUtils.isSameLength(emptyArray, twoArray));
        
        assertEquals(false, ArrayUtils.isSameLength(oneArray, nullArray));
        assertEquals(false, ArrayUtils.isSameLength(oneArray, emptyArray));
        assertEquals(true, ArrayUtils.isSameLength(oneArray, oneArray));
        assertEquals(false, ArrayUtils.isSameLength(oneArray, twoArray));
        
        assertEquals(false, ArrayUtils.isSameLength(twoArray, nullArray));
        assertEquals(false, ArrayUtils.isSameLength(twoArray, emptyArray));
        assertEquals(false, ArrayUtils.isSameLength(twoArray, oneArray));
        assertEquals(true, ArrayUtils.isSameLength(twoArray, twoArray));
    }
    
    public void testSameLengthDouble() {
        double[] nullArray = null;
        double[] emptyArray = new double[0];
        double[] oneArray = new double[] {1.3d};
        double[] twoArray = new double[] {4.5d, 6.3d};
        
        assertEquals(true, ArrayUtils.isSameLength(nullArray, nullArray));
        assertEquals(true, ArrayUtils.isSameLength(nullArray, emptyArray));
        assertEquals(false, ArrayUtils.isSameLength(nullArray, oneArray));
        assertEquals(false, ArrayUtils.isSameLength(nullArray, twoArray));
        
        assertEquals(true, ArrayUtils.isSameLength(emptyArray, nullArray));
        assertEquals(true, ArrayUtils.isSameLength(emptyArray, emptyArray));
        assertEquals(false, ArrayUtils.isSameLength(emptyArray, oneArray));
        assertEquals(false, ArrayUtils.isSameLength(emptyArray, twoArray));
        
        assertEquals(false, ArrayUtils.isSameLength(oneArray, nullArray));
        assertEquals(false, ArrayUtils.isSameLength(oneArray, emptyArray));
        assertEquals(true, ArrayUtils.isSameLength(oneArray, oneArray));
        assertEquals(false, ArrayUtils.isSameLength(oneArray, twoArray));
        
        assertEquals(false, ArrayUtils.isSameLength(twoArray, nullArray));
        assertEquals(false, ArrayUtils.isSameLength(twoArray, emptyArray));
        assertEquals(false, ArrayUtils.isSameLength(twoArray, oneArray));
        assertEquals(true, ArrayUtils.isSameLength(twoArray, twoArray));
    }
    
    public void testSameLengthFloat() {
        float[] nullArray = null;
        float[] emptyArray = new float[0];
        float[] oneArray = new float[] {2.5f};
        float[] twoArray = new float[] {6.4f, 5.8f};
        
        assertEquals(true, ArrayUtils.isSameLength(nullArray, nullArray));
        assertEquals(true, ArrayUtils.isSameLength(nullArray, emptyArray));
        assertEquals(false, ArrayUtils.isSameLength(nullArray, oneArray));
        assertEquals(false, ArrayUtils.isSameLength(nullArray, twoArray));
        
        assertEquals(true, ArrayUtils.isSameLength(emptyArray, nullArray));
        assertEquals(true, ArrayUtils.isSameLength(emptyArray, emptyArray));
        assertEquals(false, ArrayUtils.isSameLength(emptyArray, oneArray));
        assertEquals(false, ArrayUtils.isSameLength(emptyArray, twoArray));
        
        assertEquals(false, ArrayUtils.isSameLength(oneArray, nullArray));
        assertEquals(false, ArrayUtils.isSameLength(oneArray, emptyArray));
        assertEquals(true, ArrayUtils.isSameLength(oneArray, oneArray));
        assertEquals(false, ArrayUtils.isSameLength(oneArray, twoArray));
        
        assertEquals(false, ArrayUtils.isSameLength(twoArray, nullArray));
        assertEquals(false, ArrayUtils.isSameLength(twoArray, emptyArray));
        assertEquals(false, ArrayUtils.isSameLength(twoArray, oneArray));
        assertEquals(true, ArrayUtils.isSameLength(twoArray, twoArray));
    }
    
    //-----------------------------------------------------------------------
    public void testSameType() {
        try {
            ArrayUtils.isSameType(null, null);
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            ArrayUtils.isSameType(null, new Object[0]);
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            ArrayUtils.isSameType(new Object[0], null);
            fail();
        } catch (IllegalArgumentException ex) {}
        
        assertEquals(true, ArrayUtils.isSameType(new Object[0], new Object[0]));
        assertEquals(false, ArrayUtils.isSameType(new String[0], new Object[0]));
        assertEquals(true, ArrayUtils.isSameType(new String[0][0], new String[0][0]));
        assertEquals(false, ArrayUtils.isSameType(new String[0], new String[0][0]));
        assertEquals(false, ArrayUtils.isSameType(new String[0][0], new String[0]));
    }
    
    //-----------------------------------------------------------------------
    public void testReverse() {
        StringBuffer str1 = new StringBuffer("pick");
        String str2 = "a";
        String[] str3 = new String[] {"stick"};
        String str4 = "up";
        
        Object[] array = new Object[] {str1, str2, str3};
        ArrayUtils.reverse(array);
        assertEquals(array[0], str3);
        assertEquals(array[1], str2);
        assertEquals(array[2], str1);
        
        array = new Object[] {str1, str2, str3, str4};
        ArrayUtils.reverse(array);
        assertEquals(array[0], str4);
        assertEquals(array[1], str3);
        assertEquals(array[2], str2);
        assertEquals(array[3], str1);

        array = null;
        ArrayUtils.reverse(array);
        assertEquals(null, array);
    }

    public void testReverseLong() {
        long[] array = new long[] {1L, 2L, 3L};
        ArrayUtils.reverse(array);
        assertEquals(array[0], 3L);
        assertEquals(array[1], 2L);
        assertEquals(array[2], 1L);

        array = null;
        ArrayUtils.reverse(array);
        assertEquals(null, array);
    }
    
    public void testReverseInt() {
        int[] array = new int[] {1, 2, 3};
        ArrayUtils.reverse(array);
        assertEquals(array[0], 3);
        assertEquals(array[1], 2);
        assertEquals(array[2], 1);

        array = null;
        ArrayUtils.reverse(array);
        assertEquals(null, array);
    }
    
    public void testReverseShort() {
        short[] array = new short[] {1, 2, 3};
        ArrayUtils.reverse(array);
        assertEquals(array[0], 3);
        assertEquals(array[1], 2);
        assertEquals(array[2], 1);

        array = null;
        ArrayUtils.reverse(array);
        assertEquals(null, array);
    }
    
    public void testReverseChar() {
        char[] array = new char[] {'a', 'f', 'C'};
        ArrayUtils.reverse(array);
        assertEquals(array[0], 'C');
        assertEquals(array[1], 'f');
        assertEquals(array[2], 'a');

        array = null;
        ArrayUtils.reverse(array);
        assertEquals(null, array);
    }
    
    public void testReverseByte() {
        byte[] array = new byte[] {2, 3, 4};
        ArrayUtils.reverse(array);
        assertEquals(array[0], 4);
        assertEquals(array[1], 3);
        assertEquals(array[2], 2);

        array = null;
        ArrayUtils.reverse(array);
        assertEquals(null, array);
    }
    
    public void testReverseDouble() {
        double[] array = new double[] {0.3d, 0.4d, 0.5d};
        ArrayUtils.reverse(array);
        assertEquals(array[0], 0.5d, 0.0d);
        assertEquals(array[1], 0.4d, 0.0d);
        assertEquals(array[2], 0.3d, 0.0d);

        array = null;
        ArrayUtils.reverse(array);
        assertEquals(null, array);
    }
    
    public void testReverseFloat() {
        float[] array = new float[] {0.3f, 0.4f, 0.5f};
        ArrayUtils.reverse(array);
        assertEquals(array[0], 0.5f, 0.0f);
        assertEquals(array[1], 0.4f, 0.0f);
        assertEquals(array[2], 0.3f, 0.0f);

        array = null;
        ArrayUtils.reverse(array);
        assertEquals(null, array);
    }
    
    public void testReverseBoolean() {
        boolean[] array = new boolean[] {false, false, true};
        ArrayUtils.reverse(array);
        assertEquals(array[0], true);
        assertEquals(array[1], false);
        assertEquals(array[2], false);

        array = null;
        ArrayUtils.reverse(array);
        assertEquals(null, array);
    }
    
    //-----------------------------------------------------------------------
    public void testIndexOf() {
        Object[] array = new Object[] { "0", "1", "2", "3", null, "0" };
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

    public void testIndexOfWithStartIndex() {
        Object[] array = new Object[] { "0", "1", "2", "3", null, "0" };
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

    public void testLastIndexOf() {
        Object[] array = new Object[] { "0", "1", "2", "3", null, "0" };
        assertEquals(-1, ArrayUtils.lastIndexOf(null, null));
        assertEquals(-1, ArrayUtils.lastIndexOf(null, "0"));
        assertEquals(5, ArrayUtils.lastIndexOf(array, "0"));
        assertEquals(1, ArrayUtils.lastIndexOf(array, "1"));
        assertEquals(2, ArrayUtils.lastIndexOf(array, "2"));
        assertEquals(3, ArrayUtils.lastIndexOf(array, "3"));
        assertEquals(4, ArrayUtils.lastIndexOf(array, null));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, "notInArray"));
    }

    public void testLastIndexOfWithStartIndex() {
        Object[] array = new Object[] { "0", "1", "2", "3", null, "0" };
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

    public void testContains() {
        Object[] array = new Object[] { "0", "1", "2", "3", null, "0" };
        assertEquals(false, ArrayUtils.contains(null, null));
        assertEquals(false, ArrayUtils.contains(null, "1"));
        assertEquals(true, ArrayUtils.contains(array, "0"));
        assertEquals(true, ArrayUtils.contains(array, "1"));
        assertEquals(true, ArrayUtils.contains(array, "2"));
        assertEquals(true, ArrayUtils.contains(array, "3"));
        assertEquals(true, ArrayUtils.contains(array, null));
        assertEquals(false, ArrayUtils.contains(array, "notInArray"));
    }
    
    //-----------------------------------------------------------------------
    public void testIndexOfLong() {
        long[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, 0));
        array = new long[] { 0, 1, 2, 3, 0 };
        assertEquals(0, ArrayUtils.indexOf(array, 0));
        assertEquals(1, ArrayUtils.indexOf(array, 1));
        assertEquals(2, ArrayUtils.indexOf(array, 2));
        assertEquals(3, ArrayUtils.indexOf(array, 3));
        assertEquals(-1, ArrayUtils.indexOf(array, 99));
    }

    public void testIndexOfLongWithStartIndex() {
        long[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, 0, 2));
        array = new long[] { 0, 1, 2, 3, 0 };
        assertEquals(4, ArrayUtils.indexOf(array, 0, 2));
        assertEquals(-1, ArrayUtils.indexOf(array, 1, 2));
        assertEquals(2, ArrayUtils.indexOf(array, 2, 2));
        assertEquals(3, ArrayUtils.indexOf(array, 3, 2));
        assertEquals(3, ArrayUtils.indexOf(array, 3, -1));
        assertEquals(-1, ArrayUtils.indexOf(array, 99, 0));
        assertEquals(-1, ArrayUtils.indexOf(array, 0, 6));
    }

    public void testLastIndexOfLong() {
        long[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 0));
        array = new long[] { 0, 1, 2, 3, 0 };
        assertEquals(4, ArrayUtils.lastIndexOf(array, 0));
        assertEquals(1, ArrayUtils.lastIndexOf(array, 1));
        assertEquals(2, ArrayUtils.lastIndexOf(array, 2));
        assertEquals(3, ArrayUtils.lastIndexOf(array, 3));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 99));
    }

    public void testLastIndexOfLongWithStartIndex() {
        long[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 0, 2));
        array = new long[] { 0, 1, 2, 3, 0 };
        assertEquals(0, ArrayUtils.lastIndexOf(array, 0, 2));
        assertEquals(1, ArrayUtils.lastIndexOf(array, 1, 2));
        assertEquals(2, ArrayUtils.lastIndexOf(array, 2, 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 3, 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 3, -1));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 99, 4));
        assertEquals(4, ArrayUtils.lastIndexOf(array, 0, 88));
    }

    public void testContainsLong() {
        long[] array = null;
        assertEquals(false, ArrayUtils.contains(array, 1));
        array = new long[] { 0, 1, 2, 3, 0 };
        assertEquals(true, ArrayUtils.contains(array, 0));
        assertEquals(true, ArrayUtils.contains(array, 1));
        assertEquals(true, ArrayUtils.contains(array, 2));
        assertEquals(true, ArrayUtils.contains(array, 3));
        assertEquals(false, ArrayUtils.contains(array, 99));
    }
    
    //-----------------------------------------------------------------------
    public void testIndexOfInt() {
        int[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, 0));
        array = new int[] { 0, 1, 2, 3, 0 };
        assertEquals(0, ArrayUtils.indexOf(array, 0));
        assertEquals(1, ArrayUtils.indexOf(array, 1));
        assertEquals(2, ArrayUtils.indexOf(array, 2));
        assertEquals(3, ArrayUtils.indexOf(array, 3));
        assertEquals(-1, ArrayUtils.indexOf(array, 99));
    }

    public void testIndexOfIntWithStartIndex() {
        int[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, 0, 2));
        array = new int[] { 0, 1, 2, 3, 0 };
        assertEquals(4, ArrayUtils.indexOf(array, 0, 2));
        assertEquals(-1, ArrayUtils.indexOf(array, 1, 2));
        assertEquals(2, ArrayUtils.indexOf(array, 2, 2));
        assertEquals(3, ArrayUtils.indexOf(array, 3, 2));
        assertEquals(3, ArrayUtils.indexOf(array, 3, -1));
        assertEquals(-1, ArrayUtils.indexOf(array, 99, 0));
        assertEquals(-1, ArrayUtils.indexOf(array, 0, 6));
    }

    public void testLastIndexOfInt() {
        int[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 0));
        array = new int[] { 0, 1, 2, 3, 0 };
        assertEquals(4, ArrayUtils.lastIndexOf(array, 0));
        assertEquals(1, ArrayUtils.lastIndexOf(array, 1));
        assertEquals(2, ArrayUtils.lastIndexOf(array, 2));
        assertEquals(3, ArrayUtils.lastIndexOf(array, 3));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 99));
    }

    public void testLastIndexOfIntWithStartIndex() {
        int[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 0, 2));
        array = new int[] { 0, 1, 2, 3, 0 };
        assertEquals(0, ArrayUtils.lastIndexOf(array, 0, 2));
        assertEquals(1, ArrayUtils.lastIndexOf(array, 1, 2));
        assertEquals(2, ArrayUtils.lastIndexOf(array, 2, 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 3, 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 3, -1));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 99));
        assertEquals(4, ArrayUtils.lastIndexOf(array, 0, 88));
    }

    public void testContainsInt() {
        int[] array = null;
        assertEquals(false, ArrayUtils.contains(array, 1));
        array = new int[] { 0, 1, 2, 3, 0 };
        assertEquals(true, ArrayUtils.contains(array, 0));
        assertEquals(true, ArrayUtils.contains(array, 1));
        assertEquals(true, ArrayUtils.contains(array, 2));
        assertEquals(true, ArrayUtils.contains(array, 3));
        assertEquals(false, ArrayUtils.contains(array, 99));
    }
    
    //-----------------------------------------------------------------------
    public void testIndexOfShort() {
        short[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, (short) 0));
        array = new short[] { 0, 1, 2, 3, 0 };
        assertEquals(0, ArrayUtils.indexOf(array, (short) 0));
        assertEquals(1, ArrayUtils.indexOf(array, (short) 1));
        assertEquals(2, ArrayUtils.indexOf(array, (short) 2));
        assertEquals(3, ArrayUtils.indexOf(array, (short) 3));
        assertEquals(-1, ArrayUtils.indexOf(array, (short) 99));
    }

    public void testIndexOfShortWithStartIndex() {
        short[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, (short) 0, 2));
        array = new short[] { 0, 1, 2, 3, 0 };
        assertEquals(4, ArrayUtils.indexOf(array, (short) 0, 2));
        assertEquals(-1, ArrayUtils.indexOf(array, (short) 1, 2));
        assertEquals(2, ArrayUtils.indexOf(array, (short) 2, 2));
        assertEquals(3, ArrayUtils.indexOf(array, (short) 3, 2));
        assertEquals(3, ArrayUtils.indexOf(array, (short) 3, -1));
        assertEquals(-1, ArrayUtils.indexOf(array, (short) 99, 0));
        assertEquals(-1, ArrayUtils.indexOf(array, (short) 0, 6));
    }

    public void testLastIndexOfShort() {
        short[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (short) 0));
        array = new short[] { 0, 1, 2, 3, 0 };
        assertEquals(4, ArrayUtils.lastIndexOf(array, (short) 0));
        assertEquals(1, ArrayUtils.lastIndexOf(array, (short) 1));
        assertEquals(2, ArrayUtils.lastIndexOf(array, (short) 2));
        assertEquals(3, ArrayUtils.lastIndexOf(array, (short) 3));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (short) 99));
    }

    public void testLastIndexOfShortWithStartIndex() {
        short[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (short) 0, 2));
        array = new short[] { 0, 1, 2, 3, 0 };
        assertEquals(0, ArrayUtils.lastIndexOf(array, (short) 0, 2));
        assertEquals(1, ArrayUtils.lastIndexOf(array, (short) 1, 2));
        assertEquals(2, ArrayUtils.lastIndexOf(array, (short) 2, 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (short) 3, 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (short) 3, -1));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (short) 99));
        assertEquals(4, ArrayUtils.lastIndexOf(array, (short) 0, 88));
    }

    public void testContainsShort() {
        short[] array = null;
        assertEquals(false, ArrayUtils.contains(array, (short) 1));
        array = new short[] { 0, 1, 2, 3, 0 };
        assertEquals(true, ArrayUtils.contains(array, (short) 0));
        assertEquals(true, ArrayUtils.contains(array, (short) 1));
        assertEquals(true, ArrayUtils.contains(array, (short) 2));
        assertEquals(true, ArrayUtils.contains(array, (short) 3));
        assertEquals(false, ArrayUtils.contains(array, (short) 99));
    }
    
    //-----------------------------------------------------------------------
    public void testIndexOfChar() {
        char[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, 'a'));
        array = new char[] { 'a', 'b', 'c', 'd', 'a' };
        assertEquals(0, ArrayUtils.indexOf(array, 'a'));
        assertEquals(1, ArrayUtils.indexOf(array, 'b'));
        assertEquals(2, ArrayUtils.indexOf(array, 'c'));
        assertEquals(3, ArrayUtils.indexOf(array, 'd'));
        assertEquals(-1, ArrayUtils.indexOf(array, 'e'));
    }

    public void testIndexOfCharWithStartIndex() {
        char[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, 'a', 2));
        array = new char[] { 'a', 'b', 'c', 'd', 'a' };
        assertEquals(4, ArrayUtils.indexOf(array, 'a', 2));
        assertEquals(-1, ArrayUtils.indexOf(array, 'b', 2));
        assertEquals(2, ArrayUtils.indexOf(array, 'c', 2));
        assertEquals(3, ArrayUtils.indexOf(array, 'd', 2));
        assertEquals(3, ArrayUtils.indexOf(array, 'd', -1));
        assertEquals(-1, ArrayUtils.indexOf(array, 'e', 0));
        assertEquals(-1, ArrayUtils.indexOf(array, 'a', 6));
    }

    public void testLastIndexOfChar() {
        char[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 'a'));
        array = new char[] { 'a', 'b', 'c', 'd', 'a' };
        assertEquals(4, ArrayUtils.lastIndexOf(array, 'a'));
        assertEquals(1, ArrayUtils.lastIndexOf(array, 'b'));
        assertEquals(2, ArrayUtils.lastIndexOf(array, 'c'));
        assertEquals(3, ArrayUtils.lastIndexOf(array, 'd'));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 'e'));
    }

    public void testLastIndexOfCharWithStartIndex() {
        char[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 'a', 2));
        array = new char[] { 'a', 'b', 'c', 'd', 'a' };
        assertEquals(0, ArrayUtils.lastIndexOf(array, 'a', 2));
        assertEquals(1, ArrayUtils.lastIndexOf(array, 'b', 2));
        assertEquals(2, ArrayUtils.lastIndexOf(array, 'c', 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 'd', 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 'd', -1));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, 'e'));
        assertEquals(4, ArrayUtils.lastIndexOf(array, 'a', 88));
    }

    public void testContainsChar() {
        char[] array = null;
        assertEquals(false, ArrayUtils.contains(array, 'b'));
        array = new char[] { 'a', 'b', 'c', 'd', 'a' };
        assertEquals(true, ArrayUtils.contains(array, 'a'));
        assertEquals(true, ArrayUtils.contains(array, 'b'));
        assertEquals(true, ArrayUtils.contains(array, 'c'));
        assertEquals(true, ArrayUtils.contains(array, 'd'));
        assertEquals(false, ArrayUtils.contains(array, 'e'));
    }
    
    //-----------------------------------------------------------------------
    public void testIndexOfByte() {
        byte[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, (byte) 0));
        array = new byte[] { 0, 1, 2, 3, 0 };
        assertEquals(0, ArrayUtils.indexOf(array, (byte) 0));
        assertEquals(1, ArrayUtils.indexOf(array, (byte) 1));
        assertEquals(2, ArrayUtils.indexOf(array, (byte) 2));
        assertEquals(3, ArrayUtils.indexOf(array, (byte) 3));
        assertEquals(-1, ArrayUtils.indexOf(array, (byte) 99));
    }

    public void testIndexOfByteWithStartIndex() {
        byte[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, (byte) 0, 2));
        array = new byte[] { 0, 1, 2, 3, 0 };
        assertEquals(4, ArrayUtils.indexOf(array, (byte) 0, 2));
        assertEquals(-1, ArrayUtils.indexOf(array, (byte) 1, 2));
        assertEquals(2, ArrayUtils.indexOf(array, (byte) 2, 2));
        assertEquals(3, ArrayUtils.indexOf(array, (byte) 3, 2));
        assertEquals(3, ArrayUtils.indexOf(array, (byte) 3, -1));
        assertEquals(-1, ArrayUtils.indexOf(array, (byte) 99, 0));
        assertEquals(-1, ArrayUtils.indexOf(array, (byte) 0, 6));
    }

    public void testLastIndexOfByte() {
        byte[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (byte) 0));
        array = new byte[] { 0, 1, 2, 3, 0 };
        assertEquals(4, ArrayUtils.lastIndexOf(array, (byte) 0));
        assertEquals(1, ArrayUtils.lastIndexOf(array, (byte) 1));
        assertEquals(2, ArrayUtils.lastIndexOf(array, (byte) 2));
        assertEquals(3, ArrayUtils.lastIndexOf(array, (byte) 3));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (byte) 99));
    }

    public void testLastIndexOfByteWithStartIndex() {
        byte[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (byte) 0, 2));
        array = new byte[] { 0, 1, 2, 3, 0 };
        assertEquals(0, ArrayUtils.lastIndexOf(array, (byte) 0, 2));
        assertEquals(1, ArrayUtils.lastIndexOf(array, (byte) 1, 2));
        assertEquals(2, ArrayUtils.lastIndexOf(array, (byte) 2, 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (byte) 3, 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (byte) 3, -1));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (byte) 99));
        assertEquals(4, ArrayUtils.lastIndexOf(array, (byte) 0, 88));
    }

    public void testContainsByte() {
        byte[] array = null;
        assertEquals(false, ArrayUtils.contains(array, (byte) 1));
        array = new byte[] { 0, 1, 2, 3, 0 };
        assertEquals(true, ArrayUtils.contains(array, (byte) 0));
        assertEquals(true, ArrayUtils.contains(array, (byte) 1));
        assertEquals(true, ArrayUtils.contains(array, (byte) 2));
        assertEquals(true, ArrayUtils.contains(array, (byte) 3));
        assertEquals(false, ArrayUtils.contains(array, (byte) 99));
    }
    
    //-----------------------------------------------------------------------
    @SuppressWarnings("cast")
    public void testIndexOfDouble() {
        double[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, (double) 0));
        array = new double[0];
        assertEquals(-1, ArrayUtils.indexOf(array, (double) 0));
        array = new double[] { 0, 1, 2, 3, 0 };
        assertEquals(0, ArrayUtils.indexOf(array, (double) 0));
        assertEquals(1, ArrayUtils.indexOf(array, (double) 1));
        assertEquals(2, ArrayUtils.indexOf(array, (double) 2));
        assertEquals(3, ArrayUtils.indexOf(array, (double) 3));
        assertEquals(3, ArrayUtils.indexOf(array, (double) 3, -1));
        assertEquals(-1, ArrayUtils.indexOf(array, (double) 99));
    }

    @SuppressWarnings("cast")
    public void testIndexOfDoubleTolerance() {
        double[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, (double) 0, (double) 0));
        array = new double[0];
        assertEquals(-1, ArrayUtils.indexOf(array, (double) 0, (double) 0));
        array = new double[] { 0, 1, 2, 3, 0 };
        assertEquals(0, ArrayUtils.indexOf(array, (double) 0, (double) 0.3));
        assertEquals(2, ArrayUtils.indexOf(array, (double) 2.2, (double) 0.35));
        assertEquals(3, ArrayUtils.indexOf(array, (double) 4.15, (double) 2.0));
        assertEquals(1, ArrayUtils.indexOf(array, (double) 1.00001324, (double) 0.0001));
    }

    @SuppressWarnings("cast")
    public void testIndexOfDoubleWithStartIndex() {
        double[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, (double) 0, 2));
        array = new double[0];
        assertEquals(-1, ArrayUtils.indexOf(array, (double) 0, 2));
        array = new double[] { 0, 1, 2, 3, 0 };
        assertEquals(4, ArrayUtils.indexOf(array, (double) 0, 2));
        assertEquals(-1, ArrayUtils.indexOf(array, (double) 1, 2));
        assertEquals(2, ArrayUtils.indexOf(array, (double) 2, 2));
        assertEquals(3, ArrayUtils.indexOf(array, (double) 3, 2));
        assertEquals(-1, ArrayUtils.indexOf(array, (double) 99, 0));
        assertEquals(-1, ArrayUtils.indexOf(array, (double) 0, 6));
    }
    
    @SuppressWarnings("cast")
    public void testIndexOfDoubleWithStartIndexTolerance() {
        double[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, (double) 0, 2, (double) 0));
        array = new double[0];
        assertEquals(-1, ArrayUtils.indexOf(array, (double) 0, 2, (double) 0));
        array = new double[] { 0, 1, 2, 3, 0 };
        assertEquals(-1, ArrayUtils.indexOf(array, (double) 0, 99, (double) 0.3));
        assertEquals(0, ArrayUtils.indexOf(array, (double) 0, 0, (double) 0.3));
        assertEquals(4, ArrayUtils.indexOf(array, (double) 0, 3, (double) 0.3));
        assertEquals(2, ArrayUtils.indexOf(array, (double) 2.2, 0, (double) 0.35));
        assertEquals(3, ArrayUtils.indexOf(array, (double) 4.15, 0, (double) 2.0));
        assertEquals(1, ArrayUtils.indexOf(array, (double) 1.00001324, 0, (double) 0.0001));
        assertEquals(3, ArrayUtils.indexOf(array, (double) 4.15, -1, (double) 2.0));
        assertEquals(1, ArrayUtils.indexOf(array, (double) 1.00001324, -300, (double) 0.0001));
    }

    @SuppressWarnings("cast")
    public void testLastIndexOfDouble() {
        double[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (double) 0));
        array = new double[0];
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (double) 0));
        array = new double[] { 0, 1, 2, 3, 0 };
        assertEquals(4, ArrayUtils.lastIndexOf(array, (double) 0));
        assertEquals(1, ArrayUtils.lastIndexOf(array, (double) 1));
        assertEquals(2, ArrayUtils.lastIndexOf(array, (double) 2));
        assertEquals(3, ArrayUtils.lastIndexOf(array, (double) 3));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (double) 99));
    }

    @SuppressWarnings("cast")
    public void testLastIndexOfDoubleTolerance() {
        double[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (double) 0, (double) 0));
        array = new double[0];
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (double) 0, (double) 0));
        array = new double[] { 0, 1, 2, 3, 0 };
        assertEquals(4, ArrayUtils.lastIndexOf(array, (double) 0, (double) 0.3));
        assertEquals(2, ArrayUtils.lastIndexOf(array, (double) 2.2, (double) 0.35));
        assertEquals(3, ArrayUtils.lastIndexOf(array, (double) 4.15, (double) 2.0));
        assertEquals(1, ArrayUtils.lastIndexOf(array, (double) 1.00001324, (double) 0.0001));
    }

    @SuppressWarnings("cast")
    public void testLastIndexOfDoubleWithStartIndex() {
        double[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (double) 0, 2));
        array = new double[0];
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (double) 0, 2));
        array = new double[] { 0, 1, 2, 3, 0 };
        assertEquals(0, ArrayUtils.lastIndexOf(array, (double) 0, 2));
        assertEquals(1, ArrayUtils.lastIndexOf(array, (double) 1, 2));
        assertEquals(2, ArrayUtils.lastIndexOf(array, (double) 2, 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (double) 3, 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (double) 3, -1));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (double) 99));
        assertEquals(4, ArrayUtils.lastIndexOf(array, (double) 0, 88));
    }

    @SuppressWarnings("cast")
    public void testLastIndexOfDoubleWithStartIndexTolerance() {
        double[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (double) 0, 2, (double) 0));
        array = new double[0];
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (double) 0, 2, (double) 0));
        array = new double[] { (double) 3 };
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (double) 1, 0, (double) 0));
        array = new double[] { 0, 1, 2, 3, 0 };
        assertEquals(4, ArrayUtils.lastIndexOf(array, (double) 0, 99, (double) 0.3));
        assertEquals(0, ArrayUtils.lastIndexOf(array, (double) 0, 3, (double) 0.3));
        assertEquals(2, ArrayUtils.lastIndexOf(array, (double) 2.2, 3, (double) 0.35));
        assertEquals(3, ArrayUtils.lastIndexOf(array, (double) 4.15, array.length, (double) 2.0));
        assertEquals(1, ArrayUtils.lastIndexOf(array, (double) 1.00001324, array.length, (double) 0.0001));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (double) 4.15, -200, (double) 2.0));
    }

    @SuppressWarnings("cast")
    public void testContainsDouble() {
        double[] array = null;
        assertEquals(false, ArrayUtils.contains(array, (double) 1));
        array = new double[] { 0, 1, 2, 3, 0 };
        assertEquals(true, ArrayUtils.contains(array, (double) 0));
        assertEquals(true, ArrayUtils.contains(array, (double) 1));
        assertEquals(true, ArrayUtils.contains(array, (double) 2));
        assertEquals(true, ArrayUtils.contains(array, (double) 3));
        assertEquals(false, ArrayUtils.contains(array, (double) 99));
    }

    @SuppressWarnings("cast")
    public void testContainsDoubleTolerance() {
        double[] array = null;
        assertEquals(false, ArrayUtils.contains(array, (double) 1, (double) 0));
        array = new double[] { 0, 1, 2, 3, 0 };
        assertEquals(false, ArrayUtils.contains(array, (double) 4.0, (double) 0.33));
        assertEquals(false, ArrayUtils.contains(array, (double) 2.5, (double) 0.49));
        assertEquals(true, ArrayUtils.contains(array, (double) 2.5, (double) 0.50));
        assertEquals(true, ArrayUtils.contains(array, (double) 2.5, (double) 0.51));
    }
    
    //-----------------------------------------------------------------------
    @SuppressWarnings("cast")
    public void testIndexOfFloat() {
        float[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, (float) 0));
        array = new float[0];
        assertEquals(-1, ArrayUtils.indexOf(array, (float) 0));
        array = new float[] { 0, 1, 2, 3, 0 };
        assertEquals(0, ArrayUtils.indexOf(array, (float) 0));
        assertEquals(1, ArrayUtils.indexOf(array, (float) 1));
        assertEquals(2, ArrayUtils.indexOf(array, (float) 2));
        assertEquals(3, ArrayUtils.indexOf(array, (float) 3));
        assertEquals(-1, ArrayUtils.indexOf(array, (float) 99));
    }

    @SuppressWarnings("cast")
    public void testIndexOfFloatWithStartIndex() {
        float[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, (float) 0, 2));
        array = new float[0];
        assertEquals(-1, ArrayUtils.indexOf(array, (float) 0, 2));
        array = new float[] { 0, 1, 2, 3, 0 };
        assertEquals(4, ArrayUtils.indexOf(array, (float) 0, 2));
        assertEquals(-1, ArrayUtils.indexOf(array, (float) 1, 2));
        assertEquals(2, ArrayUtils.indexOf(array, (float) 2, 2));
        assertEquals(3, ArrayUtils.indexOf(array, (float) 3, 2));
        assertEquals(3, ArrayUtils.indexOf(array, (float) 3, -1));
        assertEquals(-1, ArrayUtils.indexOf(array, (float) 99, 0));
        assertEquals(-1, ArrayUtils.indexOf(array, (float) 0, 6));
    }

    @SuppressWarnings("cast")
    public void testLastIndexOfFloat() {
        float[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (float) 0));
        array = new float[0];
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (float) 0));
        array = new float[] { 0, 1, 2, 3, 0 };
        assertEquals(4, ArrayUtils.lastIndexOf(array, (float) 0));
        assertEquals(1, ArrayUtils.lastIndexOf(array, (float) 1));
        assertEquals(2, ArrayUtils.lastIndexOf(array, (float) 2));
        assertEquals(3, ArrayUtils.lastIndexOf(array, (float) 3));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (float) 99));
    }

    @SuppressWarnings("cast")
    public void testLastIndexOfFloatWithStartIndex() {
        float[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (float) 0, 2));
        array = new float[0];
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (float) 0, 2));
        array = new float[] { 0, 1, 2, 3, 0 };
        assertEquals(0, ArrayUtils.lastIndexOf(array, (float) 0, 2));
        assertEquals(1, ArrayUtils.lastIndexOf(array, (float) 1, 2));
        assertEquals(2, ArrayUtils.lastIndexOf(array, (float) 2, 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (float) 3, 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (float) 3, -1));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, (float) 99));
        assertEquals(4, ArrayUtils.lastIndexOf(array, (float) 0, 88));
    }

    @SuppressWarnings("cast")
    public void testContainsFloat() {
        float[] array = null;
        assertEquals(false, ArrayUtils.contains(array, (float) 1));
        array = new float[] { 0, 1, 2, 3, 0 };
        assertEquals(true, ArrayUtils.contains(array, (float) 0));
        assertEquals(true, ArrayUtils.contains(array, (float) 1));
        assertEquals(true, ArrayUtils.contains(array, (float) 2));
        assertEquals(true, ArrayUtils.contains(array, (float) 3));
        assertEquals(false, ArrayUtils.contains(array, (float) 99));
    }
    
    //-----------------------------------------------------------------------
    public void testIndexOfBoolean() {
        boolean[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, true));
        array = new boolean[0];
        assertEquals(-1, ArrayUtils.indexOf(array, true));
        array = new boolean[] { true, false, true };
        assertEquals(0, ArrayUtils.indexOf(array, true));
        assertEquals(1, ArrayUtils.indexOf(array, false));
        array = new boolean[] { true, true };
        assertEquals(-1, ArrayUtils.indexOf(array, false));
    }

    public void testIndexOfBooleanWithStartIndex() {
        boolean[] array = null;
        assertEquals(-1, ArrayUtils.indexOf(array, true, 2));
        array = new boolean[0];
        assertEquals(-1, ArrayUtils.indexOf(array, true, 2));
        array = new boolean[] { true, false, true };
        assertEquals(2, ArrayUtils.indexOf(array, true, 1));
        assertEquals(-1, ArrayUtils.indexOf(array, false, 2));
        assertEquals(1, ArrayUtils.indexOf(array, false, 0));
        assertEquals(1, ArrayUtils.indexOf(array, false, -1));
        array = new boolean[] { true, true };
        assertEquals(-1, ArrayUtils.indexOf(array, false, 0));
        assertEquals(-1, ArrayUtils.indexOf(array, false, -1));
    }

    public void testLastIndexOfBoolean() {
        boolean[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, true));
        array = new boolean[0];
        assertEquals(-1, ArrayUtils.lastIndexOf(array, true));
        array = new boolean[] { true, false, true };
        assertEquals(2, ArrayUtils.lastIndexOf(array, true));
        assertEquals(1, ArrayUtils.lastIndexOf(array, false));
        array = new boolean[] { true, true };
        assertEquals(-1, ArrayUtils.lastIndexOf(array, false));
    }

    public void testLastIndexOfBooleanWithStartIndex() {
        boolean[] array = null;
        assertEquals(-1, ArrayUtils.lastIndexOf(array, true, 2));
        array = new boolean[0];
        assertEquals(-1, ArrayUtils.lastIndexOf(array, true, 2));
        array = new boolean[] { true, false, true };
        assertEquals(2, ArrayUtils.lastIndexOf(array, true, 2));
        assertEquals(0, ArrayUtils.lastIndexOf(array, true, 1));
        assertEquals(1, ArrayUtils.lastIndexOf(array, false, 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, true, -1));
        array = new boolean[] { true, true };
        assertEquals(-1, ArrayUtils.lastIndexOf(array, false, 2));
        assertEquals(-1, ArrayUtils.lastIndexOf(array, true, -1));
    }

    public void testContainsBoolean() {
        boolean[] array = null;
        assertEquals(false, ArrayUtils.contains(array, true));
        array = new boolean[] { true, false, true };
        assertEquals(true, ArrayUtils.contains(array, true));
        assertEquals(true, ArrayUtils.contains(array, false));
        array = new boolean[] { true, true };
        assertEquals(true, ArrayUtils.contains(array, true));
        assertEquals(false, ArrayUtils.contains(array, false));
    }
    
    // testToPrimitive/Object for boolean
    //  -----------------------------------------------------------------------
    public void testToPrimitive_boolean() {
        final Boolean[] b = null;
        assertEquals(null, ArrayUtils.toPrimitive(b));
        assertSame(ArrayUtils.EMPTY_BOOLEAN_ARRAY, ArrayUtils.toPrimitive(new Boolean[0]));
        assertTrue(Arrays.equals(
            new boolean[] {true, false, true},
            ArrayUtils.toPrimitive(new Boolean[] {Boolean.TRUE, Boolean.FALSE, Boolean.TRUE}))
        );

        try {
            ArrayUtils.toPrimitive(new Boolean[] {Boolean.TRUE, null});
            fail();
        } catch (NullPointerException ex) {}
    }

    public void testToPrimitive_boolean_boolean() {
        assertEquals(null, ArrayUtils.toPrimitive(null, false));
        assertSame(ArrayUtils.EMPTY_BOOLEAN_ARRAY, ArrayUtils.toPrimitive(new Boolean[0], false));
        assertTrue(Arrays.equals(
            new boolean[] {true, false, true},
            ArrayUtils.toPrimitive(new Boolean[] {Boolean.TRUE, Boolean.FALSE, Boolean.TRUE}, false))
        );
        assertTrue(Arrays.equals(
            new boolean[] {true, false, false},
            ArrayUtils.toPrimitive(new Boolean[] {Boolean.TRUE, null, Boolean.FALSE}, false))
        );
        assertTrue(Arrays.equals(
            new boolean[] {true, true, false},
            ArrayUtils.toPrimitive(new Boolean[] {Boolean.TRUE, null, Boolean.FALSE}, true))
        );
    }

    public void testToObject_boolean() {
        final boolean[] b = null;
        assertEquals(null, ArrayUtils.toObject(b));
        assertSame(ArrayUtils.EMPTY_BOOLEAN_OBJECT_ARRAY, ArrayUtils.toObject(new boolean[0]));
        assertTrue(Arrays.equals(
            new Boolean[] {Boolean.TRUE, Boolean.FALSE, Boolean.TRUE},
            ArrayUtils.toObject(new boolean[] {true, false, true}))
        );
    }

    // testToPrimitive/Object for byte
    //  -----------------------------------------------------------------------
    public void testToPrimitive_char() {
        final Character[] b = null;
        assertEquals(null, ArrayUtils.toPrimitive(b));
        
        assertSame(ArrayUtils.EMPTY_CHAR_ARRAY, ArrayUtils.toPrimitive(new Character[0]));
        
        assertTrue(Arrays.equals(
            new char[] {Character.MIN_VALUE, Character.MAX_VALUE, '0'},
            ArrayUtils.toPrimitive(new Character[] {new Character(Character.MIN_VALUE), 
                new Character(Character.MAX_VALUE), new Character('0')}))
        );

        try {
            ArrayUtils.toPrimitive(new Character[] {new Character(Character.MIN_VALUE), null});
            fail();
        } catch (NullPointerException ex) {}
    }

    public void testToPrimitive_char_char() {
        final Character[] b = null;
        assertEquals(null, ArrayUtils.toPrimitive(b, Character.MIN_VALUE));
        
        assertSame(ArrayUtils.EMPTY_CHAR_ARRAY, 
            ArrayUtils.toPrimitive(new Character[0], (char)0));
        
        assertTrue(Arrays.equals(
            new char[] {Character.MIN_VALUE, Character.MAX_VALUE, '0'},
            ArrayUtils.toPrimitive(new Character[] {new Character(Character.MIN_VALUE), 
                new Character(Character.MAX_VALUE), new Character('0')}, 
                Character.MIN_VALUE))
        );
        
        assertTrue(Arrays.equals(
            new char[] {Character.MIN_VALUE, Character.MAX_VALUE, '0'},
            ArrayUtils.toPrimitive(new Character[] {new Character(Character.MIN_VALUE), null, 
                new Character('0')}, Character.MAX_VALUE))
        );
    }

    public void testToObject_char() {
        final char[] b = null;
        assertEquals(null, ArrayUtils.toObject(b));
        
        assertSame(ArrayUtils.EMPTY_CHARACTER_OBJECT_ARRAY, 
            ArrayUtils.toObject(new char[0]));
        
        assertTrue(Arrays.equals(
            new Character[] {new Character(Character.MIN_VALUE), 
                new Character(Character.MAX_VALUE), new Character('0')},
                ArrayUtils.toObject(new char[] {Character.MIN_VALUE, Character.MAX_VALUE, 
                '0'} ))
        );
    }
    
    // testToPrimitive/Object for byte
    //  -----------------------------------------------------------------------
    public void testToPrimitive_byte() {
        final Byte[] b = null;
        assertEquals(null, ArrayUtils.toPrimitive(b));
        
        assertSame(ArrayUtils.EMPTY_BYTE_ARRAY, ArrayUtils.toPrimitive(new Byte[0]));
        
        assertTrue(Arrays.equals(
            new byte[] {Byte.MIN_VALUE, Byte.MAX_VALUE, (byte)9999999},
            ArrayUtils.toPrimitive(new Byte[] {Byte.valueOf(Byte.MIN_VALUE), 
                Byte.valueOf(Byte.MAX_VALUE), Byte.valueOf((byte)9999999)}))
        );

        try {
            ArrayUtils.toPrimitive(new Byte[] {Byte.valueOf(Byte.MIN_VALUE), null});
            fail();
        } catch (NullPointerException ex) {}
    }

    public void testToPrimitive_byte_byte() {
        final Byte[] b = null;
        assertEquals(null, ArrayUtils.toPrimitive(b, Byte.MIN_VALUE));
        
        assertSame(ArrayUtils.EMPTY_BYTE_ARRAY, 
            ArrayUtils.toPrimitive(new Byte[0], (byte)1));
        
        assertTrue(Arrays.equals(
            new byte[] {Byte.MIN_VALUE, Byte.MAX_VALUE, (byte)9999999},
            ArrayUtils.toPrimitive(new Byte[] {Byte.valueOf(Byte.MIN_VALUE), 
                Byte.valueOf(Byte.MAX_VALUE), Byte.valueOf((byte)9999999)}, 
                Byte.MIN_VALUE))
        );
        
        assertTrue(Arrays.equals(
            new byte[] {Byte.MIN_VALUE, Byte.MAX_VALUE, (byte)9999999},
            ArrayUtils.toPrimitive(new Byte[] {Byte.valueOf(Byte.MIN_VALUE), null, 
                Byte.valueOf((byte)9999999)}, Byte.MAX_VALUE))
        );
    }

    public void testToObject_byte() {
        final byte[] b = null;
        assertEquals(null, ArrayUtils.toObject(b));
        
        assertSame(ArrayUtils.EMPTY_BYTE_OBJECT_ARRAY, 
            ArrayUtils.toObject(new byte[0]));
        
        assertTrue(Arrays.equals(
            new Byte[] {Byte.valueOf(Byte.MIN_VALUE), 
                Byte.valueOf(Byte.MAX_VALUE), Byte.valueOf((byte)9999999)},
                ArrayUtils.toObject(new byte[] {Byte.MIN_VALUE, Byte.MAX_VALUE, 
                (byte)9999999}))
        );
    }

    // testToPrimitive/Object for short
    //  -----------------------------------------------------------------------
    public void testToPrimitive_short() {
        final Short[] b = null;
        assertEquals(null, ArrayUtils.toPrimitive(b));
        
        assertSame(ArrayUtils.EMPTY_SHORT_ARRAY, ArrayUtils.toPrimitive(new Short[0]));
        
        assertTrue(Arrays.equals(
            new short[] {Short.MIN_VALUE, Short.MAX_VALUE, (short)9999999},
            ArrayUtils.toPrimitive(new Short[] {new Short(Short.MIN_VALUE), 
                new Short(Short.MAX_VALUE), new Short((short)9999999)}))
        );

        try {
            ArrayUtils.toPrimitive(new Short[] {new Short(Short.MIN_VALUE), null});
            fail();
        } catch (NullPointerException ex) {}
    }

    public void testToPrimitive_short_short() {
        final Short[] s = null;
        assertEquals(null, ArrayUtils.toPrimitive(s, Short.MIN_VALUE));
        
        assertSame(ArrayUtils.EMPTY_SHORT_ARRAY, ArrayUtils.toPrimitive(new Short[0], 
        Short.MIN_VALUE));
        
        assertTrue(Arrays.equals(
            new short[] {Short.MIN_VALUE, Short.MAX_VALUE, (short)9999999},
            ArrayUtils.toPrimitive(new Short[] {new Short(Short.MIN_VALUE), 
                new Short(Short.MAX_VALUE), new Short((short)9999999)}, Short.MIN_VALUE))
        );
        
        assertTrue(Arrays.equals(
            new short[] {Short.MIN_VALUE, Short.MAX_VALUE, (short)9999999},
            ArrayUtils.toPrimitive(new Short[] {new Short(Short.MIN_VALUE), null, 
                new Short((short)9999999)}, Short.MAX_VALUE))
        );
    }

    public void testToObject_short() {
        final short[] b = null;
        assertEquals(null, ArrayUtils.toObject(b));
        
        assertSame(ArrayUtils.EMPTY_SHORT_OBJECT_ARRAY, 
        ArrayUtils.toObject(new short[0]));
        
        assertTrue(Arrays.equals(
            new Short[] {new Short(Short.MIN_VALUE), new Short(Short.MAX_VALUE), 
                new Short((short)9999999)},
            ArrayUtils.toObject(new short[] {Short.MIN_VALUE, Short.MAX_VALUE, 
                (short)9999999}))
        );
    }

    //  testToPrimitive/Object for int
    //  -----------------------------------------------------------------------
     public void testToPrimitive_int() {
         final Integer[] b = null;
         assertEquals(null, ArrayUtils.toPrimitive(b));
         assertSame(ArrayUtils.EMPTY_INT_ARRAY, ArrayUtils.toPrimitive(new Integer[0]));
         assertTrue(Arrays.equals(
             new int[] {Integer.MIN_VALUE, Integer.MAX_VALUE, 9999999},
             ArrayUtils.toPrimitive(new Integer[] {new Integer(Integer.MIN_VALUE), 
                 new Integer(Integer.MAX_VALUE), new Integer(9999999)}))
         );

         try {
             ArrayUtils.toPrimitive(new Integer[] {new Integer(Integer.MIN_VALUE), null});
             fail();
         } catch (NullPointerException ex) {}
     }

     public void testToPrimitive_int_int() {
         final Long[] l = null;
         assertEquals(null, ArrayUtils.toPrimitive(l, Integer.MIN_VALUE));
         assertSame(ArrayUtils.EMPTY_INT_ARRAY, 
         ArrayUtils.toPrimitive(new Integer[0], 1));
         assertTrue(Arrays.equals(
             new int[] {Integer.MIN_VALUE, Integer.MAX_VALUE, 9999999},
             ArrayUtils.toPrimitive(new Integer[] {new Integer(Integer.MIN_VALUE), 
                 new Integer(Integer.MAX_VALUE), new Integer(9999999)},1)));
         assertTrue(Arrays.equals(
             new int[] {Integer.MIN_VALUE, Integer.MAX_VALUE, 9999999},
             ArrayUtils.toPrimitive(new Integer[] {new Integer(Integer.MIN_VALUE), 
                 null, new Integer(9999999)}, Integer.MAX_VALUE))
         );
     }
     
    public void testToPrimitive_intNull() {
        Integer[] iArray = null;
        assertEquals(null, ArrayUtils.toPrimitive(iArray, Integer.MIN_VALUE));
    }

    public void testToObject_int() {
        final int[] b = null;
        assertEquals(null, ArrayUtils.toObject(b));
    
        assertSame(
            ArrayUtils.EMPTY_INTEGER_OBJECT_ARRAY,
            ArrayUtils.toObject(new int[0]));
    
        assertTrue(
            Arrays.equals(
                new Integer[] {
                    new Integer(Integer.MIN_VALUE),
                    new Integer(Integer.MAX_VALUE),
                    new Integer(9999999)},
            ArrayUtils.toObject(
                new int[] { Integer.MIN_VALUE, Integer.MAX_VALUE, 9999999 })));
    }

    //  testToPrimitive/Object for long
    //  -----------------------------------------------------------------------
     public void testToPrimitive_long() {
         final Long[] b = null;
         assertEquals(null, ArrayUtils.toPrimitive(b));
         
         assertSame(ArrayUtils.EMPTY_LONG_ARRAY, 
            ArrayUtils.toPrimitive(new Long[0]));
         
         assertTrue(Arrays.equals(
             new long[] {Long.MIN_VALUE, Long.MAX_VALUE, 9999999},
             ArrayUtils.toPrimitive(new Long[] {new Long(Long.MIN_VALUE), 
                 new Long(Long.MAX_VALUE), new Long(9999999)}))
         );

         try {
             ArrayUtils.toPrimitive(new Long[] {new Long(Long.MIN_VALUE), null});
             fail();
         } catch (NullPointerException ex) {}
     }

     public void testToPrimitive_long_long() {
         final Long[] l = null;
         assertEquals(null, ArrayUtils.toPrimitive(l, Long.MIN_VALUE));
         
         assertSame(ArrayUtils.EMPTY_LONG_ARRAY, 
         ArrayUtils.toPrimitive(new Long[0], 1));
         
         assertTrue(Arrays.equals(
             new long[] {Long.MIN_VALUE, Long.MAX_VALUE, 9999999},
             ArrayUtils.toPrimitive(new Long[] {new Long(Long.MIN_VALUE), 
                 new Long(Long.MAX_VALUE), new Long(9999999)},1)));
         
         assertTrue(Arrays.equals(
             new long[] {Long.MIN_VALUE, Long.MAX_VALUE, 9999999},
             ArrayUtils.toPrimitive(new Long[] {new Long(Long.MIN_VALUE), 
                 null, new Long(9999999)}, Long.MAX_VALUE))
         );
     }
     
    public void testToObject_long() {
        final long[] b = null;
        assertEquals(null, ArrayUtils.toObject(b));
    
        assertSame(
            ArrayUtils.EMPTY_LONG_OBJECT_ARRAY,
            ArrayUtils.toObject(new long[0]));
    
        assertTrue(
            Arrays.equals(
                new Long[] {
                    new Long(Long.MIN_VALUE),
                    new Long(Long.MAX_VALUE),
                    new Long(9999999)},
            ArrayUtils.toObject(
                new long[] { Long.MIN_VALUE, Long.MAX_VALUE, 9999999 })));
    }

    //  testToPrimitive/Object for float
    //  -----------------------------------------------------------------------
     public void testToPrimitive_float() {
         final Float[] b = null;
         assertEquals(null, ArrayUtils.toPrimitive(b));
         
         assertSame(ArrayUtils.EMPTY_FLOAT_ARRAY, 
            ArrayUtils.toPrimitive(new Float[0]));
         
         assertTrue(Arrays.equals(
             new float[] {Float.MIN_VALUE, Float.MAX_VALUE, 9999999},
             ArrayUtils.toPrimitive(new Float[] {new Float(Float.MIN_VALUE), 
                 new Float(Float.MAX_VALUE), new Float(9999999)}))
         );

         try {
             ArrayUtils.toPrimitive(new Float[] {new Float(Float.MIN_VALUE), null});
             fail();
         } catch (NullPointerException ex) {}
     }

     public void testToPrimitive_float_float() {
         final Float[] l = null;
         assertEquals(null, ArrayUtils.toPrimitive(l, Float.MIN_VALUE));
         
         assertSame(ArrayUtils.EMPTY_FLOAT_ARRAY, 
         ArrayUtils.toPrimitive(new Float[0], 1));
         
         assertTrue(Arrays.equals(
             new float[] {Float.MIN_VALUE, Float.MAX_VALUE, 9999999},
             ArrayUtils.toPrimitive(new Float[] {new Float(Float.MIN_VALUE), 
                 new Float(Float.MAX_VALUE), new Float(9999999)},1)));
         
         assertTrue(Arrays.equals(
             new float[] {Float.MIN_VALUE, Float.MAX_VALUE, 9999999},
             ArrayUtils.toPrimitive(new Float[] {new Float(Float.MIN_VALUE), 
                 null, new Float(9999999)}, Float.MAX_VALUE))
         );
     }
     
    public void testToObject_float() {
        final float[] b = null;
        assertEquals(null, ArrayUtils.toObject(b));
    
        assertSame(
            ArrayUtils.EMPTY_FLOAT_OBJECT_ARRAY,
            ArrayUtils.toObject(new float[0]));
    
        assertTrue(
            Arrays.equals(
                new Float[] {
                    new Float(Float.MIN_VALUE),
                    new Float(Float.MAX_VALUE),
                    new Float(9999999)},
            ArrayUtils.toObject(
                new float[] { Float.MIN_VALUE, Float.MAX_VALUE, 9999999 })));
    }

    //  testToPrimitive/Object for double
    //  -----------------------------------------------------------------------
     public void testToPrimitive_double() {
         final Double[] b = null;
         assertEquals(null, ArrayUtils.toPrimitive(b));
         
         assertSame(ArrayUtils.EMPTY_DOUBLE_ARRAY, 
            ArrayUtils.toPrimitive(new Double[0]));
         
         assertTrue(Arrays.equals(
             new double[] {Double.MIN_VALUE, Double.MAX_VALUE, 9999999},
             ArrayUtils.toPrimitive(new Double[] {new Double(Double.MIN_VALUE), 
                 new Double(Double.MAX_VALUE), new Double(9999999)}))
         );

         try {
             ArrayUtils.toPrimitive(new Float[] {new Float(Float.MIN_VALUE), null});
             fail();
         } catch (NullPointerException ex) {}
     }

     public void testToPrimitive_double_double() {
         final Double[] l = null;
         assertEquals(null, ArrayUtils.toPrimitive(l, Double.MIN_VALUE));
         
         assertSame(ArrayUtils.EMPTY_DOUBLE_ARRAY, 
         ArrayUtils.toPrimitive(new Double[0], 1));
         
         assertTrue(Arrays.equals(
             new double[] {Double.MIN_VALUE, Double.MAX_VALUE, 9999999},
             ArrayUtils.toPrimitive(new Double[] {new Double(Double.MIN_VALUE), 
                 new Double(Double.MAX_VALUE), new Double(9999999)},1)));
         
         assertTrue(Arrays.equals(
             new double[] {Double.MIN_VALUE, Double.MAX_VALUE, 9999999},
             ArrayUtils.toPrimitive(new Double[] {new Double(Double.MIN_VALUE), 
                 null, new Double(9999999)}, Double.MAX_VALUE))
         );
     }
     
    public void testToObject_double() {
        final double[] b = null;
        assertEquals(null, ArrayUtils.toObject(b));
    
        assertSame(
            ArrayUtils.EMPTY_DOUBLE_OBJECT_ARRAY,
            ArrayUtils.toObject(new double[0]));
    
        assertTrue(
            Arrays.equals(
                new Double[] {
                    new Double(Double.MIN_VALUE),
                    new Double(Double.MAX_VALUE),
                    new Double(9999999)},
            ArrayUtils.toObject(
                new double[] { Double.MIN_VALUE, Double.MAX_VALUE, 9999999 })));
    }

    //-----------------------------------------------------------------------
    /**
     * Test for {@link ArrayUtils#isEmpty(java.lang.Object[])}.
     */
    public void testIsEmptyObject() {
        Object[] emptyArray = new Object[] {};
        Object[] notEmptyArray = new Object[] { new String("Value") };
        assertEquals(true, ArrayUtils.isEmpty((Object[])null));
        assertEquals(true, ArrayUtils.isEmpty(emptyArray));
        assertEquals(false, ArrayUtils.isEmpty(notEmptyArray));
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
    public void testIsEmptyPrimitives() {
        long[] emptyLongArray = new long[] {};
        long[] notEmptyLongArray = new long[] { 1L };
        assertEquals(true, ArrayUtils.isEmpty((long[])null));
        assertEquals(true, ArrayUtils.isEmpty(emptyLongArray));
        assertEquals(false, ArrayUtils.isEmpty(notEmptyLongArray));

        int[] emptyIntArray = new int[] {};
        int[] notEmptyIntArray = new int[] { 1 };
        assertEquals(true, ArrayUtils.isEmpty((int[])null));
        assertEquals(true, ArrayUtils.isEmpty(emptyIntArray));
        assertEquals(false, ArrayUtils.isEmpty(notEmptyIntArray));

        short[] emptyShortArray = new short[] {};
        short[] notEmptyShortArray = new short[] { 1 };
        assertEquals(true, ArrayUtils.isEmpty((short[])null));
        assertEquals(true, ArrayUtils.isEmpty(emptyShortArray));
        assertEquals(false, ArrayUtils.isEmpty(notEmptyShortArray));

        char[] emptyCharArray = new char[] {};
        char[] notEmptyCharArray = new char[] { 1 };
        assertEquals(true, ArrayUtils.isEmpty((char[])null));
        assertEquals(true, ArrayUtils.isEmpty(emptyCharArray));
        assertEquals(false, ArrayUtils.isEmpty(notEmptyCharArray));

        byte[] emptyByteArray = new byte[] {};
        byte[] notEmptyByteArray = new byte[] { 1 };
        assertEquals(true, ArrayUtils.isEmpty((byte[])null));
        assertEquals(true, ArrayUtils.isEmpty(emptyByteArray));
        assertEquals(false, ArrayUtils.isEmpty(notEmptyByteArray));

        double[] emptyDoubleArray = new double[] {};
        double[] notEmptyDoubleArray = new double[] { 1.0 };
        assertEquals(true, ArrayUtils.isEmpty((double[])null));
        assertEquals(true, ArrayUtils.isEmpty(emptyDoubleArray));
        assertEquals(false, ArrayUtils.isEmpty(notEmptyDoubleArray));

        float[] emptyFloatArray = new float[] {};
        float[] notEmptyFloatArray = new float[] { 1.0F };
        assertEquals(true, ArrayUtils.isEmpty((float[])null));
        assertEquals(true, ArrayUtils.isEmpty(emptyFloatArray));
        assertEquals(false, ArrayUtils.isEmpty(notEmptyFloatArray));

        boolean[] emptyBooleanArray = new boolean[] {};
        boolean[] notEmptyBooleanArray = new boolean[] { true };
        assertEquals(true, ArrayUtils.isEmpty((boolean[])null));
        assertEquals(true, ArrayUtils.isEmpty(emptyBooleanArray));
        assertEquals(false, ArrayUtils.isEmpty(notEmptyBooleanArray));
    }
    
   /**
     * Test for {@link ArrayUtils#isNotEmpty(java.lang.Object[])}.
     */
    public void testIsNotEmptyObject() {
        Object[] emptyArray = new Object[] {};
        Object[] notEmptyArray = new Object[] { new String("Value") };
        assertFalse(ArrayUtils.isNotEmpty((Object[])null));
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
    public void testIsNotEmptyPrimitives() {
        long[] emptyLongArray = new long[] {};
        long[] notEmptyLongArray = new long[] { 1L };
        assertFalse(ArrayUtils.isNotEmpty((long[])null));
        assertFalse(ArrayUtils.isNotEmpty(emptyLongArray));
        assertTrue(ArrayUtils.isNotEmpty(notEmptyLongArray));

        int[] emptyIntArray = new int[] {};
        int[] notEmptyIntArray = new int[] { 1 };
        assertFalse(ArrayUtils.isNotEmpty((int[])null));
        assertFalse(ArrayUtils.isNotEmpty(emptyIntArray));
        assertTrue(ArrayUtils.isNotEmpty(notEmptyIntArray));

        short[] emptyShortArray = new short[] {};
        short[] notEmptyShortArray = new short[] { 1 };
        assertFalse(ArrayUtils.isNotEmpty((short[])null));
        assertFalse(ArrayUtils.isNotEmpty(emptyShortArray));
        assertTrue(ArrayUtils.isNotEmpty(notEmptyShortArray));

        char[] emptyCharArray = new char[] {};
        char[] notEmptyCharArray = new char[] { 1 };
        assertFalse(ArrayUtils.isNotEmpty((char[])null));
        assertFalse(ArrayUtils.isNotEmpty(emptyCharArray));
        assertTrue(ArrayUtils.isNotEmpty(notEmptyCharArray));

        byte[] emptyByteArray = new byte[] {};
        byte[] notEmptyByteArray = new byte[] { 1 };
        assertFalse(ArrayUtils.isNotEmpty((byte[])null));
        assertFalse(ArrayUtils.isNotEmpty(emptyByteArray));
        assertTrue(ArrayUtils.isNotEmpty(notEmptyByteArray));

        double[] emptyDoubleArray = new double[] {};
        double[] notEmptyDoubleArray = new double[] { 1.0 };
        assertFalse(ArrayUtils.isNotEmpty((double[])null));
        assertFalse(ArrayUtils.isNotEmpty(emptyDoubleArray));
        assertTrue(ArrayUtils.isNotEmpty(notEmptyDoubleArray));

        float[] emptyFloatArray = new float[] {};
        float[] notEmptyFloatArray = new float[] { 1.0F };
        assertFalse(ArrayUtils.isNotEmpty((float[])null));
        assertFalse(ArrayUtils.isNotEmpty(emptyFloatArray));
        assertTrue(ArrayUtils.isNotEmpty(notEmptyFloatArray));

        boolean[] emptyBooleanArray = new boolean[] {};
        boolean[] notEmptyBooleanArray = new boolean[] { true };
        assertFalse(ArrayUtils.isNotEmpty((boolean[])null));
        assertFalse(ArrayUtils.isNotEmpty(emptyBooleanArray));
        assertTrue(ArrayUtils.isNotEmpty(notEmptyBooleanArray));
    }
    // ------------------------------------------------------------------------
    public void testGetLength() {
        assertEquals(0, ArrayUtils.getLength(null));
        
        Object[] emptyObjectArray = new Object[0];
        Object[] notEmptyObjectArray = new Object[] {"aValue"};
        assertEquals(0, ArrayUtils.getLength((Object[]) null));
        assertEquals(0, ArrayUtils.getLength(emptyObjectArray));
        assertEquals(1, ArrayUtils.getLength(notEmptyObjectArray));
 
        int[] emptyIntArray = new int[] {};
        int[] notEmptyIntArray = new int[] { 1 };
        assertEquals(0, ArrayUtils.getLength((int[]) null));
        assertEquals(0, ArrayUtils.getLength(emptyIntArray));
        assertEquals(1, ArrayUtils.getLength(notEmptyIntArray));

        short[] emptyShortArray = new short[] {};
        short[] notEmptyShortArray = new short[] { 1 };
        assertEquals(0, ArrayUtils.getLength((short[]) null));
        assertEquals(0, ArrayUtils.getLength(emptyShortArray));
        assertEquals(1, ArrayUtils.getLength(notEmptyShortArray));

        char[] emptyCharArray = new char[] {};
        char[] notEmptyCharArray = new char[] { 1 };
        assertEquals(0, ArrayUtils.getLength((char[]) null));
        assertEquals(0, ArrayUtils.getLength(emptyCharArray));
        assertEquals(1, ArrayUtils.getLength(notEmptyCharArray));

        byte[] emptyByteArray = new byte[] {};
        byte[] notEmptyByteArray = new byte[] { 1 };
        assertEquals(0, ArrayUtils.getLength((byte[]) null));
        assertEquals(0, ArrayUtils.getLength(emptyByteArray));
        assertEquals(1, ArrayUtils.getLength(notEmptyByteArray));

        double[] emptyDoubleArray = new double[] {};
        double[] notEmptyDoubleArray = new double[] { 1.0 };
        assertEquals(0, ArrayUtils.getLength((double[]) null));
        assertEquals(0, ArrayUtils.getLength(emptyDoubleArray));
        assertEquals(1, ArrayUtils.getLength(notEmptyDoubleArray));

        float[] emptyFloatArray = new float[] {};
        float[] notEmptyFloatArray = new float[] { 1.0F };
        assertEquals(0, ArrayUtils.getLength((float[]) null));
        assertEquals(0, ArrayUtils.getLength(emptyFloatArray));
        assertEquals(1, ArrayUtils.getLength(notEmptyFloatArray));

        boolean[] emptyBooleanArray = new boolean[] {};
        boolean[] notEmptyBooleanArray = new boolean[] { true };
        assertEquals(0, ArrayUtils.getLength((boolean[]) null));
        assertEquals(0, ArrayUtils.getLength(emptyBooleanArray));
        assertEquals(1, ArrayUtils.getLength(notEmptyBooleanArray));
        
        try {
            ArrayUtils.getLength("notAnArray");
            fail("IllegalArgumentException should have been thrown");
        } catch (IllegalArgumentException e) {}
    }

}
