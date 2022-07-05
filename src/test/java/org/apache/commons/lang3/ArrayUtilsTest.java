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
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.BitSet;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.Map;
import java.util.Random;

import org.junit.jupiter.api.Test;

/**
 * Unit tests {@link org.apache.commons.lang3.ArrayUtils}.
 */
@SuppressWarnings("deprecation") // deliberate use of deprecated code
public class ArrayUtilsTest extends AbstractLangTest {

    private class TestClass {
        // empty
    }

    /** A predefined seed used to initialize {@link Random} in order to get predictable results */
    private static final long SEED = 16111981L;

    @SafeVarargs
    private static <T> T[] toArrayPropagatingType(final T... items) {
        return ArrayUtils.toArray(items);
    }

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
     * Tests generic array creation with general return type.
     */
    @Test
    public void testArrayCreationWithGeneralReturnType() {
        final Object obj = ArrayUtils.toArray("foo", "bar");
        assertTrue(obj instanceof String[]);
    }

    @Test
    public void testClone() {
        assertArrayEquals(null, ArrayUtils.clone((Object[]) null));
        Object[] original1 = {};
        Object[] cloned1 = ArrayUtils.clone(original1);
        assertArrayEquals(original1, cloned1);
        assertNotSame(original1, cloned1);

        final StringBuilder builder = new StringBuilder("pick");
        original1 = new Object[]{builder, "a", new String[]{"stick"}};
        cloned1 = ArrayUtils.clone(original1);
        assertArrayEquals(original1, cloned1);
        assertNotSame(original1, cloned1);
        assertSame(original1[0], cloned1[0]);
        assertSame(original1[1], cloned1[1]);
        assertSame(original1[2], cloned1[2]);
    }

    @Test
    public void testCloneBoolean() {
        assertNull(ArrayUtils.clone((boolean[]) null));
        final boolean[] original = {true, false};
        final boolean[] cloned = ArrayUtils.clone(original);
        assertArrayEquals(original, cloned);
        assertNotSame(original, cloned);
    }

    @Test
    public void testCloneByte() {
        assertNull(ArrayUtils.clone((byte[]) null));
        final byte[] original = {1, 6};
        final byte[] cloned = ArrayUtils.clone(original);
        assertArrayEquals(original, cloned);
        assertNotSame(original, cloned);
    }

    @Test
    public void testCloneChar() {
        assertNull(ArrayUtils.clone((char[]) null));
        final char[] original = {'a', '4'};
        final char[] cloned = ArrayUtils.clone(original);
        assertArrayEquals(original, cloned);
        assertNotSame(original, cloned);
    }

    @Test
    public void testCloneDouble() {
        assertNull(ArrayUtils.clone((double[]) null));
        final double[] original = {2.4d, 5.7d};
        final double[] cloned = ArrayUtils.clone(original);
        assertArrayEquals(original, cloned);
        assertNotSame(original, cloned);
    }

    @Test
    public void testCloneFloat() {
        assertNull(ArrayUtils.clone((float[]) null));
        final float[] original = {2.6f, 6.4f};
        final float[] cloned = ArrayUtils.clone(original);
        assertArrayEquals(original, cloned);
        assertNotSame(original, cloned);
    }

    @Test
    public void testCloneInt() {
        assertNull(ArrayUtils.clone((int[]) null));
        final int[] original = {5, 8};
        final int[] cloned = ArrayUtils.clone(original);
        assertArrayEquals(original, cloned);
        assertNotSame(original, cloned);
    }

    @Test
    public void testCloneLong() {
        assertNull(ArrayUtils.clone((long[]) null));
        final long[] original = {0L, 1L};
        final long[] cloned = ArrayUtils.clone(original);
        assertArrayEquals(original, cloned);
        assertNotSame(original, cloned);
    }

    @Test
    public void testCloneShort() {
        assertNull(ArrayUtils.clone((short[]) null));
        final short[] original = {1, 4};
        final short[] cloned = ArrayUtils.clone(original);
        assertArrayEquals(original, cloned);
        assertNotSame(original, cloned);
    }

    @Test
    public void testConstructor() {
        assertNotNull(new ArrayUtils());
        final Constructor<?>[] cons = ArrayUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertTrue(Modifier.isPublic(cons[0].getModifiers()));
        assertTrue(Modifier.isPublic(ArrayUtils.class.getModifiers()));
        assertFalse(Modifier.isFinal(ArrayUtils.class.getModifiers()));
    }

    @Test
    public void testContains() {
        final Object[] array = {"0", "1", "2", "3", null, "0"};
        assertFalse(ArrayUtils.contains(null, null));
        assertFalse(ArrayUtils.contains(null, "1"));
        assertTrue(ArrayUtils.contains(array, "0"));
        assertTrue(ArrayUtils.contains(array, "1"));
        assertTrue(ArrayUtils.contains(array, "2"));
        assertTrue(ArrayUtils.contains(array, "3"));
        assertFalse(ArrayUtils.contains(array, "notInArray"));
        assertTrue(ArrayUtils.contains(array, null));
    }

    @Test
    public void testContainsAny() {
        final Object[] array = {"0", "1", "2", "3", null, "0"};
        assertFalse(ArrayUtils.containsAny(null, null));
        assertFalse(ArrayUtils.containsAny(null, "1"));
        assertTrue(ArrayUtils.containsAny(array, "0"));
        assertTrue(ArrayUtils.containsAny(array, "1"));
        assertTrue(ArrayUtils.containsAny(array, "2"));
        assertTrue(ArrayUtils.containsAny(array, "3"));
        assertFalse(ArrayUtils.containsAny(array, "notInArray"));
        assertTrue(ArrayUtils.containsAny(array, new String[] {null}));
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
            // empty.
        }

        final Object[] array = new LANG1261ChildObject[]{new LANG1261ChildObject()};

        assertTrue(ArrayUtils.contains(array, new LANG1261ParentObject()));
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

    @Test
    public void testContainsDoubleNaN() {
        final double[] a = { Double.NEGATIVE_INFINITY, Double.NaN, Double.POSITIVE_INFINITY };
        assertTrue(ArrayUtils.contains(a, Double.POSITIVE_INFINITY));
        assertTrue(ArrayUtils.contains(a, Double.NEGATIVE_INFINITY));
        assertTrue(ArrayUtils.contains(a, Double.NaN));
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

    @Test
    public void testContainsFloatNaN() {
        final float[] array = { Float.NEGATIVE_INFINITY, Float.NaN, Float.POSITIVE_INFINITY };
        assertTrue(ArrayUtils.contains(array, Float.POSITIVE_INFINITY));
        assertTrue(ArrayUtils.contains(array, Float.NEGATIVE_INFINITY));
        assertTrue(ArrayUtils.contains(array, Float.NaN));
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

    @Test
    public void testCreatePrimitiveArray() {
        assertNull(ArrayUtils.toPrimitive((Object[]) null));
        assertArrayEquals(new boolean[]{true}, ArrayUtils.toPrimitive(new Boolean[]{true}));
        assertArrayEquals(new char[]{'a'}, ArrayUtils.toPrimitive(new Character[]{'a'}));
        assertArrayEquals(new byte[]{1}, ArrayUtils.toPrimitive(new Byte[]{1}));
        assertArrayEquals(new int[]{}, ArrayUtils.toPrimitive(new Integer[]{}));
        assertArrayEquals(new short[]{2}, ArrayUtils.toPrimitive(new Short[]{2}));
        assertArrayEquals(new long[]{2, 3}, ArrayUtils.toPrimitive(new Long[]{2L, 3L}));
        assertArrayEquals(new float[]{3.14f}, ArrayUtils.toPrimitive(new Float[]{3.14f}), 0.1f);
        assertArrayEquals(new double[]{2.718}, ArrayUtils.toPrimitive(new Double[]{2.718}), 0.1);
    }

    @Test
    public void testCreatePrimitiveArrayViaObjectArray() {
        assertNull(ArrayUtils.toPrimitive((Object) null));
        assertArrayEquals(new boolean[]{true}, (boolean[]) ArrayUtils.toPrimitive((Object) new Boolean[]{true}));
        assertArrayEquals(new char[]{'a'}, (char[]) ArrayUtils.toPrimitive((Object) new Character[]{'a'}));
        assertArrayEquals(new byte[]{1}, (byte[]) ArrayUtils.toPrimitive((Object) new Byte[]{1}));
        assertArrayEquals(new int[]{}, (int[]) ArrayUtils.toPrimitive((Object) new Integer[]{}));
        assertArrayEquals(new short[]{2}, (short[]) ArrayUtils.toPrimitive((Object) new Short[]{2}));
        assertArrayEquals(new long[]{2, 3}, (long[]) ArrayUtils.toPrimitive((Object) new Long[]{2L, 3L}));
        assertArrayEquals(new float[]{3.14f}, (float[]) ArrayUtils.toPrimitive((Object) new Float[]{3.14f}), 0.1f);
        assertArrayEquals(new double[]{2.718}, (double[]) ArrayUtils.toPrimitive((Object) new Double[]{2.718}), 0.1);
    }

    /**
     * Tests generic empty array creation with generic type.
     */
    @Test
    public void testEmptyArrayCreation() {
        final String[] array = ArrayUtils.<String>toArray();
        assertEquals(0, array.length);
    }

    @Test
    public void testGet() {
        assertNull(ArrayUtils.get(null, -1));
        assertNull(ArrayUtils.get(null, 0));
        assertNull(ArrayUtils.get(null, 1));
        final String[] array0 = {};
        assertNull(ArrayUtils.get(array0, -1));
        assertNull(ArrayUtils.get(array0, 0));
        assertNull(ArrayUtils.get(array0, 1));
        final String[] array1 = { StringUtils.EMPTY };
        assertNull(ArrayUtils.get(array1, -1));
        assertEquals(StringUtils.EMPTY, ArrayUtils.get(array1, 0));
        assertNull(ArrayUtils.get(array1, 1));
    }

    @Test
    public void testGetComponentType() {
        final TestClass[] newArray = {};
        // No type-cast required.
        final Class<TestClass> componentType = ArrayUtils.getComponentType(newArray);
        assertEquals(TestClass.class, componentType);
        assertNull(ArrayUtils.getComponentType(null));
    }

    @Test
    public void testGetDefault() {
        // null default
        {
            assertNull(ArrayUtils.get(null, -1, null));
            assertNull(ArrayUtils.get(null, 0, null));
            assertNull(ArrayUtils.get(null, 1, null));
            final String[] array0 = {};
            assertNull(ArrayUtils.get(array0, -1, null));
            assertNull(ArrayUtils.get(array0, 0, null));
            assertNull(ArrayUtils.get(array0, 1, null));
            final String[] array1 = { StringUtils.EMPTY };
            assertNull(ArrayUtils.get(array1, -1, null));
            assertEquals(StringUtils.EMPTY, ArrayUtils.get(array1, 0, null));
            assertNull(ArrayUtils.get(array1, 1, null));
        }
        // non-null default
        {
            final String defaultValue = "defaultValue";
            final String[] array1 = { StringUtils.EMPTY };
            assertEquals(defaultValue, ArrayUtils.get(array1, -1, defaultValue));
            assertEquals(StringUtils.EMPTY, ArrayUtils.get(array1, 0, defaultValue));
            assertEquals(defaultValue, ArrayUtils.get(array1, 1, defaultValue));
        }
    }

    @Test
    public void testGetLength() {
        assertEquals(0, ArrayUtils.getLength(null));

        final Object[] emptyObjectArray = {};
        final Object[] notEmptyObjectArray = {"aValue"};
        assertEquals(0, ArrayUtils.getLength(null));
        assertEquals(0, ArrayUtils.getLength(emptyObjectArray));
        assertEquals(1, ArrayUtils.getLength(notEmptyObjectArray));

        final int[] emptyIntArray = {};
        final int[] notEmptyIntArray = {1};
        assertEquals(0, ArrayUtils.getLength(null));
        assertEquals(0, ArrayUtils.getLength(emptyIntArray));
        assertEquals(1, ArrayUtils.getLength(notEmptyIntArray));

        final short[] emptyShortArray = {};
        final short[] notEmptyShortArray = {1};
        assertEquals(0, ArrayUtils.getLength(null));
        assertEquals(0, ArrayUtils.getLength(emptyShortArray));
        assertEquals(1, ArrayUtils.getLength(notEmptyShortArray));

        final char[] emptyCharArray = {};
        final char[] notEmptyCharArray = {1};
        assertEquals(0, ArrayUtils.getLength(null));
        assertEquals(0, ArrayUtils.getLength(emptyCharArray));
        assertEquals(1, ArrayUtils.getLength(notEmptyCharArray));

        final byte[] emptyByteArray = {};
        final byte[] notEmptyByteArray = {1};
        assertEquals(0, ArrayUtils.getLength(null));
        assertEquals(0, ArrayUtils.getLength(emptyByteArray));
        assertEquals(1, ArrayUtils.getLength(notEmptyByteArray));

        final double[] emptyDoubleArray = {};
        final double[] notEmptyDoubleArray = {1.0};
        assertEquals(0, ArrayUtils.getLength(null));
        assertEquals(0, ArrayUtils.getLength(emptyDoubleArray));
        assertEquals(1, ArrayUtils.getLength(notEmptyDoubleArray));

        final float[] emptyFloatArray = {};
        final float[] notEmptyFloatArray = {1.0F};
        assertEquals(0, ArrayUtils.getLength(null));
        assertEquals(0, ArrayUtils.getLength(emptyFloatArray));
        assertEquals(1, ArrayUtils.getLength(notEmptyFloatArray));

        final boolean[] emptyBooleanArray = {};
        final boolean[] notEmptyBooleanArray = {true};
        assertEquals(0, ArrayUtils.getLength(null));
        assertEquals(0, ArrayUtils.getLength(emptyBooleanArray));
        assertEquals(1, ArrayUtils.getLength(notEmptyBooleanArray));

        assertThrows(IllegalArgumentException.class, () -> ArrayUtils.getLength("notAnArray"));
    }

    @Test
    public void testHashCode() {
        final long[][] array1 = {{2, 5}, {4, 5}};
        final long[][] array2 = {{2, 5}, {4, 6}};
        assertEquals(ArrayUtils.hashCode(array1), ArrayUtils.hashCode(array1));
        assertNotEquals(ArrayUtils.hashCode(array1), ArrayUtils.hashCode(array2));

        final Object[] array3 = {new String(new char[]{'A', 'B'})};
        final Object[] array4 = {"AB"};
        assertEquals(ArrayUtils.hashCode(array3), ArrayUtils.hashCode(array3));
        assertEquals(ArrayUtils.hashCode(array3), ArrayUtils.hashCode(array4));

        final Object[] arrayA = {new boolean[]{true, false}, new int[]{6, 7}};
        final Object[] arrayB = {new boolean[]{true, false}, new int[]{6, 7}};
        assertEquals(ArrayUtils.hashCode(arrayB), ArrayUtils.hashCode(arrayA));
    }

    @Test
    public void testIndexesOf() {
        final Object[] array = {"0", "1", "2", "3", null, "0"};
        final BitSet emptySet = new BitSet();
        final BitSet testSet = new BitSet();
        assertEquals(emptySet, ArrayUtils.indexesOf((Object[]) null, null));
        assertEquals(emptySet, ArrayUtils.indexesOf(new Object[0], "0"));
        testSet.set(5);
        testSet.set(0);
        assertEquals(testSet, ArrayUtils.indexesOf(array, "0"));
        testSet.clear();
        testSet.set(2);
        assertEquals(testSet, ArrayUtils.indexesOf(array, "2"));
        testSet.clear();
        testSet.set(3);
        assertEquals(testSet, ArrayUtils.indexesOf(array, "3"));
        testSet.clear();
        testSet.set(4);
        assertEquals(testSet, ArrayUtils.indexesOf(array, null));
        assertEquals(emptySet, ArrayUtils.indexesOf(array, "notInArray"));
    }

    @Test
    public void testIndexesOfBoolean() {
        boolean[] array = null;
        final BitSet emptySet = new BitSet();
        final BitSet testSet = new BitSet();
        assertEquals(emptySet, ArrayUtils.indexesOf(array, true));
        array = new boolean[0];
        assertEquals(emptySet, ArrayUtils.indexesOf(array, true));
        array = new boolean[]{true, false, true};
        testSet.set(0);
        testSet.set(2);
        assertEquals(testSet, ArrayUtils.indexesOf(array, true));
        testSet.clear();
        testSet.set(1);
        assertEquals(testSet, ArrayUtils.indexesOf(array, false));
        array = new boolean[]{true, true};
        assertEquals(emptySet, ArrayUtils.indexesOf(array, false));
    }

    @Test
    public void testIndexesOfBooleanWithStartIndex() {
        boolean[] array = null;
        final BitSet emptySet = new BitSet();
        final BitSet testSet = new BitSet();
        assertEquals(emptySet, ArrayUtils.indexesOf(array, true, 0));
        array = new boolean[0];
        assertEquals(emptySet, ArrayUtils.indexesOf(array, true, 0));
        array = new boolean[]{true, false, true};
        testSet.set(2);
        assertEquals(testSet, ArrayUtils.indexesOf(array, true, 1));
        testSet.set(0);
        assertEquals(testSet, ArrayUtils.indexesOf(array, true, 0));
        testSet.clear();
        testSet.set(1);
        assertEquals(testSet, ArrayUtils.indexesOf(array, false, 1));
        array = new boolean[]{true, true};
        assertEquals(emptySet, ArrayUtils.indexesOf(array, false, 0));
        assertEquals(emptySet, ArrayUtils.indexesOf(array, false, -1));
    }

    @Test
    public void testIndexesOfByte() {
        byte[] array = null;
        final BitSet emptySet = new BitSet();
        final BitSet testSet = new BitSet();
        assertEquals(emptySet, ArrayUtils.indexesOf(array, (byte) 0));
        array = new byte[]{0, 1, 2, 3, 0};
        testSet.set(0);
        testSet.set(4);
        assertEquals(testSet, ArrayUtils.indexesOf(array, (byte) 0));
        testSet.clear();
        testSet.set(1);
        assertEquals(testSet, ArrayUtils.indexesOf(array, (byte) 1));
        testSet.clear();
        testSet.set(2);
        assertEquals(testSet, ArrayUtils.indexesOf(array, (byte) 2));
        testSet.clear();
        testSet.set(3);
        assertEquals(testSet, ArrayUtils.indexesOf(array, (byte) 3));
        assertEquals(emptySet, ArrayUtils.indexesOf(array, (byte) 99));
    }

    @Test
    public void testIndexesOfByteWithStartIndex() {
        byte[] array = null;
        final BitSet emptySet = new BitSet();
        final BitSet testSet = new BitSet();
        assertEquals(emptySet, ArrayUtils.indexesOf(array, (byte) 0, 2));
        array = new byte[]{0, 1, 2, 3, 0};
        testSet.set(4);
        assertEquals(testSet, ArrayUtils.indexesOf(array, (byte) 0, 2));
        testSet.set(0);
        assertEquals(testSet, ArrayUtils.indexesOf(array, (byte) 0, 0));
        testSet.clear();
        testSet.set(1);
        assertEquals(testSet, ArrayUtils.indexesOf(array, (byte) 1, 1));
        testSet.clear();
        testSet.set(2);
        assertEquals(testSet, ArrayUtils.indexesOf(array, (byte) 2, 0));
        testSet.clear();
        testSet.set(3);
        assertEquals(testSet, ArrayUtils.indexesOf(array, (byte) 3, 0));
        assertEquals(testSet, ArrayUtils.indexesOf(array, (byte) 3, -1));
        assertEquals(emptySet, ArrayUtils.indexesOf(array, (byte) 99, 0));
    }

    @Test
    public void testIndexesOfChar() {
        char[] array = null;
        final BitSet emptySet = new BitSet();
        final BitSet testSet = new BitSet();
        assertEquals(emptySet, ArrayUtils.indexesOf(array, 'a'));
        array = new char[]{'a', 'b', 'c', 'd', 'a'};
        testSet.set(0);
        testSet.set(4);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 'a'));
        testSet.clear();
        testSet.set(1);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 'b'));
        testSet.clear();
        testSet.set(2);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 'c'));
        testSet.clear();
        testSet.set(3);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 'd'));
        assertEquals(emptySet, ArrayUtils.indexesOf(array, 'e'));
    }

    @Test
    public void testIndexesOfCharWithStartIndex() {
        char[] array = null;
        final BitSet emptySet = new BitSet();
        final BitSet testSet = new BitSet();
        assertEquals(emptySet, ArrayUtils.indexesOf(array, 'a', 0));
        array = new char[]{'a', 'b', 'c', 'd', 'a'};
        testSet.set(4);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 'a', 2));
        testSet.set(0);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 'a', 0));
        assertEquals(testSet, ArrayUtils.indexesOf(array, 'a', -1));
        testSet.clear();
        testSet.set(1);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 'b', 1));
        testSet.clear();
        testSet.set(2);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 'c', 0));
        testSet.clear();
        testSet.set(3);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 'd', 0));
        assertEquals(emptySet, ArrayUtils.indexesOf(array, 'd', 5));
        assertEquals(emptySet, ArrayUtils.indexesOf(array, 'e', 0));
    }

    @Test
    public void testIndexesOfDouble() {
        double[] array = null;
        final BitSet emptySet = new BitSet();
        final BitSet testSet = new BitSet();
        assertEquals(emptySet, ArrayUtils.indexesOf(array, 0));
        array = new double[]{0, 1, 2, 3, 0};
        testSet.set(0);
        testSet.set(4);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 0));
        testSet.clear();
        testSet.set(1);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 1));
        testSet.clear();
        testSet.set(2);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 2));
        testSet.clear();
        testSet.set(3);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 3));
        assertEquals(emptySet, ArrayUtils.indexesOf(array, 99));
    }

    @SuppressWarnings("cast")
    @Test
    public void testIndexesOfDoubleTolerance() {
        double[] array = null;
        final BitSet emptySet = new BitSet();
        final BitSet testSet = new BitSet();
        assertEquals(emptySet, ArrayUtils.indexesOf(array, (double) 0, (double) 0));
        array = new double[0];
        assertEquals(emptySet, ArrayUtils.indexesOf(array, (double) 0, (double) 0));
        array = new double[]{0, 1, 2, 3, 0};
        testSet.set(0);
        testSet.set(4);
        assertEquals(testSet, ArrayUtils.indexesOf(array, (double) 0, 0.3));
        testSet.clear();
        testSet.set(3);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 4.15, 2.0));
        testSet.clear();
        testSet.set(1);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 1.00001324, 0.0001));
    }

    @Test
    public void testIndexesOfDoubleWithStartIndex() {
        double[] array = null;
        final BitSet emptySet = new BitSet();
        final BitSet testSet = new BitSet();
        assertEquals(emptySet, ArrayUtils.indexesOf(array, 0, 2));
        array = new double[]{0, 1, 2, 3, 0};
        testSet.set(4);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 0, 2));
        testSet.set(0);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 0, 0));
        testSet.clear();
        testSet.set(1);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 1, 1));
        testSet.clear();
        testSet.set(2);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 2, 0));
        testSet.clear();
        testSet.set(3);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 3, 0));
        assertEquals(testSet, ArrayUtils.indexesOf(array, 3, -1));
        assertEquals(emptySet, ArrayUtils.indexesOf(array, 99, 0));
    }

    @SuppressWarnings("cast")
    @Test
    public void testIndexesOfDoubleWithStartIndexTolerance() {
        double[] array = null;
        final BitSet emptySet = new BitSet();
        final BitSet testSet = new BitSet();
        assertEquals(emptySet, ArrayUtils.indexesOf(array, (double) 0, 0, (double) 0));
        array = new double[0];
        assertEquals(emptySet, ArrayUtils.indexesOf(array, (double) 0, 0, (double) 0));
        array = new double[]{0, 1, 2, 3, 0};
        testSet.set(4);
        assertEquals(testSet, ArrayUtils.indexesOf(array, (double) 0, 1, 0.3));
        testSet.set(0);
        assertEquals(testSet, ArrayUtils.indexesOf(array, (double) 0, 0, 0.3));
        testSet.clear();
        testSet.set(2);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 2, 0, 0.35));
        assertEquals(testSet, ArrayUtils.indexesOf(array, 2, 2, 0.35));
        assertEquals(testSet, ArrayUtils.indexesOf(array, 2, -1, 0.35));
        assertEquals(emptySet, ArrayUtils.indexesOf(array, 2, 3, 0.35));
        testSet.clear();
        testSet.set(3);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 4.15, 0, 2.0));
        testSet.clear();
        testSet.set(1);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 1.00001324, 0, 0.0001));
    }

    @Test
    public void testIndexesOfFloat() {
        float[] array = null;
        final BitSet emptySet = new BitSet();
        final BitSet testSet = new BitSet();
        assertEquals(emptySet, ArrayUtils.indexesOf(array, 0));
        array = new float[]{0, 1, 2, 3, 0};
        testSet.set(0);
        testSet.set(4);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 0));
        testSet.clear();
        testSet.set(1);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 1));
        testSet.clear();
        testSet.set(2);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 2));
        testSet.clear();
        testSet.set(3);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 3));
        assertEquals(emptySet, ArrayUtils.indexesOf(array, 99));
    }

    @Test
    public void testIndexesOfFloatWithStartIndex() {
        float[] array = null;
        final BitSet emptySet = new BitSet();
        final BitSet testSet = new BitSet();
        assertEquals(emptySet, ArrayUtils.indexesOf(array, 0, 2));
        array = new float[]{0, 1, 2, 3, 0};
        testSet.set(4);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 0, 2));
        testSet.set(0);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 0, 0));
        testSet.clear();
        testSet.set(1);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 1, 1));
        testSet.clear();
        testSet.set(2);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 2, 0));
        testSet.clear();
        testSet.set(3);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 3, 0));
        assertEquals(testSet, ArrayUtils.indexesOf(array, 3, -1));
        assertEquals(emptySet, ArrayUtils.indexesOf(array, 99, 0));
    }

    @Test
    public void testIndexesOfIntWithStartIndex() {
        int[] array = null;
        final BitSet emptySet = new BitSet();
        final BitSet testSet = new BitSet();
        assertEquals(emptySet, ArrayUtils.indexesOf(array, 0, 2));
        array = new int[]{0, 1, 2, 3, 0};
        testSet.set(4);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 0, 2));
        testSet.set(0);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 0, 0));
        testSet.clear();
        testSet.set(1);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 1, 1));
        testSet.clear();
        testSet.set(2);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 2, 0));
        testSet.clear();
        testSet.set(3);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 3, 0));
        assertEquals(testSet, ArrayUtils.indexesOf(array, 3, -1));
        assertEquals(emptySet, ArrayUtils.indexesOf(array, 99, 0));
    }

    @Test
    public void testIndexesOfLong() {
        final long[] array = {0, 1, 2, 3};
        final BitSet emptySet = new BitSet();
        final BitSet testSet = new BitSet();
        assertEquals(emptySet, ArrayUtils.indexesOf((long[]) null, 0));
        assertEquals(emptySet, ArrayUtils.indexesOf(array, 4));
        testSet.set(0);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 0));
        testSet.clear();
        testSet.set(1);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 1));
        testSet.clear();
        testSet.set(2);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 2));
        testSet.clear();
        testSet.set(3);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 3));
    }

    @Test
    public void testIndexesOfLongWithStartIndex() {
        final long[] array = {0, 1, 2, 3, 2, 1, 0, 1};
        final BitSet emptySet = new BitSet();
        final BitSet testSet = new BitSet();
        assertEquals(emptySet, ArrayUtils.indexesOf((long[]) null, 0, 0));
        assertEquals(emptySet, ArrayUtils.indexesOf(array, 4, 0));
        testSet.set(6);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 0, 1));
        testSet.set(0);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 0, 0));
        testSet.clear();
        testSet.set(1);
        testSet.set(5);
        testSet.set(7);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 1, 0));
        testSet.clear();
        testSet.set(2);
        testSet.set(4);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 2, 0));
        testSet.clear();
        testSet.set(3);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 3, 0));
        assertEquals(emptySet, ArrayUtils.indexesOf(array, 3, 8));
    }

    @Test
    public void testIndexesOfShort() {
        short[] array = null;
        final BitSet emptySet = new BitSet();
        final BitSet testSet = new BitSet();
        assertEquals(emptySet, ArrayUtils.indexesOf(array, (short) 0));
        array = new short[]{0, 1, 2, 3, 0};
        testSet.set(0);
        testSet.set(4);
        assertEquals(testSet, ArrayUtils.indexesOf(array, (short) 0));
        testSet.clear();
        testSet.set(1);
        assertEquals(testSet, ArrayUtils.indexesOf(array, (short) 1));
        testSet.clear();
        testSet.set(2);
        assertEquals(testSet, ArrayUtils.indexesOf(array, (short) 2));
        testSet.clear();
        testSet.set(3);
        assertEquals(testSet, ArrayUtils.indexesOf(array, (short) 3));
        assertEquals(emptySet, ArrayUtils.indexesOf(array, (short) 99));
    }

    @Test
    public void testIndexesOfShortWithStartIndex() {
        short[] array = null;
        final BitSet emptySet = new BitSet();
        final BitSet testSet = new BitSet();
        assertEquals(emptySet, ArrayUtils.indexesOf(array, (short) 0, 2));
        array = new short[]{0, 1, 2, 3, 0};
        testSet.set(4);
        assertEquals(testSet, ArrayUtils.indexesOf(array, (short) 0, 2));
        testSet.set(0);
        assertEquals(testSet, ArrayUtils.indexesOf(array, (short) 0, 0));
        testSet.clear();
        testSet.set(1);
        assertEquals(testSet, ArrayUtils.indexesOf(array, (short) 1, 1));
        testSet.clear();
        testSet.set(2);
        assertEquals(testSet, ArrayUtils.indexesOf(array, (short) 2, 0));
        testSet.clear();
        testSet.set(3);
        assertEquals(testSet, ArrayUtils.indexesOf(array, (short) 3, 0));
        assertEquals(testSet, ArrayUtils.indexesOf(array, (short) 3, -1));
        assertEquals(emptySet, ArrayUtils.indexesOf(array, (short) 99, 0));
    }

    @Test
    public void testIndexesOfWithStartIndex() {
        final Object[] array = {"0", "1", "2", "3", "2", "3", "1", null, "0"};
        final BitSet emptySet = new BitSet();
        final BitSet testSet = new BitSet();
        assertEquals(emptySet, ArrayUtils.indexesOf(null, null, 2));
        assertEquals(emptySet, ArrayUtils.indexesOf(new Object[0], "0", 0));
        assertEquals(emptySet, ArrayUtils.indexesOf(null, "0", 2));
        testSet.set(8);
        assertEquals(testSet, ArrayUtils.indexesOf(array, "0", 8));
        testSet.set(0);
        assertEquals(testSet, ArrayUtils.indexesOf(array, "0", 0));
        testSet.clear();
        testSet.set(6);
        testSet.set(1);
        assertEquals(testSet, ArrayUtils.indexesOf(array, "1", 0));
        assertEquals(emptySet, ArrayUtils.indexesOf(array, "1", 9));
        testSet.clear();
        testSet.set(4);
        assertEquals(testSet, ArrayUtils.indexesOf(array, "2", 3));
        testSet.set(2);
        assertEquals(testSet, ArrayUtils.indexesOf(array, "2", 0));
        testSet.clear();
        testSet.set(3);
        testSet.set(5);
        assertEquals(testSet, ArrayUtils.indexesOf(array, "3", 0));
        testSet.clear();
        testSet.set(7);
        assertEquals(testSet, ArrayUtils.indexesOf(array, null, 0));

    }

    @Test
    public void testIndexOf() {
        final Object[] array = {"0", "1", "2", "3", null, "0"};
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

    @Test
    public void testIndexOfDoubleNaN() {
        final double[] array = { Double.NEGATIVE_INFINITY, Double.NaN, Double.POSITIVE_INFINITY, Double.NaN };
        assertEquals(0, ArrayUtils.indexOf(array, Double.NEGATIVE_INFINITY));
        assertEquals(1, ArrayUtils.indexOf(array, Double.NaN));
        assertEquals(2, ArrayUtils.indexOf(array, Double.POSITIVE_INFINITY));

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

    @Test
    public void testIndexOfFloatNaN() {
        final float[] array = { Float.NEGATIVE_INFINITY, Float.NaN, Float.POSITIVE_INFINITY, Float.NaN };
        assertEquals(0, ArrayUtils.indexOf(array, Float.NEGATIVE_INFINITY));
        assertEquals(1, ArrayUtils.indexOf(array, Float.NaN));
        assertEquals(2, ArrayUtils.indexOf(array, Float.POSITIVE_INFINITY));
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
    public void testIndexOfWithStartIndex() {
        final Object[] array = {"0", "1", "2", "3", null, "0"};
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
     * Tests indirect generic empty array creation with generic type.
     */
    @Test
    public void testIndirectEmptyArrayCreation() {
        final String[] array = ArrayUtilsTest.<String>toArrayPropagatingType();
        assertEquals(0, array.length);
    }

    @Test
    public void testIsArrayIndexValid() {
        assertFalse(ArrayUtils.isArrayIndexValid(null, 0));
        final String[] array = new String[1];

        //too big
        assertFalse(ArrayUtils.isArrayIndexValid(array, 1));

        //negative index
        assertFalse(ArrayUtils.isArrayIndexValid(array, -1));

        //good to go
        assertTrue(ArrayUtils.isArrayIndexValid(array, 0));
    }

    /**
     * Test for {@link ArrayUtils#isEmpty(java.lang.Object[])}.
     */
    @Test
    public void testIsEmptyObject() {
        final Object[] emptyArray = {};
        final Object[] notEmptyArray = {"Value"};
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
        final long[] emptyLongArray = {};
        final long[] notEmptyLongArray = {1L};
        assertTrue(ArrayUtils.isEmpty((long[]) null));
        assertTrue(ArrayUtils.isEmpty(emptyLongArray));
        assertFalse(ArrayUtils.isEmpty(notEmptyLongArray));

        final int[] emptyIntArray = {};
        final int[] notEmptyIntArray = {1};
        assertTrue(ArrayUtils.isEmpty((int[]) null));
        assertTrue(ArrayUtils.isEmpty(emptyIntArray));
        assertFalse(ArrayUtils.isEmpty(notEmptyIntArray));

        final short[] emptyShortArray = {};
        final short[] notEmptyShortArray = {1};
        assertTrue(ArrayUtils.isEmpty((short[]) null));
        assertTrue(ArrayUtils.isEmpty(emptyShortArray));
        assertFalse(ArrayUtils.isEmpty(notEmptyShortArray));

        final char[] emptyCharArray = {};
        final char[] notEmptyCharArray = {1};
        assertTrue(ArrayUtils.isEmpty((char[]) null));
        assertTrue(ArrayUtils.isEmpty(emptyCharArray));
        assertFalse(ArrayUtils.isEmpty(notEmptyCharArray));

        final byte[] emptyByteArray = {};
        final byte[] notEmptyByteArray = {1};
        assertTrue(ArrayUtils.isEmpty((byte[]) null));
        assertTrue(ArrayUtils.isEmpty(emptyByteArray));
        assertFalse(ArrayUtils.isEmpty(notEmptyByteArray));

        final double[] emptyDoubleArray = {};
        final double[] notEmptyDoubleArray = {1.0};
        assertTrue(ArrayUtils.isEmpty((double[]) null));
        assertTrue(ArrayUtils.isEmpty(emptyDoubleArray));
        assertFalse(ArrayUtils.isEmpty(notEmptyDoubleArray));

        final float[] emptyFloatArray = {};
        final float[] notEmptyFloatArray = {1.0F};
        assertTrue(ArrayUtils.isEmpty((float[]) null));
        assertTrue(ArrayUtils.isEmpty(emptyFloatArray));
        assertFalse(ArrayUtils.isEmpty(notEmptyFloatArray));

        final boolean[] emptyBooleanArray = {};
        final boolean[] notEmptyBooleanArray = {true};
        assertTrue(ArrayUtils.isEmpty((boolean[]) null));
        assertTrue(ArrayUtils.isEmpty(emptyBooleanArray));
        assertFalse(ArrayUtils.isEmpty(notEmptyBooleanArray));
    }

    @Test
    public void testIsEquals() {
        final long[][] larray1 = {{2, 5}, {4, 5}};
        final long[][] larray2 = {{2, 5}, {4, 6}};
        final long[] larray3 = {2, 5};
        this.assertIsEquals(larray1, larray2, larray3);

        final int[][] iarray1 = {{2, 5}, {4, 5}};
        final int[][] iarray2 = {{2, 5}, {4, 6}};
        final int[] iarray3 = {2, 5};
        this.assertIsEquals(iarray1, iarray2, iarray3);

        final short[][] sarray1 = {{2, 5}, {4, 5}};
        final short[][] sarray2 = {{2, 5}, {4, 6}};
        final short[] sarray3 = {2, 5};
        this.assertIsEquals(sarray1, sarray2, sarray3);

        final float[][] farray1 = {{2, 5}, {4, 5}};
        final float[][] farray2 = {{2, 5}, {4, 6}};
        final float[] farray3 = {2, 5};
        this.assertIsEquals(farray1, farray2, farray3);

        final double[][] darray1 = {{2, 5}, {4, 5}};
        final double[][] darray2 = {{2, 5}, {4, 6}};
        final double[] darray3 = {2, 5};
        this.assertIsEquals(darray1, darray2, darray3);

        final byte[][] byteArray1 = {{2, 5}, {4, 5}};
        final byte[][] byteArray2 = {{2, 5}, {4, 6}};
        final byte[] byteArray3 = {2, 5};
        this.assertIsEquals(byteArray1, byteArray2, byteArray3);

        final char[][] charArray1 = {{2, 5}, {4, 5}};
        final char[][] charArray2 = {{2, 5}, {4, 6}};
        final char[] charArray3 = {2, 5};
        this.assertIsEquals(charArray1, charArray2, charArray3);

        final boolean[][] barray1 = {{true, false}, {true, true}};
        final boolean[][] barray2 = {{true, false}, {true, false}};
        final boolean[] barray3 = {false, true};
        this.assertIsEquals(barray1, barray2, barray3);

        final Object[] array3 = {new String(new char[]{'A', 'B'})};
        final Object[] array4 = {"AB"};
        assertTrue(ArrayUtils.isEquals(array3, array3));
        assertTrue(ArrayUtils.isEquals(array3, array4));

        assertTrue(ArrayUtils.isEquals(null, null));
        assertFalse(ArrayUtils.isEquals(null, array4));
    }

    /**
     * Test for {@link ArrayUtils#isNotEmpty(java.lang.Object[])}.
     */
    @Test
    public void testIsNotEmptyObject() {
        final Object[] emptyArray = {};
        final Object[] notEmptyArray = {"Value"};
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
        final long[] emptyLongArray = {};
        final long[] notEmptyLongArray = {1L};
        assertFalse(ArrayUtils.isNotEmpty((long[]) null));
        assertFalse(ArrayUtils.isNotEmpty(emptyLongArray));
        assertTrue(ArrayUtils.isNotEmpty(notEmptyLongArray));

        final int[] emptyIntArray = {};
        final int[] notEmptyIntArray = {1};
        assertFalse(ArrayUtils.isNotEmpty((int[]) null));
        assertFalse(ArrayUtils.isNotEmpty(emptyIntArray));
        assertTrue(ArrayUtils.isNotEmpty(notEmptyIntArray));

        final short[] emptyShortArray = {};
        final short[] notEmptyShortArray = {1};
        assertFalse(ArrayUtils.isNotEmpty((short[]) null));
        assertFalse(ArrayUtils.isNotEmpty(emptyShortArray));
        assertTrue(ArrayUtils.isNotEmpty(notEmptyShortArray));

        final char[] emptyCharArray = {};
        final char[] notEmptyCharArray = {1};
        assertFalse(ArrayUtils.isNotEmpty((char[]) null));
        assertFalse(ArrayUtils.isNotEmpty(emptyCharArray));
        assertTrue(ArrayUtils.isNotEmpty(notEmptyCharArray));

        final byte[] emptyByteArray = {};
        final byte[] notEmptyByteArray = {1};
        assertFalse(ArrayUtils.isNotEmpty((byte[]) null));
        assertFalse(ArrayUtils.isNotEmpty(emptyByteArray));
        assertTrue(ArrayUtils.isNotEmpty(notEmptyByteArray));

        final double[] emptyDoubleArray = {};
        final double[] notEmptyDoubleArray = {1.0};
        assertFalse(ArrayUtils.isNotEmpty((double[]) null));
        assertFalse(ArrayUtils.isNotEmpty(emptyDoubleArray));
        assertTrue(ArrayUtils.isNotEmpty(notEmptyDoubleArray));

        final float[] emptyFloatArray = {};
        final float[] notEmptyFloatArray = {1.0F};
        assertFalse(ArrayUtils.isNotEmpty((float[]) null));
        assertFalse(ArrayUtils.isNotEmpty(emptyFloatArray));
        assertTrue(ArrayUtils.isNotEmpty(notEmptyFloatArray));

        final boolean[] emptyBooleanArray = {};
        final boolean[] notEmptyBooleanArray = {true};
        assertFalse(ArrayUtils.isNotEmpty((boolean[]) null));
        assertFalse(ArrayUtils.isNotEmpty(emptyBooleanArray));
        assertTrue(ArrayUtils.isNotEmpty(notEmptyBooleanArray));
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
    public void testIsSortedComparator() {
        final Comparator<Integer> c = Comparator.reverseOrder();

        Integer[] array = null;
        assertTrue(ArrayUtils.isSorted(array, c));

        array = new Integer[]{1};
        assertTrue(ArrayUtils.isSorted(array, c));

        array = new Integer[]{3, 2, 1};
        assertTrue(ArrayUtils.isSorted(array, c));

        array = new Integer[]{1, 3, 2};
        assertFalse(ArrayUtils.isSorted(array, c));
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
    public void testIsSortedNullComparator() {
        assertThrows(NullPointerException.class, () -> ArrayUtils.isSorted(null, null));
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
    public void testLastIndexOf() {
        final Object[] array = {"0", "1", "2", "3", null, "0"};
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
    public void testLastIndexOfWithStartIndex() {
        final Object[] array = {"0", "1", "2", "3", null, "0"};
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
    public void testNullToEmptyBoolean() {
        final boolean[] original = {true, false};
        assertEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyBooleanEmptyArray() {
        final boolean[] empty = {};
        final boolean[] result = ArrayUtils.nullToEmpty(empty);
        assertEquals(ArrayUtils.EMPTY_BOOLEAN_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyBooleanNull() {
        assertEquals(ArrayUtils.EMPTY_BOOLEAN_ARRAY, ArrayUtils.nullToEmpty((boolean[]) null));
    }

    @Test
    public void testNullToEmptyBooleanObject() {
        final Boolean[] original = {Boolean.TRUE, Boolean.FALSE};
        assertArrayEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyBooleanObjectEmptyArray() {
        final Boolean[] empty = {};
        final Boolean[] result = ArrayUtils.nullToEmpty(empty);
        assertArrayEquals(ArrayUtils.EMPTY_BOOLEAN_OBJECT_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyBooleanObjectNull() {
        assertArrayEquals(ArrayUtils.EMPTY_BOOLEAN_OBJECT_ARRAY, ArrayUtils.nullToEmpty((Boolean[]) null));
    }

    @Test
    public void testNullToEmptyByte() {
        final byte[] original = {0x0F, 0x0E};
        assertEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyByteEmptyArray() {
        final byte[] empty = {};
        final byte[] result = ArrayUtils.nullToEmpty(empty);
        assertEquals(ArrayUtils.EMPTY_BYTE_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyByteNull() {
        assertEquals(ArrayUtils.EMPTY_BYTE_ARRAY, ArrayUtils.nullToEmpty((byte[]) null));
    }

    @Test
    public void testNullToEmptyByteObject() {
        final Byte[] original = {0x0F, 0x0E};
        assertArrayEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyByteObjectEmptyArray() {
        final Byte[] empty = {};
        final Byte[] result = ArrayUtils.nullToEmpty(empty);
        assertArrayEquals(ArrayUtils.EMPTY_BYTE_OBJECT_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyByteObjectNull() {
        assertArrayEquals(ArrayUtils.EMPTY_BYTE_OBJECT_ARRAY, ArrayUtils.nullToEmpty((Byte[]) null));
    }

    @Test
    public void testNullToEmptyChar() {
        final char[] original = {'a', 'b'};
        assertEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyCharEmptyArray() {
        final char[] empty = {};
        final char[] result = ArrayUtils.nullToEmpty(empty);
        assertEquals(ArrayUtils.EMPTY_CHAR_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyCharNull() {
        assertEquals(ArrayUtils.EMPTY_CHAR_ARRAY, ArrayUtils.nullToEmpty((char[]) null));
    }

    @Test
    public void testNullToEmptyCharObject() {
        final Character[] original = {'a', 'b'};
        assertArrayEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyCharObjectEmptyArray() {
        final Character[] empty = {};
        final Character[] result = ArrayUtils.nullToEmpty(empty);
        assertArrayEquals(ArrayUtils.EMPTY_CHARACTER_OBJECT_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNUllToEmptyCharObjectNull() {
        assertArrayEquals(ArrayUtils.EMPTY_CHARACTER_OBJECT_ARRAY, ArrayUtils.nullToEmpty((Character[]) null));
    }

    @Test
    public void testNullToEmptyClass() {
        final Class<?>[] original = {Object.class, String.class};
        assertArrayEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyClassEmptyArray() {
        final Class<?>[] empty = {};
        final Class<?>[] result = ArrayUtils.nullToEmpty(empty);
        assertArrayEquals(ArrayUtils.EMPTY_CLASS_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyClassNull() {
        assertArrayEquals(ArrayUtils.EMPTY_CLASS_ARRAY, ArrayUtils.nullToEmpty((Class<?>[]) null));
    }

    @Test
    public void testNullToEmptyDouble() {
        final double[] original = {1L, 2L};
        assertEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyDoubleEmptyArray() {
        final double[] empty = {};
        final double[] result = ArrayUtils.nullToEmpty(empty);
        assertEquals(ArrayUtils.EMPTY_DOUBLE_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyDoubleNull() {
        assertEquals(ArrayUtils.EMPTY_DOUBLE_ARRAY, ArrayUtils.nullToEmpty((double[]) null));
    }

    @Test
    public void testNullToEmptyDoubleObject() {
        final Double[] original = {1D, 2D};
        assertArrayEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyDoubleObjectEmptyArray() {
        final Double[] empty = {};
        final Double[] result = ArrayUtils.nullToEmpty(empty);
        assertArrayEquals(ArrayUtils.EMPTY_DOUBLE_OBJECT_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyDoubleObjectNull() {
        assertArrayEquals(ArrayUtils.EMPTY_DOUBLE_OBJECT_ARRAY, ArrayUtils.nullToEmpty((Double[]) null));
    }

    @Test
    public void testNullToEmptyFloat() {
        final float[] original = {2.6f, 3.8f};
        assertEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyFloatEmptyArray() {
        final float[] empty = {};
        final float[] result = ArrayUtils.nullToEmpty(empty);
        assertEquals(ArrayUtils.EMPTY_FLOAT_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyFloatNull() {
        assertEquals(ArrayUtils.EMPTY_FLOAT_ARRAY, ArrayUtils.nullToEmpty((float[]) null));
    }

    @Test
    public void testNullToEmptyFloatObject() {
        final Float[] original = {2.6f, 3.8f};
        assertArrayEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyFloatObjectEmptyArray() {
        final Float[] empty = {};
        final Float[] result = ArrayUtils.nullToEmpty(empty);
        assertArrayEquals(ArrayUtils.EMPTY_FLOAT_OBJECT_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyFloatObjectNull() {
        assertArrayEquals(ArrayUtils.EMPTY_FLOAT_OBJECT_ARRAY, ArrayUtils.nullToEmpty((Float[]) null));
    }

    @Test
    public void testNullToEmptyGeneric() {
        final TestClass[] input = {new TestClass(), new TestClass()};
        final TestClass[] output = ArrayUtils.nullToEmpty(input, TestClass[].class);

        assertSame(input, output);
    }

    @Test
    public void testNullToEmptyGenericEmpty() {
        final TestClass[] input = {};
        final TestClass[] output = ArrayUtils.nullToEmpty(input, TestClass[].class);

        assertSame(input, output);
    }

    @Test
    public void testNullToEmptyGenericNull() {
        final TestClass[] output = ArrayUtils.nullToEmpty(null, TestClass[].class);

        assertNotNull(output);
        assertEquals(0, output.length);
    }

    @Test
    public void testNullToEmptyGenericNullType() {
        final TestClass[] input = {};
        assertThrows(IllegalArgumentException.class, () -> ArrayUtils.nullToEmpty(input, null));
    }

    @Test
    public void testNullToEmptyInt() {
        final int[] original = {1, 2};
        assertEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyIntEmptyArray() {
        final int[] empty = {};
        final int[] result = ArrayUtils.nullToEmpty(empty);
        assertEquals(ArrayUtils.EMPTY_INT_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyIntNull() {
        assertEquals(ArrayUtils.EMPTY_INT_ARRAY, ArrayUtils.nullToEmpty((int[]) null));
    }

    @Test
    public void testNullToEmptyIntObject() {
        final Integer[] original = {1, 2};
        assertArrayEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyIntObjectEmptyArray() {
        final Integer[] empty = {};
        final Integer[] result = ArrayUtils.nullToEmpty(empty);
        assertArrayEquals(ArrayUtils.EMPTY_INTEGER_OBJECT_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyIntObjectNull() {
        assertArrayEquals(ArrayUtils.EMPTY_INTEGER_OBJECT_ARRAY, ArrayUtils.nullToEmpty((Integer[]) null));
    }

    @Test
    public void testNullToEmptyLong() {
        final long[] original = {1L, 2L};
        assertEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyLongEmptyArray() {
        final long[] empty = {};
        final long[] result = ArrayUtils.nullToEmpty(empty);
        assertEquals(ArrayUtils.EMPTY_LONG_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyLongNull() {
        assertEquals(ArrayUtils.EMPTY_LONG_ARRAY, ArrayUtils.nullToEmpty((long[]) null));
    }

    @Test
    public void testNullToEmptyLongObject() {
        @SuppressWarnings("boxing") final Long[] original = {1L, 2L};
        assertArrayEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyLongObjectEmptyArray() {
        final Long[] empty = {};
        final Long[] result = ArrayUtils.nullToEmpty(empty);
        assertArrayEquals(ArrayUtils.EMPTY_LONG_OBJECT_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyLongObjectNull() {
        assertArrayEquals(ArrayUtils.EMPTY_LONG_OBJECT_ARRAY, ArrayUtils.nullToEmpty((Long[]) null));
    }

    @Test
    public void testNullToEmptyObject() {
        final Object[] original = {Boolean.TRUE, Boolean.FALSE};
        assertArrayEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyObjectEmptyArray() {
        final Object[] empty = {};
        final Object[] result = ArrayUtils.nullToEmpty(empty);
        assertArrayEquals(ArrayUtils.EMPTY_OBJECT_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyObjectNull() {
        assertArrayEquals(ArrayUtils.EMPTY_OBJECT_ARRAY, ArrayUtils.nullToEmpty((Object[]) null));
    }

    @Test
    public void testNullToEmptyShort() {
        final short[] original = {1, 2};
        assertEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyShortEmptyArray() {
        final short[] empty = {};
        final short[] result = ArrayUtils.nullToEmpty(empty);
        assertEquals(ArrayUtils.EMPTY_SHORT_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyShortNull() {
        assertEquals(ArrayUtils.EMPTY_SHORT_ARRAY, ArrayUtils.nullToEmpty((short[]) null));
    }

    @Test
    public void testNullToEmptyShortObject() {
        @SuppressWarnings("boxing") final Short[] original = {1, 2};
        assertArrayEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyShortObjectEmptyArray() {
        final Short[] empty = {};
        final Short[] result = ArrayUtils.nullToEmpty(empty);
        assertArrayEquals(ArrayUtils.EMPTY_SHORT_OBJECT_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyShortObjectNull() {
        assertArrayEquals(ArrayUtils.EMPTY_SHORT_OBJECT_ARRAY, ArrayUtils.nullToEmpty((Short[]) null));
    }

    @Test
    public void testNullToEmptyString() {
        final String[] original = {"abc", "def"};
        assertArrayEquals(original, ArrayUtils.nullToEmpty(original));
    }

    @Test
    public void testNullToEmptyStringEmptyArray() {
        final String[] empty = {};
        final String[] result = ArrayUtils.nullToEmpty(empty);
        assertArrayEquals(ArrayUtils.EMPTY_STRING_ARRAY, result);
        assertNotSame(empty, result);
    }

    @Test
    public void testNullToEmptyStringNull() {
        assertArrayEquals(ArrayUtils.EMPTY_STRING_ARRAY, ArrayUtils.nullToEmpty((String[]) null));
    }

    @Test
    public void testReverse() {
        final StringBuffer str1 = new StringBuffer("pick");
        final String str2 = "a";
        final String[] str3 = {"stick"};
        final String str4 = "up";

        Object[] array = {str1, str2, str3};
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
    public void testReverseBoolean() {
        boolean[] array = {false, false, true};
        ArrayUtils.reverse(array);
        assertTrue(array[0]);
        assertFalse(array[1]);
        assertFalse(array[2]);

        array = null;
        ArrayUtils.reverse(array);
        assertNull(array);
    }

    @Test
    public void testReverseBooleanRange() {
        boolean[] array = {false, false, true};
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
        assertNull(array);
    }

    @Test
    public void testReverseByte() {
        byte[] array = {2, 3, 4};
        ArrayUtils.reverse(array);
        assertEquals(array[0], 4);
        assertEquals(array[1], 3);
        assertEquals(array[2], 2);

        array = null;
        ArrayUtils.reverse(array);
        assertNull(array);
    }

    @Test
    public void testReverseByteRange() {
        byte[] array = {1, 2, 3};
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
        assertNull(array);
    }

    @Test
    public void testReverseChar() {
        char[] array = {'a', 'f', 'C'};
        ArrayUtils.reverse(array);
        assertEquals(array[0], 'C');
        assertEquals(array[1], 'f');
        assertEquals(array[2], 'a');

        array = null;
        ArrayUtils.reverse(array);
        assertNull(array);
    }

    @Test
    public void testReverseCharRange() {
        char[] array = {1, 2, 3};
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
        assertNull(array);
    }

    @Test
    public void testReverseDouble() {
        double[] array = {0.3d, 0.4d, 0.5d};
        ArrayUtils.reverse(array);
        assertEquals(0.5d, array[0]);
        assertEquals(0.4d, array[1]);
        assertEquals(0.3d, array[2]);

        array = null;
        ArrayUtils.reverse(array);
        assertNull(array);
    }

    @Test
    public void testReverseDoubleRange() {
        double[] array = {1, 2, 3};
        // The whole array
        ArrayUtils.reverse(array, 0, 3);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);
        // a range
        array = new double[]{1, 2, 3};
        ArrayUtils.reverse(array, 0, 2);
        assertEquals(2, array[0]);
        assertEquals(1, array[1]);
        assertEquals(3, array[2]);
        // a range with a negative start
        array = new double[]{1, 2, 3};
        ArrayUtils.reverse(array, -1, 3);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);
        // a range with a large stop index
        array = new double[]{1, 2, 3};
        ArrayUtils.reverse(array, -1, array.length + 1000);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);
        // null
        array = null;
        ArrayUtils.reverse(array, 0, 3);
        assertNull(array);
    }

    @Test
    public void testReverseFloat() {
        float[] array = {0.3f, 0.4f, 0.5f};
        ArrayUtils.reverse(array);
        assertEquals(0.5f, array[0]);
        assertEquals(0.4f, array[1]);
        assertEquals(0.3f, array[2]);

        array = null;
        ArrayUtils.reverse(array);
        assertNull(array);
    }

    @Test
    public void testReverseFloatRange() {
        float[] array = {1, 2, 3};
        // The whole array
        ArrayUtils.reverse(array, 0, 3);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);
        // a range
        array = new float[]{1, 2, 3};
        ArrayUtils.reverse(array, 0, 2);
        assertEquals(2, array[0]);
        assertEquals(1, array[1]);
        assertEquals(3, array[2]);
        // a range with a negative start
        array = new float[]{1, 2, 3};
        ArrayUtils.reverse(array, -1, 3);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);
        // a range with a large stop index
        array = new float[]{1, 2, 3};
        ArrayUtils.reverse(array, -1, array.length + 1000);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);
        // null
        array = null;
        ArrayUtils.reverse(array, 0, 3);
        assertNull(array);
    }

    @Test
    public void testReverseInt() {
        int[] array = {1, 2, 3};
        ArrayUtils.reverse(array);
        assertEquals(array[0], 3);
        assertEquals(array[1], 2);
        assertEquals(array[2], 1);

        array = null;
        ArrayUtils.reverse(array);
        assertNull(array);
    }

    @Test
    public void testReverseIntRange() {
        int[] array = {1, 2, 3};
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
        assertNull(array);
    }

    @Test
    public void testReverseLong() {
        long[] array = {1L, 2L, 3L};
        ArrayUtils.reverse(array);
        assertEquals(array[0], 3L);
        assertEquals(array[1], 2L);
        assertEquals(array[2], 1L);

        array = null;
        ArrayUtils.reverse(array);
        assertNull(array);
    }

    @Test
    public void testReverseLongRange() {
        long[] array = {1, 2, 3};
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
        assertNull(array);
    }

    @Test
    public void testReverseObjectRange() {
        String[] array = {"1", "2", "3"};
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
        assertNull(array);
    }

    @Test
    public void testReverseShort() {
        short[] array = {1, 2, 3};
        ArrayUtils.reverse(array);
        assertEquals(array[0], 3);
        assertEquals(array[1], 2);
        assertEquals(array[2], 1);

        array = null;
        ArrayUtils.reverse(array);
        assertNull(array);
    }

    @Test
    public void testReverseShortRange() {
        short[] array = {1, 2, 3};
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
        assertNull(array);
    }

    @Test
    public void testSameLength() {
        final Object[] nullArray = null;
        final Object[] emptyArray = {};
        final Object[] oneArray = {"pick"};
        final Object[] twoArray = {"pick", "stick"};

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
    public void testSameLengthAll() {
        final Object[] nullArrayObject = null;
        final Object[] emptyArrayObject = {};
        final Object[] oneArrayObject = {"pick"};
        final Object[] twoArrayObject = {"pick", "stick"};
        final boolean[] nullArrayBoolean = null;
        final boolean[] emptyArrayBoolean = {};
        final boolean[] oneArrayBoolean = {true};
        final boolean[] twoArrayBoolean = {true, false};
        final long[] nullArrayLong = null;
        final long[] emptyArrayLong = {};
        final long[] oneArrayLong = {0L};
        final long[] twoArrayLong = {0L, 76L};
        final int[] nullArrayInt = null;
        final int[] emptyArrayInt = {};
        final int[] oneArrayInt = {4};
        final int[] twoArrayInt = {5, 7};
        final short[] nullArrayShort = null;
        final short[] emptyArrayShort = {};
        final short[] oneArrayShort = {4};
        final short[] twoArrayShort = {6, 8};
        final char[] nullArrayChar = null;
        final char[] emptyArrayChar = {};
        final char[] oneArrayChar = {'f'};
        final char[] twoArrayChar = {'d', 't'};
        final byte[] nullArrayByte = null;
        final byte[] emptyArrayByte = {};
        final byte[] oneArrayByte = {3};
        final byte[] twoArrayByte = {4, 6};
        final double[] nullArrayDouble = null;
        final double[] emptyArrayDouble = {};
        final double[] oneArrayDouble = {1.3d};
        final double[] twoArrayDouble = {4.5d, 6.3d};
        final float[] nullArrayFloat = null;
        final float[] emptyArrayFloat = {};
        final float[] oneArrayFloat = {2.5f};
        final float[] twoArrayFloat = {6.4f, 5.8f};
        assertTrue(ArrayUtils.isSameLength(nullArrayObject, nullArrayObject));
        assertTrue(ArrayUtils.isSameLength(nullArrayObject, nullArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(nullArrayObject, nullArrayLong));
        assertTrue(ArrayUtils.isSameLength(nullArrayObject, nullArrayInt));
        assertTrue(ArrayUtils.isSameLength(nullArrayObject, nullArrayShort));
        assertTrue(ArrayUtils.isSameLength(nullArrayObject, nullArrayChar));
        assertTrue(ArrayUtils.isSameLength(nullArrayObject, nullArrayByte));
        assertTrue(ArrayUtils.isSameLength(nullArrayObject, nullArrayDouble));
        assertTrue(ArrayUtils.isSameLength(nullArrayObject, nullArrayFloat));
        assertTrue(ArrayUtils.isSameLength(nullArrayBoolean, nullArrayObject));
        assertTrue(ArrayUtils.isSameLength(nullArrayBoolean, nullArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(nullArrayBoolean, nullArrayLong));
        assertTrue(ArrayUtils.isSameLength(nullArrayBoolean, nullArrayInt));
        assertTrue(ArrayUtils.isSameLength(nullArrayBoolean, nullArrayShort));
        assertTrue(ArrayUtils.isSameLength(nullArrayBoolean, nullArrayChar));
        assertTrue(ArrayUtils.isSameLength(nullArrayBoolean, nullArrayByte));
        assertTrue(ArrayUtils.isSameLength(nullArrayBoolean, nullArrayDouble));
        assertTrue(ArrayUtils.isSameLength(nullArrayBoolean, nullArrayFloat));
        assertTrue(ArrayUtils.isSameLength(nullArrayLong, nullArrayObject));
        assertTrue(ArrayUtils.isSameLength(nullArrayLong, nullArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(nullArrayLong, nullArrayLong));
        assertTrue(ArrayUtils.isSameLength(nullArrayLong, nullArrayInt));
        assertTrue(ArrayUtils.isSameLength(nullArrayLong, nullArrayShort));
        assertTrue(ArrayUtils.isSameLength(nullArrayLong, nullArrayChar));
        assertTrue(ArrayUtils.isSameLength(nullArrayLong, nullArrayByte));
        assertTrue(ArrayUtils.isSameLength(nullArrayLong, nullArrayDouble));
        assertTrue(ArrayUtils.isSameLength(nullArrayLong, nullArrayFloat));
        assertTrue(ArrayUtils.isSameLength(nullArrayInt, nullArrayObject));
        assertTrue(ArrayUtils.isSameLength(nullArrayInt, nullArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(nullArrayInt, nullArrayLong));
        assertTrue(ArrayUtils.isSameLength(nullArrayInt, nullArrayInt));
        assertTrue(ArrayUtils.isSameLength(nullArrayInt, nullArrayShort));
        assertTrue(ArrayUtils.isSameLength(nullArrayInt, nullArrayChar));
        assertTrue(ArrayUtils.isSameLength(nullArrayInt, nullArrayByte));
        assertTrue(ArrayUtils.isSameLength(nullArrayInt, nullArrayDouble));
        assertTrue(ArrayUtils.isSameLength(nullArrayInt, nullArrayFloat));
        assertTrue(ArrayUtils.isSameLength(nullArrayShort, nullArrayObject));
        assertTrue(ArrayUtils.isSameLength(nullArrayShort, nullArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(nullArrayShort, nullArrayLong));
        assertTrue(ArrayUtils.isSameLength(nullArrayShort, nullArrayInt));
        assertTrue(ArrayUtils.isSameLength(nullArrayShort, nullArrayShort));
        assertTrue(ArrayUtils.isSameLength(nullArrayShort, nullArrayChar));
        assertTrue(ArrayUtils.isSameLength(nullArrayShort, nullArrayByte));
        assertTrue(ArrayUtils.isSameLength(nullArrayShort, nullArrayDouble));
        assertTrue(ArrayUtils.isSameLength(nullArrayShort, nullArrayFloat));
        assertTrue(ArrayUtils.isSameLength(nullArrayChar, nullArrayObject));
        assertTrue(ArrayUtils.isSameLength(nullArrayChar, nullArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(nullArrayChar, nullArrayLong));
        assertTrue(ArrayUtils.isSameLength(nullArrayChar, nullArrayInt));
        assertTrue(ArrayUtils.isSameLength(nullArrayChar, nullArrayShort));
        assertTrue(ArrayUtils.isSameLength(nullArrayChar, nullArrayChar));
        assertTrue(ArrayUtils.isSameLength(nullArrayChar, nullArrayByte));
        assertTrue(ArrayUtils.isSameLength(nullArrayChar, nullArrayDouble));
        assertTrue(ArrayUtils.isSameLength(nullArrayChar, nullArrayFloat));
        assertTrue(ArrayUtils.isSameLength(nullArrayByte, nullArrayObject));
        assertTrue(ArrayUtils.isSameLength(nullArrayByte, nullArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(nullArrayByte, nullArrayLong));
        assertTrue(ArrayUtils.isSameLength(nullArrayByte, nullArrayInt));
        assertTrue(ArrayUtils.isSameLength(nullArrayByte, nullArrayShort));
        assertTrue(ArrayUtils.isSameLength(nullArrayByte, nullArrayChar));
        assertTrue(ArrayUtils.isSameLength(nullArrayByte, nullArrayByte));
        assertTrue(ArrayUtils.isSameLength(nullArrayByte, nullArrayDouble));
        assertTrue(ArrayUtils.isSameLength(nullArrayByte, nullArrayFloat));
        assertTrue(ArrayUtils.isSameLength(nullArrayDouble, nullArrayObject));
        assertTrue(ArrayUtils.isSameLength(nullArrayDouble, nullArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(nullArrayDouble, nullArrayLong));
        assertTrue(ArrayUtils.isSameLength(nullArrayDouble, nullArrayInt));
        assertTrue(ArrayUtils.isSameLength(nullArrayDouble, nullArrayShort));
        assertTrue(ArrayUtils.isSameLength(nullArrayDouble, nullArrayChar));
        assertTrue(ArrayUtils.isSameLength(nullArrayDouble, nullArrayByte));
        assertTrue(ArrayUtils.isSameLength(nullArrayDouble, nullArrayDouble));
        assertTrue(ArrayUtils.isSameLength(nullArrayDouble, nullArrayFloat));
        assertTrue(ArrayUtils.isSameLength(nullArrayFloat, nullArrayObject));
        assertTrue(ArrayUtils.isSameLength(nullArrayFloat, nullArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(nullArrayFloat, nullArrayLong));
        assertTrue(ArrayUtils.isSameLength(nullArrayFloat, nullArrayInt));
        assertTrue(ArrayUtils.isSameLength(nullArrayFloat, nullArrayShort));
        assertTrue(ArrayUtils.isSameLength(nullArrayFloat, nullArrayChar));
        assertTrue(ArrayUtils.isSameLength(nullArrayFloat, nullArrayByte));
        assertTrue(ArrayUtils.isSameLength(nullArrayFloat, nullArrayDouble));
        assertTrue(ArrayUtils.isSameLength(nullArrayFloat, nullArrayFloat));
        assertTrue(ArrayUtils.isSameLength(nullArrayObject, emptyArrayObject));
        assertTrue(ArrayUtils.isSameLength(nullArrayObject, emptyArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(nullArrayObject, emptyArrayLong));
        assertTrue(ArrayUtils.isSameLength(nullArrayObject, emptyArrayInt));
        assertTrue(ArrayUtils.isSameLength(nullArrayObject, emptyArrayShort));
        assertTrue(ArrayUtils.isSameLength(nullArrayObject, emptyArrayChar));
        assertTrue(ArrayUtils.isSameLength(nullArrayObject, emptyArrayByte));
        assertTrue(ArrayUtils.isSameLength(nullArrayObject, emptyArrayDouble));
        assertTrue(ArrayUtils.isSameLength(nullArrayObject, emptyArrayFloat));
        assertTrue(ArrayUtils.isSameLength(nullArrayBoolean, emptyArrayObject));
        assertTrue(ArrayUtils.isSameLength(nullArrayBoolean, emptyArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(nullArrayBoolean, emptyArrayLong));
        assertTrue(ArrayUtils.isSameLength(nullArrayBoolean, emptyArrayInt));
        assertTrue(ArrayUtils.isSameLength(nullArrayBoolean, emptyArrayShort));
        assertTrue(ArrayUtils.isSameLength(nullArrayBoolean, emptyArrayChar));
        assertTrue(ArrayUtils.isSameLength(nullArrayBoolean, emptyArrayByte));
        assertTrue(ArrayUtils.isSameLength(nullArrayBoolean, emptyArrayDouble));
        assertTrue(ArrayUtils.isSameLength(nullArrayBoolean, emptyArrayFloat));
        assertTrue(ArrayUtils.isSameLength(nullArrayLong, emptyArrayObject));
        assertTrue(ArrayUtils.isSameLength(nullArrayLong, emptyArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(nullArrayLong, emptyArrayLong));
        assertTrue(ArrayUtils.isSameLength(nullArrayLong, emptyArrayInt));
        assertTrue(ArrayUtils.isSameLength(nullArrayLong, emptyArrayShort));
        assertTrue(ArrayUtils.isSameLength(nullArrayLong, emptyArrayChar));
        assertTrue(ArrayUtils.isSameLength(nullArrayLong, emptyArrayByte));
        assertTrue(ArrayUtils.isSameLength(nullArrayLong, emptyArrayDouble));
        assertTrue(ArrayUtils.isSameLength(nullArrayLong, emptyArrayFloat));
        assertTrue(ArrayUtils.isSameLength(nullArrayInt, emptyArrayObject));
        assertTrue(ArrayUtils.isSameLength(nullArrayInt, emptyArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(nullArrayInt, emptyArrayLong));
        assertTrue(ArrayUtils.isSameLength(nullArrayInt, emptyArrayInt));
        assertTrue(ArrayUtils.isSameLength(nullArrayInt, emptyArrayShort));
        assertTrue(ArrayUtils.isSameLength(nullArrayInt, emptyArrayChar));
        assertTrue(ArrayUtils.isSameLength(nullArrayInt, emptyArrayByte));
        assertTrue(ArrayUtils.isSameLength(nullArrayInt, emptyArrayDouble));
        assertTrue(ArrayUtils.isSameLength(nullArrayInt, emptyArrayFloat));
        assertTrue(ArrayUtils.isSameLength(nullArrayShort, emptyArrayObject));
        assertTrue(ArrayUtils.isSameLength(nullArrayShort, emptyArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(nullArrayShort, emptyArrayLong));
        assertTrue(ArrayUtils.isSameLength(nullArrayShort, emptyArrayInt));
        assertTrue(ArrayUtils.isSameLength(nullArrayShort, emptyArrayShort));
        assertTrue(ArrayUtils.isSameLength(nullArrayShort, emptyArrayChar));
        assertTrue(ArrayUtils.isSameLength(nullArrayShort, emptyArrayByte));
        assertTrue(ArrayUtils.isSameLength(nullArrayShort, emptyArrayDouble));
        assertTrue(ArrayUtils.isSameLength(nullArrayShort, emptyArrayFloat));
        assertTrue(ArrayUtils.isSameLength(nullArrayChar, emptyArrayObject));
        assertTrue(ArrayUtils.isSameLength(nullArrayChar, emptyArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(nullArrayChar, emptyArrayLong));
        assertTrue(ArrayUtils.isSameLength(nullArrayChar, emptyArrayInt));
        assertTrue(ArrayUtils.isSameLength(nullArrayChar, emptyArrayShort));
        assertTrue(ArrayUtils.isSameLength(nullArrayChar, emptyArrayChar));
        assertTrue(ArrayUtils.isSameLength(nullArrayChar, emptyArrayByte));
        assertTrue(ArrayUtils.isSameLength(nullArrayChar, emptyArrayDouble));
        assertTrue(ArrayUtils.isSameLength(nullArrayChar, emptyArrayFloat));
        assertTrue(ArrayUtils.isSameLength(nullArrayByte, emptyArrayObject));
        assertTrue(ArrayUtils.isSameLength(nullArrayByte, emptyArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(nullArrayByte, emptyArrayLong));
        assertTrue(ArrayUtils.isSameLength(nullArrayByte, emptyArrayInt));
        assertTrue(ArrayUtils.isSameLength(nullArrayByte, emptyArrayShort));
        assertTrue(ArrayUtils.isSameLength(nullArrayByte, emptyArrayChar));
        assertTrue(ArrayUtils.isSameLength(nullArrayByte, emptyArrayByte));
        assertTrue(ArrayUtils.isSameLength(nullArrayByte, emptyArrayDouble));
        assertTrue(ArrayUtils.isSameLength(nullArrayByte, emptyArrayFloat));
        assertTrue(ArrayUtils.isSameLength(nullArrayDouble, emptyArrayObject));
        assertTrue(ArrayUtils.isSameLength(nullArrayDouble, emptyArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(nullArrayDouble, emptyArrayLong));
        assertTrue(ArrayUtils.isSameLength(nullArrayDouble, emptyArrayInt));
        assertTrue(ArrayUtils.isSameLength(nullArrayDouble, emptyArrayShort));
        assertTrue(ArrayUtils.isSameLength(nullArrayDouble, emptyArrayChar));
        assertTrue(ArrayUtils.isSameLength(nullArrayDouble, emptyArrayByte));
        assertTrue(ArrayUtils.isSameLength(nullArrayDouble, emptyArrayDouble));
        assertTrue(ArrayUtils.isSameLength(nullArrayDouble, emptyArrayFloat));
        assertTrue(ArrayUtils.isSameLength(nullArrayFloat, emptyArrayObject));
        assertTrue(ArrayUtils.isSameLength(nullArrayFloat, emptyArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(nullArrayFloat, emptyArrayLong));
        assertTrue(ArrayUtils.isSameLength(nullArrayFloat, emptyArrayInt));
        assertTrue(ArrayUtils.isSameLength(nullArrayFloat, emptyArrayShort));
        assertTrue(ArrayUtils.isSameLength(nullArrayFloat, emptyArrayChar));
        assertTrue(ArrayUtils.isSameLength(nullArrayFloat, emptyArrayByte));
        assertTrue(ArrayUtils.isSameLength(nullArrayFloat, emptyArrayDouble));
        assertTrue(ArrayUtils.isSameLength(nullArrayFloat, emptyArrayFloat));
        assertFalse(ArrayUtils.isSameLength(nullArrayObject, oneArrayObject));
        assertFalse(ArrayUtils.isSameLength(nullArrayObject, oneArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(nullArrayObject, oneArrayLong));
        assertFalse(ArrayUtils.isSameLength(nullArrayObject, oneArrayInt));
        assertFalse(ArrayUtils.isSameLength(nullArrayObject, oneArrayShort));
        assertFalse(ArrayUtils.isSameLength(nullArrayObject, oneArrayChar));
        assertFalse(ArrayUtils.isSameLength(nullArrayObject, oneArrayByte));
        assertFalse(ArrayUtils.isSameLength(nullArrayObject, oneArrayDouble));
        assertFalse(ArrayUtils.isSameLength(nullArrayObject, oneArrayFloat));
        assertFalse(ArrayUtils.isSameLength(nullArrayBoolean, oneArrayObject));
        assertFalse(ArrayUtils.isSameLength(nullArrayBoolean, oneArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(nullArrayBoolean, oneArrayLong));
        assertFalse(ArrayUtils.isSameLength(nullArrayBoolean, oneArrayInt));
        assertFalse(ArrayUtils.isSameLength(nullArrayBoolean, oneArrayShort));
        assertFalse(ArrayUtils.isSameLength(nullArrayBoolean, oneArrayChar));
        assertFalse(ArrayUtils.isSameLength(nullArrayBoolean, oneArrayByte));
        assertFalse(ArrayUtils.isSameLength(nullArrayBoolean, oneArrayDouble));
        assertFalse(ArrayUtils.isSameLength(nullArrayBoolean, oneArrayFloat));
        assertFalse(ArrayUtils.isSameLength(nullArrayLong, oneArrayObject));
        assertFalse(ArrayUtils.isSameLength(nullArrayLong, oneArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(nullArrayLong, oneArrayLong));
        assertFalse(ArrayUtils.isSameLength(nullArrayLong, oneArrayInt));
        assertFalse(ArrayUtils.isSameLength(nullArrayLong, oneArrayShort));
        assertFalse(ArrayUtils.isSameLength(nullArrayLong, oneArrayChar));
        assertFalse(ArrayUtils.isSameLength(nullArrayLong, oneArrayByte));
        assertFalse(ArrayUtils.isSameLength(nullArrayLong, oneArrayDouble));
        assertFalse(ArrayUtils.isSameLength(nullArrayLong, oneArrayFloat));
        assertFalse(ArrayUtils.isSameLength(nullArrayInt, oneArrayObject));
        assertFalse(ArrayUtils.isSameLength(nullArrayInt, oneArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(nullArrayInt, oneArrayLong));
        assertFalse(ArrayUtils.isSameLength(nullArrayInt, oneArrayInt));
        assertFalse(ArrayUtils.isSameLength(nullArrayInt, oneArrayShort));
        assertFalse(ArrayUtils.isSameLength(nullArrayInt, oneArrayChar));
        assertFalse(ArrayUtils.isSameLength(nullArrayInt, oneArrayByte));
        assertFalse(ArrayUtils.isSameLength(nullArrayInt, oneArrayDouble));
        assertFalse(ArrayUtils.isSameLength(nullArrayInt, oneArrayFloat));
        assertFalse(ArrayUtils.isSameLength(nullArrayShort, oneArrayObject));
        assertFalse(ArrayUtils.isSameLength(nullArrayShort, oneArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(nullArrayShort, oneArrayLong));
        assertFalse(ArrayUtils.isSameLength(nullArrayShort, oneArrayInt));
        assertFalse(ArrayUtils.isSameLength(nullArrayShort, oneArrayShort));
        assertFalse(ArrayUtils.isSameLength(nullArrayShort, oneArrayChar));
        assertFalse(ArrayUtils.isSameLength(nullArrayShort, oneArrayByte));
        assertFalse(ArrayUtils.isSameLength(nullArrayShort, oneArrayDouble));
        assertFalse(ArrayUtils.isSameLength(nullArrayShort, oneArrayFloat));
        assertFalse(ArrayUtils.isSameLength(nullArrayChar, oneArrayObject));
        assertFalse(ArrayUtils.isSameLength(nullArrayChar, oneArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(nullArrayChar, oneArrayLong));
        assertFalse(ArrayUtils.isSameLength(nullArrayChar, oneArrayInt));
        assertFalse(ArrayUtils.isSameLength(nullArrayChar, oneArrayShort));
        assertFalse(ArrayUtils.isSameLength(nullArrayChar, oneArrayChar));
        assertFalse(ArrayUtils.isSameLength(nullArrayChar, oneArrayByte));
        assertFalse(ArrayUtils.isSameLength(nullArrayChar, oneArrayDouble));
        assertFalse(ArrayUtils.isSameLength(nullArrayChar, oneArrayFloat));
        assertFalse(ArrayUtils.isSameLength(nullArrayByte, oneArrayObject));
        assertFalse(ArrayUtils.isSameLength(nullArrayByte, oneArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(nullArrayByte, oneArrayLong));
        assertFalse(ArrayUtils.isSameLength(nullArrayByte, oneArrayInt));
        assertFalse(ArrayUtils.isSameLength(nullArrayByte, oneArrayShort));
        assertFalse(ArrayUtils.isSameLength(nullArrayByte, oneArrayChar));
        assertFalse(ArrayUtils.isSameLength(nullArrayByte, oneArrayByte));
        assertFalse(ArrayUtils.isSameLength(nullArrayByte, oneArrayDouble));
        assertFalse(ArrayUtils.isSameLength(nullArrayByte, oneArrayFloat));
        assertFalse(ArrayUtils.isSameLength(nullArrayDouble, oneArrayObject));
        assertFalse(ArrayUtils.isSameLength(nullArrayDouble, oneArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(nullArrayDouble, oneArrayLong));
        assertFalse(ArrayUtils.isSameLength(nullArrayDouble, oneArrayInt));
        assertFalse(ArrayUtils.isSameLength(nullArrayDouble, oneArrayShort));
        assertFalse(ArrayUtils.isSameLength(nullArrayDouble, oneArrayChar));
        assertFalse(ArrayUtils.isSameLength(nullArrayDouble, oneArrayByte));
        assertFalse(ArrayUtils.isSameLength(nullArrayDouble, oneArrayDouble));
        assertFalse(ArrayUtils.isSameLength(nullArrayDouble, oneArrayFloat));
        assertFalse(ArrayUtils.isSameLength(nullArrayFloat, oneArrayObject));
        assertFalse(ArrayUtils.isSameLength(nullArrayFloat, oneArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(nullArrayFloat, oneArrayLong));
        assertFalse(ArrayUtils.isSameLength(nullArrayFloat, oneArrayInt));
        assertFalse(ArrayUtils.isSameLength(nullArrayFloat, oneArrayShort));
        assertFalse(ArrayUtils.isSameLength(nullArrayFloat, oneArrayChar));
        assertFalse(ArrayUtils.isSameLength(nullArrayFloat, oneArrayByte));
        assertFalse(ArrayUtils.isSameLength(nullArrayFloat, oneArrayDouble));
        assertFalse(ArrayUtils.isSameLength(nullArrayFloat, oneArrayFloat));
        assertFalse(ArrayUtils.isSameLength(nullArrayObject, twoArrayObject));
        assertFalse(ArrayUtils.isSameLength(nullArrayObject, twoArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(nullArrayObject, twoArrayLong));
        assertFalse(ArrayUtils.isSameLength(nullArrayObject, twoArrayInt));
        assertFalse(ArrayUtils.isSameLength(nullArrayObject, twoArrayShort));
        assertFalse(ArrayUtils.isSameLength(nullArrayObject, twoArrayChar));
        assertFalse(ArrayUtils.isSameLength(nullArrayObject, twoArrayByte));
        assertFalse(ArrayUtils.isSameLength(nullArrayObject, twoArrayDouble));
        assertFalse(ArrayUtils.isSameLength(nullArrayObject, twoArrayFloat));
        assertFalse(ArrayUtils.isSameLength(nullArrayBoolean, twoArrayObject));
        assertFalse(ArrayUtils.isSameLength(nullArrayBoolean, twoArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(nullArrayBoolean, twoArrayLong));
        assertFalse(ArrayUtils.isSameLength(nullArrayBoolean, twoArrayInt));
        assertFalse(ArrayUtils.isSameLength(nullArrayBoolean, twoArrayShort));
        assertFalse(ArrayUtils.isSameLength(nullArrayBoolean, twoArrayChar));
        assertFalse(ArrayUtils.isSameLength(nullArrayBoolean, twoArrayByte));
        assertFalse(ArrayUtils.isSameLength(nullArrayBoolean, twoArrayDouble));
        assertFalse(ArrayUtils.isSameLength(nullArrayBoolean, twoArrayFloat));
        assertFalse(ArrayUtils.isSameLength(nullArrayLong, twoArrayObject));
        assertFalse(ArrayUtils.isSameLength(nullArrayLong, twoArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(nullArrayLong, twoArrayLong));
        assertFalse(ArrayUtils.isSameLength(nullArrayLong, twoArrayInt));
        assertFalse(ArrayUtils.isSameLength(nullArrayLong, twoArrayShort));
        assertFalse(ArrayUtils.isSameLength(nullArrayLong, twoArrayChar));
        assertFalse(ArrayUtils.isSameLength(nullArrayLong, twoArrayByte));
        assertFalse(ArrayUtils.isSameLength(nullArrayLong, twoArrayDouble));
        assertFalse(ArrayUtils.isSameLength(nullArrayLong, twoArrayFloat));
        assertFalse(ArrayUtils.isSameLength(nullArrayInt, twoArrayObject));
        assertFalse(ArrayUtils.isSameLength(nullArrayInt, twoArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(nullArrayInt, twoArrayLong));
        assertFalse(ArrayUtils.isSameLength(nullArrayInt, twoArrayInt));
        assertFalse(ArrayUtils.isSameLength(nullArrayInt, twoArrayShort));
        assertFalse(ArrayUtils.isSameLength(nullArrayInt, twoArrayChar));
        assertFalse(ArrayUtils.isSameLength(nullArrayInt, twoArrayByte));
        assertFalse(ArrayUtils.isSameLength(nullArrayInt, twoArrayDouble));
        assertFalse(ArrayUtils.isSameLength(nullArrayInt, twoArrayFloat));
        assertFalse(ArrayUtils.isSameLength(nullArrayShort, twoArrayObject));
        assertFalse(ArrayUtils.isSameLength(nullArrayShort, twoArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(nullArrayShort, twoArrayLong));
        assertFalse(ArrayUtils.isSameLength(nullArrayShort, twoArrayInt));
        assertFalse(ArrayUtils.isSameLength(nullArrayShort, twoArrayShort));
        assertFalse(ArrayUtils.isSameLength(nullArrayShort, twoArrayChar));
        assertFalse(ArrayUtils.isSameLength(nullArrayShort, twoArrayByte));
        assertFalse(ArrayUtils.isSameLength(nullArrayShort, twoArrayDouble));
        assertFalse(ArrayUtils.isSameLength(nullArrayShort, twoArrayFloat));
        assertFalse(ArrayUtils.isSameLength(nullArrayChar, twoArrayObject));
        assertFalse(ArrayUtils.isSameLength(nullArrayChar, twoArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(nullArrayChar, twoArrayLong));
        assertFalse(ArrayUtils.isSameLength(nullArrayChar, twoArrayInt));
        assertFalse(ArrayUtils.isSameLength(nullArrayChar, twoArrayShort));
        assertFalse(ArrayUtils.isSameLength(nullArrayChar, twoArrayChar));
        assertFalse(ArrayUtils.isSameLength(nullArrayChar, twoArrayByte));
        assertFalse(ArrayUtils.isSameLength(nullArrayChar, twoArrayDouble));
        assertFalse(ArrayUtils.isSameLength(nullArrayChar, twoArrayFloat));
        assertFalse(ArrayUtils.isSameLength(nullArrayByte, twoArrayObject));
        assertFalse(ArrayUtils.isSameLength(nullArrayByte, twoArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(nullArrayByte, twoArrayLong));
        assertFalse(ArrayUtils.isSameLength(nullArrayByte, twoArrayInt));
        assertFalse(ArrayUtils.isSameLength(nullArrayByte, twoArrayShort));
        assertFalse(ArrayUtils.isSameLength(nullArrayByte, twoArrayChar));
        assertFalse(ArrayUtils.isSameLength(nullArrayByte, twoArrayByte));
        assertFalse(ArrayUtils.isSameLength(nullArrayByte, twoArrayDouble));
        assertFalse(ArrayUtils.isSameLength(nullArrayByte, twoArrayFloat));
        assertFalse(ArrayUtils.isSameLength(nullArrayDouble, twoArrayObject));
        assertFalse(ArrayUtils.isSameLength(nullArrayDouble, twoArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(nullArrayDouble, twoArrayLong));
        assertFalse(ArrayUtils.isSameLength(nullArrayDouble, twoArrayInt));
        assertFalse(ArrayUtils.isSameLength(nullArrayDouble, twoArrayShort));
        assertFalse(ArrayUtils.isSameLength(nullArrayDouble, twoArrayChar));
        assertFalse(ArrayUtils.isSameLength(nullArrayDouble, twoArrayByte));
        assertFalse(ArrayUtils.isSameLength(nullArrayDouble, twoArrayDouble));
        assertFalse(ArrayUtils.isSameLength(nullArrayDouble, twoArrayFloat));
        assertFalse(ArrayUtils.isSameLength(nullArrayFloat, twoArrayObject));
        assertFalse(ArrayUtils.isSameLength(nullArrayFloat, twoArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(nullArrayFloat, twoArrayLong));
        assertFalse(ArrayUtils.isSameLength(nullArrayFloat, twoArrayInt));
        assertFalse(ArrayUtils.isSameLength(nullArrayFloat, twoArrayShort));
        assertFalse(ArrayUtils.isSameLength(nullArrayFloat, twoArrayChar));
        assertFalse(ArrayUtils.isSameLength(nullArrayFloat, twoArrayByte));
        assertFalse(ArrayUtils.isSameLength(nullArrayFloat, twoArrayDouble));
        assertFalse(ArrayUtils.isSameLength(nullArrayFloat, twoArrayFloat));
        assertTrue(ArrayUtils.isSameLength(emptyArrayObject, nullArrayObject));
        assertTrue(ArrayUtils.isSameLength(emptyArrayObject, nullArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(emptyArrayObject, nullArrayLong));
        assertTrue(ArrayUtils.isSameLength(emptyArrayObject, nullArrayInt));
        assertTrue(ArrayUtils.isSameLength(emptyArrayObject, nullArrayShort));
        assertTrue(ArrayUtils.isSameLength(emptyArrayObject, nullArrayChar));
        assertTrue(ArrayUtils.isSameLength(emptyArrayObject, nullArrayByte));
        assertTrue(ArrayUtils.isSameLength(emptyArrayObject, nullArrayDouble));
        assertTrue(ArrayUtils.isSameLength(emptyArrayObject, nullArrayFloat));
        assertTrue(ArrayUtils.isSameLength(emptyArrayBoolean, nullArrayObject));
        assertTrue(ArrayUtils.isSameLength(emptyArrayBoolean, nullArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(emptyArrayBoolean, nullArrayLong));
        assertTrue(ArrayUtils.isSameLength(emptyArrayBoolean, nullArrayInt));
        assertTrue(ArrayUtils.isSameLength(emptyArrayBoolean, nullArrayShort));
        assertTrue(ArrayUtils.isSameLength(emptyArrayBoolean, nullArrayChar));
        assertTrue(ArrayUtils.isSameLength(emptyArrayBoolean, nullArrayByte));
        assertTrue(ArrayUtils.isSameLength(emptyArrayBoolean, nullArrayDouble));
        assertTrue(ArrayUtils.isSameLength(emptyArrayBoolean, nullArrayFloat));
        assertTrue(ArrayUtils.isSameLength(emptyArrayLong, nullArrayObject));
        assertTrue(ArrayUtils.isSameLength(emptyArrayLong, nullArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(emptyArrayLong, nullArrayLong));
        assertTrue(ArrayUtils.isSameLength(emptyArrayLong, nullArrayInt));
        assertTrue(ArrayUtils.isSameLength(emptyArrayLong, nullArrayShort));
        assertTrue(ArrayUtils.isSameLength(emptyArrayLong, nullArrayChar));
        assertTrue(ArrayUtils.isSameLength(emptyArrayLong, nullArrayByte));
        assertTrue(ArrayUtils.isSameLength(emptyArrayLong, nullArrayDouble));
        assertTrue(ArrayUtils.isSameLength(emptyArrayLong, nullArrayFloat));
        assertTrue(ArrayUtils.isSameLength(emptyArrayInt, nullArrayObject));
        assertTrue(ArrayUtils.isSameLength(emptyArrayInt, nullArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(emptyArrayInt, nullArrayLong));
        assertTrue(ArrayUtils.isSameLength(emptyArrayInt, nullArrayInt));
        assertTrue(ArrayUtils.isSameLength(emptyArrayInt, nullArrayShort));
        assertTrue(ArrayUtils.isSameLength(emptyArrayInt, nullArrayChar));
        assertTrue(ArrayUtils.isSameLength(emptyArrayInt, nullArrayByte));
        assertTrue(ArrayUtils.isSameLength(emptyArrayInt, nullArrayDouble));
        assertTrue(ArrayUtils.isSameLength(emptyArrayInt, nullArrayFloat));
        assertTrue(ArrayUtils.isSameLength(emptyArrayShort, nullArrayObject));
        assertTrue(ArrayUtils.isSameLength(emptyArrayShort, nullArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(emptyArrayShort, nullArrayLong));
        assertTrue(ArrayUtils.isSameLength(emptyArrayShort, nullArrayInt));
        assertTrue(ArrayUtils.isSameLength(emptyArrayShort, nullArrayShort));
        assertTrue(ArrayUtils.isSameLength(emptyArrayShort, nullArrayChar));
        assertTrue(ArrayUtils.isSameLength(emptyArrayShort, nullArrayByte));
        assertTrue(ArrayUtils.isSameLength(emptyArrayShort, nullArrayDouble));
        assertTrue(ArrayUtils.isSameLength(emptyArrayShort, nullArrayFloat));
        assertTrue(ArrayUtils.isSameLength(emptyArrayChar, nullArrayObject));
        assertTrue(ArrayUtils.isSameLength(emptyArrayChar, nullArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(emptyArrayChar, nullArrayLong));
        assertTrue(ArrayUtils.isSameLength(emptyArrayChar, nullArrayInt));
        assertTrue(ArrayUtils.isSameLength(emptyArrayChar, nullArrayShort));
        assertTrue(ArrayUtils.isSameLength(emptyArrayChar, nullArrayChar));
        assertTrue(ArrayUtils.isSameLength(emptyArrayChar, nullArrayByte));
        assertTrue(ArrayUtils.isSameLength(emptyArrayChar, nullArrayDouble));
        assertTrue(ArrayUtils.isSameLength(emptyArrayChar, nullArrayFloat));
        assertTrue(ArrayUtils.isSameLength(emptyArrayByte, nullArrayObject));
        assertTrue(ArrayUtils.isSameLength(emptyArrayByte, nullArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(emptyArrayByte, nullArrayLong));
        assertTrue(ArrayUtils.isSameLength(emptyArrayByte, nullArrayInt));
        assertTrue(ArrayUtils.isSameLength(emptyArrayByte, nullArrayShort));
        assertTrue(ArrayUtils.isSameLength(emptyArrayByte, nullArrayChar));
        assertTrue(ArrayUtils.isSameLength(emptyArrayByte, nullArrayByte));
        assertTrue(ArrayUtils.isSameLength(emptyArrayByte, nullArrayDouble));
        assertTrue(ArrayUtils.isSameLength(emptyArrayByte, nullArrayFloat));
        assertTrue(ArrayUtils.isSameLength(emptyArrayDouble, nullArrayObject));
        assertTrue(ArrayUtils.isSameLength(emptyArrayDouble, nullArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(emptyArrayDouble, nullArrayLong));
        assertTrue(ArrayUtils.isSameLength(emptyArrayDouble, nullArrayInt));
        assertTrue(ArrayUtils.isSameLength(emptyArrayDouble, nullArrayShort));
        assertTrue(ArrayUtils.isSameLength(emptyArrayDouble, nullArrayChar));
        assertTrue(ArrayUtils.isSameLength(emptyArrayDouble, nullArrayByte));
        assertTrue(ArrayUtils.isSameLength(emptyArrayDouble, nullArrayDouble));
        assertTrue(ArrayUtils.isSameLength(emptyArrayDouble, nullArrayFloat));
        assertTrue(ArrayUtils.isSameLength(emptyArrayFloat, nullArrayObject));
        assertTrue(ArrayUtils.isSameLength(emptyArrayFloat, nullArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(emptyArrayFloat, nullArrayLong));
        assertTrue(ArrayUtils.isSameLength(emptyArrayFloat, nullArrayInt));
        assertTrue(ArrayUtils.isSameLength(emptyArrayFloat, nullArrayShort));
        assertTrue(ArrayUtils.isSameLength(emptyArrayFloat, nullArrayChar));
        assertTrue(ArrayUtils.isSameLength(emptyArrayFloat, nullArrayByte));
        assertTrue(ArrayUtils.isSameLength(emptyArrayFloat, nullArrayDouble));
        assertTrue(ArrayUtils.isSameLength(emptyArrayFloat, nullArrayFloat));
        assertTrue(ArrayUtils.isSameLength(emptyArrayObject, emptyArrayObject));
        assertTrue(ArrayUtils.isSameLength(emptyArrayObject, emptyArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(emptyArrayObject, emptyArrayLong));
        assertTrue(ArrayUtils.isSameLength(emptyArrayObject, emptyArrayInt));
        assertTrue(ArrayUtils.isSameLength(emptyArrayObject, emptyArrayShort));
        assertTrue(ArrayUtils.isSameLength(emptyArrayObject, emptyArrayChar));
        assertTrue(ArrayUtils.isSameLength(emptyArrayObject, emptyArrayByte));
        assertTrue(ArrayUtils.isSameLength(emptyArrayObject, emptyArrayDouble));
        assertTrue(ArrayUtils.isSameLength(emptyArrayObject, emptyArrayFloat));
        assertTrue(ArrayUtils.isSameLength(emptyArrayBoolean, emptyArrayObject));
        assertTrue(ArrayUtils.isSameLength(emptyArrayBoolean, emptyArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(emptyArrayBoolean, emptyArrayLong));
        assertTrue(ArrayUtils.isSameLength(emptyArrayBoolean, emptyArrayInt));
        assertTrue(ArrayUtils.isSameLength(emptyArrayBoolean, emptyArrayShort));
        assertTrue(ArrayUtils.isSameLength(emptyArrayBoolean, emptyArrayChar));
        assertTrue(ArrayUtils.isSameLength(emptyArrayBoolean, emptyArrayByte));
        assertTrue(ArrayUtils.isSameLength(emptyArrayBoolean, emptyArrayDouble));
        assertTrue(ArrayUtils.isSameLength(emptyArrayBoolean, emptyArrayFloat));
        assertTrue(ArrayUtils.isSameLength(emptyArrayLong, emptyArrayObject));
        assertTrue(ArrayUtils.isSameLength(emptyArrayLong, emptyArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(emptyArrayLong, emptyArrayLong));
        assertTrue(ArrayUtils.isSameLength(emptyArrayLong, emptyArrayInt));
        assertTrue(ArrayUtils.isSameLength(emptyArrayLong, emptyArrayShort));
        assertTrue(ArrayUtils.isSameLength(emptyArrayLong, emptyArrayChar));
        assertTrue(ArrayUtils.isSameLength(emptyArrayLong, emptyArrayByte));
        assertTrue(ArrayUtils.isSameLength(emptyArrayLong, emptyArrayDouble));
        assertTrue(ArrayUtils.isSameLength(emptyArrayLong, emptyArrayFloat));
        assertTrue(ArrayUtils.isSameLength(emptyArrayInt, emptyArrayObject));
        assertTrue(ArrayUtils.isSameLength(emptyArrayInt, emptyArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(emptyArrayInt, emptyArrayLong));
        assertTrue(ArrayUtils.isSameLength(emptyArrayInt, emptyArrayInt));
        assertTrue(ArrayUtils.isSameLength(emptyArrayInt, emptyArrayShort));
        assertTrue(ArrayUtils.isSameLength(emptyArrayInt, emptyArrayChar));
        assertTrue(ArrayUtils.isSameLength(emptyArrayInt, emptyArrayByte));
        assertTrue(ArrayUtils.isSameLength(emptyArrayInt, emptyArrayDouble));
        assertTrue(ArrayUtils.isSameLength(emptyArrayInt, emptyArrayFloat));
        assertTrue(ArrayUtils.isSameLength(emptyArrayShort, emptyArrayObject));
        assertTrue(ArrayUtils.isSameLength(emptyArrayShort, emptyArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(emptyArrayShort, emptyArrayLong));
        assertTrue(ArrayUtils.isSameLength(emptyArrayShort, emptyArrayInt));
        assertTrue(ArrayUtils.isSameLength(emptyArrayShort, emptyArrayShort));
        assertTrue(ArrayUtils.isSameLength(emptyArrayShort, emptyArrayChar));
        assertTrue(ArrayUtils.isSameLength(emptyArrayShort, emptyArrayByte));
        assertTrue(ArrayUtils.isSameLength(emptyArrayShort, emptyArrayDouble));
        assertTrue(ArrayUtils.isSameLength(emptyArrayShort, emptyArrayFloat));
        assertTrue(ArrayUtils.isSameLength(emptyArrayChar, emptyArrayObject));
        assertTrue(ArrayUtils.isSameLength(emptyArrayChar, emptyArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(emptyArrayChar, emptyArrayLong));
        assertTrue(ArrayUtils.isSameLength(emptyArrayChar, emptyArrayInt));
        assertTrue(ArrayUtils.isSameLength(emptyArrayChar, emptyArrayShort));
        assertTrue(ArrayUtils.isSameLength(emptyArrayChar, emptyArrayChar));
        assertTrue(ArrayUtils.isSameLength(emptyArrayChar, emptyArrayByte));
        assertTrue(ArrayUtils.isSameLength(emptyArrayChar, emptyArrayDouble));
        assertTrue(ArrayUtils.isSameLength(emptyArrayChar, emptyArrayFloat));
        assertTrue(ArrayUtils.isSameLength(emptyArrayByte, emptyArrayObject));
        assertTrue(ArrayUtils.isSameLength(emptyArrayByte, emptyArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(emptyArrayByte, emptyArrayLong));
        assertTrue(ArrayUtils.isSameLength(emptyArrayByte, emptyArrayInt));
        assertTrue(ArrayUtils.isSameLength(emptyArrayByte, emptyArrayShort));
        assertTrue(ArrayUtils.isSameLength(emptyArrayByte, emptyArrayChar));
        assertTrue(ArrayUtils.isSameLength(emptyArrayByte, emptyArrayByte));
        assertTrue(ArrayUtils.isSameLength(emptyArrayByte, emptyArrayDouble));
        assertTrue(ArrayUtils.isSameLength(emptyArrayByte, emptyArrayFloat));
        assertTrue(ArrayUtils.isSameLength(emptyArrayDouble, emptyArrayObject));
        assertTrue(ArrayUtils.isSameLength(emptyArrayDouble, emptyArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(emptyArrayDouble, emptyArrayLong));
        assertTrue(ArrayUtils.isSameLength(emptyArrayDouble, emptyArrayInt));
        assertTrue(ArrayUtils.isSameLength(emptyArrayDouble, emptyArrayShort));
        assertTrue(ArrayUtils.isSameLength(emptyArrayDouble, emptyArrayChar));
        assertTrue(ArrayUtils.isSameLength(emptyArrayDouble, emptyArrayByte));
        assertTrue(ArrayUtils.isSameLength(emptyArrayDouble, emptyArrayDouble));
        assertTrue(ArrayUtils.isSameLength(emptyArrayDouble, emptyArrayFloat));
        assertTrue(ArrayUtils.isSameLength(emptyArrayFloat, emptyArrayObject));
        assertTrue(ArrayUtils.isSameLength(emptyArrayFloat, emptyArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(emptyArrayFloat, emptyArrayLong));
        assertTrue(ArrayUtils.isSameLength(emptyArrayFloat, emptyArrayInt));
        assertTrue(ArrayUtils.isSameLength(emptyArrayFloat, emptyArrayShort));
        assertTrue(ArrayUtils.isSameLength(emptyArrayFloat, emptyArrayChar));
        assertTrue(ArrayUtils.isSameLength(emptyArrayFloat, emptyArrayByte));
        assertTrue(ArrayUtils.isSameLength(emptyArrayFloat, emptyArrayDouble));
        assertTrue(ArrayUtils.isSameLength(emptyArrayFloat, emptyArrayFloat));
        assertFalse(ArrayUtils.isSameLength(emptyArrayObject, oneArrayObject));
        assertFalse(ArrayUtils.isSameLength(emptyArrayObject, oneArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(emptyArrayObject, oneArrayLong));
        assertFalse(ArrayUtils.isSameLength(emptyArrayObject, oneArrayInt));
        assertFalse(ArrayUtils.isSameLength(emptyArrayObject, oneArrayShort));
        assertFalse(ArrayUtils.isSameLength(emptyArrayObject, oneArrayChar));
        assertFalse(ArrayUtils.isSameLength(emptyArrayObject, oneArrayByte));
        assertFalse(ArrayUtils.isSameLength(emptyArrayObject, oneArrayDouble));
        assertFalse(ArrayUtils.isSameLength(emptyArrayObject, oneArrayFloat));
        assertFalse(ArrayUtils.isSameLength(emptyArrayBoolean, oneArrayObject));
        assertFalse(ArrayUtils.isSameLength(emptyArrayBoolean, oneArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(emptyArrayBoolean, oneArrayLong));
        assertFalse(ArrayUtils.isSameLength(emptyArrayBoolean, oneArrayInt));
        assertFalse(ArrayUtils.isSameLength(emptyArrayBoolean, oneArrayShort));
        assertFalse(ArrayUtils.isSameLength(emptyArrayBoolean, oneArrayChar));
        assertFalse(ArrayUtils.isSameLength(emptyArrayBoolean, oneArrayByte));
        assertFalse(ArrayUtils.isSameLength(emptyArrayBoolean, oneArrayDouble));
        assertFalse(ArrayUtils.isSameLength(emptyArrayBoolean, oneArrayFloat));
        assertFalse(ArrayUtils.isSameLength(emptyArrayLong, oneArrayObject));
        assertFalse(ArrayUtils.isSameLength(emptyArrayLong, oneArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(emptyArrayLong, oneArrayLong));
        assertFalse(ArrayUtils.isSameLength(emptyArrayLong, oneArrayInt));
        assertFalse(ArrayUtils.isSameLength(emptyArrayLong, oneArrayShort));
        assertFalse(ArrayUtils.isSameLength(emptyArrayLong, oneArrayChar));
        assertFalse(ArrayUtils.isSameLength(emptyArrayLong, oneArrayByte));
        assertFalse(ArrayUtils.isSameLength(emptyArrayLong, oneArrayDouble));
        assertFalse(ArrayUtils.isSameLength(emptyArrayLong, oneArrayFloat));
        assertFalse(ArrayUtils.isSameLength(emptyArrayInt, oneArrayObject));
        assertFalse(ArrayUtils.isSameLength(emptyArrayInt, oneArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(emptyArrayInt, oneArrayLong));
        assertFalse(ArrayUtils.isSameLength(emptyArrayInt, oneArrayInt));
        assertFalse(ArrayUtils.isSameLength(emptyArrayInt, oneArrayShort));
        assertFalse(ArrayUtils.isSameLength(emptyArrayInt, oneArrayChar));
        assertFalse(ArrayUtils.isSameLength(emptyArrayInt, oneArrayByte));
        assertFalse(ArrayUtils.isSameLength(emptyArrayInt, oneArrayDouble));
        assertFalse(ArrayUtils.isSameLength(emptyArrayInt, oneArrayFloat));
        assertFalse(ArrayUtils.isSameLength(emptyArrayShort, oneArrayObject));
        assertFalse(ArrayUtils.isSameLength(emptyArrayShort, oneArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(emptyArrayShort, oneArrayLong));
        assertFalse(ArrayUtils.isSameLength(emptyArrayShort, oneArrayInt));
        assertFalse(ArrayUtils.isSameLength(emptyArrayShort, oneArrayShort));
        assertFalse(ArrayUtils.isSameLength(emptyArrayShort, oneArrayChar));
        assertFalse(ArrayUtils.isSameLength(emptyArrayShort, oneArrayByte));
        assertFalse(ArrayUtils.isSameLength(emptyArrayShort, oneArrayDouble));
        assertFalse(ArrayUtils.isSameLength(emptyArrayShort, oneArrayFloat));
        assertFalse(ArrayUtils.isSameLength(emptyArrayChar, oneArrayObject));
        assertFalse(ArrayUtils.isSameLength(emptyArrayChar, oneArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(emptyArrayChar, oneArrayLong));
        assertFalse(ArrayUtils.isSameLength(emptyArrayChar, oneArrayInt));
        assertFalse(ArrayUtils.isSameLength(emptyArrayChar, oneArrayShort));
        assertFalse(ArrayUtils.isSameLength(emptyArrayChar, oneArrayChar));
        assertFalse(ArrayUtils.isSameLength(emptyArrayChar, oneArrayByte));
        assertFalse(ArrayUtils.isSameLength(emptyArrayChar, oneArrayDouble));
        assertFalse(ArrayUtils.isSameLength(emptyArrayChar, oneArrayFloat));
        assertFalse(ArrayUtils.isSameLength(emptyArrayByte, oneArrayObject));
        assertFalse(ArrayUtils.isSameLength(emptyArrayByte, oneArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(emptyArrayByte, oneArrayLong));
        assertFalse(ArrayUtils.isSameLength(emptyArrayByte, oneArrayInt));
        assertFalse(ArrayUtils.isSameLength(emptyArrayByte, oneArrayShort));
        assertFalse(ArrayUtils.isSameLength(emptyArrayByte, oneArrayChar));
        assertFalse(ArrayUtils.isSameLength(emptyArrayByte, oneArrayByte));
        assertFalse(ArrayUtils.isSameLength(emptyArrayByte, oneArrayDouble));
        assertFalse(ArrayUtils.isSameLength(emptyArrayByte, oneArrayFloat));
        assertFalse(ArrayUtils.isSameLength(emptyArrayDouble, oneArrayObject));
        assertFalse(ArrayUtils.isSameLength(emptyArrayDouble, oneArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(emptyArrayDouble, oneArrayLong));
        assertFalse(ArrayUtils.isSameLength(emptyArrayDouble, oneArrayInt));
        assertFalse(ArrayUtils.isSameLength(emptyArrayDouble, oneArrayShort));
        assertFalse(ArrayUtils.isSameLength(emptyArrayDouble, oneArrayChar));
        assertFalse(ArrayUtils.isSameLength(emptyArrayDouble, oneArrayByte));
        assertFalse(ArrayUtils.isSameLength(emptyArrayDouble, oneArrayDouble));
        assertFalse(ArrayUtils.isSameLength(emptyArrayDouble, oneArrayFloat));
        assertFalse(ArrayUtils.isSameLength(emptyArrayFloat, oneArrayObject));
        assertFalse(ArrayUtils.isSameLength(emptyArrayFloat, oneArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(emptyArrayFloat, oneArrayLong));
        assertFalse(ArrayUtils.isSameLength(emptyArrayFloat, oneArrayInt));
        assertFalse(ArrayUtils.isSameLength(emptyArrayFloat, oneArrayShort));
        assertFalse(ArrayUtils.isSameLength(emptyArrayFloat, oneArrayChar));
        assertFalse(ArrayUtils.isSameLength(emptyArrayFloat, oneArrayByte));
        assertFalse(ArrayUtils.isSameLength(emptyArrayFloat, oneArrayDouble));
        assertFalse(ArrayUtils.isSameLength(emptyArrayFloat, oneArrayFloat));
        assertFalse(ArrayUtils.isSameLength(emptyArrayObject, twoArrayObject));
        assertFalse(ArrayUtils.isSameLength(emptyArrayObject, twoArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(emptyArrayObject, twoArrayLong));
        assertFalse(ArrayUtils.isSameLength(emptyArrayObject, twoArrayInt));
        assertFalse(ArrayUtils.isSameLength(emptyArrayObject, twoArrayShort));
        assertFalse(ArrayUtils.isSameLength(emptyArrayObject, twoArrayChar));
        assertFalse(ArrayUtils.isSameLength(emptyArrayObject, twoArrayByte));
        assertFalse(ArrayUtils.isSameLength(emptyArrayObject, twoArrayDouble));
        assertFalse(ArrayUtils.isSameLength(emptyArrayObject, twoArrayFloat));
        assertFalse(ArrayUtils.isSameLength(emptyArrayBoolean, twoArrayObject));
        assertFalse(ArrayUtils.isSameLength(emptyArrayBoolean, twoArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(emptyArrayBoolean, twoArrayLong));
        assertFalse(ArrayUtils.isSameLength(emptyArrayBoolean, twoArrayInt));
        assertFalse(ArrayUtils.isSameLength(emptyArrayBoolean, twoArrayShort));
        assertFalse(ArrayUtils.isSameLength(emptyArrayBoolean, twoArrayChar));
        assertFalse(ArrayUtils.isSameLength(emptyArrayBoolean, twoArrayByte));
        assertFalse(ArrayUtils.isSameLength(emptyArrayBoolean, twoArrayDouble));
        assertFalse(ArrayUtils.isSameLength(emptyArrayBoolean, twoArrayFloat));
        assertFalse(ArrayUtils.isSameLength(emptyArrayLong, twoArrayObject));
        assertFalse(ArrayUtils.isSameLength(emptyArrayLong, twoArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(emptyArrayLong, twoArrayLong));
        assertFalse(ArrayUtils.isSameLength(emptyArrayLong, twoArrayInt));
        assertFalse(ArrayUtils.isSameLength(emptyArrayLong, twoArrayShort));
        assertFalse(ArrayUtils.isSameLength(emptyArrayLong, twoArrayChar));
        assertFalse(ArrayUtils.isSameLength(emptyArrayLong, twoArrayByte));
        assertFalse(ArrayUtils.isSameLength(emptyArrayLong, twoArrayDouble));
        assertFalse(ArrayUtils.isSameLength(emptyArrayLong, twoArrayFloat));
        assertFalse(ArrayUtils.isSameLength(emptyArrayInt, twoArrayObject));
        assertFalse(ArrayUtils.isSameLength(emptyArrayInt, twoArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(emptyArrayInt, twoArrayLong));
        assertFalse(ArrayUtils.isSameLength(emptyArrayInt, twoArrayInt));
        assertFalse(ArrayUtils.isSameLength(emptyArrayInt, twoArrayShort));
        assertFalse(ArrayUtils.isSameLength(emptyArrayInt, twoArrayChar));
        assertFalse(ArrayUtils.isSameLength(emptyArrayInt, twoArrayByte));
        assertFalse(ArrayUtils.isSameLength(emptyArrayInt, twoArrayDouble));
        assertFalse(ArrayUtils.isSameLength(emptyArrayInt, twoArrayFloat));
        assertFalse(ArrayUtils.isSameLength(emptyArrayShort, twoArrayObject));
        assertFalse(ArrayUtils.isSameLength(emptyArrayShort, twoArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(emptyArrayShort, twoArrayLong));
        assertFalse(ArrayUtils.isSameLength(emptyArrayShort, twoArrayInt));
        assertFalse(ArrayUtils.isSameLength(emptyArrayShort, twoArrayShort));
        assertFalse(ArrayUtils.isSameLength(emptyArrayShort, twoArrayChar));
        assertFalse(ArrayUtils.isSameLength(emptyArrayShort, twoArrayByte));
        assertFalse(ArrayUtils.isSameLength(emptyArrayShort, twoArrayDouble));
        assertFalse(ArrayUtils.isSameLength(emptyArrayShort, twoArrayFloat));
        assertFalse(ArrayUtils.isSameLength(emptyArrayChar, twoArrayObject));
        assertFalse(ArrayUtils.isSameLength(emptyArrayChar, twoArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(emptyArrayChar, twoArrayLong));
        assertFalse(ArrayUtils.isSameLength(emptyArrayChar, twoArrayInt));
        assertFalse(ArrayUtils.isSameLength(emptyArrayChar, twoArrayShort));
        assertFalse(ArrayUtils.isSameLength(emptyArrayChar, twoArrayChar));
        assertFalse(ArrayUtils.isSameLength(emptyArrayChar, twoArrayByte));
        assertFalse(ArrayUtils.isSameLength(emptyArrayChar, twoArrayDouble));
        assertFalse(ArrayUtils.isSameLength(emptyArrayChar, twoArrayFloat));
        assertFalse(ArrayUtils.isSameLength(emptyArrayByte, twoArrayObject));
        assertFalse(ArrayUtils.isSameLength(emptyArrayByte, twoArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(emptyArrayByte, twoArrayLong));
        assertFalse(ArrayUtils.isSameLength(emptyArrayByte, twoArrayInt));
        assertFalse(ArrayUtils.isSameLength(emptyArrayByte, twoArrayShort));
        assertFalse(ArrayUtils.isSameLength(emptyArrayByte, twoArrayChar));
        assertFalse(ArrayUtils.isSameLength(emptyArrayByte, twoArrayByte));
        assertFalse(ArrayUtils.isSameLength(emptyArrayByte, twoArrayDouble));
        assertFalse(ArrayUtils.isSameLength(emptyArrayByte, twoArrayFloat));
        assertFalse(ArrayUtils.isSameLength(emptyArrayDouble, twoArrayObject));
        assertFalse(ArrayUtils.isSameLength(emptyArrayDouble, twoArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(emptyArrayDouble, twoArrayLong));
        assertFalse(ArrayUtils.isSameLength(emptyArrayDouble, twoArrayInt));
        assertFalse(ArrayUtils.isSameLength(emptyArrayDouble, twoArrayShort));
        assertFalse(ArrayUtils.isSameLength(emptyArrayDouble, twoArrayChar));
        assertFalse(ArrayUtils.isSameLength(emptyArrayDouble, twoArrayByte));
        assertFalse(ArrayUtils.isSameLength(emptyArrayDouble, twoArrayDouble));
        assertFalse(ArrayUtils.isSameLength(emptyArrayDouble, twoArrayFloat));
        assertFalse(ArrayUtils.isSameLength(emptyArrayFloat, twoArrayObject));
        assertFalse(ArrayUtils.isSameLength(emptyArrayFloat, twoArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(emptyArrayFloat, twoArrayLong));
        assertFalse(ArrayUtils.isSameLength(emptyArrayFloat, twoArrayInt));
        assertFalse(ArrayUtils.isSameLength(emptyArrayFloat, twoArrayShort));
        assertFalse(ArrayUtils.isSameLength(emptyArrayFloat, twoArrayChar));
        assertFalse(ArrayUtils.isSameLength(emptyArrayFloat, twoArrayByte));
        assertFalse(ArrayUtils.isSameLength(emptyArrayFloat, twoArrayDouble));
        assertFalse(ArrayUtils.isSameLength(emptyArrayFloat, twoArrayFloat));
        assertFalse(ArrayUtils.isSameLength(oneArrayObject, nullArrayObject));
        assertFalse(ArrayUtils.isSameLength(oneArrayObject, nullArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(oneArrayObject, nullArrayLong));
        assertFalse(ArrayUtils.isSameLength(oneArrayObject, nullArrayInt));
        assertFalse(ArrayUtils.isSameLength(oneArrayObject, nullArrayShort));
        assertFalse(ArrayUtils.isSameLength(oneArrayObject, nullArrayChar));
        assertFalse(ArrayUtils.isSameLength(oneArrayObject, nullArrayByte));
        assertFalse(ArrayUtils.isSameLength(oneArrayObject, nullArrayDouble));
        assertFalse(ArrayUtils.isSameLength(oneArrayObject, nullArrayFloat));
        assertFalse(ArrayUtils.isSameLength(oneArrayBoolean, nullArrayObject));
        assertFalse(ArrayUtils.isSameLength(oneArrayBoolean, nullArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(oneArrayBoolean, nullArrayLong));
        assertFalse(ArrayUtils.isSameLength(oneArrayBoolean, nullArrayInt));
        assertFalse(ArrayUtils.isSameLength(oneArrayBoolean, nullArrayShort));
        assertFalse(ArrayUtils.isSameLength(oneArrayBoolean, nullArrayChar));
        assertFalse(ArrayUtils.isSameLength(oneArrayBoolean, nullArrayByte));
        assertFalse(ArrayUtils.isSameLength(oneArrayBoolean, nullArrayDouble));
        assertFalse(ArrayUtils.isSameLength(oneArrayBoolean, nullArrayFloat));
        assertFalse(ArrayUtils.isSameLength(oneArrayLong, nullArrayObject));
        assertFalse(ArrayUtils.isSameLength(oneArrayLong, nullArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(oneArrayLong, nullArrayLong));
        assertFalse(ArrayUtils.isSameLength(oneArrayLong, nullArrayInt));
        assertFalse(ArrayUtils.isSameLength(oneArrayLong, nullArrayShort));
        assertFalse(ArrayUtils.isSameLength(oneArrayLong, nullArrayChar));
        assertFalse(ArrayUtils.isSameLength(oneArrayLong, nullArrayByte));
        assertFalse(ArrayUtils.isSameLength(oneArrayLong, nullArrayDouble));
        assertFalse(ArrayUtils.isSameLength(oneArrayLong, nullArrayFloat));
        assertFalse(ArrayUtils.isSameLength(oneArrayInt, nullArrayObject));
        assertFalse(ArrayUtils.isSameLength(oneArrayInt, nullArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(oneArrayInt, nullArrayLong));
        assertFalse(ArrayUtils.isSameLength(oneArrayInt, nullArrayInt));
        assertFalse(ArrayUtils.isSameLength(oneArrayInt, nullArrayShort));
        assertFalse(ArrayUtils.isSameLength(oneArrayInt, nullArrayChar));
        assertFalse(ArrayUtils.isSameLength(oneArrayInt, nullArrayByte));
        assertFalse(ArrayUtils.isSameLength(oneArrayInt, nullArrayDouble));
        assertFalse(ArrayUtils.isSameLength(oneArrayInt, nullArrayFloat));
        assertFalse(ArrayUtils.isSameLength(oneArrayShort, nullArrayObject));
        assertFalse(ArrayUtils.isSameLength(oneArrayShort, nullArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(oneArrayShort, nullArrayLong));
        assertFalse(ArrayUtils.isSameLength(oneArrayShort, nullArrayInt));
        assertFalse(ArrayUtils.isSameLength(oneArrayShort, nullArrayShort));
        assertFalse(ArrayUtils.isSameLength(oneArrayShort, nullArrayChar));
        assertFalse(ArrayUtils.isSameLength(oneArrayShort, nullArrayByte));
        assertFalse(ArrayUtils.isSameLength(oneArrayShort, nullArrayDouble));
        assertFalse(ArrayUtils.isSameLength(oneArrayShort, nullArrayFloat));
        assertFalse(ArrayUtils.isSameLength(oneArrayChar, nullArrayObject));
        assertFalse(ArrayUtils.isSameLength(oneArrayChar, nullArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(oneArrayChar, nullArrayLong));
        assertFalse(ArrayUtils.isSameLength(oneArrayChar, nullArrayInt));
        assertFalse(ArrayUtils.isSameLength(oneArrayChar, nullArrayShort));
        assertFalse(ArrayUtils.isSameLength(oneArrayChar, nullArrayChar));
        assertFalse(ArrayUtils.isSameLength(oneArrayChar, nullArrayByte));
        assertFalse(ArrayUtils.isSameLength(oneArrayChar, nullArrayDouble));
        assertFalse(ArrayUtils.isSameLength(oneArrayChar, nullArrayFloat));
        assertFalse(ArrayUtils.isSameLength(oneArrayByte, nullArrayObject));
        assertFalse(ArrayUtils.isSameLength(oneArrayByte, nullArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(oneArrayByte, nullArrayLong));
        assertFalse(ArrayUtils.isSameLength(oneArrayByte, nullArrayInt));
        assertFalse(ArrayUtils.isSameLength(oneArrayByte, nullArrayShort));
        assertFalse(ArrayUtils.isSameLength(oneArrayByte, nullArrayChar));
        assertFalse(ArrayUtils.isSameLength(oneArrayByte, nullArrayByte));
        assertFalse(ArrayUtils.isSameLength(oneArrayByte, nullArrayDouble));
        assertFalse(ArrayUtils.isSameLength(oneArrayByte, nullArrayFloat));
        assertFalse(ArrayUtils.isSameLength(oneArrayDouble, nullArrayObject));
        assertFalse(ArrayUtils.isSameLength(oneArrayDouble, nullArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(oneArrayDouble, nullArrayLong));
        assertFalse(ArrayUtils.isSameLength(oneArrayDouble, nullArrayInt));
        assertFalse(ArrayUtils.isSameLength(oneArrayDouble, nullArrayShort));
        assertFalse(ArrayUtils.isSameLength(oneArrayDouble, nullArrayChar));
        assertFalse(ArrayUtils.isSameLength(oneArrayDouble, nullArrayByte));
        assertFalse(ArrayUtils.isSameLength(oneArrayDouble, nullArrayDouble));
        assertFalse(ArrayUtils.isSameLength(oneArrayDouble, nullArrayFloat));
        assertFalse(ArrayUtils.isSameLength(oneArrayFloat, nullArrayObject));
        assertFalse(ArrayUtils.isSameLength(oneArrayFloat, nullArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(oneArrayFloat, nullArrayLong));
        assertFalse(ArrayUtils.isSameLength(oneArrayFloat, nullArrayInt));
        assertFalse(ArrayUtils.isSameLength(oneArrayFloat, nullArrayShort));
        assertFalse(ArrayUtils.isSameLength(oneArrayFloat, nullArrayChar));
        assertFalse(ArrayUtils.isSameLength(oneArrayFloat, nullArrayByte));
        assertFalse(ArrayUtils.isSameLength(oneArrayFloat, nullArrayDouble));
        assertFalse(ArrayUtils.isSameLength(oneArrayFloat, nullArrayFloat));
        assertFalse(ArrayUtils.isSameLength(oneArrayObject, emptyArrayObject));
        assertFalse(ArrayUtils.isSameLength(oneArrayObject, emptyArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(oneArrayObject, emptyArrayLong));
        assertFalse(ArrayUtils.isSameLength(oneArrayObject, emptyArrayInt));
        assertFalse(ArrayUtils.isSameLength(oneArrayObject, emptyArrayShort));
        assertFalse(ArrayUtils.isSameLength(oneArrayObject, emptyArrayChar));
        assertFalse(ArrayUtils.isSameLength(oneArrayObject, emptyArrayByte));
        assertFalse(ArrayUtils.isSameLength(oneArrayObject, emptyArrayDouble));
        assertFalse(ArrayUtils.isSameLength(oneArrayObject, emptyArrayFloat));
        assertFalse(ArrayUtils.isSameLength(oneArrayBoolean, emptyArrayObject));
        assertFalse(ArrayUtils.isSameLength(oneArrayBoolean, emptyArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(oneArrayBoolean, emptyArrayLong));
        assertFalse(ArrayUtils.isSameLength(oneArrayBoolean, emptyArrayInt));
        assertFalse(ArrayUtils.isSameLength(oneArrayBoolean, emptyArrayShort));
        assertFalse(ArrayUtils.isSameLength(oneArrayBoolean, emptyArrayChar));
        assertFalse(ArrayUtils.isSameLength(oneArrayBoolean, emptyArrayByte));
        assertFalse(ArrayUtils.isSameLength(oneArrayBoolean, emptyArrayDouble));
        assertFalse(ArrayUtils.isSameLength(oneArrayBoolean, emptyArrayFloat));
        assertFalse(ArrayUtils.isSameLength(oneArrayLong, emptyArrayObject));
        assertFalse(ArrayUtils.isSameLength(oneArrayLong, emptyArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(oneArrayLong, emptyArrayLong));
        assertFalse(ArrayUtils.isSameLength(oneArrayLong, emptyArrayInt));
        assertFalse(ArrayUtils.isSameLength(oneArrayLong, emptyArrayShort));
        assertFalse(ArrayUtils.isSameLength(oneArrayLong, emptyArrayChar));
        assertFalse(ArrayUtils.isSameLength(oneArrayLong, emptyArrayByte));
        assertFalse(ArrayUtils.isSameLength(oneArrayLong, emptyArrayDouble));
        assertFalse(ArrayUtils.isSameLength(oneArrayLong, emptyArrayFloat));
        assertFalse(ArrayUtils.isSameLength(oneArrayInt, emptyArrayObject));
        assertFalse(ArrayUtils.isSameLength(oneArrayInt, emptyArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(oneArrayInt, emptyArrayLong));
        assertFalse(ArrayUtils.isSameLength(oneArrayInt, emptyArrayInt));
        assertFalse(ArrayUtils.isSameLength(oneArrayInt, emptyArrayShort));
        assertFalse(ArrayUtils.isSameLength(oneArrayInt, emptyArrayChar));
        assertFalse(ArrayUtils.isSameLength(oneArrayInt, emptyArrayByte));
        assertFalse(ArrayUtils.isSameLength(oneArrayInt, emptyArrayDouble));
        assertFalse(ArrayUtils.isSameLength(oneArrayInt, emptyArrayFloat));
        assertFalse(ArrayUtils.isSameLength(oneArrayShort, emptyArrayObject));
        assertFalse(ArrayUtils.isSameLength(oneArrayShort, emptyArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(oneArrayShort, emptyArrayLong));
        assertFalse(ArrayUtils.isSameLength(oneArrayShort, emptyArrayInt));
        assertFalse(ArrayUtils.isSameLength(oneArrayShort, emptyArrayShort));
        assertFalse(ArrayUtils.isSameLength(oneArrayShort, emptyArrayChar));
        assertFalse(ArrayUtils.isSameLength(oneArrayShort, emptyArrayByte));
        assertFalse(ArrayUtils.isSameLength(oneArrayShort, emptyArrayDouble));
        assertFalse(ArrayUtils.isSameLength(oneArrayShort, emptyArrayFloat));
        assertFalse(ArrayUtils.isSameLength(oneArrayChar, emptyArrayObject));
        assertFalse(ArrayUtils.isSameLength(oneArrayChar, emptyArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(oneArrayChar, emptyArrayLong));
        assertFalse(ArrayUtils.isSameLength(oneArrayChar, emptyArrayInt));
        assertFalse(ArrayUtils.isSameLength(oneArrayChar, emptyArrayShort));
        assertFalse(ArrayUtils.isSameLength(oneArrayChar, emptyArrayChar));
        assertFalse(ArrayUtils.isSameLength(oneArrayChar, emptyArrayByte));
        assertFalse(ArrayUtils.isSameLength(oneArrayChar, emptyArrayDouble));
        assertFalse(ArrayUtils.isSameLength(oneArrayChar, emptyArrayFloat));
        assertFalse(ArrayUtils.isSameLength(oneArrayByte, emptyArrayObject));
        assertFalse(ArrayUtils.isSameLength(oneArrayByte, emptyArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(oneArrayByte, emptyArrayLong));
        assertFalse(ArrayUtils.isSameLength(oneArrayByte, emptyArrayInt));
        assertFalse(ArrayUtils.isSameLength(oneArrayByte, emptyArrayShort));
        assertFalse(ArrayUtils.isSameLength(oneArrayByte, emptyArrayChar));
        assertFalse(ArrayUtils.isSameLength(oneArrayByte, emptyArrayByte));
        assertFalse(ArrayUtils.isSameLength(oneArrayByte, emptyArrayDouble));
        assertFalse(ArrayUtils.isSameLength(oneArrayByte, emptyArrayFloat));
        assertFalse(ArrayUtils.isSameLength(oneArrayDouble, emptyArrayObject));
        assertFalse(ArrayUtils.isSameLength(oneArrayDouble, emptyArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(oneArrayDouble, emptyArrayLong));
        assertFalse(ArrayUtils.isSameLength(oneArrayDouble, emptyArrayInt));
        assertFalse(ArrayUtils.isSameLength(oneArrayDouble, emptyArrayShort));
        assertFalse(ArrayUtils.isSameLength(oneArrayDouble, emptyArrayChar));
        assertFalse(ArrayUtils.isSameLength(oneArrayDouble, emptyArrayByte));
        assertFalse(ArrayUtils.isSameLength(oneArrayDouble, emptyArrayDouble));
        assertFalse(ArrayUtils.isSameLength(oneArrayDouble, emptyArrayFloat));
        assertFalse(ArrayUtils.isSameLength(oneArrayFloat, emptyArrayObject));
        assertFalse(ArrayUtils.isSameLength(oneArrayFloat, emptyArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(oneArrayFloat, emptyArrayLong));
        assertFalse(ArrayUtils.isSameLength(oneArrayFloat, emptyArrayInt));
        assertFalse(ArrayUtils.isSameLength(oneArrayFloat, emptyArrayShort));
        assertFalse(ArrayUtils.isSameLength(oneArrayFloat, emptyArrayChar));
        assertFalse(ArrayUtils.isSameLength(oneArrayFloat, emptyArrayByte));
        assertFalse(ArrayUtils.isSameLength(oneArrayFloat, emptyArrayDouble));
        assertFalse(ArrayUtils.isSameLength(oneArrayFloat, emptyArrayFloat));
        assertTrue(ArrayUtils.isSameLength(oneArrayObject, oneArrayObject));
        assertTrue(ArrayUtils.isSameLength(oneArrayObject, oneArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(oneArrayObject, oneArrayLong));
        assertTrue(ArrayUtils.isSameLength(oneArrayObject, oneArrayInt));
        assertTrue(ArrayUtils.isSameLength(oneArrayObject, oneArrayShort));
        assertTrue(ArrayUtils.isSameLength(oneArrayObject, oneArrayChar));
        assertTrue(ArrayUtils.isSameLength(oneArrayObject, oneArrayByte));
        assertTrue(ArrayUtils.isSameLength(oneArrayObject, oneArrayDouble));
        assertTrue(ArrayUtils.isSameLength(oneArrayObject, oneArrayFloat));
        assertTrue(ArrayUtils.isSameLength(oneArrayBoolean, oneArrayObject));
        assertTrue(ArrayUtils.isSameLength(oneArrayBoolean, oneArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(oneArrayBoolean, oneArrayLong));
        assertTrue(ArrayUtils.isSameLength(oneArrayBoolean, oneArrayInt));
        assertTrue(ArrayUtils.isSameLength(oneArrayBoolean, oneArrayShort));
        assertTrue(ArrayUtils.isSameLength(oneArrayBoolean, oneArrayChar));
        assertTrue(ArrayUtils.isSameLength(oneArrayBoolean, oneArrayByte));
        assertTrue(ArrayUtils.isSameLength(oneArrayBoolean, oneArrayDouble));
        assertTrue(ArrayUtils.isSameLength(oneArrayBoolean, oneArrayFloat));
        assertTrue(ArrayUtils.isSameLength(oneArrayLong, oneArrayObject));
        assertTrue(ArrayUtils.isSameLength(oneArrayLong, oneArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(oneArrayLong, oneArrayLong));
        assertTrue(ArrayUtils.isSameLength(oneArrayLong, oneArrayInt));
        assertTrue(ArrayUtils.isSameLength(oneArrayLong, oneArrayShort));
        assertTrue(ArrayUtils.isSameLength(oneArrayLong, oneArrayChar));
        assertTrue(ArrayUtils.isSameLength(oneArrayLong, oneArrayByte));
        assertTrue(ArrayUtils.isSameLength(oneArrayLong, oneArrayDouble));
        assertTrue(ArrayUtils.isSameLength(oneArrayLong, oneArrayFloat));
        assertTrue(ArrayUtils.isSameLength(oneArrayInt, oneArrayObject));
        assertTrue(ArrayUtils.isSameLength(oneArrayInt, oneArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(oneArrayInt, oneArrayLong));
        assertTrue(ArrayUtils.isSameLength(oneArrayInt, oneArrayInt));
        assertTrue(ArrayUtils.isSameLength(oneArrayInt, oneArrayShort));
        assertTrue(ArrayUtils.isSameLength(oneArrayInt, oneArrayChar));
        assertTrue(ArrayUtils.isSameLength(oneArrayInt, oneArrayByte));
        assertTrue(ArrayUtils.isSameLength(oneArrayInt, oneArrayDouble));
        assertTrue(ArrayUtils.isSameLength(oneArrayInt, oneArrayFloat));
        assertTrue(ArrayUtils.isSameLength(oneArrayShort, oneArrayObject));
        assertTrue(ArrayUtils.isSameLength(oneArrayShort, oneArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(oneArrayShort, oneArrayLong));
        assertTrue(ArrayUtils.isSameLength(oneArrayShort, oneArrayInt));
        assertTrue(ArrayUtils.isSameLength(oneArrayShort, oneArrayShort));
        assertTrue(ArrayUtils.isSameLength(oneArrayShort, oneArrayChar));
        assertTrue(ArrayUtils.isSameLength(oneArrayShort, oneArrayByte));
        assertTrue(ArrayUtils.isSameLength(oneArrayShort, oneArrayDouble));
        assertTrue(ArrayUtils.isSameLength(oneArrayShort, oneArrayFloat));
        assertTrue(ArrayUtils.isSameLength(oneArrayChar, oneArrayObject));
        assertTrue(ArrayUtils.isSameLength(oneArrayChar, oneArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(oneArrayChar, oneArrayLong));
        assertTrue(ArrayUtils.isSameLength(oneArrayChar, oneArrayInt));
        assertTrue(ArrayUtils.isSameLength(oneArrayChar, oneArrayShort));
        assertTrue(ArrayUtils.isSameLength(oneArrayChar, oneArrayChar));
        assertTrue(ArrayUtils.isSameLength(oneArrayChar, oneArrayByte));
        assertTrue(ArrayUtils.isSameLength(oneArrayChar, oneArrayDouble));
        assertTrue(ArrayUtils.isSameLength(oneArrayChar, oneArrayFloat));
        assertTrue(ArrayUtils.isSameLength(oneArrayByte, oneArrayObject));
        assertTrue(ArrayUtils.isSameLength(oneArrayByte, oneArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(oneArrayByte, oneArrayLong));
        assertTrue(ArrayUtils.isSameLength(oneArrayByte, oneArrayInt));
        assertTrue(ArrayUtils.isSameLength(oneArrayByte, oneArrayShort));
        assertTrue(ArrayUtils.isSameLength(oneArrayByte, oneArrayChar));
        assertTrue(ArrayUtils.isSameLength(oneArrayByte, oneArrayByte));
        assertTrue(ArrayUtils.isSameLength(oneArrayByte, oneArrayDouble));
        assertTrue(ArrayUtils.isSameLength(oneArrayByte, oneArrayFloat));
        assertTrue(ArrayUtils.isSameLength(oneArrayDouble, oneArrayObject));
        assertTrue(ArrayUtils.isSameLength(oneArrayDouble, oneArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(oneArrayDouble, oneArrayLong));
        assertTrue(ArrayUtils.isSameLength(oneArrayDouble, oneArrayInt));
        assertTrue(ArrayUtils.isSameLength(oneArrayDouble, oneArrayShort));
        assertTrue(ArrayUtils.isSameLength(oneArrayDouble, oneArrayChar));
        assertTrue(ArrayUtils.isSameLength(oneArrayDouble, oneArrayByte));
        assertTrue(ArrayUtils.isSameLength(oneArrayDouble, oneArrayDouble));
        assertTrue(ArrayUtils.isSameLength(oneArrayDouble, oneArrayFloat));
        assertTrue(ArrayUtils.isSameLength(oneArrayFloat, oneArrayObject));
        assertTrue(ArrayUtils.isSameLength(oneArrayFloat, oneArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(oneArrayFloat, oneArrayLong));
        assertTrue(ArrayUtils.isSameLength(oneArrayFloat, oneArrayInt));
        assertTrue(ArrayUtils.isSameLength(oneArrayFloat, oneArrayShort));
        assertTrue(ArrayUtils.isSameLength(oneArrayFloat, oneArrayChar));
        assertTrue(ArrayUtils.isSameLength(oneArrayFloat, oneArrayByte));
        assertTrue(ArrayUtils.isSameLength(oneArrayFloat, oneArrayDouble));
        assertTrue(ArrayUtils.isSameLength(oneArrayFloat, oneArrayFloat));
        assertFalse(ArrayUtils.isSameLength(oneArrayObject, twoArrayObject));
        assertFalse(ArrayUtils.isSameLength(oneArrayObject, twoArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(oneArrayObject, twoArrayLong));
        assertFalse(ArrayUtils.isSameLength(oneArrayObject, twoArrayInt));
        assertFalse(ArrayUtils.isSameLength(oneArrayObject, twoArrayShort));
        assertFalse(ArrayUtils.isSameLength(oneArrayObject, twoArrayChar));
        assertFalse(ArrayUtils.isSameLength(oneArrayObject, twoArrayByte));
        assertFalse(ArrayUtils.isSameLength(oneArrayObject, twoArrayDouble));
        assertFalse(ArrayUtils.isSameLength(oneArrayObject, twoArrayFloat));
        assertFalse(ArrayUtils.isSameLength(oneArrayBoolean, twoArrayObject));
        assertFalse(ArrayUtils.isSameLength(oneArrayBoolean, twoArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(oneArrayBoolean, twoArrayLong));
        assertFalse(ArrayUtils.isSameLength(oneArrayBoolean, twoArrayInt));
        assertFalse(ArrayUtils.isSameLength(oneArrayBoolean, twoArrayShort));
        assertFalse(ArrayUtils.isSameLength(oneArrayBoolean, twoArrayChar));
        assertFalse(ArrayUtils.isSameLength(oneArrayBoolean, twoArrayByte));
        assertFalse(ArrayUtils.isSameLength(oneArrayBoolean, twoArrayDouble));
        assertFalse(ArrayUtils.isSameLength(oneArrayBoolean, twoArrayFloat));
        assertFalse(ArrayUtils.isSameLength(oneArrayLong, twoArrayObject));
        assertFalse(ArrayUtils.isSameLength(oneArrayLong, twoArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(oneArrayLong, twoArrayLong));
        assertFalse(ArrayUtils.isSameLength(oneArrayLong, twoArrayInt));
        assertFalse(ArrayUtils.isSameLength(oneArrayLong, twoArrayShort));
        assertFalse(ArrayUtils.isSameLength(oneArrayLong, twoArrayChar));
        assertFalse(ArrayUtils.isSameLength(oneArrayLong, twoArrayByte));
        assertFalse(ArrayUtils.isSameLength(oneArrayLong, twoArrayDouble));
        assertFalse(ArrayUtils.isSameLength(oneArrayLong, twoArrayFloat));
        assertFalse(ArrayUtils.isSameLength(oneArrayInt, twoArrayObject));
        assertFalse(ArrayUtils.isSameLength(oneArrayInt, twoArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(oneArrayInt, twoArrayLong));
        assertFalse(ArrayUtils.isSameLength(oneArrayInt, twoArrayInt));
        assertFalse(ArrayUtils.isSameLength(oneArrayInt, twoArrayShort));
        assertFalse(ArrayUtils.isSameLength(oneArrayInt, twoArrayChar));
        assertFalse(ArrayUtils.isSameLength(oneArrayInt, twoArrayByte));
        assertFalse(ArrayUtils.isSameLength(oneArrayInt, twoArrayDouble));
        assertFalse(ArrayUtils.isSameLength(oneArrayInt, twoArrayFloat));
        assertFalse(ArrayUtils.isSameLength(oneArrayShort, twoArrayObject));
        assertFalse(ArrayUtils.isSameLength(oneArrayShort, twoArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(oneArrayShort, twoArrayLong));
        assertFalse(ArrayUtils.isSameLength(oneArrayShort, twoArrayInt));
        assertFalse(ArrayUtils.isSameLength(oneArrayShort, twoArrayShort));
        assertFalse(ArrayUtils.isSameLength(oneArrayShort, twoArrayChar));
        assertFalse(ArrayUtils.isSameLength(oneArrayShort, twoArrayByte));
        assertFalse(ArrayUtils.isSameLength(oneArrayShort, twoArrayDouble));
        assertFalse(ArrayUtils.isSameLength(oneArrayShort, twoArrayFloat));
        assertFalse(ArrayUtils.isSameLength(oneArrayChar, twoArrayObject));
        assertFalse(ArrayUtils.isSameLength(oneArrayChar, twoArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(oneArrayChar, twoArrayLong));
        assertFalse(ArrayUtils.isSameLength(oneArrayChar, twoArrayInt));
        assertFalse(ArrayUtils.isSameLength(oneArrayChar, twoArrayShort));
        assertFalse(ArrayUtils.isSameLength(oneArrayChar, twoArrayChar));
        assertFalse(ArrayUtils.isSameLength(oneArrayChar, twoArrayByte));
        assertFalse(ArrayUtils.isSameLength(oneArrayChar, twoArrayDouble));
        assertFalse(ArrayUtils.isSameLength(oneArrayChar, twoArrayFloat));
        assertFalse(ArrayUtils.isSameLength(oneArrayByte, twoArrayObject));
        assertFalse(ArrayUtils.isSameLength(oneArrayByte, twoArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(oneArrayByte, twoArrayLong));
        assertFalse(ArrayUtils.isSameLength(oneArrayByte, twoArrayInt));
        assertFalse(ArrayUtils.isSameLength(oneArrayByte, twoArrayShort));
        assertFalse(ArrayUtils.isSameLength(oneArrayByte, twoArrayChar));
        assertFalse(ArrayUtils.isSameLength(oneArrayByte, twoArrayByte));
        assertFalse(ArrayUtils.isSameLength(oneArrayByte, twoArrayDouble));
        assertFalse(ArrayUtils.isSameLength(oneArrayByte, twoArrayFloat));
        assertFalse(ArrayUtils.isSameLength(oneArrayDouble, twoArrayObject));
        assertFalse(ArrayUtils.isSameLength(oneArrayDouble, twoArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(oneArrayDouble, twoArrayLong));
        assertFalse(ArrayUtils.isSameLength(oneArrayDouble, twoArrayInt));
        assertFalse(ArrayUtils.isSameLength(oneArrayDouble, twoArrayShort));
        assertFalse(ArrayUtils.isSameLength(oneArrayDouble, twoArrayChar));
        assertFalse(ArrayUtils.isSameLength(oneArrayDouble, twoArrayByte));
        assertFalse(ArrayUtils.isSameLength(oneArrayDouble, twoArrayDouble));
        assertFalse(ArrayUtils.isSameLength(oneArrayDouble, twoArrayFloat));
        assertFalse(ArrayUtils.isSameLength(oneArrayFloat, twoArrayObject));
        assertFalse(ArrayUtils.isSameLength(oneArrayFloat, twoArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(oneArrayFloat, twoArrayLong));
        assertFalse(ArrayUtils.isSameLength(oneArrayFloat, twoArrayInt));
        assertFalse(ArrayUtils.isSameLength(oneArrayFloat, twoArrayShort));
        assertFalse(ArrayUtils.isSameLength(oneArrayFloat, twoArrayChar));
        assertFalse(ArrayUtils.isSameLength(oneArrayFloat, twoArrayByte));
        assertFalse(ArrayUtils.isSameLength(oneArrayFloat, twoArrayDouble));
        assertFalse(ArrayUtils.isSameLength(oneArrayFloat, twoArrayFloat));
        assertFalse(ArrayUtils.isSameLength(twoArrayObject, nullArrayObject));
        assertFalse(ArrayUtils.isSameLength(twoArrayObject, nullArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(twoArrayObject, nullArrayLong));
        assertFalse(ArrayUtils.isSameLength(twoArrayObject, nullArrayInt));
        assertFalse(ArrayUtils.isSameLength(twoArrayObject, nullArrayShort));
        assertFalse(ArrayUtils.isSameLength(twoArrayObject, nullArrayChar));
        assertFalse(ArrayUtils.isSameLength(twoArrayObject, nullArrayByte));
        assertFalse(ArrayUtils.isSameLength(twoArrayObject, nullArrayDouble));
        assertFalse(ArrayUtils.isSameLength(twoArrayObject, nullArrayFloat));
        assertFalse(ArrayUtils.isSameLength(twoArrayBoolean, nullArrayObject));
        assertFalse(ArrayUtils.isSameLength(twoArrayBoolean, nullArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(twoArrayBoolean, nullArrayLong));
        assertFalse(ArrayUtils.isSameLength(twoArrayBoolean, nullArrayInt));
        assertFalse(ArrayUtils.isSameLength(twoArrayBoolean, nullArrayShort));
        assertFalse(ArrayUtils.isSameLength(twoArrayBoolean, nullArrayChar));
        assertFalse(ArrayUtils.isSameLength(twoArrayBoolean, nullArrayByte));
        assertFalse(ArrayUtils.isSameLength(twoArrayBoolean, nullArrayDouble));
        assertFalse(ArrayUtils.isSameLength(twoArrayBoolean, nullArrayFloat));
        assertFalse(ArrayUtils.isSameLength(twoArrayLong, nullArrayObject));
        assertFalse(ArrayUtils.isSameLength(twoArrayLong, nullArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(twoArrayLong, nullArrayLong));
        assertFalse(ArrayUtils.isSameLength(twoArrayLong, nullArrayInt));
        assertFalse(ArrayUtils.isSameLength(twoArrayLong, nullArrayShort));
        assertFalse(ArrayUtils.isSameLength(twoArrayLong, nullArrayChar));
        assertFalse(ArrayUtils.isSameLength(twoArrayLong, nullArrayByte));
        assertFalse(ArrayUtils.isSameLength(twoArrayLong, nullArrayDouble));
        assertFalse(ArrayUtils.isSameLength(twoArrayLong, nullArrayFloat));
        assertFalse(ArrayUtils.isSameLength(twoArrayInt, nullArrayObject));
        assertFalse(ArrayUtils.isSameLength(twoArrayInt, nullArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(twoArrayInt, nullArrayLong));
        assertFalse(ArrayUtils.isSameLength(twoArrayInt, nullArrayInt));
        assertFalse(ArrayUtils.isSameLength(twoArrayInt, nullArrayShort));
        assertFalse(ArrayUtils.isSameLength(twoArrayInt, nullArrayChar));
        assertFalse(ArrayUtils.isSameLength(twoArrayInt, nullArrayByte));
        assertFalse(ArrayUtils.isSameLength(twoArrayInt, nullArrayDouble));
        assertFalse(ArrayUtils.isSameLength(twoArrayInt, nullArrayFloat));
        assertFalse(ArrayUtils.isSameLength(twoArrayShort, nullArrayObject));
        assertFalse(ArrayUtils.isSameLength(twoArrayShort, nullArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(twoArrayShort, nullArrayLong));
        assertFalse(ArrayUtils.isSameLength(twoArrayShort, nullArrayInt));
        assertFalse(ArrayUtils.isSameLength(twoArrayShort, nullArrayShort));
        assertFalse(ArrayUtils.isSameLength(twoArrayShort, nullArrayChar));
        assertFalse(ArrayUtils.isSameLength(twoArrayShort, nullArrayByte));
        assertFalse(ArrayUtils.isSameLength(twoArrayShort, nullArrayDouble));
        assertFalse(ArrayUtils.isSameLength(twoArrayShort, nullArrayFloat));
        assertFalse(ArrayUtils.isSameLength(twoArrayChar, nullArrayObject));
        assertFalse(ArrayUtils.isSameLength(twoArrayChar, nullArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(twoArrayChar, nullArrayLong));
        assertFalse(ArrayUtils.isSameLength(twoArrayChar, nullArrayInt));
        assertFalse(ArrayUtils.isSameLength(twoArrayChar, nullArrayShort));
        assertFalse(ArrayUtils.isSameLength(twoArrayChar, nullArrayChar));
        assertFalse(ArrayUtils.isSameLength(twoArrayChar, nullArrayByte));
        assertFalse(ArrayUtils.isSameLength(twoArrayChar, nullArrayDouble));
        assertFalse(ArrayUtils.isSameLength(twoArrayChar, nullArrayFloat));
        assertFalse(ArrayUtils.isSameLength(twoArrayByte, nullArrayObject));
        assertFalse(ArrayUtils.isSameLength(twoArrayByte, nullArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(twoArrayByte, nullArrayLong));
        assertFalse(ArrayUtils.isSameLength(twoArrayByte, nullArrayInt));
        assertFalse(ArrayUtils.isSameLength(twoArrayByte, nullArrayShort));
        assertFalse(ArrayUtils.isSameLength(twoArrayByte, nullArrayChar));
        assertFalse(ArrayUtils.isSameLength(twoArrayByte, nullArrayByte));
        assertFalse(ArrayUtils.isSameLength(twoArrayByte, nullArrayDouble));
        assertFalse(ArrayUtils.isSameLength(twoArrayByte, nullArrayFloat));
        assertFalse(ArrayUtils.isSameLength(twoArrayDouble, nullArrayObject));
        assertFalse(ArrayUtils.isSameLength(twoArrayDouble, nullArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(twoArrayDouble, nullArrayLong));
        assertFalse(ArrayUtils.isSameLength(twoArrayDouble, nullArrayInt));
        assertFalse(ArrayUtils.isSameLength(twoArrayDouble, nullArrayShort));
        assertFalse(ArrayUtils.isSameLength(twoArrayDouble, nullArrayChar));
        assertFalse(ArrayUtils.isSameLength(twoArrayDouble, nullArrayByte));
        assertFalse(ArrayUtils.isSameLength(twoArrayDouble, nullArrayDouble));
        assertFalse(ArrayUtils.isSameLength(twoArrayDouble, nullArrayFloat));
        assertFalse(ArrayUtils.isSameLength(twoArrayFloat, nullArrayObject));
        assertFalse(ArrayUtils.isSameLength(twoArrayFloat, nullArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(twoArrayFloat, nullArrayLong));
        assertFalse(ArrayUtils.isSameLength(twoArrayFloat, nullArrayInt));
        assertFalse(ArrayUtils.isSameLength(twoArrayFloat, nullArrayShort));
        assertFalse(ArrayUtils.isSameLength(twoArrayFloat, nullArrayChar));
        assertFalse(ArrayUtils.isSameLength(twoArrayFloat, nullArrayByte));
        assertFalse(ArrayUtils.isSameLength(twoArrayFloat, nullArrayDouble));
        assertFalse(ArrayUtils.isSameLength(twoArrayFloat, nullArrayFloat));
        assertFalse(ArrayUtils.isSameLength(twoArrayObject, emptyArrayObject));
        assertFalse(ArrayUtils.isSameLength(twoArrayObject, emptyArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(twoArrayObject, emptyArrayLong));
        assertFalse(ArrayUtils.isSameLength(twoArrayObject, emptyArrayInt));
        assertFalse(ArrayUtils.isSameLength(twoArrayObject, emptyArrayShort));
        assertFalse(ArrayUtils.isSameLength(twoArrayObject, emptyArrayChar));
        assertFalse(ArrayUtils.isSameLength(twoArrayObject, emptyArrayByte));
        assertFalse(ArrayUtils.isSameLength(twoArrayObject, emptyArrayDouble));
        assertFalse(ArrayUtils.isSameLength(twoArrayObject, emptyArrayFloat));
        assertFalse(ArrayUtils.isSameLength(twoArrayBoolean, emptyArrayObject));
        assertFalse(ArrayUtils.isSameLength(twoArrayBoolean, emptyArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(twoArrayBoolean, emptyArrayLong));
        assertFalse(ArrayUtils.isSameLength(twoArrayBoolean, emptyArrayInt));
        assertFalse(ArrayUtils.isSameLength(twoArrayBoolean, emptyArrayShort));
        assertFalse(ArrayUtils.isSameLength(twoArrayBoolean, emptyArrayChar));
        assertFalse(ArrayUtils.isSameLength(twoArrayBoolean, emptyArrayByte));
        assertFalse(ArrayUtils.isSameLength(twoArrayBoolean, emptyArrayDouble));
        assertFalse(ArrayUtils.isSameLength(twoArrayBoolean, emptyArrayFloat));
        assertFalse(ArrayUtils.isSameLength(twoArrayLong, emptyArrayObject));
        assertFalse(ArrayUtils.isSameLength(twoArrayLong, emptyArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(twoArrayLong, emptyArrayLong));
        assertFalse(ArrayUtils.isSameLength(twoArrayLong, emptyArrayInt));
        assertFalse(ArrayUtils.isSameLength(twoArrayLong, emptyArrayShort));
        assertFalse(ArrayUtils.isSameLength(twoArrayLong, emptyArrayChar));
        assertFalse(ArrayUtils.isSameLength(twoArrayLong, emptyArrayByte));
        assertFalse(ArrayUtils.isSameLength(twoArrayLong, emptyArrayDouble));
        assertFalse(ArrayUtils.isSameLength(twoArrayLong, emptyArrayFloat));
        assertFalse(ArrayUtils.isSameLength(twoArrayInt, emptyArrayObject));
        assertFalse(ArrayUtils.isSameLength(twoArrayInt, emptyArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(twoArrayInt, emptyArrayLong));
        assertFalse(ArrayUtils.isSameLength(twoArrayInt, emptyArrayInt));
        assertFalse(ArrayUtils.isSameLength(twoArrayInt, emptyArrayShort));
        assertFalse(ArrayUtils.isSameLength(twoArrayInt, emptyArrayChar));
        assertFalse(ArrayUtils.isSameLength(twoArrayInt, emptyArrayByte));
        assertFalse(ArrayUtils.isSameLength(twoArrayInt, emptyArrayDouble));
        assertFalse(ArrayUtils.isSameLength(twoArrayInt, emptyArrayFloat));
        assertFalse(ArrayUtils.isSameLength(twoArrayShort, emptyArrayObject));
        assertFalse(ArrayUtils.isSameLength(twoArrayShort, emptyArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(twoArrayShort, emptyArrayLong));
        assertFalse(ArrayUtils.isSameLength(twoArrayShort, emptyArrayInt));
        assertFalse(ArrayUtils.isSameLength(twoArrayShort, emptyArrayShort));
        assertFalse(ArrayUtils.isSameLength(twoArrayShort, emptyArrayChar));
        assertFalse(ArrayUtils.isSameLength(twoArrayShort, emptyArrayByte));
        assertFalse(ArrayUtils.isSameLength(twoArrayShort, emptyArrayDouble));
        assertFalse(ArrayUtils.isSameLength(twoArrayShort, emptyArrayFloat));
        assertFalse(ArrayUtils.isSameLength(twoArrayChar, emptyArrayObject));
        assertFalse(ArrayUtils.isSameLength(twoArrayChar, emptyArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(twoArrayChar, emptyArrayLong));
        assertFalse(ArrayUtils.isSameLength(twoArrayChar, emptyArrayInt));
        assertFalse(ArrayUtils.isSameLength(twoArrayChar, emptyArrayShort));
        assertFalse(ArrayUtils.isSameLength(twoArrayChar, emptyArrayChar));
        assertFalse(ArrayUtils.isSameLength(twoArrayChar, emptyArrayByte));
        assertFalse(ArrayUtils.isSameLength(twoArrayChar, emptyArrayDouble));
        assertFalse(ArrayUtils.isSameLength(twoArrayChar, emptyArrayFloat));
        assertFalse(ArrayUtils.isSameLength(twoArrayByte, emptyArrayObject));
        assertFalse(ArrayUtils.isSameLength(twoArrayByte, emptyArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(twoArrayByte, emptyArrayLong));
        assertFalse(ArrayUtils.isSameLength(twoArrayByte, emptyArrayInt));
        assertFalse(ArrayUtils.isSameLength(twoArrayByte, emptyArrayShort));
        assertFalse(ArrayUtils.isSameLength(twoArrayByte, emptyArrayChar));
        assertFalse(ArrayUtils.isSameLength(twoArrayByte, emptyArrayByte));
        assertFalse(ArrayUtils.isSameLength(twoArrayByte, emptyArrayDouble));
        assertFalse(ArrayUtils.isSameLength(twoArrayByte, emptyArrayFloat));
        assertFalse(ArrayUtils.isSameLength(twoArrayDouble, emptyArrayObject));
        assertFalse(ArrayUtils.isSameLength(twoArrayDouble, emptyArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(twoArrayDouble, emptyArrayLong));
        assertFalse(ArrayUtils.isSameLength(twoArrayDouble, emptyArrayInt));
        assertFalse(ArrayUtils.isSameLength(twoArrayDouble, emptyArrayShort));
        assertFalse(ArrayUtils.isSameLength(twoArrayDouble, emptyArrayChar));
        assertFalse(ArrayUtils.isSameLength(twoArrayDouble, emptyArrayByte));
        assertFalse(ArrayUtils.isSameLength(twoArrayDouble, emptyArrayDouble));
        assertFalse(ArrayUtils.isSameLength(twoArrayDouble, emptyArrayFloat));
        assertFalse(ArrayUtils.isSameLength(twoArrayFloat, emptyArrayObject));
        assertFalse(ArrayUtils.isSameLength(twoArrayFloat, emptyArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(twoArrayFloat, emptyArrayLong));
        assertFalse(ArrayUtils.isSameLength(twoArrayFloat, emptyArrayInt));
        assertFalse(ArrayUtils.isSameLength(twoArrayFloat, emptyArrayShort));
        assertFalse(ArrayUtils.isSameLength(twoArrayFloat, emptyArrayChar));
        assertFalse(ArrayUtils.isSameLength(twoArrayFloat, emptyArrayByte));
        assertFalse(ArrayUtils.isSameLength(twoArrayFloat, emptyArrayDouble));
        assertFalse(ArrayUtils.isSameLength(twoArrayFloat, emptyArrayFloat));
        assertFalse(ArrayUtils.isSameLength(twoArrayObject, oneArrayObject));
        assertFalse(ArrayUtils.isSameLength(twoArrayObject, oneArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(twoArrayObject, oneArrayLong));
        assertFalse(ArrayUtils.isSameLength(twoArrayObject, oneArrayInt));
        assertFalse(ArrayUtils.isSameLength(twoArrayObject, oneArrayShort));
        assertFalse(ArrayUtils.isSameLength(twoArrayObject, oneArrayChar));
        assertFalse(ArrayUtils.isSameLength(twoArrayObject, oneArrayByte));
        assertFalse(ArrayUtils.isSameLength(twoArrayObject, oneArrayDouble));
        assertFalse(ArrayUtils.isSameLength(twoArrayObject, oneArrayFloat));
        assertFalse(ArrayUtils.isSameLength(twoArrayBoolean, oneArrayObject));
        assertFalse(ArrayUtils.isSameLength(twoArrayBoolean, oneArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(twoArrayBoolean, oneArrayLong));
        assertFalse(ArrayUtils.isSameLength(twoArrayBoolean, oneArrayInt));
        assertFalse(ArrayUtils.isSameLength(twoArrayBoolean, oneArrayShort));
        assertFalse(ArrayUtils.isSameLength(twoArrayBoolean, oneArrayChar));
        assertFalse(ArrayUtils.isSameLength(twoArrayBoolean, oneArrayByte));
        assertFalse(ArrayUtils.isSameLength(twoArrayBoolean, oneArrayDouble));
        assertFalse(ArrayUtils.isSameLength(twoArrayBoolean, oneArrayFloat));
        assertFalse(ArrayUtils.isSameLength(twoArrayLong, oneArrayObject));
        assertFalse(ArrayUtils.isSameLength(twoArrayLong, oneArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(twoArrayLong, oneArrayLong));
        assertFalse(ArrayUtils.isSameLength(twoArrayLong, oneArrayInt));
        assertFalse(ArrayUtils.isSameLength(twoArrayLong, oneArrayShort));
        assertFalse(ArrayUtils.isSameLength(twoArrayLong, oneArrayChar));
        assertFalse(ArrayUtils.isSameLength(twoArrayLong, oneArrayByte));
        assertFalse(ArrayUtils.isSameLength(twoArrayLong, oneArrayDouble));
        assertFalse(ArrayUtils.isSameLength(twoArrayLong, oneArrayFloat));
        assertFalse(ArrayUtils.isSameLength(twoArrayInt, oneArrayObject));
        assertFalse(ArrayUtils.isSameLength(twoArrayInt, oneArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(twoArrayInt, oneArrayLong));
        assertFalse(ArrayUtils.isSameLength(twoArrayInt, oneArrayInt));
        assertFalse(ArrayUtils.isSameLength(twoArrayInt, oneArrayShort));
        assertFalse(ArrayUtils.isSameLength(twoArrayInt, oneArrayChar));
        assertFalse(ArrayUtils.isSameLength(twoArrayInt, oneArrayByte));
        assertFalse(ArrayUtils.isSameLength(twoArrayInt, oneArrayDouble));
        assertFalse(ArrayUtils.isSameLength(twoArrayInt, oneArrayFloat));
        assertFalse(ArrayUtils.isSameLength(twoArrayShort, oneArrayObject));
        assertFalse(ArrayUtils.isSameLength(twoArrayShort, oneArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(twoArrayShort, oneArrayLong));
        assertFalse(ArrayUtils.isSameLength(twoArrayShort, oneArrayInt));
        assertFalse(ArrayUtils.isSameLength(twoArrayShort, oneArrayShort));
        assertFalse(ArrayUtils.isSameLength(twoArrayShort, oneArrayChar));
        assertFalse(ArrayUtils.isSameLength(twoArrayShort, oneArrayByte));
        assertFalse(ArrayUtils.isSameLength(twoArrayShort, oneArrayDouble));
        assertFalse(ArrayUtils.isSameLength(twoArrayShort, oneArrayFloat));
        assertFalse(ArrayUtils.isSameLength(twoArrayChar, oneArrayObject));
        assertFalse(ArrayUtils.isSameLength(twoArrayChar, oneArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(twoArrayChar, oneArrayLong));
        assertFalse(ArrayUtils.isSameLength(twoArrayChar, oneArrayInt));
        assertFalse(ArrayUtils.isSameLength(twoArrayChar, oneArrayShort));
        assertFalse(ArrayUtils.isSameLength(twoArrayChar, oneArrayChar));
        assertFalse(ArrayUtils.isSameLength(twoArrayChar, oneArrayByte));
        assertFalse(ArrayUtils.isSameLength(twoArrayChar, oneArrayDouble));
        assertFalse(ArrayUtils.isSameLength(twoArrayChar, oneArrayFloat));
        assertFalse(ArrayUtils.isSameLength(twoArrayByte, oneArrayObject));
        assertFalse(ArrayUtils.isSameLength(twoArrayByte, oneArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(twoArrayByte, oneArrayLong));
        assertFalse(ArrayUtils.isSameLength(twoArrayByte, oneArrayInt));
        assertFalse(ArrayUtils.isSameLength(twoArrayByte, oneArrayShort));
        assertFalse(ArrayUtils.isSameLength(twoArrayByte, oneArrayChar));
        assertFalse(ArrayUtils.isSameLength(twoArrayByte, oneArrayByte));
        assertFalse(ArrayUtils.isSameLength(twoArrayByte, oneArrayDouble));
        assertFalse(ArrayUtils.isSameLength(twoArrayByte, oneArrayFloat));
        assertFalse(ArrayUtils.isSameLength(twoArrayDouble, oneArrayObject));
        assertFalse(ArrayUtils.isSameLength(twoArrayDouble, oneArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(twoArrayDouble, oneArrayLong));
        assertFalse(ArrayUtils.isSameLength(twoArrayDouble, oneArrayInt));
        assertFalse(ArrayUtils.isSameLength(twoArrayDouble, oneArrayShort));
        assertFalse(ArrayUtils.isSameLength(twoArrayDouble, oneArrayChar));
        assertFalse(ArrayUtils.isSameLength(twoArrayDouble, oneArrayByte));
        assertFalse(ArrayUtils.isSameLength(twoArrayDouble, oneArrayDouble));
        assertFalse(ArrayUtils.isSameLength(twoArrayDouble, oneArrayFloat));
        assertFalse(ArrayUtils.isSameLength(twoArrayFloat, oneArrayObject));
        assertFalse(ArrayUtils.isSameLength(twoArrayFloat, oneArrayBoolean));
        assertFalse(ArrayUtils.isSameLength(twoArrayFloat, oneArrayLong));
        assertFalse(ArrayUtils.isSameLength(twoArrayFloat, oneArrayInt));
        assertFalse(ArrayUtils.isSameLength(twoArrayFloat, oneArrayShort));
        assertFalse(ArrayUtils.isSameLength(twoArrayFloat, oneArrayChar));
        assertFalse(ArrayUtils.isSameLength(twoArrayFloat, oneArrayByte));
        assertFalse(ArrayUtils.isSameLength(twoArrayFloat, oneArrayDouble));
        assertFalse(ArrayUtils.isSameLength(twoArrayFloat, oneArrayFloat));
        assertTrue(ArrayUtils.isSameLength(twoArrayObject, twoArrayObject));
        assertTrue(ArrayUtils.isSameLength(twoArrayObject, twoArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(twoArrayObject, twoArrayLong));
        assertTrue(ArrayUtils.isSameLength(twoArrayObject, twoArrayInt));
        assertTrue(ArrayUtils.isSameLength(twoArrayObject, twoArrayShort));
        assertTrue(ArrayUtils.isSameLength(twoArrayObject, twoArrayChar));
        assertTrue(ArrayUtils.isSameLength(twoArrayObject, twoArrayByte));
        assertTrue(ArrayUtils.isSameLength(twoArrayObject, twoArrayDouble));
        assertTrue(ArrayUtils.isSameLength(twoArrayObject, twoArrayFloat));
        assertTrue(ArrayUtils.isSameLength(twoArrayBoolean, twoArrayObject));
        assertTrue(ArrayUtils.isSameLength(twoArrayBoolean, twoArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(twoArrayBoolean, twoArrayLong));
        assertTrue(ArrayUtils.isSameLength(twoArrayBoolean, twoArrayInt));
        assertTrue(ArrayUtils.isSameLength(twoArrayBoolean, twoArrayShort));
        assertTrue(ArrayUtils.isSameLength(twoArrayBoolean, twoArrayChar));
        assertTrue(ArrayUtils.isSameLength(twoArrayBoolean, twoArrayByte));
        assertTrue(ArrayUtils.isSameLength(twoArrayBoolean, twoArrayDouble));
        assertTrue(ArrayUtils.isSameLength(twoArrayBoolean, twoArrayFloat));
        assertTrue(ArrayUtils.isSameLength(twoArrayLong, twoArrayObject));
        assertTrue(ArrayUtils.isSameLength(twoArrayLong, twoArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(twoArrayLong, twoArrayLong));
        assertTrue(ArrayUtils.isSameLength(twoArrayLong, twoArrayInt));
        assertTrue(ArrayUtils.isSameLength(twoArrayLong, twoArrayShort));
        assertTrue(ArrayUtils.isSameLength(twoArrayLong, twoArrayChar));
        assertTrue(ArrayUtils.isSameLength(twoArrayLong, twoArrayByte));
        assertTrue(ArrayUtils.isSameLength(twoArrayLong, twoArrayDouble));
        assertTrue(ArrayUtils.isSameLength(twoArrayLong, twoArrayFloat));
        assertTrue(ArrayUtils.isSameLength(twoArrayInt, twoArrayObject));
        assertTrue(ArrayUtils.isSameLength(twoArrayInt, twoArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(twoArrayInt, twoArrayLong));
        assertTrue(ArrayUtils.isSameLength(twoArrayInt, twoArrayInt));
        assertTrue(ArrayUtils.isSameLength(twoArrayInt, twoArrayShort));
        assertTrue(ArrayUtils.isSameLength(twoArrayInt, twoArrayChar));
        assertTrue(ArrayUtils.isSameLength(twoArrayInt, twoArrayByte));
        assertTrue(ArrayUtils.isSameLength(twoArrayInt, twoArrayDouble));
        assertTrue(ArrayUtils.isSameLength(twoArrayInt, twoArrayFloat));
        assertTrue(ArrayUtils.isSameLength(twoArrayShort, twoArrayObject));
        assertTrue(ArrayUtils.isSameLength(twoArrayShort, twoArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(twoArrayShort, twoArrayLong));
        assertTrue(ArrayUtils.isSameLength(twoArrayShort, twoArrayInt));
        assertTrue(ArrayUtils.isSameLength(twoArrayShort, twoArrayShort));
        assertTrue(ArrayUtils.isSameLength(twoArrayShort, twoArrayChar));
        assertTrue(ArrayUtils.isSameLength(twoArrayShort, twoArrayByte));
        assertTrue(ArrayUtils.isSameLength(twoArrayShort, twoArrayDouble));
        assertTrue(ArrayUtils.isSameLength(twoArrayShort, twoArrayFloat));
        assertTrue(ArrayUtils.isSameLength(twoArrayChar, twoArrayObject));
        assertTrue(ArrayUtils.isSameLength(twoArrayChar, twoArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(twoArrayChar, twoArrayLong));
        assertTrue(ArrayUtils.isSameLength(twoArrayChar, twoArrayInt));
        assertTrue(ArrayUtils.isSameLength(twoArrayChar, twoArrayShort));
        assertTrue(ArrayUtils.isSameLength(twoArrayChar, twoArrayChar));
        assertTrue(ArrayUtils.isSameLength(twoArrayChar, twoArrayByte));
        assertTrue(ArrayUtils.isSameLength(twoArrayChar, twoArrayDouble));
        assertTrue(ArrayUtils.isSameLength(twoArrayChar, twoArrayFloat));
        assertTrue(ArrayUtils.isSameLength(twoArrayByte, twoArrayObject));
        assertTrue(ArrayUtils.isSameLength(twoArrayByte, twoArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(twoArrayByte, twoArrayLong));
        assertTrue(ArrayUtils.isSameLength(twoArrayByte, twoArrayInt));
        assertTrue(ArrayUtils.isSameLength(twoArrayByte, twoArrayShort));
        assertTrue(ArrayUtils.isSameLength(twoArrayByte, twoArrayChar));
        assertTrue(ArrayUtils.isSameLength(twoArrayByte, twoArrayByte));
        assertTrue(ArrayUtils.isSameLength(twoArrayByte, twoArrayDouble));
        assertTrue(ArrayUtils.isSameLength(twoArrayByte, twoArrayFloat));
        assertTrue(ArrayUtils.isSameLength(twoArrayDouble, twoArrayObject));
        assertTrue(ArrayUtils.isSameLength(twoArrayDouble, twoArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(twoArrayDouble, twoArrayLong));
        assertTrue(ArrayUtils.isSameLength(twoArrayDouble, twoArrayInt));
        assertTrue(ArrayUtils.isSameLength(twoArrayDouble, twoArrayShort));
        assertTrue(ArrayUtils.isSameLength(twoArrayDouble, twoArrayChar));
        assertTrue(ArrayUtils.isSameLength(twoArrayDouble, twoArrayByte));
        assertTrue(ArrayUtils.isSameLength(twoArrayDouble, twoArrayDouble));
        assertTrue(ArrayUtils.isSameLength(twoArrayDouble, twoArrayFloat));
        assertTrue(ArrayUtils.isSameLength(twoArrayFloat, twoArrayObject));
        assertTrue(ArrayUtils.isSameLength(twoArrayFloat, twoArrayBoolean));
        assertTrue(ArrayUtils.isSameLength(twoArrayFloat, twoArrayLong));
        assertTrue(ArrayUtils.isSameLength(twoArrayFloat, twoArrayInt));
        assertTrue(ArrayUtils.isSameLength(twoArrayFloat, twoArrayShort));
        assertTrue(ArrayUtils.isSameLength(twoArrayFloat, twoArrayChar));
        assertTrue(ArrayUtils.isSameLength(twoArrayFloat, twoArrayByte));
        assertTrue(ArrayUtils.isSameLength(twoArrayFloat, twoArrayDouble));
        assertTrue(ArrayUtils.isSameLength(twoArrayFloat, twoArrayFloat));
    }

    @Test
    public void testSameLengthBoolean() {
        final boolean[] nullArray = null;
        final boolean[] emptyArray = {};
        final boolean[] oneArray = {true};
        final boolean[] twoArray = {true, false};

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
        final byte[] emptyArray = {};
        final byte[] oneArray = {3};
        final byte[] twoArray = {4, 6};

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
        final char[] emptyArray = {};
        final char[] oneArray = {'f'};
        final char[] twoArray = {'d', 't'};

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
        final double[] emptyArray = {};
        final double[] oneArray = {1.3d};
        final double[] twoArray = {4.5d, 6.3d};

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
        final float[] emptyArray = {};
        final float[] oneArray = {2.5f};
        final float[] twoArray = {6.4f, 5.8f};

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
        final int[] emptyArray = {};
        final int[] oneArray = {4};
        final int[] twoArray = {5, 7};

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
        final long[] emptyArray = {};
        final long[] oneArray = {0L};
        final long[] twoArray = {0L, 76L};

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
        final short[] emptyArray = {};
        final short[] oneArray = {4};
        final short[] twoArray = {6, 8};

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
    public void testSameType() {
        assertThrows(IllegalArgumentException.class, () -> ArrayUtils.isSameType(null, null));
        assertThrows(IllegalArgumentException.class, () -> ArrayUtils.isSameType(null, new Object[0]));
        assertThrows(IllegalArgumentException.class, () -> ArrayUtils.isSameType(new Object[0], null));

        assertTrue(ArrayUtils.isSameType(new Object[0], new Object[0]));
        assertFalse(ArrayUtils.isSameType(new String[0], new Object[0]));
        assertTrue(ArrayUtils.isSameType(new String[0][0], new String[0][0]));
        assertFalse(ArrayUtils.isSameType(new String[0], new String[0][0]));
        assertFalse(ArrayUtils.isSameType(new String[0][0], new String[0]));
    }

    @Test
    public void testShiftAllByte() {
        final byte[] array = {1, 2, 3, 4};
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
    public void testShiftAllChar() {
        final char[] array = {1, 2, 3, 4};
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
    public void testShiftAllDouble() {
        final double[] array = {1, 2, 3, 4};
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
    public void testShiftAllFloat() {
        final float[] array = {1, 2, 3, 4};
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
    public void testShiftAllInt() {
        final int[] array = {1, 2, 3, 4};
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
    public void testShiftAllLong() {
        final long[] array = {1, 2, 3, 4};
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
    public void testShiftAllObject() {
        final String[] array = {"1", "2", "3", "4"};
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
    public void testShiftAllShort() {
        final short[] array = {1, 2, 3, 4};
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
    public void testShiftBoolean() {
        final boolean[] array = {true, true, false, false};

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
    public void testShiftByte() {
        final byte[] array = {1, 2, 3, 4};
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
    public void testShiftChar() {
        final char[] array = {1, 2, 3, 4};
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
    public void testShiftDouble() {
        final double[] array = {1, 2, 3, 4};
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
    public void testShiftFloat() {
        final float[] array = {1, 2, 3, 4};
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
    public void testShiftInt() {
        final int[] array = {1, 2, 3, 4};
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
    public void testShiftLong() {
        final long[] array = {1, 2, 3, 4};
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
    public void testShiftNullBoolean() {
        final boolean[] array = null;

        ArrayUtils.shift(array, 1);
        assertNull(array);
    }

    @Test
    public void testShiftNullDouble() {
        final double[] array = null;

        ArrayUtils.shift(array, 1);
        assertNull(array);
    }

    @Test
    public void testShiftNullFloat() {
        final float[] array = null;

        ArrayUtils.shift(array, 1);
        assertNull(array);
    }

    @Test
    public void testShiftNullInt() {
        final int[] array = null;

        ArrayUtils.shift(array, 1);
        assertNull(array);
    }

    @Test
    public void testShiftNullLong() {
        final long[] array = null;

        ArrayUtils.shift(array, 1);
        assertNull(array);
    }

    @Test
    public void testShiftNullObject() {
        final String[] array = null;

        ArrayUtils.shift(array, 1);
        assertNull(array);
    }

    @Test
    public void testShiftNullShort() {
        final short[] array = null;

        ArrayUtils.shift(array, 1);
        assertNull(array);
    }

    @Test
    public void testShiftObject() {
        final String[] array = {"1", "2", "3", "4"};
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
    public void testShiftRangeByte() {
        final byte[] array = {1, 2, 3, 4, 5};
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
    public void testShiftRangeChar() {
        final char[] array = {1, 2, 3, 4, 5};
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
    public void testShiftRangeDouble() {
        final double[] array = {1, 2, 3, 4, 5};
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
    public void testShiftRangeFloat() {
        final float[] array = {1, 2, 3, 4, 5};
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
    public void testShiftRangeInt() {
        final int[] array = {1, 2, 3, 4, 5};
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
    public void testShiftRangeLong() {
        final long[] array = {1, 2, 3, 4, 5};
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
        final byte[] array = {1, 2, 3, 4};
        ArrayUtils.shift(array, 1, 1, 1);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);
        assertEquals(4, array[3]);
    }

    @Test
    public void testShiftRangeNoElemChar() {
        final char[] array = {1, 2, 3, 4};
        ArrayUtils.shift(array, 1, 1, 1);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);
        assertEquals(4, array[3]);
    }

    @Test
    public void testShiftRangeNoElemDouble() {
        final double[] array = {1, 2, 3, 4};
        ArrayUtils.shift(array, 1, 1, 1);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);
        assertEquals(4, array[3]);
    }

    @Test
    public void testShiftRangeNoElemFloat() {
        final float[] array = {1, 2, 3, 4};
        ArrayUtils.shift(array, 1, 1, 1);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);
        assertEquals(4, array[3]);
    }

    @Test
    public void testShiftRangeNoElemInt() {
        final int[] array = {1, 2, 3, 4};
        ArrayUtils.shift(array, 1, 1, 1);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);
        assertEquals(4, array[3]);
    }

    @Test
    public void testShiftRangeNoElemLong() {
        final long[] array = {1, 2, 3, 4};
        ArrayUtils.shift(array, 1, 1, 1);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);
        assertEquals(4, array[3]);
    }

    @Test
    public void testShiftRangeNoElemObject() {
        final String[] array = {"1", "2", "3", "4"};
        ArrayUtils.shift(array, 1, 1, 1);
        assertEquals("1", array[0]);
        assertEquals("2", array[1]);
        assertEquals("3", array[2]);
        assertEquals("4", array[3]);
    }

    @Test
    public void testShiftRangeNoElemShort() {
        final short[] array = {1, 2, 3, 4};
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
    public void testShiftRangeNullChar() {
        final char[] array = null;
        ArrayUtils.shift(array, 1, 1, 1);
        assertNull(array);
    }

    @Test
    public void testShiftRangeNullDouble() {
        final double[] array = null;
        ArrayUtils.shift(array, 1, 1, 1);
        assertNull(array);
    }

    @Test
    public void testShiftRangeNullFloat() {
        final float[] array = null;
        ArrayUtils.shift(array, 1, 1, 1);
        assertNull(array);
    }

    @Test
    public void testShiftRangeNullInt() {
        final int[] array = null;
        ArrayUtils.shift(array, 1, 1, 1);
        assertNull(array);
    }

    @Test
    public void testShiftRangeNullLong() {
        final long[] array = null;
        ArrayUtils.shift(array, 1, 1, 1);
        assertNull(array);
    }

    @Test
    public void testShiftRangeNullObject() {
        final String[] array = null;
        ArrayUtils.shift(array, 1, 1, 1);
        assertNull(array);
    }

    @Test
    public void testShiftRangeNullShort() {
        final short[] array = null;

        ArrayUtils.shift(array, 1, 1, 1);
        assertNull(array);
    }

    @Test
    public void testShiftRangeObject() {
        final String[] array = {"1", "2", "3", "4", "5"};
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
    public void testShiftRangeShort() {
        final short[] array = {1, 2, 3, 4, 5};
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
    public void testShiftShort() {
        short[] array = {1, 2, 3, 4};
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
    public void testShuffle() {
        final String[] array1 = {"1", "2", "3", "4", "5", "6", "7", "8", "9", "10"};
        final String[] array2 = ArrayUtils.clone(array1);

        ArrayUtils.shuffle(array1, new Random(SEED));
        assertFalse(Arrays.equals(array1, array2));
        for (final String element : array2) {
            assertTrue(ArrayUtils.contains(array1, element), "Element " + element + " not found");
        }
    }

    @Test
    public void testShuffleBoolean() {
        final boolean[] array1 = {true, false, true, true, false, false, true, false, false, true};
        final boolean[] array2 = ArrayUtils.clone(array1);

        ArrayUtils.shuffle(array1, new Random(SEED));
        assertFalse(Arrays.equals(array1, array2));
        assertEquals(5, ArrayUtils.removeAllOccurrences(array1, true).length);
    }

    @Test
    public void testShuffleByte() {
        final byte[] array1 = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
        final byte[] array2 = ArrayUtils.clone(array1);

        ArrayUtils.shuffle(array1, new Random(SEED));
        assertFalse(Arrays.equals(array1, array2));
        for (final byte element : array2) {
            assertTrue(ArrayUtils.contains(array1, element), "Element " + element + " not found");
        }
    }

    @Test
    public void testShuffleChar() {
        final char[] array1 = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
        final char[] array2 = ArrayUtils.clone(array1);

        ArrayUtils.shuffle(array1, new Random(SEED));
        assertFalse(Arrays.equals(array1, array2));
        for (final char element : array2) {
            assertTrue(ArrayUtils.contains(array1, element), "Element " + element + " not found");
        }
    }

    @Test
    public void testShuffleDouble() {
        final double[] array1 = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
        final double[] array2 = ArrayUtils.clone(array1);

        ArrayUtils.shuffle(array1, new Random(SEED));
        assertFalse(Arrays.equals(array1, array2));
        for (final double element : array2) {
            assertTrue(ArrayUtils.contains(array1, element), "Element " + element + " not found");
        }
    }

    @Test
    public void testShuffleFloat() {
        final float[] array1 = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
        final float[] array2 = ArrayUtils.clone(array1);

        ArrayUtils.shuffle(array1, new Random(SEED));
        assertFalse(Arrays.equals(array1, array2));
        for (final float element : array2) {
            assertTrue(ArrayUtils.contains(array1, element), "Element " + element + " not found");
        }
    }

    @Test
    public void testShuffleInt() {
        final int[] array1 = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
        final int[] array2 = ArrayUtils.clone(array1);

        ArrayUtils.shuffle(array1, new Random(SEED));
        assertFalse(Arrays.equals(array1, array2));
        for (final int element : array2) {
            assertTrue(ArrayUtils.contains(array1, element), "Element " + element + " not found");
        }
    }

    @Test
    public void testShuffleLong() {
        final long[] array1 = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
        final long[] array2 = ArrayUtils.clone(array1);

        ArrayUtils.shuffle(array1, new Random(SEED));
        assertFalse(Arrays.equals(array1, array2));
        for (final long element : array2) {
            assertTrue(ArrayUtils.contains(array1, element), "Element " + element + " not found");
        }
    }

    @Test
    public void testShuffleShort() {
        final short[] array1 = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
        final short[] array2 = ArrayUtils.clone(array1);

        ArrayUtils.shuffle(array1, new Random(SEED));
        assertFalse(Arrays.equals(array1, array2));
        for (final short element : array2) {
            assertTrue(ArrayUtils.contains(array1, element), "Element " + element + " not found");
        }
    }

    @Test
    public void testSubarrayBoolean() {
        final boolean[] nullArray = null;
        final boolean[] array = {true, true, false, true, false, true};
        final boolean[] leftSubarray = {true, true, false, true};
        final boolean[] midSubarray = {true, false, true, false};
        final boolean[] rightSubarray = {false, true, false, true};

        assertTrue(ArrayUtils.isEquals(leftSubarray, ArrayUtils.subarray(array, 0, 4)), "0 start, mid end");
        assertTrue(ArrayUtils.isEquals(array, ArrayUtils.subarray(array, 0, array.length)), "0 start, length end");
        assertTrue(ArrayUtils.isEquals(midSubarray, ArrayUtils.subarray(array, 1, 5)), "mid start, mid end");
        assertTrue(ArrayUtils.isEquals(rightSubarray, ArrayUtils.subarray(array, 2, array.length)),
                "mid start, length end");

        assertNull(ArrayUtils.subarray(nullArray, 0, 3), "null input");
        assertEquals(ArrayUtils.EMPTY_BOOLEAN_ARRAY, ArrayUtils.subarray(ArrayUtils.EMPTY_BOOLEAN_ARRAY, 1, 2),
                "empty array");
        assertEquals(ArrayUtils.EMPTY_BOOLEAN_ARRAY, ArrayUtils.subarray(array, 4, 2), "start > end");
        assertEquals(ArrayUtils.EMPTY_BOOLEAN_ARRAY, ArrayUtils.subarray(array, 3, 3), "start == end");
        assertTrue(ArrayUtils.isEquals(leftSubarray, ArrayUtils.subarray(array, -2, 4)),
                "start undershoot, normal end");
        assertEquals(ArrayUtils.EMPTY_BOOLEAN_ARRAY, ArrayUtils.subarray(array, 33, 4), "start overshoot, any end");
        assertTrue(ArrayUtils.isEquals(rightSubarray, ArrayUtils.subarray(array, 2, 33)),
                "normal start, end overshoot");
        assertTrue(ArrayUtils.isEquals(array, ArrayUtils.subarray(array, -2, 12)), "start undershoot, end overshoot");

        // empty-return tests

        assertSame(ArrayUtils.EMPTY_BOOLEAN_ARRAY, ArrayUtils.subarray(ArrayUtils.EMPTY_BOOLEAN_ARRAY, 1, 2),
                "empty array, object test");
        assertSame(ArrayUtils.EMPTY_BOOLEAN_ARRAY, ArrayUtils.subarray(array, 4, 1), "start > end, object test");
        assertSame(ArrayUtils.EMPTY_BOOLEAN_ARRAY, ArrayUtils.subarray(array, 3, 3), "start == end, object test");
        assertSame(ArrayUtils.EMPTY_BOOLEAN_ARRAY, ArrayUtils.subarray(array, 8733, 4),
                "start overshoot, any end, object test");

        // array type tests

        assertSame(boolean.class, ArrayUtils.subarray(array, 2, 4).getClass().getComponentType(), "boolean type");
    }

    @Test
    public void testSubarrayByte() {
        final byte[] nullArray = null;
        final byte[] array = {10, 11, 12, 13, 14, 15};
        final byte[] leftSubarray = {10, 11, 12, 13};
        final byte[] midSubarray = {11, 12, 13, 14};
        final byte[] rightSubarray = {12, 13, 14, 15};

        assertTrue(ArrayUtils.isEquals(leftSubarray, ArrayUtils.subarray(array, 0, 4)), "0 start, mid end");
        assertTrue(ArrayUtils.isEquals(array, ArrayUtils.subarray(array, 0, array.length)), "0 start, length end");
        assertTrue(ArrayUtils.isEquals(midSubarray, ArrayUtils.subarray(array, 1, 5)), "mid start, mid end");
        assertTrue(ArrayUtils.isEquals(rightSubarray, ArrayUtils.subarray(array, 2, array.length)),
                "mid start, length end");

        assertNull(ArrayUtils.subarray(nullArray, 0, 3), "null input");
        assertEquals(ArrayUtils.EMPTY_BYTE_ARRAY, ArrayUtils.subarray(ArrayUtils.EMPTY_BYTE_ARRAY, 1, 2),
                "empty array");
        assertEquals(ArrayUtils.EMPTY_BYTE_ARRAY, ArrayUtils.subarray(array, 4, 2), "start > end");
        assertEquals(ArrayUtils.EMPTY_BYTE_ARRAY, ArrayUtils.subarray(array, 3, 3), "start == end");
        assertTrue(ArrayUtils.isEquals(leftSubarray, ArrayUtils.subarray(array, -2, 4)),
                "start undershoot, normal end");
        assertEquals(ArrayUtils.EMPTY_BYTE_ARRAY, ArrayUtils.subarray(array, 33, 4), "start overshoot, any end");
        assertTrue(ArrayUtils.isEquals(rightSubarray, ArrayUtils.subarray(array, 2, 33)),
                "normal start, end overshoot");
        assertTrue(ArrayUtils.isEquals(array, ArrayUtils.subarray(array, -2, 12)), "start undershoot, end overshoot");

        // empty-return tests

        assertSame(ArrayUtils.EMPTY_BYTE_ARRAY, ArrayUtils.subarray(ArrayUtils.EMPTY_BYTE_ARRAY, 1, 2),
                "empty array, object test");
        assertSame(ArrayUtils.EMPTY_BYTE_ARRAY, ArrayUtils.subarray(array, 4, 1), "start > end, object test");
        assertSame(ArrayUtils.EMPTY_BYTE_ARRAY, ArrayUtils.subarray(array, 3, 3), "start == end, object test");
        assertSame(ArrayUtils.EMPTY_BYTE_ARRAY, ArrayUtils.subarray(array, 8733, 4),
                "start overshoot, any end, object test");

        // array type tests

        assertSame(byte.class, ArrayUtils.subarray(array, 2, 4).getClass().getComponentType(), "byte type");
    }

    @Test
    public void testSubarrayDouble() {
        final double[] nullArray = null;
        final double[] array = {10.123, 11.234, 12.345, 13.456, 14.567, 15.678};
        final double[] leftSubarray = {10.123, 11.234, 12.345, 13.456};
        final double[] midSubarray = {11.234, 12.345, 13.456, 14.567};
        final double[] rightSubarray = {12.345, 13.456, 14.567, 15.678};

        assertTrue(ArrayUtils.isEquals(leftSubarray, ArrayUtils.subarray(array, 0, 4)), "0 start, mid end");
        assertTrue(ArrayUtils.isEquals(array, ArrayUtils.subarray(array, 0, array.length)), "0 start, length end");
        assertTrue(ArrayUtils.isEquals(midSubarray, ArrayUtils.subarray(array, 1, 5)), "mid start, mid end");
        assertTrue(ArrayUtils.isEquals(rightSubarray, ArrayUtils.subarray(array, 2, array.length)),
                "mid start, length end");

        assertNull(ArrayUtils.subarray(nullArray, 0, 3), "null input");
        assertEquals(ArrayUtils.EMPTY_DOUBLE_ARRAY, ArrayUtils.subarray(ArrayUtils.EMPTY_DOUBLE_ARRAY, 1, 2),
                "empty array");
        assertEquals(ArrayUtils.EMPTY_DOUBLE_ARRAY, ArrayUtils.subarray(array, 4, 2), "start > end");
        assertEquals(ArrayUtils.EMPTY_DOUBLE_ARRAY, ArrayUtils.subarray(array, 3, 3), "start == end");
        assertTrue(ArrayUtils.isEquals(leftSubarray, ArrayUtils.subarray(array, -2, 4)),
                "start undershoot, normal end");
        assertEquals(ArrayUtils.EMPTY_DOUBLE_ARRAY, ArrayUtils.subarray(array, 33, 4), "start overshoot, any end");
        assertTrue(ArrayUtils.isEquals(rightSubarray, ArrayUtils.subarray(array, 2, 33)),
                "normal start, end overshoot");
        assertTrue(ArrayUtils.isEquals(array, ArrayUtils.subarray(array, -2, 12)), "start undershoot, end overshoot");

        // empty-return tests

        assertSame(ArrayUtils.EMPTY_DOUBLE_ARRAY, ArrayUtils.subarray(ArrayUtils.EMPTY_DOUBLE_ARRAY, 1, 2),
                "empty array, object test");
        assertSame(ArrayUtils.EMPTY_DOUBLE_ARRAY, ArrayUtils.subarray(array, 4, 1), "start > end, object test");
        assertSame(ArrayUtils.EMPTY_DOUBLE_ARRAY, ArrayUtils.subarray(array, 3, 3), "start == end, object test");
        assertSame(ArrayUtils.EMPTY_DOUBLE_ARRAY, ArrayUtils.subarray(array, 8733, 4),
                "start overshoot, any end, object test");

        // array type tests

        assertSame(double.class, ArrayUtils.subarray(array, 2, 4).getClass().getComponentType(), "double type");
    }

    @Test
    public void testSubarrayFloat() {
        final float[] nullArray = null;
        final float[] array = {10, 11, 12, 13, 14, 15};
        final float[] leftSubarray = {10, 11, 12, 13};
        final float[] midSubarray = {11, 12, 13, 14};
        final float[] rightSubarray = {12, 13, 14, 15};

        assertTrue(ArrayUtils.isEquals(leftSubarray, ArrayUtils.subarray(array, 0, 4)), "0 start, mid end");
        assertTrue(ArrayUtils.isEquals(array, ArrayUtils.subarray(array, 0, array.length)), "0 start, length end");
        assertTrue(ArrayUtils.isEquals(midSubarray, ArrayUtils.subarray(array, 1, 5)), "mid start, mid end");
        assertTrue(ArrayUtils.isEquals(rightSubarray, ArrayUtils.subarray(array, 2, array.length)),
                "mid start, length end");

        assertNull(ArrayUtils.subarray(nullArray, 0, 3), "null input");
        assertEquals(ArrayUtils.EMPTY_FLOAT_ARRAY, ArrayUtils.subarray(ArrayUtils.EMPTY_FLOAT_ARRAY, 1, 2),
                "empty array");
        assertEquals(ArrayUtils.EMPTY_FLOAT_ARRAY, ArrayUtils.subarray(array, 4, 2), "start > end");
        assertEquals(ArrayUtils.EMPTY_FLOAT_ARRAY, ArrayUtils.subarray(array, 3, 3), "start == end");
        assertTrue(ArrayUtils.isEquals(leftSubarray, ArrayUtils.subarray(array, -2, 4)),
                "start undershoot, normal end");
        assertEquals(ArrayUtils.EMPTY_FLOAT_ARRAY, ArrayUtils.subarray(array, 33, 4), "start overshoot, any end");
        assertTrue(ArrayUtils.isEquals(rightSubarray, ArrayUtils.subarray(array, 2, 33)),
                "normal start, end overshoot");
        assertTrue(ArrayUtils.isEquals(array, ArrayUtils.subarray(array, -2, 12)), "start undershoot, end overshoot");

        // empty-return tests

        assertSame(ArrayUtils.EMPTY_FLOAT_ARRAY, ArrayUtils.subarray(ArrayUtils.EMPTY_FLOAT_ARRAY, 1, 2),
                "empty array, object test");
        assertSame(ArrayUtils.EMPTY_FLOAT_ARRAY, ArrayUtils.subarray(array, 4, 1), "start > end, object test");
        assertSame(ArrayUtils.EMPTY_FLOAT_ARRAY, ArrayUtils.subarray(array, 3, 3), "start == end, object test");
        assertSame(ArrayUtils.EMPTY_FLOAT_ARRAY, ArrayUtils.subarray(array, 8733, 4),
                "start overshoot, any end, object test");

        // array type tests

        assertSame(float.class, ArrayUtils.subarray(array, 2, 4).getClass().getComponentType(), "float type");
    }

    @Test
    public void testSubarrayInt() {
        final int[] nullArray = null;
        final int[] array = {10, 11, 12, 13, 14, 15};
        final int[] leftSubarray = {10, 11, 12, 13};
        final int[] midSubarray = {11, 12, 13, 14};
        final int[] rightSubarray = {12, 13, 14, 15};


        assertTrue(ArrayUtils.isEquals(leftSubarray, ArrayUtils.subarray(array, 0, 4)), "0 start, mid end");

        assertTrue(ArrayUtils.isEquals(array, ArrayUtils.subarray(array, 0, array.length)), "0 start, length end");

        assertTrue(ArrayUtils.isEquals(midSubarray, ArrayUtils.subarray(array, 1, 5)), "mid start, mid end");

        assertTrue(ArrayUtils.isEquals(rightSubarray, ArrayUtils.subarray(array, 2, array.length)),
                "mid start, length end");


        assertNull(ArrayUtils.subarray(nullArray, 0, 3), "null input");

        assertEquals(ArrayUtils.EMPTY_INT_ARRAY, ArrayUtils.subarray(ArrayUtils.EMPTY_INT_ARRAY, 1, 2), "empty array");

        assertEquals(ArrayUtils.EMPTY_INT_ARRAY, ArrayUtils.subarray(array, 4, 2), "start > end");

        assertEquals(ArrayUtils.EMPTY_INT_ARRAY, ArrayUtils.subarray(array, 3, 3), "start == end");

        assertTrue(ArrayUtils.isEquals(leftSubarray, ArrayUtils.subarray(array, -2, 4)),
                "start undershoot, normal end");

        assertEquals(ArrayUtils.EMPTY_INT_ARRAY, ArrayUtils.subarray(array, 33, 4), "start overshoot, any end");

        assertTrue(ArrayUtils.isEquals(rightSubarray, ArrayUtils.subarray(array, 2, 33)),
                "normal start, end overshoot");

        assertTrue(ArrayUtils.isEquals(array, ArrayUtils.subarray(array, -2, 12)), "start undershoot, end overshoot");

        // empty-return tests

        assertSame(ArrayUtils.EMPTY_INT_ARRAY, ArrayUtils.subarray(ArrayUtils.EMPTY_INT_ARRAY, 1, 2),
                "empty array, object test");

        assertSame(ArrayUtils.EMPTY_INT_ARRAY, ArrayUtils.subarray(array, 4, 1), "start > end, object test");

        assertSame(ArrayUtils.EMPTY_INT_ARRAY, ArrayUtils.subarray(array, 3, 3), "start == end, object test");

        assertSame(ArrayUtils.EMPTY_INT_ARRAY, ArrayUtils.subarray(array, 8733, 4),
                "start overshoot, any end, object test");

        // array type tests

        assertSame(int.class, ArrayUtils.subarray(array, 2, 4).getClass().getComponentType(), "int type");
    }

    @Test
    public void testSubarrayLong() {
        final long[] nullArray = null;
        final long[] array = {999910, 999911, 999912, 999913, 999914, 999915};
        final long[] leftSubarray = {999910, 999911, 999912, 999913};
        final long[] midSubarray = {999911, 999912, 999913, 999914};
        final long[] rightSubarray = {999912, 999913, 999914, 999915};

        assertTrue(ArrayUtils.isEquals(leftSubarray, ArrayUtils.subarray(array, 0, 4)), "0 start, mid end");

        assertTrue(ArrayUtils.isEquals(array, ArrayUtils.subarray(array, 0, array.length)), "0 start, length end");

        assertTrue(ArrayUtils.isEquals(midSubarray, ArrayUtils.subarray(array, 1, 5)), "mid start, mid end");

        assertTrue(ArrayUtils.isEquals(rightSubarray, ArrayUtils.subarray(array, 2, array.length)),
                "mid start, length end");

        assertNull(ArrayUtils.subarray(nullArray, 0, 3), "null input");

        assertEquals(ArrayUtils.EMPTY_LONG_ARRAY, ArrayUtils.subarray(ArrayUtils.EMPTY_LONG_ARRAY, 1, 2),
                "empty array");

        assertEquals(ArrayUtils.EMPTY_LONG_ARRAY, ArrayUtils.subarray(array, 4, 2), "start > end");

        assertEquals(ArrayUtils.EMPTY_LONG_ARRAY, ArrayUtils.subarray(array, 3, 3), "start == end");

        assertTrue(ArrayUtils.isEquals(leftSubarray, ArrayUtils.subarray(array, -2, 4)),
                "start undershoot, normal end");

        assertEquals(ArrayUtils.EMPTY_LONG_ARRAY, ArrayUtils.subarray(array, 33, 4), "start overshoot, any end");

        assertTrue(ArrayUtils.isEquals(rightSubarray, ArrayUtils.subarray(array, 2, 33)),
                "normal start, end overshoot");

        assertTrue(ArrayUtils.isEquals(array, ArrayUtils.subarray(array, -2, 12)), "start undershoot, end overshoot");

        // empty-return tests

        assertSame(ArrayUtils.EMPTY_LONG_ARRAY, ArrayUtils.subarray(ArrayUtils.EMPTY_LONG_ARRAY, 1, 2),
                "empty array, object test");

        assertSame(ArrayUtils.EMPTY_LONG_ARRAY, ArrayUtils.subarray(array, 4, 1), "start > end, object test");

        assertSame(ArrayUtils.EMPTY_LONG_ARRAY, ArrayUtils.subarray(array, 3, 3), "start == end, object test");

        assertSame(ArrayUtils.EMPTY_LONG_ARRAY, ArrayUtils.subarray(array, 8733, 4),
                "start overshoot, any end, object test");

        // array type tests

        assertSame(long.class, ArrayUtils.subarray(array, 2, 4).getClass().getComponentType(), "long type");

    }

    @Test
    public void testSubarrayObject() {
        final Object[] nullArray = null;
        final Object[] objectArray = {"a", "b", "c", "d", "e", "f"};

        assertEquals("abcd", StringUtils.join(ArrayUtils.subarray(objectArray, 0, 4)), "0 start, mid end");
        assertEquals("abcdef", StringUtils.join(ArrayUtils.subarray(objectArray, 0, objectArray.length)),
                "0 start, length end");
        assertEquals("bcd", StringUtils.join(ArrayUtils.subarray(objectArray, 1, 4)), "mid start, mid end");
        assertEquals("bcdef", StringUtils.join(ArrayUtils.subarray(objectArray, 1, objectArray.length)),
                "mid start, length end");

        assertNull(ArrayUtils.subarray(nullArray, 0, 3), "null input");
        assertEquals("", StringUtils.join(ArrayUtils.subarray(ArrayUtils.EMPTY_OBJECT_ARRAY, 1, 2)), "empty array");
        assertEquals("", StringUtils.join(ArrayUtils.subarray(objectArray, 4, 2)), "start > end");
        assertEquals("", StringUtils.join(ArrayUtils.subarray(objectArray, 3, 3)), "start == end");
        assertEquals("abcd", StringUtils.join(ArrayUtils.subarray(objectArray, -2, 4)), "start undershoot, normal end");
        assertEquals("", StringUtils.join(ArrayUtils.subarray(objectArray, 33, 4)), "start overshoot, any end");
        assertEquals("cdef", StringUtils.join(ArrayUtils.subarray(objectArray, 2, 33)), "normal start, end overshoot");
        assertEquals("abcdef", StringUtils.join(ArrayUtils.subarray(objectArray, -2, 12)),
                "start undershoot, end overshoot");

        // array type tests
        final Date[] dateArray = {new java.sql.Date(new Date().getTime()),
                new Date(), new Date(), new Date(), new Date()};

        assertSame(Object.class, ArrayUtils.subarray(objectArray, 2, 4).getClass().getComponentType(), "Object type");
        assertSame(Date.class, ArrayUtils.subarray(dateArray, 1, 4).getClass().getComponentType(),
                "java.util.Date type");
        assertNotSame(java.sql.Date.class, ArrayUtils.subarray(dateArray, 1, 4).getClass().getComponentType(),
                "java.sql.Date type");
        assertThrows(ClassCastException.class,
                () -> java.sql.Date[].class.cast(ArrayUtils.subarray(dateArray, 1, 3)),
                "Invalid downcast");
    }

    @Test
    public void testSubarrayShort() {
        final short[] nullArray = null;
        final short[] array = {10, 11, 12, 13, 14, 15};
        final short[] leftSubarray = {10, 11, 12, 13};
        final short[] midSubarray = {11, 12, 13, 14};
        final short[] rightSubarray = {12, 13, 14, 15};

        assertTrue(ArrayUtils.isEquals(leftSubarray, ArrayUtils.subarray(array, 0, 4)), "0 start, mid end");
        assertTrue(ArrayUtils.isEquals(array, ArrayUtils.subarray(array, 0, array.length)), "0 start, length end");
        assertTrue(ArrayUtils.isEquals(midSubarray, ArrayUtils.subarray(array, 1, 5)), "mid start, mid end");
        assertTrue(ArrayUtils.isEquals(rightSubarray, ArrayUtils.subarray(array, 2, array.length)),
                "mid start, length end");

        assertNull(ArrayUtils.subarray(nullArray, 0, 3), "null input");
        assertEquals(ArrayUtils.EMPTY_SHORT_ARRAY, ArrayUtils.subarray(ArrayUtils.EMPTY_SHORT_ARRAY, 1, 2),
                "empty array");
        assertEquals(ArrayUtils.EMPTY_SHORT_ARRAY, ArrayUtils.subarray(array, 4, 2), "start > end");
        assertEquals(ArrayUtils.EMPTY_SHORT_ARRAY, ArrayUtils.subarray(array, 3, 3), "start == end");
        assertTrue(ArrayUtils.isEquals(leftSubarray, ArrayUtils.subarray(array, -2, 4)),
                "start undershoot, normal end");
        assertEquals(ArrayUtils.EMPTY_SHORT_ARRAY, ArrayUtils.subarray(array, 33, 4), "start overshoot, any end");
        assertTrue(ArrayUtils.isEquals(rightSubarray, ArrayUtils.subarray(array, 2, 33)),
                "normal start, end overshoot");
        assertTrue(ArrayUtils.isEquals(array, ArrayUtils.subarray(array, -2, 12)), "start undershoot, end overshoot");

        // empty-return tests

        assertSame(ArrayUtils.EMPTY_SHORT_ARRAY, ArrayUtils.subarray(ArrayUtils.EMPTY_SHORT_ARRAY, 1, 2),
                "empty array, object test");
        assertSame(ArrayUtils.EMPTY_SHORT_ARRAY, ArrayUtils.subarray(array, 4, 1), "start > end, object test");
        assertSame(ArrayUtils.EMPTY_SHORT_ARRAY, ArrayUtils.subarray(array, 3, 3), "start == end, object test");
        assertSame(ArrayUtils.EMPTY_SHORT_ARRAY, ArrayUtils.subarray(array, 8733, 4),
                "start overshoot, any end, object test");

        // array type tests

        assertSame(short.class, ArrayUtils.subarray(array, 2, 4).getClass().getComponentType(), "short type");
    }

    @Test
    public void testSubarrChar() {
        final char[] nullArray = null;
        final char[] array = {'a', 'b', 'c', 'd', 'e', 'f'};
        final char[] leftSubarray = {'a', 'b', 'c', 'd'};
        final char[] midSubarray = {'b', 'c', 'd', 'e'};
        final char[] rightSubarray = {'c', 'd', 'e', 'f'};

        assertTrue(ArrayUtils.isEquals(leftSubarray, ArrayUtils.subarray(array, 0, 4)), "0 start, mid end");
        assertTrue(ArrayUtils.isEquals(array, ArrayUtils.subarray(array, 0, array.length)), "0 start, length end");
        assertTrue(ArrayUtils.isEquals(midSubarray, ArrayUtils.subarray(array, 1, 5)), "mid start, mid end");
        assertTrue(ArrayUtils.isEquals(rightSubarray, ArrayUtils.subarray(array, 2, array.length)),
                "mid start, length end");

        assertNull(ArrayUtils.subarray(nullArray, 0, 3), "null input");
        assertEquals(ArrayUtils.EMPTY_CHAR_ARRAY, ArrayUtils.subarray(ArrayUtils.EMPTY_CHAR_ARRAY, 1, 2),
                "empty array");
        assertEquals(ArrayUtils.EMPTY_CHAR_ARRAY, ArrayUtils.subarray(array, 4, 2), "start > end");
        assertEquals(ArrayUtils.EMPTY_CHAR_ARRAY, ArrayUtils.subarray(array, 3, 3), "start == end");
        assertTrue(ArrayUtils.isEquals(leftSubarray, ArrayUtils.subarray(array, -2, 4)),
                "start undershoot, normal end");
        assertEquals(ArrayUtils.EMPTY_CHAR_ARRAY, ArrayUtils.subarray(array, 33, 4), "start overshoot, any end");
        assertTrue(ArrayUtils.isEquals(rightSubarray, ArrayUtils.subarray(array, 2, 33)),
                "normal start, end overshoot");
        assertTrue(ArrayUtils.isEquals(array, ArrayUtils.subarray(array, -2, 12)), "start undershoot, end overshoot");

        // empty-return tests

        assertSame(ArrayUtils.EMPTY_CHAR_ARRAY, ArrayUtils.subarray(ArrayUtils.EMPTY_CHAR_ARRAY, 1, 2),
                "empty array, object test");
        assertSame(ArrayUtils.EMPTY_CHAR_ARRAY, ArrayUtils.subarray(array, 4, 1), "start > end, object test");
        assertSame(ArrayUtils.EMPTY_CHAR_ARRAY, ArrayUtils.subarray(array, 3, 3), "start == end, object test");
        assertSame(ArrayUtils.EMPTY_CHAR_ARRAY, ArrayUtils.subarray(array, 8733, 4),
                "start overshoot, any end, object test");

        // array type tests

        assertSame(char.class, ArrayUtils.subarray(array, 2, 4).getClass().getComponentType(), "char type");
    }

    @Test
    public void testSwapBoolean() {
        final boolean[] array = {true, false, false};
        ArrayUtils.swap(array, 0, 2);
        assertFalse(array[0]);
        assertFalse(array[1]);
        assertTrue(array[2]);
    }

    @Test
    public void testSwapBooleanRange() {
        boolean[] array = {false, false, true, true};
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
    public void testSwapByte() {
        final byte[] array = {1, 2, 3};
        ArrayUtils.swap(array, 0, 2);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);
    }

    @Test
    public void testSwapByteRange() {
        byte[] array = {1, 2, 3, 4};
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
    public void testSwapChar() {
        char[] array = {1, 2, 3};
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
        char[] array = {1, 2, 3, 4};
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
    public void testSwapDouble() {
        final double[] array = {1, 2, 3};
        ArrayUtils.swap(array, 0, 2);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);
    }

    @Test
    public void testSwapDoubleRange() {
        double[] array = {1, 2, 3, 4};
        ArrayUtils.swap(array, 0, 2, 2);
        assertEquals(3, array[0]);
        assertEquals(4, array[1]);
        assertEquals(1, array[2]);
        assertEquals(2, array[3]);

        array = new double[]{1, 2, 3};
        ArrayUtils.swap(array, 0, 3);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);

        array = new double[]{1, 2, 3};
        ArrayUtils.swap(array, 0, 2, 2);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);

        array = new double[]{1, 2, 3};
        ArrayUtils.swap(array, -1, 2, 2);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);

        array = new double[]{1, 2, 3};
        ArrayUtils.swap(array, 0, -1, 2);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);

        array = new double[]{1, 2, 3};
        ArrayUtils.swap(array, -1, -1, 2);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);
    }

    @Test
    public void testSwapEmptyBooleanArray() {
        final boolean[] array = {};
        ArrayUtils.swap(array, 0, 2);
        assertEquals(0, array.length);
    }

    @Test
    public void testSwapEmptyByteArray() {
        final byte[] array = {};
        ArrayUtils.swap(array, 0, 2);
        assertEquals(0, array.length);
    }

    @Test
    public void testSwapEmptyCharArray() {
        final char[] array = {};
        ArrayUtils.swap(array, 0, 2);
        assertEquals(0, array.length);
    }

    @Test
    public void testSwapEmptyDoubleArray() {
        final double[] array = {};
        ArrayUtils.swap(array, 0, 2);
        assertEquals(0, array.length);
    }

    @Test
    public void testSwapEmptyFloatArray() {
        final float[] array = {};
        ArrayUtils.swap(array, 0, 2);
        assertEquals(0, array.length);
    }

    @Test
    public void testSwapEmptyIntArray() {
        final int[] array = {};
        ArrayUtils.swap(array, 0, 2);
        assertEquals(0, array.length);
    }

    @Test
    public void testSwapEmptyLongArray() {
        final long[] array = {};
        ArrayUtils.swap(array, 0, 2);
        assertEquals(0, array.length);
    }

    @Test
    public void testSwapEmptyObjectArray() {
        final String[] array = {};
        ArrayUtils.swap(array, 0, 2);
        assertEquals(0, array.length);
    }

    @Test
    public void testSwapEmptyShortArray() {
        final short[] array = {};
        ArrayUtils.swap(array, 0, 2);
        assertEquals(0, array.length);
    }

    @Test
    public void testSwapFloat() {
        final float[] array = {1, 2, 3};
        ArrayUtils.swap(array, 0, 2);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);
    }

    @Test
    public void testSwapFloatRange() {
        float[] array = {1, 2, 3, 4};
        ArrayUtils.swap(array, 0, 2, 2);
        assertEquals(3, array[0]);
        assertEquals(4, array[1]);
        assertEquals(1, array[2]);
        assertEquals(2, array[3]);

        array = new float[]{1, 2, 3};
        ArrayUtils.swap(array, 0, 3);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);

        array = new float[]{1, 2, 3};
        ArrayUtils.swap(array, 0, 2, 2);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);

        array = new float[]{1, 2, 3};
        ArrayUtils.swap(array, -1, 2, 2);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);

        array = new float[]{1, 2, 3};
        ArrayUtils.swap(array, 0, -1, 2);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);

        array = new float[]{1, 2, 3};
        ArrayUtils.swap(array, -1, -1, 2);
        assertEquals(1, array[0]);
        assertEquals(2, array[1]);
        assertEquals(3, array[2]);
    }

    @Test
    public void testSwapInt() {
        final int[] array = {1, 2, 3};
        ArrayUtils.swap(array, 0, 2);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);
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
    public void testSwapIntRange() {
        int[] array = {1, 2, 3, 4};
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
    public void testSwapLong() {
        final long[] array = {1, 2, 3};
        ArrayUtils.swap(array, 0, 2);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);
    }

    @Test
    public void testSwapLongRange() {
        long[] array = {1, 2, 3, 4};
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
    public void testSwapNullBooleanArray() {
        final boolean[] array = null;
        ArrayUtils.swap(array, 0, 2);
        assertNull(array);
    }

    @Test
    public void testSwapNullByteArray() {
        final byte[] array = null;
        ArrayUtils.swap(array, 0, 2);
        assertNull(array);
    }

    @Test
    public void testSwapNullCharArray() {
        final char[] array = null;
        ArrayUtils.swap(array, 0, 2);
        assertNull(array);
    }

    @Test
    public void testSwapNullDoubleArray() {
        final double[] array = null;
        ArrayUtils.swap(array, 0, 2);
        assertNull(array);
    }

    @Test
    public void testSwapNullFloatArray() {
        final float[] array = null;
        ArrayUtils.swap(array, 0, 2);
        assertNull(array);
    }

    @Test
    public void testSwapNullIntArray() {
        final int[] array = null;
        ArrayUtils.swap(array, 0, 2);
        assertNull(array);
    }

    @Test
    public void testSwapNullLongArray() {
        final long[] array = null;
        ArrayUtils.swap(array, 0, 2);
        assertNull(array);
    }

    @Test
    public void testSwapNullObjectArray() {
        final String[] array = null;
        ArrayUtils.swap(array, 0, 2);
        assertNull(array);
    }

    @Test
    public void testSwapNullShortArray() {
        final short[] array = null;
        ArrayUtils.swap(array, 0, 2);
        assertNull(array);
    }

    @Test
    public void testSwapObject() {
        final String[] array = {"1", "2", "3"};
        ArrayUtils.swap(array, 0, 2);
        assertEquals("3", array[0]);
        assertEquals("2", array[1]);
        assertEquals("1", array[2]);
    }

    @Test
    public void testSwapObjectRange() {
        String[] array = {"1", "2", "3", "4"};
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

    @Test
    public void testSwapShort() {
        final short[] array = {1, 2, 3};
        ArrayUtils.swap(array, 0, 2);
        assertEquals(3, array[0]);
        assertEquals(2, array[1]);
        assertEquals(1, array[2]);
    }

    @Test
    public void testSwapShortRange() {
        short[] array = {1, 2, 3, 4};
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
    public void testToMap() {
        Map<?, ?> map = ArrayUtils.toMap(new String[][]{{"foo", "bar"}, {"hello", "world"}});

        assertEquals("bar", map.get("foo"));
        assertEquals("world", map.get("hello"));

        assertNull(ArrayUtils.toMap(null));
        assertThrows(IllegalArgumentException.class, () ->
                ArrayUtils.toMap(new String[][]{{"foo", "bar"}, {"short"}}));
        assertThrows(IllegalArgumentException.class, () ->
                ArrayUtils.toMap(new Object[]{new Object[]{"foo", "bar"}, "illegal type"}));
        assertThrows(IllegalArgumentException.class, () ->
                ArrayUtils.toMap(new Object[]{new Object[]{"foo", "bar"}, null}));

        map = ArrayUtils.toMap(new Object[]{new Map.Entry<Object, Object>() {
            @Override
            public boolean equals(final Object o) {
                throw new UnsupportedOperationException();
            }

            @Override
            public Object getKey() {
                return "foo";
            }

            @Override
            public Object getValue() {
                return "bar";
            }

            @Override
            public int hashCode() {
                throw new UnsupportedOperationException();
            }

            @Override
            public Object setValue(final Object value) {
                throw new UnsupportedOperationException();
            }
        }});
        assertEquals("bar", map.get("foo"));

        // Return empty map when got input array with length = 0
        assertEquals(Collections.emptyMap(), ArrayUtils.toMap(new Object[0]));

        // Test all null values
        map = ArrayUtils.toMap(new Object[][] { {null, null}, {null, null} });
        assertEquals(Collections.singletonMap(null, null), map);

        // Test duplicate keys
        map = ArrayUtils.toMap(new Object[][] { {"key", "value2"}, {"key", "value1"} });
        assertEquals(Collections.singletonMap("key", "value1"), map);
    }

    @Test
    public void testToObject_boolean() {
        final boolean[] b = null;
        assertArrayEquals(null, ArrayUtils.toObject(b));
        assertSame(ArrayUtils.EMPTY_BOOLEAN_OBJECT_ARRAY, ArrayUtils.toObject(new boolean[0]));
        assertArrayEquals(new Boolean[]{Boolean.TRUE, Boolean.FALSE, Boolean.TRUE}, ArrayUtils.toObject(new boolean[]{true, false, true}));
    }

    @Test
    public void testToObject_byte() {
        final byte[] b = null;
        assertArrayEquals(null, ArrayUtils.toObject(b));

        assertSame(ArrayUtils.EMPTY_BYTE_OBJECT_ARRAY,
                ArrayUtils.toObject(new byte[0]));

        assertArrayEquals(new Byte[]{Byte.valueOf(Byte.MIN_VALUE),
                Byte.valueOf(Byte.MAX_VALUE), Byte.valueOf((byte) 9999999)}, ArrayUtils.toObject(new byte[]{Byte.MIN_VALUE, Byte.MAX_VALUE,
                (byte) 9999999}));
    }

    @Test
    public void testToObject_char() {
        final char[] b = null;
        assertArrayEquals(null, ArrayUtils.toObject(b));

        assertSame(ArrayUtils.EMPTY_CHARACTER_OBJECT_ARRAY,
                ArrayUtils.toObject(new char[0]));

        assertArrayEquals(new Character[]{Character.valueOf(Character.MIN_VALUE),
                Character.valueOf(Character.MAX_VALUE), Character.valueOf('0')}, ArrayUtils.toObject(new char[]{Character.MIN_VALUE, Character.MAX_VALUE,
                '0'}));
    }

    @Test
    public void testToObject_double() {
        final double[] b = null;
        assertArrayEquals(null, ArrayUtils.toObject(b));

        assertSame(
                ArrayUtils.EMPTY_DOUBLE_OBJECT_ARRAY,
                ArrayUtils.toObject(new double[0]));

        assertArrayEquals(new Double[]{
                Double.valueOf(Double.MIN_VALUE),
                Double.valueOf(Double.MAX_VALUE),
                Double.valueOf(9999999)}, ArrayUtils.toObject(
                new double[]{Double.MIN_VALUE, Double.MAX_VALUE, 9999999}));
    }

    @Test
    public void testToObject_float() {
        final float[] b = null;
        assertArrayEquals(null, ArrayUtils.toObject(b));

        assertSame(
                ArrayUtils.EMPTY_FLOAT_OBJECT_ARRAY,
                ArrayUtils.toObject(new float[0]));

        assertArrayEquals(new Float[]{
                Float.valueOf(Float.MIN_VALUE),
                Float.valueOf(Float.MAX_VALUE),
                Float.valueOf(9999999)}, ArrayUtils.toObject(
                new float[]{Float.MIN_VALUE, Float.MAX_VALUE, 9999999}));
    }

    @Test
    public void testToObject_int() {
        final int[] b = null;
        assertArrayEquals(null, ArrayUtils.toObject(b));

        assertSame(
                ArrayUtils.EMPTY_INTEGER_OBJECT_ARRAY,
                ArrayUtils.toObject(new int[0]));

        assertArrayEquals(new Integer[]{
                Integer.valueOf(Integer.MIN_VALUE),
                Integer.valueOf(Integer.MAX_VALUE),
                Integer.valueOf(9999999)}, ArrayUtils.toObject(
                new int[]{Integer.MIN_VALUE, Integer.MAX_VALUE, 9999999}));
    }

    @Test
    public void testToObject_long() {
        final long[] b = null;
        assertArrayEquals(null, ArrayUtils.toObject(b));

        assertSame(
                ArrayUtils.EMPTY_LONG_OBJECT_ARRAY,
                ArrayUtils.toObject(new long[0]));

        assertArrayEquals(new Long[]{
                Long.valueOf(Long.MIN_VALUE),
                Long.valueOf(Long.MAX_VALUE),
                Long.valueOf(9999999)}, ArrayUtils.toObject(
                new long[]{Long.MIN_VALUE, Long.MAX_VALUE, 9999999}));
    }

    @Test
    public void testToObject_short() {
        final short[] b = null;
        assertArrayEquals(null, ArrayUtils.toObject(b));

        assertSame(ArrayUtils.EMPTY_SHORT_OBJECT_ARRAY,
                ArrayUtils.toObject(new short[0]));

        assertArrayEquals(new Short[]{Short.valueOf(Short.MIN_VALUE), Short.valueOf(Short.MAX_VALUE),
                Short.valueOf((short) 9999999)}, ArrayUtils.toObject(new short[]{Short.MIN_VALUE, Short.MAX_VALUE,
                (short) 9999999}));
    }

    /** testToPrimitive/Object for boolean */
    @Test
    public void testToPrimitive_boolean() {
        final Boolean[] b = null;
        assertNull(ArrayUtils.toPrimitive(b));
        assertSame(ArrayUtils.EMPTY_BOOLEAN_ARRAY, ArrayUtils.toPrimitive(new Boolean[0]));
        assertArrayEquals(new boolean[]{true, false, true}, ArrayUtils.toPrimitive(new Boolean[]{Boolean.TRUE, Boolean.FALSE, Boolean.TRUE}));
        assertArrayEquals(new boolean[]{true, false}, ArrayUtils.toPrimitive(new Boolean[]{Boolean.TRUE, null}));
    }

    @Test
    public void testToPrimitive_boolean_boolean() {
        assertNull(ArrayUtils.toPrimitive(null, false));
        assertSame(ArrayUtils.EMPTY_BOOLEAN_ARRAY, ArrayUtils.toPrimitive(new Boolean[0], false));
        assertArrayEquals(new boolean[]{true, false, true}, ArrayUtils.toPrimitive(new Boolean[]{Boolean.TRUE, Boolean.FALSE, Boolean.TRUE}, false));
        assertArrayEquals(new boolean[]{true, false, false}, ArrayUtils.toPrimitive(new Boolean[]{Boolean.TRUE, null, Boolean.FALSE}, false));
        assertArrayEquals(new boolean[]{true, true, false}, ArrayUtils.toPrimitive(new Boolean[]{Boolean.TRUE, null, Boolean.FALSE}, true));
    }

    /** testToPrimitive/Object for byte */
    @Test
    public void testToPrimitive_byte() {
        final Byte[] b = null;
        assertNull(ArrayUtils.toPrimitive(b));

        assertSame(ArrayUtils.EMPTY_BYTE_ARRAY, ArrayUtils.toPrimitive(new Byte[0]));

        assertArrayEquals(new byte[]{Byte.MIN_VALUE, Byte.MAX_VALUE, (byte) 9999999}, ArrayUtils.toPrimitive(new Byte[]{Byte.valueOf(Byte.MIN_VALUE),
                Byte.valueOf(Byte.MAX_VALUE), Byte.valueOf((byte) 9999999)}));

        assertThrows(NullPointerException.class,
                () -> ArrayUtils.toPrimitive(new Byte[]{Byte.valueOf(Byte.MIN_VALUE), null}));
    }

    @Test
    public void testToPrimitive_byte_byte() {
        final Byte[] b = null;
        assertNull(ArrayUtils.toPrimitive(b, Byte.MIN_VALUE));

        assertSame(ArrayUtils.EMPTY_BYTE_ARRAY,
                ArrayUtils.toPrimitive(new Byte[0], (byte) 1));

        assertArrayEquals(new byte[]{Byte.MIN_VALUE, Byte.MAX_VALUE, (byte) 9999999}, ArrayUtils.toPrimitive(new Byte[]{Byte.valueOf(Byte.MIN_VALUE),
                        Byte.valueOf(Byte.MAX_VALUE), Byte.valueOf((byte) 9999999)},
                Byte.MIN_VALUE));

        assertArrayEquals(new byte[]{Byte.MIN_VALUE, Byte.MAX_VALUE, (byte) 9999999}, ArrayUtils.toPrimitive(new Byte[]{Byte.valueOf(Byte.MIN_VALUE), null,
                Byte.valueOf((byte) 9999999)}, Byte.MAX_VALUE));
    }

    /** testToPrimitive/Object for byte */
    @Test
    public void testToPrimitive_char() {
        final Character[] b = null;
        assertNull(ArrayUtils.toPrimitive(b));

        assertSame(ArrayUtils.EMPTY_CHAR_ARRAY, ArrayUtils.toPrimitive(new Character[0]));

        assertArrayEquals(new char[]{Character.MIN_VALUE, Character.MAX_VALUE, '0'}, ArrayUtils.toPrimitive(new Character[]{Character.valueOf(Character.MIN_VALUE),
                Character.valueOf(Character.MAX_VALUE), Character.valueOf('0')}));

        assertThrows(NullPointerException.class,
                () -> ArrayUtils.toPrimitive(new Character[]{Character.valueOf(Character.MIN_VALUE), null}));
    }

    @Test
    public void testToPrimitive_char_char() {
        final Character[] b = null;
        assertNull(ArrayUtils.toPrimitive(b, Character.MIN_VALUE));

        assertSame(ArrayUtils.EMPTY_CHAR_ARRAY,
                ArrayUtils.toPrimitive(new Character[0], (char) 0));

        assertArrayEquals(new char[]{Character.MIN_VALUE, Character.MAX_VALUE, '0'}, ArrayUtils.toPrimitive(new Character[]{Character.valueOf(Character.MIN_VALUE),
                        Character.valueOf(Character.MAX_VALUE), Character.valueOf('0')},
                Character.MIN_VALUE));

        assertArrayEquals(new char[]{Character.MIN_VALUE, Character.MAX_VALUE, '0'}, ArrayUtils.toPrimitive(new Character[]{Character.valueOf(Character.MIN_VALUE), null,
                Character.valueOf('0')}, Character.MAX_VALUE));
    }

    /**  testToPrimitive/Object for double */
    @Test
    public void testToPrimitive_double() {
        final Double[] b = null;
        assertNull(ArrayUtils.toPrimitive(b));

        assertSame(ArrayUtils.EMPTY_DOUBLE_ARRAY,
                ArrayUtils.toPrimitive(new Double[0]));

        assertArrayEquals(new double[]{Double.MIN_VALUE, Double.MAX_VALUE, 9999999}, ArrayUtils.toPrimitive(new Double[]{Double.valueOf(Double.MIN_VALUE),
                Double.valueOf(Double.MAX_VALUE), Double.valueOf(9999999)}));

        assertThrows(NullPointerException.class,
                () -> ArrayUtils.toPrimitive(new Float[]{Float.valueOf(Float.MIN_VALUE), null}));
    }

    @Test
    public void testToPrimitive_double_double() {
        final Double[] l = null;
        assertNull(ArrayUtils.toPrimitive(l, Double.MIN_VALUE));

        assertSame(ArrayUtils.EMPTY_DOUBLE_ARRAY,
                ArrayUtils.toPrimitive(new Double[0], 1));

        assertArrayEquals(new double[]{Double.MIN_VALUE, Double.MAX_VALUE, 9999999}, ArrayUtils.toPrimitive(new Double[]{Double.valueOf(Double.MIN_VALUE),
                Double.valueOf(Double.MAX_VALUE), Double.valueOf(9999999)}, 1));

        assertArrayEquals(new double[]{Double.MIN_VALUE, Double.MAX_VALUE, 9999999}, ArrayUtils.toPrimitive(new Double[]{Double.valueOf(Double.MIN_VALUE),
                null, Double.valueOf(9999999)}, Double.MAX_VALUE));
    }

    /**  testToPrimitive/Object for float */
    @Test
    public void testToPrimitive_float() {
        final Float[] b = null;
        assertNull(ArrayUtils.toPrimitive(b));

        assertSame(ArrayUtils.EMPTY_FLOAT_ARRAY,
                ArrayUtils.toPrimitive(new Float[0]));

        assertArrayEquals(new float[]{Float.MIN_VALUE, Float.MAX_VALUE, 9999999}, ArrayUtils.toPrimitive(new Float[]{Float.valueOf(Float.MIN_VALUE),
                Float.valueOf(Float.MAX_VALUE), Float.valueOf(9999999)}));

        assertThrows(NullPointerException.class,
                () -> ArrayUtils.toPrimitive(new Float[]{Float.valueOf(Float.MIN_VALUE), null}));
    }

    @Test
    public void testToPrimitive_float_float() {
        final Float[] l = null;
        assertNull(ArrayUtils.toPrimitive(l, Float.MIN_VALUE));

        assertSame(ArrayUtils.EMPTY_FLOAT_ARRAY,
                ArrayUtils.toPrimitive(new Float[0], 1));

        assertArrayEquals(new float[]{Float.MIN_VALUE, Float.MAX_VALUE, 9999999}, ArrayUtils.toPrimitive(new Float[]{Float.valueOf(Float.MIN_VALUE),
                Float.valueOf(Float.MAX_VALUE), Float.valueOf(9999999)}, 1));

        assertArrayEquals(new float[]{Float.MIN_VALUE, Float.MAX_VALUE, 9999999}, ArrayUtils.toPrimitive(new Float[]{Float.valueOf(Float.MIN_VALUE),
                null, Float.valueOf(9999999)}, Float.MAX_VALUE));
    }

    /** testToPrimitive/Object for int */
    @Test
    public void testToPrimitive_int() {
        final Integer[] b = null;
        assertNull(ArrayUtils.toPrimitive(b));
        assertSame(ArrayUtils.EMPTY_INT_ARRAY, ArrayUtils.toPrimitive(new Integer[0]));
        assertArrayEquals(new int[]{Integer.MIN_VALUE, Integer.MAX_VALUE, 9999999}, ArrayUtils.toPrimitive(new Integer[]{Integer.valueOf(Integer.MIN_VALUE),
                Integer.valueOf(Integer.MAX_VALUE), Integer.valueOf(9999999)}));

        assertThrows(NullPointerException.class,
                () -> ArrayUtils.toPrimitive(new Integer[]{Integer.valueOf(Integer.MIN_VALUE), null}));
    }

    @Test
    public void testToPrimitive_int_int() {
        final Long[] l = null;
        assertNull(ArrayUtils.toPrimitive(l, Integer.MIN_VALUE));
        assertSame(ArrayUtils.EMPTY_INT_ARRAY,
                ArrayUtils.toPrimitive(new Integer[0], 1));
        assertArrayEquals(new int[]{Integer.MIN_VALUE, Integer.MAX_VALUE, 9999999}, ArrayUtils.toPrimitive(new Integer[]{Integer.valueOf(Integer.MIN_VALUE),
                Integer.valueOf(Integer.MAX_VALUE), Integer.valueOf(9999999)}, 1));
        assertArrayEquals(new int[]{Integer.MIN_VALUE, Integer.MAX_VALUE, 9999999}, ArrayUtils.toPrimitive(new Integer[]{Integer.valueOf(Integer.MIN_VALUE),
                null, Integer.valueOf(9999999)}, Integer.MAX_VALUE));
    }

    @Test
    public void testToPrimitive_intNull() {
        final Integer[] iArray = null;
        assertNull(ArrayUtils.toPrimitive(iArray, Integer.MIN_VALUE));
    }

    /** testToPrimitive/Object for long */
    @Test
    public void testToPrimitive_long() {
        final Long[] b = null;
        assertNull(ArrayUtils.toPrimitive(b));

        assertSame(ArrayUtils.EMPTY_LONG_ARRAY,
                ArrayUtils.toPrimitive(new Long[0]));

        assertArrayEquals(new long[]{Long.MIN_VALUE, Long.MAX_VALUE, 9999999}, ArrayUtils.toPrimitive(new Long[]{Long.valueOf(Long.MIN_VALUE),
                Long.valueOf(Long.MAX_VALUE), Long.valueOf(9999999)}));

        assertThrows(NullPointerException.class,
                () -> ArrayUtils.toPrimitive(new Long[]{Long.valueOf(Long.MIN_VALUE), null}));
    }

    @Test
    public void testToPrimitive_long_long() {
        final Long[] l = null;
        assertNull(ArrayUtils.toPrimitive(l, Long.MIN_VALUE));

        assertSame(ArrayUtils.EMPTY_LONG_ARRAY,
                ArrayUtils.toPrimitive(new Long[0], 1));

        assertArrayEquals(new long[]{Long.MIN_VALUE, Long.MAX_VALUE, 9999999}, ArrayUtils.toPrimitive(new Long[]{Long.valueOf(Long.MIN_VALUE),
                Long.valueOf(Long.MAX_VALUE), Long.valueOf(9999999)}, 1));

        assertArrayEquals(new long[]{Long.MIN_VALUE, Long.MAX_VALUE, 9999999}, ArrayUtils.toPrimitive(new Long[]{Long.valueOf(Long.MIN_VALUE),
                null, Long.valueOf(9999999)}, Long.MAX_VALUE));
    }

    /** testToPrimitive/Object for short */
    @Test
    public void testToPrimitive_short() {
        final Short[] b = null;
        assertNull(ArrayUtils.toPrimitive(b));

        assertSame(ArrayUtils.EMPTY_SHORT_ARRAY, ArrayUtils.toPrimitive(new Short[0]));

        assertArrayEquals(new short[]{Short.MIN_VALUE, Short.MAX_VALUE, (short) 9999999}, ArrayUtils.toPrimitive(new Short[]{Short.valueOf(Short.MIN_VALUE),
                Short.valueOf(Short.MAX_VALUE), Short.valueOf((short) 9999999)}));

        assertThrows(NullPointerException.class,
                () -> ArrayUtils.toPrimitive(new Short[]{Short.valueOf(Short.MIN_VALUE), null}));
    }

    @Test
    public void testToPrimitive_short_short() {
        final Short[] s = null;
        assertNull(ArrayUtils.toPrimitive(s, Short.MIN_VALUE));

        assertSame(ArrayUtils.EMPTY_SHORT_ARRAY, ArrayUtils.toPrimitive(new Short[0],
                Short.MIN_VALUE));

        assertArrayEquals(new short[]{Short.MIN_VALUE, Short.MAX_VALUE, (short) 9999999}, ArrayUtils.toPrimitive(new Short[]{Short.valueOf(Short.MIN_VALUE),
                Short.valueOf(Short.MAX_VALUE), Short.valueOf((short) 9999999)}, Short.MIN_VALUE));

        assertArrayEquals(new short[]{Short.MIN_VALUE, Short.MAX_VALUE, (short) 9999999}, ArrayUtils.toPrimitive(new Short[]{Short.valueOf(Short.MIN_VALUE), null,
                Short.valueOf((short) 9999999)}, Short.MAX_VALUE));
    }

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

    @Test
    public void testToStringArray_array() {
        assertNull(ArrayUtils.toStringArray(null));

        assertArrayEquals(new String[0], ArrayUtils.toStringArray(new Object[0]));

        final Object[] array = {1, 2, 3, "array", "test"};
        assertArrayEquals(new String[]{"1", "2", "3", "array", "test"}, ArrayUtils.toStringArray(array));

        assertThrows(NullPointerException.class, () -> ArrayUtils.toStringArray(new Object[]{null}));
    }

    @Test
    public void testToStringArray_array_string() {
        assertNull(ArrayUtils.toStringArray(null, ""));

        assertArrayEquals(new String[0], ArrayUtils.toStringArray(new Object[0], ""));

        final Object[] array = {1, null, "test"};
        assertArrayEquals(new String[]{"1", "valueForNullElements", "test"},
                ArrayUtils.toStringArray(array, "valueForNullElements"));
    }

    @Test
    public void textIndexesOfInt() {
        int[] array = null;
        final BitSet emptySet = new BitSet();
        final BitSet testSet = new BitSet();
        assertEquals(emptySet, ArrayUtils.indexesOf(array, 0));
        array = new int[]{0, 1, 2, 3, 0};
        testSet.set(0);
        testSet.set(4);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 0));
        testSet.clear();
        testSet.set(1);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 1));
        testSet.clear();
        testSet.set(2);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 2));
        testSet.clear();
        testSet.set(3);
        assertEquals(testSet, ArrayUtils.indexesOf(array, 3));
        assertEquals(emptySet, ArrayUtils.indexesOf(array, 99));
    }
}
