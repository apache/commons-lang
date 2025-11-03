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
import static org.junit.jupiter.api.Assertions.assertSame;

import org.apache.commons.lang3.function.FailableIntFunction;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link ArrayFill}.
 */
class ArrayFillTest extends AbstractLangTest {

    @Test
    void testFillBooleanArray() {
        final boolean[] array = new boolean[3];
        final boolean val = true;
        final boolean[] actual = ArrayFill.fill(array, val);
        assertSame(array, actual);
        for (final boolean v : actual) {
            assertEquals(val, v);
        }
    }

    @Test
    void testFillBooleanArrayNull() {
        final boolean[] array = null;
        final boolean val = true;
        final boolean[] actual = ArrayFill.fill(array, val);
        assertSame(array, actual);
    }

    @Test
    void testFillByteArray() {
        final byte[] array = new byte[3];
        final byte val = (byte) 1;
        final byte[] actual = ArrayFill.fill(array, val);
        assertSame(array, actual);
        for (final byte v : actual) {
            assertEquals(val, v);
        }
    }

    @Test
    void testFillByteArrayNull() {
        final byte[] array = null;
        final byte val = (byte) 1;
        final byte[] actual = ArrayFill.fill(array, val);
        assertSame(array, actual);
    }

    @Test
    void testFillCharArray() {
        final char[] array = new char[3];
        final char val = 1;
        final char[] actual = ArrayFill.fill(array, val);
        assertSame(array, actual);
        for (final char v : actual) {
            assertEquals(val, v);
        }
    }

    @Test
    void testFillCharArrayNull() {
        final char[] array = null;
        final char val = 1;
        final char[] actual = ArrayFill.fill(array, val);
        assertSame(array, actual);
    }

    @Test
    void testFillDoubleArray() {
        final double[] array = new double[3];
        final double val = 1;
        final double[] actual = ArrayFill.fill(array, val);
        assertSame(array, actual);
        for (final double v : actual) {
            assertEquals(val, v);
        }
    }

    @Test
    void testFillDoubleArrayNull() {
        final double[] array = null;
        final double val = 1;
        final double[] actual = ArrayFill.fill(array, val);
        assertSame(array, actual);
    }

    @Test
    void testFillFloatArray() {
        final float[] array = new float[3];
        final float val = 1;
        final float[] actual = ArrayFill.fill(array, val);
        assertSame(array, actual);
        for (final float v : actual) {
            assertEquals(val, v);
        }
    }

    @Test
    void testFillFloatArrayNull() {
        final float[] array = null;
        final float val = 1;
        final float[] actual = ArrayFill.fill(array, val);
        assertSame(array, actual);
    }

    @Test
    void testFillFunction() throws Exception {
        final FailableIntFunction<?, Exception> nullIntFunction = null;
        assertNull(ArrayFill.fill(null, nullIntFunction));
        assertArrayEquals(null, ArrayFill.fill(null, nullIntFunction));
        assertArrayEquals(ArrayUtils.EMPTY_BOOLEAN_OBJECT_ARRAY, ArrayFill.fill(ArrayUtils.EMPTY_BOOLEAN_OBJECT_ARRAY, nullIntFunction));
        assertArrayEquals(ArrayUtils.EMPTY_OBJECT_ARRAY, ArrayFill.fill(ArrayUtils.EMPTY_OBJECT_ARRAY, nullIntFunction));
        final Integer[] array = new Integer[10];
        final Integer[] array2 = ArrayFill.fill(array, Integer::valueOf);
        assertSame(array, array2);
        for (int i = 0; i < array.length; i++) {
            assertEquals(i, array[i].intValue());
        }
    }

    @Test
    void testFillIntArray() {
        final int[] array = new int[3];
        final int val = 1;
        final int[] actual = ArrayFill.fill(array, val);
        assertSame(array, actual);
        for (final int v : actual) {
            assertEquals(val, v);
        }
    }

    @Test
    void testFillIntArrayNull() {
        final int[] array = null;
        final int val = 1;
        final int[] actual = ArrayFill.fill(array, val);
        assertSame(array, actual);
    }

    @Test
    void testFillLongArray() {
        final long[] array = new long[3];
        final long val = 1;
        final long[] actual = ArrayFill.fill(array, val);
        assertSame(array, actual);
        for (final long v : actual) {
            assertEquals(val, v);
        }
    }

    @Test
    void testFillLongArrayNull() {
        final long[] array = null;
        final long val = 1;
        final long[] actual = ArrayFill.fill(array, val);
        assertSame(array, actual);
    }

    @Test
    void testFillObjectArray() {
        final String[] array = new String[3];
        final String val = "A";
        final String[] actual = ArrayFill.fill(array, val);
        assertSame(array, actual);
        for (final String v : actual) {
            assertEquals(val, v);
        }
    }

    @Test
    void testFillObjectArrayNull() {
        final Object[] array = null;
        final Object val = 1;
        final Object[] actual = ArrayFill.fill(array, val);
        assertSame(array, actual);
    }

    @Test
    void testFillShortArray() {
        final short[] array = new short[3];
        final short val = (byte) 1;
        final short[] actual = ArrayFill.fill(array, val);
        assertSame(array, actual);
        for (final short v : actual) {
            assertEquals(val, v);
        }
    }

    @Test
    void testFillShortArrayNull() {
        final short[] array = null;
        final short val = 1;
        final short[] actual = ArrayFill.fill(array, val);
        assertSame(array, actual);
    }
}
