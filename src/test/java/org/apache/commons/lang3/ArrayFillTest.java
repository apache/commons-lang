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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

/**
 * Tests {@link ArrayFill}.
 */
public class ArrayFillTest extends AbstractLangTest {

    // Parameterized test to reduce redundancy for filling arrays of different types
    @ParameterizedTest
    @MethodSource("arrayAndValueProvider")
    public <T> void testFillArray(T[] array, T val) {
        final T[] actual = ArrayFill.fill(array, val);
        assertSame(array, actual);
        for (T v : actual) {
            assertEquals(val, v);
        }
    }

    private static Stream<Arguments> arrayAndValueProvider() {
        return Stream.of(
            Arguments.of(new Byte[3], (byte) 1),
            Arguments.of(new Character[3], 'A'),
            Arguments.of(new Double[3], 1.0),
            Arguments.of(new String[3], "Test")
        );
    }

    @Test
    public void testFillByteArray() {
        final byte[] array = new byte[3];
        final byte val = (byte) 1;
        final byte[] actual = ArrayFill.fill(array, val);
        assertSame(array, actual);
        for (final byte v : actual) {
            assertEquals(val, v);
        }
    }

    @Test
    public void testFillByteArrayNull() {
        final byte[] array = null;
        final byte val = (byte) 1;
        final byte[] actual = ArrayFill.fill(array, val);
        assertEquals(null, actual, "ArrayFill.fill should return null when input array is null.");
    }

    @Test
    public void testFillCharArray() {
        final char[] array = new char[3];
        final char val = 'A';
        final char[] actual = ArrayFill.fill(array, val);
        assertSame(array, actual);
        for (final char v : actual) {
            assertEquals(val, v);
        }
    }

    @Test
    public void testFillCharArrayNull() {
        final char[] array = null;
        final char val = 'A';
        final char[] actual = ArrayFill.fill(array, val);
        assertEquals(null, actual, "ArrayFill.fill should return null when input array is null.");
    }

    @Test
    public void testFillDoubleArray() {
        final double[] array = new double[3];
        final double val = 1;
        final double[] actual = ArrayFill.fill(array, val);
        assertSame(array, actual);
        for (final double v : actual) {
            assertEquals(val, v);
        }
    }

    @Test
    public void testFillDoubleArrayNull() {
        final double[] array = null;
        final double val = 1;
        final double[] actual = ArrayFill.fill(array, val);
        assertEquals(null, actual, "ArrayFill.fill should return null when input array is null.");
    }

    @Test
    public void testFillFloatArray() {
        final float[] array = new float[3];
        final float val = 1;
        final float[] actual = ArrayFill.fill(array, val);
        assertSame(array, actual);
        for (final float v : actual) {
            assertEquals(val, v);
        }
    }

    @Test
    public void testFillFloatArrayNull() {
        final float[] array = null;
        final float val = 1;
        final float[] actual = ArrayFill.fill(array, val);
        assertEquals(null, actual, "ArrayFill.fill should return null when input array is null.");
    }

    @Test
    public void testFillIntArray() {
        final int[] array = new int[3];
        final int val = 1;
        final int[] actual = ArrayFill.fill(array, val);
        assertSame(array, actual);
        for (final int v : actual) {
            assertEquals(val, v);
        }
    }

    @Test
    public void testFillIntArrayNull() {
        final int[] array = null;
        final int val = 1;
        final int[] actual = ArrayFill.fill(array, val);
        assertEquals(null, actual, "ArrayFill.fill should return null when input array is null.");
    }

    @Test
    public void testFillLongArray() {
        final long[] array = new long[3];
        final long val = 1;
        final long[] actual = ArrayFill.fill(array, val);
        assertSame(array, actual);
        for (final long v : actual) {
            assertEquals(val, v);
        }
    }

    @Test
    public void testFillLongArrayNull() {
        final long[] array = null;
        final long val = 1;
        final long[] actual = ArrayFill.fill(array, val);
        assertEquals(null, actual, "ArrayFill.fill should return null when input array is null.");
    }

    @Test
    public void testFillObjectArray() {
        final String[] array = new String[3];
        final String val = "A";
        final String[] actual = ArrayFill.fill(array, val);
        assertSame(array, actual);
        for (final String v : actual) {
            assertEquals(val, v);
        }
    }

    @Test
    public void testFillObjectArrayNull() {
        final String[] array = null;
        final String val = "A";
        final String[] actual = ArrayFill.fill(array, val);
        assertEquals(null, actual, "ArrayFill.fill should return null when input array is null.");
    }

    // New tests for edge cases
    @Test
    public void testFillEmptyArray() {
        final byte[] array = new byte[0];
        final byte val = (byte) 1;
        final byte[] actual = ArrayFill.fill(array, val);
        assertSame(array, actual);
        assertEquals(0, actual.length);
    }

    @Test
    public void testFillArrayWithDefaultValues() {
        final byte[] array = new byte[] {1, 1, 1};
        final byte val = (byte) 1;
        final byte[] actual = ArrayFill.fill(array, val);
        assertSame(array, actual);
        assertEquals(1, actual[0]);
        assertEquals(1, actual[1]);
        assertEquals(1, actual[2]);
    }

    @Test
    public void testFillObjectArrayWithNull() {
        final String[] array = new String[3];
        final String val = null;
        final String[] actual = ArrayFill.fill(array, val);
        assertSame(array, actual);
        for (final String v : actual) {
            assertEquals(val, v);
        }
    }
}
