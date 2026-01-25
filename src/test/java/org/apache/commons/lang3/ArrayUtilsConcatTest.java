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
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.mockStatic;

import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

/**
 * Tests {@link ArrayUtils} concat methods.
 */
class ArrayUtilsConcatTest extends AbstractLangTest {

    @Test
    void testBooleanArraysConcat() {
        assertThrows(NullPointerException.class, () -> ArrayUtils.concat((boolean[][]) null));
        assertArrayEquals(new boolean[] {}, ArrayUtils.concat((boolean[]) null, null));
        assertArrayEquals(new boolean[] { true, false, true }, ArrayUtils.concat(new boolean[] { true, false, true }, null));
        assertArrayEquals(new boolean[] { false, true, false }, ArrayUtils.concat(null, new boolean[] { false, true, false }));
        assertArrayEquals(new boolean[] { false, true, false, false, true },
                ArrayUtils.concat(new boolean[] { false, true, false }, new boolean[] { false, true }));
    }

    @Test
    void testByteArraysConcat() {
        assertThrows(NullPointerException.class, () -> ArrayUtils.concat((byte[][]) null));
        assertArrayEquals(new byte[] {}, ArrayUtils.concat((byte[]) null, null));
        assertArrayEquals(new byte[] { 1, 2, 3 }, ArrayUtils.concat(new byte[] { 1, 2, 3 }, null));
        assertArrayEquals(new byte[] { -3, 2, 1 }, ArrayUtils.concat(null, new byte[] { -3, 2, 1 }));
        assertArrayEquals(new byte[] { 1, 3, 2, 100, 7 }, ArrayUtils.concat(new byte[] { 1, 3, 2 }, new byte[] { 100, 7 }));
    }

    @Test
    void testCharArraysConcat() {
        assertThrows(NullPointerException.class, () -> ArrayUtils.concat((char[][]) null));
        assertArrayEquals(new char[] {}, ArrayUtils.concat((char[]) null, null));
        assertArrayEquals(new char[] { 'a', 'b', 'c' }, ArrayUtils.concat(new char[] { 'a', 'b', 'c' }, null));
        assertArrayEquals(new char[] { 'b', 'a', 'c' }, ArrayUtils.concat(null, new char[] { 'b', 'a', 'c' }));
        assertArrayEquals(new char[] { 'a', 'b', 'c', 'q', 'w' }, ArrayUtils.concat(new char[] { 'a', 'b', 'c' }, new char[] { 'q', 'w' }));
    }

    @Test
    void testDoubleArraysConcat() {
        assertThrows(NullPointerException.class, () -> ArrayUtils.concat((double[][]) null));
        assertArrayEquals(new double[] {}, ArrayUtils.concat((double[]) null, null));
        assertArrayEquals(new double[] { 1e-300, .2e-300, 3.0e-300 }, ArrayUtils.concat(new double[] { 1e-300, .2e-300, 3.0e-300 }, null));
        assertArrayEquals(new double[] { 3.0e-300, 1e-300, .2e-300 }, ArrayUtils.concat(null, new double[] { 3.0e-300, 1e-300, .2e-300 }));
        assertArrayEquals(new double[] { 1e-300, .2e-300, 3.0e-300, 10.01e-300, 0.001e-300 },
                ArrayUtils.concat(new double[] { 1e-300, .2e-300, 3.0e-300 }, new double[] { 10.01e-300, 0.001e-300 }));
    }

    @Test
    void testExceedSafeMaxArraySize() {
        try (MockedStatic<ArrayUtils.MathBridge> mockedStatic = mockStatic(ArrayUtils.MathBridge.class)) {
            mockedStatic.when(() -> ArrayUtils.MathBridge.addExact(anyInt(), anyInt())).thenThrow(ArithmeticException.class);
            assertThrows(IllegalArgumentException.class, () -> ArrayUtils.concat(new int[] { 0 }, new int[] { 1 }));
        }
    }

    @Test
    void testFloatArraysConcat() {
        assertThrows(NullPointerException.class, () -> ArrayUtils.concat((float[][]) null));
        assertArrayEquals(new float[] {}, ArrayUtils.concat((float[]) null, null));
        assertArrayEquals(new float[] { 1f, .2f, 3.0f }, ArrayUtils.concat(new float[] { 1f, .2f, 3.0f }, null));
        assertArrayEquals(new float[] { 3.0f, 1f, .2f }, ArrayUtils.concat(null, new float[] { 3.0f, 1f, .2f }));
        assertArrayEquals(new float[] { 1f, .2f, 3.0f, 10.01f, 0.001f }, ArrayUtils.concat(new float[] { 1f, .2f, 3.0f }, new float[] { 10.01f, 0.001f }));
    }

    @Test
    void testIntArraysConcat() {
        assertThrows(NullPointerException.class, () -> ArrayUtils.concat((int[][]) null));
        assertArrayEquals(new int[] {}, ArrayUtils.concat((int[]) null, null));
        assertArrayEquals(new int[] { 10000000, 20000000, 30000000 }, ArrayUtils.concat(new int[] { 10000000, 20000000, 30000000 }, null));
        assertArrayEquals(new int[] { -30000000, 20000000, 10000000 }, ArrayUtils.concat(null, new int[] { -30000000, 20000000, 10000000 }));
        assertArrayEquals(new int[] { 10000000, 30000000, 20000000, 100000000, 70000000 },
                ArrayUtils.concat(new int[] { 10000000, 30000000, 20000000 }, new int[] { 100000000, 70000000 }));
    }

    @Test
    void testLongArraysConcat() {
        assertThrows(NullPointerException.class, () -> ArrayUtils.concat((long[][]) null));
        assertArrayEquals(new long[] {}, ArrayUtils.concat((long[]) null, null));
        assertArrayEquals(new long[] { 10000000000L, 20000000000L, 30000000000L },
                ArrayUtils.concat(new long[] { 10000000000L, 20000000000L, 30000000000L }, null));
        assertArrayEquals(new long[] { -30000000000L, 20000000000L, 10000000000L },
                ArrayUtils.concat(null, new long[] { -30000000000L, 20000000000L, 10000000000L }));
        assertArrayEquals(new long[] { 10000000000L, 30000000000L, 20000000000L, 100000000000L, 70000000000L },
                ArrayUtils.concat(new long[] { 10000000000L, 30000000000L, 20000000000L }, new long[] { 100000000000L, 70000000000L }));
    }

    @Test
    void testShortArraysConcat() {
        assertThrows(NullPointerException.class, () -> ArrayUtils.concat((short[][]) null));
        assertArrayEquals(new short[] {}, ArrayUtils.concat((short[]) null, null));
        assertArrayEquals(new short[] { 1000, 2000, 3000 }, ArrayUtils.concat(new short[] { 1000, 2000, 3000 }, null));
        assertArrayEquals(new short[] { -3000, 2000, 1000 }, ArrayUtils.concat(null, new short[] { -3000, 2000, 1000 }));
        assertArrayEquals(new short[] { 1000, 3000, 2000, 10000, 7000 }, ArrayUtils.concat(new short[] { 1000, 3000, 2000 }, new short[] { 10000, 7000 }));
    }
}
