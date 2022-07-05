/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
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
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;

import org.junit.jupiter.api.Test;

/**
 * Tests for {@link RandomUtils}
 */
public class RandomUtilsTest extends AbstractLangTest {

    /**
     * For comparing doubles and floats
     */
    private static final double DELTA = 1e-5;

    @Test
    public void testConstructor() {
        assertNotNull(new RandomUtils());
        final Constructor<?>[] cons = RandomUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertTrue(Modifier.isPublic(cons[0].getModifiers()));
        assertTrue(Modifier.isPublic(RandomUtils.class.getModifiers()));
        assertFalse(Modifier.isFinal(RandomUtils.class.getModifiers()));
    }

    @Test
    public void testNextBytesNegative() {
        assertThrows(IllegalArgumentException.class, () -> RandomUtils.nextBytes(-1));
    }

    @Test
    public void testNextIntNegative() {
        assertThrows(IllegalArgumentException.class, () -> RandomUtils.nextInt(-1, 1));
    }

    @Test
    public void testNextLongNegative() {
        assertThrows(IllegalArgumentException.class, () -> RandomUtils.nextLong(-1, 1));
    }

    @Test
    public void testNextDoubleNegative() {
        assertThrows(IllegalArgumentException.class, () -> RandomUtils.nextDouble(-1, 1));
    }

    @Test
    public void testNextFloatNegative() {
        assertThrows(IllegalArgumentException.class, () -> RandomUtils.nextFloat(-1, 1));
    }

    @Test
    public void testNextIntLowerGreaterUpper() {
        assertThrows(IllegalArgumentException.class, () -> RandomUtils.nextInt(2, 1));
    }

    @Test
    public void testNextLongLowerGreaterUpper() {
        assertThrows(IllegalArgumentException.class, () -> RandomUtils.nextLong(2, 1));
    }

    @Test
    public void testNextDoubleLowerGreaterUpper() {
        assertThrows(IllegalArgumentException.class, () -> RandomUtils.nextDouble(2, 1));
    }

    @Test
    public void testNextFloatLowerGreaterUpper() {
        assertThrows(IllegalArgumentException.class, () -> RandomUtils.nextFloat(2, 1));
    }

    /**
     * Tests next boolean
     */
    @Test
    public void testBoolean() {
        final boolean result = RandomUtils.nextBoolean();
        assertTrue(result || !result);
    }

    /**
     * Tests a zero byte array length.
     */
    @Test
    public void testZeroLengthNextBytes() {
        assertArrayEquals(new byte[0], RandomUtils.nextBytes(0));
    }

    /**
     * Tests random byte array.
     */
    @Test
    public void testNextBytes() {
        final byte[] result = RandomUtils.nextBytes(20);
        assertEquals(20, result.length);
    }

    /**
     * Test next int range with minimal range.
     */
    @Test
    public void testNextIntMinimalRange() {
        assertEquals(42, RandomUtils.nextInt(42, 42));
    }

    /**
     * Tests next int range.
     */
    @Test
    public void testNextInt() {
        final int result = RandomUtils.nextInt(33, 42);
        assertTrue(result >= 33 && result < 42);
    }

    /**
     * Tests next int range, random result.
     */
    @Test
    public void testNextIntRandomResult() {
        final int randomResult = RandomUtils.nextInt();
        assertTrue(randomResult > 0);
        assertTrue(randomResult < Integer.MAX_VALUE);
    }

    /**
     * Test next double range with minimal range.
     */
    @Test
    public void testNextDoubleMinimalRange() {
        assertEquals(42.1, RandomUtils.nextDouble(42.1, 42.1), DELTA);
    }

    /**
     * Test next float range with minimal range.
     */
    @Test
    public void testNextFloatMinimalRange() {
        assertEquals(42.1f, RandomUtils.nextFloat(42.1f, 42.1f), DELTA);
    }

    /**
     * Tests next double range.
     */
    @Test
    public void testNextDouble() {
        final double result = RandomUtils.nextDouble(33d, 42d);
        assertTrue(result >= 33d && result <= 42d);
    }

    /**
     * Tests next double range, random result.
     */
    @Test
    public void testNextDoubleRandomResult() {
        final double randomResult = RandomUtils.nextDouble();
        assertTrue(randomResult > 0);
        assertTrue(randomResult < Double.MAX_VALUE);
    }

    /**
     * Tests next float range.
     */
    @Test
    public void testNextFloat() {
        final double result = RandomUtils.nextFloat(33f, 42f);
        assertTrue(result >= 33f && result <= 42f);
    }

    /**
     * Tests next float range, random result.
     */
    @Test
    public void testNextFloatRandomResult() {
        final float randomResult = RandomUtils.nextFloat();
        assertTrue(randomResult > 0);
        assertTrue(randomResult < Float.MAX_VALUE);
    }

    /**
     * Test next long range with minimal range.
     */
    @Test
    public void testNextLongMinimalRange() {
        assertEquals(42L, RandomUtils.nextLong(42L, 42L));
    }

    /**
     * Tests next long range.
     */
    @Test
    public void testNextLong() {
        final long result = RandomUtils.nextLong(33L, 42L);
        assertTrue(result >= 33L && result < 42L);
    }

    /**
     * Tests next long range, random result.
     */
    @Test
    public void testNextLongRandomResult() {
        final long randomResult = RandomUtils.nextLong();
        assertTrue(randomResult > 0);
        assertTrue(randomResult < Long.MAX_VALUE);
    }

    /**
     * Tests extreme range.
     */
    @Test
    public void testExtremeRangeInt() {
        final int result = RandomUtils.nextInt(0, Integer.MAX_VALUE);
        assertTrue(result >= 0 && result < Integer.MAX_VALUE);
    }

    /**
     * Tests extreme range.
     */
    @Test
    public void testExtremeRangeLong() {
        final long result = RandomUtils.nextLong(0, Long.MAX_VALUE);
        assertTrue(result >= 0 && result < Long.MAX_VALUE);
    }

    /**
     * Tests extreme range.
     */
    @Test
    public void testExtremeRangeFloat() {
        final float result = RandomUtils.nextFloat(0, Float.MAX_VALUE);
        assertTrue(result >= 0f && result <= Float.MAX_VALUE);
    }

    /**
     * Tests extreme range.
     */
    @Test
    public void testExtremeRangeDouble() {
        final double result = RandomUtils.nextDouble(0, Double.MAX_VALUE);
        assertTrue(result >= 0 && result <= Double.MAX_VALUE);
    }

    /**
     * Test a large value for long. A previous implementation using
     * {@link RandomUtils#nextDouble(double, double)} could generate a value equal
     * to the upper limit.
     *
     * <pre>
     * return (long) nextDouble(startInclusive, endExclusive);
     * </pre>
     *
     * <p>See LANG-1592.</p>
     */
    @Test
    public void testLargeValueRangeLong() {
        final long startInclusive = 12900000000001L;
        final long endExclusive = 12900000000016L;
        // Note: The method using 'return (long) nextDouble(startInclusive, endExclusive)'
        // takes thousands of calls to generate an error. This size loop fails most
        // of the time with the previous method.
        final int n = (int) (endExclusive - startInclusive) * 1000;
        for (int i = 0; i < n; i++) {
            assertNotEquals(endExclusive, RandomUtils.nextLong(startInclusive, endExclusive));
        }
    }
}
