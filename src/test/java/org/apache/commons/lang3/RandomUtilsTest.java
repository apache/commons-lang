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
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;

/**
 * Tests for {@link RandomUtils}
 */
class RandomUtilsTest {

    /**
     * For comparing doubles and floats
     */
    private static final double DELTA = 1e-5;

    @Test
    void testConstructor() {
        assertNotNull(new RandomUtils());
        final Constructor<?>[] cons = RandomUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertTrue(Modifier.isPublic(cons[0].getModifiers()));
        assertTrue(Modifier.isPublic(RandomUtils.class.getModifiers()));
        assertFalse(Modifier.isFinal(RandomUtils.class.getModifiers()));
    }

    @Test
    void testNextBytesNegative() {
        assertThrows(IllegalArgumentException.class, () -> RandomUtils.nextBytes(-1));
    }

    @Test
    void testNextIntNegative() {
        assertThrows(IllegalArgumentException.class, () -> RandomUtils.nextInt(-1, 1));
    }

    @Test
    void testNextLongNegative() {
        assertThrows(IllegalArgumentException.class, () -> RandomUtils.nextLong(-1, 1));
    }

    @Test
    void testNextDoubleNegative() {
        assertThrows(IllegalArgumentException.class, () -> RandomUtils.nextDouble(-1, 1));
    }

    @Test
    void testNextFloatNegative() {
        assertThrows(IllegalArgumentException.class, () -> RandomUtils.nextFloat(-1, 1));
    }

    @Test
    void testNextIntLowerGreaterUpper() {
        assertThrows(IllegalArgumentException.class, () -> RandomUtils.nextInt(2, 1));
    }

    @Test
    void testNextLongLowerGreaterUpper() {
        assertThrows(IllegalArgumentException.class, () -> RandomUtils.nextLong(2, 1));
    }

    @Test
    void testNextDoubleLowerGreaterUpper() {
        assertThrows(IllegalArgumentException.class, () -> RandomUtils.nextDouble(2, 1));
    }

    @Test
    void testNextFloatLowerGreaterUpper() {
        assertThrows(IllegalArgumentException.class, () -> RandomUtils.nextFloat(2, 1));
    }

    /**
     * Tests next boolean
     */
    @Test
    void testBoolean() {
        final boolean result = RandomUtils.nextBoolean();
        assertTrue(result == true || result == false);
    }

    /**
     * Tests a zero byte array length.
     */
    @Test
    void testZeroLengthNextBytes() {
        assertArrayEquals(new byte[0], RandomUtils.nextBytes(0));
    }

    /**
     * Tests random byte array.
     */
    @Test
    void testNextBytes() {
        final byte[] result = RandomUtils.nextBytes(20);
        assertEquals(20, result.length);
    }

    /**
     * Test next int range with minimal range.
     */
    @Test
    void testNextIntMinimalRange() {
        assertEquals(42, RandomUtils.nextInt(42, 42));
    }

    /**
     * Tests next int range.
     */
    @Test
    void testNextInt() {
        final int result = RandomUtils.nextInt(33, 42);
        assertTrue(result >= 33 && result < 42);
    }

    /**
     * Tests next int range, random result.
     */
    @Test
    void testNextIntRandomResult() {
        final int randomResult = RandomUtils.nextInt();
        assertTrue(randomResult > 0);
        assertTrue(randomResult < Integer.MAX_VALUE);
    }

    /**
     * Test next double range with minimal range.
     */
    @Test
    void testNextDoubleMinimalRange() {
        assertEquals(42.1, RandomUtils.nextDouble(42.1, 42.1), DELTA);
    }

    /**
     * Test next float range with minimal range.
     */
    @Test
    void testNextFloatMinimalRange() {
        assertEquals(42.1f, RandomUtils.nextFloat(42.1f, 42.1f), DELTA);
    }

    /**
     * Tests next double range.
     */
    @Test
    void testNextDouble() {
        final double result = RandomUtils.nextDouble(33d, 42d);
        assertTrue(result >= 33d && result <= 42d);
    }

    /**
     * Tests next double range, random result.
     */
    @Test
    void testNextDoubleRandomResult() {
        final double randomResult = RandomUtils.nextDouble();
        assertTrue(randomResult > 0);
        assertTrue(randomResult < Double.MAX_VALUE);
    }

    /**
     * Tests next float range.
     */
    @Test
    void testNextFloat() {
        final double result = RandomUtils.nextFloat(33f, 42f);
        assertTrue(result >= 33f && result <= 42f);
    }

    /**
     * Tests next float range, random result.
     */
    @Test
    void testNextFloatRandomResult() {
        final float randomResult = RandomUtils.nextFloat();
        assertTrue(randomResult > 0);
        assertTrue(randomResult < Float.MAX_VALUE);
    }

    /**
     * Test next long range with minimal range.
     */
    @Test
    void testNextLongMinimalRange() {
        assertEquals(42L, RandomUtils.nextLong(42L, 42L));
    }

    /**
     * Tests next long range.
     */
    @Test
    void testNextLong() {
        final long result = RandomUtils.nextLong(33L, 42L);
        assertTrue(result >= 33L && result < 42L);
    }

    /**
     * Tests next long range, random result.
     */
    @Test
    void testNextLongRandomResult() {
        final long randomResult = RandomUtils.nextLong();
        assertTrue(randomResult > 0);
        assertTrue(randomResult < Long.MAX_VALUE);
    }

    /**
     * Tests extreme range.
     */
    @Test
    void testExtremeRangeInt() {
        final int result = RandomUtils.nextInt(0, Integer.MAX_VALUE);
        assertTrue(result >= 0 && result < Integer.MAX_VALUE);
    }

    /**
     * Tests extreme range.
     */
    @Test
    void testExtremeRangeLong() {
        final long result = RandomUtils.nextLong(0, Long.MAX_VALUE);
        assertTrue(result >= 0 && result < Long.MAX_VALUE);
    }

    /**
     * Tests extreme range.
     */
    @Test
    void testExtremeRangeFloat() {
        final float result = RandomUtils.nextFloat(0, Float.MAX_VALUE);
        assertTrue(result >= 0f && result <= Float.MAX_VALUE);
    }

    /**
     * Tests extreme range.
     */
    @Test
    void testExtremeRangeDouble() {
        final double result = RandomUtils.nextDouble(0, Double.MAX_VALUE);
        assertTrue(result >= 0 && result <= Double.MAX_VALUE);
    }
}
