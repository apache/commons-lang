/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang3;

import static org.apache.commons.lang3.LangAssertions.assertIllegalArgumentException;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Tests for {@link RandomUtils}
 */
class RandomUtilsTest extends AbstractLangTest {

    /**
     * For comparing doubles and floats
     */
    private static final double DELTA = 1e-5;

    static Stream<RandomUtils> randomProvider() {
        return Stream.of(RandomUtils.secure(), RandomUtils.secureStrong(), RandomUtils.insecure());
    }

    /**
     * Tests next boolean
     */
    @Test
    void testBoolean() {
        final boolean result = RandomUtils.nextBoolean();
        assertTrue(result || !result);
    }

    @ParameterizedTest
    @MethodSource("randomProvider")
    void testBoolean(final RandomUtils ru) {
        final boolean result = ru.randomBoolean();
        assertTrue(result || !result);
    }

    @Test
    void testConstructor() {
        assertNotNull(new RandomUtils());
    }

    /**
     * Tests extreme range.
     */
    @Test
    void testExtremeRangeDouble() {
        final double result = RandomUtils.nextDouble(0, Double.MAX_VALUE);
        assertTrue(result >= 0 && result <= Double.MAX_VALUE); // TODO: should be <max?
    }

    @ParameterizedTest
    @MethodSource("randomProvider")
    void testExtremeRangeDouble(final RandomUtils ru) {
        final double result = ru.randomDouble(0, Double.MAX_VALUE);
        assertTrue(result >= 0 && result <= Double.MAX_VALUE); // TODO: should be <max?
    }

    /**
     * Tests extreme range.
     */
    @Test
    void testExtremeRangeFloat() {
        final float result = RandomUtils.nextFloat(0, Float.MAX_VALUE);
        assertTrue(result >= 0f && result <= Float.MAX_VALUE); // TODO: should be <max?
    }

    /**
     * Tests extreme range.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    void testExtremeRangeFloat(final RandomUtils ru) {
        final float result = ru.randomFloat(0, Float.MAX_VALUE);
        assertTrue(result >= 0f && result <= Float.MAX_VALUE); // TODO: should be <max?
    }

    /**
     * Tests extreme range.
     */
    @Test
    void testExtremeRangeInt() {
        final int result = RandomUtils.nextInt(0, Integer.MAX_VALUE);
        assertTrue(result >= 0);
        assertTrue(result < Integer.MAX_VALUE);
    }

    /**
     * Tests extreme range.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    void testExtremeRangeInt(final RandomUtils ru) {
        final int result = ru.randomInt(0, Integer.MAX_VALUE);
        assertTrue(result >= 0);
        assertTrue(result < Integer.MAX_VALUE);
    }

    /**
     * Tests extreme range.
     */
    @Test
    void testExtremeRangeLong() {
        final long result = RandomUtils.nextLong(0, Long.MAX_VALUE);
        assertTrue(result >= 0);
        assertTrue(result < Long.MAX_VALUE);
    }

    /**
     * Tests extreme range.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    void testExtremeRangeLong(final RandomUtils ru) {
        final long result = ru.randomLong(0, Long.MAX_VALUE);
        assertTrue(result >= 0);
        assertTrue(result < Long.MAX_VALUE);
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
    void testLargeValueRangeLong() {
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
    @ParameterizedTest
    @MethodSource("randomProvider")
    void testLargeValueRangeLong(final RandomUtils ru) {
        final long startInclusive = 12900000000001L;
        final long endExclusive = 12900000000016L;
        // Note: The method using 'return (long) nextDouble(startInclusive, endExclusive)'
        // takes thousands of calls to generate an error. This size loop fails most
        // of the time with the previous method.
        final int n = (int) (endExclusive - startInclusive) * 1000;
        for (int i = 0; i < n; i++) {
            assertNotEquals(endExclusive, ru.randomLong(startInclusive, endExclusive));
        }
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
     * Tests random byte array.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    void testNextBytes(final RandomUtils ru) {
        final byte[] result = ru.randomBytes(20);
        assertEquals(20, result.length);
    }

    @Test
    void testNextBytesNegative() {
        assertIllegalArgumentException(() -> RandomUtils.nextBytes(-1));
    }

    @ParameterizedTest
    @MethodSource("randomProvider")
    void testNextBytesNegative(final RandomUtils ru) {
        assertIllegalArgumentException(() -> ru.randomBytes(-1));
    }

    /**
     * Tests next double range.
     */
    @Test
    void testNextDouble() {
        final double result = RandomUtils.nextDouble(33d, 42d);
        assertTrue(result >= 33d);
        assertTrue(result < 42d);
    }

    /**
     * Tests next double range.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    void testNextDouble(final RandomUtils ru) {
        final double result = ru.randomDouble(33d, 42d);
        assertTrue(result >= 33d);
        assertTrue(result < 42d);
    }

    @Test
    void testNextDoubleLowerGreaterUpper() {
        assertIllegalArgumentException(() -> RandomUtils.nextDouble(2, 1));
    }

    @ParameterizedTest
    @MethodSource("randomProvider")
    void testNextDoubleLowerGreaterUpper(final RandomUtils ru) {
        assertIllegalArgumentException(() -> ru.randomDouble(2, 1));
    }

    /**
     * Test next double range with minimal range.
     */
    @Test
    void testNextDoubleMinimalRange() {
        assertEquals(42.1, RandomUtils.nextDouble(42.1, 42.1), DELTA);
    }

    /**
     * Test next double range with minimal range.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    void testNextDoubleMinimalRange(final RandomUtils ru) {
        assertEquals(42.1, ru.randomDouble(42.1, 42.1), DELTA);
    }

    @Test
    void testNextDoubleNegative() {
        assertIllegalArgumentException(() -> RandomUtils.nextDouble(-1, 1));
    }

    @ParameterizedTest
    @MethodSource("randomProvider")
    void testNextDoubleNegative(final RandomUtils ru) {
        assertIllegalArgumentException(() -> ru.randomDouble(-1, 1));
    }

    /**
     * Tests next double range, random result.
     */
    @Test
    void testNextDoubleRandomResult() {
        final double result = RandomUtils.nextDouble();
        assertTrue(result >= 0d);
        assertTrue(result < Double.MAX_VALUE);
    }

    /**
     * Tests next double range, random result.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    void testNextDoubleRandomResult(final RandomUtils ru) {
        final double result = ru.randomDouble();
        assertTrue(result >= 0d);
        assertTrue(result < Double.MAX_VALUE);
    }

    /**
     * Tests next float range.
     */
    @Test
    void testNextFloat() {
        final float result = RandomUtils.nextFloat(33f, 42f);
        assertTrue(result >= 33f);
        assertTrue(result < 42f);
    }

    /**
     * Tests next float range.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    void testNextFloat(final RandomUtils ru) {
        final float result = ru.randomFloat(33f, 42f);
        assertTrue(result >= 33f);
        assertTrue(result < 42f);
    }

    @Test
    void testNextFloatLowerGreaterUpper() {
        assertIllegalArgumentException(() -> RandomUtils.nextFloat(2, 1));
    }

    @ParameterizedTest
    @MethodSource("randomProvider")
    void testNextFloatLowerGreaterUpper(final RandomUtils ru) {
        assertIllegalArgumentException(() -> ru.randomFloat(2, 1));
    }

    /**
     * Test next float range with minimal range.
     */
    @Test
    void testNextFloatMinimalRange() {
        assertEquals(42.1f, RandomUtils.nextFloat(42.1f, 42.1f), DELTA);
    }

    /**
     * Test next float range with minimal range.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    void testNextFloatMinimalRange(final RandomUtils ru) {
        assertEquals(42.1f, ru.randomFloat(42.1f, 42.1f), DELTA);
    }

    @Test
    void testNextFloatNegative() {
        assertIllegalArgumentException(() -> RandomUtils.nextFloat(-1, 1));
    }

    @ParameterizedTest
    @MethodSource("randomProvider")
    void testNextFloatNegative(final RandomUtils ru) {
        assertIllegalArgumentException(() -> ru.randomFloat(-1, 1));
    }

    /**
     * Tests next float range, random result.
     */
    @Test
    void testNextFloatRandomResult() {
        final float result = RandomUtils.nextFloat();
        assertTrue(result >= 0f);
        assertTrue(result < Float.MAX_VALUE);
    }

    /**
     * Tests next float range, random result.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    void testNextFloatRandomResult(final RandomUtils ru) {
        final float result = ru.randomFloat();
        assertTrue(result >= 0f);
        assertTrue(result < Float.MAX_VALUE);
    }

    /**
     * Tests next int range.
     */
    @Test
    void testNextInt() {
        final int result = RandomUtils.nextInt(33, 42);
        assertTrue(result >= 33);
        assertTrue(result < 42);
    }

    /**
     * Tests next int range.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    void testNextInt(final RandomUtils ru) {
        final int result = ru.randomInt(33, 42);
        assertTrue(result >= 33);
        assertTrue(result < 42);
    }

    @Test
    void testNextIntLowerGreaterUpper() {
        assertIllegalArgumentException(() -> RandomUtils.nextInt(2, 1));
    }

    @ParameterizedTest
    @MethodSource("randomProvider")
    void testNextIntLowerGreaterUpper(final RandomUtils ru) {
        assertIllegalArgumentException(() -> ru.randomInt(2, 1));
    }

    /**
     * Test next int range with minimal range.
     */
    @Test
    void testNextIntMinimalRange() {
        assertEquals(42, RandomUtils.nextInt(42, 42));
    }

    /**
     * Test next int range with minimal range.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    void testNextIntMinimalRange(final RandomUtils ru) {
        assertEquals(42, ru.randomInt(42, 42));
    }

    @Test
    void testNextIntNegative() {
        assertIllegalArgumentException(() -> RandomUtils.nextInt(-1, 1));
    }

    @ParameterizedTest
    @MethodSource("randomProvider")
    void testNextIntNegative(final RandomUtils ru) {
        assertIllegalArgumentException(() -> ru.randomInt(-1, 1));
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
     * Tests next int range, random result.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    void testNextIntRandomResult(final RandomUtils ru) {
        final int randomResult = ru.randomInt();
        assertTrue(randomResult > 0);
        assertTrue(randomResult < Integer.MAX_VALUE);
    }

    /**
     * Tests next long range.
     */
    @Test
    void testNextLong() {
        final long result = RandomUtils.nextLong(33L, 42L);
        assertTrue(result >= 33L);
        assertTrue(result < 42L);
    }

    /**
     * Tests next long range.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    void testNextLong(final RandomUtils ru) {
        final long result = ru.randomLong(33L, 42L);
        assertTrue(result >= 33L);
        assertTrue(result < 42L);
    }

    @Test
    void testNextLongLowerGreaterUpper() {
        assertIllegalArgumentException(() -> RandomUtils.nextLong(2, 1));
    }

    @ParameterizedTest
    @MethodSource("randomProvider")
    void testNextLongLowerGreaterUpper(final RandomUtils ru) {
        assertIllegalArgumentException(() -> ru.randomLong(2, 1));
    }

    /**
     * Test next long range with minimal range.
     */
    @Test
    void testNextLongMinimalRange() {
        assertEquals(42L, RandomUtils.nextLong(42L, 42L));
    }

    /**
     * Test next long range with minimal range.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    void testNextLongMinimalRange(final RandomUtils ru) {
        assertEquals(42L, ru.randomLong(42L, 42L));
    }

    @Test
    void testNextLongNegative() {
        assertIllegalArgumentException(() -> RandomUtils.nextLong(-1, 1));
    }

    @ParameterizedTest
    @MethodSource("randomProvider")
    void testNextLongNegative(final RandomUtils ru) {
        assertIllegalArgumentException(() -> ru.randomLong(-1, 1));
    }

    /**
     * Tests next long range, random result.
     */
    @Test
    void testNextLongRandomResult() {
        final long result = RandomUtils.nextLong();
        assertTrue(result >= 0L);
        assertTrue(result < Long.MAX_VALUE);
    }

    /**
     * Tests next long range, random result.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    void testNextLongRandomResult(final RandomUtils ru) {
        final long result = ru.randomLong();
        assertTrue(result >= 0L);
        assertTrue(result < Long.MAX_VALUE);
    }

    /**
     * Tests a zero byte array length.
     */
    @Test
    void testZeroLengthNextBytes() {
        assertArrayEquals(new byte[0], RandomUtils.nextBytes(0));
    }

    /**
     * Tests a zero byte array length.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    void testZeroLengthNextBytes(final RandomUtils ru) {
        assertArrayEquals(new byte[0], ru.randomBytes(0));
    }
}
