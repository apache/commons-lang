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

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.allOf;
import static org.hamcrest.Matchers.greaterThanOrEqualTo;
import static org.hamcrest.Matchers.lessThan;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Tests for {@link RandomUtils}
 */
public class RandomUtilsTest extends AbstractLangTest {

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
    public void testBoolean() {
        final boolean result = RandomUtils.nextBoolean();
        assertTrue(result || !result);
    }

    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testBoolean(final RandomUtils ru) {
        final boolean result = ru.randomBoolean();
        assertTrue(result || !result);
    }

    @Test
    public void testConstructor() {
        assertNotNull(new RandomUtils());
    }

    /**
     * Tests extreme range.
     */
    @Test
    public void testExtremeRangeDouble() {
        final double result = RandomUtils.nextDouble(0, Double.MAX_VALUE);
        assertTrue(result >= 0 && result <= Double.MAX_VALUE); // TODO: should be <max?
    }

    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testExtremeRangeDouble(final RandomUtils ru) {
        final double result = ru.randomDouble(0, Double.MAX_VALUE);
        assertTrue(result >= 0 && result <= Double.MAX_VALUE); // TODO: should be <max?
    }

    /**
     * Tests extreme range.
     */
    @Test
    public void testExtremeRangeFloat() {
        final float result = RandomUtils.nextFloat(0, Float.MAX_VALUE);
        assertTrue(result >= 0f && result <= Float.MAX_VALUE); // TODO: should be <max?
    }

    /**
     * Tests extreme range.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testExtremeRangeFloat(final RandomUtils ru) {
        final float result = ru.randomFloat(0, Float.MAX_VALUE);
        assertTrue(result >= 0f && result <= Float.MAX_VALUE); // TODO: should be <max?
    }

    /**
     * Tests extreme range.
     */
    @Test
    public void testExtremeRangeInt() {
        final int result = RandomUtils.nextInt(0, Integer.MAX_VALUE);
        assertThat("result >= 0 && result < Integer.MAX_VALUE", result, allOf(greaterThanOrEqualTo(0), lessThan(Integer.MAX_VALUE)));
    }

    /**
     * Tests extreme range.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testExtremeRangeInt(final RandomUtils ru) {
        final int result = ru.randomInt(0, Integer.MAX_VALUE);
        assertThat("result >= 0 && result < Integer.MAX_VALUE", result, allOf(greaterThanOrEqualTo(0), lessThan(Integer.MAX_VALUE)));
    }

    /**
     * Tests extreme range.
     */
    @Test
    public void testExtremeRangeLong() {
        final long result = RandomUtils.nextLong(0, Long.MAX_VALUE);
        assertThat("result >= 0 && result < Long.MAX_VALUE", result, allOf(greaterThanOrEqualTo(0L), lessThan(Long.MAX_VALUE)));
    }

    /**
     * Tests extreme range.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testExtremeRangeLong(final RandomUtils ru) {
        final long result = ru.randomLong(0, Long.MAX_VALUE);
        assertThat("result >= 0 && result < Long.MAX_VALUE", result, allOf(greaterThanOrEqualTo(0L), lessThan(Long.MAX_VALUE)));
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
    public void testLargeValueRangeLong(final RandomUtils ru) {
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
    public void testNextBytes() {
        final byte[] result = RandomUtils.nextBytes(20);
        assertEquals(20, result.length);
    }

    /**
     * Tests random byte array.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testNextBytes(final RandomUtils ru) {
        final byte[] result = ru.randomBytes(20);
        assertEquals(20, result.length);
    }

    @Test
    public void testNextBytesNegative() {
        assertThrows(IllegalArgumentException.class, () -> RandomUtils.nextBytes(-1));
    }

    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testNextBytesNegative(final RandomUtils ru) {
        assertThrows(IllegalArgumentException.class, () -> ru.randomBytes(-1));
    }

    /**
     * Tests next double range.
     */
    @Test
    public void testNextDouble() {
        final double result = RandomUtils.nextDouble(33d, 42d);
        assertThat("result >= 33d && result < 42d", result, allOf(greaterThanOrEqualTo(33d), lessThan(42d)));
    }

    /**
     * Tests next double range.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testNextDouble(final RandomUtils ru) {
        final double result = ru.randomDouble(33d, 42d);
        assertThat("result >= 33d && result < 42d", result, allOf(greaterThanOrEqualTo(33d), lessThan(42d)));
    }

    @Test
    public void testNextDoubleLowerGreaterUpper() {
        assertThrows(IllegalArgumentException.class, () -> RandomUtils.nextDouble(2, 1));
    }

    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testNextDoubleLowerGreaterUpper(final RandomUtils ru) {
        assertThrows(IllegalArgumentException.class, () -> ru.randomDouble(2, 1));
    }

    /**
     * Test next double range with minimal range.
     */
    @Test
    public void testNextDoubleMinimalRange() {
        assertEquals(42.1, RandomUtils.nextDouble(42.1, 42.1), DELTA);
    }

    /**
     * Test next double range with minimal range.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testNextDoubleMinimalRange(final RandomUtils ru) {
        assertEquals(42.1, ru.randomDouble(42.1, 42.1), DELTA);
    }

    @Test
    public void testNextDoubleNegative() {
        assertThrows(IllegalArgumentException.class, () -> RandomUtils.nextDouble(-1, 1));
    }

    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testNextDoubleNegative(final RandomUtils ru) {
        assertThrows(IllegalArgumentException.class, () -> ru.randomDouble(-1, 1));
    }

    /**
     * Tests next double range, random result.
     */
    @Test
    public void testNextDoubleRandomResult() {
        final double randomResult = RandomUtils.nextDouble();
        assertThat("randomResult >= 0 0 && randomResult < Double.MAX_VALUE", randomResult, allOf(greaterThanOrEqualTo(0d), lessThan(Double.MAX_VALUE)));
    }

    /**
     * Tests next double range, random result.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testNextDoubleRandomResult(final RandomUtils ru) {
        final double randomResult = ru.randomDouble();
        assertThat("randomResult >= 0 0 && randomResult < Double.MAX_VALUE", randomResult, allOf(greaterThanOrEqualTo(0d), lessThan(Double.MAX_VALUE)));
    }

    /**
     * Tests next float range.
     */
    @Test
    public void testNextFloat() {
        final float result = RandomUtils.nextFloat(33f, 42f);
        assertThat("result >= 33f && result < 42f", result, allOf(greaterThanOrEqualTo(33f), lessThan(42f)));
    }

    /**
     * Tests next float range.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testNextFloat(final RandomUtils ru) {
        final float result = ru.randomFloat(33f, 42f);
        assertThat("result >= 33f && result < 42f", result, allOf(greaterThanOrEqualTo(33f), lessThan(42f)));
    }

    @Test
    public void testNextFloatLowerGreaterUpper() {
        assertThrows(IllegalArgumentException.class, () -> RandomUtils.nextFloat(2, 1));
    }

    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testNextFloatLowerGreaterUpper(final RandomUtils ru) {
        assertThrows(IllegalArgumentException.class, () -> ru.randomFloat(2, 1));
    }

    /**
     * Test next float range with minimal range.
     */
    @Test
    public void testNextFloatMinimalRange() {
        assertEquals(42.1f, RandomUtils.nextFloat(42.1f, 42.1f), DELTA);
    }

    /**
     * Test next float range with minimal range.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testNextFloatMinimalRange(final RandomUtils ru) {
        assertEquals(42.1f, ru.randomFloat(42.1f, 42.1f), DELTA);
    }

    @Test
    public void testNextFloatNegative() {
        assertThrows(IllegalArgumentException.class, () -> RandomUtils.nextFloat(-1, 1));
    }

    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testNextFloatNegative(final RandomUtils ru) {
        assertThrows(IllegalArgumentException.class, () -> ru.randomFloat(-1, 1));
    }

    /**
     * Tests next float range, random result.
     */
    @Test
    public void testNextFloatRandomResult() {
        final float randomResult = RandomUtils.nextFloat();
        assertThat("randomResult >= 0 && randomResult < Double.MAX_VALUE", randomResult, allOf(greaterThanOrEqualTo(0f), lessThan(Float.MAX_VALUE)));
    }

    /**
     * Tests next float range, random result.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testNextFloatRandomResult(final RandomUtils ru) {
        final float randomResult = ru.randomFloat();
        assertThat("randomResult >= 0 && randomResult < Double.MAX_VALUE", randomResult, allOf(greaterThanOrEqualTo(0f), lessThan(Float.MAX_VALUE)));
    }

    /**
     * Tests next int range.
     */
    @Test
    public void testNextInt() {
        final int result = RandomUtils.nextInt(33, 42);
        assertThat("result >= 33 && result < 42", result, allOf(greaterThanOrEqualTo(33), lessThan(42)));
    }

    /**
     * Tests next int range.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testNextInt(final RandomUtils ru) {
        final int result = ru.randomInt(33, 42);
        assertThat("result >= 33 && result < 42", result, allOf(greaterThanOrEqualTo(33), lessThan(42)));
    }

    @Test
    public void testNextIntLowerGreaterUpper() {
        assertThrows(IllegalArgumentException.class, () -> RandomUtils.nextInt(2, 1));
    }

    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testNextIntLowerGreaterUpper(final RandomUtils ru) {
        assertThrows(IllegalArgumentException.class, () -> ru.randomInt(2, 1));
    }

    /**
     * Test next int range with minimal range.
     */
    @Test
    public void testNextIntMinimalRange() {
        assertEquals(42, RandomUtils.nextInt(42, 42));
    }

    /**
     * Test next int range with minimal range.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testNextIntMinimalRange(final RandomUtils ru) {
        assertEquals(42, ru.randomInt(42, 42));
    }

    @Test
    public void testNextIntNegative() {
        assertThrows(IllegalArgumentException.class, () -> RandomUtils.nextInt(-1, 1));
    }

    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testNextIntNegative(final RandomUtils ru) {
        assertThrows(IllegalArgumentException.class, () -> ru.randomInt(-1, 1));
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
     * Tests next int range, random result.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testNextIntRandomResult(final RandomUtils ru) {
        final int randomResult = ru.randomInt();
        assertTrue(randomResult > 0);
        assertTrue(randomResult < Integer.MAX_VALUE);
    }

    /**
     * Tests next long range.
     */
    @Test
    public void testNextLong() {
        final long result = RandomUtils.nextLong(33L, 42L);
        assertThat("result >= 33L && result < 42L", result, allOf(greaterThanOrEqualTo(33L), lessThan(42L)));
    }

    /**
     * Tests next long range.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testNextLong(final RandomUtils ru) {
        final long result = ru.randomLong(33L, 42L);
        assertThat("result >= 33L && result < 42L", result, allOf(greaterThanOrEqualTo(33L), lessThan(42L)));
    }

    @Test
    public void testNextLongLowerGreaterUpper() {
        assertThrows(IllegalArgumentException.class, () -> RandomUtils.nextLong(2, 1));
    }

    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testNextLongLowerGreaterUpper(final RandomUtils ru) {
        assertThrows(IllegalArgumentException.class, () -> ru.randomLong(2, 1));
    }

    /**
     * Test next long range with minimal range.
     */
    @Test
    public void testNextLongMinimalRange() {
        assertEquals(42L, RandomUtils.nextLong(42L, 42L));
    }

    /**
     * Test next long range with minimal range.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testNextLongMinimalRange(final RandomUtils ru) {
        assertEquals(42L, ru.randomLong(42L, 42L));
    }

    @Test
    public void testNextLongNegative() {
        assertThrows(IllegalArgumentException.class, () -> RandomUtils.nextLong(-1, 1));
    }

    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testNextLongNegative(final RandomUtils ru) {
        assertThrows(IllegalArgumentException.class, () -> ru.randomLong(-1, 1));
    }

    /**
     * Tests next long range, random result.
     */
    @Test
    public void testNextLongRandomResult() {
        final long randomResult = RandomUtils.nextLong();
        assertThat("randomResult >= 0 && randomResult < Long.MAX_VALUE", randomResult, allOf(greaterThanOrEqualTo(0L), lessThan(Long.MAX_VALUE)));
    }

    /**
     * Tests next long range, random result.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testNextLongRandomResult(final RandomUtils ru) {
        final long randomResult = ru.randomLong();
        assertThat("randomResult >= 0 && randomResult < Long.MAX_VALUE", randomResult, allOf(greaterThanOrEqualTo(0L), lessThan(Long.MAX_VALUE)));
    }

    /**
     * Tests a zero byte array length.
     */
    @Test
    public void testZeroLengthNextBytes() {
        assertArrayEquals(new byte[0], RandomUtils.nextBytes(0));
    }

    /**
     * Tests a zero byte array length.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testZeroLengthNextBytes(final RandomUtils ru) {
        assertArrayEquals(new byte[0], ru.randomBytes(0));
    }
}
