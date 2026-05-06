/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
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

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTimeout;

import java.time.Duration;

import org.junit.jupiter.api.Test;

/**
 * Primitive joins can OOM before index validation.
 * <p>
 * capacity() is called before bounds validation in join(byte[], char, int, int). An endIndex of Integer.MAX_VALUE causes an attempt to allocate ~8 GB before
 * the array bounds are checked. Negative-startIndex bypass case.
 * </p>
 * <p>
 * For {@code startIndex = -2_000_000_000, endIndex = 1, array.length = 2}: {@code count = endIndex - startIndex = 2_000_000_001}, which is positive (no
 * overflow into negative territory), so the {@code count <= 0} early-return does not fire. The pre-existing {@code endIndex > array.length} check also passes
 * (1 &lt; 2). The capacity allocation then attempts to size a StringBuilder for ~2 billion items, triggering OOM before the loop body would have caught the bad
 * index.
 * </p>
 *
 * Pre-patch: throws OutOfMemoryError or hangs for > 2 seconds. Post-patch: throws ArrayIndexOutOfBoundsException quickly (within 2 seconds).
 */
public class StringUtilsJoinExceptionTest {

    private static final Class<ArrayIndexOutOfBoundsException> EXPECTED_EX = ArrayIndexOutOfBoundsException.class;
    private static final Duration TIMEOUT = Duration.ofSeconds(2);

    @Test
    public void testBooleanJoinValidatesEndIndexBeforeCapacity() {
        assertTimeout(TIMEOUT, () -> assertThrows(EXPECTED_EX, () -> StringUtils.join(new boolean[] { true }, ',', 0, Integer.MAX_VALUE)));
    }

    @Test
    public void testBooleanJoinValidatesNegativeStartIndexBeforeCapacity() {
        assertTimeout(TIMEOUT, () -> assertThrows(EXPECTED_EX, () -> StringUtils.join(new boolean[] { true, false }, ',', -2_000_000_000, 1)));
    }

    @Test
    public void testByteJoinValidatesEndIndexBeforeCapacity() {
        assertTimeout(TIMEOUT, () -> assertThrows(EXPECTED_EX, () -> StringUtils.join(new byte[] { 1 }, ',', 0, Integer.MAX_VALUE)));
    }

    @Test
    public void testByteJoinValidatesNegativeStartIndexBeforeCapacity() {
        assertTimeout(TIMEOUT, () -> assertThrows(EXPECTED_EX, () -> StringUtils.join(new byte[] { 1, 2 }, ',', -2_000_000_000, 1)));
    }

    @Test
    public void testCharJoinValidatesEndIndexBeforeCapacity() {
        assertTimeout(TIMEOUT, () -> assertThrows(EXPECTED_EX, () -> StringUtils.join(new char[] { 1 }, ',', 0, Integer.MAX_VALUE)));
    }

    @Test
    public void testCharJoinValidatesNegativeStartIndexBeforeCapacity() {
        assertTimeout(TIMEOUT, () -> assertThrows(EXPECTED_EX, () -> StringUtils.join(new char[] { 1, 2 }, ',', -2_000_000_000, 1)));
    }

    @Test
    public void testDoubleJoinValidatesEndIndexBeforeCapacity() {
        assertTimeout(TIMEOUT, () -> assertThrows(EXPECTED_EX, () -> StringUtils.join(new double[] { 1 }, ',', 0, Integer.MAX_VALUE)));
    }

    @Test
    public void testDoubleJoinValidatesNegativeStartIndexBeforeCapacity() {
        assertTimeout(TIMEOUT, () -> assertThrows(EXPECTED_EX, () -> StringUtils.join(new double[] { 1, 2 }, ',', Integer.MIN_VALUE + 100, 1)));
    }

    @Test
    public void testFloatJoinValidatesEndIndexBeforeCapacity() {
        assertTimeout(TIMEOUT, () -> assertThrows(EXPECTED_EX, () -> StringUtils.join(new float[] { 1 }, ',', 0, Integer.MAX_VALUE)));
    }

    @Test
    public void testFloatJoinValidatesNegativeStartIndexBeforeCapacity() {
        assertTimeout(TIMEOUT, () -> assertThrows(EXPECTED_EX, () -> StringUtils.join(new float[] { 1, 2 }, ',', Integer.MIN_VALUE + 100, 1)));
    }

    @Test
    public void testIntJoinValidatesEndIndexBeforeCapacity() {
        assertTimeout(TIMEOUT, () -> assertThrows(EXPECTED_EX, () -> StringUtils.join(new int[] { 1 }, ',', 0, Integer.MAX_VALUE)));
    }

    @Test
    public void testIntJoinValidatesNegativeStartIndexBeforeCapacity() {
        assertTimeout(TIMEOUT, () -> assertThrows(EXPECTED_EX, () -> StringUtils.join(new int[] { 1, 2 }, ',', Integer.MIN_VALUE + 100, 1)));
    }

    @Test
    public void testLongJoinValidatesEndIndexBeforeCapacity() {
        assertTimeout(TIMEOUT, () -> assertThrows(EXPECTED_EX, () -> StringUtils.join(new long[] { 1L }, ',', 0, Integer.MAX_VALUE)));
    }

    @Test
    public void testLongJoinValidatesNegativeStartIndexBeforeCapacity() {
        assertTimeout(TIMEOUT, () -> assertThrows(EXPECTED_EX, () -> StringUtils.join(new long[] { 1L, 2L }, ',', -2_000_000_000, 1)));
    }

    @Test
    public void testShortJoinValidatesEndIndexBeforeCapacity() {
        assertTimeout(TIMEOUT, () -> assertThrows(EXPECTED_EX, () -> StringUtils.join(new short[] { 1 }, ',', 0, Integer.MAX_VALUE)));
    }

    @Test
    public void testShortJoinValidatesNegativeStartIndexBeforeCapacity() {
        assertTimeout(TIMEOUT, () -> assertThrows(EXPECTED_EX, () -> StringUtils.join(new short[] { 1, 2 }, ',', -2_000_000_000, 1)));
    }
}
