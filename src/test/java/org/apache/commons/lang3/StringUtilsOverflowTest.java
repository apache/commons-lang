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

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledIfSystemProperty;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Regression tests for overflow in string length and capacity calculations.
 *
 * <p>
 * Enable the large-input tests with {@code -Dtest.stringutils.large.heap=true -DheapSize=-Xmx8g}.
 * Use {@code -DheapSize=-Xmx12g} on Java 8.
 * </p>
 */
class StringUtilsOverflowTest extends AbstractLangTest {

    @ParameterizedTest
    @ValueSource(bytes = { 1, 4, 5, 6, 11, 12, 20, 22 })
    void testJoinCapacityOverflow(final byte maxElementChars) throws Exception {
        // Exercise the shared helper without allocating a multi-gigabyte primitive array.
        final Method method = StringUtils.class.getDeclaredMethod("capacity", int.class, byte.class);
        method.setAccessible(true);
        final int firstUnsafe = (int) (((long) ArrayUtils.SAFE_MAX_ARRAY_LENGTH + 1) / (maxElementChars + 1) + 1);
        final int firstOverflow = (int) (((long) Integer.MAX_VALUE + 1) / (maxElementChars + 1) + 1);
        for (final int count : new int[] { firstUnsafe, firstOverflow, Integer.MAX_VALUE }) {
            final StringBuilder builder = (StringBuilder) method.invoke(null, count, maxElementChars);
            assertEquals(new StringBuilder().capacity(), builder.capacity());
            assertEquals("0,0", builder.append("0,0").toString());
        }
        assertEquals(2 * maxElementChars + 1, ((StringBuilder) method.invoke(null, 2, maxElementChars)).capacity());
    }

    @Test
    @EnabledIfSystemProperty(named = "test.stringutils.large.heap", matches = "true")
    void testJoinLongArrayCapacityOverflow() {
        // The maximum-width estimate overflows, but the actual zero values need only one character each.
        final int count = 102_261_127;
        final String result = StringUtils.join(new long[count], ',');
        assertEquals(2 * count - 1, result.length());
        assertEquals(count, StringUtils.countMatches(result, '0'));
        assertEquals(count - 1, StringUtils.countMatches(result, ','));
    }

    @ParameterizedTest
    @ValueSource(longs = { (long) ArrayUtils.SAFE_MAX_ARRAY_LENGTH + 1, Integer.MAX_VALUE, (long) Integer.MAX_VALUE + 1, Long.MAX_VALUE })
    void testOversizedCapacityEstimate(final long estimate) throws Exception {
        final Method method = StringUtils.class.getDeclaredMethod("newStringBuilder", long.class);
        method.setAccessible(true);
        final StringBuilder builder = (StringBuilder) method.invoke(null, estimate);
        assertEquals(new StringBuilder().capacity(), builder.capacity());
        assertEquals("result", builder.append("result").toString());
    }

    /**
     * Exercises counts immediately below and above overflow of the former intermediate length, with and without non-empty repeated text.
     */
    @ParameterizedTest
    @CsvSource({
        "'', 21474835, 2147483400",
        "'', 21474836, 2147483500",
        "'', 21474837, 2147483600",
        "x, 21262213, 2147483413",
        "x, 21262214, 2147483514",
        "x, 21262215, 2147483615"
    })
    @EnabledIfSystemProperty(named = "test.stringutils.large.heap", matches = "true")
    void testRepeatWithSeparatorLengthBoundaries(final String text, final int count, final int expectedLength) {
        final String separator = new String(new byte[100], StandardCharsets.ISO_8859_1);
        final String result = StringUtils.repeat(text, separator, count);
        final int textCount = text.isEmpty() ? 0 : count;
        assertEquals(expectedLength, result.length());
        assertEquals(textCount, StringUtils.countMatches(result, 'x'));
        assertEquals(expectedLength - textCount, StringUtils.countMatches(result, '\0'));
        assertTrue(result.startsWith(text + separator + text));
        assertTrue(result.endsWith(text + separator + text));
    }

    @ParameterizedTest
    @CsvSource({
        "a, b, 1073741825",
        "'', ab, 1073741825",
        "ab, '', 1073741824",
        "abc, d, 536870913",
        "ab, cd, 2147483647"
    })
    void testRepeatWithSeparatorRejectsUnrepresentableLength(final String text, final String separator, final int count) {
        assertThrows(IllegalArgumentException.class, () -> StringUtils.repeat(text, separator, count));
    }

    @Test
    @EnabledIfSystemProperty(named = "test.stringutils.large.heap", matches = "true")
    void testReplaceEachSingleRuleEstimateOverflow() {
        // Multiplication by three overflows even before estimates from different rules are added.
        final String[] search = { "a", "z" };
        final String[] replacement = { "b", StringUtils.repeat('x', Integer.MAX_VALUE / 3 + 2) };
        assertAll(
                () -> assertEquals("b", StringUtils.replaceEach("a", search, replacement)),
                () -> assertEquals("b", StringUtils.replaceEachRepeatedly("a", search, replacement)));
    }

    @Test
    @EnabledIfSystemProperty(named = "test.stringutils.large.heap", matches = "true")
    void testReplaceEachTotalCapacityOverflow() {
        // The input fits in a String, but input length plus the 20% estimate exceeds Integer.MAX_VALUE.
        final String text = new String(new byte[1_800_000_000], StandardCharsets.ISO_8859_1);
        final String[] search = { text, "a" };
        final String[] replacement = { "", text };
        assertAll(
                () -> assertEquals("", StringUtils.replaceEach(text, search, replacement)),
                () -> assertEquals("", StringUtils.replaceEachRepeatedly(text, search, replacement)));
    }

    @ParameterizedTest
    @ValueSource(ints = { 715, 716, 717, 1432 })
    void testReplaceEachUnusedRulesOverflow(final int unusedRules) {
        final String[] search = new String[unusedRules + 1];
        final String[] replacement = new String[unusedRules + 1];
        Arrays.fill(search, "z");
        Arrays.fill(replacement, StringUtils.repeat('x', 1_000_001));
        search[0] = "a";
        replacement[0] = "b";
        assertAll(
                () -> assertEquals("b", StringUtils.replaceEach("a", search, replacement)),
                () -> assertEquals("b", StringUtils.replaceEachRepeatedly("a", search, replacement)));
    }

    @Test
    void testUnwrapLengthBoundaries() {
        assertEquals("ababa", StringUtils.unwrap("ababa", "aba"));
        assertEquals("", StringUtils.unwrap("abaaba", "aba"));
        assertEquals("x", StringUtils.unwrap("abaxaba", "aba"));
        assertEquals("aba", StringUtils.unwrap("aba", "aba"));
    }

    /**
     * Allocates a token whose doubled length overflows an int; see the class documentation for the required heap setting.
     */
    @Test
    @EnabledIfSystemProperty(named = "test.stringutils.large.heap", matches = "true")
    void testUnwrapLengthOverflow() {
        final String token = StringUtils.repeat('x', 1 << 30);
        assertSame(token, StringUtils.unwrap(token, token));
    }

    /**
     * Exercises both missing sides around the overflow boundary of the former estimate, {@code 3 * wrapperLength + 1}.
     */
    @ParameterizedTest
    @CsvSource({
        "715827881, false",
        "715827881, true",
        "715827882, false",
        "715827882, true",
        "715827883, false",
        "715827883, true"
    })
    @EnabledIfSystemProperty(named = "test.stringutils.large.heap", matches = "true")
    void testWrapIfMissingCapacityBoundaries(final int wrapperLength, final boolean missingStart) {
        final String wrapper = new String(new byte[wrapperLength], StandardCharsets.ISO_8859_1);
        final String input = missingStart ? "x" + wrapper : wrapper + "x";
        final String result = StringUtils.wrapIfMissing(input, wrapper);
        // Check the complete result without allocating another multi-gigabyte expected string.
        assertEquals(2L * wrapperLength + 1, result.length());
        assertTrue(result.startsWith(wrapper));
        assertEquals('x', result.charAt(wrapperLength));
        assertTrue(result.endsWith(wrapper));
    }
}
