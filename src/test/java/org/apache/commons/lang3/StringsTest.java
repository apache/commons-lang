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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.junitpioneer.jupiter.DefaultLocale;

/**
 * Tests {@link Strings}.
 */
class StringsTest extends AbstractLangTest {

    public static Stream<Strings> stringsFactory() {
        return Stream.of(Strings.CS, Strings.CI);
    }

    @Test
    void testBuilder() {
        assertTrue(Strings.builder().setIgnoreCase(false).get().isCaseSensitive());
        assertFalse(Strings.builder().setIgnoreCase(true).get().isCaseSensitive());
        //
        assertTrue(Strings.builder().setNullIsLess(false).get().isCaseSensitive());
        assertTrue(Strings.builder().setNullIsLess(true).get().isCaseSensitive());
    }

    @Test
    void testBuilderDefaults() {
        final Strings strings = Strings.builder().get();
        assertTrue(strings.isCaseSensitive());
    }

    @Test
    void testCaseInsensitiveConstant() {
        assertNotNull(Strings.CI);
        assertFalse(Strings.CI.isCaseSensitive());
    }

    /**
     * For an empty search the case-insensitive {@code indexOf} returned {@code startPos} unchanged once it reached
     * {@code str.length() + 1}, so a start position one past the end yielded an index beyond the string instead of
     * {@code -1}.
     */
    @Test
    void testCaseInsensitiveIndexOfEmptyOutOfRange() {
        // repro: returned 4 (past the end of a length-3 string) before the fix
        final String emptySearch = StringUtils.EMPTY;
        assertEquals(-1, Strings.CI.indexOf("abc", emptySearch, 4));
        // documented out-of-range example, also -1
        assertEquals(-1, Strings.CI.indexOf("abc", emptySearch, 9));
        // the end position is still a valid empty match
        assertEquals(3, Strings.CI.indexOf("abc", emptySearch, 3));
        assertEquals(2, Strings.CI.indexOf("aabaabaa", emptySearch, 2));
        assertEquals(0, Strings.CI.indexOf(emptySearch, emptySearch, 0));
        assertEquals(0, Strings.CI.indexOf(emptySearch, emptySearch, -1));
        assertEquals(0, Strings.CI.indexOf("a", emptySearch, -1));
    }

    /**
     * {@code U+0130} lower-cases to the two-char sequence {@code "i̇"} outside Turkish locales, so pre-lower-casing the
     * search argument made the case-insensitive replace look for a two-char needle that no longer matches the single source
     * character.
     */
    @Test
    @DefaultLocale("en")
    void testCaseInsensitiveReplaceLengthChangingLowerCase() {
        assertEquals("aXb", Strings.CI.replace("aİb", "İ", "X", -1));
        assertEquals("x_y_z", Strings.CI.replace("xİyİz", "İ", "_", -1));
        assertEquals("X", Strings.CI.replaceOnce("İ", "İ", "X"));
    }

    /**
     * Expanding the existing test group {@link StringUtilsStartsEndsWithTest#testStartsWithAny()} to include case-insensitive cases
     */
    @Test
    void testCaseInsensitiveStartsWithAny() {
        // LANG-1682
        assertFalse(Strings.CI.startsWithAny(null, (String[]) null));
        assertFalse(Strings.CI.startsWithAny(null, "aBc"));
        assertFalse(Strings.CI.startsWithAny("AbCxYz", (String[]) null));
        assertFalse(Strings.CI.startsWithAny("AbCxYz"));
        assertTrue(Strings.CI.startsWithAny("AbCxYz", "aBc"));
        assertTrue(Strings.CI.startsWithAny("AbCxYz", null, "XyZ", "aBc"));
        assertFalse(Strings.CI.startsWithAny("AbCxYz", null, "XyZ", "aBcD"));
        assertTrue(Strings.CI.startsWithAny("AbCxYz", ""));
        assertTrue(Strings.CI.startsWithAny("abcxyz", null, "XyZ", "ABCX"));
        assertTrue(Strings.CI.startsWithAny("ABCXYZ", null, "XyZ", "abc"));

        assertTrue(Strings.CI.startsWithAny("AbCxYz", new StringBuilder("XyZ"), new StringBuffer("aBc")));
        assertTrue(Strings.CI.startsWithAny(new StringBuffer("AbCxYz"), new StringBuilder("XyZ"), new StringBuffer("abc")));
    }

    @Test
    void testCaseSensitiveConstant() {
        assertNotNull(Strings.CS);
        assertTrue(Strings.CS.isCaseSensitive());
    }

    @Test
    void testComputeInitialCapacityDoesNotReturnSafeMaxDueToOverflow() {
        final int textLength = 100;
        final int searchLength = 0;
        final int replacementLength = Integer.MAX_VALUE;
        final int max = 1;
        // Expected mathematical result:
        // 100 + (2147483647 - 0) * 1 = 2147483747
        // which exceeds SAFE_MAX_ARRAY_LENGTH (2147483639)
        final int expected = Integer.MAX_VALUE - 8;
        assertEquals(expected, Strings.initialCapacity(textLength, searchLength, replacementLength, max));
    }

    @Test
    void testComputeInitialCapacityLargeInputsDoNotIncorrectlyClampToSafeMax() {
        final int result = Strings.initialCapacity(Integer.MAX_VALUE - 1000, Integer.MAX_VALUE - 500, Integer.MAX_VALUE, 1);
        // Growth = 500
        // Expected = Integer.MAX_VALUE - 500
        assertEquals(Integer.MAX_VALUE - 500, result);
    }

    @Test
    void testComputeInitialCapacityNeverOverflowsForMaxValueInputs() {
        final int capacity = Strings.initialCapacity(Integer.MAX_VALUE, 0, Integer.MAX_VALUE, 64);
        assertEquals(ArrayUtils.SAFE_MAX_ARRAY_LENGTH, capacity);
    }

    @Test
    void testComputeInitialCapacityReplacementGrowthDoesNotOverflowInt() {
        final int capacity = Strings.initialCapacity(0, 0, 50_000_000, 64);
        assertEquals(ArrayUtils.SAFE_MAX_ARRAY_LENGTH, capacity);
    }

    @Test
    void testComputeInitialCapacityReturnsSmallerValueWhenResultIsBelowSafeMax() {
        final int textLength = 100;
        final int searchLength = 0;
        final int replacementLength = 1000;
        final int max = 1;
        final int expected = 1100;
        assertEquals(expected, Strings.initialCapacity(textLength, searchLength, replacementLength, max));
    }

    @ParameterizedTest
    @MethodSource("stringsFactory")
    void testEqualsCharSequence(final Strings strings) {
        final CharSequence nullCharSequence = null;
        assertTrue(strings.equals(nullCharSequence, nullCharSequence));
        assertFalse(strings.equals(nullCharSequence, ""));
        assertFalse(strings.equals("", nullCharSequence));
    }

    @ParameterizedTest
    @MethodSource("stringsFactory")
    void testEqualsStrings(final Strings strings) {
        final String nullStr = null;
        assertTrue(strings.equals(nullStr, nullStr));
        assertFalse(strings.equals(nullStr, ""));
        assertFalse(strings.equals("", nullStr));
    }
}
