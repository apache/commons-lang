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
import org.junit.jupiter.params.provider.CsvSource;
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

    @ParameterizedTest
    @CsvSource({
        // Shrinking, unchanged and growing replacements; zero, finite and unbounded replacement counts.
        "100, 10, 9,  -1, 100",
        "100, 10, 10, -1, 100",
        "100, 10, 11,  0, 100",
        "100, 10, 11,  1, 101",
        "100, 10, 11,  2, 102",
        "100, 10, 11, -1, 110",
        "100, 100, 101, 64, 101",
        // The historical match estimates and their boundaries.
        "1000, 1, 2, -2147483648, 1016",
        "1000, 1, 2, -1, 1016",
        "1000, 1, 2, 63, 1063",
        "1000, 1, 2, 64, 1064",
        "1000, 1, 2, 65, 1064",
        "1000, 1, 2, 2147483647, 1064",
        // Just below, at and above the 20% growth cap, including its integer-rounding boundary.
        "100, 1, 2, 19, 119",
        "100, 1, 2, 20, 120",
        "100, 1, 2, 21, 120",
        "4, 1, 2147483647, 64, 4",
        "5, 1, 2147483647, 64, 6",
        "6, 1, 2147483647, 64, 7",
        // Growth multiplication and total capacity must not overflow or force a maximum-size allocation.
        "100, 1, 2147483647, 64, 120",
        "1789569699, 1, 2147483647, 64, 2147483638",
        "1789569700, 1, 2147483647, 64, 0",
        "2147483639, 1, 1, 1, 2147483639",
        "2147483640, 1, 1, 1, 0",
        "2147483647, 1, 2147483647, 64, 0"
    })
    void testInitialCapacity(final int textLength, final int searchLength, final int replacementLength, final int max, final int expected) {
        assertEquals(expected, Strings.initialCapacity(textLength, searchLength, replacementLength, max));
    }
}
