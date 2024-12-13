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

import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests {@link Strings}.
 */
public class StringsTest {

    public static Stream<Strings> stringsFactory() {
        return Stream.of(Strings.CS, Strings.CI);
    }

    @Test
    public void testBuilder() {
        assertTrue(Strings.builder().setIgnoreCase(false).get().isCaseSensitive());
        assertFalse(Strings.builder().setIgnoreCase(true).get().isCaseSensitive());
        //
        assertTrue(Strings.builder().setNullIsLess(false).get().isCaseSensitive());
        assertTrue(Strings.builder().setNullIsLess(true).get().isCaseSensitive());
    }

    @Test
    public void testBuilderDefaults() {
        final Strings strings = Strings.builder().get();
        assertTrue(strings.isCaseSensitive());
    }

    @Test
    public void testCaseInsensitiveConstant() {
        assertNotNull(Strings.CI);
        assertFalse(Strings.CI.isCaseSensitive());
    }

    /**
     * Expanding the existing test group {@link StringUtilsStartsEndsWithTest#testStartsWithAny()} to include case-insensitive cases
     */
    @Test
    public void testCaseInsensitiveStartsWithAny() {
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
    public void testCaseSensitiveConstant() {
        assertNotNull(Strings.CS);
        assertTrue(Strings.CS.isCaseSensitive());
    }

    @ParameterizedTest
    @MethodSource("stringsFactory")
    public void testEqualsCharSequence(final Strings strings) {
        final CharSequence nullCharSequence = null;
        assertTrue(strings.equals(nullCharSequence, nullCharSequence));
        assertFalse(strings.equals(nullCharSequence, ""));
        assertFalse(strings.equals("", nullCharSequence));
    }

    @ParameterizedTest
    @MethodSource("stringsFactory")
    public void testEqualsStrings(final Strings strings) {
        final String nullStr = null;
        assertTrue(strings.equals(nullStr, nullStr));
        assertFalse(strings.equals(nullStr, ""));
        assertFalse(strings.equals("", nullStr));
    }

    @Test
    public void testTruncateWithEllipsis() {
        assertEquals("Hello...", Strings.truncateWithEllipsis("Hello, World!", 8));
        assertEquals("Hello", Strings.truncateWithEllipsis("Hello", 10));
        assertEquals("Hello", Strings.truncateWithEllipsis("Hello", 5));
        assertNull(Strings.truncateWithEllipsis(null, 5));
        assertEquals("Hello", Strings.truncateWithEllipsis("Hello", -1));
    }

    @Test
    public void testReverseWords() {
        assertEquals("World Hello", Strings.reverseWords("Hello World"));
        assertEquals("spaces multiple with String", Strings.reverseWords("String with multiple spaces"));
        assertEquals("Hello", Strings.reverseWords("Hello"));
        assertNull(Strings.reverseWords(null));
        assertEquals("", Strings.reverseWords(""));
    }

    @Test
    public void testContainsOnly() {
        assertTrue(Strings.containsOnly("abc", "abc"));
        assertFalse(Strings.containsOnly("abcd", "abc"));
        assertTrue(Strings.containsOnly("", "abc"));
        assertFalse(Strings.containsOnly(null, "abc"));
        assertFalse(Strings.containsOnly("abc", null));
    }

    @Test
    public void testMaskString() {
        assertEquals("He**o", Strings.maskString("Hello", 2, 4, '*'));
        assertEquals("****", Strings.maskString("abcd", 0, 4, '*'));
        assertEquals("Hello", Strings.maskString("Hello", 5, 3, '*'));
        assertNull(Strings.maskString(null, 1, 3, '*'));
        assertEquals("Hello", Strings.maskString("Hello", -1, 10, '*'));
    }

    @Test
    public void testRemoveAccents() {
        assertEquals("Cafe", Strings.removeAccents("Café"));
        assertEquals("Hello", Strings.removeAccents("Hello"));
        assertEquals("Cafe Ole", Strings.removeAccents("Café Olé"));
        assertNull(Strings.removeAccents(null));
        assertEquals("", Strings.removeAccents(""));
    }
}
