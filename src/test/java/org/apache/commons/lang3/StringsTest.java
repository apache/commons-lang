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

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Tests {@link Strings}.
 */
class StringsTest {

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
}
