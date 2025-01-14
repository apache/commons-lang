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

import org.junit.jupiter.api.Test;
import org.junitpioneer.jupiter.DefaultLocale;

import java.util.Locale;

import static org.apache.commons.lang3.Supplementary.*;
import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests {@link Strings} - Contains methods
 */
public class StringsContainsTest extends AbstractLangTest {
    @Test
    public void testContains_Char() {
        assertFalse(Strings.CS.contains("", null));
        assertFalse(Strings.CS.contains(null, null));
    }

    @Test
    public void testContains_String() {
        assertFalse(Strings.CS.contains(null, null));
        assertFalse(Strings.CS.contains(null, ""));
        assertFalse(Strings.CS.contains(null, "a"));
        assertFalse(Strings.CS.contains("", null));
        assertTrue(Strings.CS.contains("", ""));
        assertFalse(Strings.CS.contains("", "a"));
        assertTrue(Strings.CS.contains("abc", "a"));
        assertTrue(Strings.CS.contains("abc", "b"));
        assertTrue(Strings.CS.contains("abc", "c"));
        assertTrue(Strings.CS.contains("abc", "abc"));
        assertFalse(Strings.CS.contains("abc", "z"));
    }

    /**
     * See https://www.oracle.com/technical-resources/articles/javase/supplementary.html
     */
    @Test
    public void testContains_StringWithBadSupplementaryChars() {
        // Test edge case: 1/2 of a (broken) supplementary char
        assertFalse(Strings.CS.contains(CharUSuppCharHigh, CharU20001));
        assertFalse(Strings.CS.contains(CharUSuppCharLow, CharU20001));
        assertFalse(Strings.CS.contains(CharU20001, CharUSuppCharHigh));
        assertTrue(Strings.CS.contains(CharU20001, CharUSuppCharLow));
        assertTrue(Strings.CS.contains(CharU20001 + CharUSuppCharLow + "a", "a"));
        assertTrue(Strings.CS.contains(CharU20001 + CharUSuppCharHigh + "a", "a"));
    }

    /**
     * See https://www.oracle.com/technical-resources/articles/javase/supplementary.html
     */
    @Test
    public void testContains_StringWithSupplementaryChars() {
        assertTrue(Strings.CS.contains(CharU20000 + CharU20001, CharU20000));
        assertTrue(Strings.CS.contains(CharU20000 + CharU20001, CharU20001));
        assertTrue(Strings.CS.contains(CharU20000, CharU20000));
        assertFalse(Strings.CS.contains(CharU20000, CharU20001));
    }

    @Test
    public void testContainsAny_StringStringArray() {
        assertFalse(Strings.CS.containsAny(null, (String[]) null));
        assertFalse(Strings.CS.containsAny(null, new String[0]));
        assertFalse(Strings.CS.containsAny(null, new String[] { "hello" }));
        assertFalse(Strings.CS.containsAny("", (String[]) null));
        assertFalse(Strings.CS.containsAny("", new String[0]));
        assertFalse(Strings.CS.containsAny("", new String[] { "hello" }));
        assertFalse(Strings.CS.containsAny("hello, goodbye", (String[]) null));
        assertFalse(Strings.CS.containsAny("hello, goodbye", new String[0]));
        assertTrue(Strings.CS.containsAny("hello, goodbye", new String[] { "hello", "goodbye" }));
        assertTrue(Strings.CS.containsAny("hello, goodbye", new String[] { "hello", "Goodbye" }));
        assertFalse(Strings.CS.containsAny("hello, goodbye", new String[] { "Hello", "Goodbye" }));
        assertFalse(Strings.CS.containsAny("hello, goodbye", new String[] { "Hello", null }));
        assertFalse(Strings.CS.containsAny("hello, null", new String[] { "Hello", null }));
        // Javadoc examples:
        assertTrue(Strings.CS.containsAny("abcd", "ab", null));
        assertTrue(Strings.CS.containsAny("abcd", "ab", "cd"));
        assertTrue(Strings.CS.containsAny("abc", "d", "abc"));
    }

    @Test
    public void testContainsAnyIgnoreCase_StringStringArray() {
        assertFalse(Strings.CI.containsAny(null, (String[]) null));
        assertFalse(Strings.CI.containsAny(null, new String[0]));
        assertFalse(Strings.CI.containsAny(null, new String[] { "hello" }));
        assertFalse(Strings.CI.containsAny("", (String[]) null));
        assertFalse(Strings.CI.containsAny("", new String[0]));
        assertFalse(Strings.CI.containsAny("", new String[] { "hello" }));
        assertFalse(Strings.CI.containsAny("hello, goodbye", (String[]) null));
        assertFalse(Strings.CI.containsAny("hello, goodbye", new String[0]));
        assertTrue(Strings.CI.containsAny("hello, goodbye", new String[] { "hello", "goodbye" }));
        assertTrue(Strings.CI.containsAny("hello, goodbye", new String[] { "hello", "Goodbye" }));
        assertTrue(Strings.CI.containsAny("hello, goodbye", new String[] { "Hello", "Goodbye" }));
        assertTrue(Strings.CI.containsAny("hello, goodbye", new String[] { "Hello", null }));
        assertTrue(Strings.CI.containsAny("hello, null", new String[] { "Hello", null }));
        // Javadoc examples:
        assertTrue(Strings.CI.containsAny("abcd", "ab", null));
        assertTrue(Strings.CI.containsAny("abcd", "ab", "cd"));
        assertTrue(Strings.CI.containsAny("abc", "d", "abc"));
    }

    @DefaultLocale(language = "de", country = "DE")
    @Test
    public void testContainsIgnoreCase_LocaleIndependence() {
        final Locale[] locales = { Locale.ENGLISH, new Locale("tr"), Locale.getDefault() };

        final String[][] tdata = { { "i", "I" }, { "I", "i" }, { "\u03C2", "\u03C3" }, { "\u03A3", "\u03C2" }, { "\u03A3", "\u03C3" }, };

        final String[][] fdata = { { "\u00DF", "SS" }, };

        for (final Locale testLocale : locales) {
            Locale.setDefault(testLocale);
            for (int j = 0; j < tdata.length; j++) {
                assertTrue(Strings.CI.contains(tdata[j][0], tdata[j][1]), Locale.getDefault() + ": " + j + " " + tdata[j][0] + " " + tdata[j][1]);
            }
            for (int j = 0; j < fdata.length; j++) {
                assertFalse(Strings.CI.contains(fdata[j][0], fdata[j][1]), Locale.getDefault() + ": " + j + " " + fdata[j][0] + " " + fdata[j][1]);
            }
        }
    }

    @Test
    public void testContainsIgnoreCase_StringString() {
        assertFalse(Strings.CI.contains(null, null));

        // Null tests
        assertFalse(Strings.CI.contains(null, ""));
        assertFalse(Strings.CI.contains(null, "a"));
        assertFalse(Strings.CI.contains(null, "abc"));

        assertFalse(Strings.CI.contains("", null));
        assertFalse(Strings.CI.contains("a", null));
        assertFalse(Strings.CI.contains("abc", null));

        // Match len = 0
        assertTrue(Strings.CI.contains("", ""));
        assertTrue(Strings.CI.contains("a", ""));
        assertTrue(Strings.CI.contains("abc", ""));

        // Match len = 1
        assertFalse(Strings.CI.contains("", "a"));
        assertTrue(Strings.CI.contains("a", "a"));
        assertTrue(Strings.CI.contains("abc", "a"));
        assertFalse(Strings.CI.contains("", "A"));
        assertTrue(Strings.CI.contains("a", "A"));
        assertTrue(Strings.CI.contains("abc", "A"));

        // Match len > 1
        assertFalse(Strings.CI.contains("", "abc"));
        assertFalse(Strings.CI.contains("a", "abc"));
        assertTrue(Strings.CI.contains("xabcz", "abc"));
        assertFalse(Strings.CI.contains("", "ABC"));
        assertFalse(Strings.CI.contains("a", "ABC"));
        assertTrue(Strings.CI.contains("xabcz", "ABC"));
    }
}
