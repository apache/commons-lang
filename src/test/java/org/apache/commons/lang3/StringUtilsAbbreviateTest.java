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
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

/**
 * Tests {@link StringUtils#abbreviate(String, int)} and friends.
 */
class StringUtilsAbbreviateTest {

    private void assertAbbreviateWithAbbrevMarkerAndOffset(final String expected, final String abbrevMarker, final int offset, final int maxWidth) {
        final String abcdefghijklmno = "abcdefghijklmno";
        final String message = "abbreviate(String,String,int,int) failed";
        final String actual = StringUtils.abbreviate(abcdefghijklmno, abbrevMarker, offset, maxWidth);
        if (offset >= 0 && offset < abcdefghijklmno.length()) {
            assertTrue(actual.indexOf((char) ('a' + offset)) != -1, message + " -- should contain offset character");
        }
        assertTrue(actual.length() <= maxWidth, () -> message + " -- should not be greater than maxWidth");
        assertEquals(expected, actual, message);
    }

    private void assertAbbreviateWithOffset(final String expected, final int offset, final int maxWidth) {
        final String abcdefghijklmno = "abcdefghijklmno";
        final String message = "abbreviate(String,int,int) failed";
        final String actual = StringUtils.abbreviate(abcdefghijklmno, offset, maxWidth);
        if (offset >= 0 && offset < abcdefghijklmno.length()) {
            assertTrue(actual.indexOf((char) ('a' + offset)) != -1, message + " -- should contain offset character");
        }
        assertTrue(actual.length() <= maxWidth, () -> message + " -- should not be greater than maxWidth");
        assertEquals(expected, actual, message);
    }

    @Test
    void testAbbreviate_StringInt() {
        assertNull(StringUtils.abbreviate(null, 10));
        assertEquals("", StringUtils.abbreviate("", 10));
        assertEquals("short", StringUtils.abbreviate("short", 10));
        assertEquals("Now is ...", StringUtils.abbreviate("Now is the time for all good men to come to the aid of their party.", 10));
        final String raspberry = "raspberry peach";
        assertEquals("raspberry p...", StringUtils.abbreviate(raspberry, 14));
        assertEquals("raspberry peach", StringUtils.abbreviate("raspberry peach", 15));
        assertEquals("raspberry peach", StringUtils.abbreviate("raspberry peach", 16));
        assertEquals("abc...", StringUtils.abbreviate("abcdefg", 6));
        assertEquals("abcdefg", StringUtils.abbreviate("abcdefg", 7));
        assertEquals("abcdefg", StringUtils.abbreviate("abcdefg", 8));
        assertEquals("a...", StringUtils.abbreviate("abcdefg", 4));
        assertEquals("", StringUtils.abbreviate("", 4));
        assertThrows(IllegalArgumentException.class, () -> StringUtils.abbreviate("abc", 3), "StringUtils.abbreviate expecting IllegalArgumentException");
    }

    @Test
    void testAbbreviate_StringIntInt() {
        assertNull(StringUtils.abbreviate(null, 10, 12));
        assertEquals("", StringUtils.abbreviate("", 0, 10));
        assertEquals("", StringUtils.abbreviate("", 2, 10));
        assertThrows(IllegalArgumentException.class, () -> StringUtils.abbreviate("abcdefghij", 0, 3),
                "StringUtils.abbreviate expecting IllegalArgumentException");
        assertThrows(IllegalArgumentException.class, () -> StringUtils.abbreviate("abcdefghij", 5, 6),
                "StringUtils.abbreviate expecting IllegalArgumentException");
        final String raspberry = "raspberry peach";
        assertEquals("raspberry peach", StringUtils.abbreviate(raspberry, 11, 15));
        assertNull(StringUtils.abbreviate(null, 7, 14));
        assertAbbreviateWithOffset("abcdefg...", -1, 10);
        assertAbbreviateWithOffset("abcdefg...", 0, 10);
        assertAbbreviateWithOffset("abcdefg...", 1, 10);
        assertAbbreviateWithOffset("abcdefg...", 2, 10);
        assertAbbreviateWithOffset("abcdefg...", 3, 10);
        assertAbbreviateWithOffset("abcdefg...", 4, 10);
        assertAbbreviateWithOffset("...fghi...", 5, 10);
        assertAbbreviateWithOffset("...ghij...", 6, 10);
        assertAbbreviateWithOffset("...hijk...", 7, 10);
        assertAbbreviateWithOffset("...ijklmno", 8, 10);
        assertAbbreviateWithOffset("...ijklmno", 9, 10);
        assertAbbreviateWithOffset("...ijklmno", 10, 10);
        assertAbbreviateWithOffset("...ijklmno", 11, 10);
        assertAbbreviateWithOffset("...ijklmno", 12, 10);
        assertAbbreviateWithOffset("...ijklmno", 13, 10);
        assertAbbreviateWithOffset("...ijklmno", 14, 10);
        assertAbbreviateWithOffset("...ijklmno", 15, 10);
        assertAbbreviateWithOffset("...ijklmno", 16, 10);
        assertAbbreviateWithOffset("...ijklmno", Integer.MAX_VALUE, 10);
    }

    @Test
    void testAbbreviate_StringStringInt() {
        assertNull(StringUtils.abbreviate(null, null, 10));
        assertNull(StringUtils.abbreviate(null, "...", 10));
        assertEquals("paranaguacu", StringUtils.abbreviate("paranaguacu", null, 10));
        assertEquals("", StringUtils.abbreviate("", "...", 2));
        assertEquals("wai**", StringUtils.abbreviate("waiheke", "**", 5));
        assertEquals("And af,,,,", StringUtils.abbreviate("And after a long time, he finally met his son.", ",,,,", 10));
        final String raspberry = "raspberry peach";
        assertEquals("raspberry pe..", StringUtils.abbreviate(raspberry, "..", 14));
        assertEquals("raspberry peach", StringUtils.abbreviate("raspberry peach", "---*---", 15));
        assertEquals("raspberry peach", StringUtils.abbreviate("raspberry peach", ".", 16));
        assertEquals("abc()(", StringUtils.abbreviate("abcdefg", "()(", 6));
        assertEquals("abcdefg", StringUtils.abbreviate("abcdefg", ";", 7));
        assertEquals("abcdefg", StringUtils.abbreviate("abcdefg", "_-", 8));
        assertEquals("abc.", StringUtils.abbreviate("abcdefg", ".", 4));
        assertEquals("", StringUtils.abbreviate("", 4));
        assertThrows(IllegalArgumentException.class, () -> StringUtils.abbreviate("abcdefghij", "...", 3),
                "StringUtils.abbreviate expecting IllegalArgumentException");
    }

    @Test
    void testAbbreviate_StringStringIntInt() {
        assertNull(StringUtils.abbreviate(null, null, 10, 12));
        assertNull(StringUtils.abbreviate(null, "...", 10, 12));
        assertEquals("", StringUtils.abbreviate("", null, 0, 10));
        assertEquals("", StringUtils.abbreviate("", "...", 2, 10));
        assertThrows(IllegalArgumentException.class, () -> StringUtils.abbreviate("abcdefghij", "::", 0, 2),
                "StringUtils.abbreviate expecting IllegalArgumentException");
        assertThrows(IllegalArgumentException.class, () -> StringUtils.abbreviate("abcdefghij", "!!!", 5, 6),
                "StringUtils.abbreviate expecting IllegalArgumentException");
        final String raspberry = "raspberry peach";
        assertEquals("raspberry peach", StringUtils.abbreviate(raspberry, "--", 12, 15));
        assertNull(StringUtils.abbreviate(null, ";", 7, 14));
        assertAbbreviateWithAbbrevMarkerAndOffset("abcdefgh;;", ";;", -1, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("abcdefghi.", ".", 0, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("abcdefgh++", "++", 1, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("abcdefghi*", "*", 2, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("abcdef{{{{", "{{{{", 4, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("abcdef____", "____", 5, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("==fghijk==", "==", 5, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("___ghij___", "___", 6, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("/ghijklmno", "/", 7, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("/ghijklmno", "/", 8, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("/ghijklmno", "/", 9, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("///ijklmno", "///", 10, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("//hijklmno", "//", 10, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("//hijklmno", "//", 11, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("...ijklmno", "...", 12, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("/ghijklmno", "/", 13, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("/ghijklmno", "/", 14, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("999ijklmno", "999", 15, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("_ghijklmno", "_", 16, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("+ghijklmno", "+", Integer.MAX_VALUE, 10);
    }

    // Fixed LANG-1463
    @Test
    void testAbbreviateMarkerWithEmptyString() {
        final String greaterThanMaxTest = "much too long text";
        assertEquals("much too long", StringUtils.abbreviate(greaterThanMaxTest, "", 13));
    }

    @Test
    void testAbbreviateMiddle() {
        // javadoc examples
        assertNull(StringUtils.abbreviateMiddle(null, null, 0));
        assertEquals("abc", StringUtils.abbreviateMiddle("abc", null, 0));
        assertEquals("abc", StringUtils.abbreviateMiddle("abc", ".", 0));
        assertEquals("abc", StringUtils.abbreviateMiddle("abc", ".", 3));
        assertEquals("ab.f", StringUtils.abbreviateMiddle("abcdef", ".", 4));
        // JIRA issue (LANG-405) example (slightly different than actual expected result)
        assertEquals("A very long text with un...f the text is complete.", StringUtils.abbreviateMiddle(
                "A very long text with unimportant stuff in the middle but interesting start and " + "end to see if the text is complete.", "...", 50));
        // Test a much longer text :)
        final String longText = "Start text" + StringUtils.repeat("x", 10000) + "Close text";
        assertEquals("Start text->Close text", StringUtils.abbreviateMiddle(longText, "->", 22));
        // Test negative length
        assertEquals("abc", StringUtils.abbreviateMiddle("abc", ".", -1));
        // Test boundaries
        // Fails to change anything as method ensures first and last char are kept
        assertEquals("abc", StringUtils.abbreviateMiddle("abc", ".", 1));
        assertEquals("abc", StringUtils.abbreviateMiddle("abc", ".", 2));
        // Test length of n=1
        assertEquals("a", StringUtils.abbreviateMiddle("a", ".", 1));
        // Test smallest length that can lead to success
        assertEquals("a.d", StringUtils.abbreviateMiddle("abcd", ".", 3));
        // More from LANG-405
        assertEquals("a..f", StringUtils.abbreviateMiddle("abcdef", "..", 4));
        assertEquals("ab.ef", StringUtils.abbreviateMiddle("abcdef", ".", 5));
    }

    /**
     * Tests <a href="LANG-1770">https://issues.apache.org/jira/projects/LANG/issues/LANG-1770</a>.
     */
    @Test
    void testEmoji() {
        // @formatter:off
        final String[] expectedResultsFox = {
            "🦊...", // 4
            "🦊🦊...",
            "🦊🦊🦊...",
            "🦊🦊🦊🦊...",
            "🦊🦊🦊🦊🦊...",
            "🦊🦊🦊🦊🦊🦊...",
            "🦊🦊🦊🦊🦊🦊🦊...", // 10
        };
        final String[] expectedResultsFamilyWithCodepoints = {
            "👩...",
            "👩🏻...",
            "👩🏻‍...", // zero width joiner
            "👩🏻‍👨...",
            "👩🏻‍👨🏻...",
            "👩🏻‍👨🏻‍...",
            "👩🏻‍👨🏻‍👦..."
        };
        final String[] expectedResultsFamilyWithGrapheme = {
            "👩🏻‍👨🏻‍👦🏻‍👦🏻...", // 4
            "👩🏻‍👨🏻‍👦🏻‍👦🏻👩🏼‍👨🏼‍👦🏼‍👦🏼...",
            "👩🏻‍👨🏻‍👦🏻‍👦🏻👩🏼‍👨🏼‍👦🏼‍👦🏼👩🏽‍👨🏽‍👦🏽‍👦🏽...",
            "👩🏻‍👨🏻‍👦🏻‍👦🏻👩🏼‍👨🏼‍👦🏼‍👦🏼👩🏽‍👨🏽‍👦🏽‍👦🏽👩🏾‍👨🏾‍👦🏾‍👦🏾...",
            "👩🏻‍👨🏻‍👦🏻‍👦🏻👩🏼‍👨🏼‍👦🏼‍👦🏼👩🏽‍👨🏽‍👦🏽‍👦🏽👩🏾‍👨🏾‍👦🏾‍👦🏾👩🏿‍👨🏿‍👦🏿‍👦🏿...",
            "👩🏻‍👨🏻‍👦🏻‍👦🏻👩🏼‍👨🏼‍👦🏼‍👦🏼👩🏽‍👨🏽‍👦🏽‍👦🏽👩🏾‍👨🏾‍👦🏾‍👦🏾👩🏿‍👨🏿‍👦🏿‍👦🏿👩🏻‍👨🏻‍👦🏻‍👦🏻...",
            "👩🏻‍👨🏻‍👦🏻‍👦🏻👩🏼‍👨🏼‍👦🏼‍👦🏼👩🏽‍👨🏽‍👦🏽‍👦🏽👩🏾‍👨🏾‍👦🏾‍👦🏾👩🏿‍👨🏿‍👦🏿‍👦🏿👩🏻‍👨🏻‍👦🏻‍👦🏻👩🏼‍👨🏼‍👦🏼‍👦🏼..." // 10
        };
        // @formatter:on
        for (int i = 4; i <= 10; i++) {
            final String abbreviateResult = StringUtils.abbreviate("🦊🦊🦊🦊🦊🦊🦊🦊🦊🦊🦊🦊🦊🦊", i);
            assertNotNull(abbreviateResult);
            // assertEquals(expectedResultsFox[i - 4], abbreviateResult);
        }
        for (int i = 4; i <= 10; i++) {
            final String abbreviateResult = StringUtils.abbreviate(
                    "👩🏻‍👨🏻‍👦🏻‍👦🏻👩🏼‍👨🏼‍👦🏼‍👦🏼👩🏽‍👨🏽‍👦🏽‍👦🏽👩🏾‍👨🏾‍👦🏾‍👦🏾👩🏿‍👨🏿‍👦🏿‍👦🏿👩🏻‍👨🏻‍👦🏻‍👦🏻👩🏼‍👨🏼‍👦🏼‍👦🏼👩🏽‍👨🏽‍👦🏽‍👦🏽👩🏾‍👨🏾‍👦🏾‍👦🏾👩🏿‍👨🏿‍👦🏿‍👦🏿",
                    i);
            assertNotNull(abbreviateResult);
            // assertEquals(expectedResultsFamilyWithCodepoints[i - 4], abbreviateResult);
        }
    }
}
