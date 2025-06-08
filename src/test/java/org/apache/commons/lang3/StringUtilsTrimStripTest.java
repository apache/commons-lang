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

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link StringUtils} Trim/Strip methods.
 */
class StringUtilsTrimStripTest extends AbstractLangTest {
    private static final String FOO = "foo";

    @Test
    void testStripAccents() {
        final String cue = "\u00C7\u00FA\u00EA";
        assertEquals("Cue", StringUtils.stripAccents(cue), "Failed to strip accents from " + cue);

        final String lots = "\u00C0\u00C1\u00C2\u00C3\u00C4\u00C5\u00C7\u00C8\u00C9\u00CA\u00CB\u00CC\u00CD\u00CE\u00CF\u00D1\u00D2\u00D3"
                + "\u00D4\u00D5\u00D6\u00D9\u00DA\u00DB\u00DC\u00DD";
        assertEquals("AAAAAACEEEEIIIINOOOOOUUUUY", StringUtils.stripAccents(lots), "Failed to strip accents from " + lots);

        assertNull(StringUtils.stripAccents(null), "Failed null safety");
        assertEquals("", StringUtils.stripAccents(""), "Failed empty String");
        assertEquals("control", StringUtils.stripAccents("control"), "Failed to handle non-accented text");
        assertEquals("eclair", StringUtils.stripAccents("\u00E9clair"), "Failed to handle easy example");
        assertEquals("ALOSZZCND aloszzcnd",
                StringUtils.stripAccents("\u0104\u0141\u00D3\u015A\u017B\u0179\u0106\u0143\u0110 \u0105\u0142\u00F3\u015B\u017C\u017A\u0107\u0144\u0111"));
        assertEquals("The cafe\u2019s pinata gave me deja vu.", StringUtils.stripAccents("The caf\u00e9\u2019s pi\u00f1ata gave me d\u00e9j\u00e0 vu."),
                "Failed to handle accented text");
        assertEquals("fluid quest", StringUtils.stripAccents("\ufb02uid que\ufb06"), "Failed to handle ligatures");
        assertEquals("a b c 1 2 3", StringUtils.stripAccents("\u1d43 \u1d47 \u1d9c \u00b9 \u00b2 \u00b3"), "Failed to handle superscript text");
        assertEquals("math italic",
                StringUtils.stripAccents("\uD835\uDC5A\uD835\uDC4E\uD835\uDC61\u210E \uD835\uDC56\uD835\uDC61\uD835\uDC4E\uD835\uDC59\uD835\uDC56\uD835\uDC50"),
                "Failed to handle UTF32 example");
        assertEquals("\uD83D\uDF01 \uD83D\uDF02 \uD83D\uDF03 \uD83D\uDF04", StringUtils.stripAccents("\uD83D\uDF01 \uD83D\uDF02 \uD83D\uDF03 \uD83D\uDF04"),
                "Failed to handle non-accented text");
    }

    @Test
    void testStripAccentsIWithBar() {
        assertEquals("I i I i I", StringUtils.stripAccents("\u0197 \u0268 \u1D7B \u1DA4 \u1DA7"));
    }

    @Test
    @Disabled
    void testStripAccentsKorean() {
        // LANG-1655
        final String input = "\uC78A\uC9C0\uB9C8 \uB10C \uD750\uB9B0 \uC5B4\uB460\uC0AC\uC774 \uC67C\uC190\uC73C\uB85C \uADF8\uB9B0 \uBCC4 \uD558\uB098";
        assertEquals(input, StringUtils.stripAccents(input), "Failed to handle Korean text");
    }

    @Test
    void testStripAccentsTWithStroke() {
        assertEquals("T t", StringUtils.stripAccents("\u0166 \u0167"));
    }

    /**
     * Tests Unicode vulgar fractions.
     */
    @Test
    void testStripAccentsUnicodeVulgarFractions() {
        // 1/4, note "⁄", not "/".
        assertEquals("1⁄4", StringUtils.stripAccents("\u00BC"));
        // 1/2, note "⁄", not "/".
        assertEquals("1⁄2", StringUtils.stripAccents("\u00BD"));
        // 3/4, note "⁄", not "/".
        assertEquals("3⁄4", StringUtils.stripAccents("\u00BE"));
    }

    @Test
    void testStripAccentsUWithBar() {
        assertEquals("U u U u", StringUtils.stripAccents("\u0244 \u0289 \u1D7E \u1DB6"));
    }

    @Test
    void testStripAll() {
        // test stripAll method, merely an array version of the above strip
        final String[] empty = {};
        final String[] fooSpace = { "  " + FOO + "  ", "  " + FOO, FOO + "  " };
        final String[] fooDots = { ".." + FOO + "..", ".." + FOO, FOO + ".." };
        final String[] foo = { FOO, FOO, FOO };

        assertNull(StringUtils.stripAll((String[]) null));
        // Additional varargs tests
        assertArrayEquals(empty, StringUtils.stripAll()); // empty array
        assertArrayEquals(new String[] { null }, StringUtils.stripAll((String) null)); // == new String[]{null}

        assertArrayEquals(empty, StringUtils.stripAll(empty));
        assertArrayEquals(foo, StringUtils.stripAll(fooSpace));

        assertNull(StringUtils.stripAll(null, null));
        assertArrayEquals(foo, StringUtils.stripAll(fooSpace, null));
        assertArrayEquals(foo, StringUtils.stripAll(fooDots, "."));
    }

    @Test
    void testStripEndStringString() {
        // null stripEnd
        assertNull(StringUtils.stripEnd(null, null));
        assertEquals("", StringUtils.stripEnd("", null));
        assertEquals("", StringUtils.stripEnd("        ", null));
        assertEquals("  abc", StringUtils.stripEnd("  abc  ", null));
        assertEquals(StringUtilsTest.WHITESPACE + StringUtilsTest.NON_WHITESPACE,
                StringUtils.stripEnd(StringUtilsTest.WHITESPACE + StringUtilsTest.NON_WHITESPACE + StringUtilsTest.WHITESPACE, null));

        // "" stripEnd
        assertNull(StringUtils.stripEnd(null, ""));
        assertEquals("", StringUtils.stripEnd("", ""));
        assertEquals("        ", StringUtils.stripEnd("        ", ""));
        assertEquals("  abc  ", StringUtils.stripEnd("  abc  ", ""));
        assertEquals(StringUtilsTest.WHITESPACE, StringUtils.stripEnd(StringUtilsTest.WHITESPACE, ""));

        // " " stripEnd
        assertNull(StringUtils.stripEnd(null, " "));
        assertEquals("", StringUtils.stripEnd("", " "));
        assertEquals("", StringUtils.stripEnd("        ", " "));
        assertEquals("  abc", StringUtils.stripEnd("  abc  ", " "));

        // "ab" stripEnd
        assertNull(StringUtils.stripEnd(null, "ab"));
        assertEquals("", StringUtils.stripEnd("", "ab"));
        assertEquals("        ", StringUtils.stripEnd("        ", "ab"));
        assertEquals("  abc  ", StringUtils.stripEnd("  abc  ", "ab"));
        assertEquals("abc", StringUtils.stripEnd("abcabab", "ab"));
        assertEquals(StringUtilsTest.WHITESPACE, StringUtils.stripEnd(StringUtilsTest.WHITESPACE, ""));
    }

    @Test
    void testStripStartStringString() {
        // null stripStart
        assertNull(StringUtils.stripStart(null, null));
        assertEquals("", StringUtils.stripStart("", null));
        assertEquals("", StringUtils.stripStart("        ", null));
        assertEquals("abc  ", StringUtils.stripStart("  abc  ", null));
        assertEquals(StringUtilsTest.NON_WHITESPACE + StringUtilsTest.WHITESPACE,
                StringUtils.stripStart(StringUtilsTest.WHITESPACE + StringUtilsTest.NON_WHITESPACE + StringUtilsTest.WHITESPACE, null));

        // "" stripStart
        assertNull(StringUtils.stripStart(null, ""));
        assertEquals("", StringUtils.stripStart("", ""));
        assertEquals("        ", StringUtils.stripStart("        ", ""));
        assertEquals("  abc  ", StringUtils.stripStart("  abc  ", ""));
        assertEquals(StringUtilsTest.WHITESPACE, StringUtils.stripStart(StringUtilsTest.WHITESPACE, ""));

        // " " stripStart
        assertNull(StringUtils.stripStart(null, " "));
        assertEquals("", StringUtils.stripStart("", " "));
        assertEquals("", StringUtils.stripStart("        ", " "));
        assertEquals("abc  ", StringUtils.stripStart("  abc  ", " "));

        // "ab" stripStart
        assertNull(StringUtils.stripStart(null, "ab"));
        assertEquals("", StringUtils.stripStart("", "ab"));
        assertEquals("        ", StringUtils.stripStart("        ", "ab"));
        assertEquals("  abc  ", StringUtils.stripStart("  abc  ", "ab"));
        assertEquals("cabab", StringUtils.stripStart("abcabab", "ab"));
        assertEquals(StringUtilsTest.WHITESPACE, StringUtils.stripStart(StringUtilsTest.WHITESPACE, ""));
    }

    @Test
    void testStripString() {
        assertNull(StringUtils.strip(null));
        assertEquals("", StringUtils.strip(""));
        assertEquals("", StringUtils.strip("        "));
        assertEquals("abc", StringUtils.strip("  abc  "));
        assertEquals(StringUtilsTest.NON_WHITESPACE,
                StringUtils.strip(StringUtilsTest.WHITESPACE + StringUtilsTest.NON_WHITESPACE + StringUtilsTest.WHITESPACE));
    }

    @Test
    void testStripStringString() {
        // null strip
        assertNull(StringUtils.strip(null, null));
        assertEquals("", StringUtils.strip("", null));
        assertEquals("", StringUtils.strip("        ", null));
        assertEquals("abc", StringUtils.strip("  abc  ", null));
        assertEquals(StringUtilsTest.NON_WHITESPACE,
                StringUtils.strip(StringUtilsTest.WHITESPACE + StringUtilsTest.NON_WHITESPACE + StringUtilsTest.WHITESPACE, null));

        // "" strip
        assertNull(StringUtils.strip(null, ""));
        assertEquals("", StringUtils.strip("", ""));
        assertEquals("        ", StringUtils.strip("        ", ""));
        assertEquals("  abc  ", StringUtils.strip("  abc  ", ""));
        assertEquals(StringUtilsTest.WHITESPACE, StringUtils.strip(StringUtilsTest.WHITESPACE, ""));

        // " " strip
        assertNull(StringUtils.strip(null, " "));
        assertEquals("", StringUtils.strip("", " "));
        assertEquals("", StringUtils.strip("        ", " "));
        assertEquals("abc", StringUtils.strip("  abc  ", " "));

        // "ab" strip
        assertNull(StringUtils.strip(null, "ab"));
        assertEquals("", StringUtils.strip("", "ab"));
        assertEquals("        ", StringUtils.strip("        ", "ab"));
        assertEquals("  abc  ", StringUtils.strip("  abc  ", "ab"));
        assertEquals("c", StringUtils.strip("abcabab", "ab"));
        assertEquals(StringUtilsTest.WHITESPACE, StringUtils.strip(StringUtilsTest.WHITESPACE, ""));
    }

    @Test
    void testStripToEmptyString() {
        assertEquals("", StringUtils.stripToEmpty(null));
        assertEquals("", StringUtils.stripToEmpty(""));
        assertEquals("", StringUtils.stripToEmpty("        "));
        assertEquals("", StringUtils.stripToEmpty(StringUtilsTest.WHITESPACE));
        assertEquals("ab c", StringUtils.stripToEmpty("  ab c  "));
        assertEquals(StringUtilsTest.NON_WHITESPACE,
                StringUtils.stripToEmpty(StringUtilsTest.WHITESPACE + StringUtilsTest.NON_WHITESPACE + StringUtilsTest.WHITESPACE));
    }

    @Test
    void testStripToNullString() {
        assertNull(StringUtils.stripToNull(null));
        assertNull(StringUtils.stripToNull(""));
        assertNull(StringUtils.stripToNull("        "));
        assertNull(StringUtils.stripToNull(StringUtilsTest.WHITESPACE));
        assertEquals("ab c", StringUtils.stripToNull("  ab c  "));
        assertEquals(StringUtilsTest.NON_WHITESPACE,
                StringUtils.stripToNull(StringUtilsTest.WHITESPACE + StringUtilsTest.NON_WHITESPACE + StringUtilsTest.WHITESPACE));
    }

    @Test
    void testTrim() {
        assertEquals(FOO, StringUtils.trim(FOO + "  "));
        assertEquals(FOO, StringUtils.trim(" " + FOO + "  "));
        assertEquals(FOO, StringUtils.trim(" " + FOO));
        assertEquals(FOO, StringUtils.trim(FOO + ""));
        assertEquals("", StringUtils.trim(" \t\r\n\b "));
        assertEquals("", StringUtils.trim(StringUtilsTest.TRIMMABLE));
        assertEquals(StringUtilsTest.NON_TRIMMABLE, StringUtils.trim(StringUtilsTest.NON_TRIMMABLE));
        assertEquals("", StringUtils.trim(""));
        assertNull(StringUtils.trim(null));
    }

    @Test
    void testTrimToEmpty() {
        assertEquals(FOO, StringUtils.trimToEmpty(FOO + "  "));
        assertEquals(FOO, StringUtils.trimToEmpty(" " + FOO + "  "));
        assertEquals(FOO, StringUtils.trimToEmpty(" " + FOO));
        assertEquals(FOO, StringUtils.trimToEmpty(FOO + ""));
        assertEquals("", StringUtils.trimToEmpty(" \t\r\n\b "));
        assertEquals("", StringUtils.trimToEmpty(StringUtilsTest.TRIMMABLE));
        assertEquals(StringUtilsTest.NON_TRIMMABLE, StringUtils.trimToEmpty(StringUtilsTest.NON_TRIMMABLE));
        assertEquals("", StringUtils.trimToEmpty(""));
        assertEquals("", StringUtils.trimToEmpty(null));
    }

    @Test
    void testTrimToNull() {
        assertEquals(FOO, StringUtils.trimToNull(FOO + "  "));
        assertEquals(FOO, StringUtils.trimToNull(" " + FOO + "  "));
        assertEquals(FOO, StringUtils.trimToNull(" " + FOO));
        assertEquals(FOO, StringUtils.trimToNull(FOO + ""));
        assertNull(StringUtils.trimToNull(" \t\r\n\b "));
        assertNull(StringUtils.trimToNull(StringUtilsTest.TRIMMABLE));
        assertEquals(StringUtilsTest.NON_TRIMMABLE, StringUtils.trimToNull(StringUtilsTest.NON_TRIMMABLE));
        assertNull(StringUtils.trimToNull(""));
        assertNull(StringUtils.trimToNull(null));
    }
}
