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

package org.apache.commons.lang3.text.translate;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Tests for {@link org.apache.commons.lang3.text.translate.UnicodeEscaper}.
 */
@Deprecated
class UnicodeEscaperTest extends AbstractLangTest {

    @Test
    void testAbove() {
        final UnicodeEscaper ue = UnicodeEscaper.above('F');
        final String input = "ADFGZ";
        final String result = ue.translate(input);
        assertEquals("ADF\\u0047\\u005A", result, "Failed to escape Unicode characters via the above method");
    }

    @Test
    void testBelow() {
        final UnicodeEscaper ue = UnicodeEscaper.below('F');
        final String input = "ADFGZ";
        final String result = ue.translate(input);
        assertEquals("\\u0041\\u0044FGZ", result, "Failed to escape Unicode characters via the below method");
    }

    @Test
    void testBetween() {
        final UnicodeEscaper ue = UnicodeEscaper.between('F', 'L');
        final String input = "ADFGZ";
        final String result = ue.translate(input);
        assertEquals("AD\\u0046\\u0047Z", result, "Failed to escape Unicode characters via the between method");
    }

    @Test
    void testToUtf16EscapeBmp() {
        final UnicodeEscaper ue = new UnicodeEscaper();
        // A BMP code point below U+1000 needs zero padding to keep the four digit \\uXXXX form.
        assertEquals("\\u0000", ue.toUtf16Escape(0), "Code point 0 must escape to four hex digits");
        assertEquals("\\u000A", ue.toUtf16Escape('\n'), "Code point 0xA must escape to four hex digits");
        assertEquals("\\u0041", ue.toUtf16Escape('A'), "Code point 0x41 must escape to four hex digits");
        assertEquals("\\u0FFF", ue.toUtf16Escape(0xFFF), "Code point 0xFFF must escape to four hex digits");
        assertEquals("\\uFFFF", ue.toUtf16Escape(0xFFFF), "Code point 0xFFFF must escape to four hex digits");
        assertEquals(ue.translate("A"), ue.toUtf16Escape('A'), "toUtf16Escape must match the translate output");
        assertEquals("A", new UnicodeUnescaper().translate(ue.toUtf16Escape('A')), "Escaped BMP code point must round-trip");
    }

    @Test
    void testSupplementary() {
        final UnicodeEscaper ue = UnicodeEscaper.above(0x7f);
        // U+10437 (DESERET SMALL LETTER YEE) encodes to surrogate pair U+D801 U+DC37.
        final String input = new String(Character.toChars(0x10437));
        final String result = ue.translate(input);
        assertEquals("\\uD801\\uDC37", result, "Supplementary code point must escape to a surrogate pair");
        assertEquals(input, new UnicodeUnescaper().translate(result), "Escaped supplementary code point must round-trip");
    }
}
