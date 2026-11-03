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
 * Tests for {@link org.apache.commons.lang3.text.translate.JavaUnicodeEscaper}.
 */
@Deprecated
class JavaUnicodeEscaperTest extends AbstractLangTest {

    @Test
    void testSupplementary() {
        final JavaUnicodeEscaper jue = JavaUnicodeEscaper.above(0x7f);
        // U+10437 (DESERET SMALL LETTER YEE) encodes to surrogate pair U+D801 U+DC37.
        final String input = new String(Character.toChars(0x10437));
        final String result = jue.translate(input);
        assertEquals("\\uD801\\uDC37", result, "Supplementary code point must escape to a surrogate pair");
        assertEquals("\\uD801\\uDC37", jue.toUtf16Escape(0x10437), "toUtf16Escape must emit the surrogate pair form");
    }

    @Test
    void testToUtf16EscapeBmp() {
        final JavaUnicodeEscaper jue = new JavaUnicodeEscaper(0, Integer.MAX_VALUE, true);
        assertEquals("\\u0041", jue.toUtf16Escape('A'), "BMP code point must escape to a single \\uXXXX");
        assertEquals("\\uFFFF", jue.toUtf16Escape(0xFFFF), "BMP code point must escape to a single \\uXXXX");
        assertEquals(jue.translate("A"), jue.toUtf16Escape('A'), "toUtf16Escape must match the translate output");
    }
}
