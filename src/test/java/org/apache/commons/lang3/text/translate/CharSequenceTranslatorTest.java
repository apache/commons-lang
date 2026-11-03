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
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junitpioneer.jupiter.DefaultLocale;

/**
 * Tests {@link CharSequenceTranslator}.
 */
@Deprecated
class CharSequenceTranslatorTest extends AbstractLangTest {

    @ParameterizedTest
    // @formatter:off
    @CsvSource({
        "0, 0", "1, 1", "2, 2", "3, 3", "4, 4", "5, 5", "6, 6", "7, 7",
        "8, 8", "9, 9", "10, A", "11, B", "12, C", "13, D", "14, E", "15, F",
        "0x10, 10", "0xFF, FF", "0x100, 100", "0xFFF, FFF", "0x1000, 1000",
        "0xFFFF, FFFF", "0x10000, 10000", "0xFFFFF, FFFFF", "0x100000, 100000",
        "0xFFFFFF, FFFFFF", "0x1000000, 1000000", "0xFFFFFFF, FFFFFFF", "0x10000000, 10000000",
        "0xABCDEF, ABCDEF", "0x1020304, 1020304", "2147483647, 7FFFFFFF"
    })
    // @formatter:on
    void testHexDigitsAndWidth(final int value, final String expected) {
        assertEquals(expected, CharSequenceTranslator.hex(value));
    }

    @Test
    @DefaultLocale("tr")
    void testHexIgnoresDefaultLocale() {
        assertEquals("ABCDEF", CharSequenceTranslator.hex(0xABCDEF));
        assertEquals("FFFFFFFF", CharSequenceTranslator.hex(-1));
    }

    @ParameterizedTest
    @CsvSource({ "-1, FFFFFFFF", "-2, FFFFFFFE", "-16, FFFFFFF0", "-255, FFFFFF01", "-2147483647, 80000001", "-2147483648, 80000000" })
    void testHexNegativeValuesUseTwosComplement(final int value, final String expected) {
        assertEquals(expected, CharSequenceTranslator.hex(value));
    }

    @ParameterizedTest
    // @formatter:off
    @CsvSource({
        "0x20, 20", "0x41, 41", "0x7F, 7F", "0x80, 80", "0xE9, E9", "0x20AC, 20AC",
        "0xD7FF, D7FF", "0xD800, D800", "0xDFFF, DFFF", "0xE000, E000", "0xFFFF, FFFF",
        "0x10000, 10000", "0x1F600, 1F600", "0x10FFFF, 10FFFF", "0x110000, 110000"
    })
    // @formatter:on
    void testHexUnicodeBoundaries(final int value, final String expected) {
        // Conversion accepts any int, including surrogates and values beyond the Unicode range.
        assertEquals(expected, CharSequenceTranslator.hex(value));
    }
}
