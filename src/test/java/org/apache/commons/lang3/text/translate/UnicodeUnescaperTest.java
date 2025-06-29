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
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Tests for {@link org.apache.commons.lang3.text.translate.UnicodeEscaper}.
 */
@Deprecated
class UnicodeUnescaperTest extends AbstractLangTest {

    @Test
    void testLessThanFour() {
        final UnicodeUnescaper uu = new UnicodeUnescaper();

        final String input = "\\0047\\u006";
        assertThrows(
                IllegalArgumentException.class,
                () -> uu.translate(input),
                "A lack of digits in a Unicode escape sequence failed to throw an exception");
    }

    // Requested in LANG-507
    @Test
    void testUPlus() {
        final UnicodeUnescaper uu = new UnicodeUnescaper();

        final String input = "\\u+0047";
        assertEquals("G", uu.translate(input), "Failed to unescape Unicode characters with 'u+' notation");
    }

    @Test
    void testUuuuu() {
        final UnicodeUnescaper uu = new UnicodeUnescaper();

        final String input = "\\uuuuuuuu0047";
        final String result = uu.translate(input);
        assertEquals("G", result, "Failed to unescape Unicode characters with many 'u' characters");
    }
}
