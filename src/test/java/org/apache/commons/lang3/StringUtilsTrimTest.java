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
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

/**
 * Tests {@link StringUtils} trim methods.
 */
class StringUtilsTrimTest extends AbstractLangTest {

    private static final String FOO = "foo";

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
    void testTrimControl() {
        // null input returns null
        assertNull(StringUtils.trimControl(null));
        // empty string stays empty
        assertEquals("", StringUtils.trimControl(""));
        // no control chars: unchanged
        assertEquals(FOO, StringUtils.trimControl(FOO));
        // spaces are NOT stripped (space is char 32, not a control char)
        assertEquals(" " + FOO + " ", StringUtils.trimControl(" " + FOO + " "));
        // trailing NUL (\u0000) is stripped
        assertEquals(FOO, StringUtils.trimControl(FOO + "\u0000"));
        // leading NUL (\u0000) is stripped
        assertEquals(FOO, StringUtils.trimControl("\u0000" + FOO));
        // control chars on both ends are stripped
        assertEquals(FOO, StringUtils.trimControl("\t\r\n" + FOO + "\t\r\n"));
        // only control chars becomes empty string
        assertEquals("", StringUtils.trimControl("\u0001\u0002\u001F"));
        // embedded control chars are NOT stripped
        assertEquals("a\u0001b", StringUtils.trimControl("a\u0001b"));
        // space in the middle preserved, control chars on ends stripped
        assertEquals(" " + FOO + " ", StringUtils.trimControl("\u0001 " + FOO + " \u0001"));
        // char 31 (unit separator) is stripped; char 32 (space) is not
        assertEquals(" abc ", StringUtils.trimControl("\u001F abc \u001F"));
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
