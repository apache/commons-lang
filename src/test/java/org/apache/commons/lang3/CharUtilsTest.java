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

import static org.apache.commons.lang3.LangAssertions.assertNullPointerException;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;

import org.junit.jupiter.api.Test;

/**
 * Tests {@link CharUtils}.
 */
class CharUtilsTest extends AbstractLangTest {

    private static final char CHAR_COPY = '\u00a9';
    private static final Character CHARACTER_A = Character.valueOf('A');
    private static final Character CHARACTER_B = Character.valueOf('B');

    @Test
    void testCompare() {
        assertTrue(CharUtils.compare('a', 'b') < 0);
        assertEquals(0, CharUtils.compare('c', 'c'));
        assertTrue(CharUtils.compare('c', 'a') > 0);
    }

    @Test
    void testConstructor() {
        assertNotNull(new CharUtils());
        final Constructor<?>[] cons = CharUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertTrue(Modifier.isPublic(cons[0].getModifiers()));
        assertTrue(Modifier.isPublic(CharUtils.class.getModifiers()));
        assertFalse(Modifier.isFinal(CharUtils.class.getModifiers()));
    }

    @Test
    void testIsAscii_char() {
        assertTrue(CharUtils.isAscii('a'));
        assertTrue(CharUtils.isAscii('A'));
        assertTrue(CharUtils.isAscii('3'));
        assertTrue(CharUtils.isAscii('-'));
        assertTrue(CharUtils.isAscii('\n'));
        assertFalse(CharUtils.isAscii(CHAR_COPY));

        for (int i = 0; i < 255; i++) {
            assertEquals(i < 128, CharUtils.isAscii((char) i));
        }
    }

    @Test
    void testIsAsciiAlpha_char() {
        assertTrue(CharUtils.isAsciiAlpha('a'));
        assertTrue(CharUtils.isAsciiAlpha('A'));
        assertFalse(CharUtils.isAsciiAlpha('3'));
        assertFalse(CharUtils.isAsciiAlpha('-'));
        assertFalse(CharUtils.isAsciiAlpha('\n'));
        assertFalse(CharUtils.isAsciiAlpha(CHAR_COPY));

        for (int i = 0; i < 196; i++) {
            if (i >= 'A' && i <= 'Z' || i >= 'a' && i <= 'z') {
                assertTrue(CharUtils.isAsciiAlpha((char) i));
            } else {
                assertFalse(CharUtils.isAsciiAlpha((char) i));
            }
        }
    }

    @Test
    void testIsAsciiAlphaLower_char() {
        assertTrue(CharUtils.isAsciiAlphaLower('a'));
        assertFalse(CharUtils.isAsciiAlphaLower('A'));
        assertFalse(CharUtils.isAsciiAlphaLower('3'));
        assertFalse(CharUtils.isAsciiAlphaLower('-'));
        assertFalse(CharUtils.isAsciiAlphaLower('\n'));
        assertFalse(CharUtils.isAsciiAlphaLower(CHAR_COPY));

        for (int i = 0; i < 196; i++) {
            if (i >= 'a' && i <= 'z') {
                assertTrue(CharUtils.isAsciiAlphaLower((char) i));
            } else {
                assertFalse(CharUtils.isAsciiAlphaLower((char) i));
            }
        }
    }

    @Test
    void testIsAsciiAlphanumeric_char() {
        assertTrue(CharUtils.isAsciiAlphanumeric('a'));
        assertTrue(CharUtils.isAsciiAlphanumeric('A'));
        assertTrue(CharUtils.isAsciiAlphanumeric('3'));
        assertFalse(CharUtils.isAsciiAlphanumeric('-'));
        assertFalse(CharUtils.isAsciiAlphanumeric('\n'));
        assertFalse(CharUtils.isAsciiAlphanumeric(CHAR_COPY));

        for (int i = 0; i < 196; i++) {
            if (i >= 'A' && i <= 'Z' || i >= 'a' && i <= 'z' || i >= '0' && i <= '9') {
                assertTrue(CharUtils.isAsciiAlphanumeric((char) i));
            } else {
                assertFalse(CharUtils.isAsciiAlphanumeric((char) i));
            }
        }
    }

    @Test
    void testIsAsciiAlphaUpper_char() {
        assertFalse(CharUtils.isAsciiAlphaUpper('a'));
        assertTrue(CharUtils.isAsciiAlphaUpper('A'));
        assertFalse(CharUtils.isAsciiAlphaUpper('3'));
        assertFalse(CharUtils.isAsciiAlphaUpper('-'));
        assertFalse(CharUtils.isAsciiAlphaUpper('\n'));
        assertFalse(CharUtils.isAsciiAlphaUpper(CHAR_COPY));

        for (int i = 0; i < 196; i++) {
            if (i >= 'A' && i <= 'Z') {
                assertTrue(CharUtils.isAsciiAlphaUpper((char) i));
            } else {
                assertFalse(CharUtils.isAsciiAlphaUpper((char) i));
            }
        }
    }

    @Test
    void testIsAsciiControl_char() {
        assertFalse(CharUtils.isAsciiControl('a'));
        assertFalse(CharUtils.isAsciiControl('A'));
        assertFalse(CharUtils.isAsciiControl('3'));
        assertFalse(CharUtils.isAsciiControl('-'));
        assertTrue(CharUtils.isAsciiControl('\n'));
        assertFalse(CharUtils.isAsciiControl(CHAR_COPY));

        for (int i = 0; i < 196; i++) {
            if (i < 32 || i == 127) {
                assertTrue(CharUtils.isAsciiControl((char) i));
            } else {
                assertFalse(CharUtils.isAsciiControl((char) i));
            }
        }
    }

    @Test
    void testIsAsciiNumeric_char() {
        assertFalse(CharUtils.isAsciiNumeric('a'));
        assertFalse(CharUtils.isAsciiNumeric('A'));
        assertTrue(CharUtils.isAsciiNumeric('3'));
        assertFalse(CharUtils.isAsciiNumeric('-'));
        assertFalse(CharUtils.isAsciiNumeric('\n'));
        assertFalse(CharUtils.isAsciiNumeric(CHAR_COPY));

        for (int i = 0; i < 196; i++) {
            if (i >= '0' && i <= '9') {
                assertTrue(CharUtils.isAsciiNumeric((char) i));
            } else {
                assertFalse(CharUtils.isAsciiNumeric((char) i));
            }
        }
    }

    @Test
    void testIsAsciiPrintable_char() {
        assertTrue(CharUtils.isAsciiPrintable('a'));
        assertTrue(CharUtils.isAsciiPrintable('A'));
        assertTrue(CharUtils.isAsciiPrintable('3'));
        assertTrue(CharUtils.isAsciiPrintable('-'));
        assertFalse(CharUtils.isAsciiPrintable('\n'));
        assertFalse(CharUtils.isAsciiPrintable(CHAR_COPY));

        for (int i = 0; i < 196; i++) {
            if (i >= 32 && i <= 126) {
                assertTrue(CharUtils.isAsciiPrintable((char) i));
            } else {
                assertFalse(CharUtils.isAsciiPrintable((char) i));
            }
        }
    }

    @Test
    void testToChar_Character() {
        assertEquals('A', CharUtils.toChar(CHARACTER_A));
        assertEquals('B', CharUtils.toChar(CHARACTER_B));
        assertNullPointerException(() -> CharUtils.toChar((Character) null));
    }

    @Test
    void testToChar_Character_char() {
        assertEquals('A', CharUtils.toChar(CHARACTER_A, 'X'));
        assertEquals('B', CharUtils.toChar(CHARACTER_B, 'X'));
        assertEquals('X', CharUtils.toChar((Character) null, 'X'));
    }

    @Test
    void testToChar_String() {
        assertEquals('A', CharUtils.toChar("A"));
        assertEquals('B', CharUtils.toChar("BA"));
        assertNullPointerException(() -> CharUtils.toChar((String) null));
        assertThrows(IllegalArgumentException.class, () -> CharUtils.toChar(""));
    }

    @Test
    void testToChar_String_char() {
        assertEquals('A', CharUtils.toChar("A", 'X'));
        assertEquals('B', CharUtils.toChar("BA", 'X'));
        assertEquals('X', CharUtils.toChar("", 'X'));
        assertEquals('X', CharUtils.toChar((String) null, 'X'));
    }

    @SuppressWarnings("deprecation") // intentional test of deprecated method
    @Test
    void testToCharacterObject_char() {
        assertEquals(Character.valueOf('a'), CharUtils.toCharacterObject('a'));
        assertSame(CharUtils.toCharacterObject('a'), CharUtils.toCharacterObject('a'));

        for (int i = 0; i < 128; i++) {
            final Character ch = CharUtils.toCharacterObject((char) i);
            final Character ch2 = CharUtils.toCharacterObject((char) i);
            assertSame(ch, ch2);
            assertEquals(i, ch.charValue());
        }
        for (int i = 128; i < 196; i++) {
            final Character ch = CharUtils.toCharacterObject((char) i);
            final Character ch2 = CharUtils.toCharacterObject((char) i);
            assertEquals(ch, ch2);
            assertNotSame(ch, ch2);
            assertEquals(i, ch.charValue());
            assertEquals(i, ch2.charValue());
        }
        assertSame(CharUtils.toCharacterObject("a"), CharUtils.toCharacterObject('a'));
    }

    @Test
    void testToCharacterObject_String() {
        assertNull(CharUtils.toCharacterObject(null));
        assertNull(CharUtils.toCharacterObject(""));
        assertEquals(Character.valueOf('a'), CharUtils.toCharacterObject("a"));
        assertEquals(Character.valueOf('a'), CharUtils.toCharacterObject("abc"));
        assertSame(CharUtils.toCharacterObject("a"), CharUtils.toCharacterObject("a"));
    }

    @Test
    void testToIntValue_char() {
        assertEquals(0, CharUtils.toIntValue('0'));
        assertEquals(1, CharUtils.toIntValue('1'));
        assertEquals(2, CharUtils.toIntValue('2'));
        assertEquals(3, CharUtils.toIntValue('3'));
        assertEquals(4, CharUtils.toIntValue('4'));
        assertEquals(5, CharUtils.toIntValue('5'));
        assertEquals(6, CharUtils.toIntValue('6'));
        assertEquals(7, CharUtils.toIntValue('7'));
        assertEquals(8, CharUtils.toIntValue('8'));
        assertEquals(9, CharUtils.toIntValue('9'));
        assertThrows(IllegalArgumentException.class, () -> CharUtils.toIntValue('a'));
    }

    @Test
    void testToIntValue_char_int() {
        assertEquals(0, CharUtils.toIntValue('0', -1));
        assertEquals(3, CharUtils.toIntValue('3', -1));
        assertEquals(-1, CharUtils.toIntValue('a', -1));
    }

    @Test
    void testToIntValue_Character() {
        assertEquals(0, CharUtils.toIntValue(Character.valueOf('0')));
        assertEquals(3, CharUtils.toIntValue(Character.valueOf('3')));
        assertNullPointerException(() -> CharUtils.toIntValue(null));
        assertThrows(IllegalArgumentException.class, () -> CharUtils.toIntValue(CHARACTER_A));
    }

    @Test
    void testToIntValue_Character_int() {
        assertEquals(0, CharUtils.toIntValue(Character.valueOf('0'), -1));
        assertEquals(3, CharUtils.toIntValue(Character.valueOf('3'), -1));
        assertEquals(-1, CharUtils.toIntValue(Character.valueOf('A'), -1));
        assertEquals(-1, CharUtils.toIntValue(null, -1));
    }

    @Test
    void testToString_char() {
        assertEquals("a", CharUtils.toString('a'));
        assertSame(CharUtils.toString('a'), CharUtils.toString('a'));

        for (int i = 0; i < 128; i++) {
            final String str = CharUtils.toString((char) i);
            final String str2 = CharUtils.toString((char) i);
            assertSame(str, str2);
            assertEquals(1, str.length());
            assertEquals(i, str.charAt(0));
        }
        for (int i = 128; i < 196; i++) {
            final String str = CharUtils.toString((char) i);
            final String str2 = CharUtils.toString((char) i);
            assertEquals(str, str2);
            assertNotSame(str, str2);
            assertEquals(1, str.length());
            assertEquals(i, str.charAt(0));
            assertEquals(1, str2.length());
            assertEquals(i, str2.charAt(0));
        }
    }

    @Test
    void testToString_Character() {
        assertNull(CharUtils.toString(null));
        assertEquals("A", CharUtils.toString(CHARACTER_A));
        assertSame(CharUtils.toString(CHARACTER_A), CharUtils.toString(CHARACTER_A));
    }

    @Test
    void testToUnicodeEscaped_char() {
        assertEquals("\\u0041", CharUtils.unicodeEscaped('A'));
        assertEquals("\\u004c", CharUtils.unicodeEscaped('L'));

        for (int i = 0; i < 196; i++) {
            final String str = CharUtils.unicodeEscaped((char) i);
            assertEquals(6, str.length());
            final int val = Integer.parseInt(str.substring(2), 16);
            assertEquals(i, val);
        }
        assertEquals("\\u0999", CharUtils.unicodeEscaped((char) 0x999));
        assertEquals("\\u1001", CharUtils.unicodeEscaped((char) 0x1001));
    }

    @Test
    void testToUnicodeEscaped_Character() {
        assertNull(CharUtils.unicodeEscaped(null));
        assertEquals("\\u0041", CharUtils.unicodeEscaped(CHARACTER_A));
    }
}
