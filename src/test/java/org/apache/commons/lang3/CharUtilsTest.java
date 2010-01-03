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

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;

import junit.framework.TestCase;

/**
 * Unit tests {@link org.apache.commons.lang3.CharUtils}.
 *
 * @author Apache Software Foundation
 * @version $Id$
 */
public class CharUtilsTest extends TestCase {

    private static final Character CHARACTER_A = new Character('A');
    private static final Character CHARACTER_B = new Character('B');
    private static final char CHAR_COPY = '\u00a9';
    
    public CharUtilsTest(String name) {
        super(name);
    }

    //-----------------------------------------------------------------------
    public void testConstructor() {
        assertNotNull(new CharUtils());
        Constructor<?>[] cons = CharUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertEquals(true, Modifier.isPublic(cons[0].getModifiers()));
        assertEquals(true, Modifier.isPublic(BooleanUtils.class.getModifiers()));
        assertEquals(false, Modifier.isFinal(BooleanUtils.class.getModifiers()));
    }
    
    //-----------------------------------------------------------------------
    public void testToCharacterObject_char() {
        assertEquals(new Character('a'), CharUtils.toCharacterObject('a'));
        assertSame(CharUtils.toCharacterObject('a'), CharUtils.toCharacterObject('a'));
       
        for (int i = 0; i < 128; i++) {
            Character ch = CharUtils.toCharacterObject((char) i);
            Character ch2 = CharUtils.toCharacterObject((char) i);
            assertSame(ch, ch2);
            assertEquals(i, ch.charValue());
        }
        for (int i = 128; i < 196; i++) {
            Character ch = CharUtils.toCharacterObject((char) i);
            Character ch2 = CharUtils.toCharacterObject((char) i);
            assertEquals(ch, ch2);
            assertTrue(ch != ch2);
            assertEquals(i, ch.charValue());
            assertEquals(i, ch2.charValue());
        }
    }
    
    public void testToCharacterObject_String() {
        assertEquals(null, CharUtils.toCharacterObject(null));
        assertEquals(null, CharUtils.toCharacterObject(""));
        assertEquals(new Character('a'), CharUtils.toCharacterObject("a"));
        assertEquals(new Character('a'), CharUtils.toCharacterObject("abc"));
        assertSame(CharUtils.toCharacterObject("a"), CharUtils.toCharacterObject("a"));
        assertSame(CharUtils.toCharacterObject("a"), CharUtils.toCharacterObject('a'));
    }
    
    //-----------------------------------------------------------------------
    public void testToChar_Character() {
        assertEquals('A', CharUtils.toChar(CHARACTER_A));
        assertEquals('B', CharUtils.toChar(CHARACTER_B));
        try {
            CharUtils.toChar((Character) null);
        } catch (IllegalArgumentException ex) {}
    }
    
    public void testToChar_Character_char() {
        assertEquals('A', CharUtils.toChar(CHARACTER_A, 'X'));
        assertEquals('B', CharUtils.toChar(CHARACTER_B, 'X'));
        assertEquals('X', CharUtils.toChar((Character) null, 'X'));
    }
    
    //-----------------------------------------------------------------------
    public void testToChar_String() {
        assertEquals('A', CharUtils.toChar("A"));
        assertEquals('B', CharUtils.toChar("BA"));
        try {
            CharUtils.toChar((String) null);
        } catch (IllegalArgumentException ex) {}
        try {
            CharUtils.toChar("");
        } catch (IllegalArgumentException ex) {}
    }
    
    public void testToChar_String_char() {
        assertEquals('A', CharUtils.toChar("A", 'X'));
        assertEquals('B', CharUtils.toChar("BA", 'X'));
        assertEquals('X', CharUtils.toChar("", 'X'));
        assertEquals('X', CharUtils.toChar((String) null, 'X'));
    }
    
    //-----------------------------------------------------------------------
    public void testToIntValue_char() {
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
        try {
            CharUtils.toIntValue('a');
        } catch (IllegalArgumentException ex) {}
    }
    
    public void testToIntValue_char_int() {
        assertEquals(0, CharUtils.toIntValue('0', -1));
        assertEquals(3, CharUtils.toIntValue('3', -1));
        assertEquals(-1, CharUtils.toIntValue('a', -1));
    }
    
    //-----------------------------------------------------------------------
    public void testToIntValue_Character() {
        assertEquals(0, CharUtils.toIntValue(new Character('0')));
        assertEquals(3, CharUtils.toIntValue(new Character('3')));
        try {
            CharUtils.toIntValue(null);
        } catch (IllegalArgumentException ex) {}
        try {
            CharUtils.toIntValue(CHARACTER_A);
        } catch (IllegalArgumentException ex) {}
    }
    
    public void testToIntValue_Character_int() {
        assertEquals(0, CharUtils.toIntValue(new Character('0'), -1));
        assertEquals(3, CharUtils.toIntValue(new Character('3'), -1));
        assertEquals(-1, CharUtils.toIntValue(new Character('A'), -1));
        assertEquals(-1, CharUtils.toIntValue(null, -1));
    }
    
    //-----------------------------------------------------------------------
    public void testToString_char() {
        assertEquals("a", CharUtils.toString('a'));
        assertSame(CharUtils.toString('a'), CharUtils.toString('a'));
       
        for (int i = 0; i < 128; i++) {
            String str = CharUtils.toString((char) i);
            String str2 = CharUtils.toString((char) i);
            assertSame(str, str2);
            assertEquals(1, str.length());
            assertEquals(i, str.charAt(0));
        }
        for (int i = 128; i < 196; i++) {
            String str = CharUtils.toString((char) i);
            String str2 = CharUtils.toString((char) i);
            assertEquals(str, str2);
            assertTrue(str != str2);
            assertEquals(1, str.length());
            assertEquals(i, str.charAt(0));
            assertEquals(1, str2.length());
            assertEquals(i, str2.charAt(0));
        }
    }
    
    public void testToString_Character() {
        assertEquals(null, CharUtils.toString(null));
        assertEquals("A", CharUtils.toString(CHARACTER_A));
        assertSame(CharUtils.toString(CHARACTER_A), CharUtils.toString(CHARACTER_A));
    }
    
    //-----------------------------------------------------------------------
    public void testToUnicodeEscaped_char() {
        assertEquals("\\u0041", CharUtils.unicodeEscaped('A'));
       
        for (int i = 0; i < 196; i++) {
            String str = CharUtils.unicodeEscaped((char) i);
            assertEquals(6, str.length());
            int val = Integer.parseInt(str.substring(2), 16);
            assertEquals(i, val);
        }
        assertEquals("\\u0999", CharUtils.unicodeEscaped((char) 0x999));
        assertEquals("\\u1001", CharUtils.unicodeEscaped((char) 0x1001));
    }
    
    public void testToUnicodeEscaped_Character() {
        assertEquals(null, CharUtils.unicodeEscaped(null));
        assertEquals("\\u0041", CharUtils.unicodeEscaped(CHARACTER_A));
    }
    
    //-----------------------------------------------------------------------
    public void testIsAscii_char() {
        assertEquals(true, CharUtils.isAscii('a'));
        assertEquals(true, CharUtils.isAscii('A'));
        assertEquals(true, CharUtils.isAscii('3'));
        assertEquals(true, CharUtils.isAscii('-'));
        assertEquals(true, CharUtils.isAscii('\n'));
        assertEquals(false, CharUtils.isAscii(CHAR_COPY));
       
        for (int i = 0; i < 128; i++) {
            if (i < 128) {
                assertEquals(true, CharUtils.isAscii((char) i));
            } else {
                assertEquals(false, CharUtils.isAscii((char) i));
            }
        }
    }
    
    //-----------------------------------------------------------------------
    public void testIsAsciiPrintable_char() {
        assertEquals(true, CharUtils.isAsciiPrintable('a'));
        assertEquals(true, CharUtils.isAsciiPrintable('A'));
        assertEquals(true, CharUtils.isAsciiPrintable('3'));
        assertEquals(true, CharUtils.isAsciiPrintable('-'));
        assertEquals(false, CharUtils.isAsciiPrintable('\n'));
        assertEquals(false, CharUtils.isAscii(CHAR_COPY));
       
        for (int i = 0; i < 196; i++) {
            if (i >= 32 && i <= 126) {
                assertEquals(true, CharUtils.isAsciiPrintable((char) i));
            } else {
                assertEquals(false, CharUtils.isAsciiPrintable((char) i));
            }
        }
    }
    
    //-----------------------------------------------------------------------
    public void testIsAsciiControl_char() {
        assertEquals(false, CharUtils.isAsciiControl('a'));
        assertEquals(false, CharUtils.isAsciiControl('A'));
        assertEquals(false, CharUtils.isAsciiControl('3'));
        assertEquals(false, CharUtils.isAsciiControl('-'));
        assertEquals(true, CharUtils.isAsciiControl('\n'));
        assertEquals(false, CharUtils.isAsciiControl(CHAR_COPY));
       
        for (int i = 0; i < 196; i++) {
            if (i < 32 || i == 127) {
                assertEquals(true, CharUtils.isAsciiControl((char) i));
            } else {
                assertEquals(false, CharUtils.isAsciiControl((char) i));
            }
        }
    }
    
    //-----------------------------------------------------------------------
    public void testIsAsciiAlpha_char() {
        assertEquals(true, CharUtils.isAsciiAlpha('a'));
        assertEquals(true, CharUtils.isAsciiAlpha('A'));
        assertEquals(false, CharUtils.isAsciiAlpha('3'));
        assertEquals(false, CharUtils.isAsciiAlpha('-'));
        assertEquals(false, CharUtils.isAsciiAlpha('\n'));
        assertEquals(false, CharUtils.isAsciiAlpha(CHAR_COPY));
       
        for (int i = 0; i < 196; i++) {
            if ((i >= 'A' && i <= 'Z') || (i >= 'a' && i <= 'z')) {
                assertEquals(true, CharUtils.isAsciiAlpha((char) i));
            } else {
                assertEquals(false, CharUtils.isAsciiAlpha((char) i));
            }
        }
    }
    
    //-----------------------------------------------------------------------
    public void testIsAsciiAlphaUpper_char() {
        assertEquals(false, CharUtils.isAsciiAlphaUpper('a'));
        assertEquals(true, CharUtils.isAsciiAlphaUpper('A'));
        assertEquals(false, CharUtils.isAsciiAlphaUpper('3'));
        assertEquals(false, CharUtils.isAsciiAlphaUpper('-'));
        assertEquals(false, CharUtils.isAsciiAlphaUpper('\n'));
        assertEquals(false, CharUtils.isAsciiAlphaUpper(CHAR_COPY));
       
        for (int i = 0; i < 196; i++) {
            if (i >= 'A' && i <= 'Z') {
                assertEquals(true, CharUtils.isAsciiAlphaUpper((char) i));
            } else {
                assertEquals(false, CharUtils.isAsciiAlphaUpper((char) i));
            }
        }
    }
    
    //-----------------------------------------------------------------------
    public void testIsAsciiAlphaLower_char() {
        assertEquals(true, CharUtils.isAsciiAlphaLower('a'));
        assertEquals(false, CharUtils.isAsciiAlphaLower('A'));
        assertEquals(false, CharUtils.isAsciiAlphaLower('3'));
        assertEquals(false, CharUtils.isAsciiAlphaLower('-'));
        assertEquals(false, CharUtils.isAsciiAlphaLower('\n'));
        assertEquals(false, CharUtils.isAsciiAlphaLower(CHAR_COPY));
       
        for (int i = 0; i < 196; i++) {
            if (i >= 'a' && i <= 'z') {
                assertEquals(true, CharUtils.isAsciiAlphaLower((char) i));
            } else {
                assertEquals(false, CharUtils.isAsciiAlphaLower((char) i));
            }
        }
    }
    
    //-----------------------------------------------------------------------
    public void testIsAsciiNumeric_char() {
        assertEquals(false, CharUtils.isAsciiNumeric('a'));
        assertEquals(false, CharUtils.isAsciiNumeric('A'));
        assertEquals(true, CharUtils.isAsciiNumeric('3'));
        assertEquals(false, CharUtils.isAsciiNumeric('-'));
        assertEquals(false, CharUtils.isAsciiNumeric('\n'));
        assertEquals(false, CharUtils.isAsciiNumeric(CHAR_COPY));
       
        for (int i = 0; i < 196; i++) {
            if (i >= '0' && i <= '9') {
                assertEquals(true, CharUtils.isAsciiNumeric((char) i));
            } else {
                assertEquals(false, CharUtils.isAsciiNumeric((char) i));
            }
        }
    }
    
    //-----------------------------------------------------------------------
    public void testIsAsciiAlphanumeric_char() {
        assertEquals(true, CharUtils.isAsciiAlphanumeric('a'));
        assertEquals(true, CharUtils.isAsciiAlphanumeric('A'));
        assertEquals(true, CharUtils.isAsciiAlphanumeric('3'));
        assertEquals(false, CharUtils.isAsciiAlphanumeric('-'));
        assertEquals(false, CharUtils.isAsciiAlphanumeric('\n'));
        assertEquals(false, CharUtils.isAsciiAlphanumeric(CHAR_COPY));
       
        for (int i = 0; i < 196; i++) {
            if ((i >= 'A' && i <= 'Z') || (i >= 'a' && i <= 'z') || (i >= '0' && i <= '9')) {
                assertEquals(true, CharUtils.isAsciiAlphanumeric((char) i));
            } else {
                assertEquals(false, CharUtils.isAsciiAlphanumeric((char) i));
            }
        }
    }
    
}
