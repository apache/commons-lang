/*
 * Copyright 2004 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang;

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * Unit tests {@link org.apache.commons.lang.CharUtils}.
 *
 * @author Stephen Colebourne
 * @version $Id: CharUtilsTest.java,v 1.2 2004/02/18 23:06:19 ggregory Exp $
 */
public class CharUtilsTest extends TestCase {

    private static final Character CHARACTER_A = new Character('A');
    private static final Character CHARACTER_B = new Character('B');
    private static final char CHAR_COPY = '\u00a9';
    
    public CharUtilsTest(String name) {
        super(name);
    }

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
    	TestSuite suite = new TestSuite(CharUtilsTest.class);
    	suite.setName("CharUtils Tests");
        return suite;
    }

    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    //-----------------------------------------------------------------------
    public void testConstructor() {
        assertNotNull(new CharUtils());
        Constructor[] cons = CharUtils.class.getDeclaredConstructors();
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
    
    //-----------------------------------------------------------------------
    public void testToCharacter_Character() {
        assertEquals('A', CharUtils.toCharacter(CHARACTER_A));
        assertEquals('B', CharUtils.toCharacter(CHARACTER_B));
        try {
            CharUtils.toCharacter((Character) null);
        } catch (IllegalArgumentException ex) {}
    }
    
    public void testToCharacter_Character_char() {
        assertEquals('A', CharUtils.toCharacter(CHARACTER_A, 'X'));
        assertEquals('B', CharUtils.toCharacter(CHARACTER_B, 'X'));
        assertEquals('X', CharUtils.toCharacter((Character) null, 'X'));
    }
    
    //-----------------------------------------------------------------------
    public void testToCharacter_String() {
        assertEquals('A', CharUtils.toCharacter("A"));
        assertEquals('B', CharUtils.toCharacter("BA"));
        try {
            CharUtils.toCharacter((String) null);
        } catch (IllegalArgumentException ex) {}
        try {
            CharUtils.toCharacter("");
        } catch (IllegalArgumentException ex) {}
    }
    
    public void testToCharacter_String_char() {
        assertEquals('A', CharUtils.toCharacter("A", 'X'));
        assertEquals('B', CharUtils.toCharacter("BA", 'X'));
        assertEquals('X', CharUtils.toCharacter("", 'X'));
        assertEquals('X', CharUtils.toCharacter((String) null, 'X'));
    }
    
    //-----------------------------------------------------------------------
    public void testToInteger_char() {
        assertEquals(0, CharUtils.toInteger('0'));
        assertEquals(1, CharUtils.toInteger('1'));
        assertEquals(2, CharUtils.toInteger('2'));
        assertEquals(3, CharUtils.toInteger('3'));
        assertEquals(4, CharUtils.toInteger('4'));
        assertEquals(5, CharUtils.toInteger('5'));
        assertEquals(6, CharUtils.toInteger('6'));
        assertEquals(7, CharUtils.toInteger('7'));
        assertEquals(8, CharUtils.toInteger('8'));
        assertEquals(9, CharUtils.toInteger('9'));
        try {
            CharUtils.toInteger('a');
        } catch (IllegalArgumentException ex) {}
    }
    
    public void testToInteger_char_int() {
        assertEquals(0, CharUtils.toInteger('0', -1));
        assertEquals(3, CharUtils.toInteger('3', -1));
        assertEquals(-1, CharUtils.toInteger('a', -1));
    }
    
    //-----------------------------------------------------------------------
    public void testToInteger_Character() {
        assertEquals(0, CharUtils.toInteger(new Character('0')));
        assertEquals(3, CharUtils.toInteger(new Character('3')));
        try {
            CharUtils.toInteger(null);
        } catch (IllegalArgumentException ex) {}
        try {
            CharUtils.toInteger(CHARACTER_A);
        } catch (IllegalArgumentException ex) {}
    }
    
    public void testToInteger_Character_int() {
        assertEquals(0, CharUtils.toInteger(new Character('0'), -1));
        assertEquals(3, CharUtils.toInteger(new Character('3'), -1));
        assertEquals(-1, CharUtils.toInteger(new Character('A'), -1));
        assertEquals(-1, CharUtils.toInteger(null, -1));
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
    }
    
    public void testToUnicodeEscaped_Character() {
        assertEquals(null, CharUtils.unicodeEscaped(null));
        assertEquals("\\u0041", CharUtils.unicodeEscaped(CHARACTER_A));
    }
    
    //-----------------------------------------------------------------------
    public void testIsASCII_char() {
        assertEquals(true, CharUtils.isASCII('a'));
        assertEquals(true, CharUtils.isASCII('A'));
        assertEquals(true, CharUtils.isASCII('3'));
        assertEquals(true, CharUtils.isASCII('-'));
        assertEquals(true, CharUtils.isASCII('\n'));
        assertEquals(false, CharUtils.isASCII(CHAR_COPY));
       
        for (int i = 0; i < 128; i++) {
            if (i < 128) {
                assertEquals(true, CharUtils.isASCII((char) i));
            } else {
                assertEquals(false, CharUtils.isASCII((char) i));
            }
        }
    }
    
    //-----------------------------------------------------------------------
    public void testIsASCIIPrintable_char() {
        assertEquals(true, CharUtils.isASCIIPrintable('a'));
        assertEquals(true, CharUtils.isASCIIPrintable('A'));
        assertEquals(true, CharUtils.isASCIIPrintable('3'));
        assertEquals(true, CharUtils.isASCIIPrintable('-'));
        assertEquals(false, CharUtils.isASCIIPrintable('\n'));
        assertEquals(false, CharUtils.isASCII(CHAR_COPY));
       
        for (int i = 0; i < 196; i++) {
            if (i >= 32 && i <= 126) {
                assertEquals(true, CharUtils.isASCIIPrintable((char) i));
            } else {
                assertEquals(false, CharUtils.isASCIIPrintable((char) i));
            }
        }
    }
    
    //-----------------------------------------------------------------------
    public void testIsASCIIControl_char() {
        assertEquals(false, CharUtils.isASCIIControl('a'));
        assertEquals(false, CharUtils.isASCIIControl('A'));
        assertEquals(false, CharUtils.isASCIIControl('3'));
        assertEquals(false, CharUtils.isASCIIControl('-'));
        assertEquals(true, CharUtils.isASCIIControl('\n'));
        assertEquals(false, CharUtils.isASCIIControl(CHAR_COPY));
       
        for (int i = 0; i < 196; i++) {
            if (i < 32 || i == 127) {
                assertEquals(true, CharUtils.isASCIIControl((char) i));
            } else {
                assertEquals(false, CharUtils.isASCIIControl((char) i));
            }
        }
    }
    
    //-----------------------------------------------------------------------
    public void testIsASCIIAlpha_char() {
        assertEquals(true, CharUtils.isASCIIAlpha('a'));
        assertEquals(true, CharUtils.isASCIIAlpha('A'));
        assertEquals(false, CharUtils.isASCIIAlpha('3'));
        assertEquals(false, CharUtils.isASCIIAlpha('-'));
        assertEquals(false, CharUtils.isASCIIAlpha('\n'));
        assertEquals(false, CharUtils.isASCIIAlpha(CHAR_COPY));
       
        for (int i = 0; i < 196; i++) {
            if ((i >= 'A' && i <= 'Z') || (i >= 'a' && i <= 'z')) {
                assertEquals(true, CharUtils.isASCIIAlpha((char) i));
            } else {
                assertEquals(false, CharUtils.isASCIIAlpha((char) i));
            }
        }
    }
    
    //-----------------------------------------------------------------------
    public void testIsASCIIAlphaUpper_char() {
        assertEquals(false, CharUtils.isASCIIAlphaUpper('a'));
        assertEquals(true, CharUtils.isASCIIAlphaUpper('A'));
        assertEquals(false, CharUtils.isASCIIAlphaUpper('3'));
        assertEquals(false, CharUtils.isASCIIAlphaUpper('-'));
        assertEquals(false, CharUtils.isASCIIAlphaUpper('\n'));
        assertEquals(false, CharUtils.isASCIIAlphaUpper(CHAR_COPY));
       
        for (int i = 0; i < 196; i++) {
            if (i >= 'A' && i <= 'Z') {
                assertEquals(true, CharUtils.isASCIIAlphaUpper((char) i));
            } else {
                assertEquals(false, CharUtils.isASCIIAlphaUpper((char) i));
            }
        }
    }
    
    //-----------------------------------------------------------------------
    public void testIsASCIIAlphaLower_char() {
        assertEquals(true, CharUtils.isASCIIAlphaLower('a'));
        assertEquals(false, CharUtils.isASCIIAlphaLower('A'));
        assertEquals(false, CharUtils.isASCIIAlphaLower('3'));
        assertEquals(false, CharUtils.isASCIIAlphaLower('-'));
        assertEquals(false, CharUtils.isASCIIAlphaLower('\n'));
        assertEquals(false, CharUtils.isASCIIAlphaLower(CHAR_COPY));
       
        for (int i = 0; i < 196; i++) {
            if (i >= 'a' && i <= 'z') {
                assertEquals(true, CharUtils.isASCIIAlphaLower((char) i));
            } else {
                assertEquals(false, CharUtils.isASCIIAlphaLower((char) i));
            }
        }
    }
    
    //-----------------------------------------------------------------------
    public void testIsASCIINumeric_char() {
        assertEquals(false, CharUtils.isASCIINumeric('a'));
        assertEquals(false, CharUtils.isASCIINumeric('A'));
        assertEquals(true, CharUtils.isASCIINumeric('3'));
        assertEquals(false, CharUtils.isASCIINumeric('-'));
        assertEquals(false, CharUtils.isASCIINumeric('\n'));
        assertEquals(false, CharUtils.isASCIINumeric(CHAR_COPY));
       
        for (int i = 0; i < 196; i++) {
            if (i >= '0' && i <= '9') {
                assertEquals(true, CharUtils.isASCIINumeric((char) i));
            } else {
                assertEquals(false, CharUtils.isASCIINumeric((char) i));
            }
        }
    }
    
    //-----------------------------------------------------------------------
    public void testIsASCIIAlphanumeric_char() {
        assertEquals(true, CharUtils.isASCIIAlphanumeric('a'));
        assertEquals(true, CharUtils.isASCIIAlphanumeric('A'));
        assertEquals(true, CharUtils.isASCIIAlphanumeric('3'));
        assertEquals(false, CharUtils.isASCIIAlphanumeric('-'));
        assertEquals(false, CharUtils.isASCIIAlphanumeric('\n'));
        assertEquals(false, CharUtils.isASCIIAlphanumeric(CHAR_COPY));
       
        for (int i = 0; i < 196; i++) {
            if ((i >= 'A' && i <= 'Z') || (i >= 'a' && i <= 'z') || (i >= '0' && i <= '9')) {
                assertEquals(true, CharUtils.isASCIIAlphanumeric((char) i));
            } else {
                assertEquals(false, CharUtils.isASCIIAlphanumeric((char) i));
            }
        }
    }
    
}
