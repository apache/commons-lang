/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2004 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution, if
 *    any, must include the following acknowledgement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowledgement may appear in the software itself,
 *    if and wherever such third-party acknowledgements normally appear.
 *
 * 4. The names "The Jakarta Project", "Commons", and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
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
 * @version $Id: CharUtilsTest.java,v 1.1 2004/01/31 13:00:07 scolebourne Exp $
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
