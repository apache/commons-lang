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

/**
 * <p>Operations on char primitives and Char objects.</p>
 *
 * <p>This class tries to handle <code>null</code> input gracefully.
 * An exception will not be thrown for a <code>null</code> input.
 * Each method documents its behaviour in more detail.</p>
 * 
 * @author Stephen Colebourne
 * @since 2.1
 * @version $Id: CharUtils.java,v 1.2 2004/02/12 20:09:11 ggregory Exp $
 */
public class CharUtils {
    
    private static final String CHAR_STRING = 
        "\u0000\u0001\u0002\u0003\u0004\u0005\u0006\u0007" +
        "\b\t\n\u000b\f\r\u000e\u000f" +
        "\u0010\u0011\u0012\u0013\u0014\u0015\u0016\u0017" +
        "\u0018\u0019\u001a\u001b\u001c\u001d\u001e\u001f" +
        "\u0020\u0021\"\u0023\u0024\u0025\u0026\u0027" +
        "\u0028\u0029\u002a\u002b\u002c\u002d\u002e\u002f" +
        "\u0030\u0031\u0032\u0033\u0034\u0035\u0036\u0037" +
        "\u0038\u0039\u003a\u003b\u003c\u003d\u003e\u003f" +
        "\u0040\u0041\u0042\u0043\u0044\u0045\u0046\u0047" +
        "\u0048\u0049\u004a\u004b\u004c\u004d\u004e\u004f" +
        "\u0050\u0051\u0052\u0053\u0054\u0055\u0056\u0057" +
        "\u0058\u0059\u005a\u005b\\\u005d\u005e\u005f" +
        "\u0060\u0061\u0062\u0063\u0064\u0065\u0066\u0067" +
        "\u0068\u0069\u006a\u006b\u006c\u006d\u006e\u006f" +
        "\u0070\u0071\u0072\u0073\u0074\u0075\u0076\u0077" +
        "\u0078\u0079\u007a\u007b\u007c\u007d\u007e\u007f";
    
    private static final String[] CHAR_STRING_ARRAY = new String[128];
    private static final Character[] CHAR_ARRAY = new Character[128];
    
    static {
        for (int i = 127; i >= 0; i--) {
            CHAR_STRING_ARRAY[i] = CHAR_STRING.substring(i, i + 1);
            CHAR_ARRAY[i] = new Character((char) i);
        }
    }
    
    /**
     * <p><code>CharUtils</code> instances should NOT be constructed in standard programming.
     * Instead, the class should be used as <code>CharUtils.toString('c');</code>.</p>
     *
     * <p>This constructor is public to permit tools that require a JavaBean instance
     * to operate.</p>
     */
    public CharUtils() {
    }

    //-----------------------------------------------------------------------
    /**
     * <p>Converts the character to a Character.</p>
     * 
     * <p>For ASCII 7 bit characters, this uses a cache that will return the
     * same Character object each time.</p>
     *
     * <pre>
     *   CharUtils.toCharacterObject(' ')  = ' '
     *   CharUtils.toCharacterObject('A')  = 'A'
     * </pre>
     *
     * @param ch  the character to convert
     * @return a Character of the specified character
     */
    public static Character toCharacterObject(char ch) {
        if (ch < 128) {
            return CHAR_ARRAY[ch];
        } else {
            return new Character(ch);
        }
    }
    
    //-----------------------------------------------------------------------
    /**
     * <p>Converts the Character to a char throwing an exception for <code>null</code>.</p>
     * 
     * <pre>
     *   CharUtils.toCharacter(null, 'X') = IllegalArgumentException
     *   CharUtils.toCharacter(' ', 'X')  = ' '
     *   CharUtils.toCharacter('A', 'X')  = 'A'
     * </pre>
     *
     * @param ch  the character to convert
     * @return the char value of the Character or the default if null
     * @throws IllegalArgumentException if the Character is null
     */
    public static char toCharacter(Character ch) {
        if (ch == null) {
            throw new IllegalArgumentException("The Character must not be null");
        }
        return ch.charValue();
    }
    
    /**
     * <p>Converts the Character to a char handling <code>null</code>.</p>
     * 
     * <pre>
     *   CharUtils.toCharacter(null, 'X') = 'X'
     *   CharUtils.toCharacter(' ', 'X')  = ' '
     *   CharUtils.toCharacter('A', 'X')  = 'A'
     * </pre>
     *
     * @param ch  the character to convert
     * @param defaultValue  the value to use if the  Character is null
     * @return the char value of the Character or the default if null
     */
    public static char toCharacter(Character ch, char defaultValue) {
        if (ch == null) {
            return defaultValue;
        }
        return ch.charValue();
    }
    
    //-----------------------------------------------------------------------
    /**
     * <p>Converts the String to a char using the first character throwing
     * an exception on empty Strings.</p>
     * 
     * <pre>
     *   CharUtils.toCharacter(null, 'X') = IllegalArgumentException
     *   CharUtils.toCharacter("", 'X')   = IllegalArgumentException
     *   CharUtils.toCharacter("A", 'X')  = 'A'
     *   CharUtils.toCharacter("BA", 'X') = 'B'
     * </pre>
     *
     * @param str  the character to convert
     * @return the char value of the Character or the default if null
     * @throws IllegalArgumentException if the String is empty
     */
    public static char toCharacter(String str) {
        if (str == null || str.length() == 0) {
            throw new IllegalArgumentException("The String must not be empty");
        }
        return str.charAt(0);
    }
    
    /**
     * <p>Converts the String to a char using the first character defaulting
     * the value on empty Strings.</p>
     * 
     * <pre>
     *   CharUtils.toCharacter(null, 'X') = 'X'
     *   CharUtils.toCharacter("", 'X')   = 'X'
     *   CharUtils.toCharacter("A", 'X')  = 'A'
     *   CharUtils.toCharacter("BA", 'X') = 'B'
     * </pre>
     *
     * @param str  the character to convert
     * @param defaultValue  the value to use if the  Character is null
     * @return the char value of the Character or the default if null
     */
    public static char toCharacter(String str, char defaultValue) {
        if (str == null || str.length() == 0) {
            return defaultValue;
        }
        return str.charAt(0);
    }
    
    //-----------------------------------------------------------------------
    /**
     * <p>Converts the character to the Integer it represents, throwing an
     * exception if the character is not numeric.</p>
     * 
     * <p>This method coverts the char '1' to the int 1 and so on.</p>
     *
     * <pre>
     *   CharUtils.toInteger('3')  = 3
     *   CharUtils.toInteger('A')  = IllegalArgumentException
     * </pre>
     *
     * @param ch  the character to convert
     * @return the int value of the character
     * @throws IllegalArgumentException if the character is not ASCII numeric
     */
    public static int toInteger(char ch) {
        if (isASCIINumeric(ch) == false) {
            throw new IllegalArgumentException("The character " + ch + " is not in the range '0' - '9'");
        }
        return (ch - 48);
    }
    
    /**
     * <p>Converts the character to the Integer it represents, throwing an
     * exception if the character is not numeric.</p>
     * 
     * <p>This method coverts the char '1' to the int 1 and so on.</p>
     *
     * <pre>
     *   CharUtils.toInteger('3', -1)  = 3
     *   CharUtils.toInteger('A', -1)  = -1
     * </pre>
     *
     * @param ch  the character to convert
     * @param defaultValue  the default value to use if the character is not numeric
     * @return the int value of the character
     */
    public static int toInteger(char ch, int defaultValue) {
        if (isASCIINumeric(ch) == false) {
            return defaultValue;
        }
        return (ch - 48);
    }
    
    /**
     * <p>Converts the character to the Integer it represents, throwing an
     * exception if the character is not numeric.</p>
     * 
     * <p>This method coverts the char '1' to the int 1 and so on.</p>
     *
     * <pre>
     *   CharUtils.toInteger(null) = IllegalArgumentException
     *   CharUtils.toInteger('3')  = 3
     *   CharUtils.toInteger('A')  = IllegalArgumentException
     * </pre>
     *
     * @param ch  the character to convert, not null
     * @return the int value of the character
     * @throws IllegalArgumentException if the Character is not ASCII numeric or is null
     */
    public static int toInteger(Character ch) {
        if (ch == null) {
            throw new IllegalArgumentException("The character must not be null");
        }
        return toInteger(ch.charValue());
    }
    
    /**
     * <p>Converts the character to the Integer it represents, throwing an
     * exception if the character is not numeric.</p>
     * 
     * <p>This method coverts the char '1' to the int 1 and so on.</p>
     *
     * <pre>
     *   CharUtils.toInteger(null, -1) = -1
     *   CharUtils.toInteger('3', -1)  = 3
     *   CharUtils.toInteger('A', -1)  = -1
     * </pre>
     *
     * @param ch  the character to convert
     * @param defaultValue  the default value to use if the character is not numeric
     * @return the int value of the character
     */
    public static int toInteger(Character ch, int defaultValue) {
        if (ch == null) {
            return defaultValue;
        }
        return toInteger(ch.charValue(), defaultValue);
    }
    
    //-----------------------------------------------------------------------
    /**
     * <p>Converts the character to a String that contains the one character.</p>
     * 
     * <p>For ASCII 7 bit characters, this uses a cache that will return the
     * same String object each time.</p>
     *
     * <pre>
     *   CharUtils.toString(' ')  = " "
     *   CharUtils.toString('A')  = "A"
     * </pre>
     *
     * @param ch  the character to convert
     * @return a String containing the one specified character
     */
    public static String toString(char ch) {
        if (ch < 128) {
            return CHAR_STRING_ARRAY[ch];
        } else {
            return new String(new char[] {ch});
        }
    }
    
    /**
     * <p>Converts the character to a String that contains the one character.</p>
     * 
     * <p>For ASCII 7 bit characters, this uses a cache that will return the
     * same String object each time.</p>
     * 
     * <p>If <code>null</code> is passed in, <code>null</code> will be returned.</p>
     *
     * <pre>
     *   CharUtils.toString(null) = null
     *   CharUtils.toString(' ')  = " "
     *   CharUtils.toString('A')  = "A"
     * </pre>
     *
     * @param ch  the character to convert
     * @return a String containing the one specified character
     */
    public static String toString(Character ch) {
        if (ch == null) {
            return null;
        } else {
            return toString(ch.charValue());
        }
    }
    
    //--------------------------------------------------------------------------
    /**
     * <p>Converts the string to the unicode format '\u0020'.</p>
     * 
     * <p>This format is the Java source code format.</p>
     *
     * <pre>
     *   CharUtils.toUnicode(' ') = "\u0020"
     *   CharUtils.toUnicode('A') = "\u0041"
     * </pre>
     * 
     * @param ch  the character to convert
     * @return the escaped unicode string
     */
    public static String unicodeEscaped(char ch) {
        if (ch < 0x10) {
            return "\\u000" + Integer.toHexString(ch);
        } else if (ch < 0x100) {
            return "\\u00" + Integer.toHexString(ch);
        } else if (ch < 0x1000) {
            return "\\u0" + Integer.toHexString(ch);
        }
        return "\\u" + Integer.toHexString(ch);
    }
    
    /**
     * <p>Converts the string to the unicode format '\u0020'.</p>
     * 
     * <p>This format is the Java source code format.</p>
     * 
     * <p>If <code>null</code> is passed in, <code>null</code> will be returned.</p>
     *
     * <pre>
     *   CharUtils.toUnicode(null) = null
     *   CharUtils.toUnicode(' ')  = "\u0020"
     *   CharUtils.toUnicode('A')  = "\u0041"
     * </pre>
     * 
     * @param ch  the character to convert, may be null
     * @return the escaped unicode string, null if null input
     */
    public static String unicodeEscaped(Character ch) {
        if (ch == null) {
            return null;
        }
        return unicodeEscaped(ch.charValue());
    }
    
    //--------------------------------------------------------------------------
    /**
     * <p>Checks whether the character is ASCII 7 bit.</p>
     *
     * <pre>
     *   CharUtils.isASCII('a')  = true
     *   CharUtils.isASCII('A')  = true
     *   CharUtils.isASCII('3')  = true
     *   CharUtils.isASCII('-')  = true
     *   CharUtils.isASCII('\n') = true
     *   CharUtils.isASCII('&copy;') = false
     * </pre>
     * 
     * @param ch  the character to check
     * @return true if less than 128
     */
    public static boolean isASCII(char ch) {
        return (ch < 128);
    }
    
    /**
     * <p>Checks whether the character is ASCII 7 bit printable.</p>
     *
     * <pre>
     *   CharUtils.isASCIIPrintable('a')  = true
     *   CharUtils.isASCIIPrintable('A')  = true
     *   CharUtils.isASCIIPrintable('3')  = true
     *   CharUtils.isASCIIPrintable('-')  = true
     *   CharUtils.isASCIIPrintable('\n') = false
     *   CharUtils.isASCIIPrintable('&copy;') = false
     * </pre>
     * 
     * @param ch  the character to check
     * @return true if between 32 and 126 inclusive
     */
    public static boolean isASCIIPrintable(char ch) {
        return (ch >= 32 && ch < 127);
    }
    
    /**
     * <p>Checks whether the character is ASCII 7 bit control.</p>
     *
     * <pre>
     *   CharUtils.isASCIIControl('a')  = false
     *   CharUtils.isASCIIControl('A')  = false
     *   CharUtils.isASCIIControl('3')  = false
     *   CharUtils.isASCIIControl('-')  = false
     *   CharUtils.isASCIIControl('\n') = true
     *   CharUtils.isASCIIControl('&copy;') = false
     * </pre>
     * 
     * @param ch  the character to check
     * @return true if less than 32 or equals 127
     */
    public static boolean isASCIIControl(char ch) {
        return (ch < 32 || ch == 127);
    }
    
    /**
     * <p>Checks whether the character is ASCII 7 bit alphabetic.</p>
     *
     * <pre>
     *   CharUtils.isASCIIAlpha('a')  = true
     *   CharUtils.isASCIIAlpha('A')  = true
     *   CharUtils.isASCIIAlpha('3')  = false
     *   CharUtils.isASCIIAlpha('-')  = false
     *   CharUtils.isASCIIAlpha('\n') = false
     *   CharUtils.isASCIIAlpha('&copy;') = false
     * </pre>
     * 
     * @param ch  the character to check
     * @return true if between 65 and 90 or 97 and 122 inclusive
     */
    public static boolean isASCIIAlpha(char ch) {
        return (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z');
    }
    
    /**
     * <p>Checks whether the character is ASCII 7 bit alphabetic upper case.</p>
     *
     * <pre>
     *   CharUtils.isASCIIAlphaUpper('a')  = false
     *   CharUtils.isASCIIAlphaUpper('A')  = true
     *   CharUtils.isASCIIAlphaUpper('3')  = false
     *   CharUtils.isASCIIAlphaUpper('-')  = false
     *   CharUtils.isASCIIAlphaUpper('\n') = false
     *   CharUtils.isASCIIAlphaUpper('&copy;') = false
     * </pre>
     * 
     * @param ch  the character to check
     * @return true if between 65 and 90 inclusive
     */
    public static boolean isASCIIAlphaUpper(char ch) {
        return (ch >= 'A' && ch <= 'Z');
    }
    
    /**
     * <p>Checks whether the character is ASCII 7 bit alphabetic lower case.</p>
     *
     * <pre>
     *   CharUtils.isASCIIAlphaLower('a')  = true
     *   CharUtils.isASCIIAlphaLower('A')  = false
     *   CharUtils.isASCIIAlphaLower('3')  = false
     *   CharUtils.isASCIIAlphaLower('-')  = false
     *   CharUtils.isASCIIAlphaLower('\n') = false
     *   CharUtils.isASCIIAlphaLower('&copy;') = false
     * </pre>
     * 
     * @param ch  the character to check
     * @return true if between 97 and 122 inclusive
     */
    public static boolean isASCIIAlphaLower(char ch) {
        return (ch >= 'a' && ch <= 'z');
    }
    
    /**
     * <p>Checks whether the character is ASCII 7 bit numeric.</p>
     *
     * <pre>
     *   CharUtils.isASCIINumeric('a')  = false
     *   CharUtils.isASCIINumeric('A')  = false
     *   CharUtils.isASCIINumeric('3')  = true
     *   CharUtils.isASCIINumeric('-')  = false
     *   CharUtils.isASCIINumeric('\n') = false
     *   CharUtils.isASCIINumeric('&copy;') = false
     * </pre>
     * 
     * @param ch  the character to check
     * @return true if between 48 and 57 inclusive
     */
    public static boolean isASCIINumeric(char ch) {
        return (ch >= '0' && ch <= '9');
    }
    
    /**
     * <p>Checks whether the character is ASCII 7 bit numeric.</p>
     *
     * <pre>
     *   CharUtils.isASCIIAlphanumeric('a')  = true
     *   CharUtils.isASCIIAlphanumeric('A')  = true
     *   CharUtils.isASCIIAlphanumeric('3')  = true
     *   CharUtils.isASCIIAlphanumeric('-')  = false
     *   CharUtils.isASCIIAlphanumeric('\n') = false
     *   CharUtils.isASCIIAlphanumeric('&copy;') = false
     * </pre>
     * 
     * @param ch  the character to check
     * @return true if between 48 and 57 or 65 and 90 or 97 and 122 inclusive
     */
    public static boolean isASCIIAlphanumeric(char ch) {
        return (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || (ch >= '0' && ch <= '9');
    }
    
}
