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

import java.util.Objects;

/**
 * Operations on char primitives and Character objects.
 *
 * <p>This class tries to handle {@code null} input gracefully.
 * An exception will not be thrown for a {@code null} input.
 * Each method documents its behavior in more detail.</p>
 *
 * <p>#ThreadSafe#</p>
 * @since 2.1
 */
public class CharUtils {

    private static final String[] CHAR_STRING_ARRAY = new String[128];

    private static final char[] HEX_DIGITS = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'};

    /**
     * Linefeed character LF ({@code '\n'}, Unicode 000a).
     *
     * @see <a href="https://docs.oracle.com/javase/specs/jls/se8/html/jls-3.html#jls-3.10.6">JLF: Escape Sequences
     *      for Character and String Literals</a>
     * @since 2.2
     */
    public static final char LF = '\n';

    /**
     * Carriage return character CR ('\r', Unicode 000d).
     *
     * @see <a href="https://docs.oracle.com/javase/specs/jls/se8/html/jls-3.html#jls-3.10.6">JLF: Escape Sequences
     *      for Character and String Literals</a>
     * @since 2.2
     */
    public static final char CR = '\r';

    /**
     * {@code \u0000} null control character ('\0'), abbreviated NUL.
     *
     * @since 3.6
     */
    public static final char NUL = '\0';

    static {
        ArrayUtils.setAll(CHAR_STRING_ARRAY, i -> String.valueOf((char) i));
    }

    /**
     * {@link CharUtils} instances should NOT be constructed in standard programming.
     * Instead, the class should be used as {@code CharUtils.toString('c');}.
     *
     * <p>This constructor is public to permit tools that require a JavaBean instance
     * to operate.</p>
     */
    public CharUtils() {
    }

    /**
     * Converts the character to a Character.
     *
     * <p>For ASCII 7 bit characters, this uses a cache that will return the
     * same Character object each time.</p>
     *
     * <pre>
     *   CharUtils.toCharacterObject(' ')  = ' '
     *   CharUtils.toCharacterObject('A')  = 'A'
     * </pre>
     *
     * @deprecated Java 5 introduced {@link Character#valueOf(char)} which caches chars 0 through 127.
     * @param ch  the character to convert
     * @return a Character of the specified character
     */
    @Deprecated
    public static Character toCharacterObject(final char ch) {
        return Character.valueOf(ch);
    }

    /**
     * Converts the String to a Character using the first character, returning
     * null for empty Strings.
     *
     * <p>For ASCII 7 bit characters, this uses a cache that will return the
     * same Character object each time.</p>
     *
     * <pre>
     *   CharUtils.toCharacterObject(null) = null
     *   CharUtils.toCharacterObject("")   = null
     *   CharUtils.toCharacterObject("A")  = 'A'
     *   CharUtils.toCharacterObject("BA") = 'B'
     * </pre>
     *
     * @param str  the character to convert
     * @return the Character value of the first letter of the String
     */
    public static Character toCharacterObject(final String str) {
        return StringUtils.isEmpty(str) ? null : Character.valueOf(str.charAt(0));
    }

    /**
     * Converts the Character to a char throwing an exception for {@code null}.
     *
     * <pre>
     *   CharUtils.toChar(' ')  = ' '
     *   CharUtils.toChar('A')  = 'A'
     *   CharUtils.toChar(null) throws IllegalArgumentException
     * </pre>
     *
     * @param ch  the character to convert
     * @return the char value of the Character
     * @throws NullPointerException if the Character is null
     */
    public static char toChar(final Character ch) {
        return Objects.requireNonNull(ch, "ch").charValue();
    }

    /**
     * Converts the Character to a char handling {@code null}.
     *
     * <pre>
     *   CharUtils.toChar(null, 'X') = 'X'
     *   CharUtils.toChar(' ', 'X')  = ' '
     *   CharUtils.toChar('A', 'X')  = 'A'
     * </pre>
     *
     * @param ch  the character to convert
     * @param defaultValue  the value to use if the  Character is null
     * @return the char value of the Character or the default if null
     */
    public static char toChar(final Character ch, final char defaultValue) {
        return ch != null ? ch.charValue() : defaultValue;
    }

    /**
     * Converts the String to a char using the first character, throwing
     * an exception on empty Strings.
     *
     * <pre>
     *   CharUtils.toChar("A")  = 'A'
     *   CharUtils.toChar("BA") = 'B'
     *   CharUtils.toChar(null) throws IllegalArgumentException
     *   CharUtils.toChar("")   throws IllegalArgumentException
     * </pre>
     *
     * @param str  the character to convert
     * @return the char value of the first letter of the String
     * @throws NullPointerException if the string is null
     * @throws IllegalArgumentException if the String is empty
     */
    public static char toChar(final String str) {
        Validate.notEmpty(str, "The String must not be empty");
        return str.charAt(0);
    }

    /**
     * Converts the String to a char using the first character, defaulting
     * the value on empty Strings.
     *
     * <pre>
     *   CharUtils.toChar(null, 'X') = 'X'
     *   CharUtils.toChar("", 'X')   = 'X'
     *   CharUtils.toChar("A", 'X')  = 'A'
     *   CharUtils.toChar("BA", 'X') = 'B'
     * </pre>
     *
     * @param str  the character to convert
     * @param defaultValue  the value to use if the  Character is null
     * @return the char value of the first letter of the String or the default if null
     */
    public static char toChar(final String str, final char defaultValue) {
        return StringUtils.isEmpty(str) ? defaultValue : str.charAt(0);
    }

    /**
     * Converts the character to the Integer it represents, throwing an
     * exception if the character is not numeric.
     *
     * <p>This method converts the char '1' to the int 1 and so on.</p>
     *
     * <pre>
     *   CharUtils.toIntValue('3')  = 3
     *   CharUtils.toIntValue('A')  throws IllegalArgumentException
     * </pre>
     *
     * @param ch  the character to convert
     * @return the int value of the character
     * @throws IllegalArgumentException if the character is not ASCII numeric
     */
    public static int toIntValue(final char ch) {
        if (!isAsciiNumeric(ch)) {
            throw new IllegalArgumentException("The character " + ch + " is not in the range '0' - '9'");
        }
        return ch - 48;
    }

    /**
     * Converts the character to the Integer it represents, throwing an
     * exception if the character is not numeric.
     *
     * <p>This method converts the char '1' to the int 1 and so on.</p>
     *
     * <pre>
     *   CharUtils.toIntValue('3', -1)  = 3
     *   CharUtils.toIntValue('A', -1)  = -1
     * </pre>
     *
     * @param ch  the character to convert
     * @param defaultValue  the default value to use if the character is not numeric
     * @return the int value of the character
     */
    public static int toIntValue(final char ch, final int defaultValue) {
        return isAsciiNumeric(ch) ? ch - 48 : defaultValue;
    }

    /**
     * Converts the character to the Integer it represents, throwing an
     * exception if the character is not numeric.
     *
     * <p>This method converts the char '1' to the int 1 and so on.</p>
     *
     * <pre>
     *   CharUtils.toIntValue('3')  = 3
     *   CharUtils.toIntValue(null) throws IllegalArgumentException
     *   CharUtils.toIntValue('A')  throws IllegalArgumentException
     * </pre>
     *
     * @param ch  the character to convert, not null
     * @return the int value of the character
     * @throws NullPointerException if the Character is null
     * @throws IllegalArgumentException if the Character is not ASCII numeric
     */
    public static int toIntValue(final Character ch) {
        return toIntValue(toChar(ch));
    }

    /**
     * Converts the character to the Integer it represents, throwing an
     * exception if the character is not numeric.
     *
     * <p>This method converts the char '1' to the int 1 and so on.</p>
     *
     * <pre>
     *   CharUtils.toIntValue(null, -1) = -1
     *   CharUtils.toIntValue('3', -1)  = 3
     *   CharUtils.toIntValue('A', -1)  = -1
     * </pre>
     *
     * @param ch  the character to convert
     * @param defaultValue  the default value to use if the character is not numeric
     * @return the int value of the character
     */
    public static int toIntValue(final Character ch, final int defaultValue) {
        return ch != null ? toIntValue(ch.charValue(), defaultValue) : defaultValue;
    }

    /**
     * Converts the character to a String that contains the one character.
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
    public static String toString(final char ch) {
        if (ch < CHAR_STRING_ARRAY.length) {
            return CHAR_STRING_ARRAY[ch];
        }
        return String.valueOf(ch);
    }

    /**
     * Converts the character to a String that contains the one character.
     *
     * <p>For ASCII 7 bit characters, this uses a cache that will return the
     * same String object each time.</p>
     *
     * <p>If {@code null} is passed in, {@code null} will be returned.</p>
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
    public static String toString(final Character ch) {
        return ch != null ? toString(ch.charValue()) : null;
    }

    /**
     * Converts the string to the Unicode format '\u0020'.
     *
     * <p>This format is the Java source code format.</p>
     *
     * <pre>
     *   CharUtils.unicodeEscaped(' ') = "\u0020"
     *   CharUtils.unicodeEscaped('A') = "\u0041"
     * </pre>
     *
     * @param ch  the character to convert
     * @return the escaped Unicode string
     */
    public static String unicodeEscaped(final char ch) {
        return "\\u" +
            HEX_DIGITS[(ch >> 12) & 15] +
            HEX_DIGITS[(ch >> 8) & 15] +
            HEX_DIGITS[(ch >> 4) & 15] +
            HEX_DIGITS[(ch) & 15];
    }

    /**
     * Converts the string to the Unicode format '\u0020'.
     *
     * <p>This format is the Java source code format.</p>
     *
     * <p>If {@code null} is passed in, {@code null} will be returned.</p>
     *
     * <pre>
     *   CharUtils.unicodeEscaped(null) = null
     *   CharUtils.unicodeEscaped(' ')  = "\u0020"
     *   CharUtils.unicodeEscaped('A')  = "\u0041"
     * </pre>
     *
     * @param ch  the character to convert, may be null
     * @return the escaped Unicode string, null if null input
     */
    public static String unicodeEscaped(final Character ch) {
        return ch != null ? unicodeEscaped(ch.charValue()) : null;
    }

    /**
     * Checks whether the character is ASCII 7 bit.
     *
     * <pre>
     *   CharUtils.isAscii('a')  = true
     *   CharUtils.isAscii('A')  = true
     *   CharUtils.isAscii('3')  = true
     *   CharUtils.isAscii('-')  = true
     *   CharUtils.isAscii('\n') = true
     *   CharUtils.isAscii('&copy;') = false
     * </pre>
     *
     * @param ch  the character to check
     * @return true if less than 128
     */
    public static boolean isAscii(final char ch) {
        return ch < 128;
    }

    /**
     * Checks whether the character is ASCII 7 bit printable.
     *
     * <pre>
     *   CharUtils.isAsciiPrintable('a')  = true
     *   CharUtils.isAsciiPrintable('A')  = true
     *   CharUtils.isAsciiPrintable('3')  = true
     *   CharUtils.isAsciiPrintable('-')  = true
     *   CharUtils.isAsciiPrintable('\n') = false
     *   CharUtils.isAsciiPrintable('&copy;') = false
     * </pre>
     *
     * @param ch  the character to check
     * @return true if between 32 and 126 inclusive
     */
    public static boolean isAsciiPrintable(final char ch) {
        return ch >= 32 && ch < 127;
    }

    /**
     * Checks whether the character is ASCII 7 bit control.
     *
     * <pre>
     *   CharUtils.isAsciiControl('a')  = false
     *   CharUtils.isAsciiControl('A')  = false
     *   CharUtils.isAsciiControl('3')  = false
     *   CharUtils.isAsciiControl('-')  = false
     *   CharUtils.isAsciiControl('\n') = true
     *   CharUtils.isAsciiControl('&copy;') = false
     * </pre>
     *
     * @param ch  the character to check
     * @return true if less than 32 or equals 127
     */
    public static boolean isAsciiControl(final char ch) {
        return ch < 32 || ch == 127;
    }

    /**
     * Checks whether the character is ASCII 7 bit alphabetic.
     *
     * <pre>
     *   CharUtils.isAsciiAlpha('a')  = true
     *   CharUtils.isAsciiAlpha('A')  = true
     *   CharUtils.isAsciiAlpha('3')  = false
     *   CharUtils.isAsciiAlpha('-')  = false
     *   CharUtils.isAsciiAlpha('\n') = false
     *   CharUtils.isAsciiAlpha('&copy;') = false
     * </pre>
     *
     * @param ch  the character to check
     * @return true if between 65 and 90 or 97 and 122 inclusive
     */
    public static boolean isAsciiAlpha(final char ch) {
        return isAsciiAlphaUpper(ch) || isAsciiAlphaLower(ch);
    }

    /**
     * Checks whether the character is ASCII 7 bit alphabetic upper case.
     *
     * <pre>
     *   CharUtils.isAsciiAlphaUpper('a')  = false
     *   CharUtils.isAsciiAlphaUpper('A')  = true
     *   CharUtils.isAsciiAlphaUpper('3')  = false
     *   CharUtils.isAsciiAlphaUpper('-')  = false
     *   CharUtils.isAsciiAlphaUpper('\n') = false
     *   CharUtils.isAsciiAlphaUpper('&copy;') = false
     * </pre>
     *
     * @param ch  the character to check
     * @return true if between 65 and 90 inclusive
     */
    public static boolean isAsciiAlphaUpper(final char ch) {
        return ch >= 'A' && ch <= 'Z';
    }

    /**
     * Checks whether the character is ASCII 7 bit alphabetic lower case.
     *
     * <pre>
     *   CharUtils.isAsciiAlphaLower('a')  = true
     *   CharUtils.isAsciiAlphaLower('A')  = false
     *   CharUtils.isAsciiAlphaLower('3')  = false
     *   CharUtils.isAsciiAlphaLower('-')  = false
     *   CharUtils.isAsciiAlphaLower('\n') = false
     *   CharUtils.isAsciiAlphaLower('&copy;') = false
     * </pre>
     *
     * @param ch  the character to check
     * @return true if between 97 and 122 inclusive
     */
    public static boolean isAsciiAlphaLower(final char ch) {
        return ch >= 'a' && ch <= 'z';
    }

    /**
     * Checks whether the character is ASCII 7 bit numeric.
     *
     * <pre>
     *   CharUtils.isAsciiNumeric('a')  = false
     *   CharUtils.isAsciiNumeric('A')  = false
     *   CharUtils.isAsciiNumeric('3')  = true
     *   CharUtils.isAsciiNumeric('-')  = false
     *   CharUtils.isAsciiNumeric('\n') = false
     *   CharUtils.isAsciiNumeric('&copy;') = false
     * </pre>
     *
     * @param ch  the character to check
     * @return true if between 48 and 57 inclusive
     */
    public static boolean isAsciiNumeric(final char ch) {
        return ch >= '0' && ch <= '9';
    }

    /**
     * Checks whether the character is ASCII 7 bit numeric.
     *
     * <pre>
     *   CharUtils.isAsciiAlphanumeric('a')  = true
     *   CharUtils.isAsciiAlphanumeric('A')  = true
     *   CharUtils.isAsciiAlphanumeric('3')  = true
     *   CharUtils.isAsciiAlphanumeric('-')  = false
     *   CharUtils.isAsciiAlphanumeric('\n') = false
     *   CharUtils.isAsciiAlphanumeric('&copy;') = false
     * </pre>
     *
     * @param ch  the character to check
     * @return true if between 48 and 57 or 65 and 90 or 97 and 122 inclusive
     */
    public static boolean isAsciiAlphanumeric(final char ch) {
        return isAsciiAlpha(ch) || isAsciiNumeric(ch);
    }

    /**
     * Compares two {@code char} values numerically. This is the same functionality as provided in Java 7.
     *
     * @param x the first {@code char} to compare
     * @param y the second {@code char} to compare
     * @return the value {@code 0} if {@code x == y};
     *         a value less than {@code 0} if {@code x < y}; and
     *         a value greater than {@code 0} if {@code x > y}
     * @since 3.4
     */
    public static int compare(final char x, final char y) {
        return x - y;
    }
}
