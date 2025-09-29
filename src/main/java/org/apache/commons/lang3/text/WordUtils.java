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
package org.apache.commons.lang3.text;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;

/**
 * Operations on Strings that contain words.
 *
 * <p>This class tries to handle {@code null} input gracefully.
 * An exception will not be thrown for a {@code null} input.
 * Each method documents its behavior in more detail.</p>
 *
 * @since 2.0
 * @deprecated As of <a href="https://commons.apache.org/proper/commons-lang/changes-report.html#a3.6">3.6</a>, use Apache Commons Text
 * <a href="https://commons.apache.org/proper/commons-text/javadocs/api-release/org/apache/commons/text/WordUtils.html">
 * WordUtils</a>.
 */
@Deprecated
public class WordUtils {

    /**
     * Capitalizes all the whitespace separated words in a String.
     * Only the first character of each word is changed. To convert the
     * rest of each word to lowercase at the same time,
     * use {@link #capitalizeFully(String)}.
     *
     * <p>Whitespace is defined by {@link Character#isWhitespace(char)}.
     * A {@code null} input String returns {@code null}.
     * Capitalization uses the Unicode title case, normally equivalent to
     * upper case.</p>
     *
     * <pre>
     * WordUtils.capitalize(null)        = null
     * WordUtils.capitalize("")          = ""
     * WordUtils.capitalize("i am FINE") = "I Am FINE"
     * </pre>
     *
     * @param str  the String to capitalize, may be null.
     * @return capitalized String, {@code null} if null String input.
     * @see #uncapitalize(String)
     * @see #capitalizeFully(String)
     */
    public static String capitalize(final String str) {
        return capitalize(str, null);
    }

    /**
     * Capitalizes all the delimiter separated words in a String.
     * Only the first character of each word is changed. To convert the
     * rest of each word to lowercase at the same time,
     * use {@link #capitalizeFully(String, char[])}.
     *
     * <p>The delimiters represent a set of characters understood to separate words.
     * The first string character and the first non-delimiter character after a
     * delimiter will be capitalized.</p>
     *
     * <p>A {@code null} input String returns {@code null}.
     * Capitalization uses the Unicode title case, normally equivalent to
     * upper case.</p>
     *
     * <pre>
     * WordUtils.capitalize(null, *)            = null
     * WordUtils.capitalize("", *)              = ""
     * WordUtils.capitalize(*, new char[0])     = *
     * WordUtils.capitalize("i am fine", null)  = "I Am Fine"
     * WordUtils.capitalize("i aM.fine", {'.'}) = "I aM.Fine"
     * </pre>
     *
     * @param str  the String to capitalize, may be null.
     * @param delimiters  set of characters to determine capitalization, null means whitespace.
     * @return capitalized String, {@code null} if null String input.
     * @see #uncapitalize(String)
     * @see #capitalizeFully(String)
     * @since 2.1
     */
    public static String capitalize(final String str, final char... delimiters) {
        final int delimLen = delimiters == null ? -1 : delimiters.length;
        if (StringUtils.isEmpty(str) || delimLen == 0) {
            return str;
        }
        final char[] buffer = str.toCharArray();
        boolean capitalizeNext = true;
        for (int i = 0; i < buffer.length; i++) {
            final char ch = buffer[i];
            if (isDelimiter(ch, delimiters)) {
                capitalizeNext = true;
            } else if (capitalizeNext) {
                buffer[i] = Character.toTitleCase(ch);
                capitalizeNext = false;
            }
        }
        return new String(buffer);
    }

    /**
     * Converts all the whitespace separated words in a String into capitalized words,
     * that is each word is made up of a titlecase character and then a series of
     * lowercase characters.
     *
     * <p>Whitespace is defined by {@link Character#isWhitespace(char)}.
     * A {@code null} input String returns {@code null}.
     * Capitalization uses the Unicode title case, normally equivalent to
     * upper case.</p>
     *
     * <pre>
     * WordUtils.capitalizeFully(null)        = null
     * WordUtils.capitalizeFully("")          = ""
     * WordUtils.capitalizeFully("i am FINE") = "I Am Fine"
     * </pre>
     *
     * @param str  the String to capitalize, may be null.
     * @return capitalized String, {@code null} if null String input.
     */
    public static String capitalizeFully(final String str) {
        return capitalizeFully(str, null);
    }

    /**
     * Converts all the delimiter separated words in a String into capitalized words,
     * that is each word is made up of a titlecase character and then a series of
     * lowercase characters.
     *
     * <p>The delimiters represent a set of characters understood to separate words.
     * The first string character and the first non-delimiter character after a
     * delimiter will be capitalized.</p>
     *
     * <p>A {@code null} input String returns {@code null}.
     * Capitalization uses the Unicode title case, normally equivalent to
     * upper case.</p>
     *
     * <pre>
     * WordUtils.capitalizeFully(null, *)            = null
     * WordUtils.capitalizeFully("", *)              = ""
     * WordUtils.capitalizeFully(*, null)            = *
     * WordUtils.capitalizeFully(*, new char[0])     = *
     * WordUtils.capitalizeFully("i aM.fine", {'.'}) = "I am.Fine"
     * </pre>
     *
     * @param str  the String to capitalize, may be null.
     * @param delimiters  set of characters to determine capitalization, null means whitespace.
     * @return capitalized String, {@code null} if null String input.
     * @since 2.1
     */
    public static String capitalizeFully(final String str, final char... delimiters) {
        final int delimLen = delimiters == null ? -1 : delimiters.length;
        if (StringUtils.isEmpty(str) || delimLen == 0) {
            return str;
        }
        return capitalize(str.toLowerCase(), delimiters);
    }

    /**
     * Checks if the String contains all words in the given array.
     *
     * <p>
     * A {@code null} String will return {@code false}. A {@code null}, zero
     * length search array or if one element of array is null will return {@code false}.
     * </p>
     *
     * <pre>
     * WordUtils.containsAllWords(null, *)            = false
     * WordUtils.containsAllWords("", *)              = false
     * WordUtils.containsAllWords(*, null)            = false
     * WordUtils.containsAllWords(*, [])              = false
     * WordUtils.containsAllWords("abcd", "ab", "cd") = false
     * WordUtils.containsAllWords("abc def", "def", "abc") = true
     * </pre>
     *
     * @param word The CharSequence to check, may be null.
     * @param words The array of String words to search for, may be null.
     * @return {@code true} if all search words are found, {@code false} otherwise.
     * @since 3.5
     */
    public static boolean containsAllWords(final CharSequence word, final CharSequence... words) {
        if (StringUtils.isEmpty(word) || ArrayUtils.isEmpty(words)) {
            return false;
        }
        for (final CharSequence w : words) {
            if (StringUtils.isBlank(w)) {
                return false;
            }
            final Pattern p = Pattern.compile(".*\\b" + Pattern.quote(w.toString()) + "\\b.*");
            if (!p.matcher(word).matches()) {
                return false;
            }
        }
        return true;
    }

    /**
     * Extracts the initial characters from each word in the String.
     *
     * <p>All first characters after whitespace are returned as a new string.
     * Their case is not changed.</p>
     *
     * <p>Whitespace is defined by {@link Character#isWhitespace(char)}.
     * A {@code null} input String returns {@code null}.</p>
     *
     * <pre>
     * WordUtils.initials(null)             = null
     * WordUtils.initials("")               = ""
     * WordUtils.initials("Ben John Lee")   = "BJL"
     * WordUtils.initials("Ben J.Lee")      = "BJ"
     * </pre>
     *
     * @param str  the String to get initials from, may be null.
     * @return String of initial letters, {@code null} if null String input.
     * @see #initials(String,char[])
     * @since 2.2
     */
    public static String initials(final String str) {
        return initials(str, null);
    }

    /**
     * Extracts the initial characters from each word in the String.
     *
     * <p>All first characters after the defined delimiters are returned as a new string.
     * Their case is not changed.</p>
     *
     * <p>If the delimiters array is null, then Whitespace is used.
     * Whitespace is defined by {@link Character#isWhitespace(char)}.
     * A {@code null} input String returns {@code null}.
     * An empty delimiter array returns an empty String.</p>
     *
     * <pre>
     * WordUtils.initials(null, *)                = null
     * WordUtils.initials("", *)                  = ""
     * WordUtils.initials("Ben John Lee", null)   = "BJL"
     * WordUtils.initials("Ben J.Lee", null)      = "BJ"
     * WordUtils.initials("Ben J.Lee", [' ','.']) = "BJL"
     * WordUtils.initials(*, new char[0])         = ""
     * </pre>
     *
     * @param str  the String to get initials from, may be null.
     * @param delimiters  set of characters to determine words, null means whitespace.
     * @return String of initial characters, {@code null} if null String input.
     * @see #initials(String)
     * @since 2.2
     */
    public static String initials(final String str, final char... delimiters) {
        if (StringUtils.isEmpty(str)) {
            return str;
        }
        if (delimiters != null && delimiters.length == 0) {
            return StringUtils.EMPTY;
        }
        final int strLen = str.length();
        final char[] buf = new char[strLen / 2 + 1];
        int count = 0;
        boolean lastWasGap = true;
        for (int i = 0; i < strLen; i++) {
            final char ch = str.charAt(i);
            if (isDelimiter(ch, delimiters)) {
                lastWasGap = true;
                continue;  // ignore ch
            }
            if (lastWasGap) {
                buf[count++] = ch;
                lastWasGap = false;
            }
        }
        return new String(buf, 0, count);
    }

    /**
     * Tests if the character is a delimiter.
     *
     * @param ch  the character to check.
     * @param delimiters  the delimiters.
     * @return true if it is a delimiter.
     */
    private static boolean isDelimiter(final char ch, final char[] delimiters) {
        return delimiters == null ? Character.isWhitespace(ch) : ArrayUtils.contains(delimiters, ch);
    }

    /**
     * Swaps the case of a String using a word based algorithm.
     *
     * <ul>
     *  <li>Upper case character converts to Lower case</li>
     *  <li>Title case character converts to Lower case</li>
     *  <li>Lower case character after Whitespace or at start converts to Title case</li>
     *  <li>Other Lower case character converts to Upper case</li>
     * </ul>
     *
     * <p>Whitespace is defined by {@link Character#isWhitespace(char)}.
     * A {@code null} input String returns {@code null}.</p>
     *
     * <pre>
     * StringUtils.swapCase(null)                 = null
     * StringUtils.swapCase("")                   = ""
     * StringUtils.swapCase("The dog has a BONE") = "tHE DOG HAS A bone"
     * </pre>
     *
     * @param str  the String to swap case, may be null.
     * @return A new String, {@code null} if null String input.
     */
    public static String swapCase(final String str) {
        if (StringUtils.isEmpty(str)) {
            return str;
        }
        final char[] buffer = str.toCharArray();

        boolean whitespace = true;

        for (int i = 0; i < buffer.length; i++) {
            final char ch = buffer[i];
            if (Character.isUpperCase(ch) || Character.isTitleCase(ch)) {
                buffer[i] = Character.toLowerCase(ch);
                whitespace = false;
            } else if (Character.isLowerCase(ch)) {
                if (whitespace) {
                    buffer[i] = Character.toTitleCase(ch);
                    whitespace = false;
                } else {
                    buffer[i] = Character.toUpperCase(ch);
                }
            } else {
                whitespace = Character.isWhitespace(ch);
            }
        }
        return new String(buffer);
    }

    /**
     * Uncapitalizes all the whitespace separated words in a String.
     * Only the first character of each word is changed.
     *
     * <p>Whitespace is defined by {@link Character#isWhitespace(char)}.
     * A {@code null} input String returns {@code null}.</p>
     *
     * <pre>
     * WordUtils.uncapitalize(null)        = null
     * WordUtils.uncapitalize("")          = ""
     * WordUtils.uncapitalize("I Am FINE") = "i am fINE"
     * </pre>
     *
     * @param str  the String to uncapitalize, may be null.
     * @return uncapitalized String, {@code null} if null String input.
     * @see #capitalize(String)
     */
    public static String uncapitalize(final String str) {
        return uncapitalize(str, null);
    }

    /**
     * Uncapitalizes all the whitespace separated words in a String.
     * Only the first character of each word is changed.
     *
     * <p>The delimiters represent a set of characters understood to separate words.
     * The first string character and the first non-delimiter character after a
     * delimiter will be uncapitalized.</p>
     *
     * <p>Whitespace is defined by {@link Character#isWhitespace(char)}.
     * A {@code null} input String returns {@code null}.</p>
     *
     * <pre>
     * WordUtils.uncapitalize(null, *)            = null
     * WordUtils.uncapitalize("", *)              = ""
     * WordUtils.uncapitalize(*, null)            = *
     * WordUtils.uncapitalize(*, new char[0])     = *
     * WordUtils.uncapitalize("I AM.FINE", {'.'}) = "i AM.fINE"
     * </pre>
     *
     * @param str  the String to uncapitalize, may be null.
     * @param delimiters  set of characters to determine uncapitalization, null means whitespace.
     * @return uncapitalized String, {@code null} if null String input.
     * @see #capitalize(String)
     * @since 2.1
     */
    public static String uncapitalize(final String str, final char... delimiters) {
        final int delimLen = delimiters == null ? -1 : delimiters.length;
        if (StringUtils.isEmpty(str) || delimLen == 0) {
            return str;
        }
        final char[] buffer = str.toCharArray();
        boolean uncapitalizeNext = true;
        for (int i = 0; i < buffer.length; i++) {
            final char ch = buffer[i];
            if (isDelimiter(ch, delimiters)) {
                uncapitalizeNext = true;
            } else if (uncapitalizeNext) {
                buffer[i] = Character.toLowerCase(ch);
                uncapitalizeNext = false;
            }
        }
        return new String(buffer);
    }

    /**
     * Wraps a single line of text, identifying words by {@code ' '}.
     *
     * <p>New lines will be separated by the system property line separator.
     * Very long words, such as URLs will <em>not</em> be wrapped.</p>
     *
     * <p>Leading spaces on a new line are stripped.
     * Trailing spaces are not stripped.</p>
     *
     * <table border="1">
     *  <caption>Examples</caption>
     *  <tr>
     *   <th>input</th>
     *   <th>wrapLength</th>
     *   <th>result</th>
     *  </tr>
     *  <tr>
     *   <td>null</td>
     *   <td>*</td>
     *   <td>null</td>
     *  </tr>
     *  <tr>
     *   <td>""</td>
     *   <td>*</td>
     *   <td>""</td>
     *  </tr>
     *  <tr>
     *   <td>"Here is one line of text that is going to be wrapped after 20 columns."</td>
     *   <td>20</td>
     *   <td>"Here is one line of\ntext that is going\nto be wrapped after\n20 columns."</td>
     *  </tr>
     *  <tr>
     *   <td>"Click here to jump to the commons website - https://commons.apache.org"</td>
     *   <td>20</td>
     *   <td>"Click here to jump\nto the commons\nwebsite -\nhttps://commons.apache.org"</td>
     *  </tr>
     *  <tr>
     *   <td>"Click here, https://commons.apache.org, to jump to the commons website"</td>
     *   <td>20</td>
     *   <td>"Click here,\nhttps://commons.apache.org,\nto jump to the\ncommons website"</td>
     *  </tr>
     * </table>
     *
     * (assuming that '\n' is the systems line separator)
     *
     * @param str  the String to be word wrapped, may be null.
     * @param wrapLength  the column to wrap the words at, less than 1 is treated as 1.
     * @return a line with newlines inserted, {@code null} if null input.
     */
    public static String wrap(final String str, final int wrapLength) {
        return wrap(str, wrapLength, null, false);
    }

    /**
     * Wraps a single line of text, identifying words by {@code ' '}.
     *
     * <p>Leading spaces on a new line are stripped.
     * Trailing spaces are not stripped.</p>
     *
     * <table border="1">
     *  <caption>Examples</caption>
     *  <tr>
     *   <th>input</th>
     *   <th>wrapLength</th>
     *   <th>newLineString</th>
     *   <th>wrapLongWords</th>
     *   <th>result</th>
     *  </tr>
     *  <tr>
     *   <td>null</td>
     *   <td>*</td>
     *   <td>*</td>
     *   <td>true/false</td>
     *   <td>null</td>
     *  </tr>
     *  <tr>
     *   <td>""</td>
     *   <td>*</td>
     *   <td>*</td>
     *   <td>true/false</td>
     *   <td>""</td>
     *  </tr>
     *  <tr>
     *   <td>"Here is one line of text that is going to be wrapped after 20 columns."</td>
     *   <td>20</td>
     *   <td>"\n"</td>
     *   <td>true/false</td>
     *   <td>"Here is one line of\ntext that is going\nto be wrapped after\n20 columns."</td>
     *  </tr>
     *  <tr>
     *   <td>"Here is one line of text that is going to be wrapped after 20 columns."</td>
     *   <td>20</td>
     *   <td>"&lt;br /&gt;"</td>
     *   <td>true/false</td>
     *   <td>"Here is one line of&lt;br /&gt;text that is going&lt;br /&gt;to be wrapped after&lt;br /&gt;20 columns."</td>
     *  </tr>
     *  <tr>
     *   <td>"Here is one line of text that is going to be wrapped after 20 columns."</td>
     *   <td>20</td>
     *   <td>null</td>
     *   <td>true/false</td>
     *   <td>"Here is one line of" + systemNewLine + "text that is going" + systemNewLine + "to be wrapped after" + systemNewLine + "20 columns."</td>
     *  </tr>
     *  <tr>
     *   <td>"Click here to jump to the commons website - https://commons.apache.org"</td>
     *   <td>20</td>
     *   <td>"\n"</td>
     *   <td>false</td>
     *   <td>"Click here to jump\nto the commons\nwebsite -\nhttps://commons.apache.org"</td>
     *  </tr>
     *  <tr>
     *   <td>"Click here to jump to the commons website - https://commons.apache.org"</td>
     *   <td>20</td>
     *   <td>"\n"</td>
     *   <td>true</td>
     *   <td>"Click here to jump\nto the commons\nwebsite -\nhttps://commons.apach\ne.org"</td>
     *  </tr>
     * </table>
     *
     * @param str  the String to be word wrapped, may be null.
     * @param wrapLength  the column to wrap the words at, less than 1 is treated as 1.
     * @param newLineStr  the string to insert for a new line,
     *  {@code null} uses the system property line separator.
     * @param wrapLongWords  true if long words (such as URLs) should be wrapped.
     * @return a line with newlines inserted, {@code null} if null input.
     */
    public static String wrap(final String str, final int wrapLength, final String newLineStr, final boolean wrapLongWords) {
        return wrap(str, wrapLength, newLineStr, wrapLongWords, " ");
    }

    /**
     * Wraps a single line of text, identifying words by {@code wrapOn}.
     *
     * <p>Leading spaces on a new line are stripped.
     * Trailing spaces are not stripped.</p>
     *
     * <table border="1">
     *  <caption>Examples</caption>
     *  <tr>
     *   <th>input</th>
     *   <th>wrapLength</th>
     *   <th>newLineString</th>
     *   <th>wrapLongWords</th>
     *   <th>wrapOn</th>
     *   <th>result</th>
     *  </tr>
     *  <tr>
     *   <td>null</td>
     *   <td>*</td>
     *   <td>*</td>
     *   <td>true/false</td>
     *   <td>*</td>
     *   <td>null</td>
     *  </tr>
     *  <tr>
     *   <td>""</td>
     *   <td>*</td>
     *   <td>*</td>
     *   <td>true/false</td>
     *   <td>*</td>
     *   <td>""</td>
     *  </tr>
     *  <tr>
     *   <td>"Here is one line of text that is going to be wrapped after 20 columns."</td>
     *   <td>20</td>
     *   <td>"\n"</td>
     *   <td>true/false</td>
     *   <td>" "</td>
     *   <td>"Here is one line of\ntext that is going\nto be wrapped after\n20 columns."</td>
     *  </tr>
     *  <tr>
     *   <td>"Here is one line of text that is going to be wrapped after 20 columns."</td>
     *   <td>20</td>
     *   <td>"&lt;br /&gt;"</td>
     *   <td>true/false</td>
     *   <td>" "</td>
     *   <td>"Here is one line of&lt;br /&gt;text that is going&lt;br /&gt;to be wrapped after&lt;br /&gt;20 columns."</td>
     *  </tr>
     *  <tr>
     *   <td>"Here is one line of text that is going to be wrapped after 20 columns."</td>
     *   <td>20</td>
     *   <td>null</td>
     *   <td>true/false</td>
     *   <td>" "</td>
     *   <td>"Here is one line of" + systemNewLine + "text that is going" + systemNewLine + "to be wrapped after" + systemNewLine + "20 columns."</td>
     *  </tr>
     *  <tr>
     *   <td>"Click here to jump to the commons website - https://commons.apache.org"</td>
     *   <td>20</td>
     *   <td>"\n"</td>
     *   <td>false</td>
     *   <td>" "</td>
     *   <td>"Click here to jump\nto the commons\nwebsite -\nhttps://commons.apache.org"</td>
     *  </tr>
     *  <tr>
     *   <td>"Click here to jump to the commons website - https://commons.apache.org"</td>
     *   <td>20</td>
     *   <td>"\n"</td>
     *   <td>true</td>
     *   <td>" "</td>
     *   <td>"Click here to jump\nto the commons\nwebsite -\nhttps://commons.apach\ne.org"</td>
     *  </tr>
     *  <tr>
     *   <td>"flammable/inflammable"</td>
     *   <td>20</td>
     *   <td>"\n"</td>
     *   <td>true</td>
     *   <td>"/"</td>
     *   <td>"flammable\ninflammable"</td>
     *  </tr>
     * </table>
     * @param str  the String to be word wrapped, may be null.
     * @param wrapLength  the column to wrap the words at, less than 1 is treated as 1.
     * @param newLineStr  the string to insert for a new line,
     *  {@code null} uses the system property line separator.
     * @param wrapLongWords  true if long words (such as URLs) should be wrapped.
     * @param wrapOn regex expression to be used as a breakable characters,
     *               if blank string is provided a space character will be used.
     * @return a line with newlines inserted, {@code null} if null input.
     */
    public static String wrap(final String str, int wrapLength, String newLineStr, final boolean wrapLongWords, String wrapOn) {
        if (str == null) {
            return null;
        }
        if (newLineStr == null) {
            newLineStr = System.lineSeparator();
        }
        if (wrapLength < 1) {
            wrapLength = 1;
        }
        if (StringUtils.isBlank(wrapOn)) {
            wrapOn = " ";
        }
        final Pattern patternToWrapOn = Pattern.compile(wrapOn);
        final int inputLineLength = str.length();
        int offset = 0;
        final StringBuilder wrappedLine = new StringBuilder(inputLineLength + 32);

        while (offset < inputLineLength) {
            int spaceToWrapAt = -1;
            Matcher matcher = patternToWrapOn.matcher(
                str.substring(offset, Math.min((int) Math.min(Integer.MAX_VALUE, offset + wrapLength + 1L), inputLineLength)));
            if (matcher.find()) {
                if (matcher.start() == 0) {
                    offset += matcher.end();
                    continue;
                }
                spaceToWrapAt = matcher.start() + offset;
            }

            // only last line without leading spaces is left
            if (inputLineLength - offset <= wrapLength) {
                break;
            }

            while (matcher.find()) {
                spaceToWrapAt = matcher.start() + offset;
            }

            if (spaceToWrapAt >= offset) {
                // normal case
                wrappedLine.append(str, offset, spaceToWrapAt);
                wrappedLine.append(newLineStr);
                offset = spaceToWrapAt + 1;

            } else // really long word or URL
            if (wrapLongWords) {
                // wrap really long word one line at a time
                wrappedLine.append(str, offset, wrapLength + offset);
                wrappedLine.append(newLineStr);
                offset += wrapLength;
            } else {
                // do not wrap really long word, just extend beyond limit
                matcher = patternToWrapOn.matcher(str.substring(offset + wrapLength));
                if (matcher.find()) {
                    spaceToWrapAt = matcher.start() + offset + wrapLength;
                }

                if (spaceToWrapAt >= 0) {
                    wrappedLine.append(str, offset, spaceToWrapAt);
                    wrappedLine.append(newLineStr);
                    offset = spaceToWrapAt + 1;
                } else {
                    wrappedLine.append(str, offset, str.length());
                    offset = inputLineLength;
                }
            }
        }

        // Whatever is left in line is short enough to just pass through
        wrappedLine.append(str, offset, str.length());

        return wrappedLine.toString();
    }

    /**
     * {@link WordUtils} instances should NOT be constructed in
     * standard programming. Instead, the class should be used as
     * {@code WordUtils.wrap("foo bar", 20);}.
     *
     * <p>This constructor is public to permit tools that require a JavaBean
     * instance to operate.</p>
     */
    public WordUtils() {
    }

}
