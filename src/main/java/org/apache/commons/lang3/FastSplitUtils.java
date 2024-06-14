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

import java.util.Arrays;

/**
 * This class is an optimized version of string split methods <br>
 * The behaviours of the following methods are guaranteed to be the <strong>SAME</strong> as StringUtils:<br>
 * <pre>
 * {@link #split(String, char)} &lt;-  {@link StringUtils#split(String, char)}
 * {@link #split(String, String)} &lt;- {@link StringUtils#split(String, char)}
 * {@link #split(String, String, int)} &lt;- {@link StringUtils#split(String, String, int)}
 * {@link #splitByWholeSeparator(String, String)} &lt;- {@link StringUtils#splitByWholeSeparator(String, String)}
 * {@link #splitByWholeSeparator(String, String, int)} &lt;- {@link StringUtils#splitByWholeSeparator(String, String, int)}
 * {@link #splitByCharacterType(String)} &lt;- {@link StringUtils#splitByCharacterType(String)}
 * {@link #splitPreserveAllTokens(String, char)} &lt;- {@link StringUtils#splitPreserveAllTokens(String, char)}
 * {@link #splitPreserveAllTokens(String, String)} &lt;- {@link StringUtils#splitPreserveAllTokens(String, String)}
 * {@link #splitPreserveAllTokens(String, String, int)} &lt;- {@link StringUtils#splitPreserveAllTokens(String, String, int)}
 * {@link #splitByWholeSeparatorPreserveAllTokens(String, String)} &lt;- {@link StringUtils#splitByWholeSeparatorPreserveAllTokens(String, String)}
 * {@link #splitByWholeSeparatorPreserveAllTokens(String, String, int)} &lt;- {@link StringUtils#splitByWholeSeparatorPreserveAllTokens(String, String, int)}
 * {@link #splitByCharacterTypeCamelCase(String)} &lt;- {@link StringUtils#splitByCharacterTypeCamelCase(String)}
 * </pre>
 * However, the performances of them are supposed to be better.<br>
 * There is a inner buffer {@link SplitBuffer} hold by a {@link ThreadLocal} instance to reduce array allocation.<br>
 * This class is <strong>ThreadSafe</strong>, but NOT guaranteed to be <strong>FiberSafe</strong>.<br>
 *
 * @since 3.10
 **/
public class FastSplitUtils {

    private static final String EMPTY = "";

    private static final ThreadLocal<SplitBufferThreadLocalHelper> SPLIT_BUFFER_THREAD_LOCAL = ThreadLocal.withInitial(SplitBufferThreadLocalHelper::new);

    // Splitting
    //-----------------------------------------------------------------------

    /**
     * <p>FastSplitUtils instances should NOT be constructed in standard programming.
     * Instead, the class should be used as <code>FastSplitUtils.split("a.b.c", '.')</code>.
     *
     * <p>This constructor is public to permit tools that require a JavaBean instance
     * to operate.
     */
    public FastSplitUtils() {
        super();
    }

    /**
     * <p>Splits the provided text into an array, using whitespace as the
     * separator.
     * Whitespace is defined by {@link Character#isWhitespace(char)}.</p>
     *
     * <p>The separator is not included in the returned String array.
     * Adjacent separators are treated as one separator.
     * For more control over the split use the StrTokenizer class.</p>
     *
     * <p>A {@code null} input String returns {@code null}.</p>
     *
     * <pre>
     * FastSplitUtils.split(null)       = null
     * FastSplitUtils.split("")         = []
     * FastSplitUtils.split("abc def")  = ["abc", "def"]
     * FastSplitUtils.split("abc  def") = ["abc", "def"]
     * FastSplitUtils.split(" abc ")    = ["abc"]
     * </pre>
     *
     * @param str the String to parse, may be null
     * @return an array of parsed Strings, {@code null} if null String input
     */
    public static String[] split(final String str) {
        return split(str, null, -1);
    }

    /**
     * <p>Splits the provided text into an array, separator specified.
     * This is an alternative to using StringTokenizer.</p>
     *
     * <p>The separator is not included in the returned String array.
     * Adjacent separators are treated as one separator.
     * For more control over the split use the StrTokenizer class.</p>
     *
     * <p>A {@code null} input String returns {@code null}.</p>
     *
     * <pre>
     * FastSplitUtils.split(null, *)         = null
     * FastSplitUtils.split("", *)           = []
     * FastSplitUtils.split("a.b.c", '.')    = ["a", "b", "c"]
     * FastSplitUtils.split("a..b.c", '.')   = ["a", "b", "c"]
     * FastSplitUtils.split("a:b:c", '.')    = ["a:b:c"]
     * FastSplitUtils.split("a b c", ' ')    = ["a", "b", "c"]
     * </pre>
     *
     * @param str           the String to parse, may be null
     * @param separatorChar the character used as the delimiter
     * @return an array of parsed Strings, {@code null} if null String input
     * @since 3.10
     */
    public static String[] split(final String str, final char separatorChar) {
        return splitWorker(str, separatorChar, false);
    }

    /**
     * <p>Splits the provided text into an array, separators specified.
     * This is an alternative to using StringTokenizer.</p>
     *
     * <p>The separator is not included in the returned String array.
     * Adjacent separators are treated as one separator.
     * For more control over the split use the StrTokenizer class.</p>
     *
     * <p>A {@code null} input String returns {@code null}.
     * A {@code null} separatorChars splits on whitespace.</p>
     *
     * <pre>
     * FastSplitUtils.split(null, *)         = null
     * FastSplitUtils.split("", *)           = []
     * FastSplitUtils.split("abc def", null) = ["abc", "def"]
     * FastSplitUtils.split("abc def", " ")  = ["abc", "def"]
     * FastSplitUtils.split("abc  def", " ") = ["abc", "def"]
     * FastSplitUtils.split("ab:cd:ef", ":") = ["ab", "cd", "ef"]
     * </pre>
     *
     * @param str            the String to parse, may be null
     * @param separatorChars the characters used as the delimiters,
     *                       {@code null} splits on whitespace
     * @return an array of parsed Strings, {@code null} if null String input
     */
    public static String[] split(final String str, final String separatorChars) {
        return splitWorker(str, separatorChars, -1, false);
    }

    /**
     * <p>Splits the provided text into an array with a maximum length,
     * separators specified.</p>
     *
     * <p>The separator is not included in the returned String array.
     * Adjacent separators are treated as one separator.</p>
     *
     * <p>A {@code null} input String returns {@code null}.
     * A {@code null} separatorChars splits on whitespace.</p>
     *
     * <p>If more than {@code max} delimited substrings are found, the last
     * returned string includes all characters after the first {@code max - 1}
     * returned strings (including separator characters).</p>
     *
     * <pre>
     * FastSplitUtils.split(null, *, *)            = null
     * FastSplitUtils.split("", *, *)              = []
     * FastSplitUtils.split("ab cd ef", null, 0)   = ["ab", "cd", "ef"]
     * FastSplitUtils.split("ab   cd ef", null, 0) = ["ab", "cd", "ef"]
     * FastSplitUtils.split("ab:cd:ef", ":", 0)    = ["ab", "cd", "ef"]
     * FastSplitUtils.split("ab:cd:ef", ":", 2)    = ["ab", "cd:ef"]
     * </pre>
     *
     * @param str            the String to parse, may be null
     * @param separatorChars the characters used as the delimiters,
     *                       {@code null} splits on whitespace
     * @param max            the maximum number of elements to include in the
     *                       array. A zero or negative value implies no limit
     * @return an array of parsed Strings, {@code null} if null String input
     */
    public static String[] split(final String str, final String separatorChars, final int max) {
        return splitWorker(str, separatorChars, max, false);
    }

    /**
     * <p>Splits a String by Character type as returned by
     * {@code java.lang.Character.getType(char)}. Groups of contiguous
     * characters of the same type are returned as complete tokens.
     * <pre>
     * FastSplitUtils.splitByCharacterType(null)         = null
     * FastSplitUtils.splitByCharacterType("")           = []
     * FastSplitUtils.splitByCharacterType("ab de fg")   = ["ab", " ", "de", " ", "fg"]
     * FastSplitUtils.splitByCharacterType("ab   de fg") = ["ab", "   ", "de", " ", "fg"]
     * FastSplitUtils.splitByCharacterType("ab:cd:ef")   = ["ab", ":", "cd", ":", "ef"]
     * FastSplitUtils.splitByCharacterType("number5")    = ["number", "5"]
     * FastSplitUtils.splitByCharacterType("fooBar")     = ["foo", "B", "ar"]
     * FastSplitUtils.splitByCharacterType("foo200Bar")  = ["foo", "200", "B", "ar"]
     * FastSplitUtils.splitByCharacterType("ASFRules")   = ["ASFR", "ules"]
     * </pre>
     *
     * @param str the String to split, may be {@code null}
     * @return an array of parsed Strings, {@code null} if null String input
     * @since 3.10
     */
    public static String[] splitByCharacterType(final String str) {
        return splitByCharacterType(str, false);
    }

    /**
     * <p>Splits a String by Character type as returned by
     * {@code java.lang.Character.getType(char)}. Groups of contiguous
     * characters of the same type are returned as complete tokens, with the
     * following exception: if {@code camelCase} is {@code true},
     * the character of type {@code Character.UPPERCASE_LETTER}, if any,
     * immediately preceding a token of type {@code Character.LOWERCASE_LETTER}
     * will belong to the following token rather than to the preceding, if any,
     * {@code Character.UPPERCASE_LETTER} token.
     *
     * @param str       the String to split, may be {@code null}
     * @param camelCase whether to use so-called "camel-case" for letter types
     * @return an array of parsed Strings, {@code null} if null String input
     * @since 3.10
     */
    private static String[] splitByCharacterType(final String str, final boolean camelCase) {
        if (str == null) {
            return null;
        }
        if (str.isEmpty()) {
            return ArrayUtils.EMPTY_STRING_ARRAY;
        }
        final char[] c = str.toCharArray();
        final SplitBuffer buffer = SPLIT_BUFFER_THREAD_LOCAL.get().getBuffer();
        int tokenStart = 0;
        int currentType = Character.getType(c[tokenStart]);
        for (int pos = tokenStart + 1; pos < c.length; pos++) {
            final int type = Character.getType(c[pos]);
            if (type == currentType) {
                continue;
            }
            if (camelCase && type == Character.LOWERCASE_LETTER && currentType == Character.UPPERCASE_LETTER) {
                final int newTokenStart = pos - 1;
                if (newTokenStart != tokenStart) {
                    buffer.add(new String(c, tokenStart, newTokenStart - tokenStart));
                    tokenStart = newTokenStart;
                }
            } else {
                buffer.add(new String(c, tokenStart, pos - tokenStart));
                tokenStart = pos;
            }
            currentType = type;
        }
        buffer.add(new String(c, tokenStart, c.length - tokenStart));
        return buffer.toArray();
    }

    /**
     * <p>Splits a String by Character type as returned by
     * {@code java.lang.Character.getType(char)}. Groups of contiguous
     * characters of the same type are returned as complete tokens, with the
     * following exception: the character of type
     * {@code Character.UPPERCASE_LETTER}, if any, immediately
     * preceding a token of type {@code Character.LOWERCASE_LETTER}
     * will belong to the following token rather than to the preceding, if any,
     * {@code Character.UPPERCASE_LETTER} token.
     * <pre>
     * FastSplitUtils.splitByCharacterTypeCamelCase(null)         = null
     * FastSplitUtils.splitByCharacterTypeCamelCase("")           = []
     * FastSplitUtils.splitByCharacterTypeCamelCase("ab de fg")   = ["ab", " ", "de", " ", "fg"]
     * FastSplitUtils.splitByCharacterTypeCamelCase("ab   de fg") = ["ab", "   ", "de", " ", "fg"]
     * FastSplitUtils.splitByCharacterTypeCamelCase("ab:cd:ef")   = ["ab", ":", "cd", ":", "ef"]
     * FastSplitUtils.splitByCharacterTypeCamelCase("number5")    = ["number", "5"]
     * FastSplitUtils.splitByCharacterTypeCamelCase("fooBar")     = ["foo", "Bar"]
     * FastSplitUtils.splitByCharacterTypeCamelCase("foo200Bar")  = ["foo", "200", "Bar"]
     * FastSplitUtils.splitByCharacterTypeCamelCase("ASFRules")   = ["ASF", "Rules"]
     * </pre>
     *
     * @param str the String to split, may be {@code null}
     * @return an array of parsed Strings, {@code null} if null String input
     * @since 3.10
     */
    public static String[] splitByCharacterTypeCamelCase(final String str) {
        return splitByCharacterType(str, true);
    }

    /**
     * <p>Splits the provided text into an array, separator string specified.</p>
     *
     * <p>The separator(s) will not be included in the returned String array.
     * Adjacent separators are treated as one separator.</p>
     *
     * <p>A {@code null} input String returns {@code null}.
     * A {@code null} separator splits on whitespace.</p>
     *
     * <pre>
     * FastSplitUtils.splitByWholeSeparator(null, *)               = null
     * FastSplitUtils.splitByWholeSeparator("", *)                 = []
     * FastSplitUtils.splitByWholeSeparator("ab de fg", null)      = ["ab", "de", "fg"]
     * FastSplitUtils.splitByWholeSeparator("ab   de fg", null)    = ["ab", "de", "fg"]
     * FastSplitUtils.splitByWholeSeparator("ab:cd:ef", ":")       = ["ab", "cd", "ef"]
     * FastSplitUtils.splitByWholeSeparator("ab-!-cd-!-ef", "-!-") = ["ab", "cd", "ef"]
     * </pre>
     *
     * @param str       the String to parse, may be null
     * @param separator String containing the String to be used as a delimiter,
     *                  {@code null} splits on whitespace
     * @return an array of parsed Strings, {@code null} if null String was input
     */
    public static String[] splitByWholeSeparator(final String str, final String separator) {
        return splitByWholeSeparatorWorker(str, separator, -1, false);
    }

    /**
     * <p>Splits the provided text into an array, separator string specified.
     * Returns a maximum of {@code max} substrings.</p>
     *
     * <p>The separator(s) will not be included in the returned String array.
     * Adjacent separators are treated as one separator.</p>
     *
     * <p>A {@code null} input String returns {@code null}.
     * A {@code null} separator splits on whitespace.</p>
     *
     * <pre>
     * FastSplitUtils.splitByWholeSeparator(null, *, *)               = null
     * FastSplitUtils.splitByWholeSeparator("", *, *)                 = []
     * FastSplitUtils.splitByWholeSeparator("ab de fg", null, 0)      = ["ab", "de", "fg"]
     * FastSplitUtils.splitByWholeSeparator("ab   de fg", null, 0)    = ["ab", "de", "fg"]
     * FastSplitUtils.splitByWholeSeparator("ab:cd:ef", ":", 2)       = ["ab", "cd:ef"]
     * FastSplitUtils.splitByWholeSeparator("ab-!-cd-!-ef", "-!-", 5) = ["ab", "cd", "ef"]
     * FastSplitUtils.splitByWholeSeparator("ab-!-cd-!-ef", "-!-", 2) = ["ab", "cd-!-ef"]
     * </pre>
     *
     * @param str       the String to parse, may be null
     * @param separator String containing the String to be used as a delimiter,
     *                  {@code null} splits on whitespace
     * @param max       the maximum number of elements to include in the returned
     *                  array. A zero or negative value implies no limit.
     * @return an array of parsed Strings, {@code null} if null String was input
     */
    public static String[] splitByWholeSeparator(final String str, final String separator, final int max) {
        return splitByWholeSeparatorWorker(str, separator, max, false);
    }

    /**
     * <p>Splits the provided text into an array, separator string specified. </p>
     *
     * <p>The separator is not included in the returned String array.
     * Adjacent separators are treated as separators for empty tokens.
     * For more control over the split use the StrTokenizer class.</p>
     *
     * <p>A {@code null} input String returns {@code null}.
     * A {@code null} separator splits on whitespace.</p>
     *
     * <pre>
     * FastSplitUtils.splitByWholeSeparatorPreserveAllTokens(null, *)               = null
     * FastSplitUtils.splitByWholeSeparatorPreserveAllTokens("", *)                 = []
     * FastSplitUtils.splitByWholeSeparatorPreserveAllTokens("ab de fg", null)      = ["ab", "de", "fg"]
     * FastSplitUtils.splitByWholeSeparatorPreserveAllTokens("ab   de fg", null)    = ["ab", "", "", "de", "fg"]
     * FastSplitUtils.splitByWholeSeparatorPreserveAllTokens("ab:cd:ef", ":")       = ["ab", "cd", "ef"]
     * FastSplitUtils.splitByWholeSeparatorPreserveAllTokens("ab-!-cd-!-ef", "-!-") = ["ab", "cd", "ef"]
     * </pre>
     *
     * @param str       the String to parse, may be null
     * @param separator String containing the String to be used as a delimiter,
     *                  {@code null} splits on whitespace
     * @return an array of parsed Strings, {@code null} if null String was input
     * @since 3.10
     */
    public static String[] splitByWholeSeparatorPreserveAllTokens(final String str, final String separator) {
        return splitByWholeSeparatorWorker(str, separator, -1, true);
    }

    /**
     * <p>Splits the provided text into an array, separator string specified.
     * Returns a maximum of {@code max} substrings.</p>
     *
     * <p>The separator is not included in the returned String array.
     * Adjacent separators are treated as separators for empty tokens.
     * For more control over the split use the StrTokenizer class.</p>
     *
     * <p>A {@code null} input String returns {@code null}.
     * A {@code null} separator splits on whitespace.</p>
     *
     * <pre>
     * FastSplitUtils.splitByWholeSeparatorPreserveAllTokens(null, *, *)               = null
     * FastSplitUtils.splitByWholeSeparatorPreserveAllTokens("", *, *)                 = []
     * FastSplitUtils.splitByWholeSeparatorPreserveAllTokens("ab de fg", null, 0)      = ["ab", "de", "fg"]
     * FastSplitUtils.splitByWholeSeparatorPreserveAllTokens("ab   de fg", null, 0)    = ["ab", "", "", "de", "fg"]
     * FastSplitUtils.splitByWholeSeparatorPreserveAllTokens("ab:cd:ef", ":", 2)       = ["ab", "cd:ef"]
     * FastSplitUtils.splitByWholeSeparatorPreserveAllTokens("ab-!-cd-!-ef", "-!-", 5) = ["ab", "cd", "ef"]
     * FastSplitUtils.splitByWholeSeparatorPreserveAllTokens("ab-!-cd-!-ef", "-!-", 2) = ["ab", "cd-!-ef"]
     * </pre>
     *
     * @param str       the String to parse, may be null
     * @param separator String containing the String to be used as a delimiter,
     *                  {@code null} splits on whitespace
     * @param max       the maximum number of elements to include in the returned
     *                  array. A zero or negative value implies no limit.
     * @return an array of parsed Strings, {@code null} if null String was input
     * @since 3.10
     */
    public static String[] splitByWholeSeparatorPreserveAllTokens(final String str, final String separator, final int max) {
        return splitByWholeSeparatorWorker(str, separator, max, true);
    }

    // -----------------------------------------------------------------------

    /**
     * Performs the logic for the {@code splitByWholeSeparatorPreserveAllTokens} methods.
     *
     * @param str               the String to parse, may be {@code null}
     * @param separator         String containing the String to be used as a delimiter,
     *                          {@code null} splits on whitespace
     * @param max               the maximum number of elements to include in the returned
     *                          array. A zero or negative value implies no limit.
     * @param preserveAllTokens if {@code true}, adjacent separators are
     *                          treated as empty token separators; if {@code false}, adjacent
     *                          separators are treated as one separator.
     * @return an array of parsed Strings, {@code null} if null String input
     * @since 3.10
     */
    private static String[] splitByWholeSeparatorWorker(
            final String str, final String separator, final int max, final boolean preserveAllTokens) {
        if (str == null) {
            return null;
        }

        final int len = str.length();

        if (len == 0) {
            return ArrayUtils.EMPTY_STRING_ARRAY;
        }

        if (separator == null || EMPTY.equals(separator)) {
            // Split on whitespace.
            return splitWorker(str, null, max, preserveAllTokens);
        }

        final int separatorLength = separator.length();

        final SplitBuffer substrings = SPLIT_BUFFER_THREAD_LOCAL.get().getBuffer();
        int numberOfSubstrings = 0;
        int beg = 0;
        int end = 0;
        while (end < len) {
            end = str.indexOf(separator, beg);

            if (end > -1) {
                if (end > beg) {
                    numberOfSubstrings += 1;

                    if (numberOfSubstrings == max) {
                        end = len;
                        substrings.add(str.substring(beg));
                    } else {
                        // The following is OK, because String.substring( beg, end ) excludes
                        // the character at the position 'end'.
                        substrings.add(str.substring(beg, end));

                        // Set the starting point for the next search.
                        // The following is equivalent to beg = end + (separatorLength - 1) + 1,
                        // which is the right calculation:
                        beg = end + separatorLength;
                    }
                } else {
                    // We found a consecutive occurrence of the separator, so skip it.
                    if (preserveAllTokens) {
                        numberOfSubstrings += 1;
                        if (numberOfSubstrings == max) {
                            end = len;
                            substrings.add(str.substring(beg));
                        } else {
                            substrings.add(EMPTY);
                        }
                    }
                    beg = end + separatorLength;
                }

            } else {
                // String.substring( beg ) goes from 'beg' to the end of the String.
                substrings.add(str.substring(beg));
                end = len;
            }
        }

        return substrings.toArray();
    }

    /**
     * <p>Splits the provided text into an array, using whitespace as the
     * separator, preserving all tokens, including empty tokens created by
     * adjacent separators. This is an alternative to using StringTokenizer.
     * Whitespace is defined by {@link Character#isWhitespace(char)}.</p>
     *
     * <p>The separator is not included in the returned String array.
     * Adjacent separators are treated as separators for empty tokens.
     * For more control over the split use the StrTokenizer class.</p>
     *
     * <p>A {@code null} input String returns {@code null}.</p>
     *
     * <pre>
     * FastSplitUtils.splitPreserveAllTokens(null)       = null
     * FastSplitUtils.splitPreserveAllTokens("")         = []
     * FastSplitUtils.splitPreserveAllTokens("abc def")  = ["abc", "def"]
     * FastSplitUtils.splitPreserveAllTokens("abc  def") = ["abc", "", "def"]
     * FastSplitUtils.splitPreserveAllTokens(" abc ")    = ["", "abc", ""]
     * </pre>
     *
     * @param str the String to parse, may be {@code null}
     * @return an array of parsed Strings, {@code null} if null String input
     * @since 3.10
     */
    public static String[] splitPreserveAllTokens(final String str) {
        return splitWorker(str, null, -1, true);
    }

    /**
     * <p>Splits the provided text into an array, separator specified,
     * preserving all tokens, including empty tokens created by adjacent
     * separators. This is an alternative to using StringTokenizer.</p>
     *
     * <p>The separator is not included in the returned String array.
     * Adjacent separators are treated as separators for empty tokens.
     * For more control over the split use the StrTokenizer class.</p>
     *
     * <p>A {@code null} input String returns {@code null}.</p>
     *
     * <pre>
     * FastSplitUtils.splitPreserveAllTokens(null, *)         = null
     * FastSplitUtils.splitPreserveAllTokens("", *)           = []
     * FastSplitUtils.splitPreserveAllTokens("a.b.c", '.')    = ["a", "b", "c"]
     * FastSplitUtils.splitPreserveAllTokens("a..b.c", '.')   = ["a", "", "b", "c"]
     * FastSplitUtils.splitPreserveAllTokens("a:b:c", '.')    = ["a:b:c"]
     * FastSplitUtils.splitPreserveAllTokens("a\tb\nc", null) = ["a", "b", "c"]
     * FastSplitUtils.splitPreserveAllTokens("a b c", ' ')    = ["a", "b", "c"]
     * FastSplitUtils.splitPreserveAllTokens("a b c ", ' ')   = ["a", "b", "c", ""]
     * FastSplitUtils.splitPreserveAllTokens("a b c  ", ' ')   = ["a", "b", "c", "", ""]
     * FastSplitUtils.splitPreserveAllTokens(" a b c", ' ')   = ["", a", "b", "c"]
     * FastSplitUtils.splitPreserveAllTokens("  a b c", ' ')  = ["", "", a", "b", "c"]
     * FastSplitUtils.splitPreserveAllTokens(" a b c ", ' ')  = ["", a", "b", "c", ""]
     * </pre>
     *
     * @param str           the String to parse, may be {@code null}
     * @param separatorChar the character used as the delimiter,
     *                      {@code null} splits on whitespace
     * @return an array of parsed Strings, {@code null} if null String input
     * @since 3.10
     */
    public static String[] splitPreserveAllTokens(final String str, final char separatorChar) {
        return splitWorker(str, separatorChar, true);
    }

    /**
     * <p>Splits the provided text into an array, separators specified,
     * preserving all tokens, including empty tokens created by adjacent
     * separators. This is an alternative to using StringTokenizer.</p>
     *
     * <p>The separator is not included in the returned String array.
     * Adjacent separators are treated as separators for empty tokens.
     * For more control over the split use the StrTokenizer class.</p>
     *
     * <p>A {@code null} input String returns {@code null}.
     * A {@code null} separatorChars splits on whitespace.</p>
     *
     * <pre>
     * FastSplitUtils.splitPreserveAllTokens(null, *)           = null
     * FastSplitUtils.splitPreserveAllTokens("", *)             = []
     * FastSplitUtils.splitPreserveAllTokens("abc def", null)   = ["abc", "def"]
     * FastSplitUtils.splitPreserveAllTokens("abc def", " ")    = ["abc", "def"]
     * FastSplitUtils.splitPreserveAllTokens("abc  def", " ")   = ["abc", "", def"]
     * FastSplitUtils.splitPreserveAllTokens("ab:cd:ef", ":")   = ["ab", "cd", "ef"]
     * FastSplitUtils.splitPreserveAllTokens("ab:cd:ef:", ":")  = ["ab", "cd", "ef", ""]
     * FastSplitUtils.splitPreserveAllTokens("ab:cd:ef::", ":") = ["ab", "cd", "ef", "", ""]
     * FastSplitUtils.splitPreserveAllTokens("ab::cd:ef", ":")  = ["ab", "", cd", "ef"]
     * FastSplitUtils.splitPreserveAllTokens(":cd:ef", ":")     = ["", cd", "ef"]
     * FastSplitUtils.splitPreserveAllTokens("::cd:ef", ":")    = ["", "", cd", "ef"]
     * FastSplitUtils.splitPreserveAllTokens(":cd:ef:", ":")    = ["", cd", "ef", ""]
     * </pre>
     *
     * @param str            the String to parse, may be {@code null}
     * @param separatorChars the characters used as the delimiters,
     *                       {@code null} splits on whitespace
     * @return an array of parsed Strings, {@code null} if null String input
     * @since 3.10
     */
    public static String[] splitPreserveAllTokens(final String str, final String separatorChars) {
        return splitWorker(str, separatorChars, -1, true);
    }

    /**
     * <p>Splits the provided text into an array with a maximum length,
     * separators specified, preserving all tokens, including empty tokens
     * created by adjacent separators.</p>
     *
     * <p>The separator is not included in the returned String array.
     * Adjacent separators are treated as separators for empty tokens.
     * Adjacent separators are treated as one separator.</p>
     *
     * <p>A {@code null} input String returns {@code null}.
     * A {@code null} separatorChars splits on whitespace.</p>
     *
     * <p>If more than {@code max} delimited substrings are found, the last
     * returned string includes all characters after the first {@code max - 1}
     * returned strings (including separator characters).</p>
     *
     * <pre>
     * FastSplitUtils.splitPreserveAllTokens(null, *, *)            = null
     * FastSplitUtils.splitPreserveAllTokens("", *, *)              = []
     * FastSplitUtils.splitPreserveAllTokens("ab de fg", null, 0)   = ["ab", "de", "fg"]
     * FastSplitUtils.splitPreserveAllTokens("ab   de fg", null, 0) = ["ab", "", "", "de", "fg"]
     * FastSplitUtils.splitPreserveAllTokens("ab:cd:ef", ":", 0)    = ["ab", "cd", "ef"]
     * FastSplitUtils.splitPreserveAllTokens("ab:cd:ef", ":", 2)    = ["ab", "cd:ef"]
     * FastSplitUtils.splitPreserveAllTokens("ab   de fg", null, 2) = ["ab", "  de fg"]
     * FastSplitUtils.splitPreserveAllTokens("ab   de fg", null, 3) = ["ab", "", " de fg"]
     * FastSplitUtils.splitPreserveAllTokens("ab   de fg", null, 4) = ["ab", "", "", "de fg"]
     * </pre>
     *
     * @param str            the String to parse, may be {@code null}
     * @param separatorChars the characters used as the delimiters,
     *                       {@code null} splits on whitespace
     * @param max            the maximum number of elements to include in the
     *                       array. A zero or negative value implies no limit
     * @return an array of parsed Strings, {@code null} if null String input
     * @since 3.10
     */
    public static String[] splitPreserveAllTokens(final String str, final String separatorChars, final int max) {
        return splitWorker(str, separatorChars, max, true);
    }

    /**
     * Performs the logic for the {@code split} and
     * {@code splitPreserveAllTokens} methods that do not return a
     * maximum array length.
     *
     * @param str               the String to parse, may be {@code null}
     * @param separatorChar     the separate character
     * @param preserveAllTokens if {@code true}, adjacent separators are
     *                          treated as empty token separators; if {@code false}, adjacent
     *                          separators are treated as one separator.
     * @return an array of parsed Strings, {@code null} if null String input
     */
    private static String[] splitWorker(final String str, final char separatorChar, final boolean preserveAllTokens) {
        // Performance tuned for 2.0 (JDK1.4)

        if (str == null) {
            return null;
        }
        final int len = str.length();
        if (len == 0) {
            return ArrayUtils.EMPTY_STRING_ARRAY;
        }
        final SplitBuffer buffer = SPLIT_BUFFER_THREAD_LOCAL.get().getBuffer();
        int fromIndex = -1, toIndex = str.indexOf(separatorChar);
        while (toIndex > fromIndex) {
            if (toIndex > fromIndex + 1 || preserveAllTokens) {
                String token = str.substring(fromIndex + 1, toIndex);
                buffer.add(token);
            }
            fromIndex = toIndex;
            toIndex = str.indexOf(separatorChar, fromIndex + 1);
        }
        if (len > fromIndex + 1 || preserveAllTokens) {
            buffer.add(str.substring(fromIndex + 1));
        }
        return buffer.toArray();
    }

    /**
     * Performs the logic for the {@code split} and
     * {@code splitPreserveAllTokens} methods that return a maximum array
     * length.
     *
     * @param str               the String to parse, may be {@code null}
     * @param separatorChars    the separate character
     * @param max               the maximum number of elements to include in the
     *                          array. A zero or negative value implies no limit.
     * @param preserveAllTokens if {@code true}, adjacent separators are
     *                          treated as empty token separators; if {@code false}, adjacent
     *                          separators are treated as one separator.
     * @return an array of parsed Strings, {@code null} if null String input
     */
    private static String[] splitWorker(final String str, final String separatorChars, final int max, final boolean preserveAllTokens) {
        // Performance tuned for 2.0 (JDK1.4)
        // Direct code is quicker than StringTokenizer.
        // Also, StringTokenizer uses isSpace() not isWhitespace()

        if (str == null) {
            return null;
        }
        final int len = str.length();
        if (len == 0) {
            return ArrayUtils.EMPTY_STRING_ARRAY;
        }
        final SplitBuffer buffer = SPLIT_BUFFER_THREAD_LOCAL.get().getBuffer();
        int sizePlus1 = 1;
        int i = 0, start = 0;
        boolean match = false;
        boolean lastMatch = false;
        if (separatorChars == null) {// Null separator means use whitespace
            while (i < len) {
                if (Character.isWhitespace(str.charAt(i))) {
                    if (match || preserveAllTokens) {
                        lastMatch = true;
                        if (sizePlus1++ == max) {
                            i = len;
                            lastMatch = false;
                        }
                        buffer.add(str.substring(start, i));
                        match = false;
                    }
                    start = ++i;
                    continue;
                }
                lastMatch = false;
                match = true;
                i++;
            }
        } else if (separatorChars.length() == 1) {
            // Optimise 1 character case
            final char sep = separatorChars.charAt(0);
            while (i < len) {
                if (str.charAt(i) == sep) {
                    if (match || preserveAllTokens) {
                        lastMatch = true;
                        if (sizePlus1++ == max) {
                            i = len;
                            lastMatch = false;
                        }
                        buffer.add(str.substring(start, i));
                        match = false;
                    }
                    start = ++i;
                    continue;
                }
                lastMatch = false;
                match = true;
                i++;
            }
        } else {
            // standard case
            while (i < len) {
                if (separatorChars.indexOf(str.charAt(i)) >= 0) {
                    if (match || preserveAllTokens) {
                        lastMatch = true;
                        if (sizePlus1++ == max) {
                            i = len;
                            lastMatch = false;
                        }
                        buffer.add(str.substring(start, i));
                        match = false;
                    }
                    start = ++i;
                    continue;
                }
                lastMatch = false;
                match = true;
                i++;
            }
        }
        if (match || preserveAllTokens && lastMatch) {
            buffer.add(str.substring(start, i));
        }
        return buffer.toArray();
    }

    /**
     * Private class act as a buffer while splitting.
     * "SplitBufferThreadLocalHelper" is constructed as a thread local variable so it is
     * thread safe. The "splitBuffer" field acts as a buffer to hold the temporary
     * representation of string split segments. It is shared by all
     * calls to {@link #splitByCharacterType(String, boolean)} or {@link #splitByWholeSeparatorWorker(String, String, int, boolean)}
     * or {@link #splitWorker(String, char, boolean)} or {@link #split(String, String, int)} and its variants in that particular thread.
     */
    private static final class SplitBufferThreadLocalHelper {

        private final SplitBuffer splitBuffer = new SplitBuffer();

        SplitBuffer getBuffer() {
            splitBuffer.reset();
            return splitBuffer;
        }
    }

    /**
     * A buffer class to hold split segments,
     * this class is hold by the {@link ThreadLocal} so it is thread-safe.<br>
     * While splitting, the segments will be add to the tail of the array with
     * {@link SplitBuffer#add(String)} method(If the capacity of the array is not enough, it will enable auto-expanding).<br>
     * {@link SplitBuffer#toArray()} method will copy current segments to a single String array.<br>
     * {@link SplitBuffer#reset()} method will set the length of the array to 0, however, the "elementData" array will be reused next time.<br>
     * This class is designed to replace previous {@link java.util.ArrayList}, in previous version, an ArrayList
     * is created every time the split method is called, It means array allocation every time,if the segments
     * is large, it may also contains several resizing designed by ArrayList.<br>
     * The mainly purpose of this class is to improve performance.
     */
    public static final class SplitBuffer {

        /**
         * Default initial capacity.
         */
        private static final int DEFAULT_CAPACITY = 10;

        /**
         * Shared empty array instance used for default sized empty instances. We
         * distinguish this from EMPTY_ELEMENTDATA to know how much to inflate when
         * first element is added.
         */
        private static final String[] DEFAULTCAPACITY_EMPTY_ELEMENTDATA = {};

        /**
         * The maximum size of array to allocate (unless necessary).
         * Some VMs reserve some header words in an array.
         * Attempts to allocate larger arrays may result in
         * RuntimeException : Requested array size exceeds VM limit
         */
        private static final int MAX_ARRAY_SIZE = Integer.MAX_VALUE - 8;

        private String[] elementData = DEFAULTCAPACITY_EMPTY_ELEMENTDATA;
        private int size = 0;

        private static int hugeCapacity(int minCapacity) {
            if (minCapacity < 0) {// overflow
                throw new RuntimeException("capacity overflow!");
            }
            return (minCapacity > MAX_ARRAY_SIZE)
                    ? Integer.MAX_VALUE
                    : MAX_ARRAY_SIZE;
        }

        public String get(int index) {
            return elementData[index];
        }

        public void add(String e) {
            add(e, elementData, size);
        }

        /**
         * This helper method split out from add(E) to keep method
         * bytecode size under 35 (the -XX:MaxInlineSize default value),
         * which helps when add(E) is called in a C1-compiled loop.
         */
        private void add(String e, Object[] elementData, int s) {
            if (s == elementData.length) {
                elementData = grow(size + 1);
            }
            elementData[s] = e;
            size = s + 1;
        }

        /**
         * Increases the capacity to ensure that it can hold at least the
         * number of elements specified by the minimum capacity argument.
         *
         * @param minCapacity the desired minimum capacity
         * @throws RuntimeException if minCapacity is less than zero
         */
        private String[] grow(int minCapacity) {
            String[] newElementData = new String[newCapacity(minCapacity)];
            System.arraycopy(elementData, 0, newElementData, 0, elementData.length);
            return elementData = newElementData;
        }

        /**
         * Returns a capacity at least as large as the given minimum capacity.
         * Returns the current capacity increased by 50% if that suffices.
         * Will not return a capacity greater than MAX_ARRAY_SIZE unless
         * the given minimum capacity is greater than MAX_ARRAY_SIZE.
         *
         * @param minCapacity the desired minimum capacity
         * @throws RuntimeException if minCapacity is less than zero
         */
        private int newCapacity(int minCapacity) {
            // overflow-conscious code
            int oldCapacity = elementData.length;
            int newCapacity = oldCapacity + (oldCapacity >> 1);
            if (newCapacity - minCapacity <= 0) {
                if (elementData == DEFAULTCAPACITY_EMPTY_ELEMENTDATA) {
                    return Math.max(DEFAULT_CAPACITY, minCapacity);
                }
                if (minCapacity < 0) { // overflow
                    throw new RuntimeException("capacity overflow!");
                }
                return minCapacity;
            }
            return (newCapacity - MAX_ARRAY_SIZE <= 0)
                    ? newCapacity
                    : hugeCapacity(minCapacity);
        }

        public void reset() {
            size = 0;
        }

        public String[] toArray() {
            String[] a = new String[size];
            System.arraycopy(elementData, 0, a, 0, size);
            return a;
        }

        @Override
        public String toString() {
            return Arrays.toString(toArray());
        }
    }


}
