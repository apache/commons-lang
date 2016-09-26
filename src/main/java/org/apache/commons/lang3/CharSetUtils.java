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

/**
 * <p>Operations on {@code CharSet} instances.</p>
 *
 * <p>This class handles {@code null} input gracefully.
 * An exception will not be thrown for a {@code null} input.
 * Each method documents its behaviour in more detail.</p>
 * 
 * <p>#ThreadSafe#</p>
 * @see CharSet
 * @since 1.0
 */
public class CharSetUtils {

    /**
     * <p>CharSetUtils instances should NOT be constructed in standard programming.
     * Instead, the class should be used as {@code CharSetUtils.evaluateSet(null);}.</p>
     *
     * <p>This constructor is public to permit tools that require a JavaBean instance
     * to operate.</p>
     */
    public CharSetUtils() {
      super();
    }

    // Squeeze
    //-----------------------------------------------------------------------
    /**
     * <p>Squeezes any repetitions of a character that is mentioned in the
     * supplied set.</p>
     *
     * <pre>
     * CharSetUtils.squeeze(null, *)        = null
     * CharSetUtils.squeeze("", *)          = ""
     * CharSetUtils.squeeze(*, null)        = *
     * CharSetUtils.squeeze(*, "")          = *
     * CharSetUtils.squeeze("hello", "k-p") = "helo"
     * CharSetUtils.squeeze("hello", "a-e") = "hello"
     * </pre>
     *
     * @see CharSet#getInstance(java.lang.String...) for set-syntax.
     * @param str  the string to squeeze, may be null
     * @param set  the character set to use for manipulation, may be null
     * @return the modified String, {@code null} if null string input
     */
    public static String squeeze(final String str, final String... set) {
        if (StringUtils.isEmpty(str) || deepEmpty(set)) {
            return str;
        }
        final CharSet chars = CharSet.getInstance(set);
        final StringBuilder buffer = new StringBuilder(str.length());
        final char[] chrs = str.toCharArray();
        final int sz = chrs.length;
        char lastChar = chrs[0];
        char ch = ' ';
        Character inChars = null;
        Character notInChars = null;
        buffer.append(lastChar);
        for (int i = 1; i < sz; i++) {
            ch = chrs[i];
            if (ch == lastChar) {
                if (inChars != null && ch == inChars) {
                    continue;
                } else {
                    if (notInChars == null || ch != notInChars) {
                        if (chars.contains(ch)) {
                            inChars = ch;
                            continue;
                        } else {
                            notInChars = ch;
                        }
                    }
                }
            }
            buffer.append(ch);
            lastChar = ch;
        }
        return buffer.toString();
    }

    // ContainsAny
    //-----------------------------------------------------------------------
    /**
     * <p>Takes an argument in set-syntax, see evaluateSet,
     * and identifies whether any of the characters are present in the specified string.</p>
     *
     * <pre>
     * CharSetUtils.containsAny(null, *)        = false
     * CharSetUtils.containsAny("", *)          = false
     * CharSetUtils.containsAny(*, null)        = false
     * CharSetUtils.containsAny(*, "")          = false
     * CharSetUtils.containsAny("hello", "k-p") = true
     * CharSetUtils.containsAny("hello", "a-d") = false
     * </pre>
     *
     * @see CharSet#getInstance(java.lang.String...) for set-syntax.
     * @param str  String to look for characters in, may be null
     * @param set  String[] set of characters to identify, may be null
     * @return whether or not the characters in the set are in the primary string
     * @since 3.2
     */
    public static boolean containsAny(final String str, final String... set) {
        if (StringUtils.isEmpty(str) || deepEmpty(set)) {
            return false;
        }
        final CharSet chars = CharSet.getInstance(set);
        for (final char c : str.toCharArray()) {
            if (chars.contains(c)) {
                return true;
            }
        }
        return false;
    }

    // Count
    //-----------------------------------------------------------------------
    /**
     * <p>Takes an argument in set-syntax, see evaluateSet,
     * and returns the number of characters present in the specified string.</p>
     *
     * <pre>
     * CharSetUtils.count(null, *)        = 0
     * CharSetUtils.count("", *)          = 0
     * CharSetUtils.count(*, null)        = 0
     * CharSetUtils.count(*, "")          = 0
     * CharSetUtils.count("hello", "k-p") = 3
     * CharSetUtils.count("hello", "a-e") = 1
     * </pre>
     *
     * @see CharSet#getInstance(java.lang.String...) for set-syntax.
     * @param str  String to count characters in, may be null
     * @param set  String[] set of characters to count, may be null
     * @return the character count, zero if null string input
     */
    public static int count(final String str, final String... set) {
        if (StringUtils.isEmpty(str) || deepEmpty(set)) {
            return 0;
        }
        final CharSet chars = CharSet.getInstance(set);
        int count = 0;
        for (final char c : str.toCharArray()) {
            if (chars.contains(c)) {
                count++;
            }
        }
        return count;
    }

    // Keep
    //-----------------------------------------------------------------------
    /**
     * <p>Takes an argument in set-syntax, see evaluateSet,
     * and keeps any of characters present in the specified string.</p>
     *
     * <pre>
     * CharSetUtils.keep(null, *)        = null
     * CharSetUtils.keep("", *)          = ""
     * CharSetUtils.keep(*, null)        = ""
     * CharSetUtils.keep(*, "")          = ""
     * CharSetUtils.keep("hello", "hl")  = "hll"
     * CharSetUtils.keep("hello", "le")  = "ell"
     * </pre>
     *
     * @see CharSet#getInstance(java.lang.String...) for set-syntax.
     * @param str  String to keep characters from, may be null
     * @param set  String[] set of characters to keep, may be null
     * @return the modified String, {@code null} if null string input
     * @since 2.0
     */
    public static String keep(final String str, final String... set) {
        if (str == null) {
            return null;
        }
        if (str.isEmpty() || deepEmpty(set)) {
            return StringUtils.EMPTY;
        }
        return modify(str, set, true);
    }

    // Delete
    //-----------------------------------------------------------------------
    /**
     * <p>Takes an argument in set-syntax, see evaluateSet,
     * and deletes any of characters present in the specified string.</p>
     *
     * <pre>
     * CharSetUtils.delete(null, *)        = null
     * CharSetUtils.delete("", *)          = ""
     * CharSetUtils.delete(*, null)        = *
     * CharSetUtils.delete(*, "")          = *
     * CharSetUtils.delete("hello", "hl")  = "eo"
     * CharSetUtils.delete("hello", "le")  = "ho"
     * </pre>
     *
     * @see CharSet#getInstance(java.lang.String...) for set-syntax.
     * @param str  String to delete characters from, may be null
     * @param set  String[] set of characters to delete, may be null
     * @return the modified String, {@code null} if null string input
     */
    public static String delete(final String str, final String... set) {
        if (StringUtils.isEmpty(str) || deepEmpty(set)) {
            return str;
        }
        return modify(str, set, false);
    }

    //-----------------------------------------------------------------------
    /**
     * Implementation of delete and keep
     *
     * @param str String to modify characters within
     * @param set String[] set of characters to modify
     * @param expect whether to evaluate on match, or non-match
     * @return the modified String, not null
     */
    private static String modify(final String str, final String[] set, final boolean expect) {
        final CharSet chars = CharSet.getInstance(set);
        final StringBuilder buffer = new StringBuilder(str.length());
        final char[] chrs = str.toCharArray();
        final int sz = chrs.length;
        for(int i=0; i<sz; i++) {
            if(chars.contains(chrs[i]) == expect) {
                buffer.append(chrs[i]);
            }
        }
        return buffer.toString();
    }

    /** 
     * Determines whether or not all the Strings in an array are 
     * empty or not.
     *
     * @param strings String[] whose elements are being checked for emptiness
     * @return whether or not the String is empty
     */
    private static boolean deepEmpty(final String[] strings) {
        if (strings != null) {
            for (final String s : strings) {
                if (StringUtils.isNotEmpty(s)) {
                    return false;
                }
            }
        }
        return true;
    }
}
