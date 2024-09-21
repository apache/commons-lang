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

import static org.apache.commons.lang3.StringUtils.INDEX_NOT_FOUND;

import org.apache.commons.lang3.builder.AbstractSupplier;
import org.apache.commons.lang3.function.ToBooleanBiFunction;

/**
 * String operations where you choose case-senstive {@link #CS} vs. case-insensitive {@link #CI} through a singleton instance.
 *
 * @see CharSequenceUtils
 * @see StringUtils
 * @since 3.18.0
 */
public abstract class Strings {

    public static class Builder extends AbstractSupplier<Strings, Builder, RuntimeException> {

        /**
         * Ignores case when possible.
         */
        private boolean ignoreCase;

        /**
         * Compares null as less when possible.
         */
        private boolean nullIsLess;

        @Override
        public Strings get() {
            return ignoreCase ? new CiStrings(ignoreCase) : new CsStrings(ignoreCase);
        }

        protected boolean isIgnoreCase() {
            return ignoreCase;
        }

        protected boolean isNullIsLess() {
            return nullIsLess;
        }

        public Builder setIgnoreCase(final boolean ignoreCase) {
            this.ignoreCase = ignoreCase;
            return asThis();
        }

        public Builder setNullIsLess(final boolean nullIsLess) {
            this.nullIsLess = nullIsLess;
            return asThis();
        }

    }

    /**
     * Case-insentive extension.
     */
    private static final class CiStrings extends Strings {

        private CiStrings(final boolean nullIsLess) {
            super(true, nullIsLess);
        }

        @Override
        public int compare(final String s1, final String s2) {
            if (s1 == s2) {
                // Both null or same object
                return 0;
            }
            if (s1 == null) {
                return isNullIsLess() ? -1 : 1;
            }
            if (s2 == null) {
                return isNullIsLess() ? 1 : -1;
            }
            return s1.compareToIgnoreCase(s2);
        }

        /**
         * Tests if CharSequence contains a search CharSequence irrespective of case, handling {@code null}. Case-insensitivity is defined as by
         * {@link String#equalsIgnoreCase(String)}.
         *
         * <p>
         * A {@code null} CharSequence will return {@code false}.
         * </p>
         *
         * <pre>
         * StringUtils.containsIgnoreCase(null, *)    = false
         * StringUtils.containsIgnoreCase(*, null)    = false
         * StringUtils.containsIgnoreCase("", "")     = true
         * StringUtils.containsIgnoreCase("abc", "")  = true
         * StringUtils.containsIgnoreCase("abc", "a") = true
         * StringUtils.containsIgnoreCase("abc", "z") = false
         * StringUtils.containsIgnoreCase("abc", "A") = true
         * StringUtils.containsIgnoreCase("abc", "Z") = false
         * </pre>
         *
         * @param str       the CharSequence to check, may be null
         * @param searchStr the CharSequence to find, may be null
         * @return true if the CharSequence contains the search CharSequence irrespective of case or false if not or {@code null} string input
         */
        @Override
        public boolean contains(final CharSequence str, final CharSequence searchStr) {
            if (str == null || searchStr == null) {
                return false;
            }
            final int len = searchStr.length();
            final int max = str.length() - len;
            for (int i = 0; i <= max; i++) {
                if (CharSequenceUtils.regionMatches(str, true, i, searchStr, 0, len)) {
                    return true;
                }
            }
            return false;
        }

        @Override
        public boolean equals(final CharSequence cs1, final CharSequence cs2) {
            if (cs1 == cs2) {
                return true;
            }
            if (cs1 == null || cs2 == null) {
                return false;
            }
            if (cs1.length() != cs2.length()) {
                return false;
            }
            return CharSequenceUtils.regionMatches(cs1, true, 0, cs2, 0, cs1.length());
        }

        @Override
        public boolean equals(final String s1, final String s2) {
            return s1.equalsIgnoreCase(s2);
        }

        @Override
        public int indexOf(final CharSequence str, final CharSequence searchStr, int startPos) {
            if (str == null || searchStr == null) {
                return INDEX_NOT_FOUND;
            }
            if (startPos < 0) {
                startPos = 0;
            }
            final int endLimit = str.length() - searchStr.length() + 1;
            if (startPos > endLimit) {
                return INDEX_NOT_FOUND;
            }
            if (searchStr.length() == 0) {
                return startPos;
            }
            for (int i = startPos; i < endLimit; i++) {
                if (CharSequenceUtils.regionMatches(str, true, i, searchStr, 0, searchStr.length())) {
                    return i;
                }
            }
            return INDEX_NOT_FOUND;
        }

        /**
         * Case in-sensitive find of the last index within a CharSequence from the specified position.
         *
         * <p>
         * A {@code null} CharSequence will return {@code -1}. A negative start position returns {@code -1}. An empty ("") search CharSequence always matches
         * unless the start position is negative. A start position greater than the string length searches the whole string. The search starts at the startPos
         * and works backwards; matches starting after the start position are ignored.
         * </p>
         *
         * <pre>
         * StringUtils.lastIndexOfIgnoreCase(null, *, *)          = -1
         * StringUtils.lastIndexOfIgnoreCase(*, null, *)          = -1
         * StringUtils.lastIndexOfIgnoreCase("aabaabaa", "A", 8)  = 7
         * StringUtils.lastIndexOfIgnoreCase("aabaabaa", "B", 8)  = 5
         * StringUtils.lastIndexOfIgnoreCase("aabaabaa", "AB", 8) = 4
         * StringUtils.lastIndexOfIgnoreCase("aabaabaa", "B", 9)  = 5
         * StringUtils.lastIndexOfIgnoreCase("aabaabaa", "B", -1) = -1
         * StringUtils.lastIndexOfIgnoreCase("aabaabaa", "A", 0)  = 0
         * StringUtils.lastIndexOfIgnoreCase("aabaabaa", "B", 0)  = -1
         * </pre>
         *
         * @param str       the CharSequence to check, may be null
         * @param searchStr the CharSequence to find, may be null
         * @param startPos  the start position
         * @return the last index of the search CharSequence (always &le; startPos), -1 if no match or {@code null} input
         */
        @Override
        public int lastIndexOf(final CharSequence str, final CharSequence searchStr, int startPos) {
            if (str == null || searchStr == null) {
                return INDEX_NOT_FOUND;
            }
            final int searchStrLength = searchStr.length();
            final int strLength = str.length();
            if (startPos > strLength - searchStrLength) {
                startPos = strLength - searchStrLength;
            }
            if (startPos < 0) {
                return INDEX_NOT_FOUND;
            }
            if (searchStrLength == 0) {
                return startPos;
            }
            for (int i = startPos; i >= 0; i--) {
                if (CharSequenceUtils.regionMatches(str, true, i, searchStr, 0, searchStrLength)) {
                    return i;
                }
            }
            return INDEX_NOT_FOUND;
        }

    }

    /**
     * Case-sentive extension.
     */
    private static final class CsStrings extends Strings {

        private CsStrings(final boolean nullIsLess) {
            super(false, nullIsLess);
        }

        @Override
        public int compare(final String s1, final String s2) {
            if (s1 == s2) {
                // Both null or same object
                return 0;
            }
            if (s1 == null) {
                return isNullIsLess() ? -1 : 1;
            }
            if (s2 == null) {
                return isNullIsLess() ? 1 : -1;
            }
            return s1.compareTo(s2);
        }

        @Override
        public boolean contains(final CharSequence seq, final CharSequence searchSeq) {
            return CharSequenceUtils.indexOf(seq, searchSeq, 0) >= 0;
        }

        @Override
        public boolean equals(final CharSequence cs1, final CharSequence cs2) {
            if (cs1 == cs2) {
                return true;
            }
            if (cs1 == null || cs2 == null) {
                return false;
            }
            if (cs1.length() != cs2.length()) {
                return false;
            }
            if (cs1 instanceof String && cs2 instanceof String) {
                return cs1.equals(cs2);
            }
            // Step-wise comparison
            final int length = cs1.length();
            for (int i = 0; i < length; i++) {
                if (cs1.charAt(i) != cs2.charAt(i)) {
                    return false;
                }
            }
            return true;
        }

        @Override
        public boolean equals(final String s1, final String s2) {
            return s1.equals(s2);
        }

        @Override
        public int indexOf(final CharSequence seq, final CharSequence searchSeq, final int startPos) {
            return CharSequenceUtils.indexOf(seq, searchSeq, startPos);
        }

        /**
         * Finds the last index within a CharSequence, handling {@code null}. This method uses {@link String#lastIndexOf(String, int)} if possible.
         *
         * <p>
         * A {@code null} CharSequence will return {@code -1}. A negative start position returns {@code -1}. An empty ("") search CharSequence always matches
         * unless the start position is negative. A start position greater than the string length searches the whole string. The search starts at the startPos
         * and works backwards; matches starting after the start position are ignored.
         * </p>
         *
         * <pre>
         * StringUtils.lastIndexOf(null, *, *)          = -1
         * StringUtils.lastIndexOf(*, null, *)          = -1
         * StringUtils.lastIndexOf("aabaabaa", "a", 8)  = 7
         * StringUtils.lastIndexOf("aabaabaa", "b", 8)  = 5
         * StringUtils.lastIndexOf("aabaabaa", "ab", 8) = 4
         * StringUtils.lastIndexOf("aabaabaa", "b", 9)  = 5
         * StringUtils.lastIndexOf("aabaabaa", "b", -1) = -1
         * StringUtils.lastIndexOf("aabaabaa", "a", 0)  = 0
         * StringUtils.lastIndexOf("aabaabaa", "b", 0)  = -1
         * StringUtils.lastIndexOf("aabaabaa", "b", 1)  = -1
         * StringUtils.lastIndexOf("aabaabaa", "b", 2)  = 2
         * StringUtils.lastIndexOf("aabaabaa", "ba", 2)  = 2
         * </pre>
         *
         * @param seq       the CharSequence to check, may be null
         * @param searchSeq the CharSequence to find, may be null
         * @param startPos  the start position, negative treated as zero
         * @return the last index of the search CharSequence (always &le; startPos), -1 if no match or {@code null} string input
         */
        @Override
        public int lastIndexOf(final CharSequence seq, final CharSequence searchSeq, final int startPos) {
            return CharSequenceUtils.lastIndexOf(seq, searchSeq, startPos);
        }

    }

    /**
     * The <b>C</b>ase-<b>I</b>nsensitive singleton instance.
     */
    public static final Strings CI = new CiStrings(true);

    /**
     * The <b>C</b>ase-<b>S</b>nsensitive singleton instance.
     */
    public static final Strings CS = new CsStrings(true);

    public static final Builder builder() {
        return new Builder();
    }

    /**
     * Tests if the CharSequence contains any of the CharSequences in the given array.
     *
     * <p>
     * A {@code null} {@code cs} CharSequence will return {@code false}. A {@code null} or zero length search array will return {@code false}.
     * </p>
     *
     * @param cs                  The CharSequence to check, may be null
     * @param searchCharSequences The array of CharSequences to search for, may be null. Individual CharSequences may be null as well.
     * @return {@code true} if any of the search CharSequences are found, {@code false} otherwise
     */
    private static boolean containsAny(final ToBooleanBiFunction<CharSequence, CharSequence> test, final CharSequence cs,
            final CharSequence... searchCharSequences) {
        if (StringUtils.isEmpty(cs) || ArrayUtils.isEmpty(searchCharSequences)) {
            return false;
        }
        for (final CharSequence searchCharSequence : searchCharSequences) {
            if (test.applyAsBoolean(cs, searchCharSequence)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Ignores case when possible.
     */
    private final boolean ignoreCase;

    /**
     * Compares null as less when possible.
     */
    private final boolean nullIsLess;

    private Strings(final boolean ignoreCase, final boolean nullIsLess) {
        this.ignoreCase = ignoreCase;
        this.nullIsLess = nullIsLess;
    }

    /**
     * Appends the suffix to the end of the string if the string does not already end with the suffix.
     *
     * <p>
     * Case-sensitive examples
     * </p>
     *
     * <pre>
     * Strings.CS.appendIfMissing(null, null)      = null
     * Strings.CS.appendIfMissing("abc", null)     = "abc"
     * Strings.CS.appendIfMissing("", "xyz"        = "xyz"
     * Strings.CS.appendIfMissing("abc", "xyz")    = "abcxyz"
     * Strings.CS.appendIfMissing("abcxyz", "xyz") = "abcxyz"
     * Strings.CS.appendIfMissing("abcXYZ", "xyz") = "abcXYZxyz"
     * </pre>
     * <p>
     * With additional suffixes:
     * </p>
     *
     * <pre>
     * Strings.CS.appendIfMissing(null, null, null)       = null
     * Strings.CS.appendIfMissing("abc", null, null)      = "abc"
     * Strings.CS.appendIfMissing("", "xyz", null)        = "xyz"
     * Strings.CS.appendIfMissing("abc", "xyz", new CharSequence[]{null}) = "abcxyz"
     * Strings.CS.appendIfMissing("abc", "xyz", "")       = "abc"
     * Strings.CS.appendIfMissing("abc", "xyz", "mno")    = "abcxyz"
     * Strings.CS.appendIfMissing("abcxyz", "xyz", "mno") = "abcxyz"
     * Strings.CS.appendIfMissing("abcmno", "xyz", "mno") = "abcmno"
     * Strings.CS.appendIfMissing("abcXYZ", "xyz", "mno") = "abcXYZxyz"
     * Strings.CS.appendIfMissing("abcMNO", "xyz", "mno") = "abcMNOxyz"
     * </pre>
     *
     * <p>
     * Case-insensitive examples
     * </p>
     *
     * <pre>
     * Strings.CI.appendIfMissing(null, null)      = null
     * Strings.CI.appendIfMissing("abc", null)     = "abc"
     * Strings.CI.appendIfMissing("", "xyz")       = "xyz"
     * Strings.CI.appendIfMissing("abc", "xyz")    = "abcxyz"
     * Strings.CI.appendIfMissing("abcxyz", "xyz") = "abcxyz"
     * Strings.CI.appendIfMissing("abcXYZ", "xyz") = "abcXYZ"
     * </pre>
     * <p>
     * With additional suffixes:
     * </p>
     *
     * <pre>
     * Strings.CI.appendIfMissing(null, null, null)       = null
     * Strings.CI.appendIfMissing("abc", null, null)      = "abc"
     * Strings.CI.appendIfMissing("", "xyz", null)        = "xyz"
     * Strings.CI.appendIfMissing("abc", "xyz", new CharSequence[]{null}) = "abcxyz"
     * Strings.CI.appendIfMissing("abc", "xyz", "")       = "abc"
     * Strings.CI.appendIfMissing("abc", "xyz", "mno")    = "abcxyz"
     * Strings.CI.appendIfMissing("abcxyz", "xyz", "mno") = "abcxyz"
     * Strings.CI.appendIfMissing("abcmno", "xyz", "mno") = "abcmno"
     * Strings.CI.appendIfMissing("abcXYZ", "xyz", "mno") = "abcXYZ"
     * Strings.CI.appendIfMissing("abcMNO", "xyz", "mno") = "abcMNO"
     * </pre>
     *
     * @param str      The string.
     * @param suffix   The suffix to append to the end of the string.
     * @param suffixes Additional suffixes that are valid terminators (optional).
     * @return A new String if suffix was appended, the same string otherwise.
     */
    public String appendIfMissing(final String str, final CharSequence suffix, final CharSequence... suffixes) {
        if (str == null || StringUtils.isEmpty(suffix) || endsWith(str, suffix)) {
            return str;
        }
        if (ArrayUtils.isNotEmpty(suffixes)) {
            for (final CharSequence s : suffixes) {
                if (endsWith(str, s)) {
                    return str;
                }
            }
        }
        return str + suffix;
    }

    public abstract int compare(String s1, String s2);

    /**
     * Tests if CharSequence contains a search CharSequence, handling {@code null}. This method uses {@link String#indexOf(String)} if possible.
     *
     * <p>
     * A {@code null} CharSequence will return {@code false}.
     * </p>
     *
     * <pre>
     * Strings.CS.contains(null, *)     = false
     * Strings.CS.contains(*, null)     = false
     * Strings.CS.contains("", "")      = true
     * Strings.CS.contains("abc", "")   = true
     * Strings.CS.contains("abc", "a")  = true
     * Strings.CS.contains("abc", "z")  = false
     * </pre>
     *
     * @param seq       the CharSequence to check, may be null
     * @param searchSeq the CharSequence to find, may be null
     * @return true if the CharSequence contains the search CharSequence, false if not or {@code null} string input
     */
    public abstract boolean contains(CharSequence seq, CharSequence searchSeq);

    /**
     * Tests if the CharSequence contains any of the CharSequences in the given array, ignoring case.
     *
     * <p>
     * A {@code null} {@code cs} CharSequence will return {@code false}. A {@code null} or zero length search array will return {@code false}.
     * </p>
     *
     * <pre>
     * StringUtils.containsAny(null, *)            = false
     * StringUtils.containsAny("", *)              = false
     * StringUtils.containsAny(*, null)            = false
     * StringUtils.containsAny(*, [])              = false
     * StringUtils.containsAny("abcd", "ab", null) = true
     * StringUtils.containsAny("abcd", "ab", "cd") = true
     * StringUtils.containsAny("abc", "d", "abc")  = true
     * StringUtils.containsAny("abc", "D", "ABC")  = true
     * StringUtils.containsAny("ABC", "d", "abc")  = true
     * </pre>
     *
     * @param cs                  The CharSequence to check, may be null
     * @param searchCharSequences The array of CharSequences to search for, may be null. Individual CharSequences may be null as well.
     * @return {@code true} if any of the search CharSequences are found, {@code false} otherwise
     */
    public boolean containsAny(final CharSequence cs, final CharSequence... searchCharSequences) {
        return containsAny(this::contains, cs, searchCharSequences);
    }

    /**
     * Tests if a CharSequence ends with a specified suffix (optionally case-insensitive).
     *
     * <p>
     * Case-sensitive examples
     * </p>
     *
     * <pre>
     * Strings.CS.endsWith(null, null)      = true
     * Strings.CS.endsWith(null, "def")     = false
     * Strings.CS.endsWith("abcdef", null)  = false
     * Strings.CS.endsWith("abcdef", "def") = true
     * Strings.CS.endsWith("ABCDEF", "def") = false
     * Strings.CS.endsWith("ABCDEF", "cde") = false
     * Strings.CS.endsWith("ABCDEF", "")    = true
     * </pre>
     *
     * <p>
     * Case-insensitive examples
     * </p>
     *
     * <pre>
     * Strings.CI.endsWith(null, null)      = true
     * Strings.CI.endsWith(null, "def")     = false
     * Strings.CI.endsWith("abcdef", null)  = false
     * Strings.CI.endsWith("abcdef", "def") = true
     * Strings.CI.endsWith("ABCDEF", "def") = true
     * Strings.CI.endsWith("ABCDEF", "cde") = false
     * </pre>
     *
     * @param str    the CharSequence to check, may be null.
     * @param suffix the suffix to find, may be null.
     * @return {@code true} if the CharSequence starts with the prefix or both {@code null}.
     * @see String#endsWith(String)
     */
    public boolean endsWith(final CharSequence str, final CharSequence suffix) {
        if (str == null || suffix == null) {
            return str == suffix;
        }
        final int sufLen = suffix.length();
        if (sufLen > str.length()) {
            return false;
        }
        return CharSequenceUtils.regionMatches(str, ignoreCase, str.length() - sufLen, suffix, 0, sufLen);
    }

    public abstract boolean equals(CharSequence s1, CharSequence s2);

    /**
     * Compares this string to the specified object. The result is {@code
     * true} if and only if the argument is not {@code null} and is a {@code
     * String} object that represents the same sequence of characters as this object.
     *
     * @param s1 The left string to compare this {@code String} against
     * @param s2 The right string to compare this {@code String} against
     *
     * @return {@code true} if the given object represents a {@code String} equivalent to this string, {@code false} otherwise
     *
     * @see String#compareTo(String)
     * @see String#equalsIgnoreCase(String)
     */
    public abstract boolean equals(String s1, String s2);

    /**
     * Compares given {@code string} to a CharSequences vararg of {@code searchStrings}, returning {@code true} if the {@code string} is equal to any of the
     * {@code searchStrings}.
     *
     * <pre>
     * StringUtils.equalsAny(null, (CharSequence[]) null) = false
     * StringUtils.equalsAny(null, null, null)    = true
     * StringUtils.equalsAny(null, "abc", "def")  = false
     * StringUtils.equalsAny("abc", null, "def")  = false
     * StringUtils.equalsAny("abc", "abc", "def") = true
     * StringUtils.equalsAny("abc", "ABC", "DEF") = false
     * </pre>
     *
     * @param string        to compare, may be {@code null}.
     * @param searchStrings a vararg of strings, may be {@code null}.
     * @return {@code true} if the string is equal (case-sensitive) to any other element of {@code searchStrings}; {@code false} if {@code searchStrings} is
     *         null or contains no matches.
     */
    public boolean equalsAny(final CharSequence string, final CharSequence... searchStrings) {
        if (ArrayUtils.isNotEmpty(searchStrings)) {
            for (final CharSequence next : searchStrings) {
                if (equals(string, next)) {
                    return true;
                }
            }
        }
        return false;
    }

    public int indexOf(final CharSequence str, final CharSequence searchStr) {
        return indexOf(str, searchStr, 0);
    }

    public abstract int indexOf(CharSequence str, CharSequence searchStr, int startPos);

    public boolean isCaseSensitive() {
        return !ignoreCase;
    }

    boolean isIgnoreCase() {
        return ignoreCase;
    }

    boolean isNullIsLess() {
        return nullIsLess;
    }

    public int lastIndexOf(final CharSequence str, final CharSequence searchStr) {
        if (str == null) {
            return INDEX_NOT_FOUND;
        }
        return lastIndexOf(str, searchStr, str.length());
    }

    public abstract int lastIndexOf(CharSequence seq, CharSequence searchSeq, int startPos);

    /**
     * Prepends the prefix to the start of the string if the string does not already start with any of the prefixes.
     *
     * <p>
     * Case-sensitive examples
     * </p>
     *
     * <pre>
     * StringUtils.prependIfMissing(null, null) = null
     * StringUtils.prependIfMissing("abc", null) = "abc"
     * StringUtils.prependIfMissing("", "xyz") = "xyz"
     * StringUtils.prependIfMissing("abc", "xyz") = "xyzabc"
     * StringUtils.prependIfMissing("xyzabc", "xyz") = "xyzabc"
     * StringUtils.prependIfMissing("XYZabc", "xyz") = "xyzXYZabc"
     * </pre>
     * <p>
     * With additional prefixes,
     * </p>
     *
     * <pre>
     * StringUtils.prependIfMissing(null, null, null) = null
     * StringUtils.prependIfMissing("abc", null, null) = "abc"
     * StringUtils.prependIfMissing("", "xyz", null) = "xyz"
     * StringUtils.prependIfMissing("abc", "xyz", new CharSequence[]{null}) = "xyzabc"
     * StringUtils.prependIfMissing("abc", "xyz", "") = "abc"
     * StringUtils.prependIfMissing("abc", "xyz", "mno") = "xyzabc"
     * StringUtils.prependIfMissing("xyzabc", "xyz", "mno") = "xyzabc"
     * StringUtils.prependIfMissing("mnoabc", "xyz", "mno") = "mnoabc"
     * StringUtils.prependIfMissing("XYZabc", "xyz", "mno") = "xyzXYZabc"
     * StringUtils.prependIfMissing("MNOabc", "xyz", "mno") = "xyzMNOabc"
     * </pre>
     *
     * <p>
     * Case-insensitive examples
     * </p>
     *
     * <pre>
     * StringUtils.prependIfMissingIgnoreCase(null, null) = null
     * StringUtils.prependIfMissingIgnoreCase("abc", null) = "abc"
     * StringUtils.prependIfMissingIgnoreCase("", "xyz") = "xyz"
     * StringUtils.prependIfMissingIgnoreCase("abc", "xyz") = "xyzabc"
     * StringUtils.prependIfMissingIgnoreCase("xyzabc", "xyz") = "xyzabc"
     * StringUtils.prependIfMissingIgnoreCase("XYZabc", "xyz") = "XYZabc"
     * </pre>
     * <p>
     * With additional prefixes,
     * </p>
     *
     * <pre>
     * StringUtils.prependIfMissingIgnoreCase(null, null, null) = null
     * StringUtils.prependIfMissingIgnoreCase("abc", null, null) = "abc"
     * StringUtils.prependIfMissingIgnoreCase("", "xyz", null) = "xyz"
     * StringUtils.prependIfMissingIgnoreCase("abc", "xyz", new CharSequence[]{null}) = "xyzabc"
     * StringUtils.prependIfMissingIgnoreCase("abc", "xyz", "") = "abc"
     * StringUtils.prependIfMissingIgnoreCase("abc", "xyz", "mno") = "xyzabc"
     * StringUtils.prependIfMissingIgnoreCase("xyzabc", "xyz", "mno") = "xyzabc"
     * StringUtils.prependIfMissingIgnoreCase("mnoabc", "xyz", "mno") = "mnoabc"
     * StringUtils.prependIfMissingIgnoreCase("XYZabc", "xyz", "mno") = "XYZabc"
     * StringUtils.prependIfMissingIgnoreCase("MNOabc", "xyz", "mno") = "MNOabc"
     * </pre>
     *
     * @param str      The string.
     * @param prefix   The prefix to prepend to the start of the string.
     * @param prefixes Additional prefixes that are valid.
     * @return A new String if prefix was prepended, the same string otherwise.
     */
    public String prependIfMissing(final String str, final CharSequence prefix, final CharSequence... prefixes) {
        if (str == null || StringUtils.isEmpty(prefix) || startsWith(str, prefix)) {
            return str;
        }
        if (ArrayUtils.isNotEmpty(prefixes)) {
            for (final CharSequence p : prefixes) {
                if (startsWith(str, p)) {
                    return str;
                }
            }
        }
        return prefix + str;
    }

    /**
     * Case-insensitive removal of a substring if it is at the end of a source string, otherwise returns the source string.
     *
     * <p>
     * A {@code null} source string will return {@code null}. An empty ("") source string will return the empty string. A {@code null} search string will return
     * the source string.
     * </p>
     *
     * <p>
     * Case-sensitive examples
     * </p>
     *
     * <pre>
     * StringUtils.removeEnd(null, *)      = null
     * StringUtils.removeEnd("", *)        = ""
     * StringUtils.removeEnd(*, null)      = *
     * StringUtils.removeEnd("www.domain.com", ".com.")  = "www.domain.com"
     * StringUtils.removeEnd("www.domain.com", ".com")   = "www.domain"
     * StringUtils.removeEnd("www.domain.com", "domain") = "www.domain.com"
     * StringUtils.removeEnd("abc", "")    = "abc"
     * </pre>
     * <p>
     * Case-insensitive examples
     * </p>
     *
     * <pre>
     * StringUtils.removeEndIgnoreCase(null, *)      = null
     * StringUtils.removeEndIgnoreCase("", *)        = ""
     * StringUtils.removeEndIgnoreCase(*, null)      = *
     * StringUtils.removeEndIgnoreCase("www.domain.com", ".com.")  = "www.domain.com"
     * StringUtils.removeEndIgnoreCase("www.domain.com", ".com")   = "www.domain"
     * StringUtils.removeEndIgnoreCase("www.domain.com", "domain") = "www.domain.com"
     * StringUtils.removeEndIgnoreCase("abc", "")    = "abc"
     * StringUtils.removeEndIgnoreCase("www.domain.com", ".COM") = "www.domain")
     * StringUtils.removeEndIgnoreCase("www.domain.COM", ".com") = "www.domain")
     * </pre>
     *
     * @param str    the source String to search, may be null
     * @param remove the String to search for (case-insensitive) and remove, may be null
     * @return the substring with the string removed if found, {@code null} if null String input
     */
    public String removeEnd(final String str, final CharSequence remove) {
        if (StringUtils.isEmpty(str) || StringUtils.isEmpty(remove)) {
            return str;
        }
        if (endsWith(str, remove)) {
            return str.substring(0, str.length() - remove.length());
        }
        return str;
    }

    /**
     * Case-insensitive removal of a substring if it is at the beginning of a source string, otherwise returns the source string.
     *
     * <p>
     * A {@code null} source string will return {@code null}. An empty ("") source string will return the empty string. A {@code null} search string will return
     * the source string.
     * </p>
     *
     * <p>
     * Case-insensitive examples
     * </p>
     *
     * <pre>
     * StringUtils.removeStartIgnoreCase(null, *)      = null
     * StringUtils.removeStartIgnoreCase("", *)        = ""
     * StringUtils.removeStartIgnoreCase(*, null)      = *
     * StringUtils.removeStartIgnoreCase("www.domain.com", "www.")   = "domain.com"
     * StringUtils.removeStartIgnoreCase("www.domain.com", "WWW.")   = "domain.com"
     * StringUtils.removeStartIgnoreCase("domain.com", "www.")       = "domain.com"
     * StringUtils.removeStartIgnoreCase("www.domain.com", "domain") = "www.domain.com"
     * StringUtils.removeStartIgnoreCase("abc", "")    = "abc"
     * </pre>
     *
     * @param str    the source String to search, may be null
     * @param remove the String to search for (case-insensitive) and remove, may be null
     * @return the substring with the string removed if found, {@code null} if null String input
     */
    public String removeStart(final String str, final CharSequence remove) {
        if (str != null && startsWith(str, remove)) {
            return str.substring(StringUtils.length(remove));
        }
        return str;
    }

    /**
     * Case insensitively replaces all occurrences of a String within another String.
     *
     * <p>
     * A {@code null} reference passed to this method is a no-op.
     * </p>
     *
     * <p>
     * Case-sensitive examples
     * </p>
     *
     * <pre>
     * StringUtils.replace(null, *, *)        = null
     * StringUtils.replace("", *, *)          = ""
     * StringUtils.replace("any", null, *)    = "any"
     * StringUtils.replace("any", *, null)    = "any"
     * StringUtils.replace("any", "", *)      = "any"
     * StringUtils.replace("aba", "a", null)  = "aba"
     * StringUtils.replace("aba", "a", "")    = "b"
     * StringUtils.replace("aba", "a", "z")   = "zbz"
     * </pre>
     * <p>
     * Case-insensitive examples
     * </p>
     *
     * <pre>
     * StringUtils.replaceIgnoreCase(null, *, *)        = null
     * StringUtils.replaceIgnoreCase("", *, *)          = ""
     * StringUtils.replaceIgnoreCase("any", null, *)    = "any"
     * StringUtils.replaceIgnoreCase("any", *, null)    = "any"
     * StringUtils.replaceIgnoreCase("any", "", *)      = "any"
     * StringUtils.replaceIgnoreCase("aba", "a", null)  = "aba"
     * StringUtils.replaceIgnoreCase("abA", "A", "")    = "b"
     * StringUtils.replaceIgnoreCase("aba", "A", "z")   = "zbz"
     * </pre>
     *
     * @see #replace(String text, String searchString, String replacement, int max)
     * @param text         text to search and replace in, may be null
     * @param searchString the String to search for (case-insensitive), may be null
     * @param replacement  the String to replace it with, may be null
     * @return the text with any replacements processed, {@code null} if null String input
     */
    public String replace(final String text, final String searchString, final String replacement) {
        return replace(text, searchString, replacement, -1);
    }

    /**
     * Replaces a String with another String inside a larger String, for the first {@code max} values of the search String, case-sensitively/insensitively based
     * on {@code ignoreCase} value.
     *
     * <p>
     * A {@code null} reference passed to this method is a no-op.
     * </p>
     *
     * <pre>
     * StringUtils.replace(null, *, *, *, false)         = null
     * StringUtils.replace("", *, *, *, false)           = ""
     * StringUtils.replace("any", null, *, *, false)     = "any"
     * StringUtils.replace("any", *, null, *, false)     = "any"
     * StringUtils.replace("any", "", *, *, false)       = "any"
     * StringUtils.replace("any", *, *, 0, false)        = "any"
     * StringUtils.replace("abaa", "a", null, -1, false) = "abaa"
     * StringUtils.replace("abaa", "a", "", -1, false)   = "b"
     * StringUtils.replace("abaa", "a", "z", 0, false)   = "abaa"
     * StringUtils.replace("abaa", "A", "z", 1, false)   = "abaa"
     * StringUtils.replace("abaa", "A", "z", 1, true)   = "zbaa"
     * StringUtils.replace("abAa", "a", "z", 2, true)   = "zbza"
     * StringUtils.replace("abAa", "a", "z", -1, true)  = "zbzz"
     * </pre>
     *
     * @param text         text to search and replace in, may be null
     * @param searchString the String to search for (case-insensitive), may be null
     * @param replacement  the String to replace it with, may be null
     * @param max          maximum number of values to replace, or {@code -1} if no maximum
     * @return the text with any replacements processed, {@code null} if null String input
     */
    public String replace(final String text, String searchString, final String replacement, int max) {
        if (StringUtils.isEmpty(text) || StringUtils.isEmpty(searchString) || replacement == null || max == 0) {
            return text;
        }
        if (ignoreCase) {
            searchString = searchString.toLowerCase();
        }
        int start = 0;
        int end = indexOf(text, searchString, start);
        if (end == INDEX_NOT_FOUND) {
            return text;
        }
        final int replLength = searchString.length();
        int increase = Math.max(replacement.length() - replLength, 0);
        increase *= max < 0 ? 16 : Math.min(max, 64);
        final StringBuilder buf = new StringBuilder(text.length() + increase);
        while (end != INDEX_NOT_FOUND) {
            buf.append(text, start, end).append(replacement);
            start = end + replLength;
            if (--max == 0) {
                break;
            }
            end = indexOf(text, searchString, start);
        }
        buf.append(text, start, text.length());
        return buf.toString();
    }

    /**
     * Replaces a String with another String inside a larger String, once.
     *
     * <p>
     * A {@code null} reference passed to this method is a no-op.
     * </p>
     *
     * <p>
     * Case-sensitive examples
     * </p>
     *
     * <pre>
     * StringUtils.replaceOnce(null, *, *)        = null
     * StringUtils.replaceOnce("", *, *)          = ""
     * StringUtils.replaceOnce("any", null, *)    = "any"
     * StringUtils.replaceOnce("any", *, null)    = "any"
     * StringUtils.replaceOnce("any", "", *)      = "any"
     * StringUtils.replaceOnce("aba", "a", null)  = "aba"
     * StringUtils.replaceOnce("aba", "a", "")    = "ba"
     * StringUtils.replaceOnce("aba", "a", "z")   = "zba"
     * </pre>
     *
     * @see #replace(String text, String searchString, String replacement, int max)
     * @param text         text to search and replace in, may be null
     * @param searchString the String to search for, may be null
     * @param replacement  the String to replace with, may be null
     * @return the text with any replacements processed, {@code null} if null String input
     */
    public String replaceOnce(final String text, final String searchString, final String replacement) {
        return replace(text, searchString, replacement, 1);
    }

    /**
     * Tests if a CharSequence starts with a specified prefix.
     *
     * <p>
     * {@code null}s are handled without exceptions. Two {@code null} references are considered to be equal.
     * </p>
     *
     * <p>
     * Case-sensitive examples
     * </p>
     *
     * <pre>
     * StringUtils.startsWith(null, null)      = true
     * StringUtils.startsWith(null, "abc")     = false
     * StringUtils.startsWith("abcdef", null)  = false
     * StringUtils.startsWith("abcdef", "abc") = true
     * StringUtils.startsWith("ABCDEF", "abc") = false
     * </pre>
     *
     * <p>
     * Case-insensitive examples
     * </p>
     *
     * <pre>
     * StringUtils.startsWithIgnoreCase(null, null)      = true
     * StringUtils.startsWithIgnoreCase(null, "abc")     = false
     * StringUtils.startsWithIgnoreCase("abcdef", null)  = false
     * StringUtils.startsWithIgnoreCase("abcdef", "abc") = true
     * StringUtils.startsWithIgnoreCase("ABCDEF", "abc") = true
     * </pre>
     *
     * @see String#startsWith(String)
     * @param str    the CharSequence to check, may be null
     * @param prefix the prefix to find, may be null
     * @return {@code true} if the CharSequence starts with the prefix, case-sensitive, or both {@code null}
     */
    public boolean startsWith(final CharSequence str, final CharSequence prefix) {
        if (str == null || prefix == null) {
            return str == prefix;
        }
        final int preLen = prefix.length();
        if (preLen > str.length()) {
            return false;
        }
        return CharSequenceUtils.regionMatches(str, ignoreCase, 0, prefix, 0, preLen);
    }
}
