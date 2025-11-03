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

import static org.apache.commons.lang3.StringUtils.INDEX_NOT_FOUND;

import org.apache.commons.lang3.builder.AbstractSupplier;
import org.apache.commons.lang3.function.ToBooleanBiFunction;

/**
 * String operations where you choose case-sensitive {@link #CS} vs. case-insensitive {@link #CI} through a singleton instance.
 *
 * @see CharSequenceUtils
 * @see StringUtils
 * @since 3.18.0
 */
public abstract class Strings {

    /**
     * Builds {@link Strings} instances.
     */
    public static class Builder extends AbstractSupplier<Strings, Builder, RuntimeException> {

        /**
         * Ignores case when possible.
         */
        private boolean ignoreCase;

        /**
         * Compares null as less when possible.
         */
        private boolean nullIsLess;

        /**
         * Constructs a new instance.
         */
        private Builder() {
            // empty
        }

        /**
         * Gets a new {@link Strings} instance.
         */
        @Override
        public Strings get() {
            return ignoreCase ? new CiStrings(nullIsLess) : new CsStrings(nullIsLess);
        }

        /**
         * Sets the ignoreCase property for new Strings instances.
         *
         * @param ignoreCase the ignoreCase property for new Strings instances.
         * @return {@code this} instance.
         */
        public Builder setIgnoreCase(final boolean ignoreCase) {
            this.ignoreCase = ignoreCase;
            return asThis();
        }

        /**
         * Sets the nullIsLess property for new Strings instances.
         *
         * @param nullIsLess the nullIsLess property for new Strings instances.
         * @return {@code this} instance.
         */
        public Builder setNullIsLess(final boolean nullIsLess) {
            this.nullIsLess = nullIsLess;
            return asThis();
        }

    }

    /**
     * Case-insensitive extension.
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
            return s1 == null ? s2 == null : s1.equalsIgnoreCase(s2);
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
            return eq(s1, s2);
        }

        @Override
        public int indexOf(final CharSequence seq, final CharSequence searchSeq, final int startPos) {
            return CharSequenceUtils.indexOf(seq, searchSeq, startPos);
        }

        @Override
        public int lastIndexOf(final CharSequence seq, final CharSequence searchSeq, final int startPos) {
            return CharSequenceUtils.lastIndexOf(seq, searchSeq, startPos);
        }

    }

    /**
     * The <strong>C</strong>ase-<strong>I</strong>nsensitive singleton instance.
     */
    public static final Strings CI = new CiStrings(true);

    /**
     * The <strong>C</strong>ase-<strong>S</strong>ensitive singleton instance.
     */
    public static final Strings CS = new CsStrings(true);

    /**
     * Constructs a new {@link Builder} instance.
     *
     * @return a new {@link Builder} instance.
     */
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
     * Tests for equality in a null-safe manner.
     *
     * See JDK-8015417.
     */
    private static boolean eq(final Object o1, final Object o2) {
        return o1 == null ? o2 == null : o1.equals(o2);
    }

    /**
     * Ignores case when possible.
     */
    private final boolean ignoreCase;

    /**
     * Compares null as less when possible.
     */
    private final boolean nullIsLess;

    /**
     * Constructs a new instance.
     *
     * @param ignoreCase Ignores case when possible.
     * @param nullIsLess Compares null as less when possible.
     */
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

    /**
     * Compare two Strings lexicographically, like {@link String#compareTo(String)}.
     * <p>
     * The return values are:
     * </p>
     * <ul>
     * <li>{@code int = 0}, if {@code str1} is equal to {@code str2} (or both {@code null})</li>
     * <li>{@code int < 0}, if {@code str1} is less than {@code str2}</li>
     * <li>{@code int > 0}, if {@code str1} is greater than {@code str2}</li>
     * </ul>
     *
     * <p>
     * This is a {@code null} safe version of :
     * </p>
     *
     * <pre>
     * str1.compareTo(str2)
     * </pre>
     *
     * <p>
     * {@code null} value is considered less than non-{@code null} value. Two {@code null} references are considered equal.
     * </p>
     *
     * <p>
     * Case-sensitive examples
     * </p>
     *
     * <pre>{@code
     * Strings.CS.compare(null, null)   = 0
     * Strings.CS.compare(null , "a")   < 0
     * Strings.CS.compare("a", null)   > 0
     * Strings.CS.compare("abc", "abc") = 0
     * Strings.CS.compare("a", "b")     < 0
     * Strings.CS.compare("b", "a")     > 0
     * Strings.CS.compare("a", "B")     > 0
     * Strings.CS.compare("ab", "abc")  < 0
     * }</pre>
     * <p>
     * Case-insensitive examples
     * </p>
     *
     * <pre>{@code
     * Strings.CI.compare(null, null)   = 0
     * Strings.CI.compare(null , "a")   < 0
     * Strings.CI.compare("a", null)    > 0
     * Strings.CI.compare("abc", "abc") = 0
     * Strings.CI.compare("abc", "ABC") = 0
     * Strings.CI.compare("a", "b")     < 0
     * Strings.CI.compare("b", "a")     > 0
     * Strings.CI.compare("a", "B")     < 0
     * Strings.CI.compare("A", "b")     < 0
     * Strings.CI.compare("ab", "ABC")  < 0
     * }</pre>
     *
     * @see String#compareTo(String)
     * @param str1 the String to compare from
     * @param str2 the String to compare to
     * @return &lt; 0, 0, &gt; 0, if {@code str1} is respectively less, equal or greater than {@code str2}
     */
    public abstract int compare(String str1, String str2);

    /**
     * Tests if CharSequence contains a search CharSequence, handling {@code null}. This method uses {@link String#indexOf(String)} if possible.
     *
     * <p>
     * A {@code null} CharSequence will return {@code false}.
     * </p>
     *
     * <p>
     * Case-sensitive examples
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
     * <p>
     * Case-insensitive examples
     * </p>
     *
     * <pre>
     * Strings.CI.contains(null, *)    = false
     * Strings.CI.contains(*, null)    = false
     * Strings.CI.contains("", "")     = true
     * Strings.CI.contains("abc", "")  = true
     * Strings.CI.contains("abc", "a") = true
     * Strings.CI.contains("abc", "z") = false
     * Strings.CI.contains("abc", "A") = true
     * Strings.CI.contains("abc", "Z") = false
     * </pre>
     *
     * @param seq       the CharSequence to check, may be null
     * @param searchSeq the CharSequence to find, may be null
     * @return true if the CharSequence contains the search CharSequence, false if not or {@code null} string input
     */
    public abstract boolean contains(CharSequence seq, CharSequence searchSeq);

    /**
     * Tests if the CharSequence contains any of the CharSequences in the given array.
     *
     * <p>
     * A {@code null} {@code cs} CharSequence will return {@code false}. A {@code null} or zero length search array will return {@code false}.
     * </p>
     *
     * <p>
     * Case-sensitive examples
     * </p>
     *
     * <pre>
     * Strings.CS.containsAny(null, *)            = false
     * Strings.CS.containsAny("", *)              = false
     * Strings.CS.containsAny(*, null)            = false
     * Strings.CS.containsAny(*, [])              = false
     * Strings.CS.containsAny("abcd", "ab", null) = true
     * Strings.CS.containsAny("abcd", "ab", "cd") = true
     * Strings.CS.containsAny("abc", "d", "abc")  = true
     * </pre>
     * <p>
     * Case-insensitive examples
     * </p>
     *
     * <pre>
     * Strings.CI.containsAny(null, *)            = false
     * Strings.CI.containsAny("", *)              = false
     * Strings.CI.containsAny(*, null)            = false
     * Strings.CI.containsAny(*, [])              = false
     * Strings.CI.containsAny("abcd", "ab", null) = true
     * Strings.CI.containsAny("abcd", "ab", "cd") = true
     * Strings.CI.containsAny("abc", "d", "abc")  = true
     * Strings.CI.containsAny("abc", "D", "ABC")  = true
     * Strings.CI.containsAny("ABC", "d", "abc")  = true
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
     * Tests if a CharSequence ends with a specified suffix.
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

    /**
     * Tests if a CharSequence ends with any of the provided suffixes.
     *
     * <p>
     * Case-sensitive examples
     * </p>
     *
     * <pre>
     * Strings.CS.endsWithAny(null, null)                  = false
     * Strings.CS.endsWithAny(null, new String[] {"abc"})  = false
     * Strings.CS.endsWithAny("abcxyz", null)              = false
     * Strings.CS.endsWithAny("abcxyz", new String[] {""}) = true
     * Strings.CS.endsWithAny("abcxyz", new String[] {"xyz"}) = true
     * Strings.CS.endsWithAny("abcxyz", new String[] {null, "xyz", "abc"}) = true
     * Strings.CS.endsWithAny("abcXYZ", "def", "XYZ")      = true
     * Strings.CS.endsWithAny("abcXYZ", "def", "xyz")      = false
     * </pre>
     *
     * @param sequence      the CharSequence to check, may be null
     * @param searchStrings the CharSequence suffixes to find, may be empty or contain {@code null}
     * @see Strings#endsWith(CharSequence, CharSequence)
     * @return {@code true} if the input {@code sequence} is {@code null} AND no {@code searchStrings} are provided, or the input {@code sequence} ends in any
     *         of the provided {@code searchStrings}.
     */
    public boolean endsWithAny(final CharSequence sequence, final CharSequence... searchStrings) {
        if (StringUtils.isEmpty(sequence) || ArrayUtils.isEmpty(searchStrings)) {
            return false;
        }
        for (final CharSequence searchString : searchStrings) {
            if (endsWith(sequence, searchString)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Compares two CharSequences, returning {@code true} if they represent equal sequences of characters.
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
     * Strings.CS.equals(null, null)   = true
     * Strings.CS.equals(null, "abc")  = false
     * Strings.CS.equals("abc", null)  = false
     * Strings.CS.equals("abc", "abc") = true
     * Strings.CS.equals("abc", "ABC") = false
     * </pre>
     * <p>
     * Case-insensitive examples
     * </p>
     *
     * <pre>
     * Strings.CI.equals(null, null)   = true
     * Strings.CI.equals(null, "abc")  = false
     * Strings.CI.equals("abc", null)  = false
     * Strings.CI.equals("abc", "abc") = true
     * Strings.CI.equals("abc", "ABC") = true
     * </pre>
     *
     * @param cs1 the first CharSequence, may be {@code null}
     * @param cs2 the second CharSequence, may be {@code null}
     * @return {@code true} if the CharSequences are equal (case-sensitive), or both {@code null}
     * @see Object#equals(Object)
     * @see String#compareTo(String)
     * @see String#equalsIgnoreCase(String)
     */
    public abstract boolean equals(CharSequence cs1, CharSequence cs2);

    /**
     * Compares two CharSequences, returning {@code true} if they represent equal sequences of characters.
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
     * Strings.CS.equals(null, null)   = true
     * Strings.CS.equals(null, "abc")  = false
     * Strings.CS.equals("abc", null)  = false
     * Strings.CS.equals("abc", "abc") = true
     * Strings.CS.equals("abc", "ABC") = false
     * </pre>
     * <p>
     * Case-insensitive examples
     * </p>
     *
     * <pre>
     * Strings.CI.equals(null, null)   = true
     * Strings.CI.equals(null, "abc")  = false
     * Strings.CI.equals("abc", null)  = false
     * Strings.CI.equals("abc", "abc") = true
     * Strings.CI.equals("abc", "ABC") = true
     * </pre>
     *
     * @param str1 the first CharSequence, may be {@code null}
     * @param str2 the second CharSequence, may be {@code null}
     * @return {@code true} if the CharSequences are equal (case-sensitive), or both {@code null}
     * @see Object#equals(Object)
     * @see String#compareTo(String)
     * @see String#equalsIgnoreCase(String)
     */
    public abstract boolean equals(String str1, String str2);

    /**
     * Compares given {@code string} to a CharSequences vararg of {@code searchStrings}, returning {@code true} if the {@code string} is equal to any of the
     * {@code searchStrings}.
     *
     * <p>
     * Case-sensitive examples
     * </p>
     *
     * <pre>
     * Strings.CS.equalsAny(null, (CharSequence[]) null) = false
     * Strings.CS.equalsAny(null, null, null)    = true
     * Strings.CS.equalsAny(null, "abc", "def")  = false
     * Strings.CS.equalsAny("abc", null, "def")  = false
     * Strings.CS.equalsAny("abc", "abc", "def") = true
     * Strings.CS.equalsAny("abc", "ABC", "DEF") = false
     * </pre>
     * <p>
     * Case-insensitive examples
     * </p>
     *
     * <pre>
     * Strings.CI.equalsAny(null, (CharSequence[]) null) = false
     * Strings.CI.equalsAny(null, null, null)    = true
     * Strings.CI.equalsAny(null, "abc", "def")  = false
     * Strings.CI.equalsAny("abc", null, "def")  = false
     * Strings.CI.equalsAny("abc", "abc", "def") = true
     * Strings.CI.equalsAny("abc", "ABC", "DEF") = true
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

    /**
     * Finds the first index within a CharSequence, handling {@code null}. This method uses {@link String#indexOf(String, int)} if possible.
     *
     * <p>
     * A {@code null} CharSequence will return {@code -1}.
     * </p>
     *
     * <p>
     * Case-sensitive examples
     * </p>
     *
     * <pre>
     * Strings.CS.indexOf(null, *)          = -1
     * Strings.CS.indexOf(*, null)          = -1
     * Strings.CS.indexOf("", "")           = 0
     * Strings.CS.indexOf("", *)            = -1 (except when * = "")
     * Strings.CS.indexOf("aabaabaa", "a")  = 0
     * Strings.CS.indexOf("aabaabaa", "b")  = 2
     * Strings.CS.indexOf("aabaabaa", "ab") = 1
     * Strings.CS.indexOf("aabaabaa", "")   = 0
     * </pre>
     * <p>
     * Case-insensitive examples
     * </p>
     *
     * <pre>
     * Strings.CI.indexOf(null, *)          = -1
     * Strings.CI.indexOf(*, null)          = -1
     * Strings.CI.indexOf("", "")           = 0
     * Strings.CI.indexOf(" ", " ")         = 0
     * Strings.CI.indexOf("aabaabaa", "a")  = 0
     * Strings.CI.indexOf("aabaabaa", "b")  = 2
     * Strings.CI.indexOf("aabaabaa", "ab") = 1
     * </pre>
     *
     * @param seq       the CharSequence to check, may be null
     * @param searchSeq the CharSequence to find, may be null
     * @return the first index of the search CharSequence, -1 if no match or {@code null} string input
     */
    public int indexOf(final CharSequence seq, final CharSequence searchSeq) {
        return indexOf(seq, searchSeq, 0);
    }

    /**
     * Finds the first index within a CharSequence, handling {@code null}. This method uses {@link String#indexOf(String, int)} if possible.
     *
     * <p>
     * A {@code null} CharSequence will return {@code -1}. A negative start position is treated as zero. An empty ("") search CharSequence always matches. A
     * start position greater than the string length only matches an empty search CharSequence.
     * </p>
     *
     * <p>
     * Case-sensitive examples
     * </p>
     *
     * <pre>
     * Strings.CS.indexOf(null, *, *)          = -1
     * Strings.CS.indexOf(*, null, *)          = -1
     * Strings.CS.indexOf("", "", 0)           = 0
     * Strings.CS.indexOf("", *, 0)            = -1 (except when * = "")
     * Strings.CS.indexOf("aabaabaa", "a", 0)  = 0
     * Strings.CS.indexOf("aabaabaa", "b", 0)  = 2
     * Strings.CS.indexOf("aabaabaa", "ab", 0) = 1
     * Strings.CS.indexOf("aabaabaa", "b", 3)  = 5
     * Strings.CS.indexOf("aabaabaa", "b", 9)  = -1
     * Strings.CS.indexOf("aabaabaa", "b", -1) = 2
     * Strings.CS.indexOf("aabaabaa", "", 2)   = 2
     * Strings.CS.indexOf("abc", "", 9)        = 3
     * </pre>
     * <p>
     * Case-insensitive examples
     * </p>
     *
     * <pre>
     * Strings.CI.indexOf(null, *, *)          = -1
     * Strings.CI.indexOf(*, null, *)          = -1
     * Strings.CI.indexOf("", "", 0)           = 0
     * Strings.CI.indexOf("aabaabaa", "A", 0)  = 0
     * Strings.CI.indexOf("aabaabaa", "B", 0)  = 2
     * Strings.CI.indexOf("aabaabaa", "AB", 0) = 1
     * Strings.CI.indexOf("aabaabaa", "B", 3)  = 5
     * Strings.CI.indexOf("aabaabaa", "B", 9)  = -1
     * Strings.CI.indexOf("aabaabaa", "B", -1) = 2
     * Strings.CI.indexOf("aabaabaa", "", 2)   = 2
     * Strings.CI.indexOf("abc", "", 9)        = -1
     * </pre>
     *
     * @param seq       the CharSequence to check, may be null
     * @param searchSeq the CharSequence to find, may be null
     * @param startPos  the start position, negative treated as zero
     * @return the first index of the search CharSequence (always &ge; startPos), -1 if no match or {@code null} string input
     */
    public abstract int indexOf(CharSequence seq, CharSequence searchSeq, int startPos);

    /**
     * Tests whether to ignore case.
     *
     * @return whether to ignore case.
     */
    public boolean isCaseSensitive() {
        return !ignoreCase;
    }

    /**
     * Tests whether null is less when comparing.
     *
     * @return whether null is less when comparing.
     */
    boolean isNullIsLess() {
        return nullIsLess;
    }

    /**
     * Finds the last index within a CharSequence, handling {@code null}. This method uses {@link String#lastIndexOf(String)} if possible.
     *
     * <p>
     * A {@code null} CharSequence will return {@code -1}.
     * </p>
     *
     * <p>
     * Case-sensitive examples
     * </p>
     *
     * <pre>
     * Strings.CS.lastIndexOf(null, *)          = -1
     * Strings.CS.lastIndexOf(*, null)          = -1
     * Strings.CS.lastIndexOf("", "")           = 0
     * Strings.CS.lastIndexOf("aabaabaa", "a")  = 7
     * Strings.CS.lastIndexOf("aabaabaa", "b")  = 5
     * Strings.CS.lastIndexOf("aabaabaa", "ab") = 4
     * Strings.CS.lastIndexOf("aabaabaa", "")   = 8
     * </pre>
     * <p>
     * Case-insensitive examples
     * </p>
     *
     * <pre>
     * Strings.CI.lastIndexOf(null, *)          = -1
     * Strings.CI.lastIndexOf(*, null)          = -1
     * Strings.CI.lastIndexOf("aabaabaa", "A")  = 7
     * Strings.CI.lastIndexOf("aabaabaa", "B")  = 5
     * Strings.CI.lastIndexOf("aabaabaa", "AB") = 4
     * </pre>
     *
     * @param str       the CharSequence to check, may be null
     * @param searchStr the CharSequence to find, may be null
     * @return the last index of the search String, -1 if no match or {@code null} string input
     */
    public int lastIndexOf(final CharSequence str, final CharSequence searchStr) {
        if (str == null) {
            return INDEX_NOT_FOUND;
        }
        return lastIndexOf(str, searchStr, str.length());
    }

    /**
     * Finds the last index within a CharSequence, handling {@code null}. This method uses {@link String#lastIndexOf(String, int)} if possible.
     *
     * <p>
     * A {@code null} CharSequence will return {@code -1}. A negative start position returns {@code -1}. An empty ("") search CharSequence always matches unless
     * the start position is negative. A start position greater than the string length searches the whole string. The search starts at the startPos and works
     * backwards; matches starting after the start position are ignored.
     * </p>
     *
     * <p>
     * Case-sensitive examples
     * </p>
     *
     * <pre>
     * Strings.CS.lastIndexOf(null, *, *)          = -1
     * Strings.CS.lastIndexOf(*, null, *)          = -1
     * Strings.CS.lastIndexOf("aabaabaa", "a", 8)  = 7
     * Strings.CS.lastIndexOf("aabaabaa", "b", 8)  = 5
     * Strings.CS.lastIndexOf("aabaabaa", "ab", 8) = 4
     * Strings.CS.lastIndexOf("aabaabaa", "b", 9)  = 5
     * Strings.CS.lastIndexOf("aabaabaa", "b", -1) = -1
     * Strings.CS.lastIndexOf("aabaabaa", "a", 0)  = 0
     * Strings.CS.lastIndexOf("aabaabaa", "b", 0)  = -1
     * Strings.CS.lastIndexOf("aabaabaa", "b", 1)  = -1
     * Strings.CS.lastIndexOf("aabaabaa", "b", 2)  = 2
     * Strings.CS.lastIndexOf("aabaabaa", "ba", 2)  = 2
     * </pre>
     * <p>
     * Case-insensitive examples
     * </p>
     *
     * <pre>
     * Strings.CI.lastIndexOf(null, *, *)          = -1
     * Strings.CI.lastIndexOf(*, null, *)          = -1
     * Strings.CI.lastIndexOf("aabaabaa", "A", 8)  = 7
     * Strings.CI.lastIndexOf("aabaabaa", "B", 8)  = 5
     * Strings.CI.lastIndexOf("aabaabaa", "AB", 8) = 4
     * Strings.CI.lastIndexOf("aabaabaa", "B", 9)  = 5
     * Strings.CI.lastIndexOf("aabaabaa", "B", -1) = -1
     * Strings.CI.lastIndexOf("aabaabaa", "A", 0)  = 0
     * Strings.CI.lastIndexOf("aabaabaa", "B", 0)  = -1
     * </pre>
     *
     * @param seq       the CharSequence to check, may be null
     * @param searchSeq the CharSequence to find, may be null
     * @param startPos  the start position, negative treated as zero
     * @return the last index of the search CharSequence (always &le; startPos), -1 if no match or {@code null} string input
     */
    public abstract int lastIndexOf(CharSequence seq, CharSequence searchSeq, int startPos);

    /**
     * Prepends the prefix to the start of the string if the string does not already start with any of the prefixes.
     *
     * <p>
     * Case-sensitive examples
     * </p>
     *
     * <pre>
     * Strings.CS.prependIfMissing(null, null) = null
     * Strings.CS.prependIfMissing("abc", null) = "abc"
     * Strings.CS.prependIfMissing("", "xyz") = "xyz"
     * Strings.CS.prependIfMissing("abc", "xyz") = "xyzabc"
     * Strings.CS.prependIfMissing("xyzabc", "xyz") = "xyzabc"
     * Strings.CS.prependIfMissing("XYZabc", "xyz") = "xyzXYZabc"
     * </pre>
     * <p>
     * With additional prefixes,
     * </p>
     *
     * <pre>
     * Strings.CS.prependIfMissing(null, null, null) = null
     * Strings.CS.prependIfMissing("abc", null, null) = "abc"
     * Strings.CS.prependIfMissing("", "xyz", null) = "xyz"
     * Strings.CS.prependIfMissing("abc", "xyz", new CharSequence[]{null}) = "xyzabc"
     * Strings.CS.prependIfMissing("abc", "xyz", "") = "abc"
     * Strings.CS.prependIfMissing("abc", "xyz", "mno") = "xyzabc"
     * Strings.CS.prependIfMissing("xyzabc", "xyz", "mno") = "xyzabc"
     * Strings.CS.prependIfMissing("mnoabc", "xyz", "mno") = "mnoabc"
     * Strings.CS.prependIfMissing("XYZabc", "xyz", "mno") = "xyzXYZabc"
     * Strings.CS.prependIfMissing("MNOabc", "xyz", "mno") = "xyzMNOabc"
     * </pre>
     *
     * <p>
     * Case-insensitive examples
     * </p>
     *
     * <pre>
     * Strings.CI.prependIfMissing(null, null) = null
     * Strings.CI.prependIfMissing("abc", null) = "abc"
     * Strings.CI.prependIfMissing("", "xyz") = "xyz"
     * Strings.CI.prependIfMissing("abc", "xyz") = "xyzabc"
     * Strings.CI.prependIfMissing("xyzabc", "xyz") = "xyzabc"
     * Strings.CI.prependIfMissing("XYZabc", "xyz") = "XYZabc"
     * </pre>
     * <p>
     * With additional prefixes,
     * </p>
     *
     * <pre>
     * Strings.CI.prependIfMissing(null, null, null) = null
     * Strings.CI.prependIfMissing("abc", null, null) = "abc"
     * Strings.CI.prependIfMissing("", "xyz", null) = "xyz"
     * Strings.CI.prependIfMissing("abc", "xyz", new CharSequence[]{null}) = "xyzabc"
     * Strings.CI.prependIfMissing("abc", "xyz", "") = "abc"
     * Strings.CI.prependIfMissing("abc", "xyz", "mno") = "xyzabc"
     * Strings.CI.prependIfMissing("xyzabc", "xyz", "mno") = "xyzabc"
     * Strings.CI.prependIfMissing("mnoabc", "xyz", "mno") = "mnoabc"
     * Strings.CI.prependIfMissing("XYZabc", "xyz", "mno") = "XYZabc"
     * Strings.CI.prependIfMissing("MNOabc", "xyz", "mno") = "MNOabc"
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
     * Removes all occurrences of a substring from within the source string.
     *
     * <p>
     * A {@code null} source string will return {@code null}. An empty ("") source string will return the empty string. A {@code null} remove string will return
     * the source string. An empty ("") remove string will return the source string.
     * </p>
     *
     * <p>
     * Case-sensitive examples
     * </p>
     *
     * <pre>
     * Strings.CS.remove(null, *)        = null
     * Strings.CS.remove("", *)          = ""
     * Strings.CS.remove(*, null)        = *
     * Strings.CS.remove(*, "")          = *
     * Strings.CS.remove("queued", "ue") = "qd"
     * Strings.CS.remove("queued", "zz") = "queued"
     * </pre>
     *
     * <p>
     * Case-insensitive examples
     * </p>
     *
     * <pre>
     * Strings.CI.remove(null, *)        = null
     * Strings.CI.remove("", *)          = ""
     * Strings.CI.remove(*, null)        = *
     * Strings.CI.remove(*, "")          = *
     * Strings.CI.remove("queued", "ue") = "qd"
     * Strings.CI.remove("queued", "zz") = "queued"
     * Strings.CI.remove("quEUed", "UE") = "qd"
     * Strings.CI.remove("queued", "zZ") = "queued"
     * </pre>
     *
     * @param str    the source String to search, may be null
     * @param remove the String to search for and remove, may be null
     * @return the substring with the string removed if found, {@code null} if null String input
     */
    public String remove(final String str, final String remove) {
        return replace(str, remove, StringUtils.EMPTY, -1);
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
     * Strings.CS.removeEnd(null, *)      = null
     * Strings.CS.removeEnd("", *)        = ""
     * Strings.CS.removeEnd(*, null)      = *
     * Strings.CS.removeEnd("www.domain.com", ".com.")  = "www.domain.com"
     * Strings.CS.removeEnd("www.domain.com", ".com")   = "www.domain"
     * Strings.CS.removeEnd("www.domain.com", "domain") = "www.domain.com"
     * Strings.CS.removeEnd("abc", "")    = "abc"
     * </pre>
     * <p>
     * Case-insensitive examples
     * </p>
     *
     * <pre>
     * Strings.CI.removeEnd(null, *)      = null
     * Strings.CI.removeEnd("", *)        = ""
     * Strings.CI.removeEnd(*, null)      = *
     * Strings.CI.removeEnd("www.domain.com", ".com.")  = "www.domain.com"
     * Strings.CI.removeEnd("www.domain.com", ".com")   = "www.domain"
     * Strings.CI.removeEnd("www.domain.com", "domain") = "www.domain.com"
     * Strings.CI.removeEnd("abc", "")    = "abc"
     * Strings.CI.removeEnd("www.domain.com", ".COM") = "www.domain")
     * Strings.CI.removeEnd("www.domain.COM", ".com") = "www.domain")
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
     * Case-sensitive examples
     * </p>
     *
     * <pre>
     * Strings.CS.removeStart(null, *)      = null
     * Strings.CS.removeStart("", *)        = ""
     * Strings.CS.removeStart(*, null)      = *
     * Strings.CS.removeStart("www.domain.com", "www.")   = "domain.com"
     * Strings.CS.removeStart("domain.com", "www.")       = "domain.com"
     * Strings.CS.removeStart("www.domain.com", "domain") = "www.domain.com"
     * Strings.CS.removeStart("abc", "")    = "abc"
     * </pre>
     * <p>
     * Case-insensitive examples
     * </p>
     *
     * <pre>
     * Strings.CI.removeStart(null, *)      = null
     * Strings.CI.removeStart("", *)        = ""
     * Strings.CI.removeStart(*, null)      = *
     * Strings.CI.removeStart("www.domain.com", "www.")   = "domain.com"
     * Strings.CI.removeStart("www.domain.com", "WWW.")   = "domain.com"
     * Strings.CI.removeStart("domain.com", "www.")       = "domain.com"
     * Strings.CI.removeStart("www.domain.com", "domain") = "www.domain.com"
     * Strings.CI.removeStart("abc", "")    = "abc"
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
     * Strings.CS.replace(null, *, *)        = null
     * Strings.CS.replace("", *, *)          = ""
     * Strings.CS.replace("any", null, *)    = "any"
     * Strings.CS.replace("any", *, null)    = "any"
     * Strings.CS.replace("any", "", *)      = "any"
     * Strings.CS.replace("aba", "a", null)  = "aba"
     * Strings.CS.replace("aba", "a", "")    = "b"
     * Strings.CS.replace("aba", "a", "z")   = "zbz"
     * </pre>
     * <p>
     * Case-insensitive examples
     * </p>
     *
     * <pre>
     * Strings.CI.replace(null, *, *)        = null
     * Strings.CI.replace("", *, *)          = ""
     * Strings.CI.replace("any", null, *)    = "any"
     * Strings.CI.replace("any", *, null)    = "any"
     * Strings.CI.replace("any", "", *)      = "any"
     * Strings.CI.replace("aba", "a", null)  = "aba"
     * Strings.CI.replace("abA", "A", "")    = "b"
     * Strings.CI.replace("aba", "A", "z")   = "zbz"
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
     * Replaces a String with another String inside a larger String, for the first {@code max} values of the search String.
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
     * Strings.CS.replace(null, *, *, *)         = null
     * Strings.CS.replace("", *, *, *)           = ""
     * Strings.CS.replace("any", null, *, *)     = "any"
     * Strings.CS.replace("any", *, null, *)     = "any"
     * Strings.CS.replace("any", "", *, *)       = "any"
     * Strings.CS.replace("any", *, *, 0)        = "any"
     * Strings.CS.replace("abaa", "a", null, -1) = "abaa"
     * Strings.CS.replace("abaa", "a", "", -1)   = "b"
     * Strings.CS.replace("abaa", "a", "z", 0)   = "abaa"
     * Strings.CS.replace("abaa", "a", "z", 1)   = "zbaa"
     * Strings.CS.replace("abaa", "a", "z", 2)   = "zbza"
     * Strings.CS.replace("abaa", "a", "z", -1)  = "zbzz"
     * </pre>
     * <p>
     * Case-insensitive examples
     * </p>
     *
     * <pre>
     * Strings.CI.replace(null, *, *, *)         = null
     * Strings.CI.replace("", *, *, *)           = ""
     * Strings.CI.replace("any", null, *, *)     = "any"
     * Strings.CI.replace("any", *, null, *)     = "any"
     * Strings.CI.replace("any", "", *, *)       = "any"
     * Strings.CI.replace("any", *, *, 0)        = "any"
     * Strings.CI.replace("abaa", "a", null, -1) = "abaa"
     * Strings.CI.replace("abaa", "a", "", -1)   = "b"
     * Strings.CI.replace("abaa", "a", "z", 0)   = "abaa"
     * Strings.CI.replace("abaa", "A", "z", 1)   = "zbaa"
     * Strings.CI.replace("abAa", "a", "z", 2)   = "zbza"
     * Strings.CI.replace("abAa", "a", "z", -1)  = "zbzz"
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
     * Strings.CS.replaceOnce(null, *, *)        = null
     * Strings.CS.replaceOnce("", *, *)          = ""
     * Strings.CS.replaceOnce("any", null, *)    = "any"
     * Strings.CS.replaceOnce("any", *, null)    = "any"
     * Strings.CS.replaceOnce("any", "", *)      = "any"
     * Strings.CS.replaceOnce("aba", "a", null)  = "aba"
     * Strings.CS.replaceOnce("aba", "a", "")    = "ba"
     * Strings.CS.replaceOnce("aba", "a", "z")   = "zba"
     * </pre>
     *
     * <p>
     * Case-insensitive examples
     * </p>
     *
     * <pre>
     * Strings.CI.replaceOnce(null, *, *)        = null
     * Strings.CI.replaceOnce("", *, *)          = ""
     * Strings.CI.replaceOnce("any", null, *)    = "any"
     * Strings.CI.replaceOnce("any", *, null)    = "any"
     * Strings.CI.replaceOnce("any", "", *)      = "any"
     * Strings.CI.replaceOnce("aba", "a", null)  = "aba"
     * Strings.CI.replaceOnce("aba", "a", "")    = "ba"
     * Strings.CI.replaceOnce("aba", "a", "z")   = "zba"
     * Strings.CI.replaceOnce("FoOFoofoo", "foo", "") = "Foofoo"
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
     * Strings.CS.startsWith(null, null)      = true
     * Strings.CS.startsWith(null, "abc")     = false
     * Strings.CS.startsWith("abcdef", null)  = false
     * Strings.CS.startsWith("abcdef", "abc") = true
     * Strings.CS.startsWith("ABCDEF", "abc") = false
     * </pre>
     *
     * <p>
     * Case-insensitive examples
     * </p>
     *
     * <pre>
     * Strings.CI.startsWith(null, null)      = true
     * Strings.CI.startsWith(null, "abc")     = false
     * Strings.CI.startsWith("abcdef", null)  = false
     * Strings.CI.startsWith("abcdef", "abc") = true
     * Strings.CI.startsWith("ABCDEF", "abc") = true
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

    /**
     * Tests if a CharSequence starts with any of the provided prefixes.
     *
     * <p>
     * Case-sensitive examples
     * </p>
     *
     * <pre>
     * Strings.CS.startsWithAny(null, null)      = false
     * Strings.CS.startsWithAny(null, new String[] {"abc"})  = false
     * Strings.CS.startsWithAny("abcxyz", null)     = false
     * Strings.CS.startsWithAny("abcxyz", new String[] {""}) = true
     * Strings.CS.startsWithAny("abcxyz", new String[] {"abc"}) = true
     * Strings.CS.startsWithAny("abcxyz", new String[] {null, "xyz", "abc"}) = true
     * Strings.CS.startsWithAny("abcxyz", null, "xyz", "ABCX") = false
     * Strings.CS.startsWithAny("ABCXYZ", null, "xyz", "abc") = false
     * </pre>
     *
     * <p>
     * Case-insensitive examples
     * </p>
     *
     * <pre>
     * Strings.CI.startsWithAny(null, null)      = false
     * Strings.CI.startsWithAny(null, new String[] {"aBc"})  = false
     * Strings.CI.startsWithAny("AbCxYz", null)     = false
     * Strings.CI.startsWithAny("AbCxYz", new String[] {""}) = true
     * Strings.CI.startsWithAny("AbCxYz", new String[] {"aBc"}) = true
     * Strings.CI.startsWithAny("AbCxYz", new String[] {null, "XyZ", "aBc"}) = true
     * Strings.CI.startsWithAny("abcxyz", null, "xyz", "ABCX") = true
     * Strings.CI.startsWithAny("ABCXYZ", null, "xyz", "abc") = true
     * </pre>
     *
     * @param sequence      the CharSequence to check, may be null
     * @param searchStrings the CharSequence prefixes, may be empty or contain {@code null}
     * @see Strings#startsWith(CharSequence, CharSequence)
     * @return {@code true} if the input {@code sequence} is {@code null} AND no {@code searchStrings} are provided, or the input {@code sequence} begins with
     *         any of the provided {@code searchStrings}.
     */
    public boolean startsWithAny(final CharSequence sequence, final CharSequence... searchStrings) {
        if (StringUtils.isEmpty(sequence) || ArrayUtils.isEmpty(searchStrings)) {
            return false;
        }
        for (final CharSequence searchString : searchStrings) {
            if (startsWith(sequence, searchString)) {
                return true;
            }
        }
        return false;
    }

}
