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

import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

/**
 * A set of characters.
 *
 * <p>Instances are immutable, but instances of subclasses may not be.</p>
 *
 * <p>#ThreadSafe#</p>
 * @since 1.0
 */
public class CharSet implements Serializable {

    /**
     * Required for serialization support. Lang version 2.0.
     *
     * @see java.io.Serializable
     */
    private static final long serialVersionUID = 5947847346149275958L;

    /**
     * A CharSet defining no characters.
     * @since 2.0
     */
    public static final CharSet EMPTY = new CharSet((String) null);

    /**
     * A CharSet defining ASCII alphabetic characters "a-zA-Z".
     * @since 2.0
     */
    public static final CharSet ASCII_ALPHA = new CharSet("a-zA-Z");

    /**
     * A CharSet defining ASCII alphabetic characters "a-z".
     * @since 2.0
     */
    public static final CharSet ASCII_ALPHA_LOWER = new CharSet("a-z");

    /**
     * A CharSet defining ASCII alphabetic characters "A-Z".
     * @since 2.0
     */
    public static final CharSet ASCII_ALPHA_UPPER = new CharSet("A-Z");

    /**
     * A CharSet defining ASCII alphabetic characters "0-9".
     * @since 2.0
     */
    public static final CharSet ASCII_NUMERIC = new CharSet("0-9");

    /**
     * A Map of the common cases used in the factory.
     * Subclasses can add more common patterns if desired
     * @since 2.0
     */
    protected static final Map<String, CharSet> COMMON = Collections.synchronizedMap(new HashMap<>());

    static {
        COMMON.put(null, EMPTY);
        COMMON.put(StringUtils.EMPTY, EMPTY);
        COMMON.put("a-zA-Z", ASCII_ALPHA);
        COMMON.put("A-Za-z", ASCII_ALPHA);
        COMMON.put("a-z", ASCII_ALPHA_LOWER);
        COMMON.put("A-Z", ASCII_ALPHA_UPPER);
        COMMON.put("0-9", ASCII_NUMERIC);
    }

    /** The set of CharRange objects. */
    private final Set<CharRange> set = Collections.synchronizedSet(new HashSet<>());

    /**
     * Factory method to create a new CharSet using a special syntax.
     *
     * <ul>
     *  <li>{@code null} or empty string ("")
     * - set containing no characters</li>
     *  <li>Single character, such as "a"
     *  - set containing just that character</li>
     *  <li>Multi character, such as "a-e"
     *  - set containing characters from one character to the other</li>
     *  <li>Negated, such as "^a" or "^a-e"
     *  - set containing all characters except those defined</li>
     *  <li>Combinations, such as "abe-g"
     *  - set containing all the characters from the individual sets</li>
     * </ul>
     *
     * <p>The matching order is:</p>
     * <ol>
     *  <li>Negated multi character range, such as "^a-e"
     *  <li>Ordinary multi character range, such as "a-e"
     *  <li>Negated single character, such as "^a"
     *  <li>Ordinary single character, such as "a"
     * </ol>
     *
     * <p>Matching works left to right. Once a match is found the
     * search starts again from the next character.</p>
     *
     * <p>If the same range is defined twice using the same syntax, only
     * one range will be kept.
     * Thus, "a-ca-c" creates only one range of "a-c".</p>
     *
     * <p>If the start and end of a range are in the wrong order,
     * they are reversed. Thus "a-e" is the same as "e-a".
     * As a result, "a-ee-a" would create only one range,
     * as the "a-e" and "e-a" are the same.</p>
     *
     * <p>The set of characters represented is the union of the specified ranges.</p>
     *
     * <p>There are two ways to add a literal negation character ({@code ^}):</p>
     * <ul>
     *     <li>As the last character in a string, e.g. {@code CharSet.getInstance("a-z^")}</li>
     *     <li>As a separate element, e.g. {@code CharSet.getInstance("^", "a-z")}</li>
     * </ul>
     *
     * <p>Examples using the negation character:</p>
     * <pre>
     *     CharSet.getInstance("^a-c").contains('a') = false
     *     CharSet.getInstance("^a-c").contains('d') = true
     *     CharSet.getInstance("^^a-c").contains('a') = true // (only '^' is negated)
     *     CharSet.getInstance("^^a-c").contains('^') = false
     *     CharSet.getInstance("^a-cd-f").contains('d') = true
     *     CharSet.getInstance("a-c^").contains('^') = true
     *     CharSet.getInstance("^", "a-c").contains('^') = true
     * </pre>
     *
     * <p>All CharSet objects returned by this method will be immutable.</p>
     *
     * @param setStrs  Strings to merge into the set, may be null
     * @return a CharSet instance
     * @since 2.4
     */
    public static CharSet getInstance(final String... setStrs) {
        if (setStrs == null) {
            return null;
        }
        if (setStrs.length == 1) {
            final CharSet common = COMMON.get(setStrs[0]);
            if (common != null) {
                return common;
            }
        }
        return new CharSet(setStrs);
    }

    /**
     * Constructs a new CharSet using the set syntax.
     * Each string is merged in with the set.
     *
     * @param set  Strings to merge into the initial set
     * @throws NullPointerException if set is {@code null}
     */
    protected CharSet(final String... set) {
        Stream.of(set).forEach(this::add);
    }

    /**
     * Add a set definition string to the {@link CharSet}.
     *
     * @param str  set definition string
     */
    protected void add(final String str) {
        if (str == null) {
            return;
        }

        final int len = str.length();
        int pos = 0;
        while (pos < len) {
            final int remainder = len - pos;
            if (remainder >= 4 && str.charAt(pos) == '^' && str.charAt(pos + 2) == '-') {
                // negated range
                set.add(CharRange.isNotIn(str.charAt(pos + 1), str.charAt(pos + 3)));
                pos += 4;
            } else if (remainder >= 3 && str.charAt(pos + 1) == '-') {
                // range
                set.add(CharRange.isIn(str.charAt(pos), str.charAt(pos + 2)));
                pos += 3;
            } else if (remainder >= 2 && str.charAt(pos) == '^') {
                // negated char
                set.add(CharRange.isNot(str.charAt(pos + 1)));
                pos += 2;
            } else {
                // char
                set.add(CharRange.is(str.charAt(pos)));
                pos += 1;
            }
        }
    }

    /**
     * Gets the internal set as an array of CharRange objects.
     *
     * @return an array of immutable CharRange objects
     * @since 2.0
     */
// NOTE: This is no longer public as CharRange is no longer a public class.
//       It may be replaced when CharSet moves to Range.
    /*public*/ CharRange[] getCharRanges() {
        return set.toArray(CharRange.EMPTY_ARRAY);
    }

    /**
     * Does the {@link CharSet} contain the specified
     * character {@code ch}.
     *
     * @param ch  the character to check for
     * @return {@code true} if the set contains the characters
     */
    public boolean contains(final char ch) {
        synchronized (set) {
            return set.stream().anyMatch(range -> range.contains(ch));
        }
    }

    // Basics
    /**
     * Compares two {@link CharSet} objects, returning true if they represent
     * exactly the same set of characters defined in the same way.
     *
     * <p>The two sets {@code abc} and {@code a-c} are <i>not</i>
     * equal according to this method.</p>
     *
     * @param obj  the object to compare to
     * @return true if equal
     * @since 2.0
     */
    @Override
    public boolean equals(final Object obj) {
        if (obj == this) {
            return true;
        }
        if (!(obj instanceof CharSet)) {
            return false;
        }
        final CharSet other = (CharSet) obj;
        return set.equals(other.set);
    }

    /**
     * Gets a hash code compatible with the equals method.
     *
     * @return a suitable hash code
     * @since 2.0
     */
    @Override
    public int hashCode() {
        return 89 + set.hashCode();
    }

    /**
     * Gets a string representation of the set.
     *
     * @return string representation of the set
     */
    @Override
    public String toString() {
        return set.toString();
    }

}
