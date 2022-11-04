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
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Objects;

/**
 * A contiguous range of characters, optionally negated.
 *
 * <p>Instances are immutable.</p>
 *
 * <p>#ThreadSafe#</p>
 * @since 1.0
 */
// TODO: This is no longer public and will be removed later as CharSet is moved
// to depend on Range.
final class CharRange implements Iterable<Character>, Serializable {

    /**
     * Required for serialization support. Lang version 2.0.
     *
     * @see java.io.Serializable
     */
    private static final long serialVersionUID = 8270183163158333422L;

    /** The first character, inclusive, in the range. */
    private final char start;

    /** The last character, inclusive, in the range. */
    private final char end;

    /** True if the range is everything except the characters specified. */
    private final boolean negated;

    /** Cached toString. */
    private transient String iToString;

    /** Empty array. */
    static final CharRange[] EMPTY_ARRAY = {};

    /**
     * Constructs a {@link CharRange} over a set of characters,
     * optionally negating the range.
     *
     * <p>A negated range includes everything except that defined by the
     * start and end characters.</p>
     *
     * <p>If start and end are in the wrong order, they are reversed.
     * Thus {@code a-e} is the same as {@code e-a}.</p>
     *
     * @param start  first character, inclusive, in this range
     * @param end  last character, inclusive, in this range
     * @param negated  true to express everything except the range
     */
    private CharRange(char start, char end, final boolean negated) {
        if (start > end) {
            final char temp = start;
            start = end;
            end = temp;
        }

        this.start = start;
        this.end = end;
        this.negated = negated;
    }

    /**
     * Constructs a {@link CharRange} over a single character.
     *
     * @param ch  only character in this range
     * @return the new CharRange object
     * @since 2.5
     */
    public static CharRange is(final char ch) {
        return new CharRange(ch, ch, false);
    }

    /**
     * Constructs a negated {@link CharRange} over a single character.
     *
     * <p>A negated range includes everything except that defined by the
     * single character.</p>
     *
     * @param ch  only character in this range
     * @return the new CharRange object
     * @since 2.5
     */
    public static CharRange isNot(final char ch) {
        return new CharRange(ch, ch, true);
    }

    /**
     * Constructs a {@link CharRange} over a set of characters.
     *
     * <p>If start and end are in the wrong order, they are reversed.
     * Thus {@code a-e} is the same as {@code e-a}.</p>
     *
     * @param start  first character, inclusive, in this range
     * @param end  last character, inclusive, in this range
     * @return the new CharRange object
     * @since 2.5
     */
    public static CharRange isIn(final char start, final char end) {
        return new CharRange(start, end, false);
    }

    /**
     * Constructs a negated {@link CharRange} over a set of characters.
     *
     * <p>A negated range includes everything except that defined by the
     * start and end characters.</p>
     *
     * <p>If start and end are in the wrong order, they are reversed.
     * Thus {@code a-e} is the same as {@code e-a}.</p>
     *
     * @param start  first character, inclusive, in this range
     * @param end  last character, inclusive, in this range
     * @return the new CharRange object
     * @since 2.5
     */
    public static CharRange isNotIn(final char start, final char end) {
        return new CharRange(start, end, true);
    }

    // Accessors
    /**
     * Gets the start character for this character range.
     *
     * @return the start char (inclusive)
     */
    public char getStart() {
        return this.start;
    }

    /**
     * Gets the end character for this character range.
     *
     * @return the end char (inclusive)
     */
    public char getEnd() {
        return this.end;
    }

    /**
     * Is this {@link CharRange} negated.
     *
     * <p>A negated range includes everything except that defined by the
     * start and end characters.</p>
     *
     * @return {@code true} if negated
     */
    public boolean isNegated() {
        return negated;
    }

    // Contains
    /**
     * Is the character specified contained in this range.
     *
     * @param ch  the character to check
     * @return {@code true} if this range contains the input character
     */
    public boolean contains(final char ch) {
        return (ch >= start && ch <= end) != negated;
    }

    /**
     * Are all the characters of the passed in range contained in
     * this range.
     *
     * @param range  the range to check against
     * @return {@code true} if this range entirely contains the input range
     * @throws NullPointerException if {@code null} input
     */
    public boolean contains(final CharRange range) {
        Objects.requireNonNull(range, "range");
        if (negated) {
            if (range.negated) {
                return start >= range.start && end <= range.end;
            }
            return range.end < start || range.start > end;
        }
        if (range.negated) {
            return start == 0 && end == Character.MAX_VALUE;
        }
        return start <= range.start && end >= range.end;
    }

    // Basics
    /**
     * Compares two CharRange objects, returning true if they represent
     * exactly the same range of characters defined in the same way.
     *
     * @param obj  the object to compare to
     * @return true if equal
     */
    @Override
    public boolean equals(final Object obj) {
        if (obj == this) {
            return true;
        }
        if (!(obj instanceof CharRange)) {
            return false;
        }
        final CharRange other = (CharRange) obj;
        return start == other.start && end == other.end && negated == other.negated;
    }

    /**
     * Gets a hashCode compatible with the equals method.
     *
     * @return a suitable hashCode
     */
    @Override
    public int hashCode() {
        return 83 + start + 7 * end + (negated ? 1 : 0);
    }

    /**
     * Gets a string representation of the character range.
     *
     * @return string representation of this range
     */
    @Override
    public String toString() {
        if (iToString == null) {
            final StringBuilder buf = new StringBuilder(4);
            if (isNegated()) {
                buf.append('^');
            }
            buf.append(start);
            if (start != end) {
                buf.append('-');
                buf.append(end);
            }
            iToString = buf.toString();
        }
        return iToString;
    }

    /**
     * Returns an iterator which can be used to walk through the characters described by this range.
     *
     * <p>#NotThreadSafe# the iterator is not thread-safe</p>
     * @return an iterator to the chars represented by this range
     * @since 2.5
     */
    @Override
    public Iterator<Character> iterator() {
        return new CharacterIterator(this);
    }

    /**
     * Character {@link Iterator}.
     * <p>#NotThreadSafe#</p>
     */
    private static class CharacterIterator implements Iterator<Character> {
        /** The current character */
        private char current;

        private final CharRange range;
        private boolean hasNext;

        /**
         * Constructs a new iterator for the character range.
         *
         * @param r The character range
         */
        private CharacterIterator(final CharRange r) {
            range = r;
            hasNext = true;

            if (range.negated) {
                if (range.start == 0) {
                    if (range.end == Character.MAX_VALUE) {
                        // This range is an empty set
                        hasNext = false;
                    } else {
                        current = (char) (range.end + 1);
                    }
                } else {
                    current = 0;
                }
            } else {
                current = range.start;
            }
        }

        /**
         * Prepares the next character in the range.
         */
        private void prepareNext() {
            if (range.negated) {
                if (current == Character.MAX_VALUE) {
                    hasNext = false;
                } else if (current + 1 == range.start) {
                    if (range.end == Character.MAX_VALUE) {
                        hasNext = false;
                    } else {
                        current = (char) (range.end + 1);
                    }
                } else {
                    current = (char) (current + 1);
                }
            } else if (current < range.end) {
                current = (char) (current + 1);
            } else {
                hasNext = false;
            }
        }

        /**
         * Has the iterator not reached the end character yet?
         *
         * @return {@code true} if the iterator has yet to reach the character date
         */
        @Override
        public boolean hasNext() {
            return hasNext;
        }

        /**
         * Returns the next character in the iteration
         *
         * @return {@link Character} for the next character
         */
        @Override
        public Character next() {
            if (!hasNext) {
                throw new NoSuchElementException();
            }
            final char cur = current;
            prepareNext();
            return Character.valueOf(cur);
        }

        /**
         * Always throws UnsupportedOperationException.
         *
         * @throws UnsupportedOperationException Always thrown.
         * @see java.util.Iterator#remove()
         */
        @Override
        public void remove() {
            throw new UnsupportedOperationException();
        }
    }
}
