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
 * <p>Operations on {@link CharSequence} that are
 * {@code null} safe.</p>
 *
 * @see CharSequence
 * @since 3.0
 */
public class CharSequenceUtils {

    private static final int NOT_FOUND = -1;

    /**
     * <p>{@code CharSequenceUtils} instances should NOT be constructed in
     * standard programming. </p>
     *
     * <p>This constructor is public to permit tools that require a JavaBean
     * instance to operate.</p>
     */
    public CharSequenceUtils() {
        super();
    }

    //-----------------------------------------------------------------------
    /**
     * <p>Returns a new {@code CharSequence} that is a subsequence of this
     * sequence starting with the {@code char} value at the specified index.</p>
     *
     * <p>This provides the {@code CharSequence} equivalent to {@link String#substring(int)}.
     * The length (in {@code char}) of the returned sequence is {@code length() - start},
     * so if {@code start == end} then an empty sequence is returned.</p>
     *
     * @param cs  the specified subsequence, null returns null
     * @param start  the start index, inclusive, valid
     * @return a new subsequence, may be null
     * @throws IndexOutOfBoundsException if {@code start} is negative or if 
     *  {@code start} is greater than {@code length()}
     */
    public static CharSequence subSequence(final CharSequence cs, final int start) {
        return cs == null ? null : cs.subSequence(start, cs.length());
    }

    //-----------------------------------------------------------------------
    /**
     * <p>Finds the first index in the {@code CharSequence} that matches the
     * specified character.</p>
     *
     * @param cs  the {@code CharSequence} to be processed, not null
     * @param searchChar  the char to be searched for
     * @param start  the start index, negative starts at the string start
     * @return the index where the search char was found, -1 if not found
     */
    static int indexOf(final CharSequence cs, final int searchChar, int start) {
        if (cs instanceof String) {
            return ((String) cs).indexOf(searchChar, start);
        }
        final int sz = cs.length();
        if (start < 0) {
            start = 0;
        }
        for (int i = start; i < sz; i++) {
            if (cs.charAt(i) == searchChar) {
                return i;
            }
        }
        return NOT_FOUND;
    }

    /**
     * Used by the indexOf(CharSequence methods) as a green implementation of indexOf.
     *
     * @param cs the {@code CharSequence} to be processed
     * @param searchChar the {@code CharSequence} to be searched for
     * @param start the start index
     * @return the index where the search sequence was found
     */
    static int indexOf(final CharSequence cs, final CharSequence searchChar, final int start) {
        return cs.toString().indexOf(searchChar.toString(), start);
//        if (cs instanceof String && searchChar instanceof String) {
//            // TODO: Do we assume searchChar is usually relatively small;
//            //       If so then calling toString() on it is better than reverting to
//            //       the green implementation in the else block
//            return ((String) cs).indexOf((String) searchChar, start);
//        } else {
//            // TODO: Implement rather than convert to String
//            return cs.toString().indexOf(searchChar.toString(), start);
//        }
    }

    /**
     * <p>Finds the last index in the {@code CharSequence} that matches the
     * specified character.</p>
     *
     * @param cs  the {@code CharSequence} to be processed
     * @param searchChar  the char to be searched for
     * @param start  the start index, negative returns -1, beyond length starts at end
     * @return the index where the search char was found, -1 if not found
     */
    static int lastIndexOf(final CharSequence cs, final int searchChar, int start) {
        if (cs instanceof String) {
            return ((String) cs).lastIndexOf(searchChar, start);
        }
        final int sz = cs.length();
        if (start < 0) {
            return NOT_FOUND;
        }
        if (start >= sz) {
            start = sz - 1;
        }
        for (int i = start; i >= 0; --i) {
            if (cs.charAt(i) == searchChar) {
                return i;
            }
        }
        return NOT_FOUND;
    }

    /**
     * Used by the lastIndexOf(CharSequence methods) as a green implementation of lastIndexOf
     *
     * @param cs the {@code CharSequence} to be processed
     * @param searchChar the {@code CharSequence} to be searched for
     * @param start the start index
     * @return the index where the search sequence was found
     */
    static int lastIndexOf(final CharSequence cs, final CharSequence searchChar, final int start) {
        return cs.toString().lastIndexOf(searchChar.toString(), start);
//        if (cs instanceof String && searchChar instanceof String) {
//            // TODO: Do we assume searchChar is usually relatively small;
//            //       If so then calling toString() on it is better than reverting to
//            //       the green implementation in the else block
//            return ((String) cs).lastIndexOf((String) searchChar, start);
//        } else {
//            // TODO: Implement rather than convert to String
//            return cs.toString().lastIndexOf(searchChar.toString(), start);
//        }
    }

    /**
     * Green implementation of toCharArray.
     *
     * @param cs the {@code CharSequence} to be processed
     * @return the resulting char array
     */
    static char[] toCharArray(final CharSequence cs) {
        if (cs instanceof String) {
            return ((String) cs).toCharArray();
        }
        final int sz = cs.length();
        final char[] array = new char[cs.length()];
        for (int i = 0; i < sz; i++) {
            array[i] = cs.charAt(i);
        }
        return array;
    }

    /**
     * Green implementation of regionMatches.
     *
     * @param cs the {@code CharSequence} to be processed
     * @param ignoreCase whether or not to be case insensitive
     * @param thisStart the index to start on the {@code cs} CharSequence
     * @param substring the {@code CharSequence} to be looked for
     * @param start the index to start on the {@code substring} CharSequence
     * @param length character length of the region
     * @return whether the region matched
     */
    static boolean regionMatches(final CharSequence cs, final boolean ignoreCase, final int thisStart,
            final CharSequence substring, final int start, final int length)    {
        if (cs instanceof String && substring instanceof String) {
            return ((String) cs).regionMatches(ignoreCase, thisStart, (String) substring, start, length);
        }
        int index1 = thisStart;
        int index2 = start;
        int tmpLen = length;

        // Extract these first so we detect NPEs the same as the java.lang.String version
        final int srcLen = cs.length() - thisStart;
        final int otherLen = substring.length() - start;

        // Check for invalid parameters
        if (thisStart < 0 || start < 0 || length < 0) {
            return false;
        }

        // Check that the regions are long enough
        if (srcLen < length || otherLen < length) {
            return false;
        }

        while (tmpLen-- > 0) {
            final char c1 = cs.charAt(index1++);
            final char c2 = substring.charAt(index2++);

            if (c1 == c2) {
                continue;
            }

            if (!ignoreCase) {
                return false;
            }

            // The same check as in String.regionMatches():
            if (Character.toUpperCase(c1) != Character.toUpperCase(c2)
                    && Character.toLowerCase(c1) != Character.toLowerCase(c2)) {
                return false;
            }
        }

        return true;
    }
}
