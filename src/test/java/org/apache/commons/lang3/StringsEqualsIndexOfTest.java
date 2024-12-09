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

import org.junit.jupiter.api.Test;

import java.util.Locale;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests {@link Strings} - Equals/IndexOf methods
 */
public class StringsEqualsIndexOfTest extends AbstractLangTest {

    // The purpose of this class is to test StringUtils#equals(CharSequence, CharSequence)
    // with a CharSequence implementation whose equals(Object) override requires that the
    // other object be an instance of CustomCharSequence, even though, as char sequences,
    // `seq` may equal the other object.
    private static final class CustomCharSequence implements CharSequence {
        private final CharSequence seq;

        CustomCharSequence(final CharSequence seq) {
            this.seq = seq;
        }

        @Override
        public char charAt(final int index) {
            return seq.charAt(index);
        }

        @Override
        public boolean equals(final Object obj) {
            if (!(obj instanceof CustomCharSequence)) {
                return false;
            }
            final CustomCharSequence other = (CustomCharSequence) obj;
            return seq.equals(other.seq);
        }

        @Override
        public int hashCode() {
            return seq.hashCode();
        }

        @Override
        public int length() {
            return seq.length();
        }

        @Override
        public CharSequence subSequence(final int start, final int end) {
            return new CustomCharSequence(seq.subSequence(start, end));
        }

        @Override
        public String toString() {
            return seq.toString();
        }
    }

    private static final String BAR = "bar";

    private static final String FOO = "foo";

    private static final String FOOBAR = "foobar";

    private static final String[] FOOBAR_SUB_ARRAY = {"ob", "ba"};

    @Test
    public void testCompare_StringString() {
        assertEquals(0, Strings.CS.compare(null, null));
        assertTrue(Strings.CS.compare(null, "a") < 0);
        assertTrue(Strings.CS.compare("a", null) > 0);
        assertEquals(0, Strings.CS.compare("abc", "abc"));
        assertTrue(Strings.CS.compare("a", "b") < 0);
        assertTrue(Strings.CS.compare("b", "a") > 0);
        assertTrue(Strings.CS.compare("a", "B") > 0);
        assertTrue(Strings.CS.compare("abc", "abd") < 0);
        assertTrue(Strings.CS.compare("ab", "abc") < 0);
        assertTrue(Strings.CS.compare("ab", "ab ") < 0);
        assertTrue(Strings.CS.compare("abc", "ab ") > 0);
    }


    @Test
    public void testCompareIgnoreCase_StringString() {
        assertEquals(0, Strings.CI.compare(null, null));
        assertTrue(Strings.CI.compare(null, "a") < 0);
        assertTrue(Strings.CI.compare("a", null) > 0);
        assertEquals(0, Strings.CI.compare("abc", "abc"));
        assertEquals(0, Strings.CI.compare("abc", "ABC"));
        assertTrue(Strings.CI.compare("a", "b") < 0);
        assertTrue(Strings.CI.compare("b", "a") > 0);
        assertTrue(Strings.CI.compare("a", "B") < 0);
        assertTrue(Strings.CI.compare("A", "b") < 0);
        assertTrue(Strings.CI.compare("abc", "ABD") < 0);
        assertTrue(Strings.CI.compare("ab", "ABC") < 0);
        assertTrue(Strings.CI.compare("ab", "AB ") < 0);
        assertTrue(Strings.CI.compare("abc", "AB ") > 0);
    }


    @Test
    public void testCustomCharSequence() {
        assertEquals(new CustomCharSequence(FOO), new CustomCharSequence(FOO));
    }

    @Test
    public void testEqualsNull() {
        final CharSequence fooCs = new StringBuilder(FOO), barCs = new StringBuilder(BAR);
        assertTrue(StringUtils.equals(null, null));
        assertFalse(StringUtils.equals(null, fooCs));
        assertFalse(StringUtils.equals(fooCs, null));

        assertTrue(Strings.CS.equals(null, null));
        assertFalse(Strings.CS.equals(null, barCs));
        assertFalse(Strings.CS.equals(fooCs, null));

    }

    @Test
    public void testEquals() {
        final CharSequence fooCs = new StringBuilder(FOO), barCs = new StringBuilder(BAR), foobarCs = new StringBuilder(FOOBAR);
        assertTrue(Strings.CS.equals(fooCs, fooCs));
        assertTrue(Strings.CS.equals(fooCs, new StringBuilder(FOO)));
        assertTrue(Strings.CS.equals(fooCs, new String(new char[] { 'f', 'o', 'o' })));
        assertTrue(Strings.CS.equals(fooCs, new CustomCharSequence(FOO)));
        assertTrue(Strings.CS.equals(new CustomCharSequence(FOO), fooCs));
        assertFalse(Strings.CS.equals(fooCs, new String(new char[] { 'f', 'O', 'O' })));
        assertFalse(Strings.CS.equals(fooCs, barCs));
        assertFalse(Strings.CS.equals(fooCs, null));
        assertFalse(Strings.CS.equals(null, fooCs));
        assertFalse(Strings.CS.equals(fooCs, foobarCs));
        assertFalse(Strings.CS.equals(foobarCs, fooCs));
    }

    @Test
    public void testEqualsAny() {
        assertFalse(Strings.CS.equalsAny(FOO));
        assertFalse(Strings.CS.equalsAny(FOO, new String[]{}));

        assertTrue(Strings.CS.equalsAny(FOO, FOO));
        assertTrue(Strings.CS.equalsAny(FOO, BAR, new String(new char[] { 'f', 'o', 'o' })));
        assertFalse(Strings.CS.equalsAny(FOO, BAR, new String(new char[] { 'f', 'O', 'O' })));
        assertFalse(Strings.CS.equalsAny(FOO, BAR));
        assertFalse(Strings.CS.equalsAny(FOO, BAR, null));
        assertFalse(Strings.CS.equalsAny(null, FOO));
        assertFalse(Strings.CS.equalsAny(FOO, FOOBAR));
        assertFalse(Strings.CS.equalsAny(FOOBAR, FOO));

        assertTrue(Strings.CS.equalsAny(null, null, null));
        assertFalse(Strings.CS.equalsAny(null, FOO, BAR, FOOBAR));
        assertFalse(Strings.CS.equalsAny(FOO, null, BAR));
        assertTrue(Strings.CS.equalsAny(FOO, BAR, null, "", FOO, BAR));
        assertFalse(Strings.CS.equalsAny(FOO, FOO.toUpperCase(Locale.ROOT)));

        assertFalse(Strings.CS.equalsAny(null, (CharSequence[]) null));
        assertTrue(Strings.CS.equalsAny(FOO, new CustomCharSequence("foo")));
        assertTrue(Strings.CS.equalsAny(FOO, new StringBuilder("foo")));
        assertFalse(Strings.CS.equalsAny(FOO, new CustomCharSequence("fOo")));
        assertFalse(Strings.CS.equalsAny(FOO, new StringBuilder("fOo")));
    }

    @Test
    public void testEqualsAnyIgnoreCase() {
        assertFalse(Strings.CI.equalsAny(FOO));
        assertFalse(Strings.CI.equalsAny(FOO, new String[]{}));

        assertTrue(Strings.CI.equalsAny(FOO, FOO));
        assertTrue(Strings.CI.equalsAny(FOO, FOO.toUpperCase(Locale.ROOT)));
        assertTrue(Strings.CI.equalsAny(FOO, FOO, new String(new char[]{'f', 'o', 'o'})));
        assertTrue(Strings.CI.equalsAny(FOO, BAR, new String(new char[]{'f', 'O', 'O'})));
        assertFalse(Strings.CI.equalsAny(FOO, BAR));
        assertFalse(Strings.CI.equalsAny(FOO, BAR, null));
        assertFalse(Strings.CI.equalsAny(null, FOO));
        assertFalse(Strings.CI.equalsAny(FOO, FOOBAR));
        assertFalse(Strings.CI.equalsAny(FOOBAR, FOO));

        assertTrue(Strings.CI.equalsAny(null, null, null));
        assertFalse(Strings.CI.equalsAny(null, FOO, BAR, FOOBAR));
        assertFalse(Strings.CI.equalsAny(FOO, null, BAR));
        assertTrue(Strings.CI.equalsAny(FOO, BAR, null, "", FOO.toUpperCase(Locale.ROOT), BAR));
        assertTrue(Strings.CI.equalsAny(FOO, FOO.toUpperCase(Locale.ROOT)));

        assertFalse(Strings.CI.equalsAny(null, (CharSequence[]) null));
        assertTrue(Strings.CI.equalsAny(FOO, new CustomCharSequence("fOo")));
        assertTrue(Strings.CI.equalsAny(FOO, new StringBuilder("fOo")));
    }

    @Test
    public void testEqualsIgnoreCase() {
        assertTrue(StringUtils.equals(null, null));
        assertTrue(Strings.CI.equals(FOO, FOO));
        assertTrue(Strings.CI.equals(FOO, new String(new char[] { 'f', 'o', 'o' })));
        assertTrue(Strings.CI.equals(FOO, new String(new char[] { 'f', 'O', 'O' })));
        assertFalse(Strings.CI.equals(FOO, BAR));
        assertFalse(Strings.CI.equals(FOO, null));
        assertFalse(StringUtils.equals(null, FOO));
        assertTrue(Strings.CI.equals("", ""));
        assertFalse(Strings.CI.equals("abcd", "abcd "));
    }

    @Test
    public void testEqualsOnStrings() {
        assertTrue(StringUtils.equals(null, null));
        assertTrue(Strings.CS.equals(FOO, FOO));
        assertTrue(Strings.CS.equals(FOO, new String(new char[] { 'f', 'o', 'o' })));
        assertFalse(Strings.CS.equals(FOO, new String(new char[] { 'f', 'O', 'O' })));
        assertFalse(Strings.CS.equals(FOO, BAR));
        assertFalse(Strings.CS.equals(FOO, null));
        assertFalse(StringUtils.equals(null, FOO));
        assertFalse(Strings.CS.equals(FOO, FOOBAR));
        assertFalse(Strings.CS.equals(FOOBAR, FOO));
    }


    @Test
    public void testIndexOf_String() {
        assertEquals(-1, Strings.CS.indexOf(null, null));
        assertEquals(-1, Strings.CS.indexOf("", null));
        assertEquals(0, Strings.CS.indexOf("", ""));
        assertEquals(0, Strings.CS.indexOf("aabaabaa", "a"));
        assertEquals(2, Strings.CS.indexOf("aabaabaa", "b"));
        assertEquals(1, Strings.CS.indexOf("aabaabaa", "ab"));
        assertEquals(0, Strings.CS.indexOf("aabaabaa", ""));

        assertEquals(2, Strings.CS.indexOf(new StringBuilder("aabaabaa"), "b"));
    }

    @Test
    public void testIndexOf_StringInt() {
        assertEquals(-1, Strings.CS.indexOf(null, null, 0));
        assertEquals(-1, Strings.CS.indexOf(null, null, -1));
        assertEquals(-1, Strings.CS.indexOf(null, "", 0));
        assertEquals(-1, Strings.CS.indexOf(null, "", -1));
        assertEquals(-1, Strings.CS.indexOf("", null, 0));
        assertEquals(-1, Strings.CS.indexOf("", null, -1));
        assertEquals(0, Strings.CS.indexOf("", "", 0));
        assertEquals(0, Strings.CS.indexOf("", "", -1));
        assertEquals(0, Strings.CS.indexOf("", "", 9));
        assertEquals(0, Strings.CS.indexOf("abc", "", 0));
        assertEquals(0, Strings.CS.indexOf("abc", "", -1));
        assertEquals(3, Strings.CS.indexOf("abc", "", 9));
        assertEquals(3, Strings.CS.indexOf("abc", "", 3));
        assertEquals(0, Strings.CS.indexOf("aabaabaa", "a", 0));
        assertEquals(2, Strings.CS.indexOf("aabaabaa", "b", 0));
        assertEquals(1, Strings.CS.indexOf("aabaabaa", "ab", 0));
        assertEquals(5, Strings.CS.indexOf("aabaabaa", "b", 3));
        assertEquals(-1, Strings.CS.indexOf("aabaabaa", "b", 9));
        assertEquals(2, Strings.CS.indexOf("aabaabaa", "b", -1));
        assertEquals(2, Strings.CS.indexOf("aabaabaa", "", 2));

        // Test that startIndex works correctly, i.e. cannot match before startIndex
        assertEquals(7, Strings.CS.indexOf("12345678", "8", 5));
        assertEquals(7, Strings.CS.indexOf("12345678", "8", 6));
        assertEquals(7, Strings.CS.indexOf("12345678", "8", 7)); // 7 is last index
        assertEquals(-1, Strings.CS.indexOf("12345678", "8", 8));

        assertEquals(5, Strings.CS.indexOf(new StringBuilder("aabaabaa"), "b", 3));
    }

    @Test
    public void testIndexOfIgnoreCase_String() {
        assertEquals(-1, Strings.CI.indexOf(null, null));
        assertEquals(-1, Strings.CI.indexOf(null, ""));
        assertEquals(-1, Strings.CI.indexOf("", null));
        assertEquals(0, Strings.CI.indexOf("", ""));
        assertEquals(0, Strings.CI.indexOf("aabaabaa", "a"));
        assertEquals(0, Strings.CI.indexOf("aabaabaa", "A"));
        assertEquals(2, Strings.CI.indexOf("aabaabaa", "b"));
        assertEquals(2, Strings.CI.indexOf("aabaabaa", "B"));
        assertEquals(1, Strings.CI.indexOf("aabaabaa", "ab"));
        assertEquals(1, Strings.CI.indexOf("aabaabaa", "AB"));
        assertEquals(0, Strings.CI.indexOf("aabaabaa", ""));
    }

    @Test
    public void testIndexOfIgnoreCase_StringInt() {
        assertEquals(1, Strings.CI.indexOf("aabaabaa", "AB", -1));
        assertEquals(1, Strings.CI.indexOf("aabaabaa", "AB", 0));
        assertEquals(1, Strings.CI.indexOf("aabaabaa", "AB", 1));
        assertEquals(4, Strings.CI.indexOf("aabaabaa", "AB", 2));
        assertEquals(4, Strings.CI.indexOf("aabaabaa", "AB", 3));
        assertEquals(4, Strings.CI.indexOf("aabaabaa", "AB", 4));
        assertEquals(-1, Strings.CI.indexOf("aabaabaa", "AB", 5));
        assertEquals(-1, Strings.CI.indexOf("aabaabaa", "AB", 6));
        assertEquals(-1, Strings.CI.indexOf("aabaabaa", "AB", 7));
        assertEquals(-1, Strings.CI.indexOf("aabaabaa", "AB", 8));
        assertEquals(1, Strings.CI.indexOf("aab", "AB", 1));
        assertEquals(5, Strings.CI.indexOf("aabaabaa", "", 5));
        assertEquals(-1, Strings.CI.indexOf("ab", "AAB", 0));
        assertEquals(-1, Strings.CI.indexOf("aab", "AAB", 1));
        assertEquals(-1, Strings.CI.indexOf("abc", "", 9));
    }


    @Test
    public void testLastIndexOf_String() {
        assertEquals(-1, Strings.CS.lastIndexOf(null, null));
        assertEquals(-1, Strings.CS.lastIndexOf("", null));
        assertEquals(-1, Strings.CS.lastIndexOf("", "a"));
        assertEquals(0, Strings.CS.lastIndexOf("", ""));
        assertEquals(8, Strings.CS.lastIndexOf("aabaabaa", ""));
        assertEquals(7, Strings.CS.lastIndexOf("aabaabaa", "a"));
        assertEquals(5, Strings.CS.lastIndexOf("aabaabaa", "b"));
        assertEquals(4, Strings.CS.lastIndexOf("aabaabaa", "ab"));

        assertEquals(4, Strings.CS.lastIndexOf(new StringBuilder("aabaabaa"), "ab"));
    }

    @Test
    public void testLastIndexOf_StringInt() {
        assertEquals(-1, Strings.CS.lastIndexOf(null, null, 0));
        assertEquals(-1, Strings.CS.lastIndexOf(null, null, -1));
        assertEquals(-1, Strings.CS.lastIndexOf(null, "", 0));
        assertEquals(-1, Strings.CS.lastIndexOf(null, "", -1));
        assertEquals(-1, Strings.CS.lastIndexOf("", null, 0));
        assertEquals(-1, Strings.CS.lastIndexOf("", null, -1));
        assertEquals(0, Strings.CS.lastIndexOf("", "", 0));
        assertEquals(-1, Strings.CS.lastIndexOf("", "", -1));
        assertEquals(0, Strings.CS.lastIndexOf("", "", 9));
        assertEquals(0, Strings.CS.lastIndexOf("abc", "", 0));
        assertEquals(-1, Strings.CS.lastIndexOf("abc", "", -1));
        assertEquals(3, Strings.CS.lastIndexOf("abc", "", 9));
        assertEquals(7, Strings.CS.lastIndexOf("aabaabaa", "a", 8));
        assertEquals(5, Strings.CS.lastIndexOf("aabaabaa", "b", 8));
        assertEquals(4, Strings.CS.lastIndexOf("aabaabaa", "ab", 8));
        assertEquals(2, Strings.CS.lastIndexOf("aabaabaa", "b", 3));
        assertEquals(5, Strings.CS.lastIndexOf("aabaabaa", "b", 9));
        assertEquals(-1, Strings.CS.lastIndexOf("aabaabaa", "b", -1));
        assertEquals(-1, Strings.CS.lastIndexOf("aabaabaa", "b", 0));
        assertEquals(0, Strings.CS.lastIndexOf("aabaabaa", "a", 0));
        assertEquals(-1, Strings.CS.lastIndexOf("aabaabaa", "a", -1));

        // Test that fromIndex works correctly, i.e. cannot match after fromIndex
        assertEquals(7, Strings.CS.lastIndexOf("12345678", "8", 9));
        assertEquals(7, Strings.CS.lastIndexOf("12345678", "8", 8));
        assertEquals(7, Strings.CS.lastIndexOf("12345678", "8", 7)); // 7 is last index
        assertEquals(-1, Strings.CS.lastIndexOf("12345678", "8", 6));

        assertEquals(-1, Strings.CS.lastIndexOf("aabaabaa", "b", 1));
        assertEquals(2, Strings.CS.lastIndexOf("aabaabaa", "b", 2));
        assertEquals(2, Strings.CS.lastIndexOf("aabaabaa", "ba", 2));
        assertEquals(2, Strings.CS.lastIndexOf("aabaabaa", "ba", 3));

        assertEquals(2, Strings.CS.lastIndexOf(new StringBuilder("aabaabaa"), "b", 3));
    }

    @Test
    public void testLastIndexOfIgnoreCase_String() {
        assertEquals(-1, Strings.CI.lastIndexOf(null, null));
        assertEquals(-1, Strings.CI.lastIndexOf("", null));
        assertEquals(-1, Strings.CI.lastIndexOf(null, ""));
        assertEquals(-1, Strings.CI.lastIndexOf("", "a"));
        assertEquals(0, Strings.CI.lastIndexOf("", ""));
        assertEquals(8, Strings.CI.lastIndexOf("aabaabaa", ""));
        assertEquals(7, Strings.CI.lastIndexOf("aabaabaa", "a"));
        assertEquals(7, Strings.CI.lastIndexOf("aabaabaa", "A"));
        assertEquals(5, Strings.CI.lastIndexOf("aabaabaa", "b"));
        assertEquals(5, Strings.CI.lastIndexOf("aabaabaa", "B"));
        assertEquals(4, Strings.CI.lastIndexOf("aabaabaa", "ab"));
        assertEquals(4, Strings.CI.lastIndexOf("aabaabaa", "AB"));
        assertEquals(-1, Strings.CI.lastIndexOf("ab", "AAB"));
        assertEquals(0, Strings.CI.lastIndexOf("aab", "AAB"));
    }

    @Test
    public void testLastIndexOfIgnoreCase_StringInt() {
        assertEquals(-1, Strings.CI.lastIndexOf(null, null, 0));
        assertEquals(-1, Strings.CI.lastIndexOf(null, null, -1));
        assertEquals(-1, Strings.CI.lastIndexOf(null, "", 0));
        assertEquals(-1, Strings.CI.lastIndexOf(null, "", -1));
        assertEquals(-1, Strings.CI.lastIndexOf("", null, 0));
        assertEquals(-1, Strings.CI.lastIndexOf("", null, -1));
        assertEquals(0, Strings.CI.lastIndexOf("", "", 0));
        assertEquals(-1, Strings.CI.lastIndexOf("", "", -1));
        assertEquals(0, Strings.CI.lastIndexOf("", "", 9));
        assertEquals(0, Strings.CI.lastIndexOf("abc", "", 0));
        assertEquals(-1, Strings.CI.lastIndexOf("abc", "", -1));
        assertEquals(3, Strings.CI.lastIndexOf("abc", "", 9));
        assertEquals(7, Strings.CI.lastIndexOf("aabaabaa", "A", 8));
        assertEquals(5, Strings.CI.lastIndexOf("aabaabaa", "B", 8));
        assertEquals(4, Strings.CI.lastIndexOf("aabaabaa", "AB", 8));
        assertEquals(2, Strings.CI.lastIndexOf("aabaabaa", "B", 3));
        assertEquals(5, Strings.CI.lastIndexOf("aabaabaa", "B", 9));
        assertEquals(-1, Strings.CI.lastIndexOf("aabaabaa", "B", -1));
        assertEquals(-1, Strings.CI.lastIndexOf("aabaabaa", "B", 0));
        assertEquals(0, Strings.CI.lastIndexOf("aabaabaa", "A", 0));
        assertEquals(1, Strings.CI.lastIndexOf("aab", "AB", 1));
    }

}
