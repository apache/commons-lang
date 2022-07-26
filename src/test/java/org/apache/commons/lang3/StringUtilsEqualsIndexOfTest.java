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

import static org.apache.commons.lang3.Supplementary.CharU20000;
import static org.apache.commons.lang3.Supplementary.CharU20001;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.CharBuffer;
import java.util.Locale;

import org.hamcrest.core.IsNot;
import org.junit.jupiter.api.Test;

/**
 * Unit tests {@link org.apache.commons.lang3.StringUtils} - Equals/IndexOf methods
 */
public class StringUtilsEqualsIndexOfTest extends AbstractLangTest {

    private static final String BAR = "bar";

    private static final String FOO = "foo";

    private static final String FOOBAR = "foobar";

    private static final String[] FOOBAR_SUB_ARRAY = {"ob", "ba"};

    // The purpose of this class is to test StringUtils#equals(CharSequence, CharSequence)
    // with a CharSequence implementation whose equals(Object) override requires that the
    // other object be an instance of CustomCharSequence, even though, as char sequences,
    // `seq` may equal the other object.
    private static class CustomCharSequence implements CharSequence {
        private final CharSequence seq;

        CustomCharSequence(final CharSequence seq) {
            this.seq = seq;
        }

        @Override
        public char charAt(final int index) {
            return seq.charAt(index);
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
        public String toString() {
            return seq.toString();
        }
    }

    @Test
    public void testCustomCharSequence() {
        assertThat(new CustomCharSequence(FOO), IsNot.<CharSequence>not(FOO));
        assertThat(FOO, IsNot.<CharSequence>not(new CustomCharSequence(FOO)));
        assertEquals(new CustomCharSequence(FOO), new CustomCharSequence(FOO));
    }

    @Test
    public void testEquals() {
        final CharSequence fooCs = new StringBuilder(FOO), barCs = new StringBuilder(BAR), foobarCs = new StringBuilder(FOOBAR);
        assertTrue(StringUtils.equals(null, null));
        assertTrue(StringUtils.equals(fooCs, fooCs));
        assertTrue(StringUtils.equals(fooCs, new StringBuilder(FOO)));
        assertTrue(StringUtils.equals(fooCs, new String(new char[] { 'f', 'o', 'o' })));
        assertTrue(StringUtils.equals(fooCs, new CustomCharSequence(FOO)));
        assertTrue(StringUtils.equals(new CustomCharSequence(FOO), fooCs));
        assertFalse(StringUtils.equals(fooCs, new String(new char[] { 'f', 'O', 'O' })));
        assertFalse(StringUtils.equals(fooCs, barCs));
        assertFalse(StringUtils.equals(fooCs, null));
        assertFalse(StringUtils.equals(null, fooCs));
        assertFalse(StringUtils.equals(fooCs, foobarCs));
        assertFalse(StringUtils.equals(foobarCs, fooCs));
    }

    @Test
    public void testEqualsOnStrings() {
        assertTrue(StringUtils.equals(null, null));
        assertTrue(StringUtils.equals(FOO, FOO));
        assertTrue(StringUtils.equals(FOO, new String(new char[] { 'f', 'o', 'o' })));
        assertFalse(StringUtils.equals(FOO, new String(new char[] { 'f', 'O', 'O' })));
        assertFalse(StringUtils.equals(FOO, BAR));
        assertFalse(StringUtils.equals(FOO, null));
        assertFalse(StringUtils.equals(null, FOO));
        assertFalse(StringUtils.equals(FOO, FOOBAR));
        assertFalse(StringUtils.equals(FOOBAR, FOO));
    }

    @Test
    public void testEqualsIgnoreCase() {
        assertTrue(StringUtils.equalsIgnoreCase(null, null));
        assertTrue(StringUtils.equalsIgnoreCase(FOO, FOO));
        assertTrue(StringUtils.equalsIgnoreCase(FOO, new String(new char[] { 'f', 'o', 'o' })));
        assertTrue(StringUtils.equalsIgnoreCase(FOO, new String(new char[] { 'f', 'O', 'O' })));
        assertFalse(StringUtils.equalsIgnoreCase(FOO, BAR));
        assertFalse(StringUtils.equalsIgnoreCase(FOO, null));
        assertFalse(StringUtils.equalsIgnoreCase(null, FOO));
        assertTrue(StringUtils.equalsIgnoreCase("", ""));
        assertFalse(StringUtils.equalsIgnoreCase("abcd", "abcd "));
    }

    @Test
    public void testEqualsAny() {
        assertFalse(StringUtils.equalsAny(FOO));
        assertFalse(StringUtils.equalsAny(FOO, new String[]{}));

        assertTrue(StringUtils.equalsAny(FOO, FOO));
        assertTrue(StringUtils.equalsAny(FOO, BAR, new String(new char[] { 'f', 'o', 'o' })));
        assertFalse(StringUtils.equalsAny(FOO, BAR, new String(new char[] { 'f', 'O', 'O' })));
        assertFalse(StringUtils.equalsAny(FOO, BAR));
        assertFalse(StringUtils.equalsAny(FOO, BAR, null));
        assertFalse(StringUtils.equalsAny(null, FOO));
        assertFalse(StringUtils.equalsAny(FOO, FOOBAR));
        assertFalse(StringUtils.equalsAny(FOOBAR, FOO));

        assertTrue(StringUtils.equalsAny(null, null, null));
        assertFalse(StringUtils.equalsAny(null, FOO, BAR, FOOBAR));
        assertFalse(StringUtils.equalsAny(FOO, null, BAR));
        assertTrue(StringUtils.equalsAny(FOO, BAR, null, "", FOO, BAR));
        assertFalse(StringUtils.equalsAny(FOO, FOO.toUpperCase(Locale.ROOT)));

        assertFalse(StringUtils.equalsAny(null, (CharSequence[]) null));
        assertTrue(StringUtils.equalsAny(FOO, new CustomCharSequence("foo")));
        assertTrue(StringUtils.equalsAny(FOO, new StringBuilder("foo")));
        assertFalse(StringUtils.equalsAny(FOO, new CustomCharSequence("fOo")));
        assertFalse(StringUtils.equalsAny(FOO, new StringBuilder("fOo")));
    }

    @Test
    public void testEqualsAnyIgnoreCase() {
        assertFalse(StringUtils.equalsAnyIgnoreCase(FOO));
        assertFalse(StringUtils.equalsAnyIgnoreCase(FOO, new String[]{}));

        assertTrue(StringUtils.equalsAnyIgnoreCase(FOO, FOO));
        assertTrue(StringUtils.equalsAnyIgnoreCase(FOO, FOO.toUpperCase(Locale.ROOT)));
        assertTrue(StringUtils.equalsAnyIgnoreCase(FOO, FOO, new String(new char[]{'f', 'o', 'o'})));
        assertTrue(StringUtils.equalsAnyIgnoreCase(FOO, BAR, new String(new char[]{'f', 'O', 'O'})));
        assertFalse(StringUtils.equalsAnyIgnoreCase(FOO, BAR));
        assertFalse(StringUtils.equalsAnyIgnoreCase(FOO, BAR, null));
        assertFalse(StringUtils.equalsAnyIgnoreCase(null, FOO));
        assertFalse(StringUtils.equalsAnyIgnoreCase(FOO, FOOBAR));
        assertFalse(StringUtils.equalsAnyIgnoreCase(FOOBAR, FOO));

        assertTrue(StringUtils.equalsAnyIgnoreCase(null, null, null));
        assertFalse(StringUtils.equalsAnyIgnoreCase(null, FOO, BAR, FOOBAR));
        assertFalse(StringUtils.equalsAnyIgnoreCase(FOO, null, BAR));
        assertTrue(StringUtils.equalsAnyIgnoreCase(FOO, BAR, null, "", FOO.toUpperCase(Locale.ROOT), BAR));
        assertTrue(StringUtils.equalsAnyIgnoreCase(FOO, FOO.toUpperCase(Locale.ROOT)));

        assertFalse(StringUtils.equalsAnyIgnoreCase(null, (CharSequence[]) null));
        assertTrue(StringUtils.equalsAnyIgnoreCase(FOO, new CustomCharSequence("fOo")));
        assertTrue(StringUtils.equalsAnyIgnoreCase(FOO, new StringBuilder("fOo")));
    }

    @Test
    public void testCompare_StringString() {
        assertEquals(0, StringUtils.compare(null, null));
        assertTrue(StringUtils.compare(null, "a") < 0);
        assertTrue(StringUtils.compare("a", null) > 0);
        assertEquals(0, StringUtils.compare("abc", "abc"));
        assertTrue(StringUtils.compare("a", "b") < 0);
        assertTrue(StringUtils.compare("b", "a") > 0);
        assertTrue(StringUtils.compare("a", "B") > 0);
        assertTrue(StringUtils.compare("abc", "abd") < 0);
        assertTrue(StringUtils.compare("ab", "abc") < 0);
        assertTrue(StringUtils.compare("ab", "ab ") < 0);
        assertTrue(StringUtils.compare("abc", "ab ") > 0);
    }

    @Test
    public void testCompare_StringStringBoolean() {
        assertEquals(0, StringUtils.compare(null, null, false));
        assertTrue(StringUtils.compare(null, "a", true) < 0);
        assertTrue(StringUtils.compare(null, "a", false) > 0);
        assertTrue(StringUtils.compare("a", null, true) > 0);
        assertTrue(StringUtils.compare("a", null, false) < 0);
        assertEquals(0, StringUtils.compare("abc", "abc", false));
        assertTrue(StringUtils.compare("a", "b", false) < 0);
        assertTrue(StringUtils.compare("b", "a", false) > 0);
        assertTrue(StringUtils.compare("a", "B", false) > 0);
        assertTrue(StringUtils.compare("abc", "abd", false) < 0);
        assertTrue(StringUtils.compare("ab", "abc", false) < 0);
        assertTrue(StringUtils.compare("ab", "ab ", false) < 0);
        assertTrue(StringUtils.compare("abc", "ab ", false) > 0);
    }

    @Test
    public void testCompareIgnoreCase_StringString() {
        assertEquals(0, StringUtils.compareIgnoreCase(null, null));
        assertTrue(StringUtils.compareIgnoreCase(null, "a") < 0);
        assertTrue(StringUtils.compareIgnoreCase("a", null) > 0);
        assertEquals(0, StringUtils.compareIgnoreCase("abc", "abc"));
        assertEquals(0, StringUtils.compareIgnoreCase("abc", "ABC"));
        assertTrue(StringUtils.compareIgnoreCase("a", "b") < 0);
        assertTrue(StringUtils.compareIgnoreCase("b", "a") > 0);
        assertTrue(StringUtils.compareIgnoreCase("a", "B") < 0);
        assertTrue(StringUtils.compareIgnoreCase("A", "b") < 0);
        assertTrue(StringUtils.compareIgnoreCase("abc", "ABD") < 0);
        assertTrue(StringUtils.compareIgnoreCase("ab", "ABC") < 0);
        assertTrue(StringUtils.compareIgnoreCase("ab", "AB ") < 0);
        assertTrue(StringUtils.compareIgnoreCase("abc", "AB ") > 0);
    }

    @Test
    public void testCompareIgnoreCase_StringStringBoolean() {
        assertEquals(0, StringUtils.compareIgnoreCase(null, null, false));
        assertTrue(StringUtils.compareIgnoreCase(null, "a", true) < 0);
        assertTrue(StringUtils.compareIgnoreCase(null, "a", false) > 0);
        assertTrue(StringUtils.compareIgnoreCase("a", null, true) > 0);
        assertTrue(StringUtils.compareIgnoreCase("a", null, false) < 0);
        assertEquals(0, StringUtils.compareIgnoreCase("abc", "abc", false));
        assertEquals(0, StringUtils.compareIgnoreCase("abc", "ABC", false));
        assertTrue(StringUtils.compareIgnoreCase("a", "b", false) < 0);
        assertTrue(StringUtils.compareIgnoreCase("b", "a", false) > 0);
        assertTrue(StringUtils.compareIgnoreCase("a", "B", false) < 0);
        assertTrue(StringUtils.compareIgnoreCase("A", "b", false) < 0);
        assertTrue(StringUtils.compareIgnoreCase("abc", "ABD", false) < 0);
        assertTrue(StringUtils.compareIgnoreCase("ab", "ABC", false) < 0);
        assertTrue(StringUtils.compareIgnoreCase("ab", "AB ", false) < 0);
        assertTrue(StringUtils.compareIgnoreCase("abc", "AB ", false) > 0);
    }

    @Test
    public void testIndexOf_char() {
        assertEquals(-1, StringUtils.indexOf(null, ' '));
        assertEquals(-1, StringUtils.indexOf("", ' '));
        assertEquals(0, StringUtils.indexOf("aabaabaa", 'a'));
        assertEquals(2, StringUtils.indexOf("aabaabaa", 'b'));

        assertEquals(2, StringUtils.indexOf(new StringBuilder("aabaabaa"), 'b'));
        assertEquals(StringUtils.INDEX_NOT_FOUND, StringUtils.indexOf(new StringBuilder("aabaabaa"), -1738));
    }

    @Test
    public void testIndexOf_charInt() {
        assertEquals(-1, StringUtils.indexOf(null, ' ', 0));
        assertEquals(-1, StringUtils.indexOf(null, ' ', -1));
        assertEquals(-1, StringUtils.indexOf("", ' ', 0));
        assertEquals(-1, StringUtils.indexOf("", ' ', -1));
        assertEquals(0, StringUtils.indexOf("aabaabaa", 'a', 0));
        assertEquals(2, StringUtils.indexOf("aabaabaa", 'b', 0));
        assertEquals(5, StringUtils.indexOf("aabaabaa", 'b', 3));
        assertEquals(-1, StringUtils.indexOf("aabaabaa", 'b', 9));
        assertEquals(2, StringUtils.indexOf("aabaabaa", 'b', -1));

        assertEquals(5, StringUtils.indexOf(new StringBuilder("aabaabaa"), 'b', 3));

        //LANG-1300 tests go here
        final int CODE_POINT = 0x2070E;
        StringBuilder builder = new StringBuilder();
        builder.appendCodePoint(CODE_POINT);
        assertEquals(0, StringUtils.indexOf(builder, CODE_POINT, 0));
        assertEquals(0, StringUtils.indexOf(builder.toString(), CODE_POINT, 0));
        builder.appendCodePoint(CODE_POINT);
        assertEquals(2, StringUtils.indexOf(builder, CODE_POINT, 1));
        assertEquals(2, StringUtils.indexOf(builder.toString(), CODE_POINT, 1));
        // inner branch on the supplementary character block
        final char[] tmp = { (char) 55361 };
        builder = new StringBuilder();
        builder.append(tmp);
        assertEquals(-1, StringUtils.indexOf(builder, CODE_POINT, 0));
        assertEquals(-1, StringUtils.indexOf(builder.toString(), CODE_POINT, 0));
        builder.appendCodePoint(CODE_POINT);
        assertEquals(1, StringUtils.indexOf(builder, CODE_POINT, 0));
        assertEquals(1, StringUtils.indexOf(builder.toString(), CODE_POINT, 0));
        assertEquals(-1, StringUtils.indexOf(builder, CODE_POINT, 2));
        assertEquals(-1, StringUtils.indexOf(builder.toString(), CODE_POINT, 2));
    }

    @Test
    public void testIndexOf_String() {
        assertEquals(-1, StringUtils.indexOf(null, null));
        assertEquals(-1, StringUtils.indexOf("", null));
        assertEquals(0, StringUtils.indexOf("", ""));
        assertEquals(0, StringUtils.indexOf("aabaabaa", "a"));
        assertEquals(2, StringUtils.indexOf("aabaabaa", "b"));
        assertEquals(1, StringUtils.indexOf("aabaabaa", "ab"));
        assertEquals(0, StringUtils.indexOf("aabaabaa", ""));

        assertEquals(2, StringUtils.indexOf(new StringBuilder("aabaabaa"), "b"));
    }

    @Test
    public void testIndexOf_StringInt() {
        assertEquals(-1, StringUtils.indexOf(null, null, 0));
        assertEquals(-1, StringUtils.indexOf(null, null, -1));
        assertEquals(-1, StringUtils.indexOf(null, "", 0));
        assertEquals(-1, StringUtils.indexOf(null, "", -1));
        assertEquals(-1, StringUtils.indexOf("", null, 0));
        assertEquals(-1, StringUtils.indexOf("", null, -1));
        assertEquals(0, StringUtils.indexOf("", "", 0));
        assertEquals(0, StringUtils.indexOf("", "", -1));
        assertEquals(0, StringUtils.indexOf("", "", 9));
        assertEquals(0, StringUtils.indexOf("abc", "", 0));
        assertEquals(0, StringUtils.indexOf("abc", "", -1));
        assertEquals(3, StringUtils.indexOf("abc", "", 9));
        assertEquals(3, StringUtils.indexOf("abc", "", 3));
        assertEquals(0, StringUtils.indexOf("aabaabaa", "a", 0));
        assertEquals(2, StringUtils.indexOf("aabaabaa", "b", 0));
        assertEquals(1, StringUtils.indexOf("aabaabaa", "ab", 0));
        assertEquals(5, StringUtils.indexOf("aabaabaa", "b", 3));
        assertEquals(-1, StringUtils.indexOf("aabaabaa", "b", 9));
        assertEquals(2, StringUtils.indexOf("aabaabaa", "b", -1));
        assertEquals(2, StringUtils.indexOf("aabaabaa", "", 2));

        // Test that startIndex works correctly, i.e. cannot match before startIndex
        assertEquals(7, StringUtils.indexOf("12345678", "8", 5));
        assertEquals(7, StringUtils.indexOf("12345678", "8", 6));
        assertEquals(7, StringUtils.indexOf("12345678", "8", 7)); // 7 is last index
        assertEquals(-1, StringUtils.indexOf("12345678", "8", 8));

        assertEquals(5, StringUtils.indexOf(new StringBuilder("aabaabaa"), "b", 3));
    }

    @Test
    public void testIndexOfAny_StringCharArray() {
        assertEquals(-1, StringUtils.indexOfAny(null, (char[]) null));
        assertEquals(-1, StringUtils.indexOfAny(null, new char[0]));
        assertEquals(-1, StringUtils.indexOfAny(null, 'a', 'b'));

        assertEquals(-1, StringUtils.indexOfAny("", (char[]) null));
        assertEquals(-1, StringUtils.indexOfAny("", new char[0]));
        assertEquals(-1, StringUtils.indexOfAny("", 'a', 'b'));

        assertEquals(-1, StringUtils.indexOfAny("zzabyycdxx", (char[]) null));
        assertEquals(-1, StringUtils.indexOfAny("zzabyycdxx", new char[0]));
        assertEquals(0, StringUtils.indexOfAny("zzabyycdxx", 'z', 'a'));
        assertEquals(3, StringUtils.indexOfAny("zzabyycdxx", 'b', 'y'));
        assertEquals(-1, StringUtils.indexOfAny("ab", 'z'));
    }

    /**
     * See https://www.oracle.com/technical-resources/articles/javase/supplementary.html
     */
    @Test
    public void testIndexOfAny_StringCharArrayWithSupplementaryChars() {
        assertEquals(0, StringUtils.indexOfAny(CharU20000 + CharU20001, CharU20000.toCharArray()));
        assertEquals(2, StringUtils.indexOfAny(CharU20000 + CharU20001, CharU20001.toCharArray()));
        assertEquals(0, StringUtils.indexOfAny(CharU20000, CharU20000.toCharArray()));
        assertEquals(-1, StringUtils.indexOfAny(CharU20000, CharU20001.toCharArray()));
    }

    @Test
    public void testIndexOfAny_StringString() {
        assertEquals(-1, StringUtils.indexOfAny(null, (String) null));
        assertEquals(-1, StringUtils.indexOfAny(null, ""));
        assertEquals(-1, StringUtils.indexOfAny(null, "ab"));

        assertEquals(-1, StringUtils.indexOfAny("", (String) null));
        assertEquals(-1, StringUtils.indexOfAny("", ""));
        assertEquals(-1, StringUtils.indexOfAny("", "ab"));

        assertEquals(-1, StringUtils.indexOfAny("zzabyycdxx", (String) null));
        assertEquals(-1, StringUtils.indexOfAny("zzabyycdxx", ""));
        assertEquals(0, StringUtils.indexOfAny("zzabyycdxx", "za"));
        assertEquals(3, StringUtils.indexOfAny("zzabyycdxx", "by"));
        assertEquals(-1, StringUtils.indexOfAny("ab", "z"));
    }

    @Test
    public void testIndexOfAny_StringStringArray() {
        assertEquals(-1, StringUtils.indexOfAny(null, (String[]) null));
        assertEquals(-1, StringUtils.indexOfAny(null, FOOBAR_SUB_ARRAY));
        assertEquals(-1, StringUtils.indexOfAny(FOOBAR, (String[]) null));
        assertEquals(2, StringUtils.indexOfAny(FOOBAR, FOOBAR_SUB_ARRAY));
        assertEquals(-1, StringUtils.indexOfAny(FOOBAR, new String[0]));
        assertEquals(-1, StringUtils.indexOfAny(null, new String[0]));
        assertEquals(-1, StringUtils.indexOfAny("", new String[0]));
        assertEquals(-1, StringUtils.indexOfAny(FOOBAR, new String[] {"llll"}));
        assertEquals(0, StringUtils.indexOfAny(FOOBAR, new String[] {""}));
        assertEquals(0, StringUtils.indexOfAny("", new String[] {""}));
        assertEquals(-1, StringUtils.indexOfAny("", new String[] {"a"}));
        assertEquals(-1, StringUtils.indexOfAny("", new String[] {null}));
        assertEquals(-1, StringUtils.indexOfAny(FOOBAR, new String[] {null}));
        assertEquals(-1, StringUtils.indexOfAny(null, new String[] {null}));
    }

    /**
     * See https://www.oracle.com/technical-resources/articles/javase/supplementary.html
     */
    @Test
    public void testIndexOfAny_StringStringWithSupplementaryChars() {
        assertEquals(0, StringUtils.indexOfAny(CharU20000 + CharU20001, CharU20000));
        assertEquals(2, StringUtils.indexOfAny(CharU20000 + CharU20001, CharU20001));
        assertEquals(0, StringUtils.indexOfAny(CharU20000, CharU20000));
        assertEquals(-1, StringUtils.indexOfAny(CharU20000, CharU20001));
    }

    @Test
    public void testIndexOfAnyBut_StringCharArray() {
        assertEquals(-1, StringUtils.indexOfAnyBut(null, (char[]) null));
        assertEquals(-1, StringUtils.indexOfAnyBut(null));
        assertEquals(-1, StringUtils.indexOfAnyBut(null, 'a', 'b'));

        assertEquals(-1, StringUtils.indexOfAnyBut("", (char[]) null));
        assertEquals(-1, StringUtils.indexOfAnyBut(""));
        assertEquals(-1, StringUtils.indexOfAnyBut("", 'a', 'b'));

        assertEquals(-1, StringUtils.indexOfAnyBut("zzabyycdxx", (char[]) null));
        assertEquals(-1, StringUtils.indexOfAnyBut("zzabyycdxx"));
        assertEquals(3, StringUtils.indexOfAnyBut("zzabyycdxx", 'z', 'a'));
        assertEquals(0, StringUtils.indexOfAnyBut("zzabyycdxx", 'b', 'y'));
        assertEquals(-1, StringUtils.indexOfAnyBut("aba", 'a', 'b'));
        assertEquals(0, StringUtils.indexOfAnyBut("aba", 'z'));
    }

    @Test
    public void testIndexOfAnyBut_StringCharArrayWithSupplementaryChars() {
        assertEquals(2, StringUtils.indexOfAnyBut(CharU20000 + CharU20001, CharU20000.toCharArray()));
        assertEquals(0, StringUtils.indexOfAnyBut(CharU20000 + CharU20001, CharU20001.toCharArray()));
        assertEquals(-1, StringUtils.indexOfAnyBut(CharU20000, CharU20000.toCharArray()));
        assertEquals(0, StringUtils.indexOfAnyBut(CharU20000, CharU20001.toCharArray()));
    }

    @Test
    public void testIndexOfAnyBut_StringString() {
        assertEquals(-1, StringUtils.indexOfAnyBut(null, (String) null));
        assertEquals(-1, StringUtils.indexOfAnyBut(null, ""));
        assertEquals(-1, StringUtils.indexOfAnyBut(null, "ab"));

        assertEquals(-1, StringUtils.indexOfAnyBut("", (String) null));
        assertEquals(-1, StringUtils.indexOfAnyBut("", ""));
        assertEquals(-1, StringUtils.indexOfAnyBut("", "ab"));

        assertEquals(-1, StringUtils.indexOfAnyBut("zzabyycdxx", (String) null));
        assertEquals(-1, StringUtils.indexOfAnyBut("zzabyycdxx", ""));
        assertEquals(3, StringUtils.indexOfAnyBut("zzabyycdxx", "za"));
        assertEquals(0, StringUtils.indexOfAnyBut("zzabyycdxx", "by"));
        assertEquals(0, StringUtils.indexOfAnyBut("ab", "z"));
    }

    @Test
    public void testIndexOfAnyBut_StringStringWithSupplementaryChars() {
        assertEquals(2, StringUtils.indexOfAnyBut(CharU20000 + CharU20001, CharU20000));
        assertEquals(0, StringUtils.indexOfAnyBut(CharU20000 + CharU20001, CharU20001));
        assertEquals(-1, StringUtils.indexOfAnyBut(CharU20000, CharU20000));
        assertEquals(0, StringUtils.indexOfAnyBut(CharU20000, CharU20001));
    }

    @Test
    public void testIndexOfIgnoreCase_String() {
        assertEquals(-1, StringUtils.indexOfIgnoreCase(null, null));
        assertEquals(-1, StringUtils.indexOfIgnoreCase(null, ""));
        assertEquals(-1, StringUtils.indexOfIgnoreCase("", null));
        assertEquals(0, StringUtils.indexOfIgnoreCase("", ""));
        assertEquals(0, StringUtils.indexOfIgnoreCase("aabaabaa", "a"));
        assertEquals(0, StringUtils.indexOfIgnoreCase("aabaabaa", "A"));
        assertEquals(2, StringUtils.indexOfIgnoreCase("aabaabaa", "b"));
        assertEquals(2, StringUtils.indexOfIgnoreCase("aabaabaa", "B"));
        assertEquals(1, StringUtils.indexOfIgnoreCase("aabaabaa", "ab"));
        assertEquals(1, StringUtils.indexOfIgnoreCase("aabaabaa", "AB"));
        assertEquals(0, StringUtils.indexOfIgnoreCase("aabaabaa", ""));
    }

    @Test
    public void testIndexOfIgnoreCase_StringInt() {
        assertEquals(1, StringUtils.indexOfIgnoreCase("aabaabaa", "AB", -1));
        assertEquals(1, StringUtils.indexOfIgnoreCase("aabaabaa", "AB", 0));
        assertEquals(1, StringUtils.indexOfIgnoreCase("aabaabaa", "AB", 1));
        assertEquals(4, StringUtils.indexOfIgnoreCase("aabaabaa", "AB", 2));
        assertEquals(4, StringUtils.indexOfIgnoreCase("aabaabaa", "AB", 3));
        assertEquals(4, StringUtils.indexOfIgnoreCase("aabaabaa", "AB", 4));
        assertEquals(-1, StringUtils.indexOfIgnoreCase("aabaabaa", "AB", 5));
        assertEquals(-1, StringUtils.indexOfIgnoreCase("aabaabaa", "AB", 6));
        assertEquals(-1, StringUtils.indexOfIgnoreCase("aabaabaa", "AB", 7));
        assertEquals(-1, StringUtils.indexOfIgnoreCase("aabaabaa", "AB", 8));
        assertEquals(1, StringUtils.indexOfIgnoreCase("aab", "AB", 1));
        assertEquals(5, StringUtils.indexOfIgnoreCase("aabaabaa", "", 5));
        assertEquals(-1, StringUtils.indexOfIgnoreCase("ab", "AAB", 0));
        assertEquals(-1, StringUtils.indexOfIgnoreCase("aab", "AAB", 1));
        assertEquals(-1, StringUtils.indexOfIgnoreCase("abc", "", 9));
    }

    @Test
    public void testLastIndexOf_char() {
        assertEquals(-1, StringUtils.lastIndexOf(null, ' '));
        assertEquals(-1, StringUtils.lastIndexOf("", ' '));
        assertEquals(7, StringUtils.lastIndexOf("aabaabaa", 'a'));
        assertEquals(5, StringUtils.lastIndexOf("aabaabaa", 'b'));

        assertEquals(5, StringUtils.lastIndexOf(new StringBuilder("aabaabaa"), 'b'));
    }

    @Test
    public void testLastIndexOf_charInt() {
        assertEquals(-1, StringUtils.lastIndexOf(null, ' ', 0));
        assertEquals(-1, StringUtils.lastIndexOf(null, ' ', -1));
        assertEquals(-1, StringUtils.lastIndexOf("", ' ', 0));
        assertEquals(-1, StringUtils.lastIndexOf("", ' ', -1));
        assertEquals(7, StringUtils.lastIndexOf("aabaabaa", 'a', 8));
        assertEquals(5, StringUtils.lastIndexOf("aabaabaa", 'b', 8));
        assertEquals(2, StringUtils.lastIndexOf("aabaabaa", 'b', 3));
        assertEquals(5, StringUtils.lastIndexOf("aabaabaa", 'b', 9));
        assertEquals(-1, StringUtils.lastIndexOf("aabaabaa", 'b', -1));
        assertEquals(0, StringUtils.lastIndexOf("aabaabaa", 'a', 0));

        assertEquals(2, StringUtils.lastIndexOf(new StringBuilder("aabaabaa"), 'b', 2));

        //LANG-1300 addition test
        final int CODE_POINT = 0x2070E;
        StringBuilder builder = new StringBuilder();
        builder.appendCodePoint(CODE_POINT);
        assertEquals(0, StringUtils.lastIndexOf(builder, CODE_POINT, 0));
        builder.appendCodePoint(CODE_POINT);
        assertEquals(0, StringUtils.lastIndexOf(builder, CODE_POINT, 0));
        assertEquals(0, StringUtils.lastIndexOf(builder, CODE_POINT, 1));
        assertEquals(2, StringUtils.lastIndexOf(builder, CODE_POINT, 2));

        builder.append("aaaaa");
        assertEquals(2, StringUtils.lastIndexOf(builder, CODE_POINT, 4));
        // inner branch on the supplementary character block
        final char[] tmp = { (char) 55361 };
        builder = new StringBuilder();
        builder.append(tmp);
        assertEquals(-1, StringUtils.lastIndexOf(builder, CODE_POINT, 0));
        builder.appendCodePoint(CODE_POINT);
        assertEquals(-1, StringUtils.lastIndexOf(builder, CODE_POINT, 0));
        assertEquals(1, StringUtils.lastIndexOf(builder, CODE_POINT, 1 ));
        assertEquals(-1, StringUtils.lastIndexOf(builder.toString(), CODE_POINT, 0));
        assertEquals(1, StringUtils.lastIndexOf(builder.toString(), CODE_POINT, 1));
        assertEquals(StringUtils.INDEX_NOT_FOUND, StringUtils.lastIndexOf(CharBuffer.wrap("[%{.c.0rro"), -1738, 982));
    }

    @Test
    public void testLastIndexOf_String() {
        assertEquals(-1, StringUtils.lastIndexOf(null, null));
        assertEquals(-1, StringUtils.lastIndexOf("", null));
        assertEquals(-1, StringUtils.lastIndexOf("", "a"));
        assertEquals(0, StringUtils.lastIndexOf("", ""));
        assertEquals(8, StringUtils.lastIndexOf("aabaabaa", ""));
        assertEquals(7, StringUtils.lastIndexOf("aabaabaa", "a"));
        assertEquals(5, StringUtils.lastIndexOf("aabaabaa", "b"));
        assertEquals(4, StringUtils.lastIndexOf("aabaabaa", "ab"));

        assertEquals(4, StringUtils.lastIndexOf(new StringBuilder("aabaabaa"), "ab"));
    }

    @Test
    public void testLastIndexOf_StringInt() {
        assertEquals(-1, StringUtils.lastIndexOf(null, null, 0));
        assertEquals(-1, StringUtils.lastIndexOf(null, null, -1));
        assertEquals(-1, StringUtils.lastIndexOf(null, "", 0));
        assertEquals(-1, StringUtils.lastIndexOf(null, "", -1));
        assertEquals(-1, StringUtils.lastIndexOf("", null, 0));
        assertEquals(-1, StringUtils.lastIndexOf("", null, -1));
        assertEquals(0, StringUtils.lastIndexOf("", "", 0));
        assertEquals(-1, StringUtils.lastIndexOf("", "", -1));
        assertEquals(0, StringUtils.lastIndexOf("", "", 9));
        assertEquals(0, StringUtils.lastIndexOf("abc", "", 0));
        assertEquals(-1, StringUtils.lastIndexOf("abc", "", -1));
        assertEquals(3, StringUtils.lastIndexOf("abc", "", 9));
        assertEquals(7, StringUtils.lastIndexOf("aabaabaa", "a", 8));
        assertEquals(5, StringUtils.lastIndexOf("aabaabaa", "b", 8));
        assertEquals(4, StringUtils.lastIndexOf("aabaabaa", "ab", 8));
        assertEquals(2, StringUtils.lastIndexOf("aabaabaa", "b", 3));
        assertEquals(5, StringUtils.lastIndexOf("aabaabaa", "b", 9));
        assertEquals(-1, StringUtils.lastIndexOf("aabaabaa", "b", -1));
        assertEquals(-1, StringUtils.lastIndexOf("aabaabaa", "b", 0));
        assertEquals(0, StringUtils.lastIndexOf("aabaabaa", "a", 0));
        assertEquals(-1, StringUtils.lastIndexOf("aabaabaa", "a", -1));

        // Test that fromIndex works correctly, i.e. cannot match after fromIndex
        assertEquals(7, StringUtils.lastIndexOf("12345678", "8", 9));
        assertEquals(7, StringUtils.lastIndexOf("12345678", "8", 8));
        assertEquals(7, StringUtils.lastIndexOf("12345678", "8", 7)); // 7 is last index
        assertEquals(-1, StringUtils.lastIndexOf("12345678", "8", 6));

        assertEquals(-1, StringUtils.lastIndexOf("aabaabaa", "b", 1));
        assertEquals(2, StringUtils.lastIndexOf("aabaabaa", "b", 2));
        assertEquals(2, StringUtils.lastIndexOf("aabaabaa", "ba", 2));
        assertEquals(2, StringUtils.lastIndexOf("aabaabaa", "ba", 3));

        assertEquals(2, StringUtils.lastIndexOf(new StringBuilder("aabaabaa"), "b", 3));
    }

    @Test
    public void testLastIndexOfAny_StringStringArray() {
        assertEquals(-1, StringUtils.lastIndexOfAny(null, (CharSequence) null));   // test both types of ...
        assertEquals(-1, StringUtils.lastIndexOfAny(null, (CharSequence[]) null)); // ... varargs invocation
        assertEquals(-1, StringUtils.lastIndexOfAny(null)); // Missing varag
        assertEquals(-1, StringUtils.lastIndexOfAny(null, FOOBAR_SUB_ARRAY));
        assertEquals(-1, StringUtils.lastIndexOfAny(FOOBAR, (CharSequence) null));   // test both types of ...
        assertEquals(-1, StringUtils.lastIndexOfAny(FOOBAR, (CharSequence[]) null)); // ... varargs invocation
        assertEquals(-1, StringUtils.lastIndexOfAny(FOOBAR)); // Missing vararg
        assertEquals(3, StringUtils.lastIndexOfAny(FOOBAR, FOOBAR_SUB_ARRAY));
        assertEquals(-1, StringUtils.lastIndexOfAny(FOOBAR, new String[0]));
        assertEquals(-1, StringUtils.lastIndexOfAny(null, new String[0]));
        assertEquals(-1, StringUtils.lastIndexOfAny("", new String[0]));
        assertEquals(-1, StringUtils.lastIndexOfAny(FOOBAR, new String[] {"llll"}));
        assertEquals(6, StringUtils.lastIndexOfAny(FOOBAR, new String[] {""}));
        assertEquals(0, StringUtils.lastIndexOfAny("", new String[] {""}));
        assertEquals(-1, StringUtils.lastIndexOfAny("", new String[] {"a"}));
        assertEquals(-1, StringUtils.lastIndexOfAny("", new String[] {null}));
        assertEquals(-1, StringUtils.lastIndexOfAny(FOOBAR, new String[] {null}));
        assertEquals(-1, StringUtils.lastIndexOfAny(null, new String[] {null}));
    }

    @Test
    public void testLastIndexOfIgnoreCase_String() {
        assertEquals(-1, StringUtils.lastIndexOfIgnoreCase(null, null));
        assertEquals(-1, StringUtils.lastIndexOfIgnoreCase("", null));
        assertEquals(-1, StringUtils.lastIndexOfIgnoreCase(null, ""));
        assertEquals(-1, StringUtils.lastIndexOfIgnoreCase("", "a"));
        assertEquals(0, StringUtils.lastIndexOfIgnoreCase("", ""));
        assertEquals(8, StringUtils.lastIndexOfIgnoreCase("aabaabaa", ""));
        assertEquals(7, StringUtils.lastIndexOfIgnoreCase("aabaabaa", "a"));
        assertEquals(7, StringUtils.lastIndexOfIgnoreCase("aabaabaa", "A"));
        assertEquals(5, StringUtils.lastIndexOfIgnoreCase("aabaabaa", "b"));
        assertEquals(5, StringUtils.lastIndexOfIgnoreCase("aabaabaa", "B"));
        assertEquals(4, StringUtils.lastIndexOfIgnoreCase("aabaabaa", "ab"));
        assertEquals(4, StringUtils.lastIndexOfIgnoreCase("aabaabaa", "AB"));
        assertEquals(-1, StringUtils.lastIndexOfIgnoreCase("ab", "AAB"));
        assertEquals(0, StringUtils.lastIndexOfIgnoreCase("aab", "AAB"));
    }

    @Test
    public void testLastIndexOfIgnoreCase_StringInt() {
        assertEquals(-1, StringUtils.lastIndexOfIgnoreCase(null, null, 0));
        assertEquals(-1, StringUtils.lastIndexOfIgnoreCase(null, null, -1));
        assertEquals(-1, StringUtils.lastIndexOfIgnoreCase(null, "", 0));
        assertEquals(-1, StringUtils.lastIndexOfIgnoreCase(null, "", -1));
        assertEquals(-1, StringUtils.lastIndexOfIgnoreCase("", null, 0));
        assertEquals(-1, StringUtils.lastIndexOfIgnoreCase("", null, -1));
        assertEquals(0, StringUtils.lastIndexOfIgnoreCase("", "", 0));
        assertEquals(-1, StringUtils.lastIndexOfIgnoreCase("", "", -1));
        assertEquals(0, StringUtils.lastIndexOfIgnoreCase("", "", 9));
        assertEquals(0, StringUtils.lastIndexOfIgnoreCase("abc", "", 0));
        assertEquals(-1, StringUtils.lastIndexOfIgnoreCase("abc", "", -1));
        assertEquals(3, StringUtils.lastIndexOfIgnoreCase("abc", "", 9));
        assertEquals(7, StringUtils.lastIndexOfIgnoreCase("aabaabaa", "A", 8));
        assertEquals(5, StringUtils.lastIndexOfIgnoreCase("aabaabaa", "B", 8));
        assertEquals(4, StringUtils.lastIndexOfIgnoreCase("aabaabaa", "AB", 8));
        assertEquals(2, StringUtils.lastIndexOfIgnoreCase("aabaabaa", "B", 3));
        assertEquals(5, StringUtils.lastIndexOfIgnoreCase("aabaabaa", "B", 9));
        assertEquals(-1, StringUtils.lastIndexOfIgnoreCase("aabaabaa", "B", -1));
        assertEquals(-1, StringUtils.lastIndexOfIgnoreCase("aabaabaa", "B", 0));
        assertEquals(0, StringUtils.lastIndexOfIgnoreCase("aabaabaa", "A", 0));
        assertEquals(1, StringUtils.lastIndexOfIgnoreCase("aab", "AB", 1));
    }

    @Test
    public void testLastOrdinalIndexOf() {
        assertEquals(-1, StringUtils.lastOrdinalIndexOf(null, "*", 42) );
        assertEquals(-1, StringUtils.lastOrdinalIndexOf("*", null, 42) );
        assertEquals(0, StringUtils.lastOrdinalIndexOf("", "", 42) );
        assertEquals(7, StringUtils.lastOrdinalIndexOf("aabaabaa", "a", 1) );
        assertEquals(6, StringUtils.lastOrdinalIndexOf("aabaabaa", "a", 2) );
        assertEquals(5, StringUtils.lastOrdinalIndexOf("aabaabaa", "b", 1) );
        assertEquals(2, StringUtils.lastOrdinalIndexOf("aabaabaa", "b", 2) );
        assertEquals(4, StringUtils.lastOrdinalIndexOf("aabaabaa", "ab", 1) );
        assertEquals(1, StringUtils.lastOrdinalIndexOf("aabaabaa", "ab", 2) );
        assertEquals(8, StringUtils.lastOrdinalIndexOf("aabaabaa", "", 1) );
        assertEquals(8, StringUtils.lastOrdinalIndexOf("aabaabaa", "", 2) );
    }

    @Test
    public void testOrdinalIndexOf() {
        assertEquals(-1, StringUtils.ordinalIndexOf(null, null, Integer.MIN_VALUE));
        assertEquals(-1, StringUtils.ordinalIndexOf("", null, Integer.MIN_VALUE));
        assertEquals(-1, StringUtils.ordinalIndexOf("", "", Integer.MIN_VALUE));
        assertEquals(-1, StringUtils.ordinalIndexOf("aabaabaa", "a", Integer.MIN_VALUE));
        assertEquals(-1, StringUtils.ordinalIndexOf("aabaabaa", "b", Integer.MIN_VALUE));
        assertEquals(-1, StringUtils.ordinalIndexOf("aabaabaa", "ab", Integer.MIN_VALUE));
        assertEquals(-1, StringUtils.ordinalIndexOf("aabaabaa", "", Integer.MIN_VALUE));

        assertEquals(-1, StringUtils.ordinalIndexOf(null, null, -1));
        assertEquals(-1, StringUtils.ordinalIndexOf("", null, -1));
        assertEquals(-1, StringUtils.ordinalIndexOf("", "", -1));
        assertEquals(-1, StringUtils.ordinalIndexOf("aabaabaa", "a", -1));
        assertEquals(-1, StringUtils.ordinalIndexOf("aabaabaa", "b", -1));
        assertEquals(-1, StringUtils.ordinalIndexOf("aabaabaa", "ab", -1));
        assertEquals(-1, StringUtils.ordinalIndexOf("aabaabaa", "", -1));

        assertEquals(-1, StringUtils.ordinalIndexOf(null, null, 0));
        assertEquals(-1, StringUtils.ordinalIndexOf("", null, 0));
        assertEquals(-1, StringUtils.ordinalIndexOf("", "", 0));
        assertEquals(-1, StringUtils.ordinalIndexOf("aabaabaa", "a", 0));
        assertEquals(-1, StringUtils.ordinalIndexOf("aabaabaa", "b", 0));
        assertEquals(-1, StringUtils.ordinalIndexOf("aabaabaa", "ab", 0));
        assertEquals(-1, StringUtils.ordinalIndexOf("aabaabaa", "", 0));

        assertEquals(-1, StringUtils.ordinalIndexOf(null, null, 1));
        assertEquals(-1, StringUtils.ordinalIndexOf("", null, 1));
        assertEquals(0, StringUtils.ordinalIndexOf("", "", 1));
        assertEquals(0, StringUtils.ordinalIndexOf("aabaabaa", "a", 1));
        assertEquals(2, StringUtils.ordinalIndexOf("aabaabaa", "b", 1));
        assertEquals(1, StringUtils.ordinalIndexOf("aabaabaa", "ab", 1));
        assertEquals(0, StringUtils.ordinalIndexOf("aabaabaa", "", 1));

        assertEquals(-1, StringUtils.ordinalIndexOf(null, null, 2));
        assertEquals(-1, StringUtils.ordinalIndexOf("", null, 2));
        assertEquals(0, StringUtils.ordinalIndexOf("", "", 2));
        assertEquals(1, StringUtils.ordinalIndexOf("aabaabaa", "a", 2));
        assertEquals(5, StringUtils.ordinalIndexOf("aabaabaa", "b", 2));
        assertEquals(4, StringUtils.ordinalIndexOf("aabaabaa", "ab", 2));
        assertEquals(0, StringUtils.ordinalIndexOf("aabaabaa", "", 2));

        assertEquals(-1, StringUtils.ordinalIndexOf(null, null, Integer.MAX_VALUE));
        assertEquals(-1, StringUtils.ordinalIndexOf("", null, Integer.MAX_VALUE));
        assertEquals(0, StringUtils.ordinalIndexOf("", "", Integer.MAX_VALUE));
        assertEquals(-1, StringUtils.ordinalIndexOf("aabaabaa", "a", Integer.MAX_VALUE));
        assertEquals(-1, StringUtils.ordinalIndexOf("aabaabaa", "b", Integer.MAX_VALUE));
        assertEquals(-1, StringUtils.ordinalIndexOf("aabaabaa", "ab", Integer.MAX_VALUE));
        assertEquals(0, StringUtils.ordinalIndexOf("aabaabaa", "", Integer.MAX_VALUE));

        assertEquals(-1, StringUtils.ordinalIndexOf("aaaaaaaaa", "a", 0));
        assertEquals(0, StringUtils.ordinalIndexOf("aaaaaaaaa", "a", 1));
        assertEquals(1, StringUtils.ordinalIndexOf("aaaaaaaaa", "a", 2));
        assertEquals(2, StringUtils.ordinalIndexOf("aaaaaaaaa", "a", 3));
        assertEquals(3, StringUtils.ordinalIndexOf("aaaaaaaaa", "a", 4));
        assertEquals(4, StringUtils.ordinalIndexOf("aaaaaaaaa", "a", 5));
        assertEquals(5, StringUtils.ordinalIndexOf("aaaaaaaaa", "a", 6));
        assertEquals(6, StringUtils.ordinalIndexOf("aaaaaaaaa", "a", 7));
        assertEquals(7, StringUtils.ordinalIndexOf("aaaaaaaaa", "a", 8));
        assertEquals(8, StringUtils.ordinalIndexOf("aaaaaaaaa", "a", 9));
        assertEquals(-1, StringUtils.ordinalIndexOf("aaaaaaaaa", "a", 10));

        // match at each possible position
        assertEquals(0, StringUtils.ordinalIndexOf("aaaaaa", "aa", 1));
        assertEquals(1, StringUtils.ordinalIndexOf("aaaaaa", "aa", 2));
        assertEquals(2, StringUtils.ordinalIndexOf("aaaaaa", "aa", 3));
        assertEquals(3, StringUtils.ordinalIndexOf("aaaaaa", "aa", 4));
        assertEquals(4, StringUtils.ordinalIndexOf("aaaaaa", "aa", 5));
        assertEquals(-1, StringUtils.ordinalIndexOf("aaaaaa", "aa", 6));

        assertEquals(0, StringUtils.ordinalIndexOf("ababab", "aba", 1));
        assertEquals(2, StringUtils.ordinalIndexOf("ababab", "aba", 2));
        assertEquals(-1, StringUtils.ordinalIndexOf("ababab", "aba", 3));

        assertEquals(0, StringUtils.ordinalIndexOf("abababab", "abab", 1));
        assertEquals(2, StringUtils.ordinalIndexOf("abababab", "abab", 2));
        assertEquals(4, StringUtils.ordinalIndexOf("abababab", "abab", 3));
        assertEquals(-1, StringUtils.ordinalIndexOf("abababab", "abab", 4));
    }

    @Test
    public void testLANG1193() {
        assertEquals(0, StringUtils.ordinalIndexOf("abc", "ab", 1));
    }

    @Test
    // Non-overlapping test
    public void testLANG1241_1() {
        //                                          0  3  6
        assertEquals(0, StringUtils.ordinalIndexOf("abaabaab", "ab", 1));
        assertEquals(3, StringUtils.ordinalIndexOf("abaabaab", "ab", 2));
        assertEquals(6, StringUtils.ordinalIndexOf("abaabaab", "ab", 3));
    }

    @Test
    // Overlapping matching test
    public void testLANG1241_2() {
        //                                          0 2 4
        assertEquals(0, StringUtils.ordinalIndexOf("abababa", "aba", 1));
        assertEquals(2, StringUtils.ordinalIndexOf("abababa", "aba", 2));
        assertEquals(4, StringUtils.ordinalIndexOf("abababa", "aba", 3));
        assertEquals(0, StringUtils.ordinalIndexOf("abababab", "abab", 1));
        assertEquals(2, StringUtils.ordinalIndexOf("abababab", "abab", 2));
        assertEquals(4, StringUtils.ordinalIndexOf("abababab", "abab", 3));
    }
}
