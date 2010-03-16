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

import java.util.Locale;

import junit.framework.TestCase;

/**
 * Unit tests {@link org.apache.commons.lang3.StringUtils} - Substring methods
 *
 * @author Apache Software Foundation
 * @author <a href="mailto:ridesmet@users.sourceforge.net">Ringo De Smet</a>
 * @author Phil Steitz
 * @version $Id$
 */
public class StringUtilsEqualsIndexOfTest extends TestCase {
    private static final String BAR = "bar";
    /**
     * Supplementary character U+20000
     * See http://java.sun.com/developer/technicalArticles/Intl/Supplementary/
     */
    private static final String CharU20000 = "\uD840\uDC00";
    /**
     * Supplementary character U+20001
     * See http://java.sun.com/developer/technicalArticles/Intl/Supplementary/
     */
    private static final String CharU20001 = "\uD840\uDC01";
    /**
     * Incomplete supplementary character U+20000, high surrogate only.
     * See http://java.sun.com/developer/technicalArticles/Intl/Supplementary/
     */
    private static final String CharUSuppCharHigh = "\uDC00";

    /**
     * Incomplete supplementary character U+20000, low surrogate only.
     * See http://java.sun.com/developer/technicalArticles/Intl/Supplementary/
     */
    private static final String CharUSuppCharLow = "\uD840";

    private static final String FOO = "foo";

    private static final String FOOBAR = "foobar";

    private static final String[] FOOBAR_SUB_ARRAY = new String[] {"ob", "ba"};

    public StringUtilsEqualsIndexOfTest(String name) {
        super(name);
    }

    public void testContains_Char() {
        assertEquals(false, StringUtils.contains(null, ' '));
        assertEquals(false, StringUtils.contains("", ' '));
        assertEquals(false, StringUtils.contains("", null));
        assertEquals(false, StringUtils.contains(null, null));
        assertEquals(true, StringUtils.contains("abc", 'a'));
        assertEquals(true, StringUtils.contains("abc", 'b'));
        assertEquals(true, StringUtils.contains("abc", 'c'));
        assertEquals(false, StringUtils.contains("abc", 'z'));
    }

    public void testContains_String() {
        assertEquals(false, StringUtils.contains(null, null));
        assertEquals(false, StringUtils.contains(null, ""));
        assertEquals(false, StringUtils.contains(null, "a"));
        assertEquals(false, StringUtils.contains("", null));
        assertEquals(true, StringUtils.contains("", ""));
        assertEquals(false, StringUtils.contains("", "a"));
        assertEquals(true, StringUtils.contains("abc", "a"));
        assertEquals(true, StringUtils.contains("abc", "b"));
        assertEquals(true, StringUtils.contains("abc", "c"));
        assertEquals(true, StringUtils.contains("abc", "abc"));
        assertEquals(false, StringUtils.contains("abc", "z"));
    }

    /**
     * See http://java.sun.com/developer/technicalArticles/Intl/Supplementary/
     */
    public void testContains_StringWithBadSupplementaryChars() {
        // Test edge case: 1/2 of a (broken) supplementary char
        assertEquals(false, StringUtils.contains(CharUSuppCharHigh, CharU20001));
        assertEquals(false, StringUtils.contains(CharUSuppCharLow, CharU20001));
        assertEquals(false, StringUtils.contains(CharU20001, CharUSuppCharHigh));
        assertEquals(0, CharU20001.indexOf(CharUSuppCharLow));
        assertEquals(true, StringUtils.contains(CharU20001, CharUSuppCharLow));
        assertEquals(true, StringUtils.contains(CharU20001 + CharUSuppCharLow + "a", "a"));
        assertEquals(true, StringUtils.contains(CharU20001 + CharUSuppCharHigh + "a", "a"));
    }

    /**
     * See http://java.sun.com/developer/technicalArticles/Intl/Supplementary/
     */
    public void testContains_StringWithSupplementaryChars() {
        assertEquals(true, StringUtils.contains(CharU20000 + CharU20001, CharU20000));
        assertEquals(true, StringUtils.contains(CharU20000 + CharU20001, CharU20001));
        assertEquals(true, StringUtils.contains(CharU20000, CharU20000));
        assertEquals(false, StringUtils.contains(CharU20000, CharU20001));
    }

    public void testContainsAny_StringCharArray() {
        assertFalse(StringUtils.containsAny(null, (char[]) null));
        assertFalse(StringUtils.containsAny(null, new char[0]));
        assertFalse(StringUtils.containsAny(null, new char[] { 'a', 'b' }));

        assertFalse(StringUtils.containsAny("", (char[]) null));
        assertFalse(StringUtils.containsAny("", new char[0]));
        assertFalse(StringUtils.containsAny("", new char[] { 'a', 'b' }));

        assertFalse(StringUtils.containsAny("zzabyycdxx", (char[]) null));
        assertFalse(StringUtils.containsAny("zzabyycdxx", new char[0]));
        assertTrue(StringUtils.containsAny("zzabyycdxx", new char[] { 'z', 'a' }));
        assertTrue(StringUtils.containsAny("zzabyycdxx", new char[] { 'b', 'y' }));
        assertFalse(StringUtils.containsAny("ab", new char[] { 'z' }));
    }

    /**
     * See http://java.sun.com/developer/technicalArticles/Intl/Supplementary/
     */
    public void testContainsAny_StringCharArrayWithBadSupplementaryChars() {
        // Test edge case: 1/2 of a (broken) supplementary char
        assertEquals(false, StringUtils.containsAny(CharUSuppCharHigh, CharU20001.toCharArray()));
        assertEquals(false, StringUtils.containsAny("abc" + CharUSuppCharHigh + "xyz", CharU20001.toCharArray()));
        assertEquals(-1, CharUSuppCharLow.indexOf(CharU20001));
        assertEquals(false, StringUtils.containsAny(CharUSuppCharLow, CharU20001.toCharArray()));
        assertEquals(false, StringUtils.containsAny(CharU20001, CharUSuppCharHigh.toCharArray()));
        assertEquals(0, CharU20001.indexOf(CharUSuppCharLow));
        assertEquals(true, StringUtils.containsAny(CharU20001, CharUSuppCharLow.toCharArray()));
    }

    /**
     * See http://java.sun.com/developer/technicalArticles/Intl/Supplementary/
     */
    public void testContainsAny_StringCharArrayWithSupplementaryChars() {
        assertEquals(true, StringUtils.containsAny(CharU20000 + CharU20001, CharU20000.toCharArray()));
        assertEquals(true, StringUtils.containsAny("a" + CharU20000 + CharU20001, "a".toCharArray()));
        assertEquals(true, StringUtils.containsAny(CharU20000 + "a" + CharU20001, "a".toCharArray()));
        assertEquals(true, StringUtils.containsAny(CharU20000 + CharU20001 + "a", "a".toCharArray()));
        assertEquals(true, StringUtils.containsAny(CharU20000 + CharU20001, CharU20001.toCharArray()));
        assertEquals(true, StringUtils.containsAny(CharU20000, CharU20000.toCharArray()));
        // Sanity check:
        assertEquals(-1, CharU20000.indexOf(CharU20001));
        assertEquals(0, CharU20000.indexOf(CharU20001.charAt(0)));
        assertEquals(-1, CharU20000.indexOf(CharU20001.charAt(1)));
        // Test:
        assertEquals(false, StringUtils.containsAny(CharU20000, CharU20001.toCharArray()));
        assertEquals(false, StringUtils.containsAny(CharU20001, CharU20000.toCharArray()));
    }

    public void testContainsAny_StringString() {
        assertFalse(StringUtils.containsAny(null, (String) null));
        assertFalse(StringUtils.containsAny(null, ""));
        assertFalse(StringUtils.containsAny(null, "ab"));

        assertFalse(StringUtils.containsAny("", (String) null));
        assertFalse(StringUtils.containsAny("", ""));
        assertFalse(StringUtils.containsAny("", "ab"));

        assertFalse(StringUtils.containsAny("zzabyycdxx", (String) null));
        assertFalse(StringUtils.containsAny("zzabyycdxx", ""));
        assertTrue(StringUtils.containsAny("zzabyycdxx", "za"));
        assertTrue(StringUtils.containsAny("zzabyycdxx", "by"));
        assertFalse(StringUtils.containsAny("ab", "z"));
    }

    /**
     * See http://java.sun.com/developer/technicalArticles/Intl/Supplementary/
     */
    public void testContainsAny_StringWithBadSupplementaryChars() {
        // Test edge case: 1/2 of a (broken) supplementary char
        assertEquals(false, StringUtils.containsAny(CharUSuppCharHigh, CharU20001));
        assertEquals(-1, CharUSuppCharLow.indexOf(CharU20001));
        assertEquals(false, StringUtils.containsAny(CharUSuppCharLow, CharU20001));
        assertEquals(false, StringUtils.containsAny(CharU20001, CharUSuppCharHigh));
        assertEquals(0, CharU20001.indexOf(CharUSuppCharLow));
        assertEquals(true, StringUtils.containsAny(CharU20001, CharUSuppCharLow));
    }

    /**
     * See http://java.sun.com/developer/technicalArticles/Intl/Supplementary/
     */
    public void testContainsAny_StringWithSupplementaryChars() {
        assertEquals(true, StringUtils.containsAny(CharU20000 + CharU20001, CharU20000));
        assertEquals(true, StringUtils.containsAny(CharU20000 + CharU20001, CharU20001));
        assertEquals(true, StringUtils.containsAny(CharU20000, CharU20000));
        // Sanity check:
        assertEquals(-1, CharU20000.indexOf(CharU20001));
        assertEquals(0, CharU20000.indexOf(CharU20001.charAt(0)));
        assertEquals(-1, CharU20000.indexOf(CharU20001.charAt(1)));
        // Test:
        assertEquals(false, StringUtils.containsAny(CharU20000, CharU20001));
        assertEquals(false, StringUtils.containsAny(CharU20001, CharU20000));
    }

    public void testContainsIgnoreCase_LocaleIndependence() {
        Locale orig = Locale.getDefault();

        Locale[] locales = { Locale.ENGLISH, new Locale("tr"), Locale.getDefault() };

        String[][] tdata = {
            { "i", "I" },
            { "I", "i" },
            { "\u03C2", "\u03C3" },
            { "\u03A3", "\u03C2" },
            { "\u03A3", "\u03C3" },
        };

        String[][] fdata = {
            { "\u00DF", "SS" },
        };

        try {
            for (int i = 0; i < locales.length; i++) {
                Locale.setDefault(locales[i]);
                for (int j = 0; j < tdata.length; j++) {
                    assertTrue(Locale.getDefault() + ": " + j + " " + tdata[j][0] + " " + tdata[j][1], StringUtils
                            .containsIgnoreCase(tdata[j][0], tdata[j][1]));
                }
                for (int j = 0; j < fdata.length; j++) {
                    assertFalse(Locale.getDefault() + ": " + j + " " + fdata[j][0] + " " + fdata[j][1], StringUtils
                            .containsIgnoreCase(fdata[j][0], fdata[j][1]));
                }
            }
        } finally {
            Locale.setDefault(orig);
        }
    }

    public void testContainsIgnoreCase_StringString() {
        assertFalse(StringUtils.containsIgnoreCase(null, null));

        // Null tests
        assertFalse(StringUtils.containsIgnoreCase(null, ""));
        assertFalse(StringUtils.containsIgnoreCase(null, "a"));
        assertFalse(StringUtils.containsIgnoreCase(null, "abc"));

        assertFalse(StringUtils.containsIgnoreCase("", null));
        assertFalse(StringUtils.containsIgnoreCase("a", null));
        assertFalse(StringUtils.containsIgnoreCase("abc", null));

        // Match len = 0
        assertTrue(StringUtils.containsIgnoreCase("", ""));
        assertTrue(StringUtils.containsIgnoreCase("a", ""));
        assertTrue(StringUtils.containsIgnoreCase("abc", ""));

        // Match len = 1
        assertFalse(StringUtils.containsIgnoreCase("", "a"));
        assertTrue(StringUtils.containsIgnoreCase("a", "a"));
        assertTrue(StringUtils.containsIgnoreCase("abc", "a"));
        assertFalse(StringUtils.containsIgnoreCase("", "A"));
        assertTrue(StringUtils.containsIgnoreCase("a", "A"));
        assertTrue(StringUtils.containsIgnoreCase("abc", "A"));

        // Match len > 1
        assertFalse(StringUtils.containsIgnoreCase("", "abc"));
        assertFalse(StringUtils.containsIgnoreCase("a", "abc"));
        assertTrue(StringUtils.containsIgnoreCase("xabcz", "abc"));
        assertFalse(StringUtils.containsIgnoreCase("", "ABC"));
        assertFalse(StringUtils.containsIgnoreCase("a", "ABC"));
        assertTrue(StringUtils.containsIgnoreCase("xabcz", "ABC"));
    }

    public void testContainsNone_CharArray() {
        String str1 = "a";
        String str2 = "b";
        String str3 = "ab.";
        char[] chars1= {'b'};
        char[] chars2= {'.'};
        char[] chars3= {'c', 'd'};
        char[] emptyChars = new char[0];
        assertEquals(true, StringUtils.containsNone(null, (char[]) null));
        assertEquals(true, StringUtils.containsNone("", (char[]) null));
        assertEquals(true, StringUtils.containsNone(null, emptyChars));
        assertEquals(true, StringUtils.containsNone(str1, emptyChars));
        assertEquals(true, StringUtils.containsNone("", emptyChars));
        assertEquals(true, StringUtils.containsNone("", chars1));
        assertEquals(true, StringUtils.containsNone(str1, chars1));
        assertEquals(true, StringUtils.containsNone(str1, chars2));
        assertEquals(true, StringUtils.containsNone(str1, chars3));
        assertEquals(false, StringUtils.containsNone(str2, chars1));
        assertEquals(true, StringUtils.containsNone(str2, chars2));
        assertEquals(true, StringUtils.containsNone(str2, chars3));
        assertEquals(false, StringUtils.containsNone(str3, chars1));
        assertEquals(false, StringUtils.containsNone(str3, chars2));
        assertEquals(true, StringUtils.containsNone(str3, chars3));
    }

    /**
     * See http://java.sun.com/developer/technicalArticles/Intl/Supplementary/
     */
    public void testContainsNone_CharArrayWithBadSupplementaryChars() {
        // Test edge case: 1/2 of a (broken) supplementary char
        assertEquals(true, StringUtils.containsNone(CharUSuppCharHigh, CharU20001.toCharArray()));
        assertEquals(-1, CharUSuppCharLow.indexOf(CharU20001));
        assertEquals(true, StringUtils.containsNone(CharUSuppCharLow, CharU20001.toCharArray()));
        assertEquals(-1, CharU20001.indexOf(CharUSuppCharHigh));
        assertEquals(true, StringUtils.containsNone(CharU20001, CharUSuppCharHigh.toCharArray()));
        assertEquals(0, CharU20001.indexOf(CharUSuppCharLow));
        assertEquals(false, StringUtils.containsNone(CharU20001, CharUSuppCharLow.toCharArray()));
    }

    /**
     * See http://java.sun.com/developer/technicalArticles/Intl/Supplementary/
     */
    public void testContainsNone_CharArrayWithSupplementaryChars() {
        assertEquals(false, StringUtils.containsNone(CharU20000 + CharU20001, CharU20000.toCharArray()));
        assertEquals(false, StringUtils.containsNone(CharU20000 + CharU20001, CharU20001.toCharArray()));
        assertEquals(false, StringUtils.containsNone(CharU20000, CharU20000.toCharArray()));
        // Sanity check:
        assertEquals(-1, CharU20000.indexOf(CharU20001));
        assertEquals(0, CharU20000.indexOf(CharU20001.charAt(0)));
        assertEquals(-1, CharU20000.indexOf(CharU20001.charAt(1)));
        // Test:
        assertEquals(true, StringUtils.containsNone(CharU20000, CharU20001.toCharArray()));
        assertEquals(true, StringUtils.containsNone(CharU20001, CharU20000.toCharArray()));
    }

    public void testContainsNone_String() {
        String str1 = "a";
        String str2 = "b";
        String str3 = "ab.";
        String chars1= "b";
        String chars2= ".";
        String chars3= "cd";
        assertEquals(true, StringUtils.containsNone(null, (String) null));
        assertEquals(true, StringUtils.containsNone("", (String) null));
        assertEquals(true, StringUtils.containsNone(null, ""));
        assertEquals(true, StringUtils.containsNone(str1, ""));
        assertEquals(true, StringUtils.containsNone("", ""));
        assertEquals(true, StringUtils.containsNone("", chars1));
        assertEquals(true, StringUtils.containsNone(str1, chars1));
        assertEquals(true, StringUtils.containsNone(str1, chars2));
        assertEquals(true, StringUtils.containsNone(str1, chars3));
        assertEquals(false, StringUtils.containsNone(str2, chars1));
        assertEquals(true, StringUtils.containsNone(str2, chars2));
        assertEquals(true, StringUtils.containsNone(str2, chars3));
        assertEquals(false, StringUtils.containsNone(str3, chars1));
        assertEquals(false, StringUtils.containsNone(str3, chars2));
        assertEquals(true, StringUtils.containsNone(str3, chars3));
    }

    /**
     * See http://java.sun.com/developer/technicalArticles/Intl/Supplementary/
     */
    public void testContainsNone_StringWithBadSupplementaryChars() {
        // Test edge case: 1/2 of a (broken) supplementary char
        assertEquals(true, StringUtils.containsNone(CharUSuppCharHigh, CharU20001));
        assertEquals(-1, CharUSuppCharLow.indexOf(CharU20001));
        assertEquals(true, StringUtils.containsNone(CharUSuppCharLow, CharU20001));
        assertEquals(-1, CharU20001.indexOf(CharUSuppCharHigh));
        assertEquals(true, StringUtils.containsNone(CharU20001, CharUSuppCharHigh));
        assertEquals(0, CharU20001.indexOf(CharUSuppCharLow));
        assertEquals(false, StringUtils.containsNone(CharU20001, CharUSuppCharLow));
    }

    /**
     * See http://java.sun.com/developer/technicalArticles/Intl/Supplementary/
     */
    public void testContainsNone_StringWithSupplementaryChars() {
        assertEquals(false, StringUtils.containsNone(CharU20000 + CharU20001, CharU20000));
        assertEquals(false, StringUtils.containsNone(CharU20000 + CharU20001, CharU20001));
        assertEquals(false, StringUtils.containsNone(CharU20000, CharU20000));
        // Sanity check:
        assertEquals(-1, CharU20000.indexOf(CharU20001));
        assertEquals(0, CharU20000.indexOf(CharU20001.charAt(0)));
        assertEquals(-1, CharU20000.indexOf(CharU20001.charAt(1)));
        // Test:
        assertEquals(true, StringUtils.containsNone(CharU20000, CharU20001));
        assertEquals(true, StringUtils.containsNone(CharU20001, CharU20000));
    }

    public void testContainsOnly_CharArray() {
        String str1 = "a";
        String str2 = "b";
        String str3 = "ab";
        char[] chars1= {'b'};
        char[] chars2= {'a'};
        char[] chars3= {'a', 'b'};
        char[] emptyChars = new char[0];
        assertEquals(false, StringUtils.containsOnly(null, (char[]) null));
        assertEquals(false, StringUtils.containsOnly("", (char[]) null));
        assertEquals(false, StringUtils.containsOnly(null, emptyChars));
        assertEquals(false, StringUtils.containsOnly(str1, emptyChars));
        assertEquals(true, StringUtils.containsOnly("", emptyChars));
        assertEquals(true, StringUtils.containsOnly("", chars1));
        assertEquals(false, StringUtils.containsOnly(str1, chars1));
        assertEquals(true, StringUtils.containsOnly(str1, chars2));
        assertEquals(true, StringUtils.containsOnly(str1, chars3));
        assertEquals(true, StringUtils.containsOnly(str2, chars1));
        assertEquals(false, StringUtils.containsOnly(str2, chars2));
        assertEquals(true, StringUtils.containsOnly(str2, chars3));
        assertEquals(false, StringUtils.containsOnly(str3, chars1));
        assertEquals(false, StringUtils.containsOnly(str3, chars2));
        assertEquals(true, StringUtils.containsOnly(str3, chars3));
    }

    public void testContainsOnly_String() {
        String str1 = "a";
        String str2 = "b";
        String str3 = "ab";
        String chars1= "b";
        String chars2= "a";
        String chars3= "ab";
        assertEquals(false, StringUtils.containsOnly(null, (String) null));
        assertEquals(false, StringUtils.containsOnly("", (String) null));
        assertEquals(false, StringUtils.containsOnly(null, ""));
        assertEquals(false, StringUtils.containsOnly(str1, ""));
        assertEquals(true, StringUtils.containsOnly("", ""));
        assertEquals(true, StringUtils.containsOnly("", chars1));
        assertEquals(false, StringUtils.containsOnly(str1, chars1));
        assertEquals(true, StringUtils.containsOnly(str1, chars2));
        assertEquals(true, StringUtils.containsOnly(str1, chars3));
        assertEquals(true, StringUtils.containsOnly(str2, chars1));
        assertEquals(false, StringUtils.containsOnly(str2, chars2));
        assertEquals(true, StringUtils.containsOnly(str2, chars3));
        assertEquals(false, StringUtils.containsOnly(str3, chars1));
        assertEquals(false, StringUtils.containsOnly(str3, chars2));
        assertEquals(true, StringUtils.containsOnly(str3, chars3));
    }

    public void testEquals() {
        assertEquals(true, StringUtils.equals(null, null));
        assertEquals(true, StringUtils.equals(FOO, FOO));
        assertEquals(true, StringUtils.equals(FOO, new String(new char[] { 'f', 'o', 'o' })));
        assertEquals(false, StringUtils.equals(FOO, new String(new char[] { 'f', 'O', 'O' })));
        assertEquals(false, StringUtils.equals(FOO, BAR));
        assertEquals(false, StringUtils.equals(FOO, null));
        assertEquals(false, StringUtils.equals(null, FOO));
    }

    public void testEqualsIgnoreCase() {
        assertEquals(true, StringUtils.equalsIgnoreCase(null, null));
        assertEquals(true, StringUtils.equalsIgnoreCase(FOO, FOO));
        assertEquals(true, StringUtils.equalsIgnoreCase(FOO, new String(new char[] { 'f', 'o', 'o' })));
        assertEquals(true, StringUtils.equalsIgnoreCase(FOO, new String(new char[] { 'f', 'O', 'O' })));
        assertEquals(false, StringUtils.equalsIgnoreCase(FOO, BAR));
        assertEquals(false, StringUtils.equalsIgnoreCase(FOO, null));
        assertEquals(false, StringUtils.equalsIgnoreCase(null, FOO));
    }

    //-----------------------------------------------------------------------
    public void testIndexOf_char() {
        assertEquals(-1, StringUtils.indexOf(null, ' '));
        assertEquals(-1, StringUtils.indexOf("", ' '));
        assertEquals(0, StringUtils.indexOf("aabaabaa", 'a'));
        assertEquals(2, StringUtils.indexOf("aabaabaa", 'b'));
    }

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
    }

    public void testIndexOf_String() {
        assertEquals(-1, StringUtils.indexOf(null, null));
        assertEquals(-1, StringUtils.indexOf("", null));
        assertEquals(0, StringUtils.indexOf("", ""));
        assertEquals(0, StringUtils.indexOf("aabaabaa", "a"));
        assertEquals(2, StringUtils.indexOf("aabaabaa", "b"));
        assertEquals(1, StringUtils.indexOf("aabaabaa", "ab"));
        assertEquals(0, StringUtils.indexOf("aabaabaa", ""));
    }

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
        assertEquals(2,StringUtils.indexOf("aabaabaa", "", 2));
    }

    public void testIndexOfAny_StringCharArray() {
        assertEquals(-1, StringUtils.indexOfAny(null, (char[]) null));
        assertEquals(-1, StringUtils.indexOfAny(null, new char[0]));
        assertEquals(-1, StringUtils.indexOfAny(null, new char[] {'a','b'}));

        assertEquals(-1, StringUtils.indexOfAny("", (char[]) null));
        assertEquals(-1, StringUtils.indexOfAny("", new char[0]));
        assertEquals(-1, StringUtils.indexOfAny("", new char[] {'a','b'}));

        assertEquals(-1, StringUtils.indexOfAny("zzabyycdxx", (char[]) null));
        assertEquals(-1, StringUtils.indexOfAny("zzabyycdxx", new char[0]));
        assertEquals(0, StringUtils.indexOfAny("zzabyycdxx", new char[] {'z','a'}));
        assertEquals(3, StringUtils.indexOfAny("zzabyycdxx", new char[] {'b','y'}));
        assertEquals(-1, StringUtils.indexOfAny("ab", new char[] {'z'}));
    }

    /**
     * See http://java.sun.com/developer/technicalArticles/Intl/Supplementary/
     */
    public void testIndexOfAny_StringCharArrayWithSupplementaryChars() {
        assertEquals(0, StringUtils.indexOfAny(CharU20000 + CharU20001, CharU20000.toCharArray()));
        assertEquals(2, StringUtils.indexOfAny(CharU20000 + CharU20001, CharU20001.toCharArray()));
        assertEquals(0, StringUtils.indexOfAny(CharU20000, CharU20000.toCharArray()));
        assertEquals(-1, StringUtils.indexOfAny(CharU20000, CharU20001.toCharArray()));
    }

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
     * See http://java.sun.com/developer/technicalArticles/Intl/Supplementary/
     */
    public void testIndexOfAny_StringStringWithSupplementaryChars() {
        assertEquals(0, StringUtils.indexOfAny(CharU20000 + CharU20001, CharU20000));
        assertEquals(2, StringUtils.indexOfAny(CharU20000 + CharU20001, CharU20001));
        assertEquals(0, StringUtils.indexOfAny(CharU20000, CharU20000));
        assertEquals(-1, StringUtils.indexOfAny(CharU20000, CharU20001));
    }

    public void testIndexOfAnyBut_StringCharArray() {
        assertEquals(-1, StringUtils.indexOfAnyBut(null, (char[]) null));
        assertEquals(-1, StringUtils.indexOfAnyBut(null, new char[0]));
        assertEquals(-1, StringUtils.indexOfAnyBut(null, new char[] {'a','b'}));

        assertEquals(-1, StringUtils.indexOfAnyBut("", (char[]) null));
        assertEquals(-1, StringUtils.indexOfAnyBut("", new char[0]));
        assertEquals(-1, StringUtils.indexOfAnyBut("", new char[] {'a','b'}));

        assertEquals(-1, StringUtils.indexOfAnyBut("zzabyycdxx", (char[]) null));
        assertEquals(-1, StringUtils.indexOfAnyBut("zzabyycdxx", new char[0]));
        assertEquals(3, StringUtils.indexOfAnyBut("zzabyycdxx", new char[] {'z','a'}));
        assertEquals(0, StringUtils.indexOfAnyBut("zzabyycdxx", new char[] {'b','y'}));
        assertEquals(0, StringUtils.indexOfAnyBut("ab", new char[] {'z'}));
    }

    public void testIndexOfAnyBut_StringCharArrayWithSupplementaryChars() {
        assertEquals(2, StringUtils.indexOfAnyBut(CharU20000 + CharU20001, CharU20000.toCharArray()));
        assertEquals(0, StringUtils.indexOfAnyBut(CharU20000 + CharU20001, CharU20001.toCharArray()));
        assertEquals(-1, StringUtils.indexOfAnyBut(CharU20000, CharU20000.toCharArray()));
        assertEquals(0, StringUtils.indexOfAnyBut(CharU20000, CharU20001.toCharArray()));
    }

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

    public void testIndexOfAnyBut_StringStringWithSupplementaryChars() {
        assertEquals(2, StringUtils.indexOfAnyBut(CharU20000 + CharU20001, CharU20000));
        assertEquals(0, StringUtils.indexOfAnyBut(CharU20000 + CharU20001, CharU20001));
        assertEquals(-1, StringUtils.indexOfAnyBut(CharU20000, CharU20000));
        assertEquals(0, StringUtils.indexOfAnyBut(CharU20000, CharU20001));
    }

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
    }

    public void testLastIndexOf_char() {
        assertEquals(-1, StringUtils.lastIndexOf(null, ' '));
        assertEquals(-1, StringUtils.lastIndexOf("", ' '));
        assertEquals(7, StringUtils.lastIndexOf("aabaabaa", 'a'));
        assertEquals(5, StringUtils.lastIndexOf("aabaabaa", 'b'));
    }

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
    }

    public void testLastIndexOf_String() {
        assertEquals(-1, StringUtils.lastIndexOf(null, null));
        assertEquals(-1, StringUtils.lastIndexOf("", null));
        assertEquals(-1, StringUtils.lastIndexOf("", "a"));
        assertEquals(0, StringUtils.lastIndexOf("", ""));
        assertEquals(8, StringUtils.lastIndexOf("aabaabaa", ""));
        assertEquals(7, StringUtils.lastIndexOf("aabaabaa", "a"));
        assertEquals(5, StringUtils.lastIndexOf("aabaabaa", "b"));
        assertEquals(4, StringUtils.lastIndexOf("aabaabaa", "ab"));
    }

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
    }

    public void testLastIndexOfAny_StringStringArray() {
        assertEquals(-1, StringUtils.lastIndexOfAny(null, null));
        assertEquals(-1, StringUtils.lastIndexOfAny(null, FOOBAR_SUB_ARRAY));
        assertEquals(-1, StringUtils.lastIndexOfAny(FOOBAR, null));
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
    }

}
