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

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.UnsupportedEncodingException;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.regex.PatternSyntaxException;

import org.apache.commons.lang3.text.WordUtils;
import org.junit.jupiter.api.Test;

/**
 * Unit tests for methods of {@link org.apache.commons.lang3.StringUtils}
 * which been moved to their own test classes.
 */
@SuppressWarnings("deprecation") // deliberate use of deprecated code
public class StringUtilsTest {

    static final String WHITESPACE;
    static final String NON_WHITESPACE;
    static final String HARD_SPACE;
    static final String TRIMMABLE;
    static final String NON_TRIMMABLE;

    static {
        String ws = "";
        String nws = "";
        final String hs = String.valueOf(((char) 160));
        String tr = "";
        String ntr = "";
        for (int i = 0; i < Character.MAX_VALUE; i++) {
            if (Character.isWhitespace((char) i)) {
                ws += String.valueOf((char) i);
                if (i > 32) {
                    ntr += String.valueOf((char) i);
                }
            } else if (i < 40) {
                nws += String.valueOf((char) i);
            }
        }
        for (int i = 0; i <= 32; i++) {
            tr += String.valueOf((char) i);
        }
        WHITESPACE = ws;
        NON_WHITESPACE = nws;
        HARD_SPACE = hs;
        TRIMMABLE = tr;
        NON_TRIMMABLE = ntr;
    }

    private static final String[] ARRAY_LIST = {"foo", "bar", "baz"};
    private static final String[] EMPTY_ARRAY_LIST = {};
    private static final String[] NULL_ARRAY_LIST = {null};
    private static final Object[] NULL_TO_STRING_LIST = {
            new Object() {
                @Override
                public String toString() {
                    return null;
                }
            }
    };
    private static final String[] MIXED_ARRAY_LIST = {null, "", "foo"};
    private static final Object[] MIXED_TYPE_LIST = {"foo", Long.valueOf(2L)};
    private static final long[] LONG_PRIM_LIST = {1, 2};
    private static final int[] INT_PRIM_LIST = {1, 2};
    private static final byte[] BYTE_PRIM_LIST = {1, 2};
    private static final short[] SHORT_PRIM_LIST = {1, 2};
    private static final char[] CHAR_PRIM_LIST = {'1', '2'};
    private static final float[] FLOAT_PRIM_LIST = {1, 2};
    private static final double[] DOUBLE_PRIM_LIST = {1, 2};
    private static final List<String> MIXED_STRING_LIST = Arrays.asList(null, "", "foo");
    private static final List<Object> MIXED_TYPE_OBJECT_LIST = Arrays.<Object>asList("foo", Long.valueOf(2L));
    private static final List<String> STRING_LIST = Arrays.asList("foo", "bar", "baz");
    private static final List<String> EMPTY_STRING_LIST = Collections.emptyList();
    private static final List<String> NULL_STRING_LIST = Collections.singletonList(null);

    private static final String SEPARATOR = ",";
    private static final char SEPARATOR_CHAR = ';';

    private static final String TEXT_LIST = "foo,bar,baz";
    private static final String TEXT_LIST_CHAR = "foo;bar;baz";
    private static final String TEXT_LIST_NOSEP = "foobarbaz";

    private static final String FOO_UNCAP = "foo";
    private static final String FOO_CAP = "Foo";

    private static final String SENTENCE_UNCAP = "foo bar baz";
    private static final String SENTENCE_CAP = "Foo Bar Baz";

    private void assertAbbreviateWithAbbrevMarkerAndOffset(final String expected, final String abbrevMarker, final int offset, final int maxWidth) {
        final String abcdefghijklmno = "abcdefghijklmno";
        final String message = "abbreviate(String,String,int,int) failed";
        final String actual = StringUtils.abbreviate(abcdefghijklmno, abbrevMarker, offset, maxWidth);
        if (offset >= 0 && offset < abcdefghijklmno.length()) {
            assertTrue(actual.indexOf((char) ('a' + offset)) != -1,
                    message + " -- should contain offset character");
        }
        assertTrue(actual.length() <= maxWidth,
                message + " -- should not be greater than maxWidth");
        assertEquals(expected, actual, message);
    }

    private void assertAbbreviateWithOffset(final String expected, final int offset, final int maxWidth) {
        final String abcdefghijklmno = "abcdefghijklmno";
        final String message = "abbreviate(String,int,int) failed";
        final String actual = StringUtils.abbreviate(abcdefghijklmno, offset, maxWidth);
        if (offset >= 0 && offset < abcdefghijklmno.length()) {
            assertTrue(actual.indexOf((char) ('a' + offset)) != -1,
                    message + " -- should contain offset character");
        }
        assertTrue(actual.length() <= maxWidth,
                message + " -- should not be greater than maxWidth");
        assertEquals(expected, actual, message);
    }

    private void innerTestSplit(final char separator, final String sepStr, final char noMatch) {
        final String msg = "Failed on separator hex(" + Integer.toHexString(separator) +
                "), noMatch hex(" + Integer.toHexString(noMatch) + "), sepStr(" + sepStr + ")";

        final String str = "a" + separator + "b" + separator + separator + noMatch + "c";
        String[] res;
        // (str, sepStr)
        res = StringUtils.split(str, sepStr);
        assertEquals(3, res.length, msg);
        assertEquals("a", res[0]);
        assertEquals("b", res[1]);
        assertEquals(noMatch + "c", res[2]);

        final String str2 = separator + "a" + separator;
        res = StringUtils.split(str2, sepStr);
        assertEquals(1, res.length, msg);
        assertEquals("a", res[0], msg);

        res = StringUtils.split(str, sepStr, -1);
        assertEquals(3, res.length, msg);
        assertEquals("a", res[0], msg);
        assertEquals("b", res[1], msg);
        assertEquals(noMatch + "c", res[2], msg);

        res = StringUtils.split(str, sepStr, 0);
        assertEquals(3, res.length, msg);
        assertEquals("a", res[0], msg);
        assertEquals("b", res[1], msg);
        assertEquals(noMatch + "c", res[2], msg);

        res = StringUtils.split(str, sepStr, 1);
        assertEquals(1, res.length, msg);
        assertEquals(str, res[0], msg);

        res = StringUtils.split(str, sepStr, 2);
        assertEquals(2, res.length, msg);
        assertEquals("a", res[0], msg);
        assertEquals(str.substring(2), res[1], msg);
    }

    private void innerTestSplitPreserveAllTokens(final char separator, final String sepStr, final char noMatch) {
        final String msg = "Failed on separator hex(" + Integer.toHexString(separator) +
                "), noMatch hex(" + Integer.toHexString(noMatch) + "), sepStr(" + sepStr + ")";

        final String str = "a" + separator + "b" + separator + separator + noMatch + "c";
        String[] res;
        // (str, sepStr)
        res = StringUtils.splitPreserveAllTokens(str, sepStr);
        assertEquals(4, res.length, msg);
        assertEquals("a", res[0], msg);
        assertEquals("b", res[1], msg);
        assertEquals("", res[2], msg);
        assertEquals(noMatch + "c", res[3], msg);

        final String str2 = separator + "a" + separator;
        res = StringUtils.splitPreserveAllTokens(str2, sepStr);
        assertEquals(3, res.length, msg);
        assertEquals("", res[0], msg);
        assertEquals("a", res[1], msg);
        assertEquals("", res[2], msg);

        res = StringUtils.splitPreserveAllTokens(str, sepStr, -1);
        assertEquals(4, res.length, msg);
        assertEquals("a", res[0], msg);
        assertEquals("b", res[1], msg);
        assertEquals("", res[2], msg);
        assertEquals(noMatch + "c", res[3], msg);

        res = StringUtils.splitPreserveAllTokens(str, sepStr, 0);
        assertEquals(4, res.length, msg);
        assertEquals("a", res[0], msg);
        assertEquals("b", res[1], msg);
        assertEquals("", res[2], msg);
        assertEquals(noMatch + "c", res[3], msg);

        res = StringUtils.splitPreserveAllTokens(str, sepStr, 1);
        assertEquals(1, res.length, msg);
        assertEquals(str, res[0], msg);

        res = StringUtils.splitPreserveAllTokens(str, sepStr, 2);
        assertEquals(2, res.length, msg);
        assertEquals("a", res[0], msg);
        assertEquals(str.substring(2), res[1], msg);
    }

    //-----------------------------------------------------------------------
    @Test
    public void testAbbreviate_StringInt() {
        assertNull(StringUtils.abbreviate(null, 10));
        assertEquals("", StringUtils.abbreviate("", 10));
        assertEquals("short", StringUtils.abbreviate("short", 10));
        assertEquals("Now is ...", StringUtils.abbreviate("Now is the time for all good men to come to the aid of their party.", 10));

        final String raspberry = "raspberry peach";
        assertEquals("raspberry p...", StringUtils.abbreviate(raspberry, 14));
        assertEquals("raspberry peach", StringUtils.abbreviate("raspberry peach", 15));
        assertEquals("raspberry peach", StringUtils.abbreviate("raspberry peach", 16));
        assertEquals("abc...", StringUtils.abbreviate("abcdefg", 6));
        assertEquals("abcdefg", StringUtils.abbreviate("abcdefg", 7));
        assertEquals("abcdefg", StringUtils.abbreviate("abcdefg", 8));
        assertEquals("a...", StringUtils.abbreviate("abcdefg", 4));
        assertEquals("", StringUtils.abbreviate("", 4));

        assertThrows(
                IllegalArgumentException.class,
                () -> StringUtils.abbreviate("abc", 3),
                "StringUtils.abbreviate expecting IllegalArgumentException");
    }

    @Test
    public void testAbbreviate_StringIntInt() {
        assertNull(StringUtils.abbreviate(null, 10, 12));
        assertEquals("", StringUtils.abbreviate("", 0, 10));
        assertEquals("", StringUtils.abbreviate("", 2, 10));

        assertThrows(
                IllegalArgumentException.class,
                () -> StringUtils.abbreviate("abcdefghij", 0, 3),
                "StringUtils.abbreviate expecting IllegalArgumentException");
        assertThrows(
                IllegalArgumentException.class,
                () -> StringUtils.abbreviate("abcdefghij", 5, 6),
                "StringUtils.abbreviate expecting IllegalArgumentException");

        final String raspberry = "raspberry peach";
        assertEquals("raspberry peach", StringUtils.abbreviate(raspberry, 11, 15));

        assertNull(StringUtils.abbreviate(null, 7, 14));
        assertAbbreviateWithOffset("abcdefg...", -1, 10);
        assertAbbreviateWithOffset("abcdefg...", 0, 10);
        assertAbbreviateWithOffset("abcdefg...", 1, 10);
        assertAbbreviateWithOffset("abcdefg...", 2, 10);
        assertAbbreviateWithOffset("abcdefg...", 3, 10);
        assertAbbreviateWithOffset("abcdefg...", 4, 10);
        assertAbbreviateWithOffset("...fghi...", 5, 10);
        assertAbbreviateWithOffset("...ghij...", 6, 10);
        assertAbbreviateWithOffset("...hijk...", 7, 10);
        assertAbbreviateWithOffset("...ijklmno", 8, 10);
        assertAbbreviateWithOffset("...ijklmno", 9, 10);
        assertAbbreviateWithOffset("...ijklmno", 10, 10);
        assertAbbreviateWithOffset("...ijklmno", 10, 10);
        assertAbbreviateWithOffset("...ijklmno", 11, 10);
        assertAbbreviateWithOffset("...ijklmno", 12, 10);
        assertAbbreviateWithOffset("...ijklmno", 13, 10);
        assertAbbreviateWithOffset("...ijklmno", 14, 10);
        assertAbbreviateWithOffset("...ijklmno", 15, 10);
        assertAbbreviateWithOffset("...ijklmno", 16, 10);
        assertAbbreviateWithOffset("...ijklmno", Integer.MAX_VALUE, 10);
    }

    @Test
    public void testAbbreviate_StringStringInt() {
        assertNull(StringUtils.abbreviate(null, null, 10));
        assertNull(StringUtils.abbreviate(null, "...", 10));
        assertEquals("paranaguacu", StringUtils.abbreviate("paranaguacu", null, 10));
        assertEquals("", StringUtils.abbreviate("", "...", 2));
        assertEquals("wai**", StringUtils.abbreviate("waiheke", "**", 5));
        assertEquals("And af,,,,", StringUtils.abbreviate("And after a long time, he finally met his son.", ",,,,", 10));

        final String raspberry = "raspberry peach";
        assertEquals("raspberry pe..", StringUtils.abbreviate(raspberry, "..", 14));
        assertEquals("raspberry peach", StringUtils.abbreviate("raspberry peach", "---*---", 15));
        assertEquals("raspberry peach", StringUtils.abbreviate("raspberry peach", ".", 16));
        assertEquals("abc()(", StringUtils.abbreviate("abcdefg", "()(", 6));
        assertEquals("abcdefg", StringUtils.abbreviate("abcdefg", ";", 7));
        assertEquals("abcdefg", StringUtils.abbreviate("abcdefg", "_-", 8));
        assertEquals("abc.", StringUtils.abbreviate("abcdefg", ".", 4));
        assertEquals("", StringUtils.abbreviate("", 4));

        assertThrows(
                IllegalArgumentException.class,
                () -> StringUtils.abbreviate("abcdefghij", "...", 3),
                "StringUtils.abbreviate expecting IllegalArgumentException");
    }

    @Test
    public void testAbbreviate_StringStringIntInt() {
        assertNull(StringUtils.abbreviate(null, null, 10, 12));
        assertNull(StringUtils.abbreviate(null, "...", 10, 12));
        assertEquals("", StringUtils.abbreviate("", null, 0, 10));
        assertEquals("", StringUtils.abbreviate("", "...", 2, 10));

        assertThrows(
                IllegalArgumentException.class,
                () -> StringUtils.abbreviate("abcdefghij", "::", 0, 2),
                "StringUtils.abbreviate expecting IllegalArgumentException");
        assertThrows(
                IllegalArgumentException.class,
                () -> StringUtils.abbreviate("abcdefghij", "!!!", 5, 6),
                "StringUtils.abbreviate expecting IllegalArgumentException");

        final String raspberry = "raspberry peach";
        assertEquals("raspberry peach", StringUtils.abbreviate(raspberry, "--", 12, 15));

        assertNull(StringUtils.abbreviate(null, ";", 7, 14));
        assertAbbreviateWithAbbrevMarkerAndOffset("abcdefgh;;", ";;", -1, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("abcdefghi.", ".", 0, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("abcdefgh++", "++", 1, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("abcdefghi*", "*", 2, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("abcdef{{{{", "{{{{", 4, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("abcdef____", "____", 5, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("==fghijk==", "==", 5, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("___ghij___", "___", 6, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("/ghijklmno", "/", 7, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("/ghijklmno", "/", 8, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("/ghijklmno", "/", 9, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("///ijklmno", "///", 10, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("//hijklmno", "//", 10, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("//hijklmno", "//", 11, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("...ijklmno", "...", 12, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("/ghijklmno", "/", 13, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("/ghijklmno", "/", 14, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("999ijklmno", "999", 15, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("_ghijklmno", "_", 16, 10);
        assertAbbreviateWithAbbrevMarkerAndOffset("+ghijklmno", "+", Integer.MAX_VALUE, 10);
    }

    @Test
    public void testAbbreviateMiddle() {
        // javadoc examples
        assertNull(StringUtils.abbreviateMiddle(null, null, 0));
        assertEquals("abc", StringUtils.abbreviateMiddle("abc", null, 0));
        assertEquals("abc", StringUtils.abbreviateMiddle("abc", ".", 0));
        assertEquals("abc", StringUtils.abbreviateMiddle("abc", ".", 3));
        assertEquals("ab.f", StringUtils.abbreviateMiddle("abcdef", ".", 4));

        // JIRA issue (LANG-405) example (slightly different than actual expected result)
        assertEquals(
                "A very long text with un...f the text is complete.",
                StringUtils.abbreviateMiddle(
                        "A very long text with unimportant stuff in the middle but interesting start and " +
                                "end to see if the text is complete.", "...", 50));

        // Test a much longer text :)
        final String longText = "Start text" + StringUtils.repeat("x", 10000) + "Close text";
        assertEquals(
                "Start text->Close text",
                StringUtils.abbreviateMiddle(longText, "->", 22));

        // Test negative length
        assertEquals("abc", StringUtils.abbreviateMiddle("abc", ".", -1));

        // Test boundaries
        // Fails to change anything as method ensures first and last char are kept
        assertEquals("abc", StringUtils.abbreviateMiddle("abc", ".", 1));
        assertEquals("abc", StringUtils.abbreviateMiddle("abc", ".", 2));

        // Test length of n=1
        assertEquals("a", StringUtils.abbreviateMiddle("a", ".", 1));

        // Test smallest length that can lead to success
        assertEquals("a.d", StringUtils.abbreviateMiddle("abcd", ".", 3));

        // More from LANG-405
        assertEquals("a..f", StringUtils.abbreviateMiddle("abcdef", "..", 4));
        assertEquals("ab.ef", StringUtils.abbreviateMiddle("abcdef", ".", 5));
    }

    /**
     * Tests {@code appendIfMissing}.
     */
    @Test
    public void testAppendIfMissing() {
        assertNull(StringUtils.appendIfMissing(null, null), "appendIfMissing(null,null)");
        assertEquals("abc", StringUtils.appendIfMissing("abc", null), "appendIfMissing(abc,null)");
        assertEquals("xyz", StringUtils.appendIfMissing("", "xyz"), "appendIfMissing(\"\",xyz)");
        assertEquals("abcxyz", StringUtils.appendIfMissing("abc", "xyz"), "appendIfMissing(abc,xyz)");
        assertEquals("abcxyz", StringUtils.appendIfMissing("abcxyz", "xyz"), "appendIfMissing(abcxyz,xyz)");
        assertEquals("aXYZxyz", StringUtils.appendIfMissing("aXYZ", "xyz"), "appendIfMissing(aXYZ,xyz)");

        assertNull(StringUtils.appendIfMissing(null, null, (CharSequence[]) null), "appendIfMissing(null,null,null)");
        assertEquals("abc", StringUtils.appendIfMissing("abc", null, (CharSequence[]) null), "appendIfMissing(abc,null,null)");
        assertEquals("xyz", StringUtils.appendIfMissing("", "xyz", (CharSequence[]) null), "appendIfMissing(\"\",xyz,null))");
        assertEquals("abcxyz", StringUtils.appendIfMissing("abc", "xyz", new CharSequence[]{null}), "appendIfMissing(abc,xyz,{null})");
        assertEquals("abc", StringUtils.appendIfMissing("abc", "xyz", ""), "appendIfMissing(abc,xyz,\"\")");
        assertEquals("abcxyz", StringUtils.appendIfMissing("abc", "xyz", "mno"), "appendIfMissing(abc,xyz,mno)");
        assertEquals("abcxyz", StringUtils.appendIfMissing("abcxyz", "xyz", "mno"), "appendIfMissing(abcxyz,xyz,mno)");
        assertEquals("abcmno", StringUtils.appendIfMissing("abcmno", "xyz", "mno"), "appendIfMissing(abcmno,xyz,mno)");
        assertEquals("abcXYZxyz", StringUtils.appendIfMissing("abcXYZ", "xyz", "mno"), "appendIfMissing(abcXYZ,xyz,mno)");
        assertEquals("abcMNOxyz", StringUtils.appendIfMissing("abcMNO", "xyz", "mno"), "appendIfMissing(abcMNO,xyz,mno)");
    }

    /**
     * Tests {@code appendIfMissingIgnoreCase}.
     */
    @Test
    public void testAppendIfMissingIgnoreCase() {
        assertNull(StringUtils.appendIfMissingIgnoreCase(null, null), "appendIfMissingIgnoreCase(null,null)");
        assertEquals("abc", StringUtils.appendIfMissingIgnoreCase("abc", null), "appendIfMissingIgnoreCase(abc,null)");
        assertEquals("xyz", StringUtils.appendIfMissingIgnoreCase("", "xyz"), "appendIfMissingIgnoreCase(\"\",xyz)");
        assertEquals("abcxyz", StringUtils.appendIfMissingIgnoreCase("abc", "xyz"), "appendIfMissingIgnoreCase(abc,xyz)");
        assertEquals("abcxyz", StringUtils.appendIfMissingIgnoreCase("abcxyz", "xyz"), "appendIfMissingIgnoreCase(abcxyz,xyz)");
        assertEquals("abcXYZ", StringUtils.appendIfMissingIgnoreCase("abcXYZ", "xyz"), "appendIfMissingIgnoreCase(abcXYZ,xyz)");

        assertNull(StringUtils.appendIfMissingIgnoreCase(null, null, (CharSequence[]) null), "appendIfMissingIgnoreCase(null,null,null)");
        assertEquals("abc", StringUtils.appendIfMissingIgnoreCase("abc", null, (CharSequence[]) null), "appendIfMissingIgnoreCase(abc,null,null)");
        assertEquals("xyz", StringUtils.appendIfMissingIgnoreCase("", "xyz", (CharSequence[]) null), "appendIfMissingIgnoreCase(\"\",xyz,null)");
        assertEquals("abcxyz", StringUtils.appendIfMissingIgnoreCase("abc", "xyz", new CharSequence[]{null}), "appendIfMissingIgnoreCase(abc,xyz,{null})");
        assertEquals("abc", StringUtils.appendIfMissingIgnoreCase("abc", "xyz", ""), "appendIfMissingIgnoreCase(abc,xyz,\"\")");
        assertEquals("abcxyz", StringUtils.appendIfMissingIgnoreCase("abc", "xyz", "mno"), "appendIfMissingIgnoreCase(abc,xyz,mno)");
        assertEquals("abcxyz", StringUtils.appendIfMissingIgnoreCase("abcxyz", "xyz", "mno"), "appendIfMissingIgnoreCase(abcxyz,xyz,mno)");
        assertEquals("abcmno", StringUtils.appendIfMissingIgnoreCase("abcmno", "xyz", "mno"), "appendIfMissingIgnoreCase(abcmno,xyz,mno)");
        assertEquals("abcXYZ", StringUtils.appendIfMissingIgnoreCase("abcXYZ", "xyz", "mno"), "appendIfMissingIgnoreCase(abcXYZ,xyz,mno)");
        assertEquals("abcMNO", StringUtils.appendIfMissingIgnoreCase("abcMNO", "xyz", "mno"), "appendIfMissingIgnoreCase(abcMNO,xyz,mno)");
    }

    @Test
    public void testCapitalize() {
        assertNull(StringUtils.capitalize(null));

        assertEquals("", StringUtils.capitalize(""), "capitalize(empty-string) failed");
        assertEquals("X", StringUtils.capitalize("x"), "capitalize(single-char-string) failed");
        assertEquals(FOO_CAP, StringUtils.capitalize(FOO_CAP), "capitalize(String) failed");
        assertEquals(FOO_CAP, StringUtils.capitalize(FOO_UNCAP), "capitalize(string) failed");

        assertEquals("\u01C8", StringUtils.capitalize("\u01C9"), "capitalize(String) is not using TitleCase");

        // Javadoc examples
        assertNull(StringUtils.capitalize(null));
        assertEquals("", StringUtils.capitalize(""));
        assertEquals("Cat", StringUtils.capitalize("cat"));
        assertEquals("CAt", StringUtils.capitalize("cAt"));
        assertEquals("'cat'", StringUtils.capitalize("'cat'"));
    }

    //-----------------------------------------------------------------------
    @Test
    public void testCenter_StringInt() {
        assertNull(StringUtils.center(null, -1));
        assertNull(StringUtils.center(null, 4));
        assertEquals("    ", StringUtils.center("", 4));
        assertEquals("ab", StringUtils.center("ab", 0));
        assertEquals("ab", StringUtils.center("ab", -1));
        assertEquals("ab", StringUtils.center("ab", 1));
        assertEquals("    ", StringUtils.center("", 4));
        assertEquals(" ab ", StringUtils.center("ab", 4));
        assertEquals("abcd", StringUtils.center("abcd", 2));
        assertEquals(" a  ", StringUtils.center("a", 4));
        assertEquals("  a  ", StringUtils.center("a", 5));
    }

    @Test
    public void testCenter_StringIntChar() {
        assertNull(StringUtils.center(null, -1, ' '));
        assertNull(StringUtils.center(null, 4, ' '));
        assertEquals("    ", StringUtils.center("", 4, ' '));
        assertEquals("ab", StringUtils.center("ab", 0, ' '));
        assertEquals("ab", StringUtils.center("ab", -1, ' '));
        assertEquals("ab", StringUtils.center("ab", 1, ' '));
        assertEquals("    ", StringUtils.center("", 4, ' '));
        assertEquals(" ab ", StringUtils.center("ab", 4, ' '));
        assertEquals("abcd", StringUtils.center("abcd", 2, ' '));
        assertEquals(" a  ", StringUtils.center("a", 4, ' '));
        assertEquals("  a  ", StringUtils.center("a", 5, ' '));
        assertEquals("xxaxx", StringUtils.center("a", 5, 'x'));
    }

    @Test
    public void testCenter_StringIntString() {
        assertNull(StringUtils.center(null, 4, null));
        assertNull(StringUtils.center(null, -1, " "));
        assertNull(StringUtils.center(null, 4, " "));
        assertEquals("    ", StringUtils.center("", 4, " "));
        assertEquals("ab", StringUtils.center("ab", 0, " "));
        assertEquals("ab", StringUtils.center("ab", -1, " "));
        assertEquals("ab", StringUtils.center("ab", 1, " "));
        assertEquals("    ", StringUtils.center("", 4, " "));
        assertEquals(" ab ", StringUtils.center("ab", 4, " "));
        assertEquals("abcd", StringUtils.center("abcd", 2, " "));
        assertEquals(" a  ", StringUtils.center("a", 4, " "));
        assertEquals("yayz", StringUtils.center("a", 4, "yz"));
        assertEquals("yzyayzy", StringUtils.center("a", 7, "yz"));
        assertEquals("  abc  ", StringUtils.center("abc", 7, null));
        assertEquals("  abc  ", StringUtils.center("abc", 7, ""));
    }

    @Test
    public void testChomp() {

        final String[][] chompCases = {
                {FOO_UNCAP + "\r\n", FOO_UNCAP},
                {FOO_UNCAP + "\n", FOO_UNCAP},
                {FOO_UNCAP + "\r", FOO_UNCAP},
                {FOO_UNCAP + " \r", FOO_UNCAP + " "},
                {FOO_UNCAP, FOO_UNCAP},
                {FOO_UNCAP + "\n\n", FOO_UNCAP + "\n"},
                {FOO_UNCAP + "\r\n\r\n", FOO_UNCAP + "\r\n"},
                {"foo\nfoo", "foo\nfoo"},
                {"foo\n\rfoo", "foo\n\rfoo"},
                {"\n", ""},
                {"\r", ""},
                {"a", "a"},
                {"\r\n", ""},
                {"", ""},
                {null, null},
                {FOO_UNCAP + "\n\r", FOO_UNCAP + "\n"}
        };
        for (final String[] chompCase : chompCases) {
            final String original = chompCase[0];
            final String expectedResult = chompCase[1];
            assertEquals(expectedResult, StringUtils.chomp(original), "chomp(String) failed");
        }

        assertEquals("foo", StringUtils.chomp("foobar", "bar"), "chomp(String, String) failed");
        assertEquals("foobar", StringUtils.chomp("foobar", "baz"), "chomp(String, String) failed");
        assertEquals("foo", StringUtils.chomp("foo", "foooo"), "chomp(String, String) failed");
        assertEquals("foobar", StringUtils.chomp("foobar", ""), "chomp(String, String) failed");
        assertEquals("foobar", StringUtils.chomp("foobar", null), "chomp(String, String) failed");
        assertEquals("", StringUtils.chomp("", "foo"), "chomp(String, String) failed");
        assertEquals("", StringUtils.chomp("", null), "chomp(String, String) failed");
        assertEquals("", StringUtils.chomp("", ""), "chomp(String, String) failed");
        assertNull(StringUtils.chomp(null, "foo"), "chomp(String, String) failed");
        assertNull(StringUtils.chomp(null, null), "chomp(String, String) failed");
        assertNull(StringUtils.chomp(null, ""), "chomp(String, String) failed");
        assertEquals("", StringUtils.chomp("foo", "foo"), "chomp(String, String) failed");
        assertEquals(" ", StringUtils.chomp(" foo", "foo"), "chomp(String, String) failed");
        assertEquals("foo ", StringUtils.chomp("foo ", "foo"), "chomp(String, String) failed");
    }

    @Test
    public void testChop() {

        final String[][] chopCases = {
                {FOO_UNCAP + "\r\n", FOO_UNCAP},
                {FOO_UNCAP + "\n", FOO_UNCAP},
                {FOO_UNCAP + "\r", FOO_UNCAP},
                {FOO_UNCAP + " \r", FOO_UNCAP + " "},
                {"foo", "fo"},
                {"foo\nfoo", "foo\nfo"},
                {"\n", ""},
                {"\r", ""},
                {"\r\n", ""},
                {null, null},
                {"", ""},
                {"a", ""},
        };
        for (final String[] chopCase : chopCases) {
            final String original = chopCase[0];
            final String expectedResult = chopCase[1];
            assertEquals(expectedResult, StringUtils.chop(original), "chop(String) failed");
        }
    }

    //-----------------------------------------------------------------------
    @Test
    public void testConstructor() {
        assertNotNull(new StringUtils());
        final Constructor<?>[] cons = StringUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertTrue(Modifier.isPublic(cons[0].getModifiers()));
        assertTrue(Modifier.isPublic(StringUtils.class.getModifiers()));
        assertFalse(Modifier.isFinal(StringUtils.class.getModifiers()));
    }

    //-----------------------------------------------------------------------
    @Test
    public void testDefault_String() {
        assertEquals("", StringUtils.defaultString(null));
        assertEquals("", StringUtils.defaultString(""));
        assertEquals("abc", StringUtils.defaultString("abc"));
    }

    @Test
    public void testDefault_StringString() {
        assertEquals("NULL", StringUtils.defaultString(null, "NULL"));
        assertEquals("", StringUtils.defaultString("", "NULL"));
        assertEquals("abc", StringUtils.defaultString("abc", "NULL"));
    }

    @Test
    public void testDefaultIfBlank_CharBuffers() {
        assertEquals("NULL", StringUtils.defaultIfBlank(CharBuffer.wrap(""), CharBuffer.wrap("NULL")).toString());
        assertEquals("NULL", StringUtils.defaultIfBlank(CharBuffer.wrap(" "), CharBuffer.wrap("NULL")).toString());
        assertEquals("abc", StringUtils.defaultIfBlank(CharBuffer.wrap("abc"), CharBuffer.wrap("NULL")).toString());
        assertNull(StringUtils.defaultIfBlank(CharBuffer.wrap(""), null));
        // Tests compatibility for the API return type
        final CharBuffer s = StringUtils.defaultIfBlank(CharBuffer.wrap("abc"), CharBuffer.wrap("NULL"));
        assertEquals("abc", s.toString());
    }

    @Test
    public void testDefaultIfBlank_StringBuffers() {
        assertEquals("NULL", StringUtils.defaultIfBlank(new StringBuffer(""), new StringBuffer("NULL")).toString());
        assertEquals("NULL", StringUtils.defaultIfBlank(new StringBuffer(" "), new StringBuffer("NULL")).toString());
        assertEquals("abc", StringUtils.defaultIfBlank(new StringBuffer("abc"), new StringBuffer("NULL")).toString());
        assertNull(StringUtils.defaultIfBlank(new StringBuffer(""), null));
        // Tests compatibility for the API return type
        final StringBuffer s = StringUtils.defaultIfBlank(new StringBuffer("abc"), new StringBuffer("NULL"));
        assertEquals("abc", s.toString());
    }

    @Test
    public void testDefaultIfBlank_StringBuilders() {
        assertEquals("NULL", StringUtils.defaultIfBlank(new StringBuilder(""), new StringBuilder("NULL")).toString());
        assertEquals("NULL", StringUtils.defaultIfBlank(new StringBuilder(" "), new StringBuilder("NULL")).toString());
        assertEquals("abc", StringUtils.defaultIfBlank(new StringBuilder("abc"), new StringBuilder("NULL")).toString());
        assertNull(StringUtils.defaultIfBlank(new StringBuilder(""), null));
        // Tests compatibility for the API return type
        final StringBuilder s = StringUtils.defaultIfBlank(new StringBuilder("abc"), new StringBuilder("NULL"));
        assertEquals("abc", s.toString());
    }

    @Test
    public void testDefaultIfBlank_StringString() {
        assertEquals("NULL", StringUtils.defaultIfBlank(null, "NULL"));
        assertEquals("NULL", StringUtils.defaultIfBlank("", "NULL"));
        assertEquals("NULL", StringUtils.defaultIfBlank(" ", "NULL"));
        assertEquals("abc", StringUtils.defaultIfBlank("abc", "NULL"));
        assertNull(StringUtils.defaultIfBlank("", null));
        // Tests compatibility for the API return type
        final String s = StringUtils.defaultIfBlank("abc", "NULL");
        assertEquals("abc", s);
    }

    @Test
    public void testDefaultIfEmpty_CharBuffers() {
        assertEquals("NULL", StringUtils.defaultIfEmpty(CharBuffer.wrap(""), CharBuffer.wrap("NULL")).toString());
        assertEquals("abc", StringUtils.defaultIfEmpty(CharBuffer.wrap("abc"), CharBuffer.wrap("NULL")).toString());
        assertNull(StringUtils.defaultIfEmpty(CharBuffer.wrap(""), null));
        // Tests compatibility for the API return type
        final CharBuffer s = StringUtils.defaultIfEmpty(CharBuffer.wrap("abc"), CharBuffer.wrap("NULL"));
        assertEquals("abc", s.toString());
    }


    @Test
    public void testDefaultIfEmpty_StringBuffers() {
        assertEquals("NULL", StringUtils.defaultIfEmpty(new StringBuffer(""), new StringBuffer("NULL")).toString());
        assertEquals("abc", StringUtils.defaultIfEmpty(new StringBuffer("abc"), new StringBuffer("NULL")).toString());
        assertNull(StringUtils.defaultIfEmpty(new StringBuffer(""), null));
        // Tests compatibility for the API return type
        final StringBuffer s = StringUtils.defaultIfEmpty(new StringBuffer("abc"), new StringBuffer("NULL"));
        assertEquals("abc", s.toString());
    }

    @Test
    public void testDefaultIfEmpty_StringBuilders() {
        assertEquals("NULL", StringUtils.defaultIfEmpty(new StringBuilder(""), new StringBuilder("NULL")).toString());
        assertEquals("abc", StringUtils.defaultIfEmpty(new StringBuilder("abc"), new StringBuilder("NULL")).toString());
        assertNull(StringUtils.defaultIfEmpty(new StringBuilder(""), null));
        // Tests compatibility for the API return type
        final StringBuilder s = StringUtils.defaultIfEmpty(new StringBuilder("abc"), new StringBuilder("NULL"));
        assertEquals("abc", s.toString());
    }

    @Test
    public void testDefaultIfEmpty_StringString() {
        assertEquals("NULL", StringUtils.defaultIfEmpty(null, "NULL"));
        assertEquals("NULL", StringUtils.defaultIfEmpty("", "NULL"));
        assertEquals("abc", StringUtils.defaultIfEmpty("abc", "NULL"));
        assertNull(StringUtils.defaultIfEmpty("", null));
        // Tests compatibility for the API return type
        final String s = StringUtils.defaultIfEmpty("abc", "NULL");
        assertEquals("abc", s);
    }

    @Test
    public void testDeleteWhitespace_String() {
        assertNull(StringUtils.deleteWhitespace(null));
        assertEquals("", StringUtils.deleteWhitespace(""));
        assertEquals("", StringUtils.deleteWhitespace("  \u000C  \t\t\u001F\n\n \u000B  "));
        assertEquals("", StringUtils.deleteWhitespace(StringUtilsTest.WHITESPACE));
        assertEquals(StringUtilsTest.NON_WHITESPACE, StringUtils.deleteWhitespace(StringUtilsTest.NON_WHITESPACE));
        // Note: u-2007 and u-000A both cause problems in the source code
        // it should ignore 2007 but delete 000A
        assertEquals("\u00A0\u202F", StringUtils.deleteWhitespace("  \u00A0  \t\t\n\n \u202F  "));
        assertEquals("\u00A0\u202F", StringUtils.deleteWhitespace("\u00A0\u202F"));
        assertEquals("test", StringUtils.deleteWhitespace("\u000Bt  \t\n\u0009e\rs\n\n   \tt"));
    }

    //-----------------------------------------------------------------------
    @Test
    public void testDifference_StringString() {
        assertNull(StringUtils.difference(null, null));
        assertEquals("", StringUtils.difference("", ""));
        assertEquals("abc", StringUtils.difference("", "abc"));
        assertEquals("", StringUtils.difference("abc", ""));
        assertEquals("i am a robot", StringUtils.difference(null, "i am a robot"));
        assertEquals("i am a machine", StringUtils.difference("i am a machine", null));
        assertEquals("robot", StringUtils.difference("i am a machine", "i am a robot"));
        assertEquals("", StringUtils.difference("abc", "abc"));
        assertEquals("you are a robot", StringUtils.difference("i am a robot", "you are a robot"));
    }

    @Test
    public void testDifferenceAt_StringArray() {
        assertEquals(-1, StringUtils.indexOfDifference((String[]) null));
        assertEquals(-1, StringUtils.indexOfDifference(new String[]{}));
        assertEquals(-1, StringUtils.indexOfDifference(new String[]{"abc"}));
        assertEquals(-1, StringUtils.indexOfDifference(new String[]{null, null}));
        assertEquals(-1, StringUtils.indexOfDifference(new String[]{"", ""}));
        assertEquals(0, StringUtils.indexOfDifference(new String[]{"", null}));
        assertEquals(0, StringUtils.indexOfDifference(new String[]{"abc", null, null}));
        assertEquals(0, StringUtils.indexOfDifference(new String[]{null, null, "abc"}));
        assertEquals(0, StringUtils.indexOfDifference(new String[]{"", "abc"}));
        assertEquals(0, StringUtils.indexOfDifference(new String[]{"abc", ""}));
        assertEquals(-1, StringUtils.indexOfDifference(new String[]{"abc", "abc"}));
        assertEquals(1, StringUtils.indexOfDifference(new String[]{"abc", "a"}));
        assertEquals(2, StringUtils.indexOfDifference(new String[]{"ab", "abxyz"}));
        assertEquals(2, StringUtils.indexOfDifference(new String[]{"abcde", "abxyz"}));
        assertEquals(0, StringUtils.indexOfDifference(new String[]{"abcde", "xyz"}));
        assertEquals(0, StringUtils.indexOfDifference(new String[]{"xyz", "abcde"}));
        assertEquals(7, StringUtils.indexOfDifference(new String[]{"i am a machine", "i am a robot"}));
    }

    @Test
    public void testDifferenceAt_StringString() {
        assertEquals(-1, StringUtils.indexOfDifference(null, null));
        assertEquals(0, StringUtils.indexOfDifference(null, "i am a robot"));
        assertEquals(-1, StringUtils.indexOfDifference("", ""));
        assertEquals(0, StringUtils.indexOfDifference("", "abc"));
        assertEquals(0, StringUtils.indexOfDifference("abc", ""));
        assertEquals(0, StringUtils.indexOfDifference("i am a machine", null));
        assertEquals(7, StringUtils.indexOfDifference("i am a machine", "i am a robot"));
        assertEquals(-1, StringUtils.indexOfDifference("foo", "foo"));
        assertEquals(0, StringUtils.indexOfDifference("i am a robot", "you are a robot"));
    }

    /**
     * A sanity check for {@link StringUtils#EMPTY}.
     */
    @Test
    public void testEMPTY() {
        assertNotNull(StringUtils.EMPTY);
        assertEquals("", StringUtils.EMPTY);
        assertEquals(0, StringUtils.EMPTY.length());
    }

    @Test
    public void testEscapeSurrogatePairs() {
        assertEquals("\uD83D\uDE30", StringEscapeUtils.escapeCsv("\uD83D\uDE30"));
        // Examples from https://en.wikipedia.org/wiki/UTF-16
        assertEquals("\uD800\uDC00", StringEscapeUtils.escapeCsv("\uD800\uDC00"));
        assertEquals("\uD834\uDD1E", StringEscapeUtils.escapeCsv("\uD834\uDD1E"));
        assertEquals("\uDBFF\uDFFD", StringEscapeUtils.escapeCsv("\uDBFF\uDFFD"));
        assertEquals("\uDBFF\uDFFD", StringEscapeUtils.escapeHtml3("\uDBFF\uDFFD"));
        assertEquals("\uDBFF\uDFFD", StringEscapeUtils.escapeHtml4("\uDBFF\uDFFD"));
        assertEquals("\uDBFF\uDFFD", StringEscapeUtils.escapeXml("\uDBFF\uDFFD"));
    }

    /**
     * Tests LANG-858.
     */
    @Test
    public void testEscapeSurrogatePairsLang858() {
        assertEquals("\\uDBFF\\uDFFD", StringEscapeUtils.escapeJava("\uDBFF\uDFFD"));       //fail LANG-858
        assertEquals("\\uDBFF\\uDFFD", StringEscapeUtils.escapeEcmaScript("\uDBFF\uDFFD")); //fail LANG-858
    }

    @Test
    public void testGetBytes_Charset() {
        assertEquals(ArrayUtils.EMPTY_BYTE_ARRAY, StringUtils.getBytes(null, (Charset) null));
        assertArrayEquals(StringUtils.EMPTY.getBytes(), StringUtils.getBytes(StringUtils.EMPTY, (Charset) null));
        assertArrayEquals(StringUtils.EMPTY.getBytes(StandardCharsets.US_ASCII),
            StringUtils.getBytes(StringUtils.EMPTY, StandardCharsets.US_ASCII));
    }

    @Test
    public void testGetBytes_String() throws UnsupportedEncodingException {
        assertEquals(ArrayUtils.EMPTY_BYTE_ARRAY, StringUtils.getBytes(null, (String) null));
        assertArrayEquals(StringUtils.EMPTY.getBytes(), StringUtils.getBytes(StringUtils.EMPTY, (String) null));
        assertArrayEquals(StringUtils.EMPTY.getBytes(StandardCharsets.US_ASCII.name()),
            StringUtils.getBytes(StringUtils.EMPTY, StandardCharsets.US_ASCII.name()));
    }

    @Test
    public void testGetCommonPrefix_StringArray() {
        assertEquals("", StringUtils.getCommonPrefix((String[]) null));
        assertEquals("", StringUtils.getCommonPrefix());
        assertEquals("abc", StringUtils.getCommonPrefix("abc"));
        assertEquals("", StringUtils.getCommonPrefix(null, null));
        assertEquals("", StringUtils.getCommonPrefix("", ""));
        assertEquals("", StringUtils.getCommonPrefix("", null));
        assertEquals("", StringUtils.getCommonPrefix("abc", null, null));
        assertEquals("", StringUtils.getCommonPrefix(null, null, "abc"));
        assertEquals("", StringUtils.getCommonPrefix("", "abc"));
        assertEquals("", StringUtils.getCommonPrefix("abc", ""));
        assertEquals("abc", StringUtils.getCommonPrefix("abc", "abc"));
        assertEquals("a", StringUtils.getCommonPrefix("abc", "a"));
        assertEquals("ab", StringUtils.getCommonPrefix("ab", "abxyz"));
        assertEquals("ab", StringUtils.getCommonPrefix("abcde", "abxyz"));
        assertEquals("", StringUtils.getCommonPrefix("abcde", "xyz"));
        assertEquals("", StringUtils.getCommonPrefix("xyz", "abcde"));
        assertEquals("i am a ", StringUtils.getCommonPrefix("i am a machine", "i am a robot"));
    }

    @Test
    public void testGetDigits() {
        assertNull(StringUtils.getDigits(null));
        assertEquals("", StringUtils.getDigits(""));
        assertEquals("", StringUtils.getDigits("abc"));
        assertEquals("1000", StringUtils.getDigits("1000$"));
        assertEquals("12345", StringUtils.getDigits("123password45"));
        assertEquals("5417543010", StringUtils.getDigits("(541) 754-3010"));
        assertEquals("\u0967\u0968\u0969", StringUtils.getDigits("\u0967\u0968\u0969"));
    }

    @Test
    public void testGetFuzzyDistance() {
        assertEquals(0, StringUtils.getFuzzyDistance("", "", Locale.ENGLISH));
        assertEquals(0, StringUtils.getFuzzyDistance("Workshop", "b", Locale.ENGLISH));
        assertEquals(1, StringUtils.getFuzzyDistance("Room", "o", Locale.ENGLISH));
        assertEquals(1, StringUtils.getFuzzyDistance("Workshop", "w", Locale.ENGLISH));
        assertEquals(2, StringUtils.getFuzzyDistance("Workshop", "ws", Locale.ENGLISH));
        assertEquals(4, StringUtils.getFuzzyDistance("Workshop", "wo", Locale.ENGLISH));
        assertEquals(3, StringUtils.getFuzzyDistance("Apache Software Foundation", "asf", Locale.ENGLISH));
    }

    @Test
    public void testGetFuzzyDistance_NullNullNull() {
        assertThrows(IllegalArgumentException.class, () -> StringUtils.getFuzzyDistance(null, null, null));
    }

    @Test
    public void testGetFuzzyDistance_NullStringLocale() {
        assertThrows(IllegalArgumentException.class, () -> StringUtils.getFuzzyDistance(null, "clear", Locale.ENGLISH));
    }

    @Test
    public void testGetFuzzyDistance_StringNullLoclae() {
        assertThrows(IllegalArgumentException.class, () -> StringUtils.getFuzzyDistance(" ", null, Locale.ENGLISH));
    }

    @Test
    public void testGetFuzzyDistance_StringStringNull() {
        assertThrows(IllegalArgumentException.class, () -> StringUtils.getFuzzyDistance(" ", "clear", null));
    }

    @Test
    public void testGetJaroWinklerDistance_NullNull() {
        assertThrows(IllegalArgumentException.class, () -> StringUtils.getJaroWinklerDistance(null, null));
    }

    @Test
    public void testGetJaroWinklerDistance_NullString() {
        assertThrows(IllegalArgumentException.class, () -> StringUtils.getJaroWinklerDistance(null, "clear"));
    }

    @Test
    public void testGetJaroWinklerDistance_StringNull() {
        assertThrows(IllegalArgumentException.class, () -> StringUtils.getJaroWinklerDistance(" ", null));
    }

    @Test
    public void testGetJaroWinklerDistance_StringString() {
        assertEquals(0.93d, StringUtils.getJaroWinklerDistance("frog", "fog"));
        assertEquals(0.0d, StringUtils.getJaroWinklerDistance("fly", "ant"));
        assertEquals(0.44d, StringUtils.getJaroWinklerDistance("elephant", "hippo"));
        assertEquals(0.84d, StringUtils.getJaroWinklerDistance("dwayne", "duane"));
        assertEquals(0.93d, StringUtils.getJaroWinklerDistance("ABC Corporation", "ABC Corp"));
        assertEquals(0.95d, StringUtils.getJaroWinklerDistance("D N H Enterprises Inc", "D & H Enterprises, Inc."));
        assertEquals(0.92d, StringUtils.getJaroWinklerDistance("My Gym Children's Fitness Center", "My Gym. Childrens Fitness"));
        assertEquals(0.88d, StringUtils.getJaroWinklerDistance("PENNSYLVANIA", "PENNCISYLVNIA"));
        assertEquals(0.63d, StringUtils.getJaroWinklerDistance("Haus Ingeborg", "Ingeborg Esser"));
    }

    @Test
    public void testGetLevenshteinDistance_NullString() {
        assertThrows(IllegalArgumentException.class, () -> StringUtils.getLevenshteinDistance("a", null));
    }

    @Test
    public void testGetLevenshteinDistance_NullStringInt() {
        assertThrows(IllegalArgumentException.class, () -> StringUtils.getLevenshteinDistance(null, "a", 0));
    }

    @Test
    public void testGetLevenshteinDistance_StringNull() {
        assertThrows(IllegalArgumentException.class, () -> StringUtils.getLevenshteinDistance(null, "a"));
    }

    @Test
    public void testGetLevenshteinDistance_StringNullInt() {
        assertThrows(IllegalArgumentException.class, () -> StringUtils.getLevenshteinDistance("a", null, 0));
    }

    //-----------------------------------------------------------------------
    @Test
    public void testGetLevenshteinDistance_StringString() {
        assertEquals(0, StringUtils.getLevenshteinDistance("", ""));
        assertEquals(1, StringUtils.getLevenshteinDistance("", "a"));
        assertEquals(7, StringUtils.getLevenshteinDistance("aaapppp", ""));
        assertEquals(1, StringUtils.getLevenshteinDistance("frog", "fog"));
        assertEquals(3, StringUtils.getLevenshteinDistance("fly", "ant"));
        assertEquals(7, StringUtils.getLevenshteinDistance("elephant", "hippo"));
        assertEquals(7, StringUtils.getLevenshteinDistance("hippo", "elephant"));
        assertEquals(8, StringUtils.getLevenshteinDistance("hippo", "zzzzzzzz"));
        assertEquals(8, StringUtils.getLevenshteinDistance("zzzzzzzz", "hippo"));
        assertEquals(1, StringUtils.getLevenshteinDistance("hello", "hallo"));
    }

    @Test
    public void testGetLevenshteinDistance_StringStringInt() {
        // empty strings
        assertEquals(0, StringUtils.getLevenshteinDistance("", "", 0));
        assertEquals(7, StringUtils.getLevenshteinDistance("aaapppp", "", 8));
        assertEquals(7, StringUtils.getLevenshteinDistance("aaapppp", "", 7));
        assertEquals(-1, StringUtils.getLevenshteinDistance("aaapppp", "", 6));

        // unequal strings, zero threshold
        assertEquals(-1, StringUtils.getLevenshteinDistance("b", "a", 0));
        assertEquals(-1, StringUtils.getLevenshteinDistance("a", "b", 0));

        // equal strings
        assertEquals(0, StringUtils.getLevenshteinDistance("aa", "aa", 0));
        assertEquals(0, StringUtils.getLevenshteinDistance("aa", "aa", 2));

        // same length
        assertEquals(-1, StringUtils.getLevenshteinDistance("aaa", "bbb", 2));
        assertEquals(3, StringUtils.getLevenshteinDistance("aaa", "bbb", 3));

        // big stripe
        assertEquals(6, StringUtils.getLevenshteinDistance("aaaaaa", "b", 10));

        // distance less than threshold
        assertEquals(7, StringUtils.getLevenshteinDistance("aaapppp", "b", 8));
        assertEquals(3, StringUtils.getLevenshteinDistance("a", "bbb", 4));

        // distance equal to threshold
        assertEquals(7, StringUtils.getLevenshteinDistance("aaapppp", "b", 7));
        assertEquals(3, StringUtils.getLevenshteinDistance("a", "bbb", 3));

        // distance greater than threshold
        assertEquals(-1, StringUtils.getLevenshteinDistance("a", "bbb", 2));
        assertEquals(-1, StringUtils.getLevenshteinDistance("bbb", "a", 2));
        assertEquals(-1, StringUtils.getLevenshteinDistance("aaapppp", "b", 6));

        // stripe runs off array, strings not similar
        assertEquals(-1, StringUtils.getLevenshteinDistance("a", "bbb", 1));
        assertEquals(-1, StringUtils.getLevenshteinDistance("bbb", "a", 1));

        // stripe runs off array, strings are similar
        assertEquals(-1, StringUtils.getLevenshteinDistance("12345", "1234567", 1));
        assertEquals(-1, StringUtils.getLevenshteinDistance("1234567", "12345", 1));

        // old getLevenshteinDistance test cases
        assertEquals(1, StringUtils.getLevenshteinDistance("frog", "fog", 1));
        assertEquals(3, StringUtils.getLevenshteinDistance("fly", "ant", 3));
        assertEquals(7, StringUtils.getLevenshteinDistance("elephant", "hippo", 7));
        assertEquals(-1, StringUtils.getLevenshteinDistance("elephant", "hippo", 6));
        assertEquals(7, StringUtils.getLevenshteinDistance("hippo", "elephant", 7));
        assertEquals(-1, StringUtils.getLevenshteinDistance("hippo", "elephant", 6));
        assertEquals(8, StringUtils.getLevenshteinDistance("hippo", "zzzzzzzz", 8));
        assertEquals(8, StringUtils.getLevenshteinDistance("zzzzzzzz", "hippo", 8));
        assertEquals(1, StringUtils.getLevenshteinDistance("hello", "hallo", 1));

        assertEquals(1, StringUtils.getLevenshteinDistance("frog", "fog", Integer.MAX_VALUE));
        assertEquals(3, StringUtils.getLevenshteinDistance("fly", "ant", Integer.MAX_VALUE));
        assertEquals(7, StringUtils.getLevenshteinDistance("elephant", "hippo", Integer.MAX_VALUE));
        assertEquals(7, StringUtils.getLevenshteinDistance("hippo", "elephant", Integer.MAX_VALUE));
        assertEquals(8, StringUtils.getLevenshteinDistance("hippo", "zzzzzzzz", Integer.MAX_VALUE));
        assertEquals(8, StringUtils.getLevenshteinDistance("zzzzzzzz", "hippo", Integer.MAX_VALUE));
        assertEquals(1, StringUtils.getLevenshteinDistance("hello", "hallo", Integer.MAX_VALUE));
    }

    @Test
    public void testGetLevenshteinDistance_StringStringNegativeInt() {
        assertThrows(IllegalArgumentException.class, () -> StringUtils.getLevenshteinDistance("a", "a", -1));
    }

    /**
     * Test for {@link StringUtils#isAllLowerCase(CharSequence)}.
     */
    @Test
    public void testIsAllLowerCase() {
        assertFalse(StringUtils.isAllLowerCase(null));
        assertFalse(StringUtils.isAllLowerCase(StringUtils.EMPTY));
        assertFalse(StringUtils.isAllLowerCase("  "));
        assertTrue(StringUtils.isAllLowerCase("abc"));
        assertFalse(StringUtils.isAllLowerCase("abc "));
        assertFalse(StringUtils.isAllLowerCase("abc\n"));
        assertFalse(StringUtils.isAllLowerCase("abC"));
        assertFalse(StringUtils.isAllLowerCase("ab c"));
        assertFalse(StringUtils.isAllLowerCase("ab1c"));
        assertFalse(StringUtils.isAllLowerCase("ab/c"));
    }

    /**
     * Test for {@link StringUtils#isAllUpperCase(CharSequence)}.
     */
    @Test
    public void testIsAllUpperCase() {
        assertFalse(StringUtils.isAllUpperCase(null));
        assertFalse(StringUtils.isAllUpperCase(StringUtils.EMPTY));
        assertFalse(StringUtils.isAllUpperCase("  "));
        assertTrue(StringUtils.isAllUpperCase("ABC"));
        assertFalse(StringUtils.isAllUpperCase("ABC "));
        assertFalse(StringUtils.isAllUpperCase("ABC\n"));
        assertFalse(StringUtils.isAllUpperCase("aBC"));
        assertFalse(StringUtils.isAllUpperCase("A C"));
        assertFalse(StringUtils.isAllUpperCase("A1C"));
        assertFalse(StringUtils.isAllUpperCase("A/C"));
    }

    /**
     * Test for {@link StringUtils#isMixedCase(CharSequence)}.
     */
    @Test
    public void testIsMixedCase() {
        assertFalse(StringUtils.isMixedCase(null));
        assertFalse(StringUtils.isMixedCase(StringUtils.EMPTY));
        assertFalse(StringUtils.isMixedCase(" "));
        assertFalse(StringUtils.isMixedCase("A"));
        assertFalse(StringUtils.isMixedCase("a"));
        assertFalse(StringUtils.isMixedCase("/"));
        assertFalse(StringUtils.isMixedCase("A/"));
        assertFalse(StringUtils.isMixedCase("/b"));
        assertFalse(StringUtils.isMixedCase("abc"));
        assertFalse(StringUtils.isMixedCase("ABC"));
        assertTrue(StringUtils.isMixedCase("aBc"));
        assertTrue(StringUtils.isMixedCase("aBc "));
        assertTrue(StringUtils.isMixedCase("A c"));
        assertTrue(StringUtils.isMixedCase("aBc\n"));
        assertTrue(StringUtils.isMixedCase("A1c"));
        assertTrue(StringUtils.isMixedCase("a/C"));
    }

    @Test
    public void testJoin_ArrayCharSeparator() {
        assertNull(StringUtils.join((Object[]) null, ','));
        assertEquals(TEXT_LIST_CHAR, StringUtils.join(ARRAY_LIST, SEPARATOR_CHAR));
        assertEquals("", StringUtils.join(EMPTY_ARRAY_LIST, SEPARATOR_CHAR));
        assertEquals(";;foo", StringUtils.join(MIXED_ARRAY_LIST, SEPARATOR_CHAR));
        assertEquals("foo;2", StringUtils.join(MIXED_TYPE_LIST, SEPARATOR_CHAR));

        assertNull(StringUtils.join((Object[]) null, ',', 0, 1));
        assertEquals("/", StringUtils.join(MIXED_ARRAY_LIST, '/', 0, MIXED_ARRAY_LIST.length - 1));
        assertEquals("foo", StringUtils.join(MIXED_TYPE_LIST, '/', 0, 1));
        assertEquals("null", StringUtils.join(NULL_TO_STRING_LIST, '/', 0, 1));
        assertEquals("foo/2", StringUtils.join(MIXED_TYPE_LIST, '/', 0, 2));
        assertEquals("2", StringUtils.join(MIXED_TYPE_LIST, '/', 1, 2));
        assertEquals("", StringUtils.join(MIXED_TYPE_LIST, '/', 2, 1));
    }

    @Test
    public void testJoin_ArrayOfBytes() {
        assertNull(StringUtils.join((byte[]) null, ','));
        assertEquals("1;2", StringUtils.join(BYTE_PRIM_LIST, SEPARATOR_CHAR));
        assertEquals("2", StringUtils.join(BYTE_PRIM_LIST, SEPARATOR_CHAR, 1, 2));
        assertNull(StringUtils.join((byte[]) null, SEPARATOR_CHAR, 0, 1));
        assertEquals(StringUtils.EMPTY, StringUtils.join(BYTE_PRIM_LIST, SEPARATOR_CHAR, 0, 0));
        assertEquals(StringUtils.EMPTY, StringUtils.join(BYTE_PRIM_LIST, SEPARATOR_CHAR, 1, 0));
    }

    @Test
    public void testJoin_ArrayOfChars() {
        assertNull(StringUtils.join((char[]) null, ','));
        assertEquals("1;2", StringUtils.join(CHAR_PRIM_LIST, SEPARATOR_CHAR));
        assertEquals("2", StringUtils.join(CHAR_PRIM_LIST, SEPARATOR_CHAR, 1, 2));
        assertNull(StringUtils.join((char[]) null, SEPARATOR_CHAR, 0, 1));
        assertEquals(StringUtils.EMPTY, StringUtils.join(CHAR_PRIM_LIST, SEPARATOR_CHAR, 0, 0));
        assertEquals(StringUtils.EMPTY, StringUtils.join(CHAR_PRIM_LIST, SEPARATOR_CHAR, 1, 0));
    }

    @Test
    public void testJoin_ArrayOfDoubles() {
        assertNull(StringUtils.join((double[]) null, ','));
        assertEquals("1.0;2.0", StringUtils.join(DOUBLE_PRIM_LIST, SEPARATOR_CHAR));
        assertEquals("2.0", StringUtils.join(DOUBLE_PRIM_LIST, SEPARATOR_CHAR, 1, 2));
        assertNull(StringUtils.join((double[]) null, SEPARATOR_CHAR, 0, 1));
        assertEquals(StringUtils.EMPTY, StringUtils.join(DOUBLE_PRIM_LIST, SEPARATOR_CHAR, 0, 0));
        assertEquals(StringUtils.EMPTY, StringUtils.join(DOUBLE_PRIM_LIST, SEPARATOR_CHAR, 1, 0));
    }

    @Test
    public void testJoin_ArrayOfFloats() {
        assertNull(StringUtils.join((float[]) null, ','));
        assertEquals("1.0;2.0", StringUtils.join(FLOAT_PRIM_LIST, SEPARATOR_CHAR));
        assertEquals("2.0", StringUtils.join(FLOAT_PRIM_LIST, SEPARATOR_CHAR, 1, 2));
        assertNull(StringUtils.join((float[]) null, SEPARATOR_CHAR, 0, 1));
        assertEquals(StringUtils.EMPTY, StringUtils.join(FLOAT_PRIM_LIST, SEPARATOR_CHAR, 0, 0));
        assertEquals(StringUtils.EMPTY, StringUtils.join(FLOAT_PRIM_LIST, SEPARATOR_CHAR, 1, 0));
    }

    @Test
    public void testJoin_ArrayOfInts() {
        assertNull(StringUtils.join((int[]) null, ','));
        assertEquals("1;2", StringUtils.join(INT_PRIM_LIST, SEPARATOR_CHAR));
        assertEquals("2", StringUtils.join(INT_PRIM_LIST, SEPARATOR_CHAR, 1, 2));
        assertNull(StringUtils.join((int[]) null, SEPARATOR_CHAR, 0, 1));
        assertEquals(StringUtils.EMPTY, StringUtils.join(INT_PRIM_LIST, SEPARATOR_CHAR, 0, 0));
        assertEquals(StringUtils.EMPTY, StringUtils.join(INT_PRIM_LIST, SEPARATOR_CHAR, 1, 0));
    }

    @Test
    public void testJoin_ArrayOfLongs() {
        assertNull(StringUtils.join((long[]) null, ','));
        assertEquals("1;2", StringUtils.join(LONG_PRIM_LIST, SEPARATOR_CHAR));
        assertEquals("2", StringUtils.join(LONG_PRIM_LIST, SEPARATOR_CHAR, 1, 2));
        assertNull(StringUtils.join((long[]) null, SEPARATOR_CHAR, 0, 1));
        assertEquals(StringUtils.EMPTY, StringUtils.join(LONG_PRIM_LIST, SEPARATOR_CHAR, 0, 0));
        assertEquals(StringUtils.EMPTY, StringUtils.join(LONG_PRIM_LIST, SEPARATOR_CHAR, 1, 0));
    }

    @Test
    public void testJoin_ArrayOfShorts() {
        assertNull(StringUtils.join((short[]) null, ','));
        assertEquals("1;2", StringUtils.join(SHORT_PRIM_LIST, SEPARATOR_CHAR));
        assertEquals("2", StringUtils.join(SHORT_PRIM_LIST, SEPARATOR_CHAR, 1, 2));
        assertNull(StringUtils.join((short[]) null, SEPARATOR_CHAR, 0, 1));
        assertEquals(StringUtils.EMPTY, StringUtils.join(SHORT_PRIM_LIST, SEPARATOR_CHAR, 0, 0));
        assertEquals(StringUtils.EMPTY, StringUtils.join(SHORT_PRIM_LIST, SEPARATOR_CHAR, 1, 0));
    }

    @Test
    public void testJoin_ArrayString() {
        assertNull(StringUtils.join((Object[]) null, null));
        assertEquals(TEXT_LIST_NOSEP, StringUtils.join(ARRAY_LIST, null));
        assertEquals(TEXT_LIST_NOSEP, StringUtils.join(ARRAY_LIST, ""));

        assertEquals("", StringUtils.join(NULL_ARRAY_LIST, null));

        assertEquals("", StringUtils.join(EMPTY_ARRAY_LIST, null));
        assertEquals("", StringUtils.join(EMPTY_ARRAY_LIST, ""));
        assertEquals("", StringUtils.join(EMPTY_ARRAY_LIST, SEPARATOR));

        assertEquals(TEXT_LIST, StringUtils.join(ARRAY_LIST, SEPARATOR));
        assertEquals(",,foo", StringUtils.join(MIXED_ARRAY_LIST, SEPARATOR));
        assertEquals("foo,2", StringUtils.join(MIXED_TYPE_LIST, SEPARATOR));

        assertEquals("/", StringUtils.join(MIXED_ARRAY_LIST, "/", 0, MIXED_ARRAY_LIST.length - 1));
        assertEquals("", StringUtils.join(MIXED_ARRAY_LIST, "", 0, MIXED_ARRAY_LIST.length - 1));
        assertEquals("foo", StringUtils.join(MIXED_TYPE_LIST, "/", 0, 1));
        assertEquals("foo/2", StringUtils.join(MIXED_TYPE_LIST, "/", 0, 2));
        assertEquals("2", StringUtils.join(MIXED_TYPE_LIST, "/", 1, 2));
        assertEquals("", StringUtils.join(MIXED_TYPE_LIST, "/", 2, 1));
    }

    @Test
    public void testJoin_IterableChar() {
        assertNull(StringUtils.join((Iterable<?>) null, ','));
        assertEquals(TEXT_LIST_CHAR, StringUtils.join(Arrays.asList(ARRAY_LIST), SEPARATOR_CHAR));
        assertEquals("", StringUtils.join(Arrays.asList(NULL_ARRAY_LIST), SEPARATOR_CHAR));
        assertEquals("", StringUtils.join(Arrays.asList(EMPTY_ARRAY_LIST), SEPARATOR_CHAR));
        assertEquals("foo", StringUtils.join(Collections.singleton("foo"), 'x'));
    }

    @Test
    public void testJoin_IterableString() {
        assertNull(StringUtils.join((Iterable<?>) null, null));
        assertEquals(TEXT_LIST_NOSEP, StringUtils.join(Arrays.asList(ARRAY_LIST), null));
        assertEquals(TEXT_LIST_NOSEP, StringUtils.join(Arrays.asList(ARRAY_LIST), ""));
        assertEquals("foo", StringUtils.join(Collections.singleton("foo"), "x"));
        assertEquals("foo", StringUtils.join(Collections.singleton("foo"), null));

        assertEquals("", StringUtils.join(Arrays.asList(NULL_ARRAY_LIST), null));

        assertEquals("", StringUtils.join(Arrays.asList(EMPTY_ARRAY_LIST), null));
        assertEquals("", StringUtils.join(Arrays.asList(EMPTY_ARRAY_LIST), ""));
        assertEquals("", StringUtils.join(Arrays.asList(EMPTY_ARRAY_LIST), SEPARATOR));

        assertEquals(TEXT_LIST, StringUtils.join(Arrays.asList(ARRAY_LIST), SEPARATOR));
    }

    @Test
    public void testJoin_IteratorChar() {
        assertNull(StringUtils.join((Iterator<?>) null, ','));
        assertEquals(TEXT_LIST_CHAR, StringUtils.join(Arrays.asList(ARRAY_LIST).iterator(), SEPARATOR_CHAR));
        assertEquals("", StringUtils.join(Arrays.asList(NULL_ARRAY_LIST).iterator(), SEPARATOR_CHAR));
        assertEquals("", StringUtils.join(Arrays.asList(EMPTY_ARRAY_LIST).iterator(), SEPARATOR_CHAR));
        assertEquals("foo", StringUtils.join(Collections.singleton("foo").iterator(), 'x'));
    }

    @Test
    public void testJoin_IteratorString() {
        assertNull(StringUtils.join((Iterator<?>) null, null));
        assertEquals(TEXT_LIST_NOSEP, StringUtils.join(Arrays.asList(ARRAY_LIST).iterator(), null));
        assertEquals(TEXT_LIST_NOSEP, StringUtils.join(Arrays.asList(ARRAY_LIST).iterator(), ""));
        assertEquals("foo", StringUtils.join(Collections.singleton("foo").iterator(), "x"));
        assertEquals("foo", StringUtils.join(Collections.singleton("foo").iterator(), null));

        assertEquals("", StringUtils.join(Arrays.asList(NULL_ARRAY_LIST).iterator(), null));

        assertEquals("", StringUtils.join(Arrays.asList(EMPTY_ARRAY_LIST).iterator(), null));
        assertEquals("", StringUtils.join(Arrays.asList(EMPTY_ARRAY_LIST).iterator(), ""));
        assertEquals("", StringUtils.join(Arrays.asList(EMPTY_ARRAY_LIST).iterator(), SEPARATOR));

        assertEquals(TEXT_LIST, StringUtils.join(Arrays.asList(ARRAY_LIST).iterator(), SEPARATOR));

        assertNull(StringUtils.join(Arrays.asList(NULL_TO_STRING_LIST).iterator(), SEPARATOR));
    }

    @Test
    public void testJoin_List() {
        assertNull(StringUtils.join((List<String>) null, null));
        assertEquals(TEXT_LIST_NOSEP, StringUtils.join(STRING_LIST, null));
        assertEquals(TEXT_LIST_NOSEP, StringUtils.join(STRING_LIST, ""));

        assertEquals("", StringUtils.join(NULL_STRING_LIST, null));

        assertEquals("", StringUtils.join(EMPTY_STRING_LIST, null));
        assertEquals("", StringUtils.join(EMPTY_STRING_LIST, ""));
        assertEquals("", StringUtils.join(EMPTY_STRING_LIST, SEPARATOR));

        assertEquals(TEXT_LIST, StringUtils.join(STRING_LIST, SEPARATOR));
        assertEquals(",,foo", StringUtils.join(MIXED_STRING_LIST, SEPARATOR));
        assertEquals("foo,2", StringUtils.join(MIXED_TYPE_OBJECT_LIST, SEPARATOR));

        assertEquals("/", StringUtils.join(MIXED_STRING_LIST, "/", 0, MIXED_STRING_LIST.size() - 1));
        assertEquals("", StringUtils.join(MIXED_STRING_LIST, "", 0, MIXED_STRING_LIST.size()- 1));
        assertEquals("foo", StringUtils.join(MIXED_TYPE_OBJECT_LIST, "/", 0, 1));
        assertEquals("foo/2", StringUtils.join(MIXED_TYPE_OBJECT_LIST, "/", 0, 2));
        assertEquals("2", StringUtils.join(MIXED_TYPE_OBJECT_LIST, "/", 1, 2));
        assertEquals("", StringUtils.join(MIXED_TYPE_OBJECT_LIST, "/", 2, 1));
        assertNull(null, StringUtils.join((List<?>) null, "/", 0, 1));

        assertEquals("/", StringUtils.join(MIXED_STRING_LIST, '/', 0, MIXED_STRING_LIST.size() - 1));
        assertEquals("foo", StringUtils.join(MIXED_TYPE_OBJECT_LIST, '/', 0, 1));
        assertEquals("foo/2", StringUtils.join(MIXED_TYPE_OBJECT_LIST, '/', 0, 2));
        assertEquals("2", StringUtils.join(MIXED_TYPE_OBJECT_LIST, '/', 1, 2));
        assertEquals("", StringUtils.join(MIXED_TYPE_OBJECT_LIST, '/', 2, 1));
        assertNull(null, StringUtils.join((List<?>) null, '/', 0, 1));
    }

    @Test
    public void testJoin_Objectarray() {
//        assertNull(StringUtils.join(null)); // generates warning
        assertNull(StringUtils.join((Object[]) null)); // equivalent explicit cast
        // test additional varargs calls
        assertEquals("", StringUtils.join()); // empty array
        assertEquals("", StringUtils.join((Object) null)); // => new Object[]{null}

        assertEquals("", StringUtils.join(EMPTY_ARRAY_LIST));
        assertEquals("", StringUtils.join(NULL_ARRAY_LIST));
        assertEquals("null", StringUtils.join(NULL_TO_STRING_LIST));
        assertEquals("abc", StringUtils.join("a", "b", "c"));
        assertEquals("a", StringUtils.join(null, "a", ""));
        assertEquals("foo", StringUtils.join(MIXED_ARRAY_LIST));
        assertEquals("foo2", StringUtils.join(MIXED_TYPE_LIST));
    }

    //-----------------------------------------------------------------------
    @Test
    public void testJoin_Objects() {
        assertEquals("abc", StringUtils.join("a", "b", "c"));
        assertEquals("a", StringUtils.join(null, "", "a"));
        assertNull(StringUtils.join((Object[]) null));
    }

    @Test
    public void testJoinWith() {
        assertEquals("", StringUtils.joinWith(","));        // empty array
        assertEquals("", StringUtils.joinWith(",", (Object[]) NULL_ARRAY_LIST));
        assertEquals("null", StringUtils.joinWith(",", NULL_TO_STRING_LIST));   //toString method prints 'null'

        assertEquals("a,b,c", StringUtils.joinWith(",", "a", "b", "c"));
        assertEquals(",a,", StringUtils.joinWith(",", null, "a", ""));

        assertEquals("ab", StringUtils.joinWith(null, "a", "b"));
    }

    @Test
    public void testJoinWithThrowsException() {
        assertThrows(IllegalArgumentException.class, () -> StringUtils.joinWith(",", (Object[]) null));
    }

    @Test
    public void testLang623() {
        assertEquals("t", StringUtils.replaceChars("\u00DE", '\u00DE', 't'));
        assertEquals("t", StringUtils.replaceChars("\u00FE", '\u00FE', 't'));
    }

    @Test
    public void testLANG666() {
        assertEquals("12", StringUtils.stripEnd("120.00", ".0"));
        assertEquals("121", StringUtils.stripEnd("121.00", ".0"));
    }

    //-----------------------------------------------------------------------
    @Test
    public void testLeftPad_StringInt() {
        assertNull(StringUtils.leftPad(null, 5));
        assertEquals("     ", StringUtils.leftPad("", 5));
        assertEquals("  abc", StringUtils.leftPad("abc", 5));
        assertEquals("abc", StringUtils.leftPad("abc", 2));
    }

    @Test
    public void testLeftPad_StringIntChar() {
        assertNull(StringUtils.leftPad(null, 5, ' '));
        assertEquals("     ", StringUtils.leftPad("", 5, ' '));
        assertEquals("  abc", StringUtils.leftPad("abc", 5, ' '));
        assertEquals("xxabc", StringUtils.leftPad("abc", 5, 'x'));
        assertEquals("\uffff\uffffabc", StringUtils.leftPad("abc", 5, '\uffff'));
        assertEquals("abc", StringUtils.leftPad("abc", 2, ' '));
        final String str = StringUtils.leftPad("aaa", 10000, 'a');  // bigger than pad length
        assertEquals(10000, str.length());
        assertTrue(StringUtils.containsOnly(str, 'a'));
    }

    @Test
    public void testLeftPad_StringIntString() {
        assertNull(StringUtils.leftPad(null, 5, "-+"));
        assertNull(StringUtils.leftPad(null, 5, null));
        assertEquals("     ", StringUtils.leftPad("", 5, " "));
        assertEquals("-+-+abc", StringUtils.leftPad("abc", 7, "-+"));
        assertEquals("-+~abc", StringUtils.leftPad("abc", 6, "-+~"));
        assertEquals("-+abc", StringUtils.leftPad("abc", 5, "-+~"));
        assertEquals("abc", StringUtils.leftPad("abc", 2, " "));
        assertEquals("abc", StringUtils.leftPad("abc", -1, " "));
        assertEquals("  abc", StringUtils.leftPad("abc", 5, null));
        assertEquals("  abc", StringUtils.leftPad("abc", 5, ""));
    }

    @Test
    public void testLength_CharBuffer() {
        assertEquals(0, StringUtils.length(CharBuffer.wrap("")));
        assertEquals(1, StringUtils.length(CharBuffer.wrap("A")));
        assertEquals(1, StringUtils.length(CharBuffer.wrap(" ")));
        assertEquals(8, StringUtils.length(CharBuffer.wrap("ABCDEFGH")));
    }

    @Test
    public void testLengthString() {
        assertEquals(0, StringUtils.length(null));
        assertEquals(0, StringUtils.length(""));
        assertEquals(0, StringUtils.length(StringUtils.EMPTY));
        assertEquals(1, StringUtils.length("A"));
        assertEquals(1, StringUtils.length(" "));
        assertEquals(8, StringUtils.length("ABCDEFGH"));
    }

    @Test
    public void testLengthStringBuffer() {
        assertEquals(0, StringUtils.length(new StringBuffer("")));
        assertEquals(0, StringUtils.length(new StringBuffer(StringUtils.EMPTY)));
        assertEquals(1, StringUtils.length(new StringBuffer("A")));
        assertEquals(1, StringUtils.length(new StringBuffer(" ")));
        assertEquals(8, StringUtils.length(new StringBuffer("ABCDEFGH")));
    }

    @Test
    public void testLengthStringBuilder() {
        assertEquals(0, StringUtils.length(new StringBuilder("")));
        assertEquals(0, StringUtils.length(new StringBuilder(StringUtils.EMPTY)));
        assertEquals(1, StringUtils.length(new StringBuilder("A")));
        assertEquals(1, StringUtils.length(new StringBuilder(" ")));
        assertEquals(8, StringUtils.length(new StringBuilder("ABCDEFGH")));
    }

    @Test
    public void testLowerCase() {
        assertNull(StringUtils.lowerCase(null));
        assertNull(StringUtils.lowerCase(null, Locale.ENGLISH));
        assertEquals("foo test thing", StringUtils.lowerCase("fOo test THING"), "lowerCase(String) failed");
        assertEquals("", StringUtils.lowerCase(""), "lowerCase(empty-string) failed");
        assertEquals("foo test thing", StringUtils.lowerCase("fOo test THING", Locale.ENGLISH),
                "lowerCase(String, Locale) failed");
        assertEquals("", StringUtils.lowerCase("", Locale.ENGLISH), "lowerCase(empty-string, Locale) failed");
    }

    @Test
    public void testNormalizeSpace() {
        // Java says a non-breaking whitespace is not a whitespace.
        assertFalse(Character.isWhitespace('\u00A0'));
        //
        assertNull(StringUtils.normalizeSpace(null));
        assertEquals("", StringUtils.normalizeSpace(""));
        assertEquals("", StringUtils.normalizeSpace(" "));
        assertEquals("", StringUtils.normalizeSpace("\t"));
        assertEquals("", StringUtils.normalizeSpace("\n"));
        assertEquals("", StringUtils.normalizeSpace("\u0009"));
        assertEquals("", StringUtils.normalizeSpace("\u000B"));
        assertEquals("", StringUtils.normalizeSpace("\u000C"));
        assertEquals("", StringUtils.normalizeSpace("\u001C"));
        assertEquals("", StringUtils.normalizeSpace("\u001D"));
        assertEquals("", StringUtils.normalizeSpace("\u001E"));
        assertEquals("", StringUtils.normalizeSpace("\u001F"));
        assertEquals("", StringUtils.normalizeSpace("\f"));
        assertEquals("", StringUtils.normalizeSpace("\r"));
        assertEquals("a", StringUtils.normalizeSpace("  a  "));
        assertEquals("a b c", StringUtils.normalizeSpace("  a  b   c  "));
        assertEquals("a b c", StringUtils.normalizeSpace("a\t\f\r  b\u000B   c\n"));
        assertEquals("a   b c", StringUtils.normalizeSpace("a\t\f\r  " + HARD_SPACE + HARD_SPACE + "b\u000B   c\n"));
        assertEquals("b", StringUtils.normalizeSpace("\u0000b"));
        assertEquals("b", StringUtils.normalizeSpace("b\u0000"));
    }

    @Test
    public void testOverlay_StringStringIntInt() {
        assertNull(StringUtils.overlay(null, null, 2, 4));
        assertNull(StringUtils.overlay(null, null, -2, -4));

        assertEquals("", StringUtils.overlay("", null, 0, 0));
        assertEquals("", StringUtils.overlay("", "", 0, 0));
        assertEquals("zzzz", StringUtils.overlay("", "zzzz", 0, 0));
        assertEquals("zzzz", StringUtils.overlay("", "zzzz", 2, 4));
        assertEquals("zzzz", StringUtils.overlay("", "zzzz", -2, -4));

        assertEquals("abef", StringUtils.overlay("abcdef", null, 2, 4));
        assertEquals("abef", StringUtils.overlay("abcdef", null, 4, 2));
        assertEquals("abef", StringUtils.overlay("abcdef", "", 2, 4));
        assertEquals("abef", StringUtils.overlay("abcdef", "", 4, 2));
        assertEquals("abzzzzef", StringUtils.overlay("abcdef", "zzzz", 2, 4));
        assertEquals("abzzzzef", StringUtils.overlay("abcdef", "zzzz", 4, 2));

        assertEquals("zzzzef", StringUtils.overlay("abcdef", "zzzz", -1, 4));
        assertEquals("zzzzef", StringUtils.overlay("abcdef", "zzzz", 4, -1));
        assertEquals("zzzzabcdef", StringUtils.overlay("abcdef", "zzzz", -2, -1));
        assertEquals("zzzzabcdef", StringUtils.overlay("abcdef", "zzzz", -1, -2));
        assertEquals("abcdzzzz", StringUtils.overlay("abcdef", "zzzz", 4, 10));
        assertEquals("abcdzzzz", StringUtils.overlay("abcdef", "zzzz", 10, 4));
        assertEquals("abcdefzzzz", StringUtils.overlay("abcdef", "zzzz", 8, 10));
        assertEquals("abcdefzzzz", StringUtils.overlay("abcdef", "zzzz", 10, 8));
    }

    /**
     * Tests {@code prependIfMissing}.
     */
    @Test
    public void testPrependIfMissing() {
        assertNull(StringUtils.prependIfMissing(null, null), "prependIfMissing(null,null)");
        assertEquals("abc", StringUtils.prependIfMissing("abc", null), "prependIfMissing(abc,null)");
        assertEquals("xyz", StringUtils.prependIfMissing("", "xyz"), "prependIfMissing(\"\",xyz)");
        assertEquals("xyzabc", StringUtils.prependIfMissing("abc", "xyz"), "prependIfMissing(abc,xyz)");
        assertEquals("xyzabc", StringUtils.prependIfMissing("xyzabc", "xyz"), "prependIfMissing(xyzabc,xyz)");
        assertEquals("xyzXYZabc", StringUtils.prependIfMissing("XYZabc", "xyz"), "prependIfMissing(XYZabc,xyz)");

        assertNull(StringUtils.prependIfMissing(null, null, (CharSequence[]) null), "prependIfMissing(null,null null)");
        assertEquals("abc", StringUtils.prependIfMissing("abc", null, (CharSequence[]) null), "prependIfMissing(abc,null,null)");
        assertEquals("xyz", StringUtils.prependIfMissing("", "xyz", (CharSequence[]) null), "prependIfMissing(\"\",xyz,null)");
        assertEquals("xyzabc", StringUtils.prependIfMissing("abc", "xyz", new CharSequence[]{null}), "prependIfMissing(abc,xyz,{null})");
        assertEquals("abc", StringUtils.prependIfMissing("abc", "xyz", ""), "prependIfMissing(abc,xyz,\"\")");
        assertEquals("xyzabc", StringUtils.prependIfMissing("abc", "xyz", "mno"), "prependIfMissing(abc,xyz,mno)");
        assertEquals("xyzabc", StringUtils.prependIfMissing("xyzabc", "xyz", "mno"), "prependIfMissing(xyzabc,xyz,mno)");
        assertEquals("mnoabc", StringUtils.prependIfMissing("mnoabc", "xyz", "mno"), "prependIfMissing(mnoabc,xyz,mno)");
        assertEquals("xyzXYZabc", StringUtils.prependIfMissing("XYZabc", "xyz", "mno"), "prependIfMissing(XYZabc,xyz,mno)");
        assertEquals("xyzMNOabc", StringUtils.prependIfMissing("MNOabc", "xyz", "mno"), "prependIfMissing(MNOabc,xyz,mno)");
    }

    /**
     * Tests {@code prependIfMissingIgnoreCase}.
     */
    @Test
    public void testPrependIfMissingIgnoreCase() {
        assertNull(StringUtils.prependIfMissingIgnoreCase(null, null), "prependIfMissingIgnoreCase(null,null)");
        assertEquals("abc", StringUtils.prependIfMissingIgnoreCase("abc", null), "prependIfMissingIgnoreCase(abc,null)");
        assertEquals("xyz", StringUtils.prependIfMissingIgnoreCase("", "xyz"), "prependIfMissingIgnoreCase(\"\",xyz)");
        assertEquals("xyzabc", StringUtils.prependIfMissingIgnoreCase("abc", "xyz"), "prependIfMissingIgnoreCase(abc,xyz)");
        assertEquals("xyzabc", StringUtils.prependIfMissingIgnoreCase("xyzabc", "xyz"), "prependIfMissingIgnoreCase(xyzabc,xyz)");
        assertEquals("XYZabc", StringUtils.prependIfMissingIgnoreCase("XYZabc", "xyz"), "prependIfMissingIgnoreCase(XYZabc,xyz)");

        assertNull(StringUtils.prependIfMissingIgnoreCase(null, null, (CharSequence[]) null), "prependIfMissingIgnoreCase(null,null null)");
        assertEquals("abc", StringUtils.prependIfMissingIgnoreCase("abc", null, (CharSequence[]) null), "prependIfMissingIgnoreCase(abc,null,null)");
        assertEquals("xyz", StringUtils.prependIfMissingIgnoreCase("", "xyz", (CharSequence[]) null), "prependIfMissingIgnoreCase(\"\",xyz,null)");
        assertEquals("xyzabc", StringUtils.prependIfMissingIgnoreCase("abc", "xyz", new CharSequence[]{null}), "prependIfMissingIgnoreCase(abc,xyz,{null})");
        assertEquals("abc", StringUtils.prependIfMissingIgnoreCase("abc", "xyz", ""), "prependIfMissingIgnoreCase(abc,xyz,\"\")");
        assertEquals("xyzabc", StringUtils.prependIfMissingIgnoreCase("abc", "xyz", "mno"), "prependIfMissingIgnoreCase(abc,xyz,mno)");
        assertEquals("xyzabc", StringUtils.prependIfMissingIgnoreCase("xyzabc", "xyz", "mno"), "prependIfMissingIgnoreCase(xyzabc,xyz,mno)");
        assertEquals("mnoabc", StringUtils.prependIfMissingIgnoreCase("mnoabc", "xyz", "mno"), "prependIfMissingIgnoreCase(mnoabc,xyz,mno)");
        assertEquals("XYZabc", StringUtils.prependIfMissingIgnoreCase("XYZabc", "xyz", "mno"), "prependIfMissingIgnoreCase(XYZabc,xyz,mno)");
        assertEquals("MNOabc", StringUtils.prependIfMissingIgnoreCase("MNOabc", "xyz", "mno"), "prependIfMissingIgnoreCase(MNOabc,xyz,mno)");
    }

    @Test
    public void testReCapitalize() {
        // reflection type of tests: Sentences.
        assertEquals(SENTENCE_UNCAP, StringUtils.uncapitalize(StringUtils.capitalize(SENTENCE_UNCAP)),
                "uncapitalize(capitalize(String)) failed");
        assertEquals(SENTENCE_CAP, StringUtils.capitalize(StringUtils.uncapitalize(SENTENCE_CAP)),
                "capitalize(uncapitalize(String)) failed");

        // reflection type of tests: One word.
        assertEquals(FOO_UNCAP, StringUtils.uncapitalize(StringUtils.capitalize(FOO_UNCAP)),
                "uncapitalize(capitalize(String)) failed");
        assertEquals(FOO_CAP, StringUtils.capitalize(StringUtils.uncapitalize(FOO_CAP)),
                "capitalize(uncapitalize(String)) failed");
    }

    @Test
    public void testRemove_char() {
        // StringUtils.remove(null, *)       = null
        assertNull(StringUtils.remove(null, 'a'));
        assertNull(StringUtils.remove(null, 'a'));
        assertNull(StringUtils.remove(null, 'a'));

        // StringUtils.remove("", *)          = ""
        assertEquals("", StringUtils.remove("", 'a'));
        assertEquals("", StringUtils.remove("", 'a'));
        assertEquals("", StringUtils.remove("", 'a'));

        // StringUtils.remove("queued", 'u') = "qeed"
        assertEquals("qeed", StringUtils.remove("queued", 'u'));

        // StringUtils.remove("queued", 'z') = "queued"
        assertEquals("queued", StringUtils.remove("queued", 'z'));
    }

    @Test
    public void testRemove_String() {
        // StringUtils.remove(null, *)        = null
        assertNull(StringUtils.remove(null, null));
        assertNull(StringUtils.remove(null, ""));
        assertNull(StringUtils.remove(null, "a"));

        // StringUtils.remove("", *)          = ""
        assertEquals("", StringUtils.remove("", null));
        assertEquals("", StringUtils.remove("", ""));
        assertEquals("", StringUtils.remove("", "a"));

        // StringUtils.remove(*, null)        = *
        assertNull(StringUtils.remove(null, null));
        assertEquals("", StringUtils.remove("", null));
        assertEquals("a", StringUtils.remove("a", null));

        // StringUtils.remove(*, "")          = *
        assertNull(StringUtils.remove(null, ""));
        assertEquals("", StringUtils.remove("", ""));
        assertEquals("a", StringUtils.remove("a", ""));

        // StringUtils.remove("queued", "ue") = "qd"
        assertEquals("qd", StringUtils.remove("queued", "ue"));

        // StringUtils.remove("queued", "zz") = "queued"
        assertEquals("queued", StringUtils.remove("queued", "zz"));
    }

    @Test
    public void testRemoveAll_StringString() {
        assertNull(StringUtils.removeAll(null, ""));
        assertEquals("any", StringUtils.removeAll("any", null));

        assertEquals("any", StringUtils.removeAll("any", ""));
        assertEquals("", StringUtils.removeAll("any", ".*"));
        assertEquals("", StringUtils.removeAll("any", ".+"));
        assertEquals("", StringUtils.removeAll("any", ".?"));

        assertEquals("A\nB", StringUtils.removeAll("A<__>\n<__>B", "<.*>"));
        assertEquals("AB", StringUtils.removeAll("A<__>\n<__>B", "(?s)<.*>"));
        assertEquals("ABC123", StringUtils.removeAll("ABCabc123abc", "[a-z]"));

        assertThrows(
                PatternSyntaxException.class,
                () -> StringUtils.removeAll("any", "{badRegexSyntax}"),
                "StringUtils.removeAll expecting PatternSyntaxException");
    }

    @Test
    public void testRemoveEnd() {
        // StringUtils.removeEnd("", *)        = ""
        assertNull(StringUtils.removeEnd(null, null));
        assertNull(StringUtils.removeEnd(null, ""));
        assertNull(StringUtils.removeEnd(null, "a"));

        // StringUtils.removeEnd(*, null)      = *
        assertEquals(StringUtils.removeEnd("", null), "");
        assertEquals(StringUtils.removeEnd("", ""), "");
        assertEquals(StringUtils.removeEnd("", "a"), "");

        // All others:
        assertEquals(StringUtils.removeEnd("www.domain.com.", ".com"), "www.domain.com.");
        assertEquals(StringUtils.removeEnd("www.domain.com", ".com"), "www.domain");
        assertEquals(StringUtils.removeEnd("www.domain", ".com"), "www.domain");
        assertEquals(StringUtils.removeEnd("domain.com", ""), "domain.com");
        assertEquals(StringUtils.removeEnd("domain.com", null), "domain.com");
    }

    @Test
    public void testRemoveEndIgnoreCase() {
        // StringUtils.removeEndIgnoreCase("", *)        = ""
        assertNull(StringUtils.removeEndIgnoreCase(null, null), "removeEndIgnoreCase(null, null)");
        assertNull(StringUtils.removeEndIgnoreCase(null, ""), "removeEndIgnoreCase(null, \"\")");
        assertNull(StringUtils.removeEndIgnoreCase(null, "a"), "removeEndIgnoreCase(null, \"a\")");

        // StringUtils.removeEnd(*, null)      = *
        assertEquals(StringUtils.removeEndIgnoreCase("", null), "", "removeEndIgnoreCase(\"\", null)");
        assertEquals(StringUtils.removeEndIgnoreCase("", ""), "", "removeEndIgnoreCase(\"\", \"\")");
        assertEquals(StringUtils.removeEndIgnoreCase("", "a"), "", "removeEndIgnoreCase(\"\", \"a\")");

        // All others:
        assertEquals(StringUtils.removeEndIgnoreCase("www.domain.com.", ".com"), "www.domain.com.", "removeEndIgnoreCase(\"www.domain.com.\", \".com\")");
        assertEquals(StringUtils.removeEndIgnoreCase("www.domain.com", ".com"), "www.domain", "removeEndIgnoreCase(\"www.domain.com\", \".com\")");
        assertEquals(StringUtils.removeEndIgnoreCase("www.domain", ".com"), "www.domain", "removeEndIgnoreCase(\"www.domain\", \".com\")");
        assertEquals(StringUtils.removeEndIgnoreCase("domain.com", ""), "domain.com", "removeEndIgnoreCase(\"domain.com\", \"\")");
        assertEquals(StringUtils.removeEndIgnoreCase("domain.com", null), "domain.com", "removeEndIgnoreCase(\"domain.com\", null)");

        // Case insensitive:
        assertEquals(StringUtils.removeEndIgnoreCase("www.domain.com", ".COM"), "www.domain", "removeEndIgnoreCase(\"www.domain.com\", \".COM\")");
        assertEquals(StringUtils.removeEndIgnoreCase("www.domain.COM", ".com"), "www.domain", "removeEndIgnoreCase(\"www.domain.COM\", \".com\")");
    }

    @Test
    public void testRemoveFirst_StringString() {
        assertNull(StringUtils.removeFirst(null, ""));
        assertEquals("any", StringUtils.removeFirst("any", null));

        assertEquals("any", StringUtils.removeFirst("any", ""));
        assertEquals("", StringUtils.removeFirst("any", ".*"));
        assertEquals("", StringUtils.removeFirst("any", ".+"));
        assertEquals("bc", StringUtils.removeFirst("abc", ".?"));

        assertEquals("A\n<__>B", StringUtils.removeFirst("A<__>\n<__>B", "<.*>"));
        assertEquals("AB", StringUtils.removeFirst("A<__>\n<__>B", "(?s)<.*>"));
        assertEquals("ABCbc123", StringUtils.removeFirst("ABCabc123", "[a-z]"));
        assertEquals("ABC123abc", StringUtils.removeFirst("ABCabc123abc", "[a-z]+"));

        assertThrows(
                PatternSyntaxException.class,
                () -> StringUtils.removeFirst("any", "{badRegexSyntax}"),
                "StringUtils.removeFirst expecting PatternSyntaxException");
    }

    @Test
    public void testRemoveIgnoreCase_String() {
        // StringUtils.removeIgnoreCase(null, *) = null
        assertNull(StringUtils.removeIgnoreCase(null, null));
        assertNull(StringUtils.removeIgnoreCase(null, ""));
        assertNull(StringUtils.removeIgnoreCase(null, "a"));

        // StringUtils.removeIgnoreCase("", *) = ""
        assertEquals("", StringUtils.removeIgnoreCase("", null));
        assertEquals("", StringUtils.removeIgnoreCase("", ""));
        assertEquals("", StringUtils.removeIgnoreCase("", "a"));

        // StringUtils.removeIgnoreCase(*, null) = *
        assertNull(StringUtils.removeIgnoreCase(null, null));
        assertEquals("", StringUtils.removeIgnoreCase("", null));
        assertEquals("a", StringUtils.removeIgnoreCase("a", null));

        // StringUtils.removeIgnoreCase(*, "") = *
        assertNull(StringUtils.removeIgnoreCase(null, ""));
        assertEquals("", StringUtils.removeIgnoreCase("", ""));
        assertEquals("a", StringUtils.removeIgnoreCase("a", ""));

        // StringUtils.removeIgnoreCase("queued", "ue") = "qd"
        assertEquals("qd", StringUtils.removeIgnoreCase("queued", "ue"));

        // StringUtils.removeIgnoreCase("queued", "zz") = "queued"
        assertEquals("queued", StringUtils.removeIgnoreCase("queued", "zz"));

        // IgnoreCase
        // StringUtils.removeIgnoreCase("quEUed", "UE") = "qd"
        assertEquals("qd", StringUtils.removeIgnoreCase("quEUed", "UE"));

        // StringUtils.removeIgnoreCase("queued", "zZ") = "queued"
        assertEquals("queued", StringUtils.removeIgnoreCase("queued", "zZ"));
    }

    @Test
    public void testRemovePattern_StringString() {
        assertNull(StringUtils.removePattern(null, ""));
        assertEquals("any", StringUtils.removePattern("any", null));

        assertEquals("", StringUtils.removePattern("", ""));
        assertEquals("", StringUtils.removePattern("", ".*"));
        assertEquals("", StringUtils.removePattern("", ".+"));

        assertEquals("AB", StringUtils.removePattern("A<__>\n<__>B", "<.*>"));
        assertEquals("AB", StringUtils.removePattern("A<__>\\n<__>B", "<.*>"));
        assertEquals("", StringUtils.removePattern("<A>x\\ny</A>", "<A>.*</A>"));
        assertEquals("", StringUtils.removePattern("<A>\nxy\n</A>", "<A>.*</A>"));

        assertEquals("ABC123", StringUtils.removePattern("ABCabc123", "[a-z]"));
    }

    @Test
    public void testRemoveStart() {
        // StringUtils.removeStart("", *)        = ""
        assertNull(StringUtils.removeStart(null, null));
        assertNull(StringUtils.removeStart(null, ""));
        assertNull(StringUtils.removeStart(null, "a"));

        // StringUtils.removeStart(*, null)      = *
        assertEquals(StringUtils.removeStart("", null), "");
        assertEquals(StringUtils.removeStart("", ""), "");
        assertEquals(StringUtils.removeStart("", "a"), "");

        // All others:
        assertEquals(StringUtils.removeStart("www.domain.com", "www."), "domain.com");
        assertEquals(StringUtils.removeStart("domain.com", "www."), "domain.com");
        assertEquals(StringUtils.removeStart("domain.com", ""), "domain.com");
        assertEquals(StringUtils.removeStart("domain.com", null), "domain.com");
    }

    @Test
    public void testRemoveStartIgnoreCase() {
        // StringUtils.removeStart("", *)        = ""
        assertNull(StringUtils.removeStartIgnoreCase(null, null), "removeStartIgnoreCase(null, null)");
        assertNull(StringUtils.removeStartIgnoreCase(null, ""), "removeStartIgnoreCase(null, \"\")");
        assertNull(StringUtils.removeStartIgnoreCase(null, "a"), "removeStartIgnoreCase(null, \"a\")");

        // StringUtils.removeStart(*, null)      = *
        assertEquals(StringUtils.removeStartIgnoreCase("", null), "", "removeStartIgnoreCase(\"\", null)");
        assertEquals(StringUtils.removeStartIgnoreCase("", ""), "", "removeStartIgnoreCase(\"\", \"\")");
        assertEquals(StringUtils.removeStartIgnoreCase("", "a"), "", "removeStartIgnoreCase(\"\", \"a\")");

        // All others:
        assertEquals(StringUtils.removeStartIgnoreCase("www.domain.com", "www."), "domain.com", "removeStartIgnoreCase(\"www.domain.com\", \"www.\")");
        assertEquals(StringUtils.removeStartIgnoreCase("domain.com", "www."), "domain.com", "removeStartIgnoreCase(\"domain.com\", \"www.\")");
        assertEquals(StringUtils.removeStartIgnoreCase("domain.com", ""), "domain.com", "removeStartIgnoreCase(\"domain.com\", \"\")");
        assertEquals(StringUtils.removeStartIgnoreCase("domain.com", null), "domain.com", "removeStartIgnoreCase(\"domain.com\", null)");

        // Case insensitive:
        assertEquals(StringUtils.removeStartIgnoreCase("www.domain.com", "WWW."), "domain.com", "removeStartIgnoreCase(\"www.domain.com\", \"WWW.\")");
    }

    @Test
    public void testRepeat_CharInt() {
        assertEquals("zzz", StringUtils.repeat('z', 3));
        assertEquals("", StringUtils.repeat('z', 0));
        assertEquals("", StringUtils.repeat('z', -2));
    }

    @Test
    public void testRepeat_StringInt() {
        assertNull(StringUtils.repeat(null, 2));
        assertEquals("", StringUtils.repeat("ab", 0));
        assertEquals("", StringUtils.repeat("", 3));
        assertEquals("aaa", StringUtils.repeat("a", 3));
        assertEquals("", StringUtils.repeat("a", -2));
        assertEquals("ababab", StringUtils.repeat("ab", 3));
        assertEquals("abcabcabc", StringUtils.repeat("abc", 3));
        final String str = StringUtils.repeat("a", 10000);  // bigger than pad limit
        assertEquals(10000, str.length());
        assertTrue(StringUtils.containsOnly(str, 'a'));
    }

    @Test
    public void testRepeat_StringStringInt() {
        assertNull(StringUtils.repeat(null, null, 2));
        assertNull(StringUtils.repeat(null, "x", 2));
        assertEquals("", StringUtils.repeat("", null, 2));

        assertEquals("", StringUtils.repeat("ab", "", 0));
        assertEquals("", StringUtils.repeat("", "", 2));

        assertEquals("xx", StringUtils.repeat("", "x", 3));

        assertEquals("?, ?, ?", StringUtils.repeat("?", ", ", 3));
    }

    /**
     * Test method for 'StringUtils.replaceEach(String, String[], String[])'
     */
    @Test
    public void testReplace_StringStringArrayStringArray() {
        //JAVADOC TESTS START
        assertNull(StringUtils.replaceEach(null, new String[]{"a"}, new String[]{"b"}));
        assertEquals(StringUtils.replaceEach("", new String[]{"a"}, new String[]{"b"}), "");
        assertEquals(StringUtils.replaceEach("aba", null, null), "aba");
        assertEquals(StringUtils.replaceEach("aba", new String[0], null), "aba");
        assertEquals(StringUtils.replaceEach("aba", null, new String[0]), "aba");
        assertEquals(StringUtils.replaceEach("aba", new String[]{"a"}, null), "aba");

        assertEquals(StringUtils.replaceEach("aba", new String[]{"a"}, new String[]{""}), "b");
        assertEquals(StringUtils.replaceEach("aba", new String[]{null}, new String[]{"a"}), "aba");
        assertEquals(StringUtils.replaceEach("abcde", new String[]{"ab", "d"}, new String[]{"w", "t"}), "wcte");
        assertEquals(StringUtils.replaceEach("abcde", new String[]{"ab", "d"}, new String[]{"d", "t"}), "dcte");
        //JAVADOC TESTS END

        assertEquals("bcc", StringUtils.replaceEach("abc", new String[]{"a", "b"}, new String[]{"b", "c"}));
        assertEquals("q651.506bera", StringUtils.replaceEach("d216.102oren",
                new String[]{"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n",
                        "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "A", "B", "C", "D",
                        "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T",
                        "U", "V", "W", "X", "Y", "Z", "1", "2", "3", "4", "5", "6", "7", "8", "9"},
                new String[]{"n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "a",
                        "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "N", "O", "P", "Q",
                        "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "A", "B", "C", "D", "E", "F", "G",
                        "H", "I", "J", "K", "L", "M", "5", "6", "7", "8", "9", "1", "2", "3", "4"}));

        // Test null safety inside arrays - LANG-552
        assertEquals(StringUtils.replaceEach("aba", new String[]{"a"}, new String[]{null}), "aba");
        assertEquals(StringUtils.replaceEach("aba", new String[]{"a", "b"}, new String[]{"c", null}), "cbc");

        assertThrows(
                IllegalArgumentException.class,
                () -> StringUtils.replaceEach("abba", new String[]{"a"}, new String[]{"b", "a"}),
                "StringUtils.replaceEach(String, String[], String[]) expecting IllegalArgumentException");
    }

    /**
     * Test method for 'StringUtils.replaceEachRepeatedly(String, String[], String[])'
     */
    @Test
    public void testReplace_StringStringArrayStringArrayBoolean() {
        //JAVADOC TESTS START
        assertNull(StringUtils.replaceEachRepeatedly(null, new String[]{"a"}, new String[]{"b"}));
        assertEquals(StringUtils.replaceEachRepeatedly("", new String[]{"a"}, new String[]{"b"}), "");
        assertEquals(StringUtils.replaceEachRepeatedly("aba", null, null), "aba");
        assertEquals(StringUtils.replaceEachRepeatedly("aba", new String[0], null), "aba");
        assertEquals(StringUtils.replaceEachRepeatedly("aba", null, new String[0]), "aba");
        assertEquals(StringUtils.replaceEachRepeatedly("aba", new String[0], null), "aba");

        assertEquals(StringUtils.replaceEachRepeatedly("aba", new String[]{"a"}, new String[]{""}), "b");
        assertEquals(StringUtils.replaceEachRepeatedly("aba", new String[]{null}, new String[]{"a"}), "aba");
        assertEquals(StringUtils.replaceEachRepeatedly("abcde", new String[]{"ab", "d"}, new String[]{"w", "t"}), "wcte");
        assertEquals(StringUtils.replaceEachRepeatedly("abcde", new String[]{"ab", "d"}, new String[]{"d", "t"}), "tcte");

        assertThrows(
                IllegalStateException.class,
                () -> StringUtils.replaceEachRepeatedly("abcde", new String[]{"ab", "d"}, new String[]{"d", "ab"}),
                "Should be a circular reference");

        //JAVADOC TESTS END
    }

    @Test
    public void testReplace_StringStringString() {
        assertNull(StringUtils.replace(null, null, null));
        assertNull(StringUtils.replace(null, null, "any"));
        assertNull(StringUtils.replace(null, "any", null));
        assertNull(StringUtils.replace(null, "any", "any"));

        assertEquals("", StringUtils.replace("", null, null));
        assertEquals("", StringUtils.replace("", null, "any"));
        assertEquals("", StringUtils.replace("", "any", null));
        assertEquals("", StringUtils.replace("", "any", "any"));

        assertEquals("FOO", StringUtils.replace("FOO", "", "any"));
        assertEquals("FOO", StringUtils.replace("FOO", null, "any"));
        assertEquals("FOO", StringUtils.replace("FOO", "F", null));
        assertEquals("FOO", StringUtils.replace("FOO", null, null));

        assertEquals("", StringUtils.replace("foofoofoo", "foo", ""));
        assertEquals("barbarbar", StringUtils.replace("foofoofoo", "foo", "bar"));
        assertEquals("farfarfar", StringUtils.replace("foofoofoo", "oo", "ar"));
    }

    @Test
    public void testReplace_StringStringStringInt() {
        assertNull(StringUtils.replace(null, null, null, 2));
        assertNull(StringUtils.replace(null, null, "any", 2));
        assertNull(StringUtils.replace(null, "any", null, 2));
        assertNull(StringUtils.replace(null, "any", "any", 2));

        assertEquals("", StringUtils.replace("", null, null, 2));
        assertEquals("", StringUtils.replace("", null, "any", 2));
        assertEquals("", StringUtils.replace("", "any", null, 2));
        assertEquals("", StringUtils.replace("", "any", "any", 2));

        final String str = new String(new char[]{'o', 'o', 'f', 'o', 'o'});
        assertSame(str, StringUtils.replace(str, "x", "", -1));

        assertEquals("f", StringUtils.replace("oofoo", "o", "", -1));
        assertEquals("oofoo", StringUtils.replace("oofoo", "o", "", 0));
        assertEquals("ofoo", StringUtils.replace("oofoo", "o", "", 1));
        assertEquals("foo", StringUtils.replace("oofoo", "o", "", 2));
        assertEquals("fo", StringUtils.replace("oofoo", "o", "", 3));
        assertEquals("f", StringUtils.replace("oofoo", "o", "", 4));

        assertEquals("f", StringUtils.replace("oofoo", "o", "", -5));
        assertEquals("f", StringUtils.replace("oofoo", "o", "", 1000));
    }

    @Test
    public void testReplaceAll_StringStringString() {
        assertNull(StringUtils.replaceAll(null, "", ""));

        assertEquals("any", StringUtils.replaceAll("any", null, ""));
        assertEquals("any", StringUtils.replaceAll("any", "", null));

        assertEquals("zzz", StringUtils.replaceAll("", "", "zzz"));
        assertEquals("zzz", StringUtils.replaceAll("", ".*", "zzz"));
        assertEquals("", StringUtils.replaceAll("", ".+", "zzz"));
        assertEquals("ZZaZZbZZcZZ", StringUtils.replaceAll("abc", "", "ZZ"));

        assertEquals("z\nz", StringUtils.replaceAll("<__>\n<__>", "<.*>", "z"));
        assertEquals("z", StringUtils.replaceAll("<__>\n<__>", "(?s)<.*>", "z"));

        assertEquals("ABC___123", StringUtils.replaceAll("ABCabc123", "[a-z]", "_"));
        assertEquals("ABC_123", StringUtils.replaceAll("ABCabc123", "[^A-Z0-9]+", "_"));
        assertEquals("ABC123", StringUtils.replaceAll("ABCabc123", "[^A-Z0-9]+", ""));
        assertEquals("Lorem_ipsum_dolor_sit",
                     StringUtils.replaceAll("Lorem ipsum  dolor   sit", "( +)([a-z]+)", "_$2"));

        assertThrows(
                PatternSyntaxException.class,
                () -> StringUtils.replaceAll("any", "{badRegexSyntax}", ""),
                "StringUtils.replaceAll expecting PatternSyntaxException");
    }

    @Test
    public void testReplaceChars_StringCharChar() {
        assertNull(StringUtils.replaceChars(null, 'b', 'z'));
        assertEquals("", StringUtils.replaceChars("", 'b', 'z'));
        assertEquals("azcza", StringUtils.replaceChars("abcba", 'b', 'z'));
        assertEquals("abcba", StringUtils.replaceChars("abcba", 'x', 'z'));
    }

    @Test
    public void testReplaceChars_StringStringString() {
        assertNull(StringUtils.replaceChars(null, null, null));
        assertNull(StringUtils.replaceChars(null, "", null));
        assertNull(StringUtils.replaceChars(null, "a", null));
        assertNull(StringUtils.replaceChars(null, null, ""));
        assertNull(StringUtils.replaceChars(null, null, "x"));

        assertEquals("", StringUtils.replaceChars("", null, null));
        assertEquals("", StringUtils.replaceChars("", "", null));
        assertEquals("", StringUtils.replaceChars("", "a", null));
        assertEquals("", StringUtils.replaceChars("", null, ""));
        assertEquals("", StringUtils.replaceChars("", null, "x"));

        assertEquals("abc", StringUtils.replaceChars("abc", null, null));
        assertEquals("abc", StringUtils.replaceChars("abc", null, ""));
        assertEquals("abc", StringUtils.replaceChars("abc", null, "x"));

        assertEquals("abc", StringUtils.replaceChars("abc", "", null));
        assertEquals("abc", StringUtils.replaceChars("abc", "", ""));
        assertEquals("abc", StringUtils.replaceChars("abc", "", "x"));

        assertEquals("ac", StringUtils.replaceChars("abc", "b", null));
        assertEquals("ac", StringUtils.replaceChars("abc", "b", ""));
        assertEquals("axc", StringUtils.replaceChars("abc", "b", "x"));

        assertEquals("ayzya", StringUtils.replaceChars("abcba", "bc", "yz"));
        assertEquals("ayya", StringUtils.replaceChars("abcba", "bc", "y"));
        assertEquals("ayzya", StringUtils.replaceChars("abcba", "bc", "yzx"));

        assertEquals("abcba", StringUtils.replaceChars("abcba", "z", "w"));
        assertSame("abcba", StringUtils.replaceChars("abcba", "z", "w"));

        // Javadoc examples:
        assertEquals("jelly", StringUtils.replaceChars("hello", "ho", "jy"));
        assertEquals("ayzya", StringUtils.replaceChars("abcba", "bc", "yz"));
        assertEquals("ayya", StringUtils.replaceChars("abcba", "bc", "y"));
        assertEquals("ayzya", StringUtils.replaceChars("abcba", "bc", "yzx"));

        // From https://issues.apache.org/bugzilla/show_bug.cgi?id=25454
        assertEquals("bcc", StringUtils.replaceChars("abc", "ab", "bc"));
        assertEquals("q651.506bera", StringUtils.replaceChars("d216.102oren",
                "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ123456789",
                "nopqrstuvwxyzabcdefghijklmNOPQRSTUVWXYZABCDEFGHIJKLM567891234"));
    }

    @Test
    public void testReplaceFirst_StringStringString() {
        assertNull(StringUtils.replaceFirst(null, "", ""));

        assertEquals("any", StringUtils.replaceFirst("any", null, ""));
        assertEquals("any", StringUtils.replaceFirst("any", "", null));

        assertEquals("zzz", StringUtils.replaceFirst("", "", "zzz"));
        assertEquals("zzz", StringUtils.replaceFirst("", ".*", "zzz"));
        assertEquals("", StringUtils.replaceFirst("", ".+", "zzz"));
        assertEquals("ZZabc", StringUtils.replaceFirst("abc", "", "ZZ"));

        assertEquals("z\n<__>", StringUtils.replaceFirst("<__>\n<__>", "<.*>", "z"));
        assertEquals("z", StringUtils.replaceFirst("<__>\n<__>", "(?s)<.*>", "z"));

        assertEquals("ABC_bc123", StringUtils.replaceFirst("ABCabc123", "[a-z]", "_"));
        assertEquals("ABC_123abc", StringUtils.replaceFirst("ABCabc123abc", "[^A-Z0-9]+", "_"));
        assertEquals("ABC123abc", StringUtils.replaceFirst("ABCabc123abc", "[^A-Z0-9]+", ""));
        assertEquals("Lorem_ipsum  dolor   sit",
                     StringUtils.replaceFirst("Lorem ipsum  dolor   sit", "( +)([a-z]+)", "_$2"));

        assertThrows(
                PatternSyntaxException.class,
                () -> StringUtils.replaceFirst("any", "{badRegexSyntax}", ""),
                "StringUtils.replaceFirst expecting PatternSyntaxException");
    }

    @Test
    public void testReplaceIgnoreCase_StringStringString() {
        assertNull(StringUtils.replaceIgnoreCase(null, null, null));
        assertNull(StringUtils.replaceIgnoreCase(null, null, "any"));
        assertNull(StringUtils.replaceIgnoreCase(null, "any", null));
        assertNull(StringUtils.replaceIgnoreCase(null, "any", "any"));

        assertEquals("", StringUtils.replaceIgnoreCase("", null, null));
        assertEquals("", StringUtils.replaceIgnoreCase("", null, "any"));
        assertEquals("", StringUtils.replaceIgnoreCase("", "any", null));
        assertEquals("", StringUtils.replaceIgnoreCase("", "any", "any"));

        assertEquals("FOO", StringUtils.replaceIgnoreCase("FOO", "", "any"));
        assertEquals("FOO", StringUtils.replaceIgnoreCase("FOO", null, "any"));
        assertEquals("FOO", StringUtils.replaceIgnoreCase("FOO", "F", null));
        assertEquals("FOO", StringUtils.replaceIgnoreCase("FOO", null, null));

        assertEquals("", StringUtils.replaceIgnoreCase("foofoofoo", "foo", ""));
        assertEquals("barbarbar", StringUtils.replaceIgnoreCase("foofoofoo", "foo", "bar"));
        assertEquals("farfarfar", StringUtils.replaceIgnoreCase("foofoofoo", "oo", "ar"));

        // IgnoreCase
        assertEquals("", StringUtils.replaceIgnoreCase("foofoofoo", "FOO", ""));
        assertEquals("barbarbar", StringUtils.replaceIgnoreCase("fooFOOfoo", "foo", "bar"));
        assertEquals("farfarfar", StringUtils.replaceIgnoreCase("foofOOfoo", "OO", "ar"));
    }

    @Test
    public void testReplaceIgnoreCase_StringStringStringInt() {
        assertNull(StringUtils.replaceIgnoreCase(null, null, null, 2));
        assertNull(StringUtils.replaceIgnoreCase(null, null, "any", 2));
        assertNull(StringUtils.replaceIgnoreCase(null, "any", null, 2));
        assertNull(StringUtils.replaceIgnoreCase(null, "any", "any", 2));

        assertEquals("", StringUtils.replaceIgnoreCase("", null, null, 2));
        assertEquals("", StringUtils.replaceIgnoreCase("", null, "any", 2));
        assertEquals("", StringUtils.replaceIgnoreCase("", "any", null, 2));
        assertEquals("", StringUtils.replaceIgnoreCase("", "any", "any", 2));

        final String str = new String(new char[] { 'o', 'o', 'f', 'o', 'o' });
        assertSame(str, StringUtils.replaceIgnoreCase(str, "x", "", -1));

        assertEquals("f", StringUtils.replaceIgnoreCase("oofoo", "o", "", -1));
        assertEquals("oofoo", StringUtils.replaceIgnoreCase("oofoo", "o", "", 0));
        assertEquals("ofoo", StringUtils.replaceIgnoreCase("oofoo", "o", "", 1));
        assertEquals("foo", StringUtils.replaceIgnoreCase("oofoo", "o", "", 2));
        assertEquals("fo", StringUtils.replaceIgnoreCase("oofoo", "o", "", 3));
        assertEquals("f", StringUtils.replaceIgnoreCase("oofoo", "o", "", 4));

        assertEquals("f", StringUtils.replaceIgnoreCase("oofoo", "o", "", -5));
        assertEquals("f", StringUtils.replaceIgnoreCase("oofoo", "o", "", 1000));

        // IgnoreCase
        assertEquals("f", StringUtils.replaceIgnoreCase("oofoo", "O", "", -1));
        assertEquals("oofoo", StringUtils.replaceIgnoreCase("oofoo", "O", "", 0));
        assertEquals("ofoo", StringUtils.replaceIgnoreCase("oofoo", "O", "", 1));
        assertEquals("foo", StringUtils.replaceIgnoreCase("oofoo", "O", "", 2));
        assertEquals("fo", StringUtils.replaceIgnoreCase("oofoo", "O", "", 3));
        assertEquals("f", StringUtils.replaceIgnoreCase("oofoo", "O", "", 4));

        assertEquals("f", StringUtils.replaceIgnoreCase("oofoo", "O", "", -5));
        assertEquals("f", StringUtils.replaceIgnoreCase("oofoo", "O", "", 1000));
    }

    @Test
    public void testReplaceOnce_StringStringString() {
        assertNull(StringUtils.replaceOnce(null, null, null));
        assertNull(StringUtils.replaceOnce(null, null, "any"));
        assertNull(StringUtils.replaceOnce(null, "any", null));
        assertNull(StringUtils.replaceOnce(null, "any", "any"));

        assertEquals("", StringUtils.replaceOnce("", null, null));
        assertEquals("", StringUtils.replaceOnce("", null, "any"));
        assertEquals("", StringUtils.replaceOnce("", "any", null));
        assertEquals("", StringUtils.replaceOnce("", "any", "any"));

        assertEquals("FOO", StringUtils.replaceOnce("FOO", "", "any"));
        assertEquals("FOO", StringUtils.replaceOnce("FOO", null, "any"));
        assertEquals("FOO", StringUtils.replaceOnce("FOO", "F", null));
        assertEquals("FOO", StringUtils.replaceOnce("FOO", null, null));

        assertEquals("foofoo", StringUtils.replaceOnce("foofoofoo", "foo", ""));
    }

    @Test
    public void testReplaceOnceIgnoreCase_StringStringString() {
        assertNull(StringUtils.replaceOnceIgnoreCase(null, null, null));
        assertNull(StringUtils.replaceOnceIgnoreCase(null, null, "any"));
        assertNull(StringUtils.replaceOnceIgnoreCase(null, "any", null));
        assertNull(StringUtils.replaceOnceIgnoreCase(null, "any", "any"));

        assertEquals("", StringUtils.replaceOnceIgnoreCase("", null, null));
        assertEquals("", StringUtils.replaceOnceIgnoreCase("", null, "any"));
        assertEquals("", StringUtils.replaceOnceIgnoreCase("", "any", null));
        assertEquals("", StringUtils.replaceOnceIgnoreCase("", "any", "any"));

        assertEquals("FOO", StringUtils.replaceOnceIgnoreCase("FOO", "", "any"));
        assertEquals("FOO", StringUtils.replaceOnceIgnoreCase("FOO", null, "any"));
        assertEquals("FOO", StringUtils.replaceOnceIgnoreCase("FOO", "F", null));
        assertEquals("FOO", StringUtils.replaceOnceIgnoreCase("FOO", null, null));

        assertEquals("foofoo", StringUtils.replaceOnceIgnoreCase("foofoofoo", "foo", ""));

        // Ignore Case
        assertEquals("Foofoo", StringUtils.replaceOnceIgnoreCase("FoOFoofoo", "foo", ""));
    }

    @Test
    public void testReplacePattern_StringStringString() {
        assertNull(StringUtils.replacePattern(null, "", ""));
        assertEquals("any", StringUtils.replacePattern("any", null, ""));
        assertEquals("any", StringUtils.replacePattern("any", "", null));

        assertEquals("zzz", StringUtils.replacePattern("", "", "zzz"));
        assertEquals("zzz", StringUtils.replacePattern("", ".*", "zzz"));
        assertEquals("", StringUtils.replacePattern("", ".+", "zzz"));

        assertEquals("z", StringUtils.replacePattern("<__>\n<__>", "<.*>", "z"));
        assertEquals("z", StringUtils.replacePattern("<__>\\n<__>", "<.*>", "z"));
        assertEquals("X", StringUtils.replacePattern("<A>\nxy\n</A>", "<A>.*</A>", "X"));

        assertEquals("ABC___123", StringUtils.replacePattern("ABCabc123", "[a-z]", "_"));
        assertEquals("ABC_123", StringUtils.replacePattern("ABCabc123", "[^A-Z0-9]+", "_"));
        assertEquals("ABC123", StringUtils.replacePattern("ABCabc123", "[^A-Z0-9]+", ""));
        assertEquals("Lorem_ipsum_dolor_sit",
                     StringUtils.replacePattern("Lorem ipsum  dolor   sit", "( +)([a-z]+)", "_$2"));
    }

    //-----------------------------------------------------------------------
    @Test
    public void testReverse_String() {
        assertNull(StringUtils.reverse(null));
        assertEquals("", StringUtils.reverse(""));
        assertEquals("sdrawkcab", StringUtils.reverse("backwards"));
    }

    @Test
    public void testReverseDelimited_StringChar() {
        assertNull(StringUtils.reverseDelimited(null, '.'));
        assertEquals("", StringUtils.reverseDelimited("", '.'));
        assertEquals("c.b.a", StringUtils.reverseDelimited("a.b.c", '.'));
        assertEquals("a b c", StringUtils.reverseDelimited("a b c", '.'));
        assertEquals("", StringUtils.reverseDelimited("", '.'));
    }

    //-----------------------------------------------------------------------
    @Test
    public void testRightPad_StringInt() {
        assertNull(StringUtils.rightPad(null, 5));
        assertEquals("     ", StringUtils.rightPad("", 5));
        assertEquals("abc  ", StringUtils.rightPad("abc", 5));
        assertEquals("abc", StringUtils.rightPad("abc", 2));
        assertEquals("abc", StringUtils.rightPad("abc", -1));
    }

    @Test
    public void testRightPad_StringIntChar() {
        assertNull(StringUtils.rightPad(null, 5, ' '));
        assertEquals("     ", StringUtils.rightPad("", 5, ' '));
        assertEquals("abc  ", StringUtils.rightPad("abc", 5, ' '));
        assertEquals("abc", StringUtils.rightPad("abc", 2, ' '));
        assertEquals("abc", StringUtils.rightPad("abc", -1, ' '));
        assertEquals("abcxx", StringUtils.rightPad("abc", 5, 'x'));
        final String str = StringUtils.rightPad("aaa", 10000, 'a');  // bigger than pad length
        assertEquals(10000, str.length());
        assertTrue(StringUtils.containsOnly(str, 'a'));
    }

    @Test
    public void testRightPad_StringIntString() {
        assertNull(StringUtils.rightPad(null, 5, "-+"));
        assertEquals("     ", StringUtils.rightPad("", 5, " "));
        assertNull(StringUtils.rightPad(null, 8, null));
        assertEquals("abc-+-+", StringUtils.rightPad("abc", 7, "-+"));
        assertEquals("abc-+~", StringUtils.rightPad("abc", 6, "-+~"));
        assertEquals("abc-+", StringUtils.rightPad("abc", 5, "-+~"));
        assertEquals("abc", StringUtils.rightPad("abc", 2, " "));
        assertEquals("abc", StringUtils.rightPad("abc", -1, " "));
        assertEquals("abc  ", StringUtils.rightPad("abc", 5, null));
        assertEquals("abc  ", StringUtils.rightPad("abc", 5, ""));
    }

    //-----------------------------------------------------------------------
    @Test
    public void testRotate_StringInt() {
        assertNull(StringUtils.rotate(null, 1));
        assertEquals("", StringUtils.rotate("", 1));
        assertEquals("abcdefg", StringUtils.rotate("abcdefg", 0));
        assertEquals("fgabcde", StringUtils.rotate("abcdefg", 2));
        assertEquals("cdefgab", StringUtils.rotate("abcdefg", -2));
        assertEquals("abcdefg", StringUtils.rotate("abcdefg", 7));
        assertEquals("abcdefg", StringUtils.rotate("abcdefg", -7));
        assertEquals("fgabcde", StringUtils.rotate("abcdefg", 9));
        assertEquals("cdefgab", StringUtils.rotate("abcdefg", -9));
        assertEquals("efgabcd", StringUtils.rotate("abcdefg", 17));
        assertEquals("defgabc", StringUtils.rotate("abcdefg", -17));
    }

    @Test
    public void testSplit_String() {
        assertNull(StringUtils.split(null));
        assertEquals(0, StringUtils.split("").length);

        String str = "a b  .c";
        String[] res = StringUtils.split(str);
        assertEquals(3, res.length);
        assertEquals("a", res[0]);
        assertEquals("b", res[1]);
        assertEquals(".c", res[2]);

        str = " a ";
        res = StringUtils.split(str);
        assertEquals(1, res.length);
        assertEquals("a", res[0]);

        str = "a" + WHITESPACE + "b" + NON_WHITESPACE + "c";
        res = StringUtils.split(str);
        assertEquals(2, res.length);
        assertEquals("a", res[0]);
        assertEquals("b" + NON_WHITESPACE + "c", res[1]);
    }

    @Test
    public void testSplit_StringChar() {
        assertNull(StringUtils.split(null, '.'));
        assertEquals(0, StringUtils.split("", '.').length);

        String str = "a.b.. c";
        String[] res = StringUtils.split(str, '.');
        assertEquals(3, res.length);
        assertEquals("a", res[0]);
        assertEquals("b", res[1]);
        assertEquals(" c", res[2]);

        str = ".a.";
        res = StringUtils.split(str, '.');
        assertEquals(1, res.length);
        assertEquals("a", res[0]);

        str = "a b c";
        res = StringUtils.split(str, ' ');
        assertEquals(3, res.length);
        assertEquals("a", res[0]);
        assertEquals("b", res[1]);
        assertEquals("c", res[2]);
    }

    @Test
    public void testSplit_StringString_StringStringInt() {
        assertNull(StringUtils.split(null, "."));
        assertNull(StringUtils.split(null, ".", 3));

        assertEquals(0, StringUtils.split("", ".").length);
        assertEquals(0, StringUtils.split("", ".", 3).length);

        innerTestSplit('.', ".", ' ');
        innerTestSplit('.', ".", ',');
        innerTestSplit('.', ".,", 'x');
        for (int i = 0; i < WHITESPACE.length(); i++) {
            for (int j = 0; j < NON_WHITESPACE.length(); j++) {
                innerTestSplit(WHITESPACE.charAt(i), null, NON_WHITESPACE.charAt(j));
                innerTestSplit(WHITESPACE.charAt(i), String.valueOf(WHITESPACE.charAt(i)), NON_WHITESPACE.charAt(j));
            }
        }

        String[] results;
        final String[] expectedResults = {"ab", "de fg"};
        results = StringUtils.split("ab   de fg", null, 2);
        assertEquals(expectedResults.length, results.length);
        for (int i = 0; i < expectedResults.length; i++) {
            assertEquals(expectedResults[i], results[i]);
        }

        final String[] expectedResults2 = {"ab", "cd:ef"};
        results = StringUtils.split("ab:cd:ef", ":", 2);
        assertEquals(expectedResults2.length, results.length);
        for (int i = 0; i < expectedResults2.length; i++) {
            assertEquals(expectedResults2[i], results[i]);
        }
    }

    @Test
    public void testSplitByCharacterType() {
        assertNull(StringUtils.splitByCharacterType(null));
        assertEquals(0, StringUtils.splitByCharacterType("").length);

        assertTrue(Objects.deepEquals(new String[]{"ab", " ", "de", " ",
                "fg"}, StringUtils.splitByCharacterType("ab de fg")));

        assertTrue(Objects.deepEquals(new String[]{"ab", "   ", "de", " ",
                "fg"}, StringUtils.splitByCharacterType("ab   de fg")));

        assertTrue(Objects.deepEquals(new String[]{"ab", ":", "cd", ":",
                "ef"}, StringUtils.splitByCharacterType("ab:cd:ef")));

        assertTrue(Objects.deepEquals(new String[]{"number", "5"},
                StringUtils.splitByCharacterType("number5")));

        assertTrue(Objects.deepEquals(new String[]{"foo", "B", "ar"},
                StringUtils.splitByCharacterType("fooBar")));

        assertTrue(Objects.deepEquals(new String[]{"foo", "200", "B", "ar"},
                StringUtils.splitByCharacterType("foo200Bar")));

        assertTrue(Objects.deepEquals(new String[]{"ASFR", "ules"},
                StringUtils.splitByCharacterType("ASFRules")));
    }

    @Test
    public void testSplitByCharacterTypeCamelCase() {
        assertNull(StringUtils.splitByCharacterTypeCamelCase(null));
        assertEquals(0, StringUtils.splitByCharacterTypeCamelCase("").length);

        assertTrue(Objects.deepEquals(new String[]{"ab", " ", "de", " ",
                "fg"}, StringUtils.splitByCharacterTypeCamelCase("ab de fg")));

        assertTrue(Objects.deepEquals(new String[]{"ab", "   ", "de", " ",
                "fg"}, StringUtils.splitByCharacterTypeCamelCase("ab   de fg")));

        assertTrue(Objects.deepEquals(new String[]{"ab", ":", "cd", ":",
                "ef"}, StringUtils.splitByCharacterTypeCamelCase("ab:cd:ef")));

        assertTrue(Objects.deepEquals(new String[]{"number", "5"},
                StringUtils.splitByCharacterTypeCamelCase("number5")));

        assertTrue(Objects.deepEquals(new String[]{"foo", "Bar"},
                StringUtils.splitByCharacterTypeCamelCase("fooBar")));

        assertTrue(Objects.deepEquals(new String[]{"foo", "200", "Bar"},
                StringUtils.splitByCharacterTypeCamelCase("foo200Bar")));

        assertTrue(Objects.deepEquals(new String[]{"ASF", "Rules"},
                StringUtils.splitByCharacterTypeCamelCase("ASFRules")));
    }

    @Test
    public void testSplitByWholeSeparatorPreserveAllTokens_StringString() {
        assertArrayEquals(null, StringUtils.splitByWholeSeparatorPreserveAllTokens(null, "."));

        assertEquals(0, StringUtils.splitByWholeSeparatorPreserveAllTokens("", ".").length);

        // test whitespace
        String input = "ab   de fg";
        String[] expected = new String[]{"ab", "", "", "de", "fg"};

        String[] actual = StringUtils.splitByWholeSeparatorPreserveAllTokens(input, null);
        assertEquals(expected.length, actual.length);
        for (int i = 0; i < actual.length; i += 1) {
            assertEquals(expected[i], actual[i]);
        }

        // test delimiter singlechar
        input = "1::2:::3::::4";
        expected = new String[]{"1", "", "2", "", "", "3", "", "", "", "4"};

        actual = StringUtils.splitByWholeSeparatorPreserveAllTokens(input, ":");
        assertEquals(expected.length, actual.length);
        for (int i = 0; i < actual.length; i += 1) {
            assertEquals(expected[i], actual[i]);
        }

        // test delimiter multichar
        input = "1::2:::3::::4";
        expected = new String[]{"1", "2", ":3", "", "4"};

        actual = StringUtils.splitByWholeSeparatorPreserveAllTokens(input, "::");
        assertEquals(expected.length, actual.length);
        for (int i = 0; i < actual.length; i += 1) {
            assertEquals(expected[i], actual[i]);
        }
    }

    @Test
    public void testSplitByWholeSeparatorPreserveAllTokens_StringStringInt() {
        assertArrayEquals(null, StringUtils.splitByWholeSeparatorPreserveAllTokens(null, ".", -1));

        assertEquals(0, StringUtils.splitByWholeSeparatorPreserveAllTokens("", ".", -1).length);

        // test whitespace
        String input = "ab   de fg";
        String[] expected = new String[]{"ab", "", "", "de", "fg"};

        String[] actual = StringUtils.splitByWholeSeparatorPreserveAllTokens(input, null, -1);
        assertEquals(expected.length, actual.length);
        for (int i = 0; i < actual.length; i += 1) {
            assertEquals(expected[i], actual[i]);
        }

        // test delimiter singlechar
        input = "1::2:::3::::4";
        expected = new String[]{"1", "", "2", "", "", "3", "", "", "", "4"};

        actual = StringUtils.splitByWholeSeparatorPreserveAllTokens(input, ":", -1);
        assertEquals(expected.length, actual.length);
        for (int i = 0; i < actual.length; i += 1) {
            assertEquals(expected[i], actual[i]);
        }

        // test delimiter multichar
        input = "1::2:::3::::4";
        expected = new String[]{"1", "2", ":3", "", "4"};

        actual = StringUtils.splitByWholeSeparatorPreserveAllTokens(input, "::", -1);
        assertEquals(expected.length, actual.length);
        for (int i = 0; i < actual.length; i += 1) {
            assertEquals(expected[i], actual[i]);
        }

        // test delimiter char with max
        input = "1::2::3:4";
        expected = new String[]{"1", "", "2", ":3:4"};

        actual = StringUtils.splitByWholeSeparatorPreserveAllTokens(input, ":", 4);
        assertEquals(expected.length, actual.length);
        for (int i = 0; i < actual.length; i += 1) {
            assertEquals(expected[i], actual[i]);
        }
    }

    @Test
    public void testSplitByWholeString_StringStringBoolean() {
        assertArrayEquals(null, StringUtils.splitByWholeSeparator(null, "."));

        assertEquals(0, StringUtils.splitByWholeSeparator("", ".").length);

        final String stringToSplitOnNulls = "ab   de fg";
        final String[] splitOnNullExpectedResults = {"ab", "de", "fg"};

        final String[] splitOnNullResults = StringUtils.splitByWholeSeparator(stringToSplitOnNulls, null);
        assertEquals(splitOnNullExpectedResults.length, splitOnNullResults.length);
        for (int i = 0; i < splitOnNullExpectedResults.length; i += 1) {
            assertEquals(splitOnNullExpectedResults[i], splitOnNullResults[i]);
        }

        final String stringToSplitOnCharactersAndString = "abstemiouslyaeiouyabstemiously";

        final String[] splitOnStringExpectedResults = {"abstemiously", "abstemiously"};
        final String[] splitOnStringResults = StringUtils.splitByWholeSeparator(stringToSplitOnCharactersAndString, "aeiouy");
        assertEquals(splitOnStringExpectedResults.length, splitOnStringResults.length);
        for (int i = 0; i < splitOnStringExpectedResults.length; i += 1) {
            assertEquals(splitOnStringExpectedResults[i], splitOnStringResults[i]);
        }

        final String[] splitWithMultipleSeparatorExpectedResults = {"ab", "cd", "ef"};
        final String[] splitWithMultipleSeparator = StringUtils.splitByWholeSeparator("ab:cd::ef", ":");
        assertEquals(splitWithMultipleSeparatorExpectedResults.length, splitWithMultipleSeparator.length);
        for (int i = 0; i < splitWithMultipleSeparatorExpectedResults.length; i++) {
            assertEquals(splitWithMultipleSeparatorExpectedResults[i], splitWithMultipleSeparator[i]);
        }
    }

    @Test
    public void testSplitByWholeString_StringStringBooleanInt() {
        assertArrayEquals(null, StringUtils.splitByWholeSeparator(null, ".", 3));

        assertEquals(0, StringUtils.splitByWholeSeparator("", ".", 3).length);

        final String stringToSplitOnNulls = "ab   de fg";
        final String[] splitOnNullExpectedResults = {"ab", "de fg"};
        //String[] splitOnNullExpectedResults = { "ab", "de" } ;

        final String[] splitOnNullResults = StringUtils.splitByWholeSeparator(stringToSplitOnNulls, null, 2);
        assertEquals(splitOnNullExpectedResults.length, splitOnNullResults.length);
        for (int i = 0; i < splitOnNullExpectedResults.length; i += 1) {
            assertEquals(splitOnNullExpectedResults[i], splitOnNullResults[i]);
        }

        final String stringToSplitOnCharactersAndString = "abstemiouslyaeiouyabstemiouslyaeiouyabstemiously";

        final String[] splitOnStringExpectedResults = {"abstemiously", "abstemiouslyaeiouyabstemiously"};
        //String[] splitOnStringExpectedResults = { "abstemiously", "abstemiously" } ;
        final String[] splitOnStringResults = StringUtils.splitByWholeSeparator(stringToSplitOnCharactersAndString, "aeiouy", 2);
        assertEquals(splitOnStringExpectedResults.length, splitOnStringResults.length);
        for (int i = 0; i < splitOnStringExpectedResults.length; i++) {
            assertEquals(splitOnStringExpectedResults[i], splitOnStringResults[i]);
        }
    }

    @Test
    public void testSplitPreserveAllTokens_String() {
        assertNull(StringUtils.splitPreserveAllTokens(null));
        assertEquals(0, StringUtils.splitPreserveAllTokens("").length);

        String str = "abc def";
        String[] res = StringUtils.splitPreserveAllTokens(str);
        assertEquals(2, res.length);
        assertEquals("abc", res[0]);
        assertEquals("def", res[1]);

        str = "abc  def";
        res = StringUtils.splitPreserveAllTokens(str);
        assertEquals(3, res.length);
        assertEquals("abc", res[0]);
        assertEquals("", res[1]);
        assertEquals("def", res[2]);

        str = " abc ";
        res = StringUtils.splitPreserveAllTokens(str);
        assertEquals(3, res.length);
        assertEquals("", res[0]);
        assertEquals("abc", res[1]);
        assertEquals("", res[2]);

        str = "a b .c";
        res = StringUtils.splitPreserveAllTokens(str);
        assertEquals(3, res.length);
        assertEquals("a", res[0]);
        assertEquals("b", res[1]);
        assertEquals(".c", res[2]);

        str = " a b .c";
        res = StringUtils.splitPreserveAllTokens(str);
        assertEquals(4, res.length);
        assertEquals("", res[0]);
        assertEquals("a", res[1]);
        assertEquals("b", res[2]);
        assertEquals(".c", res[3]);

        str = "a  b  .c";
        res = StringUtils.splitPreserveAllTokens(str);
        assertEquals(5, res.length);
        assertEquals("a", res[0]);
        assertEquals("", res[1]);
        assertEquals("b", res[2]);
        assertEquals("", res[3]);
        assertEquals(".c", res[4]);

        str = " a  ";
        res = StringUtils.splitPreserveAllTokens(str);
        assertEquals(4, res.length);
        assertEquals("", res[0]);
        assertEquals("a", res[1]);
        assertEquals("", res[2]);
        assertEquals("", res[3]);

        str = " a  b";
        res = StringUtils.splitPreserveAllTokens(str);
        assertEquals(4, res.length);
        assertEquals("", res[0]);
        assertEquals("a", res[1]);
        assertEquals("", res[2]);
        assertEquals("b", res[3]);

        str = "a" + WHITESPACE + "b" + NON_WHITESPACE + "c";
        res = StringUtils.splitPreserveAllTokens(str);
        assertEquals(WHITESPACE.length() + 1, res.length);
        assertEquals("a", res[0]);
        for (int i = 1; i < WHITESPACE.length() - 1; i++) {
            assertEquals("", res[i]);
        }
        assertEquals("b" + NON_WHITESPACE + "c", res[WHITESPACE.length()]);
    }

    @Test
    public void testSplitPreserveAllTokens_StringChar() {
        assertNull(StringUtils.splitPreserveAllTokens(null, '.'));
        assertEquals(0, StringUtils.splitPreserveAllTokens("", '.').length);

        String str = "a.b. c";
        String[] res = StringUtils.splitPreserveAllTokens(str, '.');
        assertEquals(3, res.length);
        assertEquals("a", res[0]);
        assertEquals("b", res[1]);
        assertEquals(" c", res[2]);

        str = "a.b.. c";
        res = StringUtils.splitPreserveAllTokens(str, '.');
        assertEquals(4, res.length);
        assertEquals("a", res[0]);
        assertEquals("b", res[1]);
        assertEquals("", res[2]);
        assertEquals(" c", res[3]);

        str = ".a.";
        res = StringUtils.splitPreserveAllTokens(str, '.');
        assertEquals(3, res.length);
        assertEquals("", res[0]);
        assertEquals("a", res[1]);
        assertEquals("", res[2]);

        str = ".a..";
        res = StringUtils.splitPreserveAllTokens(str, '.');
        assertEquals(4, res.length);
        assertEquals("", res[0]);
        assertEquals("a", res[1]);
        assertEquals("", res[2]);
        assertEquals("", res[3]);

        str = "..a.";
        res = StringUtils.splitPreserveAllTokens(str, '.');
        assertEquals(4, res.length);
        assertEquals("", res[0]);
        assertEquals("", res[1]);
        assertEquals("a", res[2]);
        assertEquals("", res[3]);

        str = "..a";
        res = StringUtils.splitPreserveAllTokens(str, '.');
        assertEquals(3, res.length);
        assertEquals("", res[0]);
        assertEquals("", res[1]);
        assertEquals("a", res[2]);

        str = "a b c";
        res = StringUtils.splitPreserveAllTokens(str, ' ');
        assertEquals(3, res.length);
        assertEquals("a", res[0]);
        assertEquals("b", res[1]);
        assertEquals("c", res[2]);

        str = "a  b  c";
        res = StringUtils.splitPreserveAllTokens(str, ' ');
        assertEquals(5, res.length);
        assertEquals("a", res[0]);
        assertEquals("", res[1]);
        assertEquals("b", res[2]);
        assertEquals("", res[3]);
        assertEquals("c", res[4]);

        str = " a b c";
        res = StringUtils.splitPreserveAllTokens(str, ' ');
        assertEquals(4, res.length);
        assertEquals("", res[0]);
        assertEquals("a", res[1]);
        assertEquals("b", res[2]);
        assertEquals("c", res[3]);

        str = "  a b c";
        res = StringUtils.splitPreserveAllTokens(str, ' ');
        assertEquals(5, res.length);
        assertEquals("", res[0]);
        assertEquals("", res[1]);
        assertEquals("a", res[2]);
        assertEquals("b", res[3]);
        assertEquals("c", res[4]);

        str = "a b c ";
        res = StringUtils.splitPreserveAllTokens(str, ' ');
        assertEquals(4, res.length);
        assertEquals("a", res[0]);
        assertEquals("b", res[1]);
        assertEquals("c", res[2]);
        assertEquals("", res[3]);

        str = "a b c  ";
        res = StringUtils.splitPreserveAllTokens(str, ' ');
        assertEquals(5, res.length);
        assertEquals("a", res[0]);
        assertEquals("b", res[1]);
        assertEquals("c", res[2]);
        assertEquals("", res[3]);
        assertEquals("", res[3]);

        // Match example in javadoc
        {
            String[] results;
            final String[] expectedResults = {"a", "", "b", "c"};
            results = StringUtils.splitPreserveAllTokens("a..b.c", '.');
            assertEquals(expectedResults.length, results.length);
            for (int i = 0; i < expectedResults.length; i++) {
                assertEquals(expectedResults[i], results[i]);
            }
        }
    }

    @Test
    public void testSplitPreserveAllTokens_StringString_StringStringInt() {
        assertNull(StringUtils.splitPreserveAllTokens(null, "."));
        assertNull(StringUtils.splitPreserveAllTokens(null, ".", 3));

        assertEquals(0, StringUtils.splitPreserveAllTokens("", ".").length);
        assertEquals(0, StringUtils.splitPreserveAllTokens("", ".", 3).length);

        innerTestSplitPreserveAllTokens('.', ".", ' ');
        innerTestSplitPreserveAllTokens('.', ".", ',');
        innerTestSplitPreserveAllTokens('.', ".,", 'x');
        for (int i = 0; i < WHITESPACE.length(); i++) {
            for (int j = 0; j < NON_WHITESPACE.length(); j++) {
                innerTestSplitPreserveAllTokens(WHITESPACE.charAt(i), null, NON_WHITESPACE.charAt(j));
                innerTestSplitPreserveAllTokens(WHITESPACE.charAt(i), String.valueOf(WHITESPACE.charAt(i)), NON_WHITESPACE.charAt(j));
            }
        }

        {
            String[] results;
            final String[] expectedResults = {"ab", "de fg"};
            results = StringUtils.splitPreserveAllTokens("ab de fg", null, 2);
            assertEquals(expectedResults.length, results.length);
            for (int i = 0; i < expectedResults.length; i++) {
                assertEquals(expectedResults[i], results[i]);
            }
        }

        {
            String[] results;
            final String[] expectedResults = {"ab", "  de fg"};
            results = StringUtils.splitPreserveAllTokens("ab   de fg", null, 2);
            assertEquals(expectedResults.length, results.length);
            for (int i = 0; i < expectedResults.length; i++) {
                assertEquals(expectedResults[i], results[i]);
            }
        }

        {
            String[] results;
            final String[] expectedResults = {"ab", "::de:fg"};
            results = StringUtils.splitPreserveAllTokens("ab:::de:fg", ":", 2);
            assertEquals(expectedResults.length, results.length);
            for (int i = 0; i < expectedResults.length; i++) {
                assertEquals(expectedResults[i], results[i]);
            }
        }

        {
            String[] results;
            final String[] expectedResults = {"ab", "", " de fg"};
            results = StringUtils.splitPreserveAllTokens("ab   de fg", null, 3);
            assertEquals(expectedResults.length, results.length);
            for (int i = 0; i < expectedResults.length; i++) {
                assertEquals(expectedResults[i], results[i]);
            }
        }

        {
            String[] results;
            final String[] expectedResults = {"ab", "", "", "de fg"};
            results = StringUtils.splitPreserveAllTokens("ab   de fg", null, 4);
            assertEquals(expectedResults.length, results.length);
            for (int i = 0; i < expectedResults.length; i++) {
                assertEquals(expectedResults[i], results[i]);
            }
        }

        {
            final String[] expectedResults = {"ab", "cd:ef"};
            String[] results;
            results = StringUtils.splitPreserveAllTokens("ab:cd:ef", ":", 2);
            assertEquals(expectedResults.length, results.length);
            for (int i = 0; i < expectedResults.length; i++) {
                assertEquals(expectedResults[i], results[i]);
            }
        }

        {
            String[] results;
            final String[] expectedResults = {"ab", ":cd:ef"};
            results = StringUtils.splitPreserveAllTokens("ab::cd:ef", ":", 2);
            assertEquals(expectedResults.length, results.length);
            for (int i = 0; i < expectedResults.length; i++) {
                assertEquals(expectedResults[i], results[i]);
            }
        }

        {
            String[] results;
            final String[] expectedResults = {"ab", "", ":cd:ef"};
            results = StringUtils.splitPreserveAllTokens("ab:::cd:ef", ":", 3);
            assertEquals(expectedResults.length, results.length);
            for (int i = 0; i < expectedResults.length; i++) {
                assertEquals(expectedResults[i], results[i]);
            }
        }

        {
            String[] results;
            final String[] expectedResults = {"ab", "", "", "cd:ef"};
            results = StringUtils.splitPreserveAllTokens("ab:::cd:ef", ":", 4);
            assertEquals(expectedResults.length, results.length);
            for (int i = 0; i < expectedResults.length; i++) {
                assertEquals(expectedResults[i], results[i]);
            }
        }

        {
            String[] results;
            final String[] expectedResults = {"", "ab", "", "", "cd:ef"};
            results = StringUtils.splitPreserveAllTokens(":ab:::cd:ef", ":", 5);
            assertEquals(expectedResults.length, results.length);
            for (int i = 0; i < expectedResults.length; i++) {
                assertEquals(expectedResults[i], results[i]);
            }
        }

        {
            String[] results;
            final String[] expectedResults = {"", "", "ab", "", "", "cd:ef"};
            results = StringUtils.splitPreserveAllTokens("::ab:::cd:ef", ":", 6);
            assertEquals(expectedResults.length, results.length);
            for (int i = 0; i < expectedResults.length; i++) {
                assertEquals(expectedResults[i], results[i]);
            }
        }

    }

    // Methods on StringUtils that are immutable in spirit (i.e. calculate the length)
    // should take a CharSequence parameter. Methods that are mutable in spirit (i.e. capitalize)
    // should take a String or String[] parameter and return String or String[].
    // This test enforces that this is done.
    @Test
    public void testStringUtilsCharSequenceContract() {
        final Class<StringUtils> c = StringUtils.class;
        // Methods that are expressly excluded from testStringUtilsCharSequenceContract()
        final String[] excludeMethods = {
            "public static int org.apache.commons.lang3.StringUtils.compare(java.lang.String,java.lang.String)",
            "public static int org.apache.commons.lang3.StringUtils.compare(java.lang.String,java.lang.String,boolean)",
            "public static int org.apache.commons.lang3.StringUtils.compareIgnoreCase(java.lang.String,java.lang.String)",
            "public static int org.apache.commons.lang3.StringUtils.compareIgnoreCase(java.lang.String,java.lang.String,boolean)",
            "public static byte[] org.apache.commons.lang3.StringUtils.getBytes(java.lang.String,java.nio.charset.Charset)",
            "public static byte[] org.apache.commons.lang3.StringUtils.getBytes(java.lang.String,java.lang.String) throws java.io.UnsupportedEncodingException"
        };
        final Method[] methods = c.getMethods();

        for (final Method m : methods) {
            final String methodStr = m.toString();
            if (m.getReturnType() == String.class || m.getReturnType() == String[].class) {
                // Assume this is mutable and ensure the first parameter is not CharSequence.
                // It may be String or it may be something else (String[], Object, Object[]) so
                // don't actively test for that.
                final Class<?>[] params = m.getParameterTypes();
                if (params.length > 0 && (params[0] == CharSequence.class || params[0] == CharSequence[].class)) {
                    assertTrue(!ArrayUtils.contains(excludeMethods, methodStr),
                            "The method \"" + methodStr + "\" appears to be mutable in spirit and therefore must not accept a CharSequence");
                }
            } else {
                // Assume this is immutable in spirit and ensure the first parameter is not String.
                // As above, it may be something other than CharSequence.
                final Class<?>[] params = m.getParameterTypes();
                if (params.length > 0 && (params[0] == String.class || params[0] == String[].class)) {
                    assertTrue(ArrayUtils.contains(excludeMethods, methodStr),
                            "The method \"" + methodStr + "\" appears to be immutable in spirit and therefore must not accept a String");
                }
            }
        }
    }

    @Test
    public void testSwapCase_String() {
        assertNull(StringUtils.swapCase(null));
        assertEquals("", StringUtils.swapCase(""));
        assertEquals("  ", StringUtils.swapCase("  "));

        assertEquals("i", WordUtils.swapCase("I"));
        assertEquals("I", WordUtils.swapCase("i"));
        assertEquals("I AM HERE 123", StringUtils.swapCase("i am here 123"));
        assertEquals("i aM hERE 123", StringUtils.swapCase("I Am Here 123"));
        assertEquals("I AM here 123", StringUtils.swapCase("i am HERE 123"));
        assertEquals("i am here 123", StringUtils.swapCase("I AM HERE 123"));

        final String test = "This String contains a TitleCase character: \u01C8";
        final String expect = "tHIS sTRING CONTAINS A tITLEcASE CHARACTER: \u01C9";
        assertEquals(expect, WordUtils.swapCase(test));
        assertEquals(expect, StringUtils.swapCase(test));
    }

    @Test
    public void testToCodePoints() {
        final int orphanedHighSurrogate = 0xD801;
        final int orphanedLowSurrogate = 0xDC00;
        final int supplementary = 0x2070E;

        final int[] codePoints = {'a', orphanedHighSurrogate, 'b', 'c', supplementary,
                'd', orphanedLowSurrogate, 'e'};
        final String s = new String(codePoints, 0, codePoints.length);
        assertArrayEquals(codePoints, StringUtils.toCodePoints(s));

        assertNull(StringUtils.toCodePoints(null));
        assertArrayEquals(ArrayUtils.EMPTY_INT_ARRAY, StringUtils.toCodePoints(""));
    }

    /**
     * Tests {@link StringUtils#toEncodedString(byte[], Charset)}
     *
     * @see StringUtils#toEncodedString(byte[], Charset)
     */
    @Test
    public void testToEncodedString() {
        final String expectedString = "The quick brown fox jumps over the lazy dog.";
        String encoding = SystemUtils.FILE_ENCODING;
        byte[] expectedBytes = expectedString.getBytes(Charset.defaultCharset());
        // sanity check start
        assertArrayEquals(expectedBytes, expectedString.getBytes());
        // sanity check end
        assertEquals(expectedString, StringUtils.toEncodedString(expectedBytes, Charset.defaultCharset()));
        assertEquals(expectedString, StringUtils.toEncodedString(expectedBytes, Charset.forName(encoding)));
        encoding = "UTF-16";
        expectedBytes = expectedString.getBytes(Charset.forName(encoding));
        assertEquals(expectedString, StringUtils.toEncodedString(expectedBytes, Charset.forName(encoding)));
    }

    /**
     * Tests {@link StringUtils#toString(byte[], String)}
     *
     * @throws java.io.UnsupportedEncodingException because the method under test max throw it
     * @see StringUtils#toString(byte[], String)
     */
    @Test
    public void testToString() throws UnsupportedEncodingException {
        final String expectedString = "The quick brown fox jumps over the lazy dog.";
        byte[] expectedBytes = expectedString.getBytes(Charset.defaultCharset());
        // sanity check start
        assertArrayEquals(expectedBytes, expectedString.getBytes());
        // sanity check end
        assertEquals(expectedString, StringUtils.toString(expectedBytes, null));
        assertEquals(expectedString, StringUtils.toString(expectedBytes, SystemUtils.FILE_ENCODING));
        final String encoding = "UTF-16";
        expectedBytes = expectedString.getBytes(Charset.forName(encoding));
        assertEquals(expectedString, StringUtils.toString(expectedBytes, encoding));
    }

    //-----------------------------------------------------------------------
    @Test
    public void testTruncate_StringInt() {
        assertNull(StringUtils.truncate(null, 12));
        assertThrows(
                IllegalArgumentException.class, () -> StringUtils.truncate(null, -1), "maxWith cannot be negative");
        assertThrows(
                IllegalArgumentException.class, () -> StringUtils.truncate(null, -10), "maxWith cannot be negative");
        assertThrows(
                IllegalArgumentException.class,
                () -> StringUtils.truncate(null, Integer.MIN_VALUE),
                "maxWith cannot be negative");
        assertEquals("", StringUtils.truncate("", 10));
        assertEquals("", StringUtils.truncate("", 10));
        assertEquals("abc", StringUtils.truncate("abcdefghij", 3));
        assertEquals("abcdef", StringUtils.truncate("abcdefghij", 6));
        assertEquals("", StringUtils.truncate("abcdefghij", 0));
        assertThrows(
                IllegalArgumentException.class,
                () -> StringUtils.truncate("abcdefghij", -1),
                "maxWith cannot be negative");
        assertThrows(
                IllegalArgumentException.class,
                () -> StringUtils.truncate("abcdefghij", -100),
                "maxWith cannot be negative");
        assertThrows(
                IllegalArgumentException.class,
                () -> StringUtils.truncate("abcdefghij", Integer.MIN_VALUE),
                "maxWith cannot be negative");
        assertEquals("abcdefghij", StringUtils.truncate("abcdefghijklmno", 10));
        assertEquals("abcdefghijklmno", StringUtils.truncate("abcdefghijklmno", Integer.MAX_VALUE));
        assertEquals("abcde", StringUtils.truncate("abcdefghijklmno", 5));
        assertEquals("abc", StringUtils.truncate("abcdefghijklmno", 3));
    }

    @Test
    public void testTruncate_StringIntInt() {
        assertNull(StringUtils.truncate(null, 0, 12));
        assertThrows(
                IllegalArgumentException.class, () -> StringUtils.truncate(null, -1, 0), "maxWith cannot be negative");
        assertThrows(
                IllegalArgumentException.class,
                () -> StringUtils.truncate(null, -10, -4),
                "maxWith cannot be negative");
        assertThrows(
                IllegalArgumentException.class,
                () -> StringUtils.truncate(null, Integer.MIN_VALUE, Integer.MIN_VALUE),
                "maxWith cannot be negative");
        assertNull(StringUtils.truncate(null, 10, 12));
        assertEquals("", StringUtils.truncate("", 0, 10));
        assertEquals("", StringUtils.truncate("", 2, 10));
        assertEquals("abc", StringUtils.truncate("abcdefghij", 0, 3));
        assertEquals("fghij", StringUtils.truncate("abcdefghij", 5, 6));
        assertEquals("", StringUtils.truncate("abcdefghij", 0, 0));
        assertThrows(
                IllegalArgumentException.class,
                () -> StringUtils.truncate("abcdefghij", 0, -1),
                "maxWith cannot be negative");
        assertThrows(
                IllegalArgumentException.class,
                () -> StringUtils.truncate("abcdefghij", 0, -10),
                "maxWith cannot be negative");
        assertThrows(
                IllegalArgumentException.class,
                () -> StringUtils.truncate("abcdefghij", 0, -100),
                "maxWith cannot be negative");
        assertThrows(
                IllegalArgumentException.class,
                () -> StringUtils.truncate("abcdefghij", 1, -100),
                "maxWith cannot be negative");
        assertThrows(
                IllegalArgumentException.class,
                () -> StringUtils.truncate("abcdefghij", 0, Integer.MIN_VALUE),
                "maxWith cannot be negative");
        assertThrows(
                IllegalArgumentException.class,
                () -> StringUtils.truncate("abcdefghij", -1, 0),
                "offset cannot be negative");
        assertThrows(
                IllegalArgumentException.class,
                () -> StringUtils.truncate("abcdefghij", -10, 0),
                "offset cannot be negative");
        assertThrows(
                IllegalArgumentException.class,
                () -> StringUtils.truncate("abcdefghij", -100, 1),
                "offset cannot be negative");
        assertThrows(
                IllegalArgumentException.class,
                () -> StringUtils.truncate("abcdefghij", Integer.MIN_VALUE, 0),
                "offset cannot be negative");
        assertThrows(
                IllegalArgumentException.class,
                () -> StringUtils.truncate("abcdefghij", -1, -1),
                "offset cannot be negative");
        assertThrows(
                IllegalArgumentException.class,
                () -> StringUtils.truncate("abcdefghij", -10, -10),
                "offset cannot be negative");
        assertThrows(
                IllegalArgumentException.class,
                () -> StringUtils.truncate("abcdefghij", -100, -100),
                "offset  cannot be negative");
        assertThrows(
                IllegalArgumentException.class,
                () -> StringUtils.truncate("abcdefghij", Integer.MIN_VALUE, Integer.MIN_VALUE),
                "offset  cannot be negative");
        final String raspberry = "raspberry peach";
        assertEquals("peach", StringUtils.truncate(raspberry, 10, 15));
        assertEquals("abcdefghij", StringUtils.truncate("abcdefghijklmno", 0, 10));
        assertEquals("abcdefghijklmno", StringUtils.truncate("abcdefghijklmno", 0, Integer.MAX_VALUE));
        assertEquals("bcdefghijk", StringUtils.truncate("abcdefghijklmno", 1, 10));
        assertEquals("cdefghijkl", StringUtils.truncate("abcdefghijklmno", 2, 10));
        assertEquals("defghijklm", StringUtils.truncate("abcdefghijklmno", 3, 10));
        assertEquals("efghijklmn", StringUtils.truncate("abcdefghijklmno", 4, 10));
        assertEquals("fghijklmno", StringUtils.truncate("abcdefghijklmno", 5, 10));
        assertEquals("fghij", StringUtils.truncate("abcdefghijklmno", 5, 5));
        assertEquals("fgh", StringUtils.truncate("abcdefghijklmno", 5, 3));
        assertEquals("klm", StringUtils.truncate("abcdefghijklmno", 10, 3));
        assertEquals("klmno", StringUtils.truncate("abcdefghijklmno", 10, Integer.MAX_VALUE));
        assertEquals("n", StringUtils.truncate("abcdefghijklmno", 13, 1));
        assertEquals("no", StringUtils.truncate("abcdefghijklmno", 13, Integer.MAX_VALUE));
        assertEquals("o", StringUtils.truncate("abcdefghijklmno", 14, 1));
        assertEquals("o", StringUtils.truncate("abcdefghijklmno", 14, Integer.MAX_VALUE));
        assertEquals("", StringUtils.truncate("abcdefghijklmno", 15, 1));
        assertEquals("", StringUtils.truncate("abcdefghijklmno", 15, Integer.MAX_VALUE));
        assertEquals("", StringUtils.truncate("abcdefghijklmno", Integer.MAX_VALUE, Integer.MAX_VALUE));
    }

    // -----------------------------------------------------------------------

    @Test
    public void testUnCapitalize() {
        assertNull(StringUtils.uncapitalize(null));

        assertEquals(FOO_UNCAP, StringUtils.uncapitalize(FOO_CAP), "uncapitalize(String) failed");
        assertEquals(FOO_UNCAP, StringUtils.uncapitalize(FOO_UNCAP), "uncapitalize(string) failed");
        assertEquals("", StringUtils.uncapitalize(""), "uncapitalize(empty-string) failed");
        assertEquals("x", StringUtils.uncapitalize("X"), "uncapitalize(single-char-string) failed");

        // Examples from uncapitalize Javadoc
        assertEquals("cat", StringUtils.uncapitalize("cat"));
        assertEquals("cat", StringUtils.uncapitalize("Cat"));
        assertEquals("cAT", StringUtils.uncapitalize("CAT"));
    }

    @Test
    public void testUnescapeSurrogatePairs() {
        assertEquals("\uD83D\uDE30", StringEscapeUtils.unescapeCsv("\uD83D\uDE30"));
        // Examples from https://en.wikipedia.org/wiki/UTF-16
        assertEquals("\uD800\uDC00", StringEscapeUtils.unescapeCsv("\uD800\uDC00"));
        assertEquals("\uD834\uDD1E", StringEscapeUtils.unescapeCsv("\uD834\uDD1E"));
        assertEquals("\uDBFF\uDFFD", StringEscapeUtils.unescapeCsv("\uDBFF\uDFFD"));
        assertEquals("\uDBFF\uDFFD", StringEscapeUtils.unescapeHtml3("\uDBFF\uDFFD"));
        assertEquals("\uDBFF\uDFFD", StringEscapeUtils.unescapeHtml4("\uDBFF\uDFFD"));
    }

    @Test
    public void testUnwrap_StringChar() {
        assertNull(StringUtils.unwrap(null, null));
        assertNull(StringUtils.unwrap(null, CharUtils.NUL));
        assertNull(StringUtils.unwrap(null, '1'));

        assertEquals("abc", StringUtils.unwrap("abc", null));
        assertEquals("abc", StringUtils.unwrap("\'abc\'", '\''));
        assertEquals("abc", StringUtils.unwrap("AabcA", 'A'));
        assertEquals("AabcA", StringUtils.unwrap("AAabcAA", 'A'));
        assertEquals("abc", StringUtils.unwrap("abc", 'b'));
        assertEquals("#A", StringUtils.unwrap("#A", '#'));
        assertEquals("A#", StringUtils.unwrap("A#", '#'));
        assertEquals("ABA", StringUtils.unwrap("AABAA", 'A'));
    }

    @Test
    public void testUnwrap_StringString() {
        assertNull(StringUtils.unwrap(null, null));
        assertNull(StringUtils.unwrap(null, ""));
        assertNull(StringUtils.unwrap(null, "1"));

        assertEquals("abc", StringUtils.unwrap("abc", null));
        assertEquals("abc", StringUtils.unwrap("abc", ""));
        assertEquals("abc", StringUtils.unwrap("\'abc\'", "\'"));
        assertEquals("abc", StringUtils.unwrap("\"abc\"", "\""));
        assertEquals("abc\"xyz", StringUtils.unwrap("\"abc\"xyz\"", "\""));
        assertEquals("abc\"xyz\"", StringUtils.unwrap("\"abc\"xyz\"\"", "\""));
        assertEquals("abc\'xyz\'", StringUtils.unwrap("\"abc\'xyz\'\"", "\""));
        assertEquals("\"abc\'xyz\'\"", StringUtils.unwrap("AA\"abc\'xyz\'\"AA", "AA"));
        assertEquals("\"abc\'xyz\'\"", StringUtils.unwrap("123\"abc\'xyz\'\"123", "123"));
        assertEquals("AA\"abc\'xyz\'\"", StringUtils.unwrap("AA\"abc\'xyz\'\"", "AA"));
        assertEquals("AA\"abc\'xyz\'\"AA", StringUtils.unwrap("AAA\"abc\'xyz\'\"AAA", "A"));
        assertEquals("\"abc\'xyz\'\"AA", StringUtils.unwrap("\"abc\'xyz\'\"AA", "AA"));
    }

    @Test
    public void testUpperCase() {
        assertNull(StringUtils.upperCase(null));
        assertNull(StringUtils.upperCase(null, Locale.ENGLISH));
        assertEquals("FOO TEST THING", StringUtils.upperCase("fOo test THING"), "upperCase(String) failed");
        assertEquals("", StringUtils.upperCase(""), "upperCase(empty-string) failed");
        assertEquals("FOO TEST THING", StringUtils.upperCase("fOo test THING", Locale.ENGLISH),
                "upperCase(String, Locale) failed");
        assertEquals("", StringUtils.upperCase("", Locale.ENGLISH),
                "upperCase(empty-string, Locale) failed");
    }

    @Test
    public void testWrap_StringChar() {
        assertNull(StringUtils.wrap(null, CharUtils.NUL));
        assertNull(StringUtils.wrap(null, '1'));

        assertEquals("", StringUtils.wrap("", CharUtils.NUL));
        assertEquals("xabx", StringUtils.wrap("ab", 'x'));
        assertEquals("\"ab\"", StringUtils.wrap("ab", '\"'));
        assertEquals("\"\"ab\"\"", StringUtils.wrap("\"ab\"", '\"'));
        assertEquals("'ab'", StringUtils.wrap("ab", '\''));
        assertEquals("''abcd''", StringUtils.wrap("'abcd'", '\''));
        assertEquals("'\"abcd\"'", StringUtils.wrap("\"abcd\"", '\''));
        assertEquals("\"'abcd'\"", StringUtils.wrap("'abcd'", '\"'));
    }

    @Test
    public void testWrap_StringString() {
        assertNull(StringUtils.wrap(null, null));
        assertNull(StringUtils.wrap(null, ""));
        assertNull(StringUtils.wrap(null, "1"));

        assertNull(StringUtils.wrap(null, null));
        assertEquals("", StringUtils.wrap("", ""));
        assertEquals("ab", StringUtils.wrap("ab", null));
        assertEquals("xabx", StringUtils.wrap("ab", "x"));
        assertEquals("\"ab\"", StringUtils.wrap("ab", "\""));
        assertEquals("\"\"ab\"\"", StringUtils.wrap("\"ab\"", "\""));
        assertEquals("'ab'", StringUtils.wrap("ab", "'"));
        assertEquals("''abcd''", StringUtils.wrap("'abcd'", "'"));
        assertEquals("'\"abcd\"'", StringUtils.wrap("\"abcd\"", "'"));
        assertEquals("\"'abcd'\"", StringUtils.wrap("'abcd'", "\""));
    }

    @Test
    public void testWrapIfMissing_StringChar() {
        assertNull(StringUtils.wrapIfMissing(null, CharUtils.NUL));
        assertNull(StringUtils.wrapIfMissing(null, '1'));

        assertEquals("", StringUtils.wrapIfMissing("", CharUtils.NUL));
        assertEquals("xabx", StringUtils.wrapIfMissing("ab", 'x'));
        assertEquals("\"ab\"", StringUtils.wrapIfMissing("ab", '\"'));
        assertEquals("\"ab\"", StringUtils.wrapIfMissing("\"ab\"", '\"'));
        assertEquals("'ab'", StringUtils.wrapIfMissing("ab", '\''));
        assertEquals("'abcd'", StringUtils.wrapIfMissing("'abcd'", '\''));
        assertEquals("'\"abcd\"'", StringUtils.wrapIfMissing("\"abcd\"", '\''));
        assertEquals("\"'abcd'\"", StringUtils.wrapIfMissing("'abcd'", '\"'));
        assertEquals("/x/", StringUtils.wrapIfMissing("x", '/'));
        assertEquals("/x/y/z/", StringUtils.wrapIfMissing("x/y/z", '/'));
        assertEquals("/x/y/z/", StringUtils.wrapIfMissing("/x/y/z", '/'));
        assertEquals("/x/y/z/", StringUtils.wrapIfMissing("x/y/z/", '/'));
        assertEquals("/", StringUtils.wrapIfMissing("/", '/'));
    }

    @Test
    public void testWrapIfMissing_StringString() {
        assertNull(StringUtils.wrapIfMissing(null, "\0"));
        assertNull(StringUtils.wrapIfMissing(null, "1"));

        assertEquals("", StringUtils.wrapIfMissing("", "\0"));
        assertEquals("xabx", StringUtils.wrapIfMissing("ab", "x"));
        assertEquals("\"ab\"", StringUtils.wrapIfMissing("ab", "\""));
        assertEquals("\"ab\"", StringUtils.wrapIfMissing("\"ab\"", "\""));
        assertEquals("'ab'", StringUtils.wrapIfMissing("ab", "\'"));
        assertEquals("'abcd'", StringUtils.wrapIfMissing("'abcd'", "\'"));
        assertEquals("'\"abcd\"'", StringUtils.wrapIfMissing("\"abcd\"", "\'"));
        assertEquals("\"'abcd'\"", StringUtils.wrapIfMissing("'abcd'", "\""));
        assertEquals("/x/", StringUtils.wrapIfMissing("x", "/"));
        assertEquals("/x/y/z/", StringUtils.wrapIfMissing("x/y/z", "/"));
        assertEquals("/x/y/z/", StringUtils.wrapIfMissing("/x/y/z", "/"));
        assertEquals("/x/y/z/", StringUtils.wrapIfMissing("x/y/z/", "/"));
        assertEquals("/", StringUtils.wrapIfMissing("/", "/"));
        assertEquals("ab/ab", StringUtils.wrapIfMissing("/", "ab"));
        assertEquals("ab/ab", StringUtils.wrapIfMissing("ab/ab", "ab"));
    }
}
