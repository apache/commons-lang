/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002-2003 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution, if
 *    any, must include the following acknowlegement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowlegement may appear in the software itself,
 *    if and wherever such third-party acknowlegements normally appear.
 *
 * 4. The names "The Jakarta Project", "Commons", and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 */
package org.apache.commons.lang;

import java.util.Arrays;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;
/**
 * Unit tests {@link org.apache.commons.lang.StringUtils}.
 *
 * @author <a href="mailto:dlr@collab.net">Daniel Rall</a>
 * @author <a href="mailto:bayard@generationjava.com">Henri Yandell</a>
 * @author Stephen Colebourne
 * @author <a href="mailto:ridesmet@users.sourceforge.net">Ringo De Smet</a>
 * @author <a href="mailto:fredrik@westermarck.com>Fredrik Westermarck</a>
 * @author Holger Krauth
 * @author <a href="hps@intermeta.de">Henning P. Schmiedehausen</a>
 * @version $Id: StringUtilsTest.java,v 1.23 2003/07/16 21:23:50 scolebourne Exp $
 */
public class StringUtilsTest extends TestCase {

    private static final String[] ARRAY_LIST = { "foo", "bar", "baz" };
    private static final String[] EMPTY_ARRAY_LIST = {};

    private static final String SEPARATOR = ",";
    private static final char   SEPARATOR_CHAR = ';';

    private static final String TEXT_LIST = "foo,bar,baz";
    private static final String TEXT_LIST_CHAR = "foo;bar;baz";
    private static final String TEXT_LIST_NOSEP = "foobarbaz";

    private static final String FOO = "foo";
    private static final String BAR = "bar";
    private static final String CAP_FOO = "Foo";

    private static final String SENTENCE = "foo bar baz";

    public StringUtilsTest(String name) {
        super(name);
    }

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
    	TestSuite suite = new TestSuite(StringUtilsTest.class);
    	suite.setName("StringUtilsTest Tests");
        return suite;
    }

    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    //-----------------------------------------------------------------------

    public void testCaseFunctions() {
        assertEquals("capitalise(String) failed",
                     CAP_FOO, StringUtils.capitalise(FOO) );
        assertEquals("capitalise(empty-string) failed",
                     "", StringUtils.capitalise("") );
        assertEquals("capitalise(single-char-string) failed",
                     "X", StringUtils.capitalise("x") );
        assertEquals("capitaliseAllWords(String) failed",
                     "Foo Bar Baz", StringUtils.capitaliseAllWords(SENTENCE) );
        assertEquals("capitaliseAllWords(empty-string) failed",
                     "", StringUtils.capitaliseAllWords("") );
        assertEquals("uncapitalise(String) failed",
                     FOO, StringUtils.uncapitalise(CAP_FOO) );
        assertEquals("uncapitalise(empty-string) failed",
                     "", StringUtils.uncapitalise("") );
        assertEquals("uncapitalise(single-char-string) failed",
                     "x", StringUtils.uncapitalise("X") );
        assertEquals("uncapitaliseAllWords(String) failed",
                     SENTENCE, StringUtils.uncapitaliseAllWords("Foo Bar Baz") );
        assertEquals("uncapitaliseAllWords(empty-string) failed",
                     "", StringUtils.uncapitaliseAllWords("") );

        assertEquals("upperCase(String) failed",
                     "FOO TEST THING", StringUtils.upperCase("fOo test THING") );
        assertEquals("upperCase(empty-string) failed",
                     "", StringUtils.upperCase("") );
        assertEquals("lowerCase(String) failed",
                     "foo test thing", StringUtils.lowerCase("fOo test THING") );
        assertEquals("lowerCase(empty-string) failed",
                     "", StringUtils.lowerCase("") );

        assertEquals("swapCase(empty-string) failed",
                     "", StringUtils.swapCase("") );
        assertEquals("swapCase(String-with-numbers) failed",
                     "a123RgYu", StringUtils.swapCase("A123rGyU") );
        assertEquals("swapCase(String) failed",
                     "Hello aPACHE", StringUtils.swapCase("hELLO Apache") );
    }

    public void testJoin() {
        assertEquals("concatenate(Object[]) failed",
                     TEXT_LIST_NOSEP, StringUtils.concatenate(ARRAY_LIST));
        assertEquals("join(Object[], String) failed", TEXT_LIST,
                     StringUtils.join(ARRAY_LIST, SEPARATOR));
        assertEquals("join(Iterator, String) failed", TEXT_LIST,
                     StringUtils.join(Arrays.asList(ARRAY_LIST).iterator(),
                                      SEPARATOR));

        assertEquals("join(Object[], char) failed", TEXT_LIST_CHAR,
                     StringUtils.join(ARRAY_LIST, SEPARATOR_CHAR));
        assertEquals("join(Iterator, char) failed", TEXT_LIST_CHAR,
                     StringUtils.join(Arrays.asList(ARRAY_LIST).iterator(),
                                      SEPARATOR_CHAR));

        assertEquals("join(Object[], null) failed", TEXT_LIST_NOSEP,
                     StringUtils.join(ARRAY_LIST, null));
        assertEquals("join(Iterator, null) failed", TEXT_LIST_NOSEP,
                     StringUtils.join(Arrays.asList(ARRAY_LIST).iterator(),
                                      null));

        assertEquals("concatenate(Object[]) failed",
                     "", StringUtils.concatenate(EMPTY_ARRAY_LIST));
        assertEquals("join(Object[], String) failed", "",
                     StringUtils.join(EMPTY_ARRAY_LIST, SEPARATOR));
        assertEquals("join(Iterator, String) failed", "",
                     StringUtils.join(Arrays.asList(EMPTY_ARRAY_LIST).iterator(),
                                      SEPARATOR));

        assertEquals("join(Object[], char) failed", "",
                     StringUtils.join(EMPTY_ARRAY_LIST, SEPARATOR_CHAR));
        assertEquals("join(Iterator, char) failed", "",
                     StringUtils.join(Arrays.asList(EMPTY_ARRAY_LIST).iterator(),
                                      SEPARATOR_CHAR));

        assertEquals("join(Object[], null) failed", "",
                     StringUtils.join(EMPTY_ARRAY_LIST, null));
        assertEquals("join(Iterator, null) failed", "",
                     StringUtils.join(Arrays.asList(EMPTY_ARRAY_LIST).iterator(),
                                      null));
    }

    public void testSplit() {
        String[] result = StringUtils.split(TEXT_LIST, SEPARATOR, 2);
        String[] expected = { "foo", "bar,baz" };
        assertEquals("split(Object[], String, int) yielded unexpected length",
                     expected.length, result.length);
        for (int i = 0; i < result.length; i++)
        {
            assertEquals("split(Object[], String, int) failed", expected[i],
                         result[i]);
        }

        result = StringUtils.split(TEXT_LIST, SEPARATOR, 0);
        expected = ARRAY_LIST;
        assertEquals("split(Object[], String, int) yielded unexpected length",
                     expected.length, result.length);
        for (int i = 0; i < result.length; i++)
        {
            assertEquals("split(Object[], String, int) failed", expected[i],
                         result[i]);
        }

        result = StringUtils.split(TEXT_LIST, SEPARATOR, -1);
        expected = ARRAY_LIST;
        assertEquals("split(Object[], String, int) yielded unexpected length",
                     expected.length, result.length);
        for (int i = 0; i < result.length; i++)
        {
            assertEquals("split(Object[], String, int) failed", expected[i],
                         result[i]);
        }

        result = StringUtils.split("one two three four five six", null, 3);
        assertEquals("split(Object[], null, int)[0] failed", "one", result[0]);
        assertEquals("split(Object[], null, int)[1] failed", "two", result[1]);
        assertEquals("split(Object[], null, int)[2] failed", "three four five six", result[2]);
    }

    public void testReplaceFunctions() {
        assertEquals("replace(String, String, String, int) failed",
                     FOO, StringUtils.replace("oo" + FOO, "o", "", 2));
        assertEquals("replace(String, String, String) failed",
                     "", StringUtils.replace(FOO + FOO + FOO, FOO, ""));
        assertEquals("replaceOnce(String, String, String) failed",
                     FOO, StringUtils.replaceOnce(FOO + FOO, FOO, ""));
        assertEquals("carriage-return replace(String,String,String) failed",
                     "test123", StringUtils.replace("test\r1\r2\r3", "\r", ""));

        assertEquals("replace(String, String, String) failed",
            "FOO", StringUtils.replace("FOO", "", "any"));
        assertEquals("replace(String, String, String) failed",
            "FOO", StringUtils.replace("FOO", null, "any"));
        assertEquals("replace(String, String, String) failed",
            "FOO", StringUtils.replace("FOO", "F", null));
        assertEquals("replace(String, String, String) failed",
            "FOO", StringUtils.replace("FOO", null, null));
        assertEquals("replace(String, String, String) failed",
            null, StringUtils.replace(null, "", "any"));
    }

    public void testOverlayString() {
        assertEquals("overlayString(String, String, int, int) failed",
                     "foo foor baz", StringUtils.overlayString(SENTENCE, FOO, 4, 6) );
    }

    public void testRepeat() {
        assertEquals("", StringUtils.repeat("ab", 0));
        assertEquals("", StringUtils.repeat("", 3));
        assertEquals("aaa", StringUtils.repeat("a", 3));
        assertEquals("ababab", StringUtils.repeat("ab", 3));
        assertEquals("abcabcabc", StringUtils.repeat("abc", 3));
        try {
            StringUtils.repeat(null, 0);
            fail();
        } catch (NullPointerException ex) {
        }
    }

    public void testCenter() {
        assertEquals("center(String, int) failed",
                     "   "+FOO+"   ", StringUtils.center(FOO, 9) );
    }

    public void testDeprecatedChompFunctions() {
        assertEquals("chompLast(String) failed",
                     FOO, StringUtils.chompLast(FOO + "\n") );

        assertEquals("getChomp(String, String) failed",
                     "\n" + FOO, StringUtils.getChomp(FOO + "\n" + FOO, "\n") );

        assertEquals("prechomp(String, String) failed",
                     FOO, StringUtils.prechomp(FOO + "\n" + FOO, "\n") );

        assertEquals("getPrechomp(String, String) failed",
                     FOO + "\n", StringUtils.getPrechomp(FOO + "\n" + FOO, "\n") );

        assertEquals("chopNewline(String, String) failed",
                     FOO, StringUtils.chopNewline(FOO + "\r\n") );
    }

    public void testChop() {

        String[][] chopCases = {
            { FOO + "\r\n", FOO } ,
            { FOO + "\n" , FOO } ,
            { FOO + "\r", FOO },
            { "foo", "fo"},
            { "foo\nfoo", "foo\nfo" },
            { "\n", "" },
            { "\r", "" },
            { "\r\n", "" },
        };
        for (int i = 0; i < chopCases.length; i++) {
            String original = chopCases[i][0];
            String expectedResult = chopCases[i][1];
            assertEquals("chop(String) failed",
                    expectedResult, StringUtils.chop(original));
        }
    }

    public void testChomp() {

        String[][] chompCases = {
            { FOO + "\r\n", FOO } ,
            { FOO + "\n" , FOO } ,
            { FOO + "\r", FOO },
            { FOO, FOO },
            { FOO + "\n\n", FOO + "\n"},
            { "foo\nfoo", "foo\nfoo" },
            { "\n", "" },
            { "\r", "" },
            { "\r\n", "" },
        };
        for (int i = 0; i < chompCases.length; i++) {
            String original = chompCases[i][0];
            String expectedResult = chompCases[i][1];
            assertEquals("chomp(String) failed",
                    expectedResult, StringUtils.chomp(original));
        }

        assertEquals("chomp(String, String) failed",
                "foo", StringUtils.chomp("foobar", "bar"));
        assertEquals("chomp(String, String) failed",
                "foobar", StringUtils.chomp("foobar", "baz"));
        assertEquals("chomp(String, String) failed",
                "foo", StringUtils.chomp("foo", "foooo"));
    }

    public void testChopNewLine() {

        String[][] newLineCases = {
            { FOO + "\r\n", FOO } ,
            { FOO + "\n" , FOO } ,
            { FOO + "\r", FOO + "\r" },
            { FOO, FOO },
            { FOO + "\n" + FOO , FOO + "\n" + FOO },
            { FOO + "\n\n", FOO + "\n"},
            { "\n", "" },
            { "", "" },
            { "\r\n", "" }
      };

      for (int i = 0; i < newLineCases.length; i++) {
          String original = newLineCases[i][0];
          String expectedResult = newLineCases[i][1];
          assertEquals("chopNewline(String) failed",
                  expectedResult, StringUtils.chopNewline(original));
      }
    }

    public void testSliceFunctions() {

        String[][] sliceCases = {
            {"foo\n", "foo"},
            {"foo\nbar", "foo"},
            {"foo\nbar\n", "foo\nbar"},
            {"foo\nbar\nbaz", "foo\nbar"},
        };
        for (int i = 0; i < sliceCases.length; i++) {
            String original = sliceCases[i][0];
            String expectedResult = sliceCases[i][1];
            assertEquals("slice(String) failed",
                    expectedResult, StringUtils.slice(original));
        }

        String original = "fooXXbarXXbaz";
        String sep = "XX";

        assertEquals("slice(String,String) failed",
                     "fooXXbar", StringUtils.slice(original, sep) );

        assertEquals("sliceRemainder(String, String) failed",
                     "baz", StringUtils.sliceRemainder(original, sep) );

        assertEquals("sliceFirst(String, String) failed",
                     "foo", StringUtils.sliceFirst(original, sep) );

        assertEquals("sliceFirstRemainder(String, String) failed",
                     "barXXbaz", StringUtils.sliceFirstRemainder(original, sep) );

    }

    public void testPadFunctions() {
        assertEquals("rightPad(String, int) failed",
                     "1234    ", StringUtils.rightPad ("1234", 8) );

        assertEquals("rightPad(String, int, String) failed",
                     "1234-+-+", StringUtils.rightPad ("1234", 8, "-+") );

        assertEquals("rightPad(String, int, String) failed",
                     "123456-+~", StringUtils.rightPad ("123456", 9, "-+~") );

        assertEquals("leftPad(String, int) failed",
                     "    1234", StringUtils.leftPad("1234", 8) );

        assertEquals("leftPad(String, int, String) failed",
                     "-+-+1234", StringUtils.leftPad("1234", 8, "-+") );

        assertEquals("leftPad(String, int, String) failed",
                     "-+~123456", StringUtils.leftPad("123456", 9, "-+~") );
    }

    public void testReverseFunctions() {
        assertEquals("reverse(String) failed",
                     "sdrawkcab", StringUtils.reverse("backwards") );
        assertEquals("reverse(empty-string) failed",
                     "", StringUtils.reverse("") );
        assertEquals("reverseDelimitedString(String,'.') failed",
                     "org.apache.test",
                       StringUtils.reverseDelimitedString("test.apache.org", ".") );
        assertEquals("reverseDelimitedString(empty-string,'.') failed",
                     "",
                       StringUtils.reverseDelimitedString("", ".") );
        assertEquals("reverseDelimitedString(String,' ') failed",
                     "once upon a time",
                       StringUtils.reverseDelimitedString("time a upon once"," ") );
    }

    public void testDefaultFunctions() {
        assertEquals("defaultString(empty-string) failed",
                     "", StringUtils.defaultString("") );
        assertEquals("defaultString(String) failed",
                     FOO, StringUtils.defaultString(FOO) );
        assertEquals("defaultString(null) failed",
                     "", StringUtils.defaultString(null) );
        assertEquals("defaultString(empty-string,String) failed",
                     "", StringUtils.defaultString("", BAR) );
        assertEquals("defaultString(String,String) failed",
                     FOO, StringUtils.defaultString(FOO, BAR) );
        assertEquals("defaultString(null,String) failed",
                     BAR, StringUtils.defaultString(null, BAR) );

        assertEquals("defaultString((Object) empty-string) failed",
                     "", StringUtils.defaultString((Object) "") );
        assertEquals("defaultString((Object) String) failed",
                     FOO, StringUtils.defaultString((Object) FOO) );
        assertEquals("defaultString((Object) null) failed",
                     "", StringUtils.defaultString((Object) null) );
        assertEquals("defaultString((Object) empty-string,String) failed",
                     "", StringUtils.defaultString((Object) "", BAR) );
        assertEquals("defaultString((Object) String,String) failed",
                     FOO, StringUtils.defaultString((Object) FOO, BAR) );
        assertEquals("defaultString((Object) null,String) failed",
                     BAR, StringUtils.defaultString((Object) null, BAR) );
        assertEquals("defaultString(Boolean.TRUE,String) failed",
                     Boolean.TRUE.toString(), StringUtils.defaultString(Boolean.TRUE, BAR) );
    }

    public void testEscapeFunctions() {
        assertEquals("escape(empty-string) failed",
                     "", StringUtils.escape("") );
        assertEquals("escape(String) failed",
                     FOO, StringUtils.escape(FOO) );
        assertEquals("escape(String) failed",
                     "\\t", StringUtils.escape("\t") );
        assertEquals("escape(String) failed",
                     "\\\\", StringUtils.escape("\\") );
        assertEquals("escape(String) failed",
                     "\\\\\\b\\t\\r", StringUtils.escape("\\\b\t\r") );
        assertEquals("escape(String) failed",
                     "\\u1234", StringUtils.escape("\u1234") );
        assertEquals("escape(String) failed",
                     "\\u0234", StringUtils.escape("\u0234") );
        assertEquals("escape(String) failed",
                     "\\u00FD", StringUtils.escape("\u00fd") );
    }

    public void testGetLevenshteinDistance() {
        assertEquals("getLevenshteinDistance(empty-string, empty-string) failed",
                     0, StringUtils.getLevenshteinDistance("", "") );
        assertEquals("getLevenshteinDistance(empty-string, String) failed",
                     1, StringUtils.getLevenshteinDistance("", "a") );
        assertEquals("getLevenshteinDistance(String, empty-string) failed",
                     7, StringUtils.getLevenshteinDistance("aaapppp", "") );
        assertEquals("getLevenshteinDistance(String, String) failed",
                     1, StringUtils.getLevenshteinDistance("frog", "fog") );
        assertEquals("getLevenshteinDistance(String, String) failed",
                     3, StringUtils.getLevenshteinDistance("fly", "ant") );
        assertEquals("getLevenshteinDistance(String, String) failed",
                     7, StringUtils.getLevenshteinDistance("elephant", "hippo") );
        assertEquals("getLevenshteinDistance(String, String) failed",
                     7, StringUtils.getLevenshteinDistance("hippo", "elephant") );
        assertEquals("getLevenshteinDistance(String, String) failed",
                     1, StringUtils.getLevenshteinDistance("hello", "hallo") );
    }

    public void testContainsOnlyString() {
        String str1 = "a";
        String str2 = "b";
        String str3 = "ab";
        String chars1= "b";
        String chars2= "a";
        String chars3= "ab";
        String emptyChars = "";
        assertEquals("containsOnly(null, null) failed", false, StringUtils.containsOnly(null, (String) null));
        assertEquals("containsOnly(empty-string, null) failed", false, StringUtils.containsOnly("", (String) null));
        assertEquals("containsOnly(null, empty-string) failed", false, StringUtils.containsOnly(null, emptyChars));
        assertEquals("containsOnly(str1, empty-char-array) failed", false, StringUtils.containsOnly(str1, emptyChars));
        assertEquals("containsOnly(empty-string, empty-char-array) failed", true, StringUtils.containsOnly("", emptyChars));
        assertEquals("containsOnly(empty-string, chars1) failed", true, StringUtils.containsOnly("", chars1));
        assertEquals("containsOnly(str1, chars1) failed", false, StringUtils.containsOnly(str1, chars1));
        assertEquals("containsOnly(str1, chars2) success", true, StringUtils.containsOnly(str1, chars2));
        assertEquals("containsOnly(str1, chars3) success", true, StringUtils.containsOnly(str1, chars3));
        assertEquals("containsOnly(str2, chars1) success", true, StringUtils.containsOnly(str2, chars1));
        assertEquals("containsOnly(str2, chars2) failed", false, StringUtils.containsOnly(str2, chars2));
        assertEquals("containsOnly(str2, chars3) success", true, StringUtils.containsOnly(str2, chars3));
        assertEquals("containsOnly(String3, chars1) failed", false, StringUtils.containsOnly(str3, chars1));
        assertEquals("containsOnly(String3, chars2) failed", false, StringUtils.containsOnly(str3, chars2));
        assertEquals("containsOnly(String3, chars3) success", true, StringUtils.containsOnly(str3, chars3));
    }

    public void testContainsOnlyCharArray() {
        String str1 = "a";
        String str2 = "b";
        String str3 = "ab";
        char[] chars1= {'b'};
        char[] chars2= {'a'};
        char[] chars3= {'a', 'b'};
        char[] emptyChars = new char[0];
        assertEquals("containsOnly(null, null) failed", false, StringUtils.containsOnly(null, (char[]) null));
        assertEquals("containsOnly(empty-string, null) failed", false, StringUtils.containsOnly("", (char[]) null));
        assertEquals("containsOnly(null, empty-string) failed", false, StringUtils.containsOnly(null, emptyChars));
        assertEquals("containsOnly(str1, empty-char-array) failed", false, StringUtils.containsOnly(str1, emptyChars));
        assertEquals("containsOnly(empty-string, empty-char-array) failed", true, StringUtils.containsOnly("", emptyChars));
        assertEquals("containsOnly(empty-string, chars1) failed", true, StringUtils.containsOnly("", chars1));
        assertEquals("containsOnly(str1, chars1) failed", false, StringUtils.containsOnly(str1, chars1));
        assertEquals("containsOnly(str1, chars2) success", true, StringUtils.containsOnly(str1, chars2));
        assertEquals("containsOnly(str1, chars3) success", true, StringUtils.containsOnly(str1, chars3));
        assertEquals("containsOnly(str2, chars1) success", true, StringUtils.containsOnly(str2, chars1));
        assertEquals("containsOnly(str2, chars2) failed", false, StringUtils.containsOnly(str2, chars2));
        assertEquals("containsOnly(str2, chars3) success", true, StringUtils.containsOnly(str2, chars3));
        assertEquals("containsOnly(String3, chars1) failed", false, StringUtils.containsOnly(str3, chars1));
        assertEquals("containsOnly(String3, chars2) failed", false, StringUtils.containsOnly(str3, chars2));
        assertEquals("containsOnly(String3, chars3) success", true, StringUtils.containsOnly(str3, chars3));
    }

    public void testContainsNoneString() {
        String str1 = "a";
        String str2 = "b";
        String str3 = "ab.";
        String chars1= "b";
        String chars2= ".";
        String chars3= "cd";
        String emptyChars = "";
        assertEquals("containsNone(null, null) failed", true, StringUtils.containsNone(null, (String) null));
        assertEquals("containsNone(empty-string, null) failed", true, StringUtils.containsNone("", (String) null));
        assertEquals("containsNone(null, empty-string) failed", true, StringUtils.containsNone(null, emptyChars));
        assertEquals("containsNone(str1, empty-char-array) failed", true, StringUtils.containsNone(str1, emptyChars));
        assertEquals("containsNone(empty-string, empty-char-array) failed", true, StringUtils.containsNone("", emptyChars));
        assertEquals("containsNone(empty-string, chars1) failed", true, StringUtils.containsNone("", chars1));
        assertEquals("containsNone(str1, chars1)", true, StringUtils.containsNone(str1, chars1));
        assertEquals("containsNone(str1, chars2)", true, StringUtils.containsNone(str1, chars2));
        assertEquals("containsNone(str1, chars3)", true, StringUtils.containsNone(str1, chars3));
        assertEquals("containsNone(str2, chars1)", false, StringUtils.containsNone(str2, chars1));
        assertEquals("containsNone(str2, chars2)", true, StringUtils.containsNone(str2, chars2));
        assertEquals("containsNone(str2, chars3)", true, StringUtils.containsNone(str2, chars3));
        assertEquals("containsNone(str3, chars1)", false, StringUtils.containsNone(str3, chars1));
        assertEquals("containsNone(str3, chars2)", false, StringUtils.containsNone(str3, chars2));
        assertEquals("containsNone(str3, chars3)", true, StringUtils.containsNone(str3, chars3));
    }

    public void testContainsNoneCharArray() {
        String str1 = "a";
        String str2 = "b";
        String str3 = "ab.";
        char[] chars1= {'b'};
        char[] chars2= {'.'};
        char[] chars3= {'c', 'd'};
        char[] emptyChars = new char[0];
        assertEquals("containsNone(null, null) failed", true, StringUtils.containsNone(null, (char[]) null));
        assertEquals("containsNone(empty-string, null) failed", true, StringUtils.containsNone("", (char[]) null));
        assertEquals("containsNone(null, empty-string) failed", true, StringUtils.containsNone(null, emptyChars));
        assertEquals("containsNone(str1, empty-char-array) failed", true, StringUtils.containsNone(str1, emptyChars));
        assertEquals("containsNone(empty-string, empty-char-array) failed", true, StringUtils.containsNone("", emptyChars));
        assertEquals("containsNone(empty-string, chars1) failed", true, StringUtils.containsNone("", chars1));
        assertEquals("containsNone(str1, chars1)", true, StringUtils.containsNone(str1, chars1));
        assertEquals("containsNone(str1, chars2)", true, StringUtils.containsNone(str1, chars2));
        assertEquals("containsNone(str1, chars3)", true, StringUtils.containsNone(str1, chars3));
        assertEquals("containsNone(str2, chars1)", false, StringUtils.containsNone(str2, chars1));
        assertEquals("containsNone(str2, chars2)", true, StringUtils.containsNone(str2, chars2));
        assertEquals("containsNone(str2, chars3)", true, StringUtils.containsNone(str2, chars3));
        assertEquals("containsNone(str3, chars1)", false, StringUtils.containsNone(str3, chars1));
        assertEquals("containsNone(str3, chars2)", false, StringUtils.containsNone(str3, chars2));
        assertEquals("containsNone(str3, chars3)", true, StringUtils.containsNone(str3, chars3));
    }

    public void testIndexOfAnyBut() {
        String str1 = "a";
        String str2 = "b";
        String str3 = "ab";
        String chars1= "b";
        String chars2= "a";
        String chars3= "ab";
        String emptyChars = "";
        assertEquals("indexOfAnyBut(null, null)", -1, StringUtils.indexOfAnyBut(null, (String) null));
        assertEquals("indexOfAnyBut(empty-string, null)", -1, StringUtils.indexOfAnyBut("", (String) null));
        assertEquals("indexOfAnyBut(null, empty-string)", -1, StringUtils.indexOfAnyBut(null, emptyChars));
        assertEquals("indexOfAnyBut(str1, empty-char-array)", 0, StringUtils.indexOfAnyBut(str1, emptyChars));
        assertEquals("indexOfAnyBut(empty-string, empty-char-array)", -1, StringUtils.indexOfAnyBut("", emptyChars));
        assertEquals("indexOfAnyBut(empty-string, chars1)", -1, StringUtils.indexOfAnyBut("", chars1));
        assertEquals("indexOfAnyBut(str1, chars1)", 0, StringUtils.indexOfAnyBut(str1, chars1));
        assertEquals("indexOfAnyBut(str1, chars2)", -1, StringUtils.indexOfAnyBut(str1, chars2));
        assertEquals("indexOfAnyBut(str1, chars3)", -1, StringUtils.indexOfAnyBut(str1, chars3));
        assertEquals("indexOfAnyBut(str2, chars1)", -1, StringUtils.indexOfAnyBut(str2, chars1));
        assertEquals("indexOfAnyBut(str2, chars2)", 0, StringUtils.indexOfAnyBut(str2, chars2));
        assertEquals("indexOfAnyBut(str2, chars3)", -1, StringUtils.indexOfAnyBut(str2, chars3));
        assertEquals("indexOfAnyBut(String3, chars1)", 0, StringUtils.indexOfAnyBut(str3, chars1));
        assertEquals("indexOfAnyBut(String3, chars2)", 1, StringUtils.indexOfAnyBut(str3, chars2));
        assertEquals("indexOfAnyBut(String3, chars3)", -1, StringUtils.indexOfAnyBut(str3, chars3));
    }

    public void testAbbreviate()
    {
        assertEquals("abbreviate(String,int) failed",
		     "short", StringUtils.abbreviate("short", 10));
        assertEquals("abbreviate(String,int) failed",
		     "Now is ...", StringUtils.abbreviate("Now is the time for all good men to come to the aid of their party.", 10));

        String raspberry = "raspberry peach";
        assertEquals("abbreviate(String,int) failed (one past limit)",
		     "raspberry p...", StringUtils.abbreviate(raspberry, 14));
        assertEquals("abbreviate(String,int) (at limit)",
		     "raspberry peach", StringUtils.abbreviate("raspberry peach", 15));
        assertEquals("abbreviate(String,int) (one below limit)",
		     "raspberry peach", StringUtils.abbreviate("raspberry peach", 16));

        assertEquals("abbreviate(String,int,int) failed",
                "raspberry peach", StringUtils.abbreviate(raspberry, 11, 15));

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

    private void assertAbbreviateWithOffset(String expected, int offset, int maxWidth)
    {
        String abcdefghijklmno = "abcdefghijklmno";
        String message = "abbreviate(String,int,int) failed";
        String actual = StringUtils.abbreviate(abcdefghijklmno, offset, maxWidth);
        if (offset >= 0 && offset < abcdefghijklmno.length()) {
            assertTrue(message + " -- should contain offset character",
                    actual.indexOf((char)('a'+offset)) != -1);
        }
        assertTrue(message + " -- should not be greater than maxWidth",
                actual.length() <= maxWidth);
        assertEquals(message, expected, actual);
    }

    public void testDifference()
    {
        assertEquals("robot", StringUtils.difference("i am a machine", "i am a robot"));
        assertEquals("", StringUtils.difference("foo", "foo"));
        assertEquals("you are a robot", StringUtils.difference("i am a robot", "you are a robot"));
    }

    public void testDifferenceAt()
    {
        assertEquals(7, StringUtils.differenceAt("i am a machine", "i am a robot"));
        assertEquals(-1, StringUtils.differenceAt("foo", "foo"));
        assertEquals(0, StringUtils.differenceAt("i am a robot", "you are a robot"));
    }

}

