package org.apache.commons.lang;

/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002 The Apache Software Foundation.  All rights
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
 * @author <a href="mailto:scolebourne@joda.org">Stephen Colebourne</a>
 * @author <a href="mailto:ridesmet@users.sourceforge.net">Ringo De Smet</a>
 * @version $Id: StringUtilsTest.java,v 1.1 2002/07/19 03:35:55 bayard Exp $
 */
public class StringUtilsTest extends TestCase
{
    private static final String[] ARRAY_LIST = { "foo", "bar", "baz" };

    private static final String SEPARATOR = ",";

    private static final String TEXT_LIST = "foo,bar,baz";

    private static final String FOO = "foo";
    private static final String BAR = "bar";
    private static final String CAP_FOO = "Foo";
    private static final String UPPER_FOO = "FOO";

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

    public void testCaseFunctions()
    {
        assertEquals("capitalise(String) failed",
                     CAP_FOO, StringUtils.capitalise(FOO) );
        assertEquals("capitalise(empty-string) failed",
                     "", StringUtils.capitalise("") );
        assertEquals("capitaliseAllWords(String) failed",
                     "Foo Bar Baz", StringUtils.capitaliseAllWords(SENTENCE) );
        assertEquals("capitaliseAllWords(empty-string) failed",
                     "", StringUtils.capitaliseAllWords("") );
        assertEquals("uncapitalise(String) failed",
                     FOO, StringUtils.uncapitalise(CAP_FOO) );
        assertEquals("uncapitalise(empty-string) failed",
                     "", StringUtils.uncapitalise("") );

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

    public void testJoin()
    {
        assertEquals("join(Object[], String) failed", TEXT_LIST,
                     StringUtils.join(ARRAY_LIST, SEPARATOR));
        assertEquals("join(Iterator, String) failed", TEXT_LIST,
                     StringUtils.join(Arrays.asList(ARRAY_LIST).iterator(),
                                      SEPARATOR));
    }

    public void testSplit()
    {
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
    }

    public void testReplaceFunctions()
    {
        assertEquals("replace(String, String, String, int) failed",
                     FOO, StringUtils.replace("oo" + FOO, "o", "", 2));
        assertEquals("replace(String, String, String) failed",
                     "", StringUtils.replace(FOO + FOO + FOO, FOO, ""));
        assertEquals("replaceOnce(String, String, String) failed",
                     FOO, StringUtils.replaceOnce(FOO + FOO, FOO, ""));
    }

    public void testOverlayString()
    {
        assertEquals("overlayString(String, String, int, int) failed",
                     "foo foor baz", StringUtils.overlayString(SENTENCE, FOO, 4, 6) );
    }

    public void testRepeat()
    {
        assertEquals("repeat(String, int) failed",
                     FOO + FOO + FOO, StringUtils.repeat(FOO, 3) );
    }

    public void testCenter()
    {
        assertEquals("center(String, int) failed",
                     "   "+FOO+"   ", StringUtils.center(FOO, 9) );
    }

    public void testChompFunctions()
    {

        assertEquals("chomp(String) failed",
                     FOO, StringUtils.chomp(FOO + "\n" + FOO) );

        assertEquals("chompLast(String) failed",
                     FOO, StringUtils.chompLast(FOO + "\n") );

        assertEquals("getChomp(String, String) failed",
                     "\n" + FOO, StringUtils.getChomp(FOO + "\n" + FOO, "\n") );

        assertEquals("prechomp(String, String) failed",
                     FOO, StringUtils.prechomp(FOO + "\n" + FOO, "\n") );

        assertEquals("getPrechomp(String, String) failed",
                     FOO + "\n", StringUtils.getPrechomp(FOO + "\n" + FOO, "\n") );

        assertEquals("chop(String, String) failed",
                     FOO, StringUtils.chop(FOO + "\r\n") );

        assertEquals("chopNewline(String, String) failed",
                     FOO, StringUtils.chopNewline(FOO + "\r\n") );
    }

    public void testPadFunctions()
    {
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

    public void testUnicodeFunctions() throws java.io.IOException
    {
        /* this test fails on my window box with an Sun english JDK 1.3.1
           I think that the input string is not right
        */
/* Kept out for the moment.
        String input = "これは日本楔譴離謄好箸任后#";
        String unicode = StringUtils.convertNativeToUnicode(input, "iso-2022-jp");
        String iso = StringUtils.convertUnicodeToNative(unicode, "iso-2022-jp");
        assertEquals("Unicode conversions failed", input, iso);
*/
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
                     "\\u00fd", StringUtils.escape("\u00fd") );
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

}

