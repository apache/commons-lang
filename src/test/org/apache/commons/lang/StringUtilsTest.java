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
import java.util.Iterator;

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
 * @author Phil Steitz
 * @version $Id: StringUtilsTest.java,v 1.38 2003/07/25 00:50:00 scolebourne Exp $
 */
public class StringUtilsTest extends TestCase {
    
    static final String WHITESPACE;
    static final String NON_WHITESPACE;
    static final String TRIMMABLE;
    static final String NON_TRIMMABLE;
    static {
        String ws = "";
        String nws = "";
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
        TRIMMABLE = tr;
        NON_TRIMMABLE = ntr;
    }

    private static final String[] ARRAY_LIST = { "foo", "bar", "baz" };
    private static final String[] EMPTY_ARRAY_LIST = {};
    private static final String[] NULL_ARRAY_LIST = {null};
    private static final String[] MIXED_ARRAY_LIST = {null, "", "foo"};
    private static final Object[] MIXED_TYPE_LIST = {new String("foo"), new Long(2)};

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
        assertEquals(null, StringUtils.upperCase(null));
        assertEquals(null, StringUtils.lowerCase(null));
        assertEquals(null, StringUtils.swapCase(null));
        assertEquals(null, StringUtils.capitalise(null));
        assertEquals(null, StringUtils.uncapitalise(null));
        assertEquals(null, StringUtils.capitaliseAllWords(null));
        assertEquals(null, StringUtils.uncapitaliseAllWords(null));

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

    public void testJoin_ArrayChar() {
        assertEquals(null, StringUtils.join((Object[]) null, ','));
        assertEquals(TEXT_LIST_CHAR, StringUtils.join(ARRAY_LIST, SEPARATOR_CHAR));
        assertEquals("", StringUtils.join(EMPTY_ARRAY_LIST, SEPARATOR_CHAR));
        assertEquals(";;foo", StringUtils.join(MIXED_ARRAY_LIST, SEPARATOR_CHAR));
        assertEquals("foo;2", StringUtils.join(MIXED_TYPE_LIST, SEPARATOR_CHAR));
    }
    
    public void testJoin_ArrayString() {
        assertEquals(null, StringUtils.join((Object[]) null, null));
        assertEquals(TEXT_LIST_NOSEP, StringUtils.join(ARRAY_LIST, null));
        assertEquals(TEXT_LIST_NOSEP, StringUtils.join(ARRAY_LIST, ""));
        
        assertEquals("", StringUtils.join(NULL_ARRAY_LIST, null));
        
        assertEquals("", StringUtils.join(EMPTY_ARRAY_LIST, null));
        assertEquals("", StringUtils.join(EMPTY_ARRAY_LIST, ""));
        assertEquals("", StringUtils.join(EMPTY_ARRAY_LIST, SEPARATOR));

        assertEquals(TEXT_LIST, StringUtils.join(ARRAY_LIST, SEPARATOR));
        assertEquals(",,foo", StringUtils.join(MIXED_ARRAY_LIST, SEPARATOR));
        assertEquals("foo,2", StringUtils.join(MIXED_TYPE_LIST, SEPARATOR));
    }
    
    public void testJoin_IteratorChar() {
        assertEquals(null, StringUtils.join((Iterator) null, ','));
        assertEquals(TEXT_LIST_CHAR, StringUtils.join(Arrays.asList(ARRAY_LIST).iterator(), SEPARATOR_CHAR));
        assertEquals("", StringUtils.join(Arrays.asList(EMPTY_ARRAY_LIST).iterator(), SEPARATOR_CHAR));
    }
    
    public void testJoin_IteratorString() {
        assertEquals(null, StringUtils.join((Iterator) null, null));
        assertEquals(TEXT_LIST_NOSEP, StringUtils.join(Arrays.asList(ARRAY_LIST).iterator(), null));
        assertEquals(TEXT_LIST_NOSEP, StringUtils.join(Arrays.asList(ARRAY_LIST).iterator(), ""));
        
        assertEquals("", StringUtils.join(Arrays.asList(NULL_ARRAY_LIST).iterator(), null));
        
        assertEquals("", StringUtils.join(Arrays.asList(EMPTY_ARRAY_LIST).iterator(), null));
        assertEquals("", StringUtils.join(Arrays.asList(EMPTY_ARRAY_LIST).iterator(), ""));
        assertEquals("", StringUtils.join(Arrays.asList(EMPTY_ARRAY_LIST).iterator(), SEPARATOR));
        
        assertEquals(TEXT_LIST, StringUtils.join(Arrays.asList(ARRAY_LIST).iterator(), SEPARATOR));
    }
    
    public void testConcatenate_Array() {
        assertEquals(null, StringUtils.concatenate(null));
        assertEquals("", StringUtils.concatenate(EMPTY_ARRAY_LIST));
        assertEquals("", StringUtils.concatenate(NULL_ARRAY_LIST));
        assertEquals("foo", StringUtils.concatenate(MIXED_ARRAY_LIST));
        assertEquals("foo2", StringUtils.concatenate(MIXED_TYPE_LIST));
    }
        
        

    public void testSplit_String() {
        assertEquals(null, StringUtils.split(null));
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
    
    public void testSplit_StringChar() {
        assertEquals(null, StringUtils.split(null, '.'));
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
        res = StringUtils.split(str,' ');
        assertEquals(3, res.length);
        assertEquals("a", res[0]);
        assertEquals("b", res[1]);
        assertEquals("c", res[2]);
    }
    
    public void testSplit_StringString_StringStringInt() {
        assertEquals(null, StringUtils.split(null, "."));
        assertEquals(null, StringUtils.split(null, ".", 3));
        
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
    }
    
    private void innerTestSplit(char separator, String sepStr, char noMatch) {
        String msg = "Failed on separator hex(" + Integer.toHexString(separator) +
            "), noMatch hex(" + Integer.toHexString(noMatch) + "), sepStr(" + sepStr + ")";
        
        final String str = "a" + separator + "b" + separator + separator + noMatch + "c";
        String[] res;
        // (str, sepStr)
        res = StringUtils.split(str, sepStr);
        assertEquals(msg, 3, res.length);
        assertEquals(msg, "a", res[0]);
        assertEquals(msg, "b", res[1]);
        assertEquals(msg, noMatch + "c", res[2]);
        
        final String str2 = separator + "a" + separator;
        res = StringUtils.split(str2, sepStr);
        assertEquals(msg, 1, res.length);
        assertEquals(msg, "a", res[0]);

        res = StringUtils.split(str, sepStr, -1);
        assertEquals(msg, 3, res.length);
        assertEquals(msg, "a", res[0]);
        assertEquals(msg, "b", res[1]);
        assertEquals(msg, noMatch + "c", res[2]);
        
        res = StringUtils.split(str, sepStr, 0);
        assertEquals(msg, 3, res.length);
        assertEquals(msg, "a", res[0]);
        assertEquals(msg, "b", res[1]);
        assertEquals(msg, noMatch + "c", res[2]);
        
        res = StringUtils.split(str, sepStr, 1);
        assertEquals(msg, 1, res.length);
        assertEquals(msg, str, res[0]);
        
        res = StringUtils.split(str, sepStr, 2);
        assertEquals(msg, 2, res.length);
        assertEquals(msg, "a", res[0]);
        assertEquals(msg, str.substring(2), res[1]);
    }

    public void testDeleteSpace_String() {
        assertEquals(null, StringUtils.deleteSpaces(null));
        assertEquals("", StringUtils.deleteSpaces(""));
        assertEquals("", StringUtils.deleteSpaces("    \t\t\n\n   "));
        assertEquals("test", StringUtils.deleteSpaces("t  \t\ne\rs\n\n   \tt"));
    }
    
    public void testDeleteWhitespace_String() {
        assertEquals(null, StringUtils.deleteWhitespace(null));
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

    public void testReplace_StringStringString() {
        assertEquals(null, StringUtils.replace(null, null, null));
        assertEquals(null, StringUtils.replace(null, null, "any"));
        assertEquals(null, StringUtils.replace(null, "any", null));
        assertEquals(null, StringUtils.replace(null, "any", "any"));

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
    
    public void testReplace_StringStringStringInt() {
        assertEquals(null, StringUtils.replace(null, null, null, 2));
        assertEquals(null, StringUtils.replace(null, null, "any", 2));
        assertEquals(null, StringUtils.replace(null, "any", null, 2));
        assertEquals(null, StringUtils.replace(null, "any", "any", 2));

        assertEquals("f", StringUtils.replace("oofoo", "o", "", -1));
        assertEquals("oofoo", StringUtils.replace("oofoo", "o", "", 0));
        assertEquals("ofoo", StringUtils.replace("oofoo", "o", "", 1));
        assertEquals("foo", StringUtils.replace("oofoo", "o", "", 2));
        assertEquals("fo", StringUtils.replace("oofoo", "o", "", 3));
        assertEquals("f", StringUtils.replace("oofoo", "o", "", 4));
    }
    
    public void testReplaceOnce_StringStringString() {
        assertEquals(null, StringUtils.replaceOnce(null, null, null));
        assertEquals(null, StringUtils.replaceOnce(null, null, "any"));
        assertEquals(null, StringUtils.replaceOnce(null, "any", null));
        assertEquals(null, StringUtils.replaceOnce(null, "any", "any"));

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

    public void testOverlayString() {
        assertEquals("overlayString(String, String, int, int) failed",
                     "foo foor baz", StringUtils.overlayString(SENTENCE, FOO, 4, 6) );
        assertEquals(null, StringUtils.overlayString(null, null, 2, 4));
        assertEquals("abef", StringUtils.overlayString("abcdef", null, 2, 4));
        assertEquals("abef", StringUtils.overlayString("abcdef", "", 2, 4));
        assertEquals("abzzzzef", StringUtils.overlayString("abcdef", "zzzz", 2, 4));
        assertEquals("abcdzzzzcdef", StringUtils.overlayString("abcdef", "zzzz", 4, 2));
        try {
            StringUtils.overlayString("abcdef", "zzzz", -1, 4);
            fail();
        } catch (IndexOutOfBoundsException ex) {}
        try {
            StringUtils.overlayString("abcdef", "zzzz", 2, 8);
            fail();
        } catch (IndexOutOfBoundsException ex) {}
    }

    public void testRepeat_StringInt() {
        assertEquals(null, StringUtils.repeat(null, 2));
        assertEquals("", StringUtils.repeat("ab", 0));
        assertEquals("", StringUtils.repeat("", 3));
        assertEquals("aaa", StringUtils.repeat("a", 3));
        assertEquals("ababab", StringUtils.repeat("ab", 3));
        assertEquals("abcabcabc", StringUtils.repeat("abc", 3));
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
            { FOO + " \r", FOO + " " },
            { "foo", "fo"},
            { "foo\nfoo", "foo\nfo" },
            { "\n", "" },
            { "\r", "" },
            { "\r\n", "" },
            { null, null },
            { "", "" },
            { "a", "" },
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
            { FOO + "\r\n", FOO },
            { FOO + "\n" , FOO },
            { FOO + "\r", FOO },
            { FOO + " \r", FOO + " " },
            { FOO, FOO },
            { FOO + "\n\n", FOO + "\n"},
            { FOO + "\r\n\r\n", FOO + "\r\n" },
            { "foo\nfoo", "foo\nfoo" },
            { "foo\n\rfoo", "foo\n\rfoo" },
            { "\n", "" },
            { "\r", "" },
            { "\r\n", "" },
            { "", "" },
            { null, null },
            { FOO + "\n\r", FOO + "\n"}
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
        assertEquals("chomp(String, String) failed",
                "foobar", StringUtils.chomp("foobar", ""));
        assertEquals("chomp(String, String) failed",
                "foobar", StringUtils.chomp("foobar", null));
        assertEquals("chomp(String, String) failed",
                "", StringUtils.chomp("", "foo"));
        assertEquals("chomp(String, String) failed",
                "", StringUtils.chomp("", null));
        assertEquals("chomp(String, String) failed",
                "", StringUtils.chomp("", ""));
        assertEquals("chomp(String, String) failed",
                null, StringUtils.chomp(null, "foo"));
        assertEquals("chomp(String, String) failed",
                null, StringUtils.chomp(null, null));
        assertEquals("chomp(String, String) failed",
                null, StringUtils.chomp(null, ""));
        assertEquals("chomp(String, String) failed",
                "", StringUtils.chomp("foo", "foo"));
        assertEquals("chomp(String, String) failed",
                " ", StringUtils.chomp(" foo", "foo"));
        assertEquals("chomp(String, String) failed",
                "foo ", StringUtils.chomp("foo ", "foo"));
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
            {null, null},
            {"", ""},
            {"\n", ""},
            {"abc \n", "abc "},
            {"abc\r\n", "abc\r"},
            {"foo", "foo"},
        };
        for (int i = 0; i < sliceCases.length; i++) {
            String original = sliceCases[i][0];
            String expectedResult = sliceCases[i][1];
            assertEquals("slice(String) failed",
                    expectedResult, StringUtils.slice(original));
        }
    }
    
    public void testSlice_StringString() {
        assertEquals("fooXXbar", StringUtils.slice("fooXXbarXXbaz", "XX"));

        assertEquals(null, StringUtils.slice(null, null));
        assertEquals(null, StringUtils.slice(null, ""));
        assertEquals(null, StringUtils.slice(null, "XX"));
        assertEquals("", StringUtils.slice("", null));
        assertEquals("", StringUtils.slice("", ""));
        assertEquals("", StringUtils.slice("", "XX"));

        assertEquals("foo", StringUtils.slice("foo", null));
        assertEquals("foo", StringUtils.slice("foo", "b"));
        assertEquals("fo", StringUtils.slice("foo", "o"));
        assertEquals("abc\r\n", StringUtils.slice("abc\r\n", "d"));
        assertEquals("abc", StringUtils.slice("abcdabc", "d"));
        assertEquals("abcdabc", StringUtils.slice("abcdabcd", "d"));
        assertEquals("a", StringUtils.slice("abc", "b"));
        assertEquals("abc ", StringUtils.slice("abc \n", "\n"));
        assertEquals("a", StringUtils.slice("a", null));
        assertEquals("a", StringUtils.slice("a", ""));
        assertEquals("", StringUtils.slice("a", "a"));
    }
    
    public void testSliceRemainder_StringString() {
        assertEquals("baz", StringUtils.sliceRemainder("fooXXbarXXbaz", "XX"));

        assertEquals(null, StringUtils.sliceRemainder(null, null));
        assertEquals(null, StringUtils.sliceRemainder(null, ""));
        assertEquals(null, StringUtils.sliceRemainder(null, "XX"));
        assertEquals("", StringUtils.sliceRemainder("", null));
        assertEquals("", StringUtils.sliceRemainder("", ""));
        assertEquals("", StringUtils.sliceRemainder("", "a"));

        assertEquals("", StringUtils.sliceRemainder("foo", null));
        assertEquals("", StringUtils.sliceRemainder("foo", "b"));
        assertEquals("t", StringUtils.sliceRemainder("foot", "o"));
        assertEquals("bc", StringUtils.sliceRemainder("abc", "a"));
        assertEquals("a", StringUtils.sliceRemainder("abcba", "b"));
        assertEquals("", StringUtils.sliceRemainder("abc", "c"));
        assertEquals("", StringUtils.sliceRemainder("", "d"));
        assertEquals("", StringUtils.sliceRemainder("abc", ""));
    }        
        
    public void testSliceFirst_StringString() {
        assertEquals("foo", StringUtils.sliceFirst("fooXXbarXXbaz", "XX"));

        assertEquals(null, StringUtils.sliceFirst(null, null));
        assertEquals(null, StringUtils.sliceFirst(null, ""));
        assertEquals(null, StringUtils.sliceFirst(null, "XX"));
        assertEquals("", StringUtils.sliceFirst("", null));
        assertEquals("", StringUtils.sliceFirst("", ""));
        assertEquals("", StringUtils.sliceFirst("", "XX"));
        
        assertEquals("foo", StringUtils.sliceFirst("foo", null));
        assertEquals("foo", StringUtils.sliceFirst("foo", "b"));
        assertEquals("f", StringUtils.sliceFirst("foot", "o"));
        assertEquals("", StringUtils.sliceFirst("abc", "a"));
        assertEquals("a", StringUtils.sliceFirst("abcba", "b"));
        assertEquals("ab", StringUtils.sliceFirst("abc", "c"));
        assertEquals("", StringUtils.sliceFirst("abc", ""));
    }
    
    public void testSliceFirstRemainder_StringString() {
        assertEquals("barXXbaz", StringUtils.sliceFirstRemainder("fooXXbarXXbaz", "XX"));
        
        assertEquals(null, StringUtils.sliceFirstRemainder(null, null));
        assertEquals(null, StringUtils.sliceFirstRemainder(null, ""));
        assertEquals(null, StringUtils.sliceFirstRemainder(null, "XX"));
        assertEquals("", StringUtils.sliceFirstRemainder("", null));
        assertEquals("", StringUtils.sliceFirstRemainder("", ""));
        assertEquals("", StringUtils.sliceFirstRemainder("", "XX"));
        
        assertEquals("", StringUtils.sliceFirstRemainder("foo", null));
        assertEquals("ot", StringUtils.sliceFirstRemainder("foot", "o"));
        assertEquals("bc", StringUtils.sliceFirstRemainder("abc", "a"));
        assertEquals("cba", StringUtils.sliceFirstRemainder("abcba", "b"));
        assertEquals("", StringUtils.sliceFirstRemainder("abc", "c"));
        assertEquals("abc", StringUtils.sliceFirstRemainder("abc", ""));
        assertEquals("", StringUtils.sliceFirstRemainder("abc", "d"));
    }

    //-----------------------------------------------------------------------
    public void testRightPad_StringInt() {
        assertEquals(null, StringUtils.rightPad(null, 5));
        assertEquals("     ", StringUtils.rightPad("", 5));
        assertEquals("abc  ", StringUtils.rightPad("abc", 5));
        assertEquals("abc", StringUtils.rightPad("abc", 2));
        assertEquals("abc", StringUtils.rightPad("abc", -1));
    }

    public void testRightPad_StringIntChar() {
        assertEquals(null, StringUtils.rightPad(null, 5, ' '));
        assertEquals("     ", StringUtils.rightPad("", 5, ' '));
        assertEquals("abc  ", StringUtils.rightPad("abc", 5, ' '));
        assertEquals("abc", StringUtils.rightPad("abc", 2, ' '));
        assertEquals("abc", StringUtils.rightPad("abc", -1, ' '));
        assertEquals("abcxx", StringUtils.rightPad("abc", 5, 'x'));
    }

    public void testRightPad_StringIntString() {
        assertEquals(null, StringUtils.rightPad(null, 5, "-+"));
        assertEquals("     ", StringUtils.rightPad("", 5, " "));
        assertEquals(null, StringUtils.rightPad(null, 8, null));
        assertEquals("abc-+-+", StringUtils.rightPad("abc", 7, "-+"));
        assertEquals("abc-+~", StringUtils.rightPad("abc", 6, "-+~"));
        assertEquals("abc-+", StringUtils.rightPad("abc", 5, "-+~"));
        assertEquals("abc", StringUtils.rightPad("abc", 2, " "));
        assertEquals("abc", StringUtils.rightPad("abc", -1, " "));
        try {
            StringUtils.rightPad("abc56", 6, null);
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            StringUtils.rightPad("abc56", 6, "");
            fail();
        } catch (IllegalArgumentException ex) {}
    }
        
    //-----------------------------------------------------------------------
    public void testLeftPad_StringInt() {
        assertEquals(null, StringUtils.leftPad(null, 5));
        assertEquals("     ", StringUtils.leftPad("", 5));
        assertEquals("  abc", StringUtils.leftPad("abc", 5));
        assertEquals("abc", StringUtils.leftPad("abc", 2));
    }
        
    public void testLeftPad_StringIntChar() {
        assertEquals(null, StringUtils.leftPad(null, 5, ' '));
        assertEquals("     ", StringUtils.leftPad("", 5, ' '));
        assertEquals("  abc", StringUtils.leftPad("abc", 5, ' '));
        assertEquals("xxabc", StringUtils.leftPad("abc", 5, 'x'));
        assertEquals("abc", StringUtils.leftPad("abc", 2, ' '));
    }
        
    public void testLeftPad_StringIntString() {
        assertEquals(null, StringUtils.leftPad(null, 5, "-+"));
        assertEquals(null, StringUtils.leftPad(null, 5, null));
        assertEquals("     ", StringUtils.leftPad("", 5, " "));
        assertEquals("-+-+abc", StringUtils.leftPad("abc", 7, "-+"));
        assertEquals("-+~abc", StringUtils.leftPad("abc", 6, "-+~"));
        assertEquals("-+abc", StringUtils.leftPad("abc", 5, "-+~"));
        assertEquals("abc", StringUtils.leftPad("abc", 2, " "));
        assertEquals("abc", StringUtils.leftPad("abc", -1, " "));
        try {
            StringUtils.leftPad("abc56", 6, null);
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            StringUtils.leftPad("abc56", 6, "");
            fail();
        } catch (IllegalArgumentException ex) {}
    }

    //-----------------------------------------------------------------------
    public void testCenter_StringInt() {
        assertEquals(null, StringUtils.center(null, -1));
        assertEquals(null, StringUtils.center(null, 4));
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
    
    public void testCenter_StringIntChar() {
        assertEquals(null, StringUtils.center(null, -1, ' '));
        assertEquals(null, StringUtils.center(null, 4, ' '));
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
    
    public void testCenter_StringIntString() {
        assertEquals(null, StringUtils.center(null, 4, null));
        assertEquals(null, StringUtils.center(null, -1, " "));
        assertEquals(null, StringUtils.center(null, 4, " "));
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
        try {
            StringUtils.center("abc", 4, null);
            fail();
        } catch (IllegalArgumentException ex) {
        }
        try {
            StringUtils.center("abc", 4, "");
            fail();
        } catch (IllegalArgumentException ex) {
        }
    }

    //-----------------------------------------------------------------------
    public void testReverse_String() {
        assertEquals(null, StringUtils.reverse(null) );
        assertEquals("", StringUtils.reverse("") );
        assertEquals("sdrawkcab", StringUtils.reverse("backwards") );
    }
        
    public void testReverseDelimited_StringChar() {
        assertEquals(null, StringUtils.reverseDelimited(null, '.') );
        assertEquals("", StringUtils.reverseDelimited("", '.') );
        assertEquals("c.b.a", StringUtils.reverseDelimited("a.b.c", '.') );
        assertEquals("a b c", StringUtils.reverseDelimited("a b c", '.') );
        assertEquals("", StringUtils.reverseDelimited("", '.') );
    }

    public void testReverseDelimitedString_StringString() {
        assertEquals(null, StringUtils.reverseDelimitedString(null, null) );
        assertEquals("", StringUtils.reverseDelimitedString("", null) );
        assertEquals("", StringUtils.reverseDelimitedString("", ".") );
        assertEquals("a.b.c", StringUtils.reverseDelimitedString("a.b.c", null) );
        assertEquals("c b a", StringUtils.reverseDelimitedString("a b c", null) );
        assertEquals("c.b.a", StringUtils.reverseDelimitedString("a.b.c", ".") );
    }

    //-----------------------------------------------------------------------
    public void testDefault_String() {
        assertEquals("", StringUtils.defaultString(null) );
        assertEquals("", StringUtils.defaultString("") );
        assertEquals("abc", StringUtils.defaultString("abc") );
    }
            
    public void testDefault_StringString() {
        assertEquals("xyz", StringUtils.defaultString(null, "xyz") );
        assertEquals("", StringUtils.defaultString("", "xyz") );
        assertEquals("abc", StringUtils.defaultString("abc", "xyz") );
    }
    
    //-----------------------------------------------------------------------
    public void testEscapeFunctions_String() {
        assertEquals("", StringUtils.escape("") );
        assertEquals("abc", StringUtils.escape("abc") );
        assertEquals("\\t", StringUtils.escape("\t") );
        assertEquals("\\\\", StringUtils.escape("\\") );
        assertEquals("\\\\\\b\\t\\r", StringUtils.escape("\\\b\t\r") );
        assertEquals("\\u1234", StringUtils.escape("\u1234") );
        assertEquals("\\u0234", StringUtils.escape("\u0234") );
        assertEquals("\\u00FD", StringUtils.escape("\u00fd") );
    }

    //-----------------------------------------------------------------------
    public void testAbbreviate_StringInt() {
        assertEquals(null, StringUtils.abbreviate(null, 10));
        assertEquals("", StringUtils.abbreviate("", 10));
        assertEquals("short", StringUtils.abbreviate("short", 10));
        assertEquals("Now is ...", StringUtils.abbreviate("Now is the time for all good men to come to the aid of their party.", 10));

        String raspberry = "raspberry peach";
        assertEquals("raspberry p...", StringUtils.abbreviate(raspberry, 14));
        assertEquals("raspberry peach", StringUtils.abbreviate("raspberry peach", 15));
        assertEquals("raspberry peach", StringUtils.abbreviate("raspberry peach", 16));
        assertEquals("abc...", StringUtils.abbreviate("abcdefg", 6));
        assertEquals("abcdefg", StringUtils.abbreviate("abcdefg", 7));
        assertEquals("abcdefg", StringUtils.abbreviate("abcdefg", 8));
        assertEquals("a...", StringUtils.abbreviate("abcdefg", 4));
        assertEquals("", StringUtils.abbreviate("", 4));
        
        try {
            String res = StringUtils.abbreviate("abc", 3);
            fail("StringUtils.abbreviate expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
                // empty
        }              
    }
    
    public void testAbbreviate_StringIntInt() {
        assertEquals(null, StringUtils.abbreviate(null, 10, 12));
        assertEquals("", StringUtils.abbreviate("", 0, 10));
        assertEquals("", StringUtils.abbreviate("", 2, 10));
        
        try {
            String res = StringUtils.abbreviate("abcdefghij", 0, 3);
            fail("StringUtils.abbreviate expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
                // empty
        }      
        try {
            String res = StringUtils.abbreviate("abcdefghij", 5, 6);
            fail("StringUtils.abbreviate expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
                // empty
        }      
        

        String raspberry = "raspberry peach";
        assertEquals("raspberry peach", StringUtils.abbreviate(raspberry, 11, 15));

        assertEquals(null, StringUtils.abbreviate(null, 7, 14));
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

    private void assertAbbreviateWithOffset(String expected, int offset, int maxWidth) {
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

    //-----------------------------------------------------------------------
    public void testDifference_StringString() {
        assertEquals(null, StringUtils.difference(null, null));
        assertEquals("", StringUtils.difference("", ""));
        assertEquals("abc", StringUtils.difference("", "abc"));
        assertEquals("", StringUtils.difference("abc", ""));
        assertEquals("i am a robot", StringUtils.difference(null, "i am a robot"));
        assertEquals("i am a machine", StringUtils.difference("i am a machine", null));
        assertEquals("robot", StringUtils.difference("i am a machine", "i am a robot"));
        assertEquals("", StringUtils.difference("abc", "abc"));
        assertEquals("you are a robot", StringUtils.difference("i am a robot", "you are a robot"));
    }

    public void testDifferenceAt_StringString() {
        assertEquals(-1, StringUtils.differenceAt(null, null));
        assertEquals(0, StringUtils.differenceAt(null, "i am a robot"));
        assertEquals(-1, StringUtils.differenceAt("", ""));
        assertEquals(0, StringUtils.differenceAt("", "abc"));
        assertEquals(0, StringUtils.differenceAt("abc", ""));
        assertEquals(0, StringUtils.differenceAt("i am a machine", null));
        assertEquals(7, StringUtils.differenceAt("i am a machine", "i am a robot"));
        assertEquals(-1, StringUtils.differenceAt("foo", "foo"));
        assertEquals(0, StringUtils.differenceAt("i am a robot", "you are a robot"));
    }

    //-----------------------------------------------------------------------
    public void testGetLevenshteinDistance_StringString() {
        assertEquals(0, StringUtils.getLevenshteinDistance("", "") );
        assertEquals(1, StringUtils.getLevenshteinDistance("", "a") );
        assertEquals(7, StringUtils.getLevenshteinDistance("aaapppp", "") );
        assertEquals(1, StringUtils.getLevenshteinDistance("frog", "fog") );
        assertEquals(3, StringUtils.getLevenshteinDistance("fly", "ant") );
        assertEquals(7, StringUtils.getLevenshteinDistance("elephant", "hippo") );
        assertEquals(7, StringUtils.getLevenshteinDistance("hippo", "elephant") );
        assertEquals(8, StringUtils.getLevenshteinDistance("hippo", "zzzzzzzz") );
        assertEquals(8, StringUtils.getLevenshteinDistance("zzzzzzzz", "hippo") );
        assertEquals(1, StringUtils.getLevenshteinDistance("hello", "hallo") );
        try {
            int d = StringUtils.getLevenshteinDistance("a", null);
            fail("expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            // empty
        }
        try {
            int d = StringUtils.getLevenshteinDistance(null, "a");
            fail("expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            // empty
        }
    }

}

