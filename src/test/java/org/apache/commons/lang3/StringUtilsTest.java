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

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.nio.CharBuffer;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.Locale;

import junit.framework.TestCase;

import org.apache.commons.lang3.text.WordUtils;

/**
 * Unit tests {@link org.apache.commons.lang3.StringUtils}.
 *
 * @author Apache Software Foundation
 * @author Daniel L. Rall
 * @author <a href="mailto:ridesmet@users.sourceforge.net">Ringo De Smet</a>
 * @author <a href="mailto:fredrik@westermarck.com>Fredrik Westermarck</a>
 * @author Holger Krauth
 * @author <a href="hps@intermeta.de">Henning P. Schmiedehausen</a>
 * @author Phil Steitz
 * @author Gary D. Gregory
 * @author Scott Johnson
 * @author Al Chou
 * @version $Id$
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

    private static final String FOO_UNCAP = "foo";
    private static final String FOO_CAP = "Foo";

    private static final String SENTENCE_UNCAP = "foo bar baz";
    private static final String SENTENCE_CAP = "Foo Bar Baz";

    public StringUtilsTest(String name) {
        super(name);
    }

    //-----------------------------------------------------------------------
    public void testConstructor() {
        assertNotNull(new StringUtils());
        Constructor<?>[] cons = StringUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertEquals(true, Modifier.isPublic(cons[0].getModifiers()));
        assertEquals(true, Modifier.isPublic(StringUtils.class.getModifiers()));
        assertEquals(false, Modifier.isFinal(StringUtils.class.getModifiers()));
    }
    
    //-----------------------------------------------------------------------
    public void testCaseFunctions() {
        assertEquals(null, StringUtils.upperCase(null));
        assertEquals(null, StringUtils.upperCase(null, Locale.ENGLISH));
        assertEquals(null, StringUtils.lowerCase(null));
        assertEquals(null, StringUtils.lowerCase(null, Locale.ENGLISH));
        assertEquals(null, StringUtils.capitalize(null));
        assertEquals(null, StringUtils.uncapitalize(null));

        assertEquals("capitalize(empty-string) failed",
                     "", StringUtils.capitalize("") );
        assertEquals("capitalize(single-char-string) failed",
                "X", StringUtils.capitalize("x") );
        assertEquals("capitalize(single-char-string) failed",
                "X", StringUtils.capitalize(new StringBuilder("x")) );
        assertEquals("capitalize(single-char-string) failed",
                "X", StringUtils.capitalize(new StringBuffer("x")) );
        assertEquals("capitalize(single-char-string) failed",
                "X", StringUtils.capitalize(CharBuffer.wrap("x")) );
        
        assertEquals("uncapitalize(String) failed",
                     FOO_UNCAP, StringUtils.uncapitalize(FOO_CAP) );
        assertEquals("uncapitalize(empty-string) failed",
                     "", StringUtils.uncapitalize("") );
        assertEquals("uncapitalize(single-char-string) failed",
                "x", StringUtils.uncapitalize("X") );
        assertEquals("uncapitalize(single-char-string) failed",
                "x", StringUtils.uncapitalize(new StringBuilder("X")) );
        assertEquals("uncapitalize(single-char-string) failed",
                "x", StringUtils.uncapitalize(new StringBuffer("X")) );
        assertEquals("uncapitalize(single-char-string) failed",
                "x", StringUtils.uncapitalize(CharBuffer.wrap("X")) );
                     
        // reflection type of tests: Sentences.
        assertEquals("uncapitalize(capitalize(String)) failed",
                     SENTENCE_UNCAP, StringUtils.uncapitalize(StringUtils.capitalize(SENTENCE_UNCAP)) );
        assertEquals("capitalize(uncapitalize(String)) failed",
                     SENTENCE_CAP, StringUtils.capitalize(StringUtils.uncapitalize(SENTENCE_CAP)) );

        // reflection type of tests: One word.
        assertEquals("uncapitalize(capitalize(String)) failed",
                     FOO_UNCAP, StringUtils.uncapitalize(StringUtils.capitalize(FOO_UNCAP)) );
        assertEquals("capitalize(uncapitalize(String)) failed",
                     FOO_CAP, StringUtils.capitalize(StringUtils.uncapitalize(FOO_CAP)) );

        assertEquals("upperCase(String) failed",
                     "FOO TEST THING", StringUtils.upperCase("fOo test THING") );
        assertEquals("upperCase(empty-string) failed",
                     "", StringUtils.upperCase("") );
        assertEquals("lowerCase(String) failed",
                     "foo test thing", StringUtils.lowerCase("fOo test THING") );
        assertEquals("lowerCase(empty-string) failed",
                     "", StringUtils.lowerCase("") );

        assertEquals("upperCase(String, Locale) failed",
                     "FOO TEST THING", StringUtils.upperCase("fOo test THING", Locale.ENGLISH) );
        assertEquals("upperCase(empty-string, Locale) failed",
                     "", StringUtils.upperCase("", Locale.ENGLISH) );
        assertEquals("lowerCase(String, Locale) failed",
                     "foo test thing", StringUtils.lowerCase("fOo test THING", Locale.ENGLISH) );
        assertEquals("lowerCase(empty-string, Locale) failed",
                     "", StringUtils.lowerCase("", Locale.ENGLISH) );
    }

    public void testSwapCase_String() {
        assertEquals(null, StringUtils.swapCase(null));
        assertEquals("", StringUtils.swapCase(""));
        assertEquals("  ", StringUtils.swapCase("  "));
        
        assertEquals("i", WordUtils.swapCase("I") );
        assertEquals("I", WordUtils.swapCase("i") );
        assertEquals("I AM HERE 123", StringUtils.swapCase("i am here 123") );
        assertEquals("i aM hERE 123", StringUtils.swapCase("I Am Here 123") );
        assertEquals("I AM here 123", StringUtils.swapCase("i am HERE 123") );
        assertEquals("i am here 123", StringUtils.swapCase("I AM HERE 123") );
        
        String test = "This String contains a TitleCase character: \u01C8";
        String expect = "tHIS sTRING CONTAINS A tITLEcASE CHARACTER: \u01C9";
        assertEquals(expect, WordUtils.swapCase(test));
    }

    //-----------------------------------------------------------------------
    public void testConcat_Objects() {
        assertEquals("abc", StringUtils.concat("a", "b", "c"));
        assertEquals("a",   StringUtils.concat(null, "", "a"));
        assertEquals(null,  StringUtils.concat(null));
    }

    public void testConcatWith_StringObjects() {
        assertEquals("a.b.c", StringUtils.concatWith(".", "a", "b", "c"));
        assertEquals("a...b...c", StringUtils.concatWith("...", "a", "b", "c"));
        assertEquals("a",     StringUtils.concatWith("", null, "", "a"));
        assertEquals("a",     StringUtils.concatWith(null, null, "", "a"));
        assertEquals(null,    StringUtils.concatWith(null, null));
    }

    public void testJoin_Objectarray() {
        assertEquals(null, StringUtils.join(null));
        assertEquals("", StringUtils.join(EMPTY_ARRAY_LIST));
        assertEquals("", StringUtils.join(NULL_ARRAY_LIST));
        assertEquals("abc", StringUtils.join(new String[] {"a", "b", "c"}));
        assertEquals("a", StringUtils.join(new String[] {null, "a", ""}));
        assertEquals("foo", StringUtils.join(MIXED_ARRAY_LIST));
        assertEquals("foo2", StringUtils.join(MIXED_TYPE_LIST));
    }
        
    public void testJoin_ArrayChar() {
        assertEquals(null, StringUtils.join((Object[]) null, ','));
        assertEquals(TEXT_LIST_CHAR, StringUtils.join(ARRAY_LIST, SEPARATOR_CHAR));
        assertEquals("", StringUtils.join(EMPTY_ARRAY_LIST, SEPARATOR_CHAR));
        assertEquals(";;foo", StringUtils.join(MIXED_ARRAY_LIST, SEPARATOR_CHAR));
        assertEquals("foo;2", StringUtils.join(MIXED_TYPE_LIST, SEPARATOR_CHAR));

        assertEquals("/", StringUtils.join(MIXED_ARRAY_LIST, '/', 0, MIXED_ARRAY_LIST.length-1));
        assertEquals("foo", StringUtils.join(MIXED_TYPE_LIST, '/', 0, 1));
        assertEquals("foo/2", StringUtils.join(MIXED_TYPE_LIST, '/', 0, 2));
        assertEquals("2", StringUtils.join(MIXED_TYPE_LIST, '/', 1, 2));
        assertEquals("", StringUtils.join(MIXED_TYPE_LIST, '/', 2, 1));
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

        assertEquals("/", StringUtils.join(MIXED_ARRAY_LIST, "/", 0, MIXED_ARRAY_LIST.length-1));
        assertEquals("", StringUtils.join(MIXED_ARRAY_LIST, "", 0, MIXED_ARRAY_LIST.length-1));
        assertEquals("foo", StringUtils.join(MIXED_TYPE_LIST, "/", 0, 1));
        assertEquals("foo/2", StringUtils.join(MIXED_TYPE_LIST, "/", 0, 2));
        assertEquals("2", StringUtils.join(MIXED_TYPE_LIST, "/", 1, 2));
        assertEquals("", StringUtils.join(MIXED_TYPE_LIST, "/", 2, 1));
    }
    
    public void testJoin_IteratorChar() {
        assertEquals(null, StringUtils.join((Iterator<?>) null, ','));
        assertEquals(TEXT_LIST_CHAR, StringUtils.join(Arrays.asList(ARRAY_LIST).iterator(), SEPARATOR_CHAR));
        assertEquals("", StringUtils.join(Arrays.asList(NULL_ARRAY_LIST).iterator(), SEPARATOR_CHAR));
        assertEquals("", StringUtils.join(Arrays.asList(EMPTY_ARRAY_LIST).iterator(), SEPARATOR_CHAR));
        assertEquals("foo", StringUtils.join(Collections.singleton("foo").iterator(), 'x'));
    }
    
    public void testJoin_IteratorString() {
        assertEquals(null, StringUtils.join((Iterator<?>) null, null));
        assertEquals(TEXT_LIST_NOSEP, StringUtils.join(Arrays.asList(ARRAY_LIST).iterator(), null));
        assertEquals(TEXT_LIST_NOSEP, StringUtils.join(Arrays.asList(ARRAY_LIST).iterator(), ""));
        assertEquals("foo", StringUtils.join(Collections.singleton("foo").iterator(), "x"));
        assertEquals("foo", StringUtils.join(Collections.singleton("foo").iterator(), null));

        assertEquals("", StringUtils.join(Arrays.asList(NULL_ARRAY_LIST).iterator(), null));
        
        assertEquals("", StringUtils.join(Arrays.asList(EMPTY_ARRAY_LIST).iterator(), null));
        assertEquals("", StringUtils.join(Arrays.asList(EMPTY_ARRAY_LIST).iterator(), ""));
        assertEquals("", StringUtils.join(Arrays.asList(EMPTY_ARRAY_LIST).iterator(), SEPARATOR));
        
        assertEquals(TEXT_LIST, StringUtils.join(Arrays.asList(ARRAY_LIST).iterator(), SEPARATOR));
    }

    public void testJoin_IterableChar() {
        assertEquals(null, StringUtils.join((Iterable<?>) null, ','));
        assertEquals(TEXT_LIST_CHAR, StringUtils.join(Arrays.asList(ARRAY_LIST), SEPARATOR_CHAR));
        assertEquals("", StringUtils.join(Arrays.asList(NULL_ARRAY_LIST), SEPARATOR_CHAR));
        assertEquals("", StringUtils.join(Arrays.asList(EMPTY_ARRAY_LIST), SEPARATOR_CHAR));
        assertEquals("foo", StringUtils.join(Collections.singleton("foo"), 'x'));
    }

    public void testJoin_IterableString() {
        assertEquals(null, StringUtils.join((Iterable<?>) null, null));
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
        
        String[] results = null;
        String[] expectedResults = {"ab", "de fg"};
        results = StringUtils.split("ab   de fg", null, 2);
        assertEquals(expectedResults.length, results.length);
        for (int i = 0; i < expectedResults.length; i++) {
            assertEquals(expectedResults[i], results[i]);
        }
        
        String[] expectedResults2 = {"ab", "cd:ef"};
        results = StringUtils.split("ab:cd:ef",":", 2);
        assertEquals(expectedResults2.length, results.length);
        for (int i = 0; i < expectedResults2.length; i++) {
            assertEquals(expectedResults2[i], results[i]);
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

    public void testSplitByWholeString_StringStringBoolean() {
        assertEquals( null, StringUtils.splitByWholeSeparator( null, "." ) ) ;

        assertEquals( 0, StringUtils.splitByWholeSeparator( "", "." ).length ) ;

        String stringToSplitOnNulls = "ab   de fg" ;
        String[] splitOnNullExpectedResults = { "ab", "de", "fg" } ;

        String[] splitOnNullResults = StringUtils.splitByWholeSeparator( stringToSplitOnNulls, null ) ;
        assertEquals( splitOnNullExpectedResults.length, splitOnNullResults.length ) ;
        for ( int i = 0 ; i < splitOnNullExpectedResults.length ; i+= 1 ) {
            assertEquals( splitOnNullExpectedResults[i], splitOnNullResults[i] ) ;
        }

        String stringToSplitOnCharactersAndString = "abstemiouslyaeiouyabstemiously" ;

        String[] splitOnStringExpectedResults = { "abstemiously", "abstemiously" } ;
        String[] splitOnStringResults = StringUtils.splitByWholeSeparator( stringToSplitOnCharactersAndString, "aeiouy" ) ;
        assertEquals( splitOnStringExpectedResults.length, splitOnStringResults.length ) ;
        for ( int i = 0 ; i < splitOnStringExpectedResults.length ; i+= 1 ) {
            assertEquals( splitOnStringExpectedResults[i], splitOnStringResults[i] ) ;
        }

        String[] splitWithMultipleSeparatorExpectedResults = {"ab", "cd", "ef"};
        String[] splitWithMultipleSeparator = StringUtils.splitByWholeSeparator("ab:cd::ef", ":");
        assertEquals( splitWithMultipleSeparatorExpectedResults.length, splitWithMultipleSeparator.length );
        for( int i = 0; i < splitWithMultipleSeparatorExpectedResults.length ; i++ ) {
            assertEquals( splitWithMultipleSeparatorExpectedResults[i], splitWithMultipleSeparator[i] ) ;
        }
    }

    public void testSplitByWholeString_StringStringBooleanInt() {
        assertEquals( null, StringUtils.splitByWholeSeparator( null, ".", 3 ) ) ;

        assertEquals( 0, StringUtils.splitByWholeSeparator( "", ".", 3 ).length ) ;

        String stringToSplitOnNulls = "ab   de fg" ;
        String[] splitOnNullExpectedResults = { "ab", "de fg" } ;
        //String[] splitOnNullExpectedResults = { "ab", "de" } ;

        String[] splitOnNullResults = StringUtils.splitByWholeSeparator( stringToSplitOnNulls, null, 2 ) ;
        assertEquals( splitOnNullExpectedResults.length, splitOnNullResults.length ) ;
        for ( int i = 0 ; i < splitOnNullExpectedResults.length ; i+= 1 ) {
            assertEquals( splitOnNullExpectedResults[i], splitOnNullResults[i] ) ;
        }

        String stringToSplitOnCharactersAndString = "abstemiouslyaeiouyabstemiouslyaeiouyabstemiously" ;

        String[] splitOnStringExpectedResults = { "abstemiously", "abstemiouslyaeiouyabstemiously" } ;
        //String[] splitOnStringExpectedResults = { "abstemiously", "abstemiously" } ;
        String[] splitOnStringResults = StringUtils.splitByWholeSeparator( stringToSplitOnCharactersAndString, "aeiouy", 2 ) ;
        assertEquals( splitOnStringExpectedResults.length, splitOnStringResults.length ) ;
        for ( int i = 0 ; i < splitOnStringExpectedResults.length ; i++ ) {
            assertEquals( splitOnStringExpectedResults[i], splitOnStringResults[i] ) ;
        }
    }

    public void testSplitByWholeSeparatorPreserveAllTokens_StringStringInt() {
        assertEquals( null, StringUtils.splitByWholeSeparatorPreserveAllTokens( null, ".", -1 ) ) ;

        assertEquals( 0, StringUtils.splitByWholeSeparatorPreserveAllTokens( "", ".", -1 ).length ) ;

        // test whitespace
        String input = "ab   de fg" ;
        String[] expected = new String[] { "ab", "", "", "de", "fg" } ;

        String[] actual = StringUtils.splitByWholeSeparatorPreserveAllTokens( input, null, -1 ) ;
        assertEquals( expected.length, actual.length ) ;
        for ( int i = 0 ; i < actual.length ; i+= 1 ) {
            assertEquals( expected[i], actual[i] );
        }

        // test delimiter singlechar
        input = "1::2:::3::::4";
        expected = new String[] { "1", "", "2", "", "", "3", "", "", "", "4" };

        actual = StringUtils.splitByWholeSeparatorPreserveAllTokens( input, ":", -1 ) ;
        assertEquals( expected.length, actual.length ) ;
        for ( int i = 0 ; i < actual.length ; i+= 1 ) {
            assertEquals( expected[i], actual[i] );
        }

        // test delimiter multichar
        input = "1::2:::3::::4";
        expected = new String[] { "1", "2", ":3", "", "4" };

        actual = StringUtils.splitByWholeSeparatorPreserveAllTokens( input, "::", -1 ) ;
        assertEquals( expected.length, actual.length ) ;
        for ( int i = 0 ; i < actual.length ; i+= 1 ) {
            assertEquals( expected[i], actual[i] );
        }

        // test delimiter char with max
        input = "1::2::3:4";
        expected = new String[] { "1", "", "2", ":3:4" };

        actual = StringUtils.splitByWholeSeparatorPreserveAllTokens( input, ":", 4 ) ;
        assertEquals( expected.length, actual.length ) ;
        for ( int i = 0 ; i < actual.length ; i+= 1 ) {
            assertEquals( expected[i], actual[i] );
        }
    }
    
    public void testSplitPreserveAllTokens_String() {
        assertEquals(null, StringUtils.splitPreserveAllTokens(null));
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
        for(int i = 1; i < WHITESPACE.length()-1; i++)
        {
          assertEquals("", res[i]);
        }
        assertEquals("b" + NON_WHITESPACE + "c", res[WHITESPACE.length()]);                       
    }
    
    public void testSplitPreserveAllTokens_StringChar() {
        assertEquals(null, StringUtils.splitPreserveAllTokens(null, '.'));
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
        res = StringUtils.splitPreserveAllTokens(str,' ');
        assertEquals(3, res.length);
        assertEquals("a", res[0]);
        assertEquals("b", res[1]);
        assertEquals("c", res[2]);

        str = "a  b  c";
        res = StringUtils.splitPreserveAllTokens(str,' ');
        assertEquals(5, res.length);
        assertEquals("a", res[0]);
        assertEquals("", res[1]);
        assertEquals("b", res[2]);
        assertEquals("", res[3]);
        assertEquals("c", res[4]);
        
        str = " a b c";
        res = StringUtils.splitPreserveAllTokens(str,' ');
        assertEquals(4, res.length);
        assertEquals("", res[0]);
        assertEquals("a", res[1]);
        assertEquals("b", res[2]);
        assertEquals("c", res[3]);

        str = "  a b c";
        res = StringUtils.splitPreserveAllTokens(str,' ');
        assertEquals(5, res.length);
        assertEquals("", res[0]);
        assertEquals("", res[1]);
        assertEquals("a", res[2]);
        assertEquals("b", res[3]);
        assertEquals("c", res[4]);

        str = "a b c ";
        res = StringUtils.splitPreserveAllTokens(str,' ');
        assertEquals(4, res.length);
        assertEquals("a", res[0]);
        assertEquals("b", res[1]);
        assertEquals("c", res[2]);
        assertEquals("", res[3]);

        str = "a b c  ";
        res = StringUtils.splitPreserveAllTokens(str,' ');
        assertEquals(5, res.length);
        assertEquals("a", res[0]);
        assertEquals("b", res[1]);
        assertEquals("c", res[2]);
        assertEquals("", res[3]);
        assertEquals("", res[3]);

        // Match example in javadoc
        {
          String[] results = null;
          String[] expectedResults = {"a", "", "b", "c"};
          results = StringUtils.splitPreserveAllTokens("a..b.c",'.');
          assertEquals(expectedResults.length, results.length);
          for (int i = 0; i < expectedResults.length; i++) {
              assertEquals(expectedResults[i], results[i]);
          }
        }
    }
    
    public void testSplitPreserveAllTokens_StringString_StringStringInt() {
        assertEquals(null, StringUtils.splitPreserveAllTokens(null, "."));
        assertEquals(null, StringUtils.splitPreserveAllTokens(null, ".", 3));
        
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
          String[] results = null;
          String[] expectedResults = {"ab", "de fg"};
          results = StringUtils.splitPreserveAllTokens("ab de fg", null, 2);
          assertEquals(expectedResults.length, results.length);
          for (int i = 0; i < expectedResults.length; i++) {
              assertEquals(expectedResults[i], results[i]);
          }
        }

        {
          String[] results = null;
          String[] expectedResults = {"ab", "  de fg"};
          results = StringUtils.splitPreserveAllTokens("ab   de fg", null, 2);
          assertEquals(expectedResults.length, results.length);
          for (int i = 0; i < expectedResults.length; i++) {
              assertEquals(expectedResults[i], results[i]);
          }
        }
        
        {
          String[] results = null;
          String[] expectedResults = {"ab", "::de:fg"};
          results = StringUtils.splitPreserveAllTokens("ab:::de:fg", ":", 2);
          assertEquals(expectedResults.length, results.length);
          for (int i = 0; i < expectedResults.length; i++) {
              assertEquals(expectedResults[i], results[i]);
          }
        }
        
        {
          String[] results = null;
          String[] expectedResults = {"ab", "", " de fg"};
          results = StringUtils.splitPreserveAllTokens("ab   de fg", null, 3);
          assertEquals(expectedResults.length, results.length);
          for (int i = 0; i < expectedResults.length; i++) {
              assertEquals(expectedResults[i], results[i]);
          }
        }
        
        {
          String[] results = null;
          String[] expectedResults = {"ab", "", "", "de fg"};
          results = StringUtils.splitPreserveAllTokens("ab   de fg", null, 4);
          assertEquals(expectedResults.length, results.length);
          for (int i = 0; i < expectedResults.length; i++) {
              assertEquals(expectedResults[i], results[i]);
          }
        }

        {
          String[] expectedResults = {"ab", "cd:ef"};
          String[] results = null;
          results = StringUtils.splitPreserveAllTokens("ab:cd:ef",":", 2);
          assertEquals(expectedResults.length, results.length);
          for (int i = 0; i < expectedResults.length; i++) {
              assertEquals(expectedResults[i], results[i]);
          }
        }

        {
          String[] results = null;
          String[] expectedResults = {"ab", ":cd:ef"};
          results = StringUtils.splitPreserveAllTokens("ab::cd:ef",":", 2);
          assertEquals(expectedResults.length, results.length);
          for (int i = 0; i < expectedResults.length; i++) {
              assertEquals(expectedResults[i], results[i]);
          }
        }

        {
          String[] results = null;
          String[] expectedResults = {"ab", "", ":cd:ef"};
          results = StringUtils.splitPreserveAllTokens("ab:::cd:ef",":", 3);
          assertEquals(expectedResults.length, results.length);
          for (int i = 0; i < expectedResults.length; i++) {
              assertEquals(expectedResults[i], results[i]);
          }
        }

        {
          String[] results = null;
          String[] expectedResults = {"ab", "", "", "cd:ef"};
          results = StringUtils.splitPreserveAllTokens("ab:::cd:ef",":", 4);
          assertEquals(expectedResults.length, results.length);
          for (int i = 0; i < expectedResults.length; i++) {
              assertEquals(expectedResults[i], results[i]);
          }
        }

        {
          String[] results = null;
          String[] expectedResults = {"", "ab", "", "", "cd:ef"};
          results = StringUtils.splitPreserveAllTokens(":ab:::cd:ef",":", 5);
          assertEquals(expectedResults.length, results.length);
          for (int i = 0; i < expectedResults.length; i++) {
              assertEquals(expectedResults[i], results[i]);
          }
        }
        
        {
          String[] results = null;
          String[] expectedResults = {"", "", "ab", "", "", "cd:ef"};
          results = StringUtils.splitPreserveAllTokens("::ab:::cd:ef",":", 6);
          assertEquals(expectedResults.length, results.length);
          for (int i = 0; i < expectedResults.length; i++) {
              assertEquals(expectedResults[i], results[i]);
          }
        }
        
    }
    
    private void innerTestSplitPreserveAllTokens(char separator, String sepStr, char noMatch) {
        String msg = "Failed on separator hex(" + Integer.toHexString(separator) +
            "), noMatch hex(" + Integer.toHexString(noMatch) + "), sepStr(" + sepStr + ")";
        
        final String str = "a" + separator + "b" + separator + separator + noMatch + "c";
        String[] res;
        // (str, sepStr)
        res = StringUtils.splitPreserveAllTokens(str, sepStr);
        assertEquals(msg, 4, res.length);
        assertEquals(msg, "a", res[0]);
        assertEquals(msg, "b", res[1]);
        assertEquals(msg, "", res[2]);
        assertEquals(msg, noMatch + "c", res[3]);
        
        final String str2 = separator + "a" + separator;
        res = StringUtils.splitPreserveAllTokens(str2, sepStr);
        assertEquals(msg, 3, res.length);
        assertEquals(msg, "", res[0]);
        assertEquals(msg, "a", res[1]);
        assertEquals(msg, "", res[2]);

        res = StringUtils.splitPreserveAllTokens(str, sepStr, -1);
        assertEquals(msg, 4, res.length);
        assertEquals(msg, "a", res[0]);
        assertEquals(msg, "b", res[1]);
        assertEquals(msg, "", res[2]);
        assertEquals(msg, noMatch + "c", res[3]);
        
        res = StringUtils.splitPreserveAllTokens(str, sepStr, 0);
        assertEquals(msg, 4, res.length);
        assertEquals(msg, "a", res[0]);
        assertEquals(msg, "b", res[1]);
        assertEquals(msg, "", res[2]);
        assertEquals(msg, noMatch + "c", res[3]);
        
        res = StringUtils.splitPreserveAllTokens(str, sepStr, 1);
        assertEquals(msg, 1, res.length);
        assertEquals(msg, str, res[0]);
        
        res = StringUtils.splitPreserveAllTokens(str, sepStr, 2);
        assertEquals(msg, 2, res.length);
        assertEquals(msg, "a", res[0]);
        assertEquals(msg, str.substring(2), res[1]);
    }

    public void testSplitByCharacterType() {
        assertNull(StringUtils.splitByCharacterType(null));
        assertEquals(0, StringUtils.splitByCharacterType("").length);
        
        assertTrue(ArrayUtils.isEquals(new String[] { "ab", " ", "de", " ",
        "fg" }, StringUtils.splitByCharacterType("ab de fg")));
        
        assertTrue(ArrayUtils.isEquals(new String[] { "ab", "   ", "de", " ",
        "fg" }, StringUtils.splitByCharacterType("ab   de fg")));
        
        assertTrue(ArrayUtils.isEquals(new String[] { "ab", ":", "cd", ":",
        "ef" }, StringUtils.splitByCharacterType("ab:cd:ef")));
        
        assertTrue(ArrayUtils.isEquals(new String[] { "number", "5" },
                StringUtils.splitByCharacterType("number5")));
        
        assertTrue(ArrayUtils.isEquals(new String[] { "foo", "B", "ar" },
                StringUtils.splitByCharacterType("fooBar")));
        
        assertTrue(ArrayUtils.isEquals(new String[] { "foo", "200", "B", "ar" },
                StringUtils.splitByCharacterType("foo200Bar")));
        
        assertTrue(ArrayUtils.isEquals(new String[] { "ASFR", "ules" },
                StringUtils.splitByCharacterType("ASFRules")));
    }
    
    public void testSplitByCharacterTypeCamelCase() {
        assertNull(StringUtils.splitByCharacterTypeCamelCase(null));
        assertEquals(0, StringUtils.splitByCharacterTypeCamelCase("").length);

        assertTrue(ArrayUtils.isEquals(new String[] { "ab", " ", "de", " ",
                "fg" }, StringUtils.splitByCharacterTypeCamelCase("ab de fg")));

        assertTrue(ArrayUtils.isEquals(new String[] { "ab", "   ", "de", " ",
                "fg" }, StringUtils.splitByCharacterTypeCamelCase("ab   de fg")));

        assertTrue(ArrayUtils.isEquals(new String[] { "ab", ":", "cd", ":",
                "ef" }, StringUtils.splitByCharacterTypeCamelCase("ab:cd:ef")));
        
        assertTrue(ArrayUtils.isEquals(new String[] { "number", "5" },
                StringUtils.splitByCharacterTypeCamelCase("number5")));

        assertTrue(ArrayUtils.isEquals(new String[] { "foo", "Bar" },
                StringUtils.splitByCharacterTypeCamelCase("fooBar")));

        assertTrue(ArrayUtils.isEquals(new String[] { "foo", "200", "Bar" },
                StringUtils.splitByCharacterTypeCamelCase("foo200Bar")));

        assertTrue(ArrayUtils.isEquals(new String[] { "ASF", "Rules" },
                StringUtils.splitByCharacterTypeCamelCase("ASFRules")));
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

        assertEquals("", StringUtils.replace("", null, null, 2));
        assertEquals("", StringUtils.replace("", null, "any", 2));
        assertEquals("", StringUtils.replace("", "any", null, 2));
        assertEquals("", StringUtils.replace("", "any", "any", 2));
        
        String str = new String(new char[] {'o', 'o', 'f', 'o', 'o'});
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

    /**
     * Test method for 'StringUtils.replaceEach(String, String[], String[])'
     */
    public void testReplace_StringStringArrayStringArray() {

        
        //JAVADOC TESTS START
        assertNull(StringUtils.replaceEach(null, new String[]{"a"}, new String[]{"b"}));
        assertEquals(StringUtils.replaceEach("", new String[]{"a"}, new String[]{"b"}),"");
        assertEquals(StringUtils.replaceEach("aba", null, null),"aba");
        assertEquals(StringUtils.replaceEach("aba", new String[0], null),"aba");
        assertEquals(StringUtils.replaceEach("aba", null, new String[0]),"aba");
        assertEquals(StringUtils.replaceEach("aba", new String[]{"a"}, null),"aba");

        assertEquals(StringUtils.replaceEach("aba", new String[]{"a"}, new String[]{""}),"b");
        assertEquals(StringUtils.replaceEach("aba", new String[]{null}, new String[]{"a"}),"aba");
        assertEquals(StringUtils.replaceEach("abcde", new String[]{"ab", "d"}, new String[]{"w", "t"}),"wcte");
        assertEquals(StringUtils.replaceEach("abcde", new String[]{"ab", "d"}, new String[]{"d", "t"}),"dcte");
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
        assertEquals(StringUtils.replaceEach("aba", new String[]{"a"}, new String[]{null}),"aba");
        assertEquals(StringUtils.replaceEach("aba", new String[]{"a", "b"}, new String[]{"c", null}),"cbc");
    }

    /**
     * Test method for 'StringUtils.replaceEachRepeatedly(String, String[], String[])'
     */
    public void testReplace_StringStringArrayStringArrayBoolean() {
        //JAVADOC TESTS START
        assertNull(StringUtils.replaceEachRepeatedly(null, new String[]{"a"}, new String[]{"b"}));
        assertEquals(StringUtils.replaceEachRepeatedly("", new String[]{"a"}, new String[]{"b"}),"");
        assertEquals(StringUtils.replaceEachRepeatedly("aba", null, null),"aba");
        assertEquals(StringUtils.replaceEachRepeatedly("aba", new String[0], null),"aba");
        assertEquals(StringUtils.replaceEachRepeatedly("aba", null, new String[0]),"aba");
        assertEquals(StringUtils.replaceEachRepeatedly("aba", new String[0], null),"aba");

        assertEquals(StringUtils.replaceEachRepeatedly("aba", new String[]{"a"}, new String[]{""}),"b");
        assertEquals(StringUtils.replaceEachRepeatedly("aba", new String[]{null}, new String[]{"a"}),"aba");
        assertEquals(StringUtils.replaceEachRepeatedly("abcde", new String[]{"ab", "d"}, new String[]{"w", "t"}),"wcte");
        assertEquals(StringUtils.replaceEachRepeatedly("abcde", new String[]{"ab", "d"}, new String[]{"d", "t"}),"tcte");

        try {
            StringUtils.replaceEachRepeatedly("abcde", new String[]{"ab", "d"}, new String[]{"d", "ab"});
            fail("Should be a circular reference");
        } catch (IllegalStateException e) {}

        //JAVADOC TESTS END

    }
    
    public void testReplaceChars_StringCharChar() {
        assertEquals(null, StringUtils.replaceChars(null, 'b', 'z'));
        assertEquals("", StringUtils.replaceChars("", 'b', 'z'));
        assertEquals("azcza", StringUtils.replaceChars("abcba", 'b', 'z'));
        assertEquals("abcba", StringUtils.replaceChars("abcba", 'x', 'z'));
    }
    
    public void testReplaceChars_StringStringString() {
        assertEquals(null, StringUtils.replaceChars(null, null, null));
        assertEquals(null, StringUtils.replaceChars(null, "", null));
        assertEquals(null, StringUtils.replaceChars(null, "a", null));
        assertEquals(null, StringUtils.replaceChars(null, null, ""));
        assertEquals(null, StringUtils.replaceChars(null, null, "x"));
        
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
        
        // From http://issues.apache.org/bugzilla/show_bug.cgi?id=25454
        assertEquals("bcc", StringUtils.replaceChars("abc", "ab", "bc"));
        assertEquals("q651.506bera", StringUtils.replaceChars("d216.102oren",
            "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ123456789",
            "nopqrstuvwxyzabcdefghijklmNOPQRSTUVWXYZABCDEFGHIJKLM567891234"));
    }
    
    public void testOverlay_StringStringIntInt() {
        assertEquals(null, StringUtils.overlay(null, null, 2, 4));
        assertEquals(null, StringUtils.overlay(null, null, -2, -4));
        
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

    public void testRepeat_StringInt() {
        assertEquals(null, StringUtils.repeat(null, 2));
        assertEquals("", StringUtils.repeat("ab", 0));
        assertEquals("", StringUtils.repeat("", 3));
        assertEquals("aaa", StringUtils.repeat("a", 3));
        assertEquals("ababab", StringUtils.repeat("ab", 3));
        assertEquals("abcabcabc", StringUtils.repeat("abc", 3));
        String str = StringUtils.repeat("a", 10000);  // bigger than pad limit
        assertEquals(10000, str.length());
        assertEquals(true, StringUtils.containsOnly(str, new char[] {'a'}));
    }

    public void testRepeat_StringStringInt() {
        assertEquals(null, StringUtils.repeat(null, null, 2));
        assertEquals(null, StringUtils.repeat(null, "x", 2));
        assertEquals("", StringUtils.repeat("", null, 2));

        assertEquals("", StringUtils.repeat("ab", "", 0));
        assertEquals("", StringUtils.repeat("", "", 2));

        assertEquals("xx", StringUtils.repeat("", "x", 3));

        assertEquals("?, ?, ?", StringUtils.repeat("?", ", ", 3));
    }

    public void testChop() {

        String[][] chopCases = {
            { FOO_UNCAP + "\r\n", FOO_UNCAP } ,
            { FOO_UNCAP + "\n" , FOO_UNCAP } ,
            { FOO_UNCAP + "\r", FOO_UNCAP },
            { FOO_UNCAP + " \r", FOO_UNCAP + " " },
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
            { FOO_UNCAP + "\r\n", FOO_UNCAP },
            { FOO_UNCAP + "\n" , FOO_UNCAP },
            { FOO_UNCAP + "\r", FOO_UNCAP },
            { FOO_UNCAP + " \r", FOO_UNCAP + " " },
            { FOO_UNCAP, FOO_UNCAP },
            { FOO_UNCAP + "\n\n", FOO_UNCAP + "\n"},
            { FOO_UNCAP + "\r\n\r\n", FOO_UNCAP + "\r\n" },
            { "foo\nfoo", "foo\nfoo" },
            { "foo\n\rfoo", "foo\n\rfoo" },
            { "\n", "" },
            { "\r", "" },
            { "a", "a" },
            { "\r\n", "" },
            { "", "" },
            { null, null },
            { FOO_UNCAP + "\n\r", FOO_UNCAP + "\n"}
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
        String str = StringUtils.rightPad("aaa", 10000, 'a');  // bigger than pad length
        assertEquals(10000, str.length());
        assertEquals(true, StringUtils.containsOnly(str, new char[] {'a'}));
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
        assertEquals("abc  ", StringUtils.rightPad("abc", 5, null));
        assertEquals("abc  ", StringUtils.rightPad("abc", 5, ""));
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
        assertEquals("\uffff\uffffabc", StringUtils.leftPad("abc", 5, '\uffff'));
        assertEquals("abc", StringUtils.leftPad("abc", 2, ' '));
        String str = StringUtils.leftPad("aaa", 10000, 'a');  // bigger than pad length
        assertEquals(10000, str.length());
        assertEquals(true, StringUtils.containsOnly(str, new char[] {'a'}));
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
        assertEquals("  abc", StringUtils.leftPad("abc", 5, null));
        assertEquals("  abc", StringUtils.leftPad("abc", 5, ""));
    }

    public void testLengthString() {
        assertEquals(0, StringUtils.length(null));
        assertEquals(0, StringUtils.length(""));
        assertEquals(0, StringUtils.length(StringUtils.EMPTY));
        assertEquals(1, StringUtils.length("A"));
        assertEquals(1, StringUtils.length(" "));
        assertEquals(8, StringUtils.length("ABCDEFGH"));
    }

    public void testLengthStringBuffer() {
        assertEquals(0, StringUtils.length(new StringBuffer("")));
        assertEquals(0, StringUtils.length(new StringBuffer(StringUtils.EMPTY)));
        assertEquals(1, StringUtils.length(new StringBuffer("A")));
        assertEquals(1, StringUtils.length(new StringBuffer(" ")));
        assertEquals(8, StringUtils.length(new StringBuffer("ABCDEFGH")));
    }

    public void testLengthStringBuilder() {
        assertEquals(0, StringUtils.length(new StringBuilder("")));
        assertEquals(0, StringUtils.length(new StringBuilder(StringUtils.EMPTY)));
        assertEquals(1, StringUtils.length(new StringBuilder("A")));
        assertEquals(1, StringUtils.length(new StringBuilder(" ")));
        assertEquals(8, StringUtils.length(new StringBuilder("ABCDEFGH")));
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
        assertEquals("  abc  ", StringUtils.center("abc", 7, null));
        assertEquals("  abc  ", StringUtils.center("abc", 7, ""));
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

    //-----------------------------------------------------------------------
    public void testDefault_String() {
        assertEquals("", StringUtils.defaultString(null));
        assertEquals("", StringUtils.defaultString(""));
        assertEquals("abc", StringUtils.defaultString("abc"));
    }

    public void testDefault_StringString() {
        assertEquals("NULL", StringUtils.defaultString(null, "NULL"));
        assertEquals("", StringUtils.defaultString("", "NULL"));
        assertEquals("abc", StringUtils.defaultString("abc", "NULL"));
    }

    public void testDefaultIfEmpty_StringString() {
        assertEquals("NULL", StringUtils.defaultIfEmpty(null, "NULL"));
        assertEquals("NULL", StringUtils.defaultIfEmpty("", "NULL"));
        assertEquals("abc", StringUtils.defaultIfEmpty("abc", "NULL"));
        assertNull(StringUtils.defaultIfEmpty("", null));
        // Tests compatibility for the API return type
        String s = StringUtils.defaultIfEmpty("abc", "NULL");
        assertEquals("abc", s);
    }

    public void testDefaultIfEmpty_StringBuilders() {
        assertEquals("NULL", StringUtils.defaultIfEmpty(new StringBuilder(""), new StringBuilder("NULL")).toString());
        assertEquals("abc", StringUtils.defaultIfEmpty(new StringBuilder("abc"), new StringBuilder("NULL")).toString());
        assertNull(StringUtils.defaultIfEmpty(new StringBuilder(""), null));
        // Tests compatibility for the API return type
        StringBuilder s = StringUtils.defaultIfEmpty(new StringBuilder("abc"), new StringBuilder("NULL"));
        assertEquals("abc", s.toString());
    }

    public void testDefaultIfEmpty_StringBuffers() {
        assertEquals("NULL", StringUtils.defaultIfEmpty(new StringBuffer(""), new StringBuffer("NULL")).toString());
        assertEquals("abc", StringUtils.defaultIfEmpty(new StringBuffer("abc"), new StringBuffer("NULL")).toString());
        assertNull(StringUtils.defaultIfEmpty(new StringBuffer(""), null));
        // Tests compatibility for the API return type
        StringBuffer s = StringUtils.defaultIfEmpty(new StringBuffer("abc"), new StringBuffer("NULL"));
        assertEquals("abc", s.toString());
    }

    public void testDefaultIfEmpty_CharBuffers() {
        assertEquals("NULL", StringUtils.defaultIfEmpty(CharBuffer.wrap(""), CharBuffer.wrap("NULL")).toString());
        assertEquals("abc", StringUtils.defaultIfEmpty(CharBuffer.wrap("abc"), CharBuffer.wrap("NULL")).toString());
        assertNull(StringUtils.defaultIfEmpty(CharBuffer.wrap(""), null));
        // Tests compatibility for the API return type
        CharBuffer s = StringUtils.defaultIfEmpty(CharBuffer.wrap("abc"), CharBuffer.wrap("NULL"));
        assertEquals("abc", s.toString());
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
            @SuppressWarnings("unused")
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
            @SuppressWarnings("unused")
            String res = StringUtils.abbreviate("abcdefghij", 0, 3);
            fail("StringUtils.abbreviate expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
                // empty
        }      
        try {
            @SuppressWarnings("unused")
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

    public void testAbbreviateMiddle() {
        // javadoc examples
        assertNull( StringUtils.abbreviateMiddle(null, null, 0) );
        assertEquals( "abc", StringUtils.abbreviateMiddle("abc", null, 0) );
        assertEquals( "abc", StringUtils.abbreviateMiddle("abc", ".", 0) );
        assertEquals( "abc", StringUtils.abbreviateMiddle("abc", ".", 3) );
        assertEquals( "ab.f", StringUtils.abbreviateMiddle("abcdef", ".", 4) );

        // JIRA issue (LANG-405) example (slightly different than actual expected result)
        assertEquals( 
            "A very long text with un...f the text is complete.",
            StringUtils.abbreviateMiddle(
                "A very long text with unimportant stuff in the middle but interesting start and " +
                "end to see if the text is complete.", "...", 50) );

        // Test a much longer text :)
        String longText = "Start text" + StringUtils.repeat("x", 10000) + "Close text";
        assertEquals( 
            "Start text->Close text",
            StringUtils.abbreviateMiddle( longText, "->", 22 ) );

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
        assertEquals(-1, StringUtils.indexOfDifference(null, null));
        assertEquals(0, StringUtils.indexOfDifference(null, "i am a robot"));
        assertEquals(-1, StringUtils.indexOfDifference("", ""));
        assertEquals(0, StringUtils.indexOfDifference("", "abc"));
        assertEquals(0, StringUtils.indexOfDifference("abc", ""));
        assertEquals(0, StringUtils.indexOfDifference("i am a machine", null));
        assertEquals(7, StringUtils.indexOfDifference("i am a machine", "i am a robot"));
        assertEquals(-1, StringUtils.indexOfDifference("foo", "foo"));
        assertEquals(0, StringUtils.indexOfDifference("i am a robot", "you are a robot"));
        //System.out.println("indexOfDiff: " + StringUtils.indexOfDifference("i am a robot", "not machine"));
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
            @SuppressWarnings("unused")
            int d = StringUtils.getLevenshteinDistance("a", null);
            fail("expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            // empty
        }
        try {
            @SuppressWarnings("unused")
            int d = StringUtils.getLevenshteinDistance(null, "a");
            fail("expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            // empty
        }
    }

    /**
     * A sanity check for {@link StringUtils#EMPTY}.
     */
    public void testEMPTY() {
        assertNotNull(StringUtils.EMPTY);
        assertEquals("", StringUtils.EMPTY);
        assertEquals(0, StringUtils.EMPTY.length());
    }

    /**
     * Test for {@link StringUtils#isAllLowerCase(CharSequence)}.
     */
    public void testIsAllLowerCase() {
        assertFalse(StringUtils.isAllLowerCase(null));
        assertFalse(StringUtils.isAllLowerCase(StringUtils.EMPTY));
        assertTrue(StringUtils.isAllLowerCase("abc"));
        assertFalse(StringUtils.isAllLowerCase("abc "));
        assertFalse(StringUtils.isAllLowerCase("abC"));
    }

    /**
     * Test for {@link StringUtils#isAllUpperCase(CharSequence)}.
     */
    public void testIsAllUpperCase() {
        assertFalse(StringUtils.isAllUpperCase(null));
        assertFalse(StringUtils.isAllUpperCase(StringUtils.EMPTY));
        assertTrue(StringUtils.isAllUpperCase("ABC"));
        assertFalse(StringUtils.isAllUpperCase("ABC "));
        assertFalse(StringUtils.isAllUpperCase("aBC"));
    }

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
    
    public void testRemoveStartIgnoreCase() {
        // StringUtils.removeStart("", *)        = ""
        assertNull("removeStartIgnoreCase(null, null)", StringUtils.removeStartIgnoreCase(null, null));
        assertNull("removeStartIgnoreCase(null, \"\")", StringUtils.removeStartIgnoreCase(null, ""));
        assertNull("removeStartIgnoreCase(null, \"a\")", StringUtils.removeStartIgnoreCase(null, "a"));
        
        // StringUtils.removeStart(*, null)      = *
        assertEquals("removeStartIgnoreCase(\"\", null)", StringUtils.removeStartIgnoreCase("", null), "");
        assertEquals("removeStartIgnoreCase(\"\", \"\")", StringUtils.removeStartIgnoreCase("", ""), "");
        assertEquals("removeStartIgnoreCase(\"\", \"a\")", StringUtils.removeStartIgnoreCase("", "a"), "");
        
        // All others:
        assertEquals("removeStartIgnoreCase(\"www.domain.com\", \"www.\")", StringUtils.removeStartIgnoreCase("www.domain.com", "www."), "domain.com");
        assertEquals("removeStartIgnoreCase(\"domain.com\", \"www.\")", StringUtils.removeStartIgnoreCase("domain.com", "www."), "domain.com");
        assertEquals("removeStartIgnoreCase(\"domain.com\", \"\")", StringUtils.removeStartIgnoreCase("domain.com", ""), "domain.com");        
        assertEquals("removeStartIgnoreCase(\"domain.com\", null)", StringUtils.removeStartIgnoreCase("domain.com", null), "domain.com");        
        
        // Case insensitive:
        assertEquals("removeStartIgnoreCase(\"www.domain.com\", \"WWW.\")", StringUtils.removeStartIgnoreCase("www.domain.com", "WWW."), "domain.com");
    }

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

    public void testRemoveEndIgnoreCase() {
        // StringUtils.removeEndIgnoreCase("", *)        = ""
        assertNull("removeEndIgnoreCase(null, null)", StringUtils.removeEndIgnoreCase(null, null));
        assertNull("removeEndIgnoreCase(null, \"\")", StringUtils.removeEndIgnoreCase(null, ""));
        assertNull("removeEndIgnoreCase(null, \"a\")", StringUtils.removeEndIgnoreCase(null, "a"));
        
        // StringUtils.removeEnd(*, null)      = *
        assertEquals("removeEndIgnoreCase(\"\", null)", StringUtils.removeEndIgnoreCase("", null), "");
        assertEquals("removeEndIgnoreCase(\"\", \"\")", StringUtils.removeEndIgnoreCase("", ""), "");
        assertEquals("removeEndIgnoreCase(\"\", \"a\")", StringUtils.removeEndIgnoreCase("", "a"), "");
        
        // All others:
        assertEquals("removeEndIgnoreCase(\"www.domain.com.\", \".com\")", StringUtils.removeEndIgnoreCase("www.domain.com.", ".com"), "www.domain.com.");
        assertEquals("removeEndIgnoreCase(\"www.domain.com\", \".com\")", StringUtils.removeEndIgnoreCase("www.domain.com", ".com"), "www.domain");
        assertEquals("removeEndIgnoreCase(\"www.domain\", \".com\")", StringUtils.removeEndIgnoreCase("www.domain", ".com"), "www.domain");
        assertEquals("removeEndIgnoreCase(\"domain.com\", \"\")", StringUtils.removeEndIgnoreCase("domain.com", ""), "domain.com");   
        assertEquals("removeEndIgnoreCase(\"domain.com\", null)", StringUtils.removeEndIgnoreCase("domain.com", null), "domain.com");   

        // Case insensitive:
        assertEquals("removeEndIgnoreCase(\"www.domain.com\", \".COM\")", StringUtils.removeEndIgnoreCase("www.domain.com", ".COM"), "www.domain");
        assertEquals("removeEndIgnoreCase(\"www.domain.COM\", \".com\")", StringUtils.removeEndIgnoreCase("www.domain.COM", ".com"), "www.domain");
    }

    public void testRemove_String() {
        // StringUtils.remove(null, *)        = null
        assertEquals(null, StringUtils.remove(null, null));
        assertEquals(null, StringUtils.remove(null, ""));
        assertEquals(null, StringUtils.remove(null, "a"));
        
        // StringUtils.remove("", *)          = ""
        assertEquals("", StringUtils.remove("", null));
        assertEquals("", StringUtils.remove("", ""));
        assertEquals("", StringUtils.remove("", "a"));
        
        // StringUtils.remove(*, null)        = *
        assertEquals(null, StringUtils.remove(null, null));
        assertEquals("", StringUtils.remove("", null));
        assertEquals("a", StringUtils.remove("a", null));
        
        // StringUtils.remove(*, "")          = *
        assertEquals(null, StringUtils.remove(null, ""));
        assertEquals("", StringUtils.remove("", ""));
        assertEquals("a", StringUtils.remove("a", ""));
        
        // StringUtils.remove("queued", "ue") = "qd"
        assertEquals("qd", StringUtils.remove("queued", "ue"));
        
        // StringUtils.remove("queued", "zz") = "queued"
        assertEquals("queued", StringUtils.remove("queued", "zz"));
    }

    public void testRemove_char() {
        // StringUtils.remove(null, *)       = null
        assertEquals(null, StringUtils.remove(null, 'a'));
        assertEquals(null, StringUtils.remove(null, 'a'));
        assertEquals(null, StringUtils.remove(null, 'a'));
        
        // StringUtils.remove("", *)          = ""
        assertEquals("", StringUtils.remove("", 'a'));
        assertEquals("", StringUtils.remove("", 'a'));
        assertEquals("", StringUtils.remove("", 'a'));
        
        // StringUtils.remove("queued", 'u') = "qeed"
        assertEquals("qeed", StringUtils.remove("queued", 'u'));
        
        // StringUtils.remove("queued", 'z') = "queued"
        assertEquals("queued", StringUtils.remove("queued", 'z'));
    }
    
    public void testDifferenceAt_StringArray(){        
        assertEquals(-1, StringUtils.indexOfDifference(null));
        assertEquals(-1, StringUtils.indexOfDifference(new String[] {}));
        assertEquals(-1, StringUtils.indexOfDifference(new String[] {"abc"}));
        assertEquals(-1, StringUtils.indexOfDifference(new String[] {null, null}));
        assertEquals(-1, StringUtils.indexOfDifference(new String[] {"", ""}));
        assertEquals(0, StringUtils.indexOfDifference(new String[] {"", null}));
        assertEquals(0, StringUtils.indexOfDifference(new String[] {"abc", null, null}));
        assertEquals(0, StringUtils.indexOfDifference(new String[] {null, null, "abc"}));
        assertEquals(0, StringUtils.indexOfDifference(new String[] {"", "abc"}));
        assertEquals(0, StringUtils.indexOfDifference(new String[] {"abc", ""}));
        assertEquals(-1, StringUtils.indexOfDifference(new String[] {"abc", "abc"}));
        assertEquals(1, StringUtils.indexOfDifference(new String[] {"abc", "a"}));
        assertEquals(2, StringUtils.indexOfDifference(new String[] {"ab", "abxyz"}));
        assertEquals(2, StringUtils.indexOfDifference(new String[] {"abcde", "abxyz"}));
        assertEquals(0, StringUtils.indexOfDifference(new String[] {"abcde", "xyz"}));
        assertEquals(0, StringUtils.indexOfDifference(new String[] {"xyz", "abcde"}));
        assertEquals(7, StringUtils.indexOfDifference(new String[] {"i am a machine", "i am a robot"}));
    }
    
    public void testGetCommonPrefix_StringArray(){        
        assertEquals("", StringUtils.getCommonPrefix(null));
        assertEquals("", StringUtils.getCommonPrefix(new String[] {}));
        assertEquals("abc", StringUtils.getCommonPrefix(new String[] {"abc"}));
        assertEquals("", StringUtils.getCommonPrefix(new String[] {null, null}));
        assertEquals("", StringUtils.getCommonPrefix(new String[] {"", ""}));
        assertEquals("", StringUtils.getCommonPrefix(new String[] {"", null}));
        assertEquals("", StringUtils.getCommonPrefix(new String[] {"abc", null, null}));
        assertEquals("", StringUtils.getCommonPrefix(new String[] {null, null, "abc"}));
        assertEquals("", StringUtils.getCommonPrefix(new String[] {"", "abc"}));
        assertEquals("", StringUtils.getCommonPrefix(new String[] {"abc", ""}));
        assertEquals("abc", StringUtils.getCommonPrefix(new String[] {"abc", "abc"}));
        assertEquals("a", StringUtils.getCommonPrefix(new String[] {"abc", "a"}));
        assertEquals("ab", StringUtils.getCommonPrefix(new String[] {"ab", "abxyz"}));
        assertEquals("ab", StringUtils.getCommonPrefix(new String[] {"abcde", "abxyz"}));
        assertEquals("", StringUtils.getCommonPrefix(new String[] {"abcde", "xyz"}));
        assertEquals("", StringUtils.getCommonPrefix(new String[] {"xyz", "abcde"}));
        assertEquals("i am a ", StringUtils.getCommonPrefix(new String[] {"i am a machine", "i am a robot"}));
    }
        
    public void testStartsWithAny() {
        assertFalse(StringUtils.startsWithAny(null, null));
        assertFalse(StringUtils.startsWithAny(null, new String[] {"abc"}));
        assertFalse(StringUtils.startsWithAny("abcxyz", null));
        assertFalse(StringUtils.startsWithAny("abcxyz", new String[] {}));
        assertTrue(StringUtils.startsWithAny("abcxyz", new String[] {"abc"}));
        assertTrue(StringUtils.startsWithAny("abcxyz", new String[] {null, "xyz", "abc"}));
        assertFalse(StringUtils.startsWithAny("abcxyz", new String[] {null, "xyz", "abcd"}));
    }
 
}
