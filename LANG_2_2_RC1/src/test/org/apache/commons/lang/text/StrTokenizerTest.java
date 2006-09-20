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

package org.apache.commons.lang.text;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.NoSuchElementException;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.ObjectUtils;

/**
 * Unit test for Tokenizer.
 * 
 * @author Matthew Inger
 */
public class StrTokenizerTest extends TestCase {

    private static final String CSV_SIMPLE_FIXTURE = "A,b,c";

    private static final String TSV_SIMPLE_FIXTURE = "A\tb\tc";

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(StrTokenizerTest.class);
        suite.setName("TokenizerTest Tests");
        return suite;
    }

    /**
     * JUnit constructor.
     * 
     * @param name
     */
    public StrTokenizerTest(String name) {
        super(name);
    }

    private void checkClone(StrTokenizer tokenizer) {
        assertFalse(StrTokenizer.getCSVInstance() == tokenizer);
        assertFalse(StrTokenizer.getTSVInstance() == tokenizer);
    }

    // -----------------------------------------------------------------------
    public void test1() {

        String input = "a;b;c;\"d;\"\"e\";f; ; ;  ";
        StrTokenizer tok = new StrTokenizer(input);
        tok.setDelimiterChar(';');
        tok.setQuoteChar('"');
        tok.setIgnoredMatcher(StrMatcher.trimMatcher());
        tok.setIgnoreEmptyTokens(false);
        String tokens[] = tok.getTokenArray();

        String expected[] = new String[]{"a", "b", "c", "d;\"e", "f", "", "", "",};

        assertEquals(ArrayUtils.toString(tokens), expected.length, tokens.length);
        for (int i = 0; i < expected.length; i++) {
            assertTrue("token[" + i + "] was '" + tokens[i] + "' but was expected to be '" + expected[i] + "'",
                    ObjectUtils.equals(expected[i], tokens[i]));
        }

    }

    public void test2() {

        String input = "a;b;c ;\"d;\"\"e\";f; ; ;";
        StrTokenizer tok = new StrTokenizer(input);
        tok.setDelimiterChar(';');
        tok.setQuoteChar('"');
        tok.setIgnoredMatcher(StrMatcher.noneMatcher());
        tok.setIgnoreEmptyTokens(false);
        String tokens[] = tok.getTokenArray();

        String expected[] = new String[]{"a", "b", "c ", "d;\"e", "f", " ", " ", "",};

        assertEquals(ArrayUtils.toString(tokens), expected.length, tokens.length);
        for (int i = 0; i < expected.length; i++) {
            assertTrue("token[" + i + "] was '" + tokens[i] + "' but was expected to be '" + expected[i] + "'",
                    ObjectUtils.equals(expected[i], tokens[i]));
        }

    }

    public void test3() {

        String input = "a;b; c;\"d;\"\"e\";f; ; ;";
        StrTokenizer tok = new StrTokenizer(input);
        tok.setDelimiterChar(';');
        tok.setQuoteChar('"');
        tok.setIgnoredMatcher(StrMatcher.noneMatcher());
        tok.setIgnoreEmptyTokens(false);
        String tokens[] = tok.getTokenArray();

        String expected[] = new String[]{"a", "b", " c", "d;\"e", "f", " ", " ", "",};

        assertEquals(ArrayUtils.toString(tokens), expected.length, tokens.length);
        for (int i = 0; i < expected.length; i++) {
            assertTrue("token[" + i + "] was '" + tokens[i] + "' but was expected to be '" + expected[i] + "'",
                    ObjectUtils.equals(expected[i], tokens[i]));
        }

    }

    public void test4() {

        String input = "a;b; c;\"d;\"\"e\";f; ; ;";
        StrTokenizer tok = new StrTokenizer(input);
        tok.setDelimiterChar(';');
        tok.setQuoteChar('"');
        tok.setIgnoredMatcher(StrMatcher.trimMatcher());
        tok.setIgnoreEmptyTokens(true);
        String tokens[] = tok.getTokenArray();

        String expected[] = new String[]{"a", "b", "c", "d;\"e", "f",};

        assertEquals(ArrayUtils.toString(tokens), expected.length, tokens.length);
        for (int i = 0; i < expected.length; i++) {
            assertTrue("token[" + i + "] was '" + tokens[i] + "' but was expected to be '" + expected[i] + "'",
                    ObjectUtils.equals(expected[i], tokens[i]));
        }

    }

    public void test5() {

        String input = "a;b; c;\"d;\"\"e\";f; ; ;";
        StrTokenizer tok = new StrTokenizer(input);
        tok.setDelimiterChar(';');
        tok.setQuoteChar('"');
        tok.setIgnoredMatcher(StrMatcher.trimMatcher());
        tok.setIgnoreEmptyTokens(false);
        tok.setEmptyTokenAsNull(true);
        String tokens[] = tok.getTokenArray();

        String expected[] = new String[]{"a", "b", "c", "d;\"e", "f", null, null, null,};

        assertEquals(ArrayUtils.toString(tokens), expected.length, tokens.length);
        for (int i = 0; i < expected.length; i++) {
            assertTrue("token[" + i + "] was '" + tokens[i] + "' but was expected to be '" + expected[i] + "'",
                    ObjectUtils.equals(expected[i], tokens[i]));
        }

    }

    public void test6() {

        String input = "a;b; c;\"d;\"\"e\";f; ; ;";
        StrTokenizer tok = new StrTokenizer(input);
        tok.setDelimiterChar(';');
        tok.setQuoteChar('"');
        tok.setIgnoredMatcher(StrMatcher.trimMatcher());
        tok.setIgnoreEmptyTokens(false);
        // tok.setTreatingEmptyAsNull(true);
        String tokens[] = tok.getTokenArray();

        String expected[] = new String[]{"a", "b", " c", "d;\"e", "f", null, null, null,};

        int nextCount = 0;
        while (tok.hasNext()) {
            tok.next();
            nextCount++;
        }

        int prevCount = 0;
        while (tok.hasPrevious()) {
            tok.previous();
            prevCount++;
        }

        assertEquals(ArrayUtils.toString(tokens), expected.length, tokens.length);

        assertTrue("could not cycle through entire token list" + " using the 'hasNext' and 'next' methods",
                nextCount == expected.length);

        assertTrue("could not cycle through entire token list" + " using the 'hasPrevious' and 'previous' methods",
                prevCount == expected.length);

    }

    public void test7() {

        String input = "a   b c \"d e\" f ";
        StrTokenizer tok = new StrTokenizer(input);
        tok.setDelimiterMatcher(StrMatcher.spaceMatcher());
        tok.setQuoteMatcher(StrMatcher.doubleQuoteMatcher());
        tok.setIgnoredMatcher(StrMatcher.noneMatcher());
        tok.setIgnoreEmptyTokens(false);
        String tokens[] = tok.getTokenArray();

        String expected[] = new String[]{"a", "", "", "b", "c", "d e", "f", "",};

        assertEquals(ArrayUtils.toString(tokens), expected.length, tokens.length);
        for (int i = 0; i < expected.length; i++) {
            assertTrue("token[" + i + "] was '" + tokens[i] + "' but was expected to be '" + expected[i] + "'",
                    ObjectUtils.equals(expected[i], tokens[i]));
        }

    }

    public void test8() {

        String input = "a   b c \"d e\" f ";
        StrTokenizer tok = new StrTokenizer(input);
        tok.setDelimiterMatcher(StrMatcher.spaceMatcher());
        tok.setQuoteMatcher(StrMatcher.doubleQuoteMatcher());
        tok.setIgnoredMatcher(StrMatcher.noneMatcher());
        tok.setIgnoreEmptyTokens(true);
        String tokens[] = tok.getTokenArray();

        String expected[] = new String[]{"a", "b", "c", "d e", "f",};

        assertEquals(ArrayUtils.toString(tokens), expected.length, tokens.length);
        for (int i = 0; i < expected.length; i++) {
            assertTrue("token[" + i + "] was '" + tokens[i] + "' but was expected to be '" + expected[i] + "'",
                    ObjectUtils.equals(expected[i], tokens[i]));
        }

    }

    public void testBasic1() {
        String input = "a  b c";
        StrTokenizer tok = new StrTokenizer(input);
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertEquals("c", tok.next());
        assertEquals(false, tok.hasNext());
    }

    public void testBasic2() {
        String input = "a \nb\fc";
        StrTokenizer tok = new StrTokenizer(input);
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertEquals("c", tok.next());
        assertEquals(false, tok.hasNext());
    }

    public void testBasic3() {
        String input = "a \nb\u0001\fc";
        StrTokenizer tok = new StrTokenizer(input);
        assertEquals("a", tok.next());
        assertEquals("b\u0001", tok.next());
        assertEquals("c", tok.next());
        assertEquals(false, tok.hasNext());
    }

    public void testBasic4() {
        String input = "a \"b\" c";
        StrTokenizer tok = new StrTokenizer(input);
        assertEquals("a", tok.next());
        assertEquals("\"b\"", tok.next());
        assertEquals("c", tok.next());
        assertEquals(false, tok.hasNext());
    }

    public void testBasic5() {
        String input = "a:b':c";
        StrTokenizer tok = new StrTokenizer(input, ':', '\'');
        assertEquals("a", tok.next());
        assertEquals("b'", tok.next());
        assertEquals("c", tok.next());
        assertEquals(false, tok.hasNext());
    }

    public void testBasicDelim1() {
        String input = "a:b:c";
        StrTokenizer tok = new StrTokenizer(input, ':');
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertEquals("c", tok.next());
        assertEquals(false, tok.hasNext());
    }

    public void testBasicDelim2() {
        String input = "a:b:c";
        StrTokenizer tok = new StrTokenizer(input, ',');
        assertEquals("a:b:c", tok.next());
        assertEquals(false, tok.hasNext());
    }

    public void testBasicEmpty1() {
        String input = "a  b c";
        StrTokenizer tok = new StrTokenizer(input);
        tok.setIgnoreEmptyTokens(false);
        assertEquals("a", tok.next());
        assertEquals("", tok.next());
        assertEquals("b", tok.next());
        assertEquals("c", tok.next());
        assertEquals(false, tok.hasNext());
    }

    public void testBasicEmpty2() {
        String input = "a  b c";
        StrTokenizer tok = new StrTokenizer(input);
        tok.setIgnoreEmptyTokens(false);
        tok.setEmptyTokenAsNull(true);
        assertEquals("a", tok.next());
        assertEquals(null, tok.next());
        assertEquals("b", tok.next());
        assertEquals("c", tok.next());
        assertEquals(false, tok.hasNext());
    }

    public void testBasicQuoted1() {
        String input = "a 'b' c";
        StrTokenizer tok = new StrTokenizer(input, ' ', '\'');
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertEquals("c", tok.next());
        assertEquals(false, tok.hasNext());
    }

    public void testBasicQuoted2() {
        String input = "a:'b':";
        StrTokenizer tok = new StrTokenizer(input, ':', '\'');
        tok.setIgnoreEmptyTokens(false);
        tok.setEmptyTokenAsNull(true);
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertEquals(null, tok.next());
        assertEquals(false, tok.hasNext());
    }

    public void testBasicQuoted3() {
        String input = "a:'b''c'";
        StrTokenizer tok = new StrTokenizer(input, ':', '\'');
        tok.setIgnoreEmptyTokens(false);
        tok.setEmptyTokenAsNull(true);
        assertEquals("a", tok.next());
        assertEquals("b'c", tok.next());
        assertEquals(false, tok.hasNext());
    }

    public void testBasicQuoted4() {
        String input = "a: 'b' 'c' :d";
        StrTokenizer tok = new StrTokenizer(input, ':', '\'');
        tok.setTrimmerMatcher(StrMatcher.trimMatcher());
        tok.setIgnoreEmptyTokens(false);
        tok.setEmptyTokenAsNull(true);
        assertEquals("a", tok.next());
        assertEquals("b c", tok.next());
        assertEquals("d", tok.next());
        assertEquals(false, tok.hasNext());
    }

    public void testBasicQuoted5() {
        String input = "a: 'b'x'c' :d";
        StrTokenizer tok = new StrTokenizer(input, ':', '\'');
        tok.setTrimmerMatcher(StrMatcher.trimMatcher());
        tok.setIgnoreEmptyTokens(false);
        tok.setEmptyTokenAsNull(true);
        assertEquals("a", tok.next());
        assertEquals("bxc", tok.next());
        assertEquals("d", tok.next());
        assertEquals(false, tok.hasNext());
    }

    public void testBasicQuoted6() {
        String input = "a:'b'\"c':d";
        StrTokenizer tok = new StrTokenizer(input, ':');
        tok.setQuoteMatcher(StrMatcher.quoteMatcher());
        assertEquals("a", tok.next());
        assertEquals("b\"c:d", tok.next());
        assertEquals(false, tok.hasNext());
    }

    public void testBasicQuoted7() {
        String input = "a:\"There's a reason here\":b";
        StrTokenizer tok = new StrTokenizer(input, ':');
        tok.setQuoteMatcher(StrMatcher.quoteMatcher());
        assertEquals("a", tok.next());
        assertEquals("There's a reason here", tok.next());
        assertEquals("b", tok.next());
        assertEquals(false, tok.hasNext());
    }

    public void testBasicQuotedTrimmed1() {
        String input = "a: 'b' :";
        StrTokenizer tok = new StrTokenizer(input, ':', '\'');
        tok.setTrimmerMatcher(StrMatcher.trimMatcher());
        tok.setIgnoreEmptyTokens(false);
        tok.setEmptyTokenAsNull(true);
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertEquals(null, tok.next());
        assertEquals(false, tok.hasNext());
    }

    public void testBasicTrimmed1() {
        String input = "a: b :  ";
        StrTokenizer tok = new StrTokenizer(input, ':');
        tok.setTrimmerMatcher(StrMatcher.trimMatcher());
        tok.setIgnoreEmptyTokens(false);
        tok.setEmptyTokenAsNull(true);
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertEquals(null, tok.next());
        assertEquals(false, tok.hasNext());
    }

    public void testBasicTrimmed2() {
        String input = "a:  b  :";
        StrTokenizer tok = new StrTokenizer(input, ':');
        tok.setTrimmerMatcher(StrMatcher.stringMatcher("  "));
        tok.setIgnoreEmptyTokens(false);
        tok.setEmptyTokenAsNull(true);
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertEquals(null, tok.next());
        assertEquals(false, tok.hasNext());
    }

    public void testBasicIgnoreTrimmed1() {
        String input = "a: bIGNOREc : ";
        StrTokenizer tok = new StrTokenizer(input, ':');
        tok.setIgnoredMatcher(StrMatcher.stringMatcher("IGNORE"));
        tok.setTrimmerMatcher(StrMatcher.trimMatcher());
        tok.setIgnoreEmptyTokens(false);
        tok.setEmptyTokenAsNull(true);
        assertEquals("a", tok.next());
        assertEquals("bc", tok.next());
        assertEquals(null, tok.next());
        assertEquals(false, tok.hasNext());
    }

    public void testBasicIgnoreTrimmed2() {
        String input = "IGNOREaIGNORE: IGNORE bIGNOREc IGNORE : IGNORE ";
        StrTokenizer tok = new StrTokenizer(input, ':');
        tok.setIgnoredMatcher(StrMatcher.stringMatcher("IGNORE"));
        tok.setTrimmerMatcher(StrMatcher.trimMatcher());
        tok.setIgnoreEmptyTokens(false);
        tok.setEmptyTokenAsNull(true);
        assertEquals("a", tok.next());
        assertEquals("bc", tok.next());
        assertEquals(null, tok.next());
        assertEquals(false, tok.hasNext());
    }

    public void testBasicIgnoreTrimmed3() {
        String input = "IGNOREaIGNORE: IGNORE bIGNOREc IGNORE : IGNORE ";
        StrTokenizer tok = new StrTokenizer(input, ':');
        tok.setIgnoredMatcher(StrMatcher.stringMatcher("IGNORE"));
        tok.setIgnoreEmptyTokens(false);
        tok.setEmptyTokenAsNull(true);
        assertEquals("a", tok.next());
        assertEquals("  bc  ", tok.next());
        assertEquals("  ", tok.next());
        assertEquals(false, tok.hasNext());
    }

    public void testBasicIgnoreTrimmed4() {
        String input = "IGNOREaIGNORE: IGNORE 'bIGNOREc'IGNORE'd' IGNORE : IGNORE ";
        StrTokenizer tok = new StrTokenizer(input, ':', '\'');
        tok.setIgnoredMatcher(StrMatcher.stringMatcher("IGNORE"));
        tok.setTrimmerMatcher(StrMatcher.trimMatcher());
        tok.setIgnoreEmptyTokens(false);
        tok.setEmptyTokenAsNull(true);
        assertEquals("a", tok.next());
        assertEquals("bIGNOREcd", tok.next());
        assertEquals(null, tok.next());
        assertEquals(false, tok.hasNext());
    }

    //-----------------------------------------------------------------------
    public void testListArray() {
        String input = "a  b c";
        StrTokenizer tok = new StrTokenizer(input);
        String[] array = tok.getTokenArray();
        List list = tok.getTokenList();
        
        assertEquals(Arrays.asList(array), list);
        assertEquals(3, list.size());
    }

    //-----------------------------------------------------------------------
    public void testCSV(String data) {
        this.testXSVAbc(StrTokenizer.getCSVInstance(data));
        this.testXSVAbc(StrTokenizer.getCSVInstance(data.toCharArray()));
    }

    public void testCSVEmpty() {
        this.testEmpty(StrTokenizer.getCSVInstance());
        this.testEmpty(StrTokenizer.getCSVInstance(""));
    }

    public void testCSVSimple() {
        this.testCSV(CSV_SIMPLE_FIXTURE);
    }

    public void testCSVSimpleNeedsTrim() {
        this.testCSV("   " + CSV_SIMPLE_FIXTURE);
        this.testCSV("   \n\t  " + CSV_SIMPLE_FIXTURE);
        this.testCSV("   \n  " + CSV_SIMPLE_FIXTURE + "\n\n\r");
    }

    void testEmpty(StrTokenizer tokenizer) {
        this.checkClone(tokenizer);
        assertEquals(false, tokenizer.hasNext());
        assertEquals(false, tokenizer.hasPrevious());
        assertEquals(null, tokenizer.nextToken());
        assertEquals(0, tokenizer.size());
        try {
            tokenizer.next();
            fail();
        } catch (NoSuchElementException ex) {}
    }

    public void testGetContent() {
        String input = "a   b c \"d e\" f ";
        StrTokenizer tok = new StrTokenizer(input);
        assertEquals(input, tok.getContent());

        tok = new StrTokenizer(input.toCharArray());
        assertEquals(input, tok.getContent());
        
        tok = new StrTokenizer();
        assertEquals(null, tok.getContent());
    }

    //-----------------------------------------------------------------------
    public void testChaining() {
        StrTokenizer tok = new StrTokenizer();
        assertEquals(tok, tok.reset());
        assertEquals(tok, tok.reset(""));
        assertEquals(tok, tok.reset(new char[0]));
        assertEquals(tok, tok.setDelimiterChar(' '));
        assertEquals(tok, tok.setDelimiterString(" "));
        assertEquals(tok, tok.setDelimiterMatcher(null));
        assertEquals(tok, tok.setQuoteChar(' '));
        assertEquals(tok, tok.setQuoteMatcher(null));
        assertEquals(tok, tok.setIgnoredChar(' '));
        assertEquals(tok, tok.setIgnoredMatcher(null));
        assertEquals(tok, tok.setTrimmerMatcher(null));
        assertEquals(tok, tok.setEmptyTokenAsNull(false));
        assertEquals(tok, tok.setIgnoreEmptyTokens(false));
    }

    /**
     * Tests that the {@link StrTokenizer#clone()} clone method catches {@link CloneNotSupportedException} and returns
     * <code>null</code>.
     */
    public void testCloneNotSupportedException() {
        Object notCloned = (new StrTokenizer() {
            Object cloneReset() throws CloneNotSupportedException {
                throw new CloneNotSupportedException("test");
            }
        }).clone();
        assertNull(notCloned);
    }

    public void testCloneNull() {
        StrTokenizer tokenizer = new StrTokenizer((char[]) null);
        // Start sanity check
        assertEquals(null, tokenizer.nextToken());
        tokenizer.reset();
        assertEquals(null, tokenizer.nextToken());
        // End sanity check
        StrTokenizer clonedTokenizer = (StrTokenizer) tokenizer.clone();
        tokenizer.reset();
        assertEquals(null, tokenizer.nextToken());
        assertEquals(null, clonedTokenizer.nextToken());
    }

    public void testCloneReset() {
        char[] input = new char[]{'a'};
        StrTokenizer tokenizer = new StrTokenizer(input);
        // Start sanity check
        assertEquals("a", tokenizer.nextToken());
        tokenizer.reset();
        assertEquals("a", tokenizer.nextToken());
        // End sanity check
        StrTokenizer clonedTokenizer = (StrTokenizer) tokenizer.clone();
        input[0] = 'b';
        tokenizer.reset();
        assertEquals("b", tokenizer.nextToken());
        assertEquals("a", clonedTokenizer.nextToken());
    }
  
    // -----------------------------------------------------------------------
    public void testConstructor_String() {
        StrTokenizer tok = new StrTokenizer("a b");
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertEquals(false, tok.hasNext());
        
        tok = new StrTokenizer("");
        assertEquals(false, tok.hasNext());
        
        tok = new StrTokenizer((String) null);
        assertEquals(false, tok.hasNext());
    }

    //-----------------------------------------------------------------------
    public void testConstructor_String_char() {
        StrTokenizer tok = new StrTokenizer("a b", ' ');
        assertEquals(1, tok.getDelimiterMatcher().isMatch(" ".toCharArray(), 0, 0, 1));
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertEquals(false, tok.hasNext());
        
        tok = new StrTokenizer("", ' ');
        assertEquals(false, tok.hasNext());
        
        tok = new StrTokenizer((String) null, ' ');
        assertEquals(false, tok.hasNext());
    }

    //-----------------------------------------------------------------------
    public void testConstructor_String_char_char() {
        StrTokenizer tok = new StrTokenizer("a b", ' ', '"');
        assertEquals(1, tok.getDelimiterMatcher().isMatch(" ".toCharArray(), 0, 0, 1));
        assertEquals(1, tok.getQuoteMatcher().isMatch("\"".toCharArray(), 0, 0, 1));
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertEquals(false, tok.hasNext());
        
        tok = new StrTokenizer("", ' ', '"');
        assertEquals(false, tok.hasNext());
        
        tok = new StrTokenizer((String) null, ' ', '"');
        assertEquals(false, tok.hasNext());
    }

    //-----------------------------------------------------------------------
    public void testConstructor_charArray() {
        StrTokenizer tok = new StrTokenizer("a b".toCharArray());
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertEquals(false, tok.hasNext());
        
        tok = new StrTokenizer(new char[0]);
        assertEquals(false, tok.hasNext());
        
        tok = new StrTokenizer((char[]) null);
        assertEquals(false, tok.hasNext());
    }

    //-----------------------------------------------------------------------
    public void testConstructor_charArray_char() {
        StrTokenizer tok = new StrTokenizer("a b".toCharArray(), ' ');
        assertEquals(1, tok.getDelimiterMatcher().isMatch(" ".toCharArray(), 0, 0, 1));
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertEquals(false, tok.hasNext());
        
        tok = new StrTokenizer(new char[0], ' ');
        assertEquals(false, tok.hasNext());
        
        tok = new StrTokenizer((char[]) null, ' ');
        assertEquals(false, tok.hasNext());
    }

    //-----------------------------------------------------------------------
    public void testConstructor_charArray_char_char() {
        StrTokenizer tok = new StrTokenizer("a b".toCharArray(), ' ', '"');
        assertEquals(1, tok.getDelimiterMatcher().isMatch(" ".toCharArray(), 0, 0, 1));
        assertEquals(1, tok.getQuoteMatcher().isMatch("\"".toCharArray(), 0, 0, 1));
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertEquals(false, tok.hasNext());
        
        tok = new StrTokenizer(new char[0], ' ', '"');
        assertEquals(false, tok.hasNext());
        
        tok = new StrTokenizer((char[]) null, ' ', '"');
        assertEquals(false, tok.hasNext());
    }

    //-----------------------------------------------------------------------
    public void testReset() {
        StrTokenizer tok = new StrTokenizer("a b c");
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertEquals("c", tok.next());
        assertEquals(false, tok.hasNext());
        
        tok.reset();
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertEquals("c", tok.next());
        assertEquals(false, tok.hasNext());
    }

    //-----------------------------------------------------------------------
    public void testReset_String() {
        StrTokenizer tok = new StrTokenizer("x x x");
        tok.reset("d e");
        assertEquals("d", tok.next());
        assertEquals("e", tok.next());
        assertEquals(false, tok.hasNext());
        
        tok.reset((String) null);
        assertEquals(false, tok.hasNext());
    }

    //-----------------------------------------------------------------------
    public void testReset_charArray() {
        StrTokenizer tok = new StrTokenizer("x x x");
        
        char[] array = new char[] {'a', ' ', 'c'};
        tok.reset(array);
        array[1] = 'b'; // test linked array
        assertEquals("abc", tok.next());
        assertEquals(false, tok.hasNext());
        
        tok.reset((char[]) null);
        assertEquals(false, tok.hasNext());
    }

    //-----------------------------------------------------------------------
    public void testTSV() {
        this.testXSVAbc(StrTokenizer.getTSVInstance(TSV_SIMPLE_FIXTURE));
        this.testXSVAbc(StrTokenizer.getTSVInstance(TSV_SIMPLE_FIXTURE.toCharArray()));
    }

    public void testTSVEmpty() {
        this.testEmpty(StrTokenizer.getCSVInstance());
        this.testEmpty(StrTokenizer.getCSVInstance(""));
    }

    void testXSVAbc(StrTokenizer tokenizer) {
        this.checkClone(tokenizer);
        assertEquals(-1, tokenizer.previousIndex());
        assertEquals(0, tokenizer.nextIndex());
        assertEquals(null, tokenizer.previousToken());
        assertEquals("A", tokenizer.nextToken());
        assertEquals(1, tokenizer.nextIndex());
        assertEquals("b", tokenizer.nextToken());
        assertEquals(2, tokenizer.nextIndex());
        assertEquals("c", tokenizer.nextToken());
        assertEquals(3, tokenizer.nextIndex());
        assertEquals(null, tokenizer.nextToken());
        assertEquals(3, tokenizer.nextIndex());
        assertEquals("c", tokenizer.previousToken());
        assertEquals(2, tokenizer.nextIndex());
        assertEquals("b", tokenizer.previousToken());
        assertEquals(1, tokenizer.nextIndex());
        assertEquals("A", tokenizer.previousToken());
        assertEquals(0, tokenizer.nextIndex());
        assertEquals(null, tokenizer.previousToken());
        assertEquals(0, tokenizer.nextIndex());
        assertEquals(-1, tokenizer.previousIndex());
        assertEquals(3, tokenizer.size());
    }

    public void testIteration() {
        StrTokenizer tkn = new StrTokenizer("a b c");
        assertEquals(false, tkn.hasPrevious());
        try {
            tkn.previous();
            fail();
        } catch (NoSuchElementException ex) {}
        assertEquals(true, tkn.hasNext());
        
        assertEquals("a", tkn.next());
        try {
            tkn.remove();
            fail();
        } catch (UnsupportedOperationException ex) {}
        try {
            tkn.set("x");
            fail();
        } catch (UnsupportedOperationException ex) {}
        try {
            tkn.add("y");
            fail();
        } catch (UnsupportedOperationException ex) {}
        assertEquals(true, tkn.hasPrevious());
        assertEquals(true, tkn.hasNext());
        
        assertEquals("b", tkn.next());
        assertEquals(true, tkn.hasPrevious());
        assertEquals(true, tkn.hasNext());
        
        assertEquals("c", tkn.next());
        assertEquals(true, tkn.hasPrevious());
        assertEquals(false, tkn.hasNext());
        
        try {
            tkn.next();
            fail();
        } catch (NoSuchElementException ex) {}
        assertEquals(true, tkn.hasPrevious());
        assertEquals(false, tkn.hasNext());
    }

    //-----------------------------------------------------------------------
    public void testTokenizeSubclassInputChange() {
        StrTokenizer tkn = new StrTokenizer("a b c d e") {
            protected List tokenize(char[] chars, int offset, int count) {
                return super.tokenize("w x y z".toCharArray(), 2, 5);
            }
        };
        assertEquals("x", tkn.next());
        assertEquals("y", tkn.next());
    }

    //-----------------------------------------------------------------------
    public void testTokenizeSubclassOutputChange() {
        StrTokenizer tkn = new StrTokenizer("a b c") {
            protected List tokenize(char[] chars, int offset, int count) {
                List list = super.tokenize(chars, offset, count);
                Collections.reverse(list);
                return list;
            }
        };
        assertEquals("c", tkn.next());
        assertEquals("b", tkn.next());
        assertEquals("a", tkn.next());
    }

    //-----------------------------------------------------------------------
    public void testToString() {
        StrTokenizer tkn = new StrTokenizer("a b c d e");
        assertEquals("StrTokenizer[not tokenized yet]", tkn.toString());
        tkn.next();
        assertEquals("StrTokenizer[a, b, c, d, e]", tkn.toString());
    }

}
