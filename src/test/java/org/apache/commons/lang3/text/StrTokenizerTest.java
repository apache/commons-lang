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

package org.apache.commons.lang3.text;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.NoSuchElementException;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.ArrayUtils;
import org.junit.jupiter.api.Test;

/**
 * Unit test for Tokenizer.
 */
@Deprecated
public class StrTokenizerTest extends AbstractLangTest {

    private static final String CSV_SIMPLE_FIXTURE = "A,b,c";

    private static final String TSV_SIMPLE_FIXTURE = "A\tb\tc";

    private void checkClone(final StrTokenizer tokenizer) {
        assertNotSame(StrTokenizer.getCSVInstance(), tokenizer);
        assertNotSame(StrTokenizer.getTSVInstance(), tokenizer);
    }

    @Test
    public void test1() {

        final String input = "a;b;c;\"d;\"\"e\";f; ; ;  ";
        final StrTokenizer tok = new StrTokenizer(input);
        tok.setDelimiterChar(';');
        tok.setQuoteChar('"');
        tok.setIgnoredMatcher(StrMatcher.trimMatcher());
        tok.setIgnoreEmptyTokens(false);
        final String[] tokens = tok.getTokenArray();

        final String[] expected = {"a", "b", "c", "d;\"e", "f", "", "", ""};

        assertEquals(expected.length, tokens.length, ArrayUtils.toString(tokens));
        for (int i = 0; i < expected.length; i++) {
            assertEquals(expected[i], tokens[i],
                    "token[" + i + "] was '" + tokens[i] + "' but was expected to be '" + expected[i] + "'");
        }

    }

    @Test
    public void test2() {

        final String input = "a;b;c ;\"d;\"\"e\";f; ; ;";
        final StrTokenizer tok = new StrTokenizer(input);
        tok.setDelimiterChar(';');
        tok.setQuoteChar('"');
        tok.setIgnoredMatcher(StrMatcher.noneMatcher());
        tok.setIgnoreEmptyTokens(false);
        final String[] tokens = tok.getTokenArray();

        final String[] expected = {"a", "b", "c ", "d;\"e", "f", " ", " ", ""};

        assertEquals(expected.length, tokens.length, ArrayUtils.toString(tokens));
        for (int i = 0; i < expected.length; i++) {
            assertEquals(expected[i], tokens[i],
                    "token[" + i + "] was '" + tokens[i] + "' but was expected to be '" + expected[i] + "'");
        }

    }

    @Test
    public void test3() {

        final String input = "a;b; c;\"d;\"\"e\";f; ; ;";
        final StrTokenizer tok = new StrTokenizer(input);
        tok.setDelimiterChar(';');
        tok.setQuoteChar('"');
        tok.setIgnoredMatcher(StrMatcher.noneMatcher());
        tok.setIgnoreEmptyTokens(false);
        final String[] tokens = tok.getTokenArray();

        final String[] expected = {"a", "b", " c", "d;\"e", "f", " ", " ", ""};

        assertEquals(expected.length, tokens.length, ArrayUtils.toString(tokens));
        for (int i = 0; i < expected.length; i++) {
            assertEquals(expected[i], tokens[i],
                    "token[" + i + "] was '" + tokens[i] + "' but was expected to be '" + expected[i] + "'");
        }

    }

    @Test
    public void test4() {

        final String input = "a;b; c;\"d;\"\"e\";f; ; ;";
        final StrTokenizer tok = new StrTokenizer(input);
        tok.setDelimiterChar(';');
        tok.setQuoteChar('"');
        tok.setIgnoredMatcher(StrMatcher.trimMatcher());
        tok.setIgnoreEmptyTokens(true);
        final String[] tokens = tok.getTokenArray();

        final String[] expected = {"a", "b", "c", "d;\"e", "f"};

        assertEquals(expected.length, tokens.length, ArrayUtils.toString(tokens));
        for (int i = 0; i < expected.length; i++) {
            assertEquals(expected[i], tokens[i],
                    "token[" + i + "] was '" + tokens[i] + "' but was expected to be '" + expected[i] + "'");
        }

    }

    @Test
    public void test5() {

        final String input = "a;b; c;\"d;\"\"e\";f; ; ;";
        final StrTokenizer tok = new StrTokenizer(input);
        tok.setDelimiterChar(';');
        tok.setQuoteChar('"');
        tok.setIgnoredMatcher(StrMatcher.trimMatcher());
        tok.setIgnoreEmptyTokens(false);
        tok.setEmptyTokenAsNull(true);
        final String[] tokens = tok.getTokenArray();

        final String[] expected = {"a", "b", "c", "d;\"e", "f", null, null, null};

        assertEquals(expected.length, tokens.length, ArrayUtils.toString(tokens));
        for (int i = 0; i < expected.length; i++) {
            assertEquals(expected[i], tokens[i],
                    "token[" + i + "] was '" + tokens[i] + "' but was expected to be '" + expected[i] + "'");
        }

    }

    @Test
    public void test6() {

        final String input = "a;b; c;\"d;\"\"e\";f; ; ;";
        final StrTokenizer tok = new StrTokenizer(input);
        tok.setDelimiterChar(';');
        tok.setQuoteChar('"');
        tok.setIgnoredMatcher(StrMatcher.trimMatcher());
        tok.setIgnoreEmptyTokens(false);
        // tok.setTreatingEmptyAsNull(true);
        final String[] tokens = tok.getTokenArray();

        final String[] expected = {"a", "b", " c", "d;\"e", "f", null, null, null};

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

        assertEquals(expected.length, tokens.length, ArrayUtils.toString(tokens));

        assertEquals(nextCount, expected.length, "could not cycle through entire token list" + " using the 'hasNext' and 'next' methods");

        assertEquals(prevCount, expected.length, "could not cycle through entire token list" + " using the 'hasPrevious' and 'previous' methods");

    }

    @Test
    public void test7() {

        final String input = "a   b c \"d e\" f ";
        final StrTokenizer tok = new StrTokenizer(input);
        tok.setDelimiterMatcher(StrMatcher.spaceMatcher());
        tok.setQuoteMatcher(StrMatcher.doubleQuoteMatcher());
        tok.setIgnoredMatcher(StrMatcher.noneMatcher());
        tok.setIgnoreEmptyTokens(false);
        final String[] tokens = tok.getTokenArray();

        final String[] expected = {"a", "", "", "b", "c", "d e", "f", ""};

        assertEquals(expected.length, tokens.length, ArrayUtils.toString(tokens));
        for (int i = 0; i < expected.length; i++) {
            assertEquals(expected[i], tokens[i],
                    "token[" + i + "] was '" + tokens[i] + "' but was expected to be '" + expected[i] + "'");
        }

    }

    @Test
    public void test8() {

        final String input = "a   b c \"d e\" f ";
        final StrTokenizer tok = new StrTokenizer(input);
        tok.setDelimiterMatcher(StrMatcher.spaceMatcher());
        tok.setQuoteMatcher(StrMatcher.doubleQuoteMatcher());
        tok.setIgnoredMatcher(StrMatcher.noneMatcher());
        tok.setIgnoreEmptyTokens(true);
        final String[] tokens = tok.getTokenArray();

        final String[] expected = {"a", "b", "c", "d e", "f"};

        assertEquals(expected.length, tokens.length, ArrayUtils.toString(tokens));
        for (int i = 0; i < expected.length; i++) {
            assertEquals(expected[i], tokens[i],
                    "token[" + i + "] was '" + tokens[i] + "' but was expected to be '" + expected[i] + "'");
        }

    }

    @Test
    public void testBasic1() {
        final String input = "a  b c";
        final StrTokenizer tok = new StrTokenizer(input);
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertEquals("c", tok.next());
        assertFalse(tok.hasNext());
    }

    @Test
    public void testBasic2() {
        final String input = "a \nb\fc";
        final StrTokenizer tok = new StrTokenizer(input);
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertEquals("c", tok.next());
        assertFalse(tok.hasNext());
    }

    @Test
    public void testBasic3() {
        final String input = "a \nb\u0001\fc";
        final StrTokenizer tok = new StrTokenizer(input);
        assertEquals("a", tok.next());
        assertEquals("b\u0001", tok.next());
        assertEquals("c", tok.next());
        assertFalse(tok.hasNext());
    }

    @Test
    public void testBasic4() {
        final String input = "a \"b\" c";
        final StrTokenizer tok = new StrTokenizer(input);
        assertEquals("a", tok.next());
        assertEquals("\"b\"", tok.next());
        assertEquals("c", tok.next());
        assertFalse(tok.hasNext());
    }

    @Test
    public void testBasic5() {
        final String input = "a:b':c";
        final StrTokenizer tok = new StrTokenizer(input, ':', '\'');
        assertEquals("a", tok.next());
        assertEquals("b'", tok.next());
        assertEquals("c", tok.next());
        assertFalse(tok.hasNext());
    }

    @Test
    public void testBasicDelim1() {
        final String input = "a:b:c";
        final StrTokenizer tok = new StrTokenizer(input, ':');
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertEquals("c", tok.next());
        assertFalse(tok.hasNext());
    }

    @Test
    public void testBasicDelim2() {
        final String input = "a:b:c";
        final StrTokenizer tok = new StrTokenizer(input, ',');
        assertEquals("a:b:c", tok.next());
        assertFalse(tok.hasNext());
    }

    @Test
    public void testBasicEmpty1() {
        final String input = "a  b c";
        final StrTokenizer tok = new StrTokenizer(input);
        tok.setIgnoreEmptyTokens(false);
        assertEquals("a", tok.next());
        assertEquals("", tok.next());
        assertEquals("b", tok.next());
        assertEquals("c", tok.next());
        assertFalse(tok.hasNext());
    }

    @Test
    public void testBasicEmpty2() {
        final String input = "a  b c";
        final StrTokenizer tok = new StrTokenizer(input);
        tok.setIgnoreEmptyTokens(false);
        tok.setEmptyTokenAsNull(true);
        assertEquals("a", tok.next());
        assertNull(tok.next());
        assertEquals("b", tok.next());
        assertEquals("c", tok.next());
        assertFalse(tok.hasNext());
    }

    @Test
    public void testBasicQuoted1() {
        final String input = "a 'b' c";
        final StrTokenizer tok = new StrTokenizer(input, ' ', '\'');
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertEquals("c", tok.next());
        assertFalse(tok.hasNext());
    }

    @Test
    public void testBasicQuoted2() {
        final String input = "a:'b':";
        final StrTokenizer tok = new StrTokenizer(input, ':', '\'');
        tok.setIgnoreEmptyTokens(false);
        tok.setEmptyTokenAsNull(true);
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertNull(tok.next());
        assertFalse(tok.hasNext());
    }

    @Test
    public void testBasicQuoted3() {
        final String input = "a:'b''c'";
        final StrTokenizer tok = new StrTokenizer(input, ':', '\'');
        tok.setIgnoreEmptyTokens(false);
        tok.setEmptyTokenAsNull(true);
        assertEquals("a", tok.next());
        assertEquals("b'c", tok.next());
        assertFalse(tok.hasNext());
    }

    @Test
    public void testBasicQuoted4() {
        final String input = "a: 'b' 'c' :d";
        final StrTokenizer tok = new StrTokenizer(input, ':', '\'');
        tok.setTrimmerMatcher(StrMatcher.trimMatcher());
        tok.setIgnoreEmptyTokens(false);
        tok.setEmptyTokenAsNull(true);
        assertEquals("a", tok.next());
        assertEquals("b c", tok.next());
        assertEquals("d", tok.next());
        assertFalse(tok.hasNext());
    }

    @Test
    public void testBasicQuoted5() {
        final String input = "a: 'b'x'c' :d";
        final StrTokenizer tok = new StrTokenizer(input, ':', '\'');
        tok.setTrimmerMatcher(StrMatcher.trimMatcher());
        tok.setIgnoreEmptyTokens(false);
        tok.setEmptyTokenAsNull(true);
        assertEquals("a", tok.next());
        assertEquals("bxc", tok.next());
        assertEquals("d", tok.next());
        assertFalse(tok.hasNext());
    }

    @Test
    public void testBasicQuoted6() {
        final String input = "a:'b'\"c':d";
        final StrTokenizer tok = new StrTokenizer(input, ':');
        tok.setQuoteMatcher(StrMatcher.quoteMatcher());
        assertEquals("a", tok.next());
        assertEquals("b\"c:d", tok.next());
        assertFalse(tok.hasNext());
    }

    @Test
    public void testBasicQuoted7() {
        final String input = "a:\"There's a reason here\":b";
        final StrTokenizer tok = new StrTokenizer(input, ':');
        tok.setQuoteMatcher(StrMatcher.quoteMatcher());
        assertEquals("a", tok.next());
        assertEquals("There's a reason here", tok.next());
        assertEquals("b", tok.next());
        assertFalse(tok.hasNext());
    }

    @Test
    public void testBasicQuotedTrimmed1() {
        final String input = "a: 'b' :";
        final StrTokenizer tok = new StrTokenizer(input, ':', '\'');
        tok.setTrimmerMatcher(StrMatcher.trimMatcher());
        tok.setIgnoreEmptyTokens(false);
        tok.setEmptyTokenAsNull(true);
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertNull(tok.next());
        assertFalse(tok.hasNext());
    }

    @Test
    public void testBasicTrimmed1() {
        final String input = "a: b :  ";
        final StrTokenizer tok = new StrTokenizer(input, ':');
        tok.setTrimmerMatcher(StrMatcher.trimMatcher());
        tok.setIgnoreEmptyTokens(false);
        tok.setEmptyTokenAsNull(true);
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertNull(tok.next());
        assertFalse(tok.hasNext());
    }

    @Test
    public void testBasicTrimmed2() {
        final String input = "a:  b  :";
        final StrTokenizer tok = new StrTokenizer(input, ':');
        tok.setTrimmerMatcher(StrMatcher.stringMatcher("  "));
        tok.setIgnoreEmptyTokens(false);
        tok.setEmptyTokenAsNull(true);
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertNull(tok.next());
        assertFalse(tok.hasNext());
    }

    @Test
    public void testBasicIgnoreTrimmed1() {
        final String input = "a: bIGNOREc : ";
        final StrTokenizer tok = new StrTokenizer(input, ':');
        tok.setIgnoredMatcher(StrMatcher.stringMatcher("IGNORE"));
        tok.setTrimmerMatcher(StrMatcher.trimMatcher());
        tok.setIgnoreEmptyTokens(false);
        tok.setEmptyTokenAsNull(true);
        assertEquals("a", tok.next());
        assertEquals("bc", tok.next());
        assertNull(tok.next());
        assertFalse(tok.hasNext());
    }

    @Test
    public void testBasicIgnoreTrimmed2() {
        final String input = "IGNOREaIGNORE: IGNORE bIGNOREc IGNORE : IGNORE ";
        final StrTokenizer tok = new StrTokenizer(input, ':');
        tok.setIgnoredMatcher(StrMatcher.stringMatcher("IGNORE"));
        tok.setTrimmerMatcher(StrMatcher.trimMatcher());
        tok.setIgnoreEmptyTokens(false);
        tok.setEmptyTokenAsNull(true);
        assertEquals("a", tok.next());
        assertEquals("bc", tok.next());
        assertNull(tok.next());
        assertFalse(tok.hasNext());
    }

    @Test
    public void testBasicIgnoreTrimmed3() {
        final String input = "IGNOREaIGNORE: IGNORE bIGNOREc IGNORE : IGNORE ";
        final StrTokenizer tok = new StrTokenizer(input, ':');
        tok.setIgnoredMatcher(StrMatcher.stringMatcher("IGNORE"));
        tok.setIgnoreEmptyTokens(false);
        tok.setEmptyTokenAsNull(true);
        assertEquals("a", tok.next());
        assertEquals("  bc  ", tok.next());
        assertEquals("  ", tok.next());
        assertFalse(tok.hasNext());
    }

    @Test
    public void testBasicIgnoreTrimmed4() {
        final String input = "IGNOREaIGNORE: IGNORE 'bIGNOREc'IGNORE'd' IGNORE : IGNORE ";
        final StrTokenizer tok = new StrTokenizer(input, ':', '\'');
        tok.setIgnoredMatcher(StrMatcher.stringMatcher("IGNORE"));
        tok.setTrimmerMatcher(StrMatcher.trimMatcher());
        tok.setIgnoreEmptyTokens(false);
        tok.setEmptyTokenAsNull(true);
        assertEquals("a", tok.next());
        assertEquals("bIGNOREcd", tok.next());
        assertNull(tok.next());
        assertFalse(tok.hasNext());
    }

    @Test
    public void testListArray() {
        final String input = "a  b c";
        final StrTokenizer tok = new StrTokenizer(input);
        final String[] array = tok.getTokenArray();
        final List<?> list = tok.getTokenList();

        assertEquals(Arrays.asList(array), list);
        assertEquals(3, list.size());
    }

    private void testCSV(final String data) {
        this.testXSVAbc(StrTokenizer.getCSVInstance(data));
        this.testXSVAbc(StrTokenizer.getCSVInstance(data.toCharArray()));
    }

    @Test
    public void testCSVEmpty() {
        this.testEmpty(StrTokenizer.getCSVInstance());
        this.testEmpty(StrTokenizer.getCSVInstance(""));
    }

    @Test
    public void testCSVSimple() {
        this.testCSV(CSV_SIMPLE_FIXTURE);
    }

    @Test
    public void testCSVSimpleNeedsTrim() {
        this.testCSV("   " + CSV_SIMPLE_FIXTURE);
        this.testCSV("   \n\t  " + CSV_SIMPLE_FIXTURE);
        this.testCSV("   \n  " + CSV_SIMPLE_FIXTURE + "\n\n\r");
    }

    void testEmpty(final StrTokenizer tokenizer) {
        this.checkClone(tokenizer);
        assertFalse(tokenizer.hasNext());
        assertFalse(tokenizer.hasPrevious());
        assertNull(tokenizer.nextToken());
        assertEquals(0, tokenizer.size());
        assertThrows(NoSuchElementException.class, tokenizer::next);
    }

    @Test
    public void testGetContent() {
        final String input = "a   b c \"d e\" f ";
        StrTokenizer tok = new StrTokenizer(input);
        assertEquals(input, tok.getContent());

        tok = new StrTokenizer(input.toCharArray());
        assertEquals(input, tok.getContent());

        tok = new StrTokenizer();
        assertNull(tok.getContent());
    }

    @Test
    public void testChaining() {
        final StrTokenizer tok = new StrTokenizer();
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
     * {@code null}.
     */
    @Test
    public void testCloneNotSupportedException() {
        final Object notCloned = new StrTokenizer() {
            @Override
            Object cloneReset() throws CloneNotSupportedException {
                throw new CloneNotSupportedException("test");
            }
        }.clone();
        assertNull(notCloned);
    }

    @Test
    public void testCloneNull() {
        final StrTokenizer tokenizer = new StrTokenizer((char[]) null);
        // Start sanity check
        assertNull(tokenizer.nextToken());
        tokenizer.reset();
        assertNull(tokenizer.nextToken());
        // End sanity check
        final StrTokenizer clonedTokenizer = (StrTokenizer) tokenizer.clone();
        tokenizer.reset();
        assertNull(tokenizer.nextToken());
        assertNull(clonedTokenizer.nextToken());
    }

    @Test
    public void testCloneReset() {
        final char[] input = {'a'};
        final StrTokenizer tokenizer = new StrTokenizer(input);
        // Start sanity check
        assertEquals("a", tokenizer.nextToken());
        tokenizer.reset(input);
        assertEquals("a", tokenizer.nextToken());
        // End sanity check
        final StrTokenizer clonedTokenizer = (StrTokenizer) tokenizer.clone();
        input[0] = 'b';
        tokenizer.reset(input);
        assertEquals("b", tokenizer.nextToken());
        assertEquals("a", clonedTokenizer.nextToken());
    }

    @Test
    public void testConstructor_String() {
        StrTokenizer tok = new StrTokenizer("a b");
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertFalse(tok.hasNext());

        tok = new StrTokenizer("");
        assertFalse(tok.hasNext());

        tok = new StrTokenizer((String) null);
        assertFalse(tok.hasNext());
    }

    @Test
    public void testConstructor_String_char() {
        StrTokenizer tok = new StrTokenizer("a b", ' ');
        assertEquals(1, tok.getDelimiterMatcher().isMatch(" ".toCharArray(), 0, 0, 1));
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertFalse(tok.hasNext());

        tok = new StrTokenizer("", ' ');
        assertFalse(tok.hasNext());

        tok = new StrTokenizer((String) null, ' ');
        assertFalse(tok.hasNext());
    }

    @Test
    public void testConstructor_String_char_char() {
        StrTokenizer tok = new StrTokenizer("a b", ' ', '"');
        assertEquals(1, tok.getDelimiterMatcher().isMatch(" ".toCharArray(), 0, 0, 1));
        assertEquals(1, tok.getQuoteMatcher().isMatch("\"".toCharArray(), 0, 0, 1));
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertFalse(tok.hasNext());

        tok = new StrTokenizer("", ' ', '"');
        assertFalse(tok.hasNext());

        tok = new StrTokenizer((String) null, ' ', '"');
        assertFalse(tok.hasNext());
    }

    @Test
    public void testConstructor_charArray() {
        StrTokenizer tok = new StrTokenizer("a b".toCharArray());
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertFalse(tok.hasNext());

        tok = new StrTokenizer(new char[0]);
        assertFalse(tok.hasNext());

        tok = new StrTokenizer((char[]) null);
        assertFalse(tok.hasNext());
    }

    @Test
    public void testConstructor_charArray_char() {
        StrTokenizer tok = new StrTokenizer("a b".toCharArray(), ' ');
        assertEquals(1, tok.getDelimiterMatcher().isMatch(" ".toCharArray(), 0, 0, 1));
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertFalse(tok.hasNext());

        tok = new StrTokenizer(new char[0], ' ');
        assertFalse(tok.hasNext());

        tok = new StrTokenizer((char[]) null, ' ');
        assertFalse(tok.hasNext());
    }

    @Test
    public void testConstructor_charArray_char_char() {
        StrTokenizer tok = new StrTokenizer("a b".toCharArray(), ' ', '"');
        assertEquals(1, tok.getDelimiterMatcher().isMatch(" ".toCharArray(), 0, 0, 1));
        assertEquals(1, tok.getQuoteMatcher().isMatch("\"".toCharArray(), 0, 0, 1));
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertFalse(tok.hasNext());

        tok = new StrTokenizer(new char[0], ' ', '"');
        assertFalse(tok.hasNext());

        tok = new StrTokenizer((char[]) null, ' ', '"');
        assertFalse(tok.hasNext());
    }

    @Test
    public void testReset() {
        final StrTokenizer tok = new StrTokenizer("a b c");
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertEquals("c", tok.next());
        assertFalse(tok.hasNext());

        tok.reset();
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertEquals("c", tok.next());
        assertFalse(tok.hasNext());
    }

    @Test
    public void testReset_String() {
        final StrTokenizer tok = new StrTokenizer("x x x");
        tok.reset("d e");
        assertEquals("d", tok.next());
        assertEquals("e", tok.next());
        assertFalse(tok.hasNext());

        tok.reset((String) null);
        assertFalse(tok.hasNext());
    }

    @Test
    public void testReset_charArray() {
        final StrTokenizer tok = new StrTokenizer("x x x");

        final char[] array = {'a', 'b', 'c'};
        tok.reset(array);
        assertEquals("abc", tok.next());
        assertFalse(tok.hasNext());

        tok.reset((char[]) null);
        assertFalse(tok.hasNext());
    }

    @Test
    public void testTSV() {
        this.testXSVAbc(StrTokenizer.getTSVInstance(TSV_SIMPLE_FIXTURE));
        this.testXSVAbc(StrTokenizer.getTSVInstance(TSV_SIMPLE_FIXTURE.toCharArray()));
    }

    @Test
    public void testTSVEmpty() {
        this.testEmpty(StrTokenizer.getTSVInstance());
        this.testEmpty(StrTokenizer.getTSVInstance(""));
    }

    void testXSVAbc(final StrTokenizer tokenizer) {
        this.checkClone(tokenizer);
        assertEquals(-1, tokenizer.previousIndex());
        assertEquals(0, tokenizer.nextIndex());
        assertNull(tokenizer.previousToken());
        assertEquals("A", tokenizer.nextToken());
        assertEquals(1, tokenizer.nextIndex());
        assertEquals("b", tokenizer.nextToken());
        assertEquals(2, tokenizer.nextIndex());
        assertEquals("c", tokenizer.nextToken());
        assertEquals(3, tokenizer.nextIndex());
        assertNull(tokenizer.nextToken());
        assertEquals(3, tokenizer.nextIndex());
        assertEquals("c", tokenizer.previousToken());
        assertEquals(2, tokenizer.nextIndex());
        assertEquals("b", tokenizer.previousToken());
        assertEquals(1, tokenizer.nextIndex());
        assertEquals("A", tokenizer.previousToken());
        assertEquals(0, tokenizer.nextIndex());
        assertNull(tokenizer.previousToken());
        assertEquals(0, tokenizer.nextIndex());
        assertEquals(-1, tokenizer.previousIndex());
        assertEquals(3, tokenizer.size());
    }

    @Test
    public void testIteration() {
        final StrTokenizer tkn = new StrTokenizer("a b c");
        assertFalse(tkn.hasPrevious());
        assertThrows(NoSuchElementException.class, tkn::previous);
        assertTrue(tkn.hasNext());

        assertEquals("a", tkn.next());
        assertThrows(UnsupportedOperationException.class, tkn::remove);
        assertThrows(UnsupportedOperationException.class, () -> tkn.set("x"));
        assertThrows(UnsupportedOperationException.class, () -> tkn.add("y"));
        assertTrue(tkn.hasPrevious());
        assertTrue(tkn.hasNext());

        assertEquals("b", tkn.next());
        assertTrue(tkn.hasPrevious());
        assertTrue(tkn.hasNext());

        assertEquals("c", tkn.next());
        assertTrue(tkn.hasPrevious());
        assertFalse(tkn.hasNext());

        assertThrows(NoSuchElementException.class, tkn::next);
        assertTrue(tkn.hasPrevious());
        assertFalse(tkn.hasNext());
    }

    @Test
    public void testTokenizeSubclassInputChange() {
        final StrTokenizer tkn = new StrTokenizer("a b c d e") {
            @Override
            protected List<String> tokenize(final char[] chars, final int offset, final int count) {
                return super.tokenize("w x y z".toCharArray(), 2, 5);
            }
        };
        assertEquals("x", tkn.next());
        assertEquals("y", tkn.next());
    }

    @Test
    public void testTokenizeSubclassOutputChange() {
        final StrTokenizer tkn = new StrTokenizer("a b c") {
            @Override
            protected List<String> tokenize(final char[] chars, final int offset, final int count) {
                final List<String> list = super.tokenize(chars, offset, count);
                Collections.reverse(list);
                return list;
            }
        };
        assertEquals("c", tkn.next());
        assertEquals("b", tkn.next());
        assertEquals("a", tkn.next());
    }

    @Test
    public void testToString() {
        final StrTokenizer tkn = new StrTokenizer("a b c d e");
        assertEquals("StrTokenizer[not tokenized yet]", tkn.toString());
        tkn.next();
        assertEquals("StrTokenizer[a, b, c, d, e]", tkn.toString());
    }

}
