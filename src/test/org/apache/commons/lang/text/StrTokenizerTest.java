/*
 * Copyright 2003-2005 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
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

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

import org.apache.commons.lang.ObjectUtils;

/**
 * Unit test for Tokenizer.
 *
 * @author Matthew Inger
 */
public class StrTokenizerTest extends TestCase {
    
    /**
     * JUnit constructor.
     * @param name
     */
    public StrTokenizerTest(String name) {
        super(name);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(StrTokenizerTest.class);
        suite.setName("TokenizerTest Tests");
        return suite;
    }


    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    //-----------------------------------------------------------------------
    public void test1() {

        String input = "a;b;c;\"d;\"\"e\";f; ; ;";
        StrTokenizer tok = new StrTokenizer(input);
        tok.setDelimiterChar(';');
        tok.setQuoteChar('"');
        tok.setIgnoredMatcher(StrTokenizer.TRIM_MATCHER);
        tok.setIgnoreEmptyTokens(false);
        String tokens [] = tok.getAllTokens();

        String expected[] = new String[]
        {
            "a",
            "b",
            "c",
            "d;\"e",
            "f",
            "",
            "",
            "",
        };

        assertTrue(tokens.length == expected.length);
        for (int i = 0; i < expected.length; i++) {
            assertTrue("token[" + i + "] was '" + tokens[i]
                    + "' but was expected to be '" + expected[i]
                    + "'",
                    ObjectUtils.equals(expected[i], tokens[i]));
        }

    }


    public void test2() {

        String input = "a;b;c ;\"d;\"\"e\";f; ; ;";
        StrTokenizer tok = new StrTokenizer(input);
        tok.setDelimiterChar(';');
        tok.setQuoteChar('"');
        tok.setIgnoredMatcher(StrTokenizer.NONE_MATCHER);
        tok.setIgnoreEmptyTokens(false);
        String tokens [] = tok.getAllTokens();

        String expected[] = new String[]
        {
            "a",
            "b",
            "c ",
            "d;\"e",
            "f",
            " ",
            " ",
            "",
        };

        assertTrue(tokens.length == expected.length);
        for (int i = 0; i < expected.length; i++) {
            assertTrue("token[" + i + "] was '" + tokens[i]
                    + "' but was expected to be '" + expected[i]
                    + "'",
                    ObjectUtils.equals(expected[i], tokens[i]));
        }

    }


    public void test3() {

        String input = "a;b; c;\"d;\"\"e\";f; ; ;";
        StrTokenizer tok = new StrTokenizer(input);
        tok.setDelimiterChar(';');
        tok.setQuoteChar('"');
        tok.setIgnoredMatcher(StrTokenizer.NONE_MATCHER);
        tok.setIgnoreEmptyTokens(false);
        String tokens [] = tok.getAllTokens();

        String expected[] = new String[]
        {
            "a",
            "b",
            " c",
            "d;\"e",
            "f",
            " ",
            " ",
            "",
        };

        assertTrue(tokens.length == expected.length);
        for (int i = 0; i < expected.length; i++) {
            assertTrue("token[" + i + "] was '" + tokens[i]
                    + "' but was expected to be '" + expected[i]
                    + "'",
                    ObjectUtils.equals(expected[i], tokens[i]));
        }

    }


    public void test4() {

        String input = "a;b; c;\"d;\"\"e\";f; ; ;";
        StrTokenizer tok = new StrTokenizer(input);
        tok.setDelimiterChar(';');
        tok.setQuoteChar('"');
        tok.setIgnoredMatcher(StrTokenizer.TRIM_MATCHER);
        tok.setIgnoreEmptyTokens(true);
        String tokens [] = tok.getAllTokens();

        String expected[] = new String[]
        {
            "a",
            "b",
            "c",
            "d;\"e",
            "f",
        };

        assertTrue(tokens.length == expected.length);
        for (int i = 0; i < expected.length; i++) {
            assertTrue("token[" + i + "] was '" + tokens[i]
                    + "' but was expected to be '" + expected[i]
                    + "'",
                    ObjectUtils.equals(expected[i], tokens[i]));
        }

    }


    public void test5() {

        String input = "a;b; c;\"d;\"\"e\";f; ; ;";
        StrTokenizer tok = new StrTokenizer(input);
        tok.setDelimiterChar(';');
        tok.setQuoteChar('"');
        tok.setIgnoredMatcher(StrTokenizer.TRIM_MATCHER);
        tok.setIgnoreEmptyTokens(false);
        tok.setEmptyTokenAsNull(true);
        String tokens [] = tok.getAllTokens();

        String expected[] = new String[]
        {
            "a",
            "b",
            "c",
            "d;\"e",
            "f",
            null,
            null,
            null,
        };

        assertTrue(tokens.length == expected.length);
        for (int i = 0; i < expected.length; i++) {
            assertTrue("token[" + i + "] was '" + tokens[i]
                    + "' but was expected to be '" + expected[i]
                    + "'",
                    ObjectUtils.equals(expected[i], tokens[i]));
        }

    }


    public void test6() {

        String input = "a;b; c;\"d;\"\"e\";f; ; ;";
        StrTokenizer tok = new StrTokenizer(input);
        tok.setDelimiterChar(';');
        tok.setQuoteChar('"');
        tok.setIgnoredMatcher(StrTokenizer.TRIM_MATCHER);
        tok.setIgnoreEmptyTokens(false);
//        tok.setTreatingEmptyAsNull(true);
        String tokens [] = tok.getAllTokens();

        String expected[] = new String[]
        {
            "a",
            "b",
            " c",
            "d;\"e",
            "f",
            null,
            null,
            null,
        };

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

        assertTrue(tokens.length == expected.length);

        assertTrue("could not cycle through entire token list"
                + " using the 'hasNext' and 'next' methods",
                nextCount == expected.length);

        assertTrue("could not cycle through entire token list"
                + " using the 'hasPrevious' and 'previous' methods",
                prevCount == expected.length);

    }


    public void test7() {

        String input = "a   b c \"d e\" f ";
        StrTokenizer tok = new StrTokenizer(input);
        tok.setDelimiterMatcher(StrTokenizer.SPACE_MATCHER);
        tok.setQuoteMatcher(StrTokenizer.DOUBLE_QUOTE_MATCHER);
        tok.setIgnoredMatcher(StrTokenizer.NONE_MATCHER);
        tok.setIgnoreEmptyTokens(false);
        String tokens [] = tok.getAllTokens();

        String expected[] = new String[]
        {
            "a",
            "",
            "",
            "b",
            "c",
            "d e",
            "f",
            "",
        };

        assertTrue(tokens.length == expected.length);
        for (int i = 0; i < expected.length; i++) {
            assertTrue("token[" + i + "] was '" + tokens[i]
                    + "' but was expected to be '" + expected[i]
                    + "'",
                    ObjectUtils.equals(expected[i], tokens[i]));
        }

    }


    public void test8() {

        String input = "a   b c \"d e\" f ";
        StrTokenizer tok = new StrTokenizer(input);
        tok.setDelimiterMatcher(StrTokenizer.SPACE_MATCHER);
        tok.setQuoteMatcher(StrTokenizer.DOUBLE_QUOTE_MATCHER);
        tok.setIgnoredMatcher(StrTokenizer.NONE_MATCHER);
        tok.setIgnoreEmptyTokens(true);
        String tokens [] = tok.getAllTokens();

        String expected[] = new String[]
        {
            "a",
            "b",
            "c",
            "d e",
            "f",
        };

        assertTrue(tokens.length == expected.length);
        for (int i = 0; i < expected.length; i++) {
            assertTrue("token[" + i + "] was '" + tokens[i]
                    + "' but was expected to be '" + expected[i]
                    + "'",
                    ObjectUtils.equals(expected[i], tokens[i]));
        }

    }

    public void testBasic1() {
        String input = "a  b c";
        StrTokenizer tok = new StrTokenizer(input);
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertEquals("c", tok.next());
    }
    
    public void testBasic2() {
        String input = "a \nb\fc";
        StrTokenizer tok = new StrTokenizer(input);
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertEquals("c", tok.next());
    }
    
    public void testBasic3() {
        String input = "a \nb\u0001\fc";
        StrTokenizer tok = new StrTokenizer(input);
        assertEquals("a", tok.next());
        assertEquals("b\u0001", tok.next());
        assertEquals("c", tok.next());
    }
    
    public void testBasic4() {
        String input = "a \"b\" c";
        StrTokenizer tok = new StrTokenizer(input);
        assertEquals("a", tok.next());
        assertEquals("\"b\"", tok.next());
        assertEquals("c", tok.next());
    }
    
    public void testBasicQuoted1() {
        String input = "a \"b\" c";
        StrTokenizer tok = new StrTokenizer(input, ' ', '"');
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertEquals("c", tok.next());
    }
    
    public void testBasicDelim1() {
        String input = "a:b:c";
        StrTokenizer tok = new StrTokenizer(input, ':');
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertEquals("c", tok.next());
    }
    
    public void testBasicDelim2() {
        String input = "a:b:c";
        StrTokenizer tok = new StrTokenizer(input, ',');
        assertEquals("a:b:c", tok.next());
    }
    
    public void testBasicEmpty1() {
        String input = "a  b c";
        StrTokenizer tok = new StrTokenizer(input);
        tok.setIgnoreEmptyTokens(false);
        assertEquals("a", tok.next());
        assertEquals("", tok.next());
        assertEquals("b", tok.next());
        assertEquals("c", tok.next());
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
    }
    
    public void testGetContent() {
        String input = "a   b c \"d e\" f ";
        StrTokenizer tok = new StrTokenizer(input);
        assertSame(input, tok.getContent());
        
        tok = new StrTokenizer(input.toCharArray());
        assertEquals(input, tok.getContent());
    }

    public void testReset() {
        String input = "a b c";
        StrTokenizer tok = new StrTokenizer(input);
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertEquals("c", tok.next());
        tok.reset();
        assertEquals("a", tok.next());
        assertEquals("b", tok.next());
        assertEquals("c", tok.next());
        tok.reset("d e");
        assertEquals("d", tok.next());
        assertEquals("e", tok.next());
        tok.reset("f g".toCharArray());
        assertEquals("f", tok.next());
        assertEquals("g", tok.next());
    }
    
    public void testMatcher() {
        assertEquals(1, StrTokenizer.SPACE_MATCHER.isMatch(new char[] {' '}, 1, 0));
        assertEquals(0, StrTokenizer.SPACE_MATCHER.isMatch(new char[] {'\n'}, 1, 0));
        assertEquals(0, StrTokenizer.SPACE_MATCHER.isMatch(new char[] {'\u0001'}, 1, 0));
        
        assertEquals(1, StrTokenizer.TRIM_MATCHER.isMatch(new char[] {' '}, 1, 0));
        assertEquals(1, StrTokenizer.TRIM_MATCHER.isMatch(new char[] {'\n'}, 1, 0));
        assertEquals(1, StrTokenizer.TRIM_MATCHER.isMatch(new char[] {'\u0001'}, 1, 0));
        
        assertEquals(1, StrTokenizer.SPLIT_MATCHER.isMatch(new char[] {' '}, 1, 0));
        assertEquals(1, StrTokenizer.SPLIT_MATCHER.isMatch(new char[] {'\n'}, 1, 0));
        assertEquals(0, StrTokenizer.SPLIT_MATCHER.isMatch(new char[] {'\u0001'}, 1, 0));
    }
    
}
