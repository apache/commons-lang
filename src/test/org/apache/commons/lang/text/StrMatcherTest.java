/*
 * Copyright 2005 The Apache Software Foundation.
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

/**
 * Unit tests for {@link org.apache.commons.lang.text.StrMatcher}.
 *
 * @version $Id$
 */
public class StrMatcherTest extends TestCase {

    private static final char[] BUFFER1 = "0,1\t2 3\n\r\f\u0000'\"".toCharArray();

    private static final char[] BUFFER2 = "abcdef".toCharArray();

    /**
     * Main method.
     * 
     * @param args  command line arguments, ignored
     */
    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    /**
     * Return a new test suite containing this test case.
     * 
     * @return a new test suite containing this test case
     */
    public static Test suite() {
        TestSuite suite = new TestSuite(StrMatcherTest.class);
        suite.setName("StrMatcher Tests");
        return suite;
    }

    /**
     * Create a new test case with the specified name.
     * 
     * @param name  the name
     */
    public StrMatcherTest(String name) {
        super(name);
    }

    //-----------------------------------------------------------------------
    public void testCommaMatcher() {
        StrMatcher matcher = StrMatcher.commaMatcher();
        assertSame(matcher, StrMatcher.commaMatcher());
        assertEquals(0, matcher.isMatch(BUFFER1, 0, 0, BUFFER1.length));
        assertEquals(1, matcher.isMatch(BUFFER1, 1, 0, BUFFER1.length));
        assertEquals(0, matcher.isMatch(BUFFER1, 2, 0, BUFFER1.length));
    }

    //-----------------------------------------------------------------------
    public void testTabMatcher() {
        StrMatcher matcher = StrMatcher.tabMatcher();
        assertSame(matcher, StrMatcher.tabMatcher());
        assertEquals(0, matcher.isMatch(BUFFER1, 2, 0, BUFFER1.length));
        assertEquals(1, matcher.isMatch(BUFFER1, 3, 0, BUFFER1.length));
        assertEquals(0, matcher.isMatch(BUFFER1, 4, 0, BUFFER1.length));
    }

    //-----------------------------------------------------------------------
    public void testSpaceMatcher() {
        StrMatcher matcher = StrMatcher.spaceMatcher();
        assertSame(matcher, StrMatcher.spaceMatcher());
        assertEquals(0, matcher.isMatch(BUFFER1, 4, 0, BUFFER1.length));
        assertEquals(1, matcher.isMatch(BUFFER1, 5, 0, BUFFER1.length));
        assertEquals(0, matcher.isMatch(BUFFER1, 6, 0, BUFFER1.length));
    }

    //-----------------------------------------------------------------------
    public void testSplitMatcher() {
        StrMatcher matcher = StrMatcher.splitMatcher();
        assertSame(matcher, StrMatcher.splitMatcher());
        assertEquals(0, matcher.isMatch(BUFFER1, 2, 0, BUFFER1.length));
        assertEquals(1, matcher.isMatch(BUFFER1, 3, 0, BUFFER1.length));
        assertEquals(0, matcher.isMatch(BUFFER1, 4, 0, BUFFER1.length));
        assertEquals(1, matcher.isMatch(BUFFER1, 5, 0, BUFFER1.length));
        assertEquals(0, matcher.isMatch(BUFFER1, 6, 0, BUFFER1.length));
        assertEquals(1, matcher.isMatch(BUFFER1, 7, 0, BUFFER1.length));
        assertEquals(1, matcher.isMatch(BUFFER1, 8, 0, BUFFER1.length));
        assertEquals(1, matcher.isMatch(BUFFER1, 9, 0, BUFFER1.length));
        assertEquals(0, matcher.isMatch(BUFFER1, 10, 0, BUFFER1.length));
    }

    //-----------------------------------------------------------------------
    public void testTrimMatcher() {
        StrMatcher matcher = StrMatcher.trimMatcher();
        assertSame(matcher, StrMatcher.trimMatcher());
        assertEquals(0, matcher.isMatch(BUFFER1, 2, 0, BUFFER1.length));
        assertEquals(1, matcher.isMatch(BUFFER1, 3, 0, BUFFER1.length));
        assertEquals(0, matcher.isMatch(BUFFER1, 4, 0, BUFFER1.length));
        assertEquals(1, matcher.isMatch(BUFFER1, 5, 0, BUFFER1.length));
        assertEquals(0, matcher.isMatch(BUFFER1, 6, 0, BUFFER1.length));
        assertEquals(1, matcher.isMatch(BUFFER1, 7, 0, BUFFER1.length));
        assertEquals(1, matcher.isMatch(BUFFER1, 8, 0, BUFFER1.length));
        assertEquals(1, matcher.isMatch(BUFFER1, 9, 0, BUFFER1.length));
        assertEquals(1, matcher.isMatch(BUFFER1, 10, 0, BUFFER1.length));
    }

    //-----------------------------------------------------------------------
    public void testSingleQuoteMatcher() {
        StrMatcher matcher = StrMatcher.singleQuoteMatcher();
        assertSame(matcher, StrMatcher.singleQuoteMatcher());
        assertEquals(0, matcher.isMatch(BUFFER1, 10, 0, BUFFER1.length));
        assertEquals(1, matcher.isMatch(BUFFER1, 11, 0, BUFFER1.length));
        assertEquals(0, matcher.isMatch(BUFFER1, 12, 0, BUFFER1.length));
    }

    //-----------------------------------------------------------------------
    public void testDoubleQuoteMatcher() {
        StrMatcher matcher = StrMatcher.doubleQuoteMatcher();
        assertSame(matcher, StrMatcher.doubleQuoteMatcher());
        assertEquals(0, matcher.isMatch(BUFFER1, 11, 0, BUFFER1.length));
        assertEquals(1, matcher.isMatch(BUFFER1, 12, 0, BUFFER1.length));
    }

    //-----------------------------------------------------------------------
    public void testQuoteMatcher() {
        StrMatcher matcher = StrMatcher.quoteMatcher();
        assertSame(matcher, StrMatcher.quoteMatcher());
        assertEquals(0, matcher.isMatch(BUFFER1, 10, 0, BUFFER1.length));
        assertEquals(1, matcher.isMatch(BUFFER1, 11, 0, BUFFER1.length));
        assertEquals(1, matcher.isMatch(BUFFER1, 12, 0, BUFFER1.length));
    }

    //-----------------------------------------------------------------------
    public void testNoneMatcher() {
        StrMatcher matcher = StrMatcher.noneMatcher();
        assertSame(matcher, StrMatcher.noneMatcher());
        assertEquals(0, matcher.isMatch(BUFFER1, 0, 0, BUFFER1.length));
        assertEquals(0, matcher.isMatch(BUFFER1, 1, 0, BUFFER1.length));
        assertEquals(0, matcher.isMatch(BUFFER1, 2, 0, BUFFER1.length));
        assertEquals(0, matcher.isMatch(BUFFER1, 3, 0, BUFFER1.length));
        assertEquals(0, matcher.isMatch(BUFFER1, 4, 0, BUFFER1.length));
        assertEquals(0, matcher.isMatch(BUFFER1, 5, 0, BUFFER1.length));
        assertEquals(0, matcher.isMatch(BUFFER1, 6, 0, BUFFER1.length));
        assertEquals(0, matcher.isMatch(BUFFER1, 7, 0, BUFFER1.length));
        assertEquals(0, matcher.isMatch(BUFFER1, 8, 0, BUFFER1.length));
        assertEquals(0, matcher.isMatch(BUFFER1, 9, 0, BUFFER1.length));
        assertEquals(0, matcher.isMatch(BUFFER1, 10, 0, BUFFER1.length));
        assertEquals(0, matcher.isMatch(BUFFER1, 11, 0, BUFFER1.length));
        assertEquals(0, matcher.isMatch(BUFFER1, 12, 0, BUFFER1.length));
    }

    //-----------------------------------------------------------------------
    public void testCharMatcher_char() {
        StrMatcher matcher = StrMatcher.charMatcher('c');
        assertEquals(0, matcher.isMatch(BUFFER2, 0, 0, BUFFER2.length));
        assertEquals(0, matcher.isMatch(BUFFER2, 1, 0, BUFFER2.length));
        assertEquals(1, matcher.isMatch(BUFFER2, 2, 0, BUFFER2.length));
        assertEquals(0, matcher.isMatch(BUFFER2, 3, 0, BUFFER2.length));
        assertEquals(0, matcher.isMatch(BUFFER2, 4, 0, BUFFER2.length));
        assertEquals(0, matcher.isMatch(BUFFER2, 5, 0, BUFFER2.length));
    }

    //-----------------------------------------------------------------------
    public void testCharSetMatcher_String() {
        StrMatcher matcher = StrMatcher.charSetMatcher("ace");
        assertEquals(1, matcher.isMatch(BUFFER2, 0, 0, BUFFER2.length));
        assertEquals(0, matcher.isMatch(BUFFER2, 1, 0, BUFFER2.length));
        assertEquals(1, matcher.isMatch(BUFFER2, 2, 0, BUFFER2.length));
        assertEquals(0, matcher.isMatch(BUFFER2, 3, 0, BUFFER2.length));
        assertEquals(1, matcher.isMatch(BUFFER2, 4, 0, BUFFER2.length));
        assertEquals(0, matcher.isMatch(BUFFER2, 5, 0, BUFFER2.length));
        assertSame(StrMatcher.noneMatcher(), StrMatcher.charSetMatcher(""));
        assertSame(StrMatcher.noneMatcher(), StrMatcher.charSetMatcher((String) null));
        assertTrue(StrMatcher.charSetMatcher("a") instanceof StrMatcher.CharMatcher);
    }

    //-----------------------------------------------------------------------
    public void testCharSetMatcher_charArray() {
        StrMatcher matcher = StrMatcher.charSetMatcher("ace".toCharArray());
        assertEquals(1, matcher.isMatch(BUFFER2, 0, 0, BUFFER2.length));
        assertEquals(0, matcher.isMatch(BUFFER2, 1, 0, BUFFER2.length));
        assertEquals(1, matcher.isMatch(BUFFER2, 2, 0, BUFFER2.length));
        assertEquals(0, matcher.isMatch(BUFFER2, 3, 0, BUFFER2.length));
        assertEquals(1, matcher.isMatch(BUFFER2, 4, 0, BUFFER2.length));
        assertEquals(0, matcher.isMatch(BUFFER2, 5, 0, BUFFER2.length));
        assertSame(StrMatcher.noneMatcher(), StrMatcher.charSetMatcher(new char[0]));
        assertSame(StrMatcher.noneMatcher(), StrMatcher.charSetMatcher((char[]) null));
        assertTrue(StrMatcher.charSetMatcher("a".toCharArray()) instanceof StrMatcher.CharMatcher);
    }

    //-----------------------------------------------------------------------
    public void testStringMatcher_String() {
        StrMatcher matcher = StrMatcher.stringMatcher("bc");
        assertEquals(0, matcher.isMatch(BUFFER2, 0, 0, BUFFER2.length));
        assertEquals(2, matcher.isMatch(BUFFER2, 1, 0, BUFFER2.length));
        assertEquals(0, matcher.isMatch(BUFFER2, 2, 0, BUFFER2.length));
        assertEquals(0, matcher.isMatch(BUFFER2, 3, 0, BUFFER2.length));
        assertEquals(0, matcher.isMatch(BUFFER2, 4, 0, BUFFER2.length));
        assertEquals(0, matcher.isMatch(BUFFER2, 5, 0, BUFFER2.length));
        assertSame(StrMatcher.noneMatcher(), StrMatcher.stringMatcher(""));
        assertSame(StrMatcher.noneMatcher(), StrMatcher.stringMatcher((String) null));
    }

    //-----------------------------------------------------------------------
    public void testMatcherIndices() {
        // remember that the API contract is tight for the isMatch() method
        // all the onus is on the caller, so invalid inputs are not
        // the concern of StrMatcher, and are not bugs
        StrMatcher matcher = StrMatcher.stringMatcher("bc");
        assertEquals(2, matcher.isMatch(BUFFER2, 1, 1, BUFFER2.length));
        assertEquals(2, matcher.isMatch(BUFFER2, 1, 0, 3));
        assertEquals(0, matcher.isMatch(BUFFER2, 1, 0, 2));
    }

}
