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

import org.junit.Test;
import static org.junit.Assert.*;
import org.apache.commons.lang3.text.StrBuilder;

/**
 * Unit tests {@link org.apache.commons.lang3.StringUtils} - StartsWith/EndsWith methods
 *
 * @version $Id$
 */
public class StringUtilsStartsEndsWithTest {
    private static final String foo    = "foo";
    private static final String bar    = "bar";
    private static final String foobar = "foobar";
    private static final String FOO    = "FOO";
    private static final String BAR    = "BAR";
    private static final String FOOBAR = "FOOBAR";

    //-----------------------------------------------------------------------

    /**
     * Test StringUtils.startsWith()
     */
    @Test
    public void testStartsWith() {
        assertTrue("startsWith(null, null)", StringUtils.startsWith(null, (String)null));
        assertFalse("startsWith(FOOBAR, null)", StringUtils.startsWith(FOOBAR, (String)null));
        assertFalse("startsWith(null, FOO)",    StringUtils.startsWith(null, FOO));
        assertTrue("startsWith(FOOBAR, \"\")",  StringUtils.startsWith(FOOBAR, ""));

        assertTrue("startsWith(foobar, foo)",  StringUtils.startsWith(foobar, foo));
        assertTrue("startsWith(FOOBAR, FOO)",  StringUtils.startsWith(FOOBAR, FOO));
        assertFalse("startsWith(foobar, FOO)", StringUtils.startsWith(foobar, FOO));
        assertFalse("startsWith(FOOBAR, foo)", StringUtils.startsWith(FOOBAR, foo));

        assertFalse("startsWith(foo, foobar)", StringUtils.startsWith(foo, foobar));
        assertFalse("startsWith(foo, foobar)", StringUtils.startsWith(bar, foobar));

        assertFalse("startsWith(foobar, bar)", StringUtils.startsWith(foobar, bar));
        assertFalse("startsWith(FOOBAR, BAR)", StringUtils.startsWith(FOOBAR, BAR));
        assertFalse("startsWith(foobar, BAR)", StringUtils.startsWith(foobar, BAR));
        assertFalse("startsWith(FOOBAR, bar)", StringUtils.startsWith(FOOBAR, bar));
    }

    /**
     * Test StringUtils.testStartsWithIgnoreCase()
     */
    @Test
    public void testStartsWithIgnoreCase() {
        assertTrue("startsWithIgnoreCase(null, null)",    StringUtils.startsWithIgnoreCase(null, (String)null));
        assertFalse("startsWithIgnoreCase(FOOBAR, null)", StringUtils.startsWithIgnoreCase(FOOBAR, (String)null));
        assertFalse("startsWithIgnoreCase(null, FOO)",    StringUtils.startsWithIgnoreCase(null, FOO));
        assertTrue("startsWithIgnoreCase(FOOBAR, \"\")",  StringUtils.startsWithIgnoreCase(FOOBAR, ""));

        assertTrue("startsWithIgnoreCase(foobar, foo)", StringUtils.startsWithIgnoreCase(foobar, foo));
        assertTrue("startsWithIgnoreCase(FOOBAR, FOO)", StringUtils.startsWithIgnoreCase(FOOBAR, FOO));
        assertTrue("startsWithIgnoreCase(foobar, FOO)", StringUtils.startsWithIgnoreCase(foobar, FOO));
        assertTrue("startsWithIgnoreCase(FOOBAR, foo)", StringUtils.startsWithIgnoreCase(FOOBAR, foo));

        assertFalse("startsWithIgnoreCase(foo, foobar)", StringUtils.startsWithIgnoreCase(foo, foobar));
        assertFalse("startsWithIgnoreCase(foo, foobar)", StringUtils.startsWithIgnoreCase(bar, foobar));

        assertFalse("startsWithIgnoreCase(foobar, bar)", StringUtils.startsWithIgnoreCase(foobar, bar));
        assertFalse("startsWithIgnoreCase(FOOBAR, BAR)", StringUtils.startsWithIgnoreCase(FOOBAR, BAR));
        assertFalse("startsWithIgnoreCase(foobar, BAR)", StringUtils.startsWithIgnoreCase(foobar, BAR));
        assertFalse("startsWithIgnoreCase(FOOBAR, bar)", StringUtils.startsWithIgnoreCase(FOOBAR, bar));
    }

    @Test
    public void testStartsWithAny() {
        assertFalse(StringUtils.startsWithAny(null, (String[])null));
        assertFalse(StringUtils.startsWithAny(null, "abc"));
        assertFalse(StringUtils.startsWithAny("abcxyz", (String[])null));
        assertFalse(StringUtils.startsWithAny("abcxyz"));
        assertTrue(StringUtils.startsWithAny("abcxyz", "abc"));
        assertTrue(StringUtils.startsWithAny("abcxyz", null, "xyz", "abc"));
        assertFalse(StringUtils.startsWithAny("abcxyz", null, "xyz", "abcd"));

        assertTrue("StringUtils.startsWithAny(abcxyz, StringBuilder(xyz), StringBuffer(abc))", StringUtils.startsWithAny("abcxyz", new StringBuilder("xyz"), new StringBuffer("abc")));
        assertTrue("StringUtils.startsWithAny( StrBuilder(abcxyz), StringBuilder(xyz), StringBuffer(abc))", StringUtils.startsWithAny( new StrBuilder("abcxyz"), new StringBuilder("xyz"), new StringBuffer("abc")));
    }
 

    /**
     * Test StringUtils.endsWith()
     */
    @Test
    public void testEndsWith() {
        assertTrue("endsWith(null, null)",    StringUtils.endsWith(null, (String)null));
        assertFalse("endsWith(FOOBAR, null)", StringUtils.endsWith(FOOBAR, (String)null));
        assertFalse("endsWith(null, FOO)",    StringUtils.endsWith(null, FOO));
        assertTrue("endsWith(FOOBAR, \"\")",  StringUtils.endsWith(FOOBAR, ""));

        assertFalse("endsWith(foobar, foo)", StringUtils.endsWith(foobar, foo));
        assertFalse("endsWith(FOOBAR, FOO)", StringUtils.endsWith(FOOBAR, FOO));
        assertFalse("endsWith(foobar, FOO)", StringUtils.endsWith(foobar, FOO));
        assertFalse("endsWith(FOOBAR, foo)", StringUtils.endsWith(FOOBAR, foo));

        assertFalse("endsWith(foo, foobar)", StringUtils.endsWith(foo, foobar));
        assertFalse("endsWith(foo, foobar)", StringUtils.endsWith(bar, foobar));

        assertTrue("endsWith(foobar, bar)",  StringUtils.endsWith(foobar, bar));
        assertTrue("endsWith(FOOBAR, BAR)",  StringUtils.endsWith(FOOBAR, BAR));
        assertFalse("endsWith(foobar, BAR)", StringUtils.endsWith(foobar, BAR));
        assertFalse("endsWith(FOOBAR, bar)", StringUtils.endsWith(FOOBAR, bar));
    }

    /**
     * Test StringUtils.endsWithIgnoreCase()
     */
    @Test
    public void testEndsWithIgnoreCase() {
        assertTrue("endsWithIgnoreCase(null, null)",    StringUtils.endsWithIgnoreCase(null, (String)null));
        assertFalse("endsWithIgnoreCase(FOOBAR, null)", StringUtils.endsWithIgnoreCase(FOOBAR, (String)null));
        assertFalse("endsWithIgnoreCase(null, FOO)",    StringUtils.endsWithIgnoreCase(null, FOO));
        assertTrue("endsWithIgnoreCase(FOOBAR, \"\")",  StringUtils.endsWithIgnoreCase(FOOBAR, ""));

        assertFalse("endsWithIgnoreCase(foobar, foo)", StringUtils.endsWithIgnoreCase(foobar, foo));
        assertFalse("endsWithIgnoreCase(FOOBAR, FOO)", StringUtils.endsWithIgnoreCase(FOOBAR, FOO));
        assertFalse("endsWithIgnoreCase(foobar, FOO)", StringUtils.endsWithIgnoreCase(foobar, FOO));
        assertFalse("endsWithIgnoreCase(FOOBAR, foo)", StringUtils.endsWithIgnoreCase(FOOBAR, foo));

        assertFalse("endsWithIgnoreCase(foo, foobar)", StringUtils.endsWithIgnoreCase(foo, foobar));
        assertFalse("endsWithIgnoreCase(foo, foobar)", StringUtils.endsWithIgnoreCase(bar, foobar));

        assertTrue("endsWithIgnoreCase(foobar, bar)", StringUtils.endsWithIgnoreCase(foobar, bar));
        assertTrue("endsWithIgnoreCase(FOOBAR, BAR)", StringUtils.endsWithIgnoreCase(FOOBAR, BAR));
        assertTrue("endsWithIgnoreCase(foobar, BAR)", StringUtils.endsWithIgnoreCase(foobar, BAR));
        assertTrue("endsWithIgnoreCase(FOOBAR, bar)", StringUtils.endsWithIgnoreCase(FOOBAR, bar));

        // javadoc
        assertTrue(StringUtils.endsWithIgnoreCase("abcdef", "def"));
        assertTrue(StringUtils.endsWithIgnoreCase("ABCDEF", "def"));
        assertFalse(StringUtils.endsWithIgnoreCase("ABCDEF", "cde"));
    }

    @Test
    public void testEndsWithAny() {
        assertFalse("StringUtils.endsWithAny(null, null)", StringUtils.endsWithAny(null, (String)null));
        assertFalse("StringUtils.endsWithAny(null, new String[] {abc})", StringUtils.endsWithAny(null, new String[] {"abc"}));
        assertFalse("StringUtils.endsWithAny(abcxyz, null)", StringUtils.endsWithAny("abcxyz", (String)null));
        assertTrue("StringUtils.endsWithAny(abcxyz, new String[] {\"\"})", StringUtils.endsWithAny("abcxyz", new String[] {""}));
        assertTrue("StringUtils.endsWithAny(abcxyz, new String[] {xyz})", StringUtils.endsWithAny("abcxyz", new String[] {"xyz"}));
        assertTrue("StringUtils.endsWithAny(abcxyz, new String[] {null, xyz, abc})", StringUtils.endsWithAny("abcxyz", new String[] {null, "xyz", "abc"}));
        assertFalse("StringUtils.endsWithAny(defg, new String[] {null, xyz, abc})", StringUtils.endsWithAny("defg", new String[] {null, "xyz", "abc"}));

        assertTrue("StringUtils.endsWithAny(abcxyz, StringBuilder(abc), StringBuffer(xyz))", StringUtils.endsWithAny("abcxyz", new StringBuilder("abc"), new StringBuffer("xyz")));
        assertTrue("StringUtils.endsWithAny( StrBuilder(abcxyz), StringBuilder(abc), StringBuffer(xyz))", StringUtils.endsWithAny( new StrBuilder("abcxyz"), new StringBuilder("abc"), new StringBuffer("xyz")));
    }


}
