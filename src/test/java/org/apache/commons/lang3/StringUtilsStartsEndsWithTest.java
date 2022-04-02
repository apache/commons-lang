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

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;


/**
 * Unit tests {@link org.apache.commons.lang3.StringUtils} - StartsAndEndsWith/StartsWith/EndsWith methods
 */
public class StringUtilsStartsEndsWithTest {
    private static final String foo       = "foo";
    private static final String bar       = "bar";
    private static final String foobar    = "foobar";
    private static final String foobarfoo = "foobarfoo";
    private static final String FOO       = "FOO";
    private static final String BAR       = "BAR";
    private static final String FOOBAR    = "FOOBAR";
    private static final String FOOBARFOO = "FOOBARFOO";


    /**
     * Test StringUtils.startsAndEndsWith(str, other)
     */
    @Test
    public void testStartsAndEndsWith_Other() {
        assertTrue(StringUtils.startsAndEndsWith(null, null), "startsAndEndsWith(null, null)");
        assertFalse(StringUtils.startsAndEndsWith(FOOBARFOO, null), "startsAndEndsWith(FOOBARFOO, null)");
        assertFalse(StringUtils.startsAndEndsWith(null, FOO), "startsAndEndsWith(null, FOO)");
        assertTrue(StringUtils.startsAndEndsWith(FOOBARFOO, ""), "startsAndEndsWith(FOOBARFOO, \"\")");

        assertTrue(StringUtils.startsAndEndsWith(foobarfoo, foo), "startsAndEndsWith(foobarfoo, foo)");
        assertTrue(StringUtils.startsAndEndsWith(FOOBARFOO, FOO), "startsAndEndsWith(FOOBARFOO, FOO)");
        assertFalse(StringUtils.startsAndEndsWith(foobarfoo, FOO), "startsAndEndsWith(foobarfoo, FOO)");
        assertFalse(StringUtils.startsAndEndsWith(FOOBARFOO, foo), "startsAndEndsWith(FOOBARFOO, foo)");

        assertFalse(StringUtils.startsAndEndsWith(foo, foobarfoo), "startsAndEndsWith(foo, foobarfoo)");
        assertFalse(StringUtils.startsAndEndsWith(bar, foobarfoo), "startsAndEndsWith(foo, foobarfoo)");

        assertFalse(StringUtils.startsAndEndsWith(foobarfoo, bar), "startsAndEndsWith(foobarfoo, bar)");
        assertFalse(StringUtils.startsAndEndsWith(FOOBARFOO, BAR), "startsAndEndsWith(FOOBARFOO, BAR)");
        assertFalse(StringUtils.startsAndEndsWith(foobarfoo, BAR), "startsAndEndsWith(foobarfoo, BAR)");
        assertFalse(StringUtils.startsAndEndsWith(FOOBARFOO, bar), "startsAndEndsWith(FOOBARFOO, bar)");
    }

    /**
     * Test StringUtils.startsAndEndsWith(str, prefix, suffix)
     */
    @Test
    public void testStartsAndEndsWith_PrefixAndSuffix() {
        assertTrue(StringUtils.startsAndEndsWith(null, null, null), "startsAndEndsWith(null, null, null)");
        assertFalse(StringUtils.startsAndEndsWith(FOOBARFOO, null, null), "startsAndEndsWith(FOOBARFOO, null, null)");
        assertFalse(StringUtils.startsAndEndsWith(null, FOO, FOO), "startsAndEndsWith(null, FOO, FOO)");
        assertTrue(StringUtils.startsAndEndsWith(FOOBARFOO, "", ""), "startsAndEndsWith(FOOBARFOO, \"\", \"\")");

        assertTrue(StringUtils.startsAndEndsWith(foobarfoo, foo, foo), "startsAndEndsWith(foobarfoo, foo, foo)");
        assertTrue(StringUtils.startsAndEndsWith(FOOBARFOO, FOO, FOO), "startsAndEndsWith(FOOBARFOO, FOO, FOO)");
        assertFalse(StringUtils.startsAndEndsWith(foobarfoo, FOO, FOO), "startsAndEndsWith(foobarfoo, FOO, FOO)");
        assertFalse(StringUtils.startsAndEndsWith(FOOBARFOO, foo, foo), "startsAndEndsWith(FOOBARFOO, foo, foo)");

        assertFalse(StringUtils.startsAndEndsWith(foo, foobarfoo, foobarfoo), "startsAndEndsWith(foo, foobarfoo, foobarfoo)");
        assertFalse(StringUtils.startsAndEndsWith(bar, foobarfoo, foobarfoo), "startsAndEndsWith(foo, foobarfoo, foobarfoo)");

        assertFalse(StringUtils.startsAndEndsWith(foobarfoo, bar, bar), "startsAndEndsWith(foobarfoo, bar, bar)");
        assertFalse(StringUtils.startsAndEndsWith(FOOBARFOO, BAR, BAR), "startsAndEndsWith(FOOBARFOO, BAR, BAR)");
        assertFalse(StringUtils.startsAndEndsWith(foobarfoo, BAR, BAR), "startsAndEndsWith(foobarfoo, BAR, BAR)");
        assertFalse(StringUtils.startsAndEndsWith(FOOBARFOO, bar, bar), "startsAndEndsWith(FOOBARFOO, bar, bar)");
    }

    /**
     * Test StringUtils.startsAndEndsWithIgnoreCase(str, other)
     */
    @Test
    public void testStartsAndEndsWithIgnoreCase_Other() {
        assertTrue(StringUtils.startsAndEndsWithIgnoreCase(null, null), "startsAndEndsWithIgnoreCase(null, null)");
        assertFalse(StringUtils.startsAndEndsWithIgnoreCase(FOOBARFOO, null), "startsAndEndsWithIgnoreCase(FOOBARFOO, null)");
        assertFalse(StringUtils.startsAndEndsWithIgnoreCase(null, FOO), "startsAndEndsWithIgnoreCase(null, FOO)");
        assertTrue(StringUtils.startsAndEndsWithIgnoreCase(FOOBARFOO, ""), "startsAndEndsWithIgnoreCase(FOOBARFOO, \"\")");

        assertTrue(StringUtils.startsAndEndsWithIgnoreCase(foobarfoo, foo), "startsAndEndsWithIgnoreCase(foobarfoo, foo)");
        assertTrue(StringUtils.startsAndEndsWithIgnoreCase(FOOBARFOO, FOO), "startsAndEndsWithIgnoreCase(FOOBARFOO, FOO)");
        assertTrue(StringUtils.startsAndEndsWithIgnoreCase(foobarfoo, FOO), "startsAndEndsWithIgnoreCase(foobarfoo, FOO)");
        assertTrue(StringUtils.startsAndEndsWithIgnoreCase(FOOBARFOO, foo), "startsAndEndsWithIgnoreCase(FOOBARFOO, foo)");

        assertFalse(StringUtils.startsAndEndsWithIgnoreCase(foo, foobarfoo), "startsAndEndsWithIgnoreCase(foo, foobarfoo)");
        assertFalse(StringUtils.startsAndEndsWithIgnoreCase(bar, foobarfoo), "startsAndEndsWithIgnoreCase(foo, foobarfoo)");

        assertFalse(StringUtils.startsAndEndsWithIgnoreCase(foobarfoo, bar), "startsAndEndsWithIgnoreCase(foobarfoo, bar)");
        assertFalse(StringUtils.startsAndEndsWithIgnoreCase(FOOBARFOO, BAR), "startsAndEndsWithIgnoreCase(FOOBARFOO, BAR)");
        assertFalse(StringUtils.startsAndEndsWithIgnoreCase(foobarfoo, BAR), "startsAndEndsWithIgnoreCase(foobarfoo, BAR)");
        assertFalse(StringUtils.startsAndEndsWithIgnoreCase(FOOBARFOO, bar), "startsAndEndsWithIgnoreCase(FOOBARFOO, bar)");
    }

    /**
     * Test StringUtils.startsAndEndsWithIgnoreCase(str, prefix, suffix)
     */
    @Test
    public void testStartsAndEndsWithIgnoreCase_PrefixAndSuffix() {
        assertTrue(StringUtils.startsAndEndsWithIgnoreCase(null, null, null), "startsAndEndsWithIgnoreCase(null, null, null)");
        assertFalse(StringUtils.startsAndEndsWithIgnoreCase(FOOBARFOO, null, null), "startsAndEndsWithIgnoreCase(FOOBARFOO, null, null)");
        assertFalse(StringUtils.startsAndEndsWithIgnoreCase(null, FOO, FOO), "startsAndEndsWithIgnoreCase(null, FOO, FOO)");
        assertTrue(StringUtils.startsAndEndsWithIgnoreCase(FOOBARFOO, "", ""), "startsAndEndsWithIgnoreCase(FOOBARFOO, \"\", \"\")");

        assertTrue(StringUtils.startsAndEndsWithIgnoreCase(foobarfoo, foo, foo), "startsAndEndsWithIgnoreCase(foobarfoo, foo, foo)");
        assertTrue(StringUtils.startsAndEndsWithIgnoreCase(FOOBARFOO, FOO, foo), "startsAndEndsWithIgnoreCase(FOOBARFOO, FOO, foo)");
        assertTrue(StringUtils.startsAndEndsWithIgnoreCase(FOOBARFOO, foo, FOO), "startsAndEndsWithIgnoreCase(FOOBARFOO, foo, FOO)");
        assertTrue(StringUtils.startsAndEndsWithIgnoreCase(foobarfoo, FOO, foo), "startsAndEndsWithIgnoreCase(foobarfoo, FOO, foo)");
        assertTrue(StringUtils.startsAndEndsWithIgnoreCase(foobarfoo, foo, FOO), "startsAndEndsWithIgnoreCase(foobarfoo, foo, FOO)");

        assertFalse(StringUtils.startsAndEndsWithIgnoreCase(foo, foobarfoo, foobarfoo), "startsAndEndsWithIgnoreCase(foo, foobarfoo, foobarfoo)");
        assertFalse(StringUtils.startsAndEndsWithIgnoreCase(bar, foobarfoo, foobarfoo), "startsAndEndsWithIgnoreCase(foo, foobarfoo, foobarfoo)");

        assertFalse(StringUtils.startsAndEndsWithIgnoreCase(foobarfoo, bar, bar), "startsAndEndsWithIgnoreCase(foobarfoo, bar, bar)");
        assertFalse(StringUtils.startsAndEndsWithIgnoreCase(FOOBARFOO, BAR, BAR), "startsAndEndsWithIgnoreCase(FOOBARFOO, BAR, BAR)");
        assertFalse(StringUtils.startsAndEndsWithIgnoreCase(foobarfoo, BAR, BAR), "startsAndEndsWithIgnoreCase(foobarfoo, BAR, BAR)");
        assertFalse(StringUtils.startsAndEndsWithIgnoreCase(FOOBARFOO, bar, bar), "startsAndEndsWithIgnoreCase(FOOBARFOO, bar, bar)");
    }

    /**
     * Test StringUtils.startsWith()
     */
    @Test
    public void testStartsWith() {
        assertTrue(StringUtils.startsWith(null, null), "startsWith(null, null)");
        assertFalse(StringUtils.startsWith(FOOBAR, null), "startsWith(FOOBAR, null)");
        assertFalse(StringUtils.startsWith(null, FOO), "startsWith(null, FOO)");
        assertTrue(StringUtils.startsWith(FOOBAR, ""), "startsWith(FOOBAR, \"\")");

        assertTrue(StringUtils.startsWith(foobar, foo), "startsWith(foobar, foo)");
        assertTrue(StringUtils.startsWith(FOOBAR, FOO), "startsWith(FOOBAR, FOO)");
        assertFalse(StringUtils.startsWith(foobar, FOO), "startsWith(foobar, FOO)");
        assertFalse(StringUtils.startsWith(FOOBAR, foo), "startsWith(FOOBAR, foo)");

        assertFalse(StringUtils.startsWith(foo, foobar), "startsWith(foo, foobar)");
        assertFalse(StringUtils.startsWith(bar, foobar), "startsWith(foo, foobar)");

        assertFalse(StringUtils.startsWith(foobar, bar), "startsWith(foobar, bar)");
        assertFalse(StringUtils.startsWith(FOOBAR, BAR), "startsWith(FOOBAR, BAR)");
        assertFalse(StringUtils.startsWith(foobar, BAR), "startsWith(foobar, BAR)");
        assertFalse(StringUtils.startsWith(FOOBAR, bar), "startsWith(FOOBAR, bar)");
    }

    /**
     * Test StringUtils.testStartsWithIgnoreCase()
     */
    @Test
    public void testStartsWithIgnoreCase() {
        assertTrue(StringUtils.startsWithIgnoreCase(null, null), "startsWithIgnoreCase(null, null)");
        assertFalse(StringUtils.startsWithIgnoreCase(FOOBAR, null), "startsWithIgnoreCase(FOOBAR, null)");
        assertFalse(StringUtils.startsWithIgnoreCase(null, FOO), "startsWithIgnoreCase(null, FOO)");
        assertTrue(StringUtils.startsWithIgnoreCase(FOOBAR, ""), "startsWithIgnoreCase(FOOBAR, \"\")");

        assertTrue(StringUtils.startsWithIgnoreCase(foobar, foo), "startsWithIgnoreCase(foobar, foo)");
        assertTrue(StringUtils.startsWithIgnoreCase(FOOBAR, FOO), "startsWithIgnoreCase(FOOBAR, FOO)");
        assertTrue(StringUtils.startsWithIgnoreCase(foobar, FOO), "startsWithIgnoreCase(foobar, FOO)");
        assertTrue(StringUtils.startsWithIgnoreCase(FOOBAR, foo), "startsWithIgnoreCase(FOOBAR, foo)");

        assertFalse(StringUtils.startsWithIgnoreCase(foo, foobar), "startsWithIgnoreCase(foo, foobar)");
        assertFalse(StringUtils.startsWithIgnoreCase(bar, foobar), "startsWithIgnoreCase(foo, foobar)");

        assertFalse(StringUtils.startsWithIgnoreCase(foobar, bar), "startsWithIgnoreCase(foobar, bar)");
        assertFalse(StringUtils.startsWithIgnoreCase(FOOBAR, BAR), "startsWithIgnoreCase(FOOBAR, BAR)");
        assertFalse(StringUtils.startsWithIgnoreCase(foobar, BAR), "startsWithIgnoreCase(foobar, BAR)");
        assertFalse(StringUtils.startsWithIgnoreCase(FOOBAR, bar), "startsWithIgnoreCase(FOOBAR, bar)");
    }

    @Test
    public void testStartsWithAny() {
        assertFalse(StringUtils.startsWithAny(null, (String[]) null));
        assertFalse(StringUtils.startsWithAny(null, "abc"));
        assertFalse(StringUtils.startsWithAny("abcxyz", (String[]) null));
        assertFalse(StringUtils.startsWithAny("abcxyz"));
        assertTrue(StringUtils.startsWithAny("abcxyz", "abc"));
        assertTrue(StringUtils.startsWithAny("abcxyz", null, "xyz", "abc"));
        assertFalse(StringUtils.startsWithAny("abcxyz", null, "xyz", "abcd"));
        assertTrue(StringUtils.startsWithAny("abcxyz", ""));
        assertFalse(StringUtils.startsWithAny("abcxyz", null, "xyz", "ABCX"));
        assertFalse(StringUtils.startsWithAny("ABCXYZ", null, "xyz", "abc"));

        assertTrue(StringUtils.startsWithAny("abcxyz", new StringBuilder("xyz"), new StringBuffer("abc")), "StringUtils.startsWithAny(abcxyz, StringBuilder(xyz), StringBuffer(abc))");
        assertTrue(StringUtils.startsWithAny(new StringBuffer("abcxyz"), new StringBuilder("xyz"), new StringBuffer("abc")), "StringUtils.startsWithAny(StringBuffer(abcxyz), StringBuilder(xyz), StringBuffer(abc))");
    }


    /**
     * Test StringUtils.endsWith()
     */
    @Test
    public void testEndsWith() {
        assertTrue(StringUtils.endsWith(null, null), "endsWith(null, null)");
        assertFalse(StringUtils.endsWith(FOOBAR, null), "endsWith(FOOBAR, null)");
        assertFalse(StringUtils.endsWith(null, FOO), "endsWith(null, FOO)");
        assertTrue(StringUtils.endsWith(FOOBAR, ""), "endsWith(FOOBAR, \"\")");

        assertFalse(StringUtils.endsWith(foobar, foo), "endsWith(foobar, foo)");
        assertFalse(StringUtils.endsWith(FOOBAR, FOO), "endsWith(FOOBAR, FOO)");
        assertFalse(StringUtils.endsWith(foobar, FOO), "endsWith(foobar, FOO)");
        assertFalse(StringUtils.endsWith(FOOBAR, foo), "endsWith(FOOBAR, foo)");

        assertFalse(StringUtils.endsWith(foo, foobar), "endsWith(foo, foobar)");
        assertFalse(StringUtils.endsWith(bar, foobar), "endsWith(foo, foobar)");

        assertTrue(StringUtils.endsWith(foobar, bar), "endsWith(foobar, bar)");
        assertTrue(StringUtils.endsWith(FOOBAR, BAR), "endsWith(FOOBAR, BAR)");
        assertFalse(StringUtils.endsWith(foobar, BAR), "endsWith(foobar, BAR)");
        assertFalse(StringUtils.endsWith(FOOBAR, bar), "endsWith(FOOBAR, bar)");

        // "alpha, beta, gamma, delta".endsWith("delta")
        assertTrue(StringUtils.endsWith("\u03B1\u03B2\u03B3\u03B4", "\u03B4"),
                "endsWith(\u03B1\u03B2\u03B3\u03B4, \u03B4)");
        // "alpha, beta, gamma, delta".endsWith("gamma, DELTA")
        assertFalse(StringUtils.endsWith("\u03B1\u03B2\u03B3\u03B4", "\u03B3\u0394"),
                "endsWith(\u03B1\u03B2\u03B3\u03B4, \u03B3\u0394)");
    }

    /**
     * Test StringUtils.endsWithIgnoreCase()
     */
    @Test
    public void testEndsWithIgnoreCase() {
        assertTrue(StringUtils.endsWithIgnoreCase(null, null), "endsWithIgnoreCase(null, null)");
        assertFalse(StringUtils.endsWithIgnoreCase(FOOBAR, null), "endsWithIgnoreCase(FOOBAR, null)");
        assertFalse(StringUtils.endsWithIgnoreCase(null, FOO), "endsWithIgnoreCase(null, FOO)");
        assertTrue(StringUtils.endsWithIgnoreCase(FOOBAR, ""), "endsWithIgnoreCase(FOOBAR, \"\")");

        assertFalse(StringUtils.endsWithIgnoreCase(foobar, foo), "endsWithIgnoreCase(foobar, foo)");
        assertFalse(StringUtils.endsWithIgnoreCase(FOOBAR, FOO), "endsWithIgnoreCase(FOOBAR, FOO)");
        assertFalse(StringUtils.endsWithIgnoreCase(foobar, FOO), "endsWithIgnoreCase(foobar, FOO)");
        assertFalse(StringUtils.endsWithIgnoreCase(FOOBAR, foo), "endsWithIgnoreCase(FOOBAR, foo)");

        assertFalse(StringUtils.endsWithIgnoreCase(foo, foobar), "endsWithIgnoreCase(foo, foobar)");
        assertFalse(StringUtils.endsWithIgnoreCase(bar, foobar), "endsWithIgnoreCase(foo, foobar)");

        assertTrue(StringUtils.endsWithIgnoreCase(foobar, bar), "endsWithIgnoreCase(foobar, bar)");
        assertTrue(StringUtils.endsWithIgnoreCase(FOOBAR, BAR), "endsWithIgnoreCase(FOOBAR, BAR)");
        assertTrue(StringUtils.endsWithIgnoreCase(foobar, BAR), "endsWithIgnoreCase(foobar, BAR)");
        assertTrue(StringUtils.endsWithIgnoreCase(FOOBAR, bar), "endsWithIgnoreCase(FOOBAR, bar)");

        // javadoc
        assertTrue(StringUtils.endsWithIgnoreCase("abcdef", "def"));
        assertTrue(StringUtils.endsWithIgnoreCase("ABCDEF", "def"));
        assertFalse(StringUtils.endsWithIgnoreCase("ABCDEF", "cde"));

        // "alpha, beta, gamma, delta".endsWith("DELTA")
        assertTrue(StringUtils.endsWithIgnoreCase("\u03B1\u03B2\u03B3\u03B4", "\u0394"),
                "endsWith(\u03B1\u03B2\u03B3\u03B4, \u0394)");
        // "alpha, beta, gamma, delta".endsWith("GAMMA")
        assertFalse(StringUtils.endsWithIgnoreCase("\u03B1\u03B2\u03B3\u03B4", "\u0393"),
                "endsWith(\u03B1\u03B2\u03B3\u03B4, \u0393)");
    }

    @Test
    public void testEndsWithAny() {
        assertFalse(StringUtils.endsWithAny(null, (String) null), "StringUtils.endsWithAny(null, null)");
        assertFalse(StringUtils.endsWithAny(null, "abc"), "StringUtils.endsWithAny(null, new String[] {abc})");
        assertFalse(StringUtils.endsWithAny("abcxyz", (String) null), "StringUtils.endsWithAny(abcxyz, null)");
        assertTrue(StringUtils.endsWithAny("abcxyz", ""), "StringUtils.endsWithAny(abcxyz, new String[] {\"\"})");
        assertTrue(StringUtils.endsWithAny("abcxyz", "xyz"), "StringUtils.endsWithAny(abcxyz, new String[] {xyz})");
        assertTrue(StringUtils.endsWithAny("abcxyz", null, "xyz", "abc"), "StringUtils.endsWithAny(abcxyz, new String[] {null, xyz, abc})");
        assertFalse(StringUtils.endsWithAny("defg", null, "xyz", "abc"), "StringUtils.endsWithAny(defg, new String[] {null, xyz, abc})");
        assertTrue(StringUtils.endsWithAny("abcXYZ", "def", "XYZ"));
        assertFalse(StringUtils.endsWithAny("abcXYZ", "def", "xyz"));
        assertTrue(StringUtils.endsWithAny("abcXYZ", "def", "YZ"));

        /*
         * Type null of the last argument to method endsWithAny(CharSequence, CharSequence...)
         * doesn't exactly match the vararg parameter type.
         * Cast to CharSequence[] to confirm the non-varargs invocation,
         * or pass individual arguments of type CharSequence for a varargs invocation.
         *
         * assertFalse(StringUtils.endsWithAny("abcXYZ", null)); // replace with specific types to avoid warning
         */
        assertFalse(StringUtils.endsWithAny("abcXYZ", (CharSequence) null));
        assertFalse(StringUtils.endsWithAny("abcXYZ", (CharSequence[]) null));
        assertTrue(StringUtils.endsWithAny("abcXYZ", ""));

        assertTrue(StringUtils.endsWithAny("abcxyz", new StringBuilder("abc"), new StringBuffer("xyz")), "StringUtils.endsWithAny(abcxyz, StringBuilder(abc), StringBuffer(xyz))");
        assertTrue(StringUtils.endsWithAny(new StringBuffer("abcxyz"), new StringBuilder("abc"), new StringBuffer("xyz")), "StringUtils.endsWithAny(StringBuffer(abcxyz), StringBuilder(abc), StringBuffer(xyz))");
    }


}
