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

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Unit tests {@link Strings} - StartsWith/EndsWith methods
 */
public class StringsStartsEndsWithTest extends AbstractLangTest {
    private static final String foo    = "foo";
    private static final String bar    = "bar";
    private static final String foobar = "foobar";
    private static final String FOO    = "FOO";
    private static final String BAR    = "BAR";
    private static final String FOOBAR = "FOOBAR";

    /**
     * Test Strings.CS.endsWith()
     */
    @Test
    public void testEndsWith() {
        assertTrue(Strings.CS.endsWith(null, null), "endsWith(null, null)");
        assertFalse(Strings.CS.endsWith(FOOBAR, null), "endsWith(FOOBAR, null)");
        assertFalse(Strings.CS.endsWith(null, FOO), "endsWith(null, FOO)");
        assertTrue(Strings.CS.endsWith(FOOBAR, ""), "endsWith(FOOBAR, \"\")");

        assertFalse(Strings.CS.endsWith(foobar, foo), "endsWith(foobar, foo)");
        assertFalse(Strings.CS.endsWith(FOOBAR, FOO), "endsWith(FOOBAR, FOO)");
        assertFalse(Strings.CS.endsWith(foobar, FOO), "endsWith(foobar, FOO)");
        assertFalse(Strings.CS.endsWith(FOOBAR, foo), "endsWith(FOOBAR, foo)");

        assertFalse(Strings.CS.endsWith(foo, foobar), "endsWith(foo, foobar)");
        assertFalse(Strings.CS.endsWith(bar, foobar), "endsWith(foo, foobar)");

        assertTrue(Strings.CS.endsWith(foobar, bar), "endsWith(foobar, bar)");
        assertTrue(Strings.CS.endsWith(FOOBAR, BAR), "endsWith(FOOBAR, BAR)");
        assertFalse(Strings.CS.endsWith(foobar, BAR), "endsWith(foobar, BAR)");
        assertFalse(Strings.CS.endsWith(FOOBAR, bar), "endsWith(FOOBAR, bar)");

        // "alpha, beta, gamma, delta".endsWith("delta")
        assertTrue(Strings.CS.endsWith("\u03B1\u03B2\u03B3\u03B4", "\u03B4"),
                "endsWith(\u03B1\u03B2\u03B3\u03B4, \u03B4)");
        // "alpha, beta, gamma, delta".endsWith("gamma, DELTA")
        assertFalse(Strings.CS.endsWith("\u03B1\u03B2\u03B3\u03B4", "\u03B3\u0394"),
                "endsWith(\u03B1\u03B2\u03B3\u03B4, \u03B3\u0394)");
    }

    /**
     * Test Strings.CI.endsWith()
     */
    @Test
    public void testEndsWithIgnoreCase() {
        assertTrue(Strings.CI.endsWith(null, null), "endsWithIgnoreCase(null, null)");
        assertFalse(Strings.CI.endsWith(FOOBAR, null), "endsWithIgnoreCase(FOOBAR, null)");
        assertFalse(Strings.CI.endsWith(null, FOO), "endsWithIgnoreCase(null, FOO)");
        assertTrue(Strings.CI.endsWith(FOOBAR, ""), "endsWithIgnoreCase(FOOBAR, \"\")");

        assertFalse(Strings.CI.endsWith(foobar, foo), "endsWithIgnoreCase(foobar, foo)");
        assertFalse(Strings.CI.endsWith(FOOBAR, FOO), "endsWithIgnoreCase(FOOBAR, FOO)");
        assertFalse(Strings.CI.endsWith(foobar, FOO), "endsWithIgnoreCase(foobar, FOO)");
        assertFalse(Strings.CI.endsWith(FOOBAR, foo), "endsWithIgnoreCase(FOOBAR, foo)");

        assertFalse(Strings.CI.endsWith(foo, foobar), "endsWithIgnoreCase(foo, foobar)");
        assertFalse(Strings.CI.endsWith(bar, foobar), "endsWithIgnoreCase(foo, foobar)");

        assertTrue(Strings.CI.endsWith(foobar, bar), "endsWithIgnoreCase(foobar, bar)");
        assertTrue(Strings.CI.endsWith(FOOBAR, BAR), "endsWithIgnoreCase(FOOBAR, BAR)");
        assertTrue(Strings.CI.endsWith(foobar, BAR), "endsWithIgnoreCase(foobar, BAR)");
        assertTrue(Strings.CI.endsWith(FOOBAR, bar), "endsWithIgnoreCase(FOOBAR, bar)");

        // javadoc
        assertTrue(Strings.CI.endsWith("abcdef", "def"));
        assertTrue(Strings.CI.endsWith("ABCDEF", "def"));
        assertFalse(Strings.CI.endsWith("ABCDEF", "cde"));

        // "alpha, beta, gamma, delta".endsWith("DELTA")
        assertTrue(Strings.CI.endsWith("\u03B1\u03B2\u03B3\u03B4", "\u0394"),
                "endsWith(\u03B1\u03B2\u03B3\u03B4, \u0394)");
        // "alpha, beta, gamma, delta".endsWith("GAMMA")
        assertFalse(Strings.CI.endsWith("\u03B1\u03B2\u03B3\u03B4", "\u0393"),
                "endsWith(\u03B1\u03B2\u03B3\u03B4, \u0393)");
    }

    /**
     * Test Strings.CS.startsWith()
     */
    @Test
    public void testStartsWith() {
        assertTrue(Strings.CS.startsWith(null, null), "startsWith(null, null)");
        assertFalse(Strings.CS.startsWith(FOOBAR, null), "startsWith(FOOBAR, null)");
        assertFalse(Strings.CS.startsWith(null, FOO), "startsWith(null, FOO)");
        assertTrue(Strings.CS.startsWith(FOOBAR, ""), "startsWith(FOOBAR, \"\")");

        assertTrue(Strings.CS.startsWith(foobar, foo), "startsWith(foobar, foo)");
        assertTrue(Strings.CS.startsWith(FOOBAR, FOO), "startsWith(FOOBAR, FOO)");
        assertFalse(Strings.CS.startsWith(foobar, FOO), "startsWith(foobar, FOO)");
        assertFalse(Strings.CS.startsWith(FOOBAR, foo), "startsWith(FOOBAR, foo)");

        assertFalse(Strings.CS.startsWith(foo, foobar), "startsWith(foo, foobar)");
        assertFalse(Strings.CS.startsWith(bar, foobar), "startsWith(foo, foobar)");

        assertFalse(Strings.CS.startsWith(foobar, bar), "startsWith(foobar, bar)");
        assertFalse(Strings.CS.startsWith(FOOBAR, BAR), "startsWith(FOOBAR, BAR)");
        assertFalse(Strings.CS.startsWith(foobar, BAR), "startsWith(foobar, BAR)");
        assertFalse(Strings.CS.startsWith(FOOBAR, bar), "startsWith(FOOBAR, bar)");
    }

    /**
     * Test StringUtils.testStartsWithIgnoreCase()
     */
    @Test
    public void testStartsWithIgnoreCase() {
        assertTrue(Strings.CI.startsWith(null, null), "startsWithIgnoreCase(null, null)");
        assertFalse(Strings.CI.startsWith(FOOBAR, null), "startsWithIgnoreCase(FOOBAR, null)");
        assertFalse(Strings.CI.startsWith(null, FOO), "startsWithIgnoreCase(null, FOO)");
        assertTrue(Strings.CI.startsWith(FOOBAR, ""), "startsWithIgnoreCase(FOOBAR, \"\")");

        assertTrue(Strings.CI.startsWith(foobar, foo), "startsWithIgnoreCase(foobar, foo)");
        assertTrue(Strings.CI.startsWith(FOOBAR, FOO), "startsWithIgnoreCase(FOOBAR, FOO)");
        assertTrue(Strings.CI.startsWith(foobar, FOO), "startsWithIgnoreCase(foobar, FOO)");
        assertTrue(Strings.CI.startsWith(FOOBAR, foo), "startsWithIgnoreCase(FOOBAR, foo)");

        assertFalse(Strings.CI.startsWith(foo, foobar), "startsWithIgnoreCase(foo, foobar)");
        assertFalse(Strings.CI.startsWith(bar, foobar), "startsWithIgnoreCase(foo, foobar)");

        assertFalse(Strings.CI.startsWith(foobar, bar), "startsWithIgnoreCase(foobar, bar)");
        assertFalse(Strings.CI.startsWith(FOOBAR, BAR), "startsWithIgnoreCase(FOOBAR, BAR)");
        assertFalse(Strings.CI.startsWith(foobar, BAR), "startsWithIgnoreCase(foobar, BAR)");
        assertFalse(Strings.CI.startsWith(FOOBAR, bar), "startsWithIgnoreCase(FOOBAR, bar)");
    }

}
