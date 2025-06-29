/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang3;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

/**
 * Tests {@link StringUtils} - Empty/Blank methods
 */
class StringUtilsEmptyBlankTest extends AbstractLangTest {

    @Test
    void testFirstNonBlank() {
        assertNull(StringUtils.firstNonBlank());
        assertNull(StringUtils.firstNonBlank((String[]) null));
        assertNull(StringUtils.firstNonBlank(null, null, null));
        assertNull(StringUtils.firstNonBlank(null, "", " "));
        assertNull(StringUtils.firstNonBlank(null, null, " "));
        assertEquals("a", StringUtils.firstNonBlank(null, "a"));
        assertEquals("zz", StringUtils.firstNonBlank(null, "zz"));
        assertEquals("abc", StringUtils.firstNonBlank("abc"));
        assertEquals("xyz", StringUtils.firstNonBlank(null, "xyz"));
        assertEquals("xyz", StringUtils.firstNonBlank(null, "xyz", "abc"));
    }

    @Test
    void testFirstNonEmpty() {
        assertNull(StringUtils.firstNonEmpty());
        assertNull(StringUtils.firstNonEmpty((String[]) null));
        assertNull(StringUtils.firstNonEmpty(null, null, null));
        assertEquals(" ", StringUtils.firstNonEmpty(null, "", " "));
        assertNull(StringUtils.firstNonEmpty(null, null, ""));
        assertEquals("a", StringUtils.firstNonEmpty(null, "a"));
        assertEquals("zz", StringUtils.firstNonEmpty(null, "zz"));
        assertEquals("abc", StringUtils.firstNonEmpty("abc"));
        assertEquals("xyz", StringUtils.firstNonEmpty(null, "xyz"));
        assertEquals("xyz", StringUtils.firstNonEmpty(null, "xyz", "abc"));
    }

    @Test
    void testIsAllBlank() {
        assertTrue(StringUtils.isAllBlank((String) null));
        assertTrue(StringUtils.isAllBlank((String[]) null));
        assertTrue(StringUtils.isAllBlank(null, null));
        assertTrue(StringUtils.isAllBlank(null, " "));
        assertFalse(StringUtils.isAllBlank(null, "foo"));
        assertFalse(StringUtils.isAllBlank(null, "a"));
        assertFalse(StringUtils.isAllBlank("", "bar"));
        assertFalse(StringUtils.isAllBlank("bob", ""));
        assertFalse(StringUtils.isAllBlank("  bob  ", null));
        assertFalse(StringUtils.isAllBlank(" ", "bar"));
        assertFalse(StringUtils.isAllBlank("foo", "bar"));
    }

    @Test
    void testIsAllEmpty() {
        assertTrue(StringUtils.isAllEmpty());
        assertTrue(StringUtils.isAllEmpty());
        assertTrue(StringUtils.isAllEmpty((String) null));
        assertTrue(StringUtils.isAllEmpty((String[]) null));
        assertFalse(StringUtils.isAllEmpty(null, "foo"));
        assertFalse(StringUtils.isAllEmpty(null, "a"));
        assertFalse(StringUtils.isAllEmpty("", "bar"));
        assertFalse(StringUtils.isAllEmpty("bob", ""));
        assertFalse(StringUtils.isAllEmpty("  bob  ", null));
        assertFalse(StringUtils.isAllEmpty(" ", "bar"));
        assertFalse(StringUtils.isAllEmpty("foo", "bar"));
        assertTrue(StringUtils.isAllEmpty("", null));
    }

    @Test
    void testIsAnyBlank() {
        assertTrue(StringUtils.isAnyBlank((String) null));
        assertFalse(StringUtils.isAnyBlank((String[]) null));
        assertTrue(StringUtils.isAnyBlank(null, "foo"));
        assertTrue(StringUtils.isAnyBlank(null, null));
        assertTrue(StringUtils.isAnyBlank("", "bar"));
        assertTrue(StringUtils.isAnyBlank("bob", ""));
        assertTrue(StringUtils.isAnyBlank("  bob  ", null));
        assertTrue(StringUtils.isAnyBlank(" ", "bar"));
        assertFalse(StringUtils.isAnyBlank("foo", "bar"));
    }

    @Test
    void testIsAnyEmpty() {
        assertTrue(StringUtils.isAnyEmpty((String) null));
        assertFalse(StringUtils.isAnyEmpty((String[]) null));
        assertTrue(StringUtils.isAnyEmpty(null, "foo"));
        assertTrue(StringUtils.isAnyEmpty(null, "a"));
        assertTrue(StringUtils.isAnyEmpty("", "bar"));
        assertTrue(StringUtils.isAnyEmpty("bob", ""));
        assertTrue(StringUtils.isAnyEmpty("  bob  ", null));
        assertFalse(StringUtils.isAnyEmpty(" ", "bar"));
        assertFalse(StringUtils.isAnyEmpty("foo", "bar"));
    }

    @Test
    void testIsBlank() {
        assertTrue(StringUtils.isBlank(null));
        assertTrue(StringUtils.isBlank(""));
        assertTrue(StringUtils.isBlank(StringUtilsTest.WHITESPACE));
        assertFalse(StringUtils.isBlank("a"));
        assertFalse(StringUtils.isBlank("foo"));
        assertFalse(StringUtils.isBlank("  foo  "));
    }

    @Test
    void testIsEmpty() {
        assertTrue(StringUtils.isEmpty(null));
        assertTrue(StringUtils.isEmpty(""));
        assertFalse(StringUtils.isEmpty(" "));
        assertFalse(StringUtils.isEmpty("a"));
        assertFalse(StringUtils.isEmpty("foo"));
        assertFalse(StringUtils.isEmpty("  foo  "));
    }

    @Test
    void testIsNoneBlank() {
        assertFalse(StringUtils.isNoneBlank((String) null));
        assertTrue(StringUtils.isNoneBlank((String[]) null));
        assertFalse(StringUtils.isNoneBlank(null, "a"));
        assertFalse(StringUtils.isNoneBlank(null, "foo"));
        assertFalse(StringUtils.isNoneBlank(null, null));
        assertFalse(StringUtils.isNoneBlank("", "bar"));
        assertFalse(StringUtils.isNoneBlank("a", ""));
        assertFalse(StringUtils.isNoneBlank("bob", ""));
        assertFalse(StringUtils.isNoneBlank("  bob  ", null));
        assertFalse(StringUtils.isNoneBlank(" ", "bar"));
        assertTrue(StringUtils.isNoneBlank("foo", "bar"));
    }

    @Test
    void testIsNoneEmpty() {
        assertFalse(StringUtils.isNoneEmpty((String) null));
        assertTrue(StringUtils.isNoneEmpty((String[]) null));
        assertFalse(StringUtils.isNoneEmpty(null, "a"));
        assertFalse(StringUtils.isNoneEmpty(null, "foo"));
        assertFalse(StringUtils.isNoneEmpty("", "bar"));
        assertFalse(StringUtils.isNoneEmpty("bob", ""));
        assertFalse(StringUtils.isNoneEmpty("a", ""));
        assertFalse(StringUtils.isNoneEmpty("  bob  ", null));
        assertTrue(StringUtils.isNoneEmpty(" ", "bar"));
        assertTrue(StringUtils.isNoneEmpty("foo", "bar"));
    }

    @Test
    void testIsNotBlank() {
        assertFalse(StringUtils.isNotBlank(null));
        assertFalse(StringUtils.isNotBlank(""));
        assertFalse(StringUtils.isNotBlank(StringUtilsTest.WHITESPACE));
        assertTrue(StringUtils.isNotBlank("a"));
        assertTrue(StringUtils.isNotBlank("foo"));
        assertTrue(StringUtils.isNotBlank("  foo  "));
    }

    @Test
    void testIsNotEmpty() {
        assertFalse(StringUtils.isNotEmpty(null));
        assertFalse(StringUtils.isNotEmpty(""));
        assertTrue(StringUtils.isNotEmpty(" "));
        assertTrue(StringUtils.isNotEmpty("a"));
        assertTrue(StringUtils.isNotEmpty("foo"));
        assertTrue(StringUtils.isNotEmpty("  foo  "));
    }
}
