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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * Unit tests {@link org.apache.commons.lang3.StringUtils} - Empty/Blank methods
 */
public class StringUtilsEmptyBlankTest  {

    @Test
    public void testIsEmpty() {
        assertTrue(StringUtils.isEmpty(null));
        assertTrue(StringUtils.isEmpty(""));
        assertFalse(StringUtils.isEmpty(" "));
        assertFalse(StringUtils.isEmpty("foo"));
        assertFalse(StringUtils.isEmpty("  foo  "));
    }

    @Test
    public void testIsNotEmpty() {
        assertFalse(StringUtils.isNotEmpty(null));
        assertFalse(StringUtils.isNotEmpty(""));
        assertTrue(StringUtils.isNotEmpty(" "));
        assertTrue(StringUtils.isNotEmpty("foo"));
        assertTrue(StringUtils.isNotEmpty("  foo  "));
    }

    @Test
    public void testIsAnyEmpty() {
        assertTrue(StringUtils.isAnyEmpty((String) null));
        assertFalse(StringUtils.isAnyEmpty((String[]) null));
        assertTrue(StringUtils.isAnyEmpty(null, "foo"));
        assertTrue(StringUtils.isAnyEmpty("", "bar"));
        assertTrue(StringUtils.isAnyEmpty("bob", ""));
        assertTrue(StringUtils.isAnyEmpty("  bob  ", null));
        assertFalse(StringUtils.isAnyEmpty(" ", "bar"));
        assertFalse(StringUtils.isAnyEmpty("foo", "bar"));
    }

    @Test
    public void testIsAnyNotEmpty() {
        assertFalse(StringUtils.isAnyNotEmpty((String) null));
        assertFalse(StringUtils.isAnyNotEmpty((String[]) null));
        assertTrue(StringUtils.isAnyNotEmpty(null, "foo"));
        assertTrue(StringUtils.isAnyNotEmpty("", "bar"));
        assertTrue(StringUtils.isAnyNotEmpty("bob", ""));
        assertTrue(StringUtils.isAnyNotEmpty("  bob  ", null));
        assertTrue(StringUtils.isAnyNotEmpty(" ", "bar"));
        assertTrue(StringUtils.isAnyNotEmpty("foo", "bar"));
        assertFalse(StringUtils.isAnyNotEmpty("", null));
    }

    @Test
    public void testIsNoneEmpty() {
        assertFalse(StringUtils.isNoneEmpty((String) null));
        assertTrue(StringUtils.isNoneEmpty((String[]) null));
        assertFalse(StringUtils.isNoneEmpty(null, "foo"));
        assertFalse(StringUtils.isNoneEmpty("", "bar"));
        assertFalse(StringUtils.isNoneEmpty("bob", ""));
        assertFalse(StringUtils.isNoneEmpty("  bob  ", null));
        assertTrue(StringUtils.isNoneEmpty(" ", "bar"));
        assertTrue(StringUtils.isNoneEmpty("foo", "bar"));
    }

    @Test
    public void testIsBlank() {
        assertTrue(StringUtils.isBlank(null));
        assertTrue(StringUtils.isBlank(""));
        assertTrue(StringUtils.isBlank(StringUtilsTest.WHITESPACE));
        assertFalse(StringUtils.isBlank("foo"));
        assertFalse(StringUtils.isBlank("  foo  "));
    }

    @Test
    public void testIsNotBlank() {
        assertFalse(StringUtils.isNotBlank(null));
        assertFalse(StringUtils.isNotBlank(""));
        assertFalse(StringUtils.isNotBlank(StringUtilsTest.WHITESPACE));
        assertTrue(StringUtils.isNotBlank("foo"));
        assertTrue(StringUtils.isNotBlank("  foo  "));
    }

    @Test
    public void testIsAnyBlank() {
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
    public void testIsAnyNotBlank() {
        assertFalse(StringUtils.isAnyNotBlank((String) null));
        assertFalse(StringUtils.isAnyNotBlank((String[]) null));
        assertTrue(StringUtils.isAnyNotBlank(null, "foo"));
        assertFalse(StringUtils.isAnyNotBlank(null, null));
        assertTrue(StringUtils.isAnyNotBlank("", "bar"));
        assertTrue(StringUtils.isAnyNotBlank("bob", ""));
        assertTrue(StringUtils.isAnyNotBlank("  bob  ", null));
        assertTrue(StringUtils.isAnyNotBlank(" ", "bar"));
        assertTrue(StringUtils.isAnyNotBlank("foo", "bar"));
    }

    @Test
    public void testIsNoneBlank() {
        assertFalse(StringUtils.isNoneBlank((String) null));
        assertTrue(StringUtils.isNoneBlank((String[]) null));
        assertFalse(StringUtils.isNoneBlank(null, "foo"));
        assertFalse(StringUtils.isNoneBlank(null, null));
        assertFalse(StringUtils.isNoneBlank("", "bar"));
        assertFalse(StringUtils.isNoneBlank("bob", ""));
        assertFalse(StringUtils.isNoneBlank("  bob  ", null));
        assertFalse(StringUtils.isNoneBlank(" ", "bar"));
        assertTrue(StringUtils.isNoneBlank("foo", "bar"));
    }
}
