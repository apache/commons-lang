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

package org.apache.commons.lang3.util;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Tests {@link IterableStringTokenizer}.
 */
class IterableStringTokenizerTest {

    /**
     * Delimiters from {@link StringTokenizer}.
     */
    private static final String[] DELIMITERS_ARRAY = { " ", "\t", "\n", "\r", "\f" };

    private static final String DELIMITERS_STRING = String.join("", DELIMITERS_ARRAY);

    private static final String[] DATA = { "a", "b", "c" };

    public static String[] delimiters() {
        return DELIMITERS_ARRAY;
    }

    @Test
    void testConstructorArguments1ForEach() {
        final List<String> list = new ArrayList<>();
        new IterableStringTokenizer("a,b,c").forEach(list::add);
        assertEquals(Arrays.asList("a,b,c"), list);
    }

    @Test
    void testConstructorArguments1ToList() {
        assertEquals(Arrays.asList("a,b,c"), new IterableStringTokenizer("a,b,c").toList());
    }

    void testConstructorArguments2AllDelimsToList(final String singleDelim) {
        final String data = String.join(singleDelim, DATA);
        assertEquals(Arrays.asList(DATA), new IterableStringTokenizer(data, DELIMITERS_STRING).toList());
    }

    @ParameterizedTest
    @MethodSource("delimiters")
    void testConstructorArguments2ForEach(final String singleDelim) {
        final List<String> list = new ArrayList<>();
        new IterableStringTokenizer(String.join(singleDelim, DATA), singleDelim).forEach(list::add);
        assertEquals(Arrays.asList(DATA), list);
    }

    @ParameterizedTest
    @MethodSource("delimiters")
    void testConstructorArguments2ToList(final String singleDelim) {
        assertEquals(Arrays.asList(DATA), new IterableStringTokenizer(String.join(singleDelim, DATA), singleDelim).toList());
    }

    @ParameterizedTest
    @MethodSource("delimiters")
    void testConstructorArguments3AllDelimsToList(final String singleDelim) {
        final String data = String.join(singleDelim, DATA);
        assertEquals(Arrays.asList("a", singleDelim, "b", singleDelim, "c"), new IterableStringTokenizer(data, DELIMITERS_STRING, true).toList());
        assertEquals(Arrays.asList(DATA), new IterableStringTokenizer(data, DELIMITERS_STRING, false).toList());
    }

    @ParameterizedTest
    @MethodSource("delimiters")
    void testConstructorArguments3ToList(final String singleDelim) {
        final String data = String.join(singleDelim, DATA);
        assertEquals(Arrays.asList("a", singleDelim, "b", singleDelim, "c"), new IterableStringTokenizer(data, singleDelim, true).toList());
        assertEquals(Arrays.asList(DATA), new IterableStringTokenizer(data, singleDelim, false).toList());
    }

    @Test
    void testEmptyString() {
        assertTrue(new IterableStringTokenizer(StringUtils.EMPTY).toList().isEmpty());
    }

    @Test
    void testIterator() {
        final IterableStringTokenizer tokenizer = new IterableStringTokenizer("a,b,c", ",");
        final Iterator<String> iterator = tokenizer.iterator();
        assertTrue(iterator.hasNext());
        assertEquals("a", iterator.next());
        assertTrue(iterator.hasNext());
        assertEquals("b", iterator.next());
        assertTrue(iterator.hasNext());
        assertEquals("c", iterator.next());
        assertFalse(iterator.hasNext());
    }

    @Test
    void testNonDefaultDelimiterToArray() {
        assertArrayEquals(new String[] {}, new IterableStringTokenizer("", "|").toArray());
        assertArrayEquals(new String[] { "a" }, new IterableStringTokenizer("a", "|").toArray());
        assertArrayEquals(new String[] { "a", "b" }, new IterableStringTokenizer("a|b", "|").toArray());
        assertArrayEquals(new String[] { "a", "b", "c" }, new IterableStringTokenizer("a|b|c", "|").toArray());
    }

    @Test
    void testNonDefaultDelimiterToList() {
        assertEquals(Arrays.asList(DATA), new IterableStringTokenizer("a|b|c", "|").toList());
        assertEquals(Arrays.asList(DATA), new IterableStringTokenizer("a!b!c", "!").toList());
        assertEquals(Arrays.asList(DATA), new IterableStringTokenizer("a^!b^!c", "^!").toList());
    }

    @Test
    void testToArray() {
        // 0 tokens
        assertArrayEquals(new String[] {}, new IterableStringTokenizer("").toArray());
        // 1 token
        assertArrayEquals(new String[] { "a" }, new IterableStringTokenizer("a").toArray());
        assertArrayEquals(new String[] { "a,b" }, new IterableStringTokenizer("a,b").toArray());
        assertArrayEquals(new String[] { "a,b,c" }, new IterableStringTokenizer("a,b,c").toArray());
        // > 1 token
        assertArrayEquals(new String[] { "a", "b" }, new IterableStringTokenizer("a b").toArray());
    }

    @Test
    void testToList() {
        // 0 tokens
        assertEquals(Arrays.asList(), new IterableStringTokenizer("").toList());
        // 1 token
        assertEquals(Arrays.asList("a"), new IterableStringTokenizer("a").toList());
        assertEquals(Arrays.asList("a,b"), new IterableStringTokenizer("a,b").toList());
        assertEquals(Arrays.asList("a,b,c"), new IterableStringTokenizer("a,b,c").toList());
        // > 1 token
        assertEquals(Arrays.asList("a", "b"), new IterableStringTokenizer("a b").toList());
    }

    @Test
    void testToStream() {
        // 0 tokens
        assertEquals(Arrays.asList(), new IterableStringTokenizer("").toList());
        // 1 token
        assertEquals(Arrays.asList("a"), new IterableStringTokenizer("a").toList());
        assertEquals(Arrays.asList("a,b"), new IterableStringTokenizer("a,b").toStream().collect(Collectors.toList()));
        assertEquals(Arrays.asList("a,b,c"), new IterableStringTokenizer("a,b,c").toStream().collect(Collectors.toList()));
        // > 1 token
        assertEquals(Arrays.asList("a", "b"), new IterableStringTokenizer("a b").toStream().collect(Collectors.toList()));
    }
}
