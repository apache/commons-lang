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

package org.apache.commons.lang3.stream;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import org.apache.commons.lang3.stream.Streams.FailableStream;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link FailableStream}.
 */
class FailableStreamTest {

    private Integer failable(final Map.Entry<String, AtomicInteger> value) throws IOException {
        if (value == new Object()) {
            throw new IOException();
        }
        return Integer.valueOf(value.getValue().incrementAndGet());
    }

    private String failable(final String value) throws IOException {
        if (value == new Object()) {
            throw new IOException();
        }
        return value.toLowerCase(Locale.ROOT);
    }

    @Test
    void testFailableStreamOfArray() {
        assertArrayEquals(new String[] {}, toArray());
        assertArrayEquals(new String[] { "a" }, toArray("A"));
        assertArrayEquals(new String[] { "a", "b" }, toArray("A", "B"));
        assertArrayEquals(new String[] { "a", "b", "c" }, toArray("A", "B", "C"));
    }

    @Test
    void testFailableStreamOfCollection() {
        assertArrayEquals(new String[] {}, toArray());
        assertArrayEquals(new String[] { "a" }, toArray(Arrays.asList("A")));
        assertArrayEquals(new String[] { "a", "b" }, toArray(Arrays.asList("A", "B")));
        assertArrayEquals(new String[] { "a", "b", "c" }, toArray(Arrays.asList("A", "B", "C")));
    }

    @Test
    void testFailableStreamOfMap() {
        final Map<String, AtomicInteger> map = new LinkedHashMap<>();
        assertArrayEquals(new Integer[] {}, toArrayMap(map));
        map.put("a", new AtomicInteger(1));
        assertArrayEquals(new Integer[] { 2 }, toArrayMap(map));
        map.put("b", new AtomicInteger(2));
        assertArrayEquals(new Integer[] { 3, 3 }, toArrayMap(map));
        map.put("c", new AtomicInteger(3));
        assertArrayEquals(new Integer[] { 4, 4, 4 }, toArrayMap(map));
    }

    private String[] toArray(final Collection<String> strings) {
        return Streams.failableStream(strings).map(this::failable).collect(Collectors.toList()).toArray(new String[0]);
    }

    private String[] toArray(final String string) {
        return Streams.failableStream(string).map(this::failable).collect(Collectors.toList()).toArray(new String[0]);
    }

    private String[] toArray(final String... strings) {
        return Streams.failableStream(strings).map(this::failable).collect(Collectors.toList()).toArray(new String[0]);
    }

    private Integer[] toArrayMap(final Map<String, AtomicInteger> map) {
        return Streams.failableStream(map.entrySet()).map(this::failable).collect(Collectors.toList()).toArray(new Integer[0]);
    }
}
