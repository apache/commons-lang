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

package org.apache.commons.lang3.stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Objects;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Function;
import java.util.stream.Collector;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;

/**
 * Tests {@link LangCollectors}
 */
public class LangCollectorsTest {

    private static class Fixture {
        int value;

        private Fixture(final int value) {
            this.value = value;
        }

        @Override
        public String toString() {
            return Integer.toString(value);
        }
    }

    private static final Long _1L = Long.valueOf(1);
    private static final Long _2L = Long.valueOf(2);
    private static final Long _3L = Long.valueOf(3);

    private static final Function<Object, String> TO_STRING = Objects::toString;

    private static final Collector<Object, ?, String> JOINING_0 = LangCollectors.joining();
    private static final Collector<Object, ?, String> JOINING_1 = LangCollectors.joining("-");
    private static final Collector<Object, ?, String> JOINING_3 = LangCollectors.joining("-", "<", ">");
    private static final Collector<Object, ?, String> JOINING_4 = LangCollectors.joining("-", "<", ">", TO_STRING);
    private static final Collector<Object, ?, String> JOINING_4_NUL = LangCollectors.joining("-", "<", ">", o -> Objects.toString(o, "NUL"));

    @Test
    public void testJoiningNonStrings0Arg() {
        assertEquals("", Stream.of().collect(JOINING_0));
        assertEquals("1", Stream.of(_1L).collect(JOINING_0));
        assertEquals("12", Stream.of(_1L, _2L).collect(JOINING_0));
        assertEquals("123", Stream.of(_1L, _2L, _3L).collect(JOINING_0));
        assertEquals("1null3", Stream.of(_1L, null, _3L).collect(JOINING_0));
        assertEquals("12", Stream.of(new AtomicLong(1), new AtomicLong(2)).collect(JOINING_0));
        assertEquals("12", Stream.of(new Fixture(1), new Fixture(2)).collect(JOINING_0));
    }

    @Test
    public void testJoiningNonStrings1Arg() {
        assertEquals("", Stream.of().collect(JOINING_1));
        assertEquals("1", Stream.of(_1L).collect(JOINING_1));
        assertEquals("1-2", Stream.of(_1L, _2L).collect(JOINING_1));
        assertEquals("1-2-3", Stream.of(_1L, _2L, _3L).collect(JOINING_1));
        assertEquals("1-null-3", Stream.of(_1L, null, _3L).collect(JOINING_1));
        assertEquals("1-2", Stream.of(new AtomicLong(1), new AtomicLong(2)).collect(JOINING_1));
        assertEquals("1-2", Stream.of(new Fixture(1), new Fixture(2)).collect(JOINING_1));
    }

    @Test
    public void testJoiningNonStrings3Args() {
        assertEquals("<>", Stream.of().collect(JOINING_3));
        assertEquals("<1>", Stream.of(_1L).collect(JOINING_3));
        assertEquals("<1-2>", Stream.of(_1L, _2L).collect(JOINING_3));
        assertEquals("<1-2-3>", Stream.of(_1L, _2L, _3L).collect(JOINING_3));
        assertEquals("<1-null-3>", Stream.of(_1L, null, _3L).collect(JOINING_3));
        assertEquals("<1-2>", Stream.of(new AtomicLong(1), new AtomicLong(2)).collect(JOINING_3));
        assertEquals("<1-2>", Stream.of(new Fixture(1), new Fixture(2)).collect(JOINING_3));
    }

    @Test
    public void testJoiningNonStrings4Args() {
        assertEquals("<>", Stream.of().collect(JOINING_4));
        assertEquals("<1>", Stream.of(_1L).collect(JOINING_4));
        assertEquals("<1-2>", Stream.of(_1L, _2L).collect(JOINING_4));
        assertEquals("<1-2-3>", Stream.of(_1L, _2L, _3L).collect(JOINING_4));
        assertEquals("<1-null-3>", Stream.of(_1L, null, _3L).collect(JOINING_4));
        assertEquals("<1-NUL-3>", Stream.of(_1L, null, _3L).collect(JOINING_4_NUL));
        assertEquals("<1-2>", Stream.of(new AtomicLong(1), new AtomicLong(2)).collect(JOINING_4));
        assertEquals("<1-2>", Stream.of(new Fixture(1), new Fixture(2)).collect(JOINING_4));
    }

    @Test
    public void testJoiningStrings0Arg() {
        assertEquals("", Stream.of().collect(JOINING_0));
        assertEquals("1", Stream.of("1").collect(JOINING_0));
        assertEquals("12", Stream.of("1", "2").collect(JOINING_0));
        assertEquals("123", Stream.of("1", "2", "3").collect(JOINING_0));
        assertEquals("1null3", Stream.of("1", null, "3").collect(JOINING_0));
    }

    @Test
    public void testJoiningStrings1Arg() {
        assertEquals("", Stream.of().collect(JOINING_1));
        assertEquals("1", Stream.of("1").collect(JOINING_1));
        assertEquals("1-2", Stream.of("1", "2").collect(JOINING_1));
        assertEquals("1-2-3", Stream.of("1", "2", "3").collect(JOINING_1));
        assertEquals("1-null-3", Stream.of("1", null, "3").collect(JOINING_1));
    }

    @Test
    public void testJoiningStrings3Args() {
        assertEquals("<>", Stream.of().collect(JOINING_3));
        assertEquals("<1>", Stream.of("1").collect(JOINING_3));
        assertEquals("<1-2>", Stream.of("1", "2").collect(JOINING_3));
        assertEquals("<1-2-3>", Stream.of("1", "2", "3").collect(JOINING_3));
        assertEquals("<1-null-3>", Stream.of("1", null, "3").collect(JOINING_3));
    }

    @Test
    public void testJoiningStrings4Args() {
        assertEquals("<>", Stream.of().collect(JOINING_4));
        assertEquals("<1>", Stream.of("1").collect(JOINING_4));
        assertEquals("<1-2>", Stream.of("1", "2").collect(JOINING_4));
        assertEquals("<1-2-3>", Stream.of("1", "2", "3").collect(JOINING_4));
        assertEquals("<1-null-3>", Stream.of("1", null, "3").collect(JOINING_4));
        assertEquals("<1-NUL-3>", Stream.of("1", null, "3").collect(JOINING_4_NUL));
    }
}
