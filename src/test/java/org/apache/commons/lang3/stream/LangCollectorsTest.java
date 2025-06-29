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

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Arrays;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Function;
import java.util.stream.Collector;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;

/**
 * Tests {@link LangCollectors}
 */
class LangCollectorsTest {

    private static final class Fixture {
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

    private String join0(final Object... objects) {
        return LangCollectors.collect(JOINING_0, objects);
    }

    private String join1(final Object... objects) {
        return LangCollectors.collect(JOINING_1, objects);
    }

    private String join3(final Object... objects) {
        return LangCollectors.collect(JOINING_3, objects);
    }

    private String join4(final Object... objects) {
        return LangCollectors.collect(JOINING_4, objects);
    }

    private String join4NullToString(final Object... objects) {
        return LangCollectors.collect(JOINING_4_NUL, objects);
    }

    @Test
    void testCollectStrings1Arg() {
        assertEquals("", join1());
        assertEquals("1", join1("1"));
        assertEquals("1-2", join1("1", "2"));
        assertEquals("1-2-3", join1("1", "2", "3"));
        assertEquals("1-null-3", join1("1", null, "3"));
    }

    @Test
    void testJoinCollectNonStrings0Arg() {
        assertEquals("", join0());
        assertEquals("1", join0(_1L));
        assertEquals("12", join0(_1L, _2L));
        assertEquals("123", join0(_1L, _2L, _3L));
        assertEquals("1null3", join0(_1L, null, _3L));
        assertEquals("12", join0(new AtomicLong(1), new AtomicLong(2)));
        assertEquals("12", join0(new Fixture(1), new Fixture(2)));
    }

    @Test
    void testJoinCollectNonStrings1Arg() {
        assertEquals("", join1());
        assertEquals("1", join1(_1L));
        assertEquals("1-2", join1(_1L, _2L));
        assertEquals("1-2-3", join1(_1L, _2L, _3L));
        assertEquals("1-null-3", join1(_1L, null, _3L));
        assertEquals("1-2", join1(new AtomicLong(1), new AtomicLong(2)));
        assertEquals("1-2", join1(new Fixture(1), new Fixture(2)));
    }

    @Test
    void testJoinCollectNonStrings3Args() {
        assertEquals("<>", join3());
        assertEquals("<1>", join3(_1L));
        assertEquals("<1-2>", join3(_1L, _2L));
        assertEquals("<1-2-3>", join3(_1L, _2L, _3L));
        assertEquals("<1-null-3>", join3(_1L, null, _3L));
        assertEquals("<1-2>", join3(new AtomicLong(1), new AtomicLong(2)));
        assertEquals("<1-2>", join3(new Fixture(1), new Fixture(2)));
    }

    @Test
    void testJoinCollectNonStrings4Args() {
        assertEquals("<>", join4());
        assertEquals("<1>", join4(_1L));
        assertEquals("<1-2>", join4(_1L, _2L));
        assertEquals("<1-2-3>", join4(_1L, _2L, _3L));
        assertEquals("<1-null-3>", join4(_1L, null, _3L));
        assertEquals("<1-NUL-3>", join4NullToString(_1L, null, _3L));
        assertEquals("<1-2>", join4(new AtomicLong(1), new AtomicLong(2)));
        assertEquals("<1-2>", join4(new Fixture(1), new Fixture(2)));
    }

    @Test
    void testJoinCollectNullArgs() {
        assertEquals("", join0((Object[]) null));
        assertEquals("", join1((Object[]) null));
        assertEquals("<>", join3((Object[]) null));
        assertEquals("<>", join4NullToString((Object[]) null));
    }

    @Test
    void testJoinCollectStrings0Arg() {
        assertEquals("", join0());
        assertEquals("1", join0("1"));
        assertEquals("12", join0("1", "2"));
        assertEquals("123", join0("1", "2", "3"));
        assertEquals("1null3", join0("1", null, "3"));
    }

    @Test
    void testJoinCollectStrings3Args() {
        assertEquals("<>", join3());
        assertEquals("<1>", join3("1"));
        assertEquals("<1-2>", join3("1", "2"));
        assertEquals("<1-2-3>", join3("1", "2", "3"));
        assertEquals("<1-null-3>", join3("1", null, "3"));
    }

    @Test
    void testJoinCollectStrings4Args() {
        assertEquals("<>", join4());
        assertEquals("<1>", join4("1"));
        assertEquals("<1-2>", join4("1", "2"));
        assertEquals("<1-2-3>", join4("1", "2", "3"));
        assertEquals("<1-null-3>", join4("1", null, "3"));
        assertEquals("<1-NUL-3>", join4NullToString("1", null, "3"));
    }

    @Test
    void testJoiningNonStrings0Arg() {
        // Stream.of()
        assertEquals("", Stream.of().collect(JOINING_0));
        assertEquals("1", Stream.of(_1L).collect(JOINING_0));
        assertEquals("12", Stream.of(_1L, _2L).collect(JOINING_0));
        assertEquals("123", Stream.of(_1L, _2L, _3L).collect(JOINING_0));
        assertEquals("1null3", Stream.of(_1L, null, _3L).collect(JOINING_0));
        assertEquals("12", Stream.of(new AtomicLong(1), new AtomicLong(2)).collect(JOINING_0));
        assertEquals("12", Stream.of(new Fixture(1), new Fixture(2)).collect(JOINING_0));
        // Arrays.stream()
        assertEquals("", Arrays.stream(new Object[] {}).collect(JOINING_0));
        assertEquals("1", Arrays.stream(new Long[] { _1L }).collect(JOINING_0));
        assertEquals("12", Arrays.stream(new Long[] { _1L, _2L }).collect(JOINING_0));
        assertEquals("123", Arrays.stream(new Long[] { _1L, _2L, _3L }).collect(JOINING_0));
        assertEquals("1null3", Arrays.stream(new Long[] { _1L, null, _3L }).collect(JOINING_0));
        assertEquals("12", Arrays.stream(new AtomicLong[] { new AtomicLong(1), new AtomicLong(2) }).collect(JOINING_0));
        assertEquals("12", Arrays.stream(new Fixture[] { new Fixture(1), new Fixture(2) }).collect(JOINING_0));
    }

    @Test
    void testJoiningNonStrings1Arg() {
        // Stream.of()
        assertEquals("", Stream.of().collect(JOINING_1));
        assertEquals("1", Stream.of(_1L).collect(JOINING_1));
        assertEquals("1-2", Stream.of(_1L, _2L).collect(JOINING_1));
        assertEquals("1-2-3", Stream.of(_1L, _2L, _3L).collect(JOINING_1));
        assertEquals("1-null-3", Stream.of(_1L, null, _3L).collect(JOINING_1));
        assertEquals("1-2", Stream.of(new AtomicLong(1), new AtomicLong(2)).collect(JOINING_1));
        assertEquals("1-2", Stream.of(new Fixture(1), new Fixture(2)).collect(JOINING_1));
        // Arrays.stream()
        assertEquals("", Arrays.stream(new Object[] {}).collect(JOINING_1));
        assertEquals("1", Arrays.stream(new Long[] { _1L }).collect(JOINING_1));
        assertEquals("1-2", Arrays.stream(new Long[] { _1L, _2L }).collect(JOINING_1));
        assertEquals("1-2-3", Arrays.stream(new Long[] { _1L, _2L, _3L }).collect(JOINING_1));
        assertEquals("1-null-3", Arrays.stream(new Long[] { _1L, null, _3L }).collect(JOINING_1));
        assertEquals("1-2", Arrays.stream(new AtomicLong[] { new AtomicLong(1), new AtomicLong(2) }).collect(JOINING_1));
        assertEquals("1-2", Arrays.stream(new Fixture[] { new Fixture(1), new Fixture(2) }).collect(JOINING_1));
    }

    @Test
    void testJoiningNonStrings3Args() {
        assertEquals("<>", Stream.of().collect(JOINING_3));
        assertEquals("<1>", Stream.of(_1L).collect(JOINING_3));
        assertEquals("<1-2>", Stream.of(_1L, _2L).collect(JOINING_3));
        assertEquals("<1-2-3>", Stream.of(_1L, _2L, _3L).collect(JOINING_3));
        assertEquals("<1-null-3>", Stream.of(_1L, null, _3L).collect(JOINING_3));
        assertEquals("<1-2>", Stream.of(new AtomicLong(1), new AtomicLong(2)).collect(JOINING_3));
        assertEquals("<1-2>", Stream.of(new Fixture(1), new Fixture(2)).collect(JOINING_3));
    }

    @Test
    void testJoiningNonStrings4Args() {
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
    void testJoiningStrings0Arg() {
        assertEquals("", Stream.of().collect(JOINING_0));
        assertEquals("1", Stream.of("1").collect(JOINING_0));
        assertEquals("12", Stream.of("1", "2").collect(JOINING_0));
        assertEquals("123", Stream.of("1", "2", "3").collect(JOINING_0));
        assertEquals("1null3", Stream.of("1", null, "3").collect(JOINING_0));
    }

    @Test
    void testJoiningStrings1Arg() {
        assertEquals("", Stream.of().collect(JOINING_1));
        assertEquals("1", Stream.of("1").collect(JOINING_1));
        assertEquals("1-2", Stream.of("1", "2").collect(JOINING_1));
        assertEquals("1-2-3", Stream.of("1", "2", "3").collect(JOINING_1));
        assertEquals("1-null-3", Stream.of("1", null, "3").collect(JOINING_1));
    }

    @Test
    void testJoiningStrings3Args() {
        assertEquals("<>", Stream.of().collect(JOINING_3));
        assertEquals("<1>", Stream.of("1").collect(JOINING_3));
        assertEquals("<1-2>", Stream.of("1", "2").collect(JOINING_3));
        assertEquals("<1-2-3>", Stream.of("1", "2", "3").collect(JOINING_3));
        assertEquals("<1-null-3>", Stream.of("1", null, "3").collect(JOINING_3));
    }

    @Test
    void testJoiningStrings4Args() {
        assertEquals("<>", Stream.of().collect(JOINING_4));
        assertEquals("<1>", Stream.of("1").collect(JOINING_4));
        assertEquals("<1-2>", Stream.of("1", "2").collect(JOINING_4));
        assertEquals("<1-2-3>", Stream.of("1", "2", "3").collect(JOINING_4));
        assertEquals("<1-null-3>", Stream.of("1", null, "3").collect(JOINING_4));
        assertEquals("<1-NUL-3>", Stream.of("1", null, "3").collect(JOINING_4_NUL));
    }
}
