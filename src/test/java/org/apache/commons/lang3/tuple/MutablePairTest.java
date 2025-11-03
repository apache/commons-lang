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
package org.apache.commons.lang3.tuple;

import static org.apache.commons.lang3.LangAssertions.assertNullPointerException;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.HashMap;
import java.util.Map.Entry;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.SerializationUtils;
import org.junit.jupiter.api.Test;

/**
 * Test the MutablePair class.
 */
class MutablePairTest extends AbstractLangTest {

    @Test
    void testBasic() {
        MutablePair<Integer, String> oldPair = new MutablePair<>(0, "foo");
        MutablePair<Integer, String> nowPair;
        for (int i = 0; i < 4; i++) {
            nowPair = MutablePair.of(oldPair);
            assertEquals(0, nowPair.left.intValue());
            assertEquals(0, nowPair.getLeft().intValue());
            assertEquals("foo", nowPair.right);
            assertEquals("foo", nowPair.getRight());
            assertEquals(oldPair, nowPair);
            oldPair = nowPair;
        }

        MutablePair<Object, String> oldPair2 = new MutablePair<>(null, "bar");
        MutablePair<Object, String> nowPair2;
        for (int i = 0; i < 4; i++) {
            nowPair2 = MutablePair.of(oldPair2);
            assertNull(nowPair2.left);
            assertNull(nowPair2.getLeft());
            assertEquals("bar", nowPair2.right);
            assertEquals("bar", nowPair2.getRight());
            oldPair2 = nowPair2;
        }
    }

    @Test
    void testDefault() {
        final MutablePair<Integer, String> pair = new MutablePair<>();
        assertNull(pair.getLeft());
        assertNull(pair.getRight());
    }

    @Test
    void testEmptyArrayGenerics() {
        final MutablePair<Integer, String>[] empty = MutablePair.emptyArray();
        assertEquals(0, empty.length);
    }

    @Test
    void testEmptyArrayLength() {
        @SuppressWarnings("unchecked")
        final MutablePair<Integer, String>[] empty = (MutablePair<Integer, String>[]) MutablePair.EMPTY_ARRAY;
        assertEquals(0, empty.length);
    }

    @Test
    void testEquals() {
        assertEquals(MutablePair.of(null, "foo"), MutablePair.of(null, "foo"));
        assertNotEquals(MutablePair.of("foo", 0), MutablePair.of("foo", null));
        assertNotEquals(MutablePair.of("foo", "bar"), MutablePair.of("xyz", "bar"));

        final MutablePair<String, String> p = MutablePair.of("foo", "bar");
        assertEquals(p, p);
        assertNotEquals(p, new Object());
    }

    @Test
    void testHashCode() {
        assertEquals(MutablePair.of(null, "foo").hashCode(), MutablePair.of(null, "foo").hashCode());
    }

    @Test
    void testMutate() {
        final MutablePair<Integer, String> pair = new MutablePair<>(0, "foo");
        pair.setLeft(42);
        pair.setRight("bar");
        assertEquals(42, pair.getLeft().intValue());
        assertEquals("bar", pair.getRight());
    }

    @Test
    void testOfNonNull() {
        assertNullPointerException(() -> MutablePair.ofNonNull(null, null));
        assertNullPointerException(() -> MutablePair.ofNonNull(null, "x"));
        assertNullPointerException(() -> MutablePair.ofNonNull("x", null));
        final MutablePair<String, String> pair = MutablePair.ofNonNull("x", "y");
        assertEquals("x", pair.left);
        assertEquals("y", pair.right);
    }

    @Test
    void testOfNonNullMapEntry() {
        assertNullPointerException(() -> MutablePair.ofNonNull(null));
        final Pair<Integer, String> pair = Pair.of(0, "foo");
        final MutablePair<Integer, String> mutablePair = MutablePair.ofNonNull(pair);
        assertEquals(pair.getLeft(), mutablePair.getLeft());
        assertEquals(pair.getRight(), mutablePair.getRight());
    }

    @Test
    void testPairOfMapEntry() {
        assertNull(MutablePair.of(null).getLeft());
        assertNull(MutablePair.of(null).getRight());
        final HashMap<Integer, String> map = new HashMap<>();
        map.put(0, "foo");
        final Entry<Integer, String> entry = map.entrySet().iterator().next();
        final Pair<Integer, String> pair = MutablePair.of(entry);
        assertEquals(entry.getKey(), pair.getLeft());
        assertEquals(entry.getValue(), pair.getRight());
    }

    @Test
    void testPairOfObjects() {
        final MutablePair<Integer, String> pair = MutablePair.of(0, "foo");
        assertEquals(0, pair.getLeft().intValue());
        assertEquals("foo", pair.getRight());
        final MutablePair<Object, String> pair2 = MutablePair.of(null, "bar");
        assertNull(pair2.getLeft());
        assertEquals("bar", pair2.getRight());
        final MutablePair<?, ?> pair3 = MutablePair.of(null, null);
        assertNull(pair3.left);
        assertNull(pair3.right);
    }

    @Test
    void testSerialization() throws Exception {
        final MutablePair<Integer, String> origPair = MutablePair.of(0, "foo");
        final MutablePair<Integer, String> deserializedPair = SerializationUtils.roundtrip(origPair);
        assertEquals(origPair, deserializedPair);
        assertEquals(origPair.hashCode(), deserializedPair.hashCode());
    }

    @Test
    void testToString() {
        assertEquals("(null,null)", MutablePair.of(null, null).toString());
        assertEquals("(null,two)", MutablePair.of(null, "two").toString());
        assertEquals("(one,null)", MutablePair.of("one", null).toString());
        assertEquals("(one,two)", MutablePair.of("one", "two").toString());
    }
}
