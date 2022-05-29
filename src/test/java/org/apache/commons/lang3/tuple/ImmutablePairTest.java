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
package org.apache.commons.lang3.tuple;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map.Entry;
import java.util.TreeMap;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.SerializationUtils;
import org.junit.jupiter.api.Test;

/**
 * Test the Pair class.
 */
public class ImmutablePairTest extends AbstractLangTest {

    @Test
    public void testBasic() {
        ImmutablePair<Integer, String> oldPair = new ImmutablePair<>(0, "foo");
        ImmutablePair<Integer, String> nowPair;
        for (int i=0; i<4; i++) {
            nowPair = ImmutablePair.of(oldPair);
            assertEquals(0, nowPair.left.intValue());
            assertEquals(0, nowPair.getLeft().intValue());
            assertEquals("foo", nowPair.right);
            assertEquals("foo", nowPair.getRight());
            assertEquals(oldPair, nowPair);
            oldPair = nowPair;
        }

        ImmutablePair<Object, String> oldPair2 = new ImmutablePair<>(null, "bar");
        ImmutablePair<Object, String> nowPair2;
        for (int i=0; i<4; i++) {
            nowPair2 = ImmutablePair.of(oldPair2);
            assertNull(nowPair2.left);
            assertNull(nowPair2.getLeft());
            assertEquals("bar", nowPair2.right);
            assertEquals("bar", nowPair2.getRight());
            oldPair2 = nowPair2;
        }
    }

    @Test
    public void testComparableLeftOnly() {
        final Pair<String, String> pair1 = ImmutablePair.left("A");
        final Pair<String, String> pair2 = ImmutablePair.left("B");
        assertEquals("A", pair1.getLeft());
        assertEquals("B", pair2.getLeft());
        assertEquals(0, pair1.compareTo(pair1));
        assertTrue(pair1.compareTo(pair2) < 0);
        assertEquals(0, pair2.compareTo(pair2));
        assertTrue(pair2.compareTo(pair1) > 0);
    }

    @Test
    public void testComparableRightOnly() {
        final Pair<String, String> pair1 = ImmutablePair.right("A");
        final Pair<String, String> pair2 = ImmutablePair.right("B");
        assertEquals("A", pair1.getRight());
        assertEquals("B", pair2.getRight());
        assertEquals(0, pair1.compareTo(pair1));
        assertTrue(pair1.compareTo(pair2) < 0);
        assertEquals(0, pair2.compareTo(pair2));
        assertTrue(pair2.compareTo(pair1) > 0);
    }

    @Test
    public void testEmptyArrayGenerics() {
        final ImmutablePair<Integer, String>[] empty = ImmutablePair.emptyArray();
        assertEquals(0, empty.length);
    }

    @Test
    public void testEmptyArrayLength() {
        @SuppressWarnings("unchecked")
        final ImmutablePair<Integer, String>[] empty = (ImmutablePair<Integer, String>[]) ImmutablePair.EMPTY_ARRAY;
        assertEquals(0, empty.length);
    }

    @Test
    public void testEquals() {
        assertEquals(ImmutablePair.of(null, "foo"), ImmutablePair.of(null, "foo"));
        assertNotEquals(ImmutablePair.of("foo", 0), ImmutablePair.of("foo", null));
        assertNotEquals(ImmutablePair.of("foo", "bar"), ImmutablePair.of("xyz", "bar"));

        final ImmutablePair<String, String> p = ImmutablePair.of("foo", "bar");
        assertEquals(p, p);
        assertNotEquals(p, new Object());
    }

    @Test
    public void testHashCode() {
        assertEquals(ImmutablePair.of(null, "foo").hashCode(), ImmutablePair.of(null, "foo").hashCode());
    }

    @Test
    public void testNullPairEquals() {
        assertEquals(ImmutablePair.nullPair(), ImmutablePair.nullPair());
    }

    @Test
    public void testNullPairKey() {
        assertNull(ImmutablePair.nullPair().getKey());
    }

    @Test
    public void testNullPairLeft() {
        assertNull(ImmutablePair.nullPair().getLeft());
    }

    @Test
    public void testNullPairRight() {
        assertNull(ImmutablePair.nullPair().getRight());
    }

    @Test
    public void testNullPairSame() {
        assertSame(ImmutablePair.nullPair(), ImmutablePair.nullPair());
    }

    @Test
    public void testNullPairTyped() {
        // No compiler warnings
        // How do we assert that?
        final ImmutablePair<String, String> pair = ImmutablePair.nullPair();
        assertNotNull(pair);
    }

    @Test
    public void testNullPairValue() {
        assertNull(ImmutablePair.nullPair().getValue());
    }

    @Test
    public void testOfNonNull() {
        assertThrows(NullPointerException.class, () -> ImmutablePair.ofNonNull(null, null));
        assertThrows(NullPointerException.class, () -> ImmutablePair.ofNonNull(null, "x"));
        assertThrows(NullPointerException.class, () -> ImmutablePair.ofNonNull("x", null));
        final ImmutablePair<String, String> pair = ImmutablePair.ofNonNull("x", "y");
        assertEquals("x", pair.left);
        assertEquals("y", pair.right);
    }

    @Test
    public void testPairOfMapEntry() {
        final HashMap<Integer, String> map = new HashMap<>();
        map.put(0, "foo");
        final Entry<Integer, String> entry = map.entrySet().iterator().next();
        final Pair<Integer, String> pair = ImmutablePair.of(entry);
        assertEquals(entry.getKey(), pair.getLeft());
        assertEquals(entry.getValue(), pair.getRight());
    }

    @Test
    public void testPairOfObjects() {
        final ImmutablePair<Integer, String> pair = ImmutablePair.of(0, "foo");
        assertEquals(0, pair.left.intValue());
        assertEquals(0, pair.getLeft().intValue());
        assertEquals("foo", pair.right);
        assertEquals("foo", pair.getRight());
        final ImmutablePair<Object, String> pair2 = ImmutablePair.of(null, "bar");
        assertNull(pair2.left);
        assertNull(pair2.getLeft());
        assertEquals("bar", pair2.right);
        assertEquals("bar", pair2.getRight());
        final ImmutablePair<?, ?> pair3 = ImmutablePair.of(null, null);
        assertNull(pair3.left);
        assertNull(pair3.right);
    }

    @Test
    public void testSerialization() throws Exception {
        final ImmutablePair<Integer, String> origPair = ImmutablePair.of(0, "foo");
        final ImmutablePair<Integer, String> deserializedPair = SerializationUtils.roundtrip(origPair);
        assertEquals(origPair, deserializedPair);
        assertEquals(origPair.hashCode(), deserializedPair.hashCode());
    }

    @Test
    public void testToString() {
        assertEquals("(null,null)", ImmutablePair.of(null, null).toString());
        assertEquals("(null,two)", ImmutablePair.of(null, "two").toString());
        assertEquals("(one,null)", ImmutablePair.of("one", null).toString());
        assertEquals("(one,two)", ImmutablePair.of("one", "two").toString());
    }

    @Test
    public void testToStringLeft() {
        final Pair<String, String> pair = ImmutablePair.left("Key");
        assertEquals("(Key,null)", pair.toString());
    }

    @Test
    public void testToStringRight() {
        final Pair<String, String> pair = ImmutablePair.right("Value");
        assertEquals("(null,Value)", pair.toString());
    }

    @Test
    public void testUseAsKeyOfHashMap() {
        final HashMap<ImmutablePair<Object, Object>, String> map = new HashMap<>();
        final Object o1 = new Object();
        final Object o2 = new Object();
        final ImmutablePair<Object, Object> key1 = ImmutablePair.of(o1, o2);
        final String value1 = "a1";
        map.put(key1, value1);
        assertEquals(value1, map.get(key1));
        assertEquals(value1, map.get(ImmutablePair.of(o1, o2)));
    }

    @Test
    public void testUseAsKeyOfTreeMap() {
        final TreeMap<ImmutablePair<Integer, Integer>, String> map = new TreeMap<>();
        map.put(ImmutablePair.of(1, 2), "12");
        map.put(ImmutablePair.of(1, 1), "11");
        map.put(ImmutablePair.of(0, 1), "01");
        final ArrayList<ImmutablePair<Integer, Integer>> expected = new ArrayList<>();
        expected.add(ImmutablePair.of(0, 1));
        expected.add(ImmutablePair.of(1, 1));
        expected.add(ImmutablePair.of(1, 2));
        final Iterator<Entry<ImmutablePair<Integer, Integer>, String>> it = map.entrySet().iterator();
        for (final ImmutablePair<Integer, Integer> item : expected) {
            final Entry<ImmutablePair<Integer, Integer>, String> entry = it.next();
            assertEquals(item, entry.getKey());
            assertEquals(item.getLeft() + "" + item.getRight(), entry.getValue());
        }
    }

    @Test
    public void testUnsupportedOperation() {
        final ImmutablePair<Integer, String> pair = new ImmutablePair<>(0, "foo");
        assertThrows(UnsupportedOperationException.class, () -> pair.setValue("any"));

    }
}
