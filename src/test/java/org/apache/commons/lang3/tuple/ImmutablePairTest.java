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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map.Entry;
import java.util.TreeMap;

import org.junit.jupiter.api.Test;

/**
 * Test the Pair class.
 */
public class ImmutablePairTest {

    @Test
    public void testBasic() {
        final ImmutablePair<Integer, String> pair = new ImmutablePair<>(0, "foo");
        assertEquals(0, pair.left.intValue());
        assertEquals(0, pair.getLeft().intValue());
        assertEquals("foo", pair.right);
        assertEquals("foo", pair.getRight());
        final ImmutablePair<Object, String> pair2 = new ImmutablePair<>(null, "bar");
        assertNull(pair2.left);
        assertNull(pair2.getLeft());
        assertEquals("bar", pair2.right);
        assertEquals("bar", pair2.getRight());
    }

    @Test
    public void testPairOf() {
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
    public void testNullPairSame() {
        assertSame(ImmutablePair.nullPair(), ImmutablePair.nullPair());
    }

    @Test
    public void testNullPairLeft() {
        assertNull(ImmutablePair.nullPair().getLeft());
    }

    @Test
    public void testNullPairKey() {
        assertNull(ImmutablePair.nullPair().getKey());
    }

    @Test
    public void testNullPairRight() {
        assertNull(ImmutablePair.nullPair().getRight());
    }

    @Test
    public void testNullPairValue() {
        assertNull(ImmutablePair.nullPair().getValue());
    }

    @Test
    public void testNullPairTyped() {
        // No compiler warnings
        // How do we assert that?
        final ImmutablePair<String, String> pair = ImmutablePair.nullPair();
        assertNotNull(pair);
    }

    @Test
    public void testToString() {
        assertEquals("(null,null)", ImmutablePair.of(null, null).toString());
        assertEquals("(null,two)", ImmutablePair.of(null, "two").toString());
        assertEquals("(one,null)", ImmutablePair.of("one", null).toString());
        assertEquals("(one,two)", ImmutablePair.of("one", "two").toString());
    }

    @Test
    @SuppressWarnings("unchecked")
    public void testSerialization() throws Exception {
        final ImmutablePair<Integer, String> origPair = ImmutablePair.of(0, "foo");
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        final ObjectOutputStream out = new ObjectOutputStream(baos);
        out.writeObject(origPair);
        final ImmutablePair<Integer, String> deserializedPair = (ImmutablePair<Integer, String>) new ObjectInputStream(
                new ByteArrayInputStream(baos.toByteArray())).readObject();
        assertEquals(origPair, deserializedPair);
        assertEquals(origPair.hashCode(), deserializedPair.hashCode());
    }

    @Test
    public void testUseAsKeyOfHashMap() {
        HashMap<ImmutablePair<Object, Object>, String> map = new HashMap<>();
        Object o1 = new Object();
        Object o2 = new Object();
        ImmutablePair<Object, Object> key1 = ImmutablePair.of(o1, o2);
        String value1 = "a1";
        map.put(key1, value1);
        assertEquals(value1, map.get(key1));
        assertEquals(value1, map.get(ImmutablePair.of(o1, o2)));
    }

    @Test
    public void testUseAsKeyOfTreeMap() {
        TreeMap<ImmutablePair<Integer, Integer>, String> map = new TreeMap<>();
        map.put(ImmutablePair.of(1, 2), "12");
        map.put(ImmutablePair.of(1, 1), "11");
        map.put(ImmutablePair.of(0, 1), "01");
        ArrayList<ImmutablePair<Integer, Integer>> expected = new ArrayList<>();
        expected.add(ImmutablePair.of(0, 1));
        expected.add(ImmutablePair.of(1, 1));
        expected.add(ImmutablePair.of(1, 2));
        Iterator<Entry<ImmutablePair<Integer, Integer>, String>> it = map.entrySet().iterator();
        for (ImmutablePair<Integer, Integer> item : expected) {
            Entry<ImmutablePair<Integer, Integer>, String> entry = it.next();
            assertEquals(item, entry.getKey());
            assertEquals(item.getLeft() + "" + item.getRight(), entry.getValue());
        }
    }
}
