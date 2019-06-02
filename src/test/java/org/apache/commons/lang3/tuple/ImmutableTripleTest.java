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
import java.util.TreeMap;
import java.util.Map.Entry;

import org.junit.jupiter.api.Test;

/**
 * Test the Triple class.
 */
public class ImmutableTripleTest {

    @Test
    public void testBasic() {
        final ImmutableTriple<Integer, String, Boolean> triple = new ImmutableTriple<>(0, "foo", Boolean.TRUE);
        assertEquals(0, triple.left.intValue());
        assertEquals(0, triple.getLeft().intValue());
        assertEquals("foo", triple.middle);
        assertEquals("foo", triple.getMiddle());
        assertEquals(Boolean.TRUE, triple.right);
        assertEquals(Boolean.TRUE, triple.getRight());
        final ImmutableTriple<Object, String, Integer> triple2 = new ImmutableTriple<>(null, "bar", 42);
        assertNull(triple2.left);
        assertNull(triple2.getLeft());
        assertEquals("bar", triple2.middle);
        assertEquals("bar", triple2.getMiddle());
        assertEquals(new Integer(42), triple2.right);
        assertEquals(new Integer(42), triple2.getRight());
    }

    @Test
    public void testTripleOf() {
        final ImmutableTriple<Integer, String, Boolean> triple = ImmutableTriple.of(0, "foo", Boolean.FALSE);
        assertEquals(0, triple.left.intValue());
        assertEquals(0, triple.getLeft().intValue());
        assertEquals("foo", triple.middle);
        assertEquals("foo", triple.getMiddle());
        assertEquals(Boolean.FALSE, triple.right);
        assertEquals(Boolean.FALSE, triple.getRight());
        final ImmutableTriple<Object, String, Boolean> triple2 = ImmutableTriple.of(null, "bar", Boolean.TRUE);
        assertNull(triple2.left);
        assertNull(triple2.getLeft());
        assertEquals("bar", triple2.middle);
        assertEquals("bar", triple2.getMiddle());
        assertEquals(Boolean.TRUE, triple2.right);
        assertEquals(Boolean.TRUE, triple2.getRight());
    }

    @Test
    public void testEquals() {
        assertEquals(ImmutableTriple.of(null, "foo", 42), ImmutableTriple.of(null, "foo", 42));
        assertNotEquals(ImmutableTriple.of("foo", 0, Boolean.TRUE), ImmutableTriple.of("foo", null, null));
        assertNotEquals(ImmutableTriple.of("foo", "bar", "baz"), ImmutableTriple.of("xyz", "bar", "blo"));

        final ImmutableTriple<String, String, String> p = ImmutableTriple.of("foo", "bar", "baz");
        assertEquals(p, p);
        assertNotEquals(p, new Object());
    }

    @Test
    public void testHashCode() {
        assertEquals(ImmutableTriple.of(null, "foo", Boolean.TRUE).hashCode(), ImmutableTriple.of(null, "foo", Boolean.TRUE).hashCode());
    }

    @Test
    public void testNullTripleEquals() {
        assertEquals(ImmutableTriple.nullTriple(), ImmutableTriple.nullTriple());
    }

    @Test
    public void testNullTripleSame() {
        assertSame(ImmutableTriple.nullTriple(), ImmutableTriple.nullTriple());
    }

    @Test
    public void testNullTripleLeft() {
        assertNull(ImmutableTriple.nullTriple().getLeft());
    }

    @Test
    public void testNullTripleMiddle() {
        assertNull(ImmutableTriple.nullTriple().getMiddle());
    }

    @Test
    public void testNullTripleRight() {
        assertNull(ImmutableTriple.nullTriple().getRight());
    }

    @Test
    public void testNullTripleTyped() {
        // No compiler warnings
        // How do we assert that?
        final ImmutableTriple<String, String, String> triple = ImmutableTriple.nullTriple();
        assertNotNull(triple);
    }

    @Test
    public void testToString() {
        assertEquals("(null,null,null)", ImmutableTriple.of(null, null, null).toString());
        assertEquals("(null,two,null)", ImmutableTriple.of(null, "two", null).toString());
        assertEquals("(one,null,null)", ImmutableTriple.of("one", null, null).toString());
        assertEquals("(one,two,null)", ImmutableTriple.of("one", "two", null).toString());
        assertEquals("(null,two,three)", ImmutableTriple.of(null, "two", "three").toString());
        assertEquals("(one,null,three)", ImmutableTriple.of("one", null, "three").toString());
        assertEquals("(one,two,three)", MutableTriple.of("one", "two", "three").toString());
    }

    @Test
    @SuppressWarnings("unchecked")
    public void testSerialization() throws Exception {
        final ImmutableTriple<Integer, String, Boolean> origTriple = ImmutableTriple.of(0, "foo", Boolean.TRUE);
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        final ObjectOutputStream out = new ObjectOutputStream(baos);
        out.writeObject(origTriple);
        final ImmutableTriple<Integer, String, Boolean> deserializedTriple = (ImmutableTriple<Integer, String, Boolean>) new ObjectInputStream(
                new ByteArrayInputStream(baos.toByteArray())).readObject();
        assertEquals(origTriple, deserializedTriple);
        assertEquals(origTriple.hashCode(), deserializedTriple.hashCode());
    }

    @Test
    public void testUseAsKeyOfHashMap() {
        HashMap<ImmutableTriple<Object, Object, Object>, String> map = new HashMap<>();
        Object o1 = new Object();
        Object o2 = new Object();
        Object o3 = new Object();
        ImmutableTriple<Object, Object, Object> key1 = ImmutableTriple.of(o1, o2, o3);
        String value1 = "a1";
        map.put(key1, value1);
        assertEquals(value1, map.get(key1));
        assertEquals(value1, map.get(ImmutableTriple.of(o1, o2, o3)));
    }

    @Test
    public void testUseAsKeyOfTreeMap() {
        TreeMap<ImmutableTriple<Integer, Integer, Integer>, String> map = new TreeMap<>();
        map.put(ImmutableTriple.of(0, 1, 2), "012");
        map.put(ImmutableTriple.of(0, 1, 1), "011");
        map.put(ImmutableTriple.of(0, 0, 1), "001");
        ArrayList<ImmutableTriple<Integer, Integer, Integer>> expected = new ArrayList<>();
        expected.add(ImmutableTriple.of(0, 0, 1));
        expected.add(ImmutableTriple.of(0, 1, 1));
        expected.add(ImmutableTriple.of(0, 1, 2));
        Iterator<Entry<ImmutableTriple<Integer, Integer, Integer>, String>> it = map.entrySet().iterator();
        for (ImmutableTriple<Integer, Integer, Integer> item : expected) {
            Entry<ImmutableTriple<Integer, Integer, Integer>, String> entry = it.next();
            assertEquals(item, entry.getKey());
            assertEquals(item.getLeft() + "" + item.getMiddle() + "" + item.getRight(), entry.getValue());
        }
    }
}

