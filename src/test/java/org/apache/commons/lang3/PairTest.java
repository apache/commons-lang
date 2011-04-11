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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map.Entry;

import org.junit.Test;

/**
 * Test the Pair class.
 * @version $Id$
 */
public class PairTest {

    @Test
    public void testPairOf() throws Exception {
        Pair<Integer, String> pair = Pair.of(0, "foo");
        assertTrue(pair instanceof ImmutablePair<?, ?>);
        assertEquals(0, ((ImmutablePair<Integer, String>) pair).left.intValue());
        assertEquals("foo", ((ImmutablePair<Integer, String>) pair).right);
        Pair<Object, String> pair2 = Pair.of(null, "bar");
        assertTrue(pair2 instanceof ImmutablePair<?, ?>);
        assertNull(((ImmutablePair<Object, String>) pair2).left);
        assertEquals("bar", ((ImmutablePair<Object, String>) pair2).right);
    }

    @Test
    public void testCompatibilityBetweenPairs() throws Exception {
        Pair<Integer, String> pair = ImmutablePair.of(0, "foo");
        Pair<Integer, String> pair2 = MutablePair.of(0, "foo");
        assertEquals(pair, pair2);
        assertEquals(pair.hashCode(), pair2.hashCode());
        HashSet<Pair<Integer, String>> set = new HashSet<Pair<Integer, String>>();
        set.add(pair);
        assertTrue(set.contains(pair2));

        pair2.setValue("bar");
        assertFalse(pair.equals(pair2));
        assertFalse(pair.hashCode() == pair2.hashCode());
    }

    @Test
    public void testMapEntry() throws Exception {
        Pair<Integer, String> pair = ImmutablePair.of(0, "foo");
        HashMap<Integer, String> map = new HashMap<Integer, String>();
        map.put(0, "foo");
        Entry<Integer, String> entry = map.entrySet().iterator().next();
        assertEquals(pair, entry);
        assertEquals(pair.hashCode(), entry.hashCode());
    }

    @Test
    public void testComparable1() throws Exception {
        Pair<String, String> pair1 = Pair.of("A", "D");
        Pair<String, String> pair2 = Pair.of("B", "C");
        assertEquals(true, pair1.compareTo(pair1) == 0);
        assertEquals(true, pair1.compareTo(pair2) < 0);
        assertEquals(true, pair2.compareTo(pair2) == 0);
        assertEquals(true, pair2.compareTo(pair1) > 0);
    }

    @Test
    public void testComparable2() throws Exception {
        Pair<String, String> pair1 = Pair.of("A", "C");
        Pair<String, String> pair2 = Pair.of("A", "D");
        assertEquals(true, pair1.compareTo(pair1) == 0);
        assertEquals(true, pair1.compareTo(pair2) < 0);
        assertEquals(true, pair2.compareTo(pair2) == 0);
        assertEquals(true, pair2.compareTo(pair1) > 0);
    }

    @Test
    public void testToString() throws Exception {
        Pair<String, String> pair = Pair.of("Key", "Value");
        assertEquals("(Key,Value)", pair.toString());
    }

    @Test
    public void testToStringFormat() throws Exception {
        Pair<String, String> pair = Pair.of("Key", "Value");
        assertEquals("ImmutablePair", pair.toString("%1$s"));
        assertEquals("Key", pair.toString("%2$s"));
        assertEquals("Value", pair.toString("%3$s"));
        assertEquals("Key: Value", pair.toString("%2$s: %3$s"));
        pair = Pair.of(null, null);
        assertEquals("null: null", pair.toString("%2$s: %3$s"));
    }

}
