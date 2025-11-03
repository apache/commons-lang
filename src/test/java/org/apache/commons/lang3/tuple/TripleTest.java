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
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Calendar;
import java.util.HashSet;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Test the Triple class.
 */
class TripleTest extends AbstractLangTest {

    @Test
    void testComparable1() {
        final Triple<String, String, String> triple1 = Triple.of("A", "D", "A");
        final Triple<String, String, String> triple2 = Triple.of("B", "C", "A");
        assertEquals(0, triple1.compareTo(triple1));
        assertTrue(triple1.compareTo(triple2) < 0);
        assertEquals(0, triple2.compareTo(triple2));
        assertTrue(triple2.compareTo(triple1) > 0);
    }

    @Test
    void testComparable2() {
        final Triple<String, String, String> triple1 = Triple.of("A", "C", "B");
        final Triple<String, String, String> triple2 = Triple.of("A", "D", "B");
        assertEquals(0, triple1.compareTo(triple1));
        assertTrue(triple1.compareTo(triple2) < 0);
        assertEquals(0, triple2.compareTo(triple2));
        assertTrue(triple2.compareTo(triple1) > 0);
    }

    @Test
    void testComparable3() {
        final Triple<String, String, String> triple1 = Triple.of("A", "A", "D");
        final Triple<String, String, String> triple2 = Triple.of("A", "B", "C");
        assertEquals(0, triple1.compareTo(triple1));
        assertTrue(triple1.compareTo(triple2) < 0);
        assertEquals(0, triple2.compareTo(triple2));
        assertTrue(triple2.compareTo(triple1) > 0);
    }

    @Test
    void testComparable4() {
        final Triple<String, String, String> triple1 = Triple.of("B", "A", "C");
        final Triple<String, String, String> triple2 = Triple.of("B", "A", "D");
        assertEquals(0, triple1.compareTo(triple1));
        assertTrue(triple1.compareTo(triple2) < 0);
        assertEquals(0, triple2.compareTo(triple2));
        assertTrue(triple2.compareTo(triple1) > 0);
    }

    @Test
    void testCompatibilityBetweenTriples() {
        final Triple<Integer, String, Boolean> triple = ImmutableTriple.of(0, "foo", Boolean.TRUE);
        final Triple<Integer, String, Boolean> triple2 = MutableTriple.of(0, "foo", Boolean.TRUE);
        assertEquals(triple, triple2);
        assertEquals(triple.hashCode(), triple2.hashCode());
        final HashSet<Triple<Integer, String, Boolean>> set = new HashSet<>();
        set.add(triple);
        assertTrue(set.contains(triple2));
    }

    @Test
    void testEmptyArrayGenerics() {
        final Triple<Integer, String, Boolean>[] empty = Triple.emptyArray();
        assertEquals(0, empty.length);
    }

    @Test
    void testEmptyArrayLength() {
        @SuppressWarnings("unchecked")
        final Triple<Integer, String, Boolean>[] empty = (Triple<Integer, String, Boolean>[]) Triple.EMPTY_ARRAY;
        assertEquals(0, empty.length);
    }

    @Test
    void testFormattable_padded() {
        final Triple<String, String, String> triple = Triple.of("Key", "Something", "Value");
        assertEquals("         (Key,Something,Value)", String.format("%1$30s", triple));
    }

    @Test
    void testFormattable_simple() {
        final Triple<String, String, String> triple = Triple.of("Key", "Something", "Value");
        assertEquals("(Key,Something,Value)", String.format("%1$s", triple));
    }

    @Test
    void testOfNonNull() {
        assertNullPointerException(() -> Triple.ofNonNull(null, null, null));
        assertNullPointerException(() -> Triple.ofNonNull(null, null, "z"));
        assertNullPointerException(() -> Triple.ofNonNull(null, "y", "z"));
        assertNullPointerException(() -> Triple.ofNonNull("x", null, null));
        assertNullPointerException(() -> Triple.ofNonNull("x", "y", null));
        final Triple<String, String, String> pair = Triple.ofNonNull("x", "y", "z");
        assertEquals("x", pair.getLeft());
        assertEquals("y", pair.getMiddle());
        assertEquals("z", pair.getRight());
    }

    @Test
    void testToString() {
        final Triple<String, String, String> triple = Triple.of("Key", "Something", "Value");
        assertEquals("(Key,Something,Value)", triple.toString());
    }

    @Test
    void testToStringCustom() {
        final Calendar date = Calendar.getInstance();
        date.set(2011, Calendar.APRIL, 25);
        final Triple<String, String, Calendar> triple = Triple.of("DOB", "string", date);
        assertEquals("Test created on 04-25-2011", triple.toString("Test created on %3$tm-%3$td-%3$tY"));
    }

    @Test
    void testTripleOf() {
        final Triple<Integer, String, Boolean> triple = Triple.of(0, "foo", Boolean.TRUE);
        assertInstanceOf(ImmutableTriple.class, triple);
        assertEquals(0, ((ImmutableTriple<Integer, String, Boolean>) triple).left.intValue());
        assertEquals("foo", ((ImmutableTriple<Integer, String, Boolean>) triple).middle);
        assertEquals(Boolean.TRUE, ((ImmutableTriple<Integer, String, Boolean>) triple).right);
        final Triple<Object, String, Long> triple2 = Triple.of(null, "bar", Long.valueOf(200L));
        assertInstanceOf(ImmutableTriple.class, triple2);
        assertNull(((ImmutableTriple<Object, String, Long>) triple2).left);
        assertEquals("bar", ((ImmutableTriple<Object, String, Long>) triple2).middle);
        assertEquals(Long.valueOf(200L), ((ImmutableTriple<Object, String, Long>) triple2).right);
    }

}

