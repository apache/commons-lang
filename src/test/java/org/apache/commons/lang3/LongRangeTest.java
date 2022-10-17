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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Comparator;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link LongRange}.
 */
@SuppressWarnings("boxing")
public class LongRangeTest extends AbstractLangTest {

    private static LongRange of(final int min, final int max) {
        return LongRange.of(min, max);
    }

    private static LongRange of(final Long min, final Long max) {
        return LongRange.of(min, max);
    }

    private LongRange range1;

    private LongRange range2;

    private LongRange range3;

    private LongRange rangeFull;

    @BeforeEach
    public void setUp() {
        range1 = of(10, 20);
        range2 = of(10, 20);
        range3 = of(-2, -1);
        rangeFull = of(Long.MIN_VALUE, Long.MAX_VALUE);
    }

    @Test
    public void testContainsInt() {
        assertFalse(range1.contains(null));

        assertFalse(range1.contains(5L));
        assertTrue(range1.contains(10L));
        assertTrue(range1.contains(15L));
        assertTrue(range1.contains(20L));
        assertFalse(range1.contains(25L));
    }

    @Test
    public void testContainsLong() {
        assertFalse(range1.contains(null));

        assertTrue(rangeFull.contains(Long.MAX_VALUE));
        assertTrue(rangeFull.contains(Long.MIN_VALUE));
        assertTrue(rangeFull.contains((long) Integer.MAX_VALUE + 1));
        assertTrue(rangeFull.contains((long) Integer.MIN_VALUE - 1));
        assertTrue(rangeFull.contains((long) Integer.MAX_VALUE));
        assertTrue(rangeFull.contains((long) Integer.MIN_VALUE));

        assertFalse(range1.contains(5L));
        assertTrue(range1.contains(10L));
        assertTrue(range1.contains(15L));
        assertTrue(range1.contains(20L));
        assertFalse(range1.contains(25L));
    }

    @Test
    public void testContainsRange() {

        // null handling
        assertFalse(range1.containsRange(null));

        // easy inside range
        assertTrue(range1.containsRange(Range.of(12L, 18L)));
        assertTrue(range1.containsRange(of(12, 18)));

        // outside range on each side
        assertFalse(range1.containsRange(Range.of(32L, 45L)));
        assertFalse(range1.containsRange(of(32, 45)));
        assertFalse(range1.containsRange(Range.of(2L, 8L)));
        assertFalse(range1.containsRange(of(2, 8)));

        // equals range
        assertTrue(range1.containsRange(Range.of(10L, 20L)));
        assertTrue(range1.containsRange(of(10, 20)));

        // overlaps
        assertFalse(range1.containsRange(Range.of(9L, 14L)));
        assertFalse(range1.containsRange(of(9, 14)));
        assertFalse(range1.containsRange(Range.of(16L, 21L)));
        assertFalse(range1.containsRange(of(16, 21)));

        // touches lower boundary
        assertTrue(range1.containsRange(Range.of(10L, 19L)));
        assertTrue(range1.containsRange(of(10, 19)));
        assertFalse(range1.containsRange(Range.of(10L, 21L)));
        assertFalse(range1.containsRange(of(10, 21)));

        // touches upper boundary
        assertTrue(range1.containsRange(Range.of(11L, 20L)));
        assertTrue(range1.containsRange(of(11, 20)));
        assertFalse(range1.containsRange(Range.of(9L, 20L)));
        assertFalse(range1.containsRange(of(9, 20)));

        // negative
        assertFalse(range1.containsRange(Range.of(-11L, -18L)));
        assertFalse(range1.containsRange(of(-11, -18)));
    }

    @Test
    public void testElementCompareTo() {
        assertThrows(NullPointerException.class, () -> range1.elementCompareTo(null));

        assertEquals(-1, range1.elementCompareTo(5L));
        assertEquals(0, range1.elementCompareTo(10L));
        assertEquals(0, range1.elementCompareTo(15L));
        assertEquals(0, range1.elementCompareTo(20L));
        assertEquals(1, range1.elementCompareTo(25L));
    }

    @Test
    public void testEqualsObject() {
        assertEquals(range1, range1);
        assertEquals(range1, range2);
        assertEquals(range2, range2);
        assertEquals(range1, range1);
        assertEquals(range2, range2);
        assertEquals(range3, range3);
        assertNotEquals(range2, range3);
        assertNotEquals(null, range2);
        assertNotEquals("Ni!", range2);
    }

    @Test
    public void testFit() {
        assertEquals(range1.getMinimum(), range1.fit((long) Integer.MIN_VALUE));
        assertEquals(range1.getMinimum(), range1.fit(range1.getMinimum()));
        assertEquals(range1.getMaximum(), range1.fit((long) Integer.MAX_VALUE));
        assertEquals(range1.getMaximum(), range1.fit(range1.getMaximum()));
        assertEquals(15, range1.fit(15L));
    }

    @Test
    public void testFitNull() {
        assertThrows(NullPointerException.class, () -> {
            range1.fit(null);
        });
    }

    @Test
    public void testGetMaximum() {
        assertEquals(20, range1.getMaximum());
    }

    @Test
    public void testGetMinimum() {
        assertEquals(10, range1.getMinimum());
    }

    @Test
    public void testHashCode() {
        assertEquals(range1.hashCode(), range2.hashCode());
        assertNotEquals(range1.hashCode(), range3.hashCode());

        assertEquals(range1.hashCode(), range1.hashCode());
        assertTrue(range1.hashCode() != 0);
    }

    @Test
    public void testIntersectionWith() {
        assertSame(range1, range1.intersectionWith(range1));

        assertEquals(Range.of(10L, 15L), range1.intersectionWith(Range.of(5L, 15L)));
    }

    @Test
    public void testIntersectionWithNonOverlapping() {
        assertThrows(IllegalArgumentException.class, () -> range1.intersectionWith(Range.of(0L, 9L)));
    }

    @Test
    public void testIntersectionWithNull() {
        assertThrows(IllegalArgumentException.class, () -> range1.intersectionWith(null));
    }

    @Test
    public void testIsAfter() {
        assertFalse(range1.isAfter(null));

        assertTrue(range1.isAfter(5L));
        assertFalse(range1.isAfter(10L));
        assertFalse(range1.isAfter(15L));
        assertFalse(range1.isAfter(20L));
        assertFalse(range1.isAfter(25L));
    }

    @Test
    public void testIsAfterRange() {
        assertFalse(range1.isAfterRange(null));

        assertTrue(range1.isAfterRange(Range.of(5L, 9L)));

        assertFalse(range1.isAfterRange(Range.of(5L, 10L)));
        assertFalse(range1.isAfterRange(Range.of(5L, 20L)));
        assertFalse(range1.isAfterRange(Range.of(5L, 25L)));
        assertFalse(range1.isAfterRange(Range.of(15L, 25L)));

        assertFalse(range1.isAfterRange(Range.of(21L, 25L)));

        assertFalse(range1.isAfterRange(Range.of(10L, 20L)));
    }

    @Test
    public void testIsBefore() {
        assertFalse(range1.isBefore(null));

        assertFalse(range1.isBefore(5L));
        assertFalse(range1.isBefore(10L));
        assertFalse(range1.isBefore(15L));
        assertFalse(range1.isBefore(20L));
        assertTrue(range1.isBefore(25L));
    }

    @Test
    public void testIsBeforeIntegerRange() {
        assertFalse(range1.isBeforeRange(null));

        assertFalse(range1.isBeforeRange(of(5, 9)));

        assertFalse(range1.isBeforeRange(of(5, 10)));
        assertFalse(range1.isBeforeRange(of(5, 20)));
        assertFalse(range1.isBeforeRange(of(5, 25)));
        assertFalse(range1.isBeforeRange(of(15, 25)));

        assertTrue(range1.isBeforeRange(of(21, 25)));

        assertFalse(range1.isBeforeRange(of(10, 20)));
    }

    @Test
    public void testIsBeforeRange() {
        assertFalse(range1.isBeforeRange(null));

        assertFalse(range1.isBeforeRange(Range.of(5L, 9L)));

        assertFalse(range1.isBeforeRange(Range.of(5L, 10L)));
        assertFalse(range1.isBeforeRange(Range.of(5L, 20L)));
        assertFalse(range1.isBeforeRange(Range.of(5L, 25L)));
        assertFalse(range1.isBeforeRange(Range.of(15L, 25L)));

        assertTrue(range1.isBeforeRange(Range.of(21L, 25L)));

        assertFalse(range1.isBeforeRange(Range.of(10L, 20L)));
    }

    @Test
    public void testIsEndedBy() {
        assertFalse(range1.isEndedBy(null));

        assertFalse(range1.isEndedBy(5L));
        assertFalse(range1.isEndedBy(10L));
        assertFalse(range1.isEndedBy(15L));
        assertTrue(range1.isEndedBy(20L));
        assertFalse(range1.isEndedBy(25L));
    }

    @Test
    public void testIsOverlappedByIntegerRange() {

        // null handling
        assertFalse(range1.isOverlappedBy(null));

        // easy inside range
        assertTrue(range1.isOverlappedBy(of(12, 18)));

        // outside range on each side
        assertFalse(range1.isOverlappedBy(of(32, 45)));
        assertFalse(range1.isOverlappedBy(of(2, 8)));

        // equals range
        assertTrue(range1.isOverlappedBy(of(10, 20)));

        // overlaps
        assertTrue(range1.isOverlappedBy(of(9, 14)));
        assertTrue(range1.isOverlappedBy(of(16, 21)));

        // touches lower boundary
        assertTrue(range1.isOverlappedBy(of(10, 19)));
        assertTrue(range1.isOverlappedBy(of(10, 21)));

        // touches upper boundary
        assertTrue(range1.isOverlappedBy(of(11, 20)));
        assertTrue(range1.isOverlappedBy(of(9, 20)));

        // negative
        assertFalse(range1.isOverlappedBy(of(-11, -18)));

        // outside range whole range
        assertTrue(range1.isOverlappedBy(of(9, 21)));
    }

    @Test
    public void testIsOverlappedByRange() {

        // null handling
        assertFalse(range1.isOverlappedBy(null));

        // easy inside range
        assertTrue(range1.isOverlappedBy(Range.of(12L, 18L)));

        // outside range on each side
        assertFalse(range1.isOverlappedBy(Range.of(32L, 45L)));
        assertFalse(range1.isOverlappedBy(Range.of(2L, 8L)));

        // equals range
        assertTrue(range1.isOverlappedBy(Range.of(10L, 20L)));

        // overlaps
        assertTrue(range1.isOverlappedBy(Range.of(9L, 14L)));
        assertTrue(range1.isOverlappedBy(Range.of(16L, 21L)));

        // touches lower boundary
        assertTrue(range1.isOverlappedBy(Range.of(10L, 19L)));
        assertTrue(range1.isOverlappedBy(Range.of(10L, 21L)));

        // touches upper boundary
        assertTrue(range1.isOverlappedBy(Range.of(11L, 20L)));
        assertTrue(range1.isOverlappedBy(Range.of(9L, 20L)));

        // negative
        assertFalse(range1.isOverlappedBy(Range.of(-11L, -18L)));

        // outside range whole range
        assertTrue(range1.isOverlappedBy(Range.of(9L, 21L)));
    }

    @Test
    public void testIsStartedBy() {
        assertFalse(range1.isStartedBy(null));

        assertFalse(range1.isStartedBy(5L));
        assertTrue(range1.isStartedBy(10L));
        assertFalse(range1.isStartedBy(15L));
        assertFalse(range1.isStartedBy(20L));
        assertFalse(range1.isStartedBy(25L));
    }

    @Test
    public void testIsWithCompareRange() {
        // all integers are equal
        final Comparator<Integer> c = (o1, o2) -> 0;
        Range<Integer> ri = Range.is(10);
        assertFalse(ri.contains(null), "should not contain null");
        assertTrue(ri.contains(10), "should contain 10");
        assertFalse(ri.contains(11), "should not contain 11");
        ri = Range.is(10, c);
        assertFalse(ri.contains(null), "should not contain null");
        assertTrue(ri.contains(10), "should contain 10");
        assertTrue(ri.contains(11), "should contain 11");
    }

    @Test
    public void testOfWithContains() {
        // all integers are equal
        final LongRange rb = of(-10, 20);
        assertFalse(rb.contains(null), "should not contain null");
        assertTrue(rb.contains(10L), "should contain 10");
        assertTrue(rb.contains(-10L), "should contain -10");
        assertFalse(rb.contains(21L), "should not contain 21");
        assertFalse(rb.contains(-11L), "should not contain -11");

        assertThrows(NullPointerException.class, () -> of(null, null));
    }

    @Test
    public void testRangeOfChars() {
        final LongRange chars = of((long) 'a', (long) 'z');
        assertTrue(chars.contains((long) 'b'));
        assertFalse(chars.contains((long) 'B'));
    }

    @Test
    public void testSerializing() {
        SerializationUtils.clone(range1);
    }

    @Test
    public void testToString() {
        assertNotNull(range1.toString());

        final String str = range1.toString();
        assertEquals("[10..20]", str);
        assertEquals("[-20..-10]", Range.of(-20, -10).toString());
    }

    @Test
    public void testToStringFormat() {
        final String str = range1.toString("From %1$s to %2$s");
        assertEquals("From 10 to 20", str);
    }
}
