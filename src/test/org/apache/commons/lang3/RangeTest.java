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

import java.util.Comparator;

import junit.framework.TestCase;

/**
 * <p>
 * Tests the methods in the {@link org.apache.commons.lang3.Range} class.
 * </p>
 * 
 * @version $Id: RangeTest.java 754804 2009-03-16 02:06:18Z sebb $
 */
@SuppressWarnings("boxing")
public class RangeTest extends TestCase {

    private Range<Byte> byteRange;
    private Range<Byte> byteRange2;
    private Range<Byte> byteRange3;

    private Range<Integer> intRange;
    private Range<Long> longRange;
    private Range<Float> floatRange;
    private Range<Double> doubleRange;

    @SuppressWarnings("cast") // intRange
    @Override
    public void setUp() {
        byteRange   = Range.between((byte) 0, (byte) 5);
        byteRange2  = Range.between((byte) 0, (byte) 5);
        byteRange3  = Range.between((byte) 0, (byte) 10);

        intRange    = Range.between((int) 10, (int) 20);
        longRange   = Range.between((long) 10, (long) 20);
        floatRange  = Range.between((float) 10, (float) 20);
        doubleRange = Range.between((double) 10, (double) 20);
    }

    // --------------------------------------------------------------------------

    public void testComparableConstructors() {
        Comparable c = 
            new Comparable() { 
                public int compareTo(Object other) {
                    return 1;
                }
            };
        Range.is(c);
        Range.between(c, c);
    }

    public void testIsWithCompare(){
        Comparator<Integer> c = new Comparator<Integer>(){
            public int compare(Integer o1, Integer o2) {
                return 0; // all integers are equal
            }
        };
        Range<Integer> ri = Range.is(10);
        assertFalse("should not contain null",ri.contains(null));
        assertTrue("should contain 10",ri.contains(10));
        assertFalse("should not contain 11",ri.contains(11));
        ri = Range.is(10,c);
        assertFalse("should not contain null",ri.contains(null));
        assertTrue("should contain 10",ri.contains(10));
        assertTrue("should contain 11",ri.contains(11));
    }

    public void testBetweenWithCompare(){
        // TODO add tests with a better comparator
        Comparator<Integer> c = new Comparator<Integer>(){
            public int compare(Integer o1, Integer o2) {
                return 0; // all integers are equal
            }
        };
        Range<Integer> rb = Range.between(-10,20);
        assertFalse("should not contain null",rb.contains(null));
        assertTrue("should contain 10",rb.contains(10));
        assertTrue("should contain -10",rb.contains(-10));
        assertFalse("should not contain 21",rb.contains(21));
        assertFalse("should not contain -11",rb.contains(-11));
        rb = Range.between(-10,20,c);
        assertFalse("should not contain null",rb.contains(null));
        assertTrue("should contain 10",rb.contains(10));
        assertTrue("should contain -10",rb.contains(-10));
        assertTrue("should contain 21",rb.contains(21));
        assertTrue("should contain -11",rb.contains(-11));
    }

    // --------------------------------------------------------------------------

    public void testRangeOfChars() {
        Range<Character> chars = Range.between('a', 'z');
        assertTrue(chars.contains('b'));
        assertFalse(chars.contains('B'));
    }

    // --------------------------------------------------------------------------

    public void testEqualsObject() {
        assertEquals(byteRange, byteRange);
        assertEquals(byteRange, byteRange2);
        assertEquals(byteRange2, byteRange2);
        assertTrue(byteRange.equals(byteRange));
        assertTrue(byteRange2.equals(byteRange2));
        assertTrue(byteRange3.equals(byteRange3));
        assertFalse(byteRange2.equals(byteRange3));
        assertFalse(byteRange2.equals(null));
        assertFalse(byteRange2.equals("Ni!"));
    }

    public void testHashCode() {
        assertEquals(byteRange.hashCode(), byteRange2.hashCode());
        assertFalse(byteRange.hashCode() == byteRange3.hashCode());

        assertEquals(intRange.hashCode(), intRange.hashCode());
        assertTrue(intRange.hashCode() != 0);
    }

    public void testToString() {
        assertNotNull(byteRange.toString());

        String str = intRange.toString();
        assertEquals("Range[10,20]", str);
//        assertSame(str, intRange.toString());  // no longer passes - does it matter?
        assertEquals("Range[-20,-10]", Range.between(-20, -10).toString());
    }

    // --------------------------------------------------------------------------

    public void testGetMinimum() {
        assertEquals(10, (int) intRange.getMinimum());
        assertEquals(10L, (long) longRange.getMinimum());
        assertEquals(10f, floatRange.getMinimum(), 0.00001f);
        assertEquals(10d, doubleRange.getMinimum(), 0.00001d);
    }
    
    public void testGetMaximum() {
        assertEquals(20, (int) intRange.getMaximum());
        assertEquals(20L, (long) longRange.getMaximum());
        assertEquals(20f, floatRange.getMaximum(), 0.00001f);
        assertEquals(20d, doubleRange.getMaximum(), 0.00001d);
    }

    public void testContains() {
        assertFalse(intRange.contains(null));
        
        assertFalse(intRange.contains(5));
        assertTrue(intRange.contains(10));
        assertTrue(intRange.contains(15));
        assertTrue(intRange.contains(20));
        assertFalse(intRange.contains(25));
    }

    public void testElementBefore() {
        assertFalse(intRange.elementBefore(null));
        
        assertTrue(intRange.elementBefore(5));
        assertFalse(intRange.elementBefore(10));
        assertFalse(intRange.elementBefore(15));
        assertFalse(intRange.elementBefore(20));
        assertFalse(intRange.elementBefore(25));
    }

    public void testElementAfter() {
        assertFalse(intRange.elementAfter(null));
        
        assertFalse(intRange.elementAfter(5));
        assertFalse(intRange.elementAfter(10));
        assertFalse(intRange.elementAfter(15));
        assertFalse(intRange.elementAfter(20));
        assertTrue(intRange.elementAfter(25));
    }

    public void testElementCompareTo() {
        try {
            intRange.elementCompareTo(null);
            fail("NullPointerException should have been thrown");
        } catch(NullPointerException npe) {
            // expected
        }
        
        assertEquals(-1, intRange.elementCompareTo(5));
        assertEquals(0, intRange.elementCompareTo(10));
        assertEquals(0, intRange.elementCompareTo(15));
        assertEquals(0, intRange.elementCompareTo(20));
        assertEquals(1, intRange.elementCompareTo(25));
    }

    // --------------------------------------------------------------------------

    public void testContainsRange() {

        // null handling
        assertFalse(intRange.containsRange(null));

        // easy inside range
        assertTrue(intRange.containsRange(Range.between(12, 18)));

        // outside range on each side
        assertFalse(intRange.containsRange(Range.between(32, 45)));
        assertFalse(intRange.containsRange(Range.between(2, 8)));

        // equals range
        assertTrue(intRange.containsRange(Range.between(10, 20)));

        // overlaps
        assertFalse(intRange.containsRange(Range.between(9, 14)));
        assertFalse(intRange.containsRange(Range.between(16, 21)));

        // touches lower boundary
        assertTrue(intRange.containsRange(Range.between(10, 19)));
        assertFalse(intRange.containsRange(Range.between(10, 21)));

        // touches upper boundary
        assertTrue(intRange.containsRange(Range.between(11, 20)));
        assertFalse(intRange.containsRange(Range.between(9, 20)));
        
        // negative
        assertFalse(intRange.containsRange(Range.between(-11, -18)));

    }

    public void testOverlapsRange() {

        // null handling
        assertFalse(intRange.overlapsRange(null));

        // easy inside range
        assertTrue(intRange.overlapsRange(Range.between(12, 18)));

        // outside range on each side
        assertFalse(intRange.overlapsRange(Range.between(32, 45)));
        assertFalse(intRange.overlapsRange(Range.between(2, 8)));

        // equals range
        assertTrue(intRange.overlapsRange(Range.between(10, 20)));

        // overlaps
        assertTrue(intRange.overlapsRange(Range.between(9, 14)));
        assertTrue(intRange.overlapsRange(Range.between(16, 21)));

        // touches lower boundary
        assertTrue(intRange.overlapsRange(Range.between(10, 19)));
        assertTrue(intRange.overlapsRange(Range.between(10, 21)));

        // touches upper boundary
        assertTrue(intRange.overlapsRange(Range.between(11, 20)));
        assertTrue(intRange.overlapsRange(Range.between(9, 20)));
        
        // negative
        assertFalse(intRange.overlapsRange(Range.between(-11, -18)));

    }

    public void testSerializing() {
        SerializationUtils.clone(intRange);
    }

}
