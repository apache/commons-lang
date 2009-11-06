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

package org.apache.commons.lang;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * <p>
 * Tests the methods in the {@link org.apache.commons.lang.Range} class.
 * </p>
 * 
 * @version $Id: RangeTest.java 754804 2009-03-16 02:06:18Z sebb $
 */
public class RangeTest extends TestCase {

    private Range byteRange;
    private Range byteRange2;
    private Range byteRange3;

    private Range<Integer> intRange;
    private Range<Long> longRange;
    private Range<Float> floatRange;
    private Range<Double> doubleRange;

    @Override
    public void setUp() {
        byteRange   = new Range((byte) 0, (byte) 5);
        byteRange2  = new Range((byte) 0, (byte) 5);
        byteRange3  = new Range((byte) 0, (byte) 10);

        intRange    = new Range<Integer>((int) 10, (int) 20);
        longRange   = new Range<Long>((long) 10, (long) 20);
        floatRange  = new Range<Float>((float) 10, (float) 20);
        doubleRange = new Range<Double>((double) 10, (double) 20);
    }

    /**
     * Test method for 'org.apache.commons.lang.Range.equals(Object)'
     */
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

    /**
     * Test method for 'org.apache.commons.lang.Range.hashCode()'
     */
    public void testHashCode() {
        assertEquals(byteRange.hashCode(), byteRange2.hashCode());
        assertFalse(byteRange.hashCode() == byteRange3.hashCode());
    }

    /**
     * Test method for 'org.apache.commons.lang.Range.toString()'
     */
    public void testToString() {
        assertNotNull(byteRange.toString());
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

    public void testContainsRange() {

        // null handling
        assertFalse(intRange.containsRange(null));

        // easy inside range
        assertTrue(intRange.containsRange(new Range(12, 18)));

        // outside range on each side
        assertFalse(intRange.containsRange(new Range(32, 45)));
        assertFalse(intRange.containsRange(new Range(2, 8)));

        // equals range
        assertTrue(intRange.containsRange(new Range(10, 20)));

        // overlaps
        assertFalse(intRange.containsRange(new Range(9, 14)));
        assertFalse(intRange.containsRange(new Range(16, 21)));

        // touches lower boundary
        assertTrue(intRange.containsRange(new Range(10, 19)));
        assertFalse(intRange.containsRange(new Range(10, 21)));

        // touches upper boundary
        assertTrue(intRange.containsRange(new Range(11, 20)));
        assertFalse(intRange.containsRange(new Range(9, 20)));
        
        // negative
        assertFalse(intRange.containsRange(new Range(-11, -18)));

    }

}
