/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.commons.lang.math;

import junit.framework.TestCase;

/**
 * Test cases for the {@link Range} classes.
 *
 * @author Stephen Colebourne
 * @version $Id$
 */
public abstract class AbstractRangeTest extends TestCase {

    protected Range tenToTwenty;
    protected Range otherRange;
    
    protected Integer five;
    protected Integer ten;
    protected Integer twelve;
    protected Integer fifteen;
    protected Integer twenty;
    protected Integer twentyFive;
    protected Long long8;
    protected Long long10;
    protected Long long12;
    protected Long long20;
    protected Long long21;
    protected Double double8;
    protected Double double10;
    protected Double double12;
    protected Double double20;
    protected Double double21;
    protected Float float8;
    protected Float float10;
    protected Float float12;
    protected Float float20;
    protected Float float21;
    
    private static class InnerNumber extends Number {
        public double doubleValue() {
            return 12d;
        }
        public float floatValue() {
            return 12f;
        }
        public int intValue() {
            return 12;
        }
        public long longValue() {
            return 12L;
        }
    }
    
    protected InnerNumber nonComparableNumber = new InnerNumber();


    public AbstractRangeTest(String name) {
        super(name);
    }

    public void setUp() {
        five = new Integer(5);
        ten = new Integer(10);
        twelve = new Integer(12);
        fifteen = new Integer(15);
        twenty = new Integer(20);
        twentyFive = new Integer(25);
        long8 = new Long(8);
        long10 = new Long(10);
        long12 = new Long(12);
        long20 = new Long(20);
        long21 = new Long(21);
        double8 = new Double(8);
        double10 = new Double(10);
        double12 = new Double(12);
        double20 = new Double(20);
        double21 = new Double(21);
        float8 = new Float(8);
        float10 = new Float(10);
        float12 = new Float(12);
        float20 = new Float(20);
        float21 = new Float(21);
    }

    // --------------------------------------------------------------------------

    public void testGetMinimum() {
        assertEquals(10L, tenToTwenty.getMinimumLong());
        assertEquals(10, tenToTwenty.getMinimumInteger());
        assertEquals(10d, tenToTwenty.getMinimumDouble(), 0.00001d);
        assertEquals(10f, tenToTwenty.getMinimumFloat(), 0.00001f);
    }
    
    public void testGetMaximum() {
        assertEquals(20L, tenToTwenty.getMaximumLong());
        assertEquals(20, tenToTwenty.getMaximumInteger());
        assertEquals(20d, tenToTwenty.getMaximumDouble(), 0.00001d);
        assertEquals(20f, tenToTwenty.getMaximumFloat(), 0.00001f);
    }

    //--------------------------------------------------------------------------

    public void testContainsLong() {
        assertEquals(false, tenToTwenty.containsLong(null));
        assertEquals(true, tenToTwenty.containsLong(nonComparableNumber));
        
        assertEquals(false, tenToTwenty.containsLong(five));
        assertEquals(true, tenToTwenty.containsLong(ten));
        assertEquals(true, tenToTwenty.containsLong(fifteen));
        assertEquals(true, tenToTwenty.containsLong(twenty));
        assertEquals(false, tenToTwenty.containsLong(twentyFive));
        
        assertEquals(false, tenToTwenty.containsLong(long8));
        assertEquals(true, tenToTwenty.containsLong(long10));
        assertEquals(true, tenToTwenty.containsLong(long12));
        assertEquals(true, tenToTwenty.containsLong(long20));
        assertEquals(false, tenToTwenty.containsLong(long21));
        
        assertEquals(false, tenToTwenty.containsLong(double8));
        assertEquals(true, tenToTwenty.containsLong(double10));
        assertEquals(true, tenToTwenty.containsLong(double12));
        assertEquals(true, tenToTwenty.containsLong(double20));
        assertEquals(false, tenToTwenty.containsLong(double21));
        
        assertEquals(false, tenToTwenty.containsLong(float8));
        assertEquals(true, tenToTwenty.containsLong(float10));
        assertEquals(true, tenToTwenty.containsLong(float12));
        assertEquals(true, tenToTwenty.containsLong(float20));
        assertEquals(false, tenToTwenty.containsLong(float21));
        
        assertEquals(false, tenToTwenty.containsLong(9L));
        assertEquals(true, tenToTwenty.containsLong(10L));
        assertEquals(true, tenToTwenty.containsLong(15L));
        assertEquals(true, tenToTwenty.containsLong(20L));
        assertEquals(false, tenToTwenty.containsLong(21L));
    }
        
    public void testContainsInteger() {
        assertEquals(false, tenToTwenty.containsInteger(null));
        assertEquals(true, tenToTwenty.containsInteger(nonComparableNumber));
        
        assertEquals(false, tenToTwenty.containsInteger(five));
        assertEquals(true, tenToTwenty.containsInteger(ten));
        assertEquals(true, tenToTwenty.containsInteger(fifteen));
        assertEquals(true, tenToTwenty.containsInteger(twenty));
        assertEquals(false, tenToTwenty.containsInteger(twentyFive));
        
        assertEquals(false, tenToTwenty.containsInteger(long8));
        assertEquals(true, tenToTwenty.containsInteger(long10));
        assertEquals(true, tenToTwenty.containsInteger(long12));
        assertEquals(true, tenToTwenty.containsInteger(long20));
        assertEquals(false, tenToTwenty.containsInteger(long21));
        
        assertEquals(false, tenToTwenty.containsInteger(double8));
        assertEquals(true, tenToTwenty.containsInteger(double10));
        assertEquals(true, tenToTwenty.containsInteger(double12));
        assertEquals(true, tenToTwenty.containsInteger(double20));
        assertEquals(false, tenToTwenty.containsInteger(double21));
        
        assertEquals(false, tenToTwenty.containsInteger(float8));
        assertEquals(true, tenToTwenty.containsInteger(float10));
        assertEquals(true, tenToTwenty.containsInteger(float12));
        assertEquals(true, tenToTwenty.containsInteger(float20));
        assertEquals(false, tenToTwenty.containsInteger(float21));
        
        assertEquals(false, tenToTwenty.containsInteger(9));
        assertEquals(true, tenToTwenty.containsInteger(10));
        assertEquals(true, tenToTwenty.containsInteger(15));
        assertEquals(true, tenToTwenty.containsInteger(20));
        assertEquals(false, tenToTwenty.containsInteger(21));
    }

    public void testContainsDouble() {
        assertEquals(false, tenToTwenty.containsDouble(null));
        assertEquals(true, tenToTwenty.containsDouble(nonComparableNumber));
        
        assertEquals(false, tenToTwenty.containsDouble(five));
        assertEquals(true, tenToTwenty.containsDouble(ten));
        assertEquals(true, tenToTwenty.containsDouble(fifteen));
        assertEquals(true, tenToTwenty.containsDouble(twenty));
        assertEquals(false, tenToTwenty.containsDouble(twentyFive));
        
        assertEquals(false, tenToTwenty.containsDouble(long8));
        assertEquals(true, tenToTwenty.containsDouble(long10));
        assertEquals(true, tenToTwenty.containsDouble(long12));
        assertEquals(true, tenToTwenty.containsDouble(long20));
        assertEquals(false, tenToTwenty.containsDouble(long21));
        
        assertEquals(false, tenToTwenty.containsDouble(double8));
        assertEquals(true, tenToTwenty.containsDouble(double10));
        assertEquals(true, tenToTwenty.containsDouble(double12));
        assertEquals(true, tenToTwenty.containsDouble(double20));
        assertEquals(false, tenToTwenty.containsDouble(double21));
        
        assertEquals(false, tenToTwenty.containsDouble(float8));
        assertEquals(true, tenToTwenty.containsDouble(float10));
        assertEquals(true, tenToTwenty.containsDouble(float12));
        assertEquals(true, tenToTwenty.containsDouble(float20));
        assertEquals(false, tenToTwenty.containsDouble(float21));
        
        assertEquals(false, tenToTwenty.containsDouble(9d));
        assertEquals(true, tenToTwenty.containsDouble(10d));
        assertEquals(true, tenToTwenty.containsDouble(15d));
        assertEquals(true, tenToTwenty.containsDouble(20d));
        assertEquals(false, tenToTwenty.containsDouble(21d));
    }

    public void testContainsFloat() {
        assertEquals(false, tenToTwenty.containsFloat(null));
        assertEquals(true, tenToTwenty.containsFloat(nonComparableNumber));
        
        assertEquals(false, tenToTwenty.containsFloat(five));
        assertEquals(true, tenToTwenty.containsFloat(ten));
        assertEquals(true, tenToTwenty.containsFloat(fifteen));
        assertEquals(true, tenToTwenty.containsFloat(twenty));
        assertEquals(false, tenToTwenty.containsFloat(twentyFive));
        
        assertEquals(false, tenToTwenty.containsFloat(long8));
        assertEquals(true, tenToTwenty.containsFloat(long10));
        assertEquals(true, tenToTwenty.containsFloat(long12));
        assertEquals(true, tenToTwenty.containsFloat(long20));
        assertEquals(false, tenToTwenty.containsFloat(long21));
        
        assertEquals(false, tenToTwenty.containsFloat(double8));
        assertEquals(true, tenToTwenty.containsFloat(double10));
        assertEquals(true, tenToTwenty.containsFloat(double12));
        assertEquals(true, tenToTwenty.containsFloat(double20));
        assertEquals(false, tenToTwenty.containsFloat(double21));
        
        assertEquals(false, tenToTwenty.containsFloat(float8));
        assertEquals(true, tenToTwenty.containsFloat(float10));
        assertEquals(true, tenToTwenty.containsFloat(float12));
        assertEquals(true, tenToTwenty.containsFloat(float20));
        assertEquals(false, tenToTwenty.containsFloat(float21));
        
        assertEquals(false, tenToTwenty.containsFloat(9f));
        assertEquals(true, tenToTwenty.containsFloat(10f));
        assertEquals(true, tenToTwenty.containsFloat(15f));
        assertEquals(true, tenToTwenty.containsFloat(20f));
        assertEquals(false, tenToTwenty.containsFloat(21f));
    }

    //--------------------------------------------------------------------------

    public void testContainsRange() {
        assertEquals(false, tenToTwenty.containsRange(null));
        assertEquals(false, tenToTwenty.containsRange(createRange(five, five)));
        assertEquals(false, tenToTwenty.containsRange(createRange(five, ten)));
        assertEquals(false, tenToTwenty.containsRange(createRange(five, twelve)));
        assertEquals(false, tenToTwenty.containsRange(createRange(five, fifteen)));
        assertEquals(false, tenToTwenty.containsRange(createRange(five, twenty)));
        assertEquals(false, tenToTwenty.containsRange(createRange(five, twentyFive)));
        
        assertEquals(true, tenToTwenty.containsRange(createRange(ten, ten)));
        assertEquals(true, tenToTwenty.containsRange(createRange(ten, twelve)));
        assertEquals(true, tenToTwenty.containsRange(createRange(ten, fifteen)));
        assertEquals(true, tenToTwenty.containsRange(createRange(ten, twenty)));
        assertEquals(false, tenToTwenty.containsRange(createRange(ten, twentyFive)));
        
        assertEquals(true, tenToTwenty.containsRange(createRange(twelve, twelve)));
        assertEquals(true, tenToTwenty.containsRange(createRange(twelve, fifteen)));
        assertEquals(true, tenToTwenty.containsRange(createRange(twelve, twenty)));
        assertEquals(false, tenToTwenty.containsRange(createRange(twelve, twentyFive)));
        
        assertEquals(true, tenToTwenty.containsRange(createRange(fifteen, fifteen)));
        assertEquals(true, tenToTwenty.containsRange(createRange(fifteen, twenty)));
        assertEquals(false, tenToTwenty.containsRange(createRange(fifteen, twentyFive)));
        
        assertEquals(true, tenToTwenty.containsRange(createRange(twenty, twenty)));
        assertEquals(false, tenToTwenty.containsRange(createRange(twenty, twentyFive)));
        
        assertEquals(false, tenToTwenty.containsRange(createRange(twentyFive, twentyFive)));
    }

    public void testOverlapsRange() {
        assertEquals(false, tenToTwenty.overlapsRange(null));
        assertEquals(false, tenToTwenty.overlapsRange(createRange(five, five)));
        assertEquals(true, tenToTwenty.overlapsRange(createRange(five, ten)));
        assertEquals(true, tenToTwenty.overlapsRange(createRange(five, twelve)));
        assertEquals(true, tenToTwenty.overlapsRange(createRange(five, fifteen)));
        assertEquals(true, tenToTwenty.overlapsRange(createRange(five, twenty)));
        assertEquals(true, tenToTwenty.overlapsRange(createRange(five, twentyFive)));
        
        assertEquals(true, tenToTwenty.overlapsRange(createRange(ten, ten)));
        assertEquals(true, tenToTwenty.overlapsRange(createRange(ten, twelve)));
        assertEquals(true, tenToTwenty.overlapsRange(createRange(ten, fifteen)));
        assertEquals(true, tenToTwenty.overlapsRange(createRange(ten, twenty)));
        assertEquals(true, tenToTwenty.overlapsRange(createRange(ten, twentyFive)));
        
        assertEquals(true, tenToTwenty.overlapsRange(createRange(twelve, twelve)));
        assertEquals(true, tenToTwenty.overlapsRange(createRange(twelve, fifteen)));
        assertEquals(true, tenToTwenty.overlapsRange(createRange(twelve, twenty)));
        assertEquals(true, tenToTwenty.overlapsRange(createRange(twelve, twentyFive)));
        
        assertEquals(true, tenToTwenty.overlapsRange(createRange(fifteen, fifteen)));
        assertEquals(true, tenToTwenty.overlapsRange(createRange(fifteen, twenty)));
        assertEquals(true, tenToTwenty.overlapsRange(createRange(fifteen, twentyFive)));
        
        assertEquals(true, tenToTwenty.overlapsRange(createRange(twenty, twenty)));
        assertEquals(true, tenToTwenty.overlapsRange(createRange(twenty, twentyFive)));
        
        assertEquals(false, tenToTwenty.overlapsRange(createRange(twentyFive, twentyFive)));
    }

    //--------------------------------------------------------------------------

    public void testEquals() {
        assertEquals(false, tenToTwenty.equals(createRange(ten, fifteen)));
        assertEquals(false, tenToTwenty.equals(createRange(ten, twentyFive)));
        
        assertEquals(false, tenToTwenty.equals(createRange(fifteen, twenty)));
        assertEquals(false, tenToTwenty.equals(createRange(five, twenty)));
        
        assertEquals(false, tenToTwenty.equals(createRange(five, ten)));
        assertEquals(false, tenToTwenty.equals(createRange(ten)));

        assertEquals(true, tenToTwenty.equals(tenToTwenty));
        assertEquals(true, tenToTwenty.equals(createRange(ten, twenty)));
        assertEquals(true, tenToTwenty.equals(createRange(twenty, ten)));
        
        assertEquals(false, tenToTwenty.equals(null));
        assertEquals(false, tenToTwenty.equals(new Object()));
        assertEquals(false, tenToTwenty.equals(otherRange));
    }

    public void testHashCode() {
        assertEquals(tenToTwenty.hashCode(), tenToTwenty.hashCode());
        assertTrue(tenToTwenty.hashCode() != 0);
    }
    
    public void testToString() {
        String str = tenToTwenty.toString();
        assertEquals("Range[10,20]", str);
        assertSame(str, tenToTwenty.toString());
        assertEquals("Range[-20,-10]", createRange(new Integer(-20), new Integer(-10)).toString());
    }


    protected abstract Range createRange(Integer integer);
    protected abstract Range createRange(Integer integer1, Integer integer2);

}
