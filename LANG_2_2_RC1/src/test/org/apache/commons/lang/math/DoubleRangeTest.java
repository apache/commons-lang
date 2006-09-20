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

import junit.framework.Test;
import junit.framework.TestSuite;

/**
 * Test cases for the {@link DoubleRange} class.
 *
 * @author Stephen Colebourne
 * @version $Id$
 */
public final class DoubleRangeTest extends AbstractRangeTest {

    public DoubleRangeTest(String name) {
        super(name);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(DoubleRangeTest.class);
        suite.setName("DoubleRange Tests");
        return suite;
    }
    
    public void setUp() {
        super.setUp();
        tenToTwenty = new DoubleRange(double10, double20);
        otherRange = new NumberRange(ten, twenty);
    }

    protected Range createRange(Integer integer1, Integer integer2) {
        return new DoubleRange(integer1, integer2);
    }
    protected Range createRange(Integer integer) {
        return new NumberRange(integer);
    }
    
    //--------------------------------------------------------------------------

    public void testConstructor1a() {
        DoubleRange nr = new DoubleRange(8d);
        assertEquals(double8, nr.getMinimumNumber());
        assertEquals(double8, nr.getMaximumNumber());
        
        try {
            new DoubleRange(Double.NaN);
            fail();
        } catch (IllegalArgumentException ex) {}
    }
    
    public void testConstructor1b() {
        DoubleRange nr = new DoubleRange(double8);
        assertSame(double8, nr.getMinimumNumber());
        assertSame(double8, nr.getMaximumNumber());
        
        Range r = new DoubleRange(nonComparableNumber);
        
        try {
            new DoubleRange(null);
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            new DoubleRange(new Double(Double.NaN));
            fail();
        } catch (IllegalArgumentException ex) {}
    }
    
    public void testConstructor2a() {
        DoubleRange nr = new DoubleRange(8d, 10d);
        assertEquals(double8, nr.getMinimumNumber());
        assertEquals(double10, nr.getMaximumNumber());
        
        nr = new DoubleRange(10d, 8d);
        assertEquals(double8, nr.getMinimumNumber());
        assertEquals(double10, nr.getMaximumNumber());
        
        try {
            new DoubleRange(Double.NaN, 8d);
            fail();
        } catch (IllegalArgumentException ex) {}
    }

    public void testConstructor2b() {
        DoubleRange nr = new DoubleRange(double8, double10);
        assertSame(double8, nr.getMinimumNumber());
        assertSame(double10, nr.getMaximumNumber());
        
        nr = new DoubleRange(double10, double8);
        assertSame(double8, nr.getMinimumNumber());
        assertSame(double10, nr.getMaximumNumber());
        
        nr = new DoubleRange(double8, double10);
        assertSame(double8, nr.getMinimumNumber());
        assertEquals(double10, nr.getMaximumNumber());
        
        // not null
        try {
            new DoubleRange(double8, null);
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            new DoubleRange(null, double8);
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            new DoubleRange(null, null);
            fail();
        } catch (IllegalArgumentException ex) {}
        
        try {
            new DoubleRange(new Double(Double.NaN), double10);
            fail();
        } catch (IllegalArgumentException ex) {}
    }

    //--------------------------------------------------------------------------

    public void testContainsNumber() {
        assertEquals(false, tenToTwenty.containsNumber(null));
        assertEquals(true, tenToTwenty.containsNumber(nonComparableNumber));
        
        assertEquals(false, tenToTwenty.containsNumber(five));
        assertEquals(true, tenToTwenty.containsNumber(ten));
        assertEquals(true, tenToTwenty.containsNumber(fifteen));
        assertEquals(true, tenToTwenty.containsNumber(twenty));
        assertEquals(false, tenToTwenty.containsNumber(twentyFive));
        
        assertEquals(false, tenToTwenty.containsNumber(long8));
        assertEquals(true, tenToTwenty.containsNumber(long10));
        assertEquals(true, tenToTwenty.containsNumber(long12));
        assertEquals(true, tenToTwenty.containsNumber(long20));
        assertEquals(false, tenToTwenty.containsNumber(long21));
        
        assertEquals(false, tenToTwenty.containsNumber(double8));
        assertEquals(true, tenToTwenty.containsNumber(double10));
        assertEquals(true, tenToTwenty.containsNumber(double12));
        assertEquals(true, tenToTwenty.containsNumber(double20));
        assertEquals(false, tenToTwenty.containsNumber(double21));
        
        assertEquals(false, tenToTwenty.containsNumber(float8));
        assertEquals(true, tenToTwenty.containsNumber(float10));
        assertEquals(true, tenToTwenty.containsNumber(float12));
        assertEquals(true, tenToTwenty.containsNumber(float20));
        assertEquals(false, tenToTwenty.containsNumber(float21));
    }

    public void testToString() {
        String str = tenToTwenty.toString();
        assertEquals("Range[10.0,20.0]", str);
        assertSame(str, tenToTwenty.toString());
        assertEquals("Range[-20.0,-10.0]", createRange(new Integer(-20), new Integer(-10)).toString());
    }
    
    //--------------------------------------------------------------------------
    
}
