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
 * Test cases for the {@link IntRange} class.
 *
 * @author Stephen Colebourne
 * @author Janek Bogucki
 * @author Phil Steitz
 * @version $Id$
 */
public final class IntRangeTest extends AbstractRangeTest {

    public IntRangeTest(String name) {
        super(name);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(IntRangeTest.class);
        suite.setName("IntRange Tests");
        return suite;
    }
    
    public void setUp() {
        super.setUp();
        tenToTwenty = new IntRange(ten, twenty);
        otherRange = new NumberRange(ten, twenty);
    }

    protected Range createRange(Integer integer1, Integer integer2) {
        return new IntRange(integer1, integer2);
    }
    protected Range createRange(Integer integer) {
        return new NumberRange(integer);
    }
    
    //--------------------------------------------------------------------------

    public void testConstructor1a() {
        IntRange nr = new IntRange(5);
        assertEquals(five, nr.getMinimumNumber());
        assertEquals(five, nr.getMaximumNumber());
    }
    
    public void testConstructor1b() {
        IntRange nr = new IntRange(five);
        assertSame(five, nr.getMinimumNumber());
        assertSame(five, nr.getMaximumNumber());
        
        Range r = new IntRange(nonComparableNumber);
        
        try {
            new IntRange(null);
            fail();
        } catch (IllegalArgumentException ex) {}
    }
    
    public void testConstructor2a() {
        IntRange nr = new IntRange(5, 10);
        assertEquals(five, nr.getMinimumNumber());
        assertEquals(ten, nr.getMaximumNumber());
        
        nr = new IntRange(5, 10);
        assertEquals(five, nr.getMinimumNumber());
        assertEquals(ten, nr.getMaximumNumber());
    }

    public void testConstructor2b() {
        IntRange nr = new IntRange(five, ten);
        assertSame(five, nr.getMinimumNumber());
        assertSame(ten, nr.getMaximumNumber());
        
        nr = new IntRange(ten, five);
        assertSame(five, nr.getMinimumNumber());
        assertSame(ten, nr.getMaximumNumber());
        
        nr = new IntRange(five, long10);
        assertSame(five, nr.getMinimumNumber());
        assertEquals(ten, nr.getMaximumNumber());
        
        // test non Integer, for full coverage
        Long fiveL = new Long(5L);
        Long tenL = new Long(10L);
        nr = new IntRange(fiveL, tenL);
        assertEquals(five, nr.getMinimumNumber());
        assertEquals(ten, nr.getMaximumNumber());
        nr = new IntRange(tenL, fiveL);
        assertEquals(five, nr.getMinimumNumber());
        assertEquals(ten, nr.getMaximumNumber());
        
        // not null
        try {
            new IntRange(five, null);
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            new IntRange(null, five);
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            new IntRange(null, null);
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

    public void testContainsIntegerBig() {
        IntRange big = new IntRange(Integer.MAX_VALUE, Integer.MAX_VALUE- 2);
        assertEquals(true, big.containsInteger(Integer.MAX_VALUE - 1));
        assertEquals(false, big.containsInteger(Integer.MAX_VALUE - 3));
    }

    //--------------------------------------------------------------------------
    
}
