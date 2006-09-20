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
 * Test cases for the {@link LongRange} class.
 *
 * @author Stephen Colebourne
 * @version $Id$
 */
public final class LongRangeTest extends AbstractRangeTest {

    public LongRangeTest(String name) {
        super(name);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(LongRangeTest.class);
        suite.setName("LongRange Tests");
        return suite;
    }
    
    public void setUp() {
        super.setUp();
        tenToTwenty = new LongRange(long10, long20);
        otherRange = new NumberRange(ten, twenty);
    }

    protected Range createRange(Integer integer1, Integer integer2) {
        return new LongRange(integer1, integer2);
    }
    protected Range createRange(Integer integer) {
        return new NumberRange(integer);
    }
    
    //--------------------------------------------------------------------------

    public void testConstructor1a() {
        LongRange nr = new LongRange(8L);
        assertEquals(long8, nr.getMinimumNumber());
        assertEquals(long8, nr.getMaximumNumber());
    }
    
    public void testConstructor1b() {
        LongRange nr = new LongRange(long8);
        assertSame(long8, nr.getMinimumNumber());
        assertSame(long8, nr.getMaximumNumber());
        
        Range r = new LongRange(nonComparableNumber);
        
        try {
            new LongRange(null);
            fail();
        } catch (IllegalArgumentException ex) {}
    }
    
    public void testConstructor2a() {
        LongRange nr = new LongRange(8L, 10L);
        assertEquals(long8, nr.getMinimumNumber());
        assertEquals(long10, nr.getMaximumNumber());
        
        nr = new LongRange(10L, 8L);
        assertEquals(long8, nr.getMinimumNumber());
        assertEquals(long10, nr.getMaximumNumber());
    }

    public void testConstructor2b() {
        LongRange nr = new LongRange(long8, long10);
        assertSame(long8, nr.getMinimumNumber());
        assertSame(long10, nr.getMaximumNumber());
        
        nr = new LongRange(long10, long8);
        assertSame(long8, nr.getMinimumNumber());
        assertSame(long10, nr.getMaximumNumber());
        
        nr = new LongRange(long8, long10);
        assertSame(long8, nr.getMinimumNumber());
        assertEquals(long10, nr.getMaximumNumber());
        
        // not null
        try {
            new LongRange(long8, null);
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            new LongRange(null, long8);
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            new LongRange(null, null);
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

    public void testContainsLongBig() {
        LongRange big = new LongRange(Long.MAX_VALUE, Long.MAX_VALUE- 2);
        assertEquals(true, big.containsLong(Long.MAX_VALUE - 1));
        assertEquals(false, big.containsLong(Long.MAX_VALUE - 3));
    }

    //--------------------------------------------------------------------------
    
}
