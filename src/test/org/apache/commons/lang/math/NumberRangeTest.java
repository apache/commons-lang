/*
 * Copyright 2002,2004 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang.math;

import junit.framework.Test;
import junit.framework.TestSuite;

/**
 * Test cases for the {@link NumberRange} class.
 *
 * @author <a href="mailto:chrise@esha.com">Christopher Elkins</a>
 * @author <a href="mailto:ridesmet@users.sourceforge.net">Ringo De Smet</a>
 * @author Stephen Colebourne
 * @version $Id$
 */
public final class NumberRangeTest extends AbstractRangeTest {

    public NumberRangeTest(String name) {
        super(name);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(NumberRangeTest.class);
        suite.setName("NumberRange Tests");
        return suite;
    }
    
    public void setUp() {
        super.setUp();
        tenToTwenty = new NumberRange(ten, twenty);
        otherRange = new IntRange(ten, twenty);
    }

    protected Range createRange(Integer integer1, Integer integer2) {
        return new NumberRange(integer1, integer2);
    }
    protected Range createRange(Integer integer) {
        return new NumberRange(integer);
    }

    //--------------------------------------------------------------------------

    public void testConstructor1() {
        NumberRange nr = new NumberRange(five);
        assertSame(five, nr.getMinimumNumber());
        assertSame(five, nr.getMaximumNumber());
        
        try {
            new NumberRange(null);
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            new NumberRange(nonComparable);
            fail();
        } catch (IllegalArgumentException ex) {}
    }
    
    public void testConstructor2() {
        NumberRange nr = new NumberRange(five, ten);
        assertSame(five, nr.getMinimumNumber());
        assertSame(ten, nr.getMaximumNumber());
        
        nr = new NumberRange(ten, five);
        assertSame(five, nr.getMinimumNumber());
        assertSame(ten, nr.getMaximumNumber());
        
        // not null
        try {
            new NumberRange(five, null);
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            new NumberRange(null, five);
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            new NumberRange(null, null);
            fail();
        } catch (IllegalArgumentException ex) {}
        
        // no mixed types
        try {
            new NumberRange(five, long21);
            fail();
        } catch (IllegalArgumentException ex) {}
        
        // must be comparable
        try {
            new NumberRange(nonComparable, nonComparable);
            fail();
        } catch (IllegalArgumentException ex) {}
        
        // no double NaN
        try {
            new NumberRange(new Double(0), new Double(Double.NaN));
            fail();
        } catch (IllegalArgumentException ex) {}
        
        try {
            new NumberRange(new Double(Double.NaN), new Double(0));
            fail();
        } catch (IllegalArgumentException ex) {}
        
        // no float NaN
        try {
            new NumberRange(new Float(0), new Float(Float.NaN));
            fail();
        } catch (IllegalArgumentException ex) {}
        
        try {
            new NumberRange(new Float(Float.NaN), new Float(0));
            fail();
        } catch (IllegalArgumentException ex) {}
    }

    //--------------------------------------------------------------------------

    public void testContainsNumber() {
        assertEquals(false, tenToTwenty.containsNumber(null));
        assertEquals(false, tenToTwenty.containsNumber(five));
        assertEquals(true, tenToTwenty.containsNumber(ten));
        assertEquals(true, tenToTwenty.containsNumber(fifteen));
        assertEquals(true, tenToTwenty.containsNumber(twenty));
        assertEquals(false, tenToTwenty.containsNumber(twentyFive));
        
        try {
            tenToTwenty.containsNumber(long21);
            fail();
        } catch (IllegalArgumentException ex) {}
    }

    public void testContainsLongBig() {
        // original NumberRange class failed this test
        NumberRange big = new NumberRange(new Long(Long.MAX_VALUE), new Long(Long.MAX_VALUE- 2));
        assertEquals(true, big.containsLong(Long.MAX_VALUE - 1));
        assertEquals(false, big.containsLong(Long.MAX_VALUE - 3));
    }

    //--------------------------------------------------------------------------

}
