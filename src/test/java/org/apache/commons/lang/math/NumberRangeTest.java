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
 * Test cases for the {@link NumberRange} class.
 * 
 * @author <a href="mailto:chrise@esha.com">Christopher Elkins</a>
 * @author <a href="mailto:ridesmet@users.sourceforge.net">Ringo De Smet</a>
 * @author Stephen Colebourne
 * @version $Id$
 */
public final class NumberRangeTest extends AbstractRangeTest {

    public static Test suite() {
        TestSuite suite = new TestSuite(NumberRangeTest.class);
        suite.setName("NumberRange Tests");
        return suite;
    }

    public NumberRangeTest(String name) {
        super(name);
    }

    void checkConstructorException(Number num) {
        try {
            new NumberRange(num);
            fail("Expected an IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // Expected.
        }
    }

    void checkConstructorException(Number num1, Number num2) {
        try {
            new NumberRange(num1, num2);
            fail("Expected an IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // Expected.
        }
    }

    protected Range createRange(Integer integer) {
        return new NumberRange(integer);
    }

    // --------------------------------------------------------------------------

    protected Range createRange(Integer integer1, Integer integer2) {
        return new NumberRange(integer1, integer2);
    }

    public void setUp() {
        super.setUp();
        tenToTwenty = new NumberRange(ten, twenty);
        otherRange = new IntRange(ten, twenty);
    }

    /**
     * Tests non-exceptional conditions for the one argument constructor.
     */
    public void testConstructor1() {
        NumberRange nr = new NumberRange(five);
        assertSame(five, nr.getMinimumNumber());
        assertSame(five, nr.getMaximumNumber());
    }

    /**
     * Tests exceptional conditions for the one argument constructor.
     */
    public void testConstructor1Exceptions() {
        this.checkConstructorException(null);
        this.checkConstructorException(nonComparableNumber);
        this.checkConstructorException(new Float(Float.NaN));
        this.checkConstructorException(new Double(Double.NaN));
    }

    /**
     * Tests non-exceptional conditions for the two argument constructor.
     */
    public void testConstructor2() {
        NumberRange nr = new NumberRange(five, ten);
        assertSame(five, nr.getMinimumNumber());
        assertSame(ten, nr.getMaximumNumber());

        nr = new NumberRange(ten, five);
        assertSame(five, nr.getMinimumNumber());
        assertSame(ten, nr.getMaximumNumber());
    }

    /**
     * Tests exceptional conditions for the two argument constructor.
     */
    public void testConstructor2Exceptions() {
        this.checkConstructorException(null, null);

        this.checkConstructorException(new Float(12.2f), new Double(12.2));
        this.checkConstructorException(new Float(Float.NaN), new Double(12.2));
        this.checkConstructorException(new Double(Double.NaN), new Double(12.2));
        this.checkConstructorException(new Double(12.2), new Double(Double.NaN));
        this.checkConstructorException(new Double(Double.NaN), new Double(Double.NaN));
        this.checkConstructorException(null, new Double(12.2));
        this.checkConstructorException(new Double(12.2), null);

        this.checkConstructorException(new Double(12.2f), new Float(12.2));
        this.checkConstructorException(new Double(Double.NaN), new Float(12.2));
        this.checkConstructorException(new Float(Float.NaN), new Float(12.2));
        this.checkConstructorException(new Float(12.2), new Float(Float.NaN));
        this.checkConstructorException(new Float(Float.NaN), new Float(Float.NaN));
        this.checkConstructorException(null, new Float(12.2));
        this.checkConstructorException(new Float(12.2), null);

        this.checkConstructorException(nonComparableNumber, nonComparableNumber);
        this.checkConstructorException(null, nonComparableNumber);
        this.checkConstructorException(nonComparableNumber, null);
        this.checkConstructorException(new Float(12.2), nonComparableNumber);
        this.checkConstructorException(nonComparableNumber, new Float(12.2));
    }

    // --------------------------------------------------------------------------

    public void testContainsLongBig() {
        // original NumberRange class failed this test
        NumberRange big = new NumberRange(new Long(Long.MAX_VALUE), new Long(Long.MAX_VALUE - 2));
        assertEquals(true, big.containsLong(Long.MAX_VALUE - 1));
        assertEquals(false, big.containsLong(Long.MAX_VALUE - 3));
    }

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
        } catch (IllegalArgumentException ex) {
        }
    }

    // --------------------------------------------------------------------------

}
