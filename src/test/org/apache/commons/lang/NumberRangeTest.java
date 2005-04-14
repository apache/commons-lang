/*
 * Copyright 2002-2005 The Apache Software Foundation.
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
package org.apache.commons.lang;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * Test cases for the {@link NumberRange} class.
 *
 * @author <a href="mailto:chrise@esha.com">Christopher Elkins</a>
 * @author <a href="mailto:ridesmet@users.sourceforge.net">Ringo De Smet</a>
 * @author Tim O'Brien
 * @version $Revision$ $Date$
 */

public final class NumberRangeTest extends TestCase {


    private NumberRange tenToTwenty;
    private NumberRange fifteenToTwentyFive;
    private NumberRange fiveToNine;
    private Number five;
    private Number nine;
    private Number ten;
    private Number fifteen;
    private Number twenty;
    private Number twentyFive;

    public NumberRangeTest(String name) {
        super(name);
    }

    public void setUp() {
        five       = new Integer(5);
        nine      = new Double(9.0);
        ten        = new Integer(10);
        fifteen    = new Integer(15);
        twenty     = new Integer(20);
        twentyFive = new Integer(25);

        tenToTwenty = new NumberRange(ten, twenty);
        fifteenToTwentyFive = new NumberRange( fifteen, twentyFive);
        fiveToNine = new NumberRange( five, nine );
    }


    public static Test suite() {
        TestSuite suite = new TestSuite(NumberRangeTest.class);
        suite.setName("NumberRange Tests");
        return suite;
    }

    public void testMaxMin() {
        boolean expected = true;
        boolean result = tenToTwenty.getMaximum().equals(twenty);
        assertEquals(expected, result);

        expected = true;
        result = tenToTwenty.getMinimum().equals(ten);
        assertEquals(expected, result);
    }

    public void testEquals() {
        boolean expected = false;
        boolean result = tenToTwenty.equals(new NumberRange(five, ten));
        assertEquals(expected, result);

        expected = true;
        result = tenToTwenty.equals(new NumberRange(ten, twenty));
        assertEquals(expected, result);

        expected = false;
        result = tenToTwenty.equals(new NumberRange(ten, fifteen));
        assertEquals(expected, result);

        expected = false;
        result = tenToTwenty.equals(new NumberRange(fifteen, twenty));
        assertEquals(expected, result);
    }
    
    public void testEqualsWithOtherObject() {
        assertEquals( "A NumberRange should not equals a String object", false, fiveToNine.equals("TEST"));
    }

    public void testEqualsWithSameReference() {
        assertEquals( "A NumberRange should equal itself", true, fiveToNine.equals(fiveToNine));
    }

    public void testEqualsNull() {
        assertEquals( "A NumberRange should not equal null", false, fiveToNine.equals(null));
    }

    public void testHashCode() {
        NumberRange nr = new NumberRange( new Integer(5), new Double(9.0));
        assertEquals( "The hashCode of 5-9 should equals the hashcode of another NumberRange of the same min/max",
                                fiveToNine.hashCode(), nr.hashCode());
        assertTrue( "The hashCode of 10-20 should not equal the hashCode of 5-9", 
                            fiveToNine.hashCode() != tenToTwenty.hashCode());                        
    }

    public void testIncludesNumber() {
        boolean expected = false;
        boolean result = tenToTwenty.includesNumber(five);
        assertEquals(expected, result);

        expected = true;
        result = tenToTwenty.includesNumber(ten);
        assertEquals(expected, result);

        expected = true;
        result = tenToTwenty.includesNumber(fifteen);
        assertEquals(expected, result);

        expected = true;
        result = tenToTwenty.includesNumber(twenty);
        assertEquals(expected, result);

        expected = false;
        result = tenToTwenty.includesNumber(twentyFive);
        assertEquals(expected, result);
    }

    public void testIncludesNumberNull() {
        boolean result = tenToTwenty.includesNumber(null);
        assertEquals("Includes number should return false for null values", false, result);
    }

    public void testIncludesRange() {
        boolean expected = false;
        boolean result = tenToTwenty.includesRange(new NumberRange(five, ten));
        assertEquals(expected, result);

        expected = false;
        result = tenToTwenty.includesRange(new NumberRange(five, fifteen));
        assertEquals(expected, result);

        expected = true;
        result = tenToTwenty.includesRange(new NumberRange(ten, fifteen));
        assertEquals(expected, result);

        expected = true;
        result = tenToTwenty.includesRange(new NumberRange(ten, twenty));
        assertEquals(expected, result);

        expected = true;
        result = tenToTwenty.includesRange(new NumberRange(fifteen, twenty));
        assertEquals(expected, result);

        expected = false;
        result = 
            tenToTwenty.includesRange(new NumberRange(fifteen, twentyFive));
        assertEquals(expected, result);

        expected = false;
        result = 
            tenToTwenty.includesRange(new NumberRange(twenty, twentyFive));
        assertEquals(expected, result);
    }

    public void testIncludesRangeNull() {
        boolean result = tenToTwenty.includesRange(null);
        assertEquals("Includes range should return false for null values", false, result);
    }

    public void testConstructor() {
        NumberRange nr = new NumberRange( new Double(2.0));
        assertEquals("Unexpected min on NumberRange", 2.0, nr.getMinimum().doubleValue(), Double.MIN_VALUE);
        assertEquals("Unexpected max on NumberRange", 2.0, nr.getMaximum().doubleValue(), Double.MIN_VALUE);
    }

    public void testConstructorNullParameters() {
        try {
            NumberRange nr = new NumberRange(null);
            fail("NumberRange(null) did not throw an exception.");
        } catch (Exception e) {
            assertTrue(    "NumberRange(null)", e instanceof NullPointerException);
        }

        try {
            NumberRange nr = new NumberRange(five, null);
            fail("NumberRange(five, null) did not throw an exception.");
        } catch (Exception e) {
            assertTrue("NumberRange(five, null)", e instanceof NullPointerException);
        }

        try {
            NumberRange nr = new NumberRange(null, five);
            fail("NumberRange(null, five) did not throw an exception.");
        } catch (Exception e) {
            assertTrue("NumberRange(null, five)", e instanceof NullPointerException);
        }
    }

    public void testConstructorWithMaxLessThanMin() {
        NumberRange nr = new NumberRange( new Double(2.0), new Double(1.0));
        assertEquals("Unexpected min on NumberRange", 2.0, nr.getMinimum().doubleValue(), Double.MIN_VALUE);
        assertEquals("Unexpected max on NumberRange", 2.0, nr.getMaximum().doubleValue(), Double.MIN_VALUE);
    }

    public void testOverlap() {
        assertEquals( "5-9 should not overlap 10-20", false, fiveToNine.overlaps( tenToTwenty ));
        assertEquals( "10-20 should overlap 15-25", true, tenToTwenty.overlaps( fifteenToTwentyFive ));
    }

    public void testOverlapNull() {
        assertEquals( "5-9 should not overlap null", false, fiveToNine.overlaps( null ));
    }

    public void testToString() {
        String expected = "10-20";
        String result = tenToTwenty.toString();
        assertEquals(expected, result);
    }

    public void testToStringWithNegatives() {
        String expected = "(-20)-(-10)";
        NumberRange nr = new NumberRange( new Integer(-20), new Integer(-10));
        String result = nr.toString();
        assertEquals(expected, result);

        expected = "(-20)-10";
        nr = new NumberRange( new Integer(-20), new Integer(10));
        result = nr.toString();
        assertEquals(expected, result);
    }


}

