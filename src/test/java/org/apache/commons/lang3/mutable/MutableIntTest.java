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
package org.apache.commons.lang3.mutable;

import org.junit.Test;
import static org.junit.Assert.*;

/**
 * JUnit tests.
 * 
 * @version $Id$
 * @see MutableInt
 */
public class MutableIntTest {

    // ----------------------------------------------------------------
    @Test
    public void testConstructors() {
        assertEquals(0, new MutableInt().intValue());
        
        assertEquals(1, new MutableInt(1).intValue());
        
        assertEquals(2, new MutableInt(Integer.valueOf(2)).intValue());
        assertEquals(3, new MutableInt(new MutableLong(3)).intValue());

        assertEquals(2, new MutableInt("2").intValue());

    }

    @Test(expected=NullPointerException.class)
    public void testConstructorNull() {
        new MutableInt((Number)null);
    }

    @Test
    public void testGetSet() {
        final MutableInt mutNum = new MutableInt(0);
        assertEquals(0, new MutableInt().intValue());
        assertEquals(Integer.valueOf(0), new MutableInt().getValue());
        
        mutNum.setValue(1);
        assertEquals(1, mutNum.intValue());
        assertEquals(Integer.valueOf(1), mutNum.getValue());
        
        mutNum.setValue(Integer.valueOf(2));
        assertEquals(2, mutNum.intValue());
        assertEquals(Integer.valueOf(2), mutNum.getValue());
        
        mutNum.setValue(new MutableLong(3));
        assertEquals(3, mutNum.intValue());
        assertEquals(Integer.valueOf(3), mutNum.getValue());
    }

    @Test(expected=NullPointerException.class)
    public void testSetNull() {
        final MutableInt mutNum = new MutableInt(0);
        mutNum.setValue(null);
    }

    @Test
    public void testEquals() {
        this.testEquals(new MutableInt(0), new MutableInt(0), new MutableInt(1));
        // Should Numbers be supported? GaryG July-21-2005.
        //this.testEquals(mutNumA, Integer.valueOf(0), mutNumC);
    }

    /**
     * @param numA must not be a 0 Integer; must not equal numC.
     * @param numB must equal numA; must not equal numC.
     * @param numC must not equal numA; must not equal numC.
     */
    void testEquals(final Number numA, final Number numB, final Number numC) {
        assertTrue(numA.equals(numA));
        assertTrue(numA.equals(numB));
        assertTrue(numB.equals(numA));
        assertTrue(numB.equals(numB));
        assertFalse(numA.equals(numC));
        assertFalse(numB.equals(numC));
        assertTrue(numC.equals(numC));
        assertFalse(numA.equals(null));
        assertFalse(numA.equals(Integer.valueOf(0)));
        assertFalse(numA.equals("0"));
    }

    @Test
    public void testHashCode() {
        final MutableInt mutNumA = new MutableInt(0);
        final MutableInt mutNumB = new MutableInt(0);
        final MutableInt mutNumC = new MutableInt(1);

        assertTrue(mutNumA.hashCode() == mutNumA.hashCode());
        assertTrue(mutNumA.hashCode() == mutNumB.hashCode());
        assertFalse(mutNumA.hashCode() == mutNumC.hashCode());
        assertTrue(mutNumA.hashCode() == Integer.valueOf(0).hashCode());
    }

    @Test
    public void testCompareTo() {
        final MutableInt mutNum = new MutableInt(0);

        assertEquals(0, mutNum.compareTo(new MutableInt(0)));
        assertEquals(+1, mutNum.compareTo(new MutableInt(-1)));
        assertEquals(-1, mutNum.compareTo(new MutableInt(1)));
    }

    @Test(expected=NullPointerException.class)
    public void testCompareToNull() {
        final MutableInt mutNum = new MutableInt(0);
        mutNum.compareTo(null);
    }

    @Test
    public void testPrimitiveValues() {
        MutableInt mutNum = new MutableInt(1);
        
        assertEquals( (byte) 1, mutNum.byteValue() );
        assertEquals( (short) 1, mutNum.shortValue() );
        assertEquals( 1.0F, mutNum.floatValue(), 0 );
        assertEquals( 1.0, mutNum.doubleValue(), 0 );
        assertEquals( 1L, mutNum.longValue() );
    }

    @Test
    public void testToInteger() {
        assertEquals(Integer.valueOf(0), new MutableInt(0).toInteger());
        assertEquals(Integer.valueOf(123), new MutableInt(123).toInteger());
    }

    @Test
    public void testIncrement() {
        MutableInt mutNum = new MutableInt(1);
        mutNum.increment();
        
        assertEquals(2, mutNum.intValue());
        assertEquals(2L, mutNum.longValue());
    }

    @Test
    public void testDecrement() {
        MutableInt mutNum = new MutableInt(1);
        mutNum.decrement();
        
        assertEquals(0, mutNum.intValue());
        assertEquals(0L, mutNum.longValue());
    }

    @Test
    public void testAddValuePrimitive() {
        MutableInt mutNum = new MutableInt(1);
        mutNum.add(1);
        
        assertEquals(2, mutNum.intValue());
        assertEquals(2L, mutNum.longValue());
    }

    @Test
    public void testAddValueObject() {
        MutableInt mutNum = new MutableInt(1);
        mutNum.add(Integer.valueOf(1));
        
        assertEquals(2, mutNum.intValue());
        assertEquals(2L, mutNum.longValue());
    }

    @Test
    public void testSubtractValuePrimitive() {
        MutableInt mutNum = new MutableInt(1);
        mutNum.subtract(1);
        
        assertEquals(0, mutNum.intValue());
        assertEquals(0L, mutNum.longValue());
    }

    @Test
    public void testSubtractValueObject() {
        MutableInt mutNum = new MutableInt(1);
        mutNum.subtract(Integer.valueOf(1));
        
        assertEquals(0, mutNum.intValue());
        assertEquals(0L, mutNum.longValue());
    }

    @Test
    public void testToString() {
        assertEquals("0", new MutableInt(0).toString());
        assertEquals("10", new MutableInt(10).toString());
        assertEquals("-123", new MutableInt(-123).toString());
    }

}
