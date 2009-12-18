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

import junit.framework.TestCase;

/**
 * JUnit tests.
 * 
 * @version $Id$
 * @see MutableDouble
 */
public class MutableDoubleTest extends TestCase {

    public MutableDoubleTest(String testName) {
        super(testName);
    }

    // ----------------------------------------------------------------
    public void testConstructors() {
        assertEquals(0d, new MutableDouble().doubleValue(), 0.0001d);
        
        assertEquals(1d, new MutableDouble(1d).doubleValue(), 0.0001d);
        
        assertEquals(2d, new MutableDouble(new Double(2d)).doubleValue(), 0.0001d);
        assertEquals(3d, new MutableDouble(new MutableDouble(3d)).doubleValue(), 0.0001d);
        
        assertEquals(2d, new MutableDouble("2.0").doubleValue(), 0.0001d);

        try {
            new MutableDouble((Number)null);
            fail();
        } catch (NullPointerException ex) {}
    }

    public void testGetSet() {
        final MutableDouble mutNum = new MutableDouble(0d);
        assertEquals(0d, new MutableDouble().doubleValue(), 0.0001d);
        assertEquals(new Double(0), new MutableDouble().getValue());
        
        mutNum.setValue(1);
        assertEquals(1d, mutNum.doubleValue(), 0.0001d);
        assertEquals(new Double(1d), mutNum.getValue());
        
        mutNum.setValue(new Double(2d));
        assertEquals(2d, mutNum.doubleValue(), 0.0001d);
        assertEquals(new Double(2d), mutNum.getValue());
        
        mutNum.setValue(new MutableDouble(3d));
        assertEquals(3d, mutNum.doubleValue(), 0.0001d);
        assertEquals(new Double(3d), mutNum.getValue());
        try {
            mutNum.setValue(null);
            fail();
        } catch (NullPointerException ex) {}
    }

    public void testNanInfinite() {
        MutableDouble mutNum = new MutableDouble(Double.NaN);
        assertEquals(true, mutNum.isNaN());
        
        mutNum = new MutableDouble(Double.POSITIVE_INFINITY);
        assertEquals(true, mutNum.isInfinite());
        
        mutNum = new MutableDouble(Double.NEGATIVE_INFINITY);
        assertEquals(true, mutNum.isInfinite());
    }

    public void testEquals() {
        final MutableDouble mutNumA = new MutableDouble(0d);
        final MutableDouble mutNumB = new MutableDouble(0d);
        final MutableDouble mutNumC = new MutableDouble(1d);

        assertEquals(true, mutNumA.equals(mutNumA));
        assertEquals(true, mutNumA.equals(mutNumB));
        assertEquals(true, mutNumB.equals(mutNumA));
        assertEquals(true, mutNumB.equals(mutNumB));
        assertEquals(false, mutNumA.equals(mutNumC));
        assertEquals(false, mutNumB.equals(mutNumC));
        assertEquals(true, mutNumC.equals(mutNumC));
        assertEquals(false, mutNumA.equals(null));
        assertEquals(false, mutNumA.equals(new Double(0d)));
        assertEquals(false, mutNumA.equals("0"));
    }

    public void testHashCode() {
        final MutableDouble mutNumA = new MutableDouble(0d);
        final MutableDouble mutNumB = new MutableDouble(0d);
        final MutableDouble mutNumC = new MutableDouble(1d);

        assertEquals(true, mutNumA.hashCode() == mutNumA.hashCode());
        assertEquals(true, mutNumA.hashCode() == mutNumB.hashCode());
        assertEquals(false, mutNumA.hashCode() == mutNumC.hashCode());
        assertEquals(true, mutNumA.hashCode() == new Double(0d).hashCode());
    }

    public void testCompareTo() {
        final MutableDouble mutNum = new MutableDouble(0d);

        assertEquals(0, mutNum.compareTo(new MutableDouble(0d)));
        assertEquals(+1, mutNum.compareTo(new MutableDouble(-1d)));
        assertEquals(-1, mutNum.compareTo(new MutableDouble(1d)));
        try {
            mutNum.compareTo(null);
            fail();
        } catch (NullPointerException ex) {}
    }

    public void testPrimitiveValues() {
        MutableDouble mutNum = new MutableDouble(1.7);
        
        assertEquals( 1.7F, mutNum.floatValue(), 0 );
        assertEquals( 1.7, mutNum.doubleValue(), 0 );
        assertEquals( (byte) 1, mutNum.byteValue() );
        assertEquals( (short) 1, mutNum.shortValue() );
        assertEquals( 1, mutNum.intValue() );
        assertEquals( 1L, mutNum.longValue() );
    }

    public void testToDouble() {
        assertEquals(new Double(0d), new MutableDouble(0d).toDouble());
        assertEquals(new Double(12.3d), new MutableDouble(12.3d).toDouble());
    }

    public void testIncrement() {
        MutableDouble mutNum = new MutableDouble(1);
        mutNum.increment();
        
        assertEquals(2, mutNum.intValue());
        assertEquals(2L, mutNum.longValue());
    }

    public void testDecrement() {
        MutableDouble mutNum = new MutableDouble(1);
        mutNum.decrement();
        
        assertEquals(0, mutNum.intValue());
        assertEquals(0L, mutNum.longValue());
    }

    public void testAddValuePrimitive() {
        MutableDouble mutNum = new MutableDouble(1);
        mutNum.add(1.1d);
        
        assertEquals(2.1d, mutNum.doubleValue(), 0.01d);
    }

    public void testAddValueObject() {
        MutableDouble mutNum = new MutableDouble(1);
        mutNum.add(new Double(1.1d));
        
        assertEquals(2.1d, mutNum.doubleValue(), 0.01d);
    }

    public void testSubtractValuePrimitive() {
        MutableDouble mutNum = new MutableDouble(1);
        mutNum.subtract(0.9d);
        
        assertEquals(0.1d, mutNum.doubleValue(), 0.01d);
    }

    public void testSubtractValueObject() {
        MutableDouble mutNum = new MutableDouble(1);
        mutNum.subtract(new Double(0.9d));
        
        assertEquals(0.1d, mutNum.doubleValue(), 0.01d);
    }

    public void testToString() {
        assertEquals("0.0", new MutableDouble(0d).toString());
        assertEquals("10.0", new MutableDouble(10d).toString());
        assertEquals("-123.0", new MutableDouble(-123d).toString());
    }

}
