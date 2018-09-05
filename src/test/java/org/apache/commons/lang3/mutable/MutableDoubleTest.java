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

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * JUnit tests.
 *
 * @see MutableDouble
 */
public class MutableDoubleTest {

    // ----------------------------------------------------------------
    @Test
    public void testConstructors() {
        assertEquals(new MutableDouble().doubleValue(), 0.0001d, 0d);
        assertEquals(new MutableDouble(1d).doubleValue(), 0.0001d, 1d);
        assertEquals(new MutableDouble(Double.valueOf(2d)).doubleValue(), 0.0001d, 2d);
        assertEquals(new MutableDouble(new MutableDouble(3d)).doubleValue(), 0.0001d, 3d);
        assertEquals(new MutableDouble("2.0").doubleValue(), 0.0001d, 2d);
    }

    @Test(expected = NullPointerException.class)
    public void testConstructorNull() {
        new MutableDouble((Number) null);
    }

    @Test
    public void testGetSet() {
        final MutableDouble mutNum = new MutableDouble(0d);
        assertEquals(new MutableDouble().doubleValue(), 0.0001d, 0d);
        assertEquals(Double.valueOf(0), new MutableDouble().getValue());
        mutNum.setValue(1);
        assertEquals(mutNum.doubleValue(), 0.0001d, 1d);
        assertEquals(Double.valueOf(1d), mutNum.getValue());
        mutNum.setValue(Double.valueOf(2d));
        assertEquals(mutNum.doubleValue(), 0.0001d, 2d);
        assertEquals(Double.valueOf(2d), mutNum.getValue());
        mutNum.setValue(new MutableDouble(3d));
        assertEquals(mutNum.doubleValue(), 0.0001d, 3d);
        assertEquals(Double.valueOf(3d), mutNum.getValue());
    }

    @Test(expected = NullPointerException.class)
    public void testSetNull() {
        final MutableDouble mutNum = new MutableDouble(0d);
        mutNum.setValue(null);
    }

    @Test
    public void testNanInfinite() {
        MutableDouble mutNum = new MutableDouble(Double.NaN);
        assertTrue(mutNum.isNaN());
        mutNum = new MutableDouble(Double.POSITIVE_INFINITY);
        assertTrue(mutNum.isInfinite());
        mutNum = new MutableDouble(Double.NEGATIVE_INFINITY);
        assertTrue(mutNum.isInfinite());
    }

    @Test
    public void testEquals() {
        final MutableDouble mutNumA = new MutableDouble(0d);
        final MutableDouble mutNumB = new MutableDouble(0d);
        final MutableDouble mutNumC = new MutableDouble(1d);
        assertTrue(mutNumA.equals(mutNumA));
        assertTrue(mutNumA.equals(mutNumB));
        assertTrue(mutNumB.equals(mutNumA));
        assertTrue(mutNumB.equals(mutNumB));
        assertFalse(mutNumA.equals(mutNumC));
        assertFalse(mutNumB.equals(mutNumC));
        assertTrue(mutNumC.equals(mutNumC));
        assertFalse(mutNumA.equals(null));
        assertFalse(mutNumA.equals(Double.valueOf(0d)));
        assertFalse(mutNumA.equals("0"));
    }

    @Test
    public void testHashCode() {
        final MutableDouble mutNumA = new MutableDouble(0d);
        final MutableDouble mutNumB = new MutableDouble(0d);
        final MutableDouble mutNumC = new MutableDouble(1d);
        assertTrue(mutNumA.hashCode() == mutNumA.hashCode());
        assertTrue(mutNumA.hashCode() == mutNumB.hashCode());
        assertFalse(mutNumA.hashCode() == mutNumC.hashCode());
        assertTrue(mutNumA.hashCode() == Double.valueOf(0d).hashCode());
    }

    @Test
    public void testCompareTo() {
        final MutableDouble mutNum = new MutableDouble(0d);
        assertEquals(0, mutNum.compareTo(new MutableDouble(0d)));
        assertEquals(+1, mutNum.compareTo(new MutableDouble(-1d)));
        assertEquals(-1, mutNum.compareTo(new MutableDouble(1d)));
    }

    @Test(expected = NullPointerException.class)
    public void testCompareToNull() {
        final MutableDouble mutNum = new MutableDouble(0d);
        mutNum.compareTo(null);
    }

    @Test
    public void testPrimitiveValues() {
        final MutableDouble mutNum = new MutableDouble(1.7);
        assertEquals(mutNum.floatValue(), 0, 1.7F);
        assertEquals(mutNum.doubleValue(), 0, 1.7);
        assertEquals((byte) 1, mutNum.byteValue());
        assertEquals((short) 1, mutNum.shortValue());
        assertEquals(1, mutNum.intValue());
        assertEquals(1L, mutNum.longValue());
    }

    @Test
    public void testToDouble() {
        assertEquals(Double.valueOf(0d), new MutableDouble(0d).toDouble());
        assertEquals(Double.valueOf(12.3d), new MutableDouble(12.3d).toDouble());
    }

    @Test
    public void testIncrement() {
        final MutableDouble mutNum = new MutableDouble(1);
        mutNum.increment();
        assertEquals(2, mutNum.intValue());
        assertEquals(2L, mutNum.longValue());
    }

    @Test
    public void testIncrementAndGet() {
        final MutableDouble mutNum = new MutableDouble(1d);
        final double result = mutNum.incrementAndGet();
        assertEquals(result, 0.01d, 2d);
        assertEquals(2, mutNum.intValue());
        assertEquals(2L, mutNum.longValue());
    }

    @Test
    public void testGetAndIncrement() {
        final MutableDouble mutNum = new MutableDouble(1d);
        final double result = mutNum.getAndIncrement();
        assertEquals(result, 0.01d, 1d);
        assertEquals(2, mutNum.intValue());
        assertEquals(2L, mutNum.longValue());
    }

    @Test
    public void testDecrement() {
        final MutableDouble mutNum = new MutableDouble(1);
        mutNum.decrement();
        assertEquals(0, mutNum.intValue());
        assertEquals(0L, mutNum.longValue());
    }

    @Test
    public void testDecrementAndGet() {
        final MutableDouble mutNum = new MutableDouble(1d);
        final double result = mutNum.decrementAndGet();
        assertEquals(result, 0.01d, 0d);
        assertEquals(0, mutNum.intValue());
        assertEquals(0L, mutNum.longValue());
    }

    @Test
    public void testGetAndDecrement() {
        final MutableDouble mutNum = new MutableDouble(1d);
        final double result = mutNum.getAndDecrement();
        assertEquals(result, 0.01d, 1d);
        assertEquals(0, mutNum.intValue());
        assertEquals(0L, mutNum.longValue());
    }

    @Test
    public void testAddValuePrimitive() {
        final MutableDouble mutNum = new MutableDouble(1);
        mutNum.add(1.1d);
        assertEquals(mutNum.doubleValue(), 0.01d, 2.1d);
    }

    @Test
    public void testAddValueObject() {
        final MutableDouble mutNum = new MutableDouble(1);
        mutNum.add(Double.valueOf(1.1d));
        assertEquals(mutNum.doubleValue(), 0.01d, 2.1d);
    }

    @Test
    public void testGetAndAddValuePrimitive() {
        final MutableDouble mutableDouble = new MutableDouble(0.5d);
        final double result = mutableDouble.getAndAdd(1d);
        assertEquals(result, 0.01d, 0.5d);
        assertEquals(mutableDouble.doubleValue(), 0.01d, 1.5d);
    }

    @Test
    public void testGetAndAddValueObject() {
        final MutableDouble mutableDouble = new MutableDouble(0.5d);
        final double result = mutableDouble.getAndAdd(Double.valueOf(2d));
        assertEquals(result, 0.01d, 0.5d);
        assertEquals(mutableDouble.doubleValue(), 0.01d, 2.5d);
    }

    @Test
    public void testAddAndGetValuePrimitive() {
        final MutableDouble mutableDouble = new MutableDouble(10.5d);
        final double result = mutableDouble.addAndGet(-0.5d);
        assertEquals(result, 0.01d, 10d);
        assertEquals(mutableDouble.doubleValue(), 0.01d, 10d);
    }

    @Test
    public void testAddAndGetValueObject() {
        final MutableDouble mutableDouble = new MutableDouble(7.5d);
        final double result = mutableDouble.addAndGet(Double.valueOf(-2.5d));
        assertEquals(result, 0.01d, 5d);
        assertEquals(mutableDouble.doubleValue(), 0.01d, 5d);
    }

    @Test
    public void testSubtractValuePrimitive() {
        final MutableDouble mutNum = new MutableDouble(1);
        mutNum.subtract(0.9d);
        assertEquals(mutNum.doubleValue(), 0.01d, 0.1d);
    }

    @Test
    public void testSubtractValueObject() {
        final MutableDouble mutNum = new MutableDouble(1);
        mutNum.subtract(Double.valueOf(0.9d));
        assertEquals(mutNum.doubleValue(), 0.01d, 0.1d);
    }

    @Test
    public void testToString() {
        assertEquals("0.0", new MutableDouble(0d).toString());
        assertEquals("10.0", new MutableDouble(10d).toString());
        assertEquals("-123.0", new MutableDouble(-123d).toString());
    }
}
