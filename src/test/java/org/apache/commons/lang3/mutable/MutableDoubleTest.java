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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * JUnit tests.
 *
 * @see MutableDouble
 */
public class MutableDoubleTest extends AbstractLangTest {

    @Test
    public void testAddAndGetValueObject() {
        final MutableDouble mutableDouble = new MutableDouble(7.5d);
        final double result = mutableDouble.addAndGet(Double.valueOf(-2.5d));

        assertEquals(5d, result, 0.01d);
        assertEquals(5d, mutableDouble.doubleValue(), 0.01d);
    }

    @Test
    public void testAddAndGetValuePrimitive() {
        final MutableDouble mutableDouble = new MutableDouble(10.5d);
        final double result = mutableDouble.addAndGet(-0.5d);

        assertEquals(10d, result, 0.01d);
        assertEquals(10d, mutableDouble.doubleValue(), 0.01d);
    }

    @Test
    public void testAddValueObject() {
        final MutableDouble mutNum = new MutableDouble(1);
        mutNum.add(Double.valueOf(1.1d));

        assertEquals(2.1d, mutNum.doubleValue(), 0.01d);
    }

    @Test
    public void testAddValuePrimitive() {
        final MutableDouble mutNum = new MutableDouble(1);
        mutNum.add(1.1d);

        assertEquals(2.1d, mutNum.doubleValue(), 0.01d);
    }

    @Test
    public void testCompareTo() {
        final MutableDouble mutNum = new MutableDouble(0d);

        assertEquals(0, mutNum.compareTo(new MutableDouble(0d)));
        assertEquals(+1, mutNum.compareTo(new MutableDouble(-1d)));
        assertEquals(-1, mutNum.compareTo(new MutableDouble(1d)));
    }

    @Test
    public void testCompareToNull() {
        final MutableDouble mutNum = new MutableDouble(0d);
        assertThrows(NullPointerException.class, () -> mutNum.compareTo(null));
    }

    @Test
    public void testConstructorNull() {
        assertThrows(NullPointerException.class, () -> new MutableDouble((Number) null));
    }

    @Test
    public void testConstructors() {
        assertEquals(0d, new MutableDouble().doubleValue(), 0.0001d);

        assertEquals(1d, new MutableDouble(1d).doubleValue(), 0.0001d);

        assertEquals(2d, new MutableDouble(Double.valueOf(2d)).doubleValue(), 0.0001d);
        assertEquals(3d, new MutableDouble(new MutableDouble(3d)).doubleValue(), 0.0001d);

        assertEquals(2d, new MutableDouble("2.0").doubleValue(), 0.0001d);

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

        assertEquals(0d, result, 0.01d);
        assertEquals(0, mutNum.intValue());
        assertEquals(0L, mutNum.longValue());
    }

    @Test
    public void testEquals() {
        final MutableDouble mutNumA = new MutableDouble(0d);
        final MutableDouble mutNumB = new MutableDouble(0d);
        final MutableDouble mutNumC = new MutableDouble(1d);

        assertEquals(mutNumA, mutNumA);
        assertEquals(mutNumA, mutNumB);
        assertEquals(mutNumB, mutNumA);
        assertEquals(mutNumB, mutNumB);
        assertNotEquals(mutNumA, mutNumC);
        assertNotEquals(mutNumB, mutNumC);
        assertEquals(mutNumC, mutNumC);
        assertNotEquals(null, mutNumA);
        assertNotEquals(mutNumA, Double.valueOf(0d));
        assertNotEquals("0", mutNumA);
    }

    @Test
    public void testGetAndAddValueObject() {
        final MutableDouble mutableDouble = new MutableDouble(0.5d);
        final double result = mutableDouble.getAndAdd(Double.valueOf(2d));

        assertEquals(0.5d, result, 0.01d);
        assertEquals(2.5d, mutableDouble.doubleValue(), 0.01d);
    }

    @Test
    public void testGetAndAddValuePrimitive() {
        final MutableDouble mutableDouble = new MutableDouble(0.5d);
        final double result = mutableDouble.getAndAdd(1d);

        assertEquals(0.5d, result, 0.01d);
        assertEquals(1.5d, mutableDouble.doubleValue(), 0.01d);
    }

    @Test
    public void testGetAndDecrement() {
        final MutableDouble mutNum = new MutableDouble(1d);
        final double result = mutNum.getAndDecrement();

        assertEquals(1d, result, 0.01d);
        assertEquals(0, mutNum.intValue());
        assertEquals(0L, mutNum.longValue());
    }

    @Test
    public void testGetAndIncrement() {
        final MutableDouble mutNum = new MutableDouble(1d);
        final double result = mutNum.getAndIncrement();

        assertEquals(1d, result, 0.01d);
        assertEquals(2, mutNum.intValue());
        assertEquals(2L, mutNum.longValue());
    }

    @Test
    public void testGetSet() {
        final MutableDouble mutNum = new MutableDouble(0d);
        assertEquals(0d, new MutableDouble().doubleValue(), 0.0001d);
        assertEquals(Double.valueOf(0), new MutableDouble().getValue());

        mutNum.setValue(1);
        assertEquals(1d, mutNum.doubleValue(), 0.0001d);
        assertEquals(Double.valueOf(1d), mutNum.getValue());

        mutNum.setValue(Double.valueOf(2d));
        assertEquals(2d, mutNum.doubleValue(), 0.0001d);
        assertEquals(Double.valueOf(2d), mutNum.getValue());

        mutNum.setValue(new MutableDouble(3d));
        assertEquals(3d, mutNum.doubleValue(), 0.0001d);
        assertEquals(Double.valueOf(3d), mutNum.getValue());
    }

    @Test
    public void testHashCode() {
        final MutableDouble mutNumA = new MutableDouble(0d);
        final MutableDouble mutNumB = new MutableDouble(0d);
        final MutableDouble mutNumC = new MutableDouble(1d);

        assertEquals(mutNumA.hashCode(), mutNumA.hashCode());
        assertEquals(mutNumA.hashCode(), mutNumB.hashCode());
        assertNotEquals(mutNumA.hashCode(), mutNumC.hashCode());
        assertEquals(mutNumA.hashCode(), Double.valueOf(0d).hashCode());
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

        assertEquals(2d, result, 0.01d);
        assertEquals(2, mutNum.intValue());
        assertEquals(2L, mutNum.longValue());
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
    public void testPrimitiveValues() {
        final MutableDouble mutNum = new MutableDouble(1.7);
        assertEquals(1.7F, mutNum.floatValue());
        assertEquals(1.7, mutNum.doubleValue());
        assertEquals( (byte) 1, mutNum.byteValue() );
        assertEquals( (short) 1, mutNum.shortValue() );
        assertEquals( 1, mutNum.intValue() );
        assertEquals( 1L, mutNum.longValue() );
    }

    @Test
    public void testSetNull() {
        final MutableDouble mutNum = new MutableDouble(0d);
        assertThrows(NullPointerException.class, () -> mutNum.setValue(null));
    }

    @Test
    public void testSubtractValueObject() {
        final MutableDouble mutNum = new MutableDouble(1);
        mutNum.subtract(Double.valueOf(0.9d));

        assertEquals(0.1d, mutNum.doubleValue(), 0.01d);
    }

    @Test
    public void testSubtractValuePrimitive() {
        final MutableDouble mutNum = new MutableDouble(1);
        mutNum.subtract(0.9d);

        assertEquals(0.1d, mutNum.doubleValue(), 0.01d);
    }

    @Test
    public void testToDouble() {
        assertEquals(Double.valueOf(0d), new MutableDouble(0d).toDouble());
        assertEquals(Double.valueOf(12.3d), new MutableDouble(12.3d).toDouble());
    }

    @Test
    public void testToString() {
        assertEquals("0.0", new MutableDouble(0d).toString());
        assertEquals("10.0", new MutableDouble(10d).toString());
        assertEquals("-123.0", new MutableDouble(-123d).toString());
    }

}
