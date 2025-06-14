/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
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
 * @see MutableFloat
 */
class MutableFloatTest extends AbstractLangTest {

    @Test
    void testAddAndGetValueObject() {
        final MutableFloat mutableFloat = new MutableFloat(5f);
        final float result = mutableFloat.addAndGet(Float.valueOf(2.5f));

        assertEquals(7.5f, result, 0.01f);
        assertEquals(7.5f, mutableFloat.floatValue(), 0.01f);
    }

    @Test
    void testAddAndGetValuePrimitive() {
        final MutableFloat mutableFloat = new MutableFloat(0.5f);
        final float result = mutableFloat.addAndGet(1f);

        assertEquals(1.5f, result, 0.01f);
        assertEquals(1.5f, mutableFloat.floatValue(), 0.01f);
    }

    @Test
    void testAddValueObject() {
        final MutableFloat mutNum = new MutableFloat(1);
        mutNum.add(Float.valueOf(1.1f));

        assertEquals(2.1f, mutNum.floatValue(), 0.01f);
    }

    @Test
    void testAddValuePrimitive() {
        final MutableFloat mutNum = new MutableFloat(1);
        mutNum.add(1.1f);

        assertEquals(2.1f, mutNum.floatValue(), 0.01f);
    }

    @Test
    void testCompareTo() {
        final MutableFloat mutNum = new MutableFloat(0f);

        assertEquals(0, mutNum.compareTo(new MutableFloat(0f)));
        assertEquals(+1, mutNum.compareTo(new MutableFloat(-1f)));
        assertEquals(-1, mutNum.compareTo(new MutableFloat(1f)));
    }

    @Test
    void testCompareToNull() {
        final MutableFloat mutNum = new MutableFloat(0f);
        assertThrows(NullPointerException.class, () -> mutNum.compareTo(null));
    }

    @Test
    void testConstructorNull() {
        assertThrows(NullPointerException.class, () -> new MutableFloat((Number) null));
    }

    @Test
    void testConstructors() {
        assertEquals(0f, new MutableFloat().floatValue(), 0.0001f);

        assertEquals(1f, new MutableFloat(1f).floatValue(), 0.0001f);

        assertEquals(2f, new MutableFloat(Float.valueOf(2f)).floatValue(), 0.0001f);
        assertEquals(3f, new MutableFloat(new MutableFloat(3f)).floatValue(), 0.0001f);

        assertEquals(2f, new MutableFloat("2.0").floatValue(), 0.0001f);

    }

    @Test
    void testDecrement() {
        final MutableFloat mutNum = new MutableFloat(1);
        mutNum.decrement();

        assertEquals(0, mutNum.intValue());
        assertEquals(0L, mutNum.longValue());
    }

    @Test
    void testDecrementAndGet() {
        final MutableFloat mutNum = new MutableFloat(1f);
        final float result = mutNum.decrementAndGet();

        assertEquals(0f, result, 0.01f);
        assertEquals(0, mutNum.intValue());
        assertEquals(0L, mutNum.longValue());
    }

    @Test
    void testEquals() {
        final MutableFloat mutNumA = new MutableFloat(0f);
        final MutableFloat mutNumB = new MutableFloat(0f);
        final MutableFloat mutNumC = new MutableFloat(1f);

        assertEquals(mutNumA, mutNumA);
        assertEquals(mutNumA, mutNumB);
        assertEquals(mutNumB, mutNumA);
        assertEquals(mutNumB, mutNumB);
        assertNotEquals(mutNumA, mutNumC);
        assertNotEquals(mutNumB, mutNumC);
        assertEquals(mutNumC, mutNumC);
        assertNotEquals(null, mutNumA);
        assertNotEquals(mutNumA, Float.valueOf(0f));
        assertNotEquals("0", mutNumA);
    }

    @Test
    void testGetAndAddValueObject() {
        final MutableFloat mutableFloat = new MutableFloat(7.75f);
        final float result = mutableFloat.getAndAdd(Float.valueOf(2.25f));

        assertEquals(7.75f, result, 0.01f);
        assertEquals(10f, mutableFloat.floatValue(), 0.01f);
    }

    @Test
    void testGetAndAddValuePrimitive() {
        final MutableFloat mutableFloat = new MutableFloat(1.25f);
        final float result = mutableFloat.getAndAdd(0.75f);

        assertEquals(1.25f, result, 0.01f);
        assertEquals(2f, mutableFloat.floatValue(), 0.01f);
    }

    @Test
    void testGetAndDecrement() {
        final MutableFloat mutNum = new MutableFloat(1f);
        final float result = mutNum.getAndDecrement();

        assertEquals(1f, result, 0.01f);
        assertEquals(0, mutNum.intValue());
        assertEquals(0L, mutNum.longValue());
    }

    @Test
    void testGetAndIncrement() {
        final MutableFloat mutNum = new MutableFloat(1f);
        final float result = mutNum.getAndIncrement();

        assertEquals(1f, result, 0.01f);
        assertEquals(2, mutNum.intValue());
        assertEquals(2L, mutNum.longValue());
    }

    @Test
    void testGetSet() {
        final MutableFloat mutNum = new MutableFloat(0f);
        assertEquals(0f, new MutableFloat().floatValue(), 0.0001f);
        assertEquals(Float.valueOf(0), new MutableFloat().get());
        assertEquals(Float.valueOf(0), new MutableFloat().getValue());

        mutNum.setValue(1);
        assertEquals(1f, mutNum.floatValue(), 0.0001f);
        assertEquals(Float.valueOf(1f), mutNum.get());
        assertEquals(Float.valueOf(1f), mutNum.getValue());

        mutNum.setValue(Float.valueOf(2f));
        assertEquals(2f, mutNum.floatValue(), 0.0001f);
        assertEquals(Float.valueOf(2f), mutNum.get());
        assertEquals(Float.valueOf(2f), mutNum.getValue());

        mutNum.setValue(new MutableFloat(3f));
        assertEquals(3f, mutNum.floatValue(), 0.0001f);
        assertEquals(Float.valueOf(3f), mutNum.get());
        assertEquals(Float.valueOf(3f), mutNum.getValue());
    }

    @Test
    void testHashCode() {
        final MutableFloat mutNumA = new MutableFloat(0f);
        final MutableFloat mutNumB = new MutableFloat(0f);
        final MutableFloat mutNumC = new MutableFloat(1f);

        assertEquals(mutNumA.hashCode(), mutNumA.hashCode());
        assertEquals(mutNumA.hashCode(), mutNumB.hashCode());
        assertNotEquals(mutNumA.hashCode(), mutNumC.hashCode());
        assertEquals(mutNumA.hashCode(), Float.valueOf(0f).hashCode());
    }

    @Test
    void testIncrement() {
        final MutableFloat mutNum = new MutableFloat(1);
        mutNum.increment();

        assertEquals(2, mutNum.intValue());
        assertEquals(2L, mutNum.longValue());
    }

    @Test
    void testIncrementAndGet() {
        final MutableFloat mutNum = new MutableFloat(1f);
        final float result = mutNum.incrementAndGet();

        assertEquals(2f, result, 0.01f);
        assertEquals(2, mutNum.intValue());
        assertEquals(2L, mutNum.longValue());
    }

    @Test
    void testNanInfinite() {
        MutableFloat mutNum = new MutableFloat(Float.NaN);
        assertTrue(mutNum.isNaN());

        mutNum = new MutableFloat(Float.POSITIVE_INFINITY);
        assertTrue(mutNum.isInfinite());

        mutNum = new MutableFloat(Float.NEGATIVE_INFINITY);
        assertTrue(mutNum.isInfinite());
    }

    @Test
    void testPrimitiveValues() {
        final MutableFloat mutNum = new MutableFloat(1.7F);

        assertEquals(1, mutNum.intValue());
        assertEquals(1.7, mutNum.doubleValue(), 0.00001);
        assertEquals((byte) 1, mutNum.byteValue());
        assertEquals((short) 1, mutNum.shortValue());
        assertEquals(1, mutNum.intValue());
        assertEquals(1L, mutNum.longValue());
    }

    @Test
    void testSetNull() {
        final MutableFloat mutNum = new MutableFloat(0f);
        assertThrows(NullPointerException.class, () -> mutNum.setValue(null));
    }

    @Test
    void testSubtractValueObject() {
        final MutableFloat mutNum = new MutableFloat(1);
        mutNum.subtract(Float.valueOf(0.9f));

        assertEquals(0.1f, mutNum.floatValue(), 0.01f);
    }

    @Test
    void testSubtractValuePrimitive() {
        final MutableFloat mutNum = new MutableFloat(1);
        mutNum.subtract(0.9f);

        assertEquals(0.1f, mutNum.floatValue(), 0.01f);
    }

    @Test
    void testToFloat() {
        assertEquals(Float.valueOf(0f), new MutableFloat(0f).toFloat());
        assertEquals(Float.valueOf(12.3f), new MutableFloat(12.3f).toFloat());
    }

    @Test
    void testToString() {
        assertEquals("0.0", new MutableFloat(0f).toString());
        assertEquals("10.0", new MutableFloat(10f).toString());
        assertEquals("-123.0", new MutableFloat(-123f).toString());
    }

}
