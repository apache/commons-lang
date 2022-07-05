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

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * JUnit tests.
 *
 * @see MutableLong
 */
public class MutableLongTest extends AbstractLangTest {

    @Test
    public void testAddAndGetValueObject() {
        final MutableLong mutableLong = new MutableLong(0L);
        final long result = mutableLong.addAndGet(Long.valueOf(1L));

        assertEquals(1L, result);
        assertEquals(1L, mutableLong.longValue());
    }

    @Test
    public void testAddAndGetValuePrimitive() {
        final MutableLong mutableLong = new MutableLong(0L);
        final long result = mutableLong.addAndGet(1L);

        assertEquals(1L, result);
        assertEquals(1L, mutableLong.longValue());
    }

    @Test
    public void testAddValueObject() {
        final MutableLong mutNum = new MutableLong(1);
        mutNum.add(Long.valueOf(1));

        assertEquals(2, mutNum.intValue());
        assertEquals(2L, mutNum.longValue());
    }

    @Test
    public void testAddValuePrimitive() {
        final MutableLong mutNum = new MutableLong(1);
        mutNum.add(1);

        assertEquals(2, mutNum.intValue());
        assertEquals(2L, mutNum.longValue());
    }

    @Test
    public void testCompareTo() {
        final MutableLong mutNum = new MutableLong(0);

        assertEquals(0, mutNum.compareTo(new MutableLong(0)));
        assertEquals(+1, mutNum.compareTo(new MutableLong(-1)));
        assertEquals(-1, mutNum.compareTo(new MutableLong(1)));
    }

    @Test
    public void testCompareToNull() {
        final MutableLong mutNum = new MutableLong(0);
        assertThrows(NullPointerException.class, () -> mutNum.compareTo(null));
    }

    @Test
    public void testConstructorNull() {
        assertThrows(NullPointerException.class, () -> new MutableLong((Number) null));
    }

    @Test
    public void testConstructors() {
        assertEquals(0, new MutableLong().longValue());

        assertEquals(1, new MutableLong(1).longValue());

        assertEquals(2, new MutableLong(Long.valueOf(2)).longValue());
        assertEquals(3, new MutableLong(new MutableLong(3)).longValue());

        assertEquals(2, new MutableLong("2").longValue());

    }

    @Test
    public void testDecrement() {
        final MutableLong mutNum = new MutableLong(1);
        mutNum.decrement();

        assertEquals(0, mutNum.intValue());
        assertEquals(0L, mutNum.longValue());
    }

    @Test
    public void testDecrementAndGet() {
        final MutableLong mutNum = new MutableLong(1L);
        final long result = mutNum.decrementAndGet();

        assertEquals(0, result);
        assertEquals(0, mutNum.intValue());
        assertEquals(0L, mutNum.longValue());
    }

    @Test
    public void testEquals() {
        final MutableLong mutNumA = new MutableLong(0);
        final MutableLong mutNumB = new MutableLong(0);
        final MutableLong mutNumC = new MutableLong(1);

        assertEquals(mutNumA, mutNumA);
        assertEquals(mutNumA, mutNumB);
        assertEquals(mutNumB, mutNumA);
        assertEquals(mutNumB, mutNumB);
        assertNotEquals(mutNumA, mutNumC);
        assertNotEquals(mutNumB, mutNumC);
        assertEquals(mutNumC, mutNumC);
        assertNotEquals(null, mutNumA);
        assertNotEquals(mutNumA, Long.valueOf(0));
        assertNotEquals("0", mutNumA);
    }

    @Test
    public void testGetAndAddValueObject() {
        final MutableLong mutableLong = new MutableLong(0L);
        final long result = mutableLong.getAndAdd(Long.valueOf(1L));

        assertEquals(0L, result);
        assertEquals(1L, mutableLong.longValue());
    }

    @Test
    public void testGetAndAddValuePrimitive() {
        final MutableLong mutableLong = new MutableLong(0L);
        final long result = mutableLong.getAndAdd(1L);

        assertEquals(0L, result);
        assertEquals(1L, mutableLong.longValue());
    }

    @Test
    public void testGetAndDecrement() {
        final MutableLong mutNum = new MutableLong(1L);
        final long result = mutNum.getAndDecrement();

        assertEquals(1, result);
        assertEquals(0, mutNum.intValue());
        assertEquals(0L, mutNum.longValue());
    }

    @Test
    public void testGetAndIncrement() {
        final MutableLong mutNum = new MutableLong(1L);
        final long result = mutNum.getAndIncrement();

        assertEquals(1, result);
        assertEquals(2, mutNum.intValue());
        assertEquals(2L, mutNum.longValue());
    }

    @Test
    public void testGetSet() {
        final MutableLong mutNum = new MutableLong(0);
        assertEquals(0, new MutableLong().longValue());
        assertEquals(Long.valueOf(0), new MutableLong().getValue());

        mutNum.setValue(1);
        assertEquals(1, mutNum.longValue());
        assertEquals(Long.valueOf(1), mutNum.getValue());

        mutNum.setValue(Long.valueOf(2));
        assertEquals(2, mutNum.longValue());
        assertEquals(Long.valueOf(2), mutNum.getValue());

        mutNum.setValue(new MutableLong(3));
        assertEquals(3, mutNum.longValue());
        assertEquals(Long.valueOf(3), mutNum.getValue());
    }

    @Test
    public void testHashCode() {
        final MutableLong mutNumA = new MutableLong(0);
        final MutableLong mutNumB = new MutableLong(0);
        final MutableLong mutNumC = new MutableLong(1);

        assertEquals(mutNumA.hashCode(), mutNumA.hashCode());
        assertEquals(mutNumA.hashCode(), mutNumB.hashCode());
        assertNotEquals(mutNumA.hashCode(), mutNumC.hashCode());
        assertEquals(mutNumA.hashCode(), Long.valueOf(0).hashCode());
    }

    @Test
    public void testIncrement() {
        final MutableLong mutNum = new MutableLong(1);
        mutNum.increment();

        assertEquals(2, mutNum.intValue());
        assertEquals(2L, mutNum.longValue());
    }

    @Test
    public void testIncrementAndGet() {
        final MutableLong mutNum = new MutableLong(1L);
        final long result = mutNum.incrementAndGet();

        assertEquals(2, result);
        assertEquals(2, mutNum.intValue());
        assertEquals(2L, mutNum.longValue());
    }

    @Test
    public void testPrimitiveValues() {
        final MutableLong mutNum = new MutableLong(1L);
        assertEquals(1.0F, mutNum.floatValue());
        assertEquals(1.0, mutNum.doubleValue());
        assertEquals( (byte) 1, mutNum.byteValue() );
        assertEquals( (short) 1, mutNum.shortValue() );
        assertEquals( 1, mutNum.intValue() );
        assertEquals( 1L, mutNum.longValue() );
    }

    @Test
    public void testSetNull() {
        final MutableLong mutNum = new MutableLong(0);
        assertThrows(NullPointerException.class, () -> mutNum.setValue(null));
    }

    @Test
    public void testSubtractValueObject() {
        final MutableLong mutNum = new MutableLong(1);
        mutNum.subtract(Long.valueOf(1));

        assertEquals(0, mutNum.intValue());
        assertEquals(0L, mutNum.longValue());
    }

    @Test
    public void testSubtractValuePrimitive() {
        final MutableLong mutNum = new MutableLong(1);
        mutNum.subtract(1);

        assertEquals(0, mutNum.intValue());
        assertEquals(0L, mutNum.longValue());
    }

    @Test
    public void testToLong() {
        assertEquals(Long.valueOf(0L), new MutableLong(0L).toLong());
        assertEquals(Long.valueOf(123L), new MutableLong(123L).toLong());
    }

    @Test
    public void testToString() {
        assertEquals("0", new MutableLong(0).toString());
        assertEquals("10", new MutableLong(10).toString());
        assertEquals("-123", new MutableLong(-123).toString());
    }

}
