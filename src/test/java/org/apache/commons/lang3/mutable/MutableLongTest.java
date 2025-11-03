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

import static org.apache.commons.lang3.LangAssertions.assertNullPointerException;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * JUnit tests.
 *
 * @see MutableLong
 */
class MutableLongTest extends AbstractLangTest {

    @Test
    void testAddAndGetValueObject() {
        final MutableLong mutableLong = new MutableLong(0L);
        final long result = mutableLong.addAndGet(Long.valueOf(1L));

        assertEquals(1L, result);
        assertEquals(1L, mutableLong.longValue());
    }

    @Test
    void testAddAndGetValuePrimitive() {
        final MutableLong mutableLong = new MutableLong(0L);
        final long result = mutableLong.addAndGet(1L);

        assertEquals(1L, result);
        assertEquals(1L, mutableLong.longValue());
    }

    @Test
    void testAddValueObject() {
        final MutableLong mutNum = new MutableLong(1);
        mutNum.add(Long.valueOf(1));

        assertEquals(2, mutNum.intValue());
        assertEquals(2L, mutNum.longValue());
    }

    @Test
    void testAddValuePrimitive() {
        final MutableLong mutNum = new MutableLong(1);
        mutNum.add(1);

        assertEquals(2, mutNum.intValue());
        assertEquals(2L, mutNum.longValue());
    }

    @Test
    void testCompareTo() {
        final MutableLong mutNum = new MutableLong(0);

        assertEquals(0, mutNum.compareTo(new MutableLong(0)));
        assertEquals(+1, mutNum.compareTo(new MutableLong(-1)));
        assertEquals(-1, mutNum.compareTo(new MutableLong(1)));
    }

    @Test
    void testCompareToNull() {
        final MutableLong mutNum = new MutableLong(0);
        assertNullPointerException(() -> mutNum.compareTo(null));
    }

    @Test
    void testConstructorNull() {
        assertNullPointerException(() -> new MutableLong((Number) null));
    }

    @Test
    void testConstructors() {
        assertEquals(0, new MutableLong().longValue());

        assertEquals(1, new MutableLong(1).longValue());

        assertEquals(2, new MutableLong(Long.valueOf(2)).longValue());
        assertEquals(3, new MutableLong(new MutableLong(3)).longValue());

        assertEquals(2, new MutableLong("2").longValue());

    }

    @Test
    void testDecrement() {
        final MutableLong mutNum = new MutableLong(1);
        mutNum.decrement();

        assertEquals(0, mutNum.intValue());
        assertEquals(0L, mutNum.longValue());
    }

    @Test
    void testDecrementAndGet() {
        final MutableLong mutNum = new MutableLong(1L);
        final long result = mutNum.decrementAndGet();

        assertEquals(0, result);
        assertEquals(0, mutNum.intValue());
        assertEquals(0L, mutNum.longValue());
    }

    @Test
    void testEquals() {
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
    void testGetAndAddValueObject() {
        final MutableLong mutableLong = new MutableLong(0L);
        final long result = mutableLong.getAndAdd(Long.valueOf(1L));

        assertEquals(0L, result);
        assertEquals(1L, mutableLong.longValue());
    }

    @Test
    void testGetAndAddValuePrimitive() {
        final MutableLong mutableLong = new MutableLong(0L);
        final long result = mutableLong.getAndAdd(1L);

        assertEquals(0L, result);
        assertEquals(1L, mutableLong.longValue());
    }

    @Test
    void testGetAndDecrement() {
        final MutableLong mutNum = new MutableLong(1L);
        final long result = mutNum.getAndDecrement();

        assertEquals(1, result);
        assertEquals(0, mutNum.intValue());
        assertEquals(0L, mutNum.longValue());
    }

    @Test
    void testGetAndIncrement() {
        final MutableLong mutNum = new MutableLong(1L);
        final long result = mutNum.getAndIncrement();

        assertEquals(1, result);
        assertEquals(2, mutNum.intValue());
        assertEquals(2L, mutNum.longValue());
    }

    @Test
    void testGetSet() {
        final MutableLong mutNum = new MutableLong(0);
        assertEquals(0, new MutableLong().longValue());
        assertEquals(Long.valueOf(0), new MutableLong().get());
        assertEquals(Long.valueOf(0), new MutableLong().getValue());

        mutNum.setValue(1);
        assertEquals(1, mutNum.longValue());
        assertEquals(Long.valueOf(1), mutNum.get());
        assertEquals(Long.valueOf(1), mutNum.getValue());

        mutNum.setValue(Long.valueOf(2));
        assertEquals(2, mutNum.longValue());
        assertEquals(Long.valueOf(2), mutNum.get());
        assertEquals(Long.valueOf(2), mutNum.getValue());

        mutNum.setValue(new MutableLong(3));
        assertEquals(3, mutNum.longValue());
        assertEquals(Long.valueOf(3), mutNum.get());
        assertEquals(Long.valueOf(3), mutNum.getValue());
    }

    @Test
    void testHashCode() {
        final MutableLong mutNumA = new MutableLong(0);
        final MutableLong mutNumB = new MutableLong(0);
        final MutableLong mutNumC = new MutableLong(1);

        assertEquals(mutNumA.hashCode(), mutNumA.hashCode());
        assertEquals(mutNumA.hashCode(), mutNumB.hashCode());
        assertNotEquals(mutNumA.hashCode(), mutNumC.hashCode());
        assertEquals(mutNumA.hashCode(), Long.valueOf(0).hashCode());
    }

    @Test
    void testIncrement() {
        final MutableLong mutNum = new MutableLong(1);
        mutNum.increment();

        assertEquals(2, mutNum.intValue());
        assertEquals(2L, mutNum.longValue());
    }

    @Test
    void testIncrementAndGet() {
        final MutableLong mutNum = new MutableLong(1L);
        final long result = mutNum.incrementAndGet();

        assertEquals(2, result);
        assertEquals(2, mutNum.intValue());
        assertEquals(2L, mutNum.longValue());
    }

    @Test
    void testPrimitiveValues() {
        final MutableLong mutNum = new MutableLong(1L);
        assertEquals(1.0F, mutNum.floatValue());
        assertEquals(1.0, mutNum.doubleValue());
        assertEquals((byte) 1, mutNum.byteValue());
        assertEquals((short) 1, mutNum.shortValue());
        assertEquals(1, mutNum.intValue());
        assertEquals(1L, mutNum.longValue());
    }

    @Test
    void testSetNull() {
        final MutableLong mutNum = new MutableLong(0);
        assertNullPointerException(() -> mutNum.setValue(null));
    }

    @Test
    void testSubtractValueObject() {
        final MutableLong mutNum = new MutableLong(1);
        mutNum.subtract(Long.valueOf(1));

        assertEquals(0, mutNum.intValue());
        assertEquals(0L, mutNum.longValue());
    }

    @Test
    void testSubtractValuePrimitive() {
        final MutableLong mutNum = new MutableLong(1);
        mutNum.subtract(1);

        assertEquals(0, mutNum.intValue());
        assertEquals(0L, mutNum.longValue());
    }

    @Test
    void testToLong() {
        assertEquals(Long.valueOf(0L), new MutableLong(0L).toLong());
        assertEquals(Long.valueOf(123L), new MutableLong(123L).toLong());
    }

    @Test
    void testToString() {
        assertEquals("0", new MutableLong(0).toString());
        assertEquals("10", new MutableLong(10).toString());
        assertEquals("-123", new MutableLong(-123).toString());
    }

}
