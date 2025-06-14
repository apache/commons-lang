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

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * JUnit tests.
 *
 * @see MutableByte
 */
class MutableByteTest extends AbstractLangTest {

    @Test
    void testAddAndGetValueObject() {
        final MutableByte mutableByte = new MutableByte((byte) 0);
        final byte result = mutableByte.addAndGet(Byte.valueOf((byte) 1));

        assertEquals((byte) 1, result);
        assertEquals((byte) 1, mutableByte.byteValue());
    }

    @Test
    void testAddAndGetValuePrimitive() {
        final MutableByte mutableByte = new MutableByte((byte) 0);
        final byte result = mutableByte.addAndGet((byte) 1);

        assertEquals((byte) 1, result);
        assertEquals((byte) 1, mutableByte.byteValue());
    }

    @Test
    void testAddValueObject() {
        final MutableByte mutNum = new MutableByte((byte) 1);
        mutNum.add(Integer.valueOf(1));

        assertEquals((byte) 2, mutNum.byteValue());
    }

    @Test
    void testAddValuePrimitive() {
        final MutableByte mutNum = new MutableByte((byte) 1);
        mutNum.add((byte) 1);

        assertEquals((byte) 2, mutNum.byteValue());
    }

    @Test
    void testCompareTo() {
        final MutableByte mutNum = new MutableByte((byte) 0);

        assertEquals((byte) 0, mutNum.compareTo(new MutableByte((byte) 0)));
        assertEquals((byte) +1, mutNum.compareTo(new MutableByte((byte) -1)));
        assertEquals((byte) -1, mutNum.compareTo(new MutableByte((byte) 1)));
    }

    @Test
    void testCompareToNull() {
        final MutableByte mutNum = new MutableByte((byte) 0);
        assertThrows(NullPointerException.class, () -> mutNum.compareTo(null));
    }

    @Test
    void testConstructorNull() {
        assertThrows(NullPointerException.class, () -> new MutableByte((Number) null));
    }

    @Test
    void testConstructors() {
        assertEquals((byte) 0, new MutableByte().byteValue());

        assertEquals((byte) 1, new MutableByte((byte) 1).byteValue());

        assertEquals((byte) 2, new MutableByte(Byte.valueOf((byte) 2)).byteValue());
        assertEquals((byte) 3, new MutableByte(new MutableByte((byte) 3)).byteValue());

        assertEquals((byte) 2, new MutableByte("2").byteValue());

    }

    @Test
    void testDecrement() {
        final MutableByte mutNum = new MutableByte((byte) 1);
        mutNum.decrement();

        assertEquals(0, mutNum.intValue());
        assertEquals(0L, mutNum.longValue());
    }

    @Test
    void testDecrementAndGet() {
        final MutableByte mutNum = new MutableByte((byte) 1);
        final byte result = mutNum.decrementAndGet();

        assertEquals(0, result);
        assertEquals(0, mutNum.intValue());
        assertEquals(0L, mutNum.longValue());
    }

    @Test
    void testEquals() {
        final MutableByte mutNumA = new MutableByte((byte) 0);
        final MutableByte mutNumB = new MutableByte((byte) 0);
        final MutableByte mutNumC = new MutableByte((byte) 1);

        assertEquals(mutNumA, mutNumA);
        assertEquals(mutNumA, mutNumB);
        assertEquals(mutNumB, mutNumA);
        assertEquals(mutNumB, mutNumB);
        assertNotEquals(mutNumA, mutNumC);
        assertNotEquals(mutNumB, mutNumC);
        assertEquals(mutNumC, mutNumC);
        assertNotEquals(null, mutNumA);
        assertNotEquals(mutNumA, Byte.valueOf((byte) 0));
        assertNotEquals("0", mutNumA);
    }

    @Test
    void testGetAndAddValueObject() {
        final MutableByte mutableByte = new MutableByte((byte) 0);
        final byte result = mutableByte.getAndAdd(Byte.valueOf((byte) 1));

        assertEquals((byte) 0, result);
        assertEquals((byte) 1, mutableByte.byteValue());
    }

    @Test
    void testGetAndAddValuePrimitive() {
        final MutableByte mutableByte = new MutableByte((byte) 0);
        final byte result = mutableByte.getAndAdd((byte) 1);

        assertEquals((byte) 0, result);
        assertEquals((byte) 1, mutableByte.byteValue());
    }

    @Test
    void testGetAndDecrement() {
        final MutableByte mutNum = new MutableByte((byte) 1);
        final byte result = mutNum.getAndDecrement();

        assertEquals(1, result);
        assertEquals(0, mutNum.intValue());
        assertEquals(0L, mutNum.longValue());
    }

    @Test
    void testGetAndIncrement() {
        final MutableByte mutNum = new MutableByte((byte) 1);
        final byte result = mutNum.getAndIncrement();

        assertEquals(1, result);
        assertEquals(2, mutNum.intValue());
        assertEquals(2L, mutNum.longValue());
    }

    @Test
    void testGetSet() {
        final MutableByte mutNum = new MutableByte((byte) 0);
        assertEquals((byte) 0, new MutableByte().byteValue());
        assertEquals(Byte.valueOf((byte) 0), new MutableByte().get());
        assertEquals(Byte.valueOf((byte) 0), new MutableByte().getValue());

        mutNum.setValue((byte) 1);
        assertEquals((byte) 1, mutNum.byteValue());
        assertEquals(Byte.valueOf((byte) 1), mutNum.get());
        assertEquals(Byte.valueOf((byte) 1), mutNum.getValue());

        mutNum.setValue(Byte.valueOf((byte) 2));
        assertEquals((byte) 2, mutNum.byteValue());
        assertEquals(Byte.valueOf((byte) 2), mutNum.get());
        assertEquals(Byte.valueOf((byte) 2), mutNum.getValue());

        mutNum.setValue(new MutableByte((byte) 3));
        assertEquals((byte) 3, mutNum.byteValue());
        assertEquals(Byte.valueOf((byte) 3), mutNum.get());
        assertEquals(Byte.valueOf((byte) 3), mutNum.getValue());
    }

    @Test
    void testHashCode() {
        final MutableByte mutNumA = new MutableByte((byte) 0);
        final MutableByte mutNumB = new MutableByte((byte) 0);
        final MutableByte mutNumC = new MutableByte((byte) 1);

        assertEquals(mutNumA.hashCode(), mutNumA.hashCode());
        assertEquals(mutNumA.hashCode(), mutNumB.hashCode());
        assertNotEquals(mutNumA.hashCode(), mutNumC.hashCode());
        assertEquals(mutNumA.hashCode(), Byte.valueOf((byte) 0).hashCode());
    }

    @Test
    void testIncrement() {
        final MutableByte mutNum = new MutableByte((byte) 1);
        mutNum.increment();

        assertEquals(2, mutNum.intValue());
        assertEquals(2L, mutNum.longValue());
    }

    @Test
    void testIncrementAndGet() {
        final MutableByte mutNum = new MutableByte((byte) 1);
        final byte result = mutNum.incrementAndGet();

        assertEquals(2, result);
        assertEquals(2, mutNum.intValue());
        assertEquals(2L, mutNum.longValue());
    }

    @Test
    void testPrimitiveValues() {
        final MutableByte mutNum = new MutableByte((byte) 1);
        assertEquals(1.0F, mutNum.floatValue());
        assertEquals(1.0, mutNum.doubleValue());
        assertEquals((byte) 1, mutNum.byteValue());
        assertEquals((short) 1, mutNum.shortValue());
        assertEquals(1, mutNum.intValue());
        assertEquals(1L, mutNum.longValue());
    }

    @Test
    void testSetNull() {
        final MutableByte mutNum = new MutableByte((byte) 0);
        assertThrows(NullPointerException.class, () -> mutNum.setValue(null));
    }

    @Test
    void testSubtractValueObject() {
        final MutableByte mutNum = new MutableByte((byte) 1);
        mutNum.subtract(Integer.valueOf(1));

        assertEquals((byte) 0, mutNum.byteValue());
    }

    @Test
    void testSubtractValuePrimitive() {
        final MutableByte mutNum = new MutableByte((byte) 1);
        mutNum.subtract((byte) 1);

        assertEquals((byte) 0, mutNum.byteValue());
    }

    @Test
    void testToByte() {
        assertEquals(Byte.valueOf((byte) 0), new MutableByte((byte) 0).toByte());
        assertEquals(Byte.valueOf((byte) 123), new MutableByte((byte) 123).toByte());
    }

    @Test
    void testToString() {
        assertEquals("0", new MutableByte((byte) 0).toString());
        assertEquals("10", new MutableByte((byte) 10).toString());
        assertEquals("-123", new MutableByte((byte) -123).toString());
    }

}
