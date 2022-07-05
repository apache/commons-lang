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
 * @see MutableByte
 */
public class MutableByteTest extends AbstractLangTest {

    @Test
    public void testAddAndGetValueObject() {
        final MutableByte mutableByte = new MutableByte((byte) 0);
        final byte result = mutableByte.addAndGet(Byte.valueOf((byte) 1));

        assertEquals((byte) 1, result);
        assertEquals((byte) 1, mutableByte.byteValue());
    }

    @Test
    public void testAddAndGetValuePrimitive() {
        final MutableByte mutableByte = new MutableByte((byte) 0);
        final byte result = mutableByte.addAndGet((byte) 1);

        assertEquals((byte) 1, result);
        assertEquals((byte) 1, mutableByte.byteValue());
    }

    @Test
    public void testAddValueObject() {
        final MutableByte mutNum = new MutableByte((byte) 1);
        mutNum.add(Integer.valueOf(1));

        assertEquals((byte) 2, mutNum.byteValue());
    }

    @Test
    public void testAddValuePrimitive() {
        final MutableByte mutNum = new MutableByte((byte) 1);
        mutNum.add((byte) 1);

        assertEquals((byte) 2, mutNum.byteValue());
    }

    @Test
    public void testCompareTo() {
        final MutableByte mutNum = new MutableByte((byte) 0);

        assertEquals((byte) 0, mutNum.compareTo(new MutableByte((byte) 0)));
        assertEquals((byte) +1, mutNum.compareTo(new MutableByte((byte) -1)));
        assertEquals((byte) -1, mutNum.compareTo(new MutableByte((byte) 1)));
    }

    @Test
    public void testCompareToNull() {
        final MutableByte mutNum = new MutableByte((byte) 0);
        assertThrows(NullPointerException.class, () -> mutNum.compareTo(null));
    }

    @Test
    public void testConstructorNull() {
        assertThrows(NullPointerException.class, () -> new MutableByte((Number) null));
    }

    @Test
    public void testConstructors() {
        assertEquals((byte) 0, new MutableByte().byteValue());

        assertEquals((byte) 1, new MutableByte((byte) 1).byteValue());

        assertEquals((byte) 2, new MutableByte(Byte.valueOf((byte) 2)).byteValue());
        assertEquals((byte) 3, new MutableByte(new MutableByte((byte) 3)).byteValue());

        assertEquals((byte) 2, new MutableByte("2").byteValue());

    }

    @Test
    public void testDecrement() {
        final MutableByte mutNum = new MutableByte((byte) 1);
        mutNum.decrement();

        assertEquals(0, mutNum.intValue());
        assertEquals(0L, mutNum.longValue());
    }

    @Test
    public void testDecrementAndGet() {
        final MutableByte mutNum = new MutableByte((byte) 1);
        final byte result = mutNum.decrementAndGet();

        assertEquals(0, result);
        assertEquals(0, mutNum.intValue());
        assertEquals(0L, mutNum.longValue());
    }

    @Test
    public void testEquals() {
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
    public void testGetAndAddValueObject() {
        final MutableByte mutableByte = new MutableByte((byte) 0);
        final byte result = mutableByte.getAndAdd(Byte.valueOf((byte) 1));

        assertEquals((byte) 0, result);
        assertEquals((byte) 1, mutableByte.byteValue());
    }

    @Test
    public void testGetAndAddValuePrimitive() {
        final MutableByte mutableByte = new MutableByte((byte) 0);
        final byte result = mutableByte.getAndAdd((byte) 1);

        assertEquals((byte) 0, result);
        assertEquals((byte) 1, mutableByte.byteValue());
    }

    @Test
    public void testGetAndDecrement() {
        final MutableByte mutNum = new MutableByte((byte) 1);
        final byte result = mutNum.getAndDecrement();

        assertEquals(1, result);
        assertEquals(0, mutNum.intValue());
        assertEquals(0L, mutNum.longValue());
    }

    @Test
    public void testGetAndIncrement() {
        final MutableByte mutNum = new MutableByte((byte) 1);
        final byte result = mutNum.getAndIncrement();

        assertEquals(1, result);
        assertEquals(2, mutNum.intValue());
        assertEquals(2L, mutNum.longValue());
    }

    @Test
    public void testGetSet() {
        final MutableByte mutNum = new MutableByte((byte) 0);
        assertEquals((byte) 0, new MutableByte().byteValue());
        assertEquals(Byte.valueOf((byte) 0), new MutableByte().getValue());

        mutNum.setValue((byte) 1);
        assertEquals((byte) 1, mutNum.byteValue());
        assertEquals(Byte.valueOf((byte) 1), mutNum.getValue());

        mutNum.setValue(Byte.valueOf((byte) 2));
        assertEquals((byte) 2, mutNum.byteValue());
        assertEquals(Byte.valueOf((byte) 2), mutNum.getValue());

        mutNum.setValue(new MutableByte((byte) 3));
        assertEquals((byte) 3, mutNum.byteValue());
        assertEquals(Byte.valueOf((byte) 3), mutNum.getValue());
    }

    @Test
    public void testHashCode() {
        final MutableByte mutNumA = new MutableByte((byte) 0);
        final MutableByte mutNumB = new MutableByte((byte) 0);
        final MutableByte mutNumC = new MutableByte((byte) 1);

        assertEquals(mutNumA.hashCode(), mutNumA.hashCode());
        assertEquals(mutNumA.hashCode(), mutNumB.hashCode());
        assertNotEquals(mutNumA.hashCode(), mutNumC.hashCode());
        assertEquals(mutNumA.hashCode(), Byte.valueOf((byte) 0).hashCode());
    }

    @Test
    public void testIncrement() {
        final MutableByte mutNum = new MutableByte((byte) 1);
        mutNum.increment();

        assertEquals(2, mutNum.intValue());
        assertEquals(2L, mutNum.longValue());
    }

    @Test
    public void testIncrementAndGet() {
        final MutableByte mutNum = new MutableByte((byte) 1);
        final byte result = mutNum.incrementAndGet();

        assertEquals(2, result);
        assertEquals(2, mutNum.intValue());
        assertEquals(2L, mutNum.longValue());
    }

    @Test
    public void testPrimitiveValues() {
        final MutableByte mutNum = new MutableByte( (byte) 1 );
        assertEquals(1.0F, mutNum.floatValue());
        assertEquals(1.0, mutNum.doubleValue());
        assertEquals( (byte) 1, mutNum.byteValue() );
        assertEquals( (short) 1, mutNum.shortValue() );
        assertEquals( 1, mutNum.intValue() );
        assertEquals( 1L, mutNum.longValue() );
    }

    @Test
    public void testSetNull() {
        final MutableByte mutNum = new MutableByte((byte) 0);
        assertThrows(NullPointerException.class, () -> mutNum.setValue(null));
    }

    @Test
    public void testSubtractValueObject() {
        final MutableByte mutNum = new MutableByte((byte) 1);
        mutNum.subtract(Integer.valueOf(1));

        assertEquals((byte) 0, mutNum.byteValue());
    }

    @Test
    public void testSubtractValuePrimitive() {
        final MutableByte mutNum = new MutableByte((byte) 1);
        mutNum.subtract((byte) 1);

        assertEquals((byte) 0, mutNum.byteValue());
    }

    @Test
    public void testToByte() {
        assertEquals(Byte.valueOf((byte) 0), new MutableByte((byte) 0).toByte());
        assertEquals(Byte.valueOf((byte) 123), new MutableByte((byte) 123).toByte());
    }

    @Test
    public void testToString() {
        assertEquals("0", new MutableByte((byte) 0).toString());
        assertEquals("10", new MutableByte((byte) 10).toString());
        assertEquals("-123", new MutableByte((byte) -123).toString());
    }

}
