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
 * @see MutableByte
 */
public class MutableByteTest {

    // ----------------------------------------------------------------
    @Test
    public void testConstructors() {
        assertEquals((byte) 0, new MutableByte().byteValue());
        
        assertEquals((byte) 1, new MutableByte((byte) 1).byteValue());
        
        assertEquals((byte) 2, new MutableByte(Byte.valueOf((byte) 2)).byteValue());
        assertEquals((byte) 3, new MutableByte(new MutableByte((byte) 3)).byteValue());

        assertEquals((byte) 2, new MutableByte("2").byteValue());

    }

    @Test(expected=NullPointerException.class)
    public void testConstructorNull() {
        new MutableByte((Number)null);
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

    @Test(expected=NullPointerException.class)
    public void testSetNull() {
        final MutableByte mutNum = new MutableByte((byte) 0);
        mutNum.setValue(null);
    }

    @Test
    public void testEquals() {
        final MutableByte mutNumA = new MutableByte((byte) 0);
        final MutableByte mutNumB = new MutableByte((byte) 0);
        final MutableByte mutNumC = new MutableByte((byte) 1);

        assertTrue(mutNumA.equals(mutNumA));
        assertTrue(mutNumA.equals(mutNumB));
        assertTrue(mutNumB.equals(mutNumA));
        assertTrue(mutNumB.equals(mutNumB));
        assertFalse(mutNumA.equals(mutNumC));
        assertFalse(mutNumB.equals(mutNumC));
        assertTrue(mutNumC.equals(mutNumC));
        assertFalse(mutNumA.equals(null));
        assertFalse(mutNumA.equals(Byte.valueOf((byte) 0)));
        assertFalse(mutNumA.equals("0"));
    }

    @Test
    public void testHashCode() {
        final MutableByte mutNumA = new MutableByte((byte) 0);
        final MutableByte mutNumB = new MutableByte((byte) 0);
        final MutableByte mutNumC = new MutableByte((byte) 1);

        assertTrue(mutNumA.hashCode() == mutNumA.hashCode());
        assertTrue(mutNumA.hashCode() == mutNumB.hashCode());
        assertFalse(mutNumA.hashCode() == mutNumC.hashCode());
        assertTrue(mutNumA.hashCode() == Byte.valueOf((byte) 0).hashCode());
    }

    @Test
    public void testCompareTo() {
        final MutableByte mutNum = new MutableByte((byte) 0);

        assertEquals((byte) 0, mutNum.compareTo(new MutableByte((byte) 0)));
        assertEquals((byte) +1, mutNum.compareTo(new MutableByte((byte) -1)));
        assertEquals((byte) -1, mutNum.compareTo(new MutableByte((byte) 1)));
    }

    @Test(expected=NullPointerException.class)
    public void testCompareToNull() {
        final MutableByte mutNum = new MutableByte((byte) 0);
        mutNum.compareTo(null);
    }

    @Test
    public void testPrimitiveValues() {
        MutableByte mutNum = new MutableByte( (byte) 1 );
        
        assertEquals( 1.0F, mutNum.floatValue(), 0 );
        assertEquals( 1.0, mutNum.doubleValue(), 0 );
        assertEquals( (byte) 1, mutNum.byteValue() );
        assertEquals( (short) 1, mutNum.shortValue() );
        assertEquals( 1, mutNum.intValue() );
        assertEquals( 1L, mutNum.longValue() );
    }

    @Test
    public void testToByte() {
        assertEquals(Byte.valueOf((byte) 0), new MutableByte((byte) 0).toByte());
        assertEquals(Byte.valueOf((byte) 123), new MutableByte((byte) 123).toByte());
    }

    @Test
    public void testIncrement() {
        MutableByte mutNum = new MutableByte((byte) 1);
        mutNum.increment();
        
        assertEquals(2, mutNum.intValue());
        assertEquals(2L, mutNum.longValue());
    }

    @Test
    public void testDecrement() {
        MutableByte mutNum = new MutableByte((byte) 1);
        mutNum.decrement();
        
        assertEquals(0, mutNum.intValue());
        assertEquals(0L, mutNum.longValue());
    }

    @Test
    public void testAddValuePrimitive() {
        MutableByte mutNum = new MutableByte((byte) 1);
        mutNum.add((byte)1);
        
        assertEquals((byte) 2, mutNum.byteValue());
    }

    @Test
    public void testAddValueObject() {
        MutableByte mutNum = new MutableByte((byte) 1);
        mutNum.add(Integer.valueOf(1));
        
        assertEquals((byte) 2, mutNum.byteValue());
    }

    @Test
    public void testSubtractValuePrimitive() {
        MutableByte mutNum = new MutableByte((byte) 1);
        mutNum.subtract((byte) 1);
        
        assertEquals((byte) 0, mutNum.byteValue());
    }

    @Test
    public void testSubtractValueObject() {
        MutableByte mutNum = new MutableByte((byte) 1);
        mutNum.subtract(Integer.valueOf(1));
        
        assertEquals((byte) 0, mutNum.byteValue());
    }

    @Test
    public void testToString() {
        assertEquals("0", new MutableByte((byte) 0).toString());
        assertEquals("10", new MutableByte((byte) 10).toString());
        assertEquals("-123", new MutableByte((byte) -123).toString());
    }

}
