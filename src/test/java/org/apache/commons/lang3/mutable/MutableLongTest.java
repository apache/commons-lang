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
 * @see MutableLong
 */
public class MutableLongTest {

    // ----------------------------------------------------------------
    @Test
    public void testConstructors() {
        assertEquals(0, new MutableLong().longValue());
        
        assertEquals(1, new MutableLong(1).longValue());
        
        assertEquals(2, new MutableLong(Long.valueOf(2)).longValue());
        assertEquals(3, new MutableLong(new MutableLong(3)).longValue());

        assertEquals(2, new MutableLong("2").longValue());

    }

    @Test(expected=NullPointerException.class)
    public void testConstructorNull() {
        new MutableLong((Number)null);
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

    @Test(expected=NullPointerException.class)
    public void testSetNull() {
        final MutableLong mutNum = new MutableLong(0);
        mutNum.setValue(null);
    }

    @Test
    public void testEquals() {
        final MutableLong mutNumA = new MutableLong(0);
        final MutableLong mutNumB = new MutableLong(0);
        final MutableLong mutNumC = new MutableLong(1);

        assertTrue(mutNumA.equals(mutNumA));
        assertTrue(mutNumA.equals(mutNumB));
        assertTrue(mutNumB.equals(mutNumA));
        assertTrue(mutNumB.equals(mutNumB));
        assertFalse(mutNumA.equals(mutNumC));
        assertFalse(mutNumB.equals(mutNumC));
        assertTrue(mutNumC.equals(mutNumC));
        assertFalse(mutNumA.equals(null));
        assertFalse(mutNumA.equals(Long.valueOf(0)));
        assertFalse(mutNumA.equals("0"));
    }

    @Test
    public void testHashCode() {
        final MutableLong mutNumA = new MutableLong(0);
        final MutableLong mutNumB = new MutableLong(0);
        final MutableLong mutNumC = new MutableLong(1);

        assertTrue(mutNumA.hashCode() == mutNumA.hashCode());
        assertTrue(mutNumA.hashCode() == mutNumB.hashCode());
        assertFalse(mutNumA.hashCode() == mutNumC.hashCode());
        assertTrue(mutNumA.hashCode() == Long.valueOf(0).hashCode());
    }

    @Test
    public void testCompareTo() {
        final MutableLong mutNum = new MutableLong(0);

        assertEquals(0, mutNum.compareTo(new MutableLong(0)));
        assertEquals(+1, mutNum.compareTo(new MutableLong(-1)));
        assertEquals(-1, mutNum.compareTo(new MutableLong(1)));
    }

    @Test(expected=NullPointerException.class)
    public void testCompareToNull() {
        final MutableLong mutNum = new MutableLong(0);
        mutNum.compareTo(null);
    }

    @Test
    public void testPrimitiveValues() {
        MutableLong mutNum = new MutableLong(1L);

        assertEquals( 1.0F, mutNum.floatValue(), 0 );
        assertEquals( 1.0, mutNum.doubleValue(), 0 );
        assertEquals( (byte) 1, mutNum.byteValue() );
        assertEquals( (short) 1, mutNum.shortValue() );
        assertEquals( 1, mutNum.intValue() );
        assertEquals( 1L, mutNum.longValue() );
    }

    @Test
    public void testToLong() {
        assertEquals(Long.valueOf(0L), new MutableLong(0L).toLong());
        assertEquals(Long.valueOf(123L), new MutableLong(123L).toLong());
    }

    @Test
    public void testIncrement() {
        MutableLong mutNum = new MutableLong(1);
        mutNum.increment();
        
        assertEquals(2, mutNum.intValue());
        assertEquals(2L, mutNum.longValue());
    }

    @Test
    public void testDecrement() {
        MutableLong mutNum = new MutableLong(1);
        mutNum.decrement();
        
        assertEquals(0, mutNum.intValue());
        assertEquals(0L, mutNum.longValue());
    }

    @Test
    public void testAddValuePrimitive() {
        MutableLong mutNum = new MutableLong(1);
        mutNum.add(1);
        
        assertEquals(2, mutNum.intValue());
        assertEquals(2L, mutNum.longValue());
    }

    @Test
    public void testAddValueObject() {
        MutableLong mutNum = new MutableLong(1);
        mutNum.add(Long.valueOf(1));
        
        assertEquals(2, mutNum.intValue());
        assertEquals(2L, mutNum.longValue());
    }

    @Test
    public void testSubtractValuePrimitive() {
        MutableLong mutNum = new MutableLong(1);
        mutNum.subtract(1);
        
        assertEquals(0, mutNum.intValue());
        assertEquals(0L, mutNum.longValue());
    }

    @Test
    public void testSubtractValueObject() {
        MutableLong mutNum = new MutableLong(1);
        mutNum.subtract(Long.valueOf(1));
        
        assertEquals(0, mutNum.intValue());
        assertEquals(0L, mutNum.longValue());
    }

    @Test
    public void testToString() {
        assertEquals("0", new MutableLong(0).toString());
        assertEquals("10", new MutableLong(10).toString());
        assertEquals("-123", new MutableLong(-123).toString());
    }

}
