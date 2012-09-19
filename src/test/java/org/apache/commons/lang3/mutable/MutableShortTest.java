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
 * @see MutableShort
 */
public class MutableShortTest {

    // ----------------------------------------------------------------
    @Test
    public void testConstructors() {
        assertEquals((short) 0, new MutableShort().shortValue());
        
        assertEquals((short) 1, new MutableShort((short) 1).shortValue());
        
        assertEquals((short) 2, new MutableShort(Short.valueOf((short) 2)).shortValue());
        assertEquals((short) 3, new MutableShort(new MutableShort((short) 3)).shortValue());

        assertEquals((short) 2, new MutableShort("2").shortValue());

        try {
            new MutableShort((Number)null);
            fail();
        } catch (NullPointerException ex) {}
    }

    @Test
    public void testGetSet() {
        final MutableShort mutNum = new MutableShort((short) 0);
        assertEquals((short) 0, new MutableShort().shortValue());
        assertEquals(Short.valueOf((short) 0), new MutableShort().getValue());
        
        mutNum.setValue((short) 1);
        assertEquals((short) 1, mutNum.shortValue());
        assertEquals(Short.valueOf((short) 1), mutNum.getValue());
        
        mutNum.setValue(Short.valueOf((short) 2));
        assertEquals((short) 2, mutNum.shortValue());
        assertEquals(Short.valueOf((short) 2), mutNum.getValue());
        
        mutNum.setValue(new MutableShort((short) 3));
        assertEquals((short) 3, mutNum.shortValue());
        assertEquals(Short.valueOf((short) 3), mutNum.getValue());
        try {
            mutNum.setValue(null);
            fail();
        } catch (NullPointerException ex) {}
    }

    @Test
    public void testEquals() {
        final MutableShort mutNumA = new MutableShort((short) 0);
        final MutableShort mutNumB = new MutableShort((short) 0);
        final MutableShort mutNumC = new MutableShort((short) 1);

        assertTrue(mutNumA.equals(mutNumA));
        assertTrue(mutNumA.equals(mutNumB));
        assertTrue(mutNumB.equals(mutNumA));
        assertTrue(mutNumB.equals(mutNumB));
        assertFalse(mutNumA.equals(mutNumC));
        assertFalse(mutNumB.equals(mutNumC));
        assertTrue(mutNumC.equals(mutNumC));
        assertFalse(mutNumA.equals(null));
        assertFalse(mutNumA.equals(Short.valueOf((short) 0)));
        assertFalse(mutNumA.equals("0"));
    }

    @Test
    public void testHashCode() {
        final MutableShort mutNumA = new MutableShort((short) 0);
        final MutableShort mutNumB = new MutableShort((short) 0);
        final MutableShort mutNumC = new MutableShort((short) 1);

        assertTrue(mutNumA.hashCode() == mutNumA.hashCode());
        assertTrue(mutNumA.hashCode() == mutNumB.hashCode());
        assertFalse(mutNumA.hashCode() == mutNumC.hashCode());
        assertTrue(mutNumA.hashCode() == Short.valueOf((short) 0).hashCode());
    }

    @Test
    public void testCompareTo() {
        final MutableShort mutNum = new MutableShort((short) 0);

        assertEquals((short) 0, mutNum.compareTo(new MutableShort((short) 0)));
        assertEquals((short) +1, mutNum.compareTo(new MutableShort((short) -1)));
        assertEquals((short) -1, mutNum.compareTo(new MutableShort((short) 1)));
        try {
            mutNum.compareTo(null);
            fail();
        } catch (NullPointerException ex) {}
    }

    @Test
    public void testPrimitiveValues() {
        MutableShort mutNum = new MutableShort( (short) 1 );
        
        assertEquals( 1.0F, mutNum.floatValue(), 0 );
        assertEquals( 1.0, mutNum.doubleValue(), 0 );
        assertEquals( (byte) 1, mutNum.byteValue() );
        assertEquals( (short) 1, mutNum.shortValue() );
        assertEquals( 1, mutNum.intValue() );
        assertEquals( 1L, mutNum.longValue() );
    }

    @Test
    public void testToShort() {
        assertEquals(Short.valueOf((short) 0), new MutableShort((short) 0).toShort());
        assertEquals(Short.valueOf((short) 123), new MutableShort((short) 123).toShort());
    }

    @Test
    public void testIncrement() {
        MutableShort mutNum = new MutableShort((short) 1);
        mutNum.increment();
        
        assertEquals(2, mutNum.intValue());
        assertEquals(2L, mutNum.longValue());
    }

    @Test
    public void testDecrement() {
        MutableShort mutNum = new MutableShort((short) 1);
        mutNum.decrement();
        
        assertEquals(0, mutNum.intValue());
        assertEquals(0L, mutNum.longValue());
    }

    @Test
    public void testAddValuePrimitive() {
        MutableShort mutNum = new MutableShort((short) 1);
        mutNum.add((short) 1);
        
        assertEquals((short) 2, mutNum.shortValue());
    }

    @Test
    public void testAddValueObject() {
        MutableShort mutNum = new MutableShort((short) 1);
        mutNum.add(Short.valueOf((short) 1));
        
        assertEquals((short) 2, mutNum.shortValue());
    }

    @Test
    public void testSubtractValuePrimitive() {
        MutableShort mutNum = new MutableShort((short) 1);
        mutNum.subtract((short) 1);
        
        assertEquals((short) 0, mutNum.shortValue());
    }

    @Test
    public void testSubtractValueObject() {
        MutableShort mutNum = new MutableShort((short) 1);
        mutNum.subtract(Short.valueOf((short) 1));
        
        assertEquals((short) 0, mutNum.shortValue());
    }

    @Test
    public void testToString() {
        assertEquals("0", new MutableShort((short) 0).toString());
        assertEquals("10", new MutableShort((short) 10).toString());
        assertEquals("-123", new MutableShort((short) -123).toString());
    }

}
