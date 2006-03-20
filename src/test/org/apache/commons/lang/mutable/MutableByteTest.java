/*
 * Copyright 2002-2006 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang.mutable;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * JUnit tests.
 * 
 * @version $Id$
 * @see MutableByte
 */
public class MutableByteTest extends TestCase {

    public MutableByteTest(String testName) {
        super(testName);
    }

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
        return new TestSuite(MutableByteTest.class);
    }

    // ----------------------------------------------------------------
    public void testConstructors() {
        assertEquals((byte) 0, new MutableByte().byteValue());
        
        assertEquals((byte) 1, new MutableByte((byte) 1).byteValue());
        
        assertEquals((byte) 2, new MutableByte(new Byte((byte) 2)).byteValue());
        assertEquals((byte) 3, new MutableByte(new MutableByte((byte) 3)).byteValue());
        try {
            new MutableByte(null);
            fail();
        } catch (NullPointerException ex) {}
    }

    public void testGetSet() {
        final MutableByte mutNum = new MutableByte((byte) 0);
        assertEquals((byte) 0, new MutableByte().byteValue());
        assertEquals(new Byte((byte) 0), new MutableByte().getValue());
        
        mutNum.setValue((byte) 1);
        assertEquals((byte) 1, mutNum.byteValue());
        assertEquals(new Byte((byte) 1), mutNum.getValue());
        
        mutNum.setValue(new Byte((byte) 2));
        assertEquals((byte) 2, mutNum.byteValue());
        assertEquals(new Byte((byte) 2), mutNum.getValue());
        
        mutNum.setValue(new MutableByte((byte) 3));
        assertEquals((byte) 3, mutNum.byteValue());
        assertEquals(new Byte((byte) 3), mutNum.getValue());
        try {
            mutNum.setValue(null);
            fail();
        } catch (NullPointerException ex) {}
        try {
            mutNum.setValue("0");
            fail();
        } catch (ClassCastException ex) {}
    }

    public void testEquals() {
        final MutableByte mutNumA = new MutableByte((byte) 0);
        final MutableByte mutNumB = new MutableByte((byte) 0);
        final MutableByte mutNumC = new MutableByte((byte) 1);

        assertEquals(true, mutNumA.equals(mutNumA));
        assertEquals(true, mutNumA.equals(mutNumB));
        assertEquals(true, mutNumB.equals(mutNumA));
        assertEquals(true, mutNumB.equals(mutNumB));
        assertEquals(false, mutNumA.equals(mutNumC));
        assertEquals(false, mutNumB.equals(mutNumC));
        assertEquals(true, mutNumC.equals(mutNumC));
        assertEquals(false, mutNumA.equals(null));
        assertEquals(false, mutNumA.equals(new Byte((byte) 0)));
        assertEquals(false, mutNumA.equals("0"));
    }

    public void testHashCode() {
        final MutableByte mutNumA = new MutableByte((byte) 0);
        final MutableByte mutNumB = new MutableByte((byte) 0);
        final MutableByte mutNumC = new MutableByte((byte) 1);

        assertEquals(true, mutNumA.hashCode() == mutNumA.hashCode());
        assertEquals(true, mutNumA.hashCode() == mutNumB.hashCode());
        assertEquals(false, mutNumA.hashCode() == mutNumC.hashCode());
        assertEquals(true, mutNumA.hashCode() == new Byte((byte) 0).hashCode());
    }

    public void testCompareTo() {
        final MutableByte mutNum = new MutableByte((byte) 0);

        assertEquals((byte) 0, mutNum.compareTo(new MutableByte((byte) 0)));
        assertEquals((byte) +1, mutNum.compareTo(new MutableByte((byte) -1)));
        assertEquals((byte) -1, mutNum.compareTo(new MutableByte((byte) 1)));
        try {
            mutNum.compareTo(null);
            fail();
        } catch (NullPointerException ex) {}
        try {
            mutNum.compareTo(new Byte((byte) 0));
            fail();
        } catch (ClassCastException ex) {}
        try {
            mutNum.compareTo("0");
            fail();
        } catch (ClassCastException ex) {}
    }

    public void testPrimitiveValues() {
        MutableByte mutNum = new MutableByte( (byte) 1 );
        
        assertEquals( 1.0F, mutNum.floatValue(), 0 );
        assertEquals( 1.0, mutNum.doubleValue(), 0 );
        assertEquals( (byte) 1, mutNum.byteValue() );
        assertEquals( (short) 1, mutNum.shortValue() );
        assertEquals( 1, mutNum.intValue() );
        assertEquals( 1L, mutNum.longValue() );
    }

    public void testToByte() {
        assertEquals(new Byte((byte) 0), new MutableByte((byte) 0).toByte());
        assertEquals(new Byte((byte) 123), new MutableByte((byte) 123).toByte());
    }

    public void testIncrement() {
        MutableByte mutNum = new MutableByte((byte) 1);
        mutNum.increment();
        
        assertEquals(2, mutNum.intValue());
        assertEquals(2L, mutNum.longValue());
    }

    public void testDecrement() {
        MutableByte mutNum = new MutableByte((byte) 1);
        mutNum.decrement();
        
        assertEquals(0, mutNum.intValue());
        assertEquals(0L, mutNum.longValue());
    }

    public void testAddValuePrimitive() {
        MutableByte mutNum = new MutableByte((byte) 1);
        mutNum.add((byte)1);
        
        assertEquals((byte) 2, mutNum.byteValue());
    }

    public void testAddValueObject() {
        MutableByte mutNum = new MutableByte((byte) 1);
        mutNum.add(new Integer(1));
        
        assertEquals((byte) 2, mutNum.byteValue());
    }

    public void testSubtractValuePrimitive() {
        MutableByte mutNum = new MutableByte((byte) 1);
        mutNum.subtract((byte) 1);
        
        assertEquals((byte) 0, mutNum.byteValue());
    }

    public void testSubtractValueObject() {
        MutableByte mutNum = new MutableByte((byte) 1);
        mutNum.subtract(new Integer(1));
        
        assertEquals((byte) 0, mutNum.byteValue());
    }

    public void testToString() {
        assertEquals("0", new MutableByte((byte) 0).toString());
        assertEquals("10", new MutableByte((byte) 10).toString());
        assertEquals("-123", new MutableByte((byte) -123).toString());
    }

}
