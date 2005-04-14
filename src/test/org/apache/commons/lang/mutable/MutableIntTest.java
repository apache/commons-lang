/*
 * Copyright 2002-2005 The Apache Software Foundation.
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
 * @see MutableInt
 */
public class MutableIntTest extends TestCase {

    public MutableIntTest(String testName) {
        super(testName);
    }

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
        return new TestSuite(MutableIntTest.class);
    }

    // ----------------------------------------------------------------
    public void testConstructors() {
        assertEquals(0, new MutableInt().intValue());
        
        assertEquals(1, new MutableInt(1).intValue());
        
        assertEquals(2, new MutableInt(new Integer(2)).intValue());
        assertEquals(3, new MutableInt(new MutableLong(3)).intValue());
        try {
            new MutableInt(null);
            fail();
        } catch (NullPointerException ex) {}
    }

    public void testGetSet() {
        final MutableInt mutNum = new MutableInt(0);
        assertEquals(0, new MutableInt().intValue());
        assertEquals(new Integer(0), new MutableInt().getValue());
        
        mutNum.setValue(1);
        assertEquals(1, mutNum.intValue());
        assertEquals(new Integer(1), mutNum.getValue());
        
        mutNum.setValue(new Integer(2));
        assertEquals(2, mutNum.intValue());
        assertEquals(new Integer(2), mutNum.getValue());
        
        mutNum.setValue(new MutableLong(3));
        assertEquals(3, mutNum.intValue());
        assertEquals(new Integer(3), mutNum.getValue());
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
        final MutableInt mutNumA = new MutableInt(0);
        final MutableInt mutNumB = new MutableInt(0);
        final MutableInt mutNumC = new MutableInt(1);

        assertEquals(true, mutNumA.equals(mutNumA));
        assertEquals(true, mutNumA.equals(mutNumB));
        assertEquals(true, mutNumB.equals(mutNumA));
        assertEquals(true, mutNumB.equals(mutNumB));
        assertEquals(false, mutNumA.equals(mutNumC));
        assertEquals(false, mutNumB.equals(mutNumC));
        assertEquals(true, mutNumC.equals(mutNumC));
        assertEquals(false, mutNumA.equals(null));
        assertEquals(false, mutNumA.equals(new Integer(0)));
        assertEquals(false, mutNumA.equals("0"));
    }

    public void testHashCode() {
        final MutableInt mutNumA = new MutableInt(0);
        final MutableInt mutNumB = new MutableInt(0);
        final MutableInt mutNumC = new MutableInt(1);

        assertEquals(true, mutNumA.hashCode() == mutNumA.hashCode());
        assertEquals(true, mutNumA.hashCode() == mutNumB.hashCode());
        assertEquals(false, mutNumA.hashCode() == mutNumC.hashCode());
        assertEquals(true, mutNumA.hashCode() == new Integer(0).hashCode());
    }

    public void testCompareTo() {
        final MutableInt mutNum = new MutableInt(0);

        assertEquals(0, mutNum.compareTo(new MutableInt(0)));
        assertEquals(+1, mutNum.compareTo(new MutableInt(-1)));
        assertEquals(-1, mutNum.compareTo(new MutableInt(1)));
        try {
            mutNum.compareTo(null);
            fail();
        } catch (NullPointerException ex) {}
        try {
            mutNum.compareTo(new Integer(0));
            fail();
        } catch (ClassCastException ex) {}
        try {
            mutNum.compareTo("0");
            fail();
        } catch (ClassCastException ex) {}
    }

    public void testPrimitiveValues() {
        MutableInt mutNum = new MutableInt(1);
        
        assertEquals( (byte) 1, mutNum.byteValue() );
        assertEquals( (short) 1, mutNum.shortValue() );
        assertEquals( 1.0F, mutNum.floatValue(), 0 );
        assertEquals( 1.0, mutNum.doubleValue(), 0 );
        assertEquals( 1L, mutNum.longValue() );
    }

    public void testToString() {
        assertEquals("0", new MutableInt(0).toString());
        assertEquals("10", new MutableInt(10).toString());
        assertEquals("-123", new MutableInt(-123).toString());
    }

}
