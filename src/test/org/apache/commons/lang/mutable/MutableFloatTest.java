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
 * @see MutableFloat
 */
public class MutableFloatTest extends TestCase {

    public MutableFloatTest(String testName) {
        super(testName);
    }

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
        return new TestSuite(MutableFloatTest.class);
    }

    // ----------------------------------------------------------------
    public void testConstructors() {
        assertEquals(0f, new MutableFloat().floatValue(), 0.0001f);
        
        assertEquals(1f, new MutableFloat(1f).floatValue(), 0.0001f);
        
        assertEquals(2f, new MutableFloat(new Float(2f)).floatValue(), 0.0001f);
        assertEquals(3f, new MutableFloat(new MutableFloat(3f)).floatValue(), 0.0001f);
        try {
            new MutableFloat(null);
            fail();
        } catch (NullPointerException ex) {}
    }

    public void testGetSet() {
        final MutableFloat mutNum = new MutableFloat(0f);
        assertEquals(0f, new MutableFloat().floatValue(), 0.0001f);
        assertEquals(new Float(0), new MutableFloat().getValue());
        
        mutNum.setValue(1);
        assertEquals(1f, mutNum.floatValue(), 0.0001f);
        assertEquals(new Float(1f), mutNum.getValue());
        
        mutNum.setValue(new Float(2f));
        assertEquals(2f, mutNum.floatValue(), 0.0001f);
        assertEquals(new Float(2f), mutNum.getValue());
        
        mutNum.setValue(new MutableFloat(3f));
        assertEquals(3f, mutNum.floatValue(), 0.0001f);
        assertEquals(new Float(3f), mutNum.getValue());
        try {
            mutNum.setValue(null);
            fail();
        } catch (NullPointerException ex) {}
        try {
            mutNum.setValue("0");
            fail();
        } catch (ClassCastException ex) {}
    }

    public void testNanInfinite() {
        MutableFloat mutNum = new MutableFloat(Float.NaN);
        assertEquals(true, mutNum.isNaN());
        
        mutNum = new MutableFloat(Float.POSITIVE_INFINITY);
        assertEquals(true, mutNum.isInfinite());
        
        mutNum = new MutableFloat(Float.NEGATIVE_INFINITY);
        assertEquals(true, mutNum.isInfinite());
    }

    public void testEquals() {
        final MutableFloat mutNumA = new MutableFloat(0f);
        final MutableFloat mutNumB = new MutableFloat(0f);
        final MutableFloat mutNumC = new MutableFloat(1f);

        assertEquals(true, mutNumA.equals(mutNumA));
        assertEquals(true, mutNumA.equals(mutNumB));
        assertEquals(true, mutNumB.equals(mutNumA));
        assertEquals(true, mutNumB.equals(mutNumB));
        assertEquals(false, mutNumA.equals(mutNumC));
        assertEquals(false, mutNumB.equals(mutNumC));
        assertEquals(true, mutNumC.equals(mutNumC));
        assertEquals(false, mutNumA.equals(null));
        assertEquals(false, mutNumA.equals(new Float(0f)));
        assertEquals(false, mutNumA.equals("0"));
    }

    public void testHashCode() {
        final MutableFloat mutNumA = new MutableFloat(0f);
        final MutableFloat mutNumB = new MutableFloat(0f);
        final MutableFloat mutNumC = new MutableFloat(1f);

        assertEquals(true, mutNumA.hashCode() == mutNumA.hashCode());
        assertEquals(true, mutNumA.hashCode() == mutNumB.hashCode());
        assertEquals(false, mutNumA.hashCode() == mutNumC.hashCode());
        assertEquals(true, mutNumA.hashCode() == new Float(0f).hashCode());
    }

    public void testCompareTo() {
        final MutableFloat mutNum = new MutableFloat(0f);

        assertEquals(0, mutNum.compareTo(new MutableFloat(0f)));
        assertEquals(+1, mutNum.compareTo(new MutableFloat(-1f)));
        assertEquals(-1, mutNum.compareTo(new MutableFloat(1f)));
        try {
            mutNum.compareTo(null);
            fail();
        } catch (NullPointerException ex) {}
        try {
            mutNum.compareTo(new Float(0f));
            fail();
        } catch (ClassCastException ex) {}
        try {
            mutNum.compareTo("0");
            fail();
        } catch (ClassCastException ex) {}
    }

    public void testPrimitiveValues() {
        MutableFloat mutNum = new MutableFloat(1.7F);
        
        assertEquals( 1, mutNum.intValue() );
        assertEquals( 1.7, mutNum.doubleValue(), 0.00001 );
        assertEquals( (byte) 1, mutNum.byteValue() );
        assertEquals( (short) 1, mutNum.shortValue() );
        assertEquals( 1, mutNum.intValue() );
        assertEquals( 1L, mutNum.longValue() );
    }

    public void testToString() {
        assertEquals("0.0", new MutableFloat(0f).toString());
        assertEquals("10.0", new MutableFloat(10f).toString());
        assertEquals("-123.0", new MutableFloat(-123f).toString());
    }

}
