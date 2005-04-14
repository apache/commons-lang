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
 * @see MutableShort
 */
public class MutableObjectTest extends TestCase {

    public MutableObjectTest(String testName) {
        super(testName);
    }

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
        return new TestSuite(MutableObjectTest.class);
    }

    // ----------------------------------------------------------------
    public void testConstructors() {
        assertEquals(null, new MutableObject().getValue());
        
        Integer i = new Integer(6);
        assertSame(i, new MutableObject(i).getValue());
        assertSame("HI", new MutableObject("HI").getValue());
        assertSame(null, new MutableObject(null).getValue());
    }

    public void testGetSet() {
        final MutableObject mutNum = new MutableObject();
        assertEquals(null, new MutableObject().getValue());
        
        mutNum.setValue("HELLO");
        assertSame("HELLO", mutNum.getValue());
        
        mutNum.setValue(null);
        assertSame(null, mutNum.getValue());
    }

    public void testEquals() {
        final MutableObject mutNumA = new MutableObject("ALPHA");
        final MutableObject mutNumB = new MutableObject("ALPHA");
        final MutableObject mutNumC = new MutableObject("BETA");
        final MutableObject mutNumD = new MutableObject(null);

        assertEquals(true, mutNumA.equals(mutNumA));
        assertEquals(true, mutNumA.equals(mutNumB));
        assertEquals(true, mutNumB.equals(mutNumA));
        assertEquals(true, mutNumB.equals(mutNumB));
        assertEquals(false, mutNumA.equals(mutNumC));
        assertEquals(false, mutNumB.equals(mutNumC));
        assertEquals(true, mutNumC.equals(mutNumC));
        assertEquals(false, mutNumA.equals(mutNumD));
        assertEquals(true, mutNumD.equals(mutNumD));
        
        assertEquals(false, mutNumA.equals(null));
        assertEquals(false, mutNumA.equals(new Object()));
        assertEquals(false, mutNumA.equals("0"));
    }

    public void testHashCode() {
        final MutableObject mutNumA = new MutableObject("ALPHA");
        final MutableObject mutNumB = new MutableObject("ALPHA");
        final MutableObject mutNumC = new MutableObject("BETA");
        final MutableObject mutNumD = new MutableObject(null);

        assertEquals(true, mutNumA.hashCode() == mutNumA.hashCode());
        assertEquals(true, mutNumA.hashCode() == mutNumB.hashCode());
        assertEquals(false, mutNumA.hashCode() == mutNumC.hashCode());
        assertEquals(false, mutNumA.hashCode() == mutNumD.hashCode());
        assertEquals(true, mutNumA.hashCode() == "ALPHA".hashCode());
        assertEquals(0, mutNumD.hashCode());
    }

    public void testToString() {
        assertEquals("HI", new MutableObject("HI").toString());
        assertEquals("10.0", new MutableObject(new Double(10)).toString());
        assertEquals("null", new MutableObject(null).toString());
    }

}
