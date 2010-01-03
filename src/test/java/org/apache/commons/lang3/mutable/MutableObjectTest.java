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

import junit.framework.TestCase;

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

    // ----------------------------------------------------------------
    public void testConstructors() {
        assertEquals(null, new MutableObject<String>().getValue());
        
        Integer i = new Integer(6);
        assertSame(i, new MutableObject<Integer>(i).getValue());
        assertSame("HI", new MutableObject<String>("HI").getValue());
        assertSame(null, new MutableObject<Object>(null).getValue());
    }

    public void testGetSet() {
        final MutableObject<String> mutNum = new MutableObject<String>();
        assertEquals(null, new MutableObject<Object>().getValue());
        
        mutNum.setValue("HELLO");
        assertSame("HELLO", mutNum.getValue());
        
        mutNum.setValue(null);
        assertSame(null, mutNum.getValue());
    }

    public void testEquals() {
        final MutableObject<String> mutNumA = new MutableObject<String>("ALPHA");
        final MutableObject<String> mutNumB = new MutableObject<String>("ALPHA");
        final MutableObject<String> mutNumC = new MutableObject<String>("BETA");
        final MutableObject<String> mutNumD = new MutableObject<String>(null);

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
        final MutableObject<String> mutNumA = new MutableObject<String>("ALPHA");
        final MutableObject<String> mutNumB = new MutableObject<String>("ALPHA");
        final MutableObject<String> mutNumC = new MutableObject<String>("BETA");
        final MutableObject<String> mutNumD = new MutableObject<String>(null);

        assertEquals(true, mutNumA.hashCode() == mutNumA.hashCode());
        assertEquals(true, mutNumA.hashCode() == mutNumB.hashCode());
        assertEquals(false, mutNumA.hashCode() == mutNumC.hashCode());
        assertEquals(false, mutNumA.hashCode() == mutNumD.hashCode());
        assertEquals(true, mutNumA.hashCode() == "ALPHA".hashCode());
        assertEquals(0, mutNumD.hashCode());
    }

    public void testToString() {
        assertEquals("HI", new MutableObject<String>("HI").toString());
        assertEquals("10.0", new MutableObject<Double>(new Double(10)).toString());
        assertEquals("null", new MutableObject<Object>(null).toString());
    }

}
