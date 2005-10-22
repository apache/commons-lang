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
 * @since 2.2
 * @see MutableBoolean
 * @author Apache Software Foundation
 * @version $Id$
 */
public class MutableBooleanTest extends TestCase {

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
        return new TestSuite(MutableBooleanTest.class);
    }

    public MutableBooleanTest(String testName) {
        super(testName);
    }

    public void testCompareTo() {
        final MutableBoolean mutBool = new MutableBoolean(false);

        assertEquals(0, mutBool.compareTo(new MutableBoolean(false)));
        assertEquals(-1, mutBool.compareTo(new MutableBoolean(true)));
        mutBool.setValue(true);
        assertEquals(+1, mutBool.compareTo(new MutableBoolean(false)));
        assertEquals(0, mutBool.compareTo(new MutableBoolean(true)));

        try {
            mutBool.compareTo(null);
            fail();
        } catch (NullPointerException ex) {
        }
        try {
            mutBool.compareTo(Boolean.FALSE);
            fail();
        } catch (ClassCastException ex) {
        }
        try {
            mutBool.compareTo("false");
            fail();
        } catch (ClassCastException ex) {
        }
    }

    // ----------------------------------------------------------------
    public void testConstructors() {
        assertEquals(false, new MutableBoolean().booleanValue());

        assertEquals(true, new MutableBoolean(true).booleanValue());
        assertEquals(false, new MutableBoolean(false).booleanValue());

        assertEquals(true, new MutableBoolean(Boolean.TRUE).booleanValue());
        assertEquals(false, new MutableBoolean(Boolean.FALSE).booleanValue());

        try {
            new MutableBoolean(null);
            fail();
        } catch (NullPointerException ex) {
        }
    }

    public void testEquals() {
        final MutableBoolean mutBoolA = new MutableBoolean(false);
        final MutableBoolean mutBoolB = new MutableBoolean(false);
        final MutableBoolean mutBoolC = new MutableBoolean(true);

        assertEquals(true, mutBoolA.equals(mutBoolA));
        assertEquals(true, mutBoolA.equals(mutBoolB));
        assertEquals(true, mutBoolB.equals(mutBoolA));
        assertEquals(true, mutBoolB.equals(mutBoolB));
        assertEquals(false, mutBoolA.equals(mutBoolC));
        assertEquals(false, mutBoolB.equals(mutBoolC));
        assertEquals(true, mutBoolC.equals(mutBoolC));
        assertEquals(false, mutBoolA.equals(null));
        assertEquals(false, mutBoolA.equals(Boolean.FALSE));
        assertEquals(false, mutBoolA.equals("false"));
    }

    public void testGetSet() {
        final MutableBoolean mutBool = new MutableBoolean(false);
        assertEquals(false, new MutableBoolean().booleanValue());

        mutBool.setValue(Boolean.TRUE);
        assertEquals(true, mutBool.booleanValue());

        mutBool.setValue(false);
        assertEquals(false, mutBool.booleanValue());

        mutBool.setValue(true);
        assertEquals(true, mutBool.booleanValue());

        try {
            mutBool.setValue(null);
            fail();
        } catch (NullPointerException ex) {
        }
        try {
            mutBool.setValue("false");
            fail();
        } catch (ClassCastException ex) {
        }
    }

    public void testHashCode() {
        final MutableBoolean mutBoolA = new MutableBoolean(false);
        final MutableBoolean mutBoolB = new MutableBoolean(false);
        final MutableBoolean mutBoolC = new MutableBoolean(true);

        assertEquals(true, mutBoolA.hashCode() == mutBoolA.hashCode());
        assertEquals(true, mutBoolA.hashCode() == mutBoolB.hashCode());
        assertEquals(false, mutBoolA.hashCode() == mutBoolC.hashCode());
        assertEquals(true, mutBoolA.hashCode() == Boolean.FALSE.hashCode());
        assertEquals(true, mutBoolC.hashCode() == Boolean.TRUE.hashCode());
    }

    public void testToString() {
        assertEquals(Boolean.FALSE.toString(), new MutableBoolean(false).toString());
        assertEquals(Boolean.TRUE.toString(), new MutableBoolean(true).toString());
    }

}
