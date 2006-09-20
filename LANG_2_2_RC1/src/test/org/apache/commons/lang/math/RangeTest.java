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

package org.apache.commons.lang.math;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * <p>
 * Tests the base methods in the {@link org.apache.commons.lang.math.Range} class.
 * </p>
 * 
 * @author Nathan Beyer
 * @version $Id$
 */
public class RangeTest extends TestCase {

    private static class RangeTestFixture extends Range {
        private byte max;

        private byte min;

        RangeTestFixture(byte min, byte max) {
            super();
            this.min = min;
            this.max = max;
        }

        public boolean containsNumber(Number number) {
            if (number.byteValue() >= min && number.byteValue() <= max) {
                return true;
            }
            return false;
        }

        public Number getMaximumNumber() {
            return new Byte(max);
        }

        public Number getMinimumNumber() {
            return new Byte(min);
        }
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(RangeTest.class);
        suite.setName("Range Tests");
        return suite;
    }

    public RangeTest(String name) {
        super(name);
    }

    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    /**
     * Test method for 'org.apache.commons.lang.math.Range.equals(Object)'
     */
    public void testEqualsObject() {
        RangeTestFixture r1 = new RangeTestFixture((byte) 0, (byte) 5);
        RangeTestFixture r2 = new RangeTestFixture((byte) 0, (byte) 5);
        RangeTestFixture r3 = new RangeTestFixture((byte) 0, (byte) 10);

        assertEquals(r1, r1);
        assertEquals(r1, r2);
        assertEquals(r2, r2);
        assertTrue(r1.equals(r1));
        assertTrue(r2.equals(r2));
        assertTrue(r3.equals(r3));
        assertFalse(r2.equals(r3));
        assertFalse(r2.equals(null));
        assertFalse(r2.equals("Ni!"));
    }

    /**
     * Test method for 'org.apache.commons.lang.math.Range.hashCode()'
     */
    public void testHashCode() {
        RangeTestFixture r1 = new RangeTestFixture((byte) 0, (byte) 5);
        RangeTestFixture r2 = new RangeTestFixture((byte) 0, (byte) 5);
        RangeTestFixture r3 = new RangeTestFixture((byte) 0, (byte) 10);

        assertEquals(r1.hashCode(), r2.hashCode());
        assertFalse(r1.hashCode() == r3.hashCode());
    }

    /**
     * Test method for 'org.apache.commons.lang.math.Range.toString()'
     */
    public void testToString() {
        RangeTestFixture r1 = new RangeTestFixture((byte) 0, (byte) 5);
        assertNotNull(r1.toString());
        assertNotNull(r1.toString());
        RangeTestFixture r2 = new RangeTestFixture((byte) 0, (byte) 5);
        assertNotNull(r2.toString());
        assertNotNull(r2.toString());
        RangeTestFixture r3 = new RangeTestFixture((byte) 0, (byte) 10);
        assertNotNull(r3.toString());
        assertNotNull(r3.toString());
    }

}
