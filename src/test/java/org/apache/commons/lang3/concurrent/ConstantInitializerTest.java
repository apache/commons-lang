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
package org.apache.commons.lang3.concurrent;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.regex.Pattern;

import org.junit.Before;
import org.junit.Test;

/**
 * Test class for {@code ConstantInitializer}.
 *
 * @version $Id$
 */
public class ConstantInitializerTest {
    /** Constant for the object managed by the initializer. */
    private static final Integer VALUE = 42;

    /** The initializer to be tested. */
    private ConstantInitializer<Integer> init;

    @Before
    public void setUp() throws Exception {
        init = new ConstantInitializer<Integer>(VALUE);
    }

    /**
     * Helper method for testing equals() and hashCode().
     *
     * @param obj the object to compare with the test instance
     * @param expected the expected result
     */
    private void checkEquals(Object obj, boolean expected) {
        assertTrue("Wrong result of equals", expected == init.equals(obj));
        if (obj != null) {
            assertTrue("Not symmetric", expected == obj.equals(init));
            if (expected) {
                assertEquals("Different hash codes", init.hashCode(),
                        obj.hashCode());
            }
        }
    }

    /**
     * Tests whether the correct object is returned.
     */
    @Test
    public void testGetObject() {
        assertEquals("Wrong object", VALUE, init.getObject());
    }

    /**
     * Tests whether get() returns the correct object.
     */
    @Test
    public void testGet() throws ConcurrentException {
        assertEquals("Wrong object", VALUE, init.get());
    }

    /**
     * Tests equals() if the expected result is true.
     */
    @Test
    public void testEqualsTrue() {
        checkEquals(init, true);
        ConstantInitializer<Integer> init2 = new ConstantInitializer<Integer>(
                Integer.valueOf(VALUE.intValue()));
        checkEquals(init2, true);
        init = new ConstantInitializer<Integer>(null);
        init2 = new ConstantInitializer<Integer>(null);
        checkEquals(init2, true);
    }

    /**
     * Tests equals() if the expected result is false.
     */
    @Test
    public void testEqualsFalse() {
        ConstantInitializer<Integer> init2 = new ConstantInitializer<Integer>(
                null);
        checkEquals(init2, false);
        init2 = new ConstantInitializer<Integer>(VALUE + 1);
        checkEquals(init2, false);
    }

    /**
     * Tests equals() with objects of other classes.
     */
    @Test
    public void testEqualsWithOtherObjects() {
        checkEquals(null, false);
        checkEquals(this, false);
        checkEquals(new ConstantInitializer<String>("Test"), false);
    }

    /**
     * Tests the string representation.
     */
    @Test
    public void testToString() {
        String s = init.toString();
        Pattern pattern = Pattern
                .compile("ConstantInitializer@\\d+ \\[ object = " + VALUE
                        + " \\]");
        assertTrue("Wrong string: " + s, pattern.matcher(s).matches());
    }

    /**
     * Tests the string representation if the managed object is null.
     */
    @Test
    public void testToStringNull() {
        String s = new ConstantInitializer<Object>(null).toString();
        assertTrue("Object not found: " + s, s.indexOf("object = null") > 0);
    }
}
