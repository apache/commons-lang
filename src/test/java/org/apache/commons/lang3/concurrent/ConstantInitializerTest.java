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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.regex.Pattern;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * Test class for {@code ConstantInitializer}.
 */
public class ConstantInitializerTest extends AbstractLangTest {
    /** Constant for the object managed by the initializer. */
    private static final Integer VALUE = 42;

    /** The initializer to be tested. */
    private ConstantInitializer<Integer> init;

    @BeforeEach
    public void setUp() {
        init = new ConstantInitializer<>(VALUE);
    }

    /**
     * Helper method for testing equals() and hashCode().
     *
     * @param obj the object to compare with the test instance
     * @param expected the expected result
     */
    private void checkEquals(final Object obj, final boolean expected) {
        assertEquals(expected, init.equals(obj), "Wrong result of equals");
        if (obj != null) {
            assertEquals(expected, obj.equals(init), "Not symmetric");
            if (expected) {
                assertEquals(init.hashCode(), obj.hashCode(), "Different hash codes");
            }
        }
    }

    /**
     * Tests whether the correct object is returned.
     */
    @Test
    public void testGetObject() {
        assertEquals(VALUE, init.getObject(), "Wrong object");
    }

    /**
     * Tests whether get() returns the correct object.
     *
     * @throws org.apache.commons.lang3.concurrent.ConcurrentException so we don't have to catch it
     */
    @Test
    public void testGet() throws ConcurrentException {
        assertEquals(VALUE, init.get(), "Wrong object");
    }

    /**
     * Tests equals() if the expected result is true.
     */
    @Test
    public void testEqualsTrue() {
        checkEquals(init, true);
        ConstantInitializer<Integer> init2 = new ConstantInitializer<>(
                Integer.valueOf(VALUE.intValue()));
        checkEquals(init2, true);
        init = new ConstantInitializer<>(null);
        init2 = new ConstantInitializer<>(null);
        checkEquals(init2, true);
    }

    /**
     * Tests equals() if the expected result is false.
     */
    @Test
    public void testEqualsFalse() {
        ConstantInitializer<Integer> init2 = new ConstantInitializer<>(
                null);
        checkEquals(init2, false);
        init2 = new ConstantInitializer<>(VALUE + 1);
        checkEquals(init2, false);
    }

    /**
     * Tests equals() with objects of other classes.
     */
    @Test
    public void testEqualsWithOtherObjects() {
        checkEquals(null, false);
        checkEquals(this, false);
        checkEquals(new ConstantInitializer<>("Test"), false);
    }

    /**
     * Tests the string representation.
     */
    @Test
    public void testToString() {
        final String s = init.toString();
        final Pattern pattern = Pattern
                .compile("ConstantInitializer@-?\\d+ \\[ object = " + VALUE
                        + " \\]");
        assertTrue(pattern.matcher(s).matches(), "Wrong string: " + s);
    }

    /**
     * Tests the string representation if the managed object is null.
     */
    @Test
    public void testToStringNull() {
        final String s = new ConstantInitializer<>(null).toString();
        assertTrue(s.indexOf("object = null") > 0, "Object not found: " + s);
    }
}
