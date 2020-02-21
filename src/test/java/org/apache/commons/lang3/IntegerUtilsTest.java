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
package org.apache.commons.lang3;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;

import org.junit.jupiter.api.Test;

/**
 * Unit tests {@link org.apache.commons.lang3.IntegerUtils}.
 */
public class IntegerUtilsTest {

    //-----------------------------------------------------------------------
    @Test
    public void testConstructor() {
        assertNotNull(new IntegerUtils());
        final Constructor<?>[] cons = IntegerUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertTrue(Modifier.isPublic(cons[0].getModifiers()));
        assertTrue(Modifier.isPublic(IntegerUtils.class.getModifiers()));
        assertFalse(Modifier.isFinal(IntegerUtils.class.getModifiers()));
    }

    //-----------------------------------------------------------------------
    @Test
    public void test_isPositive_int_Integer() {
        int testInt = 10;
        assertFalse(IntegerUtils.isPositive(null));
        assertTrue(IntegerUtils.isPositive(10));
        assertFalse(IntegerUtils.isPositive(-10));
        assertTrue(IntegerUtils.isPositive(testInt));
    }

    //-----------------------------------------------------------------------
    @Test
    public void test_isNotPositive_int_Integer() {
        int zero = 0;
        asserTrue(IntegerUtils.isNotPositive(null));
        assertFalse(IntegerUtils.isNotPositive(10));
        assertTrue(IntegerUtils.isNotPositive(-10));
        assertTrue(IntegerUtils.isNotPositive(zero));
    }

    //-----------------------------------------------------------------------
    @Test
    public void test_isNegative_int_Integer() {
        int testInt = -10;
        assertFalse(IntegerUtils.isNegative(null));
        assertTrue(IntegerUtils.isNegative(-10));
        assertFalse(IntegerUtils.isNegative(10));
        assertTrue(IntegerUtils.isNegative(testInt));
    }

    //-----------------------------------------------------------------------
    @Test
    public void test_isNotNegative_int_Integer() {
        int zero = 0;
        asserTrue(IntegerUtils.isNotNegative(null));
        assertFalse(IntegerUtils.isNotNegative(-10));
        assertTrue(IntegerUtils.isNotNegative(10));
        assertTrue(IntegerUtils.isNotNegative(zero));
    }

    //-----------------------------------------------------------------------
    @Test
    public void test_isZero_int_Integer() {
        int zero = 0;
        assertFalse(IntegerUtils.isZero(null));
        assertTrue(IntegerUtils.isZero(0));
        assertFalse(IntegerUtils.isZero(-10));
        assertTrue(IntegerUtils.isZero(zero));
    }

    //-----------------------------------------------------------------------
    @Test
    public void test_isNotZero_int_Integer() {
        int notZero = 10;
        assertTrue(IntegerUtils.isNotZero(null));
        assertTrue(IntegerUtils.isNotZero(10));
        assertFalse(IntegerUtils.isNotZero(0));
        assertTrue(IntegerUtils.isNotZero(notZero));
    }
}
