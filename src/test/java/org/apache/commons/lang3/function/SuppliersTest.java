/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.commons.lang3.function;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.function.Supplier;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link Suppliers}.
 */
class SuppliersTest extends AbstractLangTest {

    /**
     * Tests {@link Suppliers#get(Supplier)}.
     */
    @Test
    void testGet() {
        assertNull(Suppliers.get(null));
        assertNull(Suppliers.get(() -> null));
        assertNull(Suppliers.get(Suppliers.nul()));
        assertEquals("foo", Suppliers.get(() -> "foo"));
        assertEquals("foo", Suppliers.get(Suppliers.supply("foo")));
    }

    /**
     * Tests {@link Suppliers#supply(Object)}.
     */
    @Test
    void testSupply() {
        assertNull(Suppliers.supply(null).get());
        assertEquals("foo", Suppliers.supply("foo").get());
    }

    /**
     * Tests {@link Suppliers#supplyBoolean(boolean)}.
     */
    @Test
    void testSupplyBoolean() {
        assertTrue(Suppliers.supplyBoolean(true).getAsBoolean());
        assertFalse(Suppliers.supplyBoolean(false).getAsBoolean());
        assertSame(Suppliers.supplyBoolean(true), Suppliers.supplyBoolean(true));
        assertSame(Suppliers.supplyBoolean(false), Suppliers.supplyBoolean(false));
    }

    /**
     * Tests {@link Suppliers#supplyByte(byte)}.
     */
    @Test
    void testSupplyByte() {
        assertEquals((byte) 0, Suppliers.supplyByte((byte) 0).getAsByte());
        assertEquals(Byte.MAX_VALUE, Suppliers.supplyByte(Byte.MAX_VALUE).getAsByte());
        assertEquals(Byte.MIN_VALUE, Suppliers.supplyByte(Byte.MIN_VALUE).getAsByte());
    }

    /**
     * Tests {@link Suppliers#supplyDouble(double)}.
     */
    @Test
    void testSupplyDouble() {
        assertEquals(0d, Suppliers.supplyDouble(0d).getAsDouble());
        assertEquals(Double.MAX_VALUE, Suppliers.supplyDouble(Double.MAX_VALUE).getAsDouble());
        assertEquals(Double.MIN_VALUE, Suppliers.supplyDouble(Double.MIN_VALUE).getAsDouble());
        assertEquals(Double.NaN, Suppliers.supplyDouble(Double.NaN).getAsDouble());
    }

    /**
     * Tests {@link Suppliers#supplyInt(int)}.
     */
    @Test
    void testSupplyInt() {
        assertEquals(0, Suppliers.supplyInt(0).getAsInt());
        assertEquals(Integer.MAX_VALUE, Suppliers.supplyInt(Integer.MAX_VALUE).getAsInt());
        assertEquals(Integer.MIN_VALUE, Suppliers.supplyInt(Integer.MIN_VALUE).getAsInt());
    }

    /**
     * Tests {@link Suppliers#supplyLong(long)}.
     */
    @Test
    void testSupplyLong() {
        assertEquals(0L, Suppliers.supplyLong(0L).getAsLong());
        assertEquals(Long.MAX_VALUE, Suppliers.supplyLong(Long.MAX_VALUE).getAsLong());
        assertEquals(Long.MIN_VALUE, Suppliers.supplyLong(Long.MIN_VALUE).getAsLong());
    }
}
