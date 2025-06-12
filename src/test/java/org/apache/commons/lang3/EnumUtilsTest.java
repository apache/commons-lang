/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.commons.lang3;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.function.ToIntFunction;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

enum Enum64 {
    A00, A01, A02, A03, A04, A05, A06, A07, A08, A09, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22,
    A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45,
    A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63
}

/**
 */
class EnumUtilsTest extends AbstractLangTest {

    private void assertArrayEquals(final long[] actual, final long... expected) {
        Assertions.assertArrayEquals(expected, actual);
    }

    @Test
    void testConstructable() {
        // enforce public constructor
        new EnumUtils();
    }

    @Test
    void testGenerateBitVector() {
        assertEquals(0L, EnumUtils.generateBitVector(Traffic.class, EnumSet.noneOf(Traffic.class)));
        assertEquals(1L, EnumUtils.generateBitVector(Traffic.class, EnumSet.of(Traffic.RED)));
        assertEquals(2L, EnumUtils.generateBitVector(Traffic.class, EnumSet.of(Traffic.AMBER)));
        assertEquals(4L, EnumUtils.generateBitVector(Traffic.class, EnumSet.of(Traffic.GREEN)));
        assertEquals(3L, EnumUtils.generateBitVector(Traffic.class, EnumSet.of(Traffic.RED, Traffic.AMBER)));
        assertEquals(5L, EnumUtils.generateBitVector(Traffic.class, EnumSet.of(Traffic.RED, Traffic.GREEN)));
        assertEquals(6L, EnumUtils.generateBitVector(Traffic.class, EnumSet.of(Traffic.AMBER, Traffic.GREEN)));
        assertEquals(7L,
            EnumUtils.generateBitVector(Traffic.class, EnumSet.of(Traffic.RED, Traffic.AMBER, Traffic.GREEN)));

        // 64 values Enum (to test whether no int<->long jdk conversion issue exists)
        assertEquals(1L << 31, EnumUtils.generateBitVector(Enum64.class, EnumSet.of(Enum64.A31)));
        assertEquals(1L << 32, EnumUtils.generateBitVector(Enum64.class, EnumSet.of(Enum64.A32)));
        assertEquals(1L << 63, EnumUtils.generateBitVector(Enum64.class, EnumSet.of(Enum64.A63)));
        assertEquals(Long.MIN_VALUE, EnumUtils.generateBitVector(Enum64.class, EnumSet.of(Enum64.A63)));
    }

    @Test
    void testGenerateBitVector_longClass() {
        assertThrows(IllegalArgumentException.class,
            () -> EnumUtils.generateBitVector(TooMany.class, EnumSet.of(TooMany.A1)));
    }

    @Test
    void testGenerateBitVector_longClassWithArray() {
        assertThrows(IllegalArgumentException.class, () -> EnumUtils.generateBitVector(TooMany.class, TooMany.A1));
    }

    @SuppressWarnings("unchecked")
    @Test
    void testGenerateBitVector_nonEnumClass() {
        @SuppressWarnings("rawtypes")
        final Class rawType = Object.class;
        @SuppressWarnings("rawtypes")
        final List rawList = new ArrayList();
        assertThrows(IllegalArgumentException.class, () -> EnumUtils.generateBitVector(rawType, rawList));
    }

    @SuppressWarnings("unchecked")
    @Test
    void testGenerateBitVector_nonEnumClassWithArray() {
        @SuppressWarnings("rawtypes")
        final Class rawType = Object.class;
        assertThrows(IllegalArgumentException.class, () -> EnumUtils.generateBitVector(rawType));
    }

    @Test
    void testGenerateBitVector_nullArray() {
        assertThrows(NullPointerException.class, () -> EnumUtils.generateBitVector(Traffic.class, (Traffic[]) null));
    }

    @Test
    void testGenerateBitVector_nullArrayElement() {
        assertThrows(IllegalArgumentException.class,
            () -> EnumUtils.generateBitVector(Traffic.class, Traffic.RED, null));
    }

    @Test
    void testGenerateBitVector_nullClass() {
        assertThrows(NullPointerException.class, () -> EnumUtils.generateBitVector(null, EnumSet.of(Traffic.RED)));
    }

    @Test
    void testGenerateBitVector_nullClassWithArray() {
        assertThrows(NullPointerException.class, () -> EnumUtils.generateBitVector(null, Traffic.RED));
    }

    @Test
    void testGenerateBitVector_nullElement() {
        assertThrows(NullPointerException.class,
            () -> EnumUtils.generateBitVector(Traffic.class, Arrays.asList(Traffic.RED, null)));
    }

    @Test
    void testGenerateBitVector_nullIterable() {
        assertThrows(NullPointerException.class,
            () -> EnumUtils.generateBitVector(Traffic.class, (Iterable<Traffic>) null));
    }

    @Test
    void testGenerateBitVectorFromArray() {
        assertEquals(0L, EnumUtils.generateBitVector(Traffic.class));
        assertEquals(1L, EnumUtils.generateBitVector(Traffic.class, Traffic.RED));
        assertEquals(2L, EnumUtils.generateBitVector(Traffic.class, Traffic.AMBER));
        assertEquals(4L, EnumUtils.generateBitVector(Traffic.class, Traffic.GREEN));
        assertEquals(3L, EnumUtils.generateBitVector(Traffic.class, Traffic.RED, Traffic.AMBER));
        assertEquals(5L, EnumUtils.generateBitVector(Traffic.class, Traffic.RED, Traffic.GREEN));
        assertEquals(6L, EnumUtils.generateBitVector(Traffic.class, Traffic.AMBER, Traffic.GREEN));
        assertEquals(7L, EnumUtils.generateBitVector(Traffic.class, Traffic.RED, Traffic.AMBER, Traffic.GREEN));
        // gracefully handles duplicates:
        assertEquals(7L,
            EnumUtils.generateBitVector(Traffic.class, Traffic.RED, Traffic.AMBER, Traffic.GREEN, Traffic.GREEN));

        // 64 values Enum (to test whether no int<->long jdk conversion issue exists)
        assertEquals(1L << 31, EnumUtils.generateBitVector(Enum64.class, Enum64.A31));
        assertEquals(1L << 32, EnumUtils.generateBitVector(Enum64.class, Enum64.A32));
        assertEquals(1L << 63, EnumUtils.generateBitVector(Enum64.class, Enum64.A63));
        assertEquals(Long.MIN_VALUE, EnumUtils.generateBitVector(Enum64.class, Enum64.A63));
    }

    @Test
    void testGenerateBitVectors() {
        assertArrayEquals(EnumUtils.generateBitVectors(Traffic.class, EnumSet.noneOf(Traffic.class)), 0L);
        assertArrayEquals(EnumUtils.generateBitVectors(Traffic.class, EnumSet.of(Traffic.RED)), 1L);
        assertArrayEquals(EnumUtils.generateBitVectors(Traffic.class, EnumSet.of(Traffic.AMBER)), 2L);
        assertArrayEquals(EnumUtils.generateBitVectors(Traffic.class, EnumSet.of(Traffic.GREEN)), 4L);
        assertArrayEquals(EnumUtils.generateBitVectors(Traffic.class, EnumSet.of(Traffic.RED, Traffic.AMBER)), 3L);
        assertArrayEquals(EnumUtils.generateBitVectors(Traffic.class, EnumSet.of(Traffic.RED, Traffic.GREEN)), 5L);
        assertArrayEquals(EnumUtils.generateBitVectors(Traffic.class, EnumSet.of(Traffic.AMBER, Traffic.GREEN)), 6L);
        assertArrayEquals(
            EnumUtils.generateBitVectors(Traffic.class, EnumSet.of(Traffic.RED, Traffic.AMBER, Traffic.GREEN)), 7L);

        // 64 values Enum (to test whether no int<->long jdk conversion issue exists)
        assertArrayEquals(EnumUtils.generateBitVectors(Enum64.class, EnumSet.of(Enum64.A31)), 1L << 31);
        assertArrayEquals(EnumUtils.generateBitVectors(Enum64.class, EnumSet.of(Enum64.A32)), 1L << 32);
        assertArrayEquals(EnumUtils.generateBitVectors(Enum64.class, EnumSet.of(Enum64.A63)), 1L << 63);
        assertArrayEquals(EnumUtils.generateBitVectors(Enum64.class, EnumSet.of(Enum64.A63)), Long.MIN_VALUE);

        // More than 64 values Enum
        assertArrayEquals(EnumUtils.generateBitVectors(TooMany.class, EnumSet.of(TooMany.M2)), 1L, 0L);
        assertArrayEquals(EnumUtils.generateBitVectors(TooMany.class, EnumSet.of(TooMany.L2, TooMany.M2)), 1L,
            1L << 63);
    }

    @SuppressWarnings("unchecked")
    @Test
    void testGenerateBitVectors_nonEnumClass() {
        @SuppressWarnings("rawtypes")
        final Class rawType = Object.class;
        @SuppressWarnings("rawtypes")
        final List rawList = new ArrayList();
        assertThrows(IllegalArgumentException.class, () -> EnumUtils.generateBitVectors(rawType, rawList));
    }

    @SuppressWarnings("unchecked")
    @Test
    void testGenerateBitVectors_nonEnumClassWithArray() {
        @SuppressWarnings("rawtypes")
        final Class rawType = Object.class;
        assertThrows(IllegalArgumentException.class, () -> EnumUtils.generateBitVectors(rawType));
    }

    @Test
    void testGenerateBitVectors_nullArray() {
        assertThrows(NullPointerException.class, () -> EnumUtils.generateBitVectors(Traffic.class, (Traffic[]) null));
    }

    @Test
    void testGenerateBitVectors_nullArrayElement() {
        assertThrows(IllegalArgumentException.class,
            () -> EnumUtils.generateBitVectors(Traffic.class, Traffic.RED, null));
    }

    @Test
    void testGenerateBitVectors_nullClass() {
        assertThrows(NullPointerException.class, () -> EnumUtils.generateBitVectors(null, EnumSet.of(Traffic.RED)));
    }

    @Test
    void testGenerateBitVectors_nullClassWithArray() {
        assertThrows(NullPointerException.class, () -> EnumUtils.generateBitVectors(null, Traffic.RED));
    }

    @Test
    void testGenerateBitVectors_nullElement() {
        assertThrows(NullPointerException.class,
            () -> EnumUtils.generateBitVectors(Traffic.class, Arrays.asList(Traffic.RED, null)));
    }

    @Test
    void testGenerateBitVectors_nullIterable() {
        assertThrows(NullPointerException.class, () -> EnumUtils.generateBitVectors(null, (Iterable<Traffic>) null));
    }

    @Test
    void testGenerateBitVectorsFromArray() {
        assertArrayEquals(EnumUtils.generateBitVectors(Traffic.class), 0L);
        assertArrayEquals(EnumUtils.generateBitVectors(Traffic.class, Traffic.RED), 1L);
        assertArrayEquals(EnumUtils.generateBitVectors(Traffic.class, Traffic.AMBER), 2L);
        assertArrayEquals(EnumUtils.generateBitVectors(Traffic.class, Traffic.GREEN), 4L);
        assertArrayEquals(EnumUtils.generateBitVectors(Traffic.class, Traffic.RED, Traffic.AMBER), 3L);
        assertArrayEquals(EnumUtils.generateBitVectors(Traffic.class, Traffic.RED, Traffic.GREEN), 5L);
        assertArrayEquals(EnumUtils.generateBitVectors(Traffic.class, Traffic.AMBER, Traffic.GREEN), 6L);
        assertArrayEquals(EnumUtils.generateBitVectors(Traffic.class, Traffic.RED, Traffic.AMBER, Traffic.GREEN), 7L);
        // gracefully handles duplicates:
        assertArrayEquals(
            EnumUtils.generateBitVectors(Traffic.class, Traffic.RED, Traffic.AMBER, Traffic.GREEN, Traffic.GREEN), 7L);

        // 64 values Enum (to test whether no int<->long jdk conversion issue exists)
        assertArrayEquals(EnumUtils.generateBitVectors(Enum64.class, Enum64.A31), 1L << 31);
        assertArrayEquals(EnumUtils.generateBitVectors(Enum64.class, Enum64.A32), 1L << 32);
        assertArrayEquals(EnumUtils.generateBitVectors(Enum64.class, Enum64.A63), 1L << 63);
        assertArrayEquals(EnumUtils.generateBitVectors(Enum64.class, Enum64.A63), Long.MIN_VALUE);

        // More than 64 values Enum
        assertArrayEquals(EnumUtils.generateBitVectors(TooMany.class, TooMany.M2), 1L, 0L);
        assertArrayEquals(EnumUtils.generateBitVectors(TooMany.class, TooMany.L2, TooMany.M2), 1L, 1L << 63);

    }

    @Test
    void testGetEnum() {
        assertEquals(Traffic.RED, EnumUtils.getEnum(Traffic.class, "RED"));
        assertEquals(Traffic.AMBER, EnumUtils.getEnum(Traffic.class, "AMBER"));
        assertEquals(Traffic.GREEN, EnumUtils.getEnum(Traffic.class, "GREEN"));
        assertNull(EnumUtils.getEnum(Traffic.class, "PURPLE"));
        assertNull(EnumUtils.getEnum(Traffic.class, null));
    }

    @Test
    void testGetEnum_defaultEnum() {
        assertEquals(Traffic.RED, EnumUtils.getEnum(Traffic.class, "RED", Traffic.AMBER));
        assertEquals(Traffic.AMBER, EnumUtils.getEnum(Traffic.class, "AMBER", Traffic.GREEN));
        assertEquals(Traffic.GREEN, EnumUtils.getEnum(Traffic.class, "GREEN", Traffic.RED));
        assertEquals(Traffic.AMBER, EnumUtils.getEnum(Traffic.class, "PURPLE", Traffic.AMBER));
        assertEquals(Traffic.GREEN, EnumUtils.getEnum(Traffic.class, "PURPLE", Traffic.GREEN));
        assertEquals(Traffic.RED, EnumUtils.getEnum(Traffic.class, "PURPLE", Traffic.RED));
        assertEquals(Traffic.AMBER, EnumUtils.getEnum(Traffic.class, null, Traffic.AMBER));
        assertEquals(Traffic.GREEN, EnumUtils.getEnum(Traffic.class, null, Traffic.GREEN));
        assertEquals(Traffic.RED, EnumUtils.getEnum(Traffic.class, null, Traffic.RED));
        assertNull(EnumUtils.getEnum(Traffic.class, "PURPLE", null));
    }

    /**
     * Tests raw type.
     */
    @SuppressWarnings("unchecked")
    @Test
    void testGetEnum_nonEnumClass() {
        @SuppressWarnings("rawtypes")
        final Class rawType = Object.class;
        assertNull(EnumUtils.getEnum(rawType, "rawType"));
    }

    @Test
    void testGetEnum_nullClass() {
        assertThrows(NullPointerException.class, () -> EnumUtils.getEnum((Class<Traffic>) null, "PURPLE"));
    }

    @Test
    void testGetEnumIgnoreCase() {
        assertEquals(Traffic.RED, EnumUtils.getEnumIgnoreCase(Traffic.class, "red"));
        assertEquals(Traffic.AMBER, EnumUtils.getEnumIgnoreCase(Traffic.class, "Amber"));
        assertEquals(Traffic.GREEN, EnumUtils.getEnumIgnoreCase(Traffic.class, "grEEn"));
        assertNull(EnumUtils.getEnumIgnoreCase(Traffic.class, "purple"));
        assertNull(EnumUtils.getEnumIgnoreCase(Traffic.class, null));
    }

    @Test
    void testGetEnumIgnoreCase_defaultEnum() {
        assertEquals(Traffic.RED, EnumUtils.getEnumIgnoreCase(Traffic.class, "red", Traffic.AMBER));
        assertEquals(Traffic.AMBER, EnumUtils.getEnumIgnoreCase(Traffic.class, "Amber", Traffic.GREEN));
        assertEquals(Traffic.GREEN, EnumUtils.getEnumIgnoreCase(Traffic.class, "grEEn", Traffic.RED));
        assertEquals(Traffic.AMBER, EnumUtils.getEnumIgnoreCase(Traffic.class, "PURPLE", Traffic.AMBER));
        assertEquals(Traffic.GREEN, EnumUtils.getEnumIgnoreCase(Traffic.class, "purple", Traffic.GREEN));
        assertEquals(Traffic.RED, EnumUtils.getEnumIgnoreCase(Traffic.class, "pUrPlE", Traffic.RED));
        assertEquals(Traffic.AMBER, EnumUtils.getEnumIgnoreCase(Traffic.class, null, Traffic.AMBER));
        assertEquals(Traffic.GREEN, EnumUtils.getEnumIgnoreCase(Traffic.class, null, Traffic.GREEN));
        assertEquals(Traffic.RED, EnumUtils.getEnumIgnoreCase(Traffic.class, null, Traffic.RED));
        assertNull(EnumUtils.getEnumIgnoreCase(Traffic.class, "PURPLE", null));
    }

    /**
     * Tests raw type.
     */
    @SuppressWarnings("unchecked")
    @Test
    void testGetEnumIgnoreCase_nonEnumClass() {
        @SuppressWarnings("rawtypes")
        final Class rawType = Object.class;
        assertNull(EnumUtils.getEnumIgnoreCase(rawType, "rawType"));
    }

    @Test
    void testGetEnumIgnoreCase_nullClass() {
        assertThrows(NullPointerException.class, () -> EnumUtils.getEnumIgnoreCase((Class<Traffic>) null, "PURPLE"));
    }

    @Test
    void testGetEnumList() {
        final List<Traffic> test = EnumUtils.getEnumList(Traffic.class);
        assertEquals(3, test.size());
        assertEquals(Traffic.RED, test.get(0));
        assertEquals(Traffic.AMBER, test.get(1));
        assertEquals(Traffic.GREEN, test.get(2));
    }

    @Test
    void testGetEnumMap() {
        final Map<String, Traffic> test = EnumUtils.getEnumMap(Traffic.class);
        final Map<String, Traffic> expected = new HashMap<>();
        expected.put("RED", Traffic.RED);
        expected.put("AMBER", Traffic.AMBER);
        expected.put("GREEN", Traffic.GREEN);
        assertEquals(expected, test, "getEnumMap not created correctly");
        assertEquals(3, test.size());
        assertTrue(test.containsKey("RED"));
        assertEquals(Traffic.RED, test.get("RED"));
        assertTrue(test.containsKey("AMBER"));
        assertEquals(Traffic.AMBER, test.get("AMBER"));
        assertTrue(test.containsKey("GREEN"));
        assertEquals(Traffic.GREEN, test.get("GREEN"));
        assertFalse(test.containsKey("PURPLE"));
    }

    @Test
    void testGetEnumMap_keyFunction() {
        final Map<Integer, Month> test = EnumUtils.getEnumMap(Month.class, Month::getId);
        final Map<Integer, Month> expected = new HashMap<>();
        expected.put(1, Month.JAN);
        expected.put(2, Month.FEB);
        expected.put(3, Month.MAR);
        expected.put(4, Month.APR);
        expected.put(5, Month.MAY);
        expected.put(6, Month.JUN);
        expected.put(7, Month.JUL);
        expected.put(8, Month.AUG);
        expected.put(9, Month.SEP);
        expected.put(10, Month.OCT);
        expected.put(11, Month.NOV);
        expected.put(12, Month.DEC);
        assertEquals(expected, test, "getEnumMap not created correctly");
        assertEquals(12, test.size());
        assertFalse(test.containsKey(0));
        assertTrue(test.containsKey(1));
        assertEquals(Month.JAN, test.get(1));
        assertTrue(test.containsKey(2));
        assertEquals(Month.FEB, test.get(2));
        assertTrue(test.containsKey(3));
        assertEquals(Month.MAR, test.get(3));
        assertTrue(test.containsKey(4));
        assertEquals(Month.APR, test.get(4));
        assertTrue(test.containsKey(5));
        assertEquals(Month.MAY, test.get(5));
        assertTrue(test.containsKey(6));
        assertEquals(Month.JUN, test.get(6));
        assertTrue(test.containsKey(7));
        assertEquals(Month.JUL, test.get(7));
        assertTrue(test.containsKey(8));
        assertEquals(Month.AUG, test.get(8));
        assertTrue(test.containsKey(9));
        assertEquals(Month.SEP, test.get(9));
        assertTrue(test.containsKey(10));
        assertEquals(Month.OCT, test.get(10));
        assertTrue(test.containsKey(11));
        assertEquals(Month.NOV, test.get(11));
        assertTrue(test.containsKey(12));
        assertEquals(Month.DEC, test.get(12));
        assertFalse(test.containsKey(13));
    }

    @Test
    void testGetEnumSystemProperty() {
        final String key = getClass().getName();
        System.setProperty(key, Traffic.RED.toString());
        try {
            assertEquals(Traffic.RED, EnumUtils.getEnumSystemProperty(Traffic.class, key, null));
            assertEquals(Traffic.RED, EnumUtils.getEnumSystemProperty(Traffic.class, "?", Traffic.RED));
            assertEquals(Traffic.RED, EnumUtils.getEnumSystemProperty(null, null, Traffic.RED));
            assertEquals(Traffic.RED, EnumUtils.getEnumSystemProperty(null, "?", Traffic.RED));
            assertEquals(Traffic.RED, EnumUtils.getEnumSystemProperty(Traffic.class, null, Traffic.RED));
        } finally {
            System.getProperties().remove(key);
        }
    }

    @Test
    void testGetFirstEnumIgnoreCase_defaultEnum() {
        final Function<Traffic2, String> f = Traffic2::getLabel;
        assertEquals(Traffic2.RED, EnumUtils.getFirstEnumIgnoreCase(Traffic2.class, "***red***", f, Traffic2.AMBER));
        assertEquals(Traffic2.AMBER, EnumUtils.getFirstEnumIgnoreCase(Traffic2.class, "**Amber**", f, Traffic2.GREEN));
        assertEquals(Traffic2.GREEN, EnumUtils.getFirstEnumIgnoreCase(Traffic2.class, "*grEEn*", f, Traffic2.RED));
        assertEquals(Traffic2.AMBER, EnumUtils.getFirstEnumIgnoreCase(Traffic2.class, "PURPLE", f, Traffic2.AMBER));
        assertEquals(Traffic2.GREEN, EnumUtils.getFirstEnumIgnoreCase(Traffic2.class, "purple", f, Traffic2.GREEN));
        assertEquals(Traffic2.RED, EnumUtils.getFirstEnumIgnoreCase(Traffic2.class, "pUrPlE", f, Traffic2.RED));
        assertEquals(Traffic2.AMBER, EnumUtils.getFirstEnumIgnoreCase(Traffic2.class, null, f, Traffic2.AMBER));
        assertEquals(Traffic2.GREEN, EnumUtils.getFirstEnumIgnoreCase(Traffic2.class, null, f, Traffic2.GREEN));
        assertEquals(Traffic2.RED, EnumUtils.getFirstEnumIgnoreCase(Traffic2.class, null, f, Traffic2.RED));
        assertNull(EnumUtils.getFirstEnumIgnoreCase(Traffic2.class, "PURPLE", f, null));
    }

    @Test
    void testGetFirstEnumToIntFunction() {
        final ToIntFunction<Traffic2> f = Traffic2::getValue;
        assertEquals(Traffic2.RED, EnumUtils.getFirstEnum(Traffic2.class, 1, f, Traffic2.AMBER));
        assertEquals(Traffic2.AMBER, EnumUtils.getFirstEnum(Traffic2.class, 2, f, Traffic2.GREEN));
        assertEquals(Traffic2.GREEN, EnumUtils.getFirstEnum(Traffic2.class, 3, f, Traffic2.RED));
        assertEquals(Traffic2.AMBER, EnumUtils.getFirstEnum(Traffic2.class, 4, f, Traffic2.AMBER));
        assertEquals(Traffic2.GREEN, EnumUtils.getFirstEnum(Traffic2.class, 5, f, Traffic2.GREEN));
        assertEquals(Traffic2.RED, EnumUtils.getFirstEnum(Traffic2.class, 6, f, Traffic2.RED));
        assertEquals(Traffic2.AMBER, EnumUtils.getFirstEnum(Traffic2.class, 0, f, Traffic2.AMBER));
        assertEquals(Traffic2.GREEN, EnumUtils.getFirstEnum(Traffic2.class, -1, f, Traffic2.GREEN));
        assertEquals(Traffic2.RED, EnumUtils.getFirstEnum(Traffic2.class, 0, f, Traffic2.RED));
        assertNull(EnumUtils.getFirstEnum(Traffic2.class, 7, f, null));
    }

    @Test
    void testIsValidEnum() {
        assertTrue(EnumUtils.isValidEnum(Traffic.class, "RED"));
        assertTrue(EnumUtils.isValidEnum(Traffic.class, "AMBER"));
        assertTrue(EnumUtils.isValidEnum(Traffic.class, "GREEN"));
        assertFalse(EnumUtils.isValidEnum(Traffic.class, "PURPLE"));
        assertFalse(EnumUtils.isValidEnum(Traffic.class, null));
    }

    @Test
    void testIsValidEnum_nullClass() {
        assertThrows(NullPointerException.class, () -> EnumUtils.isValidEnum(null, "PURPLE"));
    }

    @Test
    void testIsValidEnumIgnoreCase() {
        assertTrue(EnumUtils.isValidEnumIgnoreCase(Traffic.class, "red"));
        assertTrue(EnumUtils.isValidEnumIgnoreCase(Traffic.class, "Amber"));
        assertTrue(EnumUtils.isValidEnumIgnoreCase(Traffic.class, "grEEn"));
        assertFalse(EnumUtils.isValidEnumIgnoreCase(Traffic.class, "purple"));
        assertFalse(EnumUtils.isValidEnumIgnoreCase(Traffic.class, null));
    }

    @Test
    void testIsValidEnumIgnoreCase_nullClass() {
        assertThrows(NullPointerException.class, () -> EnumUtils.isValidEnumIgnoreCase(null, "PURPLE"));
    }

    @Test
    void testProcessBitVector() {
        assertEquals(EnumSet.noneOf(Traffic.class), EnumUtils.processBitVector(Traffic.class, 0L));
        assertEquals(EnumSet.of(Traffic.RED), EnumUtils.processBitVector(Traffic.class, 1L));
        assertEquals(EnumSet.of(Traffic.AMBER), EnumUtils.processBitVector(Traffic.class, 2L));
        assertEquals(EnumSet.of(Traffic.RED, Traffic.AMBER), EnumUtils.processBitVector(Traffic.class, 3L));
        assertEquals(EnumSet.of(Traffic.GREEN), EnumUtils.processBitVector(Traffic.class, 4L));
        assertEquals(EnumSet.of(Traffic.RED, Traffic.GREEN), EnumUtils.processBitVector(Traffic.class, 5L));
        assertEquals(EnumSet.of(Traffic.AMBER, Traffic.GREEN), EnumUtils.processBitVector(Traffic.class, 6L));
        assertEquals(EnumSet.of(Traffic.RED, Traffic.AMBER, Traffic.GREEN),
            EnumUtils.processBitVector(Traffic.class, 7L));

        // 64 values Enum (to test whether no int<->long jdk conversion issue exists)
        assertEquals(EnumSet.of(Enum64.A31), EnumUtils.processBitVector(Enum64.class, 1L << 31));
        assertEquals(EnumSet.of(Enum64.A32), EnumUtils.processBitVector(Enum64.class, 1L << 32));
        assertEquals(EnumSet.of(Enum64.A63), EnumUtils.processBitVector(Enum64.class, 1L << 63));
        assertEquals(EnumSet.of(Enum64.A63), EnumUtils.processBitVector(Enum64.class, Long.MIN_VALUE));
    }

    @Test
    void testProcessBitVector_longClass() {
        assertThrows(IllegalArgumentException.class, () -> EnumUtils.processBitVector(TooMany.class, 0L));
    }

    @Test
    void testProcessBitVector_nullClass() {
        final Class<Traffic> empty = null;
        assertThrows(NullPointerException.class, () -> EnumUtils.processBitVector(empty, 0L));
    }

    @Test
    void testProcessBitVectors() {
        assertEquals(EnumSet.noneOf(Traffic.class), EnumUtils.processBitVectors(Traffic.class, 0L));
        assertEquals(EnumSet.of(Traffic.RED), EnumUtils.processBitVectors(Traffic.class, 1L));
        assertEquals(EnumSet.of(Traffic.AMBER), EnumUtils.processBitVectors(Traffic.class, 2L));
        assertEquals(EnumSet.of(Traffic.RED, Traffic.AMBER), EnumUtils.processBitVectors(Traffic.class, 3L));
        assertEquals(EnumSet.of(Traffic.GREEN), EnumUtils.processBitVectors(Traffic.class, 4L));
        assertEquals(EnumSet.of(Traffic.RED, Traffic.GREEN), EnumUtils.processBitVectors(Traffic.class, 5L));
        assertEquals(EnumSet.of(Traffic.AMBER, Traffic.GREEN), EnumUtils.processBitVectors(Traffic.class, 6L));
        assertEquals(EnumSet.of(Traffic.RED, Traffic.AMBER, Traffic.GREEN),
            EnumUtils.processBitVectors(Traffic.class, 7L));

        assertEquals(EnumSet.noneOf(Traffic.class), EnumUtils.processBitVectors(Traffic.class, 0L, 0L));
        assertEquals(EnumSet.of(Traffic.RED), EnumUtils.processBitVectors(Traffic.class, 0L, 1L));
        assertEquals(EnumSet.of(Traffic.AMBER), EnumUtils.processBitVectors(Traffic.class, 0L, 2L));
        assertEquals(EnumSet.of(Traffic.RED, Traffic.AMBER), EnumUtils.processBitVectors(Traffic.class, 0L, 3L));
        assertEquals(EnumSet.of(Traffic.GREEN), EnumUtils.processBitVectors(Traffic.class, 0L, 4L));
        assertEquals(EnumSet.of(Traffic.RED, Traffic.GREEN), EnumUtils.processBitVectors(Traffic.class, 0L, 5L));
        assertEquals(EnumSet.of(Traffic.AMBER, Traffic.GREEN), EnumUtils.processBitVectors(Traffic.class, 0L, 6L));
        assertEquals(EnumSet.of(Traffic.RED, Traffic.AMBER, Traffic.GREEN),
            EnumUtils.processBitVectors(Traffic.class, 0L, 7L));

        // demonstrate tolerance of irrelevant high-order digits:
        assertEquals(EnumSet.noneOf(Traffic.class), EnumUtils.processBitVectors(Traffic.class, 666L, 0L));
        assertEquals(EnumSet.of(Traffic.RED), EnumUtils.processBitVectors(Traffic.class, 666L, 1L));
        assertEquals(EnumSet.of(Traffic.AMBER), EnumUtils.processBitVectors(Traffic.class, 666L, 2L));
        assertEquals(EnumSet.of(Traffic.RED, Traffic.AMBER), EnumUtils.processBitVectors(Traffic.class, 666L, 3L));
        assertEquals(EnumSet.of(Traffic.GREEN), EnumUtils.processBitVectors(Traffic.class, 666L, 4L));
        assertEquals(EnumSet.of(Traffic.RED, Traffic.GREEN), EnumUtils.processBitVectors(Traffic.class, 666L, 5L));
        assertEquals(EnumSet.of(Traffic.AMBER, Traffic.GREEN), EnumUtils.processBitVectors(Traffic.class, 666L, 6L));
        assertEquals(EnumSet.of(Traffic.RED, Traffic.AMBER, Traffic.GREEN),
            EnumUtils.processBitVectors(Traffic.class, 666L, 7L));

        // 64 values Enum (to test whether no int<->long jdk conversion issue exists)
        assertEquals(EnumSet.of(Enum64.A31), EnumUtils.processBitVectors(Enum64.class, 1L << 31));
        assertEquals(EnumSet.of(Enum64.A32), EnumUtils.processBitVectors(Enum64.class, 1L << 32));
        assertEquals(EnumSet.of(Enum64.A63), EnumUtils.processBitVectors(Enum64.class, 1L << 63));
        assertEquals(EnumSet.of(Enum64.A63), EnumUtils.processBitVectors(Enum64.class, Long.MIN_VALUE));
    }

    @Test
    void testProcessBitVectors_longClass() {
        assertEquals(EnumSet.noneOf(TooMany.class), EnumUtils.processBitVectors(TooMany.class, 0L));
        assertEquals(EnumSet.of(TooMany.A), EnumUtils.processBitVectors(TooMany.class, 1L));
        assertEquals(EnumSet.of(TooMany.B), EnumUtils.processBitVectors(TooMany.class, 2L));
        assertEquals(EnumSet.of(TooMany.A, TooMany.B), EnumUtils.processBitVectors(TooMany.class, 3L));
        assertEquals(EnumSet.of(TooMany.C), EnumUtils.processBitVectors(TooMany.class, 4L));
        assertEquals(EnumSet.of(TooMany.A, TooMany.C), EnumUtils.processBitVectors(TooMany.class, 5L));
        assertEquals(EnumSet.of(TooMany.B, TooMany.C), EnumUtils.processBitVectors(TooMany.class, 6L));
        assertEquals(EnumSet.of(TooMany.A, TooMany.B, TooMany.C), EnumUtils.processBitVectors(TooMany.class, 7L));

        assertEquals(EnumSet.noneOf(TooMany.class), EnumUtils.processBitVectors(TooMany.class, 0L, 0L));
        assertEquals(EnumSet.of(TooMany.A), EnumUtils.processBitVectors(TooMany.class, 0L, 1L));
        assertEquals(EnumSet.of(TooMany.B), EnumUtils.processBitVectors(TooMany.class, 0L, 2L));
        assertEquals(EnumSet.of(TooMany.A, TooMany.B), EnumUtils.processBitVectors(TooMany.class, 0L, 3L));
        assertEquals(EnumSet.of(TooMany.C), EnumUtils.processBitVectors(TooMany.class, 0L, 4L));
        assertEquals(EnumSet.of(TooMany.A, TooMany.C), EnumUtils.processBitVectors(TooMany.class, 0L, 5L));
        assertEquals(EnumSet.of(TooMany.B, TooMany.C), EnumUtils.processBitVectors(TooMany.class, 0L, 6L));
        assertEquals(EnumSet.of(TooMany.A, TooMany.B, TooMany.C), EnumUtils.processBitVectors(TooMany.class, 0L, 7L));
        assertEquals(EnumSet.of(TooMany.A, TooMany.B, TooMany.C), EnumUtils.processBitVectors(TooMany.class, 0L, 7L));

        assertEquals(EnumSet.of(TooMany.M2), EnumUtils.processBitVectors(TooMany.class, 1L, 0L));
        assertEquals(EnumSet.of(TooMany.A, TooMany.M2), EnumUtils.processBitVectors(TooMany.class, 1L, 1L));
        assertEquals(EnumSet.of(TooMany.B, TooMany.M2), EnumUtils.processBitVectors(TooMany.class, 1L, 2L));
        assertEquals(EnumSet.of(TooMany.A, TooMany.B, TooMany.M2), EnumUtils.processBitVectors(TooMany.class, 1L, 3L));
        assertEquals(EnumSet.of(TooMany.C, TooMany.M2), EnumUtils.processBitVectors(TooMany.class, 1L, 4L));
        assertEquals(EnumSet.of(TooMany.A, TooMany.C, TooMany.M2), EnumUtils.processBitVectors(TooMany.class, 1L, 5L));
        assertEquals(EnumSet.of(TooMany.B, TooMany.C, TooMany.M2), EnumUtils.processBitVectors(TooMany.class, 1L, 6L));
        assertEquals(EnumSet.of(TooMany.A, TooMany.B, TooMany.C, TooMany.M2),
            EnumUtils.processBitVectors(TooMany.class, 1L, 7L));
        assertEquals(EnumSet.of(TooMany.A, TooMany.B, TooMany.C, TooMany.M2),
            EnumUtils.processBitVectors(TooMany.class, 1L, 7L));

        // demonstrate tolerance of irrelevant high-order digits:
        assertEquals(EnumSet.of(TooMany.M2), EnumUtils.processBitVectors(TooMany.class, 9L, 0L));
        assertEquals(EnumSet.of(TooMany.A, TooMany.M2), EnumUtils.processBitVectors(TooMany.class, 9L, 1L));
        assertEquals(EnumSet.of(TooMany.B, TooMany.M2), EnumUtils.processBitVectors(TooMany.class, 9L, 2L));
        assertEquals(EnumSet.of(TooMany.A, TooMany.B, TooMany.M2), EnumUtils.processBitVectors(TooMany.class, 9L, 3L));
        assertEquals(EnumSet.of(TooMany.C, TooMany.M2), EnumUtils.processBitVectors(TooMany.class, 9L, 4L));
        assertEquals(EnumSet.of(TooMany.A, TooMany.C, TooMany.M2), EnumUtils.processBitVectors(TooMany.class, 9L, 5L));
        assertEquals(EnumSet.of(TooMany.B, TooMany.C, TooMany.M2), EnumUtils.processBitVectors(TooMany.class, 9L, 6L));
        assertEquals(EnumSet.of(TooMany.A, TooMany.B, TooMany.C, TooMany.M2),
            EnumUtils.processBitVectors(TooMany.class, 9L, 7L));
        assertEquals(EnumSet.of(TooMany.A, TooMany.B, TooMany.C, TooMany.M2),
            EnumUtils.processBitVectors(TooMany.class, 9L, 7L));
    }

    @Test
    void testProcessBitVectors_nullClass() {
        final Class<Traffic> empty = null;
        assertThrows(NullPointerException.class, () -> EnumUtils.processBitVectors(empty, 0L));
    }

}

enum Month {
    JAN(1), FEB(2), MAR(3), APR(4), MAY(5), JUN(6), JUL(7), AUG(8), SEP(9), OCT(10), NOV(11), DEC(12);

    private final int id;

    Month(final int id) {
        this.id = id;
    }

    public int getId() {
        return this.id;
    }
}

enum TooMany {
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1,
    J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2,
    M2
}

enum Traffic {
    RED, AMBER, GREEN
}

enum Traffic2 {

    RED("***Red***", 1), AMBER("**Amber**", 2), GREEN("*green*", 3);

    final String label;
    final int value;

    Traffic2(final String label, final int value) {
        this.label = label;
        this.value = value;
    }

    public String getLabel() {
        return label;
    }

    public int getValue() {
        return value;
    }
}
