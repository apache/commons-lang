/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.commons.lang3;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;

import org.junit.Test;

/**
 * 
 * @version $Id$
 */
public class EnumUtilsTest {

    @Test
    public void testConstructable() {
        // enforce public constructor
        new EnumUtils();
    }

    @Test
    public void test_getEnumMap() {
        Map<String, Traffic> test = EnumUtils.getEnumMap(Traffic.class);
        assertEquals( "getEnumMap not created correctly", "{RED=RED, AMBER=AMBER, GREEN=GREEN}", test.toString());
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
    public void test_getEnumList() {
        List<Traffic> test = EnumUtils.getEnumList(Traffic.class);
        assertEquals(3, test.size());
        assertEquals(Traffic.RED, test.get(0));
        assertEquals(Traffic.AMBER, test.get(1));
        assertEquals(Traffic.GREEN, test.get(2));
    }

    @Test
    public void test_isEnum() {
        assertTrue(EnumUtils.isValidEnum(Traffic.class, "RED"));
        assertTrue(EnumUtils.isValidEnum(Traffic.class, "AMBER"));
        assertTrue(EnumUtils.isValidEnum(Traffic.class, "GREEN"));
        assertFalse(EnumUtils.isValidEnum(Traffic.class, "PURPLE"));
        assertFalse(EnumUtils.isValidEnum(Traffic.class, null));
    }

    @Test(expected=NullPointerException.class)
    public void test_isEnum_nullClass() {
        EnumUtils.isValidEnum((Class<Traffic>) null, "PURPLE");
    }

    @Test
    public void test_getEnum() {
        assertEquals(Traffic.RED, EnumUtils.getEnum(Traffic.class, "RED"));
        assertEquals(Traffic.AMBER, EnumUtils.getEnum(Traffic.class, "AMBER"));
        assertEquals(Traffic.GREEN, EnumUtils.getEnum(Traffic.class, "GREEN"));
        assertEquals(null, EnumUtils.getEnum(Traffic.class, "PURPLE"));
        assertEquals(null, EnumUtils.getEnum(Traffic.class, null));
    }

    @Test(expected=NullPointerException.class)
    public void test_getEnum_nullClass() {
        EnumUtils.getEnum((Class<Traffic>) null, "PURPLE");
    }

    @Test(expected=NullPointerException.class)
    public void test_generateBitVector_nullClass() {
        EnumUtils.generateBitVector(null, EnumSet.of(Traffic.RED));
    }

    @Test(expected=NullPointerException.class)
    public void test_generateBitVector_nullIterable() {
        EnumUtils.generateBitVector(null, (Iterable<Traffic>) null);
    }

    @Test(expected=NullPointerException.class)
    public void test_generateBitVector_nullClassWithArray() {
        EnumUtils.generateBitVector(null, Traffic.RED);
    }
    
    @Test(expected=NullPointerException.class)
    public void test_generateBitVector_nullArray() {
        EnumUtils.generateBitVector(null, (Traffic[]) null);
    }

    @Test(expected=IllegalArgumentException.class)
    public void test_generateBitVector_longClass() {
        EnumUtils.generateBitVector(TooMany.class, EnumSet.of(TooMany.A1));
    }

    @Test(expected=IllegalArgumentException.class)
    public void test_generateBitVector_longClassWithArray() {
        EnumUtils.generateBitVector(TooMany.class, TooMany.A1);
    }

    @SuppressWarnings("unchecked")
    @Test(expected=IllegalArgumentException.class)
    public void test_generateBitVector_nonEnumClass() {
        @SuppressWarnings("rawtypes")
        Class rawType = Object.class;
        @SuppressWarnings("rawtypes")
        List rawList = new ArrayList();
        EnumUtils.generateBitVector(rawType, rawList);
    }
    
    @SuppressWarnings("unchecked")
    @Test(expected=IllegalArgumentException.class)
    public void test_generateBitVector_nonEnumClassWithArray() {
        @SuppressWarnings("rawtypes")
        Class rawType = Object.class;
        EnumUtils.generateBitVector(rawType);
    }
    
    @Test
    public void test_generateBitVector() {
        assertEquals(0L, EnumUtils.generateBitVector(Traffic.class, EnumSet.noneOf(Traffic.class)));
        assertEquals(1L, EnumUtils.generateBitVector(Traffic.class, EnumSet.of(Traffic.RED)));
        assertEquals(2L, EnumUtils.generateBitVector(Traffic.class, EnumSet.of(Traffic.AMBER)));
        assertEquals(4L, EnumUtils.generateBitVector(Traffic.class, EnumSet.of(Traffic.GREEN)));
        assertEquals(3L, EnumUtils.generateBitVector(Traffic.class, EnumSet.of(Traffic.RED, Traffic.AMBER)));
        assertEquals(5L, EnumUtils.generateBitVector(Traffic.class, EnumSet.of(Traffic.RED, Traffic.GREEN)));
        assertEquals(6L, EnumUtils.generateBitVector(Traffic.class, EnumSet.of(Traffic.AMBER, Traffic.GREEN)));
        assertEquals(7L, EnumUtils.generateBitVector(Traffic.class, EnumSet.of(Traffic.RED, Traffic.AMBER, Traffic.GREEN)));
    }

    @Test
    public void test_generateBitVectorFromArray() {
        assertEquals(0L, EnumUtils.generateBitVector(Traffic.class));
        assertEquals(1L, EnumUtils.generateBitVector(Traffic.class, Traffic.RED));
        assertEquals(2L, EnumUtils.generateBitVector(Traffic.class, Traffic.AMBER));
        assertEquals(4L, EnumUtils.generateBitVector(Traffic.class, Traffic.GREEN));
        assertEquals(3L, EnumUtils.generateBitVector(Traffic.class, Traffic.RED, Traffic.AMBER));
        assertEquals(5L, EnumUtils.generateBitVector(Traffic.class, Traffic.RED, Traffic.GREEN));
        assertEquals(6L, EnumUtils.generateBitVector(Traffic.class, Traffic.AMBER, Traffic.GREEN));
        assertEquals(7L, EnumUtils.generateBitVector(Traffic.class, Traffic.RED, Traffic.AMBER, Traffic.GREEN));
        //gracefully handles duplicates:
        assertEquals(7L, EnumUtils.generateBitVector(Traffic.class, Traffic.RED, Traffic.AMBER, Traffic.GREEN, Traffic.GREEN));
    }
    
    @Test(expected=NullPointerException.class)
    public void test_processBitVector_nullClass() {
        final Class<Traffic> empty = null;
        EnumUtils.processBitVector(empty, 0L);
    }

    @Test(expected=IllegalArgumentException.class)
    public void test_processBitVector_longClass() {
        EnumUtils.processBitVector(TooMany.class, 0L);
    }

    @Test
    public void test_processBitVector() {
        assertEquals(EnumSet.noneOf(Traffic.class), EnumUtils.processBitVector(Traffic.class, 0L));
        assertEquals(EnumSet.of(Traffic.RED), EnumUtils.processBitVector(Traffic.class, 1L));
        assertEquals(EnumSet.of(Traffic.AMBER), EnumUtils.processBitVector(Traffic.class, 2L));
        assertEquals(EnumSet.of(Traffic.RED, Traffic.AMBER), EnumUtils.processBitVector(Traffic.class, 3L));
        assertEquals(EnumSet.of(Traffic.GREEN), EnumUtils.processBitVector(Traffic.class, 4L));
        assertEquals(EnumSet.of(Traffic.RED, Traffic.GREEN), EnumUtils.processBitVector(Traffic.class, 5L));
        assertEquals(EnumSet.of(Traffic.AMBER, Traffic.GREEN), EnumUtils.processBitVector(Traffic.class, 6L));
        assertEquals(EnumSet.of(Traffic.RED, Traffic.AMBER, Traffic.GREEN), EnumUtils.processBitVector(Traffic.class, 7L));
    }
}

enum Traffic {
    RED, AMBER, GREEN
}

enum TooMany {
    A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,
    A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,Q1,R1,S1,T1,U1,V1,W1,X1,Y1,Z1,
    A2,B2,C2,D2,E2,F2,G2,H2,I2,J2,K2,L2,M2;
}
