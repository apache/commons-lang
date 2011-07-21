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

import java.util.EnumSet;
import java.util.List;
import java.util.Map;

import junit.framework.TestCase;

/**
 * 
 * @version $Id$
 */
public class EnumUtilsTest extends TestCase {

    public void testConstructable() {
        // enforce public constructor
        new EnumUtils();
    }

    public void test_getEnumMap() {
        Map<String, Traffic> test = EnumUtils.getEnumMap(Traffic.class);
        assertEquals( "getEnumMap not created correctly", "{RED=RED, AMBER=AMBER, GREEN=GREEN}", test.toString());
        assertEquals(3, test.size());
        assertEquals(true, test.containsKey("RED"));
        assertEquals(Traffic.RED, test.get("RED"));
        assertEquals(true, test.containsKey("AMBER"));
        assertEquals(Traffic.AMBER, test.get("AMBER"));
        assertEquals(true, test.containsKey("GREEN"));
        assertEquals(Traffic.GREEN, test.get("GREEN"));
        assertEquals(false, test.containsKey("PURPLE"));
    }

    public void test_getEnumList() {
        List<Traffic> test = EnumUtils.getEnumList(Traffic.class);
        assertEquals(3, test.size());
        assertEquals(Traffic.RED, test.get(0));
        assertEquals(Traffic.AMBER, test.get(1));
        assertEquals(Traffic.GREEN, test.get(2));
    }

    public void test_isEnum() {
        assertEquals(true, EnumUtils.isValidEnum(Traffic.class, "RED"));
        assertEquals(true, EnumUtils.isValidEnum(Traffic.class, "AMBER"));
        assertEquals(true, EnumUtils.isValidEnum(Traffic.class, "GREEN"));
        assertEquals(false, EnumUtils.isValidEnum(Traffic.class, "PURPLE"));
        assertEquals(false, EnumUtils.isValidEnum(Traffic.class, null));
    }

    public void test_isEnum_nullClass() {
        try {
            EnumUtils.isValidEnum((Class<Traffic>) null, "PURPLE");
            fail();
        } catch (NullPointerException ex) {
            // ok
        }
    }

    public void test_getEnum() {
        assertEquals(Traffic.RED, EnumUtils.getEnum(Traffic.class, "RED"));
        assertEquals(Traffic.AMBER, EnumUtils.getEnum(Traffic.class, "AMBER"));
        assertEquals(Traffic.GREEN, EnumUtils.getEnum(Traffic.class, "GREEN"));
        assertEquals(null, EnumUtils.getEnum(Traffic.class, "PURPLE"));
        assertEquals(null, EnumUtils.getEnum(Traffic.class, null));
    }

    public void test_getEnum_nullClass() {
        try {
            EnumUtils.getEnum((Class<Traffic>) null, "PURPLE");
            fail();
        } catch (NullPointerException ex) {
            // ok
        }
    }

    public void test_generateBitVector_nullClass() {
        try {
            EnumUtils.generateBitVector(null, EnumSet.of(Traffic.RED));
        } catch (IllegalArgumentException ex) {
            // ok
        }
    }

    public void test_generateBitVector_longClass() {
        try {
            EnumUtils.generateBitVector(TooMany.class, EnumSet.of(TooMany.A1));
        } catch (IllegalArgumentException ex) {
            // ok
        }
    }

    public void test_generateBitVector() {
        assertEquals(0L, EnumUtils.generateBitVector(Traffic.class, null));
        assertEquals(0L, EnumUtils.generateBitVector(Traffic.class, EnumSet.noneOf(Traffic.class)));
        assertEquals(1L, EnumUtils.generateBitVector(Traffic.class, EnumSet.of(Traffic.RED)));
        assertEquals(2L, EnumUtils.generateBitVector(Traffic.class, EnumSet.of(Traffic.AMBER)));
        assertEquals(4L, EnumUtils.generateBitVector(Traffic.class, EnumSet.of(Traffic.GREEN)));
        assertEquals(3L, EnumUtils.generateBitVector(Traffic.class, EnumSet.of(Traffic.RED, Traffic.AMBER)));
        assertEquals(5L, EnumUtils.generateBitVector(Traffic.class, EnumSet.of(Traffic.RED, Traffic.GREEN)));
        assertEquals(6L, EnumUtils.generateBitVector(Traffic.class, EnumSet.of(Traffic.AMBER, Traffic.GREEN)));
        assertEquals(7L, EnumUtils.generateBitVector(Traffic.class, EnumSet.of(Traffic.RED, Traffic.AMBER, Traffic.GREEN)));
    }

    public void test_processBitVector_nullClass() {
        final Class<Traffic> empty = null;
        try {
            EnumUtils.processBitVector(empty, 0L);
        } catch (IllegalArgumentException ex) {
            // ok
        }
    }

    public void test_processBitVector_longClass() {
        try {
            EnumUtils.processBitVector(TooMany.class, 0L);
        } catch (IllegalArgumentException ex) {
            // ok
        }
    }

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

enum TooMany{
    A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,
    A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,Q1,R1,S1,T1,U1,V1,W1,X1,Y1,Z1;

}
