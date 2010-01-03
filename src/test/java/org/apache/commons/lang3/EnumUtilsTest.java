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

import java.util.List;
import java.util.Map;

import junit.framework.TestCase;

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
    }

    public void test_getEnum() {
        assertEquals(Traffic.RED, EnumUtils.getEnum(Traffic.class, "RED"));
        assertEquals(Traffic.AMBER, EnumUtils.getEnum(Traffic.class, "AMBER"));
        assertEquals(Traffic.GREEN, EnumUtils.getEnum(Traffic.class, "GREEN"));
        assertEquals(null, EnumUtils.getEnum(Traffic.class, "PURPLE"));
    }

}

enum Traffic {
    RED, AMBER, GREEN
}
