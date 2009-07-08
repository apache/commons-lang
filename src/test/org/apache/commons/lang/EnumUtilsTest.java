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
package org.apache.commons.lang;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

public class EnumUtilsTest extends TestCase {

    public static Test suite() {
        TestSuite suite = new TestSuite(EnumUtilsTest.class);
        suite.setName("EnumUtils Tests");
        return suite;
    }

    public void testConstructable() {
        // enforce public constructor
        new EnumUtils();
    }

    public void testGetEnumMap() {
        String toString = EnumUtils.getEnumMap(Traffic.class).toString(); 
        assertEquals( "getEnumMap not created correctly", "{RED=RED, AMBER=AMBER, GREEN=GREEN}", toString);
    }

}

enum Traffic {
    RED, AMBER, GREEN
}
