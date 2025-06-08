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
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

/**
 * Tests {@link JavaVersion}.
 */
class JavaVersionTest extends AbstractLangTest {

    @Test
    void testAtLeast() {
        assertFalse(JavaVersion.JAVA_1_2.atLeast(JavaVersion.JAVA_1_5), "1.2 at least 1.5 passed");
        assertTrue(JavaVersion.JAVA_1_5.atLeast(JavaVersion.JAVA_1_2), "1.5 at least 1.2 failed");
        assertFalse(JavaVersion.JAVA_1_6.atLeast(JavaVersion.JAVA_1_7), "1.6 at least 1.7 passed");

        assertTrue(JavaVersion.JAVA_0_9.atLeast(JavaVersion.JAVA_1_5), "0.9 at least 1.5 failed");
        assertFalse(JavaVersion.JAVA_0_9.atLeast(JavaVersion.JAVA_1_6), "0.9 at least 1.6 passed");
    }

    @Test
    void testGetJavaVersion() throws Exception {
        assertEquals(JavaVersion.JAVA_0_9, JavaVersion.get("0.9"), "0.9 failed");
        assertEquals(JavaVersion.JAVA_1_1, JavaVersion.get("1.1"), "1.1 failed");
        assertEquals(JavaVersion.JAVA_1_2, JavaVersion.get("1.2"), "1.2 failed");
        assertEquals(JavaVersion.JAVA_1_3, JavaVersion.get("1.3"), "1.3 failed");
        assertEquals(JavaVersion.JAVA_1_4, JavaVersion.get("1.4"), "1.4 failed");
        assertEquals(JavaVersion.JAVA_1_5, JavaVersion.get("1.5"), "1.5 failed");
        assertEquals(JavaVersion.JAVA_1_6, JavaVersion.get("1.6"), "1.6 failed");
        assertEquals(JavaVersion.JAVA_1_7, JavaVersion.get("1.7"), "1.7 failed");
        assertEquals(JavaVersion.JAVA_1_8, JavaVersion.get("1.8"), "1.8 failed");
        assertEquals(JavaVersion.JAVA_9, JavaVersion.get("9"));
        assertEquals(JavaVersion.JAVA_10, JavaVersion.get("10"));
        assertEquals(JavaVersion.JAVA_11, JavaVersion.get("11"));
        assertEquals(JavaVersion.JAVA_12, JavaVersion.get("12"));
        assertEquals(JavaVersion.JAVA_13, JavaVersion.get("13"));
        assertEquals(JavaVersion.JAVA_14, JavaVersion.get("14"));
        assertEquals(JavaVersion.JAVA_15, JavaVersion.get("15"));
        assertEquals(JavaVersion.JAVA_16, JavaVersion.get("16"));
        assertEquals(JavaVersion.JAVA_17, JavaVersion.get("17"));
        assertEquals(JavaVersion.JAVA_18, JavaVersion.get("18"));
        assertEquals(JavaVersion.JAVA_19, JavaVersion.get("19"));
        assertEquals(JavaVersion.JAVA_20, JavaVersion.get("20"));
        assertEquals(JavaVersion.JAVA_21, JavaVersion.get("21"));
        assertEquals(JavaVersion.JAVA_22, JavaVersion.get("22"));
        assertEquals(JavaVersion.JAVA_23, JavaVersion.get("23"));

        assertEquals(JavaVersion.JAVA_RECENT, JavaVersion.get("1.10"), "1.10 failed");
        // assertNull("2.10 unexpectedly worked", JavaVersion.get("2.10"));
        assertEquals(JavaVersion.get("1.5"), JavaVersion.getJavaVersion("1.5"), "Wrapper method failed");
        assertEquals(JavaVersion.JAVA_RECENT, JavaVersion.get("24"), "Unhandled"); // LANG-1384
    }

    @Test
    void testToString() {
        assertEquals("1.2", JavaVersion.JAVA_1_2.toString());
    }

}
