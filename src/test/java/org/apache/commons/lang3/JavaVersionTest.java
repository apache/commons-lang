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

import static org.apache.commons.lang3.JavaVersion.JAVA_0_9;
import static org.apache.commons.lang3.JavaVersion.JAVA_1_1;
import static org.apache.commons.lang3.JavaVersion.JAVA_1_2;
import static org.apache.commons.lang3.JavaVersion.JAVA_1_3;
import static org.apache.commons.lang3.JavaVersion.JAVA_1_4;
import static org.apache.commons.lang3.JavaVersion.JAVA_1_5;
import static org.apache.commons.lang3.JavaVersion.JAVA_1_6;
import static org.apache.commons.lang3.JavaVersion.JAVA_1_7;
import static org.apache.commons.lang3.JavaVersion.JAVA_1_8;
import static org.apache.commons.lang3.JavaVersion.JAVA_RECENT;
import static org.apache.commons.lang3.JavaVersion.get;
import static org.apache.commons.lang3.JavaVersion.getJavaVersion;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

/**
 * Unit tests {@link org.apache.commons.lang3.JavaVersion}.
 */
public class JavaVersionTest extends AbstractLangTest {

    @Test
    public void testGetJavaVersion() throws Exception {
        assertEquals(JAVA_0_9, get("0.9"), "0.9 failed");
        assertEquals(JAVA_1_1, get("1.1"), "1.1 failed");
        assertEquals(JAVA_1_2, get("1.2"), "1.2 failed");
        assertEquals(JAVA_1_3, get("1.3"), "1.3 failed");
        assertEquals(JAVA_1_4, get("1.4"), "1.4 failed");
        assertEquals(JAVA_1_5, get("1.5"), "1.5 failed");
        assertEquals(JAVA_1_6, get("1.6"), "1.6 failed");
        assertEquals(JAVA_1_7, get("1.7"), "1.7 failed");
        assertEquals(JAVA_1_8, get("1.8"), "1.8 failed");

        int lastSupportedVersion = Integer.parseInt(JavaVersion.values()[JavaVersion.values().length - 2].toString());
        for (int i = 9; i <= lastSupportedVersion; i++) {
            assertEquals(JavaVersion.class.getField("JAVA_" + i).get(null), get("" + i), i + " failed");
        }

        assertEquals(JAVA_RECENT, get("1.10"), "1.10 failed");
        // assertNull("2.10 unexpectedly worked", get("2.10"));
        assertEquals(get("1.5"), getJavaVersion("1.5"), "Wrapper method failed");
        assertEquals(JAVA_RECENT, get("22"), "Unhandled"); // LANG-1384
    }

    @Test
    public void testAtLeast() {
        assertFalse(JAVA_1_2.atLeast(JAVA_1_5), "1.2 at least 1.5 passed");
        assertTrue(JAVA_1_5.atLeast(JAVA_1_2), "1.5 at least 1.2 failed");
        assertFalse(JAVA_1_6.atLeast(JAVA_1_7), "1.6 at least 1.7 passed");

        assertTrue(JAVA_0_9.atLeast(JAVA_1_5), "0.9 at least 1.5 failed");
        assertFalse(JAVA_0_9.atLeast(JAVA_1_6), "0.9 at least 1.6 passed");
    }

    @Test
    public void testToString() {
        assertEquals("1.2", JAVA_1_2.toString());
    }

}
