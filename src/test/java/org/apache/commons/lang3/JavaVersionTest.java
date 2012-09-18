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

import org.junit.Test;
import static org.junit.Assert.*;
import static org.apache.commons.lang3.JavaVersion.JAVA_0_9;
import static org.apache.commons.lang3.JavaVersion.JAVA_1_1;
import static org.apache.commons.lang3.JavaVersion.JAVA_1_2;
import static org.apache.commons.lang3.JavaVersion.JAVA_1_3;
import static org.apache.commons.lang3.JavaVersion.JAVA_1_4;
import static org.apache.commons.lang3.JavaVersion.JAVA_1_5;
import static org.apache.commons.lang3.JavaVersion.JAVA_1_6;
import static org.apache.commons.lang3.JavaVersion.JAVA_1_7;
import static org.apache.commons.lang3.JavaVersion.JAVA_1_8;
import static org.apache.commons.lang3.JavaVersion.get;
import static org.apache.commons.lang3.JavaVersion.getJavaVersion;

/**
 * Unit tests {@link org.apache.commons.lang3.JavaVersion}.
 *
 * @version $Id: JavaVersionTest.java 918366 2010-03-03 08:56:22Z bayard $
 */
public class JavaVersionTest {

    @Test
    public void testGetJavaVersion() {
        assertEquals("0.9 failed", JAVA_0_9, get("0.9"));
        assertEquals("1.1 failed", JAVA_1_1, get("1.1"));
        assertEquals("1.2 failed", JAVA_1_2, get("1.2"));
        assertEquals("1.3 failed", JAVA_1_3, get("1.3"));
        assertEquals("1.4 failed", JAVA_1_4, get("1.4"));
        assertEquals("1.5 failed", JAVA_1_5, get("1.5"));
        assertEquals("1.6 failed", JAVA_1_6, get("1.6"));
        assertEquals("1.7 failed", JAVA_1_7, get("1.7"));
        assertEquals("1.8 failed", JAVA_1_8, get("1.8"));
        assertNull("1.9 unexpectedly worked", get("1.9"));
        assertEquals("Wrapper method failed", get("1.5"), getJavaVersion("1.5"));
    }

    @Test
    public void testAtLeast() {
        assertFalse("1.2 at least 1.5 passed", JAVA_1_2.atLeast(JAVA_1_5));
        assertTrue("1.5 at least 1.2 failed", JAVA_1_5.atLeast(JAVA_1_2));
        assertFalse("1.6 at least 1.7 passed", JAVA_1_6.atLeast(JAVA_1_7));

        assertTrue("0.9 at least 1.5 failed", JAVA_0_9.atLeast(JAVA_1_5));
        assertFalse("0.9 at least 1.6 passed", JAVA_0_9.atLeast(JAVA_1_6));
    }

    @Test
    public void testToString() {
        assertEquals("1.2", JAVA_1_2.toString());
    }

}
