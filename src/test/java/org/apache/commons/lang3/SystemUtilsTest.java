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

import static org.apache.commons.lang3.JavaVersion.JAVA_1_1;
import static org.apache.commons.lang3.JavaVersion.JAVA_1_2;
import static org.apache.commons.lang3.JavaVersion.JAVA_1_3;
import static org.apache.commons.lang3.JavaVersion.JAVA_1_4;
import static org.apache.commons.lang3.JavaVersion.JAVA_1_5;
import static org.apache.commons.lang3.JavaVersion.JAVA_1_6;
import static org.apache.commons.lang3.JavaVersion.JAVA_1_7;
import static org.apache.commons.lang3.JavaVersion.JAVA_1_8;
import static org.apache.commons.lang3.JavaVersion.JAVA_9;
import static org.apache.commons.lang3.JavaVersion.JAVA_10;
import static org.apache.commons.lang3.JavaVersion.JAVA_11;
import static org.apache.commons.lang3.JavaVersion.JAVA_12;
import static org.apache.commons.lang3.JavaVersion.JAVA_13;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.util.Locale;

import org.junit.jupiter.api.Test;

/**
 * Unit tests {@link org.apache.commons.lang3.SystemUtils}.
 *
 * Only limited testing can be performed.
 */
public class SystemUtilsTest {

    @Test
    public void testConstructor() {
        assertNotNull(new SystemUtils());
        final Constructor<?>[] cons = SystemUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertTrue(Modifier.isPublic(cons[0].getModifiers()));
        assertTrue(Modifier.isPublic(SystemUtils.class.getModifiers()));
        assertFalse(Modifier.isFinal(SystemUtils.class.getModifiers()));
    }

    @Test
    public void testGetEnvironmentVariableAbsent() {
        final String name = "THIS_ENV_VAR_SHOULD_NOT_EXIST_FOR_THIS_TEST_TO_PASS";
        final String expected = System.getenv(name);
        assertNull(expected);
        final String value = SystemUtils.getEnvironmentVariable(name, "DEFAULT");
        assertEquals("DEFAULT", value);
    }

    @Test
    public void testGetEnvironmentVariablePresent() {
        final String name = "PATH";
        final String expected = System.getenv(name);
        final String value = SystemUtils.getEnvironmentVariable(name, null);
        assertEquals(expected, value);
    }

    @Test
    public void testGetHostName() {
        final String hostName = SystemUtils.getHostName();
        final String expected = SystemUtils.IS_OS_WINDOWS ? System.getenv("COMPUTERNAME") : System.getenv("HOSTNAME");
        assertEquals(expected, hostName);
    }

    /**
     * Assumes no security manager exists.
     */
    @Test
    public void testGetJavaHome() {
        final File dir = SystemUtils.getJavaHome();
        assertNotNull(dir);
        assertTrue(dir.exists());
    }

    /**
     * Assumes no security manager exists.
     */
    @Test
    public void testGetJavaIoTmpDir() {
        final File dir = SystemUtils.getJavaIoTmpDir();
        assertNotNull(dir);
        assertTrue(dir.exists());
    }

    /**
     * Assumes no security manager exists.
     */
    @Test
    public void testGetUserDir() {
        final File dir = SystemUtils.getUserDir();
        assertNotNull(dir);
        assertTrue(dir.exists());
    }

    /**
     * Assumes no security manager exists.
     */
    @Test
    public void testGetUserHome() {
        final File dir = SystemUtils.getUserHome();
        assertNotNull(dir);
        assertTrue(dir.exists());
    }

    @Test
    @SuppressWarnings("deprecation")
    public void testIS_JAVA() {
        final String javaVersion = SystemUtils.JAVA_VERSION;
        if (javaVersion == null) {
            assertFalse(SystemUtils.IS_JAVA_1_1);
            assertFalse(SystemUtils.IS_JAVA_1_2);
            assertFalse(SystemUtils.IS_JAVA_1_3);
            assertFalse(SystemUtils.IS_JAVA_1_4);
            assertFalse(SystemUtils.IS_JAVA_1_5);
            assertFalse(SystemUtils.IS_JAVA_1_6);
            assertFalse(SystemUtils.IS_JAVA_1_7);
            assertFalse(SystemUtils.IS_JAVA_1_8);
            assertFalse(SystemUtils.IS_JAVA_1_9);
            assertFalse(SystemUtils.IS_JAVA_9);
            assertFalse(SystemUtils.IS_JAVA_10);
            assertFalse(SystemUtils.IS_JAVA_11);
            assertFalse(SystemUtils.IS_JAVA_12);
            assertFalse(SystemUtils.IS_JAVA_13);
        } else if (javaVersion.startsWith("1.8")) {
            assertFalse(SystemUtils.IS_JAVA_1_1);
            assertFalse(SystemUtils.IS_JAVA_1_2);
            assertFalse(SystemUtils.IS_JAVA_1_3);
            assertFalse(SystemUtils.IS_JAVA_1_4);
            assertFalse(SystemUtils.IS_JAVA_1_5);
            assertFalse(SystemUtils.IS_JAVA_1_6);
            assertFalse(SystemUtils.IS_JAVA_1_7);
            assertTrue(SystemUtils.IS_JAVA_1_8);
            assertFalse(SystemUtils.IS_JAVA_1_9);
            assertFalse(SystemUtils.IS_JAVA_9);
            assertFalse(SystemUtils.IS_JAVA_10);
            assertFalse(SystemUtils.IS_JAVA_11);
            assertFalse(SystemUtils.IS_JAVA_12);
            assertFalse(SystemUtils.IS_JAVA_13);
        } else if (javaVersion.startsWith("9")) {
            assertFalse(SystemUtils.IS_JAVA_1_1);
            assertFalse(SystemUtils.IS_JAVA_1_2);
            assertFalse(SystemUtils.IS_JAVA_1_3);
            assertFalse(SystemUtils.IS_JAVA_1_4);
            assertFalse(SystemUtils.IS_JAVA_1_5);
            assertFalse(SystemUtils.IS_JAVA_1_6);
            assertFalse(SystemUtils.IS_JAVA_1_7);
            assertFalse(SystemUtils.IS_JAVA_1_8);
            assertTrue(SystemUtils.IS_JAVA_1_9);
            assertTrue(SystemUtils.IS_JAVA_9);
            assertFalse(SystemUtils.IS_JAVA_10);
            assertFalse(SystemUtils.IS_JAVA_11);
            assertFalse(SystemUtils.IS_JAVA_12);
            assertFalse(SystemUtils.IS_JAVA_13);
        } else if (javaVersion.startsWith("10")) {
            assertFalse(SystemUtils.IS_JAVA_1_1);
            assertFalse(SystemUtils.IS_JAVA_1_2);
            assertFalse(SystemUtils.IS_JAVA_1_3);
            assertFalse(SystemUtils.IS_JAVA_1_4);
            assertFalse(SystemUtils.IS_JAVA_1_5);
            assertFalse(SystemUtils.IS_JAVA_1_6);
            assertFalse(SystemUtils.IS_JAVA_1_7);
            assertFalse(SystemUtils.IS_JAVA_1_8);
            assertFalse(SystemUtils.IS_JAVA_1_9);
            assertFalse(SystemUtils.IS_JAVA_9);
            assertTrue(SystemUtils.IS_JAVA_10);
            assertFalse(SystemUtils.IS_JAVA_11);
            assertFalse(SystemUtils.IS_JAVA_12);
            assertFalse(SystemUtils.IS_JAVA_13);
        } else if (javaVersion.startsWith("11")) {
            assertFalse(SystemUtils.IS_JAVA_1_1);
            assertFalse(SystemUtils.IS_JAVA_1_2);
            assertFalse(SystemUtils.IS_JAVA_1_3);
            assertFalse(SystemUtils.IS_JAVA_1_4);
            assertFalse(SystemUtils.IS_JAVA_1_5);
            assertFalse(SystemUtils.IS_JAVA_1_6);
            assertFalse(SystemUtils.IS_JAVA_1_7);
            assertFalse(SystemUtils.IS_JAVA_1_8);
            assertFalse(SystemUtils.IS_JAVA_1_9);
            assertFalse(SystemUtils.IS_JAVA_9);
            assertFalse(SystemUtils.IS_JAVA_10);
            assertTrue(SystemUtils.IS_JAVA_11);
            assertFalse(SystemUtils.IS_JAVA_12);
            assertFalse(SystemUtils.IS_JAVA_13);
        } else if (javaVersion.startsWith("12")) {
            assertFalse(SystemUtils.IS_JAVA_1_1);
            assertFalse(SystemUtils.IS_JAVA_1_2);
            assertFalse(SystemUtils.IS_JAVA_1_3);
            assertFalse(SystemUtils.IS_JAVA_1_4);
            assertFalse(SystemUtils.IS_JAVA_1_5);
            assertFalse(SystemUtils.IS_JAVA_1_6);
            assertFalse(SystemUtils.IS_JAVA_1_7);
            assertFalse(SystemUtils.IS_JAVA_1_8);
            assertFalse(SystemUtils.IS_JAVA_1_9);
            assertFalse(SystemUtils.IS_JAVA_9);
            assertFalse(SystemUtils.IS_JAVA_10);
            assertFalse(SystemUtils.IS_JAVA_11);
            assertTrue(SystemUtils.IS_JAVA_12);
            assertFalse(SystemUtils.IS_JAVA_13);
        } else if (javaVersion.startsWith("13")) {
            assertFalse(SystemUtils.IS_JAVA_1_1);
            assertFalse(SystemUtils.IS_JAVA_1_2);
            assertFalse(SystemUtils.IS_JAVA_1_3);
            assertFalse(SystemUtils.IS_JAVA_1_4);
            assertFalse(SystemUtils.IS_JAVA_1_5);
            assertFalse(SystemUtils.IS_JAVA_1_6);
            assertFalse(SystemUtils.IS_JAVA_1_7);
            assertFalse(SystemUtils.IS_JAVA_1_8);
            assertFalse(SystemUtils.IS_JAVA_1_9);
            assertFalse(SystemUtils.IS_JAVA_9);
            assertFalse(SystemUtils.IS_JAVA_10);
            assertFalse(SystemUtils.IS_JAVA_11);
            assertFalse(SystemUtils.IS_JAVA_12);
            assertTrue(SystemUtils.IS_JAVA_13);
        } else {
            System.out.println("Can't test IS_JAVA value: " + javaVersion);
        }
    }

    @Test
    public void testIS_OS() {
        final String osName = System.getProperty("os.name");
        if (osName == null) {
            assertFalse(SystemUtils.IS_OS_WINDOWS);
            assertFalse(SystemUtils.IS_OS_UNIX);
            assertFalse(SystemUtils.IS_OS_SOLARIS);
            assertFalse(SystemUtils.IS_OS_LINUX);
            assertFalse(SystemUtils.IS_OS_MAC_OSX);
        } else if (osName.startsWith("Windows")) {
            assertFalse(SystemUtils.IS_OS_UNIX);
            assertTrue(SystemUtils.IS_OS_WINDOWS);
        } else if (osName.startsWith("Solaris")) {
            assertTrue(SystemUtils.IS_OS_SOLARIS);
            assertTrue(SystemUtils.IS_OS_UNIX);
            assertFalse(SystemUtils.IS_OS_WINDOWS);
        } else if (osName.toLowerCase(Locale.ENGLISH).startsWith("linux")) {
            assertTrue(SystemUtils.IS_OS_LINUX);
            assertTrue(SystemUtils.IS_OS_UNIX);
            assertFalse(SystemUtils.IS_OS_WINDOWS);
        } else if (osName.startsWith("Mac OS X")) {
            assertTrue(SystemUtils.IS_OS_MAC_OSX);
            assertTrue(SystemUtils.IS_OS_UNIX);
            assertFalse(SystemUtils.IS_OS_WINDOWS);
        } else if (osName.startsWith("OS/2")) {
            assertTrue(SystemUtils.IS_OS_OS2);
            assertFalse(SystemUtils.IS_OS_UNIX);
            assertFalse(SystemUtils.IS_OS_WINDOWS);
        } else if (osName.startsWith("SunOS")) {
            assertTrue(SystemUtils.IS_OS_SUN_OS);
            assertTrue(SystemUtils.IS_OS_UNIX);
            assertFalse(SystemUtils.IS_OS_WINDOWS);
        } else if (osName.startsWith("FreeBSD")) {
            assertTrue(SystemUtils.IS_OS_FREE_BSD);
            assertTrue(SystemUtils.IS_OS_UNIX);
            assertFalse(SystemUtils.IS_OS_WINDOWS);
        } else {
            System.out.println("Can't test IS_OS value: " + osName);
        }
    }

    @Test
    public void testIS_zOS() {
        final String osName = System.getProperty("os.name");
        if (osName == null) {
            assertFalse(SystemUtils.IS_OS_ZOS);
        } else if (osName.contains("z/OS")) {
            assertFalse(SystemUtils.IS_OS_WINDOWS);
            assertTrue(SystemUtils.IS_OS_ZOS);
        }
    }

    @Test
    public void testJavaVersionMatches() {
        String javaVersion = null;
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.8"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "9"));
        javaVersion = "";
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.8"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "9"));
        javaVersion = "1.0";
        assertTrue(SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.8"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "9"));
        javaVersion = "1.1";
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertTrue(SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.8"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "9"));
        javaVersion = "1.2";
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertTrue(SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.8"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "9"));
        javaVersion = "1.3.0";
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertTrue(SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.8"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "9"));
        javaVersion = "1.3.1";
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertTrue(SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.8"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "9"));
        javaVersion = "1.4.0";
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertTrue(SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.8"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "9"));
        javaVersion = "1.4.1";
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertTrue(SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.8"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "9"));
        javaVersion = "1.4.2";
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertTrue(SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.8"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "9"));
        javaVersion = "1.5.0";
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertTrue(SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.8"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "9"));
        javaVersion = "1.6.0";
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertTrue(SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.8"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "9"));
        javaVersion = "1.7.0";
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertTrue(SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.8"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "9"));
        javaVersion = "1.8.0";
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        assertTrue(SystemUtils.isJavaVersionMatch(javaVersion, "1.8"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "9"));
        javaVersion = "9";
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.8"));
        assertTrue(SystemUtils.isJavaVersionMatch(javaVersion, "9"));
    }

    @Test
    public void testIsJavaVersionAtLeast() {
        if (SystemUtils.IS_JAVA_1_8) {
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_1));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_2));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_3));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_4));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_5));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_6));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_7));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_8));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JAVA_9));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JAVA_10));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JAVA_11));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JAVA_12));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JAVA_13));
        } else if (SystemUtils.IS_JAVA_9) {
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_1));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_2));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_3));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_4));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_5));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_6));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_7));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_8));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_9));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JAVA_10));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JAVA_11));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JAVA_12));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JAVA_13));
        } else if (SystemUtils.IS_JAVA_10) {
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_1));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_2));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_3));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_4));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_5));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_6));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_7));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_8));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_9));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_10));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JAVA_11));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JAVA_12));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JAVA_13));
        } else if (SystemUtils.IS_JAVA_11) {
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_1));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_2));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_3));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_4));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_5));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_6));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_7));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_8));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_9));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_10));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_11));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JAVA_12));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JAVA_13));
        } else if (SystemUtils.IS_JAVA_12) {
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_1));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_2));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_3));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_4));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_5));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_6));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_7));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_8));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_9));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_10));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_11));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_12));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JAVA_13));
        } else if (SystemUtils.IS_JAVA_13) {
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_1));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_2));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_3));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_4));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_5));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_6));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_7));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_1_8));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_9));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_10));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_11));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_12));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JAVA_13));
        }
    }

    @Test
    public void testIsJavaVersionAtMost() {
        if (SystemUtils.IS_JAVA_1_8) {
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_1));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_2));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_3));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_4));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_5));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_6));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_7));
            assertTrue(SystemUtils.isJavaVersionAtMost(JAVA_1_8));
            assertTrue(SystemUtils.isJavaVersionAtMost(JAVA_9));
            assertTrue(SystemUtils.isJavaVersionAtMost(JAVA_10));
            assertTrue(SystemUtils.isJavaVersionAtMost(JAVA_11));
            assertTrue(SystemUtils.isJavaVersionAtMost(JAVA_12));
            assertTrue(SystemUtils.isJavaVersionAtMost(JAVA_13));
        } else if (SystemUtils.IS_JAVA_9) {
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_1));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_2));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_3));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_4));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_5));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_6));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_7));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_8));
            assertTrue(SystemUtils.isJavaVersionAtMost(JAVA_9));
            assertTrue(SystemUtils.isJavaVersionAtMost(JAVA_10));
            assertTrue(SystemUtils.isJavaVersionAtMost(JAVA_11));
            assertTrue(SystemUtils.isJavaVersionAtMost(JAVA_12));
            assertTrue(SystemUtils.isJavaVersionAtMost(JAVA_13));
        } else if (SystemUtils.IS_JAVA_10) {
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_1));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_2));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_3));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_4));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_5));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_6));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_7));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_8));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_9));
            assertTrue(SystemUtils.isJavaVersionAtMost(JAVA_10));
            assertTrue(SystemUtils.isJavaVersionAtMost(JAVA_11));
            assertTrue(SystemUtils.isJavaVersionAtMost(JAVA_12));
            assertTrue(SystemUtils.isJavaVersionAtMost(JAVA_13));
        } else if (SystemUtils.IS_JAVA_11) {
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_1));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_2));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_3));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_4));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_5));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_6));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_7));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_8));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_9));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_10));
            assertTrue(SystemUtils.isJavaVersionAtMost(JAVA_11));
            assertTrue(SystemUtils.isJavaVersionAtMost(JAVA_12));
            assertTrue(SystemUtils.isJavaVersionAtMost(JAVA_13));
        } else if (SystemUtils.IS_JAVA_12) {
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_1));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_2));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_3));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_4));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_5));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_6));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_7));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_8));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_9));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_10));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_11));
            assertTrue(SystemUtils.isJavaVersionAtMost(JAVA_12));
            assertTrue(SystemUtils.isJavaVersionAtMost(JAVA_13));
        } else if (SystemUtils.IS_JAVA_13) {
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_1));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_2));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_3));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_4));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_5));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_6));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_7));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_1_8));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_9));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_10));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_11));
            assertFalse(SystemUtils.isJavaVersionAtMost(JAVA_12));
            assertTrue(SystemUtils.isJavaVersionAtMost(JAVA_13));
        }
    }

    @Test
    public void testOSMatchesName() {
        String osName = null;
        assertFalse(SystemUtils.isOSNameMatch(osName, "Windows"));
        osName = "";
        assertFalse(SystemUtils.isOSNameMatch(osName, "Windows"));
        osName = "Windows 95";
        assertTrue(SystemUtils.isOSNameMatch(osName, "Windows"));
        osName = "Windows NT";
        assertTrue(SystemUtils.isOSNameMatch(osName, "Windows"));
        osName = "OS/2";
        assertFalse(SystemUtils.isOSNameMatch(osName, "Windows"));
    }

    @Test
    public void testOSMatchesNameAndVersion() {
        String osName = null;
        String osVersion = null;
        assertFalse(SystemUtils.isOSMatch(osName, osVersion, "Windows 9", "4.1"));
        osName = "";
        osVersion = "";
        assertFalse(SystemUtils.isOSMatch(osName, osVersion, "Windows 9", "4.1"));
        osName = "Windows 95";
        osVersion = "4.0";
        assertFalse(SystemUtils.isOSMatch(osName, osVersion, "Windows 9", "4.1"));
        osName = "Windows 95";
        osVersion = "4.1";
        assertTrue(SystemUtils.isOSMatch(osName, osVersion, "Windows 9", "4.1"));
        osName = "Windows 98";
        osVersion = "4.1";
        assertTrue(SystemUtils.isOSMatch(osName, osVersion, "Windows 9", "4.1"));
        osName = "Windows NT";
        osVersion = "4.0";
        assertFalse(SystemUtils.isOSMatch(osName, osVersion, "Windows 9", "4.1"));
        osName = "OS/2";
        osVersion = "4.0";
        assertFalse(SystemUtils.isOSMatch(osName, osVersion, "Windows 9", "4.1"));
    }

    @Test
    public void testOsVersionMatches() {
        String osVersion = null;
        assertFalse(SystemUtils.isOSVersionMatch(osVersion, "10.1"));

        osVersion = "";
        assertFalse(SystemUtils.isOSVersionMatch(osVersion, "10.1"));

        osVersion = "10";
        assertTrue(SystemUtils.isOSVersionMatch(osVersion, "10.1"));
        assertTrue(SystemUtils.isOSVersionMatch(osVersion, "10.1.1"));
        assertTrue(SystemUtils.isOSVersionMatch(osVersion, "10.10"));
        assertTrue(SystemUtils.isOSVersionMatch(osVersion, "10.10.1"));

        osVersion = "10.1";
        assertTrue(SystemUtils.isOSVersionMatch(osVersion, "10.1"));
        assertTrue(SystemUtils.isOSVersionMatch(osVersion, "10.1.1"));
        assertFalse(SystemUtils.isOSVersionMatch(osVersion, "10.10"));
        assertFalse(SystemUtils.isOSVersionMatch(osVersion, "10.10.1"));

        osVersion = "10.1.1";
        assertTrue(SystemUtils.isOSVersionMatch(osVersion, "10.1"));
        assertTrue(SystemUtils.isOSVersionMatch(osVersion, "10.1.1"));
        assertFalse(SystemUtils.isOSVersionMatch(osVersion, "10.10"));
        assertFalse(SystemUtils.isOSVersionMatch(osVersion, "10.10.1"));

        osVersion = "10.10";
        assertFalse(SystemUtils.isOSVersionMatch(osVersion, "10.1"));
        assertFalse(SystemUtils.isOSVersionMatch(osVersion, "10.1.1"));
        assertTrue(SystemUtils.isOSVersionMatch(osVersion, "10.10"));
        assertTrue(SystemUtils.isOSVersionMatch(osVersion, "10.10.1"));

        osVersion = "10.10.1";
        assertFalse(SystemUtils.isOSVersionMatch(osVersion, "10.1"));
        assertFalse(SystemUtils.isOSVersionMatch(osVersion, "10.1.1"));
        assertTrue(SystemUtils.isOSVersionMatch(osVersion, "10.10"));
        assertTrue(SystemUtils.isOSVersionMatch(osVersion, "10.10.1"));
    }

    @Test
    public void testJavaAwtHeadless() {
        final String expectedStringValue = System.getProperty("java.awt.headless");
        final String expectedStringValueWithDefault = System.getProperty("java.awt.headless", "false");
        assertNotNull(expectedStringValueWithDefault);
        final boolean expectedValue = Boolean.valueOf(expectedStringValue).booleanValue();
        if (expectedStringValue != null) {
            assertEquals(expectedStringValue, SystemUtils.JAVA_AWT_HEADLESS);
        }
        assertEquals(expectedValue, SystemUtils.isJavaAwtHeadless());
        assertEquals(expectedStringValueWithDefault, "" + SystemUtils.isJavaAwtHeadless());
    }
}
