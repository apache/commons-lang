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
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Locale;

import org.junit.jupiter.api.Test;

/**
 * Tests {@link SystemUtils}.
 *
 * Only limited testing can be performed.
 */
class SystemUtilsTest extends AbstractLangTest {

    /**
     * Returns the value of the SystemUtils.IS_JAVA_X field for the versions >= 9.
     */
    private boolean getIS_JAVA(final int version) throws Exception {
        return SystemUtils.class.getField("IS_JAVA_" + version).getBoolean(null);
    }

    /**
     * Returns the last supported version with the SystemUtils.IS_JAVA_X fields.
     */
    public int getLastSupportedJavaVersion() {
        int lastSupportedVersion = 0;
        for (final Field field : SystemUtils.class.getFields()) {
            if (field.getName().matches("IS_JAVA_\\d+")) {
                lastSupportedVersion = Math.max(lastSupportedVersion, Integer.parseInt(field.getName().substring(8)));
            }
        }
        return lastSupportedVersion;
    }

    @Test
    @SuppressWarnings("deprecation")
    void test_IS_JAVA() throws Exception {
        final String javaVersion = SystemUtils.JAVA_VERSION;
        final int lastSupportedVersion = getLastSupportedJavaVersion();
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
            assertFalse(SystemUtils.IS_JAVA_10);
            assertFalse(SystemUtils.IS_JAVA_11);
            assertFalse(SystemUtils.IS_JAVA_12);
            assertFalse(SystemUtils.IS_JAVA_13);
            assertFalse(SystemUtils.IS_JAVA_14);
            assertFalse(SystemUtils.IS_JAVA_15);
            assertFalse(SystemUtils.IS_JAVA_16);
            assertFalse(SystemUtils.IS_JAVA_17);
            assertFalse(SystemUtils.IS_JAVA_18);
            assertFalse(SystemUtils.IS_JAVA_19);
            assertFalse(SystemUtils.IS_JAVA_20);
            assertFalse(SystemUtils.IS_JAVA_21);
            assertFalse(SystemUtils.IS_JAVA_22);
            assertFalse(SystemUtils.IS_JAVA_23);
            assertFalse(SystemUtils.IS_JAVA_24);
            assertFalse(SystemUtils.IS_JAVA_25);
            assertFalse(SystemUtils.IS_JAVA_26);
            for (int version = 9; version <= lastSupportedVersion; version++) {
                assertFalse(getIS_JAVA(version));
            }
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
            assertFalse(SystemUtils.IS_JAVA_10);
            assertFalse(SystemUtils.IS_JAVA_11);
            assertFalse(SystemUtils.IS_JAVA_12);
            assertFalse(SystemUtils.IS_JAVA_13);
            assertFalse(SystemUtils.IS_JAVA_14);
            assertFalse(SystemUtils.IS_JAVA_15);
            assertFalse(SystemUtils.IS_JAVA_16);
            assertFalse(SystemUtils.IS_JAVA_17);
            assertFalse(SystemUtils.IS_JAVA_18);
            assertFalse(SystemUtils.IS_JAVA_19);
            assertFalse(SystemUtils.IS_JAVA_20);
            assertFalse(SystemUtils.IS_JAVA_21);
            assertFalse(SystemUtils.IS_JAVA_22);
            assertFalse(SystemUtils.IS_JAVA_23);
            assertFalse(SystemUtils.IS_JAVA_24);
            assertFalse(SystemUtils.IS_JAVA_25);
            assertFalse(SystemUtils.IS_JAVA_26);
            for (int version = 9; version <= lastSupportedVersion; version++) {
                assertFalse(getIS_JAVA(version));
            }
        } else if (!javaVersion.startsWith("1.")) {
            assertFalse(SystemUtils.IS_JAVA_1_1);
            assertFalse(SystemUtils.IS_JAVA_1_2);
            assertFalse(SystemUtils.IS_JAVA_1_3);
            assertFalse(SystemUtils.IS_JAVA_1_4);
            assertFalse(SystemUtils.IS_JAVA_1_5);
            assertFalse(SystemUtils.IS_JAVA_1_6);
            assertFalse(SystemUtils.IS_JAVA_1_7);
            assertFalse(SystemUtils.IS_JAVA_1_8);
            assertEquals(javaVersion.startsWith("9"), SystemUtils.IS_JAVA_1_9);
            for (int version = 9; version <= lastSupportedVersion; version++) {
                assertEquals(javaVersion.startsWith("" + version), getIS_JAVA(version));
            }
        } else {
            System.out.println("Can't test IS_JAVA value: " + javaVersion);
        }
    }

    @Test
    void test_IS_OS() {
        final String osName = System.getProperty("os.name");
        if (osName == null) {
            assertFalse(SystemUtils.IS_OS_WINDOWS);
            assertFalse(SystemUtils.IS_OS_UNIX);
            assertFalse(SystemUtils.IS_OS_SOLARIS);
            assertFalse(SystemUtils.IS_OS_LINUX);
            assertFalse(SystemUtils.IS_OS_MAC_OSX);
            assertFalse(SystemUtils.IS_OS_NETWARE);
        } else if (osName.startsWith("Windows")) {
            assertTrue(SystemUtils.IS_OS_WINDOWS);
            assertFalse(SystemUtils.IS_OS_ANDROID);
            assertFalse(SystemUtils.IS_OS_UNIX);
            assertFalse(SystemUtils.IS_OS_NETWARE);
        } else if (osName.startsWith("Solaris")) {
            assertTrue(SystemUtils.IS_OS_SOLARIS);
            assertTrue(SystemUtils.IS_OS_UNIX);
            assertFalse(SystemUtils.IS_OS_ANDROID);
            assertFalse(SystemUtils.IS_OS_WINDOWS);
            assertFalse(SystemUtils.IS_OS_NETWARE);
        } else if (osName.toLowerCase(Locale.ENGLISH).startsWith("linux")) {
            assertTrue(SystemUtils.IS_OS_LINUX);
            assertTrue(SystemUtils.IS_OS_UNIX);
            assertFalse(SystemUtils.IS_OS_WINDOWS);
            assertFalse(SystemUtils.IS_OS_NETWARE);
        } else if (osName.startsWith("Mac OS X")) {
            assertTrue(SystemUtils.IS_OS_MAC_OSX);
            assertTrue(SystemUtils.IS_OS_UNIX);
            assertFalse(SystemUtils.IS_OS_ANDROID);
            assertFalse(SystemUtils.IS_OS_WINDOWS);
            assertFalse(SystemUtils.IS_OS_NETWARE);
            // @formatter:off
            final boolean[] macOsValues = {
                    SystemUtils.IS_OS_MAC_OSX_BIG_SUR,
                    SystemUtils.IS_OS_MAC_OSX_CATALINA,
                    SystemUtils.IS_OS_MAC_OSX_CHEETAH,
                    SystemUtils.IS_OS_MAC_OSX_EL_CAPITAN,
                    SystemUtils.IS_OS_MAC_OSX_HIGH_SIERRA,
                    SystemUtils.IS_OS_MAC_OSX_JAGUAR,
                    SystemUtils.IS_OS_MAC_OSX_LEOPARD,
                    SystemUtils.IS_OS_MAC_OSX_LION,
                    SystemUtils.IS_OS_MAC_OSX_MAVERICKS,
                    SystemUtils.IS_OS_MAC_OSX_MOJAVE,
                    SystemUtils.IS_OS_MAC_OSX_MONTEREY,
                    SystemUtils.IS_OS_MAC_OSX_MOUNTAIN_LION,
                    SystemUtils.IS_OS_MAC_OSX_PANTHER,
                    SystemUtils.IS_OS_MAC_OSX_PUMA,
                    SystemUtils.IS_OS_MAC_OSX_SEQUOIA,
                    SystemUtils.IS_OS_MAC_OSX_SIERRA,
                    SystemUtils.IS_OS_MAC_OSX_SNOW_LEOPARD,
                    SystemUtils.IS_OS_MAC_OSX_SONOMA,
                    SystemUtils.IS_OS_MAC_OSX_TIGER,
                    SystemUtils.IS_OS_MAC_OSX_VENTURA,
                    SystemUtils.IS_OS_MAC_OSX_YOSEMITE };
            // @formatter:on
            if (BooleanUtils.or(macOsValues)) {
                // If one is true, then only one should be true.
                assertTrue(BooleanUtils.xor(macOsValues));
            }
        } else if (osName.startsWith("OS/2")) {
            assertTrue(SystemUtils.IS_OS_OS2);
            assertFalse(SystemUtils.IS_OS_UNIX);
            assertFalse(SystemUtils.IS_OS_ANDROID);
            assertFalse(SystemUtils.IS_OS_WINDOWS);
            assertFalse(SystemUtils.IS_OS_NETWARE);
        } else if (osName.startsWith("SunOS")) {
            assertTrue(SystemUtils.IS_OS_SUN_OS);
            assertTrue(SystemUtils.IS_OS_UNIX);
            assertFalse(SystemUtils.IS_OS_ANDROID);
            assertFalse(SystemUtils.IS_OS_WINDOWS);
            assertFalse(SystemUtils.IS_OS_NETWARE);
        } else if (osName.startsWith("FreeBSD")) {
            assertTrue(SystemUtils.IS_OS_FREE_BSD);
            assertTrue(SystemUtils.IS_OS_UNIX);
            assertFalse(SystemUtils.IS_OS_ANDROID);
            assertFalse(SystemUtils.IS_OS_WINDOWS);
            assertFalse(SystemUtils.IS_OS_NETWARE);
        } else {
            System.err.println("Can't test IS_OS_ value: " + osName);
        }
    }

    @Test
    void test_IS_zOS() {
        final String osName = System.getProperty("os.name");
        if (osName == null) {
            assertFalse(SystemUtils.IS_OS_ZOS);
        } else if (osName.contains("z/OS")) {
            assertFalse(SystemUtils.IS_OS_WINDOWS);
            assertTrue(SystemUtils.IS_OS_ZOS);
        }
    }

    /**
     * Assumes no security manager exists.
     */
    @Test
    void test_USER_NAME() {
        assertEquals(System.getProperty("user.name"), SystemUtils.USER_NAME);
    }

    @Test
    void testConstructor() {
        assertNotNull(new SystemUtils());
        final Constructor<?>[] cons = SystemUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertTrue(Modifier.isPublic(cons[0].getModifiers()));
        assertTrue(Modifier.isPublic(SystemUtils.class.getModifiers()));
        assertFalse(Modifier.isFinal(SystemUtils.class.getModifiers()));
    }

    @Test
    void testGetEnvironmentVariableAbsent() {
        final String name = "THIS_ENV_VAR_SHOULD_NOT_EXIST_FOR_THIS_TEST_TO_PASS";
        final String expected = System.getenv(name);
        assertNull(expected);
        final String value = SystemUtils.getEnvironmentVariable(name, "DEFAULT");
        assertEquals("DEFAULT", value);
    }

    @Test
    void testGetEnvironmentVariablePresent() {
        final String name = "PATH";
        final String expected = System.getenv(name);
        final String value = SystemUtils.getEnvironmentVariable(name, null);
        assertEquals(expected, value);
    }

    @Test
    void testGetHostName() {
        final String hostName = SystemUtils.getHostName();
        final String expected = SystemUtils.IS_OS_WINDOWS ? System.getenv("COMPUTERNAME") : System.getenv("HOSTNAME");
        assertEquals(expected, hostName);
    }

    /**
     * Assumes no security manager exists.
     */
    @Test
    void testGetJavaHome() {
        final File dir = SystemUtils.getJavaHome();
        assertNotNull(dir);
        assertTrue(dir.exists());
    }

    /**
     * Assumes no security manager exists.
     */
    @Test
    void testGetJavaHomePath() {
        final Path dir = SystemUtils.getJavaHomePath();
        assertNotNull(dir);
        assertTrue(Files.exists(dir));
    }

    /**
     * Assumes no security manager exists.
     */
    @Test
    void testGetJavaIoTmpDir() {
        final File dir = SystemUtils.getJavaIoTmpDir();
        assertNotNull(dir);
        assertTrue(dir.exists());
    }

    /**
     * Assumes no security manager exists.
     */
    @Test
    void testGetJavaIoTmpDirPath() {
        final Path dir = SystemUtils.getJavaIoTmpDirPath();
        assertNotNull(dir);
        assertTrue(Files.exists(dir));
    }

    /**
     * Assumes no security manager exists.
     */
    @Test
    void testGetUserDir() {
        final File dir = SystemUtils.getUserDir();
        assertNotNull(dir);
        assertTrue(dir.exists());
    }

    /**
     * Assumes no security manager exists.
     */
    @Test
    void testGetUserDirPath() {
        final Path dir = SystemUtils.getUserDirPath();
        assertNotNull(dir);
        assertTrue(Files.exists(dir));
    }

    /**
     * Assumes no security manager exists.
     */
    @Test
    void testGetUserHome() {
        final File dir = SystemUtils.getUserHome();
        assertNotNull(dir);
        assertTrue(dir.exists());
    }

    /**
     * Assumes no security manager exists.
     */
    @Test
    void testGetUserHomePath() {
        final Path dir = SystemUtils.getUserHomePath();
        assertNotNull(dir);
        assertTrue(Files.exists(dir));
    }

    /**
     * Assumes no security manager exists.
     */
    @Test
    void testGetUserName() {
        assertEquals(System.getProperty("user.name"), SystemUtils.getUserName());
        // Don't overwrite the system property in this test in case something goes awfully wrong.
        assertEquals(System.getProperty("user.name", "foo"), SystemUtils.getUserName("foo"));
    }

    @Test
    void testIsJavaVersionAtLeast() {
        if (SystemUtils.IS_JAVA_1_8) {
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_1));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_2));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_3));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_4));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_5));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_6));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_7));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_8));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_9));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_10));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_11));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_12));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_13));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_14));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_15));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_16));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_17));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_18));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_19));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_20));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_21));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_22));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_23));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_24));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_25));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_26));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_9) {
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_1));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_2));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_3));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_4));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_5));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_6));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_7));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_8));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_9));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_10));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_11));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_12));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_13));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_14));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_15));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_16));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_17));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_18));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_19));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_20));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_21));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_22));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_23));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_24));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_25));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_26));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_10) {
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_1));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_2));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_3));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_4));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_5));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_6));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_7));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_8));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_9));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_10));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_11));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_12));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_13));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_14));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_15));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_16));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_17));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_18));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_19));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_20));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_21));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_22));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_23));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_24));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_25));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_26));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_11) {
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_1));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_2));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_3));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_4));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_5));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_6));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_7));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_8));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_9));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_10));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_11));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_12));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_13));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_14));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_15));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_16));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_17));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_18));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_19));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_20));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_21));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_22));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_23));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_24));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_25));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_26));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_12) {
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_1));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_2));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_3));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_4));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_5));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_6));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_7));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_8));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_9));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_10));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_11));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_12));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_13));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_14));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_15));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_16));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_17));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_18));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_19));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_20));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_21));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_22));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_23));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_24));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_25));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_26));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_13) {
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_1));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_2));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_3));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_4));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_5));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_6));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_7));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_8));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_9));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_10));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_11));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_12));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_13));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_14));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_15));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_16));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_17));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_18));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_19));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_20));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_21));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_22));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_23));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_24));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_25));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_26));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_14) {
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_1));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_2));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_3));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_4));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_5));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_6));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_7));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_8));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_9));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_10));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_11));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_12));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_13));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_14));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_15));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_16));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_17));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_18));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_19));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_20));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_21));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_22));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_23));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_24));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_25));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_26));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_15) {
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_1));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_2));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_3));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_4));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_5));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_6));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_7));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_8));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_9));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_10));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_11));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_12));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_13));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_14));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_15));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_16));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_17));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_18));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_19));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_20));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_21));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_22));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_23));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_24));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_25));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_26));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_16) {
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_1));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_2));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_3));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_4));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_5));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_6));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_7));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_8));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_9));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_10));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_11));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_12));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_13));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_14));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_15));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_16));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_17));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_18));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_19));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_20));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_21));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_22));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_23));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_24));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_25));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_26));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_17) {
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_1));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_2));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_3));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_4));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_5));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_6));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_7));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_8));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_9));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_10));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_11));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_12));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_13));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_14));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_15));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_16));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_17));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_18));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_19));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_20));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_21));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_22));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_23));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_24));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_25));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_26));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_18) {
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_1));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_2));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_3));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_4));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_5));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_6));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_7));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_8));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_9));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_10));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_11));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_12));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_13));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_14));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_15));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_16));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_17));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_18));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_19));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_20));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_21));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_22));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_23));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_24));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_25));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_26));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_19) {
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_1));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_2));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_3));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_4));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_5));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_6));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_7));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_8));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_9));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_10));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_11));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_12));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_13));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_14));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_15));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_16));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_17));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_18));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_19));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_20));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_21));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_22));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_23));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_24));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_25));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_26));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_20) {
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_1));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_2));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_3));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_4));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_5));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_6));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_7));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_8));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_9));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_10));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_11));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_12));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_13));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_14));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_15));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_16));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_17));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_18));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_19));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_20));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_21));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_22));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_23));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_24));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_25));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_26));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_21) {
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_1));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_2));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_3));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_4));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_5));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_6));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_7));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_8));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_9));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_10));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_11));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_12));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_13));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_14));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_15));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_16));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_17));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_18));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_19));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_20));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_21));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_22));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_23));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_24));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_25));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_26));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_22) {
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_1));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_2));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_3));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_4));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_5));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_6));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_7));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_8));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_9));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_10));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_11));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_12));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_13));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_14));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_15));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_16));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_17));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_18));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_19));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_20));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_21));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_22));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_23));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_24));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_25));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_26));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_23) {
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_1));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_2));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_3));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_4));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_5));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_6));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_7));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_8));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_9));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_10));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_11));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_12));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_13));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_14));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_15));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_16));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_17));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_18));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_19));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_20));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_21));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_22));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_23));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_24));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_25));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_26));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_24) {
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_1));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_2));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_3));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_4));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_5));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_6));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_7));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_8));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_9));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_10));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_11));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_12));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_13));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_14));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_15));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_16));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_17));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_18));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_19));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_20));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_21));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_22));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_23));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_24));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_25));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_26));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_25) {
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_1));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_2));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_3));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_4));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_5));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_6));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_7));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_8));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_9));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_10));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_11));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_12));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_13));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_14));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_15));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_16));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_17));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_18));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_19));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_20));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_21));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_22));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_23));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_24));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_25));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_26));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_26) {
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_1));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_2));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_3));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_4));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_5));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_6));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_7));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_8));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_9));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_10));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_11));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_12));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_13));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_14));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_15));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_16));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_17));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_18));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_19));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_20));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_21));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_22));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_23));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_24));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_25));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_26));
            assertFalse(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_27) {
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_1));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_2));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_3));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_4));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_5));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_6));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_7));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_8));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_9));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_10));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_11));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_12));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_13));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_14));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_15));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_16));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_17));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_18));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_19));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_20));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_21));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_22));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_23));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_24));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_25));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_26));
            assertTrue(SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_27));
        }
    }

    @Test
    void testIsJavaVersionAtMost() {
        if (SystemUtils.IS_JAVA_1_8) {
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_1));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_2));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_3));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_4));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_5));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_6));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_7));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_8));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_9));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_10));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_11));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_12));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_13));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_14));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_15));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_16));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_17));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_18));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_19));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_20));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_21));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_22));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_23));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_24));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_25));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_26));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_9) {
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_1));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_2));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_3));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_4));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_5));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_6));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_7));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_8));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_9));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_10));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_11));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_12));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_13));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_14));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_15));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_16));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_17));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_18));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_19));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_20));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_21));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_22));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_23));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_24));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_25));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_26));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_27));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_10) {
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_1));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_2));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_3));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_4));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_5));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_6));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_7));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_8));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_9));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_10));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_11));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_12));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_13));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_14));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_15));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_16));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_17));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_18));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_19));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_20));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_21));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_22));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_23));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_24));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_25));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_26));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_11) {
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_1));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_2));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_3));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_4));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_5));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_6));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_7));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_8));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_9));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_10));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_11));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_12));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_13));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_14));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_15));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_16));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_17));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_18));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_19));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_20));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_21));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_22));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_23));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_24));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_25));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_26));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_12) {
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_1));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_2));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_3));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_4));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_5));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_6));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_7));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_8));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_9));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_10));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_11));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_12));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_13));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_14));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_15));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_16));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_17));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_18));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_19));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_20));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_21));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_22));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_23));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_24));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_25));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_26));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_13) {
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_1));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_2));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_3));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_4));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_5));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_6));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_7));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_8));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_9));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_10));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_11));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_12));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_13));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_14));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_15));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_16));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_17));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_18));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_19));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_20));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_21));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_22));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_23));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_24));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_25));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_26));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_14) {
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_1));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_2));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_3));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_4));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_5));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_6));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_7));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_8));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_9));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_10));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_11));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_12));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_13));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_14));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_15));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_16));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_17));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_18));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_19));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_20));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_21));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_22));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_23));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_24));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_25));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_26));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_15) {
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_1));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_2));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_3));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_4));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_5));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_6));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_7));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_8));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_9));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_10));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_11));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_12));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_13));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_14));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_15));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_16));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_17));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_18));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_19));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_20));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_21));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_22));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_23));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_24));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_25));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_26));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_16) {
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_1));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_2));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_3));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_4));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_5));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_6));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_7));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_8));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_9));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_10));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_11));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_12));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_13));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_14));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_15));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_16));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_17));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_18));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_19));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_20));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_21));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_22));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_23));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_24));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_25));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_26));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_17) {
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_1));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_2));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_3));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_4));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_5));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_6));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_7));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_8));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_9));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_10));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_11));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_12));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_13));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_14));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_15));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_16));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_17));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_18));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_19));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_20));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_21));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_22));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_23));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_24));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_25));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_26));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_18) {
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_1));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_2));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_3));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_4));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_5));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_6));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_7));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_8));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_9));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_10));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_11));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_12));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_13));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_14));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_15));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_16));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_17));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_18));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_19));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_20));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_21));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_22));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_23));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_24));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_25));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_26));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_19) {
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_1));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_2));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_3));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_4));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_5));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_6));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_7));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_8));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_9));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_10));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_11));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_12));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_13));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_14));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_15));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_16));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_17));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_18));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_19));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_20));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_21));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_22));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_23));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_24));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_25));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_26));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_20) {
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_1));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_2));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_3));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_4));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_5));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_6));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_7));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_8));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_9));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_10));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_11));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_12));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_13));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_14));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_15));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_16));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_17));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_18));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_19));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_20));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_21));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_22));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_23));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_24));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_25));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_26));
        } else if (SystemUtils.IS_JAVA_21) {
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_1));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_2));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_3));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_4));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_5));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_6));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_7));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_8));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_9));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_10));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_11));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_12));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_13));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_14));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_15));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_16));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_17));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_18));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_19));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_20));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_21));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_22));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_23));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_24));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_25));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_26));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_22) {
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_1));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_2));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_3));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_4));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_5));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_6));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_7));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_8));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_9));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_10));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_11));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_12));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_13));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_14));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_15));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_16));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_17));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_18));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_19));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_20));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_21));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_22));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_23));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_24));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_25));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_26));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_23) {
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_1));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_2));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_3));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_4));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_5));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_6));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_7));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_8));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_9));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_10));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_11));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_12));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_13));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_14));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_15));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_16));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_17));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_18));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_19));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_20));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_21));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_22));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_23));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_24));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_25));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_26));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_24) {
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_1));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_2));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_3));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_4));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_5));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_6));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_7));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_8));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_9));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_10));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_11));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_12));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_13));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_14));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_15));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_16));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_17));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_18));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_19));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_20));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_21));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_22));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_23));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_24));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_25));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_26));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_25) {
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_1));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_2));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_3));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_4));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_5));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_6));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_7));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_8));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_9));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_10));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_11));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_12));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_13));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_14));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_15));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_16));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_17));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_18));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_19));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_20));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_21));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_22));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_23));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_24));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_25));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_26));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_26) {
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_1));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_2));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_3));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_4));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_5));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_6));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_7));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_8));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_9));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_10));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_11));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_12));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_13));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_14));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_15));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_16));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_17));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_18));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_19));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_20));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_21));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_22));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_23));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_24));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_25));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_26));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_27));
        } else if (SystemUtils.IS_JAVA_27) {
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_1));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_2));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_3));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_4));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_5));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_6));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_7));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_1_8));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_9));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_10));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_11));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_12));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_13));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_14));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_15));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_16));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_17));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_18));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_19));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_20));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_21));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_22));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_23));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_24));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_25));
            assertFalse(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_26));
            assertTrue(SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_27));
        }
    }

    @Test
    void testJavaAwtHeadless() {
        final String expectedStringValue = System.getProperty("java.awt.headless");
        final String expectedStringValueWithDefault = System.getProperty("java.awt.headless", "false");
        assertNotNull(expectedStringValueWithDefault);
        final boolean expectedValue = Boolean.parseBoolean(expectedStringValue);
        if (expectedStringValue != null) {
            assertEquals(expectedStringValue, SystemUtils.JAVA_AWT_HEADLESS);
        }
        assertEquals(expectedValue, SystemUtils.isJavaAwtHeadless());
        assertEquals(expectedStringValueWithDefault, "" + SystemUtils.isJavaAwtHeadless());
    }

    @Test
    void testJavaVersionMatches() {
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
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "10"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "11"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "12"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "13"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "14"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "15"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "16"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "17"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "18"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "19"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "20"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "21"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "22"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "23"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "24"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "25"));
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
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "10"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "11"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "12"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "13"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "14"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "15"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "16"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "17"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "18"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "19"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "20"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "21"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "22"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "23"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "24"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "25"));
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
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "10"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "11"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "12"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "13"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "14"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "15"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "16"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "17"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "18"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "19"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "20"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "21"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "22"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "23"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "24"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "25"));
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
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "10"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "11"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "12"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "13"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "14"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "15"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "16"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "17"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "18"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "19"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "20"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "21"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "22"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "23"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "24"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "25"));
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
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "10"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "11"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "12"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "13"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "14"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "15"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "16"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "17"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "18"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "19"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "20"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "21"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "22"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "23"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "24"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "25"));
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
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "10"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "11"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "12"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "13"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "14"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "15"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "16"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "17"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "18"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "19"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "20"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "21"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "22"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "23"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "24"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "25"));
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
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "10"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "11"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "12"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "13"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "14"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "15"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "16"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "17"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "18"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "19"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "20"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "21"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "22"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "23"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "24"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "25"));
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
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "10"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "11"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "12"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "13"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "14"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "15"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "16"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "17"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "18"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "19"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "20"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "21"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "22"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "23"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "24"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "25"));
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
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "10"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "11"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "12"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "13"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "14"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "15"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "16"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "17"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "18"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "19"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "20"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "21"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "22"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "23"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "24"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "25"));
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
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "10"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "11"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "12"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "13"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "14"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "15"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "16"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "17"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "18"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "19"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "20"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "21"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "22"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "23"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "24"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "25"));
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
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "10"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "11"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "12"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "13"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "14"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "15"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "16"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "17"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "18"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "19"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "20"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "21"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "22"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "23"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "24"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "25"));
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
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "10"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "11"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "12"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "13"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "14"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "15"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "16"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "17"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "18"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "19"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "20"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "21"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "22"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "23"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "24"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "25"));
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
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "10"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "11"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "12"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "13"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "14"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "15"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "16"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "17"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "18"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "19"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "20"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "21"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "22"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "23"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "24"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "25"));
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
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "10"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "11"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "12"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "13"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "14"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "15"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "16"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "17"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "18"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "19"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "20"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "21"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "22"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "23"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "24"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "25"));
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
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "10"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "11"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "12"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "13"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "14"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "15"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "16"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "17"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "18"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "19"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "20"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "21"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "22"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "23"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "24"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "25"));
        javaVersion = "10";
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
        assertTrue(SystemUtils.isJavaVersionMatch(javaVersion, "10"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "11"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "12"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "13"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "14"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "15"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "16"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "17"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "18"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "19"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "20"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "21"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "22"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "23"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "24"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "25"));
        javaVersion = "11"; // LTS
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
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "10"));
        assertTrue(SystemUtils.isJavaVersionMatch(javaVersion, "11"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "12"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "13"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "14"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "15"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "16"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "17"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "18"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "19"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "20"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "21"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "22"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "23"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "24"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "25"));
        javaVersion = "17"; // LTS
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
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "10"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "11"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "12"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "13"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "14"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "15"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "16"));
        assertTrue(SystemUtils.isJavaVersionMatch(javaVersion, "17"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "18"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "19"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "20"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "21"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "22"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "23"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "24"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "25"));
        javaVersion = "21"; // LTS
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
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "10"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "11"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "12"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "13"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "14"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "15"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "16"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "17"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "18"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "19"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "20"));
        assertTrue(SystemUtils.isJavaVersionMatch(javaVersion, "21"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "22"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "23"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "24"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "25"));
        javaVersion = "22";
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
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "10"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "11"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "12"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "13"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "14"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "15"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "16"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "17"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "18"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "19"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "20"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "21"));
        assertTrue(SystemUtils.isJavaVersionMatch(javaVersion, "22"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "23"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "24"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "25"));
        javaVersion = "23";
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
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "10"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "11"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "12"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "13"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "14"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "15"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "16"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "17"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "18"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "19"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "20"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "21"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "22"));
        assertTrue(SystemUtils.isJavaVersionMatch(javaVersion, "23"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "24"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "25"));
        javaVersion = "24";
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
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "10"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "11"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "12"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "13"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "14"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "15"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "16"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "17"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "18"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "19"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "20"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "21"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "22"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "23"));
        assertTrue(SystemUtils.isJavaVersionMatch(javaVersion, "24"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "25"));
        javaVersion = "25";
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
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "10"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "11"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "12"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "13"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "14"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "15"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "16"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "17"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "18"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "19"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "20"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "21"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "22"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "23"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "24"));
        assertTrue(SystemUtils.isJavaVersionMatch(javaVersion, "25"));
    }

    @Test
    void testOSMatchesName() {
        String osName = null;
        assertFalse(SystemUtils.isOsNameMatch(osName, "Windows"));
        osName = "";
        assertFalse(SystemUtils.isOsNameMatch(osName, "Windows"));
        osName = "Windows 95";
        assertTrue(SystemUtils.isOsNameMatch(osName, "Windows"));
        osName = "Windows NT";
        assertTrue(SystemUtils.isOsNameMatch(osName, "Windows"));
        osName = "OS/2";
        assertFalse(SystemUtils.isOsNameMatch(osName, "Windows"));
    }

    @Test
    void testOSMatchesNameAndVersion() {
        String osName = null;
        String osVersion = null;
        assertFalse(SystemUtils.isOsMatch(osName, osVersion, "Windows 9", "4.1"));
        osName = "";
        osVersion = "";
        assertFalse(SystemUtils.isOsMatch(osName, osVersion, "Windows 9", "4.1"));
        osName = "Windows 95";
        osVersion = "4.0";
        assertFalse(SystemUtils.isOsMatch(osName, osVersion, "Windows 9", "4.1"));
        osName = "Windows 95";
        osVersion = "4.1";
        assertTrue(SystemUtils.isOsMatch(osName, osVersion, "Windows 9", "4.1"));
        osName = "Windows 98";
        osVersion = "4.1";
        assertTrue(SystemUtils.isOsMatch(osName, osVersion, "Windows 9", "4.1"));
        osName = "Windows NT";
        osVersion = "4.0";
        assertFalse(SystemUtils.isOsMatch(osName, osVersion, "Windows 9", "4.1"));
        osName = "OS/2";
        osVersion = "4.0";
        assertFalse(SystemUtils.isOsMatch(osName, osVersion, "Windows 9", "4.1"));
    }

    @Test
    void testOsVersionMatches() {
        String osVersion = null;
        assertFalse(SystemUtils.isOsVersionMatch(osVersion, "10.1"));
        osVersion = "";
        assertFalse(SystemUtils.isOsVersionMatch(osVersion, "10.1"));
        osVersion = "10";
        assertTrue(SystemUtils.isOsVersionMatch(osVersion, "10.1"));
        assertTrue(SystemUtils.isOsVersionMatch(osVersion, "10.1.1"));
        assertTrue(SystemUtils.isOsVersionMatch(osVersion, "10.10"));
        assertTrue(SystemUtils.isOsVersionMatch(osVersion, "10.10.1"));
        osVersion = "10.1";
        assertTrue(SystemUtils.isOsVersionMatch(osVersion, "10.1"));
        assertTrue(SystemUtils.isOsVersionMatch(osVersion, "10.1.1"));
        assertFalse(SystemUtils.isOsVersionMatch(osVersion, "10.10"));
        assertFalse(SystemUtils.isOsVersionMatch(osVersion, "10.10.1"));
        osVersion = "10.1.1";
        assertTrue(SystemUtils.isOsVersionMatch(osVersion, "10.1"));
        assertTrue(SystemUtils.isOsVersionMatch(osVersion, "10.1.1"));
        assertFalse(SystemUtils.isOsVersionMatch(osVersion, "10.10"));
        assertFalse(SystemUtils.isOsVersionMatch(osVersion, "10.10.1"));
        osVersion = "10.10";
        assertFalse(SystemUtils.isOsVersionMatch(osVersion, "10.1"));
        assertFalse(SystemUtils.isOsVersionMatch(osVersion, "10.1.1"));
        assertTrue(SystemUtils.isOsVersionMatch(osVersion, "10.10"));
        assertTrue(SystemUtils.isOsVersionMatch(osVersion, "10.10.1"));
        osVersion = "10.10.1";
        assertFalse(SystemUtils.isOsVersionMatch(osVersion, "10.1"));
        assertFalse(SystemUtils.isOsVersionMatch(osVersion, "10.1.1"));
        assertTrue(SystemUtils.isOsVersionMatch(osVersion, "10.10"));
        assertTrue(SystemUtils.isOsVersionMatch(osVersion, "10.10.1"));
    }
}
