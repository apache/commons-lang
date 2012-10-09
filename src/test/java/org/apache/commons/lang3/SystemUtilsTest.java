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
import static org.apache.commons.lang3.JavaVersion.JAVA_1_4;

import java.io.File;
import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.util.Locale;

import junit.framework.Assert;

/**
 * Unit tests {@link org.apache.commons.lang3.SystemUtils}.
 * 
 * Only limited testing can be performed.
 * 
 * @version $Id$
 */
public class SystemUtilsTest {

    @Test
    public void testConstructor() {
        assertNotNull(new SystemUtils());
        Constructor<?>[] cons = SystemUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertTrue(Modifier.isPublic(cons[0].getModifiers()));
        assertTrue(Modifier.isPublic(SystemUtils.class.getModifiers()));
        assertFalse(Modifier.isFinal(SystemUtils.class.getModifiers()));
    }

    /**
     * Assums no security manager exists.
     */
    @Test
    public void testGetJavaHome() {
        File dir = SystemUtils.getJavaHome();
        Assert.assertNotNull(dir);
        Assert.assertTrue(dir.exists());
    }

    /**
     * Assums no security manager exists.
     */
    @Test
    public void testGetJavaIoTmpDir() {
        File dir = SystemUtils.getJavaIoTmpDir();
        Assert.assertNotNull(dir);
        Assert.assertTrue(dir.exists());
    }

    /**
     * Assums no security manager exists.
     */
    @Test
    public void testGetUserDir() {
        File dir = SystemUtils.getUserDir();
        Assert.assertNotNull(dir);
        Assert.assertTrue(dir.exists());
    }

    /**
     * Assums no security manager exists.
     */
    @Test
    public void testGetUserHome() {
        File dir = SystemUtils.getUserHome();
        Assert.assertNotNull(dir);
        Assert.assertTrue(dir.exists());
    }

    @Test
    public void testIS_JAVA() {
        String javaVersion = System.getProperty("java.version");
        if (javaVersion == null) {
            assertFalse(SystemUtils.IS_JAVA_1_1);
            assertFalse(SystemUtils.IS_JAVA_1_2);
            assertFalse(SystemUtils.IS_JAVA_1_3);
            assertFalse(SystemUtils.IS_JAVA_1_4);
            assertFalse(SystemUtils.IS_JAVA_1_5);
            assertFalse(SystemUtils.IS_JAVA_1_6);
            assertFalse(SystemUtils.IS_JAVA_1_7);
        } else if (javaVersion.startsWith("1.1")) {
            assertTrue(SystemUtils.IS_JAVA_1_1);
            assertFalse(SystemUtils.IS_JAVA_1_2);
            assertFalse(SystemUtils.IS_JAVA_1_3);
            assertFalse(SystemUtils.IS_JAVA_1_4);
            assertFalse(SystemUtils.IS_JAVA_1_5);
            assertFalse(SystemUtils.IS_JAVA_1_6);
            assertFalse(SystemUtils.IS_JAVA_1_7);
        } else if (javaVersion.startsWith("1.2")) {
            assertFalse(SystemUtils.IS_JAVA_1_1);
            assertTrue(SystemUtils.IS_JAVA_1_2);
            assertFalse(SystemUtils.IS_JAVA_1_3);
            assertFalse(SystemUtils.IS_JAVA_1_4);
            assertFalse(SystemUtils.IS_JAVA_1_5);
            assertFalse(SystemUtils.IS_JAVA_1_6);
            assertFalse(SystemUtils.IS_JAVA_1_7);
        } else if (javaVersion.startsWith("1.3")) {
            assertFalse(SystemUtils.IS_JAVA_1_1);
            assertFalse(SystemUtils.IS_JAVA_1_2);
            assertTrue(SystemUtils.IS_JAVA_1_3);
            assertFalse(SystemUtils.IS_JAVA_1_4);
            assertFalse(SystemUtils.IS_JAVA_1_5);
            assertFalse(SystemUtils.IS_JAVA_1_6);
            assertFalse(SystemUtils.IS_JAVA_1_7);
        } else if (javaVersion.startsWith("1.4")) {
            assertFalse(SystemUtils.IS_JAVA_1_1);
            assertFalse(SystemUtils.IS_JAVA_1_2);
            assertFalse(SystemUtils.IS_JAVA_1_3);
            assertTrue(SystemUtils.IS_JAVA_1_4);
            assertFalse(SystemUtils.IS_JAVA_1_5);
            assertFalse(SystemUtils.IS_JAVA_1_6);
            assertFalse(SystemUtils.IS_JAVA_1_7);
        } else if (javaVersion.startsWith("1.5")) {
            assertFalse(SystemUtils.IS_JAVA_1_1);
            assertFalse(SystemUtils.IS_JAVA_1_2);
            assertFalse(SystemUtils.IS_JAVA_1_3);
            assertFalse(SystemUtils.IS_JAVA_1_4);
            assertTrue(SystemUtils.IS_JAVA_1_5);
            assertFalse(SystemUtils.IS_JAVA_1_6);
            assertFalse(SystemUtils.IS_JAVA_1_7);
        } else if (javaVersion.startsWith("1.6")) {
            assertFalse(SystemUtils.IS_JAVA_1_1);
            assertFalse(SystemUtils.IS_JAVA_1_2);
            assertFalse(SystemUtils.IS_JAVA_1_3);
            assertFalse(SystemUtils.IS_JAVA_1_4);
            assertFalse(SystemUtils.IS_JAVA_1_5);
            assertTrue(SystemUtils.IS_JAVA_1_6);
            assertFalse(SystemUtils.IS_JAVA_1_7);
        } else {
            System.out.println("Can't test IS_JAVA value: "+javaVersion);
        }
    }

    @Test
    public void testIS_OS() {
        String osName = System.getProperty("os.name");
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
            System.out.println("Can't test IS_OS value: "+osName);
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
        javaVersion = "";
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        javaVersion = "1.0";
        assertTrue(SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        javaVersion = "1.1";
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertTrue(SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        javaVersion = "1.2";
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertTrue(SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        javaVersion = "1.3.0";
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertTrue(SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        javaVersion = "1.3.1";
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertTrue(SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        javaVersion = "1.4.0";
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertTrue(SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        javaVersion = "1.4.1";
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertTrue(SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        javaVersion = "1.4.2";
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertTrue(SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        javaVersion = "1.5.0";
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertTrue(SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        javaVersion = "1.6.0";
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertTrue(SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        javaVersion = "1.7.0";
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertFalse(SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertTrue(SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
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
    public void testJavaAwtHeadless() {
        boolean atLeastJava14 = SystemUtils.isJavaVersionAtLeast(JAVA_1_4);
        String expectedStringValue = System.getProperty("java.awt.headless");
        String expectedStringValueWithDefault = System.getProperty("java.awt.headless", "false");
        assertNotNull(expectedStringValueWithDefault);
        if (atLeastJava14) {
            boolean expectedValue = Boolean.valueOf(expectedStringValue).booleanValue();
            if (expectedStringValue != null) {
                assertEquals(expectedStringValue, SystemUtils.JAVA_AWT_HEADLESS);
            }
            assertEquals(expectedValue, SystemUtils.isJavaAwtHeadless());
        } else {
            assertNull(expectedStringValue);
            assertNull(SystemUtils.JAVA_AWT_HEADLESS);
            assertEquals(expectedStringValueWithDefault, "" + SystemUtils.isJavaAwtHeadless());
        }
        assertEquals(expectedStringValueWithDefault, "" + SystemUtils.isJavaAwtHeadless());
    }
}
