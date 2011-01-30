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

import java.io.File;
import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.util.Locale;

import junit.framework.Assert;
import junit.framework.TestCase;

import static org.apache.commons.lang3.JavaVersion.*;

/**
 * Unit tests {@link org.apache.commons.lang3.SystemUtils}.
 * 
 * Only limited testing can be performed.
 * 
 * @author Apache Software Foundation
 * @author Tetsuya Kaneuchi
 * @author Gary D. Gregory
 * @version $Id$
 */
public class SystemUtilsTest extends TestCase {

    public SystemUtilsTest(String name) {
        super(name);
    }

    public void testConstructor() {
        assertNotNull(new SystemUtils());
        Constructor<?>[] cons = SystemUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertEquals(true, Modifier.isPublic(cons[0].getModifiers()));
        assertEquals(true, Modifier.isPublic(SystemUtils.class.getModifiers()));
        assertEquals(false, Modifier.isFinal(SystemUtils.class.getModifiers()));
    }

    /**
     * Assums no security manager exists.
     */
    public void testGetJavaHome() {
        File dir = SystemUtils.getJavaHome();
        Assert.assertNotNull(dir);
        Assert.assertTrue(dir.exists());
    }

    /**
     * Assums no security manager exists.
     */
    public void testGetJavaIoTmpDir() {
        File dir = SystemUtils.getJavaIoTmpDir();
        Assert.assertNotNull(dir);
        Assert.assertTrue(dir.exists());
    }

    /**
     * Assums no security manager exists.
     */
    public void testGetUserDir() {
        File dir = SystemUtils.getUserDir();
        Assert.assertNotNull(dir);
        Assert.assertTrue(dir.exists());
    }

    /**
     * Assums no security manager exists.
     */
    public void testGetUserHome() {
        File dir = SystemUtils.getUserHome();
        Assert.assertNotNull(dir);
        Assert.assertTrue(dir.exists());
    }

    public void testIS_JAVA() {
        String javaVersion = System.getProperty("java.version");
        if (javaVersion == null) {
            assertEquals(false, SystemUtils.IS_JAVA_1_1);
            assertEquals(false, SystemUtils.IS_JAVA_1_2);
            assertEquals(false, SystemUtils.IS_JAVA_1_3);
            assertEquals(false, SystemUtils.IS_JAVA_1_4);
            assertEquals(false, SystemUtils.IS_JAVA_1_5);
            assertEquals(false, SystemUtils.IS_JAVA_1_6);
            assertEquals(false, SystemUtils.IS_JAVA_1_7);
        } else if (javaVersion.startsWith("1.1")) {
            assertEquals(true, SystemUtils.IS_JAVA_1_1);
            assertEquals(false, SystemUtils.IS_JAVA_1_2);
            assertEquals(false, SystemUtils.IS_JAVA_1_3);
            assertEquals(false, SystemUtils.IS_JAVA_1_4);
            assertEquals(false, SystemUtils.IS_JAVA_1_5);
            assertEquals(false, SystemUtils.IS_JAVA_1_6);
            assertEquals(false, SystemUtils.IS_JAVA_1_7);
        } else if (javaVersion.startsWith("1.2")) {
            assertEquals(false, SystemUtils.IS_JAVA_1_1);
            assertEquals(true, SystemUtils.IS_JAVA_1_2);
            assertEquals(false, SystemUtils.IS_JAVA_1_3);
            assertEquals(false, SystemUtils.IS_JAVA_1_4);
            assertEquals(false, SystemUtils.IS_JAVA_1_5);
            assertEquals(false, SystemUtils.IS_JAVA_1_6);
            assertEquals(false, SystemUtils.IS_JAVA_1_7);
        } else if (javaVersion.startsWith("1.3")) {
            assertEquals(false, SystemUtils.IS_JAVA_1_1);
            assertEquals(false, SystemUtils.IS_JAVA_1_2);
            assertEquals(true, SystemUtils.IS_JAVA_1_3);
            assertEquals(false, SystemUtils.IS_JAVA_1_4);
            assertEquals(false, SystemUtils.IS_JAVA_1_5);
            assertEquals(false, SystemUtils.IS_JAVA_1_6);
            assertEquals(false, SystemUtils.IS_JAVA_1_7);
        } else if (javaVersion.startsWith("1.4")) {
            assertEquals(false, SystemUtils.IS_JAVA_1_1);
            assertEquals(false, SystemUtils.IS_JAVA_1_2);
            assertEquals(false, SystemUtils.IS_JAVA_1_3);
            assertEquals(true, SystemUtils.IS_JAVA_1_4);
            assertEquals(false, SystemUtils.IS_JAVA_1_5);
            assertEquals(false, SystemUtils.IS_JAVA_1_6);
            assertEquals(false, SystemUtils.IS_JAVA_1_7);
        } else if (javaVersion.startsWith("1.5")) {
            assertEquals(false, SystemUtils.IS_JAVA_1_1);
            assertEquals(false, SystemUtils.IS_JAVA_1_2);
            assertEquals(false, SystemUtils.IS_JAVA_1_3);
            assertEquals(false, SystemUtils.IS_JAVA_1_4);
            assertEquals(true, SystemUtils.IS_JAVA_1_5);
            assertEquals(false, SystemUtils.IS_JAVA_1_6);
            assertEquals(false, SystemUtils.IS_JAVA_1_7);
        } else if (javaVersion.startsWith("1.6")) {
            assertEquals(false, SystemUtils.IS_JAVA_1_1);
            assertEquals(false, SystemUtils.IS_JAVA_1_2);
            assertEquals(false, SystemUtils.IS_JAVA_1_3);
            assertEquals(false, SystemUtils.IS_JAVA_1_4);
            assertEquals(false, SystemUtils.IS_JAVA_1_5);
            assertEquals(true, SystemUtils.IS_JAVA_1_6);
            assertEquals(false, SystemUtils.IS_JAVA_1_7);
        } else {
            System.out.println("Can't test IS_JAVA value");
        }
    }

    public void testIS_OS() {
        String osName = System.getProperty("os.name");
        if (osName == null) {
            assertEquals(false, SystemUtils.IS_OS_WINDOWS);
            assertEquals(false, SystemUtils.IS_OS_UNIX);
            assertEquals(false, SystemUtils.IS_OS_SOLARIS);
            assertEquals(false, SystemUtils.IS_OS_LINUX);
            assertEquals(false, SystemUtils.IS_OS_MAC_OSX);
        } else if (osName.startsWith("Windows")) {
            assertEquals(false, SystemUtils.IS_OS_UNIX);
            assertEquals(true, SystemUtils.IS_OS_WINDOWS);
        } else if (osName.startsWith("Solaris")) {
            assertEquals(true, SystemUtils.IS_OS_SOLARIS);
            assertEquals(true, SystemUtils.IS_OS_UNIX);
            assertEquals(false, SystemUtils.IS_OS_WINDOWS);
        } else if (osName.toLowerCase(Locale.ENGLISH).startsWith("linux")) {
            assertEquals(true, SystemUtils.IS_OS_LINUX);
            assertEquals(true, SystemUtils.IS_OS_UNIX);
            assertEquals(false, SystemUtils.IS_OS_WINDOWS);
        } else if (osName.startsWith("Mac OS X")) {
            assertEquals(true, SystemUtils.IS_OS_MAC_OSX);
            assertEquals(true, SystemUtils.IS_OS_UNIX);
            assertEquals(false, SystemUtils.IS_OS_WINDOWS);
        } else if (osName.startsWith("OS/2")) {
            assertEquals(true, SystemUtils.IS_OS_OS2);
            assertEquals(false, SystemUtils.IS_OS_UNIX);
            assertEquals(false, SystemUtils.IS_OS_WINDOWS);
        } else if (osName.startsWith("SunOS")) {
            assertEquals(true, SystemUtils.IS_OS_SUN_OS);
            assertEquals(true, SystemUtils.IS_OS_UNIX);
            assertEquals(false, SystemUtils.IS_OS_WINDOWS);
        } else {
            System.out.println("Can't test IS_OS value");
        }
    }

    public void testJavaVersionMatches() {
        String javaVersion = null;
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        javaVersion = "";
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        javaVersion = "1.0";
        assertEquals(true, SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        javaVersion = "1.1";
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertEquals(true, SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        javaVersion = "1.2";
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertEquals(true, SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        javaVersion = "1.3.0";
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertEquals(true, SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        javaVersion = "1.3.1";
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertEquals(true, SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        javaVersion = "1.4.0";
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertEquals(true, SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        javaVersion = "1.4.1";
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertEquals(true, SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        javaVersion = "1.4.2";
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertEquals(true, SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        javaVersion = "1.5.0";
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertEquals(true, SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        javaVersion = "1.6.0";
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertEquals(true, SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
        javaVersion = "1.7.0";
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.0"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.1"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.2"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.3"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.4"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.5"));
        assertEquals(false, SystemUtils.isJavaVersionMatch(javaVersion, "1.6"));
        assertEquals(true, SystemUtils.isJavaVersionMatch(javaVersion, "1.7"));
    }

    public void testOSMatchesName() {
        String osName = null;
        assertEquals(false, SystemUtils.isOSNameMatch(osName, "Windows"));
        osName = "";
        assertEquals(false, SystemUtils.isOSNameMatch(osName, "Windows"));
        osName = "Windows 95";
        assertEquals(true, SystemUtils.isOSNameMatch(osName, "Windows"));
        osName = "Windows NT";
        assertEquals(true, SystemUtils.isOSNameMatch(osName, "Windows"));
        osName = "OS/2";
        assertEquals(false, SystemUtils.isOSNameMatch(osName, "Windows"));
    }

    public void testOSMatchesNameAndVersion() {
        String osName = null;
        String osVersion = null;
        assertEquals(false, SystemUtils.isOSMatch(osName, osVersion, "Windows 9", "4.1"));
        osName = "";
        osVersion = "";
        assertEquals(false, SystemUtils.isOSMatch(osName, osVersion, "Windows 9", "4.1"));
        osName = "Windows 95";
        osVersion = "4.0";
        assertEquals(false, SystemUtils.isOSMatch(osName, osVersion, "Windows 9", "4.1"));
        osName = "Windows 95";
        osVersion = "4.1";
        assertEquals(true, SystemUtils.isOSMatch(osName, osVersion, "Windows 9", "4.1"));
        osName = "Windows 98";
        osVersion = "4.1";
        assertEquals(true, SystemUtils.isOSMatch(osName, osVersion, "Windows 9", "4.1"));
        osName = "Windows NT";
        osVersion = "4.0";
        assertEquals(false, SystemUtils.isOSMatch(osName, osVersion, "Windows 9", "4.1"));
        osName = "OS/2";
        osVersion = "4.0";
        assertEquals(false, SystemUtils.isOSMatch(osName, osVersion, "Windows 9", "4.1"));
    }

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
