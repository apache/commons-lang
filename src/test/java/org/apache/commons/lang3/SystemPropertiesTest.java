/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.commons.lang3;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import org.junit.jupiter.api.Test;

public class SystemPropertiesTest {

    private boolean isJava11OrGreater() {
        return SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_11);
    }

    @Test
    public void testGetAwtToolkit() {
        assertDoesNotThrow(SystemProperties::getAwtToolkit);
    }

    @Test
    public void testGetFileEncoding() {
        assertNotNull(SystemProperties.getFileEncoding());
    }

    @Test
    public void testGetFileSeparator() {
        assertNotNull(SystemProperties.getFileSeparator());
    }

    @Test
    public void testGetJavaAwtFonts() {
        assertNull(SystemProperties.getJavaAwtFonts());
    }

    @Test
    public void testGetJavaAwtGraphicsenv() {
        assertDoesNotThrow(SystemProperties::getJavaAwtGraphicsenv);
    }

    @Test
    public void testGetJavaAwtHeadless() {
        assertNull(SystemProperties.getJavaAwtHeadless());
    }

    @Test
    public void testGetJavaAwtPrinterjob() {
        assertDoesNotThrow(SystemProperties::getJavaAwtPrinterjob);
    }

    @Test
    public void testGetJavaClassPath() {
        assertNotNull(SystemProperties.getJavaClassPath());
    }

    @Test
    public void testGetJavaClassVersion() {
        assertNotNull(SystemProperties.getJavaClassVersion());
    }

    @Test
    public void testGetJavaCompiler() {
        if (SystemUtils.IS_JAVA_14) {
            // Not in Java 11
            assertNotNull(SystemProperties.getJavaCompiler());
        }
    }

    @Test
    public void testGetJavaEndorsedDirs() {
        if (isJava11OrGreater()) {
            // Not in Java 11
            assertNull(SystemProperties.getJavaEndorsedDirs());
        } else {
            assertNotNull(SystemProperties.getJavaExtDirs());
        }
    }

    @Test
    public void testGetJavaExtDirs() {
        if (isJava11OrGreater()) {
            // Not in Java 11
            assertNull(SystemProperties.getJavaExtDirs());
        } else {
            assertNotNull(SystemProperties.getJavaExtDirs());
        }
    }

    @Test
    public void testGetJavaHome() {
        assertNotNull(SystemProperties.getJavaHome());
    }

    @Test
    public void testGetJavaIoTmpdir() {
        assertNotNull(SystemProperties.getJavaIoTmpdir());
    }

    @Test
    public void testGetJavaLibraryPath() {
        assertNotNull(SystemProperties.getJavaLibraryPath());
    }

    @Test
    public void testGetJavaRuntimeName() {
        assertNotNull(SystemProperties.getJavaRuntimeName());
    }

    @Test
    public void testGetJavaRuntimeVersion() {
        assertNotNull(SystemProperties.getJavaRuntimeVersion());
    }

    @Test
    public void testGetJavaSpecificationName() {
        assertNotNull(SystemProperties.getJavaSpecificationName());
    }

    @Test
    public void testGetJavaSpecificationVendor() {
        assertNotNull(SystemProperties.getJavaSpecificationVendor());
    }

    @Test
    public void testGetJavaSpecificationVersion() {
        assertNotNull(SystemProperties.getJavaSpecificationVersion());
    }

    @Test
    public void testGetJavaUtilPrefsPreferencesFactory() {
        assertNull(SystemProperties.getJavaUtilPrefsPreferencesFactory());
    }

    @Test
    public void testGetJavaVendor() {
        assertNotNull(SystemProperties.getJavaVendor());
    }

    @Test
    public void testGetJavaVendorUrl() {
        assertNotNull(SystemProperties.getJavaVendorUrl());
    }

    @Test
    public void testGetJavaVersion() {
        assertNotNull(SystemProperties.getJavaVersion());
    }

    @Test
    public void testGetJavaVmInfo() {
        assertNotNull(SystemProperties.getJavaVmInfo());
    }

    @Test
    public void testGetJavaVmName() {
        assertNotNull(SystemProperties.getJavaVmName());
    }

    @Test
    public void testGetJavaVmSpecificationName() {
        assertNotNull(SystemProperties.getJavaVmSpecificationName());
    }

    @Test
    public void testGetJavaVmSpecificationVendor() {
        assertNotNull(SystemProperties.getJavaVmSpecificationVendor());
    }

    @Test
    public void testGetJavaVmSpecificationVersion() {
        assertNotNull(SystemProperties.getJavaVmSpecificationVersion());
    }

    @Test
    public void testGetJavaVmVendor() {
        assertNotNull(SystemProperties.getJavaVmVendor());
    }

    @Test
    public void testGetJavaVmVersion() {
        assertNotNull(SystemProperties.getJavaVmVersion());
    }

    @Test
    public void testGetLineSeparator() {
        assertNotNull(SystemProperties.getLineSeparator());
    }

    @Test
    public void testGetOsArch() {
        assertNotNull(SystemProperties.getOsArch());
    }

    @Test
    public void testGetOsName() {
        assertNotNull(SystemProperties.getOsName());
    }

    @Test
    public void testGetOsVersion() {
        assertNotNull(SystemProperties.getOsVersion());
    }

    @Test
    public void testGetPathSeparator() {
        assertNotNull(SystemProperties.getPathSeparator());
    }

    @Test
    public void testGetUserCountry() {
        assertDoesNotThrow(SystemProperties::getUserCountry);
    }

    @Test
    public void testGetUserDir() {
        assertNotNull(SystemProperties.getUserDir());
    }

    @Test
    public void testGetUserHome() {
        assertNotNull(SystemProperties.getUserHome());
    }

    @Test
    public void testGetUserLanguage() {
        assertNotNull(SystemProperties.getUserLanguage());
    }

    @Test
    public void testGetUserName() {
        assertNotNull(SystemProperties.getUserName());
    }

    @Test
    public void testGetUserTimezone() {
        assertDoesNotThrow(SystemProperties::getUserTimezone);
    }

}
