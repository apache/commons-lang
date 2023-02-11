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

import java.util.function.Supplier;

/**
 * Accesses current system property names and values.
 *
 * @since 3.13.0
 */
public final class SystemProperties {

    private static final Supplier<String> NULL_SUPPLIER = () -> null;

    /**
     * The System property name {@value}.
     */
    public static final String AWT_TOOLKIT = "awt.toolkit";

    /**
     * The System property name {@value}.
     */
    public static final String FILE_ENCODING = "file.encoding";

    /**
     * The System property name {@value}.
     */
    public static final String FILE_SEPARATOR = "file.separator";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_AWT_FONTS = "java.awt.fonts";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_AWT_GRAPHICSENV = "java.awt.graphicsenv";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_AWT_HEADLESS = "java.awt.headless";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_AWT_PRINTERJOB = "java.awt.printerjob";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_CLASS_PATH = "java.class.path";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_CLASS_VERSION = "java.class.version";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_COMPILER = "java.compiler";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_ENDORSED_DIRS = "java.endorsed.dirs";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_EXT_DIRS = "java.ext.dirs";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_HOME = "java.home";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_IO_TMPDIR = "java.io.tmpdir";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_LIBRARY_PATH = "java.library.path";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_RUNTIME_NAME = "java.runtime.name";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_RUNTIME_VERSION = "java.runtime.version";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_SPECIFICATION_NAME = "java.specification.name";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_SPECIFICATION_VENDOR = "java.specification.vendor";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_SPECIFICATION_VERSION = "java.specification.version";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_UTIL_PREFS_PREFERENCES_FACTORY = "java.util.prefs.PreferencesFactory";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_VENDOR = "java.vendor";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_VENDOR_URL = "java.vendor.url";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_VERSION = "java.version";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_VM_INFO = "java.vm.info";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_VM_NAME = "java.vm.name";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_VM_SPECIFICATION_NAME = "java.vm.specification.name";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_VM_SPECIFICATION_VENDOR = "java.vm.specification.vendor";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_VM_SPECIFICATION_VERSION = "java.vm.specification.version";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_VM_VENDOR = "java.vm.vendor";

    /**
     * The System property name {@value}.
     */
    public static final String JAVA_VM_VERSION = "java.vm.version";

    /**
     * The System property name {@value}.
     */
    public static final String LINE_SEPARATOR = "line.separator";

    /**
     * The System property name {@value}.
     */
    public static final String OS_ARCH = "os.arch";

    /**
     * The System property name {@value}.
     */
    public static final String OS_NAME = "os.name";

    /**
     * The System property name {@value}.
     */
    public static final String OS_VERSION = "os.version";

    /**
     * The System property name {@value}.
     */
    public static final String PATH_SEPARATOR = "path.separator";

    /**
     * The System property name {@value}.
     */
    public static final String USER_COUNTRY = "user.country";

    /**
     * The System property name {@value}.
     */
    public static final String USER_DIR = "user.dir";

    /**
     * The System property name {@value}.
     */
    public static final String USER_HOME = "user.home";

    /**
     * The System property name {@value}.
     */
    public static final String USER_LANGUAGE = "user.language";

    /**
     * The System property name {@value}.
     */
    public static final String USER_NAME = "user.name";

    /**
     * The System property name {@value}.
     */
    public static final String USER_REGION = "user.region";

    /**
     * The System property name {@value}.
     */
    public static final String USER_TIMEZONE = "user.timezone";

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getAwtToolkit() {
        return getProperty(AWT_TOOLKIT);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getFileEncoding() {
        return getProperty(FILE_ENCODING);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getFileSeparator() {
        return getProperty(FILE_SEPARATOR);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaAwtFonts() {
        return getProperty(JAVA_AWT_FONTS);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaAwtGraphicsenv() {
        return getProperty(JAVA_AWT_GRAPHICSENV);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaAwtHeadless() {
        return getProperty(JAVA_AWT_HEADLESS);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaAwtPrinterjob() {
        return getProperty(JAVA_AWT_PRINTERJOB);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaClassPath() {
        return getProperty(JAVA_CLASS_PATH);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaClassVersion() {
        return getProperty(JAVA_CLASS_VERSION);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaCompiler() {
        return getProperty(JAVA_COMPILER);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaEndorsedDirs() {
        return getProperty(JAVA_ENDORSED_DIRS);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaExtDirs() {
        return getProperty(JAVA_EXT_DIRS);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaHome() {
        return getProperty(JAVA_HOME);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaIoTmpdir() {
        return getProperty(JAVA_IO_TMPDIR);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaLibraryPath() {
        return getProperty(JAVA_LIBRARY_PATH);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaRuntimeName() {
        return getProperty(JAVA_RUNTIME_NAME);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaRuntimeVersion() {
        return getProperty(JAVA_RUNTIME_VERSION);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaSpecificationName() {
        return getProperty(JAVA_SPECIFICATION_NAME);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaSpecificationVendor() {
        return getProperty(JAVA_SPECIFICATION_VENDOR);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaSpecificationVersion() {
        return getProperty(JAVA_SPECIFICATION_VERSION);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaUtilPrefsPreferencesFactory() {
        return getProperty(JAVA_UTIL_PREFS_PREFERENCES_FACTORY);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaVendor() {
        return getProperty(JAVA_VENDOR);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaVendorUrl() {
        return getProperty(JAVA_VENDOR_URL);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaVersion() {
        return getProperty(JAVA_VERSION);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaVmInfo() {
        return getProperty(JAVA_VM_INFO);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaVmName() {
        return getProperty(JAVA_VM_NAME);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaVmSpecificationName() {
        return getProperty(JAVA_VM_SPECIFICATION_NAME);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaVmSpecificationVendor() {
        return getProperty(JAVA_VM_SPECIFICATION_VENDOR);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaVmSpecificationVersion() {
        return getProperty(JAVA_VM_SPECIFICATION_VERSION);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaVmVendor() {
        return getProperty(JAVA_VM_VENDOR);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getJavaVmVersion() {
        return getProperty(JAVA_VM_VERSION);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getLineSeparator() {
        return getProperty(LINE_SEPARATOR);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getOsArch() {
        return getProperty(OS_ARCH);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getOsName() {
        return getProperty(OS_NAME);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getOsVersion() {
        return getProperty(OS_VERSION);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getPathSeparator() {
        return getProperty(PATH_SEPARATOR);
    }

    /**
     * Gets a System property, defaulting to {@code null} if the property cannot be read.
     * <p>
     * If a {@link SecurityException} is caught, the return value is {@code null}.
     * </p>
     *
     * @param property the system property name
     * @return the system property value or {@code null} if a security problem occurs
     */
    public static String getProperty(final String property) {
        return getProperty(property, NULL_SUPPLIER);
    }

    /**
     * Gets a System property, defaulting to {@code null} if the property cannot be read.
     * <p>
     * If a {@link SecurityException} is caught, the return value is {@code null}.
     * </p>
     *
     * @param property        the system property name.
     * @param defaultValue get this Supplier when the property is empty or throws SecurityException.
     * @return the system property value or {@code null} if a security problem occurs
     */
    static String getProperty(final String property, final Supplier<String> defaultValue) {
        try {
            if (StringUtils.isEmpty(property)) {
                return defaultValue.get();
            }
            final String value = System.getProperty(property);
            return StringUtils.getIfEmpty(value, defaultValue);
        } catch (final SecurityException ignore) {
            // We are not allowed to look at this property.
            //
            // System.err.println("Caught a SecurityException reading the system property '" + property
            // + "'; the SystemUtils property value will default to null.");
            return defaultValue.get();
        }
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getUserCountry() {
        return getProperty(USER_COUNTRY);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getUserDir() {
        return getProperty(USER_DIR);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getUserHome() {
        return getProperty(USER_HOME);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getUserLanguage() {
        return getProperty(USER_LANGUAGE);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getUserName() {
        return getProperty(USER_NAME);
    }

    /**
     * Gets the current value from the system properties map.
     * <p>
     * Returns {@code null} if the property cannot be read due to a {@link SecurityException}.
     * </p>
     *
     * @return the current value from the system properties map.
     */
    public static String getUserTimezone() {
        return getProperty(USER_TIMEZONE);
    }
}
