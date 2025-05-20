/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang3;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * Helpers for {@link System}.
 *
 * <p>
 * If a system property cannot be read due to security restrictions, the corresponding field in this class will be set to {@code null} and a message will be
 * written to {@code System.err}.
 * </p>
 * <p>
 * #ThreadSafe#
 * </p>
 *
 * @since 1.0
 * @see SystemProperties
 */
public class SystemUtils {

    /**
     * The prefix String for all Windows OS.
     */
    private static final String OS_NAME_WINDOWS_PREFIX = "Windows";

    // System property constants
    // -----------------------------------------------------------------------
    // These MUST be declared first. Other constants depend on this.

    /**
     * A constant for the System Property {@code file.encoding}.
     *
     * <p>
     * File encoding, such as {@code Cp1252}.
     * </p>
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getFileEncoding()
     * @since 2.0
     * @since Java 1.2
     */
    public static final String FILE_ENCODING = SystemProperties.getFileEncoding();

    /**
     * A constant for the System Property {@code file.separator}.
     * <p>
     * The file separator is:
     * </p>
     * <ul>
     * <li>{@code "/"} on Unix</li>
     * <li>{@code "\"} on Windows.</li>
     * </ul>
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getFileSeparator()
     * @deprecated Use {@link File#separator}, since it is guaranteed to be a string containing a single character and it does not require a privilege check.
     * @since Java 1.1
     */
    @Deprecated
    public static final String FILE_SEPARATOR = SystemProperties.getFileSeparator();

    /**
     * A constant for the System Property {@code java.awt.fonts}.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaAwtFonts()
     * @since 2.1
     */
    public static final String JAVA_AWT_FONTS = SystemProperties.getJavaAwtFonts();

    /**
     * A constant for the System Property {@code java.awt.graphicsenv}.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaAwtGraphicsenv()
     * @since 2.1
     */
    public static final String JAVA_AWT_GRAPHICSENV = SystemProperties.getJavaAwtGraphicsenv();

    /**
     * A constant for the System Property {@code java.awt.headless}. The value of this property is the String {@code "true"} or {@code "false"}.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see #isJavaAwtHeadless()
     * @see SystemProperties#getJavaAwtHeadless()
     * @since 2.1
     * @since Java 1.4
     */
    public static final String JAVA_AWT_HEADLESS = SystemProperties.getJavaAwtHeadless();

    /**
     * A constant for the System Property {@code java.awt.printerjob}.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaAwtPrinterjob()
     * @since 2.1
     */
    public static final String JAVA_AWT_PRINTERJOB = SystemProperties.getJavaAwtPrinterjob();

    /**
     * A constant for the System Property {@code java.class.path}. Java class path.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaClassPath()
     * @since Java 1.1
     */
    public static final String JAVA_CLASS_PATH = SystemProperties.getJavaClassPath();

    /**
     * A constant for the System Property {@code java.class.version}. Java class format version number.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaClassVersion()
     * @since Java 1.1
     */
    public static final String JAVA_CLASS_VERSION = SystemProperties.getJavaClassVersion();

    /**
     * A constant for the System Property {@code java.compiler}. Name of JIT compiler to use. First in JDK version 1.2. Not used in Sun JDKs after 1.2.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaCompiler()
     * @since Java 1.2. Not used in Sun versions after 1.2.
     */
    public static final String JAVA_COMPILER = SystemProperties.getJavaCompiler();

    /**
     * A constant for the System Property {@code java.endorsed.dirs}. Path of endorsed directory or directories.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaEndorsedDirs()
     * @since Java 1.4
     */
    public static final String JAVA_ENDORSED_DIRS = SystemProperties.getJavaEndorsedDirs();

    /**
     * A constant for the System Property {@code java.ext.dirs}. Path of extension directory or directories.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaExtDirs()
     * @since Java 1.3
     */
    public static final String JAVA_EXT_DIRS = SystemProperties.getJavaExtDirs();

    /**
     * A constant for the System Property {@code java.home}. Java installation directory.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaHome()
     * @since Java 1.1
     */
    public static final String JAVA_HOME = SystemProperties.getJavaHome();

    /**
     * A constant for the System Property {@code java.io.tmpdir}. Default temp file path.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaIoTmpdir()
     * @since Java 1.2
     */
    public static final String JAVA_IO_TMPDIR = SystemProperties.getJavaIoTmpdir();

    /**
     * A constant for the System Property {@code java.library.path}. List of paths to search when loading libraries.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaLibraryPath()
     * @since Java 1.2
     */
    public static final String JAVA_LIBRARY_PATH = SystemProperties.getJavaLibraryPath();

    /**
     * A constant for the System Property {@code java.runtime.name}. Java Runtime Environment name.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaRuntimeName()
     * @since 2.0
     * @since Java 1.3
     */
    public static final String JAVA_RUNTIME_NAME = SystemProperties.getJavaRuntimeName();

    /**
     * A constant for the System Property {@code java.runtime.version}. Java Runtime Environment version.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaRuntimeVersion()
     * @since 2.0
     * @since Java 1.3
     */
    public static final String JAVA_RUNTIME_VERSION = SystemProperties.getJavaRuntimeVersion();

    /**
     * A constant for the System Property {@code java.specification.name}. Java Runtime Environment specification name.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaSpecificationName()
     * @since Java 1.2
     */
    public static final String JAVA_SPECIFICATION_NAME = SystemProperties.getJavaSpecificationName();

    /**
     * A constant for the System Property {@code java.specification.vendor}. Java Runtime Environment specification vendor.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaSpecificationVendor()
     * @since Java 1.2
     */
    public static final String JAVA_SPECIFICATION_VENDOR = SystemProperties.getJavaSpecificationVendor();

    /**
     * A constant for the System Property {@code java.specification.version}. Java Runtime Environment specification version.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaSpecificationVersion()
     * @since Java 1.3
     */
    public static final String JAVA_SPECIFICATION_VERSION = SystemProperties.getJavaSpecificationVersion();

    private static final JavaVersion JAVA_SPECIFICATION_VERSION_AS_ENUM = JavaVersion.get(JAVA_SPECIFICATION_VERSION);

    /**
     * A constant for the System Property {@code java.util.prefs.PreferencesFactory}. A class name.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaUtilPrefsPreferencesFactory()
     * @since 2.1
     * @since Java 1.4
     */
    public static final String JAVA_UTIL_PREFS_PREFERENCES_FACTORY = SystemProperties.getJavaUtilPrefsPreferencesFactory();

    /**
     * A constant for the System Property {@code java.vendor}. Java vendor-specific string.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaVendor()
     * @since Java 1.1
     */
    public static final String JAVA_VENDOR = SystemProperties.getJavaVendor();

    /**
     * A constant for the System Property {@code java.vendor.url}. Java vendor URL.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaVendorUrl()
     * @since Java 1.1
     */
    public static final String JAVA_VENDOR_URL = SystemProperties.getJavaVendorUrl();

    /**
     * A constant for the System Property {@code java.version}. Java version number.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaVersion()
     * @since Java 1.1
     */
    public static final String JAVA_VERSION = SystemProperties.getJavaVersion();

    /**
     * A constant for the System Property {@code java.vm.info}. Java Virtual Machine implementation info.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaVmInfo()
     * @since 2.0
     * @since Java 1.2
     */
    public static final String JAVA_VM_INFO = SystemProperties.getJavaVmInfo();

    /**
     * A constant for the System Property {@code java.vm.name}. Java Virtual Machine implementation name.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaVmName()
     * @since Java 1.2
     */
    public static final String JAVA_VM_NAME = SystemProperties.getJavaVmName();

    /**
     * A constant for the System Property {@code java.vm.specification.name}. Java Virtual Machine specification name.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaVmSpecificationName()
     * @since Java 1.2
     */
    public static final String JAVA_VM_SPECIFICATION_NAME = SystemProperties.getJavaVmSpecificationName();

    /**
     * A constant for the System Property {@code java.vm.specification.vendor}. Java Virtual Machine specification vendor.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaVmSpecificationVendor()
     * @since Java 1.2
     */
    public static final String JAVA_VM_SPECIFICATION_VENDOR = SystemProperties.getJavaVmSpecificationVendor();

    /**
     * A constant for the System Property {@code java.vm.specification.version}. Java Virtual Machine specification version.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaVmSpecificationVersion()
     * @since Java 1.2
     */
    public static final String JAVA_VM_SPECIFICATION_VERSION = SystemProperties.getJavaVmSpecificationVersion();

    /**
     * A constant for the System Property {@code java.vm.vendor}. Java Virtual Machine implementation vendor.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaVmVendor()
     * @since Java 1.2
     */
    public static final String JAVA_VM_VENDOR = SystemProperties.getJavaVmVendor();

    /**
     * A constant for the System Property {@code java.vm.version}. Java Virtual Machine implementation version.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaVmVersion()
     * @since Java 1.2
     */
    public static final String JAVA_VM_VERSION = SystemProperties.getJavaVmVersion();

    /**
     * A constant for the System Property {@code line.separator}. Line separator ({@code &quot;\n&quot;} on Unix).
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getLineSeparator()
     * @deprecated Use {@link System#lineSeparator()} instead, since it does not require a privilege check.
     * @since Java 1.1
     */
    @Deprecated
    public static final String LINE_SEPARATOR = SystemProperties.getLineSeparator();

    /**
     * A constant for the System Property {@code os.arch}. Operating system architecture.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getOsArch()
     * @since Java 1.1
     */
    public static final String OS_ARCH = SystemProperties.getOsArch();

    /**
     * A constant for the System Property {@code os.name}. Operating system name.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getOsName()
     * @since Java 1.1
     */
    public static final String OS_NAME = SystemProperties.getOsName();

    /**
     * A constant for the System Property {@code os.version}. Operating system version.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getOsVersion()
     * @since Java 1.1
     */
    public static final String OS_VERSION = SystemProperties.getOsVersion();

    /**
     * A constant for the System Property {@code path.separator}. Path separator ({@code &quot;:&quot;} on Unix).
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getPathSeparator()
     * @deprecated Use {@link File#pathSeparator}, since it is guaranteed to be a string containing a single character and it does not require a privilege
     *             check.
     * @since Java 1.1
     */
    @Deprecated
    public static final String PATH_SEPARATOR = SystemProperties.getPathSeparator();

    /**
     * A constant for the System Property {@code user.country} or {@code user.region}. User's country code, such as {@code "GB"}. First in Java version 1.2 as
     * {@code user.region}. Renamed to {@code user.country} in 1.4
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @since 2.0
     * @since Java 1.2
     */
    public static final String USER_COUNTRY = SystemProperties.getProperty(SystemProperties.USER_COUNTRY,
            () -> SystemProperties.getProperty(SystemProperties.USER_REGION));

    /**
     * A constant for the System Property {@code user.dir}. User's current working directory.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getUserDir()
     * @since Java 1.1
     */
    public static final String USER_DIR = SystemProperties.getUserDir();

    /**
     * A constant for the System Property {@code user.home}. User's home directory.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getUserHome()
     * @since Java 1.1
     */
    public static final String USER_HOME = SystemProperties.getUserHome();

    /**
     * A constant for the System Property {@code user.language}. User's language code, such as {@code "en"}.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getUserLanguage()
     * @since 2.0
     * @since Java 1.2
     */
    public static final String USER_LANGUAGE = SystemProperties.getUserLanguage();

    /**
     * A constant for the System Property {@code user.name}. User's account name.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getUserName()
     * @since Java 1.1
     */
    public static final String USER_NAME = SystemProperties.getUserName();

    /**
     * A constant for the System Property {@code user.timezone}. For example: {@code "America/Los_Angeles"}.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @see SystemProperties#getUserTimezone()
     * @since 2.1
     */
    public static final String USER_TIMEZONE = SystemProperties.getUserTimezone();

    // Java version checks
    // -----------------------------------------------------------------------
    // These MUST be declared after those above as they depend on the
    // values being set up

    /**
     * The constant {@code true} if this is Java version 1.1 (also 1.1.x versions).
     * <p>
     * The result depends on the value of the {@link #JAVA_SPECIFICATION_VERSION} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #JAVA_SPECIFICATION_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     */
    public static final boolean IS_JAVA_1_1 = getJavaVersionMatches("1.1");

    /**
     * The constant {@code true} if this is Java version 1.2 (also 1.2.x versions).
     * <p>
     * The result depends on the value of the {@link #JAVA_SPECIFICATION_VERSION} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #JAVA_SPECIFICATION_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     */
    public static final boolean IS_JAVA_1_2 = getJavaVersionMatches("1.2");

    /**
     * The constant {@code true} if this is Java version 1.3 (also 1.3.x versions).
     * <p>
     * The result depends on the value of the {@link #JAVA_SPECIFICATION_VERSION} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #JAVA_SPECIFICATION_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     */
    public static final boolean IS_JAVA_1_3 = getJavaVersionMatches("1.3");

    /**
     * The constant {@code true} if this is Java version 1.4 (also 1.4.x versions).
     * <p>
     * The result depends on the value of the {@link #JAVA_SPECIFICATION_VERSION} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #JAVA_SPECIFICATION_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     */
    public static final boolean IS_JAVA_1_4 = getJavaVersionMatches("1.4");

    /**
     * The constant {@code true} if this is Java version 1.5 (also 1.5.x versions).
     * <p>
     * The result depends on the value of the {@link #JAVA_SPECIFICATION_VERSION} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #JAVA_SPECIFICATION_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     */
    public static final boolean IS_JAVA_1_5 = getJavaVersionMatches("1.5");

    /**
     * The constant {@code true} if this is Java version 1.6 (also 1.6.x versions).
     * <p>
     * The result depends on the value of the {@link #JAVA_SPECIFICATION_VERSION} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #JAVA_SPECIFICATION_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     */
    public static final boolean IS_JAVA_1_6 = getJavaVersionMatches("1.6");

    /**
     * The constant {@code true} if this is Java version 1.7 (also 1.7.x versions).
     * <p>
     * The result depends on the value of the {@link #JAVA_SPECIFICATION_VERSION} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #JAVA_SPECIFICATION_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.0
     */
    public static final boolean IS_JAVA_1_7 = getJavaVersionMatches("1.7");

    /**
     * The constant {@code true} if this is Java version 1.8 (also 1.8.x versions).
     * <p>
     * The result depends on the value of the {@link #JAVA_SPECIFICATION_VERSION} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #JAVA_SPECIFICATION_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.3.2
     */
    public static final boolean IS_JAVA_1_8 = getJavaVersionMatches("1.8");

    /**
     * The constant {@code true} if this is Java version 1.9 (also 1.9.x versions).
     * <p>
     * The result depends on the value of the {@link #JAVA_SPECIFICATION_VERSION} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #JAVA_SPECIFICATION_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.4
     * @deprecated As of release 3.5, replaced by {@link #IS_JAVA_9}
     */
    @Deprecated
    public static final boolean IS_JAVA_1_9 = getJavaVersionMatches("9");

    /**
     * The constant {@code true} if this is Java version 9 (also 9.x versions).
     * <p>
     * The result depends on the value of the {@link #JAVA_SPECIFICATION_VERSION} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #JAVA_SPECIFICATION_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.5
     */
    public static final boolean IS_JAVA_9 = getJavaVersionMatches("9");

    /**
     * The constant {@code true} if this is Java version 10 (also 10.x versions).
     * <p>
     * The result depends on the value of the {@link #JAVA_SPECIFICATION_VERSION} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #JAVA_SPECIFICATION_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.7
     */
    public static final boolean IS_JAVA_10 = getJavaVersionMatches("10");

    /**
     * The constant {@code true} if this is Java version 11 (also 11.x versions).
     * <p>
     * The result depends on the value of the {@link #JAVA_SPECIFICATION_VERSION} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #JAVA_SPECIFICATION_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.8
     */
    public static final boolean IS_JAVA_11 = getJavaVersionMatches("11");

    /**
     * The constant {@code true} if this is Java version 12 (also 12.x versions).
     * <p>
     * The result depends on the value of the {@link #JAVA_SPECIFICATION_VERSION} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #JAVA_SPECIFICATION_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.9
     */
    public static final boolean IS_JAVA_12 = getJavaVersionMatches("12");

    /**
     * The constant {@code true} if this is Java version 13 (also 13.x versions).
     * <p>
     * The result depends on the value of the {@link #JAVA_SPECIFICATION_VERSION} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #JAVA_SPECIFICATION_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.9
     */
    public static final boolean IS_JAVA_13 = getJavaVersionMatches("13");

    /**
     * The constant {@code true} if this is Java version 14 (also 14.x versions).
     * <p>
     * The result depends on the value of the {@link #JAVA_SPECIFICATION_VERSION} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #JAVA_SPECIFICATION_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.10
     */
    public static final boolean IS_JAVA_14 = getJavaVersionMatches("14");

    /**
     * The constant {@code true} if this is Java version 15 (also 15.x versions).
     * <p>
     * The result depends on the value of the {@link #JAVA_SPECIFICATION_VERSION} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #JAVA_SPECIFICATION_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.10
     */
    public static final boolean IS_JAVA_15 = getJavaVersionMatches("15");

    /**
     * The constant {@code true} if this is Java version 16 (also 16.x versions).
     * <p>
     * The result depends on the value of the {@link #JAVA_SPECIFICATION_VERSION} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #JAVA_SPECIFICATION_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.13.0
     */
    public static final boolean IS_JAVA_16 = getJavaVersionMatches("16");

    /**
     * The constant {@code true} if this is Java version 17 (also 17.x versions).
     * <p>
     * The result depends on the value of the {@link #JAVA_SPECIFICATION_VERSION} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #JAVA_SPECIFICATION_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.13.0
     */
    public static final boolean IS_JAVA_17 = getJavaVersionMatches("17");

    /**
     * The constant {@code true} if this is Java version 18 (also 18.x versions).
     * <p>
     * The result depends on the value of the {@link #JAVA_SPECIFICATION_VERSION} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #JAVA_SPECIFICATION_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.13.0
     */
    public static final boolean IS_JAVA_18 = getJavaVersionMatches("18");

    /**
     * The constant {@code true} if this is Java version 19 (also 19.x versions).
     * <p>
     * The result depends on the value of the {@link #JAVA_SPECIFICATION_VERSION} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #JAVA_SPECIFICATION_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.13.0
     */
    public static final boolean IS_JAVA_19 = getJavaVersionMatches("19");

    /**
     * The constant {@code true} if this is Java version 20 (also 20.x versions).
     * <p>
     * The result depends on the value of the {@link #JAVA_SPECIFICATION_VERSION} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #JAVA_SPECIFICATION_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.13.0
     */
    public static final boolean IS_JAVA_20 = getJavaVersionMatches("20");

    /**
     * The constant {@code true} if this is Java version 21 (also 21.x versions).
     * <p>
     * The result depends on the value of the {@link #JAVA_SPECIFICATION_VERSION} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #JAVA_SPECIFICATION_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.13.0
     */
    public static final boolean IS_JAVA_21 = getJavaVersionMatches("21");

    /**
     * The constant {@code true} if this is Java version 22 (also 22.x versions).
     * <p>
     * The result depends on the value of the {@link #JAVA_SPECIFICATION_VERSION} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #JAVA_SPECIFICATION_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.15.0
     */
    public static final boolean IS_JAVA_22 = getJavaVersionMatches("22");

    /**
     * The constant {@code true} if this is Java version 23 (also 23.x versions).
     * <p>
     * The result depends on the value of the {@link #JAVA_SPECIFICATION_VERSION} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #JAVA_SPECIFICATION_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.18.0
     */
    public static final boolean IS_JAVA_23 = getJavaVersionMatches("23");

    // Operating system checks
    // -----------------------------------------------------------------------
    // These MUST be declared after those above as they depend on the
    // values being set up
    // Please advise dev@commons.apache.org if you want another added
    // or a mistake corrected

    /**
     * The constant {@code true} if this is AIX.
     * <p>
     * The result depends on the value of the {@link #OS_NAME} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 2.0
     */
    public static final boolean IS_OS_AIX = getOsNameMatches("AIX");

    /**
     * The constant {@code true} if this is Android.
     *
     * <p>
     * See https://developer.android.com/reference/java/lang/System#getProperties().
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.15.0
     */
    public static final boolean IS_OS_ANDROID = SystemProperties.getJavaVendor().contains("Android");

    /**
     * The constant {@code true} if this is HP-UX.
     * <p>
     * The result depends on the value of the {@link #OS_NAME} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 2.0
     */
    public static final boolean IS_OS_HP_UX = getOsNameMatches("HP-UX");

    /**
     * The constant {@code true} if this is IBM OS/400.
     * <p>
     * The result depends on the value of the {@link #OS_NAME} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.3
     */
    public static final boolean IS_OS_400 = getOsNameMatches("OS/400");

    /**
     * The constant {@code true} if this is Irix.
     * <p>
     * The result depends on the value of the {@link #OS_NAME} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 2.0
     */
    public static final boolean IS_OS_IRIX = getOsNameMatches("Irix");

    /**
     * The constant {@code true} if this is Linux.
     * <p>
     * The result depends on the value of the {@link #OS_NAME} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 2.0
     */
    public static final boolean IS_OS_LINUX = getOsNameMatches("Linux");

    /**
     * The constant {@code true} if this is Mac.
     * <p>
     * The result depends on the value of the {@link #OS_NAME} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 2.0
     */
    public static final boolean IS_OS_MAC = getOsNameMatches("Mac");

    /**
     * The constant {@code true} if this is Mac.
     * <p>
     * The result depends on the value of the {@link #OS_NAME} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 2.0
     */
    public static final boolean IS_OS_MAC_OSX = getOsNameMatches("Mac OS X");

    /**
     * The constant {@code true} if this is macOS X Cheetah.
     * <p>
     * The value depends on the value of the {@link #OS_NAME} and {@link #OS_VERSION} constants.
     * </p>
     * <p>
     * The value is {@code false} if {@link #OS_NAME} or {@link #OS_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.4
     */
    public static final boolean IS_OS_MAC_OSX_CHEETAH = getOsMatches("Mac OS X", "10.0");

    /**
     * The constant {@code true} if this is macOS X Puma.
     * <p>
     * The value depends on the value of the {@link #OS_NAME} and {@link #OS_VERSION} constants.
     * </p>
     * <p>
     * The value is {@code false} if {@link #OS_NAME} or {@link #OS_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.4
     */
    public static final boolean IS_OS_MAC_OSX_PUMA = getOsMatches("Mac OS X", "10.1");

    /**
     * The constant {@code true} if this is macOS X Jaguar.
     * <p>
     * The value depends on the value of the {@link #OS_NAME} and {@link #OS_VERSION} constants.
     * </p>
     * <p>
     * The value is {@code false} if {@link #OS_NAME} or {@link #OS_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.4
     */
    public static final boolean IS_OS_MAC_OSX_JAGUAR = getOsMatches("Mac OS X", "10.2");

    /**
     * The constant {@code true} if this is macOS X Panther.
     * <p>
     * The value depends on the value of the {@link #OS_NAME} and {@link #OS_VERSION} constants.
     * </p>
     * <p>
     * The value is {@code false} if {@link #OS_NAME} or {@link #OS_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.4
     */
    public static final boolean IS_OS_MAC_OSX_PANTHER = getOsMatches("Mac OS X", "10.3");

    /**
     * The constant {@code true} if this is macOS X Tiger.
     * <p>
     * The value depends on the value of the {@link #OS_NAME} and {@link #OS_VERSION} constants.
     * </p>
     * <p>
     * The value is {@code false} if {@link #OS_NAME} or {@link #OS_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.4
     */
    public static final boolean IS_OS_MAC_OSX_TIGER = getOsMatches("Mac OS X", "10.4");

    /**
     * The constant {@code true} if this is macOS X Leopard.
     * <p>
     * The value depends on the value of the {@link #OS_NAME} and {@link #OS_VERSION} constants.
     * </p>
     * <p>
     * The value is {@code false} if {@link #OS_NAME} or {@link #OS_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.4
     */
    public static final boolean IS_OS_MAC_OSX_LEOPARD = getOsMatches("Mac OS X", "10.5");

    /**
     * The constant {@code true} if this is macOS X Snow Leopard.
     * <p>
     * The value depends on the value of the {@link #OS_NAME} and {@link #OS_VERSION} constants.
     * </p>
     * <p>
     * The value is {@code false} if {@link #OS_NAME} or {@link #OS_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.4
     */
    public static final boolean IS_OS_MAC_OSX_SNOW_LEOPARD = getOsMatches("Mac OS X", "10.6");

    /**
     * The constant {@code true} if this is macOS X Lion.
     * <p>
     * The value depends on the value of the {@link #OS_NAME} and {@link #OS_VERSION} constants.
     * </p>
     * <p>
     * The value is {@code false} if {@link #OS_NAME} or {@link #OS_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.4
     */
    public static final boolean IS_OS_MAC_OSX_LION = getOsMatches("Mac OS X", "10.7");

    /**
     * The constant {@code true} if this is macOS X Mountain Lion.
     * <p>
     * The value depends on the value of the {@link #OS_NAME} and {@link #OS_VERSION} constants.
     * </p>
     * <p>
     * The value is {@code false} if {@link #OS_NAME} or {@link #OS_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.4
     */
    public static final boolean IS_OS_MAC_OSX_MOUNTAIN_LION = getOsMatches("Mac OS X", "10.8");

    /**
     * The constant {@code true} if this is macOS X Mavericks.
     * <p>
     * The value depends on the value of the {@link #OS_NAME} and {@link #OS_VERSION} constants.
     * </p>
     * <p>
     * The value is {@code false} if {@link #OS_NAME} or {@link #OS_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.4
     */
    public static final boolean IS_OS_MAC_OSX_MAVERICKS = getOsMatches("Mac OS X", "10.9");

    /**
     * The constant {@code true} if this is macOS X Yosemite.
     * <p>
     * The value depends on the value of the {@link #OS_NAME} and {@link #OS_VERSION} constants.
     * </p>
     * <p>
     * The value is {@code false} if {@link #OS_NAME} or {@link #OS_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.4
     */
    public static final boolean IS_OS_MAC_OSX_YOSEMITE = getOsMatches("Mac OS X", "10.10");

    /**
     * The constant {@code true} if this is macOS X El Capitan.
     * <p>
     * The value depends on the value of the {@link #OS_NAME} and {@link #OS_VERSION} constants.
     * </p>
     * <p>
     * The value is {@code false} if {@link #OS_NAME} or {@link #OS_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.5
     */
    public static final boolean IS_OS_MAC_OSX_EL_CAPITAN = getOsMatches("Mac OS X", "10.11");

    /**
     * The constant {@code true} if this is macOS X Sierra.
     * <p>
     * The value depends on the value of the {@link #OS_NAME} and {@link #OS_VERSION} constants.
     * </p>
     * <p>
     * The value is {@code false} if {@link #OS_NAME} or {@link #OS_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.12.0
     */
    public static final boolean IS_OS_MAC_OSX_SIERRA = getOsMatches("Mac OS X", "10.12");

    /**
     * The constant {@code true} if this is macOS X High Sierra.
     * <p>
     * The value depends on the value of the {@link #OS_NAME} and {@link #OS_VERSION} constants.
     * </p>
     * <p>
     * The value is {@code false} if {@link #OS_NAME} or {@link #OS_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.12.0
     */
    public static final boolean IS_OS_MAC_OSX_HIGH_SIERRA = getOsMatches("Mac OS X", "10.13");

    /**
     * The constant {@code true} if this is macOS X Mojave.
     * <p>
     * The value depends on the value of the {@link #OS_NAME} and {@link #OS_VERSION} constants.
     * </p>
     * <p>
     * The value is {@code false} if {@link #OS_NAME} or {@link #OS_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.12.0
     */
    public static final boolean IS_OS_MAC_OSX_MOJAVE = getOsMatches("Mac OS X", "10.14");

    /**
     * The constant {@code true} if this is macOS X Catalina.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.12.0
     */
    public static final boolean IS_OS_MAC_OSX_CATALINA = getOsMatches("Mac OS X", "10.15");

    /**
     * The constant {@code true} if this is macOS X Big Sur.
     * <p>
     * The value depends on the value of the {@link #OS_NAME} and {@link #OS_VERSION} constants.
     * </p>
     * <p>
     * The value is {@code false} if {@link #OS_NAME} or {@link #OS_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.12.0
     */
    public static final boolean IS_OS_MAC_OSX_BIG_SUR = getOsMatches("Mac OS X", "11");

    /**
     * The constant {@code true} if this is macOS X Monterey.
     * <p>
     * The value depends on the value of the {@link #OS_NAME} and {@link #OS_VERSION} constants.
     * </p>
     * <p>
     * The value is {@code false} if {@link #OS_NAME} or {@link #OS_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.13.0
     */
    public static final boolean IS_OS_MAC_OSX_MONTEREY = getOsMatches("Mac OS X", "12");

    /**
     * The constant {@code true} if this is macOS X Ventura.
     * <p>
     * The value depends on the value of the {@link #OS_NAME} and {@link #OS_VERSION} constants.
     * </p>
     * <p>
     * The value is {@code false} if {@link #OS_NAME} or {@link #OS_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.13.0
     */
    public static final boolean IS_OS_MAC_OSX_VENTURA = getOsMatches("Mac OS X", "13");

    /**
     * The constant {@code true} if this is macOS X Sonoma.
     * <p>
     * The value depends on the value of the {@link #OS_NAME} and {@link #OS_VERSION} constants.
     * </p>
     * <p>
     * The value is {@code false} if {@link #OS_NAME} or {@link #OS_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.15.0
     */
    public static final boolean IS_OS_MAC_OSX_SONOMA = getOsMatches("Mac OS X", "14");

    /**
     * The constant {@code true} if this is macOS X Sequoia.
     * <p>
     * The value depends on the value of the {@link #OS_NAME} and {@link #OS_VERSION} constants.
     * </p>
     * <p>
     * The value is {@code false} if {@link #OS_NAME} or {@link #OS_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.18.0
     */
    public static final boolean IS_OS_MAC_OSX_SEQUOIA = getOsMatches("Mac OS X", "15");

    /**
     * The constant {@code true} if this is FreeBSD.
     * <p>
     * The result depends on the value of the {@link #OS_NAME} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.1
     */
    public static final boolean IS_OS_FREE_BSD = getOsNameMatches("FreeBSD");

    /**
     * The constant {@code true} if this is OpenBSD.
     * <p>
     * The result depends on the value of the {@link #OS_NAME} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.1
     */
    public static final boolean IS_OS_OPEN_BSD = getOsNameMatches("OpenBSD");

    /**
     * The constant {@code true} if this is NetBSD.
     * <p>
     * The result depends on the value of the {@link #OS_NAME} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.1
     */
    public static final boolean IS_OS_NET_BSD = getOsNameMatches("NetBSD");

    /**
     * The constant {@code true} if this is OS/2.
     * <p>
     * The result depends on the value of the {@link #OS_NAME} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 2.0
     */
    public static final boolean IS_OS_OS2 = getOsNameMatches("OS/2");

    /**
     * The constant {@code true} if this is Solaris.
     * <p>
     * The result depends on the value of the {@link #OS_NAME} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 2.0
     */
    public static final boolean IS_OS_SOLARIS = getOsNameMatches("Solaris");

    /**
     * The constant {@code true} if this is SunOS.
     * <p>
     * The result depends on the value of the {@link #OS_NAME} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 2.0
     */
    public static final boolean IS_OS_SUN_OS = getOsNameMatches("SunOS");

    /**
     * The constant {@code true} if this is a Unix like system, as in any of AIX, HP-UX, Irix, Linux, MacOSX, Solaris or SUN OS.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 2.1
     */
    public static final boolean IS_OS_UNIX = IS_OS_AIX || IS_OS_HP_UX || IS_OS_IRIX || IS_OS_LINUX || IS_OS_MAC_OSX || IS_OS_SOLARIS || IS_OS_SUN_OS
            || IS_OS_FREE_BSD || IS_OS_OPEN_BSD || IS_OS_NET_BSD;

    /**
     * The constant {@code true} if this is Windows.
     * <p>
     * The result depends on the value of the {@link #OS_NAME} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 2.0
     */
    public static final boolean IS_OS_WINDOWS = getOsNameMatches(OS_NAME_WINDOWS_PREFIX);

    /**
     * The constant {@code true} if this is Windows 2000.
     * <p>
     * The result depends on the value of the {@link #OS_NAME} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 2.0
     */
    public static final boolean IS_OS_WINDOWS_2000 = getOsNameMatches(OS_NAME_WINDOWS_PREFIX + " 2000");

    /**
     * The constant {@code true} if this is Windows 2003.
     * <p>
     * The result depends on the value of the {@link #OS_NAME} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.1
     */
    public static final boolean IS_OS_WINDOWS_2003 = getOsNameMatches(OS_NAME_WINDOWS_PREFIX + " 2003");

    /**
     * The constant {@code true} if this is Windows Server 2008.
     * <p>
     * The result depends on the value of the {@link #OS_NAME} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.1
     */
    public static final boolean IS_OS_WINDOWS_2008 = getOsNameMatches(OS_NAME_WINDOWS_PREFIX + " Server 2008");

    /**
     * The constant {@code true} if this is Windows Server 2012.
     * <p>
     * The result depends on the value of the {@link #OS_NAME} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.4
     */
    public static final boolean IS_OS_WINDOWS_2012 = getOsNameMatches(OS_NAME_WINDOWS_PREFIX + " Server 2012");

    /**
     * The constant {@code true} if this is Windows 95.
     * <p>
     * The result depends on the value of the {@link #OS_NAME} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 2.0
     */
    public static final boolean IS_OS_WINDOWS_95 = getOsNameMatches(OS_NAME_WINDOWS_PREFIX + " 95");

    /**
     * The constant {@code true} if this is Windows 98.
     * <p>
     * The result depends on the value of the {@link #OS_NAME} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 2.0
     */
    public static final boolean IS_OS_WINDOWS_98 = getOsNameMatches(OS_NAME_WINDOWS_PREFIX + " 98");

    /**
     * The constant {@code true} if this is Windows ME.
     * <p>
     * The result depends on the value of the {@link #OS_NAME} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 2.0
     */
    public static final boolean IS_OS_WINDOWS_ME = getOsNameMatches(OS_NAME_WINDOWS_PREFIX + " Me");

    /**
     * The constant {@code true} if this is Windows NT.
     * <p>
     * The result depends on the value of the {@link #OS_NAME} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 2.0
     */
    public static final boolean IS_OS_WINDOWS_NT = getOsNameMatches(OS_NAME_WINDOWS_PREFIX + " NT");

    /**
     * The constant {@code true} if this is Windows XP.
     * <p>
     * The result depends on the value of the {@link #OS_NAME} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 2.0
     */
    public static final boolean IS_OS_WINDOWS_XP = getOsNameMatches(OS_NAME_WINDOWS_PREFIX + " XP");

    /**
     * The constant {@code true} if this is Windows Vista.
     * <p>
     * The result depends on the value of the {@link #OS_NAME} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 2.4
     */
    public static final boolean IS_OS_WINDOWS_VISTA = getOsNameMatches(OS_NAME_WINDOWS_PREFIX + " Vista");

    /**
     * The constant {@code true} if this is Windows 7.
     * <p>
     * The result depends on the value of the {@link #OS_NAME} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.0
     */
    public static final boolean IS_OS_WINDOWS_7 = getOsNameMatches(OS_NAME_WINDOWS_PREFIX + " 7");

    /**
     * The constant {@code true} if this is Windows 8.
     * <p>
     * The result depends on the value of the {@link #OS_NAME} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.2
     */
    public static final boolean IS_OS_WINDOWS_8 = getOsNameMatches(OS_NAME_WINDOWS_PREFIX + " 8");

    /**
     * The constant {@code true} if this is Windows 10.
     * <p>
     * The result depends on the value of the {@link #OS_NAME} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.5
     */
    public static final boolean IS_OS_WINDOWS_10 = getOsNameMatches(OS_NAME_WINDOWS_PREFIX + " 10");

    /**
     * The constant {@code true} if this is Windows 11.
     * <p>
     * The result depends on the value of the {@link #OS_NAME} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #OS_NAME} is {@code null}.
     * </p>
     * <p>
     * OpenJDK fixed the return value for {@code os.name} on Windows 11 to versions 8, 11, and 17:
     * </p>
     * <ul>
     * <li>Affects Java versions 7u321, 8u311, 11.0.13-oracle, 17.0.1: https://bugs.openjdk.org/browse/JDK-8274737</li>
     * <li>Fixed in OpenJDK commit https://github.com/openjdk/jdk/commit/97ea9dd2f24f9f1fb9b9345a4202a825ee28e014</li>
     * </ul>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.13.0
     */
    public static final boolean IS_OS_WINDOWS_11 = getOsNameMatches(OS_NAME_WINDOWS_PREFIX + " 11");

    /**
     * The constant {@code true} if this is z/OS.
     * <p>
     * The result depends on the value of the {@link #OS_NAME} constant.
     * </p>
     * <p>
     * The field will return {@code false} if {@link #OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.5
     */
    // Values on a z/OS system I tested (Gary Gregory - 2016-03-12)
    // os.arch = s390x
    // os.encoding = ISO8859_1
    // os.name = z/OS
    // os.version = 02.02.00
    public static final boolean IS_OS_ZOS = getOsNameMatches("z/OS");

    /**
     * The System property key for the user home directory.
     */
    public static final String USER_HOME_KEY = "user.home";

    /**
     * The System property key for the user name.
     *
     * @deprecated Use {@link SystemProperties#USER_NAME}.
     */
    @Deprecated
    public static final String USER_NAME_KEY = "user.name";

    /**
     * The System property key for the user directory.
     *
     * @deprecated Use {@link SystemProperties#USER_DIR}.
     */
    @Deprecated
    public static final String USER_DIR_KEY = "user.dir";

    /**
     * The System property key for the Java IO temporary directory.
     *
     * @deprecated Use {@link SystemProperties#JAVA_IO_TMPDIR}.
     */
    @Deprecated
    public static final String JAVA_IO_TMPDIR_KEY = "java.io.tmpdir";

    /**
     * The System property key for the Java home directory.
     *
     * @deprecated Use {@link SystemProperties#JAVA_HOME}.
     */
    @Deprecated
    public static final String JAVA_HOME_KEY = "java.home";

    /**
     * A constant for the System Property {@code awt.toolkit}.
     *
     * <p>
     * Holds a class name, on Windows XP this is {@code sun.awt.windows.WToolkit}.
     * </p>
     * <p>
     * <strong>On platforms without a GUI, this value is {@code null}.</strong>
     * </p>
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or {@link System#setProperties(java.util.Properties)} is
     * called after this class is loaded, the value will be out of sync with that System property.
     * </p>
     *
     * @since 2.1
     * @see SystemProperties#getAwtToolkit()
     */
    public static final String AWT_TOOLKIT = SystemProperties.getAwtToolkit();

    /**
     * Gets an environment variable, defaulting to {@code defaultValue} if the variable cannot be read.
     *
     * <p>
     * If a {@link SecurityException} is caught, the return value is {@code defaultValue} and a message is written to {@code System.err}.
     * </p>
     *
     * @param name         the environment variable name
     * @param defaultValue the default value
     * @return the environment variable value or {@code defaultValue} if a security problem occurs
     * @since 3.8
     */
    public static String getEnvironmentVariable(final String name, final String defaultValue) {
        try {
            final String value = System.getenv(name);
            return value == null ? defaultValue : value;
        } catch (final SecurityException ex) {
            // we are not allowed to look at this property
            // System.err.println("Caught a SecurityException reading the environment variable '" + name + "'.");
            return defaultValue;
        }
    }

    /**
     * Gets the host name from an environment variable ({@code COMPUTERNAME} on Windows, {@code HOSTNAME} elsewhere).
     *
     * <p>
     * If you want to know what the network stack says is the host name, you should use {@code InetAddress.getLocalHost().getHostName()}.
     * </p>
     *
     * @return the host name. Will be {@code null} if the environment variable is not defined.
     * @since 3.6
     */
    public static String getHostName() {
        return IS_OS_WINDOWS ? System.getenv("COMPUTERNAME") : System.getenv("HOSTNAME");
    }

    /**
     * Gets the current Java home directory as a {@link File}.
     *
     * @return a directory
     * @throws SecurityException if a security manager exists and its {@code checkPropertyAccess} method doesn't allow access to the specified system property.
     * @see SystemProperties#getJavaHome()
     * @since 2.1
     */
    public static File getJavaHome() {
        return new File(SystemProperties.getJavaHome());
    }

    /**
     * Gets the current Java home directory as a {@link File}.
     *
     * @return a directory
     * @throws SecurityException if a security manager exists and its {@code checkPropertyAccess} method doesn't allow access to the specified system property.
     * @see SystemProperties#getJavaHome()
     * @since 3.18.0
     */
    public static Path getJavaHomePath() {
        return Paths.get(SystemProperties.getJavaHome());
    }

    /**
     * Gets the current Java IO temporary directory as a {@link File}.
     *
     * @return a directory
     * @throws SecurityException if a security manager exists and its {@code checkPropertyAccess} method doesn't allow access to the specified system property.
     * @see SystemProperties#getJavaIoTmpdir()
     * @since 2.1
     */
    public static File getJavaIoTmpDir() {
        return new File(SystemProperties.getJavaIoTmpdir());
    }

    /**
     * Gets the current Java IO temporary directory as a {@link Path}.
     *
     * @return a directory
     * @throws SecurityException if a security manager exists and its {@code checkPropertyAccess} method doesn't allow access to the specified system property.
     * @see SystemProperties#getJavaIoTmpdir()
     * @since 3.18.0
     */
    public static Path getJavaIoTmpDirPath() {
        return Paths.get(SystemProperties.getJavaIoTmpdir());
    }

    /**
     * Tests if the Java version matches the version we are running.
     * <p>
     * The result depends on the value of the {@link #JAVA_SPECIFICATION_VERSION} constant.
     * </p>
     *
     * @param versionPrefix the prefix for the Java version
     * @return true if matches, or false if not or can't determine
     */
    private static boolean getJavaVersionMatches(final String versionPrefix) {
        return isJavaVersionMatch(JAVA_SPECIFICATION_VERSION, versionPrefix);
    }

    /**
     * Tests if the operating system matches the given name prefix and version prefix.
     * <p>
     * The result depends on the value of the {@link #OS_NAME} and {@link #OS_VERSION} constants.
     * </p>
     * <p>
     * The method returns {@code false} if {@link #OS_NAME} or {@link #OS_VERSION} is {@code null}.
     * </p>
     *
     * @param osNamePrefix    the prefix for the OS name
     * @param osVersionPrefix the prefix for the version
     * @return true if matches, or false if not or can't determine
     */
    private static boolean getOsMatches(final String osNamePrefix, final String osVersionPrefix) {
        return isOsMatch(OS_NAME, OS_VERSION, osNamePrefix, osVersionPrefix);
    }

    /**
     * Tests if the operating system matches the given string with a case-insensitive comparison.
     * <p>
     * The result depends on the value of the {@link #OS_NAME} constant.
     * </p>
     * <p>
     * The method returns {@code false} if {@link #OS_NAME} is {@code null}.
     * </p>
     *
     * @param osNamePrefix the prefix for the OS name
     * @return true if matches, or false if not or can't determine
     */
    private static boolean getOsNameMatches(final String osNamePrefix) {
        return isOsNameMatch(OS_NAME, osNamePrefix);
    }

    /**
     * Gets the current user directory as a {@link File}.
     * <p>
     * The result is based on the system property {@value SystemProperties#USER_DIR}.
     * </p>
     *
     * @return a directory
     * @throws SecurityException if a security manager exists and its {@code checkPropertyAccess} method doesn't allow access to the specified system property.
     * @see SystemProperties#getUserDir()
     * @since 2.1
     */
    public static File getUserDir() {
        return new File(SystemProperties.getUserDir());
    }

    /**
     * Gets the current user directory as a {@link Path}.
     * <p>
     * The result is based on the system property {@value SystemProperties#USER_DIR}.
     * </p>
     *
     * @return a directory
     * @throws SecurityException if a security manager exists and its {@code checkPropertyAccess} method doesn't allow access to the specified system property.
     * @see SystemProperties#getUserDir()
     * @since 3.18.0
     */
    public static Path getUserDirPath() {
        return Paths.get(SystemProperties.getUserDir());
    }

    /**
     * Gets the current user home directory as a {@link File}.
     * <p>
     * The result is based on the system property {@value SystemProperties#USER_HOME}.
     * </p>
     *
     * @return a directory
     * @throws SecurityException if a security manager exists and its {@code checkPropertyAccess} method doesn't allow access to the specified system property.
     * @see SystemProperties#getUserHome()
     * @since 2.1
     */
    public static File getUserHome() {
        return new File(SystemProperties.getUserHome());
    }

    /**
     * Gets the current user home directory as a {@link Path}.
     * <p>
     * The result is based on the system property {@value SystemProperties#USER_HOME}.
     * </p>
     *
     * @return a directory
     * @throws SecurityException if a security manager exists and its {@code checkPropertyAccess} method doesn't allow access to the specified system property.
     * @see SystemProperties#getUserHome()
     * @since 3.18.0
     */
    public static Path getUserHomePath() {
        return Paths.get(SystemProperties.getUserHome());
    }

    /**
     * Gets the current user name.
     * <p>
     * The result is based on the system property {@value SystemProperties#USER_NAME}.
     * </p>
     *
     * @return a name
     * @throws SecurityException if a security manager exists and its {@code checkPropertyAccess} method doesn't allow access to the specified system property.
     * @see SystemProperties#getUserName()
     * @since 3.10
     * @deprecated Use {@link SystemProperties#getUserName()}.
     */
    @Deprecated
    public static String getUserName() {
        return SystemProperties.getUserName();
    }

    /**
     * Gets the user name.
     * <p>
     * The result is based on the system property {@value SystemProperties#USER_NAME}.
     * </p>
     *
     * @param defaultValue A default value.
     * @return a name
     * @throws SecurityException if a security manager exists and its {@code checkPropertyAccess} method doesn't allow access to the specified system property.
     * @see SystemProperties#getUserName()
     * @since 3.10
     * @deprecated Use {@link SystemProperties#getUserName(String)}.
     */
    @Deprecated
    public static String getUserName(final String defaultValue) {
        return SystemProperties.getUserName(defaultValue);
    }

    /**
     * Tests whether the {@link #JAVA_AWT_HEADLESS} value is {@code true}.
     * <p>
     * The result is based on the system property {@value SystemProperties#JAVA_AWT_HEADLESS}.
     * </p>
     *
     * @return {@code true} if {@code JAVA_AWT_HEADLESS} is {@code "true"}, {@code false} otherwise.
     * @see #JAVA_AWT_HEADLESS
     * @since 2.1
     * @since Java 1.4
     */
    public static boolean isJavaAwtHeadless() {
        return Boolean.TRUE.toString().equals(JAVA_AWT_HEADLESS);
    }

    /**
     * Tests whether the Java version at least the requested version.
     * <p>
     * The result is based on the system property saved in {@link #JAVA_SPECIFICATION_VERSION}.
     * </p>
     *
     * @param requiredVersion the required version, for example 1.31f
     * @return {@code true} if the actual version is equal or greater than the required version
     */
    public static boolean isJavaVersionAtLeast(final JavaVersion requiredVersion) {
        return JAVA_SPECIFICATION_VERSION_AS_ENUM.atLeast(requiredVersion);
    }

    /**
     * Tests whether the Java version at most the requested version.
     * <p>
     * The result is based on the system property saved in {@link #JAVA_SPECIFICATION_VERSION}.
     * </p>
     *
     * @param requiredVersion the required version, for example 1.31f
     * @return {@code true} if the actual version is equal or less than the required version
     * @since 3.9
     */
    public static boolean isJavaVersionAtMost(final JavaVersion requiredVersion) {
        return JAVA_SPECIFICATION_VERSION_AS_ENUM.atMost(requiredVersion);
    }

    /**
     * Tests whether the Java version matches.
     *
     * <p>
     * This method is package private instead of private to support unit test invocation.
     * </p>
     *
     * @param version       the actual Java version
     * @param versionPrefix the prefix for the expected Java version
     * @return true if matches, or false if not or can't determine
     */
    static boolean isJavaVersionMatch(final String version, final String versionPrefix) {
        if (version == null) {
            return false;
        }
        return version.startsWith(versionPrefix);
    }

    /**
     * Tests whether the operating system matches.
     * <p>
     * This method is package private instead of private to support unit test invocation.
     * </p>
     *
     * @param osName          the actual OS name
     * @param osVersion       the actual OS version
     * @param osNamePrefix    the prefix for the expected OS name
     * @param osVersionPrefix the prefix for the expected OS version
     * @return true if matches, or false if not or can't determine
     */
    static boolean isOsMatch(final String osName, final String osVersion, final String osNamePrefix, final String osVersionPrefix) {
        if (osName == null || osVersion == null) {
            return false;
        }
        return isOsNameMatch(osName, osNamePrefix) && isOsVersionMatch(osVersion, osVersionPrefix);
    }

    /**
     * Tests whether the operating system matches with a case-insensitive comparison.
     * <p>
     * This method is package private instead of private to support unit test invocation.
     * </p>
     *
     * @param osName       the actual OS name.
     * @param osNamePrefix the prefix for the expected OS name.
     * @return true for a case-insensitive match, or false if not.
     */
    static boolean isOsNameMatch(final String osName, final String osNamePrefix) {
        if (osName == null) {
            return false;
        }
        return Strings.CI.startsWith(osName, osNamePrefix);
    }

    /**
     * Tests whether the operating system version matches.
     * <p>
     * This method is package private instead of private to support unit test invocation.
     * </p>
     *
     * @param osVersion       the actual OS version
     * @param osVersionPrefix the prefix for the expected OS version
     * @return true if matches, or false if not or can't determine
     */
    static boolean isOsVersionMatch(final String osVersion, final String osVersionPrefix) {
        if (StringUtils.isEmpty(osVersion)) {
            return false;
        }
        // Compare parts of the version string instead of using String.startsWith(String) because otherwise
        // osVersionPrefix 10.1 would also match osVersion 10.10
        final String[] versionPrefixParts = JavaVersion.split(osVersionPrefix);
        final String[] versionParts = JavaVersion.split(osVersion);
        for (int i = 0; i < Math.min(versionPrefixParts.length, versionParts.length); i++) {
            if (!versionPrefixParts[i].equals(versionParts[i])) {
                return false;
            }
        }
        return true;
    }

    /**
     * SystemUtils instances shouldn't be constructed in standard programming. Instead, elements should be accessed directly, for example
     * {@code SystemUtils.FILE_SEPARATOR}.
     *
     * <p>
     * This constructor is public to permit tools that require a JavaBean instance to operate.
     * </p>
     */
    public SystemUtils() {
    }

}
