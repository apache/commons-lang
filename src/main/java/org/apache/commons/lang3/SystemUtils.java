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

import java.io.File;

/**
 * Helpers for {@code java.lang.System}.
 *
 * <p>
 * If a system property cannot be read due to security restrictions, the corresponding field in this class will be set
 * to {@code null} and a message will be written to {@code System.err}.
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
     * The {@code file.encoding} System Property.
     *
     * <p>
     * File encoding, such as {@code Cp1252}.
     * </p>
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getFileEncoding()
     * @since 2.0
     * @since Java 1.2
     */
    public static final String FILE_ENCODING = SystemProperties.getFileEncoding();

    /**
     * The {@code file.separator} System Property.
     * The file separator is:
     *
     * <ul>
     * <li>{@code "/"} on UNIX</li>
     * <li>{@code "\"} on Windows.</li>
     * </ul>
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getFileSeparator()
     * @deprecated Use {@link File#separator}, since it is guaranteed to be a
     *             string containing a single character and it does not require a privilege check.
     * @since Java 1.1
     */
    @Deprecated
    public static final String FILE_SEPARATOR = SystemProperties.getFileSeparator();

    /**
     * The {@code java.awt.fonts} System Property.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaAwtFonts()
     * @since 2.1
     */
    public static final String JAVA_AWT_FONTS = SystemProperties.getJavaAwtFonts();

    /**
     * The {@code java.awt.graphicsenv} System Property.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaAwtGraphicsenv()
     * @since 2.1
     */
    public static final String JAVA_AWT_GRAPHICSENV = SystemProperties.getJavaAwtGraphicsenv();

    /**
     * The {@code java.awt.headless} System Property. The value of this property is the String {@code "true"} or
     * {@code "false"}.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see #isJavaAwtHeadless()
     * @see SystemProperties#getJavaAwtHeadless()
     * @since 2.1
     * @since Java 1.4
     */
    public static final String JAVA_AWT_HEADLESS = SystemProperties.getJavaAwtHeadless();

    /**
     * The {@code java.awt.printerjob} System Property.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaAwtPrinterjob()
     * @since 2.1
     */
    public static final String JAVA_AWT_PRINTERJOB = SystemProperties.getJavaAwtPrinterjob();

    /**
     * The {@code java.class.path} System Property. Java class path.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaClassPath()
     * @since Java 1.1
     */
    public static final String JAVA_CLASS_PATH = SystemProperties.getJavaClassPath();

    /**
     * The {@code java.class.version} System Property. Java class format version number.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaClassVersion()
     * @since Java 1.1
     */
    public static final String JAVA_CLASS_VERSION = SystemProperties.getJavaClassVersion();

    /**
     * The {@code java.compiler} System Property. Name of JIT compiler to use. First in JDK version 1.2. Not used in Sun
     * JDKs after 1.2.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaCompiler()
     * @since Java 1.2. Not used in Sun versions after 1.2.
     */
    public static final String JAVA_COMPILER = SystemProperties.getJavaCompiler();

    /**
     * The {@code java.endorsed.dirs} System Property. Path of endorsed directory or directories.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaEndorsedDirs()
     * @since Java 1.4
     */
    public static final String JAVA_ENDORSED_DIRS = SystemProperties.getJavaEndorsedDirs();

    /**
     * The {@code java.ext.dirs} System Property. Path of extension directory or directories.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaExtDirs()
     * @since Java 1.3
     */
    public static final String JAVA_EXT_DIRS = SystemProperties.getJavaExtDirs();

    /**
     * The {@code java.home} System Property. Java installation directory.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaHome()
     * @since Java 1.1
     */
    public static final String JAVA_HOME = SystemProperties.getJavaHome();

    /**
     * The {@code java.io.tmpdir} System Property. Default temp file path.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaIoTmpdir()
     * @since Java 1.2
     */
    public static final String JAVA_IO_TMPDIR = SystemProperties.getJavaIoTmpdir();

    /**
     * The {@code java.library.path} System Property. List of paths to search when loading libraries.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaLibraryPath()
     * @since Java 1.2
     */
    public static final String JAVA_LIBRARY_PATH = SystemProperties.getJavaLibraryPath();

    /**
     * The {@code java.runtime.name} System Property. Java Runtime Environment name.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaRuntimeName()
     * @since 2.0
     * @since Java 1.3
     */
    public static final String JAVA_RUNTIME_NAME = SystemProperties.getJavaRuntimeName();

    /**
     * The {@code java.runtime.version} System Property. Java Runtime Environment version.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaRuntimeVersion()
     * @since 2.0
     * @since Java 1.3
     */
    public static final String JAVA_RUNTIME_VERSION = SystemProperties.getJavaRuntimeVersion();

    /**
     * The {@code java.specification.name} System Property. Java Runtime Environment specification name.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaSpecificationName()
     * @since Java 1.2
     */
    public static final String JAVA_SPECIFICATION_NAME = SystemProperties.getJavaSpecificationName();

    /**
     * The {@code java.specification.vendor} System Property. Java Runtime Environment specification vendor.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaSpecificationVendor()
     * @since Java 1.2
     */
    public static final String JAVA_SPECIFICATION_VENDOR = SystemProperties.getJavaSpecificationVendor();

    /**
     * The {@code java.specification.version} System Property. Java Runtime Environment specification version.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaSpecificationVersion()
     * @since Java 1.3
     */
    public static final String JAVA_SPECIFICATION_VERSION = SystemProperties.getJavaSpecificationVersion();

    private static final JavaVersion JAVA_SPECIFICATION_VERSION_AS_ENUM = JavaVersion.get(JAVA_SPECIFICATION_VERSION);

    /**
     * The {@code java.util.prefs.PreferencesFactory} System Property. A class name.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaUtilPrefsPreferencesFactory()
     * @since 2.1
     * @since Java 1.4
     */
    public static final String JAVA_UTIL_PREFS_PREFERENCES_FACTORY = SystemProperties.getJavaUtilPrefsPreferencesFactory();

    /**
     * The {@code java.vendor} System Property. Java vendor-specific string.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaVersion()
     * @since Java 1.1
     */
    public static final String JAVA_VENDOR = SystemProperties.getJavaVersion();

    /**
     * The {@code java.vendor.url} System Property. Java vendor URL.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaVendorUrl()
     * @since Java 1.1
     */
    public static final String JAVA_VENDOR_URL = SystemProperties.getJavaVendorUrl();

    /**
     * The {@code java.version} System Property. Java version number.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaVersion()
     * @since Java 1.1
     */
    public static final String JAVA_VERSION = SystemProperties.getJavaVersion();

    /**
     * The {@code java.vm.info} System Property. Java Virtual Machine implementation info.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaVmInfo()
     * @since 2.0
     * @since Java 1.2
     */
    public static final String JAVA_VM_INFO = SystemProperties.getJavaVmInfo();

    /**
     * The {@code java.vm.name} System Property. Java Virtual Machine implementation name.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaVmName()
     * @since Java 1.2
     */
    public static final String JAVA_VM_NAME = SystemProperties.getJavaVmName();

    /**
     * The {@code java.vm.specification.name} System Property. Java Virtual Machine specification name.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaVmSpecificationName()
     * @since Java 1.2
     */
    public static final String JAVA_VM_SPECIFICATION_NAME = SystemProperties.getJavaVmSpecificationName();

    /**
     * The {@code java.vm.specification.vendor} System Property. Java Virtual Machine specification vendor.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaVmSpecificationVendor()
     * @since Java 1.2
     */
    public static final String JAVA_VM_SPECIFICATION_VENDOR = SystemProperties.getJavaVmSpecificationVendor();

    /**
     * The {@code java.vm.specification.version} System Property. Java Virtual Machine specification version.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaVmSpecificationVersion()
     * @since Java 1.2
     */
    public static final String JAVA_VM_SPECIFICATION_VERSION = SystemProperties.getJavaVmSpecificationVersion();

    /**
     * The {@code java.vm.vendor} System Property. Java Virtual Machine implementation vendor.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaVmVendor()
     * @since Java 1.2
     */
    public static final String JAVA_VM_VENDOR = SystemProperties.getJavaVmVendor();

    /**
     * The {@code java.vm.version} System Property. Java Virtual Machine implementation version.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getJavaVmVersion()
     * @since Java 1.2
     */
    public static final String JAVA_VM_VERSION = SystemProperties.getJavaVmVersion();

    /**
     * The {@code line.separator} System Property. Line separator ({@code &quot;\n&quot;} on UNIX).
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getLineSeparator()
     * @deprecated Use {@link System#lineSeparator()} instead, since it does not require a privilege check.
     * @since Java 1.1
     */
    @Deprecated
    public static final String LINE_SEPARATOR = SystemProperties.getLineSeparator();

    /**
     * The {@code os.arch} System Property. Operating system architecture.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getOsArch()
     * @since Java 1.1
     */
    public static final String OS_ARCH = SystemProperties.getOsArch();

    /**
     * The {@code os.name} System Property. Operating system name.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getOsName()
     * @since Java 1.1
     */
    public static final String OS_NAME = SystemProperties.getOsName();

    /**
     * The {@code os.version} System Property. Operating system version.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getOsVersion()
     * @since Java 1.1
     */
    public static final String OS_VERSION = SystemProperties.getOsVersion();

    /**
     * The {@code path.separator} System Property. Path separator ({@code &quot;:&quot;} on UNIX).
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getPathSeparator()
     * @deprecated Use {@link File#pathSeparator}, since it is guaranteed to be a
     *             string containing a single character and it does not require a privilege check.
     * @since Java 1.1
     */
    @Deprecated
    public static final String PATH_SEPARATOR = SystemProperties.getPathSeparator();

    /**
     * The {@code user.country} or {@code user.region} System Property. User's country code, such as {@code "GB"}. First
     * in Java version 1.2 as {@code user.region}. Renamed to {@code user.country} in 1.4
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @since 2.0
     * @since Java 1.2
     */
    public static final String USER_COUNTRY = SystemProperties.getProperty(SystemProperties.USER_COUNTRY,
            () -> SystemProperties.getProperty(SystemProperties.USER_REGION));

    /**
     * The {@code user.dir} System Property. User's current working directory.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getUserDir()
     * @since Java 1.1
     */
    public static final String USER_DIR = SystemProperties.getUserDir();

    /**
     * The {@code user.home} System Property. User's home directory.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getUserHome()
     * @since Java 1.1
     */
    public static final String USER_HOME = SystemProperties.getUserHome();

    /**
     * The {@code user.language} System Property. User's language code, such as {@code "en"}.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getUserLanguage()
     * @since 2.0
     * @since Java 1.2
     */
    public static final String USER_LANGUAGE = SystemProperties.getUserLanguage();

    /**
     * The {@code user.name} System Property. User's account name.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
     * </p>
     *
     * @see SystemProperties#getUserName()
     * @since Java 1.1
     */
    public static final String USER_NAME = SystemProperties.getUserName();

    /**
     * The {@code user.timezone} System Property. For example: {@code "America/Los_Angeles"}.
     *
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
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
     * Is {@code true} if this is Java version 1.1 (also 1.1.x versions).
     *
     * <p>
     * The field will return {@code false} if {@link #JAVA_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     */
    public static final boolean IS_JAVA_1_1 = getJavaVersionMatches("1.1");

    /**
     * Is {@code true} if this is Java version 1.2 (also 1.2.x versions).
     *
     * <p>
     * The field will return {@code false} if {@link #JAVA_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     */
    public static final boolean IS_JAVA_1_2 = getJavaVersionMatches("1.2");

    /**
     * Is {@code true} if this is Java version 1.3 (also 1.3.x versions).
     *
     * <p>
     * The field will return {@code false} if {@link #JAVA_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     */
    public static final boolean IS_JAVA_1_3 = getJavaVersionMatches("1.3");

    /**
     * Is {@code true} if this is Java version 1.4 (also 1.4.x versions).
     *
     * <p>
     * The field will return {@code false} if {@link #JAVA_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     */
    public static final boolean IS_JAVA_1_4 = getJavaVersionMatches("1.4");

    /**
     * Is {@code true} if this is Java version 1.5 (also 1.5.x versions).
     *
     * <p>
     * The field will return {@code false} if {@link #JAVA_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     */
    public static final boolean IS_JAVA_1_5 = getJavaVersionMatches("1.5");

    /**
     * Is {@code true} if this is Java version 1.6 (also 1.6.x versions).
     *
     * <p>
     * The field will return {@code false} if {@link #JAVA_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     */
    public static final boolean IS_JAVA_1_6 = getJavaVersionMatches("1.6");

    /**
     * Is {@code true} if this is Java version 1.7 (also 1.7.x versions).
     *
     * <p>
     * The field will return {@code false} if {@link #JAVA_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.0
     */
    public static final boolean IS_JAVA_1_7 = getJavaVersionMatches("1.7");

    /**
     * Is {@code true} if this is Java version 1.8 (also 1.8.x versions).
     *
     * <p>
     * The field will return {@code false} if {@link #JAVA_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.3.2
     */
    public static final boolean IS_JAVA_1_8 = getJavaVersionMatches("1.8");

    /**
     * Is {@code true} if this is Java version 1.9 (also 1.9.x versions).
     *
     * <p>
     * The field will return {@code false} if {@link #JAVA_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.4
     *
     * @deprecated As of release 3.5, replaced by {@link #IS_JAVA_9}
     */
    @Deprecated
    public static final boolean IS_JAVA_1_9 = getJavaVersionMatches("9");

    /**
     * Is {@code true} if this is Java version 9 (also 9.x versions).
     *
     * <p>
     * The field will return {@code false} if {@link #JAVA_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.5
     */
    public static final boolean IS_JAVA_9 = getJavaVersionMatches("9");

    /**
     * Is {@code true} if this is Java version 10 (also 10.x versions).
     *
     * <p>
     * The field will return {@code false} if {@link #JAVA_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.7
     */
    public static final boolean IS_JAVA_10 = getJavaVersionMatches("10");

    /**
     * Is {@code true} if this is Java version 11 (also 11.x versions).
     *
     * <p>
     * The field will return {@code false} if {@link #JAVA_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.8
     */
    public static final boolean IS_JAVA_11 = getJavaVersionMatches("11");

    /**
     * Is {@code true} if this is Java version 12 (also 12.x versions).
     *
     * <p>
     * The field will return {@code false} if {@link #JAVA_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.9
     */
    public static final boolean IS_JAVA_12 = getJavaVersionMatches("12");

    /**
     * Is {@code true} if this is Java version 13 (also 13.x versions).
     *
     * <p>
     * The field will return {@code false} if {@link #JAVA_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.9
     */
    public static final boolean IS_JAVA_13 = getJavaVersionMatches("13");

    /**
     * Is {@code true} if this is Java version 14 (also 14.x versions).
     *
     * <p>
     * The field will return {@code false} if {@link #JAVA_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.10
     */
    public static final boolean IS_JAVA_14 = getJavaVersionMatches("14");

    /**
     * Is {@code true} if this is Java version 15 (also 15.x versions).
     *
     * <p>
     * The field will return {@code false} if {@link #JAVA_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.10
     */
    public static final boolean IS_JAVA_15 = getJavaVersionMatches("15");

    /**
     * Is {@code true} if this is Java version 16 (also 16.x versions).
     * <p>
     * The field will return {@code false} if {@link #JAVA_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.13.0
     */
    public static final boolean IS_JAVA_16 = getJavaVersionMatches("16");

    /**
     * Is {@code true} if this is Java version 17 (also 17.x versions).
     * <p>
     * The field will return {@code false} if {@link #JAVA_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.13.0
     */
    public static final boolean IS_JAVA_17 = getJavaVersionMatches("17");

    /**
     * Is {@code true} if this is Java version 18 (also 18.x versions).
     * <p>
     * The field will return {@code false} if {@link #JAVA_VERSION} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.13.0
     */
    public static final boolean IS_JAVA_18 = getJavaVersionMatches("18");

    // Operating system checks
    // -----------------------------------------------------------------------
    // These MUST be declared after those above as they depend on the
    // values being set up
    // Please advise dev@commons.apache.org if you want another added
    // or a mistake corrected

    /**
     * Is {@code true} if this is AIX.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 2.0
     */
    public static final boolean IS_OS_AIX = getOsMatchesName("AIX");

    /**
     * Is {@code true} if this is HP-UX.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 2.0
     */
    public static final boolean IS_OS_HP_UX = getOsMatchesName("HP-UX");

    /**
     * Is {@code true} if this is IBM OS/400.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.3
     */
    public static final boolean IS_OS_400 = getOsMatchesName("OS/400");

    /**
     * Is {@code true} if this is Irix.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 2.0
     */
    public static final boolean IS_OS_IRIX = getOsMatchesName("Irix");

    /**
     * Is {@code true} if this is Linux.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 2.0
     */
    public static final boolean IS_OS_LINUX = getOsMatchesName("Linux") || getOsMatchesName("LINUX");

    /**
     * Is {@code true} if this is Mac.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 2.0
     */
    public static final boolean IS_OS_MAC = getOsMatchesName("Mac");

    /**
     * Is {@code true} if this is Mac.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 2.0
     */
    public static final boolean IS_OS_MAC_OSX = getOsMatchesName("Mac OS X");

    /**
     * Is {@code true} if this is Mac OS X Cheetah.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.4
     */
    public static final boolean IS_OS_MAC_OSX_CHEETAH = getOsMatches("Mac OS X", "10.0");

    /**
     * Is {@code true} if this is Mac OS X Puma.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.4
     */
    public static final boolean IS_OS_MAC_OSX_PUMA = getOsMatches("Mac OS X", "10.1");

    /**
     * Is {@code true} if this is Mac OS X Jaguar.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.4
     */
    public static final boolean IS_OS_MAC_OSX_JAGUAR = getOsMatches("Mac OS X", "10.2");

    /**
     * Is {@code true} if this is Mac OS X Panther.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.4
     */
    public static final boolean IS_OS_MAC_OSX_PANTHER = getOsMatches("Mac OS X", "10.3");

    /**
     * Is {@code true} if this is Mac OS X Tiger.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.4
     */
    public static final boolean IS_OS_MAC_OSX_TIGER = getOsMatches("Mac OS X", "10.4");

    /**
     * Is {@code true} if this is Mac OS X Leopard.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.4
     */
    public static final boolean IS_OS_MAC_OSX_LEOPARD = getOsMatches("Mac OS X", "10.5");

    /**
     * Is {@code true} if this is Mac OS X Snow Leopard.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.4
     */
    public static final boolean IS_OS_MAC_OSX_SNOW_LEOPARD = getOsMatches("Mac OS X", "10.6");

    /**
     * Is {@code true} if this is Mac OS X Lion.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.4
     */
    public static final boolean IS_OS_MAC_OSX_LION = getOsMatches("Mac OS X", "10.7");

    /**
     * Is {@code true} if this is Mac OS X Mountain Lion.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.4
     */
    public static final boolean IS_OS_MAC_OSX_MOUNTAIN_LION = getOsMatches("Mac OS X", "10.8");

    /**
     * Is {@code true} if this is Mac OS X Mavericks.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.4
     */
    public static final boolean IS_OS_MAC_OSX_MAVERICKS = getOsMatches("Mac OS X", "10.9");

    /**
     * Is {@code true} if this is Mac OS X Yosemite.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.4
     */
    public static final boolean IS_OS_MAC_OSX_YOSEMITE = getOsMatches("Mac OS X", "10.10");

    /**
     * Is {@code true} if this is Mac OS X El Capitan.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.5
     */
    public static final boolean IS_OS_MAC_OSX_EL_CAPITAN = getOsMatches("Mac OS X", "10.11");

    /**
     * Is {@code true} if this is Mac OS X Sierra.
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
    public static final boolean IS_OS_MAC_OSX_SIERRA = getOsMatches("Mac OS X", "10.12");

    /**
     * Is {@code true} if this is Mac OS X High Sierra.
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
    public static final boolean IS_OS_MAC_OSX_HIGH_SIERRA = getOsMatches("Mac OS X", "10.13");

    /**
     * Is {@code true} if this is Mac OS X Mojave.
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
    public static final boolean IS_OS_MAC_OSX_MOJAVE = getOsMatches("Mac OS X", "10.14");

    /**
     * Is {@code true} if this is Mac OS X Catalina.
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
     * Is {@code true} if this is Mac OS X Big Sur.
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
    public static final boolean IS_OS_MAC_OSX_BIG_SUR = getOsMatches("Mac OS X", "10.16");

    /**
     * Is {@code true} if this is FreeBSD.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.1
     */
    public static final boolean IS_OS_FREE_BSD = getOsMatchesName("FreeBSD");

    /**
     * Is {@code true} if this is OpenBSD.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.1
     */
    public static final boolean IS_OS_OPEN_BSD = getOsMatchesName("OpenBSD");

    /**
     * Is {@code true} if this is NetBSD.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.1
     */
    public static final boolean IS_OS_NET_BSD = getOsMatchesName("NetBSD");

    /**
     * Is {@code true} if this is OS/2.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 2.0
     */
    public static final boolean IS_OS_OS2 = getOsMatchesName("OS/2");

    /**
     * Is {@code true} if this is Solaris.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 2.0
     */
    public static final boolean IS_OS_SOLARIS = getOsMatchesName("Solaris");

    /**
     * Is {@code true} if this is SunOS.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 2.0
     */
    public static final boolean IS_OS_SUN_OS = getOsMatchesName("SunOS");

    /**
     * Is {@code true} if this is a UNIX like system, as in any of AIX, HP-UX, Irix, Linux, MacOSX, Solaris or SUN OS.
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
    public static final boolean IS_OS_UNIX = IS_OS_AIX || IS_OS_HP_UX || IS_OS_IRIX || IS_OS_LINUX || IS_OS_MAC_OSX
            || IS_OS_SOLARIS || IS_OS_SUN_OS || IS_OS_FREE_BSD || IS_OS_OPEN_BSD || IS_OS_NET_BSD;

    /**
     * Is {@code true} if this is Windows.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 2.0
     */
    public static final boolean IS_OS_WINDOWS = getOsMatchesName(OS_NAME_WINDOWS_PREFIX);

    /**
     * Is {@code true} if this is Windows 2000.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 2.0
     */
    public static final boolean IS_OS_WINDOWS_2000 = getOsMatchesName(OS_NAME_WINDOWS_PREFIX + " 2000");

    /**
     * Is {@code true} if this is Windows 2003.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.1
     */
    public static final boolean IS_OS_WINDOWS_2003 = getOsMatchesName(OS_NAME_WINDOWS_PREFIX + " 2003");

    /**
     * Is {@code true} if this is Windows Server 2008.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.1
     */
    public static final boolean IS_OS_WINDOWS_2008 = getOsMatchesName(OS_NAME_WINDOWS_PREFIX + " Server 2008");

    /**
     * Is {@code true} if this is Windows Server 2012.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.4
     */
    public static final boolean IS_OS_WINDOWS_2012 = getOsMatchesName(OS_NAME_WINDOWS_PREFIX + " Server 2012");

    /**
     * Is {@code true} if this is Windows 95.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 2.0
     */
    public static final boolean IS_OS_WINDOWS_95 = getOsMatchesName(OS_NAME_WINDOWS_PREFIX + " 95");

    /**
     * Is {@code true} if this is Windows 98.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 2.0
     */
    public static final boolean IS_OS_WINDOWS_98 = getOsMatchesName(OS_NAME_WINDOWS_PREFIX + " 98");

    /**
     * Is {@code true} if this is Windows ME.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 2.0
     */
    public static final boolean IS_OS_WINDOWS_ME = getOsMatchesName(OS_NAME_WINDOWS_PREFIX + " Me");

    /**
     * Is {@code true} if this is Windows NT.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 2.0
     */
    public static final boolean IS_OS_WINDOWS_NT = getOsMatchesName(OS_NAME_WINDOWS_PREFIX + " NT");

    /**
     * Is {@code true} if this is Windows XP.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 2.0
     */
    public static final boolean IS_OS_WINDOWS_XP = getOsMatchesName(OS_NAME_WINDOWS_PREFIX + " XP");

    /**
     * Is {@code true} if this is Windows Vista.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 2.4
     */
    public static final boolean IS_OS_WINDOWS_VISTA = getOsMatchesName(OS_NAME_WINDOWS_PREFIX + " Vista");

    /**
     * Is {@code true} if this is Windows 7.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.0
     */
    public static final boolean IS_OS_WINDOWS_7 = getOsMatchesName(OS_NAME_WINDOWS_PREFIX + " 7");

    /**
     * Is {@code true} if this is Windows 8.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.2
     */
    public static final boolean IS_OS_WINDOWS_8 = getOsMatchesName(OS_NAME_WINDOWS_PREFIX + " 8");

    /**
     * Is {@code true} if this is Windows 10.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
     * </p>
     * <p>
     * This value is initialized when the class is loaded.
     * </p>
     *
     * @since 3.5
     */
    public static final boolean IS_OS_WINDOWS_10 = getOsMatchesName(OS_NAME_WINDOWS_PREFIX + " 10");

    /**
     * Is {@code true} if this is Windows 11.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
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
    public static final boolean IS_OS_WINDOWS_11 = getOsMatchesName(OS_NAME_WINDOWS_PREFIX + " 11");

    /**
     * Is {@code true} if this is z/OS.
     *
     * <p>
     * The field will return {@code false} if {@code OS_NAME} is {@code null}.
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
    public static final boolean IS_OS_ZOS = getOsMatchesName("z/OS");

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
     * The {@code awt.toolkit} System Property.
     *
     * <p>
     * Holds a class name, on Windows XP this is {@code sun.awt.windows.WToolkit}.
     * </p>
     * <p>
     * <b>On platforms without a GUI, this value is {@code null}.</b>
     * </p>
     * <p>
     * Defaults to {@code null} if the runtime does not have security access to read this property or the property does
     * not exist.
     * </p>
     * <p>
     * This value is initialized when the class is loaded. If {@link System#setProperty(String,String)} or
     * {@link System#setProperties(java.util.Properties)} is called after this class is loaded, the value will be out of
     * sync with that System property.
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
     * If a {@link SecurityException} is caught, the return value is {@code defaultValue} and a message is written to
     * {@code System.err}.
     * </p>
     *
     * @param name
     *            the environment variable name
     * @param defaultValue
     *            the default value
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
     * Gets the host name from an environment variable
     * (COMPUTERNAME on Windows, HOSTNAME elsewhere).
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
     * @throws SecurityException if a security manager exists and its {@code checkPropertyAccess} method doesn't allow
     * access to the specified system property.
     * @see SystemProperties#getJavaHome()
     * @since 2.1
     */
    public static File getJavaHome() {
        return new File(SystemProperties.getJavaHome());
    }

    /**
     * Gets the current Java IO temporary directory as a {@link File}.
     *
     * @return a directory
     * @throws SecurityException if a security manager exists and its {@code checkPropertyAccess} method doesn't allow
     * access to the specified system property.
     * @see SystemProperties#getJavaIoTmpdir()
     * @since 2.1
     */
    public static File getJavaIoTmpDir() {
        return new File(SystemProperties.getJavaIoTmpdir());
    }

    /**
     * Decides if the Java version matches.
     *
     * @param versionPrefix the prefix for the java version
     * @return true if matches, or false if not or can't determine
     */
    private static boolean getJavaVersionMatches(final String versionPrefix) {
        return isJavaVersionMatch(JAVA_SPECIFICATION_VERSION, versionPrefix);
    }

    /**
     * Decides if the operating system matches.
     *
     * @param osNamePrefix the prefix for the OS name
     * @param osVersionPrefix the prefix for the version
     * @return true if matches, or false if not or can't determine
     */
    private static boolean getOsMatches(final String osNamePrefix, final String osVersionPrefix) {
        return isOSMatch(OS_NAME, OS_VERSION, osNamePrefix, osVersionPrefix);
    }

    /**
     * Decides if the operating system matches.
     *
     * @param osNamePrefix the prefix for the OS name
     * @return true if matches, or false if not or can't determine
     */
    private static boolean getOsMatchesName(final String osNamePrefix) {
        return isOSNameMatch(OS_NAME, osNamePrefix);
    }

    /**
     * Gets the current user directory as a {@link File}.
     *
     * @return a directory
     * @throws SecurityException if a security manager exists and its {@code checkPropertyAccess} method doesn't allow
     * access to the specified system property.
     * @see SystemProperties#getUserDir()
     * @since 2.1
     */
    public static File getUserDir() {
        return new File(SystemProperties.getUserDir());
    }

    /**
     * Gets the current user home directory as a {@link File}.
     *
     * @return a directory
     * @throws SecurityException if a security manager exists and its {@code checkPropertyAccess} method doesn't allow
     * access to the specified system property.
     * @see SystemProperties#getUserHome()
     * @since 2.1
     */
    public static File getUserHome() {
        return new File(SystemProperties.getUserHome());
    }

    /**
     * Gets the current user name.
     *
     * @return a name
     * @throws SecurityException if a security manager exists and its {@code checkPropertyAccess} method doesn't allow
     * access to the specified system property.
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
     *
     * @param defaultValue A default value.
     * @return a name
     * @throws SecurityException if a security manager exists and its {@code checkPropertyAccess} method doesn't allow
     * access to the specified system property.
     * @see SystemProperties#getUserName()
     * @since 3.10
     */
    public static String getUserName(final String defaultValue) {
        return System.getProperty(SystemProperties.USER_NAME, defaultValue);
    }

    /**
     * Returns whether the {@link #JAVA_AWT_HEADLESS} value is {@code true}.
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
     * Is the Java version at least the requested version.
     *
     * @param requiredVersion the required version, for example 1.31f
     * @return {@code true} if the actual version is equal or greater than the required version
     */
    public static boolean isJavaVersionAtLeast(final JavaVersion requiredVersion) {
        return JAVA_SPECIFICATION_VERSION_AS_ENUM.atLeast(requiredVersion);
    }

    /**
     * Is the Java version at most the requested version.
     *
     * <p>
     * Example input:
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
     * Decides if the Java version matches.
     *
     * <p>
     * This method is package private instead of private to support unit test invocation.
     * </p>
     *
     * @param version the actual Java version
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
     * Decides if the operating system matches.
     * <p>
     * This method is package private instead of private to support unit test invocation.
     * </p>
     *
     * @param osName the actual OS name
     * @param osVersion the actual OS version
     * @param osNamePrefix the prefix for the expected OS name
     * @param osVersionPrefix the prefix for the expected OS version
     * @return true if matches, or false if not or can't determine
     */
    static boolean isOSMatch(final String osName, final String osVersion, final String osNamePrefix, final String osVersionPrefix) {
        if (osName == null || osVersion == null) {
            return false;
        }
        return isOSNameMatch(osName, osNamePrefix) && isOSVersionMatch(osVersion, osVersionPrefix);
    }

    /**
     * Decides if the operating system matches.
     * <p>
     * This method is package private instead of private to support unit test invocation.
     * </p>
     *
     * @param osName the actual OS name
     * @param osNamePrefix the prefix for the expected OS name
     * @return true if matches, or false if not or can't determine
     */
    static boolean isOSNameMatch(final String osName, final String osNamePrefix) {
        if (osName == null) {
            return false;
        }
        return osName.startsWith(osNamePrefix);
    }

    /**
     * Decides if the operating system version matches.
     * <p>
     * This method is package private instead of private to support unit test invocation.
     * </p>
     *
     * @param osVersion the actual OS version
     * @param osVersionPrefix the prefix for the expected OS version
     * @return true if matches, or false if not or can't determine
     */
    static boolean isOSVersionMatch(final String osVersion, final String osVersionPrefix) {
        if (StringUtils.isEmpty(osVersion)) {
            return false;
        }
        // Compare parts of the version string instead of using String.startsWith(String) because otherwise
        // osVersionPrefix 10.1 would also match osVersion 10.10
        final String[] versionPrefixParts = osVersionPrefix.split("\\.");
        final String[] versionParts = osVersion.split("\\.");
        for (int i = 0; i < Math.min(versionPrefixParts.length, versionParts.length); i++) {
            if (!versionPrefixParts[i].equals(versionParts[i])) {
                return false;
            }
        }
        return true;
    }

    /**
     * SystemUtils instances should NOT be constructed in standard programming. Instead, the class should be used as
     * {@code SystemUtils.FILE_SEPARATOR}.
     *
     * <p>
     * This constructor is public to permit tools that require a JavaBean instance to operate.
     * </p>
     */
    public SystemUtils() {
    }

}
