/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002-2003 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution, if
 *    any, must include the following acknowlegement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowlegement may appear in the software itself,
 *    if and wherever such third-party acknowlegements normally appear.
 *
 * 4. The names "The Jakarta Project", "Commons", and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 */
package org.apache.commons.lang;

/**
 * <p>Helpers for <code>java.lang.System</code>.</p>
 * 
 * <p>If a system property cannot be read due to security restrictions, 
 * the corresponding field in this class will be set to <code>null</code>
 * and a message will be written to <code>System.err</code>.</p>
 *
 * @author Based on code from Avalon Excalibur
 * @author Based on code from Lucene
 * @author Stephen Colebourne
 * @author <a href="mailto:sdowney@panix.com">Steve Downey</a>
 * @author <a href="mailto:ggregory@seagullsw.com">Gary Gregory</a>
 * @author Michael Becke
 * @since 1.0
 * @version $Id: SystemUtils.java,v 1.16 2003/06/08 14:10:54 scolebourne Exp $
 */
public class SystemUtils {

    //-----------------------------------------------------------------------
    /**
     * The <code>file.encoding</code> System Property.
     * <p>File encoding, such as <code>Cp1252</code>.</p>
     * 
     * <p>Defaults to <code>null</code> if the runtime does not have
     * security access to read this property or the property does not exist.</p>
     * 
     * @since Java version 1.2.
     */
    public static final String FILE_ENCODING = getSystemProperty("file.encoding");

    /**
     * The <code>file.separator</code> System Property.
     * File separator ("/" on UNIX).
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property or the property does not exist.</p>
     * 
     * @since Java version 1.1.
     */
    public static final String FILE_SEPARATOR = getSystemProperty("file.separator");

    //-----------------------------------------------------------------------
    /**
     * Is <code>true</code> if this is Java version 1.1 (also 1.1.x versions).
     * <p>
     * The field will return false if {@link #JAVA_VERSION} is <code>null</code>.</p>
     */
    public static final boolean IS_JAVA_1_1 = getJavaVersionMatches("1.1");

    /**
     * Is <code>true</code> if this is Java version 1.2 (also 1.2.x versions).
     * <p>
     * The field will return false if {@link #JAVA_VERSION} is <code>null</code>.</p>
     */
    public static final boolean IS_JAVA_1_2 = getJavaVersionMatches("1.2");

    /**
     * Is <code>true</code> if this is Java version 1.3 (also 1.3.x versions).
     * <p>
     * The field will return false if {@link #JAVA_VERSION} is <code>null</code>.</p>
     */
    public static final boolean IS_JAVA_1_3 = getJavaVersionMatches("1.3");

    /**
     * Is <code>true</code> if this is Java version 1.4 (also 1.4.x versions).
     * <p>
     * The field will return false if {@link #JAVA_VERSION} is <code>null</code>.</p>
     */
    public static final boolean IS_JAVA_1_4 = getJavaVersionMatches("1.4");

    /**
     * Is <code>true</code> if this is Java version 1.5 (also 1.5.x versions).
     * <p>
     * The field will return false if {@link #JAVA_VERSION} is <code>null</code>.</p>
     */
    public static final boolean IS_JAVA_1_5 = getJavaVersionMatches("1.5");

    /**
     * Is <code>true</code> if this is AIX.
     * <p>
     * The field will return false if <code>OS_NAME</code> is <code>null</code>.</p>
     */
    public static final boolean IS_OS_AIX = getOSMatches("AIX");

    /**
     * Is <code>true</code> if this is HP-UX.
     * <p>
     * The field will return false if <code>OS_NAME</code> is <code>null</code>.</p>
     */
    public static final boolean IS_OS_HP_UX = getOSMatches("HP-UX");

    /**
     * Is <code>true</code> if this is Irix.
     * <p>
     * The field will return false if <code>OS_NAME</code> is <code>null</code>.</p>
     */
    public static final boolean IS_OS_IRIX = getOSMatches("Irix");

    /**
     * Is <code>true</code> if this is Linux.
     * <p>
     * The field will return false if <code>OS_NAME</code> is <code>null</code>.</p>
     */
    public static final boolean IS_OS_LINUX = getOSMatches("Linux") || getOSMatches("LINUX");
    // Windows XP returns 'Windows 2000' just for fun...

    /**
     * Is <code>true</code> if this is Mac.
     * <p>
     * The field will return false if <code>OS_NAME</code> is <code>null</code>.</p>
     */
    public static final boolean IS_OS_MAC = getOSMatches("Mac");

    /**
     * Is <code>true</code> if this is Mac.
     * <p>
     * The field will return false if <code>OS_NAME</code> is <code>null</code>.</p>
     */
    public static final boolean IS_OS_MAC_OSX = getOSMatches("Mac OS X");

    /**
     * Is <code>true</code> if this is Mac.
     * <p>
     * The field will return false if <code>OS_NAME</code> is <code>null</code>.</p>
     */
    public static final boolean IS_OS_OS2 = getOSMatches("OS/2");

    /**
     * Is <code>true</code> if this is Solaris.
     * <p>
     * The field will return false if <code>OS_NAME</code> is <code>null</code>.</p>
     */
    public static final boolean IS_OS_SOLARIS = getOSMatches("Solaris");

    /**
     * Is <code>true</code> if this is SunOS.
     * <p>
     * The field will return false if <code>OS_NAME</code> is <code>null</code>.</p>
     */
    public static final boolean IS_OS_SUN_OS = getOSMatches("SunOS");

    //-----------------------------------------------------------------------
    // OS names from http://www.vamphq.com/os.html
    // Selected ones included - please advise commons-dev@jakarta.apache.org
    // if you want another added or a mistake corrected

    /**
     * Is <code>true</code> if this is Windows.
     * <p>
     * The field will return false if <code>OS_NAME</code> is <code>null</code>.</p>
     */
    public static final boolean IS_OS_WINDOWS = getOSMatches("Windows");

    /**
     * Is <code>true</code> if this is Windows 2000.
     * <p>
     * The field will return false if <code>OS_NAME</code> is <code>null</code>.</p>
     */
    public static final boolean IS_OS_WINDOWS_2000 = getOSMatches("Windows", "5.0");

    /**
     * Is <code>true</code> if this is Windows 95.
     * <p>
     * The field will return false if <code>OS_NAME</code> is <code>null</code>.</p>
     */
    public static final boolean IS_OS_WINDOWS_95 = getOSMatches("Windows 9", "4.0");
    // JDK 1.2 running on Windows98 returns 'Windows 95', hence the above

    /**
     * Is <code>true</code> if this is Windows 98.
     * <p>
     * The field will return false if <code>OS_NAME</code> is <code>null</code>.</p>
     */
    public static final boolean IS_OS_WINDOWS_98 = getOSMatches("Windows 9", "4.1");
    // JDK 1.2 running on Windows98 returns 'Windows 95', hence the above

    /**
     * Is <code>true</code> if this is Windows ME.
     * <p>
     * The field will return false if <code>OS_NAME</code> is <code>null</code>.</p>
     */
    public static final boolean IS_OS_WINDOWS_ME = getOSMatches("Windows", "4.9");
    // JDK 1.2 running on WindowsME may return 'Windows 95', hence the above

    /**
     * Is <code>true</code> if this is Windows NT.
     * <p>
     * The field will return false if <code>OS_NAME</code> is <code>null</code>.</p>
     */
    public static final boolean IS_OS_WINDOWS_NT = getOSMatches("Windows NT");
    // Windows 2000 returns 'Windows 2000' but may suffer from same JDK1.2 problem

    /**
     * Is <code>true</code> if this is Windows XP.
     * <p>
     * The field will return false if <code>OS_NAME</code> is <code>null</code>.</p>
     */
    public static final boolean IS_OS_WINDOWS_XP = getOSMatches("Windows", "5.1");

    /**
     * The <code>java.class.path</code> System Property.
     * Java class path.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property or the property does not exist.</p>
     * 
     * @since Java version 1.1.
     */
    public static final String JAVA_CLASS_PATH = getSystemProperty("java.class.path");

    /**
     * The <code>java.class.version</code> System Property.
     * Java class format version number.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property or the property does not exist.</p>
     * 
     * @since Java version 1.1.
     */
    public static final String JAVA_CLASS_VERSION = getSystemProperty("java.class.version");

    /**
     * The <code>java.compiler</code> System Property.
     * Name of JIT compiler to use.
     * First in JDK version 1.2. Not used in Sun JDKs after 1.2.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property or the property does not exist.</p>
     * 
     * @since Java version 1.2. Not used in Sun versions after 1.2.
     */
    public static final String JAVA_COMPILER = getSystemProperty("java.compiler");

    /**
     * The <code>java.ext.dirs</code> System Property.
     * Path of extension directory or directories.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property or the property does not exist.</p>
     * 
     * @since Java 1.3
     */
    public static final String JAVA_EXT_DIRS = getSystemProperty("java.ext.dirs");

    /**
     * The <code>java.home</code> System Property.
     * Java installation directory.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property or the property does not exist.</p>
     * 
     * @since Java 1.1
     */
    public static final String JAVA_HOME = getSystemProperty("java.home");

    /**
     * The <code>java.io.tmpdir</code> System Property.
     * Default temp file path.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property or the property does not exist.</p>
     * 
     * @since Java 1.2
     */
    public static final String JAVA_IO_TMPDIR = getSystemProperty("java.io.tmpdir");

    /**
     * The <code>java.library.path</code> System Property.
     * List of paths to search when loading libraries.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property or the property does not exist.</p>
     * 
     * @since Java 1.2
     */
    public static final String JAVA_LIBRARY_PATH = getSystemProperty("java.library.path");

    /**
     * The <code>java.runtime.name</code> System Property.
     * Java Runtime Environment name.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property or the property does not exist.</p>
     * 
     * @since Java 1.3
     */
    public static final String JAVA_RUNTIME_NAME = getSystemProperty("java.runtime.name");

    /**
     * The <code>java.runtime.version</code> System Property.
     * Java Runtime Environment version.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property or the property does not exist.</p>
     * 
     * @since Java 1.3
     */
    public static final String JAVA_RUNTIME_VERSION = getSystemProperty("java.runtime.version");

    /**
     * The <code>java.specification.name</code> System Property.
     * Java Runtime Environment specification name.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property or the property does not exist.</p>
     * 
     * @since Java 1.2
     */
    public static final String JAVA_SPECIFICATION_NAME = getSystemProperty("java.specification.name");

    /**
     * The <code>java.specification.vendor</code> System Property.
     * Java Runtime Environment specification vendor.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property or the property does not exist.</p>
     * 
     * @since Java 1.2
     */
    public static final String JAVA_SPECIFICATION_VENDOR = getSystemProperty("java.specification.vendor");

    /**
     * The <code>java.specification.version</code> System Property.
     * Java Runtime Environment specification version.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property or the property does not exist.</p>
     * 
     * @since Java 1.3
     */
    public static final String JAVA_SPECIFICATION_VERSION = getSystemProperty("java.specification.version");

    /**
     * The <code>java.vendor</code> System Property.
     * Java vendor-specific string.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property or the property does not exist.</p>
     * 
     * @since Java 1.1
     */
    public static final String JAVA_VENDOR = getSystemProperty("java.vendor");

    /**
     * The <code>java.vendor.url</code> System Property.
     * Java vendor URL.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property or the property does not exist.</p>
      * 
     * @since Java 1.1
    */
    public static final String JAVA_VENDOR_URL = getSystemProperty("java.vendor.url");

    /**
     * The <code>java.version</code> System Property.
     * Java version number.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property or the property does not exist.</p>
     * 
     * @since Java 1.1
     */
    public static final String JAVA_VERSION = getSystemProperty("java.version");

    //-----------------------------------------------------------------------
    /**
     * Gets the Java version as a <code>float</code>.
     * <p>Example return values:</p>
     * <ul>
     *  <li><code>1.2f</code> for JDK 1.2
     *  <li><code>1.31f</code> for JDK 1.3.1
     * </ul>
     * <p>
     * The field will return zero if {@link #JAVA_VERSION} is <code>null</code>.</p>
     */
    public static final float JAVA_VERSION_FLOAT = getJavaVersionAsFloat();

    /**
     * Gets the Java version as an <code>int</code>.
     * <p>Example return values:</p>
     * <ul>
     *  <li><code>120</code> for JDK 1.2
     *  <li><code>131</code> for JDK 1.3.1
     * </ul>
     * <p>
     * The field will return zero if {@link #JAVA_VERSION} is <code>null</code>.</p>
     */
    public static final int JAVA_VERSION_INT = getJavaVersionAsInt();

    /**
     * The <code>java.vm.info</code> System Property.
     * Java Virtual Machine implementation info.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property or the property does not exist.</p>
     * 
     * @since Java 1.2
     */
    public static final String JAVA_VM_INFO = getSystemProperty("java.vm.info");

    /**
     * The <code>java.vm.name</code> System Property.
     * Java Virtual Machine implementation name.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property or the property does not exist.</p>
     * 
     * @since Java 1.2
     */
    public static final String JAVA_VM_NAME = getSystemProperty("java.vm.name");

    /**
     * The <code>java.vm.specification.name</code> System Property.
     * Java Virtual Machine specification name.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property or the property does not exist.</p>
     * 
     * @since Java 1.2
     */
    public static final String JAVA_VM_SPECIFICATION_NAME = getSystemProperty("java.vm.specification.name");

    /**
     * The <code>java.vm.specification.vendor</code> System Property.
     * Java Virtual Machine specification vendor.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property or the property does not exist.</p>
     * 
     * @since Java 1.2
     */
    public static final String JAVA_VM_SPECIFICATION_VENDOR = getSystemProperty("java.vm.specification.vendor");

    /**
     * The <code>java.vm.specification.version</code> System Property.
     * Java Virtual Machine specification version.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property or the property does not exist.</p>
     * 
     * @since Java 1.2
     */
    public static final String JAVA_VM_SPECIFICATION_VERSION = getSystemProperty("java.vm.specification.version");

    /**
     * The <code>java.vm.vendor</code> System Property.
     * Java Virtual Machine implementation vendor.
      * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property or the property does not exist.</p>
     * 
     * @since Java 1.2
     */
    public static final String JAVA_VM_VENDOR = getSystemProperty("java.vm.vendor");

    /**
     * The <code>java.vm.version</code> System Property.
     * Java Virtual Machine implementation version.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property or the property does not exist.</p>
     * 
     * @since Java 1.2
     */
    public static final String JAVA_VM_VERSION = getSystemProperty("java.vm.version");

    /**
     * The <code>line.separator</code> System Property.
     * Line separator ("\n" on UNIX).
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property or the property does not exist.</p>
     * 
     * @since Java 1.1
     */
    public static final String LINE_SEPARATOR = getSystemProperty("line.separator");

    /**
     * The <code>os.arch</code> System Property.
     * Operating system architecture.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property or the property does not exist.</p>
     * 
     * @since Java 1.1
     */
    public static final String OS_ARCH = getSystemProperty("os.arch");

    /**
     * The <code>os.name</code> System Property.
     * Operating system name.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property or the property does not exist.</p>
     * 
     * @since Java 1.1
     */
    public static final String OS_NAME = getSystemProperty("os.name");

    /**
     * The <code>os.version</code> System Property.
     * Operating system version.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property or the property does not exist.</p>
     * 
     * @since Java 1.1
     */
    public static final String OS_VERSION = getSystemProperty("os.version");

    /**
     * The <code>path.separator</code> System Property.
     * Path separator (":" on UNIX).
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property or the property does not exist.</p>
     * 
     * @since Java 1.1
     */
    public static final String PATH_SEPARATOR = getSystemProperty("path.separator");

    /**
     * The <code>user.country</code> or <code>user.region</code> System Property.
     * User's country code, such as <code>GB</code>.
     * First in JDK version 1.2 as <code>user.region</code>.
     * Renamed to <code>user.country</code> in 1.4
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property or the property does not exist.</p>
     * 
     * @since Java 1.2
     */
    public static final String USER_COUNTRY = 
        (getSystemProperty("user.country") == null ?
            getSystemProperty("user.region") : getSystemProperty("user.country"));

    /**
     * The <code>user.dir</code> System Property.
     * User's current working directory.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property or the property does not exist.</p>
     * 
     * @since Java 1.1
     */
    public static final String USER_DIR = getSystemProperty("user.dir");

    /**
     * The <code>user.home</code> System Property.
     * User's home directory.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property or the property does not exist.</p>
     * 
     * @since Java 1.1
     */
    public static final String USER_HOME = getSystemProperty("user.home");

    /**
     * The <code>user.language</code> System Property.
     * User's language code, such as 'en'.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property or the property does not exist.</p>
     * 
     * @since Java 1.2
     */
    public static final String USER_LANGUAGE = getSystemProperty("user.language");

    /**
     * The <code>user.name</code> System Property.
     * User's account name.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property or the property does not exist.</p>
     * 
     * @since Java 1.1
     */
    public static final String USER_NAME = getSystemProperty("user.name");

    //-----------------------------------------------------------------------    
    /**
     * <p>Gets the Java version number as a <code>float</code>.</p>
     *
     * <p>Example return values:</p>
     * <ul>
     *  <li><code>1.2f</code> for JDK 1.2
     *  <li><code>1.31f</code> for JDK 1.3.1
     * </ul>
     * 
     * @deprecated Use {@link #JAVA_VERSION_FLOAT} instead.
     *             Method will be removed in Commons Lang 3.0.
     * @return the version, for example 1.31f for JDK 1.3.1
     */
    public static float getJavaVersion() {
        return JAVA_VERSION_FLOAT;
    }

    /**
     * <p>Gets the Java version number as a <code>float</code>.</p>
     *
     * <p>Example return values:</p>
     * <ul>
     *  <li><code>1.2f</code> for JDK 1.2
     *  <li><code>1.31f</code> for JDK 1.3.1
     * </ul>
     * 
     * <p>Patch releases are not reported.
     * Zero is returned if {@link #JAVA_VERSION} is <code>null</code>.</p>
     * 
     * @return the version, for example 1.31f for JDK 1.3.1
     */
    private static float getJavaVersionAsFloat() {
        if (JAVA_VERSION == null) {
            return 0f;
        }
        String str = JAVA_VERSION.substring(0, 3);
        if (JAVA_VERSION.length() >= 5) {
            str = str + JAVA_VERSION.substring(4, 5);
        }
        return Float.parseFloat(str);
    }
    
    /**
     * <p>Gets the Java version number as an <code>int</code>.</p>
     *
     * <p>Example return values:</p>
     * <ul>
     *  <li><code>120</code> for JDK 1.2
     *  <li><code>131</code> for JDK 1.3.1
     * </ul>
     * 
     * <p>Patch releases are not reported.
     * Zero is returned if {@link #JAVA_VERSION} is <code>null</code>.</p>
     * 
     * @return the version, for example 131 for JDK 1.3.1
     */
    private static int getJavaVersionAsInt() {
        if (JAVA_VERSION == null) {
            return 0;
        }
        String str = JAVA_VERSION.substring(0, 1);
        str = str + JAVA_VERSION.substring(2, 3);
        if (JAVA_VERSION.length() >= 5) {
            str = str + JAVA_VERSION.substring(4, 5);
        } else {
            str = str + "0";
        }
        return Integer.parseInt(str);
    }

    /**
     * Decides if the java version matches.
     * 
     * @param versionPrefix  the prefix for the java version
     * @return true if matches, or false if not or can't determine
     */
    private static boolean getJavaVersionMatches(String versionPrefix) {
        if (JAVA_VERSION == null) {
            return false;
        }
        return JAVA_VERSION.startsWith(versionPrefix);
    }    
    
    /**
     * Decides if the operating system matches.
     * 
     * @param osNamePrefix  the prefix for the os name
     * @return true if matches, or false if not or can't determine
     */
    private static boolean getOSMatches(String osNamePrefix) {
        if (OS_NAME == null) {
            return false;
        }
        return OS_NAME.startsWith(osNamePrefix);
    }    

    /**
     * Decides if the operating system matches.
     * 
     * @param osNamePrefix  the prefix for the os name
     * @param osVersionPrefix  the prefix for the version
     * @return true if matches, or false if not or can't determine
     */
    private static boolean getOSMatches(String osNamePrefix, String osVersionPrefix) {
        if (OS_NAME == null || OS_VERSION == null) {
            return false;
        }
        return OS_NAME.startsWith(osNamePrefix) && OS_VERSION.startsWith(osVersionPrefix);
    }    

    //-----------------------------------------------------------------------
    /**
     * Gets a System property, defaulting to <code>null</code> if the property 
     * cannot be read. If a <code>SecurityException</code> is caught, the return
     * value is <code>null</code> and a message is written to <code>System.err</code>.
     * 
     * @param property the system property name
     * @return the system property value or <code>null</code> if a security problem occurs
     */
    private static String getSystemProperty(String property) {
        try {
            return System.getProperty(property);
        } catch (SecurityException ex) {
            // we are not allowed to look at this property
            System.err.println(
                "Caught a SecurityException reading the system property '" + property 
                + "'; the SystemUtils property value will default to null."
            );
            return null;
        }
    }    
    
    /**
     * <p>Is the Java version at least the requested version.</p>
     *
     * <p>Example input:</p>
     * <ul>
     *  <li><code>1.2f</code> to test for JDK 1.2
     *  <li><code>1.31f</code> to test for JDK 1.3.1
     * </ul>
     * 
     * @param requiredVersion  the required version, for example 1.31f
     * @return <code>true</code> if the actual version is equal or greater
     *  than the required version
     */
    public static boolean isJavaVersionAtLeast(float requiredVersion) {
        return (JAVA_VERSION_FLOAT >= requiredVersion);
    }
    
    /**
     * <p>Is the Java version at least the requested version.</p>
     *
     * <p>Example input:</p>
     * <ul>
     *  <li><code>120</code> to test for JDK 1.2 or greater
     *  <li><code>131</code> to test for JDK 1.3.1 or greater
     * </ul>
     * 
     * @param requiredVersion  the required version, for example 131
     * @return <code>true</code> if the actual version is equal or greater
     *  than the required version
     */
    public static boolean isJavaVersionAtLeast(int requiredVersion) {
        return (JAVA_VERSION_INT >= requiredVersion);
    }
    
    /**
     * <p>SystemUtils instances should NOT be constructed in standard
     * programming. Instead, the class should be used as
     * <code>SystemUtils.FILE_SEPARATOR</code>.</p>
     *
     * <p>This constructor is public to permit tools that require a JavaBean
     * instance to operate.</p>
     */
    public SystemUtils() {
    }
    
}
