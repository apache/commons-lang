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
 * <p>Common <code>System</code> class helpers.</p>
 * 
 * <p>If a particular system property cannot be read due to security
 * restrictions, the field will return <code>null</code>.</p>
 *
 * @author Based on code from Avalon Excalibur
 * @author Based on code from Lucene
 * @author Stephen Colebourne
 * @author <a href="mailto:sdowney@panix.com">Steve Downey</a>
 * @author <a href="mailto:ggregory@seagullsw.com">Gary Gregory</a>
 * @author Michael Becke
 * @since 1.0
 * @version $Id: SystemUtils.java,v 1.11 2003/05/24 12:06:37 scolebourne Exp $
 */
public class SystemUtils {
    
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

    //-----------------------------------------------------------------------
    /**
     * The <code>file.separator</code> System Property.
     * File separator ("/" on UNIX).
     * First in JDK version 1.1.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property.
     */
    public static final String FILE_SEPARATOR = getSystemProperty("file.separator");

    /**
     * The <code>java.class.path</code> System Property.
     * Java class path.
     * First in JDK version 1.1.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property.
     */
    public static final String JAVA_CLASS_PATH = getSystemProperty("java.class.path");

    /**
     * The <code>java.class.version</code> System Property.
     * Java class format version number.
     * First in JDK version 1.1.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property.
     */
    public static final String JAVA_CLASS_VERSION = getSystemProperty("java.class.version");

    /**
     * The <code>java.compiler</code> System Property.
     * Name of JIT compiler to use.
     * First in JDK version 1.4.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property.
     */
    public static final String JAVA_COMPILER = getSystemProperty("java.compiler");

    /**
     * The <code>java.ext.dirs</code> System Property.
     * Path of extension directory or directories.
     * First in JDK version 1.3.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property.
     */
    public static final String JAVA_EXT_DIRS = getSystemProperty("java.ext.dirs");

    /**
     * The <code>java.home</code> System Property.
     * Java installation directory.
     * First in JDK version 1.1.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property.
     */
    public static final String JAVA_HOME = getSystemProperty("java.home");

    /**
     * The <code>java.io.tmpdir</code> System Property.
     * Default temp file path.
     * First in JDK version 1.4.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property.
     */
    public static final String JAVA_IO_TMPDIR = getSystemProperty("java.io.tmpdir");

    /**
     * The <code>java.library.path</code> System Property.
     * List of paths to search when loading libraries.
     * First in JDK version 1.4.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property.
     */
    public static final String JAVA_LIBRARY_PATH = getSystemProperty("java.library.path");

    /**
     * The <code>java.specification.name</code> System Property.
     * Java Runtime Environment specification name.
     * First in JDK version 1.2.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property.
     */
    public static final String JAVA_SPECIFICATION_NAME = getSystemProperty("java.specification.name");

    /**
     * The <code>java.specification.vendor</code> System Property.
     * Java Runtime Environment specification vendor.
     * First in JDK version 1.2.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property.
     */
    public static final String JAVA_SPECIFICATION_VENDOR = getSystemProperty("java.specification.vendor");

    /**
     * The <code>java.specification.version</code> System Property.
     * Java Runtime Environment specification version.
     * First in JDK version 1.2.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property.
     */
    public static final String JAVA_SPECIFICATION_VERSION = getSystemProperty("java.specification.version");

    /**
     * The <code>java.vendor</code> System Property.
     * Java vendor-specific string.
     * First in JDK version 1.1.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property.
     */
    public static final String JAVA_VENDOR = getSystemProperty("java.vendor");

    /**
     * The <code>java.vendor.url</code> System Property.
     * Java vendor URL.
     * First in JDK version 1.1. 
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property.
     */
    public static final String JAVA_VENDOR_URL = getSystemProperty("java.vendor.url");

    /**
     * The <code>java.version</code> System Property.
     * Java version number.
     * First in JDK version 1.1. 
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property.
     */
    public static final String JAVA_VERSION = getSystemProperty("java.version");

    /**
     * The <code>java.vm.name</code> System Property.
     * Java Virtual Machine implementation name.
     * First in JDK version 1.2.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property.
     */
    public static final String JAVA_VM_NAME = getSystemProperty("java.vm.name");

    /**
     * The <code>java.vm.specification.name</code> System Property.
     * Java Virtual Machine specification name.
     * First in JDK version 1.2.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property.
     */
    public static final String JAVA_VM_SPECIFICATION_NAME = getSystemProperty("java.vm.specification.name");

    /**
     * The <code>java.vm.specification.vendor</code> System Property.
     * Java Virtual Machine specification vendor.
     * First in JDK version 1.2.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property.
     */
    public static final String JAVA_VM_SPECIFICATION_VENDOR = getSystemProperty("java.vm.specification.vendor");

    /**
     * The <code>java.vm.specification.version</code> System Property.
     * Java Virtual Machine specification version.
     * First in JDK version 1.2.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property.
     */
    public static final String JAVA_VM_SPECIFICATION_VERSION = getSystemProperty("java.vm.specification.version");

    /**
     * The <code>java.vm.vendor</code> System Property.
     * Java Virtual Machine implementation vendor.
     * First in JDK version 1.2.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property.
     */
    public static final String JAVA_VM_VENDOR = getSystemProperty("java.vm.vendor");

    /**
     * The <code>java.vm.version</code> System Property.
     * Java Virtual Machine implementation version.
     * First in JDK version 1.2.
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property.
     */
    public static final String JAVA_VM_VERSION = getSystemProperty("java.vm.version");

    /**
     * The <code>line.separator</code> System Property.
     * Line separator ("\n" on UNIX).
     * First in JDK version 1.1. 
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property.
     */
    public static final String LINE_SEPARATOR = getSystemProperty("line.separator");

    /**
     * The <code>os.arch</code> System Property.
     * Operating system architecture.
     * First in JDK version 1.1. 
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property.
     */
    public static final String OS_ARCH = getSystemProperty("os.arch");

    /**
     * The <code>os.name</code> System Property.
     * Operating system name.
     * First in JDK version 1.1. 
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property.
     */
    public static final String OS_NAME = getSystemProperty("os.name");

    /**
     * The <code>os.version</code> System Property.
     * Operating system version.
     * First in JDK version 1.1. 
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property.
     */
    public static final String OS_VERSION = getSystemProperty("os.version");

    /**
     * The <code>path.separator</code> System Property.
     * Path separator (":" on UNIX).
     * First in JDK version 1.1. 
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property.
     */
    public static final String PATH_SEPARATOR = getSystemProperty("path.separator");

    /**
     * The <code>user.dir</code> System Property.
     * User's current working directory.
     * First in JDK version 1.1. 
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property.
     */
    public static final String USER_DIR = getSystemProperty("user.dir");

    /**
     * The <code>user.home</code> System Property.
     * User's home directory.
     * First in JDK version 1.1. 
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property.
     */
    public static final String USER_HOME = getSystemProperty("user.home");

    /**
     * The <code>user.name</code> System Property.
     * User's account name.
     * First in JDK version 1.1. 
     * <p>
     * Defaults to <code>null</code> if the runtime does not have
     * security access to read this property.
     */
    public static final String USER_NAME = getSystemProperty("user.name");

    //-----------------------------------------------------------------------
    /**
     * Is <code>true</code> if this is Java version 1.1 (also 1.1.x versions).
     * <p>
     * The field will return false if <code>JAVA_VERSION</code> is <code>null</code>.
     */
    public static final boolean IS_JAVA_1_1 = getJavaVersionMatches("1.1");

    /**
     * Is <code>true</code> if this is Java version 1.2 (also 1.2.x versions).
     * <p>
     * The field will return false if <code>JAVA_VERSION</code> is <code>null</code>.
     */
    public static final boolean IS_JAVA_1_2 = getJavaVersionMatches("1.2");

    /**
     * Is <code>true</code> if this is Java version 1.3 (also 1.3.x versions).
     * <p>
     * The field will return false if <code>JAVA_VERSION</code> is <code>null</code>.
     */
    public static final boolean IS_JAVA_1_3 = getJavaVersionMatches("1.3");

    /**
     * Is <code>true</code> if this is Java version 1.4 (also 1.4.x versions).
     * <p>
     * The field will return false if <code>JAVA_VERSION</code> is <code>null</code>.
     */
    public static final boolean IS_JAVA_1_4 = getJavaVersionMatches("1.4");

    /**
     * Is <code>true</code> if this is Java version 1.5 (also 1.5.x versions).
     * <p>
     * The field will return false if <code>JAVA_VERSION</code> is <code>null</code>.
     */
    public static final boolean IS_JAVA_1_5 = getJavaVersionMatches("1.5");

    //-----------------------------------------------------------------------
    /**
     * Gets the Java version as a <code>float</code>.
     * Example output:
     * <ul>
     *  <li><code>1.2f</code> for JDK 1.2
     *  <li><code>1.31f</code> for JDK 1.3.1
     * </ul>
     * <p>
     * The field will return zero if <code>JAVA_VERSION</code> is <code>null</code>.
     */
    public static final float JAVA_VERSION_FLOAT = getJavaVersionAsFloat();

    /**
     * Gets the Java version as an <code>int</code>.
     * Example output:
     * <ul>
     *  <li><code>120</code> for JDK 1.2
     *  <li><code>131</code> for JDK 1.3.1
     * </ul>
     * <p>
     * The field will return zero if <code>JAVA_VERSION</code> is <code>null</code>.
     */
    public static final int JAVA_VERSION_INT = getJavaVersionAsInt();

    //-----------------------------------------------------------------------
    /**
     * Gets a System property, defaulting to the given value if the property 
     * cannot be read.
     * 
     * @param property the system property name
     * @return the system property value or <code>null</code> if security problem
     */
    private static String getSystemProperty(String property) {
        try {
            return System.getProperty(property);
            
        } catch (SecurityException ex) {
            // we are not allowed to look at this property
            System.err.println(
                "SecurityException thrown when reading system property '" + property 
                + "' - property value will default to null"
            );
            return null;
        }
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
     * <p>Get the Java version number as a <code>float</code>.</p>
     *
     * <p>Example output:</p>
     * <ul>
     *  <li><code>1.2f</code> for JDK 1.2
     *  <li><code>1.31f</code> for JDK 1.3.1
     * </ul>
     * 
     * <p>Patch releases are not reported.
     * Zero is returned if JAVA_VERSION is <code>null</code>.</p>
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
     * <p>Get the Java version number as an <code>int</code>.</p>
     *
     * <p>Example output:</p>
     * <ul>
     *  <li><code>120</code> for JDK 1.2
     *  <li><code>131</code> for JDK 1.3.1
     * </ul>
     * 
     * <p>Patch releases are not reported.
     * Zero is returned if JAVA_VERSION is <code>null</code>.</p>
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
    
    //-----------------------------------------------------------------------    
    // Parsing operating system may stay here, or it may be moved somewhere else entirely
//    /** True iff this is running on Windows */
//    public static final boolean IS_WINDOWS;
//    /** True iff this is running on Unix */
//    public static final boolean IS_UNIX;
//    /** True iff this is running on Mac */
//    public static final boolean IS_MAC;
//    /** True iff this is running on OS2 */
//    public static final boolean IS_OS2;
//    /** True iff this is running on Linux */
//    public static final boolean IS_LINUX;
//    
//    /*
//     * The JLS doesn't seem to specify an exact naming convention for the
//     * os.name. We ensure a uniform naming here.
//     */
//    static {
//        // from http://www.geocities.com/vamp201/os.html
//        if (OS_NAME.startsWith("Windows")) {
//            IS_WINDOWS = true;
//            IS_UNIX = false;
//            IS_MAC = false;
//            IS_OS2 = false;
//            IS_LINUX = false;
//        } else if (OS_NAME.startsWith("SunOS")) {
//            IS_WINDOWS = false;
//            IS_UNIX = true;
//            IS_MAC = false;
//            IS_OS2 = false;
//            IS_LINUX = false;
//        } else if (OS_NAME.startsWith("Solaris")) {
//            IS_WINDOWS = false;
//            IS_UNIX = true;
//            IS_MAC = false;
//            IS_OS2 = false;
//            IS_LINUX = false;
//        } else if (OS_NAME.startsWith("Linux")) {
//            IS_WINDOWS = false;
//            IS_UNIX = true;
//            IS_MAC = false;
//            IS_OS2 = false;
//            IS_LINUX = true;
//        } else if (OS_NAME.startsWith("HP-UX")) {
//            IS_WINDOWS = false;
//            IS_UNIX = true;
//            IS_MAC = false;
//            IS_OS2 = false;
//            IS_LINUX = false;
//        } else if (OS_NAME.startsWith("AIX")) {
//            IS_WINDOWS = false;
//            IS_UNIX = true;
//            IS_MAC = false;
//            IS_OS2 = false;
//            IS_LINUX = false;
//        } else if (OS_NAME.startsWith("Irix")) {
//            IS_WINDOWS = false;
//            IS_UNIX = true;
//            IS_MAC = false;
//            IS_OS2 = false;
//            IS_LINUX = false;
//        } else if (OS_NAME.startsWith("Digital Unix")) {
//            IS_WINDOWS = false;
//            IS_UNIX = true;
//            IS_MAC = false;
//            IS_OS2 = false;
//            IS_LINUX = false;
//        } else if (OS_NAME.startsWith("OS/400")) {
//            IS_WINDOWS = false;
//            IS_UNIX = true;
//            IS_MAC = false;
//            IS_OS2 = false;
//            IS_LINUX = false;
//        } else if (OS_NAME.startsWith("OS/2")) {
//            IS_WINDOWS = false;
//            IS_UNIX = false;
//            IS_MAC = false;
//            IS_OS2 = true;
//            IS_LINUX = false;
//        } else if (OS_NAME.startsWith("Mac OS X")) {
//            IS_WINDOWS = false;
//            IS_UNIX = true;
//            IS_MAC = true;
//            IS_OS2 = false;
//            IS_LINUX = false;
//        } else if (OS_NAME.startsWith("Mac")) {
//            IS_WINDOWS = false;
//            IS_UNIX = false;
//            IS_MAC = true;
//            IS_OS2 = false;
//            IS_LINUX = false;
//        } else {
//            IS_WINDOWS = false;
//            IS_UNIX = false;
//            IS_MAC = false;
//            IS_OS2 = false;
//            IS_LINUX = false;
//        }
//    }

    //-----------------------------------------------------------------------
    /**
     * <p>Get the Java version number as a <code>float</code>.</p>
     *
     * <P>Example output:</p>
     * <ul>
     *  <li><code>1.2f</code> for JDK 1.2
     *  <li><code>1.31f</code> for JDK 1.3.1
     * </ul>
     * 
     * @deprecated Use JAVA_VERSION_FLOAT instead.
     *             Will be removed in Commons Lang 3.0.
     * @return the version, for example 1.31f for JDK 1.3.1
     */
    public static float getJavaVersion() {
        return JAVA_VERSION_FLOAT;
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
    
}
