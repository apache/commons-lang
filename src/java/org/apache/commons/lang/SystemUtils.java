package org.apache.commons.lang;

/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002 The Apache Software Foundation.  All rights
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

/**
 * Common <code>System</code> class helpers.
 *
 * @author Based on code from Avalon Excalibur
 * @author Based on code from Lucene
 * @author <a href="mailto:scolebourne@apache.org">Stephen Colebourne</a>
 * @author <a href="mailto:sdowney@panix.com">Steve Downey</a>
 * @version $Id: SystemUtils.java,v 1.2.2.1 2002/11/22 23:31:13 bayard Exp $
 */
public class SystemUtils {
    
    /**
     * SystemUtils instances should NOT be constructed in standard programming.
     * Instead, the class should be used as <code>SystemUtils.FILE_SEPARATOR</code>.
     * This constructor is public to permit tools that require a JavaBean instance
     * to operate.
     */
    public SystemUtils() {
    }

    /**
     * System Property: file.separator
     * File separator ("/" on UNIX).
     * First in version: 1.1
     */
    public static final String FILE_SEPARATOR = System.getProperty("file.separator");
    

    /**
     * System Property: java.class.path
     * Java class path.
     * First in version: 1.1
     */
    public static final String JAVA_CLASS_PATH = System.getProperty("java.class.path");
    

    /**
     * System Property: java.class.version
     * Java class format version number.
     * First in version: 1.1
     */
    public static final String JAVA_CLASS_VERSION = System.getProperty("java.class.version");
    

    /**
     * System Property: java.compiler
     * Name of JIT compiler to use.
     * First in version: 1.4 
     */
    public static final String JAVA_COMPILER = System.getProperty("java.compiler");
    

    /**
     * System Property: java.ext.dirs
     * Path of extension directory or directories.
     * First in version: 1.3 
     */
    public static final String JAVA_EXT_DIRS = System.getProperty("java.ext.dirs");
    

    /**
     * System Property: java.home
     * Java installation directory.
     * First in version: 1.1 
     */
    public static final String JAVA_HOME = System.getProperty("java.home");
    

    /**
     * System Property: java.io.tmpdir
     * Default temp file path.
     * First in version: 1.4 
     */
    public static final String JAVA_IO_TMPDIR = System.getProperty("java.io.tmpdir");
    

    /**
     * System Property: java.library.path
     * List of paths to search when loading libraries.
     * First in version: 1.4 
     */
    public static final String JAVA_LIBRARY_PATH = System.getProperty("java.library.path");
    

    /**
     * System Property: java.specification.name
     * Java Runtime Environment specification name.
     * First in version: 1.2 
     */
    public static final String JAVA_SPECIFICATION_NAME = System.getProperty("java.specification.name");
    

    /**
     * System Property: java.specification.vendor
     * Java Runtime Environment specification vendor.
     * First in version: 1.2 
     */
    public static final String JAVA_SPECIFICATION_VENDOR = System.getProperty("java.specification.vendor");
    

    /**
     * System Property: java.specification.version
     * Java Runtime Environment specification version.
     * First in version: 1.2 
     */
    public static final String JAVA_SPECIFICATION_VERSION = System.getProperty("java.specification.version");
    

    /**
     * System Property: java.vendor
     * Java vendor-specific string.
     * First in version: 1.1 
     */
    public static final String JAVA_VENDOR = System.getProperty("java.vendor");
    

    /**
     * System Property: java.vendor.url
     * Java vendor URL.
     * First in version: 1.1 
     */
    public static final String JAVA_VENDOR_URL = System.getProperty("java.vendor.url");
    

    /**
     * System Property: java.version
     * Java version number.
     * First in version: 1.1 
     */
    public static final String JAVA_VERSION = System.getProperty("java.version");
    

    /**
     * System Property: java.vm.name
     * Java Virtual Machine implementation name.
     * First in version: 1.2 
     */
    public static final String JAVA_VM_NAME = System.getProperty("java.vm.name");
    

    /**
     * System Property: java.vm.specification.name
     * Java Virtual Machine specification name.
     * First in version: 1.2 
     */
    public static final String JAVA_VM_SPECIFICATION_NAME = System.getProperty("java.vm.specification.name");
    

    /**
     * System Property: java.vm.specification.vendor
     * Java Virtual Machine specification vendor.
     * First in version: 1.2 
     */
    public static final String JAVA_VM_SPECIFICATION_VENDOR = System.getProperty("java.vm.specification.vendor");
    

    /**
     * System Property: java.vm.specification.version
     * Java Virtual Machine specification version.
     * First in version: 1.2 
     */
    public static final String JAVA_VM_SPECIFICATION_VERSION = System.getProperty("java.vm.specification.version");
    

    /**
     * System Property: java.vm.vendor
     * Java Virtual Machine implementation vendor.
     * First in version: 1.2 
     */
    public static final String JAVA_VM_VENDOR = System.getProperty("java.vm.vendor");
    

    /**
     * System Property: java.vm.version
     * Java Virtual Machine implementation version.
     * First in version: 1.2 
     */
    public static final String JAVA_VM_VERSION = System.getProperty("java.vm.version");
    

    /**
     * System Property: line.separator
     * Line separator ("\n" on UNIX).
     * First in version: 1.1 
     */
    public static final String LINE_SEPARATOR = System.getProperty("line.separator");
    

    /**
     * System Property: os.arch
     * Operating system architecture.
     * First in version: 1.1 
     */
    public static final String OS_ARCH = System.getProperty("os.arch");
    

    /**
     * System Property: os.name
     * Operating system name.
     * First in version: 1.1 
     */
    public static final String OS_NAME = System.getProperty("os.name");
    

    /**
     * System Property: os.version
     * Operating system version.
     * First in version: 1.1 
     */
    public static final String OS_VERSION = System.getProperty("os.version");
    

    /**
     * System Property: path.separator
     * Path separator (":" on UNIX).
     * First in version: 1.1 
     */
    public static final String PATH_SEPARATOR = System.getProperty("path.separator");
    

    /**
     * System Property: user.dir
     * User's current working directory.
     * First in version: 1.1 
     */
    public static final String USER_DIR = System.getProperty("user.dir");
    

    /**
     * System Property: user.home
     * User's home directory.
     * First in version: 1.1 
     */
    public static final String USER_HOME = System.getProperty("user.home");
    

    /**
     * System Property: user.name
     * User's account name.
     * First in version: 1.1 
     */
    public static final String USER_NAME = System.getProperty("user.name");
    
    /** True iff this is Java version 1.1. */
    public static final boolean IS_JAVA_1_1 = JAVA_VERSION.startsWith("1.1.");
    /** True iff this is Java version 1.2. */
    public static final boolean IS_JAVA_1_2 = JAVA_VERSION.startsWith("1.2.");
    /** True iff this is Java version 1.3. */
    public static final boolean IS_JAVA_1_3 = JAVA_VERSION.startsWith("1.3.");
    /** True iff this is Java version 1.4. */
    public static final boolean IS_JAVA_1_4 = JAVA_VERSION.startsWith("1.4.");
    /** True iff this is Java version 1.3. */
    public static final boolean IS_JAVA_1_5 = JAVA_VERSION.startsWith("1.5.");


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

    /**
     * Get the Java version number as a float.
     * Example output:<br>
     * 1.2f  for JDK 1.2<br>
     * 1.31f  for JDK 1.3.1<br>
     * 
     * @return the version, for example 1.31f for JDK 1.3.1
     */
    public static float getJavaVersion() {
        String str = JAVA_VERSION.substring(0, 3);
        if (JAVA_VERSION.length() >= 5) {
            str = str + JAVA_VERSION.substring(4, 5);
        }
        return Float.parseFloat(str);
    }
    
    /**
     * Is the Java version at the the requested version.
     * Example input:<br>
     * 1.2f  for JDK 1.2<br>
     * 1.31f  for JDK 1.3.1<br>
     * 
     * @param requiredVersion  the required version, for example 1.31f
     * @return true if the actual version is equal or greater than the required version
     */
    public static boolean isJavaVersionAtLeast(float requiredVersion) {
        return (getJavaVersion() >= requiredVersion);
    }
    
}
