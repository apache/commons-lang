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
 * @version $Id: SystemUtils.java,v 1.1 2002/08/22 22:11:25 scolebourne Exp $
 */
public class SystemUtils {
    
    /**
     * Prevent construction of SystemUtils instances
     */
    private SystemUtils() {
    }

    /** The line separator string from System.getProperty() */
    public static final String LINE_SEPARATOR = System.getProperty("line.separator");
    /** The path separator string from System.getProperty() */
    public static final String PATH_SEPARATOR = System.getProperty("path.separator");
    /** The file separator string from System.getProperty() */
    public static final String FILE_SEPARATOR = System.getProperty("file.separator");
    
    /** The working (user) directory string from System.getProperty() */
    public static final String USER_WORKING_DIRECTORY = System.getProperty("user.dir");
    /** The user home directory string from System.getProperty() */
    public static final String USER_HOME_DIRECTORY = System.getProperty("user.home");
    /** The user account name string from System.getProperty() */
    public static final String USER_NAME = System.getProperty("user.name");
    
    /** The os name string from System.getProperty() */
    public static final String OS_NAME = System.getProperty("os.name");
    /** The os architecture string from System.getProperty() */
    public static final String OS_ARCHITECTURE = System.getProperty("os.arch");
    /** The os version string from System.getProperty() */
    public static final String OS_VERSION = System.getProperty("os.version");
    /** True iff this is running on Windows */
    public static final boolean IS_WINDOWS;
    /** True iff this is running on Unix */
    public static final boolean IS_UNIX;
    /** True iff this is running on Mac */
    public static final boolean IS_MAC;
    /** True iff this is running on OS2 */
    public static final boolean IS_OS2;
    /** True iff this is running on Linux */
    public static final boolean IS_LINUX;
    
    /** The Java vendor string from System.getProperty() */
    public static final String JAVA_VENDOR = System.getProperty("java.vendor");
    /** The Java vendor url string from System.getProperty() */
    public static final String JAVA_VENDOR_URL = System.getProperty("java.vendor.url");
    /** The Java installation directory string from System.getProperty() */
    public static final String JAVA_HOME = System.getProperty("java.home");
    /** The Java class version number string from System.getProperty() */
    public static final String JAVA_CLASS_VERSION = System.getProperty("java.class.version");
    /** The Java classpath string from System.getProperty() */
    public static final String JAVA_CLASS_PATH = System.getProperty("java.class.path");
    
    /** The Java version string from System.getProperty() */
    public static final String JAVA_VERSION = System.getProperty("java.version");
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

    /*
     * The JLS doesn't seem to specify an exact naming convention for the
     * os.name. We ensure a uniform naming here.
     */
    static {
        // from http://www.geocities.com/vamp201/os.html
        if (OS_NAME.startsWith("Windows")) {
            IS_WINDOWS = true;
            IS_UNIX = false;
            IS_MAC = false;
            IS_OS2 = false;
            IS_LINUX = false;
        } else if (OS_NAME.startsWith("SunOS")) {
            IS_WINDOWS = false;
            IS_UNIX = true;
            IS_MAC = false;
            IS_OS2 = false;
            IS_LINUX = false;
        } else if (OS_NAME.startsWith("Solaris")) {
            IS_WINDOWS = false;
            IS_UNIX = true;
            IS_MAC = false;
            IS_OS2 = false;
            IS_LINUX = false;
        } else if (OS_NAME.startsWith("Linux")) {
            IS_WINDOWS = false;
            IS_UNIX = true;
            IS_MAC = false;
            IS_OS2 = false;
            IS_LINUX = true;
        } else if (OS_NAME.startsWith("HP-UX")) {
            IS_WINDOWS = false;
            IS_UNIX = true;
            IS_MAC = false;
            IS_OS2 = false;
            IS_LINUX = false;
        } else if (OS_NAME.startsWith("AIX")) {
            IS_WINDOWS = false;
            IS_UNIX = true;
            IS_MAC = false;
            IS_OS2 = false;
            IS_LINUX = false;
        } else if (OS_NAME.startsWith("Irix")) {
            IS_WINDOWS = false;
            IS_UNIX = true;
            IS_MAC = false;
            IS_OS2 = false;
            IS_LINUX = false;
        } else if (OS_NAME.startsWith("Digital Unix")) {
            IS_WINDOWS = false;
            IS_UNIX = true;
            IS_MAC = false;
            IS_OS2 = false;
            IS_LINUX = false;
        } else if (OS_NAME.startsWith("OS/2")) {
            IS_WINDOWS = false;
            IS_UNIX = false;
            IS_MAC = false;
            IS_OS2 = true;
            IS_LINUX = false;
        } else if (OS_NAME.startsWith("Mac")) {
            IS_WINDOWS = false;
            IS_UNIX = false;
            IS_MAC = true;
            IS_OS2 = false;
            IS_LINUX = false;
        } else {
            IS_WINDOWS = false;
            IS_UNIX = false;
            IS_MAC = false;
            IS_OS2 = false;
            IS_LINUX = false;
        }
    }

}
