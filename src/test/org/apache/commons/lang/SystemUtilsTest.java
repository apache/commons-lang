/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2003 The Apache Software Foundation.  All rights
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

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * Unit tests {@link org.apache.commons.lang.SystemUtils}.
 * 
 * Only limited testing can be performed.
 *
 * @author Stephen Colebourne
 * @version $Id: SystemUtilsTest.java,v 1.2 2003/05/24 14:34:14 scolebourne Exp $
 */
public class SystemUtilsTest extends TestCase {

    public SystemUtilsTest(String name) {
        super(name);
    }

    public static void main(String[] args) {
        TestRunner.run(suite());
//        SystemUtils s = new SystemUtils();
//        System.out.println(s.IS_OS_WINDOWS);
//        System.out.println(s.IS_OS_WINDOWS_95);
//        System.out.println(s.IS_OS_WINDOWS_98);
//        java.util.Properties p = System.getProperties();
//        java.util.Enumeration keys = p.keys();
//        java.util.List list = new java.util.ArrayList();
//        while( keys.hasMoreElements() ) {
//            list.add(keys.nextElement());
//        }
//        java.util.Collections.sort(list);
//        for (java.util.Iterator it = list.iterator(); it.hasNext();) {
//            String key = (String) it.next();
//            System.out.println(key + " " + p.getProperty(key));
//        }
    
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(SystemUtilsTest.class);
        suite.setName("SystemUtils Tests");
        return suite;
    }

    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    //-----------------------------------------------------------------------
    // COPIED FROM SystemUtils
    //-----------------------------------------------------------------------
    private String JAVA_VERSION;
    private String OS_NAME;
    private String OS_VERSION;
    
    /**
     * Decides if the java version matches.
     * 
     * @param versionPrefix  the prefix for the java version
     * @return true if matches, or false if not or can't determine
     */
    private boolean getJavaVersionMatches(String versionPrefix) {
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
    private float getJavaVersionAsFloat() {
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
    private int getJavaVersionAsInt() {
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
     * Decides if the operating system matches.
     * 
     * @param osNamePrefix  the prefix for the os name
     * @return true if matches, or false if not or can't determine
     */
    private boolean getOSMatches(String osNamePrefix) {
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
    private boolean getOSMatches(String osNamePrefix, String osVersionPrefix) {
        if (OS_NAME == null || OS_VERSION == null) {
            return false;
        }
        return OS_NAME.startsWith(osNamePrefix) && OS_VERSION.startsWith(osVersionPrefix);
    }    

    //-----------------------------------------------------------------------
    public void testJavaVersionMatches() {
        JAVA_VERSION = null;
        assertEquals(false, getJavaVersionMatches("1.1"));
        assertEquals(false, getJavaVersionMatches("1.2"));
        assertEquals(false, getJavaVersionMatches("1.3"));
        assertEquals(false, getJavaVersionMatches("1.4"));
        assertEquals(false, getJavaVersionMatches("1.5"));
        JAVA_VERSION = "1.1";
        assertEquals(true, getJavaVersionMatches("1.1"));
        assertEquals(false, getJavaVersionMatches("1.2"));
        assertEquals(false, getJavaVersionMatches("1.3"));
        assertEquals(false, getJavaVersionMatches("1.4"));
        assertEquals(false, getJavaVersionMatches("1.5"));
        JAVA_VERSION = "1.2";
        assertEquals(false, getJavaVersionMatches("1.1"));
        assertEquals(true, getJavaVersionMatches("1.2"));
        assertEquals(false, getJavaVersionMatches("1.3"));
        assertEquals(false, getJavaVersionMatches("1.4"));
        assertEquals(false, getJavaVersionMatches("1.5"));
        JAVA_VERSION = "1.3.0";
        assertEquals(false, getJavaVersionMatches("1.1"));
        assertEquals(false, getJavaVersionMatches("1.2"));
        assertEquals(true, getJavaVersionMatches("1.3"));
        assertEquals(false, getJavaVersionMatches("1.4"));
        assertEquals(false, getJavaVersionMatches("1.5"));
        JAVA_VERSION = "1.3.1";
        assertEquals(false, getJavaVersionMatches("1.1"));
        assertEquals(false, getJavaVersionMatches("1.2"));
        assertEquals(true, getJavaVersionMatches("1.3"));
        assertEquals(false, getJavaVersionMatches("1.4"));
        assertEquals(false, getJavaVersionMatches("1.5"));
        JAVA_VERSION = "1.4.0";
        assertEquals(false, getJavaVersionMatches("1.1"));
        assertEquals(false, getJavaVersionMatches("1.2"));
        assertEquals(false, getJavaVersionMatches("1.3"));
        assertEquals(true, getJavaVersionMatches("1.4"));
        assertEquals(false, getJavaVersionMatches("1.5"));
        JAVA_VERSION = "1.4.1";
        assertEquals(false, getJavaVersionMatches("1.1"));
        assertEquals(false, getJavaVersionMatches("1.2"));
        assertEquals(false, getJavaVersionMatches("1.3"));
        assertEquals(true, getJavaVersionMatches("1.4"));
        assertEquals(false, getJavaVersionMatches("1.5"));
        JAVA_VERSION = "1.5.0";
        assertEquals(false, getJavaVersionMatches("1.1"));
        assertEquals(false, getJavaVersionMatches("1.2"));
        assertEquals(false, getJavaVersionMatches("1.3"));
        assertEquals(false, getJavaVersionMatches("1.4"));
        assertEquals(true, getJavaVersionMatches("1.5"));
        JAVA_VERSION = "1.6.0";
        assertEquals(false, getJavaVersionMatches("1.1"));
        assertEquals(false, getJavaVersionMatches("1.2"));
        assertEquals(false, getJavaVersionMatches("1.3"));
        assertEquals(false, getJavaVersionMatches("1.4"));
        assertEquals(false, getJavaVersionMatches("1.5"));
    }
   
    public void testJavaVersionAsFloat() {
        JAVA_VERSION = null;
        assertEquals(0f, getJavaVersionAsFloat(), 0.000001f);
        JAVA_VERSION = "1.1";
        assertEquals(1.1f, getJavaVersionAsFloat(), 0.000001f);
        JAVA_VERSION = "1.2";
        assertEquals(1.2f, getJavaVersionAsFloat(), 0.000001f);
        JAVA_VERSION = "1.3.0";
        assertEquals(1.3f, getJavaVersionAsFloat(), 0.000001f);
        JAVA_VERSION = "1.3.1";
        assertEquals(1.31f, getJavaVersionAsFloat(), 0.000001f);
        JAVA_VERSION = "1.4.0";
        assertEquals(1.4f, getJavaVersionAsFloat(), 0.000001f);
        JAVA_VERSION = "1.4.1";
        assertEquals(1.41f, getJavaVersionAsFloat(), 0.000001f);
        JAVA_VERSION = "1.5.0";
        assertEquals(1.5f, getJavaVersionAsFloat(), 0.000001f);
        JAVA_VERSION = "1.6.0";
        assertEquals(1.6f, getJavaVersionAsFloat(), 0.000001f);
    }
    
    public void testJavaVersionAsInt() {
        JAVA_VERSION = null;
        assertEquals(0, getJavaVersionAsInt());
        JAVA_VERSION = "1.1";
        assertEquals(110, getJavaVersionAsInt());
        JAVA_VERSION = "1.2";
        assertEquals(120, getJavaVersionAsInt());
        JAVA_VERSION = "1.3.0";
        assertEquals(130, getJavaVersionAsInt());
        JAVA_VERSION = "1.3.1";
        assertEquals(131, getJavaVersionAsInt());
        JAVA_VERSION = "1.4.0";
        assertEquals(140, getJavaVersionAsInt());
        JAVA_VERSION = "1.4.1";
        assertEquals(141, getJavaVersionAsInt());
        JAVA_VERSION = "1.5.0";
        assertEquals(150, getJavaVersionAsInt());
        JAVA_VERSION = "1.6.0";
        assertEquals(160, getJavaVersionAsInt());
    }
    
    public void testJavaVersionAtLeastFloat() {
        float version = SystemUtils.JAVA_VERSION_FLOAT;
        assertEquals(true, SystemUtils.isJavaVersionAtLeast(version));
        version -= 0.1f;
        assertEquals(true, SystemUtils.isJavaVersionAtLeast(version));
        version += 0.2f;
        assertEquals(false, SystemUtils.isJavaVersionAtLeast(version));
    }
    
    public void testJavaVersionAtLeastInt() {
        int version = SystemUtils.JAVA_VERSION_INT;
        assertEquals(true, SystemUtils.isJavaVersionAtLeast(version));
        version -= 10;
        assertEquals(true, SystemUtils.isJavaVersionAtLeast(version));
        version += 20;
        assertEquals(false, SystemUtils.isJavaVersionAtLeast(version));
    }
    
    public void testOSMatches() {
        OS_NAME = null;
        assertEquals(false, getOSMatches("Windows"));
        OS_NAME = "Windows 95";
        assertEquals(true, getOSMatches("Windows"));
        OS_NAME = "Windows NT";
        assertEquals(true, getOSMatches("Windows"));
        OS_NAME = "OS/2";
        assertEquals(false, getOSMatches("Windows"));
    }
    
    public void testOSMatches2() {
        OS_NAME = null;
        OS_VERSION = null;
        assertEquals(false, getOSMatches("Windows 9", "4.1"));
        OS_NAME = "Windows 95";
        OS_VERSION = "4.0";
        assertEquals(false, getOSMatches("Windows 9", "4.1"));
        OS_NAME = "Windows 95";
        OS_VERSION = "4.1";
        assertEquals(true, getOSMatches("Windows 9", "4.1"));
        OS_NAME = "Windows 98";
        OS_VERSION = "4.1";
        assertEquals(true, getOSMatches("Windows 9", "4.1"));
        OS_NAME = "Windows NT";
        OS_VERSION = "4.0";
        assertEquals(false, getOSMatches("Windows 9", "4.1"));
        OS_NAME = "OS/2";
        OS_VERSION = "4.0";
        assertEquals(false, getOSMatches("Windows 9", "4.1"));
    }
    
}
