/*
 * Copyright 2001-2004 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.commons.lang;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * Tests CharacterEncoding.
 * 
 * @see CharacterEncoding
 * @author Gary D. Gregory
 * @version $Id: CharacterEncodingTest.java,v 1.1 2004/08/03 17:17:08 ggregory Exp $
 */
public class CharacterEncodingTest extends TestCase {

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(CharacterEncodingTest.class);
        suite.setName("CharacterEncoding Tests");
        return suite;
    }

    private void assertSupportedEncoding(String name) {
        assertTrue("Encoding should be supported: " + name, CharacterEncoding.isSupported(name));
    }

    public void testMustBeSupportedJava1_3_1() {
        if (SystemUtils.isJavaVersionAtLeast(1.3f)) {
            this.assertSupportedEncoding(CharacterEncoding.ISO_8859_1);
            this.assertSupportedEncoding(CharacterEncoding.US_ASCII);
            this.assertSupportedEncoding(CharacterEncoding.UTF_16);
            this.assertSupportedEncoding(CharacterEncoding.UTF_16BE);
            this.assertSupportedEncoding(CharacterEncoding.UTF_16LE);
            this.assertSupportedEncoding(CharacterEncoding.UTF_8);
        } else {
            this.warn("Java 1.3 tests not run since the current version is " + SystemUtils.JAVA_VERSION);
        }
    }

    public void testNotSupported() {
        assertFalse(CharacterEncoding.isSupported(null));
        assertFalse(CharacterEncoding.isSupported(""));
        assertFalse(CharacterEncoding.isSupported(" "));
        assertFalse(CharacterEncoding.isSupported("\t\r\n"));
        assertFalse(CharacterEncoding.isSupported("DOESNOTEXIST"));
        assertFalse(CharacterEncoding.isSupported("this is not a valid encoding name"));
    }

    public void testWorksOnJava1_1_8() {
        //
        // In this test, I simply deleted the encodings from the 1.3.1 list.
        // The Javadoc do not specify which encodings are required.
        //
        if (SystemUtils.isJavaVersionAtLeast(1.1f)) {
            this.assertSupportedEncoding(CharacterEncoding.ISO_8859_1);
            this.assertSupportedEncoding(CharacterEncoding.US_ASCII);
            this.assertSupportedEncoding(CharacterEncoding.UTF_8);
        } else {
            this.warn("Java 1.1 tests not run since the current version is " + SystemUtils.JAVA_VERSION);
        }
    }

    public void testWorksOnJava1_2_2() {
        //
        // In this test, I simply deleted the encodings from the 1.3.1 list.
        // The Javadoc do not specify which encodings are required.
        //
        if (SystemUtils.isJavaVersionAtLeast(1.2f)) {
            this.assertSupportedEncoding(CharacterEncoding.ISO_8859_1);
            this.assertSupportedEncoding(CharacterEncoding.US_ASCII);
            this.assertSupportedEncoding(CharacterEncoding.UTF_8);
        } else {
            this.warn("Java 1.2 tests not run since the current version is " + SystemUtils.JAVA_VERSION);
        }
    }

    void warn(String msg) {
        System.err.println(msg);
    }
}