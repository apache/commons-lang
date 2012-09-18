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

import static org.apache.commons.lang3.JavaVersion.JAVA_1_1;
import static org.apache.commons.lang3.JavaVersion.JAVA_1_2;
import static org.apache.commons.lang3.JavaVersion.JAVA_1_3;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * Tests CharEncoding.
 * 
 * @see CharEncoding
 * @version $Id$
 */
public class CharEncodingTest  {

    private void assertSupportedEncoding(String name) {
        assertTrue("Encoding should be supported: " + name, CharEncoding.isSupported(name));
    }

    /**
     * The class can be instantiated.
     */
    @Test
    public void testConstructor() {
        new CharEncoding();
    }

    @Test
    public void testMustBeSupportedJava1_3_1() {
        if (SystemUtils.isJavaVersionAtLeast(JAVA_1_3)) {
            this.assertSupportedEncoding(CharEncoding.ISO_8859_1);
            this.assertSupportedEncoding(CharEncoding.US_ASCII);
            this.assertSupportedEncoding(CharEncoding.UTF_16);
            this.assertSupportedEncoding(CharEncoding.UTF_16BE);
            this.assertSupportedEncoding(CharEncoding.UTF_16LE);
            this.assertSupportedEncoding(CharEncoding.UTF_8);
        } else {
            this.warn("Java 1.3 tests not run since the current version is " + SystemUtils.JAVA_SPECIFICATION_VERSION);
        }
    }

    @Test
    public void testSupported() {
        assertTrue(CharEncoding.isSupported("UTF8"));
        assertTrue(CharEncoding.isSupported("UTF-8"));
        assertTrue(CharEncoding.isSupported("ASCII"));
    }

    @Test
    public void testNotSupported() {
        assertFalse(CharEncoding.isSupported(null));
        assertFalse(CharEncoding.isSupported(""));
        assertFalse(CharEncoding.isSupported(" "));
        assertFalse(CharEncoding.isSupported("\t\r\n"));
        assertFalse(CharEncoding.isSupported("DOESNOTEXIST"));
        assertFalse(CharEncoding.isSupported("this is not a valid encoding name"));
    }

    @Test
    public void testWorksOnJava1_1_8() {
        //
        // In this test, I simply deleted the encodings from the 1.3.1 list.
        // The Javadoc do not specify which encodings are required.
        //
        if (SystemUtils.isJavaVersionAtLeast(JAVA_1_1)) {
            this.assertSupportedEncoding(CharEncoding.ISO_8859_1);
            this.assertSupportedEncoding(CharEncoding.US_ASCII);
            this.assertSupportedEncoding(CharEncoding.UTF_8);
        } else {
            this.warn("Java 1.1 tests not run since the current version is " + SystemUtils.JAVA_SPECIFICATION_VERSION);
        }
    }

    @Test
    public void testWorksOnJava1_2_2() {
        //
        // In this test, I simply deleted the encodings from the 1.3.1 list.
        // The Javadoc do not specify which encodings are required.
        //
        if (SystemUtils.isJavaVersionAtLeast(JAVA_1_2)) {
            this.assertSupportedEncoding(CharEncoding.ISO_8859_1);
            this.assertSupportedEncoding(CharEncoding.US_ASCII);
            this.assertSupportedEncoding(CharEncoding.UTF_8);
        } else {
            this.warn("Java 1.2 tests not run since the current version is " + SystemUtils.JAVA_SPECIFICATION_VERSION);
        }
    }

    void warn(String msg) {
        System.err.println(msg);
    }
}
