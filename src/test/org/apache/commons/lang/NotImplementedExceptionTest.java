/*
 * Copyright 2002-2004 The Apache Software Foundation.
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
 * JUnit tests.
 * 
 * @author Matthew Hawthorne
 * @version $Id: NotImplementedExceptionTest.java,v 1.3 2004/02/18 23:06:19 ggregory Exp $
 * @see NotImplementedException
 */
public class NotImplementedExceptionTest extends TestCase {

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
        return new TestSuite(NotImplementedExceptionTest.class);
    }

    public NotImplementedExceptionTest(String testName) {
        super(testName);
    }

    // testConstructor

    public void testConstructor_classArg_nullInput() {
        final Class c = null;
        new NotImplementedException(c);
    }

    public void testConstructor_stringArg_nullInput() {
        final String s = null;
        new NotImplementedException(s);
    }

    // testGetMessage

    public void testGetMessage_classArg_nullInput() {
        final Class c = null;
        final Throwable t = new NotImplementedException(c);
        assertEquals("Method is not implemented in class null", t.getMessage());
    }

    public void testGetMessage_classArg_validInput() {
        final Throwable t = new NotImplementedException(String.class);
        assertEquals(
            "Method is not implemented in class java.lang.String",
            t.getMessage());
    }

    public void testGetMessage_stringArg_nullInput() {
        final String s = null;
        final Throwable t = new NotImplementedException(s);
        assertEquals(null, t.getMessage());
    }

    public void testGetMessage_stringArg_validInput() {
        final String msg = "message";
        final Throwable t = new NotImplementedException(msg);
        assertEquals(msg, t.getMessage());
    }

} // NotImplementedExceptionTest
