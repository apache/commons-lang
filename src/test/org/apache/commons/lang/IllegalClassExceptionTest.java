/*
 * Copyright 2002-2005 The Apache Software Foundation.
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
 * @version $Id$
 * @see IllegalClassException
 */
public class IllegalClassExceptionTest extends TestCase {

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
        return new TestSuite(IllegalClassExceptionTest.class);
    }

    public IllegalClassExceptionTest(String testName) {
        super(testName);
    }

    // testConstructor_classArgs

    public void testConstructor_classArgs_allNullInput() {
        new IllegalClassException(null, null);
    }

    public void testConstructor_classArgs_nullExpected() {
        new IllegalClassException(null, String.class);
    }

    public void testConstructor_classArgs_nullActual() {
        new IllegalClassException(String.class, null);
    }

    //  testConstructor_stringArg

    public void testConstructor_stringArg_nullInput() {
        new IllegalClassException(null);
    }

    // testConstructor_classObjectArgs

    public void testConstructor_classObjectArgs_allNullInput() {
        new IllegalClassException(null, (Object) null);
    }

    public void testConstructor_classObjectArgs_nullExpected() {
        new IllegalClassException(null, new Object());
    }

    public void testConstructor_classObjectArgs_nullActual() {
        new IllegalClassException(String.class, (Object) null);
    }

    // testGetMessage

    public void testGetMessage_classArgs_nullInput() {
        final Throwable t = new IllegalClassException(null, null);
        assertEquals("Expected: null, actual: null", t.getMessage());
    }

    public void testGetMessage_classArgs_normalInput() {
        final Throwable t =
            new IllegalClassException(String.class, Integer.class);
        assertEquals(
            "Expected: java.lang.String, actual: java.lang.Integer",
            t.getMessage());
    }

    public void testGetMessage_classObjectArgs_nullInput() {
        final Throwable t = new IllegalClassException(null, (Object) null);
        assertEquals("Expected: null, actual: null", t.getMessage());
    }

    public void testGetMessage_classObjectArgs_normalInput() {
        final Throwable t =
            new IllegalClassException(String.class, new Object());
        assertEquals(
            "Expected: java.lang.String, actual: java.lang.Object",
            t.getMessage());
    }

    public void testGetMessage_stringArg_nullInput() {
        final Throwable t = new IllegalClassException(null);
        assertEquals(null, t.getMessage());
    }

    public void testGetMessage_stringArg_validInput() {
        final String message = "message";
        final Throwable t = new IllegalClassException(message);
        assertEquals(message, t.getMessage());
    }

} // IllegalClassExceptionTest
