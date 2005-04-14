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
import org.apache.commons.lang.exception.Nestable;

/**
 * JUnit tests.
 * 
 * @author Matthew Hawthorne
 * @version $Id$
 * @see UnhandledException
 */
public class UnhandledExceptionTest extends TestCase {

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
        return new TestSuite(UnhandledExceptionTest.class);
    }

    public UnhandledExceptionTest(String testName) {
        super(testName);
    }

    // testConstructor

    public void testConstructor_throwable_nullInput() {
        final Throwable t = null;
        new UnhandledException(t);
    }

    public void testConstructor_stringAndThrowable_nullInput() {
        new UnhandledException(null, null);
    }

    // testGetCause

    public void testGetCause() {
        final Throwable t = new NullPointerException();
        final Nestable n = new UnhandledException(t);
        assertEquals(t, n.getCause());
    }

    public void testGetCauseAndGetMessage() {
        final Throwable t = new NullPointerException();
        final String msg = "nullArg";
        final Nestable n = new UnhandledException(msg, t);
        assertEquals(t, n.getCause());
        assertEquals(msg, n.getMessage());
    }

} // UnhandledExceptionTest
