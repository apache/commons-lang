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
 * @author Stephen Colebourne
 * @version $Id$
 * @see NullArgumentException
 */
public class NullArgumentExceptionTest extends TestCase {

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
        return new TestSuite(NullArgumentExceptionTest.class);
    }

    public NullArgumentExceptionTest(String testName) {
        super(testName);
    }

    // testConstructor

    public void testConstructor_nullInput() {
        new NullArgumentException(null);
    }

    // testGetMessage

    public void testGetMessage_nullConstructorInput() {
        final Throwable t = new NullArgumentException(null);
        assertEquals("Argument must not be null.", t.getMessage());
    }

    public void testGetMessage_validConstructorInput() {
        final String argName = "name";
        final Throwable t = new NullArgumentException(argName);
        assertEquals(argName + " must not be null.", t.getMessage());
    }

} // NullArgumentExceptionTest

