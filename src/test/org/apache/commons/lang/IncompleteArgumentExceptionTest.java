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
 * @see IncompleteArgumentException
 */
public class IncompleteArgumentExceptionTest extends TestCase {

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
        return new TestSuite(IncompleteArgumentExceptionTest.class);
    }

    public IncompleteArgumentExceptionTest(String testName) {
        super(testName);
    }

    // testConstructor

    public void test1arg_nullInput() {
        final Throwable t = new IncompleteArgumentException(null);
        assertEquals("null is incomplete.", t.getMessage());
    }

    public void test1arg_validInput() {
        final String name = "argument";
        final Throwable t = new IncompleteArgumentException(name);
        assertEquals(name + " is incomplete.", t.getMessage());
    }

    public void test2arg_allNullInput() {
        final Throwable t = new IncompleteArgumentException(null, null);
        assertEquals(
            "null is missing the following items: null",
            t.getMessage());
    }

    public void test2arg_nullString() {
        final Throwable t =
            new IncompleteArgumentException(
                null,
                new String[] { "one", "two" });
        assertEquals(
            "null is missing the following items: [one, two]",
            t.getMessage());
    }

    public void test2arg_nullArray() {
        final String name = "one";
        final Throwable t = new IncompleteArgumentException(name, null);
        assertEquals(
            name + " is missing the following items: null",
            t.getMessage());
    }

    public void test2arg_validInput() {
        final String name = "input";
        final Throwable t =
            new IncompleteArgumentException(
                name,
                new String[] { "one", "two" });
        assertEquals(
            name + " is missing the following items: [one, two]",
            t.getMessage());
    }

} // IncompleteArgumentExceptionTest
