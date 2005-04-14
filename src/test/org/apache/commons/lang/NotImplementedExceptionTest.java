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

    //-----------------------------------------------------------------------
    public void testConstructor_() {
        NotImplementedException ex = new NotImplementedException();
        assertEquals("Code is not implemented", ex.getMessage());
        assertEquals(null, ex.getCause());
    }

    public void testConstructor_String1() {
        NotImplementedException ex = new NotImplementedException((String) null);
        assertEquals("Code is not implemented", ex.getMessage());
        assertEquals(null, ex.getCause());
    }        
    public void testConstructor_String2() {
        NotImplementedException ex = new NotImplementedException("msg");
        assertEquals("msg", ex.getMessage());
        assertEquals(null, ex.getCause());
    }

    public void testConstructor_Throwable1() {
        NotImplementedException ex = new NotImplementedException((Throwable) null);
        assertEquals("Code is not implemented", ex.getMessage());
        assertEquals(null, ex.getCause());
    }        
    public void testConstructor_Throwable2() {
        Exception npe = new NullPointerException();
        NotImplementedException ex = new NotImplementedException(npe);
        assertEquals("Code is not implemented", ex.getMessage());
        assertSame(npe, ex.getCause());
    }

    public void testConstructor_StringThrowable1() {
        NotImplementedException ex = new NotImplementedException((String) null, (Throwable) null);
        assertEquals("Code is not implemented", ex.getMessage());
        assertEquals(null, ex.getCause());
    }
    public void testConstructor_StringThrowable2() {
        Exception npe = new NullPointerException();
        NotImplementedException ex = new NotImplementedException("msg", npe);
        assertEquals("msg", ex.getMessage());
        assertSame(npe, ex.getCause());
    }

    public void testConstructor_Class1() {
        NotImplementedException ex = new NotImplementedException((Class) null);
        assertEquals("Code is not implemented", ex.getMessage());
        assertEquals(null, ex.getCause());
    }
    public void testConstructor_Class2() {
        NotImplementedException ex = new NotImplementedException(String.class);
        assertEquals("Code is not implemented in class java.lang.String", ex.getMessage());
        assertEquals(null, ex.getCause());
    }

}
