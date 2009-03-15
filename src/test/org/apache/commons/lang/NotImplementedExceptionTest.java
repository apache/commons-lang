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
package org.apache.commons.lang;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Constructor;

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
        assertEquals("Code is not implemented", ex.getMessage());
    }

    public void testConstructor_String1() {
        NotImplementedException ex = new NotImplementedException((String) null);
        assertEquals("Code is not implemented", ex.getMessage());
        assertEquals(null, ex.getCause());
        assertEquals("Code is not implemented", ex.getMessage());
    }        
    public void testConstructor_String2() {
        NotImplementedException ex = new NotImplementedException("msg");
        assertEquals("msg", ex.getMessage());
        assertEquals(null, ex.getCause());
        assertEquals("msg", ex.getMessage());
    }

    public void testConstructor_Throwable1() {
        NotImplementedException ex = new NotImplementedException((Throwable) null);
        assertEquals("Code is not implemented", ex.getMessage());
        assertEquals(null, ex.getCause());
        assertEquals("Code is not implemented", ex.getMessage());
    }        
    public void testConstructor_Throwable2() {
        Exception npe = new NullPointerException();
        NotImplementedException ex = new NotImplementedException(npe);
        assertEquals("Code is not implemented", ex.getMessage());
        assertSame(npe, ex.getCause());
        assertEquals("Code is not implemented", ex.getMessage());
    }

    public void testConstructor_StringThrowable1() {
        NotImplementedException ex = new NotImplementedException((String) null, (Throwable) null);
        assertEquals("Code is not implemented", ex.getMessage());
        assertEquals(null, ex.getCause());
        assertEquals("Code is not implemented", ex.getMessage());
    }
    public void testConstructor_StringThrowable2() {
        Exception npe = new NullPointerException();
        NotImplementedException ex = new NotImplementedException("msg", npe);
        assertEquals("msg", ex.getMessage());
        assertSame(npe, ex.getCause());
        assertEquals("msg", ex.getMessage());
    }

    public void testConstructor_Class1() {
        NotImplementedException ex = new NotImplementedException((Class) null);
        assertEquals("Code is not implemented", ex.getMessage());
        assertEquals(null, ex.getCause());
        assertEquals("Code is not implemented", ex.getMessage());
    }
    public void testConstructor_Class2() {
        NotImplementedException ex = new NotImplementedException(String.class);
        assertEquals("Code is not implemented in class java.lang.String", ex.getMessage());
        assertEquals(null, ex.getCause());
        assertEquals("Code is not implemented in class java.lang.String", ex.getMessage());
    }

    public void testPrintStackTrace() {
        NotImplementedException ex = new NotImplementedException(new Exception("nested 1", new RuntimeException("nested 2")));
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        PrintStream ps = new PrintStream(baos);
        PrintStream errStream = System.err;
        System.setErr(ps);
        ex.printStackTrace();
        System.setErr(errStream);
        assertTrue(baos.toString().length() > 0);
    }
    
    public void testPrintStackTrace_Stream() {
        NotImplementedException ex = new NotImplementedException(new Exception("nested 1", new RuntimeException("nested 2")));
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        PrintStream ps = new PrintStream(baos);
        ex.printStackTrace(ps);
        assertTrue(baos.toString().length() > 0);
    }
    
    public void testPrintStackTrace_Writer() {
        NotImplementedException ex = new NotImplementedException(new Exception("nested 1", new RuntimeException("nested 2")));
        StringWriter stringWriter = new StringWriter();
        PrintWriter writer = new PrintWriter(stringWriter);
        ex.printStackTrace(writer);
        assertTrue(stringWriter.toString().length() > 0);
    }
    
}
