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

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Constructor;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

import org.apache.commons.lang.exception.NestableException;

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

    public void testGetMessage_Indexed() throws Exception {
        if (SystemUtils.isJavaVersionAtLeast(1.4f)) {
            Exception ex1 = new Exception("nested 2");
            Constructor con = Exception.class.getConstructor(new Class[] {String.class, Throwable.class});
            Exception ex2 = (Exception) con.newInstance(new Object[] {"nested 1", ex1});
            NotImplementedException ex = new NotImplementedException(ex2);
            assertEquals("Code is not implemented", ex.getMessage());
            assertEquals("Code is not implemented", ex.getMessage(0));
            assertEquals("nested 1", ex.getMessage(1));
            assertEquals("nested 2", ex.getMessage(2));
            
            String[] messages = ex.getMessages();
            assertEquals(3, messages.length);
            assertEquals("Code is not implemented", messages[0]);
            assertEquals("nested 1", messages[1]);
            assertEquals("nested 2", messages[2]);
        }
    }
    
    public void testGetThrowable() {
        NotImplementedException ex = new NotImplementedException(new NestableException("nested 1", new NestableException("nested 2")));
        
        assertEquals(3, ex.getThrowableCount());
        
        assertEquals(NotImplementedException.class, ex.getThrowable(0).getClass());
        assertEquals("Code is not implemented", ex.getThrowable(0).getMessage());
        assertEquals(NestableException.class, ex.getThrowable(1).getClass());
        assertEquals("nested 1", ex.getThrowable(1).getMessage());
        assertEquals(NestableException.class, ex.getThrowable(2).getClass());
        assertEquals("nested 2", ex.getThrowable(2).getMessage());
        
        assertEquals(3, ex.getThrowables().length);
        assertEquals(NotImplementedException.class, ex.getThrowables()[0].getClass());
        assertEquals("Code is not implemented", ex.getThrowables()[0].getMessage());
        assertEquals(NestableException.class, ex.getThrowables()[1].getClass());
        assertEquals("nested 1", ex.getThrowables()[1].getMessage());
        assertEquals(NestableException.class, ex.getThrowables()[2].getClass());
        assertEquals("nested 2", ex.getThrowables()[2].getMessage());
    }
    
    public void testIndexOfThrowable() {
        NotImplementedException ex = new NotImplementedException(new NestableException("nested 1", new NestableException("nested 2")));
        assertEquals(0, ex.indexOfThrowable(NotImplementedException.class));
        assertEquals(1, ex.indexOfThrowable(NestableException.class));
    }
    
    public void testIndexOfThrowable_Index() {
        NotImplementedException ex = new NotImplementedException(new NestableException("nested 1", new NestableException("nested 2")));
        assertEquals(1, ex.indexOfThrowable(NestableException.class, 1));
    }
    
    public void testPrintStackTrace() {
        NotImplementedException ex = new NotImplementedException(new NestableException("nested 1", new NestableException("nested 2")));
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        PrintStream ps = new PrintStream(baos);
        PrintStream errStream = System.err;
        System.setErr(ps);
        ex.printStackTrace();
        System.setErr(errStream);
        assertTrue(baos.toString().length() > 0);
    }
    
    public void testPrintStackTrace_Stream() {
        NotImplementedException ex = new NotImplementedException(new NestableException("nested 1", new NestableException("nested 2")));
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        PrintStream ps = new PrintStream(baos);
        ex.printStackTrace(ps);
        assertTrue(baos.toString().length() > 0);
    }
    
    public void testPrintStackTrace_Writer() {
        NotImplementedException ex = new NotImplementedException(new NestableException("nested 1", new NestableException("nested 2")));
        StringWriter stringWriter = new StringWriter();
        PrintWriter writer = new PrintWriter(stringWriter);
        ex.printStackTrace(writer);
        assertTrue(stringWriter.toString().length() > 0);
    }
    
    public void testPrintPartialStackTrace_Writer() {
      NotImplementedException ex = new NotImplementedException(new NestableException("nested 1", new NestableException("nested 2")));
      StringWriter stringWriter = new StringWriter();
      PrintWriter writer = new PrintWriter(stringWriter);
      ex.printPartialStackTrace(writer);
      assertTrue(stringWriter.toString().length() > 0);
  }
}
