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
package org.apache.commons.lang.exception;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.EOFException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.PrintStream;

import junit.framework.Test;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * Tests the org.apache.commons.lang.exception.NestableException class.
 *
 * @author <a href="mailto:steven@caswell.name">Steven Caswell</a>
 * @version $Id$
 */
public class NestableExceptionTestCase extends AbstractNestableTestCase {
    
    /**
     * Construct a new instance of
     * <code>NestableExceptionTestCase</code>.
     *
     * @param name test case name
     */
    public NestableExceptionTestCase(String name)
    {
        super(name);
    }

    /**
     * Sets up instance variables required by this test case.
     */
    public void setUp()
    {
    }
    
    /**
     * Returns the test suite
     *
     * @return the test suite
     */
    public static Test suite()
    {
        return new TestSuite(NestableExceptionTestCase.class);
    }
    
    /**
     * Tears down instance variables required by this test case.
     */
    public void tearDown()
    {
    }
    
    /**
     * Command line entry point for running the test suite.
     *
     * @param args array of command line arguments
     */
    public static void main(String args[])
    {
        TestRunner.run(suite());
    }
    
    /**
     * @see AbstractNestableTestCase#getNestable()
     */
    public Nestable getNestable()
    {
        return new NestableException();
    }
    
    /**
     * @see AbstractNestableTestCase#getNestable(Nestable)
     */
    public Nestable getNestable(Nestable n)
    {
        return new NestableException((Throwable) n);
    }
    
    /**
     * @see AbstractNestableTestCase#getNestable(String)
     */
    public Nestable getNestable(String msg)
    {
        return new NestableException(msg);
    }
    
    /**
     * @see AbstractNestableTestCase#getNestable(Throwable)
     */
    public Nestable getNestable(Throwable t)
    {
        return new NestableException(t);
    }
    
    /**
     * @see AbstractNestableTestCase#getNestable(String, Throwable)
     */
    public Nestable getNestable(String msg, Throwable t)
    {
        return new NestableException(msg, t);
    }
    
    /**
     * @see AbstractNestableTestCase#getNestable(String, Nestable)
     */
    public Nestable getNestable(String msg, Nestable n)
    {
        return new NestableException(msg, (Throwable) n);
    }
    
    /**
     * @see AbstractNestableTestCase#getTester1(Throwable)
     */
    public Nestable getTester1(Throwable t)
    {
        return new NestableExceptionTester1(t);
    }
    
    /**
     * @see AbstractNestableTestCase#getTester1(Nestable)
     */
    public Nestable getTester1(Nestable n)
    {
        return new NestableExceptionTester1((Throwable) n);
    }
    
    /**
     * @see AbstractNestableTestCase#getTester1(String, Throwable)
     */
    public Nestable getTester1(String msg, Throwable t)
    {
        return new NestableExceptionTester1(msg, t);
    }
    
    /**
     * @see AbstractNestableTestCase#getTester1(String, Nestable)
     */
    public Nestable getTester1(String msg, Nestable n)
    {
        return new NestableExceptionTester1(msg, (Throwable) n);
    }
    
    /**
     * @see AbstractNestableTestCase#getTester1Class()
     */
    public Class getTester1Class()
    {
        return NestableExceptionTester1.class;
    }
    
    /**
     * @see AbstractNestableTestCase#getTester2(String, Throwable)
     */
    public Nestable getTester2(String msg, Throwable t)
    {
        return new NestableExceptionTester2(msg, t);
    }
    
    /**
     * @see AbstractNestableTestCase#getTester2(String, Nestable)
     */
    public Nestable getTester2(String msg, Nestable n)
    {
        return new NestableExceptionTester2(msg, (Throwable) n);
    }
    
    /**
     * @see AbstractNestableTestCase#getTester2Class()
     */
    public Class getTester2Class()
    {
        return NestableExceptionTester2.class;
    }
    
    /**
     * @see AbstractNestableTestCase#getThrowable(String)
     */
    public Throwable getThrowable(String msg)
    {
        return new EOFException(msg);
    }
    
    /**
     * @see AbstractNestableTestCase#getThrowableClass()
     */
    public Class getThrowableClass()
    {
        return EOFException.class;
    }

    /**
     * @see AbstractNestableTestCase#getBaseThrowableClass()
     */
    public Class getBaseThrowableClass()
    {
        return Exception.class;
    }
    
    public void testSpecificPrintStackTrace()
    {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        PrintStream ps = new PrintStream(baos);
        NestableException ne = new NestableException("outer", new NestableException("inner", new Exception("another exception")));
        for(int i = 0; i < 2; i++)
        {
            if(i == 0)
            {
                // Test printStackTrac()
                // Replace System.err with our own PrintStream so that we can
                // obtain and check the printStrackTrace output
                PrintStream err = System.err;
                System.setErr(ps);
                ne.printStackTrace();
                // Restore the System.err
                System.setErr(err);
            }
            else
            {
                // Test printStackTrace(PrintStream)
                ne.printStackTrace(ps);
            }
        }
        String msg = baos.toString();
        assertTrue( "printStackTrace() starts with outer message", msg.startsWith("org.apache.commons.lang.exception.NestableException: outer"));
        assertTrue( "printStackTrace() contains 1st nested message", msg.indexOf("Caused by: org.apache.commons.lang.exception.NestableException: inner") >= 0);
        assertTrue( "printStackTrace() contains 2nd nested message", msg.indexOf("Caused by: java.lang.Exception: another exception") >= 0);
        assertTrue( "printStackTrace() inner message after outer message", 
            msg.indexOf("org.apache.commons.lang.exception.NestableException: outer") <
            msg.indexOf("Caused by: org.apache.commons.lang.exception.NestableException: inner"));
        assertTrue( "printStackTrace() cause message after inner message",
            msg.indexOf("Caused by: org.apache.commons.lang.exception.NestableException: inner") <
            msg.indexOf("Caused by: java.lang.Exception: another exception"));
    }
    
    public void testSerialization()
        throws java.io.IOException, ClassNotFoundException
    {
        RuntimeException nestedEx = new RuntimeException("nested exception message");
        NestableExceptionTester1 ex = new NestableExceptionTester1("serialization test", nestedEx);

        assertTrue( "implements java.io.Serializable", nestedEx instanceof java.io.Serializable);
        
        assertTrue( "implements java.io.Serializable", ex instanceof java.io.Serializable);
        
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        ByteArrayInputStream bais = null;
        ObjectOutputStream oos = null;
        ObjectInputStream ois = null;

        try
        {        
            oos = new ObjectOutputStream(baos);
            oos.writeObject(ex);
            oos.flush();
            bais = new ByteArrayInputStream(baos.toByteArray());
            ois = new ObjectInputStream(bais);
            NestableExceptionTester1 deserializedEx = (NestableExceptionTester1) ois.readObject();
            assertEquals( 
                    "getThrowableCount() return value",
                        ex.getThrowableCount(),
                        deserializedEx.getThrowableCount());
            
            for (int i = 0; i < ex.getThrowableCount(); i++)
            {
                Throwable t = ex.getThrowable(i);
                Throwable deserializedThrowable = deserializedEx.getThrowable(i);
                
                assertEquals( t.getClass(),
                        deserializedThrowable.getClass());
                        
                assertEquals(
                    t.getMessage(),
                    deserializedThrowable.getMessage());
            }
        }
        finally
        {
            if (null != oos)
            {
                try
                {
                    oos.close();
                }
                catch (Exception ignored)
                {
                    // intentionally empty
                }
            }
        }
        
    }
}

/**
 * First nestable tester implementation for use in test cases.
 */
class NestableExceptionTester1 extends NestableException
{
    public NestableExceptionTester1()
    {
        super();
    }

    public NestableExceptionTester1(String reason, Throwable cause)
    {
        super(reason, cause);
    }
    
    public NestableExceptionTester1(String reason)
    {
        super(reason);
    }
    
    public NestableExceptionTester1(Throwable cause)
    {
        super(cause);
    }
    
}

/**
 * Second nestable tester implementation for use in test cases.
 */
class NestableExceptionTester2 extends NestableException
{
    public NestableExceptionTester2()
    {
        super();
    }
    
    public NestableExceptionTester2(String reason, Throwable cause)
    {
        super(reason, cause);
    }
    
    public NestableExceptionTester2(String reason)
    {
        super(reason);
    }
    
    public NestableExceptionTester2(Throwable cause)
    {
        super(cause);
    }
    
}

