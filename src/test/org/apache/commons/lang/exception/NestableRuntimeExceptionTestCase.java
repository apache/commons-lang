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

import java.io.ByteArrayOutputStream;
import java.io.EOFException;
import java.io.PrintStream;

import junit.framework.Test;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * Tests the org.apache.commons.lang.exception.NestableRuntimeException class.
 *
 * @author <a href="mailto:steven@caswell.name">Steven Caswell</a>
 * @version $Id$
 */
public class NestableRuntimeExceptionTestCase extends AbstractNestableTestCase {
    
    /**
     * Construct a new instance of
     * <code>NestableRuntimeExceptionTestCase</code>.
     *
     * @param name test case name
     */
    public NestableRuntimeExceptionTestCase(String name)
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
        return new TestSuite(NestableRuntimeExceptionTestCase.class);
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
        return new NestableRuntimeException();
    }
    
    /**
     * @see AbstractNestableTestCase#getNestable(Nestable)
     */
    public Nestable getNestable(Nestable n)
    {
        return new NestableRuntimeException((Throwable) n);
    }
    
    /**
     * @see AbstractNestableTestCase#getNestable(String)
     */
    public Nestable getNestable(String msg)
    {
        return new NestableRuntimeException(msg);
    }
    
    /**
     * @see AbstractNestableTestCase#getNestable(Throwable)
     */
    public Nestable getNestable(Throwable t)
    {
        return new NestableRuntimeException(t);
    }
    
    /**
     * @see AbstractNestableTestCase#getNestable(String, Throwable)
     */
    public Nestable getNestable(String msg, Throwable t)
    {
        return new NestableRuntimeException(msg, t);
    }
    
    /**
     * @see AbstractNestableTestCase#getNestable(String, Nestable)
     */
    public Nestable getNestable(String msg, Nestable n)
    {
        return new NestableRuntimeException(msg, (Throwable) n);
    }
    
    /**
     * @see AbstractNestableTestCase#getTester1(Throwable)
     */
    public Nestable getTester1(Throwable t)
    {
        return new NestableRuntimeExceptionTester1(t);
    }
    
    /**
     * @see AbstractNestableTestCase#getTester1(Nestable)
     */
    public Nestable getTester1(Nestable n)
    {
        return new NestableRuntimeExceptionTester1((Throwable) n);
    }
    
    /**
     * @see AbstractNestableTestCase#getTester1(String, Throwable)
     */
    public Nestable getTester1(String msg, Throwable t)
    {
        return new NestableRuntimeExceptionTester1(msg, t);
    }
    
    /**
     * @see AbstractNestableTestCase#getTester1(String, Nestable)
     */
    public Nestable getTester1(String msg, Nestable n)
    {
        return new NestableRuntimeExceptionTester1(msg, (Throwable) n);
    }
    
    /**
     * @see AbstractNestableTestCase#getTester1Class()
     */
    public Class getTester1Class()
    {
        return NestableRuntimeExceptionTester1.class;
    }
    
    /**
     * @see AbstractNestableTestCase#getTester2(String, Throwable)
     */
    public Nestable getTester2(String msg, Throwable t)
    {
        return new NestableRuntimeExceptionTester2(msg, t);
    }
    
    /**
     * @see AbstractNestableTestCase#getTester2(String, Nestable)
     */
    public Nestable getTester2(String msg, Nestable n)
    {
        return new NestableRuntimeExceptionTester2(msg, (Throwable) n);
    }
    
    /**
     * @see AbstractNestableTestCase#getTester2Class()
     */
    public Class getTester2Class()
    {
        return NestableRuntimeExceptionTester2.class;
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
        return RuntimeException.class;
    }
    
    public void testSpecificPrintStackTrace()
    {
        // Test printStackTrac()
        // Replace System.err with our own PrintStream so that we can obtain
        // and check the printStrackTrace output
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        PrintStream ps = new PrintStream(baos);
        NestableRuntimeException ne = new NestableRuntimeException("outer", new NestableRuntimeException("inner", new Exception("another exception")));
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
        assertTrue( "printStackTrace() starts with outer message", msg.startsWith("org.apache.commons.lang.exception.NestableRuntimeException: outer"));
        assertTrue( "printStackTrace() contains 1st nested message", msg.indexOf("Caused by: org.apache.commons.lang.exception.NestableRuntimeException: inner") >= 0);
        assertTrue( "printStackTrace() contains 2nd nested message", msg.indexOf("Caused by: java.lang.Exception: another exception") >= 0);
        assertTrue( "printStackTrace() inner message after outer message", 
            msg.indexOf("org.apache.commons.lang.exception.NestableRuntimeException: outer") <
            msg.indexOf("Caused by: org.apache.commons.lang.exception.NestableRuntimeException: inner"));
        assertTrue( "printStackTrace() cause message after inner message",
            msg.indexOf("Caused by: org.apache.commons.lang.exception.NestableRuntimeException: inner") <
            msg.indexOf("Caused by: java.lang.Exception: another exception"));
    }
    
}

/**
 * First nestable tester implementation for use in test cases.
 */
class NestableRuntimeExceptionTester1 extends NestableRuntimeException
{
    public NestableRuntimeExceptionTester1()
    {
        super();
    }

    public NestableRuntimeExceptionTester1(String reason, Throwable cause)
    {
        super(reason, cause);
    }
    
    public NestableRuntimeExceptionTester1(String reason)
    {
        super(reason);
    }
    
    public NestableRuntimeExceptionTester1(Throwable cause)
    {
        super(cause);
    }
    
}

/**
 * Second nestable tester implementation.
 */
class NestableRuntimeExceptionTester2 extends NestableRuntimeException
{
    public NestableRuntimeExceptionTester2()
    {
        super();
    }
    
    public NestableRuntimeExceptionTester2(String reason, Throwable cause)
    {
        super(reason, cause);
    }
    
    public NestableRuntimeExceptionTester2(String reason)
    {
        super(reason);
    }
    
    public NestableRuntimeExceptionTester2(Throwable cause)
    {
        super(cause);
    }
    
}

