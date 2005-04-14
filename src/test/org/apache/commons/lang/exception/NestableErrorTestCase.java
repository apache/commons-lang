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

import java.io.EOFException;

import junit.framework.Test;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * Tests the org.apache.commons.lang.exception.NestableError class.
 *
 * @author <a href="mailto:steven@caswell.name">Steven Caswell</a>
 * @version $Id$
 */
public class NestableErrorTestCase extends AbstractNestableTestCase {
    
    /**
     * Construct a new instance of
     * <code>NestableErrorTestCase</code>.
     *
     * @param name test case name
     */
    public NestableErrorTestCase(String name)
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
        return new TestSuite(NestableErrorTestCase.class);
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
        return new NestableError();
    }    
    
    /**
     * @see AbstractNestableTestCase#getNestable(Nestable)
     */
    public Nestable getNestable(Nestable n)
    {
        return new NestableError((Throwable) n);
    }
    
    /**
     * @see AbstractNestableTestCase#getNestable(String)
     */
    public Nestable getNestable(String msg)
    {
        return new NestableError(msg);
    }
    
    /**
     * @see AbstractNestableTestCase#getNestable(Throwable)
     */
    public Nestable getNestable(Throwable t)
    {
        return new NestableError(t);
    }
    
    /**
     * @see AbstractNestableTestCase#getNestable(String, Throwable)
     */
    public Nestable getNestable(String msg, Throwable t)
    {
        return new NestableError(msg, t);
    }
    
    /**
     * @see AbstractNestableTestCase#getNestable(String, Nestable)
     */
    public Nestable getNestable(String msg, Nestable n)
    {
        return new NestableError(msg, (Throwable) n);
    }
    
    /**
     * @see AbstractNestableTestCase#getTester1(Throwable)
     */
    public Nestable getTester1(Throwable t)
    {
        return new NestableErrorTester1(t);
    }
    
    /**
     * @see AbstractNestableTestCase#getTester1(Nestable)
     */
    public Nestable getTester1(Nestable n)
    {
        return new NestableErrorTester1((Throwable) n);
    }
    
    /**
     * @see AbstractNestableTestCase#getTester1(String, Throwable)
     */
    public Nestable getTester1(String msg, Throwable t)
    {
        return new NestableErrorTester1(msg, t);
    }
    
    /**
     * @see AbstractNestableTestCase#getTester1(String, Nestable)
     */
    public Nestable getTester1(String msg, Nestable n)
    {
        return new NestableErrorTester1(msg, (Throwable) n);
    }
    
    /**
     * @see AbstractNestableTestCase#getTester1Class()
     */
    public Class getTester1Class()
    {
        return NestableErrorTester1.class;
    }
    
    /**
     * @see AbstractNestableTestCase#getTester2(String, Throwable)
     */
    public Nestable getTester2(String msg, Throwable t)
    {
        return new NestableErrorTester2(msg, t);
    }
    
    /**
     * @see AbstractNestableTestCase#getTester2(String, Nestable)
     */
    public Nestable getTester2(String msg, Nestable n)
    {
        return new NestableErrorTester2(msg, (Throwable) n);
    }
    
    /**
     * @see AbstractNestableTestCase#getTester2Class()
     */
    public Class getTester2Class()
    {
        return NestableErrorTester2.class;
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
        return Error.class;
    }
    
}

/**
 * First nestable tester implementation for use in test cases.
 */
class NestableErrorTester1 extends NestableError
{
    public NestableErrorTester1()
    {
        super();
    }

    public NestableErrorTester1(String reason, Throwable cause)
    {
        super(reason, cause);
    }
    
    public NestableErrorTester1(String reason)
    {
        super(reason);
    }
    
    public NestableErrorTester1(Throwable cause)
    {
        super(cause);
    }
    
}

/**
 * Second nestable tester implementation for use in test cases.
 */
class NestableErrorTester2 extends NestableError
{
    public NestableErrorTester2()
    {
        super();
    }
    
    public NestableErrorTester2(String reason, Throwable cause)
    {
        super(reason, cause);
    }
    
    public NestableErrorTester2(String reason)
    {
        super(reason);
    }
    
    public NestableErrorTester2(Throwable cause)
    {
        super(cause);
    }
    
}
