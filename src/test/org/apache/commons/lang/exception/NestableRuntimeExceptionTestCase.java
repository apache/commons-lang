package org.apache.commons.lang.exception;

/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution, if
 *    any, must include the following acknowlegement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowlegement may appear in the software itself,
 *    if and wherever such third-party acknowlegements normally appear.
 *
 * 4. The names "The Jakarta Project", "Commons", and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 */

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;

import junit.framework.*;
import junit.textui.TestRunner;
/**
 * Tests the org.apache.commons.lang.exception.NestableRuntimeException class.
 *
 * @author <a href="mailto:steven@caswell.name">Steven Caswell</a>
 * @version $Id: NestableRuntimeExceptionTestCase.java,v 1.4 2002/09/11 18:16:53 stevencaswell Exp $
 */
public class NestableRuntimeExceptionTestCase extends AbstractNestableTestCase
{
    
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
     * Set up instance variables required by this test case.
     */
    public void setUp()
    {
    }
    
    public static Test suite()
    {
        return new TestSuite(NestableExceptionTestCase.class);
    }
    
    /**
     * Tear down instance variables required by this test case.
     */
    public void tearDown()
    {
    }
    
    public static void main(String args[])
    {
        TestRunner.run(suite());
    }
    
    public Nestable getNestable()
    {
        return new NestableRuntimeException();
    }
    
    public Nestable getNestable(Nestable n)
    {
        return new NestableRuntimeException((Throwable) n);
    }
    
    public Nestable getNestable(String msg)
    {
        return new NestableRuntimeException(msg);
    }
    
    public Nestable getNestable(Throwable t)
    {
        return new NestableRuntimeException(t);
    }
    
    public Nestable getNestable(String msg, Throwable t)
    {
        return new NestableRuntimeException(msg, t);
    }
    
    public Nestable getNestable(String msg, Nestable n)
    {
        return new NestableRuntimeException(msg, (Throwable) n);
    }
    
    public Nestable getTester1(Throwable t)
    {
        return new NestableRuntimeExceptionTester1(t);
    }
    
    public Nestable getTester1(Nestable n)
    {
        return new NestableRuntimeExceptionTester1((Throwable) n);
    }
    
    public Nestable getTester1(String msg, Throwable t)
    {
        return new NestableRuntimeExceptionTester1(msg, t);
    }
    
    public Nestable getTester1(String msg, Nestable n)
    {
        return new NestableRuntimeExceptionTester1(msg, (Throwable) n);
    }
    
    public Class getTester1Class()
    {
        return NestableRuntimeExceptionTester1.class;
    }
    
    public Nestable getTester2(String msg, Throwable t)
    {
        return new NestableRuntimeExceptionTester2(msg, t);
    }
    
    public Nestable getTester2(String msg, Nestable n)
    {
        return new NestableRuntimeExceptionTester1(msg, (Throwable) n);
    }
    
    public Class getTester2Class()
    {
        return NestableRuntimeExceptionTester2.class;
    }
    
    public Throwable getThrowable(String msg)
    {
        return new RuntimeException(msg);
    }
    
    public Class getThrowableClass()
    {
        return RuntimeException.class;
    }
    
}

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
