/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002-2003 The Apache Software Foundation.  All rights
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
package org.apache.commons.lang.exception;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import junit.framework.Test;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * Tests the org.apache.commons.lang.exception.NestableException class.
 *
 * @author <a href="mailto:steven@caswell.name">Steven Caswell</a>
 * @version $Id: NestableExceptionTestCase.java,v 1.8 2003/05/21 23:49:14 scolebourne Exp $
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
        return new Exception(msg);
    }
    
    /**
     * @see AbstractNestableTestCase#getThrowableClass()
     */
    public Class getThrowableClass()
    {
        return Exception.class;
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

