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
 * @version $Id: NestableRuntimeExceptionTestCase.java,v 1.3 2002/08/25 13:21:24 stevencaswell Exp $
 */
public class NestableRuntimeExceptionTestCase extends junit.framework.TestCase
{
    /**
     * Construct a new instance of NestableRuntimeExceptionTestCase with the specified name
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
        return new TestSuite(NestableRuntimeExceptionTestCase.class);
    }
    
    /**
     * Tear down instance variables required by this test case.
     */
    public void tearDown()
    {
    }
    
    /**
     * Test the implementation
     */
    public void testGetCause()
    {
        NestableRuntimeException ne1 = new NestableRuntimeException();
        assertNull("nestable runtime exception() cause is null", ne1.getCause()); 
        
        NestableRuntimeException ne2 = new NestableRuntimeException("ne2");
        assertNull("nestable runtime exception(\"ne2\") cause is null", ne2.getCause());

        NestableRuntimeException ne3 = new NestableRuntimeException(new Exception("ne3 exception"));
        assertNotNull("nestable runtime exception(new Exception(\"ne3 exception\") cause is not null",
            ne3.getCause()); 
        assertTrue("nestable runtime exception(new Exception(\"ne3 exception\") cause message == ne3 exception",
            ne3.getCause().getMessage().equals("ne3 exception")); 

        NestableRuntimeException ne4 = new NestableRuntimeException("ne4", new Exception("ne4 exception"));
        assertNotNull("nestable runtime exception(\"ne4\", new Exception(\"ne4 exception\") cause is not null", 
            ne4.getCause()); 

        NestableRuntimeException ne5 = new NestableRuntimeException("ne5", null);
        assertNull("nestable runtime exception(\"ne5\", null) cause is null", 
            ne5.getCause());

        NestableRuntimeException ne6 = new NestableRuntimeException(null, new Exception("ne6 exception"));
        assertNotNull("nestable runtime exception(null, new Exception(\"ne6 exception\") cause is not null", 
            ne6.getCause()); 
    }
    
    public void testGetThrowableCount()
    {
        NestableRuntimeException ne1 = new NestableRuntimeException();
        assertEquals("ne1 throwable count", 1, ne1.getThrowableCount());

        NestableRuntimeException ne2 = new NestableRuntimeException("ne2");
        assertEquals("ne2 throwable count", 1, ne2.getThrowableCount());
        
        NestableRuntimeException ne3 = new NestableRuntimeException(new Exception("ne3 exception"));
        assertEquals("ne3 throwable count", 2, ne3.getThrowableCount());
        
        NestableRuntimeException ne4 = new NestableRuntimeException("ne4", new Exception("ne4 exception"));
        assertEquals("ne4 throwable count", 2, ne4.getThrowableCount());
        
        NestableRuntimeException ne5 = new NestableRuntimeException("ne5", null);
        assertEquals("ne 5 throwable count", 1, ne5.getThrowableCount());
        
        NestableRuntimeException ne6 = new NestableRuntimeException(null, new Exception("ne6 exception"));
        assertEquals("ne 6 throwable count", 2, ne6.getThrowableCount());
        
        NestableRuntimeException ne7 = new NestableRuntimeException("ne7o", new NestableRuntimeException("ne7i", new Exception("ne7 exception")));
        assertEquals("ne 7 throwable count", 3, ne7.getThrowableCount());

        NestableRuntimeException ne8 = new NestableRuntimeException("level 1", new NestableRuntimeException("level 2", new NestableRuntimeException(new NestableRuntimeException("level 4", new Exception("level 5")))));
        assertEquals("ne 8 throwable count", 5, ne8.getThrowableCount());
    }
    
    public void testGetMessage()
    {
        NestableRuntimeException ne1 = new NestableRuntimeException();
        assertNull("nestable runtime exception() message is null", ne1.getMessage()); 
        
        NestableRuntimeException ne2 = new NestableRuntimeException("ne2");
        assertNotNull("nestable runtime exception(\"ne2\") message is not null", ne2.getMessage()); 
        assertTrue("nestable runtime exception(\"ne2\") message == ne2", ne2.getMessage().equals("ne2")); 

        NestableRuntimeException ne3 = new NestableRuntimeException(new Exception("ne3 exception"));
        assertNotNull("nestable runtime exception(new Exception(\"ne3 exception\") message is not null",
            ne3.getMessage());
        assertTrue("nestable runtime exception(new Exception(\"ne3 exception\") message == cause message",
            ne3.getMessage().equals(ne3.getCause().getMessage())); 

        NestableRuntimeException ne4 = new NestableRuntimeException("ne4", new Exception("ne4 exception"));
        assertNotNull("nestable runtime exception(\"ne4\", new Exception(\"ne4 exception\") message is not null", 
            ne4.getMessage()); 
        assertTrue("nestable runtime exception(\"ne4\", new Exception(\"ne4 exception\") message == ne4: ne4 exception", 
            ne4.getMessage().equals("ne4: ne4 exception")); 

        NestableRuntimeException ne5 = new NestableRuntimeException("ne5", null);
        assertNotNull("nestable runtime exception(\"ne5\", new Exception(\"ne5 exception\") message is not null", 
            ne5.getMessage()); 
        assertTrue("nestable runtime exception(\"ne5\", null) message == ne5", 
            ne5.getMessage().equals("ne5")); 

        NestableRuntimeException ne6 = new NestableRuntimeException(null, new Exception("ne6 exception"));
        assertTrue("nestable runtime exception(null, new Exception(\"ne6 exception\") cause == ne6 exception", 
            ne6.getMessage().equals("ne6 exception")); 

        NestableRuntimeException ne7 = new NestableRuntimeException("ne7o", new NestableRuntimeException("ne7i", new Exception("ne7 exception")));
        assertTrue("nextable exception(\"ne7o\", new NestableRuntimeException(\"ne7i\", new Exception(\"ne7 exception\"))) message is ne7o: ne7i: ne7 exception",
            ne7.getMessage().equals("ne7o: ne7i: ne7 exception")); 
    }
    
    public void testGetMessageI()
    {
        String[] msgs = new String[5];
        msgs[0] = "level 1";
        msgs[1] = "level 2";
        msgs[2] = null;
        msgs[3] = "level 4";
        msgs[4] = "level 5";
        NestableRuntimeException ne = new NestableRuntimeException(msgs[0], new NestableRuntimeException(msgs[1], new NestableRuntimeException(new NestableRuntimeException(msgs[3], new Exception(msgs[4])))));
        for(int i = 0; i < msgs.length; i++)
        {
            assertEquals("message " + i, msgs[i], ne.getMessage(i));
        }
        
        // Test for index out of bounds
        try
        {
            String msg = ne.getMessage(-1);
            fail("getMessage(-1) should have thrown IndexOutOfBoundsException");
        }
        catch(IndexOutOfBoundsException ioode)
        {
        }
        try
        {
            String msg = ne.getMessage(msgs.length + 100);
            fail("getMessage(999) should have thrown IndexOutOfBoundsException");
        }
        catch(IndexOutOfBoundsException ioode)
        {
        }
    }
    
    public void testGetMessages()
    {
        String[] msgs = new String[5];
        msgs[0] = "level 1";
        msgs[1] = "level 2";
        msgs[2] = null;
        msgs[3] = "level 4";
        msgs[4] = "level 5";
        NestableRuntimeException ne = new NestableRuntimeException(msgs[0], new NestableRuntimeException(msgs[1], new NestableRuntimeException(new NestableRuntimeException(msgs[3], new Exception(msgs[4])))));
        String[] nMsgs = ne.getMessages();
        assertEquals("messages length", msgs.length, nMsgs.length);
        for(int i = 0; i < nMsgs.length; i++)
        {
            assertEquals("message " + i, msgs[i], nMsgs[i]);
        }
    }

    public void testGetThrowableI()
    {
        Nestable n = null;
        String msgs[] = null;
        Class[] throwables = null;
        
        msgs = new String[2];
        msgs[0] = null;
        msgs[1] = "level 2";
        throwables = new Class[2];
        throwables[0] = NestableRuntimeExceptionTester1.class;
        throwables[1] = Exception.class;
        n = new NestableRuntimeExceptionTester1(new Exception(msgs[1]));
        doNestableRuntimeExceptionGetThrowableI(n, throwables, msgs);
 
        msgs = new String[5];
        msgs[0] = "level 1";
        msgs[1] = "level 2";
        msgs[2] = null;
        msgs[3] = "level 4";
        msgs[4] = "level 5";
        throwables = new Class[5];
        throwables[0] = NestableRuntimeExceptionTester1.class;
        throwables[1] = NestableRuntimeExceptionTester2.class;
        throwables[2] = NestableRuntimeExceptionTester1.class;
        throwables[3] = NestableRuntimeExceptionTester2.class;
        throwables[4] = Exception.class;        
        n = new NestableRuntimeExceptionTester1(msgs[0], new NestableRuntimeExceptionTester2(msgs[1], new NestableRuntimeExceptionTester1(new NestableRuntimeExceptionTester2(msgs[3], new Exception(msgs[4])))));
        doNestableRuntimeExceptionGetThrowableI(n, throwables, msgs);
    }
    
    private void doNestableRuntimeExceptionGetThrowableI(Nestable n, Class[] classes, String[] msgs)
    {
        Throwable t = null;
        String msg = null;

        for(int i = 0; i < classes.length; i++)
        {
            t = n.getThrowable(i);
            assertEquals("throwable class", classes[i], t.getClass());
            if(Nestable.class.isInstance(t))
            {
                msg = ((Nestable) t).getMessage(0);
            }
            else
            {
                msg = t.getMessage();
            }
            assertEquals("throwable message", msgs[i], msg);
        }
        
        // Test for index out of bounds
        try
        {
            t = n.getThrowable(-1);
            fail("getThrowable(-1) should have thrown IndexOutOfBoundsException");
        }
        catch(IndexOutOfBoundsException ioobe)
        {
        }
        try
        {
            t = n.getThrowable(999);
            fail("getThrowable(999) should have thrown IndexOutOfBoundsException");
        }
        catch(IndexOutOfBoundsException ioobe)
        {
        }
    }
    
    public void testGetThrowables()
    {
        Nestable n = null;
        String msgs[] = null;
        Class[] throwables = null;
        
        msgs = new String[2];
        msgs[0] = null;
        msgs[1] = "level 2";
        throwables = new Class[2];
        throwables[0] = NestableRuntimeExceptionTester1.class;
        throwables[1] = Exception.class;
        n = new NestableRuntimeExceptionTester1(new Exception(msgs[1]));
        doNestableRuntimeExceptionGetThrowables(n, throwables, msgs);
 
        msgs = new String[5];
        msgs[0] = "level 1";
        msgs[1] = "level 2";
        msgs[2] = null;
        msgs[3] = "level 4";
        msgs[4] = "level 5";
        throwables = new Class[5];
        throwables[0] = NestableRuntimeExceptionTester1.class;
        throwables[1] = NestableRuntimeExceptionTester2.class;
        throwables[2] = NestableRuntimeExceptionTester1.class;
        throwables[3] = NestableRuntimeExceptionTester2.class;
        throwables[4] = Exception.class;        
        n = new NestableRuntimeExceptionTester1(msgs[0], new NestableRuntimeExceptionTester2(msgs[1], new NestableRuntimeExceptionTester1(new NestableRuntimeExceptionTester2(msgs[3], new Exception(msgs[4])))));
        doNestableRuntimeExceptionGetThrowables(n, throwables, msgs);
    }
    
    private void doNestableRuntimeExceptionGetThrowables(Nestable n, Class[] classes, String[] msgs)
    {
        String msg = null;

        Throwable throwables[] = n.getThrowables();
        assertEquals("throwables length", classes.length, throwables.length);
        for(int i = 0; i < classes.length; i++)
        {
            assertEquals("throwable class", classes[i], throwables[i].getClass());
            Throwable t = throwables[i];
            if(Nestable.class.isInstance(t))
            {
                msg = ((Nestable) t).getMessage(0);
            }
            else
            {
                msg = t.getMessage();
            }
            assertEquals("throwable message", msgs[i], msg);
        }
    }
    
    public void testIndexOfThrowable()
    {
        Nestable n = null;
        String msgs[] = null;
        Class[] throwables = null;
        
        msgs = new String[5];
        msgs[0] = "level 1";
        msgs[1] = "level 2";
        msgs[2] = null;
        msgs[3] = "level 4";
        msgs[4] = "level 5";
        throwables = new Class[5];
        throwables[0] = NestableRuntimeExceptionTester1.class;
        throwables[1] = NestableRuntimeExceptionTester2.class;
        throwables[2] = NestableRuntimeExceptionTester1.class;
        throwables[3] = NestableRuntimeExceptionTester2.class;
        throwables[4] = Exception.class;
        int[] indexes = {0, 1, 0, 1, 4};
        n = new NestableRuntimeExceptionTester1(msgs[0], new NestableRuntimeExceptionTester2(msgs[1], new NestableRuntimeExceptionTester1(new NestableRuntimeExceptionTester2(msgs[3], new Exception(msgs[4])))));
        for(int i = 0; i < throwables.length; i++)
        {
            doNestableRuntimeExceptionIndexOfThrowable(n, throwables[i], indexes[i], msgs[indexes[i]]);
        }
        doNestableRuntimeExceptionIndexOfThrowable(n, java.util.Date.class, -1, null);
    }
    
    private void doNestableRuntimeExceptionIndexOfThrowable(Nestable n, Class type, int expectedIndex, String expectedMsg)
    {
        Throwable t = null;
        
        int index = n.indexOfThrowable(type);
        assertEquals("index of throwable " + type.getName(), expectedIndex, index);
        if(expectedMsg != null)
        {
            t = n.getThrowable(index);
            String msg = null;
            if(Nestable.class.isInstance(t))
            {
                msg = ((Nestable) t).getMessage(0);
            }
            else
            {
                msg = t.getMessage();
            }
            assertEquals("message of indexed throwable", expectedMsg, msg);
        }
    }
    
    public void testIndexOfThrowableI()
    {
        Nestable n = null;
        String msgs[] = null;
        Class[] throwables = null;
        
        msgs = new String[5];
        msgs[0] = "level 1";
        msgs[1] = "level 2";
        msgs[2] = null;
        msgs[3] = "level 4";
        msgs[4] = "level 5";
        throwables = new Class[5];
        throwables[0] = NestableRuntimeExceptionTester1.class;
        throwables[1] = NestableRuntimeExceptionTester2.class;
        throwables[2] = NestableRuntimeExceptionTester1.class;
        throwables[3] = NestableRuntimeExceptionTester2.class;
        throwables[4] = Exception.class;
        int[] indexes = {0, 1, 0, 1, 4};
        n = new NestableRuntimeExceptionTester1(msgs[0], new NestableRuntimeExceptionTester2(msgs[1], new NestableRuntimeExceptionTester1(new NestableRuntimeExceptionTester2(msgs[3], new Exception(msgs[4])))));
        for(int i = 0; i < throwables.length; i++)
        {
            doNestableRuntimeExceptionIndexOfThrowableI(n, throwables[i], 0, indexes[i], msgs[indexes[i]]);
        }
        doNestableRuntimeExceptionIndexOfThrowableI(n, NestableRuntimeExceptionTester2.class, 2, 3, msgs[3]);
        doNestableRuntimeExceptionIndexOfThrowableI(n, NestableRuntimeExceptionTester1.class, 1, 2, msgs[2]);
        doNestableRuntimeExceptionIndexOfThrowableI(n, NestableRuntimeExceptionTester1.class, 3, -1, null);
        doNestableRuntimeExceptionIndexOfThrowableI(n, NestableRuntimeExceptionTester1.class, 4, -1, null);
        doNestableRuntimeExceptionIndexOfThrowableI(n, java.util.Date.class, 0, -1, null);
        
        try
        {
            int index = n.indexOfThrowable(NestableRuntimeExceptionTester1.class, -1);
            fail("method should have thrown IndexOutOfBoundsException");
        }
        catch(IndexOutOfBoundsException iooob)
        {
        }
        try
        {
            int index = n.indexOfThrowable(NestableRuntimeExceptionTester1.class, 5);
            fail("method should have thrown IndexOutOfBoundsException");
        }
        catch(IndexOutOfBoundsException iooob)
        {
        }
        
    }

    private void doNestableRuntimeExceptionIndexOfThrowableI(Nestable n, Class type, int fromIndex, int expectedIndex, String expectedMsg)
    {
        Throwable t = null;
        
        int index = n.indexOfThrowable(type, fromIndex);
        assertEquals("index of throwable " + type.getName(), expectedIndex, index);
        if(expectedIndex > -1)
        {
            t = n.getThrowable(index);
            if(expectedMsg != null)
            {
                String msg = null;
                if(Nestable.class.isInstance(t))
                {
                    msg = ((Nestable) t).getMessage(0);
                }
                else
                {
                    msg = t.getMessage();
                }
                assertEquals("message of indexed throwable", expectedMsg, msg);
            }
        }
        
    }
    
    public void testPrintStackTrace()
    {
        NestableRuntimeException ne8 = new NestableRuntimeException("ne8", new Exception("ne8 exception"));
        ByteArrayOutputStream baos1 = new ByteArrayOutputStream();
        PrintStream ps1 = new PrintStream(baos1);
        PrintWriter pw1 = new PrintWriter(ps1, true);
        ne8.printStackTrace(ps1);
        String stack1 = baos1.toString();
        assertTrue("stack trace startsWith == java.lang.Exception: ne8 exception",
            stack1.startsWith("java.lang.Exception: ne8 exception")); 
        assertTrue("stack trace indexOf org.apache.commons.lang.exception.NestableRuntimeException: ne8: ne8 exception > -1",
            stack1.indexOf("org.apache.commons.lang.exception.NestableRuntimeException: ne8: ne8 exception") > -1); 
    }

    public void testPrintPartialStackTrace()
    {
        NestableRuntimeException ne9 = new NestableRuntimeException("ne9", new Exception("ne9 exception"));
        ByteArrayOutputStream baos2 = new ByteArrayOutputStream();
        PrintStream ps2 = new PrintStream(baos2);
        PrintWriter pw2 = new PrintWriter(ps2, true);
        ne9.printPartialStackTrace(pw2);
        String stack2 = baos2.toString();
        assertTrue("stack trace startsWith == org.apache.commons.lang.exception.NestableRuntimeException: ne9: ne9 exception",
            stack2.startsWith("org.apache.commons.lang.exception.NestableRuntimeException: ne9: ne9 exception"));
        assertEquals("stack trace indexOf rethrown == -1",
            stack2.indexOf("rethrown"), -1);
    }

    public static void main(String args[])
    {
        TestRunner.run(suite());
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
