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
 * Tests the org.apache.commons.lang.exception.NestableException class.
 *
 * @author <a href="mailto:steven@caswell.name">Steven Caswell</a>
 * @version $Id: NestableExceptionTestCase.java,v 1.1 2002/07/19 03:35:55 bayard Exp $
 */
public class NestableExceptionTestCase extends junit.framework.TestCase
{
    
    /**
     * Construct a new instance of NestableExceptionTestCase with the specified name
     */
    public NestableExceptionTestCase(String name)
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
    
    /**
     * Test the implementation
     */
    public void testGetCause()
    {
        NestableException ne1 = new NestableException();
        assertNull("nestable exception() cause is null", ne1.getCause()); 
        
        NestableException ne2 = new NestableException("ne2");
        assertNull("nestable exception(\"ne2\") cause is null", ne2.getCause());
        
        NestableException ne3 = new NestableException(new Exception("ne3 exception"));
        assertNotNull("nestable exception(new Exception(\"ne3 exception\") cause is not null",
            ne3.getCause()); 
        assertTrue("nestable exception(new Exception(\"ne3 exception\") cause message == ne3 exception",
            ne3.getCause().getMessage().equals("ne3 exception")); 
        
        NestableException ne4 = new NestableException("ne4", new Exception("ne4 exception"));
        assertNotNull("nestable exception(\"ne4\", new Exception(\"ne4 exception\") cause is not null", 
            ne4.getCause()); 
        
        NestableException ne5 = new NestableException("ne5", null);
        assertNull("nestable exception(\"ne5\", null) cause is null", 
            ne5.getCause()); 
        
        NestableException ne6 = new NestableException(null, new Exception("ne6 exception"));
        assertNotNull("nestable exception(null, new Exception(\"ne6 exception\") cause is not null", 
            ne6.getCause()); 
    }

    public void testGetLength()
    {
        NestableException ne1 = new NestableException();
        assertEquals("ne1 length", 1, ne1.getLength());

        NestableException ne2 = new NestableException("ne2");
        assertEquals("ne2 length", 1, ne2.getLength());
        
        NestableException ne3 = new NestableException(new Exception("ne3 exception"));
        assertEquals("ne3 length", 2, ne3.getLength());
        
        NestableException ne4 = new NestableException("ne4", new Exception("ne4 exception"));
        assertEquals("ne4 length", 2, ne4.getLength());
        
        NestableException ne5 = new NestableException("ne5", null);
        assertEquals("ne 5 length", 1, ne5.getLength());
        
        NestableException ne6 = new NestableException(null, new Exception("ne6 exception"));
        assertEquals("ne 6 length", 2, ne6.getLength());
        
        NestableException ne7 = new NestableException("ne7o", new NestableException("ne7i", new Exception("ne7 exception")));
        assertEquals("ne 7 length", 3, ne7.getLength());

        NestableException ne8 = new NestableException("level 1", new NestableException("level 2", new NestableException(new NestableException("level 4", new Exception("level 5")))));
        assertEquals("ne 8 length", 5, ne8.getLength());
    }
    
    public void testGetMessage()
    {
        NestableException ne1 = new NestableException();
        assertNull("nestable exception() message is null", ne1.getMessage()); 

        NestableException ne2 = new NestableException("ne2");
        assertNotNull("nestable exception(\"ne2\") message is not null", ne2.getMessage());
        assertTrue("nestable exception(\"ne2\") message == ne2", ne2.getMessage().equals("ne2"));
        
        NestableException ne3 = new NestableException(new Exception("ne3 exception"));
        assertNotNull("nestable exception(new Exception(\"ne3 exception\") message is not null",
            ne3.getMessage()); 
        assertTrue("nestable exception(new Exception(\"ne3 exception\") message == cause message",
            ne3.getMessage().equals(ne3.getCause().getMessage())); 
        
        NestableException ne4 = new NestableException("ne4", new Exception("ne4 exception"));
        assertNotNull("nestable exception(\"ne4\", new Exception(\"ne4 exception\") message is not null", 
            ne4.getMessage()); 
        assertTrue("nestable exception(\"ne4\", new Exception(\"ne4 exception\") message == ne4: ne4 exception", 
            ne4.getMessage().equals("ne4: ne4 exception")); 
        
        NestableException ne5 = new NestableException("ne5", null);
        assertNotNull("nestable exception(\"ne5\", new Exception(\"ne5 exception\") message is not null", 
            ne5.getMessage()); 
        assertTrue("nestable exception(\"ne5\", null) message == ne5", 
            ne5.getMessage().equals("ne5")); 
        
        NestableException ne6 = new NestableException(null, new Exception("ne6 exception"));
        assertTrue("nestable exception(null, new Exception(\"ne6 exception\") cause == ne6 exception", 
            ne6.getMessage().equals("ne6 exception")); 
        
        NestableException ne7 = new NestableException("ne7o", new NestableException("ne7i", new Exception("ne7 exception")));
        assertTrue("nextable exception(\"ne7o\", new NestableException(\"ne7i\", new Exception(\"ne7 exception\"))) message is ne7o: ne7i: ne7 exception",
            ne7.getMessage().equals("ne7o: ne7i: ne7 exception"));

    }

    public void testGetMessageN()
    {
        String[] msgs = new String[5];
        msgs[0] = "level 1";
        msgs[1] = "level 2";
        msgs[2] = null;
        msgs[3] = "level 4";
        msgs[4] = "level 5";
        NestableException ne = new NestableException(msgs[0], new NestableException(msgs[1], new NestableException(new NestableException(msgs[3], new Exception(msgs[4])))));
        for(int i = 0; i < msgs.length; i++)
        {
            assertEquals("message " + i, msgs[i], ne.getMessage(i));
        }
        assertEquals("message -1", msgs[0], ne.getMessage(-1));
        assertEquals("message 999", msgs[4], ne.getMessage(999));
    }
    
    public void testGetMessages()
    {
        String[] msgs = new String[5];
        msgs[0] = "level 1";
        msgs[1] = "level 2";
        msgs[2] = null;
        msgs[3] = "level 4";
        msgs[4] = "level 5";
        NestableException ne = new NestableException(msgs[0], new NestableException(msgs[1], new NestableException(new NestableException(msgs[3], new Exception(msgs[4])))));
        String[] nMsgs = ne.getMessages();
        assertEquals("messages length", msgs.length, nMsgs.length);
        for(int i = 0; i < nMsgs.length; i++)
        {
            assertEquals("message " + i, msgs[i], nMsgs[i]);
        }
    }

    public void testGetThrowable()
    {
        Nestable n = null;
        String msgs[] = null;
        Class[] throwables = null;
        
        msgs = new String[2];
        msgs[0] = null;
        msgs[1] = "level 2";
        throwables = new Class[2];
        throwables[0] = NestableExceptionTester1.class;
        throwables[1] = Exception.class;
        n = new NestableExceptionTester1(new Exception(msgs[1]));
        doNestableExceptionGetThrowable(n, throwables, msgs);
 
        msgs = new String[5];
        msgs[0] = "level 1";
        msgs[1] = "level 2";
        msgs[2] = null;
        msgs[3] = "level 4";
        msgs[4] = "level 5";
        throwables = new Class[5];
        throwables[0] = NestableExceptionTester1.class;
        throwables[1] = NestableExceptionTester2.class;
        throwables[2] = NestableExceptionTester1.class;
        throwables[3] = NestableExceptionTester2.class;
        throwables[4] = Exception.class;        
        n = new NestableExceptionTester1(msgs[0], new NestableExceptionTester2(msgs[1], new NestableExceptionTester1(new NestableExceptionTester2(msgs[3], new Exception(msgs[4])))));
        doNestableExceptionGetThrowable(n, throwables, msgs);
    }
    
    private void doNestableExceptionGetThrowable(Nestable n, Class[] classes, String[] msgs)
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
        t = n.getThrowable(-1);
        assertEquals("throwable(-1)", classes[0], t.getClass());
        if(Nestable.class.isInstance(t))
        {
            msg = ((Nestable) t).getMessage(0);
        }
        else
        {
            msg = t.getMessage();
        }
        assertEquals("throwable message", msgs[0], msg);
        t = n.getThrowable(999);
        assertEquals("throwable(999)", classes[classes.length - 1], t.getClass());
        if(Nestable.class.isInstance(t))
        {
            msg = ((Nestable) t).getMessage(0);
        }
        else
        {
            msg = t.getMessage();
        }
        assertEquals("throwable message", msgs[msgs.length - 1], msg);
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
        throwables[0] = NestableExceptionTester1.class;
        throwables[1] = Exception.class;
        n = new NestableExceptionTester1(new Exception(msgs[1]));
        doNestableExceptionGetThrowables(n, throwables, msgs);
 
        msgs = new String[5];
        msgs[0] = "level 1";
        msgs[1] = "level 2";
        msgs[2] = null;
        msgs[3] = "level 4";
        msgs[4] = "level 5";
        throwables = new Class[5];
        throwables[0] = NestableExceptionTester1.class;
        throwables[1] = NestableExceptionTester2.class;
        throwables[2] = NestableExceptionTester1.class;
        throwables[3] = NestableExceptionTester2.class;
        throwables[4] = Exception.class;        
        n = new NestableExceptionTester1(msgs[0], new NestableExceptionTester2(msgs[1], new NestableExceptionTester1(new NestableExceptionTester2(msgs[3], new Exception(msgs[4])))));
        doNestableExceptionGetThrowables(n, throwables, msgs);
    }
    
    private void doNestableExceptionGetThrowables(Nestable n, Class[] classes, String[] msgs)
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
        throwables[0] = NestableExceptionTester1.class;
        throwables[1] = NestableExceptionTester2.class;
        throwables[2] = NestableExceptionTester1.class;
        throwables[3] = NestableExceptionTester2.class;
        throwables[4] = Exception.class;
        int[] indexes = {0, 1, 0, 1, 4};
        n = new NestableExceptionTester1(msgs[0], new NestableExceptionTester2(msgs[1], new NestableExceptionTester1(new NestableExceptionTester2(msgs[3], new Exception(msgs[4])))));
        for(int i = 0; i < throwables.length; i++)
        {
            doNestableExceptionIndexOfThrowable(n, throwables[i], indexes[i], msgs[indexes[i]]);
        }
        doNestableExceptionIndexOfThrowable(n, java.util.Date.class, -1, null);
    }
    
    private void doNestableExceptionIndexOfThrowable(Nestable n, Class type, int expectedIndex, String expectedMsg)
    {
        Throwable t = null;
        
        int index = n.indexOfThrowable(type);
        assertEquals("index of throwable " + type.getName(), expectedIndex, index);
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
    
    public void testIndexOfThrowableN()
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
        throwables[0] = NestableExceptionTester1.class;
        throwables[1] = NestableExceptionTester2.class;
        throwables[2] = NestableExceptionTester1.class;
        throwables[3] = NestableExceptionTester2.class;
        throwables[4] = Exception.class;
        int[] indexes = {0, 1, 0, 1, 4};
        n = new NestableExceptionTester1(msgs[0], new NestableExceptionTester2(msgs[1], new NestableExceptionTester1(new NestableExceptionTester2(msgs[3], new Exception(msgs[4])))));
        for(int i = 0; i < throwables.length; i++)
        {
            doNestableExceptionIndexOfThrowableN(n, throwables[i], 0, indexes[i], msgs[indexes[i]]);
        }
        doNestableExceptionIndexOfThrowableN(n, NestableExceptionTester2.class, 2, 3, msgs[3]);
        doNestableExceptionIndexOfThrowableN(n, NestableExceptionTester1.class, 1, 2, msgs[2]);
        doNestableExceptionIndexOfThrowableN(n, java.util.Date.class, 0, -1, null);
    }

    private void doNestableExceptionIndexOfThrowableN(Nestable n, Class type, int pos, int expectedIndex, String expectedMsg)
    {
        Throwable t = null;
        
        int index = n.indexOfThrowable(pos, type);
        assertEquals("index of throwable " + type.getName(), expectedIndex, index);
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
    
    public void testPrintPartialStackTrace()
    {
        NestableException ne9 = new NestableException("ne9", new Exception("ne9 exception"));
        ByteArrayOutputStream baos2 = new ByteArrayOutputStream();
        PrintStream ps2 = new PrintStream(baos2);
        PrintWriter pw2 = new PrintWriter(ps2, true);
        ne9.printPartialStackTrace(pw2);
        String stack2 = baos2.toString();
        assertTrue("stack trace startsWith == org.apache.commons.lang.exception.NestableException: ne9: ne9 exception",
            stack2.startsWith("org.apache.commons.lang.exception.NestableException: ne9: ne9 exception"));
        assertEquals("stack trace indexOf rethrown == -1",
            stack2.indexOf("rethrown"), -1);
    }
    
    public void testPrintStackTrace()
    {
        NestableException ne8 = new NestableException("ne8", new Exception("ne8 exception"));
        ByteArrayOutputStream baos1 = new ByteArrayOutputStream();
        PrintStream ps1 = new PrintStream(baos1);
        PrintWriter pw1 = new PrintWriter(ps1, true);
        ne8.printStackTrace(ps1);
        String stack1 = baos1.toString();
        assertTrue("stack trace startsWith == java.lang.Exception: ne8 exception",
            stack1.startsWith("java.lang.Exception: ne8 exception")); 
        assertTrue("stack trace indexOf org.apache.commons.lang.exception.NestableException: ne8: ne8 exception > -1",
            stack1.indexOf("org.apache.commons.lang.exception.NestableException: ne8: ne8 exception") > -1); 
    }

    public static void main(String args[])
    {
        TestRunner.run(suite());
    }
}

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

