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
import java.io.PrintWriter;
import java.io.PrintStream;

import junit.framework.*;
import junit.textui.TestRunner;
/**
 * Tests the org.apache.commons.lang.exception.NestableDelegate class.
 *
 * @author <a href="mailto:steven@caswell.name">Steven Caswell</a>
 * @author <a href="mailto:dlr@finemaltcoding.com">Daniel Rall</a>
 * @version $Id: NestableDelegateTestCase.java,v 1.1 2002/07/19 03:35:55 bayard Exp $
 */
public class NestableDelegateTestCase extends junit.framework.TestCase
{
    private static final String CONSTRUCTOR_FAILED_MSG = 
    "The Nestable implementation passed to the NestableDelegate(Nestable) constructor must extend java.lang.Throwable";

    private static final String PARTIAL_STACK_TRACE =
        "rethrown as ThrowableNestedNestable partial stack trace place-holder";

    protected String lineSeparator;

    /**
     * Construct a new instance of NestableDelegateTestCase with the specified name
     */
    public NestableDelegateTestCase(String name)
    {
        super(name);
    }

    /**
     * Set up instance variables required by this test case.
     */
    public void setUp()
    {
        lineSeparator = System.getProperty("line.separator");
    }
    
    public static Test suite()
    {
        return new TestSuite(NestableDelegateTestCase.class);
    }
    
    /**
     * Tear down instance variables required by this test case.
     */
    public void tearDown()
    {
        lineSeparator = null;
    }
    
    /**
     * Test the implementation
     */
    public void testNestableDelegateConstructor()
    {
        String msg = null;
        boolean constructorFailed = false;
        try
        {
            NestableDelegate nonThrowableCause = new NestableDelegate(new NonThrowableNestable());
        }
        catch(IllegalArgumentException iae)
        {
            constructorFailed = true;
            msg = iae.getMessage();
        }
        assertTrue("nestable delegate constructor with non-throwable cause failed == true", constructorFailed);
        assertTrue("constructor failed exception msg == " + CONSTRUCTOR_FAILED_MSG,
            msg.equals(CONSTRUCTOR_FAILED_MSG));

        constructorFailed = false;
        try
        {
            NestableDelegate nd1 = new NestableDelegate(new ThrowableNestable());
        }
        catch(IllegalArgumentException iae)
        {
            constructorFailed = true;
        }
        assertTrue("nestable delegate constructor with throwable cause failed == false", !constructorFailed);
    }

    public void testNestableDelegateGetMessage()
    {
        Nestable ne1 = new ThrowableNestable();
        assertTrue("ThrowableNestable ne1 getMessage() == ThrowableNestable exception",
            ne1.getMessage().equals("ThrowableNestable exception"));
        NestableDelegate nd1 = new NestableDelegate(ne1);
        assertTrue("nd1 getMessage() == " + ne1.getCause().getMessage(),
            nd1.getMessage("base").equals("base: " + ne1.getCause().getMessage()));
        
        Nestable ne2 = new ThrowableNestedNestable(new Exception("nested exception 2"));
        NestableDelegate nd2 = new NestableDelegate(ne2);
        assertTrue("nd2 getMessage() == base: " + ne2.getCause().getMessage(),
            nd2.getMessage("base").equals("base: " + ne2.getCause().getMessage()));
    }

    public void testNestableDelegateGetLength()
    {
        Nestable n = null;
        NestableDelegate d = null;
        
        n = new NestableDelegateTester1();
        d = new NestableDelegate(n);
        doNestableDelegateGetLength(d, 1);
        
        n = new NestableDelegateTester1("level 1");
        d = new NestableDelegate(n);
        doNestableDelegateGetLength(d, 1);
        
        n = new NestableDelegateTester1(new Exception());
        d = new NestableDelegate(n);
        doNestableDelegateGetLength(d, 2);
        
        n = new NestableDelegateTester1(new Exception("level 2"));
        d = new NestableDelegate(n);
        doNestableDelegateGetLength(d, 2);
        
        n = new NestableDelegateTester1("level 1", new NestableDelegateTester2("level 2", new NestableDelegateTester1(new NestableDelegateTester2("level 4", new Exception("level 5")))));
        d = new NestableDelegate(n);
        doNestableDelegateGetLength(d, 5);
    }

    private void doNestableDelegateGetLength(NestableDelegate d, int len)
    {
        // Compare the lengths
        assertEquals("delegate length", len, d.getLength());
    }
    
    public void testNestableDelegateGetMessages()
    {
        Nestable n = null;
        NestableDelegate d = null;
        String msgs[] = null;
        
        msgs = new String[1];
        n = new NestableDelegateTester1();
        d = new NestableDelegate(n);        
        doNestableDelegateGetMessages(d, msgs);
        
        msgs = new String[1];
        msgs[0] = "level 1";
        n = new NestableDelegateTester1(msgs[0]);
        d = new NestableDelegate(n);
        doNestableDelegateGetMessages(d, msgs);

        msgs = new String[2];
        n = new NestableDelegateTester1(new Exception());
        d = new NestableDelegate(n);
        doNestableDelegateGetMessages(d, msgs);

        msgs = new String[2];
        msgs[0] = null;
        msgs[1] = "level 2";
        n = new NestableDelegateTester1(new Exception(msgs[1]));
        d = new NestableDelegate(n);
        doNestableDelegateGetMessages(d, msgs);
 
        msgs = new String[5];
        msgs[0] = "level 1";
        msgs[1] = "level 2";
        msgs[2] = null;
        msgs[3] = "level 4";
        msgs[4] = "level 5";
        n = new NestableDelegateTester1(msgs[0], new NestableDelegateTester2(msgs[1], new NestableDelegateTester1(new NestableDelegateTester2(msgs[3], new Exception(msgs[4])))));
        d = new NestableDelegate(n);
        doNestableDelegateGetMessages(d, msgs);
    }

    private void doNestableDelegateGetMessages(NestableDelegate d, String[] nMsgs)
    {
        // Compare the messages
        String[] dMsgs = d.getMessages();
        assertEquals("messages length", nMsgs.length, dMsgs.length);
        for(int i = 0; i < nMsgs.length; i++)
        {
            assertEquals("message " + i, nMsgs[i], dMsgs[i]);
        }
    }

    public void testNestableDelegateGetMessageN()
    {
        Nestable n = null;
        NestableDelegate d = null;
        String[] msgs = new String[5];
        msgs[0] = "level 1";
        msgs[1] = "level 2";
        msgs[2] = null;
        msgs[3] = "level 4";
        msgs[4] = "level 5";
        n = new NestableDelegateTester1(msgs[0], new NestableDelegateTester2(msgs[1], new NestableDelegateTester1(new NestableDelegateTester2(msgs[3], new Exception(msgs[4])))));
        d = new NestableDelegate(n);
        for(int i = 0; i < msgs.length; i++)
        {
            assertEquals("message " + i, msgs[i], d.getMessage(i));
        }
        assertEquals("message -1", msgs[0], d.getMessage(-1));
        assertEquals("message -1", msgs[msgs.length - 1], d.getMessage(msgs.length + 100));
    }

    public void testNestableDelegateGetThrowable()
    {
        Nestable n = null;
        NestableDelegate d = null;
        String msgs[] = null;
        Class[] throwables = null;
        
        msgs = new String[2];
        msgs[0] = null;
        msgs[1] = "level 2";
        throwables = new Class[2];
        throwables[0] = NestableDelegateTester1.class;
        throwables[1] = Exception.class;
        n = new NestableDelegateTester1(new Exception(msgs[1]));
        d = new NestableDelegate(n);
        doNestableDelegateGetThrowable(d, throwables, msgs);
 
        msgs = new String[5];
        msgs[0] = "level 1";
        msgs[1] = "level 2";
        msgs[2] = null;
        msgs[3] = "level 4";
        msgs[4] = "level 5";
        throwables = new Class[5];
        throwables[0] = NestableDelegateTester1.class;
        throwables[1] = NestableDelegateTester2.class;
        throwables[2] = NestableDelegateTester1.class;
        throwables[3] = NestableDelegateTester2.class;
        throwables[4] = Exception.class;        
        n = new NestableDelegateTester1(msgs[0], new NestableDelegateTester2(msgs[1], new NestableDelegateTester1(new NestableDelegateTester2(msgs[3], new Exception(msgs[4])))));
        d = new NestableDelegate(n);
        doNestableDelegateGetThrowable(d, throwables, msgs);
    }

    private void doNestableDelegateGetThrowable(NestableDelegate d, Class[] classes, String[] msgs)
    {
        Throwable t = null;
        String msg = null;
        
        for(int i = 0; i < classes.length; i++)
        {
            t = d.getThrowable(i);
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
        t = d.getThrowable(-1);
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
        t = d.getThrowable(999);
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

    public void testNestableDelegateGetThrowables()
    {
        Nestable n = null;
        NestableDelegate d = null;
        String msgs[] = null;
        Class[] throwables = null;
        
        msgs = new String[2];
        msgs[0] = null;
        msgs[1] = "level 2";
        throwables = new Class[2];
        throwables[0] = NestableDelegateTester1.class;
        throwables[1] = Exception.class;
        n = new NestableDelegateTester1(new Exception(msgs[1]));
        d = new NestableDelegate(n);
        doNestableDelegateGetThrowables(d, throwables, msgs);
 
        msgs = new String[5];
        msgs[0] = "level 1";
        msgs[1] = "level 2";
        msgs[2] = null;
        msgs[3] = "level 4";
        msgs[4] = "level 5";
        throwables = new Class[5];
        throwables[0] = NestableDelegateTester1.class;
        throwables[1] = NestableDelegateTester2.class;
        throwables[2] = NestableDelegateTester1.class;
        throwables[3] = NestableDelegateTester2.class;
        throwables[4] = Exception.class;        
        n = new NestableDelegateTester1(msgs[0], new NestableDelegateTester2(msgs[1], new NestableDelegateTester1(new NestableDelegateTester2(msgs[3], new Exception(msgs[4])))));
        d = new NestableDelegate(n);
        doNestableDelegateGetThrowables(d, throwables, msgs);
    }
    
    private void doNestableDelegateGetThrowables(NestableDelegate d, Class[] classes, String[] msgs)
    {
        Throwable[] throwables = null;
        String msg = null;

        throwables = d.getThrowables();
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
        NestableDelegate d = null;
        String msgs[] = null;
        Class[] throwables = null;
        
        msgs = new String[5];
        msgs[0] = "level 1";
        msgs[1] = "level 2";
        msgs[2] = null;
        msgs[3] = "level 4";
        msgs[4] = "level 5";
        throwables = new Class[5];
        throwables[0] = NestableDelegateTester1.class;
        throwables[1] = NestableDelegateTester2.class;
        throwables[2] = NestableDelegateTester1.class;
        throwables[3] = NestableDelegateTester2.class;
        throwables[4] = Exception.class;
        int[] indexes = {0, 1, 0, 1, 4};
        n = new NestableDelegateTester1(msgs[0], new NestableDelegateTester2(msgs[1], new NestableDelegateTester1(new NestableDelegateTester2(msgs[3], new Exception(msgs[4])))));
        d = new NestableDelegate(n);
        for(int i = 0; i < throwables.length; i++)
        {
            doNestableDelegateIndexOfThrowable(d, throwables[i], 0, indexes[i], msgs[indexes[i]]);
        }
        doNestableDelegateIndexOfThrowable(d, NestableDelegateTester2.class, 2, 3, msgs[3]);
        doNestableDelegateIndexOfThrowable(d, NestableDelegateTester1.class, 1, 2, msgs[2]);
        doNestableDelegateIndexOfThrowable(d, java.util.Date.class, 0, -1, null);
    }

    private void doNestableDelegateIndexOfThrowable(NestableDelegate d, Class type, int pos, int expectedIndex, String expectedMsg)
    {
        Throwable t = null;
        
        int index = d.indexOfThrowable(pos, type);
        assertEquals("index of throwable " + type.getName(), expectedIndex, index);
        t = d.getThrowable(index);
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
    
    public void testNestableDelegetePrintStackTrace()
    {
        int lineSepLen = lineSeparator.length();
        int partialStackTraceLen = PARTIAL_STACK_TRACE.length();
        Nestable ne3 = new ThrowableNestedNestable(new Exception("nested exception 3"));
        NestableDelegate nd3 = new NestableDelegate(ne3);

        ByteArrayOutputStream baos1 = new ByteArrayOutputStream();
        PrintStream ps1 = new PrintStream(baos1);
        nd3.printStackTrace(ps1);
        String stack1 = baos1.toString();
        assertTrue("stack trace startsWith == java.lang.Exception: nested exception 3",
            stack1.startsWith("java.lang.Exception: nested exception 3"));
        int start1 = (stack1.length() - lineSepLen) - partialStackTraceLen;
        int end1 = stack1.length() - lineSepLen;
        assertEquals("stack trace substring(" + start1 + "," + end1 + ") == " +
                     PARTIAL_STACK_TRACE,
                     PARTIAL_STACK_TRACE,
                     stack1.substring(start1, end1));

        ByteArrayOutputStream baos2 = new ByteArrayOutputStream();
        PrintStream ps2 = new PrintStream(baos2);
        System.setErr(ps2);
        nd3.printStackTrace();
        String stack2 = baos2.toString();
        assertTrue("stack trace startsWith == java.lang.Exception: nested exception 3",
            stack2.startsWith("java.lang.Exception: nested exception 3"));
        int start2 = (stack2.length() - lineSepLen) - partialStackTraceLen;
        int end2 = stack2.length() - lineSepLen;
        assertTrue("stack trace substring(" + start2 + "," + end2 + ") == " + PARTIAL_STACK_TRACE,
            stack2.substring(start2, end2).equals(PARTIAL_STACK_TRACE));
    }
    
    public static void main(String args[])
    {
        TestRunner.run(suite());
    }
}

class NestableDelegateTester1 extends Exception implements Nestable
{
    private Throwable cause = null;

    public NestableDelegateTester1()
    {
        super();
    }

    public NestableDelegateTester1(String reason, Throwable cause)
    {
        super(reason);
        this.cause = cause;
    }
    
    public NestableDelegateTester1(String reason)
    {
        super(reason);
    }
    
    public NestableDelegateTester1(Throwable cause)
    {
        super();
        this.cause = cause;
    }
    
    /**
     * Returns the error message of this and any nested <code>Throwable</code>s
     * in an array of Strings, one element for each message. Any
     * <code>Throwable</code> specified without a message is represented in
     * the array by a null.
     *
     * @return the <code>Throwable</code>s
     */
    public Throwable[] getThrowables()
    {
        return new Throwable[0];
    }
    
    /**
     * Returns the error message of this and any nested <code>Throwable</code>s
     * in an array of Strings, one element for each message. Any
     * <code>Throwable</code> specified without a message is represented in
     * the array by a null.
     *
     * @return the error messages
     */
    public String[] getMessages()
    {
        return new String[0];
    }
    
    /**
     * Returns the index, numbered from 0, of the first occurrence of the
     * specified type in the chain of <code>Throwable</code>s, or -1 if the
     * specified type is not found in the chain. If <code>pos</code> is
     * negative, the effect is the same as if it were 0. If <code>pos</code>
     * is greater than or equal to the length of the chain, the effect is the
     * same as if it were the index of the last element in the chain.
     *
     * @param type <code>Class</code> to be found
     * @return index of the first occurrence of the type in the chain, or -1 if
     * the type is not found
     */
    public int indexOfThrowable(Class type)
    {
        return -1;
    }
    
    /**
     * Returns the <code>Throwable</code> in the chain of
     * <code>Throwable</code>s at the specified index, numbererd from 0. If
     * <code>index</code> is negative, the effect is the same as if it
     * were 0. If <code>index</code> is greater than or equal to the length
     * of the chain, the last <code>Throwable</code> in the chain is returned.
     *
     * @param index the index of the <code>Throwable</code> in the chain of
     * <code>Throwable</code>s
     * @return the <code>Throwable</code>
     */
    public Throwable getThrowable(int index)
    {
        return null;
    }
    
    /**
     * Returns the number of nested <code>Throwable</code>s represented by
     * this <code>Nestable</code>, including this <code>Nestable</code>.
     */
    public int getLength()
    {
        return 1;
    }
    
    /**
     * Returns the reference to the exception or error that caused the
     * exception implementing the <code>Nestable</code> to be thrown.
     */
    public Throwable getCause()
    {
        return cause;
    }
    
    /**
     * Prints the stack trace for this exception only--root cause not
     * included--using the provided writer.  Used by {@link
     * org.apache.commons.lang.exception.NestableDelegate} to write
     * individual stack traces to a buffer.  The implementation of
     * this method should call
     * <code>super.printStackTrace(out);</code> in most cases.
     *
     * @param out The writer to use.
     */
    public void printPartialStackTrace(PrintWriter out)
    {
    }
    
    /**
     * Returns the error message of the <code>Throwable</code> in the chain
     * of <code>Throwable</code>s at the specified index, numbererd from 0.
     * If <code>index</code> is negative, the effect is the same as if it
     * were 0. If <code>index</code> is greater than or equal to the length
     * of the chain, the message of the last <code>Throwable</code> in the
     * chain is returned.
     *
     * @param index the index of the <code>Throwable</code> in the chain of
     * <code>Throwable</code>s
     * @return the error message
     */
    public String getMessage(int index)
    {
        if(index == 0)
        {
            return super.getMessage();
        }
        else
        {
            return "";
        }
    }
    
    /**
     * Returns the index, numbered from 0, of the first <code>Throwable</code>
     * that matches the specified type in the chain of <code>Throwable</code>s
     * with an index greater than or equal to the specified position, or -1 if
     * the type is not found. If <code>pos</code> is negative, the effect is the
     * same as if it were 0. If <code>pos</code> is greater than or equal to the
     * length of the chain, the effect is the same as if it were the index of
     * the last element in the chain.
     *
     * @param type <code>Class</code> to be found
     * @param pos index, numbered from 0, of the starting position in the chain
     * to be searched
     *
     * @return index of the first occurrence of the type in the chain, or -1 if
     * the type is not found
     */
    public int indexOfThrowable(int pos, Class type)
    {
        return -1;
    }
    
}

class NestableDelegateTester2 extends Throwable implements Nestable
{
    private Throwable cause = null;

    public NestableDelegateTester2()
    {
        super();
    }
    
    public NestableDelegateTester2(String reason, Throwable cause)
    {
        super(reason);
        this.cause = cause;
    }
    
    public NestableDelegateTester2(String reason)
    {
        super(reason);
    }
    
    public NestableDelegateTester2(Throwable cause)
    {
        super();
        this.cause = cause;
    }
    
    /**
     * Returns the error message of this and any nested <code>Throwable</code>s
     * in an array of Strings, one element for each message. Any
     * <code>Throwable</code> specified without a message is represented in
     * the array by a null.
     *
     * @return the <code>Throwable</code>s
     */
    public Throwable[] getThrowables()
    {
        return new Throwable[0];
    }
    
    /**
     * Returns the error message of this and any nested <code>Throwable</code>s
     * in an array of Strings, one element for each message. Any
     * <code>Throwable</code> specified without a message is represented in
     * the array by a null.
     *
     * @return the error messages
     */
    public String[] getMessages()
    {
        return new String[0];
    }
    
    /**
     * Returns the index, numbered from 0, of the first occurrence of the
     * specified type in the chain of <code>Throwable</code>s, or -1 if the
     * specified type is not found in the chain. If <code>pos</code> is
     * negative, the effect is the same as if it were 0. If <code>pos</code>
     * is greater than or equal to the length of the chain, the effect is the
     * same as if it were the index of the last element in the chain.
     *
     * @param type <code>Class</code> to be found
     * @return index of the first occurrence of the type in the chain, or -1 if
     * the type is not found
     */
    public int indexOfThrowable(Class type)
    {
        return -1;
    }
    
    /**
     * Returns the <code>Throwable</code> in the chain of
     * <code>Throwable</code>s at the specified index, numbererd from 0. If
     * <code>index</code> is negative, the effect is the same as if it
     * were 0. If <code>index</code> is greater than or equal to the length
     * of the chain, the last <code>Throwable</code> in the chain is returned.
     *
     * @param index the index of the <code>Throwable</code> in the chain of
     * <code>Throwable</code>s
     * @return the <code>Throwable</code>
     */
    public Throwable getThrowable(int index)
    {
        return null;
    }
    
    /**
     * Returns the number of nested <code>Throwable</code>s represented by
     * this <code>Nestable</code>, including this <code>Nestable</code>.
     */
    public int getLength()
    {
        return 1;
    }
    
    /**
     * Returns the reference to the exception or error that caused the
     * exception implementing the <code>Nestable</code> to be thrown.
     */
    public Throwable getCause()
    {
        return cause;
    }
    
    /**
     * Prints the stack trace for this exception only--root cause not
     * included--using the provided writer.  Used by {@link
     * org.apache.commons.lang.exception.NestableDelegate} to write
     * individual stack traces to a buffer.  The implementation of
     * this method should call
     * <code>super.printStackTrace(out);</code> in most cases.
     *
     * @param out The writer to use.
     */
    public void printPartialStackTrace(PrintWriter out)
    {
    }
    
    /**
     * Returns the error message of the <code>Throwable</code> in the chain
     * of <code>Throwable</code>s at the specified index, numbererd from 0.
     * If <code>index</code> is negative, the effect is the same as if it
     * were 0. If <code>index</code> is greater than or equal to the length
     * of the chain, the message of the last <code>Throwable</code> in the
     * chain is returned.
     *
     * @param index the index of the <code>Throwable</code> in the chain of
     * <code>Throwable</code>s
     * @return the error message
     */
    public String getMessage(int index)
    {
        if(index == 0)
        {
            return super.getMessage();
        }
        else
        {
            return "";
        }
    }
    
    /**
     * Returns the index, numbered from 0, of the first <code>Throwable</code>
     * that matches the specified type in the chain of <code>Throwable</code>s
     * with an index greater than or equal to the specified position, or -1 if
     * the type is not found. If <code>pos</code> is negative, the effect is the
     * same as if it were 0. If <code>pos</code> is greater than or equal to the
     * length of the chain, the effect is the same as if it were the index of
     * the last element in the chain.
     *
     * @param type <code>Class</code> to be found
     * @param pos index, numbered from 0, of the starting position in the chain
     * to be searched
     *
     * @return index of the first occurrence of the type in the chain, or -1 if
     * the type is not found
     */
    public int indexOfThrowable(int pos, Class type)
    {
        return -1;
    }
    
}

class ThrowableNestable extends Throwable implements Nestable
{
    private Throwable cause = new Exception("ThrowableNestable cause");

    /**
     * Returns the number of nested <code>Throwable</code>s represented by
     * this <code>Nestable</code>, including this <code>Nestable</code>.
     */
    public int getLength()
    {
        return 1;
    }
    
    /**
     * Returns the error message of this and any nested
     * <code>Throwable</code>.
     *
     * @return The error message.
     */
    public String getMessage()
    {
        return "ThrowableNestable exception";
    }

    /**
     * Returns the error message of the <code>Throwable</code> in the chain
     * of <code>Throwable</code>s at the specified index, numbererd from 0.
     * If <code>index</code> is negative, the effect is the same as if it
     * were 0. If <code>index</code> is greater than or equal to the length
     * of the chain, the message of the last <code>Throwable</code> in the
     * chain is returned.
     *
     * @param index the index of the <code>Throwable</code> in the chain of
     * <code>Throwable</code>s
     * @return the error message
     */
    public String getMessage(int index)
    {
        return getMessage();
    }

    /**
     * Returns the error message of this and any nested <code>Throwable</code>s
     * in an array of Strings, one element for each message. Any
     * <code>Throwable</code> specified without a message is represented in
     * the array by a null.
     */
    public String[] getMessages()
    {
        String msgs[] = new String[1];
        msgs[0] = getMessage();
        return msgs;
    }
    
    /**
     * Returns the reference to the exception or error that caused the
     * exception implementing the <code>Nestable</code> to be thrown.
     */
    public Throwable getCause()
    {
        return cause;
    }

    /**
     * Prints the stack trace of this exception to the specified print
     * writer.  Includes inforamation from the exception--if
     * any--which caused this exception.
     *
     * @param out <code>PrintWriter</code> to use for output.
     */
    public void printStackTrace(PrintWriter out)
    {
    }
    
    /**
     * Prints the stack trace for this exception only--root cause not
     * included--using the provided writer.  Used by {@link
     * org.apache.commons.lang.exception.NestableDelegate} to write
     * individual stack traces to a buffer.  The implementation of
     * this method should call
     * <code>super.printStackTrace(out);</code> in most cases.
     *
     * @param out The writer to use.
     */
    public void printPartialStackTrace(PrintWriter out)
    {
    }
    
    public Throwable getThrowable(int index)
    {
        return cause;
    }
    
    public Throwable[] getThrowables()
    {
        Throwable throwables[] = new Throwable[1];
        throwables[0] = cause;
        return throwables;
    }
    
    public int indexOfThrowable(Class type)
    {
        if(Exception.class.isInstance(type))
        {
            return 0;
        }
        return -1;
    }
    
    /**
     * Returns the index of the first <code>Throwable</code> that matches the
     * specified type with an index greater than or equal to the specified
     * position, or -1 if the type is not found.
     *
     * @param type <code>Class</code> to be found
     * @param pos
     * @return index of the first occurrence of the type in the chain, or -1 if
     * the type is not found
     */
    public int indexOfThrowable(int pos, Class type)
    {
        return indexOfThrowable(type);
    }
    
}

class ThrowableNestedNestable extends Throwable implements Nestable
{
    private Throwable cause = null;
    
    public ThrowableNestedNestable(Throwable cause)
    {
        this.cause = cause;
    }
    
    public int getLength()
    {
        return 1;
    }
    
    /**
     * Returns the error message of this and any nested
     * <code>Throwable</code>.
     *
     * @return The error message.
     */
    public String getMessage()
    {
        return "ThrowableNestedNestable exception (" + cause.getMessage() + ")";
    }

    public String getMessage(int index)
    {
        return "ThrowableNestedNestable exception (" + cause.getMessage() + ")";
    }
    
    /**
     * Returns the error message of this and any nested <code>Throwable</code>s
     * in an array of Strings, one element for each message. Any
     * <code>Throwable</code> specified without a message is represented in
     * the array by a null.
     */
    public String[] getMessages()
    {
        String[] msgs = new String[1];
        msgs[0] = "ThrowableNestedNestable exception (" + cause.getMessage() + ")";
        return msgs;
    }
    
    /**
     * Returns the reference to the exception or error that caused the
     * exception implementing the <code>Nestable</code> to be thrown.
     */
    public Throwable getCause()
    {
        return cause;
    }
    
    /**
     * Prints the stack trace of this exception to the specified print
     * writer.  Includes inforamation from the exception--if
     * any--which caused this exception.
     *
     * @param out <code>PrintWriter</code> to use for output.
     */
    public void printStackTrace(PrintWriter out)
    {
        out.println("ThrowableNestedNestable stack trace place-holder");
    }
    
    /**
     * Prints the stack trace for this exception only--root cause not
     * included--using the provided writer.  Used by {@link
     * org.apache.commons.lang.exception.NestableDelegate} to write
     * individual stack traces to a buffer.  The implementation of
     * this method should call
     * <code>super.printStackTrace(out);</code> in most cases.
     *
     * @param out The writer to use.
     */
    public void printPartialStackTrace(PrintWriter out)
    {
        out.println("ThrowableNestedNestable partial stack trace place-holder");
    }
    
    public Throwable getThrowable(int index)
    {
        return cause;
    }
    
    public Throwable[] getThrowables()
    {
        Throwable throwables[] = new Throwable[1];
        throwables[0] = cause;
        return throwables;
    }
    
    public int indexOfThrowable(Class type)
    {
        if(Exception.class.isInstance(type))
        {
            return 0;
        }
        return -1;
    }
    
    /**
     * Returns the index of the first <code>Throwable</code> that matches the
     * specified type with an index greater than or equal to the specified
     * position, or -1 if the type is not found.
     *
     * @param type <code>Class</code> to be found
     * @param pos
     * @return index of the first occurrence of the type in the chain, or -1 if
     * the type is not found
     */
    public int indexOfThrowable(int pos, Class type)
    {
        return indexOfThrowable(type);
    }
    
}

class NonThrowableNestable implements Nestable
{
    public int getLength()
    {
        return 1;
    }
    
    /**
     * Returns the error message of this and any nested
     * <code>Throwable</code>.
     *
     * @return The error message.
     */
    public String getMessage()
    {
        return "non-throwable";
    }

    public String getMessage(int index)
    {
        return "non-throwable";
    }
    
    /**
     * Returns the error message of this and any nested <code>Throwable</code>s
     * in an array of Strings, one element for each message. Any
     * <code>Throwable</code> specified without a message is represented in
     * the array by a null.
     */
    public String[] getMessages()
    {
        String[] msgs = new String[1];
        msgs[0] = "non-throwable";
        return msgs;
    }
    
    /**
     * Returns the reference to the exception or error that caused the
     * exception implementing the <code>Nestable</code> to be thrown.
     */
    public Throwable getCause()
    {
        return null;
    }
    
    /**
     * Prints the stack trace of this exception to the specified print
     * writer.  Includes inforamation from the exception--if
     * any--which caused this exception.
     *
     * @param out <code>PrintWriter</code> to use for output.
     */
    public void printStackTrace(PrintWriter out)
    {
    }
    
    /**
     * Prints the stack trace for this exception only--root cause not
     * included--using the provided writer.  Used by {@link
     * org.apache.commons.lang.exception.NestableDelegate} to write
     * individual stack traces to a buffer.  The implementation of
     * this method should call
     * <code>super.printStackTrace(out);</code> in most cases.
     *
     * @param out The writer to use.
     */
    public void printPartialStackTrace(PrintWriter out)
    {
    }
    
    public Throwable getThrowable(int index)
    {
        return null;
    }
    
    public Throwable[] getThrowables()
    {
        return new Throwable[0];
    }
    
    public int indexOfThrowable(Class type)
    {
        return -1;
    }
    
    /**
     * Returns the index of the first <code>Throwable</code> that matches the
     * specified type with an index greater than or equal to the specified
     * position, or -1 if the type is not found.
     *
     * @param type <code>Class</code> to be found
     * @param pos
     * @return index of the first occurrence of the type in the chain, or -1 if
     * the type is not found
     */
    public int indexOfThrowable(int pos, Class type)
    {
        return -1;
    }
    
}
