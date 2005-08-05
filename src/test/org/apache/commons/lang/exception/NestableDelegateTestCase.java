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
import java.io.IOException;
import java.io.PrintStream;
import java.io.PrintWriter;

import junit.framework.Test;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * Tests the org.apache.commons.lang.exception.NestableDelegate class.
 *
 * @author <a href="mailto:steven@caswell.name">Steven Caswell</a>
 * @author <a href="mailto:dlr@finemaltcoding.com">Daniel Rall</a>
 * @version $Id$
 */
public class NestableDelegateTestCase extends junit.framework.TestCase {
    private static final String CONSTRUCTOR_FAILED_MSG = 
    "The Nestable implementation passed to the NestableDelegate(Nestable) constructor must extend java.lang.Throwable";

    private static final String PARTIAL_STACK_TRACE =
        "ThrowableNestedNestable partial stack trace place-holder";

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

    public void testNestableDelegateGetThrowableCount()
    {
        Nestable n = null;
        NestableDelegate d = null;
        
        n = new NestableDelegateTester1();
        d = new NestableDelegate(n);
        doNestableDelegateGetThrowableCount(d, 1);
        
        n = new NestableDelegateTester1("level 1");
        d = new NestableDelegate(n);
        doNestableDelegateGetThrowableCount(d, 1);
        
        n = new NestableDelegateTester1(new Exception());
        d = new NestableDelegate(n);
        doNestableDelegateGetThrowableCount(d, 2);
        
        n = new NestableDelegateTester1(new Exception("level 2"));
        d = new NestableDelegate(n);
        doNestableDelegateGetThrowableCount(d, 2);
        
        n = new NestableDelegateTester1("level 1", 
                new NestableDelegateTester2("level 2", 
                    new NestableDelegateTester1(
                        new NestableDelegateTester2("level 4", 
                            new Exception("level 5")
                        )
                    )
                )
            );
        d = new NestableDelegate(n);
        doNestableDelegateGetThrowableCount(d, 5);
    }

    private void doNestableDelegateGetThrowableCount(NestableDelegate d, int len)
    {
        // Compare the lengths
        assertEquals("delegate length", len, d.getThrowableCount());
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
        n = new NestableDelegateTester1(msgs[0], 
                new NestableDelegateTester2(msgs[1], 
                    new NestableDelegateTester1(
                        new NestableDelegateTester2(msgs[3], 
                            new Exception(msgs[4])
                        )
                    )
                )
            );
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
    
    public void testGetMessageString()
    {
        NestableDelegateTester1 ndt1 = new NestableDelegateTester1 (new NullPointerException ());
        NestableDelegate nd = new NestableDelegate (ndt1);
        assertNull (nd.getMessage((String)null));
        
        ndt1 = new NestableDelegateTester1 (new NullPointerException ("null pointer"));
        nd = new NestableDelegate (ndt1);
        assertNotNull(nd.getMessage((String)null));
        
        ndt1 = new NestableDelegateTester1 ();
        nd = new NestableDelegate (ndt1);
        assertNull(nd.getMessage((String)null));
        
        ndt1 = new NestableDelegateTester1 ("root");
        nd = new NestableDelegate (ndt1);
        assertNull(nd.getMessage((String)null));
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
        n = new NestableDelegateTester1(msgs[0], 
                new NestableDelegateTester2(msgs[1], 
                    new NestableDelegateTester1(
                        new NestableDelegateTester2(msgs[3], 
                            new Exception(msgs[4])
                        )
                    )
                )
            );
        d = new NestableDelegate(n);
        for(int i = 0; i < msgs.length; i++)
        {
            assertEquals("message " + i, msgs[i], d.getMessage(i));
        }
        
        // Test for index out of bounds
        try
        {
            String msg = d.getMessage(-1);
            fail("getMessage(-1) should have thrown IndexOutOfBoundsException");
        }
        catch(IndexOutOfBoundsException ioode)
        {
        }
        try
        {
            String msg = d.getMessage(msgs.length + 100);
            fail("getMessage(999) should have thrown IndexOutOfBoundsException");
        }
        catch(IndexOutOfBoundsException ioode)
        {
        }
    }

    public void testNestableDelegateGetThrowableN()
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
        doNestableDelegateGetThrowableN(d, throwables, msgs);
 
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
        n = new NestableDelegateTester1(msgs[0], 
                new NestableDelegateTester2(msgs[1], 
                    new NestableDelegateTester1(
                        new NestableDelegateTester2(msgs[3], 
                            new Exception(msgs[4])
                            )
                        )
                    )
                );
        d = new NestableDelegate(n);
        doNestableDelegateGetThrowableN(d, throwables, msgs);
    }

    private void doNestableDelegateGetThrowableN(NestableDelegate d, Class[] classes, String[] msgs)
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
        
        // Test for index out of bounds
        try
        {
            t = d.getThrowable(-1);
            fail("getThrowable(-1) should have thrown IndexOutOfBoundsException");
        }
        catch(IndexOutOfBoundsException ioobe)
        {
        }
        try
        {
            t = d.getThrowable(999);
            fail("getThrowable(999) should have thrown IndexOutOfBoundsException");
        }
        catch(IndexOutOfBoundsException ioobe)
        {
        }
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
        n = new NestableDelegateTester1(msgs[0], 
                new NestableDelegateTester2(msgs[1], 
                    new NestableDelegateTester1(
                        new NestableDelegateTester2(msgs[3], 
                            new Exception(msgs[4])
                        )
                    )
                )
            );
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
        throwables[4] = EOFException.class;
        int[] indexes = {0, 1, 0, 1, 4};
        n = new NestableDelegateTester1(msgs[0], 
                new NestableDelegateTester2(msgs[1], 
                    new NestableDelegateTester1(
                        new NestableDelegateTester2(msgs[3], 
                            new EOFException(msgs[4])
                        )
                    )
                )
            );
        d = new NestableDelegate(n);
        for(int i = 0; i < throwables.length; i++)
        {
            doNestableDelegateIndexOfThrowable(d, throwables[i], 0, indexes[i], msgs[indexes[i]]);
        }
        doNestableDelegateIndexOfThrowable(d, NestableDelegateTester2.class, 2, 3, msgs[3]);
        doNestableDelegateIndexOfThrowable(d, NestableDelegateTester1.class, 1, 2, msgs[2]);
        doNestableDelegateIndexOfThrowable(d, NestableDelegateTester1.class, 3, -1, null);
        doNestableDelegateIndexOfThrowable(d, NestableDelegateTester1.class, 4, -1, null);
        doNestableDelegateIndexOfThrowable(d, EOFException.class, 2, 4, msgs[4]);
        doNestableDelegateIndexOfThrowable(d, IOException.class, 2, 4, msgs[4]);
        doNestableDelegateIndexOfThrowable(d, Exception.class, 2, 2, msgs[2]);
        doNestableDelegateIndexOfThrowable(d, Exception.class, 0, 0, msgs[0]);
        doNestableDelegateIndexOfThrowable(d, java.util.Date.class, 0, -1, null);
        doNestableDelegateIndexOfThrowable(d, null, 0, -1, null);
        
        // Test for index out of bounds
        try
        {
            int index = d.indexOfThrowable(NestableDelegateTester1.class, -1);
            fail("method should have thrown IndexOutOfBoundsException");
        }
        catch(IndexOutOfBoundsException iooob)
        {
        }
        try
        {
            int index = d.indexOfThrowable(NestableDelegateTester1.class, 5);
            fail("method should have thrown IndexOutOfBoundsException");
        }
        catch(IndexOutOfBoundsException iooob)
        {
        }
    }

    private void doNestableDelegateIndexOfThrowable(NestableDelegate d, Class type, int fromIndex, int expectedIndex, String expectedMsg)
    {
        Throwable t = null;
        
        int index = d.indexOfThrowable(type, fromIndex);
        assertEquals("index of throwable " + (type == null ? "null" : type.getName()), expectedIndex, index);
        if(expectedIndex > -1)
        {
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
        assertTrue("stack trace startsWith", stack1.startsWith(PARTIAL_STACK_TRACE));

        Nestable n = new NestableDelegateTester1("level 1", 
                new NestableDelegateTester2("level 2", 
                    new NestableDelegateTester1(
                        new NestableDelegateTester2("level 4", 
                            new Exception("level 5")
                        )
                    )
                )
            );
        NestableDelegate d = new NestableDelegate(n);
        
        // Only testing the flags for jdk1.3 and below
        if (!ExceptionUtils.isThrowableNested()) {
            NestableDelegate.topDown = true; NestableDelegate.trimStackFrames = true;
            checkStackTrace(d, true, true, NestableDelegateTester1.class.getName()+": level 1", 24);
            NestableDelegate.topDown = true; NestableDelegate.trimStackFrames = false;
            checkStackTrace(d, true, false, NestableDelegateTester1.class.getName()+": level 1", 80);
            NestableDelegate.topDown = false; NestableDelegate.trimStackFrames = true;
            checkStackTrace(d, false, true, "java.lang.Exception: level 5", 24);
            NestableDelegate.topDown = false; NestableDelegate.trimStackFrames = false;
            checkStackTrace(d, false, false, "java.lang.Exception: level 5", 80);
            NestableDelegate.topDown = true; NestableDelegate.trimStackFrames = true;
        }
    }
    private void checkStackTrace(NestableDelegate d, boolean topDown, boolean trimStackFrames,
            String startsWith, int expCount) {
        ByteArrayOutputStream baos1 = new ByteArrayOutputStream();
        PrintStream ps1 = new PrintStream(baos1);
        d.printStackTrace(ps1);
        String stack1 = baos1.toString();
        int actCount = countLines(stack1);
        assertTrue("topDown: "+topDown+", trimStackFrames: "+trimStackFrames+" startsWith",
            stack1.startsWith(startsWith));
        // test is unreliable, as count varies depending on JUnit version/where main method is
//        assertEquals("topDown: "+topDown+", trimStackFrames: "+trimStackFrames+" lineCount",
//            expCount, actCount);
    }
    private int countLines(String s) {
        if (s == null) return 0;
        
        int i = 0, ndx = -1;
        while ((ndx = s.indexOf("\n", ndx+1)) != -1) {
            i++;
        }
        return i;
    }
    
    public static void main(String args[])
    {
        TestRunner.run(suite());
    }
}

/**
 * Nestable and Throwable class which can be passed to the NestableDelegate
 * constructor. Used for testing various methods which iterate through the
 * nested causes.
 */
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
     * @see Nestable#getThrowables()
     * Returns zero-length <code>Throwable</code> array for this test.
     */
    public Throwable[] getThrowables()
    {
        return new Throwable[0];
    }
    
    /**
     * @see Nestable#getMessages()
     * Returns zero-length String array for this test.
     */
    public String[] getMessages()
    {
        return new String[0];
    }
    
    /**
     * @see Nestable#indexOfThrowable(Class)
     * Returns -1 for this test.
     */
    public int indexOfThrowable(Class type)
    {
        return -1;
    }
    
    /**
     * @see Nestable#getThrowable(int)
     * Returns <code>null</code> for this test.
     */
    public Throwable getThrowable(int index)
    {
        return null;
    }
    
    /**
     * @see Nestable#getThrowableCount()
     * Returns 1 for this test.
     */
    public int getThrowableCount()
    {
        return 1;
    }
    
    /**
     * @see Nestable#getCause()
     */
    public Throwable getCause()
    {
        return cause;
    }
    
    /**
     * Empty method to satisfy the implemented interface. Does nothing
     * in this test.
     *
     * @param out The writer to use.
     */
    public void printPartialStackTrace(PrintWriter out)
    {
        super.printStackTrace(out);
    }
    
    /**
     * @see Nestable#getMessage(int)
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
     * @see Nestable#indexOfThrowable(Class, int)
     * Returns -1 for this test.
     */
    public int indexOfThrowable(Class type, int fromIndex)
    {
        return -1;
    }
    
}

/**
 * Nestable and Throwable class which can be passed to the NestableDelegate
 * constructor. Used for testing various methods which iterate through the
 * nested causes.
 */
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
     * @see Nestable#getThrowables()
     * Returns zero-length <code>Throwable</code> array for this test.
     */
    public Throwable[] getThrowables()
    {
        return new Throwable[0];
    }
    
    /**
     * @see Nestable#getMessages()
     * Returns zero-length String array for this test.
     */
    public String[] getMessages()
    {
        return new String[0];
    }
    
    /**
     * @see Nestable#indexOfThrowable(Class)
     * Returns -1 for this test.
     */
    public int indexOfThrowable(Class type)
    {
        return -1;
    }
    
    /**
     * @see Nestable#getThrowable(int)
     * Returns <code>null</code> for this test.
     */
    public Throwable getThrowable(int index)
    {
        return null;
    }
    
    /**
     * @see Nestable#getThrowableCount()
     * Returns 1 for this test.
     *
     * @return 1
     */
    public int getThrowableCount()
    {
        return 1;
    }
    
    /**
     * @see Nestable#getCause()
     */
    public Throwable getCause()
    {
        return cause;
    }
    
    /**
     * Empty method to satisfy the implemented interface. Does nothing
     * in this test.
     *
     * @param out The writer to use.
     */
    public void printPartialStackTrace(PrintWriter out)
    {
        super.printStackTrace(out);
    }
    
    /**
     * @see Nestable#getMessage(int)
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
     * @see Nestable#indexOfThrowable(Class, int)     
     * Returns -1 for this test.
     */
    public int indexOfThrowable(Class type, int fromIndex)
    {
        return -1;
    }
    
}

/**
 * Used to test that the constructor passes when passed a throwable cause
 * And, the NestableDelegate.getMessage() returns the message from underlying 
 * nestable (which also has to be a Throwable).
 */
class ThrowableNestable extends Throwable implements Nestable
{
    private Throwable cause = new Exception("ThrowableNestable cause");

    /**
     * @see Nestable#getThrowableCount()
     * Returns 1 for this test.
     */
    public int getThrowableCount()
    {
        return 1;
    }
    
    /**
     * @see Nestable#getMessage()
     * Returns the hard-coded string "ThrowableNestable exception" for this
     * test.
     */
    public String getMessage()
    {
        return "ThrowableNestable exception";
    }

    /**
     * @see Nestable#getMessage(int)
     * Returns the hard-coded string "ThrowableNestable exception" for this
     * test.
     */
    public String getMessage(int index)
    {
        return getMessage();
    }

    /**
     * @see Nestable#getMessages()
     * Returns single-element string array with "ThrowableNestable exception".
     */
    public String[] getMessages()
    {
        String msgs[] = new String[1];
        msgs[0] = getMessage();
        return msgs;
    }
    
    /**
     * @see Nestable#getCause()
     */
    public Throwable getCause()
    {
        return cause;
    }

    /**
     * @see Nestable#printStackTrace(PrintWriter)
     * Empty method to satisfy the implemented interface. Does nothing
     * in this test.
     */
    public void printStackTrace(PrintWriter out)
    {
    }
    
    /**
     * @see Nestable#printPartialStackTrace(PrintWriter)
     * Empty method to satisfy the implemented interface. Does nothing
     * in this test.
     */
    public void printPartialStackTrace(PrintWriter out)
    {
    }
    
    /**
     * @see Nestable#getThrowable(int)
     */
    public Throwable getThrowable(int index)
    {
        return cause;
    }
    
    /**
     * @see Nestable#getThrowables()
     */
    public Throwable[] getThrowables()
    {
        Throwable throwables[] = new Throwable[1];
        throwables[0] = cause;
        return throwables;
    }
    
    /**
     * @see Nestable#indexOfThrowable(Class)
     */
    public int indexOfThrowable(Class type)
    {
        if(Exception.class.isInstance(type))
        {
            return 0;
        }
        return -1;
    }
    
    /**
     * @see Nestable#indexOfThrowable(Class,int)
     */
    public int indexOfThrowable(Class type, int fromIndex)
    {
        return indexOfThrowable(type);
    }
    
}

/**
 * Nestable and Throwable class which takes in a 'cause' object.
 * Returns a message wrapping the 'cause' message
 * Prints a fixed stack trace and partial stack trace.
 */
class ThrowableNestedNestable extends Throwable implements Nestable
{
    private Throwable cause = null;
    
    public ThrowableNestedNestable(Throwable cause)
    {
        this.cause = cause;
    }
    
    /**
     * @see Nestable#getThrowableCount()
     * Returns 1 for this test.
     */
    public int getThrowableCount()
    {
        return 1;
    }
    
    /**
     * @see Nestable#getMessage()
     * For this test, returns "ThrowableNestable exception (" appended to the
     * message of the cause specified in the constructor.
     */
    public String getMessage()
    {
        return "ThrowableNestedNestable exception (" + cause.getMessage() + ")";
    }

    /**
     * @see Nestable#getMessage(int)
     * For this test, returns "ThrowableNestable exception (" appended to the
     * message of the cause specified in the constructor.
     */
    public String getMessage(int index)
    {
        return "ThrowableNestedNestable exception (" + cause.getMessage() + ")";
    }
    
    /**
     * @see Nestable#getMessages()
     * For this test, returns a single-element string array containing
     * "ThrowableNestable exception (" appended to the
     * message of the cause specified in the constructor.
     */
    public String[] getMessages()
    {
        String[] msgs = new String[1];
        msgs[0] = "ThrowableNestedNestable exception (" + cause.getMessage() + ")";
        return msgs;
    }
    
    /**
     * @see Nestable#getCause()
     */
    public Throwable getCause()
    {
        return cause;
    }
    
    /**
     * @see Nestable#printStackTrace(PrintWriter)
     * For this test, writes the string
     * "ThrowableNestedNestable stack trace place-holder" to the print writer.
     */
    public void printStackTrace(PrintWriter out)
    {
        out.println("ThrowableNestedNestable stack trace place-holder");
    }
    
    /**
     * @see Nestable#printPartialStackTrace(PrintWriter)
     * For this test, writes the string
     * "ThrowableNestedNestable partial stack trace place-holder" to the print
     * writer.
     */
    public void printPartialStackTrace(PrintWriter out)
    {
        out.println("ThrowableNestedNestable partial stack trace place-holder");
    }
    
    /**
     * @see Nestable#getThrowable(int)
     */
    public Throwable getThrowable(int index)
    {
        return cause;
    }
    
    /**
     * @see Nestable#getThrowableS()
     */
    public Throwable[] getThrowables()
    {
        Throwable throwables[] = new Throwable[1];
        throwables[0] = cause;
        return throwables;
    }
    
    /**
     * @see Nestable#indexOfThrowable(Class)
     */
    public int indexOfThrowable(Class type)
    {
        if(Exception.class.isInstance(type))
        {
            return 0;
        }
        return -1;
    }
    
    /**
     * @see Nestable#indexOfThrowable(Class, int)
     */
    public int indexOfThrowable(Class type, int fromIndex)
    {
        return indexOfThrowable(type);
    }
    
}

/**
 * Used to test that the constructor fails when passed a non-throwable cause
 */
class NonThrowableNestable implements Nestable
{
    /**
     * @see Nestable#getThrowableCount()
     * Returns 1 for this test.
     */
    public int getThrowableCount()
    {
        return 1;
    }
    
    /**
     * @see Nestable#getMessage()
     * Returns the string "non-throwable" for this test.
     */
    public String getMessage()
    {
        return "non-throwable";
    }

    /**
     * @see Nestable#getMessage(int)
     * Returns the string "non-throwable" for this test.
     */
    public String getMessage(int index)
    {
        return "non-throwable";
    }
    
    /**
     * @see Nestable#getMessage()
     * Returns a single-element array containing the string "non-throwable" for
     * this test.
     */
    public String[] getMessages()
    {
        String[] msgs = new String[1];
        msgs[0] = "non-throwable";
        return msgs;
    }
    
    /**
     * @see Nestable#getCause()
     * Returns <code>null</code> for this test.
     */
    public Throwable getCause()
    {
        return null;
    }
    
    /**
     * @see Nestable#printStackTrace(PrintWriter)
     * Empty method to satisfy the implemented interface. Does nothing
     * in this test.
     */
    public void printStackTrace(PrintWriter out)
    {
    }
    
    /**
     * @see Nestable#printStackTrace(PrintStream)
     * Empty method to satisfy the implemented interface. Does nothing
     * in this test.
     */
    public void printStackTrace(PrintStream out)
    {
    }
    
    /**
     * @see Nestable#printPartialStackTrace(PrintWriter)
     * Empty method to satisfy the implemented interface. Does nothing
     * in this test.
     */
    public void printPartialStackTrace(PrintWriter out)
    {
    }
    

    /**
     * @see Nestable#getThrowable(int)
     * Returns <code>null</code> for this test.
     */
    public Throwable getThrowable(int index)
    {
        return null;
    }
    
    /**
     * @see Nestable#getThrowables()
     * Returns zero-length <code>Throwable</code> array.
     */
    public Throwable[] getThrowables()
    {
        return new Throwable[0];
    }
    
    /**
     * @see Nestable#indexOfThrowable(Class)
     * Returns -1 for this test.
     */
    public int indexOfThrowable(Class type)
    {
        return -1;
    }
    
    /**
     * @see Nestable#indexOfThrowable(Class, int)
     * Returns -1 for this test.
     */
    public int indexOfThrowable(Class type, int fromIndex)
    {
        return -1;
    }
    
}
