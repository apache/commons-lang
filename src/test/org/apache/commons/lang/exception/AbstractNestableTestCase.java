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
import java.io.PrintStream;
import java.io.PrintWriter;

import junit.framework.TestCase;
/**
 * Tests implementations of the org.apache.commons.lang.exception.Nestable
 * interface.
 *
 * @author <a href="mailto:steven@caswell.name">Steven Caswell</a>
 * @version $Id$
 */
public abstract class AbstractNestableTestCase extends TestCase
{

    /**
     * Constructs an instance of
     * <code>AbstractNestableTestCase</code>.
     *
     * @param name the test name
     */
    public AbstractNestableTestCase(String name)
    {
        super(name);
    }
    
    /**
     * Tests the getCause() operation.
     */
    public void testGetCause()
    {
        Nestable ne1 = getNestable();
        assertNull("nestable exception() cause is null", ne1.getCause()); 
        
        Nestable ne2 = getNestable("ne2");
        assertNull("nestable exception(\"ne2\") cause is null", ne2.getCause());
        
        Nestable ne3 = getNestable(getThrowable("ne3 exception"));
        assertNotNull("nestable exception(Throwable(\"ne3 exception\") cause is not null",
            ne3.getCause()); 
        assertTrue("nestable exception(Throwable(\"ne3 exception\") cause message == ne3 exception",
            ne3.getCause().getMessage().equals("ne3 exception")); 
        
        Nestable ne4 = getNestable("ne4", getThrowable("ne4 exception"));
        assertNotNull("nestable exception(\"ne4\", Throwable(\"ne4 exception\") cause is not null", 
            ne4.getCause()); 
        
        Nestable ne5 = getNestable("ne5", (Throwable) null);
        assertNull("nestable exception(\"ne5\", null) cause is null", 
            ne5.getCause()); 
        
        Nestable ne6 = getNestable(null, getThrowable("ne6 exception"));
        assertNotNull("nestable exception(null, Throwable(\"ne6 exception\") cause is not null", 
            ne6.getCause()); 
    }

    /**
     * Tests the getThrowableCount() operation.
     */
    public void testGetThrowableCount()
    {
        Nestable ne1 = getNestable();
        assertEquals("ne1 throwable count", 1, ne1.getThrowableCount());

        Nestable ne2 = getNestable("ne2");
        assertEquals("ne2 throwable count", 1, ne2.getThrowableCount());
        
        Nestable ne3 = getNestable(getThrowable("ne3 exception"));
        assertEquals("ne3 throwable count", 2, ne3.getThrowableCount());
        
        Nestable ne4 = getNestable("ne4", getThrowable("ne4 exception"));
        assertEquals("ne4 throwable count", 2, ne4.getThrowableCount());
        
        Nestable ne5 = getNestable("ne5", (Throwable) null);
        assertEquals("ne 5 throwable count", 1, ne5.getThrowableCount());
        
        Nestable ne6 = getNestable(null, getThrowable("ne6 exception"));
        assertEquals("ne 6 throwable count", 2, ne6.getThrowableCount());
        
        Nestable ne7 = getNestable("ne7o", getNestable("ne7i", getThrowable("ne7 exception")));
        assertEquals("ne 7 throwable count", 3, ne7.getThrowableCount());

        Nestable ne8 = getNestable("level 1", getNestable("level 2", getNestable(getNestable("level 4", getThrowable("level 5")))));
        assertEquals("ne 8 throwable count", 5, ne8.getThrowableCount());
    }
    
    /**
     * Tests the getMessage() operation.
     */
    public void testGetMessage()
    {
        Nestable ne1 = getNestable();
        assertNull("nestable exception() message is null", ne1.getMessage()); 

        Nestable ne2 = getNestable("ne2");
        assertNotNull("nestable exception(\"ne2\") message is not null", ne2.getMessage());
        assertEquals("nestable exception(\"ne2\") message == ne2", ne2.getMessage(), "ne2");
        
        Nestable ne3 = getNestable(getThrowable("ne3 exception"));
        assertNotNull("nestable exception(Throwable(\"ne3 exception\") message is not null",
            ne3.getMessage()); 
        assertEquals("nestable exception(Throwable(\"ne3 exception\") message equals cause.toString()",
            ne3.getMessage(), ne3.getCause().toString()); 
        
        Nestable ne4 = getNestable("ne4", getThrowable("ne4 exception"));
        assertNotNull("nestable exception(\"ne4\", Throwable(\"ne4 exception\") message is not null", 
            ne4.getMessage()); 
        assertEquals("nestable exception(\"ne4\", Throwable(\"ne4 exception\") message == ne4", 
            ne4.getMessage(), "ne4"); 
        
        Nestable ne5 = getNestable("ne5", (Throwable) null);
        assertNotNull("nestable exception(\"ne5\", null) message is not null", 
            ne5.getMessage()); 
        assertEquals("nestable exception(\"ne5\", null) message == ne5", 
            ne5.getMessage(), "ne5"); 
        
        Throwable t6 = getThrowable("ne6 exception");
        Nestable ne6 = getNestable(null, t6);
        assertNotNull("nestable exception(null, Throwable(\"ne6 exception\") message is not null",
            ne6.getMessage()); 
        assertEquals("nestable exception(null, Throwable(\"ne6 exception\") message equals cause.toString()",
            ne6.getMessage(), ne6.getCause().toString()); 
        
        Nestable ne7 = getNestable("ne7o", getNestable("ne7i", getThrowable("ne7 exception")));
        assertEquals("nestable exception(\"ne7o\", getNestable(\"ne7i\", Throwable(\"ne7 exception\"))) message is ne7o: ne7i: ne7 exception",
            ne7.getMessage(), "ne7o");

        Nestable ne8 = getNestable();
        assertNull("nestable exception() message is null",
            ne8.getMessage());

    }

    /**
     * Tests the getMessage(int) operation.
     */
    public void testGetMessageI()
    {
        String[] msgs = new String[5];
        msgs[0] = "level 1";
        msgs[1] = "level 2";
        msgs[2] = null;
        msgs[3] = "level 4";
        msgs[4] = "level 5";
        Nestable ne = getNestable(msgs[0], getNestable(msgs[1], getNestable(getNestable(msgs[3], getThrowable(msgs[4])))));
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

    /**
     * Tests the getMessages() operation.
     */
    public void testGetMessages()
    {
        String[] msgs = new String[5];
        msgs[0] = "level 1";
        msgs[1] = "level 2";
        msgs[2] = null;
        msgs[3] = "level 4";
        msgs[4] = "level 5";
        Nestable ne = getNestable(msgs[0], getNestable(msgs[1], getNestable(getNestable(msgs[3], getThrowable(msgs[4])))));
        String[] nMsgs = ne.getMessages();
        assertEquals("messages length", msgs.length, nMsgs.length);
        for(int i = 0; i < nMsgs.length; i++)
        {
            assertEquals("message " + i, msgs[i], nMsgs[i]);
        }
    }

    /**
     * Tests the getThrowable(int) operation.
     */
    public void testGetThrowableI()
    {
        Nestable n = null;
        String msgs[] = null;
        Class[] throwables = null;
        
        msgs = new String[2];
        msgs[0] = null;
        msgs[1] = "level 2";
        throwables = new Class[2];
        throwables[0] = getTester1Class();
        throwables[1] = getThrowableClass();
        n = getTester1(getThrowable(msgs[1]));
        doNestableExceptionGetThrowableI(n, throwables, msgs);
 
        msgs = new String[5];
        msgs[0] = "level 1";
        msgs[1] = "level 2";
        msgs[2] = null;
        msgs[3] = "level 4";
        msgs[4] = "level 5";
        throwables = new Class[5];
        throwables[0] = getTester1Class();
        throwables[1] = getTester2Class();
        throwables[2] = getTester1Class();
        throwables[3] = getTester2Class();
        throwables[4] = getThrowableClass();        
        n = getTester1(msgs[0], getTester2(msgs[1], getTester1(getTester2(msgs[3], getThrowable(msgs[4])))));
        doNestableExceptionGetThrowableI(n, throwables, msgs);
    }
    
    private void doNestableExceptionGetThrowableI(Nestable n, Class[] classes, String[] msgs)
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

    /**
     * Tests the getThrowables() operation.
     */
    public void testGetThrowables()
    {
        Nestable n = null;
        String msgs[] = null;
        Class[] throwables = null;
        
        msgs = new String[2];
        msgs[0] = null;
        msgs[1] = "level 2";
        throwables = new Class[2];
        throwables[0] = getTester1Class();
        throwables[1] = getThrowableClass();
        n = getTester1(getThrowable(msgs[1]));
        doNestableExceptionGetThrowables(n, throwables, msgs);
 
        msgs = new String[5];
        msgs[0] = "level 1";
        msgs[1] = "level 2";
        msgs[2] = null;
        msgs[3] = "level 4";
        msgs[4] = "level 5";
        throwables = new Class[5];
        throwables[0] = getTester1Class();
        throwables[1] = getTester2Class();
        throwables[2] = getTester1Class();
        throwables[3] = getTester2Class();
        throwables[4] = getThrowableClass();
        n = getTester1(msgs[0], getTester2(msgs[1], getTester1(getTester2(msgs[3], getThrowable(msgs[4])))));
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

    /**
     * Tests the indexOfThrowable() operation.
     */
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
        throwables[0] = getTester1Class();
        throwables[1] = getTester2Class();
        throwables[2] = getTester1Class();
        throwables[3] = getTester2Class();
        throwables[4] = getThrowableClass();
        int[] indexes = {0, 1, 0, 1, 4};
        n = getTester1(msgs[0], getTester2(msgs[1], getTester1(getTester2(msgs[3], getThrowable(msgs[4])))));
        for(int i = 0; i < throwables.length; i++)
        {
            doNestableExceptionIndexOfThrowable(n, throwables[i], indexes[i], msgs[indexes[i]]);
        }
        doNestableExceptionIndexOfThrowable(n, getBaseThrowableClass(), 0, msgs[0]);
        doNestableExceptionIndexOfThrowable(n, java.util.Date.class, -1, null);
        doNestableExceptionIndexOfThrowable(n, null, -1, null);
    }
    
    private void doNestableExceptionIndexOfThrowable(Nestable n, Class type, int expectedIndex, String expectedMsg)
    {
        Throwable t = null;
        
        int index = n.indexOfThrowable(type);
        assertEquals("index of throwable " + (type == null ? "null" : type.getName()), expectedIndex, index);
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
    
    /**
     * Tests the indexOfThrowable(int) operation.
     */
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
        throwables[0] = getTester1Class();
        throwables[1] = getTester2Class();
        throwables[2] = getTester1Class();
        throwables[3] = getTester2Class();
        throwables[4] = getThrowableClass();
        int[] indexes = {0, 1, 0, 1, 4};
        n = getTester1(msgs[0], getTester2(msgs[1], getTester1(getTester2(msgs[3], getThrowable(msgs[4])))));
        for(int i = 0; i < throwables.length; i++)
        {
            doNestableExceptionIndexOfThrowableI(n, throwables[i], 0, indexes[i], msgs[indexes[i]]);
        }
        doNestableExceptionIndexOfThrowableI(n, getTester2Class(), 2, 3, msgs[3]);
        doNestableExceptionIndexOfThrowableI(n, getTester1Class(), 1, 2, msgs[2]);
        doNestableExceptionIndexOfThrowableI(n, getTester1Class(), 3, -1, null);
        doNestableExceptionIndexOfThrowableI(n, getTester1Class(), 4, -1, null);
        doNestableExceptionIndexOfThrowableI(n, getThrowableClass(), 2, 4, msgs[4]);
        doNestableExceptionIndexOfThrowableI(n, java.util.Date.class, 0, -1, null);
        doNestableExceptionIndexOfThrowableI(n, null, 0, -1, null);
        
        // Test for index out of bounds
        try
        {
            int index = n.indexOfThrowable(getTester1Class(), -1);
            fail("method should have thrown IndexOutOfBoundsException");
        }
        catch(IndexOutOfBoundsException iooob)
        {
        }
        try
        {
            int index = n.indexOfThrowable(getTester1Class(), 5);
            fail("method should have thrown IndexOutOfBoundsException");
        }
        catch(IndexOutOfBoundsException iooob)
        {
        }
        
    }

    private void doNestableExceptionIndexOfThrowableI(Nestable n, Class type, int fromIndex, int expectedIndex, String expectedMsg)
    {
        Throwable t = null;
        
        int index = n.indexOfThrowable(type, fromIndex);
        assertEquals("index of throwable " + (type == null ? "null" : type.getName()), expectedIndex, index);
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

    /**
     * Tests the printPartialStackTrace() operation.
     */
    public void testPrintPartialStackTrace()
    {
        Nestable ne9 = getNestable("ne9", getThrowable("ne9 exception"));
        ByteArrayOutputStream baos2 = new ByteArrayOutputStream();
        PrintStream ps2 = new PrintStream(baos2);
        PrintWriter pw2 = new PrintWriter(ps2, true);
        ne9.printPartialStackTrace(pw2);
        String stack2 = baos2.toString();
        String startsWith = ne9.getClass().getName() + ": ne9";
        assertTrue("stack trace startsWith == " + startsWith,
            stack2.startsWith(startsWith));
        assertEquals("stack trace indexOf rethrown == -1",
            stack2.indexOf("rethrown"), -1);
    }

    /**
     * Tests the printStackTrace() operation.
     */
    public void testPrintStackTrace()
    {
        Nestable ne8 = getNestable("ne8", getThrowable("ne8 exception"));
        ByteArrayOutputStream baos1 = new ByteArrayOutputStream();
        PrintStream ps1 = new PrintStream(baos1);
        PrintWriter pw1 = new PrintWriter(ps1, true);
        ne8.printStackTrace(pw1);
        String stack1 = baos1.toString();
        String startsWith = ne8.getClass().getName() + ": ne8";
        assertTrue("stack trace startsWith == " + startsWith,
            stack1.startsWith(startsWith));
        String indexOf = getThrowableClass().getName() + ": ne8 exception";
        assertTrue("stack trace indexOf " + indexOf + " > -1",
            stack1.indexOf(indexOf) > -1); 
    }

    /**
     * Returns an instance of the <code>Nestable</code> implementation being
     * tested.
     *
     * @return the instance
     */
    public abstract Nestable getNestable();
    
    /**
     * Returns an instance of the <code>Nestable</code> implementation being
     * tested.
     *
     * @param n <code>Nestable</code> argument to be provided to the instance
     * constructor
     * @return the instance
     */
    public abstract Nestable getNestable(Nestable n);
    
    /**
     * Returns an instance of the <code>Nestable</code> implementation being
     * tested.
     *
     * @param msg <code>String</code> argument to be provided to the instance
     * constructor
     * @return the instance
     */
    public abstract Nestable getNestable(String msg);
    
    /**
     * Returns an instance of the <code>Nestable</code> implementation being
     * tested.
     *
     * @param msg <code>String</code> argument to be provided to the instance
     * constructor
     * @param n <code>Nestable</code> argument to be provided to the instance
     * constructor
     * @return the instance
     */
    public abstract Nestable getNestable(String msg, Nestable n);
    
    /**
     * Returns an instance of the <code>Nestable</code> implementation being
     * tested.
     *
     * @param msg <code>String</code> argument to be provided to the instance
     * constructor
     * @param t <code>Throwable</code> argument to be provided to the instance
     * constructor
     * @return the instance
     */
    public abstract Nestable getNestable(String msg, Throwable t);
    
    /**
     * Returns an instance of the <code>Nestable</code> implementation being
     * tested.
     *
     * @param t <code>Throwable</code> argument to be provided to the instance
     * constructor
     * @return the instance
     */
    public abstract Nestable getNestable(Throwable t);
    
    /**
     * Returns an instance of a <code>Throwable</code> to be used in
     * constructing instances of the <code>Nestable</code> implementation being
     * tested.
     *
     * @param msg <code>String</code> argument to be provided to the instance
     * constructor
     * @return the instance
     */
    public abstract Throwable getThrowable(String msg);

    /**
     * Returns an instance of one tester <code>Nestable</code> implementation.
     *
     * @param n <code>Nestable</code> argument to be provided to the instance
     * constructor
     * @return the instance
     */
    public abstract Nestable getTester1(Nestable n);
    
    /**
     * Returns an instance of one tester <code>Nestable</code> implementation.
     *
     * @param t <code>Throwable</code> argument to be provided to the instance
     * constructor
     * @return the instance
     */
    public abstract Nestable getTester1(Throwable t);
    
    /**
     * Returns an instance of one tester <code>Nestable</code> implementation.
     *
     * @param msg <code>String</code> argument to be provided to the instance
     * constructor
     * @param n <code>Nestable</code> argument to be provided to the instance
     * constructor
     * @return the instance
     */
    public abstract Nestable getTester1(String msg, Nestable n);
    
    /**
     * Returns an instance of one tester <code>Nestable</code> implementation.
     *
     * @param msg <code>String</code> argument to be provided to the instance
     * constructor
     * @param t <code>Throwable</code> argument to be provided to the instance
     * constructor
     * @return the instance
     */
    public abstract Nestable getTester1(String msg, Throwable t);
    
    /**
     * Returns an instance of a second tester <code>Nestable</code>
     * implementation.
     *
     * @param msg <code>String</code> argument to be provided to the instance
     * constructor
     * @param n <code>Nestable</code> argument to be provided to the instance
     * constructor
     * @return the instance
     */
    public abstract Nestable getTester2(String msg, Nestable n);
    
    /**
     * Returns an instance of a second tester <code>Nestable</code>
     * implementation.
     *
     * @param msg <code>String</code> argument to be provided to the instance
     * constructor
     * @param t <code>Throwable</code> argument to be provided to the instance
     * constructor
     * @return the instance
     */
    public abstract Nestable getTester2(String msg, Throwable t);

    /**
     * Returns the class of the first tester <code>Nestable</code>
     * implementation.
     *
     * @return the class
     */
    public abstract Class getTester1Class();
    
    /**
     * Returns the class of the second tester <code>Nestable</code>
     * implementation.
     *
     * @return the class
     */
    public abstract Class getTester2Class();

    /**
     * Returns the class of the <code>Throwable</code> used in constructing
     * instances of the <code>Nestable</code> implementation being tested.
     *
     * @return the class
     */
    public abstract Class getThrowableClass();

    /**
     * Returns the base class being used, typically Error, Eception or RuntimeException.
     *
     * @return the class
     */
    public abstract Class getBaseThrowableClass();

}

