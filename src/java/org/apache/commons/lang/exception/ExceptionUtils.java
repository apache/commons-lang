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

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.StringTokenizer;

import org.apache.commons.lang.SystemUtils;

/**
 * Utility routines for manipulating <code>Throwable</code> objects.
 *
 * @author <a href="mailto:dlr@finemaltcoding.com">Daniel Rall</a>
 * @since 1.0
 */
public class ExceptionUtils
{
    /**
     * The names of methods commonly used to access a wrapped
     * exception.
     */
    protected static final String[] CAUSE_METHOD_NAMES =
    {
        "getCause",
        "getNextException",
        "getTargetException",
        "getException",
        "getSourceException",
        "getRootCause",
        "getCausedByException"
    };

    /**
     * The empty parameter list passed to methods used to access a
     * wrapped exception.
     */
    protected static final Object[] CAUSE_METHOD_PARAMS = {};

    /**
     * Constructs a new <code>ExceptionUtils</code>.  Protected to
     * discourage instantiation.
     */
    protected ExceptionUtils()
    {
    }
    
    /**
     * Introspects the specified <code>Throwable</code> for a
     * <code>getCause()</code>, <code>getNextException()</code>,
     * <code>getTargetException()</code>, or
     * <code>getException()</code> method which returns a
     * <code>Throwable</code> object (standard as of JDK 1.4, and part
     * of the {@link
     * org.apache.commons.lang.exception.NestableException} API),
     * extracting and returning the cause of the exception.  In the
     * absence of any such method, the object is inspected for a
     * <code>detail</code> field assignable to a
     * <code>Throwable</code>.  If none of the above is found, returns
     * <code>null</code>.
     *
     * @param t The exception to introspect for a cause.
     * @return The cause of the <code>Throwable</code>.
     */
    public static Throwable getCause(Throwable t)
    {
        return getCause(t, CAUSE_METHOD_NAMES);
    }
    
    /**
     * Extends the API of {@link #getCause(Throwable)} by
     * introspecting for only user-specified method names.
     *
     * @see #getCause(Throwable)
     */
    public static Throwable getCause(Throwable t, String[] methodNames)
    {
        Throwable cause = getCauseUsingWellKnownTypes(t);
        if (cause == null)
        {
            for (int i = 0; i < methodNames.length; i++)
            {
                cause = getCauseUsingMethodName(t, methodNames[i]);
                if (cause != null)
                {
                    break;
                }
            }

            if (cause == null)
            {
                cause = getCauseUsingFieldName(t, "detail");
            }
        }
        return cause;
    }
    
    /**
     * Walks through the exception chain to the last element -- the
     * "root" of the tree -- using {@link #getCause(Throwable)}, and
     * returns that exception.
     *
     * @return The root cause of the <code>Throwable</code>.
     * @see #getCause(Throwable)
     */
    public static Throwable getRootCause(Throwable t)
    {
        Throwable cause = getCause(t);
        if (cause != null)
        {
            t = cause;
            while ((t = getCause(t)) != null)
            {
                cause = t;
            }
        }
        return cause;
    }

    /**
     * Uses <code>instanceof</code> checks to examine the exception,
     * looking for well known types which could contain chained or
     * wrapped exceptions.
     *
     * @param t The exception to examine.
     * @return The wrapped exception, or <code>null</code> if not
     * found.
     */
    private static Throwable getCauseUsingWellKnownTypes(Throwable t)
    {
        if (t instanceof Nestable)
        {
            return ((Nestable) t).getCause();
        }
        else if (t instanceof SQLException)
        {
            return ((SQLException) t).getNextException();
        }
        else if (t instanceof InvocationTargetException)
        {
            return ((InvocationTargetException) t).getTargetException();
        }
        else
        {
            return null;
        }
    }

    /**
     * @param t The exception to examine.
     * @param methodName The name of the method to find and invoke.
     * @return The wrapped exception, or <code>null</code> if not
     * found.
     */
    private static Throwable getCauseUsingMethodName(Throwable t,
                                                     String methodName)
    {
        Method method = null;
        try
        {
            method = t.getClass().getMethod(methodName, null);
        }
        catch (NoSuchMethodException ignored)
        {
        }
        catch (SecurityException ignored)
        {
        }

        if (method != null &&
            Throwable.class.isAssignableFrom(method.getReturnType()))
        {
            try
            {
                return (Throwable) method.invoke(t, CAUSE_METHOD_PARAMS);
            }
            catch (IllegalAccessException ignored)
            {
            }
            catch (IllegalArgumentException ignored)
            {
            }
            catch (InvocationTargetException ignored)
            {
            }
        }
        return null;
    }

    /**
     * @param t The exception to examine.
     * @param fieldName The name of the attribute to examine.
     * @return The wrapped exception, or <code>null</code> if not
     * found.
     */
    private static Throwable getCauseUsingFieldName(Throwable t,
                                                    String fieldName)
    {
        Field field = null;
        try
        {
            field = t.getClass().getField(fieldName);
        }
        catch (NoSuchFieldException ignored)
        {
        }
        catch (SecurityException ignored)
        {
        }

        if (field != null &&
            Throwable.class.isAssignableFrom(field.getType()))
        {
            try
            {
                return (Throwable) field.get(t);
            }
            catch (IllegalAccessException ignored)
            {
            }
            catch (IllegalArgumentException ignored)
            {
            }
        }
        return null;
    }

    /**
     * Returns the number of <code>Throwable</code> objects in the
     * exception chain.
     *
     * @param t The exception to inspect.
     * @return The throwable count.
     */
    public static int getThrowableCount(Throwable t)
    {
        // Count the number of throwables
        int count = 0;
        while (t != null)
        {
            count++;
            t = ExceptionUtils.getCause(t);
        }
        return count;
    }

    /**
     * Returns the list of <code>Throwable</code> objects in the
     * exception chain.
     *
     * @param t The exception to inspect.
     * @return The list of <code>Throwable</code> objects.
     */
    public static Throwable[] getThrowables(Throwable t)
    {
        List list = new ArrayList();
        while (t != null)
        {
            list.add(t);
            t = ExceptionUtils.getCause(t);
        }
        return (Throwable []) list.toArray(new Throwable[list.size()]);
    }

    /**
     * Delegates to {@link #indexOfThrowable(Throwable, Class, int)},
     * starting the search at the beginning of the exception chain.
     *
     * @see #indexOfThrowable(Throwable, Class, int)
     */
    public static int indexOfThrowable(Throwable t, Class type)
    {
        return indexOfThrowable(t, type, 0);
    }

    /**
     * Returns the (zero based) index, of the first
     * <code>Throwable</code> that matches the specified type in the
     * exception chain of <code>Throwable</code> objects with an index
     * greater than or equal to the specified index, or
     * <code>-1</code> if the type is not found.
     *
     * @param t The exception to inspect.
     * @param type <code>Class</code> to look for.
     * @param fromIndex The (zero based) index of the starting
     * position in the chain to be searched.
     * @return index The first occurrence of the type in the chain, or
     * <code>-1</code> if the type is not found.
     * @throws IndexOutOfBoundsException If the <code>fromIndex</code>
     * argument is negative or not less than the count of
     * <code>Throwable</code>s in the chain.
     */
    public static int indexOfThrowable(Throwable t, Class type, int fromIndex)
    {
        if (fromIndex < 0)
        {
            throw new IndexOutOfBoundsException
                ("Throwable index out of range: " + fromIndex);
        }
        Throwable[] throwables = ExceptionUtils.getThrowables(t);
        if (fromIndex >= throwables.length)
        {
            throw new IndexOutOfBoundsException
                ("Throwable index out of range: " + fromIndex);
        }
        for (int i = fromIndex; i < throwables.length; i++)
        {
            if (throwables[i].getClass().equals(type))
            {
                return i;
            }
        }
        return -1;
    }

    /**
     * A convenient way of extracting the stack trace from an
     * exception.
     *
     * @param t The <code>Throwable</code>.
     * @return The stack trace as generated by the exception's
     * <code>printStackTrace(PrintWriter)</code> method.
     */
    public static String getStackTrace(Throwable t)
    {
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw, true);
        t.printStackTrace(pw);
        return sw.getBuffer().toString();
    }

    /**
     * Captures the stack trace associated with the specified
     * <code>Throwable</code> object, decomposing it into a list of
     * stack frames.
     *
     * @param t The <code>Throwable</code>.
     * @return  An array of strings describing each stack frame.
     */
    public static String[] getStackFrames(Throwable t)
    {
        return getStackFrames(getStackTrace(t));
    }

    /**
     * Functionality shared between the
     * <code>getStackFrames(Throwable)</code> methods of this and the
     * {@link org.apache.commons.lang.exception.NestableDelegate}
     * classes.
     */
    static String[] getStackFrames(String stackTrace)
    {
        String linebreak = SystemUtils.LINE_SEPARATOR;
        StringTokenizer frames = new StringTokenizer(stackTrace, linebreak);
        List list = new LinkedList();
        while (frames.hasMoreTokens())
        {
            list.add(frames.nextToken());
        }
        return (String []) list.toArray(new String[] {});
    }
}
