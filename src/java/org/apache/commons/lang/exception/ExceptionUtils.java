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

import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.StringTokenizer;
import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.SystemUtils;

/**
 * <p>Provides utilities for manipulating and examining 
 * <code>Throwable</code> objects.</p>
 *
 * @author <a href="mailto:dlr@finemaltcoding.com">Daniel Rall</a>
 * @author Dmitri Plotnikov
 * @author Stephen Colebourne
 * @author <a href="mailto:ggregory@seagullsw.com">Gary Gregory</a>
 * @since 1.0
 * @version $Id: ExceptionUtils.java,v 1.25 2003/07/14 22:22:46 bayard Exp $
 */
public class ExceptionUtils {
    
    /**
     * <p>Used when printing stack frames to denote the start of a
     * wrapped exception.</p>
     *
     * <p>Package private for accessibility by test suite.</p>
     */
    static final String WRAPPED_MARKER = " [wrapped] ";

    /**
     * <p>The names of methods commonly used to access a wrapped
     * exception.</p>
     */
    protected static String[] CAUSE_METHOD_NAMES = {
        "getCause",
        "getNextException",
        "getTargetException",
        "getException",
        "getSourceException",
        "getRootCause",
        "getCausedByException",
        "getNested"
    };

    /**
     * <p>Constructs a new <code>ExceptionUtils</code>. Protected to
     * discourage instantiation.</p>
     */
    protected ExceptionUtils() {
    }

    /**
     * <p>Adds to the list of method names used in the search for <code>Throwable</code>
     * objects.</p>
     * 
     * @param methodName  the methodName to add to the list, <code>null</code>
     *  and empty strings are ignored
     */
    public static void addCauseMethodName(String methodName) {
      if(StringUtils.isNotEmpty(methodName)) {
            List list = new ArrayList(Arrays.asList(CAUSE_METHOD_NAMES));
            list.add(methodName);
            CAUSE_METHOD_NAMES = (String[]) list.toArray(new String[list.size()]);
        }
    }

    /**
     * <p>Introspects the specified <code>Throwable</code> to obtain the cause.</p>
     * 
     * <p>The method searches for methods with specific names that return a 
     * <code>Throwable</code> object. This will pick up most wrapping exceptions,
     * including those from JDK 1.4, and
     * {@link org.apache.commons.lang.exception.NestableException NestableException}.
     * The method names can be added to using {@link #addCauseMethodName(String)}.</p>
     *
     * <p>The default list searched for are:</p>
     * <ul>
     *  <li><code>getCause()</code></li>
     *  <li><code>getNextException()</code></li>
     *  <li><code>getTargetException()</code></li>
     *  <li><code>getException()</code></li>
     *  <li><code>getSourceException()</code></li>
     *  <li><code>getRootCause()</code></li>
     *  <li><code>getCausedByException()</code></li>
     *  <li><code>getNested()</code></li>
     * </ul>
     * 
     * <p>In the absence of any such method, the object is inspected for a
     * <code>detail</code> field assignable to a <code>Throwable</code>.</p>
     * 
     * <p>If none of the above is found, returns <code>null</code>.</p>
     *
     * @param throwable The exception to introspect for a cause.
     * @return The cause of the <code>Throwable</code>.
     * @throws NullPointerException if the throwable is <code>null</code>
     */
    public static Throwable getCause(Throwable throwable) {
        return getCause(throwable, CAUSE_METHOD_NAMES);
    }

    /**
     * <p>Introspects the specified <code>Throwable</code> to obtain the cause
     * using a supplied array of method names.</p>
     *
     * @param throwable The exception to introspect for a cause.
     * @return The cause of the <code>Throwable</code>.
     * @throws NullPointerException if the method names array is <code>null</code>
     *  or contains <code>null</code>
     * @throws NullPointerException if the throwable is <code>null</code>
     */
    public static Throwable getCause(Throwable throwable, String[] methodNames) {
        Throwable cause = getCauseUsingWellKnownTypes(throwable);
        if (cause == null) {
            for (int i = 0; i < methodNames.length; i++) {
                cause = getCauseUsingMethodName(throwable, methodNames[i]);
                if (cause != null) {
                    break;
                }
            }

            if (cause == null) {
                cause = getCauseUsingFieldName(throwable, "detail");
            }
        }
        return cause;
    }

    /**
     * <p>Walks through the exception chain to the last element -- the
     * "root" of the tree -- using {@link #getCause(Throwable)}, and
     * returns that exception.</p>
     *
     * @param throwable  the throwable to get the root cause for
     * @return The root cause of the <code>Throwable</code>.
     */
    public static Throwable getRootCause(Throwable throwable) {
        Throwable cause = getCause(throwable);
        if (cause != null) {
            throwable = cause;
            while ((throwable = getCause(throwable)) != null) {
                cause = throwable;
            }
        }
        return cause;
    }

    /**
     * <p>Uses <code>instanceof</code> checks to examine the exception,
     * looking for well known types which could contain chained or
     * wrapped exceptions.</p>
     *
     * @param throwable  the exception to examine
     * @return The wrapped exception, or <code>null</code> if not
     * found.
     */
    private static Throwable getCauseUsingWellKnownTypes(Throwable throwable) {
        if (throwable instanceof Nestable) {
            return ((Nestable) throwable).getCause();
        } else if (throwable instanceof SQLException) {
            return ((SQLException) throwable).getNextException();
        } else if (throwable instanceof InvocationTargetException) {
            return ((InvocationTargetException) throwable).getTargetException();
        } else {
            return null;
        }
    }

    /**
     * <p>Finds a <code>Throwable</code> by method name.</p>
     * 
     * @param throwable  the exception to examine
     * @param methodName  the name of the method to find and invoke
     * @return The wrapped exception, or <code>null</code> if not
     *  found.
     */
    private static Throwable getCauseUsingMethodName(Throwable throwable, String methodName) {
        Method method = null;
        try {
            method = throwable.getClass().getMethod(methodName, null);
        } catch (NoSuchMethodException ignored) {
        } catch (SecurityException ignored) {
        }

        if (method != null && Throwable.class.isAssignableFrom(method.getReturnType())) {
            try {
                return (Throwable) method.invoke(throwable, ArrayUtils.EMPTY_OBJECT_ARRAY);
            } catch (IllegalAccessException ignored) {
            } catch (IllegalArgumentException ignored) {
            } catch (InvocationTargetException ignored) {
            }
        }
        return null;
    }

    /**
     * <p>Finds a <code>Throwable</code> by field name.</p>
     * 
     * @param throwable  the exception to examine
     * @param fieldName  the name of the attribute to examine
     * @return The wrapped exception, or <code>null</code> if not
     *  found.
     */
    private static Throwable getCauseUsingFieldName(Throwable throwable, String fieldName) {
        Field field = null;
        try {
            field = throwable.getClass().getField(fieldName);
        } catch (NoSuchFieldException ignored) {
        } catch (SecurityException ignored) {
        }

        if (field != null && Throwable.class.isAssignableFrom(field.getType())) {
            try {
                return (Throwable) field.get(throwable);
            } catch (IllegalAccessException ignored) {
            } catch (IllegalArgumentException ignored) {
            }
        }
        return null;
    }

    /**
     * <p>Returns the number of <code>Throwable</code> objects in the
     * exception chain.</p>
     *
     * @param throwable  the exception to inspect
     * @return The throwable count.
     */
    public static int getThrowableCount(Throwable throwable) {
        // Count the number of throwables
        int count = 0;
        while (throwable != null) {
            count++;
            throwable = ExceptionUtils.getCause(throwable);
        }
        return count;
    }

    /**
     * <p>Returns the list of <code>Throwable</code> objects in the
     * exception chain.</p>
     *
     * @param throwable  the exception to inspect
     * @return The list of <code>Throwable</code> objects.
     */
    public static Throwable[] getThrowables(Throwable throwable) {
        List list = new ArrayList();
        while (throwable != null) {
            list.add(throwable);
            throwable = ExceptionUtils.getCause(throwable);
        }
        return (Throwable[]) list.toArray(new Throwable[list.size()]);
    }

    /**
     * <p>Delegates to {@link #indexOfThrowable(Throwable, Class, int)},
     * starting the search at the beginning of the exception chain.</p>
     *
     * @see #indexOfThrowable(Throwable, Class, int)
     */
    public static int indexOfThrowable(Throwable throwable, Class type) {
        return indexOfThrowable(throwable, type, 0);
    }

    /**
     * <p>Returns the (zero based) index, of the first
     * <code>Throwable</code> that matches the specified type in the
     * exception chain of <code>Throwable</code> objects with an index
     * greater than or equal to the specified index, or
     * <code>-1</code> if the type is not found.</p>
     *
     * @param throwable  the exception to inspect
     * @param type  <code>Class</code> to look for
     * @param fromIndex  the (zero based) index of the starting
     *  position in the chain to be searched
     * @return the first occurrence of the type in the chain, or
     *  <code>-1</code> if the type is not found
     * @throws IndexOutOfBoundsException If the <code>fromIndex</code>
     *  argument is negative or not less than the count of
     *  <code>Throwable</code>s in the chain.
     */
    public static int indexOfThrowable(Throwable throwable, Class type, int fromIndex) {
        if (fromIndex < 0) {
            throw new IndexOutOfBoundsException("Throwable index out of range: " + fromIndex);
        }
        Throwable[] throwables = ExceptionUtils.getThrowables(throwable);
        if (fromIndex >= throwables.length) {
            throw new IndexOutOfBoundsException("Throwable index out of range: " + fromIndex);
        }
        for (int i = fromIndex; i < throwables.length; i++) {
            if (throwables[i].getClass().equals(type)) {
                return i;
            }
        }
        return -1;
    }

    /**
     * <p>Prints a compact stack trace for the root cause of a throwable.</p>
     *
     * <p>The compact stack trace starts with the root cause and prints
     * stack frames up to the place where it was caught and wrapped.
     * Then it prints the wrapped exception and continues with stack frames
     * until the wrapper exception is caught and wrapped again, etc.</p>
     *
     * <p>The method is equivalent to t.printStackTrace() for throwables
     * that don't have nested causes.</p>
     */
    public static void printRootCauseStackTrace(Throwable t, PrintStream stream) {
        String trace[] = getRootCauseStackTrace(t);
        for (int i = 0; i < trace.length; i++) {
            stream.println(trace[i]);
        }
        stream.flush();
    }

    /**
     * <p>Equivalent to <code>printRootCauseStackTrace(t, System.err);</code></p>
     * 
     * @see #printRootCauseStackTrace(Throwable,PrintWriter)
     */
    public static void printRootCauseStackTrace(Throwable t) {
        printRootCauseStackTrace(t, System.err);
    }

    /**
     * <p>Same as {@link #printRootCauseStackTrace(Throwable,java.io.PrintStream)},
     * except it takes a <code>PrintWriter</code> as an argument.</p>
     */
    public static void printRootCauseStackTrace(Throwable t, PrintWriter writer) {
        String trace[] = getRootCauseStackTrace(t);
        for (int i = 0; i < trace.length; i++) {
            writer.println(trace[i]);
        }
        writer.flush();
    }

    /**
     * <p>Creates a compact stack trace for the root cause of the supplied
     * <code>Throwable</code>.</p>
     */
    public static String[] getRootCauseStackTrace(Throwable t) {
        Throwable throwables[] = getThrowables(t);
        int count = throwables.length;
        ArrayList frames = new ArrayList();
        List nextTrace = getStackFrameList(throwables[count - 1]);
        for (int i = count; --i >= 0;) {
            List trace = nextTrace;
            if (i != 0) {
                nextTrace = getStackFrameList(throwables[i - 1]);
                removeCommonFrames(trace, nextTrace);
            }
            if (i == count - 1) {
                frames.add(throwables[i].toString());
            } else {
                frames.add(WRAPPED_MARKER + throwables[i].toString());
            }
            for (int j = 0; j < trace.size(); j++) {
                frames.add(trace.get(j));
            }
        }
        return (String[]) frames.toArray(new String[0]);
    }

    /**
     * <p>Removes common frames from the cause trace given the two stack traces.</p>
     * 
     * @param causeFrames  stack trace of a cause throwable
     * @param wrapperFrames stack trace of a wrapper throwable 
     */
    public static void removeCommonFrames(List causeFrames, List wrapperFrames) {
        int causeFrameIndex = causeFrames.size() - 1;
        int wrapperFrameIndex = wrapperFrames.size() - 1;
        while (causeFrameIndex >= 0 && wrapperFrameIndex >= 0) {
            // Remove the frame from the cause trace if it is the same
            // as in the wrapper trace
            String causeFrame = (String) causeFrames.get(causeFrameIndex);
            String wrapperFrame = (String) wrapperFrames.get(wrapperFrameIndex);
            if (causeFrame.equals(wrapperFrame)) {
                causeFrames.remove(causeFrameIndex);
            }
            causeFrameIndex--;
            wrapperFrameIndex--;
        }
    }

    /**
     * <p>A convenient way of extracting the stack trace from an
     * exception.</p>
     *
     * @param t The <code>Throwable</code>.
     * @return The stack trace as generated by the exception's
     *  <code>printStackTrace(PrintWriter)</code> method.
     */
    public static String getStackTrace(Throwable t) {
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw, true);
        t.printStackTrace(pw);
        return sw.getBuffer().toString();
    }

    /**
     * <p>A way to get the entire nested stack-trace of an throwable.</p>
     *
     * @param t The <code>Throwable</code>.
     * @return The nested stack trace, with the root cause first.
     */
    public static String getFullStackTrace(Throwable t) {
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw, true);
        Throwable[] ts = getThrowables(t);
        for(int i=0; i<ts.length; i++) {
            ts[i].printStackTrace(pw);
            if(isNestedThrowable(ts[i])) {
                break;
            }
        }
        return sw.getBuffer().toString();
    }

    /**
     * <p>Returns whether a <code>Throwable</code> is considered nested
     * or not.</p>
     *
     * @param throwable  The <code>Throwable</code>.
     * @return boolean <code>true</code> if nested otherwise <code>false</code>
     */
    public static boolean isNestedThrowable(Throwable throwable) {
        if(throwable == null) {
            return false;
        }

        if (throwable instanceof Nestable) {
            return true;
        } else if (throwable instanceof SQLException) {
            return true;
        } else if (throwable instanceof InvocationTargetException) {
            return true;
        }

        int sz = CAUSE_METHOD_NAMES.length;
        for(int i=0; i<sz; i++) {
            try {
                Method method = throwable.getClass().getMethod(CAUSE_METHOD_NAMES[i], null);
                if(method != null) {
                    return true;
                }
            } catch (NoSuchMethodException ignored) {
            } catch (SecurityException ignored) {
            }
        }

        try {
            Field field = throwable.getClass().getField("detail");
            if(field != null) {
                return true;
            }
        } catch (NoSuchFieldException ignored) {
        } catch (SecurityException ignored) {
        }

        return false;
    }

    /**
     * <p>Captures the stack trace associated with the specified
     * <code>Throwable</code> object, decomposing it into a list of
     * stack frames.</p>
     *
     * @param t The <code>Throwable</code>.
     * @return  An array of strings describing each stack frame.
     */
    public static String[] getStackFrames(Throwable t) {
        return getStackFrames(getStackTrace(t));
    }

    /**
     * <p>Functionality shared between the
     * <code>getStackFrames(Throwable)</code> methods of this and the
     * {@link org.apache.commons.lang.exception.NestableDelegate}
     * classes.</p>
     */
    static String[] getStackFrames(String stackTrace) {
        String linebreak = SystemUtils.LINE_SEPARATOR;
        StringTokenizer frames = new StringTokenizer(stackTrace, linebreak);
        List list = new LinkedList();
        while (frames.hasMoreTokens()) {
            list.add(frames.nextToken());
        }
        return (String[]) list.toArray(new String[] {
        });
    }

    /**
     * <p>Produces a <code>List</code> of stack frames - the message
     * is not included.</p>
     *
     * <p>This works in most cases - it will only fail if the exception
     * message contains a line that starts with:
     * <code>&quot;&nbsp;&nbsp;&nbsp;at&quot;.</code></p>
     * 
     * @param t is any throwable
     * @return List of stack frames
     */
    static List getStackFrameList(Throwable t) {
        String stackTrace = getStackTrace(t);
        String linebreak = SystemUtils.LINE_SEPARATOR;
        StringTokenizer frames = new StringTokenizer(stackTrace, linebreak);
        List list = new LinkedList();
        boolean traceStarted = false;
        while (frames.hasMoreTokens()) {
            String token = frames.nextToken();
            // Determine if the line starts with <whitespace>at
            int at = token.indexOf("at");
            if (at != -1 && token.substring(0, at).trim().length() == 0) {
                traceStarted = true;
                list.add(token);
            } else if (traceStarted) {
                break;
            }
        }
        return list;
    }
    
    private static Object getCauseMethod = null;
    static {
        try {
            getCauseMethod = Throwable.class.getMethod("getCause", null);
        } catch (Exception e) {
            // ignore
        }
    }
    
    /**
     * <p>Checks if the Throwable class has a <code>getCause</code> method.</p>
     */
    public static boolean isThrowableNested() {
        return (getCauseMethod != null);
    }
}
