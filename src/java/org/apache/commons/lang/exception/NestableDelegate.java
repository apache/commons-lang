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

import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.StringWriter;

/**
 * @author <a href="mailto:Rafal.Krzewski@e-point.pl">Rafal Krzewski</a>
 * @author <a href="mailto:dlr@collab.net">Daniel Rall</a>
 * @author <a href="mailto:knielsen@apache.org">Kasper Nielsen</a>
 * @author <a href="mailto:steven@caswell.name">Steven Caswell</a>
 * @author Sean C. Sullivan
 * @version $Id: NestableDelegate.java,v 1.9.2.1 2002/11/22 20:11:30 bayard Exp $
 */
public class NestableDelegate
	implements java.io.Serializable
{
    /**
     * Constructor error message.
     */
    private transient static final String MUST_BE_THROWABLE =
        "The Nestable implementation passed to the NestableDelegate(Nestable) "
        + "constructor must extend java.lang.Throwable";

    /**
     * Holds the reference to the exception or error that we're
     * wrapping (which must be a {@link
     * org.apache.commons.lang.exception.Nestable} implementation).
     */
    private Throwable nestable = null;

    /**
     * Constructs a new <code>NestableDelegate</code> instance to manage the
     * specified <code>Nestable</code>.
     *
     * @param nestable the Nestable implementation (<i>must</i> extend
     * {@link java.lang.Throwable})
     */
    NestableDelegate(Nestable nestable) // package
    {
        if (nestable instanceof Throwable)
        {
            this.nestable = (Throwable) nestable;
        }
        else
        {
            throw new IllegalArgumentException(MUST_BE_THROWABLE);
        }
    }

    /**
     * Returns the error message of the <code>Throwable</code> in the chain
     * of <code>Throwable</code>s at the specified index, numbererd from 0.
     *
     * @param index the index of the <code>Throwable</code> in the chain of
     * <code>Throwable</code>s
     * @return the error message, or null if the <code>Throwable</code> at the
     * specified index in the chain does not contain a message
     * @throws IndexOutOfBoundsException if the <code>index</code> argument is
     * negative or not less than the count of <code>Throwable</code>s in the
     * chain
     */
    String getMessage(int index)
    {
        Throwable t = this.getThrowable(index);
        if(Nestable.class.isInstance(t))
        {
            return ((Nestable) t).getMessage(0);
        }
        else
        {
            return t.getMessage();
        }
    }
    
    /**
     * Returns the full message contained by the <code>Nestable</code>
     * and any nested <code>Throwable</code>s.
     *
     * @param baseMsg the base message to use when creating the full
     * message. Should be generally be called via
     * <code>nestableHelper.getMessage(super.getMessage())</code>,
     * where <code>super</code> is an instance of {@link
     * java.lang.Throwable}.
     * @return The concatenated message for this and all nested
     * <code>Throwable</code>s
     */
    String getMessage(String baseMsg) // package
    {
        StringBuffer msg = new StringBuffer();
        if (baseMsg != null)
        {
            msg.append(baseMsg);
        }

        Throwable nestedCause = ExceptionUtils.getCause(this.nestable);
        if (nestedCause != null)
        {
            String causeMsg = nestedCause.getMessage();
            if (causeMsg != null)
            {
                if (baseMsg != null)
                {
                    msg.append(": ");
                }
                msg.append(causeMsg);
            }

        }
        return (msg.length() > 0 ? msg.toString() : null);
    }

    /**
     * Returns the error message of this and any nested <code>Throwable</code>s
     * in an array of Strings, one element for each message. Any
     * <code>Throwable</code> not containing a message is represented in the
     * array by a null. This has the effect of cause the length of the returned
     * array to be equal to the result of the {@link #getThrowableCount()}
     * operation.
     *
     * @return the error messages
     */
    String[] getMessages() // package
    {
        Throwable[] throwables = this.getThrowables();
        String[] msgs = new String[throwables.length];
        for(int i = 0; i < throwables.length; i++)
        {
            msgs[i] = (Nestable.class.isInstance(throwables[i]) ?
                       ((Nestable) throwables[i]).getMessage(0) :
                       throwables[i].getMessage());
        }
        return msgs;
    }

    /**
     * Returns the <code>Throwable</code> in the chain of
     * <code>Throwable</code>s at the specified index, numbererd from 0.
     *
     * @param index the index, numbered from 0, of the <code>Throwable</code> in
     * the chain of <code>Throwable</code>s
     * @return the <code>Throwable</code>
     * @throws IndexOutOfBoundsException if the <code>index</code> argument is
     * negative or not less than the count of <code>Throwable</code>s in the
     * chain
     */
    Throwable getThrowable(int index)
    {
        if(index == 0)
        {
            return this.nestable;
        }
        Throwable[] throwables = this.getThrowables();
        return throwables[index];
    }
    
    /**
     * Returns the number of <code>Throwable</code>s contained in the
     * <code>Nestable</code> contained by this delegate.
     *
     * @return the throwable count
     */
    int getThrowableCount() // package
    {
        return ExceptionUtils.getThrowableCount(this.nestable);
    }
    
    /**
     * Returns this delegate's <code>Nestable</code> and any nested
     * <code>Throwable</code>s in an array of <code>Throwable</code>s, one
     * element for each <code>Throwable</code>.
     *
     * @return the <code>Throwable</code>s
     */
    Throwable[] getThrowables() // package
    {
        return ExceptionUtils.getThrowables(this.nestable);
    }

    /**
     * Returns the index, numbered from 0, of the first <code>Throwable</code>
     * that matches the specified type in the chain of <code>Throwable</code>s
     * held in this delegate's <code>Nestable</code> with an index greater than
     * or equal to the specified index, or -1 if the type is not found.
     *
     * @param type <code>Class</code> to be found
     * @param fromIndex the index, numbered from 0, of the starting position in
     * the chain to be searched
     * @return index of the first occurrence of the type in the chain, or -1 if
     * the type is not found
     * @throws IndexOutOfBoundsException if the <code>fromIndex</code> argument
     * is negative or not less than the count of <code>Throwable</code>s in the
     * chain
     */
    int indexOfThrowable(Class type, int fromIndex) // package
    {
        return ExceptionUtils.indexOfThrowable(this.nestable, type, fromIndex);
    }
    
    /**
     * Prints the stack trace of this exception the the standar error
     * stream.
     */
    public void printStackTrace()
    {
        printStackTrace(System.err);
    }

    /**
     * Prints the stack trace of this exception to the specified
     * stream.
     *
     * @param out <code>PrintStream</code> to use for output.
     * @see #printStackTrace(PrintWriter)
     */
    public void printStackTrace(PrintStream out)
    {
        synchronized (out)
        {
            PrintWriter pw = new PrintWriter(out, false);
            printStackTrace(pw);
            // Flush the PrintWriter before it's GC'ed.
            pw.flush();
        }
    }

    /**
     * Prints the stack trace of this exception to the specified
     * writer.
     *
     * @param out <code>PrintWriter</code> to use for output.
     */
    public void printStackTrace(PrintWriter out)
    {
        synchronized (out)
        {
            String[] st = getStackFrames(this.nestable);
            Throwable nestedCause = ExceptionUtils.getCause(this.nestable);
            if (nestedCause != null)
            {
                if (nestedCause instanceof Nestable)
                {
                    // Recurse until a non-Nestable is encountered.
                    ((Nestable) nestedCause).printStackTrace(out);
                }
                else
                {
                    String[] nst = getStackFrames(nestedCause);
                    for (int i = 0; i < nst.length; i++)
                    {
                        out.println(nst[i]);
                    }
                }
                out.print("rethrown as ");
            }

            // Output desired frames from stack trace.
            for (int i = 0; i < st.length; i++)
            {
                out.println(st[i]);
            }
        }
    }

    /**
     * Captures the stack trace associated with the specified
     * <code>Throwable</code> object, decomposing it into a list of
     * stack frames.
     *
     * @param t The <code>Throwable</code>.
     * @return  An array of strings describing each stack frame.
     */
    private String[] getStackFrames(Throwable t)
    {
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw, true);

        // Avoid infinite loop between decompose() and printStackTrace().
        if (t instanceof Nestable)
        {
            ((Nestable) t).printPartialStackTrace(pw);
        }
        else
        {
            t.printStackTrace(pw);
        }
        return ExceptionUtils.getStackFrames(sw.getBuffer().toString());
    }
}
