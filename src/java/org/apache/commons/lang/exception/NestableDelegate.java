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

import java.io.OutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.Writer;
import java.util.LinkedList;
import java.util.StringTokenizer;

/**
 * @author <a href="mailto:Rafal.Krzewski@e-point.pl">Rafal Krzewski</a>
 * @author <a href="mailto:dlr@collab.net">Daniel Rall</a>
 * @author <a href="mailto:knielsen@apache.org">Kasper Nielsen</a>
 * @author <a href="mailto:steven@caswell.name">Steven Caswell</a>
 * @version $Id: NestableDelegate.java,v 1.1 2002/07/19 03:35:54 bayard Exp $
 */
public class NestableDelegate
{
    /**
     * Constructor error message.
     */
    private static final String MUST_BE_THROWABLE =
        "The Nestable implementation passed to the NestableDelegate(Nestable) "
        + "constructor must extend java.lang.Throwable";

    /**
     * Holds the reference to the exception or error that caused
     * this exception to be thrown.
     */
    private Nestable cause = null;

    /**
     * @param cause The Nestable implementation to get a stack trace for
     * (<i>must</i> extend {@link java.lang.Throwable}).
     */
    NestableDelegate(Nestable cause) // package
    {
        if (cause instanceof Throwable)
        {
            this.cause = cause;
        }
        else
        {
            throw new IllegalArgumentException(MUST_BE_THROWABLE);
        }
    }

    /**
     * Returns the number of <code>Throwable</code>s contained in the
     * <code>Nestable</code> contained by this delegate.
     */
    int getLength() // package
    {
        // Count the number of throwables
        int count = 1;
        String msg = null;
        if(this.cause.getCause() == null)
        {
            return count;
        }
        Throwable t = this.cause.getCause();
        while(t != null)
        {
            ++count;
            if(Nestable.class.isInstance(t))
            {
                t = ((Nestable) t).getCause();
            }
            else
            {
                t = null;
            }
        }
        return count;
    }
    
    /**
     * @param baseMsg The base message to use when creating the full
     * message.  Should be generally be called via
     * <code>nestableHelper.getMessage(super.getMessage())</code>,
     * where <code>super</code> is an instance of {@link
     * java.lang.Throwable}.
     * @return The concatenated message for this and all nested
     * exceptions.
     */
    String getMessage(String baseMsg) // package
    {
        StringBuffer msg = new StringBuffer();
        if (baseMsg != null)
        {
            msg.append(baseMsg);
        }

        Throwable nestedCause = cause.getCause();
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
    
    Throwable getThrowable(int index)
    {
        Throwable[] throwables = this.getThrowables();
        if(index < 0)
        {
            index = 0;
        }
        if(index == 0)
        {
            return (Throwable) this.cause;
        }
        if(index >= throwables.length)
        {
            index = throwables.length - 1;
        }
        return throwables[index];
    }
    
    Throwable[] getThrowables() // package
    {
        int count = this.getLength();
        // Allocate an array to hold the messages
        Throwable[] throwables = new Throwable[count];
        count = 0;
        if(cause != null)
        {
            throwables[count++] = (Throwable) this.cause;
            Throwable t = this.cause.getCause();
            while(t != null)
            {
                throwables[count++] = t;
                if(Nestable.class.isInstance(t))
                {
                    t = ((Nestable) t).getCause();
                }
                else
                {
                    t = null;
                }
            }
        }
        return throwables;
    }

    String[] getMessages() // package
    {
        Throwable throwables[] = this.getThrowables();
        String[] msgs = new String[throwables.length];
        for(int i = 0; i < throwables.length; i++)
        {
            msgs[i] = (Nestable.class.isInstance(throwables[i]) ? ((Nestable) throwables[i]).getMessage(0) : throwables[i].getMessage());
        }
        return msgs;
    }

    int indexOfThrowable(int pos, Class type) // package
    {
        pos = (pos < 0) ? 0 : pos;
        Throwable throwables[] = this.getThrowables();
        pos = (pos >= throwables.length) ? throwables.length - 1 : pos;
        for(int i = pos; i < throwables.length; i++)
        {
            if(throwables[i].getClass().equals(type))
            {
                return i;
            }
        }
        return -1;
    }
    
    /**
     * Prints the stack trace of this exception the the standar error
     * stream.
     */
    public void printStackTrace()
    {
        synchronized (System.err)
        {
            printStackTrace(System.err);
        }
    }

    /**
     * Prints the stack trace of this exception to the specified print stream.
     *
     * @param out <code>PrintStream</code> to use for output.
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
     * Prints the stack trace of this exception to the specified print writer.
     *
     * @param out <code>PrintWriter</code> to use for output.
     */
    public void printStackTrace(PrintWriter out)
    {
        synchronized (out)
        {
            String[] st = decompose((Throwable) cause);
            Throwable nestedCause = cause.getCause();
            if (nestedCause != null)
            {
                if (nestedCause instanceof Nestable)
                {
                    // Recurse until a non-Nestable is encountered.
                    ((Nestable) nestedCause).printStackTrace(out);
                }
                else
                {
                    String[] nst = decompose(nestedCause);
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
     * Captures the stack trace associated with a <code>Throwable</code>
     * object, decomposing it into a list of stack frames.
     *
     * @param t The <code>Throwable</code>.
     * @return  An array of strings describing each stack frame.
     */
    private String[] decompose(Throwable t)
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

        String linebreak = System.getProperty("line.separator");
        StringTokenizer st = new StringTokenizer(sw.getBuffer().toString(),
                                                 linebreak);
        LinkedList list = new LinkedList();
        while (st.hasMoreTokens())
        {
            list.add(st.nextToken());
        }
        return (String []) list.toArray(new String[] {});
    }
}
