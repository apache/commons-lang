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

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/**
 * Utility routines for manipulating <code>Throwable</code> objects.
 *
 * @author <a href="mailto:dlr@finemaltcoding.com">Daniel Rall</a>
 * @since 1.0
 */
public class ExceptionUtils
{
    /**
     * The name of the <code>getCause()</code> method.
     */
    protected static final String CAUSE_METHOD_NAME = "getCause";

    /**
     * The parameters of the <code>getCause()</code> method.
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
     * <code>getCause()</code> method which returns a
     * <code>Throwable</code> object (standard as of JDK 1.4, and part
     * of the {@link
     * org.apache.commons.lang.exception.NestableException} API),
     * extracting and returning the cause of the exception.
     * Otherwise, returns <code>null</code>.
     *
     * <p>TODO: Examine for a "detail" public member attribute from
     * java.rmi.RemoteException.
     *
     * @param t The exception to introspect for a cause.
     * @return The cause of the <code>Throwable</code>.
     */
    public static Throwable getCause(Throwable t)
    {
        Throwable cause = null;

        if (t instanceof NestableException)
        {
            cause = ((NestableException) t).getCause();
        }
        else if (t instanceof NestableRuntimeException)
        {
            cause = ((NestableRuntimeException) t).getCause();
        }
        else
        {
            Method getCause = null;
            Class c = t.getClass();
            try
            {
                getCause = c.getMethod(CAUSE_METHOD_NAME, null);
            }
            catch (NoSuchMethodException ignored)
            {
            }
            catch (SecurityException ignored)
            {
            }

            if (getCause != null &&
                Throwable.class.isAssignableFrom(getCause.getReturnType()))
            {
                try
                {
                    cause = (Throwable) getCause.invoke(t, CAUSE_METHOD_PARAMS);
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
}
