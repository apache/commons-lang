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

import java.io.PrintWriter;

/**
 * An interface to be implemented by {@link java.lang.Throwable}
 * extensions which would like to be able to nest root exceptions
 * inside themselves.
 *
 * @author <a href="mailto:dlr@collab.net">Daniel Rall</a>
 * @author <a href="mailto:knielsen@apache.org">Kasper Nielsen</a>
 * @author <a href="mailto:steven@caswell.name">Steven Caswell</a>
 * @version $Id: Nestable.java,v 1.1 2002/07/19 03:35:54 bayard Exp $
 */
public interface Nestable
{
    /**
     * Returns the reference to the exception or error that caused the
     * exception implementing the <code>Nestable</code> to be thrown.
     */
    public Throwable getCause();

    /**
     * Returns the number of nested <code>Throwable</code>s represented by
     * this <code>Nestable</code>, including this <code>Nestable</code>.
     */
    public int getLength();
    
    /**
     * Returns the error message of this and any nested
     * <code>Throwable</code>.
     *
     * @return The error message.
     */
    public String getMessage();

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
    public String getMessage(int index);

    /**
     * Returns the error message of this and any nested <code>Throwable</code>s
     * in an array of Strings, one element for each message. Any
     * <code>Throwable</code> specified without a message is represented in
     * the array by a null.
     *
     * @return the error messages
     */
    public String[] getMessages();

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
    public Throwable getThrowable(int index);

    /**
     * Returns the error message of this and any nested <code>Throwable</code>s
     * in an array of Strings, one element for each message. Any
     * <code>Throwable</code> specified without a message is represented in
     * the array by a null.
     *
     * @return the <code>Throwable</code>s
     */
    public Throwable[] getThrowables();

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
    public int indexOfThrowable(Class type);

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
    public int indexOfThrowable(int pos, Class type);
    
    /**
     * Prints the stack trace of this exception to the specified print
     * writer.  Includes inforamation from the exception--if
     * any--which caused this exception.
     *
     * @param out <code>PrintWriter</code> to use for output.
     */
    public void printStackTrace(PrintWriter out);

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
    public void printPartialStackTrace(PrintWriter out);
}
