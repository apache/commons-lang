/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang;

import java.io.PrintStream;
import java.io.PrintWriter;

import org.apache.commons.lang.exception.Nestable;
import org.apache.commons.lang.exception.NestableDelegate;

/**
 * <p>Thrown to indicate that a block of code has not been implemented.
 * This exception supplements <code>UnsupportedOperationException</code>
 * by providing a more semantically rich description of the problem.</p>
 * 
 * <p><code>NotImplementedException</code> represents the case where the
 * author has yet to implement the logic at this point in the program.
 * This can act as an exception based TODO tag.
 * Because this logic might be within a catch block, this exception
 * suports exception chaining.</p>
 * 
 * <pre>
 * public void foo() {
 *   try {
 *     // do something that throws an Exception
 *   } catch (Exception ex) {
 *     // don't know what to do here yet
 *     throw new NotImplementedException("TODO", ex);
 *   }
 * }
 * </pre>
 * 
 * @author Matthew Hawthorne
 * @author Stephen Colebourne
 * @since 2.0
 * @version $Id$
 */
public class NotImplementedException
        extends UnsupportedOperationException implements Nestable {

    private static final String DEFAULT_MESSAGE = "Code is not implemented";

    /**
     * Required for serialization support.
     * 
     * @see java.io.Serializable
     */
    private static final long serialVersionUID = -6894122266938754088L;

    /**
     * The exception helper to delegate nested exception handling to.
     */
    private NestableDelegate delegate = new NestableDelegate(this);

    /**
     * Holds the reference to the exception or error that caused
     * this exception to be thrown.
     */
    private Throwable cause;

    //-----------------------------------------------------------------------
    /**
     * Constructs a new <code>NotImplementedException</code> with default message.
     * 
     * @since 2.1
     */
    public NotImplementedException() {
        super(DEFAULT_MESSAGE);
    }

    /**
     * Constructs a new <code>NotImplementedException</code> with specified
     * detail message.
     *
     * @param msg  the error message.
     */
    public NotImplementedException(String msg) {
        super(msg == null ? DEFAULT_MESSAGE : msg);
    }

    /**
     * Constructs a new <code>NotImplementedException</code> with specified
     * nested <code>Throwable</code> and default message.
     *
     * @param cause  the exception that caused this exception to be thrown
     * @since 2.1
     */
    public NotImplementedException(Throwable cause) {
        super(DEFAULT_MESSAGE);
        this.cause = cause;
    }

    /**
     * Constructs a new <code>NotImplementedException</code> with specified
     * detail message and nested <code>Throwable</code>.
     *
     * @param msg  the error message
     * @param cause  the exception that caused this exception to be thrown
     * @since 2.1
     */
    public NotImplementedException(String msg, Throwable cause) {
        super(msg == null ? DEFAULT_MESSAGE : msg);
        this.cause = cause;
    }

    /**
     * Constructs a new <code>NotImplementedException</code> referencing the specified class.
     * 
     * @param clazz
     *            the <code>Class</code> that has not implemented the method
     */
    public NotImplementedException(Class clazz) {
        super(clazz == null ? DEFAULT_MESSAGE : DEFAULT_MESSAGE + " in " + clazz);
    }

    // -----------------------------------------------------------------------
    /**
     * Gets the root cause of this exception.
     * @return the root cause of this exception.
     * 
     * @since 2.1
     */
    public Throwable getCause() {
        return cause;
    }

    /**
     * Gets the combined the error message of this and any nested errors.
     *
     * @return the error message
     * @since 2.1
     */
    public String getMessage() {
        if (super.getMessage() != null) {
            return super.getMessage();
        } else if (cause != null) {
            return cause.toString();
        } else {
            return null;
        }
    }

    /**
     * Returns the error message of the <code>Throwable</code> in the chain
     * of <code>Throwable</code>s at the specified index, numbered from 0.
     *
     * @param index  the index of the <code>Throwable</code> in the chain
     * @return the error message, or null if the <code>Throwable</code> at the
     *  specified index in the chain does not contain a message
     * @throws IndexOutOfBoundsException if the <code>index</code> argument is
     *  negative or not less than the count of <code>Throwable</code>s in the chain
     * @since 2.1
     */
    public String getMessage(int index) {
        if (index == 0) {
            return super.getMessage();
        }
        return delegate.getMessage(index);
    }

    /**
     * Returns the error message of this and any nested <code>Throwable</code> objects.
     * Each throwable returns a message, a null string is included in the array if
     * there is no message for a particular <code>Throwable</code>.
     *
     * @return the error messages
     * @since 2.1
     */
    public String[] getMessages() {
        return delegate.getMessages();
    }

    /**
     * Returns the <code>Throwable</code> in the chain by index.
     *
     * @param index  the index to retrieve
     * @return the <code>Throwable</code>
     * @throws IndexOutOfBoundsException if the <code>index</code> argument is
     *  negative or not less than the count of <code>Throwable</code>s in the chain
     * @since 2.1
     */
    public Throwable getThrowable(int index) {
        return delegate.getThrowable(index);
    }

    /**
     * Returns the number of nested <code>Throwable</code>s represented by
     * this <code>Nestable</code>, including this <code>Nestable</code>.
     *
     * @return the throwable count
     * @since 2.1
     */
    public int getThrowableCount() {
        return delegate.getThrowableCount();
    }

    /**
     * Returns this <code>Nestable</code> and any nested <code>Throwable</code>s
     * in an array of <code>Throwable</code>s, one element for each
     * <code>Throwable</code>.
     *
     * @return the <code>Throwable</code>s
     * @since 2.1
     */
    public Throwable[] getThrowables() {
        return delegate.getThrowables();
    }

    /**
     * Returns the index of the first occurrence of the specified type.
     * If there is no match, <code>-1</code> is returned.
     *
     * @param type  the type to search for
     * @return index of the first occurrence of the type in the chain, or -1 if
     *  the type is not found
     * @since 2.1
     */
    public int indexOfThrowable(Class type) {
        return delegate.indexOfThrowable(type, 0);
    }

    /**
     * Returns the index of the first occurrence of the specified type starting
     * from the specified index. If there is no match, <code>-1</code> is returned.
     *
     * @param type  the type to search for
     * @param fromIndex  the index of the starting position in the chain to be searched
     * @return index of the first occurrence of the type in the chain, or -1 if
     *  the type is not found
     * @throws IndexOutOfBoundsException if the <code>fromIndex</code> argument
     *  is negative or not less than the count of <code>Throwable</code>s in the chain
     * @since 2.1
     */
    public int indexOfThrowable(Class type, int fromIndex) {
        return delegate.indexOfThrowable(type, fromIndex);
    }

    /**
     * Prints the stack trace of this exception.
     * Includes information from the exception, if any, which caused this exception.
     * 
     * @since 2.1
     */
    public void printStackTrace() {
        delegate.printStackTrace();
    }

    /**
     * Prints the stack trace of this exception to the specified stream.
     * Includes information from the exception, if any, which caused this exception.
     *
     * @param out  the stream to write to
     * @since 2.1
     */
    public void printStackTrace(PrintStream out) {
        delegate.printStackTrace(out);
    }

    /**
     * Prints the stack trace of this exception to the specified writer.
     * Includes information from the exception, if any, which caused this exception.
     *
     * @param out  the writer to write to
     * @since 2.1
     */
    public void printStackTrace(PrintWriter out) {
        delegate.printStackTrace(out);
    }

    /**
     * Prints the stack trace for this exception only (root cause not included)
     * using the specified writer.
     * 
     * @param out  the writer to write to
     * @since 2.1
     */
    public final void printPartialStackTrace(PrintWriter out) {
        super.printStackTrace(out);
    }

}
