/*
 * Copyright 2002-2004 The Apache Software Foundation.
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
package org.apache.commons.lang;

import java.io.PrintStream;
import java.io.PrintWriter;

import org.apache.commons.lang.exception.Nestable;
import org.apache.commons.lang.exception.NestableDelegate;

/**
 * <p>Thrown to indicate that a block of code has not been implemented.</p>
 * 
 * @author Matthew Hawthorne
 * @author Stephen Colebourne
 * @since 2.0
 * @version $Id: NotImplementedException.java,v 1.7 2004/03/04 00:13:38 scolebourne Exp $
 */
public class NotImplementedException
        extends UnsupportedOperationException implements Nestable {

    /**
     * The exception helper to delegate nested exception handling to.
     */
    protected NestableDelegate delegate = new NestableDelegate(this);

    /**
     * Holds the reference to the exception or error that caused
     * this exception to be thrown.
     */
    private Throwable cause;

    //-----------------------------------------------------------------------
    /**
     * Constructs a new <code>NotImplementedException</code> with default
     * detail message.
     */
    public NotImplementedException() {
        super("Code is not implemented");
    }

    /**
     * Constructs a new <code>NotImplementedException</code> with specified
     * detail message.
     *
     * @param msg The error message.
     */
    public NotImplementedException(String msg) {
        super(msg == null ? "Code is not implemented" : msg);
    }

    /**
     * Constructs a new <code>NotImplementedException</code> with specified
     * nested <code>Throwable</code> and default message.
     *
     * @param cause the exception or error that caused this exception to be
     * thrown
     */
    public NotImplementedException(Throwable cause) {
        super("Code is not implemented");
        this.cause = cause;
    }

    /**
     * Constructs a new <code>NotImplementedException</code> with specified
     * detail message and nested <code>Throwable</code>.
     *
     * @param msg    the error message
     * @param cause  the exception or error that caused this exception to be
     * thrown
     */
    public NotImplementedException(String msg, Throwable cause) {
        super(msg == null ? "Code is not implemented" : msg);
        this.cause = cause;
    }

    /**
     * Constructs a new <code>NotImplementedException</code> referencing
     * the specified class.
     * 
     * @param clazz  the <code>Class</code> that has not implemented the method
     */
    public NotImplementedException(Class clazz) {
        super((clazz == null ?
                "Code is not implemented" :
                "Code is not implemented in " + clazz));
    }

    //-----------------------------------------------------------------------
    public Throwable getCause() {
        return cause;
    }

    /**
     * Returns the detail message string of this throwable. If it was
     * created with a null message, returns the following:
     * (cause==null ? null : cause.toString()).
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

    public String getMessage(int index) {
        if (index == 0) {
            return super.getMessage();
        } else {
            return delegate.getMessage(index);
        }
    }

    public String[] getMessages() {
        return delegate.getMessages();
    }

    public Throwable getThrowable(int index) {
        return delegate.getThrowable(index);
    }

    public int getThrowableCount() {
        return delegate.getThrowableCount();
    }

    public Throwable[] getThrowables() {
        return delegate.getThrowables();
    }

    public int indexOfThrowable(Class type) {
        return delegate.indexOfThrowable(type, 0);
    }

    public int indexOfThrowable(Class type, int fromIndex) {
        return delegate.indexOfThrowable(type, fromIndex);
    }

    public void printStackTrace() {
        delegate.printStackTrace();
    }

    public void printStackTrace(PrintStream out) {
        delegate.printStackTrace(out);
    }

    public void printStackTrace(PrintWriter out) {
        delegate.printStackTrace(out);
    }

    public final void printPartialStackTrace(PrintWriter out) {
        super.printStackTrace(out);
    }

}
