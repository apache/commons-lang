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


/**
 * <p>Thrown to indicate that a block of code has not been implemented.
 * This exception supplements <code>UnsupportedOperationException</code>
 * by providing a more semantically rich description of the problem.</p>
 * 
 * <p><code>NotImplementedException</code> represents the case where the
 * author has yet to implement the logic at this point in the program.
 * This can act as an exception based TODO tag. </p>
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
//@Immutable
public class NotImplementedException extends UnsupportedOperationException {

    private static final String DEFAULT_MESSAGE = "Code is not implemented";

    /**
     * Required for serialization support.
     * 
     * @see java.io.Serializable
     */
    private static final long serialVersionUID = -6894122266938754088L;

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
        super(DEFAULT_MESSAGE, cause);
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
        super(msg == null ? DEFAULT_MESSAGE : msg, cause);
    }

    /**
     * Constructs a new <code>NotImplementedException</code> referencing the specified class.
     * 
     * @param clazz
     *            the <code>Class</code> that has not implemented the method
     */
    public NotImplementedException(Class<?> clazz) {
        super(clazz == null ? DEFAULT_MESSAGE : DEFAULT_MESSAGE + " in " + clazz);
    }

}
