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
package org.apache.commons.lang3;

/**
 * Thrown to indicate that a block of code has not been implemented.
 * This exception supplements {@link UnsupportedOperationException}
 * by providing a more semantically rich description of the problem.
 *
 * <p>{@link NotImplementedException} represents the case where the
 * author has yet to implement the logic at this point in the program.
 * This can act as an exception based TODO tag.</p>
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
 * This class was originally added in Lang 2.0, but removed in 3.0.
 *
 * @since 3.2
 */
public class NotImplementedException extends UnsupportedOperationException {

    private static final long serialVersionUID = 20131021L;

    /** A resource for more information regarding the lack of implementation. */
    private final String code;

    /**
     * Constructs a NotImplementedException.
     *
     * @since 3.10
     */
    public NotImplementedException() {
        this.code = null;
    }

    /**
     * Constructs a NotImplementedException.
     *
     * @param message description of the exception
     * @since 3.2
     */
    public NotImplementedException(final String message) {
        this(message, (String) null);
    }

    /**
     * Constructs a NotImplementedException.
     *
     * @param cause cause of the exception
     * @since 3.2
     */
    public NotImplementedException(final Throwable cause) {
        this(cause, null);
    }

    /**
     * Constructs a NotImplementedException.
     *
     * @param message description of the exception
     * @param cause cause of the exception
     * @since 3.2
     */
    public NotImplementedException(final String message, final Throwable cause) {
        this(message, cause, null);
    }

    /**
     * Constructs a NotImplementedException.
     *
     * @param message description of the exception
     * @param code code indicating a resource for more information regarding the lack of implementation
     * @since 3.2
     */
    public NotImplementedException(final String message, final String code) {
        super(message);
        this.code = code;
    }

    /**
     * Constructs a NotImplementedException.
     *
     * @param cause cause of the exception
     * @param code code indicating a resource for more information regarding the lack of implementation
     * @since 3.2
     */
    public NotImplementedException(final Throwable cause, final String code) {
        super(cause);
        this.code = code;
    }

    /**
     * Constructs a NotImplementedException.
     *
     * @param message description of the exception
     * @param cause cause of the exception
     * @param code code indicating a resource for more information regarding the lack of implementation
     * @since 3.2
     */
    public NotImplementedException(final String message, final Throwable cause, final String code) {
        super(message, cause);
        this.code = code;
    }

    /**
     * Obtain the not implemented code. This is an unformatted piece of text intended to point to
     * further information regarding the lack of implementation. It might, for example, be an issue
     * tracker ID or a URL.
     *
     * @return a code indicating a resource for more information regarding the lack of implementation
     */
    public String getCode() {
        return this.code;
    }
}
