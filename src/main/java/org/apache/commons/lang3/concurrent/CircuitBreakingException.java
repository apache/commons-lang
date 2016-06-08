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
package org.apache.commons.lang3.concurrent;

/**
 * <p>
 * An exception class used for reporting runtime error conditions related to
 * circuit breakers.
 * </p>
 * @since 3.5
 */
public class CircuitBreakingException extends RuntimeException {
    /**
     * The serial version UID.
     */
    private static final long serialVersionUID = 1408176654686913340L;

    /**
     * Creates a new, uninitialized instance of {@code CircuitBreakingException}.
     */
    public CircuitBreakingException() {
        super();
    }

    /**
     * Creates a new instance of {@code CircuitBreakingException} and initializes it with the given message and cause.
     *
     * @param message the error message
     * @param cause the cause of this exception
     */
    public CircuitBreakingException(String message, Throwable cause) {
        super(message, cause);
    }

    /**
     * Creates a new instance of {@code CircuitBreakingException} and initializes it with the given message.
     *
     * @param message the error message
     */
    public CircuitBreakingException(String message) {
        super(message);
    }

    /**
     * Creates a new instance of {@code CircuitBreakingException} and initializes it with the given cause.
     *
     * @param cause the cause of this exception
     */
    public CircuitBreakingException(Throwable cause) {
        super(cause);
    }

}
