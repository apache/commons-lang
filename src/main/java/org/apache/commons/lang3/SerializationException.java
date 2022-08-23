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
 * Exception thrown when the Serialization process fails.
 *
 * <p>The original error is wrapped within this one.</p>
 *
 * <p>#NotThreadSafe# because Throwable is not thread-safe</p>
 * @since 1.0
 */
public class SerializationException extends RuntimeException {

    /**
     * Required for serialization support.
     *
     * @see java.io.Serializable
     */
    private static final long serialVersionUID = 4029025366392702726L;

    /**
     * Constructs a new {@link SerializationException} without specified
     * detail message.
     */
    public SerializationException() {
    }

    /**
     * Constructs a new {@link SerializationException} with specified
     * detail message.
     *
     * @param msg  The error message.
     */
    public SerializationException(final String msg) {
        super(msg);
    }

    /**
     * Constructs a new {@link SerializationException} with specified
     * nested {@link Throwable}.
     *
     * @param cause  The {@link Exception} or {@link Error}
     *  that caused this exception to be thrown.
     */
    public SerializationException(final Throwable cause) {
        super(cause);
    }

    /**
     * Constructs a new {@link SerializationException} with specified
     * detail message and nested {@link Throwable}.
     *
     * @param msg    The error message.
     * @param cause  The {@link Exception} or {@link Error}
     *  that caused this exception to be thrown.
     */
    public SerializationException(final String msg, final Throwable cause) {
        super(msg, cause);
    }

}
