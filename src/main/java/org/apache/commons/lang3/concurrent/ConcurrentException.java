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

import java.util.concurrent.ExecutionException;

/**
 * An exception class used for reporting error conditions related to accessing data of background tasks.
 *
 * <p>
 * The purpose of this exception class is analogous to the default JDK exception class {@link ExecutionException}, i.e.
 * it wraps an exception that occurred during the execution of a task. However, in contrast to
 * {@link ExecutionException}, it wraps only checked exceptions. Runtime exceptions are thrown directly.
 * </p>
 *
 * @since 3.0
 */
public class ConcurrentException extends Exception {
    /**
     * The serial version UID.
     */
    private static final long serialVersionUID = 6622707671812226130L;

    /**
     * Creates a new, uninitialized instance of {@link ConcurrentException}.
     */
    protected ConcurrentException() {
    }

    /**
     * Creates a new instance of {@link ConcurrentException} and initializes it
     * with the given cause.
     *
     * @param cause the cause of this exception
     * @throws IllegalArgumentException if the cause is not a checked exception
     */
    public ConcurrentException(final Throwable cause) {
        super(ConcurrentUtils.checkedException(cause));
    }

    /**
     * Creates a new instance of {@link ConcurrentException} and initializes it
     * with the given message and cause.
     *
     * @param msg the error message
     * @param cause the cause of this exception
     * @throws IllegalArgumentException if the cause is not a checked exception
     */
    public ConcurrentException(final String msg, final Throwable cause) {
        super(msg, ConcurrentUtils.checkedException(cause));
    }
}
