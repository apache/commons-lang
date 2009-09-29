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
package org.apache.commons.lang.concurrent;

import java.util.concurrent.ExecutionException;

/**
 * <p>
 * An utility class providing functionality related to the {@code
 * java.util.concurrent} package.
 * </p>
 *
 * @version $Id$
 */
public class ConcurrentUtils {
    /**
     * Private constructor so that no instances can be created. This class
     * contains only static utility methods.
     */
    private ConcurrentUtils() {
    }

    /**
     * Inspects the cause of the specified {@code ExecutionException} and
     * creates a {@code ConcurrentException} with the checked cause if
     * necessary. This method performs the following checks on the cause of the
     * passed in exception:
     * <ul>
     * <li>If the passed in exception is <b>null</b> or the cause is
     * <b>null</b>, this method returns <b>null</b>.</li>
     * <li>If the cause is a runtime exception, it is directly thrown.</li>
     * <li>If the cause is an error, it is directly thrown, too.</li>
     * <li>In any other case the cause is a checked exception. The method then
     * creates a {@link ConcurrentException}, initializes it with the cause, and
     * returns it.</li>
     * </ul>
     *
     * @param ex the exception to be processed
     * @return a {@code ConcurrentException} with the checked cause
     */
    public static ConcurrentException extractCause(ExecutionException ex) {
        if (ex == null || ex.getCause() == null) {
            return null;
        }

        throwCause(ex);
        return new ConcurrentException(ex.getMessage(), ex.getCause());
    }

    /**
     * Handles the specified {@code ExecutionException}. This method calls
     * {@link #extractCause(ExecutionException)} for obtaining the cause of the
     * exception - which might already cause an unchecked exception or an error
     * being thrown. If the cause is a checked exception however, it is wrapped
     * in a {@code ConcurrentException}, which is thrown. If the passed in
     * exception is <b>null</b> or has no cause, the method simply returns
     * without throwing an exception.
     *
     * @param ex the exception to be handled
     * @throws ConcurrentException if the cause of the {@code
     * ExecutionException} is a checked exception
     */
    public static void handleCause(ExecutionException ex)
            throws ConcurrentException {
        ConcurrentException cex = extractCause(ex);

        if (cex != null) {
            throw cex;
        }
    }

    /**
     * Tests whether the specified {@code Throwable} is a checked exception. If
     * not, an exception is thrown.
     *
     * @param ex the {@code Throwable} to check
     * @return a flag whether the passed in exception is a checked exception
     * @throws IllegalArgumentException if the {@code Throwable} is not a
     * checked exception
     */
    static Throwable checkedException(Throwable ex) {
        if (ex != null && !(ex instanceof RuntimeException)
                && !(ex instanceof Error)) {
            return ex;
        } else {
            throw new IllegalArgumentException("Not a checked exception: " + ex);
        }
    }

    /**
     * Tests whether the cause of the specified {@code ExecutionException}
     * should be thrown and does it if necessary.
     *
     * @param ex the exception in question
     */
    private static void throwCause(ExecutionException ex) {
        if (ex.getCause() instanceof RuntimeException) {
            throw (RuntimeException) ex.getCause();
        }

        if (ex.getCause() instanceof Error) {
            throw (Error) ex.getCause();
        }
    }
}
