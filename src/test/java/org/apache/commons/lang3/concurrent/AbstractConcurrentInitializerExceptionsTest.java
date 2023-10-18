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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.sql.SQLException;

import org.apache.commons.lang3.function.FailableSupplier;
import org.junit.jupiter.api.Test;

/**
 *
 * An abstract base class for tests of exceptions thrown during initialise and close methods
 * on concrete {@code ConcurrentInitializer} implementations.
 * </p>
 * <p>
 * This class provides some basic tests for initializer implementations. Derived
 * class have to create a {@link ConcurrentInitializer} object on which the
 * tests are executed.
 */
public abstract class AbstractConcurrentInitializerExceptionsTest extends AbstractConcurrentInitializerTest {

    /**
     * This method tests that if a ConcurrentInitializer.initialize method catches a
     * ConcurrentException it will rethrow it without wrapping it.
     */
    @Test
    public void testSupplierThrowsConcurrentException() {
        final ConcurrentException concurrentException = new ConcurrentException();
        final ConcurrentInitializer<Object> initializer = createInitializerThatThrowsPreCreatedException(
                () -> {
                    if ("test".equals("test")) {
                        throw concurrentException;
                    }
                    return new Object();
                });
        try {
            initializer.get();
            fail();
        } catch (ConcurrentException e) {
            assertEquals(concurrentException, e);
        }
    }

    /**
     * This method tests that if LazyInitializer.initialize catches a ConcurrentException it will rethrow it without wrapping it
     */
    @Test
    public void testSupplierThrowsCheckedException() {
        final ConcurrentInitializer<Object> initializer = createInitializerThatThrowsException(
                () -> methodThatThrowsException(ExceptionToThrow.IOException));
        assertThrows(ConcurrentException.class, () -> initializer.get());
    }

    @Test
    public void testSupplierThrowsRuntimeException() {
        final ConcurrentInitializer<Object> initializer = createInitializerThatThrowsException(
                () -> methodThatThrowsException(ExceptionToThrow.NullPointerException));
        assertThrows(NullPointerException.class, () -> initializer.get());
    }

    protected enum ExceptionToThrow {
        IOException,
        SQLException,
        NullPointerException
    }

    //The use of enums makes this look like a realistic method to the compiler
    protected static String methodThatThrowsException(ExceptionToThrow input) throws IOException, SQLException, ConcurrentException {
        switch (input) {
        case IOException:
            throw new IOException();
        case SQLException:
            throw new SQLException();
        case NullPointerException:
            throw new NullPointerException();
        default:
            fail();
            return "failed";
        }
    }

    protected abstract ConcurrentInitializer<Object> createInitializerThatThrowsException(FailableSupplier<Object, ? extends Exception> supplier);

    protected abstract ConcurrentInitializer<Object> createInitializerThatThrowsPreCreatedException(FailableSupplier<Object, ? extends Exception> supplier);
}
