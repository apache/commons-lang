/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang3.concurrent;

import static org.apache.commons.lang3.LangAssertions.assertNullPointerException;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.sql.SQLException;

import org.apache.commons.lang3.function.FailableConsumer;
import org.apache.commons.lang3.function.FailableSupplier;
import org.junit.jupiter.api.Test;

/**
 * An abstract base class for tests of exceptions thrown during initialize and close methods
 * on concrete {@code ConcurrentInitializer} implementations.
 *
 * This class provides some basic tests for initializer implementations. Derived
 * class have to create a {@link ConcurrentInitializer} object on which the
 * tests are executed.
 *
 * @param <T> Domain type.
 */
public abstract class AbstractConcurrentInitializerCloseAndExceptionsTest<T> extends AbstractConcurrentInitializerTest<T> {

    protected static final class CloseableObject {
        boolean closed;

        public void close() {
            closed = true;
        }

        public boolean isClosed() {
            return closed;
        }
    }

    protected enum ExceptionToThrow {
        IOException,
        SQLException,
        NullPointerException
    }

    // The use of enums rather than accepting an Exception as the input means we can have
    // multiple exception types on the method signature.
    protected static CloseableObject methodThatThrowsException(final ExceptionToThrow input) throws IOException, SQLException, ConcurrentException {
        switch (input) {
        case IOException:
            throw new IOException();
        case SQLException:
            throw new SQLException();
        case NullPointerException:
            throw new NullPointerException();
        default:
            fail();
            return new CloseableObject();
        }
    }

    protected abstract ConcurrentInitializer<CloseableObject> createInitializerThatThrowsException(
            FailableSupplier<CloseableObject, ? extends Exception> supplier, FailableConsumer<CloseableObject, ? extends Exception> closer);

    /**
     * This method tests that if AbstractConcurrentInitializer.close catches a
     * ConcurrentException it will rethrow it wrapped in a ConcurrentException
     */
    @SuppressWarnings("rawtypes")
    @Test
    void testCloserThrowsCheckedException() throws ConcurrentException {
        final ConcurrentInitializer<CloseableObject> initializer = createInitializerThatThrowsException(
                CloseableObject::new,
                CloseableObject -> methodThatThrowsException(ExceptionToThrow.IOException));
        try {
            initializer.get();
            ((AbstractConcurrentInitializer) initializer).close();
            fail();
        } catch (final Exception e) {
            assertInstanceOf(ConcurrentException.class, e);
            assertInstanceOf(IOException.class, e.getCause());
        }
    }

    /**
     * This method tests that if AbstractConcurrentInitializer.close catches a
     * RuntimeException it will throw it without wrapping it in a ConcurrentException
     */
    @SuppressWarnings("rawtypes")
    @Test
    void testCloserThrowsRuntimeException() throws ConcurrentException {
        final ConcurrentInitializer<CloseableObject> initializer = createInitializerThatThrowsException(
                CloseableObject::new,
                CloseableObject -> methodThatThrowsException(ExceptionToThrow.NullPointerException));

        initializer.get();
        assertNullPointerException(() -> {
            ((AbstractConcurrentInitializer) initializer).close();
            });
    }

    /**
     * This method tests that if AbstractConcurrentInitializer.initialize catches a checked
     * exception it will rethrow it wrapped in a ConcurrentException
     */
    @SuppressWarnings("unchecked") //for NOP
    @Test
    void testSupplierThrowsCheckedException() {
        final ConcurrentInitializer<CloseableObject> initializer = createInitializerThatThrowsException(
                () -> methodThatThrowsException(ExceptionToThrow.IOException),
                FailableConsumer.NOP);
        assertThrows(ConcurrentException.class, () -> initializer.get());
    }

    /**
     * This method tests that if a AbstractConcurrentInitializer.initialize method catches a
     * ConcurrentException it will rethrow it without wrapping it.
     */
    @Test
    void testSupplierThrowsConcurrentException() {
        final ConcurrentException concurrentException = new ConcurrentException();
        @SuppressWarnings("unchecked")
        final ConcurrentInitializer<CloseableObject> initializer = createInitializerThatThrowsException(() -> {
            if ("test".equals("test")) {
                throw concurrentException;
            }
            return new CloseableObject();
        }, FailableConsumer.NOP);
        try {
            initializer.get();
            fail();
        } catch (final ConcurrentException e) {
            assertEquals(concurrentException, e);
        }
    }

    /**
     * This method tests that if AbstractConcurrentInitializer.initialize catches a runtime exception
     * it will not be wrapped in a ConcurrentException
     */
    @SuppressWarnings("unchecked")
    @Test
    void testSupplierThrowsRuntimeException() {
        final ConcurrentInitializer<CloseableObject> initializer = createInitializerThatThrowsException(
                () -> methodThatThrowsException(ExceptionToThrow.NullPointerException),
                FailableConsumer.NOP);
        assertNullPointerException(() -> initializer.get());
    }

    /**
     * This method tests that if AbstractConcurrentInitializer.close actually closes the wrapped object
     */
    @SuppressWarnings("rawtypes")
    @Test
    void testWorkingCloser() throws Exception {
        final ConcurrentInitializer<CloseableObject> initializer = createInitializerThatThrowsException(
                CloseableObject::new,
                CloseableObject::close);

        final CloseableObject closeableObject = initializer.get();
        assertFalse(closeableObject.isClosed());
        ((AbstractConcurrentInitializer) initializer).close();
        assertTrue(closeableObject.isClosed());
    }
}
