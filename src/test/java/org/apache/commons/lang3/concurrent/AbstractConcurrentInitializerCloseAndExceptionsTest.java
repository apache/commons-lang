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
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.sql.SQLException;

import org.apache.commons.lang3.function.FailableConsumer;
import org.apache.commons.lang3.function.FailableSupplier;
import org.junit.jupiter.api.Test;

/**
 *
 * An abstract base class for tests of exceptions thrown during initialize and close methods
 * on concrete {@code ConcurrentInitializer} implementations.
 * </p>
 * <p>
 * This class provides some basic tests for initializer implementations. Derived
 * class have to create a {@link ConcurrentInitializer} object on which the
 * tests are executed.
 */
public abstract class AbstractConcurrentInitializerCloseAndExceptionsTest extends AbstractConcurrentInitializerTest {

    /**
     * This method tests that if a ConcurrentInitializer.initialize method catches a
     * ConcurrentException it will rethrow it without wrapping it.
     */
    @Test
    public void testSupplierThrowsConcurrentException() {
        final ConcurrentException concurrentException = new ConcurrentException();

        @SuppressWarnings("unchecked")
        final ConcurrentInitializer<CloseableObject> initializer = createInitializerThatThrowsPreCreatedException(
                () -> {
                    if ("test".equals("test")) {
                        throw concurrentException;
                    }
                    return new CloseableObject();
                },
                FailableConsumer.NOP);
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
    @SuppressWarnings("unchecked") //for NOP
    @Test
    public void testSupplierThrowsCheckedException() {
        final ConcurrentInitializer<CloseableObject> initializer = createInitializerThatThrowsException(
                () -> methodThatThrowsException(ExceptionToThrow.IOException),
                FailableConsumer.NOP);
        assertThrows(ConcurrentException.class, () -> initializer.get());
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testSupplierThrowsRuntimeException() {
        final ConcurrentInitializer<CloseableObject> initializer = createInitializerThatThrowsException(
                () -> methodThatThrowsException(ExceptionToThrow.NullPointerException),
                FailableConsumer.NOP);
        assertThrows(NullPointerException.class, () -> initializer.get());
    }

    /**
     * This method tests that if LazyInitializer.initialize catches a ConcurrentException it will rethrow it without wrapping it
     */
    @SuppressWarnings("rawtypes")
    @Test
    public void testCloserThrowsIllegalStateException() {
        final IllegalStateException illegalStateException = new IllegalStateException();

        final ConcurrentInitializer<CloseableObject> initializer = createInitializerThatThrowsPreCreatedException(
                CloseableObject::new,
                (CloseableObject) -> {
                    throw illegalStateException;
                    });
        try {
            initializer.get();
            ((AbstractConcurrentInitializer) initializer).close();
            fail();
        } catch (Exception e) {
            // We do not have access to the type information as that is in the concrete subclasses
            // of AbstractConcurrentInitializer. So catch Exception, the assert checks everything
            // important
            assertEquals(illegalStateException, e);
        }
    }

    @SuppressWarnings("rawtypes")
    @Test
    public void testCloserThrowsCheckedException() throws ConcurrentException {
        final ConcurrentInitializer<CloseableObject> initializer = createInitializerThatThrowsException(
                CloseableObject::new,
                (CloseableObject) -> methodThatThrowsException(ExceptionToThrow.IOException));

        initializer.get();
        assertThrows(IllegalStateException.class, () -> {
            ((AbstractConcurrentInitializer) initializer).close();
            });
    }

    @SuppressWarnings("rawtypes")
    @Test
    public void testCloserThrowsRuntimeException() throws ConcurrentException {
        final ConcurrentInitializer<CloseableObject> initializer = createInitializerThatThrowsException(
                CloseableObject::new,
                (CloseableObject) -> methodThatThrowsException(ExceptionToThrow.NullPointerException));

        initializer.get();
        assertThrows(NullPointerException.class, () -> {
            ((AbstractConcurrentInitializer) initializer).close();
            });
    }

    @SuppressWarnings("rawtypes")
    @Test
    public void testWorkingCloser() throws Exception {
        final ConcurrentInitializer<CloseableObject> initializer = createInitializerThatThrowsException(
                CloseableObject::new,
                CloseableObject::close);

        CloseableObject cloesableObject = initializer.get();
        assertFalse(cloesableObject.isClosed());
        ((AbstractConcurrentInitializer) initializer).close();
        assertTrue(cloesableObject.isClosed());
    }

    protected enum ExceptionToThrow {
        IOException,
        SQLException,
        NullPointerException
    }

    //The use of enums makes this look like a realistic method to the compiler
    protected static CloseableObject methodThatThrowsException(ExceptionToThrow input) throws IOException, SQLException, ConcurrentException {
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

    protected abstract ConcurrentInitializer<CloseableObject> createInitializerThatThrowsPreCreatedException(
            FailableSupplier<CloseableObject, ? extends Exception> supplier, FailableConsumer<CloseableObject, ? extends Exception> closer);

    protected static final class CloseableObject {
        boolean closed = false;

        public boolean isClosed() {
            return closed;
        }

        public void close() {
            closed = true;
        }
    }
}
