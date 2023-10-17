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

import org.junit.jupiter.api.Test;

/**
 * Tests {@code LazyInitializer}.
 */
public class LazyInitializerSupplierTest extends AbstractConcurrentInitializerTest {

    /**
     * Creates the initializer to be tested. This implementation returns the {@code LazyInitializer} created in the {@code setUp()} method.
     *
     * @return the initializer to be tested
     */
    @Override
    protected LazyInitializer<Object> createInitializer() {
        return new LazyInitializer.Builder<>().setInitializer(Object::new).get();
    }

    @Test
    public void testIsInitialized() throws ConcurrentException {
        final LazyInitializer<Object> initializer = createInitializer();
        assertFalse(initializer.isInitialized());
        initializer.get();
        assertTrue(initializer.isInitialized());
    }

    @Test
    public void testSupplierThrowsCheckedException() {
        final LazyInitializer<Object> initializer = LazyInitializer.builder().setInitializer(
                () -> methodThatThrowsException(ExceptionToThrow.IOException)).get();
        assertThrows(ConcurrentException.class, () -> initializer.get());
    }

    @Test
    public void testSupplierThrowsRuntimeException() {
        final LazyInitializer<Object> initializer = LazyInitializer.builder().setInitializer(
                () -> methodThatThrowsException(ExceptionToThrow.NullPointerException)).get();
        assertThrows(NullPointerException.class, () -> initializer.get());
    }

    /*
     * This method tests that if LazyInitializer.initialize catches a ConcurrentException it will rethrow it without wrapping it
     */
    @Test
    public void testSupplierThrowsConcurrentException() {
        final ConcurrentException concurrentException = new ConcurrentException();
        final LazyInitializer<Object> initializer = LazyInitializer.builder().setInitializer(
                () -> {
                    if ("test".equals("test")) {
                        throw concurrentException;
                    }
                    return new Object();
                } ).get();
        try {
            initializer.get();
            fail();
        } catch (ConcurrentException e) {
            assertEquals(concurrentException, e);
        }
    }

    private enum ExceptionToThrow {
        IOException,
        SQLException,
        NullPointerException
    }

    private static String methodThatThrowsException(final ExceptionToThrow input) throws IOException, SQLException {
        switch (input) {
        case IOException:
            throw new IOException();
        case SQLException:
            throw new SQLException();
        case NullPointerException:
            throw new NullPointerException();
        default: return "success";
        }
    }

}
