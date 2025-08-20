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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.IOException;
import java.nio.file.FileSystemException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import org.apache.commons.lang3.exception.ExceptionUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Timeout.ThreadMode;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Test class for {@code AtomicSafeInitializer} which also serves as a simple example.
 */
class AtomicSafeInitializerTest extends AbstractConcurrentInitializerTest<Object> {

    /**
     * A concrete test implementation of {@code AtomicSafeInitializer} which also serves as a simple example.
     * <p>
     * This implementation also counts the number of invocations of the initialize() method.
     * </p>
     */
    private static final class AtomicSafeInitializerTestImpl extends AtomicSafeInitializer<Object> {

        /** A counter for initialize() invocations. */
        final AtomicInteger initCounter = new AtomicInteger();

        @Override
        protected Object initialize() {
            initCounter.incrementAndGet();
            return new Object();
        }
    }

    /** The instance to be tested. */
    private AtomicSafeInitializerTestImpl initializer;

    /**
     * Returns the initializer to be tested.
     *
     * @return the {@code AtomicSafeInitializer} under test.
     */
    @Override
    protected ConcurrentInitializer<Object> createInitializer() {
        return initializer;
    }

    @BeforeEach
    public void setUp() {
        initializer = new AtomicSafeInitializerTestImpl();
    }

    @Test
    void testGetThatReturnsNullFirstTime() throws ConcurrentException {
        final AtomicSafeInitializer<Object> initializer = new AtomicSafeInitializer<Object>() {

            final AtomicBoolean firstRun = new AtomicBoolean(true);

            @Override
            protected Object initialize() {
                if (firstRun.getAndSet(false)) {
                    return null;
                }
                return new Object();
            }
        };
        assertNull(initializer.get());
        assertNull(initializer.get());
    }

    @ParameterizedTest
    @ValueSource(classes = { IOException.class, Exception.class, FileSystemException.class, ReflectiveOperationException.class, ConcurrentException.class })
    @Timeout(value = 5, unit = TimeUnit.SECONDS, threadMode = ThreadMode.SAME_THREAD)
    void testInitializerThrowsChecked(final Class<Exception> throwableClass) throws ConcurrentException {
        final String message = "Initializing";
        final AtomicSafeInitializer<Object> asi = AtomicSafeInitializer.builder().setInitializer(() -> {
            throw throwableClass.getConstructor(String.class).newInstance(message);
        }).get();
        final String expected = throwableClass.getSimpleName() + ": " + message;
        assertEquals(expected, ExceptionUtils.getRootCauseMessage(assertThrows(ConcurrentException.class, asi::get)));
        assertEquals(expected, ExceptionUtils.getRootCauseMessage(assertThrows(ConcurrentException.class, asi::get)));
    }

    @ParameterizedTest
    @ValueSource(classes = { IllegalStateException.class, IllegalArgumentException.class, NullPointerException.class, RuntimeException.class })
    @Timeout(value = 5, unit = TimeUnit.SECONDS, threadMode = ThreadMode.SAME_THREAD)
    void testInitializerThrowsUnchecked(final Class<Exception> throwableClass) throws ConcurrentException {
        final String message = "Initializing";
        final AtomicSafeInitializer<Object> asi = AtomicSafeInitializer.builder().setInitializer(() -> {
            throw throwableClass.getConstructor(String.class).newInstance(message);
        }).get();
        assertEquals(message, assertThrows(throwableClass, asi::get).getMessage());
        assertEquals(message, assertThrows(throwableClass, asi::get).getMessage());
    }

    /**
     * Tests that initialize() is called only once.
     *
     * @throws org.apache.commons.lang3.concurrent.ConcurrentException because {@link #testGetConcurrent()} may throw it.
     * @throws InterruptedException                                    because {@link #testGetConcurrent()} may throw it.
     */
    @Test
    void testNumberOfInitializeInvocations() throws ConcurrentException, InterruptedException {
        testGetConcurrent();
        assertEquals(1, initializer.initCounter.get(), "Wrong number of invocations");
    }
}
