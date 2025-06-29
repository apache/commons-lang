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

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import org.apache.commons.lang3.function.FailableConsumer;
import org.apache.commons.lang3.function.FailableSupplier;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * Test class for {@code AtomicSafeInitializer} which also serves as a simple example.
 */
class AtomicSafeInitializerSupplierTest extends AbstractConcurrentInitializerCloseAndExceptionsTest<Object> {

    /** An initCounter used in testing. Reset before each test */
    private AtomicInteger initCounter = new AtomicInteger();

    /**
     * Creates the initializer to be tested.
     *
     * @return the {@code AtomicSafeInitializer} under test
     */
    @Override
    protected ConcurrentInitializer<Object> createInitializer() {
        return AtomicSafeInitializer.<Object>builder().setInitializer(this::incAndMakeObject).get();
    }

    @Override
    protected ConcurrentInitializer<CloseableObject> createInitializerThatThrowsException(
            final FailableSupplier<CloseableObject, ? extends Exception> supplier,
            final FailableConsumer<CloseableObject, ? extends Exception> closer) {
        return AtomicSafeInitializer.<CloseableObject>builder().setInitializer(supplier).setCloser(closer).get();
    }

    /** A supplier method used in testing */
    private Object incAndMakeObject() {
        initCounter.incrementAndGet();
        return new Object();
    }

    @BeforeEach
    public void setUp() {
        initCounter = new AtomicInteger();
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

    /**
     * Tests that initialize() is called only once.
     *
     * @throws org.apache.commons.lang3.concurrent.ConcurrentException because {@link #testGetConcurrent()} may throw it
     * @throws InterruptedException because {@link #testGetConcurrent()} may throw it
     */
    @Test
    void testNumberOfInitializeInvocations() throws ConcurrentException, InterruptedException {
        testGetConcurrent();
        assertEquals(1, initCounter.get(), "Wrong number of invocations");
    }
}
