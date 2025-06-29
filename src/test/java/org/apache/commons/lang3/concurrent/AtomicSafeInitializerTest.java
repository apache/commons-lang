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

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

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
     * @return the {@code AtomicSafeInitializer} under test
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

    /**
     * Tests that initialize() is called only once.
     *
     * @throws org.apache.commons.lang3.concurrent.ConcurrentException because {@link #testGetConcurrent()} may throw it
     * @throws InterruptedException because {@link #testGetConcurrent()} may throw it
     */
    @Test
    void testNumberOfInitializeInvocations() throws ConcurrentException, InterruptedException {
        testGetConcurrent();
        assertEquals(1, initializer.initCounter.get(), "Wrong number of invocations");
    }
}
