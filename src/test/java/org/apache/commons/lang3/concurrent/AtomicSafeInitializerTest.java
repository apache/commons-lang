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

import java.util.concurrent.atomic.AtomicInteger;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.spy;

/**
 * Test class for {@code AtomicSafeInitializer}.
 */
public class AtomicSafeInitializerTest extends
        AbstractConcurrentInitializerTest {
    // Create variables for tracking behaviors of mock object

    AtomicInteger initializerInitCounter = new AtomicInteger();

    /** The instance to be tested. */
    private AtomicSafeInitializer<Object> initializer;

    @BeforeEach
    public void setUp() {
        // Construct mock object
        initializer = spy(AtomicSafeInitializer.class);
        // Method Stubs
        try {
            doAnswer((stubInvo) -> {
                initializerInitCounter.incrementAndGet();
                return new Object();
            }).when(initializer).initialize();
        } catch (Throwable exception) {
            exception.printStackTrace();
        }
    }

    /**
     * Returns the initializer to be tested.
     *
     * @return the {@code AtomicSafeInitializer} under test
     */
    @Override
    protected ConcurrentInitializer<Object> createInitializer() {
        return initializer;
    }

    /**
     * Tests that initialize() is called only once.
     *
     * @throws org.apache.commons.lang3.concurrent.ConcurrentException because {@link #testGetConcurrent()} may throw it
     * @throws java.lang.InterruptedException because {@link #testGetConcurrent()} may throw it
     */
    @Test
    public void testNumberOfInitializeInvocations() throws ConcurrentException,
            InterruptedException {
        testGetConcurrent();
        assertEquals(1, initializerInitCounter.get(), "Wrong number of invocations");
    }
}
