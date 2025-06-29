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

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * Tests {@code LazyInitializer}.
 */
class LazyInitializerSingleInstanceTest extends AbstractConcurrentInitializerTest<Object> {

    /**
     * A test implementation of LazyInitializer. This class creates a plain Object. As Object does not provide a specific equals() method, it is easy to check
     * whether multiple instances were created.
     */
    private static final class LazyInitializerTestImpl extends LazyInitializer<Object> {
        @Override
        protected Object initialize() {
            return new Object();
        }
    }

    /** The initializer to be tested. */
    private LazyInitializerTestImpl initializer;

    /**
     * Creates the initializer to be tested. This implementation returns the {@code LazyInitializer} created in the {@code setUp()} method.
     *
     * @return the initializer to be tested
     */
    @Override
    protected LazyInitializer<Object> createInitializer() {
        return initializer;
    }

    @BeforeEach
    public void setUp() {
        initializer = new LazyInitializerTestImpl();
    }

    @Test
    void testIsInitialized() throws ConcurrentException {
        final LazyInitializer<Object> initializer = createInitializer();
        assertFalse(initializer.isInitialized());
        initializer.get();
        assertTrue(initializer.isInitialized());
    }
}
