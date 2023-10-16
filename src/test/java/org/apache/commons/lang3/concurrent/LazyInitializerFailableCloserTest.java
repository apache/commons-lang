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

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.jupiter.api.Test;

/**
 * Tests {@code LazyInitializer}.
 */
public class LazyInitializerFailableCloserTest extends AbstractConcurrentInitializerTest {

    private final AtomicBoolean closed = new AtomicBoolean();

    /**
     * Creates the initializer to be tested. This implementation returns the {@code LazyInitializer} created in the {@code setUp()} method.
     *
     * @return the initializer to be tested
     */
    @Override
    protected LazyInitializer<Object> createInitializer() {
        return LazyInitializer.builder().setInitializer(this::makeObject).setCloser(e -> throwingCloser()).get();
    }

    private Object makeObject() throws ConcurrentException {
        if (closed.get()) {
            // Doesn't actually throw, just for the method sig without unused warning.
            throw new ConcurrentException("test", new IOException());
        }
        return new Object();
    }

    @Test
    public void testIsInitialized() throws ConcurrentException {
        final LazyInitializer<Object> initializer = createInitializer();
        assertFalse(initializer.isInitialized());
        initializer.get();
        assertTrue(initializer.isInitialized());
        assertFalse(closed.get());
        initializer.close();
        assertTrue(closed.get());
    }

    private void throwingCloser() throws ConcurrentException {
        closed.set(true);
        // always false:
        if (!closed.get()) {
            // Doesn't actually throw, just for the method sig without unused warning.
            throw new ConcurrentException("test", new IOException());
        }
    }

}
