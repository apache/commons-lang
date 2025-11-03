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

import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.concurrent.atomic.AtomicInteger;

import org.junit.jupiter.api.Test;

/**
 * Test class for {@code AtomicInitializer}.
 */
class AtomicInitializerNonObjectTest extends AbstractConcurrentInitializerTest<Integer> {

    /**
     * Returns the initializer to be tested.
     *
     * @return the {@code AtomicInitializer}
     */
    @Override
    protected ConcurrentInitializer<Integer> createInitializer() {
        return new AtomicInitializer<Integer>() {
            @Override
            protected Integer initialize() {
                return Integer.valueOf(0);
            }
        };
    }

    @Test
    void testGetThatReturnsNullFirstTime() throws ConcurrentException {
        final AtomicInitializer<Integer> initializer = new AtomicInitializer<Integer>() {
            final AtomicInteger firstRun = new AtomicInteger(1);

            @Override
            protected Integer initialize() {
                if (firstRun.getAndSet(0) == 1) {
                    return null;
                }
                return Integer.valueOf(0);
            }
        };
        assertNull(initializer.get());
        assertNull(initializer.get());
    }
}
