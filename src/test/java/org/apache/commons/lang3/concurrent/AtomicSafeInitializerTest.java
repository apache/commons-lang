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

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test class for {@code AtomicSafeInitializer}.
 */
@ExtendWith(MockitoExtension.class)
public class AtomicSafeInitializerTest extends AbstractConcurrentInitializerTest {

    @Spy
    private AtomicSafeInitializer<Object> initializer;

    /**
     * Returns the initializer to be tested.
     *
     * @throws org.apache.commons.lang3.concurrent.ConcurrentException because {@link AtomicSafeInitializer#initialize()} may throw it
     * @return the {@code AtomicSafeInitializer} under test
     */
    @Override
    protected ConcurrentInitializer<Object> createInitializer() throws ConcurrentException {
        when(initializer.initialize()).thenReturn(new Object());
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
        when(initializer.initialize()).thenReturn(new Object());

        testGetConcurrent();

        verify(initializer, times(1)).initialize();
    }
}
