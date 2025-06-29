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
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.concurrent.CountDownLatch;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * <p>
 * An abstract base class for tests of concrete {@code ConcurrentInitializer} implementations.
 * </p>
 * <p>
 * This class provides some basic tests for initializer implementations. Derived class have to create a {@link ConcurrentInitializer} object on which the tests
 * are executed.
 * </p>
 *
 * @param <T> Domain type.
 */
public abstract class AbstractConcurrentInitializerTest<T> extends AbstractLangTest {

    static final class GetThread extends Thread {

        private Object object;
        private final CountDownLatch startLatch;
        private final ConcurrentInitializer<?> initializer;

        GetThread(final CountDownLatch startLatch, final ConcurrentInitializer<?> initializer) {
            this.startLatch = startLatch;
            this.initializer = initializer;
        }

        @Override
        public void run() {
            try {
                // wait until all threads are ready for maximum parallelism
                startLatch.await();
                // access the initializer
                object = initializer.get();
            } catch (final InterruptedException iex) {
                // ignore
            } catch (final ConcurrentException cex) {
                object = cex;
            }
        }
    }

    /**
     * Creates the {@link ConcurrentInitializer} object to be tested. This method is called whenever the test fixture needs to be obtained.
     *
     * @return the initializer object to be tested
     */
    protected abstract ConcurrentInitializer<T> createInitializer();

    /**
     * Tests a simple invocation of the get() method.
     *
     * @throws org.apache.commons.lang3.concurrent.ConcurrentException because the object under test may throw it.
     */
    @Test
    void testGet() throws ConcurrentException {
        assertNotNull(createInitializer().get(), "No managed object");
    }

    /**
     * Tests whether get() can be invoked from multiple threads concurrently. Always the same object should be returned.
     *
     * @throws org.apache.commons.lang3.concurrent.ConcurrentException because the object under test may throw it.
     * @throws InterruptedException                                    because the threading API my throw it.
     */
    @Test
    void testGetConcurrent() throws ConcurrentException, InterruptedException {
        final ConcurrentInitializer<T> initializer = createInitializer();
        final int threadCount = 20;
        final CountDownLatch startLatch = new CountDownLatch(1);
        final GetThread[] threads = new GetThread[threadCount];
        for (int i = 0; i < threadCount; i++) {
            threads[i] = new GetThread(startLatch, initializer);
            threads[i].start();
        }

        // fire all threads and wait until they are ready
        startLatch.countDown();
        for (final Thread t : threads) {
            t.join();
        }

        // check results
        final Object managedObject = initializer.get();
        for (final GetThread t : threads) {
            assertEquals(managedObject, t.object, "Wrong object");
        }
    }

    /**
     * Tests whether sequential get() invocations always return the same instance.
     *
     * @throws org.apache.commons.lang3.concurrent.ConcurrentException because the object under test may throw it.
     */
    @Test
    void testGetMultipleTimes() throws ConcurrentException {
        final ConcurrentInitializer<T> initializer = createInitializer();
        final Object obj = initializer.get();
        for (int i = 0; i < 10; i++) {
            assertEquals(obj, initializer.get(), "Got different object at " + i);
        }
    }

    /**
     * Tests a simple invocation of the isInitialized() method.
     *
     * @throws Throwable on test failure.
     */
    @Test
    void testisInitialized() throws Throwable {
        final ConcurrentInitializer<T> initializer = createInitializer();
        if (initializer instanceof AbstractConcurrentInitializer) {
            @SuppressWarnings("unchecked")
            final AbstractConcurrentInitializer<T, Exception> castedInitializer = (AbstractConcurrentInitializer<T, Exception>) initializer;
            assertFalse(castedInitializer.isInitialized(), "was initialized before get()");
            assertNotNull(castedInitializer.get(), "No managed object");
            assertTrue(castedInitializer.isInitialized(), "was not initialized after get()");
        }
    }
}
