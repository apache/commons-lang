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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.util.concurrent.CountDownLatch;

import org.junit.Test;

/**
 * <p>
 * An abstract base class for tests of concrete {@code ConcurrentInitializer}
 * implementations.
 * </p>
 * <p>
 * This class provides some basic tests for initializer implementations. Derived
 * class have to create a {@link ConcurrentInitializer} object on which the
 * tests are executed.
 * </p>
 *
 * @version $Id$
 */
public abstract class AbstractConcurrentInitializerTest {
    /**
     * Tests a simple invocation of the get() method.
     *
     * @throws org.apache.commons.lang3.concurrent.ConcurrentException because the object under test may throw it
     */
    @Test
    public void testGet() throws ConcurrentException {
        assertNotNull("No managed object", createInitializer().get());
    }

    /**
     * Tests whether sequential get() invocations always return the same
     * instance.
     *
     * @throws org.apache.commons.lang3.concurrent.ConcurrentException because the object under test may throw it
     */
    @Test
    public void testGetMultipleTimes() throws ConcurrentException {
        final ConcurrentInitializer<Object> initializer = createInitializer();
        final Object obj = initializer.get();
        for (int i = 0; i < 10; i++) {
            assertEquals("Got different object at " + i, obj, initializer.get());
        }
    }

    /**
     * Tests whether get() can be invoked from multiple threads concurrently.
     * Always the same object should be returned.
     *
     * @throws org.apache.commons.lang3.concurrent.ConcurrentException because the object under test may throw it
     * @throws java.lang.InterruptedException because the threading API my throw it
     */
    @Test
    public void testGetConcurrent() throws ConcurrentException,
            InterruptedException {

        this.testGetConcurrentOptionallyWithException(false, null, null);
    }

    /**
     * Tests the handling of exceptions thrown on the initialized when multiple threads execute concurrently.
     * Always an exception with the same message and cause should be thrown.
     *
     * @throws org.apache.commons.lang3.concurrent.ConcurrentException because the object under test may throw it
     * @throws java.lang.InterruptedException because the threading API my throw it
     */
    public void testGetConcurrentWithException(String expectedMessage,
                                               Exception expectedCause)
            throws ConcurrentException, InterruptedException {

        this.testGetConcurrentOptionallyWithException(true, expectedMessage, expectedCause);
    }

    /**
     * Tests whether get() can be invoked from multiple threads concurrently.  Supports the exception-handling case
     * and the normal, non-exception case.
     *
     * Always the same object should be returned, or an exception with the same message and cause should be thrown.
     *
     * @throws org.apache.commons.lang3.concurrent.ConcurrentException because the object under test may throw it
     * @throws java.lang.InterruptedException because the threading API my throw it
     */
    protected void testGetConcurrentOptionallyWithException(boolean expectExceptions, String expectedMessage,
                                                            Exception expectedCause)
            throws ConcurrentException, InterruptedException {

        final ConcurrentInitializer<Object> initializer = expectExceptions ?
                createExceptionThrowingInitializer() :
                createInitializer();

        final int threadCount = 20;
        final CountDownLatch startLatch = new CountDownLatch(1);
        class GetThread extends Thread {
            Object object;

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

        final GetThread[] threads = new GetThread[threadCount];
        for (int i = 0; i < threadCount; i++) {
            threads[i] = new GetThread();
            threads[i].start();
        }

        // fire all threads and wait until they are ready
        startLatch.countDown();
        for (final Thread t : threads) {
            t.join();
        }

        // check results
        if ( expectExceptions ) {
            for (GetThread t : threads) {
                assertTrue(t.object instanceof Exception);
                Exception exc = (Exception) t.object;
                assertEquals(expectedMessage, exc.getMessage());
                assertSame(expectedCause, exc.getCause());
            }
        } else {
            final Object managedObject = initializer.get();
            for (final GetThread t : threads) {
                assertEquals("Wrong object", managedObject, t.object);
            }
        }
    }

    /**
     * Creates the {@link ConcurrentInitializer} object to be tested. This
     * method is called whenever the test fixture needs to be obtained.
     *
     * @return the initializer object to be tested
     */
    protected abstract ConcurrentInitializer<Object> createInitializer();

    /**
     * Creates a {@link ConcurrentInitializer} object that always throws
     * exceptions.
     *
     * @return
     */
    protected abstract ConcurrentInitializer<Object> createExceptionThrowingInitializer();
}
