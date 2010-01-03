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

import java.util.concurrent.CountDownLatch;

import junit.framework.TestCase;

/**
 * Test class for {@code LazyInitializer}.
 *
 * @version $Id$
 */
public class LazyInitializerTest extends TestCase {
    /** The initializer to be tested. */
    private LazyInitializerTestImpl initializer;

    @Override
    protected void setUp() throws Exception {
        super.setUp();
        initializer = new LazyInitializerTestImpl();
    }

    /**
     * Tests obtaining the managed object.
     */
    public void testGet() {
        assertNotNull("No managed object", initializer.get());
    }

    /**
     * Tests whether sequential get() invocations always return the same
     * instance.
     */
    public void testGetMultipleTimes() {
        Object obj = initializer.get();
        for (int i = 0; i < 10; i++) {
            assertEquals("Got different object at " + i, obj, initializer.get());
        }
    }

    /**
     * Tests invoking get() from multiple threads concurrently.
     */
    public void testGetConcurrent() throws InterruptedException {
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
                } catch (InterruptedException iex) {
                    // ignore
                }
            }
        }

        GetThread[] threads = new GetThread[threadCount];
        for (int i = 0; i < threadCount; i++) {
            threads[i] = new GetThread();
            threads[i].start();
        }

        // fire all threads and wait until they are ready
        startLatch.countDown();
        for (Thread t : threads) {
            t.join();
        }

        // check results
        Object managedObject = initializer.get();
        for (GetThread t : threads) {
            assertEquals("Wrong object", managedObject, t.object);
        }
    }

    /**
     * A test implementation of LazyInitializer. This class creates a plain
     * Object. As Object does not provide a specific equals() method, it is easy
     * to check whether multiple instances were created.
     */
    private static class LazyInitializerTestImpl extends
            LazyInitializer<Object> {
        @Override
        protected Object initialize() {
            return new Object();
        }
    }
}
