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

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import junit.framework.TestCase;

/**
 * Test class for {@code CallableBackgroundInitializer}
 *
 * @version $Id$
 */
public class CallableBackgroundInitializerTest extends TestCase {
    /** Constant for the result of the call() invocation. */
    private static final Integer RESULT = 42;

    /**
     * Tries to create an instance without a Callable. This should cause an
     * exception.
     */
    public void testInitNullCallable() {
        try {
            new CallableBackgroundInitializer<Object>(null);
            fail("Could create instance without a Callable!");
        } catch (IllegalArgumentException iex) {
            // ok
        }
    }

    /**
     * Tests whether the executor service is correctly passed to the super
     * class.
     */
    public void testInitExecutor() {
        ExecutorService exec = Executors.newSingleThreadExecutor();
        CallableBackgroundInitializer<Integer> init = new CallableBackgroundInitializer<Integer>(
                new TestCallable(), exec);
        assertEquals("Executor not set", exec, init.getExternalExecutor());
    }

    /**
     * Tries to pass a null Callable to the constructor that takes an executor.
     * This should cause an exception.
     */
    public void testInitExecutorNullCallable() {
        ExecutorService exec = Executors.newSingleThreadExecutor();
        try {
            new CallableBackgroundInitializer<Integer>(null, exec);
            fail("Could create instance without a Callable!");
        } catch (IllegalArgumentException iex) {
            // ok
        }
    }

    /**
     * Tests the implementation of initialize().
     */
    public void testInitialize() throws Exception {
        TestCallable call = new TestCallable();
        CallableBackgroundInitializer<Integer> init = new CallableBackgroundInitializer<Integer>(
                call);
        assertEquals("Wrong result", RESULT, init.initialize());
        assertEquals("Wrong number of invocations", 1, call.callCount);
    }

    /**
     * A test Callable implementation for checking the initializer's
     * implementation of the initialize() method.
     */
    private static class TestCallable implements Callable<Integer> {
        /** A counter for the number of call() invocations. */
        int callCount;

        /**
         * Records this invocation and returns the test result.
         */
        public Integer call() throws Exception {
            callCount++;
            return RESULT;
        }
    }
}
