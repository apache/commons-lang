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

import org.junit.Before;
import org.junit.Test;

/**
 * Test class for {@code LazyInitializer}.
 *
 * @version $Id$
 */
public class LazyInitializerTest extends AbstractConcurrentInitializerTest {
    /** The initializer to be tested. */
    private LazyInitializerTestImpl initializer;
    private ExceptionThrowingLazyInitializerTestImpl exceptionThrowingInitializer;
    private Exception testCauseException;
    private String testExceptionMessage;

    @Before
    public void setUp() throws Exception {
        initializer = new LazyInitializerTestImpl();
        exceptionThrowingInitializer = new ExceptionThrowingLazyInitializerTestImpl();
        testExceptionMessage = "x-test-exception-message-x";
        testCauseException = new Exception(testExceptionMessage);
    }

    /**
     * Returns the initializer to be tested. This implementation returns the
     * {@code LazyInitializer} created in the {@code setUp()} method.
     *
     * @return the initializer to be tested
     */
    @Override
    protected ConcurrentInitializer<Object> createInitializer() {
        return initializer;
    }

    @Override
    protected ConcurrentInitializer<Object> createExceptionThrowingInitializer() {
        return exceptionThrowingInitializer;
    }

    @Test
    public void testGetConcurrentWithException ()
            throws ConcurrentException, InterruptedException {

        super.testGetConcurrentWithException(testExceptionMessage, testCauseException);
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


    /**
     * A concrete test implementation of {@code AtomicSafeInitializer}.  This
     * implementation always throws an exception.
     */
    private class ExceptionThrowingLazyInitializerTestImpl extends LazyInitializer<Object> {
        @Override
        protected Object initialize() throws ConcurrentException {
            throw new ConcurrentException(testExceptionMessage, testCauseException);
        }
    }
}
