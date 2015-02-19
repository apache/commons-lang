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
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Test class for {@code AtomicSafeInitializer}.
 *
 * @version $Id$
 */
public class AtomicSafeInitializerTest extends
        AbstractConcurrentInitializerTest {

    /** The instance to be tested. */
    private AtomicSafeInitializerTestImpl initializer;
    private ExceptionThrowingAtomicSafeInitializerTestImpl exceptionThrowingInitializer;
    private Exception testCauseException;
    private String testExceptionMessage;

    @Before
    public void setUp() throws Exception {
        initializer = new AtomicSafeInitializerTestImpl();
        exceptionThrowingInitializer = new ExceptionThrowingAtomicSafeInitializerTestImpl();
        testExceptionMessage = "x-test-exception-message-x";
        testCauseException = new Exception(testExceptionMessage);
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
     * Returns the exception-throwing initializer to be tested.
     *
     * @return the {@code AtomicSafeInitializer} under test when validating
     * exception handling
     */
    @Override
    protected ConcurrentInitializer<Object> createExceptionThrowingInitializer() {
        return exceptionThrowingInitializer;
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
        assertEquals("Wrong number of invocations", 1,
                initializer.initCounter.get());
    }

    @Test
    public void testExceptionOnInitialize() throws ConcurrentException,
            InterruptedException {

        testGetConcurrentWithException(testExceptionMessage, testCauseException);
    }

    /**
     * Validate the handling of an interrupted exception on a thread waiting for another thread to finish calling the
     * initialize() method.
     *
     * @throws Exception
     */
    @Test(timeout = 3000)
    public void testInterruptedWaitingOnInitialize() throws Exception {
        this.execTestWithWaitingOnInitialize(true);
    }

    /**
     * Test the success path of two threads reaching the initialization point at the same time.
     */
    @Test(timeout = 3000)
    public void testOneThreadWaitingForAnotherToInitialize () throws Exception {
        execTestWithWaitingOnInitialize(false);
    }


    /**
     * Execute a test that requires one thread to be waiting on the initialize() method of another thread.  This test
     * uses latches to guarantee the code path being tested.
     *
     * @throws Exception
     */
    protected void execTestWithWaitingOnInitialize(boolean interruptInd) throws Exception {
        final CountDownLatch startLatch = new CountDownLatch(1);
        final CountDownLatch finishLatch = new CountDownLatch(1);
        final WaitingInitializerTestImpl initializer = new WaitingInitializerTestImpl(startLatch, finishLatch);

        InitializerTestThread execThread1 = new InitializerTestThread(initializer);
        InitializerTestThread execThread2 = new InitializerTestThread(initializer);

        // Start the first thread and wait for it to get into the initialize method so we are sure it is the thread
        //  executing initialize().
        execThread1.start();
        startLatch.await();

        // Start the second thread and interrupt it to force the InterruptedException.  There is no need to make sure
        //  the thread reaches the await() call before interrupting it.
        execThread2.start();

        if ( interruptInd ) {
            // Interrupt the second thread now and wait for it to complete to ensure it reaches the wait inside the
            //  get() method.
            execThread2.interrupt();
            execThread2.join();
        }

        // Signal the completion of the initialize method now.
        finishLatch.countDown();

        // Wait for the initialize() to finish.
        execThread1.join();

        // Wait for thread2 to finish, if it was not already done
        if ( ! interruptInd ) {
            execThread2.join();
        }

        //
        // Validate: thread1 should have the valid result; thread2 should have caught an interrupted exception, if
        //  interrupted, or should have the same result otherwise.
        //
        assertFalse(execThread1.isCaughtException());
        assertSame(initializer.getAnswer(), execThread1.getResult());

        if ( interruptInd ) {
            assertTrue(execThread2.isCaughtException());
            Exception exc = (Exception) execThread2.getResult();
            assertTrue(exc.getCause() instanceof InterruptedException);
        } else {
            assertFalse(execThread2.isCaughtException());
            assertSame(initializer.getAnswer(), execThread2.getResult());
        }
    }

    /**
     * A concrete test implementation of {@code AtomicSafeInitializer}. This
     * implementation also counts the number of invocations of the initialize()
     * method.
     */
    private static class AtomicSafeInitializerTestImpl extends
            AtomicSafeInitializer<Object> {
        /** A counter for initialize() invocations. */
        final AtomicInteger initCounter = new AtomicInteger();

        @Override
        protected Object initialize() throws ConcurrentException {
            initCounter.incrementAndGet();
            return new Object();
        }
    }

    /**
     * A concrete test implementation of {@code AtomicSafeInitializer}.  This
     * implementation always throws an exception.
     */
    private class ExceptionThrowingAtomicSafeInitializerTestImpl extends AtomicSafeInitializer<Object> {
        @Override
        protected Object initialize() throws ConcurrentException {
            throw new ConcurrentException(testExceptionMessage, testCauseException);
        }
    }

    /**
     * Initializer that signals it has started and waits to complete until signalled in order to enable a guaranteed
     * order-of-operations.  This allows the test code to peg one thread to the initialize method for a period of time
     * that the test can dictate.
     */
    private class WaitingInitializerTestImpl extends AtomicSafeInitializer<Object> {
        private final CountDownLatch startedLatch;
        private final CountDownLatch finishLatch;
        private final Object answer = new Object();

        public WaitingInitializerTestImpl(CountDownLatch startedLatch, CountDownLatch finishLatch) {
            this.startedLatch = startedLatch;
            this.finishLatch = finishLatch;
        }

        @Override
        protected Object initialize() throws ConcurrentException {
            this.startedLatch.countDown();
            try {
                this.finishLatch.await();
            } catch (InterruptedException intExc) {
                throw new ConcurrentException(intExc);
            }

            return  answer;
        }

        public Object getAnswer () {
            return answer;
        }
    }

    /**
     * Test executor of the initializer get() operation that captures the result.
     */
    private class InitializerTestThread extends Thread {
        private AtomicSafeInitializer<Object>   initializer;
        private Object result;
        private boolean caughtException;

        public InitializerTestThread(AtomicSafeInitializer<Object> initializer) {
            super("AtomicSafeInitializer test thread");
            this.initializer = initializer;
        }

        @Override
        public void run() {
            try {
                this.result = initializer.get();
            } catch ( ConcurrentException concurrentExc ) {
                this.caughtException = true;
                this.result = concurrentExc;
            }
        }

        /**
         * Resulting object, if the get() method returned successfully, or exception if an exception was thrown.
         *
         * @return resulting object or exception from the get() method call.
         */
        public Object getResult () {
            return  this.result;
        }

        /**
         * Determine whether an exception was caught on the get() call.  Does not guarantee that the get() method was
         * called or completed.
         *
         * @return true => exception was caught; false => exception was not caught.
         */
        public boolean  isCaughtException () {
            return  this.caughtException;
        }
    }
}
