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
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Arrays;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.exception.UncheckedInterruptedException;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link UncheckedFuture}.
 */
class UncheckedFutureTest extends AbstractLangTest {

    private static final class TestFuture<V> extends AbstractFutureProxy<V> {

        private final Exception exception;

        TestFuture(final Exception throwable) {
            super(ConcurrentUtils.constantFuture(null));
            this.exception = throwable;
        }

        TestFuture(final V value) {
            super(ConcurrentUtils.constantFuture(value));
            this.exception = null;
        }

        @SuppressWarnings("unchecked") // Programming error if call site blows up at runtime.
        private <T extends Exception> void checkException() throws T {
            if (exception != null) {
                throw (T) exception;
            }
        }

        @Override
        public V get() throws InterruptedException, ExecutionException {
            checkException();
            return super.get();
        }

        @Override
        public V get(final long timeout, final TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
            checkException();
            return super.get(timeout, unit);
        }

    }

    private static void assertInterruptPreserved(final Consumer<UncheckedFuture<Integer>> call) throws Exception {
        final CountDownLatch enteredGet = new CountDownLatch(1);
        final Future<Integer> blockingFuture = new AbstractFutureProxy<Integer>(ConcurrentUtils.constantFuture(42)) {

            private final CountDownLatch neverRelease = new CountDownLatch(1);

            @Override
            public Integer get() throws InterruptedException {
                enteredGet.countDown();
                neverRelease.await();
                throw new AssertionError("We should not get here");
            }

            @Override
            public Integer get(final long timeout, final TimeUnit unit) throws InterruptedException {
                enteredGet.countDown();
                neverRelease.await();
                throw new AssertionError("We should not get here");
            }

            @Override
            public boolean isDone() {
                return false;
            }
        };
        final UncheckedFuture<Integer> uf = UncheckedFuture.on(blockingFuture);
        final AtomicReference<Throwable> thrown = new AtomicReference<>();
        final AtomicBoolean interruptObserved = new AtomicBoolean(false);
        final Thread worker = new Thread(() -> {
            try {
                call.accept(uf);
                thrown.set(new AssertionError("We should not get here"));
            } catch (final Throwable e) {
                interruptObserved.set(Thread.currentThread().isInterrupted());
                thrown.set(e);
            }
        }, "unchecked-future-test-worker");
        worker.start();
        assertTrue(enteredGet.await(2, TimeUnit.SECONDS), "Worker did not enter Future.get() in time");
        worker.interrupt();
        worker.join();
        final Throwable t = thrown.get();
        assertInstanceOf(UncheckedInterruptedException.class, t, "Unexpected exception: " + t);
        assertInstanceOf(InterruptedException.class, t.getCause(), "Cause should be InterruptedException");
        assertTrue(interruptObserved.get(), "Interrupt flag was not restored by the wrapper");
    }

    @Test
    void interruptFlagIsPreservedOnGet() throws Exception {
        assertInterruptPreserved(UncheckedFuture::get);
    }

    @Test
    void interruptFlagIsPreservedOnGetWithTimeout() throws Exception {
        assertInterruptPreserved(uf -> uf.get(1, TimeUnit.DAYS));
    }

    @Test
    void testGetExecutionException() {
        final ExecutionException e = new ExecutionException(new Exception());
        assertThrows(UncheckedExecutionException.class, () -> UncheckedFuture.on(new TestFuture<>(e)).get());
    }

    @Test
    void testGetInterruptedException() {
        final InterruptedException e = new InterruptedException();
        assertThrows(UncheckedInterruptedException.class, () -> UncheckedFuture.on(new TestFuture<>(e)).get());
    }

    @Test
    void testGetLongExecutionException() {
        final ExecutionException e = new ExecutionException(new Exception());
        assertThrows(UncheckedExecutionException.class, () -> UncheckedFuture.on(new TestFuture<>(e)).get(1, TimeUnit.MICROSECONDS));
    }

    @Test
    void testGetLongInterruptedException() {
        final InterruptedException e = new InterruptedException();
        assertThrows(UncheckedInterruptedException.class, () -> UncheckedFuture.on(new TestFuture<>(e)).get(1, TimeUnit.MICROSECONDS));
    }

    @Test
    void testGetLongTimeoutException() {
        final TimeoutException e = new TimeoutException();
        assertThrows(UncheckedTimeoutException.class, () -> UncheckedFuture.on(new TestFuture<>(e)).get(1, TimeUnit.MICROSECONDS));
    }

    @Test
    void testMap() {
        final List<String> expected = Arrays.asList("Y", "Z");
        final List<Future<String>> input = Arrays.asList(new TestFuture<>("Y"), new TestFuture<>("Z"));
        assertEquals(expected, UncheckedFuture.map(input).map(UncheckedFuture::get).collect(Collectors.toList()));
    }

    @Test
    void testOnCollection() {
        final List<String> expected = Arrays.asList("Y", "Z");
        final List<Future<String>> input = Arrays.asList(new TestFuture<>("Y"), new TestFuture<>("Z"));
        assertEquals(expected, UncheckedFuture.on(input).stream().map(UncheckedFuture::get).collect(Collectors.toList()));
    }

    @Test
    void testOnFuture() {
        assertEquals("Z", UncheckedFuture.on(new TestFuture<>("Z")).get());
    }
}
