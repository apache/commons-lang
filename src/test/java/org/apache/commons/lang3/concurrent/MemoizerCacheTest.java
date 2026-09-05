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
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Tests failure retention, cache retention, and computation lock scope.
 */
class MemoizerCacheTest extends AbstractLangTest {

    private static final int TIMEOUT_SECONDS = 10;

    private static void await(final CountDownLatch latch) {
        try {
            assertTrue(latch.await(TIMEOUT_SECONDS, TimeUnit.SECONDS), "Timed out waiting for a test worker");
        } catch (final InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new AssertionError(e);
        }
    }

    private static <I, O> Memoizer<I, O> newMemoizer(final boolean useFunction, final Function<I, O> function) {
        return useFunction ? new Memoizer<>(function) : new Memoizer<>((Computable<I, O>) function::apply);
    }

    private static <I, O> Memoizer<I, O> newMemoizer(final boolean useFunction, final Function<I, O> function, final boolean recalculate) {
        return useFunction ? new Memoizer<>(function, recalculate) : new Memoizer<>((Computable<I, O>) function::apply, recalculate);
    }

    private static void shutdown(final ExecutorService executor) throws InterruptedException {
        executor.shutdownNow();
        assertTrue(executor.awaitTermination(TIMEOUT_SECONDS, TimeUnit.SECONDS), "Test workers did not terminate");
    }

    @ParameterizedTest
    @ValueSource(booleans = { false, true })
    void testConcurrentCallsForSameKeyComputeOnce(final boolean useFunction) throws Exception {
        final int callerCount = 8;
        final AtomicInteger calls = new AtomicInteger();
        final CountDownLatch ready = new CountDownLatch(callerCount);
        final CountDownLatch start = new CountDownLatch(1);
        final CountDownLatch entered = new CountDownLatch(1);
        final CountDownLatch release = new CountDownLatch(1);
        final Object result = new Object();
        final Memoizer<String, Object> memoizer = newMemoizer(useFunction, key -> {
            calls.incrementAndGet();
            entered.countDown();
            await(release);
            return result;
        });
        final ExecutorService executor = Executors.newFixedThreadPool(callerCount);
        try {
            final List<Future<Object>> futures = new ArrayList<>();
            for (int i = 0; i < callerCount; i++) {
                futures.add(executor.submit(() -> {
                    ready.countDown();
                    await(start);
                    return memoizer.compute("key");
                }));
            }
            await(ready);
            start.countDown();
            await(entered);
            release.countDown();
            for (final Future<Object> future : futures) {
                assertSame(result, future.get(TIMEOUT_SECONDS, TimeUnit.SECONDS));
            }
            assertSame(result, memoizer.compute("key"));
            assertEquals(1, calls.get());
        } finally {
            start.countDown();
            release.countDown();
            shutdown(executor);
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = { false, true })
    void testDefaultCachesFirstFailure(final boolean useFunction) throws Exception {
        final AtomicInteger calls = new AtomicInteger();
        final IllegalStateException failure = new IllegalStateException("Transient failure");
        final Memoizer<String, String> memoizer = newMemoizer(useFunction, key -> {
            if (calls.incrementAndGet() == 1) {
                throw failure;
            }
            return key;
        });
        for (int i = 0; i < 3; i++) {
            assertSame(failure, assertThrows(IllegalStateException.class, () -> memoizer.compute("failed")));
        }
        assertEquals(1, calls.get(), "A transient failure remains cached by default");
        assertEquals("other", memoizer.compute("other"));
        assertSame(failure, assertThrows(IllegalStateException.class, () -> memoizer.compute("failed")));
        assertEquals(2, calls.get());
    }

    @ParameterizedTest
    @ValueSource(booleans = { false, true })
    void testDistinctKeysRetainCachedResults(final boolean useFunction) throws Exception {
        final int keyCount = 1024;
        final AtomicInteger calls = new AtomicInteger();
        final Memoizer<Integer, Object> memoizer = newMemoizer(useFunction, key -> {
            calls.incrementAndGet();
            return new Object();
        });
        final List<Object> results = new ArrayList<>();
        for (int i = 0; i < keyCount; i++) {
            results.add(memoizer.compute(i));
        }
        // Characterize retention over a bounded sample without exhausting memory or inspecting the backing map.
        for (int i = 0; i < keyCount; i++) {
            assertSame(results.get(i), memoizer.compute(i));
        }
        assertEquals(keyCount, calls.get(), "Adding distinct keys must not evict earlier results");
    }

    @ParameterizedTest
    @ValueSource(booleans = { false, true })
    void testRecalculateRetriesFailureOnNextCall(final boolean useFunction) throws Exception {
        final AtomicInteger calls = new AtomicInteger();
        final IllegalStateException failure = new IllegalStateException("Transient failure");
        final Object result = new Object();
        final Memoizer<String, Object> memoizer = newMemoizer(useFunction, key -> {
            if (calls.incrementAndGet() == 1) {
                throw failure;
            }
            return result;
        }, true);
        assertSame(failure, assertThrows(IllegalStateException.class, () -> memoizer.compute("key")));
        assertEquals(1, calls.get(), "The failing call must propagate its failure without retrying internally");
        assertSame(result, memoizer.compute("key"));
        assertSame(result, memoizer.compute("key"));
        assertEquals(2, calls.get());
    }

    @ParameterizedTest
    @ValueSource(booleans = { false, true })
    void testReentrantComputationForDistinctCollidingKey(final boolean useFunction) throws Exception {
        assertEquals("Aa".hashCode(), "BB".hashCode());
        final AtomicInteger calls = new AtomicInteger();
        final AtomicReference<Memoizer<String, String>> reference = new AtomicReference<>();
        final Memoizer<String, String> memoizer = newMemoizer(useFunction, key -> {
            calls.incrementAndGet();
            if ("Aa".equals(key)) {
                try {
                    return reference.get().compute("BB");
                } catch (final InterruptedException e) {
                    Thread.currentThread().interrupt();
                    throw new AssertionError(e);
                }
            }
            return key;
        });
        reference.set(memoizer);
        final ExecutorService executor = Executors.newSingleThreadExecutor();
        try {
            assertEquals("BB", executor.submit(() -> memoizer.compute("Aa")).get(TIMEOUT_SECONDS, TimeUnit.SECONDS));
            assertEquals("BB", memoizer.compute("Aa"));
            assertEquals("BB", memoizer.compute("BB"));
            assertEquals(2, calls.get());
        } finally {
            shutdown(executor);
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = { false, true })
    void testSlowComputationDoesNotBlockDistinctCollidingKey(final boolean useFunction) throws Exception {
        assertEquals("Aa".hashCode(), "BB".hashCode());
        final CountDownLatch entered = new CountDownLatch(1);
        final CountDownLatch release = new CountDownLatch(1);
        final Memoizer<String, String> memoizer = newMemoizer(useFunction, key -> {
            if ("Aa".equals(key)) {
                entered.countDown();
                await(release);
            }
            return key;
        });
        final ExecutorService executor = Executors.newFixedThreadPool(2);
        try {
            final Future<String> slow = executor.submit(() -> memoizer.compute("Aa"));
            await(entered);
            final Future<String> other = executor.submit(() -> memoizer.compute("BB"));
            assertEquals("BB", other.get(TIMEOUT_SECONDS, TimeUnit.SECONDS));
            assertEquals(1L, release.getCount(), "The colliding key must complete while the first computation is blocked");
            release.countDown();
            assertEquals("Aa", slow.get(TIMEOUT_SECONDS, TimeUnit.SECONDS));
        } finally {
            release.countDown();
            shutdown(executor);
        }
    }
}
