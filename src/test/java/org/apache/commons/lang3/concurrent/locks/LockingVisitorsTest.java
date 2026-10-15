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
package org.apache.commons.lang3.concurrent.locks;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
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
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.concurrent.locks.StampedLock;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.concurrent.locks.LockingVisitors.LockVisitor;
import org.apache.commons.lang3.concurrent.locks.LockingVisitors.ReadWriteLockVisitor;
import org.apache.commons.lang3.concurrent.locks.LockingVisitors.ReentrantLockVisitor;
import org.apache.commons.lang3.concurrent.locks.LockingVisitors.StampedLockVisitor;
import org.apache.commons.lang3.function.FailableConsumer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Tests {@link LockingVisitors}.
 */
class LockingVisitorsTest extends AbstractLangTest {

    private static final int NUMBER_OF_THREADS = 10;

    private static boolean tryLock(final Lock lock) {
        if (!lock.tryLock()) {
            return false;
        }
        lock.unlock();
        return true;
    }

    private <T> void assertConcurrentReaders(final LockVisitor<T, ?> visitor, final ExecutorService executor) throws Exception {
        final CountDownLatch entered = new CountDownLatch(NUMBER_OF_THREADS);
        final CountDownLatch release = new CountDownLatch(1);
        final List<Future<?>> readers = new ArrayList<>();
        try {
            for (int i = 0; i < NUMBER_OF_THREADS; i++) {
                readers.add(executor.submit(() -> visitor.acceptReadLocked(values -> {
                    entered.countDown();
                    assertTrue(release.await(10, TimeUnit.SECONDS), "Readers released");
                })));
            }
            // All callbacks must enter before any callback is allowed to leave.
            assertTrue(entered.await(10, TimeUnit.SECONDS), "Read callbacks run concurrently");
        } finally {
            release.countDown();
        }
        for (final Future<?> reader : readers) {
            reader.get(10, TimeUnit.SECONDS);
        }
    }

    private <T> void invoke(final LockVisitor<T, ?> visitor, final boolean write, final boolean apply,
            final FailableConsumer<T, Exception> consumer) {
        if (apply) {
            final T result;
            if (write) {
                result = visitor.applyWriteLocked(values -> {
                    consumer.accept(values);
                    return values;
                });
            } else {
                result = visitor.applyReadLocked(values -> {
                    consumer.accept(values);
                    return values;
                });
            }
            assertSame(visitor.getObject(), result);
        } else if (write) {
            visitor.acceptWriteLocked(consumer);
        } else {
            visitor.acceptReadLocked(consumer);
        }
    }

    private <T> void runTest(final boolean write, final boolean exclusive, final LockVisitor<T, ?> visitor,
            final Lock readLock, final Lock writeLock) throws Exception {
        assertNotNull(visitor.getLock());
        assertNotNull(visitor.getObject());
        final ExecutorService executor = Executors.newFixedThreadPool(NUMBER_OF_THREADS);
        try {
            for (final boolean apply : new boolean[] { false, true }) {
                for (final boolean fail : new boolean[] { false, true }) {
                    final AtomicInteger calls = new AtomicInteger();
                    final IllegalStateException failure = new IllegalStateException("Callback failure");
                    final FailableConsumer<T, Exception> consumer = values -> {
                        assertSame(visitor.getObject(), values);
                        calls.incrementAndGet();
                        // A single other thread probes both locks, so probes cannot mask a missing visitor lock.
                        executor.submit(() -> {
                            assertEquals(!exclusive, tryLock(readLock), "Read access during the callback");
                            assertFalse(tryLock(writeLock), "Write access during the callback");
                        }).get(10, TimeUnit.SECONDS);
                        if (fail) {
                            throw failure;
                        }
                    };
                    if (fail) {
                        assertSame(failure, assertThrows(IllegalStateException.class, () -> invoke(visitor, write, apply, consumer)));
                    } else {
                        invoke(visitor, write, apply, consumer);
                    }
                    assertEquals(1, calls.get(), "Callback invoked exactly once");
                    // Probe after both successful and failing callbacks, from a thread that does not own the lock.
                    executor.submit(() -> {
                        assertTrue(tryLock(readLock), "Read lock released after the callback");
                        assertTrue(tryLock(writeLock), "Write lock released after the callback");
                    }).get(10, TimeUnit.SECONDS);
                }
            }
            if (!exclusive) {
                assertConcurrentReaders(visitor, executor);
            }
        } finally {
            executor.shutdownNow();
            assertTrue(executor.awaitTermination(10, TimeUnit.SECONDS));
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = { true, false })
    void testBuilderLockVisitor(final boolean fair) throws Exception {
        final AtomicInteger obj = new AtomicInteger();
        final ReadWriteLock lock = new ReentrantReadWriteLock(fair);
        // @formatter:off
        final LockVisitor<AtomicInteger, ReadWriteLock> lockVisitor = new LockVisitor.LVBuilder()
          .setObject(obj)
          .setLock(lock)
          .setReadLockSupplier(lock::readLock)
          .setWriteLockSupplier(lock::writeLock)
          .get();
        // @formatter:on
        lockVisitor.acceptReadLocked(AtomicInteger::incrementAndGet);
        assertEquals(1, obj.get());
        lockVisitor.acceptReadLocked(AtomicInteger::incrementAndGet);
        assertEquals(2, obj.get());
        runTest(false, false, lockVisitor, lock.readLock(), lock.writeLock());
        runTest(true, true, lockVisitor, lock.readLock(), lock.writeLock());
    }

    @ParameterizedTest
    @ValueSource(booleans = { true, false })
    void testBuilderReadWriteLockVisitor(final boolean fair) throws Exception {
        final AtomicInteger obj = new AtomicInteger();
        final ReadWriteLock lock = new ReentrantReadWriteLock(fair);
        // @formatter:off
        final LockingVisitors.ReadWriteLockVisitor<AtomicInteger> lockVisitor = ReadWriteLockVisitor.<AtomicInteger>builder()
          .setObject(obj)
          .setLock(lock)
          .get();
        // @formatter:on
        lockVisitor.acceptReadLocked(AtomicInteger::incrementAndGet);
        assertEquals(1, obj.get());
        lockVisitor.acceptReadLocked(AtomicInteger::incrementAndGet);
        assertEquals(2, obj.get());
        runTest(false, false, lockVisitor, lock.readLock(), lock.writeLock());
        runTest(true, true, lockVisitor, lock.readLock(), lock.writeLock());
    }

    @ParameterizedTest
    @ValueSource(booleans = { true, false })
    void testBuilderReentrantLockVisitor(final boolean fair) throws Exception {
        final AtomicInteger obj = new AtomicInteger();
        final ReentrantLock lock = new ReentrantLock(fair);
        // @formatter:off
        final LockingVisitors.ReentrantLockVisitor<AtomicInteger> lockVisitor = ReentrantLockVisitor.<AtomicInteger>builder()
          .setObject(obj)
          .setLock(lock)
          .get();
        // @formatter:on
        lockVisitor.acceptReadLocked(AtomicInteger::incrementAndGet);
        assertEquals(1, obj.get());
        lockVisitor.acceptReadLocked(AtomicInteger::incrementAndGet);
        assertEquals(2, obj.get());
        runTest(false, true, lockVisitor, lock, lock);
        runTest(true, true, lockVisitor, lock, lock);
    }

    @ParameterizedTest
    @ValueSource(booleans = { true, false })
    void testBuilderReentrantReadWriteLockVisitor(final boolean fair) throws Exception {
        final AtomicInteger obj = new AtomicInteger();
        final ReentrantReadWriteLock lock = new ReentrantReadWriteLock(fair);
        // @formatter:off
        final LockingVisitors.ReadWriteLockVisitor<AtomicInteger> lockVisitor = ReadWriteLockVisitor.<AtomicInteger>builder()
          .setObject(obj)
          .setLock(lock)
          .get();
        // @formatter:on
        lockVisitor.acceptReadLocked(AtomicInteger::incrementAndGet);
        assertEquals(1, obj.get());
        lockVisitor.acceptReadLocked(AtomicInteger::incrementAndGet);
        assertEquals(2, obj.get());
        runTest(false, false, lockVisitor, lock.readLock(), lock.writeLock());
        runTest(true, true, lockVisitor, lock.readLock(), lock.writeLock());
    }

    @Test
    void testBuilderReentrantStampedLockVisitor() throws Exception {
        final AtomicInteger obj = new AtomicInteger();
        final StampedLock lock = new StampedLock();
        // @formatter:off
        final LockingVisitors.StampedLockVisitor<AtomicInteger> lockVisitor = StampedLockVisitor.<AtomicInteger>builder()
          .setObject(obj)
          .setLock(lock)
          .get();
        // @formatter:on
        lockVisitor.acceptReadLocked(AtomicInteger::incrementAndGet);
        assertEquals(1, obj.get());
        lockVisitor.acceptReadLocked(AtomicInteger::incrementAndGet);
        assertEquals(2, obj.get());
        runTest(false, false, lockVisitor, lock.asReadLock(), lock.asWriteLock());
        runTest(true, true, lockVisitor, lock.asReadLock(), lock.asWriteLock());
    }

    @Test
    void testCreate() {
        final AtomicInteger obj = new AtomicInteger();
        final ReadWriteLock lock = new ReentrantReadWriteLock();
        LockingVisitors.create(obj, lock).acceptReadLocked(AtomicInteger::incrementAndGet);
        LockingVisitors.create(obj, lock).acceptReadLocked(null);
        assertEquals(1, obj.get());
        LockingVisitors.create(obj, lock).acceptWriteLocked(AtomicInteger::incrementAndGet);
        LockingVisitors.create(obj, lock).acceptWriteLocked(null);
        assertEquals(2, obj.get());
    }

    @SuppressWarnings("deprecation")
    @Test
    void testDeprecatedConstructor() {
        assertNotNull(new LockingVisitors().toString());
    }

    @Test
    void testReentrantLock() throws Exception {
        final boolean[] values = new boolean[NUMBER_OF_THREADS];
        final ReentrantLockVisitor<boolean[]> visitor = LockingVisitors.reentrantLockVisitor(values);
        assertSame(values, visitor.getObject());
        runTest(false, true, visitor, visitor.getLock(), visitor.getLock());
        runTest(true, true, visitor, visitor.getLock(), visitor.getLock());
    }

    @ParameterizedTest
    @ValueSource(booleans = { true, false })
    void testReentrantLockFairness(final boolean fairness) throws Exception {
        final boolean[] values = new boolean[NUMBER_OF_THREADS];
        final ReentrantLock lock = new ReentrantLock(fairness);
        final ReentrantLockVisitor<boolean[]> visitor = LockingVisitors.create(values, lock);
        assertSame(values, visitor.getObject());
        runTest(false, true, visitor, lock, lock);
        runTest(true, true, visitor, lock, lock);
    }

    @Test
    void testReentrantReadWriteLockExclusive() throws Exception {
        final boolean[] values = new boolean[NUMBER_OF_THREADS];
        final ReadWriteLockVisitor<boolean[]> visitor = LockingVisitors.reentrantReadWriteLockVisitor(values);
        assertSame(values, visitor.getObject());
        runTest(true, true, visitor, visitor.getLock().readLock(), visitor.getLock().writeLock());
    }

    @Test
    void testReentrantReadWriteLockNotExclusive() throws Exception {
        final boolean[] values = new boolean[NUMBER_OF_THREADS];
        final ReadWriteLockVisitor<boolean[]> visitor = LockingVisitors.reentrantReadWriteLockVisitor(values);
        assertSame(values, visitor.getObject());
        runTest(false, false, visitor, visitor.getLock().readLock(), visitor.getLock().writeLock());
    }

    @Test
    void testResultValidation() {
        final Object hidden = new Object();
        final StampedLockVisitor<Object> lock = LockingVisitors.stampedLockVisitor(hidden);
        final Object o1 = lock.applyReadLocked(h -> new Object());
        assertNotNull(o1);
        assertNotSame(hidden, o1);
        final Object o2 = lock.applyWriteLocked(h -> new Object());
        assertNotNull(o2);
        assertNotSame(hidden, o2);
    }

    @Test
    void testStampedLockExclusive() throws Exception {
        final boolean[] values = new boolean[NUMBER_OF_THREADS];
        final StampedLockVisitor<boolean[]> visitor = LockingVisitors.stampedLockVisitor(values);
        assertSame(values, visitor.getObject());
        runTest(true, true, visitor, visitor.getLock().asReadLock(), visitor.getLock().asWriteLock());
    }

    @Test
    void testStampedLockNotExclusive() throws Exception {
        final boolean[] values = new boolean[NUMBER_OF_THREADS];
        final StampedLockVisitor<boolean[]> visitor = LockingVisitors.stampedLockVisitor(values);
        assertSame(values, visitor.getObject());
        runTest(false, false, visitor, visitor.getLock().asReadLock(), visitor.getLock().asWriteLock());
    }
}
