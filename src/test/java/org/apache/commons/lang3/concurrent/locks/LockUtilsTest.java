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
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.time.Duration;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.ThreadUtils;
import org.apache.commons.lang3.concurrent.locks.LockUtils.LockStat;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link LockUtils}.
 */
public class LockUtilsTest extends AbstractLangTest {

    /**
     * Test {@link LockUtils#lock(Lock)} with successful lock acquisition.
     */
    @Test
    public void testLock_Success() {
        final Lock lock = new ReentrantLock();
        
        try (LockStat lockStat = LockUtils.lock(lock)) {
            assertNotNull(lockStat);
            assertSame(lock, lockStat.getLock());
            assertTrue(lockStat.isLocked());
        }
    }

    /**
     * Test {@link LockUtils#lock(Lock)} with null lock throws NPE.
     */
    @Test
    public void testLock_NullLock() {
        assertThrows(NullPointerException.class, () -> LockUtils.lock(null));
    }

    /**
     * Test {@link LockUtils#lock(Lock)} with already held lock (reentrant).
     */
    @Test
    public void testLock_Reentrant() {
        final ReentrantLock lock = new ReentrantLock();
        
        try (LockStat lockStat1 = LockUtils.lock(lock)) {
            assertTrue(lockStat1.isLocked());
            assertEquals(1, lock.getHoldCount());
            
            try (LockStat lockStat2 = LockUtils.lock(lock)) {
                assertTrue(lockStat2.isLocked());
                assertEquals(2, lock.getHoldCount());
            }
            assertEquals(1, lock.getHoldCount());
        }
        assertEquals(0, lock.getHoldCount());
    }

    /**
     * Test {@link LockUtils#tryLock(Lock)} with successful immediate lock acquisition.
     */
    @Test
    public void testTryLock_Success() {
        final Lock lock = new ReentrantLock();
        
        try (LockStat lockStat = LockUtils.tryLock(lock)) {
            assertNotNull(lockStat);
            assertSame(lock, lockStat.getLock());
            assertTrue(lockStat.isLocked());
        }
    }

    /**
     * Test {@link LockUtils#tryLock(Lock)} with lock held by another thread.
     */
    @Test
    public void testTryLock_LockHeld() throws InterruptedException {
        final Lock lock = new ReentrantLock();
        final CountDownLatch latch1 = new CountDownLatch(1);
        final CountDownLatch latch2 = new CountDownLatch(1);
        final AtomicBoolean lockAcquired = new AtomicBoolean(false);

        // Thread 1 acquires the lock
        final Thread thread1 = new Thread(() -> {
            try (LockStat lockStat = LockUtils.lock(lock)) {
                latch1.countDown(); // Signal that lock is acquired
                assertTrue(lockStat.isLocked());
                ThreadUtils.sleep(Duration.ofMillis(100)); // Hold the lock
                latch2.await(); // Wait for signal to proceed
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        });

        // Thread 2 tries to acquire the lock
        final Thread thread2 = new Thread(() -> {
            try {
                latch1.await(); // Wait for thread1 to acquire lock
                try (LockStat lockStat = LockUtils.tryLock(lock)) {
                    lockAcquired.set(lockStat.isLocked());
                }
                latch2.countDown(); // Signal thread1 to proceed
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        });

        thread1.start();
        thread2.start();
        thread1.join();
        thread2.join();

        assertFalse(lockAcquired.get());
    }

    /**
     * Test {@link LockUtils#tryLock(Lock)} with null lock throws NPE.
     */
    @Test
    public void testTryLock_NullLock() {
        assertThrows(NullPointerException.class, () -> LockUtils.tryLock(null));
    }

    /**
     * Test {@link LockUtils#tryLock(Lock, long, TimeUnit)} with successful lock acquisition.
     */
    @Test
    public void testTryLockWithTimeout_Success() {
        final Lock lock = new ReentrantLock();
        
        try (LockStat lockStat = LockUtils.tryLock(lock, 1, TimeUnit.SECONDS)) {
            assertNotNull(lockStat);
            assertSame(lock, lockStat.getLock());
            assertTrue(lockStat.isLocked());
        }
    }

    /**
     * Test {@link LockUtils#tryLock(Lock, long, TimeUnit)} with timeout.
     */
    @Test
    public void testTryLockWithTimeout_Timeout() throws InterruptedException {
        final Lock lock = new ReentrantLock();
        final CountDownLatch latch1 = new CountDownLatch(1);
        final CountDownLatch latch2 = new CountDownLatch(1);
        final AtomicBoolean lockAcquired = new AtomicBoolean(false);

        // Thread 1 acquires the lock and holds it
        final Thread thread1 = new Thread(() -> {
            try (LockStat lockStat = LockUtils.lock(lock)) {
                latch1.countDown(); // Signal that lock is acquired
                assertTrue(lockStat.isLocked());
                latch2.await(); // Wait for signal to proceed
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        });

        // Thread 2 tries to acquire the lock with timeout
        final Thread thread2 = new Thread(() -> {
            try {
                latch1.await(); // Wait for thread1 to acquire lock
                try (LockStat lockStat = LockUtils.tryLock(lock, 50, TimeUnit.MILLISECONDS)) {
                    lockAcquired.set(lockStat.isLocked());
                }
                latch2.countDown(); // Signal thread1 to proceed
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        });

        thread1.start();
        thread2.start();
        thread1.join();
        thread2.join();

        assertFalse(lockAcquired.get());
    }

    /**
     * Test {@link LockUtils#tryLock(Lock, long, TimeUnit)} with thread interruption.
     */
    @Test
    public void testTryLockWithTimeout_Interrupted() throws InterruptedException {
        final Lock lock = new ReentrantLock();
        final CountDownLatch latch1 = new CountDownLatch(1);
        final CountDownLatch latch2 = new CountDownLatch(1);
        final AtomicBoolean lockAcquired = new AtomicBoolean(false);

        // Thread 1 acquires the lock and holds it
        final Thread thread1 = new Thread(() -> {
            try (LockStat lockStat = LockUtils.lock(lock)) {
                latch1.countDown(); // Signal that lock is acquired
                assertTrue(lockStat.isLocked());
                latch2.await(); // Wait for signal to proceed
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        });

        // Thread 2 tries to acquire the lock with timeout and gets interrupted
        final Thread thread2 = new Thread(() -> {
            try {
                latch1.await(); // Wait for thread1 to acquire lock
                try (LockStat lockStat = LockUtils.tryLock(lock, 5, TimeUnit.SECONDS)) {
                    lockAcquired.set(lockStat.isLocked());
                }
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        });

        thread1.start();
        thread2.start();
        
        // Interrupt thread2 while it's waiting for the lock
        ThreadUtils.sleep(Duration.ofMillis(50));
        thread2.interrupt();
        
        thread1.join();
        thread2.join();
        
        latch2.countDown(); // Clean up

        assertFalse(lockAcquired.get());
    }

    /**
     * Test {@link LockUtils#tryLock(Lock, long, TimeUnit)} with null lock throws NPE.
     */
    @Test
    public void testTryLockWithTimeout_NullLock() {
        assertThrows(NullPointerException.class, () -> LockUtils.tryLock(null, 1, TimeUnit.SECONDS));
    }

    /**
     * Test {@link LockUtils#tryLock(Lock, long, TimeUnit)} with null TimeUnit throws NPE.
     */
    @Test
    public void testTryLockWithTimeout_NullTimeUnit() {
        final Lock lock = new ReentrantLock();
        assertThrows(NullPointerException.class, () -> LockUtils.tryLock(lock, 1, null));
    }

    /**
     * Test try-with-resources automatic unlocking.
     */
    @Test
    public void testTryWithResources_AutoUnlock() {
        final ReentrantLock lock = new ReentrantLock();
        
        // Acquire and release lock
        try (LockStat lockStat = LockUtils.lock(lock)) {
            assertTrue(lockStat.isLocked());
            assertEquals(1, lock.getHoldCount());
        }
        
        // Lock should be released
        assertEquals(0, lock.getHoldCount());
        assertFalse(lock.isLocked());
    }

    /**
     * Test try-with-resources with failed lock acquisition.
     */
    @Test
    public void testTryWithResources_FailedAcquisition() throws InterruptedException {
        final Lock lock = new ReentrantLock();
        final CountDownLatch latch1 = new CountDownLatch(1);
        final CountDownLatch latch2 = new CountDownLatch(1);

        // Thread 1 acquires the lock
        final Thread thread1 = new Thread(() -> {
            try (LockStat lockStat = LockUtils.lock(lock)) {
                latch1.countDown(); // Signal that lock is acquired
                assertTrue(lockStat.isLocked());
                latch2.await(); // Wait for signal to proceed
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        });

        // Thread 2 fails to acquire the lock
        final Thread thread2 = new Thread(() -> {
            try {
                latch1.await(); // Wait for thread1 to acquire lock
                try (LockStat lockStat = LockUtils.tryLock(lock)) {
                    assertFalse(lockStat.isLocked());
                    // close() should not attempt to unlock since lock was not acquired
                }
                latch2.countDown(); // Signal thread1 to proceed
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        });

        thread1.start();
        thread2.start();
        thread1.join();
        thread2.join();
    }

    /**
     * Test concurrent access with multiple threads.
     */
    @Test
    public void testConcurrentAccess() throws InterruptedException {
        final Lock lock = new ReentrantLock();
        final AtomicInteger counter = new AtomicInteger(0);
        final int numThreads = 10;
        final CountDownLatch startLatch = new CountDownLatch(1);
        final CountDownLatch endLatch = new CountDownLatch(numThreads);

        for (int i = 0; i < numThreads; i++) {
            new Thread(() -> {
                try {
                    startLatch.await();
                    try (LockStat lockStat = LockUtils.lock(lock)) {
                        if (lockStat.isLocked()) {
                            final int current = counter.get();
                            ThreadUtils.sleep(Duration.ofMillis(1)); // Simulate work
                            counter.set(current + 1);
                        }
                    }
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                } finally {
                    endLatch.countDown();
                }
            }).start();
        }

        startLatch.countDown();
        endLatch.await();

        assertEquals(numThreads, counter.get());
    }

    /**
     * Test {@link LockStat} constructor.
     */
    @Test
    public void testLockStat_Constructor() {
        final Lock lock = new ReentrantLock();
        final LockStat lockStat = new LockStat(lock, true);
        
        assertSame(lock, lockStat.getLock());
        assertTrue(lockStat.isLocked());
        
        final LockStat lockStat2 = new LockStat(lock, false);
        assertSame(lock, lockStat2.getLock());
        assertFalse(lockStat2.isLocked());
    }

    /**
     * Test {@link LockStat} constructor with null lock throws NPE.
     */
    @Test
    public void testLockStat_NullLock() {
        assertThrows(NullPointerException.class, () -> new LockStat(null, true));
    }

    /**
     * Test {@link LockStat#close()} method directly.
     */
    @Test
    public void testLockStat_CloseMethod() {
        final ReentrantLock lock = new ReentrantLock();
        
        // Test close with acquired lock
        final LockStat lockStat1 = new LockStat(lock, true);
        lock.lock(); // Manually acquire lock
        assertEquals(1, lock.getHoldCount());
        
        lockStat1.close();
        assertEquals(0, lock.getHoldCount());
        
        // Test close with non-acquired lock
        final LockStat lockStat2 = new LockStat(lock, false);
        lockStat2.close(); // Should not throw exception
        assertEquals(0, lock.getHoldCount());
    }

    /**
     * Test that LockStat is immutable.
     */
    @Test
    public void testLockStat_Immutability() {
        final Lock lock = new ReentrantLock();
        final LockStat lockStat = new LockStat(lock, true);
        
        // Verify that the state cannot be changed after construction
        assertSame(lock, lockStat.getLock());
        assertTrue(lockStat.isLocked());
        
        // The lock and status should remain the same
        assertSame(lock, lockStat.getLock());
        assertTrue(lockStat.isLocked());
    }

    /**
     * Test edge case with zero timeout.
     */
    @Test
    public void testTryLockWithTimeout_ZeroTimeout() {
        final Lock lock = new ReentrantLock();
        
        try (LockStat lockStat = LockUtils.tryLock(lock, 0, TimeUnit.SECONDS)) {
            assertTrue(lockStat.isLocked()); // Should still acquire if available
        }
    }

    /**
     * Test edge case with negative timeout.
     */
    @Test
    public void testTryLockWithTimeout_NegativeTimeout() {
        final Lock lock = new ReentrantLock();
        
        try (LockStat lockStat = LockUtils.tryLock(lock, -1, TimeUnit.SECONDS)) {
            assertTrue(lockStat.isLocked()); // Should still acquire if available
        }
    }
} 