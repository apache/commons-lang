package org.apache.commons.lang3.concurrent.locks;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;

/**
 * Utility class providing convenient methods for working with {@link Lock} objects.
 * <p>
 * This class offers simplified lock management with automatic resource cleanup using 
 * try-with-resources syntax. It wraps lock operations and provides a {@link LockStat} 
 * object that implements {@link AutoCloseable} for automatic unlocking.
 * </p>
 * 
 * <p>Example usage:</p>
 * <pre>
 * Lock myLock = new ReentrantLock();
 * 
 * // Try to acquire lock with timeout
 * try (LockStat lockStat = LockUtils.tryLock(myLock, 5, TimeUnit.SECONDS)) {
 *     if (lockStat.isLocked()) {
 *         // Critical section - lock acquired successfully
 *         doSomethingCritical();
 *     } else {
 *         // Handle case where lock was not acquired
 *         handleLockTimeout();
 *     }
 * } // Lock is automatically released here
 * 
 * // Or acquire lock without timeout
 * try (LockStat lockStat = LockUtils.lock(myLock)) {
 *     // Critical section - lock is guaranteed to be acquired
 *     doSomethingCritical();
 * } // Lock is automatically released here
 * </pre>
 *
 */
public class LockUtils {

    /**
     * Attempts to acquire the specified lock within the given timeout period.
     * <p>
     * This method tries to acquire the lock and waits for the specified timeout
     * if the lock is not immediately available. If the thread is interrupted
     * while waiting, the method returns a {@link LockStat} indicating that the
     * lock was not acquired.
     * </p>
     * 
     * @param lock the lock to acquire, must not be null
     * @param timeout the maximum time to wait for the lock
     * @param timeUnit the time unit of the timeout argument, must not be null
     * @return a {@link LockStat} object that can be used with try-with-resources
     *         to automatically release the lock. Use {@code isLocked()} to check
     *         if the lock was successfully acquired.
     * @throws NullPointerException if lock or timeUnit is null
     */
    public static LockStat tryLock(Lock lock, long timeout, TimeUnit timeUnit) {
        boolean tryLock = false;
        try {
            tryLock = lock.tryLock(timeout, timeUnit);
        } catch (InterruptedException e) {
            tryLock = false;
        }
        return new LockStat(lock, tryLock);
    }

    /**
     * Attempts to acquire the specified lock immediately without waiting.
     * <p>
     * This method tries to acquire the lock immediately. If the lock is not
     * available, it returns immediately without waiting.
     * </p>
     * 
     * @param lock the lock to acquire, must not be null
     * @return a {@link LockStat} object that can be used with try-with-resources
     *         to automatically release the lock. Use {@code isLocked()} to check
     *         if the lock was successfully acquired.
     * @throws NullPointerException if lock is null
     */
    public static LockStat tryLock(Lock lock) {
        boolean tryLock = lock.tryLock();
        return new LockStat(lock, tryLock);
    }

    /**
     * Acquires the specified lock, blocking if necessary until the lock is available.
     * <p>
     * This method blocks the current thread until the lock is acquired. Unlike
     * the {@code tryLock} methods, this method guarantees that the lock will be
     * acquired (unless the thread is interrupted).
     * </p>
     * 
     * @param lock the lock to acquire, must not be null
     * @return a {@link LockStat} object that can be used with try-with-resources
     *         to automatically release the lock. The returned {@code LockStat}
     *         will always have {@code isLocked()} return {@code true}.
     * @throws NullPointerException if lock is null
     */
    public static LockStat lock(Lock lock) {
        lock.lock();
        return new LockStat(lock, true);
    }

    /**
     * A container class that holds a {@link Lock} and its acquisition status.
     * <p>
     * This class implements {@link AutoCloseable} to enable automatic resource
     * management with try-with-resources statements. When used in a try-with-resources
     * block, the lock will be automatically released when the block is exited,
     * but only if the lock was successfully acquired.
     * </p>
     * 
     * <p>This class is immutable and thread-safe.</p>
     */
    public static class LockStat implements AutoCloseable {
        private final Lock lock;
        private final boolean isLocked;

        /**
         * Constructs a new LockStat with the specified lock and acquisition status.
         * 
         * @param lock the lock object, must not be null
         * @param isLocked {@code true} if the lock was successfully acquired,
         *                 {@code false} otherwise
         * @throws NullPointerException if lock is null
         */
        public LockStat(Lock lock, boolean isLocked) {
            this.lock = lock;
            this.isLocked = isLocked;
        }

        /**
         * Returns the lock associated with this LockStat.
         * 
         * @return the lock object, never null
         */
        public Lock getLock() {
            return lock;
        }

        /**
         * Returns whether the lock was successfully acquired.
         * 
         * @return {@code true} if the lock was acquired, {@code false} otherwise
         */
        public boolean isLocked() {
            return isLocked;
        }

        /**
         * Releases the lock if it was successfully acquired.
         * <p>
         * This method is called automatically when used in a try-with-resources
         * statement. It only releases the lock if {@code isLocked()} returns
         * {@code true}, preventing attempts to unlock a lock that was never acquired.
         * </p>
         */
        @Override
        public void close() {
            if (isLocked) {
                lock.unlock();
            }
        }
    }
}
