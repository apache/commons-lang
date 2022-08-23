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

import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.apache.commons.lang3.Validate;

/**
 * A specialized <em>semaphore</em> implementation that provides a number of
 * permits in a given time frame.
 *
 * <p>
 * This class is similar to the {@code java.util.concurrent.Semaphore} class
 * provided by the JDK in that it manages a configurable number of permits.
 * Using the {@link #acquire()} method a permit can be requested by a thread.
 * However, there is an additional timing dimension: there is no {@code
 * release()} method for freeing a permit, but all permits are automatically
 * released at the end of a configurable time frame. If a thread calls
 * {@link #acquire()} and the available permits are already exhausted for this
 * time frame, the thread is blocked. When the time frame ends all permits
 * requested so far are restored, and blocking threads are waked up again, so
 * that they can try to acquire a new permit. This basically means that in the
 * specified time frame only the given number of operations is possible.
 * </p>
 * <p>
 * A use case for this class is to artificially limit the load produced by a
 * process. As an example consider an application that issues database queries
 * on a production system in a background process to gather statistical
 * information. This background processing should not produce so much database
 * load that the functionality and the performance of the production system are
 * impacted. Here a {@link TimedSemaphore} could be installed to guarantee that
 * only a given number of database queries are issued per second.
 * </p>
 * <p>
 * A thread class for performing database queries could look as follows:
 * </p>
 *
 * <pre>
 * public class StatisticsThread extends Thread {
 *     // The semaphore for limiting database load.
 *     private final TimedSemaphore semaphore;
 *     // Create an instance and set the semaphore
 *     public StatisticsThread(TimedSemaphore timedSemaphore) {
 *         semaphore = timedSemaphore;
 *     }
 *     // Gather statistics
 *     public void run() {
 *         try {
 *             while (true) {
 *                 semaphore.acquire();   // limit database load
 *                 performQuery();        // issue a query
 *             }
 *         } catch(InterruptedException) {
 *             // fall through
 *         }
 *     }
 *     ...
 * }
 * </pre>
 *
 * <p>
 * The following code fragment shows how a {@link TimedSemaphore} is created
 * that allows only 10 operations per second and passed to the statistics
 * thread:
 * </p>
 *
 * <pre>
 * TimedSemaphore sem = new TimedSemaphore(1, TimeUnit.SECOND, 10);
 * StatisticsThread thread = new StatisticsThread(sem);
 * thread.start();
 * </pre>
 *
 * <p>
 * When creating an instance the time period for the semaphore must be
 * specified. {@link TimedSemaphore} uses an executor service with a
 * corresponding period to monitor this interval. The {@code
 * ScheduledExecutorService} to be used for this purpose can be provided at
 * construction time. Alternatively the class creates an internal executor
 * service.
 * </p>
 * <p>
 * Client code that uses {@link TimedSemaphore} has to call the
 * {@link #acquire()} method in each processing step. {@link TimedSemaphore}
 * keeps track of the number of invocations of the {@link #acquire()} method and
 * blocks the calling thread if the counter exceeds the limit specified. When
 * the timer signals the end of the time period the counter is reset and all
 * waiting threads are released. Then another cycle can start.
 * </p>
 * <p>
 * An alternative to {@code acquire()} is the {@link #tryAcquire()} method. This
 * method checks whether the semaphore is under the specified limit and
 * increases the internal counter if this is the case. The return value is then
 * <strong>true</strong>, and the calling thread can continue with its action.
 * If the semaphore is already at its limit, {@code tryAcquire()} immediately
 * returns <strong>false</strong> without blocking; the calling thread must
 * then abort its action. This usage scenario prevents blocking of threads.
 * </p>
 * <p>
 * It is possible to modify the limit at any time using the
 * {@link #setLimit(int)} method. This is useful if the load produced by an
 * operation has to be adapted dynamically. In the example scenario with the
 * thread collecting statistics it may make sense to specify a low limit during
 * day time while allowing a higher load in the night time. Reducing the limit
 * takes effect immediately by blocking incoming callers. If the limit is
 * increased, waiting threads are not released immediately, but wake up when the
 * timer runs out. Then, in the next period more processing steps can be
 * performed without blocking. By setting the limit to 0 the semaphore can be
 * switched off: in this mode the {@link #acquire()} method never blocks, but
 * lets all callers pass directly.
 * </p>
 * <p>
 * When the {@link TimedSemaphore} is no more needed its {@link #shutdown()}
 * method should be called. This causes the periodic task that monitors the time
 * interval to be canceled. If the {@link ScheduledExecutorService} has been
 * created by the semaphore at construction time, it is also shut down.
 * resources. After that {@link #acquire()} must not be called any more.
 * </p>
 *
 * @since 3.0
 */
public class TimedSemaphore {
    /**
     * Constant for a value representing no limit. If the limit is set to a
     * value less or equal this constant, the {@link TimedSemaphore} will be
     * effectively switched off.
     */
    public static final int NO_LIMIT = 0;

    /** Constant for the thread pool size for the executor. */
    private static final int THREAD_POOL_SIZE = 1;

    /** The executor service for managing the timer thread. */
    private final ScheduledExecutorService executorService;

    /** Stores the period for this timed semaphore. */
    private final long period;

    /** The time unit for the period. */
    private final TimeUnit unit;

    /** A flag whether the executor service was created by this object. */
    private final boolean ownExecutor;

    /** A future object representing the timer task. */
    private ScheduledFuture<?> task; // @GuardedBy("this")

    /** Stores the total number of invocations of the acquire() method. */
    private long totalAcquireCount; // @GuardedBy("this")

    /**
     * The counter for the periods. This counter is increased every time a
     * period ends.
     */
    private long periodCount; // @GuardedBy("this")

    /** The limit. */
    private int limit; // @GuardedBy("this")

    /** The current counter. */
    private int acquireCount;  // @GuardedBy("this")

    /** The number of invocations of acquire() in the last period. */
    private int lastCallsPerPeriod; // @GuardedBy("this")

    /** A flag whether shutdown() was called. */
    private boolean shutdown;  // @GuardedBy("this")

    /**
     * Creates a new instance of {@link TimedSemaphore} and initializes it with
     * the given time period and the limit.
     *
     * @param timePeriod the time period
     * @param timeUnit the unit for the period
     * @param limit the limit for the semaphore
     * @throws IllegalArgumentException if the period is less or equals 0
     */
    public TimedSemaphore(final long timePeriod, final TimeUnit timeUnit, final int limit) {
        this(null, timePeriod, timeUnit, limit);
    }

    /**
     * Creates a new instance of {@link TimedSemaphore} and initializes it with
     * an executor service, the given time period, and the limit. The executor
     * service will be used for creating a periodic task for monitoring the time
     * period. It can be <b>null</b>, then a default service will be created.
     *
     * @param service the executor service
     * @param timePeriod the time period
     * @param timeUnit the unit for the period
     * @param limit the limit for the semaphore
     * @throws IllegalArgumentException if the period is less or equals 0
     */
    public TimedSemaphore(final ScheduledExecutorService service, final long timePeriod,
            final TimeUnit timeUnit, final int limit) {
        Validate.inclusiveBetween(1, Long.MAX_VALUE, timePeriod, "Time period must be greater than 0!");

        period = timePeriod;
        unit = timeUnit;

        if (service != null) {
            executorService = service;
            ownExecutor = false;
        } else {
            final ScheduledThreadPoolExecutor s = new ScheduledThreadPoolExecutor(
                    THREAD_POOL_SIZE);
            s.setContinueExistingPeriodicTasksAfterShutdownPolicy(false);
            s.setExecuteExistingDelayedTasksAfterShutdownPolicy(false);
            executorService = s;
            ownExecutor = true;
        }

        setLimit(limit);
    }

    /**
     * Returns the limit enforced by this semaphore. The limit determines how
     * many invocations of {@link #acquire()} are allowed within the monitored
     * period.
     *
     * @return the limit
     */
    public final synchronized int getLimit() {
        return limit;
    }

    /**
     * Sets the limit. This is the number of times the {@link #acquire()} method
     * can be called within the time period specified. If this limit is reached,
     * further invocations of {@link #acquire()} will block. Setting the limit
     * to a value &lt;= {@link #NO_LIMIT} will cause the limit to be disabled,
     * i.e. an arbitrary number of{@link #acquire()} invocations is allowed in
     * the time period.
     *
     * @param limit the limit
     */
    public final synchronized void setLimit(final int limit) {
        this.limit = limit;
    }

    /**
     * Initializes a shutdown. After that the object cannot be used anymore.
     * This method can be invoked an arbitrary number of times. All invocations
     * after the first one do not have any effect.
     */
    public synchronized void shutdown() {
        if (!shutdown) {

            if (ownExecutor) {
                // if the executor was created by this instance, it has
                // to be shutdown
                getExecutorService().shutdownNow();
            }
            if (task != null) {
                task.cancel(false);
            }

            shutdown = true;
        }
    }

    /**
     * Tests whether the {@link #shutdown()} method has been called on this
     * object. If this method returns <b>true</b>, this instance cannot be used
     * any longer.
     *
     * @return a flag whether a shutdown has been performed
     */
    public synchronized boolean isShutdown() {
        return shutdown;
    }

    /**
     * Acquires a permit from this semaphore. This method will block if
     * the limit for the current period has already been reached. If
     * {@link #shutdown()} has already been invoked, calling this method will
     * cause an exception. The very first call of this method starts the timer
     * task which monitors the time period set for this {@link TimedSemaphore}.
     * From now on the semaphore is active.
     *
     * @throws InterruptedException if the thread gets interrupted
     * @throws IllegalStateException if this semaphore is already shut down
     */
    public synchronized void acquire() throws InterruptedException {
        prepareAcquire();

        boolean canPass;
        do {
            canPass = acquirePermit();
            if (!canPass) {
                wait();
            }
        } while (!canPass);
    }

    /**
     * Tries to acquire a permit from this semaphore. If the limit of this semaphore has
     * not yet been reached, a permit is acquired, and this method returns
     * <strong>true</strong>. Otherwise, this method returns immediately with the result
     * <strong>false</strong>.
     *
     * @return <strong>true</strong> if a permit could be acquired; <strong>false</strong>
     * otherwise
     * @throws IllegalStateException if this semaphore is already shut down
     * @since 3.5
     */
    public synchronized boolean tryAcquire() {
        prepareAcquire();
        return acquirePermit();
    }

    /**
     * Returns the number of (successful) acquire invocations during the last
     * period. This is the number of times the {@link #acquire()} method was
     * called without blocking. This can be useful for testing or debugging
     * purposes or to determine a meaningful threshold value. If a limit is set,
     * the value returned by this method won't be greater than this limit.
     *
     * @return the number of non-blocking invocations of the {@link #acquire()}
     * method
     */
    public synchronized int getLastAcquiresPerPeriod() {
        return lastCallsPerPeriod;
    }

    /**
     * Returns the number of invocations of the {@link #acquire()} method for
     * the current period. This may be useful for testing or debugging purposes.
     *
     * @return the current number of {@link #acquire()} invocations
     */
    public synchronized int getAcquireCount() {
        return acquireCount;
    }

    /**
     * Returns the number of calls to the {@link #acquire()} method that can
     * still be performed in the current period without blocking. This method
     * can give an indication whether it is safe to call the {@link #acquire()}
     * method without risking to be suspended. However, there is no guarantee
     * that a subsequent call to {@link #acquire()} actually is not-blocking
     * because in the meantime other threads may have invoked the semaphore.
     *
     * @return the current number of available {@link #acquire()} calls in the
     * current period
     */
    public synchronized int getAvailablePermits() {
        return getLimit() - getAcquireCount();
    }

    /**
     * Returns the average number of successful (i.e. non-blocking)
     * {@link #acquire()} invocations for the entire life-time of this {@code
     * TimedSemaphore}. This method can be used for instance for statistical
     * calculations.
     *
     * @return the average number of {@link #acquire()} invocations per time
     * unit
     */
    public synchronized double getAverageCallsPerPeriod() {
        return periodCount == 0 ? 0 : (double) totalAcquireCount
                / (double) periodCount;
    }

    /**
     * Returns the time period. This is the time monitored by this semaphore.
     * Only a given number of invocations of the {@link #acquire()} method is
     * possible in this period.
     *
     * @return the time period
     */
    public long getPeriod() {
        return period;
    }

    /**
     * Returns the time unit. This is the unit used by {@link #getPeriod()}.
     *
     * @return the time unit
     */
    public TimeUnit getUnit() {
        return unit;
    }

    /**
     * Returns the executor service used by this instance.
     *
     * @return the executor service
     */
    protected ScheduledExecutorService getExecutorService() {
        return executorService;
    }

    /**
     * Starts the timer. This method is called when {@link #acquire()} is called
     * for the first time. It schedules a task to be executed at fixed rate to
     * monitor the time period specified.
     *
     * @return a future object representing the task scheduled
     */
    protected ScheduledFuture<?> startTimer() {
        return getExecutorService().scheduleAtFixedRate(this::endOfPeriod, getPeriod(), getPeriod(), getUnit());
    }

    /**
     * The current time period is finished. This method is called by the timer
     * used internally to monitor the time period. It resets the counter and
     * releases the threads waiting for this barrier.
     */
    synchronized void endOfPeriod() {
        lastCallsPerPeriod = acquireCount;
        totalAcquireCount += acquireCount;
        periodCount++;
        acquireCount = 0;
        notifyAll();
    }

    /**
     * Prepares an acquire operation. Checks for the current state and starts the internal
     * timer if necessary. This method must be called with the lock of this object held.
     */
    private void prepareAcquire() {
        if (isShutdown()) {
            throw new IllegalStateException("TimedSemaphore is shut down!");
        }

        if (task == null) {
            task = startTimer();
        }
    }

    /**
     * Internal helper method for acquiring a permit. This method checks whether currently
     * a permit can be acquired and - if so - increases the internal counter. The return
     * value indicates whether a permit could be acquired. This method must be called with
     * the lock of this object held.
     *
     * @return a flag whether a permit could be acquired
     */
    private boolean acquirePermit() {
        if (getLimit() <= NO_LIMIT || acquireCount < getLimit()) {
            acquireCount++;
            return true;
        }
        return false;
    }
}
