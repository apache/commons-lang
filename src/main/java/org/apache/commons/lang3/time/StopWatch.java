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

package org.apache.commons.lang3.time;

import java.time.Duration;
import java.time.Instant;
import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.BiPredicate;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.function.FailableBiConsumer;
import org.apache.commons.lang3.function.FailableBiFunction;
import org.apache.commons.lang3.function.FailableBiPredicate;
import org.apache.commons.lang3.function.FailableConsumer;
import org.apache.commons.lang3.function.FailableFunction;
import org.apache.commons.lang3.function.FailablePredicate;
import org.apache.commons.lang3.function.FailableRunnable;
import org.apache.commons.lang3.function.FailableSupplier;
import org.apache.commons.lang3.function.TriConsumer;
import org.apache.commons.lang3.function.TriFunction;

/**
 * {@link StopWatch} provides a convenient API for timings.
 *
 * <p>
 * To start the watch, call {@link #start()} or {@link StopWatch#createStarted()}. At this point you can:
 * </p>
 * <ul>
 * <li>{@link #split()} the watch to get the time whilst the watch continues in the background. {@link #unsplit()} will remove the effect of the split. At this
 * point, these three options are available again.</li>
 * <li>{@link #suspend()} the watch to pause it. {@link #resume()} allows the watch to continue. Any time between the suspend and resume will not be counted in
 * the total. At this point, these three options are available again.</li>
 * <li>{@link #stop()} the watch to complete the timing session.</li>
 * </ul>
 *
 * <p>
 * It is intended that the output methods {@link #toString()} and {@link #getTime()} should only be called after stop, split or suspend, however a suitable
 * result will be returned at other points.
 * </p>
 *
 * <p>
 * NOTE: As from v2.1, the methods protect against inappropriate calls. Thus you cannot now call stop before start, resume before suspend or unsplit before
 * split.
 * </p>
 *
 * <ol>
 * <li>{@link #split()}, {@link #suspend()}, or {@link #stop()} cannot be invoked twice</li>
 * <li>{@link #unsplit()} may only be called if the watch has been {@link #split()}</li>
 * <li>{@link #resume()} may only be called if the watch has been {@link #suspend()}</li>
 * <li>{@link #start()} cannot be called twice without calling {@link #reset()}</li>
 * </ol>
 *
 * <p>
 * This class is not thread-safe
 * </p>
 *
 * @see DurationUtils#of(FailableRunnable)
 * @see DurationUtils#of(FailableConsumer)
 *
 * @since 2.0
 */
public class StopWatch {

    /**
     * Enumeration type which indicates the split status of a StopWatch.
     */
    private enum SplitState {
        SPLIT, UNSPLIT
    }

    /**
     * Enumeration type which indicates the status of a StopWatch.
     */
    private enum State {

        RUNNING {
            @Override
            boolean isStarted() {
                return true;
            }

            @Override
            boolean isStopped() {
                return false;
            }

            @Override
            boolean isSuspended() {
                return false;
            }
        },

        STOPPED {
            @Override
            boolean isStarted() {
                return false;
            }

            @Override
            boolean isStopped() {
                return true;
            }

            @Override
            boolean isSuspended() {
                return false;
            }
        },

        SUSPENDED {
            @Override
            boolean isStarted() {
                return true;
            }

            @Override
            boolean isStopped() {
                return false;
            }

            @Override
            boolean isSuspended() {
                return true;
            }
        },

        UNSTARTED {
            @Override
            boolean isStarted() {
                return false;
            }

            @Override
            boolean isStopped() {
                return true;
            }

            @Override
            boolean isSuspended() {
                return false;
            }
        };

        /**
         * Tests whether the StopWatch is started. A suspended StopWatch is also started.
         *
         * @return boolean If the StopWatch is started.
         */
        abstract boolean isStarted();

        /**
         * Tests whether the StopWatch is stopped. A StopWatch which is not yet started and explicitly stopped is considered stopped.
         *
         * @return boolean If the StopWatch is stopped.
         */
        abstract boolean isStopped();

        /**
         * Tests whether the StopWatch is suspended.
         *
         * @return boolean If the StopWatch is suspended.
         */
        abstract boolean isSuspended();
    }

    private static final long NANO_2_MILLIS = 1_000_000L;

    /**
     * Creates a StopWatch.
     *
     * @return StopWatch a StopWatch.
     *
     * @since 3.10
     */
    public static StopWatch create() {
        return new StopWatch();
    }

    /**
     * Creates and starts a StopWatch.
     *
     * @return StopWatch a started StopWatch.
     *
     * @since 3.5
     */
    public static StopWatch createStarted() {
        final StopWatch sw = new StopWatch();
        sw.start();
        return sw;
    }

    /**
     * A message for string presentation.
     *
     * @since 3.10
     */
    private final String message;

    /**
     * The current running state of the StopWatch.
     */
    private State runningState = State.UNSTARTED;

    /**
     * Whether the StopWatch has a split time recorded.
     */
    private SplitState splitState = SplitState.UNSPLIT;

    /**
     * The start time in nanoseconds.
     *
     * This field can be removed once we move off of Java 8.
     */
    private long startTimeNanos;

    /**
     * The start Instant.
     * <p>
     * nanoTime is only for elapsed time so we need to also store the currentTimeMillis to maintain the old getStartTime API.
     * </p>
     * <p>
     * On Java 8, Instant has millisecond precision, only later versions use nanoseconds.
     * </p>
     */
    private Instant startInstant;

    /**
     * The end Instant.
     * <p>
     * nanoTime is only for elapsed time so we need to also store the currentTimeMillis to maintain the old getStartTime API.
     * </p>
     * <p>
     * On Java 8, Instant has millisecond precision, only later versions use nanoseconds.
     * </p>
     */
    private Instant stopInstant;

    /**
     * The stop time in nanoseconds.
     *
     * This field can be removed once we move off of Java 8.
     */
    private long stopTimeNanos;

    /**
     * Constructs a new instance.
     */
    public StopWatch() {
        this(null);
    }

    /**
     * Constructs a new instance.
     *
     * @param message A message for string presentation.
     * @since 3.10
     */
    public StopWatch(final String message) {
        this.message = message;
    }

    /**
     * Formats the split time with {@link DurationFormatUtils#formatDurationHMS}.
     *
     * @return the split time formatted by {@link DurationFormatUtils#formatDurationHMS}.
     * @since 3.10
     */
    public String formatSplitTime() {
        return DurationFormatUtils.formatDurationHMS(getSplitDuration().toMillis());
    }

    /**
     * Formats the time formatted with {@link DurationFormatUtils#formatDurationHMS}.
     *
     * @return the time formatted by {@link DurationFormatUtils#formatDurationHMS}.
     * @since 3.10
     */
    public String formatTime() {
        return DurationFormatUtils.formatDurationHMS(getTime());
    }

    /**
     * Gets the Duration on the StopWatch.
     *
     * <p>
     * This is either the Duration between the start and the moment this method is called, or the Duration between start and stop.
     * </p>
     *
     * @return the Duration.
     * @since 3.16.0
     */
    public Duration getDuration() {
        return Duration.ofNanos(getNanoTime());
    }

    /**
     * Gets the message for string presentation.
     *
     * @return the message for string presentation.
     * @since 3.10
     */
    public String getMessage() {
        return message;
    }

    /**
     * Gets the <em>elapsed</em> time in nanoseconds.
     *
     * <p>
     * This is either the time between the start and the moment this method is called, or the amount of time between start and stop.
     * </p>
     *
     * @return the <em>elapsed</em> time in nanoseconds.
     * @see System#nanoTime()
     * @since 3.0
     */
    public long getNanoTime() {
        if (runningState == State.STOPPED || runningState == State.SUSPENDED) {
            return stopTimeNanos - startTimeNanos;
        }
        if (runningState == State.UNSTARTED) {
            return 0;
        }
        if (runningState == State.RUNNING) {
            return System.nanoTime() - startTimeNanos;
        }
        throw new IllegalStateException("Illegal running state has occurred.");
    }

    /**
     * Gets the split Duration on the StopWatch.
     *
     * <p>
     * This is the Duration between start and latest split.
     * </p>
     *
     * @return the split Duration
     *
     * @throws IllegalStateException if the StopWatch has not yet been split.
     * @since 3.16.0
     */
    public Duration getSplitDuration() {
        return Duration.ofNanos(getSplitNanoTime());
    }

    /**
     * Gets the split time in nanoseconds.
     *
     * <p>
     * This is the time between start and latest split.
     * </p>
     *
     * @return the split time in nanoseconds
     *
     * @throws IllegalStateException if the StopWatch has not yet been split.
     * @since 3.0
     */
    public long getSplitNanoTime() {
        if (splitState != SplitState.SPLIT) {
            throw new IllegalStateException("Stopwatch must be split to get the split time.");
        }
        return stopTimeNanos - startTimeNanos;
    }

    /**
     * Gets the split time on the StopWatch.
     *
     * <p>
     * This is the time between start and latest split.
     * </p>
     *
     * @return the split time in milliseconds
     *
     * @throws IllegalStateException if the StopWatch has not yet been split.
     * @since 2.1
     * @deprecated Use {@link #getSplitDuration()}.
     */
    @Deprecated
    public long getSplitTime() {
        return nanosToMillis(getSplitNanoTime());
    }

    /**
     * Gets the Instant this StopWatch was started, between the current time and midnight, January 1, 1970 UTC.
     *
     * @return the Instant this StopWatch was started, between the current time and midnight, January 1, 1970 UTC.
     * @throws IllegalStateException if this StopWatch has not been started
     * @since 3.16.0
     */
    public Instant getStartInstant() {
        return Instant.ofEpochMilli(getStartTime());
    }

    /**
     * Gets the time this StopWatch was started in milliseconds, between the current time and midnight, January 1, 1970 UTC.
     *
     * @return the time this StopWatch was started in milliseconds, between the current time and midnight, January 1, 1970 UTC.
     * @throws IllegalStateException if this StopWatch has not been started
     * @since 2.4
     * @deprecated Use {@link #getStartInstant()}.
     */
    @Deprecated
    public long getStartTime() {
        if (runningState == State.UNSTARTED) {
            throw new IllegalStateException("Stopwatch has not been started");
        }
        // stopTimeNanos stores System.nanoTime() for elapsed time
        return startInstant.toEpochMilli();
    }

    /**
     * Gets the Instant this StopWatch was stopped, between the current time and midnight, January 1, 1970 UTC.
     *
     * @return the Instant this StopWatch was stopped in milliseconds, between the current time and midnight, January 1, 1970 UTC.
     * @throws IllegalStateException if this StopWatch has not been started
     * @since 3.16.0
     */
    public Instant getStopInstant() {
        return Instant.ofEpochMilli(getStopTime());
    }

    /**
     * Gets the time this StopWatch was stopped in milliseconds, between the current time and midnight, January 1, 1970 UTC.
     *
     * @return the time this StopWatch was stopped in milliseconds, between the current time and midnight, January 1, 1970 UTC.
     * @throws IllegalStateException if this StopWatch has not been started
     * @since 3.12.0
     * @deprecated Use {@link #getStopInstant()}.
     */
    @Deprecated
    public long getStopTime() {
        if (runningState == State.UNSTARTED) {
            throw new IllegalStateException("Stopwatch has not been started");
        }
        // stopTimeNanos stores System.nanoTime() for elapsed time
        return stopInstant.toEpochMilli();
    }

    /**
     * Gets the time on the StopWatch.
     *
     * <p>
     * This is either the time between the start and the moment this method is called, or the amount of time between start and stop.
     * </p>
     *
     * @return the time in milliseconds
     * @deprecated Use {@link #getDuration()}.
     */
    @Deprecated
    public long getTime() {
        return nanosToMillis(getNanoTime());
    }

    /**
     * Gets the time in the specified TimeUnit.
     *
     * <p>
     * This is either the time between the start and the moment this method is called, or the amount of time between start and stop. The resulting time will be
     * expressed in the desired TimeUnit with any remainder rounded down. For example, if the specified unit is {@code TimeUnit.HOURS} and the StopWatch time is
     * 59 minutes, then the result returned will be {@code 0}.
     * </p>
     *
     * @param timeUnit the unit of time, not null
     * @return the time in the specified TimeUnit, rounded down
     * @since 3.5
     */
    public long getTime(final TimeUnit timeUnit) {
        return timeUnit.convert(getNanoTime(), TimeUnit.NANOSECONDS);
    }

    /**
     * Tests whether the StopWatch is started. A suspended StopWatch is also started watch.
     *
     * @return boolean If the StopWatch is started.
     * @since 3.2
     */
    public boolean isStarted() {
        return runningState.isStarted();
    }

    /**
     * Tests whether StopWatch is stopped. The StopWatch which's not yet started and explicitly stopped StopWatch is considered as stopped.
     *
     * @return boolean If the StopWatch is stopped.
     * @since 3.2
     */
    public boolean isStopped() {
        return runningState.isStopped();
    }

    /**
     * Tests whether the StopWatch is suspended.
     *
     * @return boolean If the StopWatch is suspended.
     * @since 3.2
     */
    public boolean isSuspended() {
        return runningState.isSuspended();
    }

    /**
     * Converts nanoseconds to milliseconds.
     *
     * @param nanos nanoseconds to convert.
     * @return milliseconds conversion result.
     */
    private long nanosToMillis(final long nanos) {
        return nanos / NANO_2_MILLIS;
    }

    /**
     * Resets the StopWatch. Stops it if need be.
     *
     * <p>
     * This method clears the internal values to allow the object to be reused.
     * </p>
     */
    public void reset() {
        runningState = State.UNSTARTED;
        splitState = SplitState.UNSPLIT;
    }

    /**
     * Resumes the StopWatch after a suspend.
     *
     * <p>
     * This method resumes the watch after it was suspended. The watch will not include time between the suspend and resume calls in the total time.
     * </p>
     *
     * @throws IllegalStateException if the StopWatch has not been suspended.
     */
    public void resume() {
        if (runningState != State.SUSPENDED) {
            throw new IllegalStateException("Stopwatch must be suspended to resume. ");
        }
        startTimeNanos += System.nanoTime() - stopTimeNanos;
        runningState = State.RUNNING;
    }

    /**
     * Splits the time.
     *
     * <p>
     * This method sets the stop time of the watch to allow a time to be extracted. The start time is unaffected, enabling {@link #unsplit()} to continue the
     * timing from the original start point.
     * </p>
     *
     * @throws IllegalStateException if the StopWatch is not running.
     */
    public void split() {
        if (runningState != State.RUNNING) {
            throw new IllegalStateException("Stopwatch is not running. ");
        }
        stopTimeNanos = System.nanoTime();
        splitState = SplitState.SPLIT;
    }

    /**
     * Starts the StopWatch.
     *
     * <p>
     * This method starts a new timing session, clearing any previous values.
     * </p>
     *
     * @throws IllegalStateException if the StopWatch is already running.
     */
    public void start() {
        if (runningState == State.STOPPED) {
            throw new IllegalStateException("Stopwatch must be reset before being restarted. ");
        }
        if (runningState != State.UNSTARTED) {
            throw new IllegalStateException("Stopwatch already started. ");
        }
        startTimeNanos = System.nanoTime();
        startInstant = Instant.now();
        runningState = State.RUNNING;
    }

    /**
     * Stops the StopWatch.
     *
     * <p>
     * This method ends a new timing session, allowing the time to be retrieved.
     * </p>
     *
     * @throws IllegalStateException if the StopWatch is not running.
     */
    public void stop() {
        if (runningState != State.RUNNING && runningState != State.SUSPENDED) {
            throw new IllegalStateException("Stopwatch is not running. ");
        }
        if (runningState == State.RUNNING) {
            stopTimeNanos = System.nanoTime();
            stopInstant = Instant.now();
        }
        runningState = State.STOPPED;
    }

    /**
     * Suspends the StopWatch for later resumption.
     *
     * <p>
     * This method suspends the watch until it is resumed. The watch will not include time between the suspend and resume calls in the total time.
     * </p>
     *
     * @throws IllegalStateException if the StopWatch is not currently running.
     */
    public void suspend() {
        if (runningState != State.RUNNING) {
            throw new IllegalStateException("Stopwatch must be running to suspend. ");
        }
        stopTimeNanos = System.nanoTime();
        stopInstant = Instant.now();
        runningState = State.SUSPENDED;
    }

    /**
     * Gets a summary of the split time that the StopWatch recorded as a string.
     *
     * <p>
     * The format used is ISO 8601-like, [<i>message</i> ]<i>hours</i>:<i>minutes</i>:<i>seconds</i>.<i>milliseconds</i>.
     * </p>
     *
     * @return the split time as a String
     * @since 2.1
     * @since 3.10 Returns the prefix {@code "message "} if the message is set.
     */
    public String toSplitString() {
        final String msgStr = Objects.toString(message, StringUtils.EMPTY);
        final String formattedTime = formatSplitTime();
        return msgStr.isEmpty() ? formattedTime : msgStr + StringUtils.SPACE + formattedTime;
    }

    /**
     * Gets a summary of the time that the StopWatch recorded as a string.
     *
     * <p>
     * The format used is ISO 8601-like, [<i>message</i> ]<i>hours</i>:<i>minutes</i>:<i>seconds</i>.<i>milliseconds</i>.
     * </p>
     *
     * @return the time as a String
     * @since 3.10 Returns the prefix {@code "message "} if the message is set.
     */
    @Override
    public String toString() {
        final String msgStr = Objects.toString(message, StringUtils.EMPTY);
        final String formattedTime = formatTime();
        return msgStr.isEmpty() ? formattedTime : msgStr + StringUtils.SPACE + formattedTime;
    }

    /**
     * Removes a split.
     *
     * <p>
     * This method clears the stop time. The start time is unaffected, enabling timing from the original start point to continue.
     * </p>
     *
     * @throws IllegalStateException if the StopWatch has not been split.
     */
    public void unsplit() {
        if (splitState != SplitState.SPLIT) {
            throw new IllegalStateException("Stopwatch has not been split. ");
        }
        splitState = SplitState.UNSPLIT;
    }

    /**
     * Take the time of the exectution of a given {@link FailableSupplier}.
     *
     * <p>
     * <em>Take the time of the execution of given {@link FailableSupplier}</em>
     * </p>
     * <pre>{@code
     * final StopWatch watch = StopWatch.create();
     * String result = watch.get(() -> "A");
     * }</pre>
     *
     * <p>
     *  <em>Note:</em>
     * </p>
     * <ul>
     *     <li>Be careful if you pass a {@link FailableSupplier} to other methods, as the wrapped instance depends on
     *         the state of used {@linkplain StopWatch}</li>
     * </ul>
     *
     * @param <T> the type of return value of the supplier
     * @param <E> the kind of thrown exception or error
     * @param supplier the {@link FailableSupplier} those execution should be measured
     * @return the result of the {@link FailableSupplier#get()} operation
     * @throws E if the supplier fails
     * @since 3.18.0
     */
    public <T, E extends Throwable> T get(final FailableSupplier<T, E> supplier) throws E {
        resumeOrStartStopWatch();
        try {
            return supplier.get();
        } finally {
            suspend();
        }
    }

    /**
     * Take the time of the execution of a given {@link Supplier}.
     *
     * <p>
     * <em>Take the time of the execution of given {@link Supplier}</em>
     * </p>
     * <pre>{@code
     * final StopWatch watch = StopWatch.create();
     * String result = watch.get(() -> "A");
     * }</pre>
     *
     * <p>
     *  <em>Note:</em>
     * </p>
     * <ul>
     *     <li>Be careful if you pass a {@link Supplier} to other methods, as the wrapped instance depends on
     *         the state of used {@linkplain StopWatch}</li>
     * </ul>
     *
     * @param <T> the type of the input to the function
     * @param supplier the {@link Supplier} those execution should be measured
     * @return the result of the {@link Supplier#get()} operation
     * @throws IllegalStateException if the {@link StopWatch} is not stopped or suspended
     * @since 3.18.0
     */
    public <T> T get(final Supplier<T> supplier) {
        resumeOrStartStopWatch();
        try {
            return supplier.get();
        } finally {
            suspend();
        }
    }

    /**
     * Take the time of the execution of a given {@link Function}.
     *
     * <p>
     * <em>Take the time of given {@link Function}</em>
     * </p>
     * <pre>{@code
     * final StopWatch watch = StopWatch.create();
     * String result = watch.apply(it -> it.toLowerCase(Locale.ROOT)).apply("A");
     * }</pre>
     *
     * <p>
     * <em>Take the time of an applied {@link Function} in a stream</em>
     * </p>
     * <pre>{@code
     * final StopWatch watch = StopWatch.create();
     * String result = Stream.of("A", "B", "C")
     *                       .map(watch.apply(it -> it.toLowerCase(Locale.ROOT)))
     *                       .collect(Collectors.joining());
     * }</pre>
     *
     * <p>
     *  <em>Note:</em>
     * </p>
     * <ul>
     *     <li>Be careful if you pass a {@link Function} to other methods, as the wrapped instance depends on
     *         the state of used {@linkplain StopWatch}</li>
     * </ul>
     *
     * @param <T> the type of the input to the function
     * @param <R> the type of the result of the function
     * @param function the function those application should be measured
     * @return the given function prepared to take time if applied
     * @throws IllegalStateException if the {@link StopWatch} is not stopped or suspended
     * @since 3.18.0
     */
    public <T, R> Function<T, R> apply(final Function<T, R> function) {
        return arg -> {
            resumeOrStartStopWatch();
            try {
                return function.apply(arg);
            } finally {
                suspend();
            }
        };
    }

    /**
     * Take the time of the execution of a given {@link FailableFunction}.
     *
     * <p>
     * <em>Take the time of given {@link FailableFunction}</em>
     * </p>
     * <pre>{@code
     * final StopWatch watch = StopWatch.create();
     * String result = watch.apply((arg) -> arg.toLowerCase())).apply("A");
     * }</pre>
     *
     * <p>
     *  <em>Note:</em>
     * </p>
     * <ul>
     *     <li>Be careful if you pass a {@link FailableFunction} to other methods, as the wrapped instance depends on
     *         the state of used {@linkplain StopWatch}</li>
     * </ul>
     *
     * @param <T> the type of the first argument to the function
     * @param <R> the type of the result of the function
     * @param <E> the type of thrown exception or error
     * @param function the function those application should be measured
     * @return the given function prepared to take time if applied
     * @throws IllegalStateException if the {@link StopWatch} is not stopped or suspended
     * @since 3.18.0
     */
    public <T, R, E extends Throwable> FailableFunction<T, R, E> apply(final FailableFunction<T, R, E> function) {
        return arg -> {
            resumeOrStartStopWatch();
            try {
                return function.apply(arg);
            } finally {
                suspend();
            }
        };
    }

    /**
     * Take the time of the execution of a given {@link BiFunction}.
     *
     * <p>
     * <em>Take the time of given {@link BiFunction}</em>
     * </p>
     * <pre>{@code
     * final StopWatch watch = StopWatch.create();
     * String result = watch.apply((first, second) -> first + second)).apply("A", "B");
     * }</pre>
     *
     * <p>
     *  <em>Note:</em>
     * </p>
     * <ul>
     *     <li>Be careful if you pass a {@link BiFunction} to other methods, as the wrapped instance depends on
     *         the state of used {@linkplain StopWatch}</li>
     * </ul>
     *
     * @param <T> the type of the first argument to the function
     * @param <U> the type of the second argument to the function
     * @param <R> the type of the result of the function
     * @param function the function those application should be measured
     * @return the given function prepared to take time if applied
     * @throws IllegalStateException if the {@link StopWatch} is not stopped or suspended
     * @since 3.18.0
     */
    public <T, U, R> BiFunction<T, U, R> apply(final BiFunction<T, U, R> function) {
        return (firstArg, secondArg) -> {
            resumeOrStartStopWatch();
            try {
                return function.apply(firstArg, secondArg);
            } finally {
                suspend();
            }
        };
    }

    /**
     * Take the time of the execution of a given {@link FailableBiFunction}.
     *
     * <p>
     * <em>Take the time of given {@link FailableBiFunction}</em>
     * </p>
     * <pre>{@code
     * final StopWatch watch = StopWatch.create();
     * String result = watch.apply((first, second) -> first + second)).apply("A", "B");
     * }</pre>
     *
     * <p>
     *  <em>Note:</em>
     * </p>
     * <ul>
     *     <li>Be careful if you pass a {@link FailableBiFunction} to other methods, as the wrapped instance depends on
     *         the state of used {@linkplain StopWatch}</li>
     * </ul>
     *
     * @param <T> the type of the first argument to the function
     * @param <U> the type of the second argument to the function
     * @param <R> the type of the result of the function
     * @param <E> the type of thrown exception or error
     * @param function the function those application should be measured
     * @return the given function prepared to take time if applied
     * @throws IllegalStateException if the {@link StopWatch} is not stopped or suspended
     * @since 3.18.0
     */
    public <T, U, R, E extends Throwable> FailableBiFunction<T, U, R, E> apply(final FailableBiFunction<T, U, R, E> function) {
        return (firstArg, secondArg) -> {
            resumeOrStartStopWatch();
            try {
                return function.apply(firstArg, secondArg);
            } finally {
                suspend();
            }
        };
    }

    /**
     * Take the time of the execution of a given {@link TriFunction}.
     *
     * <p>
     * <em>Take the time of given {@link TriFunction}</em>
     * </p>
     * <pre>{@code
     * final StopWatch watch = StopWatch.create();
     * String result = watch.apply((first, second, third) -> first + second)).apply("A", "B", "C");
     * }</pre>
     *
     * <p>
     *  <em>Note:</em>
     * </p>
     * <ul>
     *     <li>Be careful if you pass a {@link TriFunction} to other methods, as the wrapped instance depends on
     *         the state of used {@linkplain StopWatch}</li>
     * </ul>
     *
     * @param <T> the type of the first argument to the function
     * @param <U> the type of the second argument to the function
     * @param <V> the type of the third argument to the function
     * @param <R> the type of the result of the function
     * @param function the function those application should be measured
     * @return the given function prepared to take time if applied
     * @throws IllegalStateException if the StopWatch is not stopped or suspended
     * @since 3.18.0
     */
    public <T, U, V, R> TriFunction<T, U, V, R> apply(final TriFunction<T, U, V, R> function) {
        return (firstArg, secondArg, thirdArg) -> {
            resumeOrStartStopWatch();
            try {
                return function.apply(firstArg, secondArg, thirdArg);
            } finally {
                suspend();
            }
        };
    }

    /**
     * Take the time of the execution of a given {@link Consumer}.
     *
     * <p>
     * <em>Take the time of given {@link Consumer}</em>
     * </p>
     * <pre>{@code
     * final StopWatch watch = StopWatch.create();
     * watch.accept((argument) -> process(argument)).accept("A");
     * }</pre>
     *
     * <p>
     *  <em>Note:</em>
     * </p>
     * <ul>
     *     <li>Be careful if you pass a {@link Consumer} to other methods, as the wrapped instance depends on
     *         the state of used {@linkplain StopWatch}</li>
     * </ul>
     *
     * @param <T> the type of the argument to the consumer
     * @param consumer the consumer those application should be measured
     * @return the given consumer prepared to take time if applied
     * @throws IllegalStateException if the {@link StopWatch} is not stopped or suspended
     * @since 3.18.0
     */
    public <T> Consumer<T> accept(final Consumer<T> consumer) {
        return arg -> {
            resumeOrStartStopWatch();
            try {
                consumer.accept(arg);
            } finally {
                suspend();
            }
        };
    }

    /**
     * Take the time of the execution of a given {@link BiConsumer}.
     *
     * <p>
     * <em>Take the time of given {@link BiConsumer}</em>
     * </p>
     * <pre>{@code
     * final StopWatch watch = StopWatch.create();
     * watch.accept((first, second) -> process(first, second)).accept("A", "B");
     * }</pre>
     *
     * <p>
     *  <em>Note:</em>
     * </p>
     * <ul>
     *   <li>Be careful if you pass a {@link BiConsumer} to other methods, as the wrapped instance depends on
     *       the state of used {@linkplain StopWatch}</li>
     * </ul>
     *
     * @param <T> the type of the first argument to the consumer
     * @param <U> the type of the second argument to the consumer
     * @param consumer the consumer those application should be measured
     * @return the given consumer prepared to take time if applied
     * @throws IllegalStateException if the {@link StopWatch} is not stopped or suspended
     * @since 3.18.0
     */
    public <T, U> BiConsumer<T, U> accept(final BiConsumer<T, U> consumer) {
        return (first, second) -> {
            resumeOrStartStopWatch();
            try {
                consumer.accept(first, second);
            } finally {
                suspend();
            }
        };
    }

    /**
     * Take the time of the execution of a given {@link FailableBiConsumer}.
     *
     * <p>
     * <em>Take the time of given {@link FailableBiConsumer}</em>
     * </p>
     * <pre>{@code
     * final StopWatch watch = StopWatch.create();
     * watch.accept((first, second) -> process(first, second)).accept("A", "B");
     * }</pre>
     *
     * <p>
     *  <em>Note:</em>
     * </p>
     * <ul>
     *     <li>Be careful if you pass a {@link FailableBiConsumer} to other methods, as the wrapped instance depends on
     *         the state of used {@linkplain StopWatch}</li>
     * </ul>
     *
     * @param <T> the type of the first argument to the consumer
     * @param <U> the type of the second argument to the consumer
     * @param <E> The kind of thrown exception or error
     * @param consumer the consumer those application should be measured
     * @return the given consumer prepared to take time if applied
     * @throws IllegalStateException if the StopWatch is not stopped or suspended
     * @since 3.18.0
     */
    public <T, U, E extends Exception> FailableBiConsumer<T, U, E> accept(final FailableBiConsumer<T, U, E> consumer) {
        return (first, second) -> {
            resumeOrStartStopWatch();
            try {
                consumer.accept(first, second);
            } finally {
                suspend();
            }
        };
    }

    /**
     * Take the time of the execution of a given {@link FailableConsumer}.
     *
     * <p>
     * <em>Take the time of given {@link FailableConsumer}</em>
     * </p>
     * <pre>{@code
     * final StopWatch watch = StopWatch.create();
     * watch.accept((first, second) -> process(arg)).accept("A");
     * }</pre>
     *
     * <p>
     *  <em>Note:</em>
     * </p>
     * <ul>
     *     <li>Be careful if you pass a {@link FailableConsumer} to other methods, as the wrapped instance depends on
     *         the state of used {@linkplain StopWatch}</li>
     * </ul>
     *
     * @param <T> the type of first argument to the consumer
     * @param <E> The kind of thrown exception or error
     * @param consumer the consumer those application should be measured
     * @return the given consumer prepared to take time if applied
     * @throws IllegalStateException if the {@link StopWatch} is not stopped or suspended
     * @since 3.18.0
     */
    public <T, E extends Exception> FailableConsumer<T, E> accept(final FailableConsumer<T, E> consumer) {
        return arg -> {
            resumeOrStartStopWatch();
            try {
                consumer.accept(arg);
            } finally {
                suspend();
            }
        };
    }

    /**
     * Take the time of the execution of a given {@link TriConsumer}.
     *
     * <p>
     * <em>Take the time of given {@link TriConsumer}</em>
     * </p>
     * <pre>{@code
     * final StopWatch watch = StopWatch.create();
     * watch.accept((first, second, third) -> process(first, second, third)).accept("A", "B", "C");
     * }</pre>
     *
     * <p>
     *  <em>Note:</em>
     * </p>
     * <ul>
     *     <li>Be careful if you pass a {@link TriConsumer} to other methods, as the wrapped instance depends on
     *         the state of used {@linkplain StopWatch}</li>
     * </ul>
     *
     * @param <T> the type of the first argument to the consumer
     * @param <U> the type of the second argument to the consumer
     * @param <V> the type of the third argument to the consumer
     * @param consumer the consumer those application should be measured
     * @return the given consumer prepared to take time if applied
     * @throws IllegalStateException if the {@link StopWatch} is not stopped or suspended
     * @since 3.18.0
     */
    public <T, U, V> TriConsumer<T, U, V> accept(final TriConsumer<T, U, V> consumer) {
        return (firstArg, secondArg, thirdArg) -> {
            resumeOrStartStopWatch();
            try {
                consumer.accept(firstArg, secondArg, thirdArg);
            } finally {
                suspend();
            }
        };
    }

    /**
     * Take the time of the execution of a given {@link Predicate}.
     *
     * <p>
     * <em>Take the time of given {@link Predicate}</em>
     * </p>
     * <pre>{@code
     * final StopWatch watch = StopWatch.create();
     * Streams.of("A", "B")
     *         .filter(watch.test(it -> "A".equals(it))
     *         .forEach(it -> {});
     * }</pre>
     *
     * <p>
     *  <em>Note:</em>
     * </p>
     * <ul>
     *     <li>Be careful if you pass a {@link StopWatch} to other methods, as the wrapped instance depends on
     *         the state of used {@linkplain StopWatch}</li>
     * </ul>
     *
     * @param <T> the type of the argument to the predicate
     * @param predicate the predicate those application should be measured
     * @return the given predicate prepared to take time if applied
     * @throws IllegalStateException if the {@link StopWatch} is not stopped or suspended
     * @since 3.18.0
     */
    public <T> Predicate<T> test(final Predicate<T> predicate) {
        return arg -> {
            resumeOrStartStopWatch();
            try {
                return predicate.test(arg);
            } finally {
                suspend();
            }
        };
    }

    /**
     * Take the time of the execution of a given {@link BiPredicate}.
     *
     * <p>
     * <em>Take the time of given {@link BiPredicate}</em>
     * </p>
     * <pre>{@code
     * final StopWatch watch = StopWatch.create();
     * BiPredicate<String, String> predicate = Objects::equals;
     * long result = Streams.of(ImmutablePair.of("A", "A"), ImmutablePair.of("A", "B"))
     *                       .filter(it -> watch.test(predicate).test(it.getLeft(), it.getRight()))
     *                       .count();
     * }</pre>
     *
     * <p>
     *  <em>Note:</em>
     * </p>
     * <ul>
     *     <li>Be careful if you pass a {@link BiPredicate} to other methods, as the wrapped instance depends on
     *         the state of used {@linkplain StopWatch}</li>
     * </ul>
     *
     * @param <T> the type of the first argument to the predicate
     * @param <U> the type of the second argument to the predicate
     * @param predicate the predicate those application should be measured
     * @return the given predicate prepared to take time if applied
     * @throws IllegalStateException if the {@link StopWatch} is not stopped or suspended
     * @since 3.18.0
     */
    public <T, U> BiPredicate<T, U> test(final BiPredicate<T, U> predicate) {
        return (firstArg, secondArg) -> {
            resumeOrStartStopWatch();
            try {
                return predicate.test(firstArg, secondArg);
            } finally {
                suspend();
            }
        };
    }

    /**
     * Take the time of the execution of a given {@link FailableBiPredicate}.
     *
     * <p>
     * <em>Take the time of given {@link FailableBiPredicate}</em>
     * </p>
     * <pre>{@code
     * final StopWatch watch = StopWatch.create();
     * FailableBiPredicate<String, String> predicate = Objects::equals;
     * boolean result = watch.test(predicate).test("A", "B");
     * }</pre>
     *
     * <p>
     *  <em>Note:</em>
     * </p>
     * <ul>
     *     <li>Be careful if you pass a {@link FailableBiPredicate} to other methods, as the wrapped instance depends on
     *         the state of used {@linkplain StopWatch}</li>
     * </ul>
     *
     * @param <T> the type of the first argument to the predicate
     * @param <U> the type of the second argument to the predicate
     * @param <E> The kind of thrown exception or error
     * @param predicate the predicate those application should be measured
     * @return the given predicate prepared to take time if applied
     * @throws IllegalStateException if the {@link StopWatch} is not stopped or suspended
     * @since 3.18.0
     */
    public <T, U, E extends Throwable> FailableBiPredicate<T, U, E> test(final FailableBiPredicate<T, U, E> predicate) {
        return (firstArg, secondArg) -> {
            resumeOrStartStopWatch();
            try {
                return predicate.test(firstArg, secondArg);
            } finally {
                suspend();
            }
        };
    }

    /**
     * Take the time of the execution of a given {@link FailablePredicate}.
     *
     * <p>
     * <em>Take the time of given {@link FailablePredicate}</em>
     * </p>
     * <pre>{@code
     * final StopWatch watch = StopWatch.create();
     * FailablePredicate<String> predicate = false;
     * boolean result = watch.test(predicate).test("A");
     * }</pre>
     *
     * <p>
     *  <em>Note:</em>
     * </p>
     *  <ul>
     *      <li>Be careful if you pass a {@link FailablePredicate} to other methods, as the wrapped instance depends on
     *          the state of used {@linkplain StopWatch}</li>
     *  </ul>
     *
     * @param <T> the type of the argument to the predicate
     * @param <E> The kind of thrown exception or error
     * @param predicate the predicate those application should be measured
     * @return the given predicate prepared to take time if applied
     * @throws IllegalStateException if the {@link StopWatch} is not stopped or suspended
     * @since 3.18.0
     */
    public <T, E extends Throwable> FailablePredicate<T, E> test(final FailablePredicate<T, E> predicate) {
        resumeOrStartStopWatch();
        return (arg) -> {
            try {
                return predicate.test(arg);
            } finally {
                suspend();
            }
        };
    }

    private void resumeOrStartStopWatch() {
        if (isStopped()) {
            start();
        } else if (isSuspended()) {
            resume();
        }
    }
}
