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

import java.util.Objects;
import java.util.concurrent.TimeUnit;

import org.apache.commons.lang3.StringUtils;

/**
 * {@link StopWatch} provides a convenient API for timings.
 *
 * <p>
 * To start the watch, call {@link #start()} or {@link StopWatch#createStarted()}. At this point you can:
 * </p>
 * <ul>
 * <li>{@link #split()} the watch to get the time whilst the watch continues in the background. {@link #unsplit()} will
 * remove the effect of the split. At this point, these three options are available again.</li>
 * <li>{@link #suspend()} the watch to pause it. {@link #resume()} allows the watch to continue. Any time between the
 * suspend and resume will not be counted in the total. At this point, these three options are available again.</li>
 * <li>{@link #stop()} the watch to complete the timing session.</li>
 * </ul>
 *
 * <p>
 * It is intended that the output methods {@link #toString()} and {@link #getTime()} should only be called after stop,
 * split or suspend, however a suitable result will be returned at other points.
 * </p>
 *
 * <p>
 * NOTE: As from v2.1, the methods protect against inappropriate calls. Thus you cannot now call stop before start,
 * resume before suspend or unsplit before split.
 * </p>
 *
 * <p>
 * 1. split(), suspend(), or stop() cannot be invoked twice<br>
 * 2. unsplit() may only be called if the watch has been split()<br>
 * 3. resume() may only be called if the watch has been suspend()<br>
 * 4. start() cannot be called twice without calling reset()
 * </p>
 *
 * <p>This class is not thread-safe</p>
 *
 * @since 2.0
 */
public class StopWatch {

    /**
     * Enumeration type which indicates the split status of stopwatch.
     */
    private enum SplitState {
        SPLIT,
        UNSPLIT
    }

    /**
     * Enumeration type which indicates the status of stopwatch.
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
         * Returns whether the StopWatch is started. A suspended StopWatch is also started watch.
         *
         * @return boolean If the StopWatch is started.
         */
        abstract boolean isStarted();

        /**
         * Returns whether the StopWatch is stopped. The stopwatch which's not yet started and explicitly stopped stopwatch is
         * considered as stopped.
         *
         * @return boolean If the StopWatch is stopped.
         */
        abstract boolean isStopped();

        /**
         * Returns whether the StopWatch is suspended.
         *
         * @return boolean
         *             If the StopWatch is suspended.
         */
        abstract boolean isSuspended();
    }

    private static final long NANO_2_MILLIS = 1000000L;

    /**
     * Creates a stopwatch for convenience.
     *
     * @return StopWatch a stopwatch.
     *
     * @since 3.10
     */
    public static StopWatch create() {
        return new StopWatch();
    }

    /**
     * Creates a started stopwatch for convenience.
     *
     * @return StopWatch a stopwatch that's already been started.
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
     * Whether the stopwatch has a split time recorded.
     */
    private SplitState splitState = SplitState.UNSPLIT;

    /**
     * The start time in nanoseconds.
     */
    private long startTimeNanos;

    /**
     * The start time in milliseconds - nanoTime is only for elapsed time so we
     * need to also store the currentTimeMillis to maintain the old
     * getStartTime API.
     */
    private long startTimeMillis;

    /**
     * The end time in milliseconds - nanoTime is only for elapsed time so we
     * need to also store the currentTimeMillis to maintain the old
     * getStartTime API.
     */
    private long stopTimeMillis;

    /**
     * The stop time in nanoseconds.
     */
    private long stopTimeNanos;

    /**
     * Constructor.
     *
     */
    public StopWatch() {
        this(null);
    }

    /**
     * Constructor.
     *
     * @param message A message for string presentation.
     * @since 3.10
     */
    public StopWatch(final String message) {
        this.message = message;
    }

    /**
     * Returns the split time formatted by {@link DurationFormatUtils#formatDurationHMS}.
     *
     * @return the split time formatted by {@link DurationFormatUtils#formatDurationHMS}.
     * @since 3.10
     */
    public String formatSplitTime() {
        return DurationFormatUtils.formatDurationHMS(getSplitTime());
    }

    /**
     * Returns the time formatted by {@link DurationFormatUtils#formatDurationHMS}.
     *
     * @return the time formatted by {@link DurationFormatUtils#formatDurationHMS}.
     * @since 3.10
     */
    public String formatTime() {
        return DurationFormatUtils.formatDurationHMS(getTime());
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
     * This is either the time between the start and the moment this method is called, or the amount of time between
     * start and stop.
     * </p>
     *
     * @return the <em>elapsed</em> time in nanoseconds.
     * @see System#nanoTime()
     * @since 3.0
     */
    public long getNanoTime() {
        if (this.runningState == State.STOPPED || this.runningState == State.SUSPENDED) {
            return this.stopTimeNanos - this.startTimeNanos;
        }
        if (this.runningState == State.UNSTARTED) {
            return 0;
        }
        if (this.runningState == State.RUNNING) {
            return System.nanoTime() - this.startTimeNanos;
        }
        throw new IllegalStateException("Illegal running state has occurred.");
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
     * @throws IllegalStateException
     *             if the StopWatch has not yet been split.
     * @since 3.0
     */
    public long getSplitNanoTime() {
        if (this.splitState != SplitState.SPLIT) {
            throw new IllegalStateException("Stopwatch must be split to get the split time.");
        }
        return this.stopTimeNanos - this.startTimeNanos;
    }

    /**
     * Gets the split time on the stopwatch.
     *
     * <p>
     * This is the time between start and latest split.
     * </p>
     *
     * @return the split time in milliseconds
     *
     * @throws IllegalStateException
     *             if the StopWatch has not yet been split.
     * @since 2.1
     */
    public long getSplitTime() {
        return getSplitNanoTime() / NANO_2_MILLIS;
    }

    /**
     * Gets the time this stopwatch was started in milliseconds, between the current time and midnight, January 1, 1970
     * UTC.
     *
     * @return the time this stopwatch was started in milliseconds, between the current time and midnight, January 1,
     *         1970 UTC.
     * @throws IllegalStateException if this StopWatch has not been started
     * @since 2.4
     */
    public long getStartTime() {
        if (this.runningState == State.UNSTARTED) {
            throw new IllegalStateException("Stopwatch has not been started");
        }
        // System.nanoTime is for elapsed time
        return this.startTimeMillis;
    }

    /**
     * Gets the time this stopwatch was stopped in milliseconds, between the current time and midnight, January 1, 1970
     * UTC.
     *
     * @return the time this stopwatch was started in milliseconds, between the current time and midnight, January 1,
     *         1970 UTC.
     * @throws IllegalStateException if this StopWatch has not been started
     * @since 3.12.0
     */
    public long getStopTime() {
        if (this.runningState == State.UNSTARTED) {
            throw new IllegalStateException("Stopwatch has not been started");
        }
        // System.nanoTime is for elapsed time
        return this.stopTimeMillis;
    }

    /**
     * Gets the time on the stopwatch.
     *
     * <p>
     * This is either the time between the start and the moment this method is called, or the amount of time between
     * start and stop.
     * </p>
     *
     * @return the time in milliseconds
     */
    public long getTime() {
        return getNanoTime() / NANO_2_MILLIS;
    }

    /**
     * Gets the time in the specified TimeUnit.
     *
     * <p>
     * This is either the time between the start and the moment this method is called, or the amount of time between
     * start and stop. The resulting time will be expressed in the desired TimeUnit with any remainder rounded down.
     * For example, if the specified unit is {@code TimeUnit.HOURS} and the stopwatch time is 59 minutes, then the
     * result returned will be {@code 0}.
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
     * Returns whether the StopWatch is started. A suspended StopWatch is also started watch.
     *
     * @return boolean If the StopWatch is started.
     * @since 3.2
     */
    public boolean isStarted() {
        return runningState.isStarted();
    }

    /**
     * Returns whether StopWatch is stopped. The stopwatch which's not yet started and explicitly stopped stopwatch is considered
     * as stopped.
     *
     * @return boolean If the StopWatch is stopped.
     * @since 3.2
     */
    public boolean isStopped() {
        return runningState.isStopped();
    }

    /**
     * Returns whether the StopWatch is suspended.
     *
     * @return boolean
     *             If the StopWatch is suspended.
     * @since 3.2
     */
    public boolean isSuspended() {
        return runningState.isSuspended();
    }

    /**
     * Resets the stopwatch. Stops it if need be.
     *
     * <p>
     * This method clears the internal values to allow the object to be reused.
     * </p>
     */
    public void reset() {
        this.runningState = State.UNSTARTED;
        this.splitState = SplitState.UNSPLIT;
    }

    /**
     * Resumes the stopwatch after a suspend.
     *
     * <p>
     * This method resumes the watch after it was suspended. The watch will not include time between the suspend and
     * resume calls in the total time.
     * </p>
     *
     * @throws IllegalStateException
     *             if the StopWatch has not been suspended.
     */
    public void resume() {
        if (this.runningState != State.SUSPENDED) {
            throw new IllegalStateException("Stopwatch must be suspended to resume. ");
        }
        this.startTimeNanos += System.nanoTime() - this.stopTimeNanos;
        this.runningState = State.RUNNING;
    }

    /**
     * Splits the time.
     *
     * <p>
     * This method sets the stop time of the watch to allow a time to be extracted. The start time is unaffected,
     * enabling {@link #unsplit()} to continue the timing from the original start point.
     * </p>
     *
     * @throws IllegalStateException
     *             if the StopWatch is not running.
     */
    public void split() {
        if (this.runningState != State.RUNNING) {
            throw new IllegalStateException("Stopwatch is not running. ");
        }
        this.stopTimeNanos = System.nanoTime();
        this.splitState = SplitState.SPLIT;
    }

    /**
     * Starts the stopwatch.
     *
     * <p>
     * This method starts a new timing session, clearing any previous values.
     * </p>
     *
     * @throws IllegalStateException
     *             if the StopWatch is already running.
     */
    public void start() {
        if (this.runningState == State.STOPPED) {
            throw new IllegalStateException("Stopwatch must be reset before being restarted. ");
        }
        if (this.runningState != State.UNSTARTED) {
            throw new IllegalStateException("Stopwatch already started. ");
        }
        this.startTimeNanos = System.nanoTime();
        this.startTimeMillis = System.currentTimeMillis();
        this.runningState = State.RUNNING;
    }

    /**
     * Stops the stopwatch.
     *
     * <p>
     * This method ends a new timing session, allowing the time to be retrieved.
     * </p>
     *
     * @throws IllegalStateException
     *             if the StopWatch is not running.
     */
    public void stop() {
        if (this.runningState != State.RUNNING && this.runningState != State.SUSPENDED) {
            throw new IllegalStateException("Stopwatch is not running. ");
        }
        if (this.runningState == State.RUNNING) {
            this.stopTimeNanos = System.nanoTime();
            this.stopTimeMillis = System.currentTimeMillis();
        }
        this.runningState = State.STOPPED;
    }

    /**
     * Suspends the stopwatch for later resumption.
     *
     * <p>
     * This method suspends the watch until it is resumed. The watch will not include time between the suspend and
     * resume calls in the total time.
     * </p>
     *
     * @throws IllegalStateException
     *             if the StopWatch is not currently running.
     */
    public void suspend() {
        if (this.runningState != State.RUNNING) {
            throw new IllegalStateException("Stopwatch must be running to suspend. ");
        }
        this.stopTimeNanos = System.nanoTime();
        this.stopTimeMillis = System.currentTimeMillis();
        this.runningState = State.SUSPENDED;
    }

    /**
     * Gets a summary of the split time that the stopwatch recorded as a string.
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
     * Gets a summary of the time that the stopwatch recorded as a string.
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
     * This method clears the stop time. The start time is unaffected, enabling timing from the original start point to
     * continue.
     * </p>
     *
     * @throws IllegalStateException
     *             if the StopWatch has not been split.
     */
    public void unsplit() {
        if (this.splitState != SplitState.SPLIT) {
            throw new IllegalStateException("Stopwatch has not been split. ");
        }
        this.splitState = SplitState.UNSPLIT;
    }

}
