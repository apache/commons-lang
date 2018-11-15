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

/**
 * <p>A suite of utilities surround the use of the {@link java.time.Duration} object</p>
 * <p>DurationUtils contains methods to rounding duration value</p>
 *
 * @since 3.9
 */
public class DurationUtils {

    /**
     * Number of nanoseconds in a standard day
     *
     * @since 3.9
     */
    public static final long NANOS_PER_DAY = 86_400_000_000_000L;

    /**
     * Number of nanoseconds in a standard hour
     *
     * @since 3.9
     */
    public static final long NANOS_PER_HOUR = 3_600_000_000_000L;

    /**
     * Number of nanoseconds in a standard minute
     *
     * @since 3.9
     */
    public static final long NANOS_PER_MINUTE = 60_000_000_000L;

    /**
     * Number of nanoseconds in a standard second
     *
     * @since 3.9
     */
    public static final long NANOS_PER_SECOND = 1_000_000_000L;

    /**
     * Number of nanoseconds in a standard millisecond
     *
     * @since 3.9
     */
    public static final long NANOS_PER_MILLISECOND = 1_000_000L;

    /**
     * <p>{@code DurationUtils} instances should NOT be constructed in
     * standard programming. </p>
     *
     * <p>This constructor is public to permit tools that require a JavaBean
     * instance to operate</p>
     */
    public DurationUtils() {
        super();
    }

    /**
     * <p>Rounds up the quantity of days from the given duration</p>
     *
     * <p>For the duration of 24h it should return 1(day) and the duration of 25 h it should return 2(days)</p>
     *
     * @param duration the given duration from which the quantity of days will be calculated
     * @return the rounded up quantity of days
     * @throws IllegalArgumentException if the given duration is null
     * @since 3.9
     */
    public static long roundUpDaysQuantity(final Duration duration) {
        if (duration == null) {
            throw new IllegalArgumentException("the duration must not be null");
        }
        boolean shouldRoundUpDaysQuantity = (duration.toNanos() % NANOS_PER_DAY > 0);
        if (shouldRoundUpDaysQuantity) {
            return duration.toDays() + 1;
        }
        return duration.toDays();
    }

    /**
     * <p>Rounds the quantity of days from the given duration</p>
     *
     * <p>For the duration of 36h it should return 2 (days) and for the duration of 36h 59 minutes it should return 1(day)</p>
     *
     * @param duration the given duration from which the quantity of days will be calculated
     * @return the rounded quantity of days
     * @throws IllegalArgumentException if the given duration is null
     * @since 3.9
     */
    public static long roundDaysQuantity(final Duration duration) {
        if (duration == null) {
            throw new IllegalArgumentException("the duration must not be null");
        }
        boolean shouldRoundDaysQuantity = ((duration.toNanos() % NANOS_PER_DAY) >= (NANOS_PER_DAY / 2));
        if (shouldRoundDaysQuantity) {
            return duration.toDays() + 1;
        }
        return duration.toDays();
    }

    /**
     * <p>Rounds up the quantity of hours from the given duration</p>
     *
     * <p>For the duration of 1h it should return 1(hour) and the duration of 1h 10 min  it should return 2(hours)</p>
     *
     * @param duration the given duration from which the quantity of hours will be calculated
     * @return the rounded up quantity of hours
     * @throws IllegalArgumentException if the given duration is null
     * @since 3.9
     */
    public static long roundUpHoursQuantity(final Duration duration) {
        if (duration == null) {
            throw new IllegalArgumentException("the duration must not be null");
        }
        boolean shouldRoundUpHoursQuantity = (duration.toNanos() % NANOS_PER_HOUR > 0);
        if (shouldRoundUpHoursQuantity) {
            return duration.toHours() + 1;
        }
        return duration.toHours();
    }

    /**
     * <p>Rounds the quantity of hours from the given duration</p>
     *
     * <p>For the duration of 1h  30min it should return 2 (hours) and for the duration of 1h 29 min it should return 1(hour)</p>
     *
     * @param duration the given duration from which the quantity of hours will be calculated
     * @return the rounded quantity of hours
     * @throws IllegalArgumentException if the given duration is null
     * @since 3.9
     */
    public static long roundHoursQuantity(final Duration duration) {
        if (duration == null) {
            throw new IllegalArgumentException("the duration must not be null");
        }
        boolean shouldRoundHoursQuantity = ((duration.toNanos() % NANOS_PER_HOUR) >= (NANOS_PER_HOUR / 2));
        if (shouldRoundHoursQuantity) {
            return duration.toHours() + 1;
        }
        return duration.toHours();
    }

    /**
     * <p>Rounds up the quantity of minutes from the given duration</p>
     *
     * <p>For the duration of 10 minutes it should return 10(minutes) and the duration of 10 minutes 10 seconds  it should return 11(minutes)</p>
     *
     * @param duration the given duration from which the quantity of minutes will be calculated
     * @return the rounded up quantity of minutes
     * @throws IllegalArgumentException if the given duration is null
     * @since 3.9
     */
    public static long roundUpMinutesQuantity(final Duration duration) {
        if (duration == null) {
            throw new IllegalArgumentException("the duration must not be null");
        }
        boolean shouldRoundUpMinutes = (duration.toNanos() % NANOS_PER_MINUTE > 0);
        if (shouldRoundUpMinutes) {
            return duration.toMinutes() + 1;
        }
        return duration.toMinutes();
    }

    /**
     * <p>Rounds the quantity of minutes from the given duration</p>
     *
     * <p>For the duration of 1 minute  30 seconds it should return 2(minutes) and for the duration of 1 minute 29 seconds it should return 1(minute)</p>
     *
     * @param duration the given duration from which the quantity of minutes will be calculated
     * @return the rounded quantity of days
     * @throws IllegalArgumentException if the given duration is null
     * @since 3.9
     */
    public static long roundMinutesQuantity(final Duration duration) {
        if (duration == null) {
            throw new IllegalArgumentException("the duration must not be null");
        }
        boolean shouldRoundMinutes = ((duration.toNanos() % NANOS_PER_MINUTE) >= (NANOS_PER_MINUTE / 2));
        if (shouldRoundMinutes) {
            return duration.toMinutes() + 1;
        }
        return duration.toMinutes();
    }

    /**
     * <p>Rounds up the quantity of seconds from the given duration</p>
     *
     * <p>For the duration of 10 seconds it should return 10(seconds) and the duration of 1 second 10 milliseconds  it should return 2(seconds)</p>
     *
     * @param duration the given duration from which the quantity of seconds will be calculated
     * @return the rounded up quantity of seconds
     * @throws IllegalArgumentException if the given duration is null
     * @since 3.9
     */
    public static long roundUpSecondsQuantity(final Duration duration) {
        if (duration == null) {
            throw new IllegalArgumentException("the duration must not be null");
        }
        boolean shouldRoundUpSeconds = (duration.toNanos() % NANOS_PER_SECOND > 0);
        if (shouldRoundUpSeconds) {
            return duration.getSeconds() + 1;
        }
        return duration.getSeconds();
    }

    /**
     * <p>Rounds the quantity of seconds from the given duration</p>
     *
     * <p>For the duration of 10 seconds and 499 milliseconds it should return 10(seconds)
     * and the duration of 1 second 500 milliseconds  it should return 2(seconds)</p>
     *
     * @param duration the given duration from which the quantity of seconds will be calculated
     * @return the rounded quantity of seconds
     * @throws IllegalArgumentException if the given duration is null
     * @since 3.9
     */
    public static long roundSecondsQuantity(final Duration duration) {
        if (duration == null) {
            throw new IllegalArgumentException("the duration must not be null");
        }
        boolean shouldRoundSeconds = ((duration.toNanos() % NANOS_PER_SECOND) >= (NANOS_PER_SECOND / 2));
        if (shouldRoundSeconds) {
            return duration.getSeconds() + 1;
        }
        return duration.getSeconds();
    }

    /**
     * <p>Rounds up the quantity of milliseconds from the given duration</p>
     *
     * <p>For the duration of 10 milliseconds it should return 10(milliseconds) and the duration of 1 milliseconds 10 nanoseconds  it should return 2(milliseconds)</p>
     *
     * @param duration the given duration from which the quantity of milliseconds will be calculated
     * @return the rounded up quantity of milliseconds
     * @throws IllegalArgumentException if the given duration is null
     * @since 3.9
     */
    public static long roundUpMillisecondsQuantity(final Duration duration) {
        if (duration == null) {
            throw new IllegalArgumentException("the duration must not be null");
        }
        boolean shouldRoundUp = (duration.toNanos() % NANOS_PER_MILLISECOND > 0);
        if (shouldRoundUp) {
            return duration.toMillis() + 1;
        }
        return duration.toMillis();
    }

    /**
     * <p>Rounds the quantity of milliseconds from the given duration</p>
     *
     * <p>For the duration of 10 milliseconds 499 999 nanoseconds it should return 10(milliseconds) and the duration of 10 milliseconds and  500 000 nanoseconds it should return 11(milliseconds)</p>
     *
     * @param duration the given duration from which the quantity of milliseconds will be calculated
     * @return the rounded quantity of milliseconds
     * @throws IllegalArgumentException if the given duration is null
     * @since 3.9
     */
    public static long roundMillisecondsQuantity(final Duration duration) {
        if (duration == null) {
            throw new IllegalArgumentException("the duration must not be null");
        }
        boolean shouldRound = ((duration.toNanos() % NANOS_PER_MILLISECOND) >= (NANOS_PER_MILLISECOND / 2));
        if (shouldRound) {
            return duration.toMillis() + 1;
        }
        return duration.toMillis();
    }

}
