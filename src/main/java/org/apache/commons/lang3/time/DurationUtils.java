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
import java.time.temporal.ChronoUnit;
import java.time.temporal.Temporal;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

import org.apache.commons.lang3.LongRange;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.function.FailableBiConsumer;
import org.apache.commons.lang3.function.FailableConsumer;
import org.apache.commons.lang3.function.FailableRunnable;
import org.apache.commons.lang3.math.NumberUtils;

/**
 * Utilities for {@link Duration}.
 *
 * @since 3.12.0
 */
public class DurationUtils {

    /**
     * An Integer Range that accepts Longs.
     */
    static final LongRange LONG_TO_INT_RANGE = LongRange.of(NumberUtils.LONG_INT_MIN_VALUE, NumberUtils.LONG_INT_MAX_VALUE);

    /**
     * Accepts the function with the duration as a long milliseconds and int nanoseconds.
     *
     * @param <T> The function exception.
     * @param consumer Accepting function.
     * @param duration The duration to pick apart.
     * @throws T See the function signature.
     */
    @SuppressWarnings("boxing") // boxing unavoidable
    public static <T extends Throwable> void accept(final FailableBiConsumer<Long, Integer, T> consumer, final Duration duration)
            throws T {
        if (consumer != null && duration != null) {
            consumer.accept(duration.toMillis(), getNanosOfMilli(duration));
        }
    }

    /**
     * Gets the nanosecond part of a Duration converted to milliseconds.
     * <p>
     * Handy when calling an API that takes a long of milliseconds and an int of nanoseconds. For example,
     * {@link Object#wait(long, int)} and {@link Thread#sleep(long, int)}.
     * </p>
     * <p>
     * Note that is this different from {@link Duration#getNano()} because a duration are seconds and nanoseconds.
     * </p>
     *
     * @param duration The duration to query.
     * @return nanoseconds between 0 and 999,999.
     * @deprecated Use {@link #getNanosOfMilli(Duration)}.
     */
    @Deprecated
    public static int getNanosOfMiili(final Duration duration) {
        return getNanosOfMilli(duration);
    }

    /**
     * Gets the nanosecond part of a Duration converted to milliseconds.
     * <p>
     * Handy when calling an API that takes a long of milliseconds and an int of nanoseconds. For example,
     * {@link Object#wait(long, int)} and {@link Thread#sleep(long, int)}.
     * </p>
     * <p>
     * Note that is this different from {@link Duration#getNano()} because a duration are seconds and nanoseconds.
     * </p>
     *
     * @param duration The duration to query.
     * @return nanoseconds between 0 and 999,999.
     * @since 3.13.0
     */
    public static int getNanosOfMilli(final Duration duration) {
        return zeroIfNull(duration).getNano() % 1_000_000;
    }

    /**
     * Tests whether the given Duration is positive (&gt;0).
     *
     * @param duration the value to test
     * @return whether the given Duration is positive (&gt;0).
     */
    public static boolean isPositive(final Duration duration) {
        return !duration.isNegative() && !duration.isZero();
    }

    /**
     * Runs the lambda and returns the duration of its execution.
     *
     * @param <E> The type of exception throw by the lambda.
     * @param consumer What to execute.
     * @return The Duration of execution.
     * @throws E thrown by the lambda.
     * @since 3.13.0
     */
    public static <E extends Throwable> Duration of(final FailableConsumer<Instant, E> consumer) throws E {
        return since(now(consumer::accept));
    }

    /**
     * Runs the lambda and returns the duration of its execution.
     *
     * @param <E> The type of exception throw by the lambda.
     * @param runnable What to execute.
     * @return The Duration of execution.
     * @throws E thrown by the lambda.
     * @since 3.13.0
     */
    public static <E extends Throwable> Duration of(final FailableRunnable<E> runnable) throws E {
        return of(start -> runnable.run());
    }

    private static <E extends Throwable> Instant now(final FailableConsumer<Instant, E> nowConsumer) throws E {
        final Instant start = Instant.now();
        nowConsumer.accept(start);
        return start;
    }

    /**
     * Computes the Duration between a start instant and now.
     *
     * @param startInclusive the start instant, inclusive, not null.
     * @return a {@link Duration}, not null.
     * @since 3.13.0
     */
    public static Duration since(final Temporal startInclusive) {
        return Duration.between(startInclusive, Instant.now());
    }

    /**
     * Converts a {@link TimeUnit} to a {@link ChronoUnit}.
     *
     * @param timeUnit A non-null TimeUnit.
     * @return The corresponding ChronoUnit.
     */
    static ChronoUnit toChronoUnit(final TimeUnit timeUnit) {
        // TODO when using Java >= 9: Use TimeUnit.toChronoUnit().
        switch (Objects.requireNonNull(timeUnit)) {
        case NANOSECONDS:
            return ChronoUnit.NANOS;
        case MICROSECONDS:
            return ChronoUnit.MICROS;
        case MILLISECONDS:
            return ChronoUnit.MILLIS;
        case SECONDS:
            return ChronoUnit.SECONDS;
        case MINUTES:
            return ChronoUnit.MINUTES;
        case HOURS:
            return ChronoUnit.HOURS;
        case DAYS:
            return ChronoUnit.DAYS;
        default:
            throw new IllegalArgumentException(timeUnit.toString());
        }
    }

    /**
     * Converts an amount and TimeUnit into a Duration.
     *
     * @param amount   the amount of the duration, measured in terms of the unit, positive or negative
     * @param timeUnit the unit that the duration is measured in, must have an exact duration, not null
     * @return a Duration.
     */
    public static Duration toDuration(final long amount, final TimeUnit timeUnit) {
        return Duration.of(amount, toChronoUnit(timeUnit));
    }

    /**
     * Converts a Duration to milliseconds bound to an int (instead of a long).
     * <p>
     * Handy for low-level APIs that take millisecond timeouts in ints rather than longs.
     * </p>
     * <ul>
     * <li>If the duration milliseconds are greater than {@link Integer#MAX_VALUE}, then return
     * {@link Integer#MAX_VALUE}.</li>
     * <li>If the duration milliseconds are lesser than {@link Integer#MIN_VALUE}, then return
     * {@link Integer#MIN_VALUE}.</li>
     * </ul>
     *
     * @param duration The duration to convert, not null.
     * @return int milliseconds.
     */
    public static int toMillisInt(final Duration duration) {
        Objects.requireNonNull(duration, "duration");
        // intValue() does not do a narrowing conversion here
        return LONG_TO_INT_RANGE.fit(Long.valueOf(duration.toMillis())).intValue();
    }

    /**
     * Returns the given non-null value or {@link Duration#ZERO} if null.
     *
     * @param duration The duration to test.
     * @return The given duration or {@link Duration#ZERO}.
     */
    public static Duration zeroIfNull(final Duration duration) {
        return ObjectUtils.defaultIfNull(duration, Duration.ZERO);
    }

}
