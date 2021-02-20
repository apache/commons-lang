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
import java.time.temporal.ChronoUnit;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.Range;
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
    static final Range<Long> LONG_TO_INT_RANGE = Range.between(NumberUtils.LONG_INT_MIN_VALUE,
            NumberUtils.LONG_INT_MAX_VALUE);

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
