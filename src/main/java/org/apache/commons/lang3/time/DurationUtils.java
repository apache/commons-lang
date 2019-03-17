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

/**
 * This class contains the utils methods for rounding a duration value in a given unit
 * It supports rounding up (the ceil value) and normal rounding
 * @since 3.9
 */
public class DurationUtils {

    private static final long NANOS_PER_DAY = 86_400_000_000_000L;
    private static final long NANOS_PER_HOUR = 3_600_000_000_000L;
    private static final long NANOS_PER_MINUTE = 60_000_000_000L;
    private static final long NANOS_PER_SECOND = 1_000_000_000L;
    private static final long NANOS_PER_MILLISECOND = 1_000_000L;

    /**
     * <p>DurationUtils instances should NOT be constructed in standard programming.</p>
     *
     * <p>This constructor is public to permit tools that require a JavaBean instance
     * to operate.</p>
     *
     * @since 3.9
     */
    public DurationUtils() {
    }

    /**
     * A method for rounding up a value of days for a given duration
     *
     * @param duration the duration from which days value will be rounded up
     * @return rounded up days value. For instance for  the duration equals 6 days 1 hour this  method
     * returns 7 and for the duration equals exactly 6 days it returns 6
     * @since 3.9
     */
    public static long ceilDays(final Duration duration) {
        if (duration.toNanos() % NANOS_PER_DAY > 0) {
            return duration.toDays() + 1;
        } else {
            return duration.toDays();
        }
    }

    /**
     * A method for rounding a value of days for a given duration
     *
     * @param duration the duration from which days value will be rounded up
     * @return rounded days value. For instance for the duration equals 6 days and 12 hours this method
     * returns 7 and for the duration equals exactly 6 days 11 hours and 59 minutes it returns 6
     * @since 3.9
     */
    public static long roundDays(final Duration duration) {
        if ((duration.toNanos() % NANOS_PER_DAY) >= (NANOS_PER_DAY / 2)) {
            return duration.toDays() + 1;
        } else {
            return duration.toDays();
        }
    }

    /**
     * A method for rounding up a value of hours for a given duration
     *
     * @param duration the duration from which hours value will be rounded up
     * @return rounded up hours value. For instance for the duration equals 6 hours and one millisecond this method
     * returns 7 and for the duration equals exactly 6 hours it returns 6
     * @since 3.9
     */
    public static long ceilHours(final Duration duration) {
        if (duration.toNanos() % NANOS_PER_HOUR > 0) {
            return duration.toHours() + 1;
        } else {
            return duration.toHours();
        }
    }

    /**
     * A method for rounding up a value of hours for a given duration
     *
     * @param duration the duration from which hours value will be rounded
     * @return rounded hours value. For instance for the duration equals 6 hours 30 minutes this method  returns 6
     * and for the duration equals 6 hours 29 minutes and 59 seconds it returns 6
     * @since 3.9
     */
    public static long roundHours(final Duration duration) {
        if ((duration.toNanos() % NANOS_PER_HOUR) >= (NANOS_PER_HOUR / 2)) {
            return duration.toHours() + 1;
        } else {
            return duration.toHours();
        }
    }

    /**
     * A method for rounding up a value of minutes for a given duration
     *
     * @param duration the duration from which minutes value will be rounded up
     * @return rounded up minutes value. For instance for the duration equals 10 minutes 1 millisecond this method returns 11
     * and for duration equals exactly 10 minutes it returns 10.
     *
     * @since 3.9
     */
    public static long ceilMinutes(final Duration duration) {
        if ((duration.toNanos() % NANOS_PER_MINUTE) > 0) {
            return duration.toMinutes() + 1;
        }
        return duration.toMinutes();
    }

    /**
     * A method for rounding a value of minutes for a given duration
     *
     * @param duration the duration from which minutes value will be rounded
     * @return rounded minutes value. For instance for the duration equals 10 minutes and 30 seconds this method returns 11
     * and for the duration equals 10 minutes 29 seconds 999 milliseconds it returns 10.
     * @since 3.9
     */
    public static long roundMinutes(final Duration duration) {
        if ((duration.toNanos() % NANOS_PER_MINUTE) >= (NANOS_PER_MINUTE / 2)) {
            return duration.toMinutes() + 1;
        }
        return duration.toMinutes();
    }


    /**
     * A method for rounding up a value of seconds for a given duration
     *
     * @param duration the duration from which seconds value will be rounded up
     * @return rounded up seconds value. For instance for the duration equals 10 seconds and 1 millisecond this method
     * returns 11 and for the duration equals exactly 10 seconds it returns 10
     * @since 3.9
     */
    public static long ceilSeconds(final Duration duration) {
        if ((duration.toNanos() % NANOS_PER_SECOND) > 0) {
            return duration.get(ChronoUnit.SECONDS) + 1;
        }
        return duration.get(ChronoUnit.SECONDS);
    }

    /**
     * A method for rounding up a value of seconds for a given duration
     *
     * @param duration the duration from which seconds value will be rounded up
     * @return rounded seconds value. For instance for the duration equals 10 seconds and 500 millisecond this method
     * returns 11 and for the duration equals 10 second and 499 millisecond it returns 10.
     * @since 3.9
     */
    public static long roundSeconds(final Duration duration) {
        if ((duration.toNanos() % NANOS_PER_SECOND) >= (NANOS_PER_SECOND / 2)) {
            return duration.get(ChronoUnit.SECONDS) + 1;
        }
        return duration.get(ChronoUnit.SECONDS);
    }


    /**
     * A method for rounding up a value of milliseconds for a given duration
     *
     * @param duration the duration from which milliseconds value will be rounded up
     * @return rounded up milliseconds value. For instance for the duration equals 10 millisecond and 1 nanosecond this method
     * returns 11 and for the duration equals exactly 10 millisecond it returns 10
     * @since 3.9
     */
    public static long ceilMilliseconds(Duration duration) {
        if ((duration.toNanos() % NANOS_PER_MILLISECOND) > 0) {
            return duration.toMillis() + 1;
        }
        return duration.toMillis();
    }


    /**
     * A method for rounding a value of milliseconds for a given duration
     *
     * @param duration the duration from which milliseconds value will be rounded up
     * @return rounded  milliseconds value. For instance for the duration equals 10 millisecond and 500 000 nanosecond this method
     * returns 11 and for the duration equals 10 millisecond and 499 999 nanoseconds it returns 10
     * @since 3.9
     */
    public static long roundMilliseconds(Duration duration) {
        if ((duration.toNanos() % NANOS_PER_MILLISECOND) >= (NANOS_PER_MILLISECOND / 2)) {
            return duration.toMillis() + 1;
        }
        return duration.toMillis();
    }

}
