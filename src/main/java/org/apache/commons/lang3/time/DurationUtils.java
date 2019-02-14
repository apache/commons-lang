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
 *
 * @since 3.9
 */
public class DurationUtils {

    private static final long NANOS_PER_DAY = 86_400_000_000_000L;
    private static final long NANOS_PER_HOUR = 3_600_000_000_000L;
    private static final long NANOS_PER_MINUTE = 60_000_000_000L;
    private static final long NANOS_PER_SECOND = 1_000_000_000L;

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
     * @return rounded days value
     * @since 3.9
     */
    public static long roundUpDays(final Duration duration) {
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
     * @return rounded days value
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
     * @return rounded hours value
     * @since 3.9
     */
    public static long roundUpHours(final Duration duration) {
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
     * @return rounded hours value
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
     * @return rounded minutes value
     * @since 3.9
     */
    public static long roundUpMinutes(final Duration duration) {
        if ((duration.toNanos() % NANOS_PER_MINUTE) > 0) {
            return duration.toMinutes() + 1;
        }
        return duration.toMinutes();
    }

    /**
     * A method for rounding a value of minutes for a given duration
     *
     * @param duration the duration from which minutes value will be rounded
     * @return rounded minutes value
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
     * @return rounded seconds value
     * @since 3.9
     */
    public static long roundUpSeconds(final Duration duration) {
        if ((duration.toNanos() % NANOS_PER_SECOND) > 0) {
            return duration.get(ChronoUnit.SECONDS) + 1;
        }
        return duration.get(ChronoUnit.SECONDS);
    }

    /**
     * A method for rounding up a value of seconds for a given duration
     *
     * @param duration the duration from which seconds value will be rounded up
     * @return rounded seconds value
     * @since 3.9
     */
    public static long roundSeconds(final Duration duration) {
        if ((duration.toNanos() % NANOS_PER_SECOND) >= (NANOS_PER_SECOND / 2)) {
            return duration.get(ChronoUnit.SECONDS) + 1;
        }
        return duration.get(ChronoUnit.SECONDS);
    }
}
