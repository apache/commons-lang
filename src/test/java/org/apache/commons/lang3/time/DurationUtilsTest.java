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

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.time.Duration;

public class DurationUtilsTest {

    @Nested
    class DaysQuantityRoundingTest {

        @DisplayName("roundUpDaysQuantity should throw the IllegalArgumentException for the null duration")
        @Test
        void roundUpDaysQuantityShouldThrowIllegalArgumentExceptionForNullDuration() {
            Assertions.assertThrows(IllegalArgumentException.class, () -> DurationUtils.roundUpDaysQuantity(null));
        }

        @DisplayName("the quantity of days from the Duration 3 days and 1 second should be rounded up")
        @Test
        void durationOf3DaysAnd1SecondShouldBeRoundedUp() {
            Duration duration = Duration.ofDays(3).plusSeconds(1);
            long daysQuantity = DurationUtils.roundUpDaysQuantity(duration);
            Assertions.assertEquals(4, daysQuantity);
        }

        @DisplayName("the quantity of days from the Duration 3 days should not be rounded up")
        @Test
        void durationOfExactly3DaysShouldNotBeRoundedUp() {
            Duration duration = Duration.ofDays(3);
            long daysQuantity = DurationUtils.roundUpDaysQuantity(duration);
            Assertions.assertEquals(3, daysQuantity);
        }

        @DisplayName("roundDaysQuantity should throw the IllegalArgumentException for the null duration")
        @Test
        void roundDaysQuantityShouldThrowIllegalArgumentExceptionForNullDuration() {
            Assertions.assertThrows(IllegalArgumentException.class, () -> DurationUtils.roundDaysQuantity(null));
        }

        @DisplayName("the quantity of days from the Duration 3 days 12 hours should be rounded")
        @Test
        void durationOf3AndHalfDaysShouldBeRounded() {
            Duration duration = Duration.ofDays(3).plusHours(12);
            long daysQuantity = DurationUtils.roundDaysQuantity(duration);
            Assertions.assertEquals(4, daysQuantity);
        }

        @DisplayName("the quantity of days from the Duration 3 days 11 hours 59 minutes should not be rounded")
        @Test
        void durationOf3Days11Hours59MinutesShouldNotBeRounded() {
            Duration duration = Duration.ofDays(3).plusHours(11).plusMinutes(59);
            long daysQuantity = DurationUtils.roundDaysQuantity(duration);
            Assertions.assertEquals(3, daysQuantity);
        }

    }

    @Nested
    class HoursQuantityRoundingTest {

        @DisplayName("roundUpHoursQuantity should throw the IllegalArgumentException for the null duration")
        @Test
        void roundUpHoursQuantityShouldThrowIllegalArgumentExceptionForNullDuration() {
            Assertions.assertThrows(IllegalArgumentException.class, () -> DurationUtils.roundUpHoursQuantity(null));
        }

        @DisplayName("the quantity of hours from the Duration 2 hours and 1 second should be rounded up")
        @Test
        void durationOf2HoursAnd1SecondShouldBeRoundedUp() {
            Duration duration = Duration.ofHours(2).plusSeconds(1);
            long hoursQuantity = DurationUtils.roundUpHoursQuantity(duration);
            Assertions.assertEquals(3, hoursQuantity);
        }

        @DisplayName("the quantity of hours from the Duration 3 hours should not be rounded up")
        @Test
        void durationOfExactly3HoursShouldNotBeRoundedUp() {
            Duration duration = Duration.ofHours(3);
            long hoursQuantity = DurationUtils.roundUpHoursQuantity(duration);
            Assertions.assertEquals(3, hoursQuantity);
        }

        @DisplayName("roundUpHoursQuantity should throw the IllegalArgumentException for the null duration")
        @Test
        void roundHoursQuantityShouldThrowIllegalArgumentExceptionForNullDuration() {
            Assertions.assertThrows(IllegalArgumentException.class, () -> DurationUtils.roundHoursQuantity(null));
        }

        @DisplayName("the quantity of hours from the Duration 2 hours 30 minutes should be rounded")
        @Test
        void durationOf3AndHalfDaysShouldBeRounded() {
            Duration duration = Duration.ofHours(2).plusMinutes(30);
            long hoursQuantity = DurationUtils.roundHoursQuantity(duration);
            Assertions.assertEquals(3, hoursQuantity);
        }

        @DisplayName("the quantity of hours from the Duration 3 hours 29 minutes  59 seconds should not be rounded")
        @Test
        void durationOf3Days11Hours59MinutesShouldNotBeRounded() {
            Duration duration = Duration.ofHours(3).plusMinutes(29).plusSeconds(59);
            long hoursQuantity = DurationUtils.roundHoursQuantity(duration);
            Assertions.assertEquals(3, hoursQuantity);
        }
    }

    @Nested
    class MinutesQuantityRoundingTest {

        @DisplayName("roundUpMinutesQuantity should throw the IllegalArgumentException for the null duration")
        @Test
        void roundUpMinutesQuantityShouldThrowIllegalArgumentExceptionForNullDuration() {
            Assertions.assertThrows(IllegalArgumentException.class, () -> DurationUtils.roundUpMinutesQuantity(null));
        }

        @DisplayName("the quantity of minutes from the Duration 10 minutes and 1 second should be rounded up")
        @Test
        void durationOf10MinutesAnd1SecondShouldBeRoundedUp() {
            Duration duration = Duration.ofMinutes(10).plusSeconds(1);
            long minutesQuantity = DurationUtils.roundUpMinutesQuantity(duration);
            Assertions.assertEquals(11, minutesQuantity);
        }

        @DisplayName("the quantity of minutes from the Duration 10 minutes should not be rounded up")
        @Test
        void durationOfExactly10MinutesShouldNotBeRoundedUp() {
            Duration duration = Duration.ofMinutes(10);
            long minutesQuantity = DurationUtils.roundUpMinutesQuantity(duration);
            Assertions.assertEquals(10, minutesQuantity);
        }

        @DisplayName("roundMinutesQuantity should throw the IllegalArgumentException for the null duration")
        @Test
        void roundMinutesQuantityShouldThrowIllegalArgumentExceptionForNullDuration() {
            Assertions.assertThrows(IllegalArgumentException.class, () -> DurationUtils.roundMinutesQuantity(null));
        }

        @DisplayName("the quantity of minutes from the Duration 10 minutes and 30 second should be rounded")
        @Test
        void durationOf10MinutesAnd30SecondShouldBeRoundedUp() {
            Duration duration = Duration.ofMinutes(10).plusSeconds(30);
            long minutesQuantity = DurationUtils.roundMinutesQuantity(duration);
            Assertions.assertEquals(11, minutesQuantity);
        }

        @DisplayName("the quantity of minutes from the Duration 10 minutes and 29 seconds should not be rounded")
        @Test
        void durationOfExactly10MinutesAnd29SecondsShouldNotBeRounded() {
            Duration duration = Duration.ofMinutes(10).plusSeconds(29);
            long minutesQuantity = DurationUtils.roundMinutesQuantity(duration);
            Assertions.assertEquals(10, minutesQuantity);
        }

    }

    @Nested
    class SecondsQuantityRoundingTest {

        @DisplayName("roundUpSecondsQuantity should throw the IllegalArgumentException for the null duration")
        @Test
        void roundUpMinutesQuantityShouldThrowIllegalArgumentExceptionForNullDuration() {
            Assertions.assertThrows(IllegalArgumentException.class, () -> DurationUtils.roundUpSecondsQuantity(null));
        }

        @DisplayName("the quantity of seconds from the Duration 10 seconds and 1 millisecond should be rounded up")
        @Test
        void durationOf10SecondsAnd1MillisecondShouldBeRoundedUp() {
            Duration duration = Duration.ofSeconds(10).plusMillis(1);
            long secondsQuantity = DurationUtils.roundUpSecondsQuantity(duration);
            Assertions.assertEquals(11, secondsQuantity);
        }

        @DisplayName("the quantity of seconds from the Duration 10 minutes should not be rounded up")
        @Test
        void durationOfExactly10SecondsShouldNotBeRoundedUp() {
            Duration duration = Duration.ofSeconds(10);
            long secondsQuantity = DurationUtils.roundUpSecondsQuantity(duration);
            Assertions.assertEquals(10, secondsQuantity);
        }

        @DisplayName("roundSecondsQuantity should throw the IllegalArgumentException for the null duration")
        @Test
        void roundSecondsQuantityShouldThrowIllegalArgumentExceptionForNullDuration() {
            Assertions.assertThrows(IllegalArgumentException.class, () -> DurationUtils.roundSecondsQuantity(null));
        }

        @DisplayName("the quantity of seconds from the Duration 10 seconds and 500 milliseconds should be rounded")
        @Test
        void durationOf10SecondsAnd500MillisecondShouldBeRounded() {
            Duration duration = Duration.ofSeconds(10).plusMillis(500);
            long secondsQuantity = DurationUtils.roundSecondsQuantity(duration);
            Assertions.assertEquals(11, secondsQuantity);
        }

        @DisplayName("the quantity of seconds from the Duration 10 seconds and 499 milliseconds should not be rounded")
        @Test
        void durationOfExactly10MinutesAnd29SecondsShouldNotBeRounded() {
            Duration duration = Duration.ofSeconds(10).plusMillis(499);
            long secondsQuantity = DurationUtils.roundSecondsQuantity(duration);
            Assertions.assertEquals(10, secondsQuantity);
        }

    }

    @Nested
    class MillisecondsQuantityRoundingTest {

        @DisplayName("roundUpMillisecondsQuantity should throw the IllegalArgumentException for the null duration")
        @Test
        void roundUpMillisecondsQuantityShouldThrowIllegalArgumentExceptionForNullDuration() {
            Assertions.assertThrows(IllegalArgumentException.class, () -> DurationUtils.roundUpMillisecondsQuantity(null));
        }

        @DisplayName("the quantity of milliseconds from the Duration 10 milliseconds and 1 nanoseconds should be rounded up")
        @Test
        void durationOf10SecondsAnd1MillisecondShouldBeRoundedUp() {
            Duration duration = Duration.ofMillis(10).plusNanos(1);
            long millisecondsQuantity = DurationUtils.roundUpMillisecondsQuantity(duration);
            Assertions.assertEquals(11, millisecondsQuantity);
        }

        @DisplayName("the quantity of milliseconds from the Duration 10 milliseconds should not be rounded up")
        @Test
        void durationOfExactly10SecondsShouldNotBeRoundedUp() {
            Duration duration = Duration.ofMillis(10);
            long millisecondsQuantity = DurationUtils.roundUpMillisecondsQuantity(duration);
            Assertions.assertEquals(10, millisecondsQuantity);
        }

        @DisplayName("roundMillisecondsQuantity should throw the IllegalArgumentException for the null duration")
        @Test
        void roundSecondsQuantityShouldThrowIllegalArgumentExceptionForNullDuration() {
            Assertions.assertThrows(IllegalArgumentException.class, () -> DurationUtils.roundMillisecondsQuantity(null));
        }

        @DisplayName("the quantity of milliseconds from the Duration 10 milliseconds and 500 000 nanoseconds should be rounded")
        @Test
        void durationOf10SecondsAnd500MillisecondShouldBeRounded() {
            Duration duration = Duration.ofMillis(10).plusNanos(500_000);
            long millisecondsQuantity = DurationUtils.roundMillisecondsQuantity(duration);
            Assertions.assertEquals(11, millisecondsQuantity);
        }

        @DisplayName("the quantity of seconds from the Duration 10 seconds and 499 999 nanoseconds should not be rounded")
        @Test
        void durationOfExactly10MinutesAnd29SecondsShouldNotBeRounded() {
            Duration duration = Duration.ofMillis(10).plusNanos(499_999);
            long millisecondsQuantity = DurationUtils.roundMillisecondsQuantity(duration);
            Assertions.assertEquals(10, millisecondsQuantity);
        }
    }

}
