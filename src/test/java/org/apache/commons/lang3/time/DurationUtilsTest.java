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
import java.time.temporal.ChronoUnit;

/**
 * TestCase for DurationUtils
 */
public class DurationUtilsTest {

    @Nested
    class DaysRounding {

        @Test
        @DisplayName("2 days 1 millisecond should be rounded up to 3 days")
        void shouldCeilDays() {
            //given
            Duration duration = Duration.of(2, ChronoUnit.DAYS).plusMillis(1);
            //when
            long roundedDays = DurationUtils.ceilDays(duration);
            //then
            Assertions.assertEquals(3, roundedDays);
        }

        @Test
        @DisplayName("2 days should not be rounded up to 3 days")
        void shouldNotCeilDays() {
            //given
            Duration duration = Duration.of(2, ChronoUnit.DAYS);
            //when
            long roundedDays = DurationUtils.ceilDays(duration);
            //then
            Assertions.assertEquals(2, roundedDays);
        }

        @Test
        @DisplayName("2 days 12 hours should be rounded up to 3 days")
        void shouldRoundDays() {
            //given
            Duration duration = Duration.of(2, ChronoUnit.DAYS).plusHours(12);
            //when
            long roundedDays = DurationUtils.roundDays(duration);
            //then
            Assertions.assertEquals(3, roundedDays);
        }

        @Test
        @DisplayName("2 days 11 hours 59 minutes 59 seconds should not be rounded up to 3 days")
        void shouldNotRoundDays() {
            //given
            Duration duration = Duration.of(2, ChronoUnit.DAYS)
                    .plusHours(11)
                    .plusMinutes(59)
                    .plusSeconds(59);
            //when
            long roundedDays = DurationUtils.roundDays(duration);
            //then
            Assertions.assertEquals(2, roundedDays);
        }

    }

    @Nested
    class HoursRounding {

        @Test
        @DisplayName("2 hours 1 millisecond should be rounded up to 3 hours")
        void shouldCeilHours() {
            //given
            Duration duration = Duration.of(2, ChronoUnit.HOURS).plusMillis(1);
            //when
            long roundedHours = DurationUtils.ceilHours(duration);
            //then
            Assertions.assertEquals(3, roundedHours);
        }

        @Test
        @DisplayName("2 hours should not be rounded up to 3 hours")
        void shouldNotCeilHours() {
            //given
            Duration duration = Duration.of(2, ChronoUnit.HOURS);
            //when
            long roundedHours = DurationUtils.ceilHours(duration);
            //then
            Assertions.assertEquals(2, roundedHours);
        }

        @Test
        @DisplayName("2 hours 30 minutes should be rounded  to 3 hours")
        void shouldRoundHours() {
            //given
            Duration duration = Duration.of(2, ChronoUnit.HOURS).plusMinutes(30);
            //when
            long roundedHours = DurationUtils.roundHours(duration);
            //then
            Assertions.assertEquals(3, roundedHours);
        }

        @Test
        @DisplayName("2 hours 29 minutes 59 seconds should be rounded  to 3 hours")
        void shouldNotRoundHours() {
            //given
            Duration duration = Duration.of(2, ChronoUnit.HOURS).plusMinutes(29).plusSeconds(59);
            //when
            long roundedHours = DurationUtils.roundHours(duration);
            //then
            Assertions.assertEquals(2, roundedHours);
        }

    }

    @Nested
    class MinutesRounding {

        @Test
        @DisplayName("10 minutes 1 second should be rounded up to 11 minutes")
        void shouldCeilMinutes() {
            //given
            Duration duration = Duration.of(10, ChronoUnit.MINUTES).plus(1, ChronoUnit.SECONDS);
            //when
            long roundedMinutes = DurationUtils.ceilMinutes(duration);
            //then
            Assertions.assertEquals(11, roundedMinutes);
        }

        @Test
        @DisplayName("10 minutes should not be rounded up to 11 minutes")
        void shouldNotCeilMinutes() {
            //given
            Duration duration = Duration.of(10, ChronoUnit.MINUTES).plus(1, ChronoUnit.SECONDS);
            //when
            long roundedMinutes = DurationUtils.ceilMinutes(duration);
            //then
            Assertions.assertEquals(11, roundedMinutes);
        }

        @Test
        @DisplayName("10 minutes 30 second should be rounded  to 11 minutes")
        void shouldRoundMinutes() {
            //given
            Duration duration = Duration.of(10, ChronoUnit.MINUTES).plus(30, ChronoUnit.SECONDS);
            //when
            long roundedMinutes = DurationUtils.roundMinutes(duration);
            //then
            Assertions.assertEquals(11, roundedMinutes);
        }

        @Test
        @DisplayName("10 minutes 29 second and 999 milliseconds should not be rounded to 11 minutes")
        void shouldNotRoundMinutes() {
            //given
            Duration duration = Duration.of(10, ChronoUnit.MINUTES)
                    .plus(29, ChronoUnit.SECONDS)
                    .plus(999, ChronoUnit.MILLIS);
            //when
            long roundedMinutes = DurationUtils.roundMinutes(duration);
            //then
            Assertions.assertEquals(10, roundedMinutes);
        }
    }

    @Nested
    class SecondsRounding {

        @Test
        @DisplayName("10 seconds 1 millisecond should be rounded up to 11 seconds")
        void shouldCeilSeconds() {
            //given
            Duration duration = Duration.of(10, ChronoUnit.SECONDS).plus(1, ChronoUnit.MILLIS);
            //when
            long roundedSeconds = DurationUtils.ceilSeconds(duration);
            //then
            Assertions.assertEquals(11, roundedSeconds);
        }

        @Test
        @DisplayName("10 seconds should not be rounded up to 11 seconds")
        void shouldNotCeilSeconds() {
            //given
            Duration duration = Duration.of(10, ChronoUnit.SECONDS);
            //when
            long roundedSeconds = DurationUtils.ceilSeconds(duration);
            //then
            Assertions.assertEquals(10, roundedSeconds);
        }

        @Test
        @DisplayName("10 seconds 500 millisecond should be rounded to 11 seconds")
        void shouldRoundSeconds() {
            //given
            Duration duration = Duration.of(10, ChronoUnit.SECONDS).plus(500, ChronoUnit.MILLIS);
            //when
            long roundedSeconds = DurationUtils.ceilSeconds(duration);
            //then
            Assertions.assertEquals(11, roundedSeconds);
        }

        @Test
        @DisplayName("10 seconds 499 millisecond should not be rounded to 11 seconds")
        void shouldNotRoundSeconds() {
            //given
            Duration duration = Duration.of(10, ChronoUnit.SECONDS).plus(499, ChronoUnit.MILLIS);
            //when
            long roundedSeconds = DurationUtils.roundSeconds(duration);
            //then
            Assertions.assertEquals(10, roundedSeconds);
        }

    }

    @Nested
    class MillisecondsRounding {
        @Test
        @DisplayName("10 millisecond 1 nanosecond should be rounded up to 11 milliseconds")
        void shouldCeilMilliseconds() {
            //given
            Duration duration = Duration.of(10, ChronoUnit.MILLIS).plusNanos(1);
            //when
            long roundedMilliseconds = DurationUtils.ceilMilliseconds(duration);
            //then
            Assertions.assertEquals(11, roundedMilliseconds);
        }

        @Test
        @DisplayName("10 milliseconds should not be rounded up to 11 milliseconds")
        void shouldNotCeilMilliSeconds() {
            //given
            Duration duration = Duration.of(10, ChronoUnit.MILLIS);
            //when
            long roundedMilliseconds = DurationUtils.ceilMilliseconds(duration);
            //then
            Assertions.assertEquals(10, roundedMilliseconds);
        }

        @Test
        @DisplayName("10 millisecond 500 000 nanosecond should be rounded up to 11 milliseconds")
        void shouldRoundMilliseconds () {
            //given
            Duration duration = Duration.of(10, ChronoUnit.MILLIS).plusNanos(500_000);
            //when
            long roundedMilliseconds = DurationUtils.roundMilliseconds(duration);
            //then
            Assertions.assertEquals(11, roundedMilliseconds);
        }

        @Test
        @DisplayName("10 milliseconds  499 999 nanoseconds should not be rounded up to 11 milliseconds")
        void shouldNotRoundMilliSeconds() {
            //given
            Duration duration = Duration.of(10, ChronoUnit.MILLIS);
            //when
            long roundedMilliseconds = DurationUtils.roundMilliseconds(duration);
            //then
            Assertions.assertEquals(10, roundedMilliseconds);
        }
    }



}
