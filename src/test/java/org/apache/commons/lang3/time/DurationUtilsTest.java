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
            Duration duration = Duration.ofDays(3)        ;
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

}
