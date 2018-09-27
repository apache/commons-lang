package org.apache.commons.lang3.time;

import java.time.Duration;
import java.time.temporal.ChronoUnit;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

/**
 * TestCase for DurationUtils.
 */
public class DurationUtilsTest {

    @DisplayName("the quantity of days of duration 24h 1min should be rounded up to 2 days")
    @Test
    void roundUpDaysShouldUpDuration() {
        //given
        Duration duration = Duration.of(24, ChronoUnit.HOURS).plusMinutes(1);
        //when
        long days = DurationUtils.roundUpDays(duration);
        //then
        Assertions.assertEquals(2, days);
    }

    @DisplayName("the quantity of days of duration 1 day should not be rounded up to 2 days")
    @Test
    void roundUpDaysShouldNotRoundUpDuration() {
        //given
        Duration duration = Duration.of(1, ChronoUnit.DAYS);
        //when
        long days = DurationUtils.roundUpDays(duration);
        //then
        Assertions.assertEquals(1, days);
    }

    @DisplayName("the quantity of days of duration 1 day 12 h should be rounded up to 2 days")
    @Test
    void roundDaysShouldRoundUpDuration() {
        //given
        Duration duration = Duration.of(36, ChronoUnit.HOURS);
        //when
        long days = DurationUtils.roundDays(duration);
        //then
        Assertions.assertEquals(2, days);
    }

    @DisplayName("the quantity of days of duration 1 day 11 hours and 59 minutes should be rounded up to 2 days")
    @Test
    void roundDaysShouldNotRoundUpDuration() {
        //given
        Duration duration = Duration.of(35, ChronoUnit.HOURS).plusMinutes(59);
        //when
        long days = DurationUtils.roundDays(duration);
        //then
        Assertions.assertEquals(1, days);
    }

    @DisplayName("the quantity of hours of duration 1h 10 min should be rounded up to 2 hours")
    @Test
    void roundUpHoursShouldRoundUpDuration() {
        //given
        Duration duration = Duration.of(70, ChronoUnit.MINUTES);
        //when
        long hours = DurationUtils.roundUpHours(duration);
        //then
        Assertions.assertEquals(2, hours);
    }

    @DisplayName("the quantity of hours of duration 1h should not be rounded up")
    @Test
    void roundUpHoursShouldNotRoundUpDuration() {
        //given
        Duration duration = Duration.of(60, ChronoUnit.MINUTES);
        //when
        long hours = DurationUtils.roundUpHours(duration);
        //then
        Assertions.assertEquals(1, hours);
    }

    @DisplayName("the quantity of hours of duration 1h 30 min should be rounded to 2 hours")
    @Test
    void roundHoursShouldRoundUpDuration() {
        //given
        Duration duration = Duration.of(90, ChronoUnit.MINUTES);
        //when
        long hours = DurationUtils.roundHours(duration);
        //then
        Assertions.assertEquals(2, hours);
    }


    @DisplayName("the quantity of hours of duration 1h 10 min should not be rounded")
    @Test
    void roundHoursShouldNotRoundUDuration() {
        //given
        Duration duration = Duration.of(70, ChronoUnit.MINUTES);
        //when
        long hours = DurationUtils.roundHours(duration);
        //then
        Assertions.assertEquals(1, hours);
    }

    @DisplayName("the quantity of minutes from duration 62 seconds should be rounded up to 2 minutes")
    @Test
    void roundUpMinutesShouldRoundUpDuration() {
        //given
        Duration duration = Duration.of(62, ChronoUnit.SECONDS);
        //when
        long minutes = DurationUtils.roundUpMinutes(duration);
        //then
        Assertions.assertEquals(2, minutes);
    }

    @DisplayName("the quantity of minutes from duration 60 seconds should not be rounded up")
    @Test
    void roundUpMinutesShouldNotRoundUpDuration() {
        //given
        Duration duration = Duration.of(60, ChronoUnit.SECONDS);
        //when
        long minutes = DurationUtils.roundUpMinutes(duration);
        //then
        Assertions.assertEquals(1, minutes);
    }

    @DisplayName("the quantity of minutes from duration 90 second should be rounded up to 2")
    @Test
    void roundMinutesShouldRoundUpMinutes() {
        //given
        Duration duration = Duration.of(90, ChronoUnit.SECONDS);
        //when
        long minutes = DurationUtils.roundMinutes(duration);
        //then
        Assertions.assertEquals(2, minutes);
    }

    @DisplayName("the quantity of minutes from duration 89 second should not be rounded up to 2")
    @Test
    void roundMinutesShouldNotRoundUpMinutes() {
        //given
        Duration duration = Duration.of(89, ChronoUnit.SECONDS);
        //when
        long minutes = DurationUtils.roundMinutes(duration);
        //then
        Assertions.assertEquals(1, minutes);
    }

    @DisplayName("the quantity of seconds from duration 1001 millisecond should be rounded up to 2 minutes")
    @Test
    void roundUpSecondShouldRoundUpSeconds() {
        //given
        Duration duration = Duration.of(1001, ChronoUnit.MILLIS);
        //when
        long seconds = DurationUtils.roundUpSeconds(duration);
        //then
        Assertions.assertEquals(2, seconds);
    }

    @DisplayName("the quantity of seconds from duration 1000 millisecond should not be rounded up")
    @Test
    void roundUpSecondShouldNotRoundUpSeconds() {
        //given
        Duration duration = Duration.of(1000, ChronoUnit.MILLIS);
        //when
        long seconds = DurationUtils.roundUpSeconds(duration);
        //then
        Assertions.assertEquals(1, seconds);
    }

    @DisplayName("the quantity of seconds from duration 1500 millisecond should be rounded up to 2 minutes")
    @Test
    void roundSecondsShouldRoundUpSeconds() {
        //given
        Duration duration = Duration.of(1500, ChronoUnit.MILLIS);
        //when
        long seconds = DurationUtils.roundSeconds(duration);
        //then
        Assertions.assertEquals(2, seconds);
    }

    @DisplayName("the quantity of seconds from duration 1499 millisecond should not be rounded up")
    @Test
    void roundSecondsShouldNotRoundUpSeconds() {
        //given
        Duration duration = Duration.of(1499, ChronoUnit.MILLIS);
        //when
        long seconds = DurationUtils.roundSeconds(duration);
        //then
        Assertions.assertEquals(1, seconds);
    }

}
