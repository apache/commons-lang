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

}
