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
        boolean shouldRoundDaysQuantity = ((duration.toNanos() % NANOS_PER_DAY) >= (NANOS_PER_DAY/2));
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
     * @param duration the given duration
     * @return the rounded quantity of hours
     * @throws IllegalArgumentException if the given duration is null
     * @since 3.9
     */
    public static long roundHoursQuantity(final Duration duration) {
        if (duration == null) {
            throw new IllegalArgumentException("the duration must not be null");
        }
        boolean shouldRoundHoursQuantity = ((duration.toNanos() % NANOS_PER_HOUR) >= (NANOS_PER_HOUR/2));
        if (shouldRoundHoursQuantity) {
            return duration.toHours() + 1;
        }
        return duration.toHours();
    }

}
