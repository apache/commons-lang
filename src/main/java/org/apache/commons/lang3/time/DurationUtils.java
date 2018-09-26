package org.apache.commons.lang3.time;

import java.time.Duration;

/**
 * <p>A suite of utilities surrounding the use of the
 * {@link java.time.Duration} object</p>
 *
 * <p> DurationUtils contains a lot common methods to rounding value of Duration</p>
 *
 * @since 3.9
 */
public class DurationUtils {

    private static final long NANOS_IN_HOUR = 3_600_000_000_000L;
    private static final long NANOS_IN_MINUTE = 60_000_000_000L;

    /**
     * <p>DurationUtils instances should NOT be constructed in standard programming.</p>
     * <p>This constructor is public to permit tools that require a JavaBean instance
     * to operate.</p>
     */
    public DurationUtils() {
    }

    /**
     * @param duration the given duration from which the quantity of hours should be rounded up
     * @return rounded up hours value
     */
    public static long roundUpHours(final Duration duration) {
        boolean shouldRoundUpHours = (duration.toNanos() % NANOS_IN_HOUR != 0);
        if (shouldRoundUpHours) {
            return duration.toHours() + 1;
        }
        return duration.toHours();
    }

    /**
     * @param duration the given duration from which the quantity of hours should be rounded
     * @return rounded hours value
     */
    public static long roundHours(final Duration duration) {
        boolean shouldRoundHours = ((duration.toNanos() % NANOS_IN_HOUR) >= NANOS_IN_HOUR / 2);
        if (shouldRoundHours) {
            return duration.toHours() + 1;
        }
        return duration.toHours();
    }

    /**
     * @param duration the given duration from which the quantity of minutes should be rounded up
     * @return rounded minutes value
     */
    public static long roundUpMinutes(final Duration duration) {
        boolean shouldRoundUpMinutes = (duration.toNanos() % NANOS_IN_MINUTE != 0);
        if (shouldRoundUpMinutes) {
            return duration.toMinutes() + 1;
        }
        return duration.toMinutes();
    }

    /**
     * @param duration the given duration from which the quantity of minutes should be rounded
     * @return rounded minutes value
     */
    public static long roundMinutes(final Duration duration) {
        boolean shouldRoundMinutes = ((duration.toNanos() % NANOS_IN_MINUTE) >= NANOS_IN_MINUTE / 2);
        if (shouldRoundMinutes) {
            return duration.toMinutes() + 1;
        }
        return duration.toMinutes();
    }
}
