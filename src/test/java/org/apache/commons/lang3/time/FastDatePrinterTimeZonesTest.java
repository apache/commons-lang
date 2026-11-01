/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.commons.lang3.time;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.text.SimpleDateFormat;
import java.time.Instant;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;
import java.util.stream.Stream;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Tests date formatting in every available time zone using fixed dates and {@link Locale#US}.
 *
 * <p>
 * The old test used the current time. It began failing on November 1, 2026 because newer JDKs and {@link FastDatePrinter} chose different names for some
 * Canadian zones, even though the clock times and UTC offsets matched. {@link SimpleDateFormat} can use CLDR naming rules to choose a name such as {@code MDT},
 * while the printer uses the calendar's DST flag and can choose {@code MST} for the same instant.
 * </p>
 *
 * <p>
 * We compare the complete output with {@link SimpleDateFormat} for fixed January and July dates. Around DST changes, we compare numeric offsets with
 * {@link SimpleDateFormat} and names with {@link TimeZone}. The dates cover New York's skipped and repeated hours and the November 1 failure.
 * </p>
 *
 * <p>
 * Keep the dates and locale explicit, and set each calendar's time before formatting. Do not use the current time or remove transition cases to make the test
 * pass. Comparing {@link SimpleDateFormat}'s zone names for every case would bring back the failure. These tests check the printer's existing naming behavior;
 * they do not change it to match newer JDKs.
 * </p>
 */
class FastDatePrinterTimeZonesTest extends AbstractLangTest {

    private static final Locale LOCALE = Locale.US;

    /**
     * Gets the time zones and fixed dates for offset and name checks, including both occurrences of New York's repeated hour.
     *
     * @return time-zone IDs paired with fixed instants.
     */
    static Stream<Arguments> getTimeZonesAndInstants() {
        // @formatter:off
        return Stream.concat(getTimeZonesAndSeasonalInstants(), timeZonesAndInstants(
                // Immediately before and after the spring gap in New York.
                "2026-03-08T06:59:59Z",
                "2026-03-08T07:00:00Z",
                // Both occurrences of 1:30 AM during the fall overlap in New York.
                "2026-11-01T05:30:00Z",
                "2026-11-01T06:30:00Z",
                // Exercises the Canadian zone-name differences after the 2026 transition.
                "2026-11-01T12:00:00Z"));
        // @formatter:on
    }

    /**
     * Gets the January and July dates used for complete {@link SimpleDateFormat} comparisons. Keep these separate from the transition cases where the
     * formatters can choose different zone names.
     *
     * @return time-zone IDs paired with the fixed seasonal instants.
     */
    static Stream<Arguments> getTimeZonesAndSeasonalInstants() {
        return timeZonesAndInstants("2026-01-15T12:00:00Z", "2026-07-15T12:00:00Z");
    }

    private static Stream<Arguments> timeZonesAndInstants(final String... instants) {
        return Stream.of(TimeZones.SORTED_AVAILABLE_IDS).flatMap(id -> Stream.of(instants).map(instant -> Arguments.of(id, Instant.parse(instant))));
    }

    /**
     * Checks the clock time and the name chosen by {@link TimeZone}.
     *
     * <p>
     * Keep the zone name out of the {@link SimpleDateFormat} pattern here. Its name can differ from the printer's even when the time is correct.
     * </p>
     *
     * @param timeZoneId the time-zone ID to test.
     * @param instant    the fixed instant to format.
     */
    @ParameterizedTest
    @MethodSource("getTimeZonesAndInstants")
    void testCalendarTimezoneDisplayName(final String timeZoneId, final Instant instant) {
        final TimeZone timeZone = TimeZones.getTimeZone(timeZoneId);
        final Calendar cal = Calendar.getInstance(timeZone, LOCALE);
        cal.setTimeInMillis(instant.toEpochMilli());
        final Date date = cal.getTime();
        final SimpleDateFormat sdf = new SimpleDateFormat("h:mma", LOCALE);
        sdf.setTimeZone(timeZone);
        final String expectedValue = sdf.format(date) + " " + timeZone.getDisplayName(timeZone.inDaylightTime(date), TimeZone.SHORT, LOCALE);
        final String actualValue = FastDateFormat.getInstance("h:mma z", timeZone, LOCALE).format(cal);
        assertEquals(expectedValue, actualValue);
    }

    /**
     * Checks the clock time and numeric UTC offset against {@link SimpleDateFormat}.
     *
     * <p>
     * Keep the uppercase {@code Z} pattern. Lowercase {@code z} compares names and would bring back the naming mismatch.
     * </p>
     *
     * @param timeZoneId the time-zone ID to test.
     * @param instant    the fixed instant to format.
     */
    @ParameterizedTest
    @MethodSource("getTimeZonesAndInstants")
    void testCalendarTimezoneOffset(final String timeZoneId, final Instant instant) {
        final TimeZone timeZone = TimeZones.getTimeZone(timeZoneId);
        final Calendar cal = Calendar.getInstance(timeZone, LOCALE);
        cal.setTimeInMillis(instant.toEpochMilli());
        final String pattern = "h:mma Z";
        final SimpleDateFormat sdf = new SimpleDateFormat(pattern, LOCALE);
        sdf.setTimeZone(timeZone);
        assertEquals(sdf.format(cal.getTime()), FastDateFormat.getInstance(pattern, timeZone, LOCALE).format(cal));
    }

    /**
     * Keeps the original comparison of complete formatted output, including zone names, for the fixed January and July dates.
     *
     * @param timeZoneId the time-zone ID to test.
     * @param instant    the fixed seasonal instant to format.
     */
    @ParameterizedTest
    @MethodSource("getTimeZonesAndSeasonalInstants")
    void testCalendarTimezoneRespected(final String timeZoneId, final Instant instant) {
        final TimeZone timeZone = TimeZones.getTimeZone(timeZoneId);
        final Calendar cal = Calendar.getInstance(timeZone, LOCALE);
        cal.setTimeInMillis(instant.toEpochMilli());
        final String pattern = "h:mma z";
        final SimpleDateFormat sdf = new SimpleDateFormat(pattern, LOCALE);
        sdf.setTimeZone(timeZone);
        assertEquals(sdf.format(cal.getTime()), FastDateFormat.getInstance(pattern, timeZone, LOCALE).format(cal));
    }
}
