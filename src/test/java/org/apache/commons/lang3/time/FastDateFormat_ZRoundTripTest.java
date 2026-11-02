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
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;

import java.text.ParsePosition;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junitpioneer.jupiter.ReadsDefaultLocale;
import org.junitpioneer.jupiter.ReadsDefaultTimeZone;

/**
 * Tests that {@link FastDateFormat} can parse what its own 'Z' patterns format: FastDatePrinter emits the
 * ISO 8601 extended offset form "+hh:mm" for a pattern with two, or three or more, 'Z' letters, so the
 * parser side must accept that form.
 */
class FastDateFormat_ZRoundTripTest extends AbstractLangTest {

    private static final TimeZone NEW_YORK = TimeZones.getTimeZone("America/New_York");

    private static Date parse(final DateParser parser, final String text) {
        final ParsePosition parsePosition = new ParsePosition(0);
        final Date date = parser.parse(text, parsePosition);
        assertNotNull(date, () -> parser.getPattern() + " failed to parse '" + text + "' (error index " + parsePosition.getErrorIndex() + ")");
        assertEquals(text.length(), parsePosition.getIndex(), parser.getPattern());
        return date;
    }

    private static Date parse(final String pattern, final String text) {
        return parse(FastDateFormat.getInstance(pattern, NEW_YORK, Locale.US), text);
    }

    @Test
    @ReadsDefaultLocale
    @ReadsDefaultTimeZone
    void testClearExtendedTimeZoneCache() {
        final Object strategy = FastDateParser.TimeZoneStrategy.getIsoExtendedStrategy(Locale.US);
        assertSame(strategy, FastDateParser.TimeZoneStrategy.getIsoExtendedStrategy(Locale.US));
        final FastDateParser parser = new FastDateParser("yyyy-MM-ddZZZ", NEW_YORK, Locale.US);
        final String text = "2023-10-27+05:30";
        final Date expected = parse("yyyy-MM-ddZZ", text);
        FastDateParser.clear();
        assertNotSame(strategy, FastDateParser.TimeZoneStrategy.getIsoExtendedStrategy(Locale.US));
        assertEquals(expected, parse(parser, text));
        assertEquals(expected, parse(new FastDateParser("yyyy-MM-ddZZZ", NEW_YORK, Locale.US), text));
    }

    @Test
    @ReadsDefaultLocale
    @ReadsDefaultTimeZone
    void testParseExplicitColonOffset() {
        // Same instant as the known-good 'ZZ' parsing of the identical text.
        assertEquals(parse("yyyy-MM-ddZZ", "2023-10-27-05:30"), parse("yyyy-MM-ddZZZ", "2023-10-27-05:30"));
    }

    @ParameterizedTest
    @CsvSource({"+0530, +05:30", "-0330, -03:30", "GMT+05:30, +05:30", "PST, -08:00", "Pacific Standard Time, -08:00", "UTC, +00:00"})
    @ReadsDefaultLocale
    @ReadsDefaultTimeZone
    void testParseLegacyTimeZones(final String zone, final String offset) {
        assertEquals(parse("yyyy-MM-ddZZ", "2023-10-27" + offset), parse("yyyy-MM-ddZZZ", "2023-10-27" + zone));
    }

    @ParameterizedTest
    @CsvSource({"UTC, +00:00", "GMT+05:30, +05:30", "GMT-03:30, -03:30", "GMT+12:45, +12:45"})
    @ReadsDefaultLocale
    @ReadsDefaultTimeZone
    void testRoundTripOffsets(final String zoneId, final String offset) {
        final Date date = new Date(1698446400123L);
        final TimeZone zone = TimeZones.getTimeZone(zoneId);
        for (int zCount = 2; zCount <= 8; zCount++) {
            final String pattern = "yyyy-MM-dd HH:mm:ss.SSS" + "ZZZZZZZZ".substring(0, zCount);
            final FastDateFormat format = FastDateFormat.getInstance(pattern, zone, Locale.US);
            final String text = format.format(date);
            final String expectedOffset = zCount == 2 && "UTC".equals(zoneId) ? "Z" : offset;
            final String localDateTime = FastDateFormat.getInstance("yyyy-MM-dd HH:mm:ss.SSS", zone, Locale.US).format(date);
            assertEquals(localDateTime + expectedOffset, text, pattern);
            assertEquals(date, parse(format, text), pattern);
        }
    }

    @Test
    @ReadsDefaultLocale
    @ReadsDefaultTimeZone
    void testRoundTripOneAndTwoZ() {
        final Date date = new Date(1698446400000L);
        assertEquals("2023-10-27-0400", FastDateFormat.getInstance("yyyy-MM-ddZ", NEW_YORK, Locale.US).format(date));
        assertEquals("2023-10-27-04:00", FastDateFormat.getInstance("yyyy-MM-ddZZ", NEW_YORK, Locale.US).format(date));
        assertEquals(parse("yyyy-MM-ddZZ", "2023-10-27-04:00"), parse("yyyy-MM-ddZ", "2023-10-27-0400"));
    }

    @Test
    @ReadsDefaultLocale
    @ReadsDefaultTimeZone
    void testRoundTripThreeOrMoreZ() {
        for (int zCount = 3; zCount <= 6; zCount++) {
            final String pattern = "yyyy-MM-dd" + "ZZZZZZZZ".substring(0, zCount);
            final FastDateFormat format = FastDateFormat.getInstance(pattern, NEW_YORK, Locale.US);
            final String text = format.format(new Date(1698446400000L));
            assertEquals("2023-10-27-04:00", text, pattern);
            assertEquals(parse("yyyy-MM-ddZZ", "2023-10-27-04:00"), parse(format, text), pattern);
        }
    }
}
