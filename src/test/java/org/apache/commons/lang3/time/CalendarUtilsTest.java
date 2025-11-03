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

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.Locale;
import java.util.TimeZone;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

class CalendarUtilsTest extends AbstractLangTest {

    @Test
    void testGetDayOfMonth() {
        assertEquals(Calendar.getInstance().get(Calendar.DAY_OF_MONTH), CalendarUtils.getInstance().getDayOfMonth());
    }

    @Test
    void testGetDayOfYear() {
        assertEquals(Calendar.getInstance().get(Calendar.DAY_OF_YEAR), CalendarUtils.getInstance().getDayOfYear());
    }

    @Test
    void testGetMonth() {
        assertEquals(Calendar.getInstance().get(Calendar.MONTH), CalendarUtils.getInstance().getMonth());
    }

    @Test
    void testGetStandaloneLongMonthNames() {
        final String[] monthNames = CalendarUtils.getInstance(Locale.GERMAN).getStandaloneLongMonthNames();
        assertEquals(12, monthNames.length);
        assertEquals("Januar", monthNames[0]);
        assertEquals("Februar", monthNames[1]);
        assertEquals("M\u00e4rz", monthNames[2]);
        assertEquals("April", monthNames[3]);
        assertEquals("Mai", monthNames[4]);
        assertEquals("Juni", monthNames[5]);
        assertEquals("Juli", monthNames[6]);
        assertEquals("August", monthNames[7]);
        assertEquals("September", monthNames[8]);
        assertEquals("Oktober", monthNames[9]);
        assertEquals("November", monthNames[10]);
        assertEquals("Dezember", monthNames[11]);
    }

    @Test
    void testGetStandaloneShortMonthNames() {
        final String[] monthNames = CalendarUtils.getInstance(Locale.GERMAN).getStandaloneShortMonthNames();
        assertEquals(12, monthNames.length);
        assertEquals("Jan", monthNames[0]);
        assertEquals("Feb", monthNames[1]);
        assertEquals("M\u00e4r", monthNames[2]);
        assertEquals("Apr", monthNames[3]);
        assertEquals("Mai", monthNames[4]);
        assertEquals("Jun", monthNames[5]);
        assertEquals("Jul", monthNames[6]);
        assertEquals("Aug", monthNames[7]);
        assertEquals("Sep", monthNames[8]);
        assertEquals("Okt", monthNames[9]);
        assertEquals("Nov", monthNames[10]);
        assertEquals("Dez", monthNames[11]);
    }

    @Test
    void testGetYear() {
        assertEquals(Calendar.getInstance().get(Calendar.YEAR), CalendarUtils.INSTANCE.getYear());
    }

    /**
     * Tests {@link CalendarUtils#toLocalDate()} from https://github.com/apache/commons-lang/pull/725.
     */
    @Test
    void testToLocalDate() {
        final Calendar calendar = new GregorianCalendar(TimeZones.getTimeZone(TimeZones.GMT_ID));
        calendar.setTimeInMillis(-27078001200000L);
        assertEquals("1111-12-08T05:00:00Z", calendar.toInstant().toString());
        assertEquals(LocalDate.of(1111, Month.DECEMBER, 8), new CalendarUtils(calendar).toLocalDate());
        calendar.setTimeInMillis(1614700215000L);
        assertEquals(LocalDate.of(2021, Month.MARCH, 2), new CalendarUtils(calendar).toLocalDate());
    }

    @ParameterizedTest
    @MethodSource(TimeZonesTest.TIME_ZONE_GET_AVAILABLE_IDS)
    void testToLocalDateTime(final String timeZoneId) {
        final TimeZone timeZone = TimeZones.getTimeZone(timeZoneId);
        final ZoneId zoneId = timeZone.toZoneId();
        final Calendar calendar = new GregorianCalendar(timeZone);
        calendar.setTimeInMillis(0);
        assertEquals(LocalDateTime.ofInstant(calendar.toInstant(), calendar.getTimeZone().toZoneId()), new CalendarUtils(calendar).toLocalDateTime());
        final ZonedDateTime zdt1 = ZonedDateTime.of(1, 2, 3, 4, 5, 6, 0, zoneId);
        calendar.setTimeInMillis(zdt1.toInstant().toEpochMilli());
        assertEquals(LocalDateTime.ofInstant(zdt1.toInstant(), calendar.getTimeZone().toZoneId()), new CalendarUtils(calendar).toLocalDateTime());
    }

    @ParameterizedTest
    @MethodSource(TimeZonesTest.TIME_ZONE_GET_AVAILABLE_IDS)
    void testToOffsetDateTime(final String timeZoneId) {
        final TimeZone timeZone = TimeZones.getTimeZone(timeZoneId);
        final ZoneId zoneId = timeZone.toZoneId();
        final Calendar calendar = new GregorianCalendar(timeZone);
        calendar.setTimeInMillis(0);
        assertEquals(OffsetDateTime.ofInstant(calendar.toInstant(), calendar.getTimeZone().toZoneId()), new CalendarUtils(calendar).toOffsetDateTime());
        final ZonedDateTime zdt1 = ZonedDateTime.of(1, 2, 3, 4, 5, 6, 0, zoneId);
        calendar.setTimeInMillis(zdt1.toInstant().toEpochMilli());
        assertEquals(OffsetDateTime.ofInstant(zdt1.toInstant(), calendar.getTimeZone().toZoneId()), new CalendarUtils(calendar).toOffsetDateTime());
    }

    @ParameterizedTest
    @MethodSource(TimeZonesTest.TIME_ZONE_GET_AVAILABLE_IDS)
    void testToZonedDateTime(final String timeZoneId) {
        final TimeZone timeZone = TimeZones.getTimeZone(timeZoneId);
        final ZoneId zoneId = timeZone.toZoneId();
        final Calendar calendar = new GregorianCalendar(timeZone);
        calendar.setTimeInMillis(0);
        assertEquals(ZonedDateTime.ofInstant(calendar.toInstant(), calendar.getTimeZone().toZoneId()), new CalendarUtils(calendar).toZonedDateTime());
        final ZonedDateTime zdt1 = ZonedDateTime.of(1, 2, 3, 4, 5, 6, 0, zoneId);
        calendar.setTimeInMillis(zdt1.toInstant().toEpochMilli());
        assertEquals(ZonedDateTime.ofInstant(zdt1.toInstant(), calendar.getTimeZone().toZoneId()), new CalendarUtils(calendar).toZonedDateTime());
    }
}
