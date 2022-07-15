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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Iterator;
import java.util.Locale;
import java.util.NoSuchElementException;
import java.util.TimeZone;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junitpioneer.jupiter.DefaultLocale;

/**
 * Unit tests {@link org.apache.commons.lang3.time.DateUtils}.
 */
public class DateUtilsTest extends AbstractLangTest {

    private static Date BASE_DATE;

    /**
     * Used to check that Calendar objects are close enough
     * delta is in milliseconds
     */
    private static void assertCalendarsEquals(final String message, final Calendar cal1, final Calendar cal2, final long delta) {
        assertFalse(Math.abs(cal1.getTime().getTime() - cal2.getTime().getTime()) > delta,
                message + " expected " + cal1.getTime() + " but got " + cal2.getTime());
    }

    /**
     * This checks that this is a 7 element iterator of Calendar objects
     * that are dates (no time), and exactly 1 day spaced after each other.
     */
    private static void assertWeekIterator(final Iterator<?> it, final Calendar start) {
        final Calendar end = (Calendar) start.clone();
        end.add(Calendar.DATE, 6);

        assertWeekIterator(it, start, end);
    }
    /**
     * This checks that this is a 7 divisible iterator of Calendar objects
     * that are dates (no time), and exactly 1 day spaced after each other
     * (in addition to the proper start and stop dates)
     */
    private static void assertWeekIterator(final Iterator<?> it, final Calendar start, final Calendar end) {
        Calendar cal = (Calendar) it.next();
        assertCalendarsEquals("", start, cal, 0);
        Calendar last = null;
        int count = 1;
        while (it.hasNext()) {
            //Check this is just a date (no time component)
            assertCalendarsEquals("", cal, DateUtils.truncate(cal, Calendar.DATE), 0);

            last = cal;
            cal = (Calendar) it.next();
            count++;

            //Check that this is one day more than the last date
            last.add(Calendar.DATE, 1);
            assertCalendarsEquals("", last, cal, 0);
        }

        assertFalse(count % 7 != 0, "There were " + count + " days in this iterator");
        assertCalendarsEquals("", end, cal, 0);
    }
    /**
     * Convenience method for when working with Date objects
     */
    private static void assertWeekIterator(final Iterator<?> it, final Date start, final Date end) {
        final Calendar calStart = Calendar.getInstance();
        calStart.setTime(start);
        final Calendar calEnd = Calendar.getInstance();
        calEnd.setTime(end);

        assertWeekIterator(it, calStart, calEnd);
    }
    @BeforeAll
    public static void classSetup() {
        final GregorianCalendar cal = new GregorianCalendar(2000, 6, 5, 4, 3, 2);
        cal.set(Calendar.MILLISECOND, 1);
        BASE_DATE = cal.getTime();
    }
    private DateFormat dateParser = null;
    private DateFormat dateTimeParser = null;
    private Date dateAmPm1 = null;
    private Date dateAmPm2 = null;
    private Date dateAmPm3 = null;
    private Date dateAmPm4 = null;
    private Date date0 = null;
    private Date date1 = null;
    private Date date2 = null;
    private Date date3 = null;
    private Date date4 = null;
    private Date date5 = null;
    private Date date6 = null;
    private Date date7 = null;
    private Date date8 = null;
    private Calendar calAmPm1 = null;
    private Calendar calAmPm2 = null;
    private Calendar calAmPm3 = null;
    private Calendar calAmPm4 = null;
    private Calendar cal1 = null;
    private Calendar cal2 = null;
    private Calendar cal3 = null;
    private Calendar cal4 = null;
    private Calendar cal5 = null;
    private Calendar cal6 = null;

    private Calendar cal7 = null;

    private Calendar cal8 = null;

    private TimeZone zone = null;

    private TimeZone defaultZone = null;

    private void assertDate(final Date date, final int year, final int month, final int day, final int hour, final int min, final int sec, final int mil) {
        final GregorianCalendar cal = new GregorianCalendar();
        cal.setTime(date);
        assertEquals(year, cal.get(Calendar.YEAR));
        assertEquals(month, cal.get(Calendar.MONTH));
        assertEquals(day, cal.get(Calendar.DAY_OF_MONTH));
        assertEquals(hour, cal.get(Calendar.HOUR_OF_DAY));
        assertEquals(min, cal.get(Calendar.MINUTE));
        assertEquals(sec, cal.get(Calendar.SECOND));
        assertEquals(mil, cal.get(Calendar.MILLISECOND));
    }

    @BeforeEach
    public void setUp() throws Exception {
        dateParser = new SimpleDateFormat("MMM dd, yyyy", Locale.ENGLISH);
        dateTimeParser = new SimpleDateFormat("MMM dd, yyyy H:mm:ss.SSS", Locale.ENGLISH);

        dateAmPm1 = dateTimeParser.parse("February 3, 2002 01:10:00.000");
        dateAmPm2 = dateTimeParser.parse("February 3, 2002 11:10:00.000");
        dateAmPm3 = dateTimeParser.parse("February 3, 2002 13:10:00.000");
        dateAmPm4 = dateTimeParser.parse("February 3, 2002 19:10:00.000");
        date0 = dateTimeParser.parse("February 3, 2002 12:34:56.789");
        date1 = dateTimeParser.parse("February 12, 2002 12:34:56.789");
        date2 = dateTimeParser.parse("November 18, 2001 1:23:11.321");
        defaultZone = TimeZone.getDefault();
        zone = TimeZone.getTimeZone("MET");
        try {
            TimeZone.setDefault(zone);
            dateTimeParser.setTimeZone(zone);
            date3 = dateTimeParser.parse("March 30, 2003 05:30:45.000");
            date4 = dateTimeParser.parse("March 30, 2003 01:10:00.000");
            date5 = dateTimeParser.parse("March 30, 2003 01:40:00.000");
            date6 = dateTimeParser.parse("March 30, 2003 02:10:00.000");
            date7 = dateTimeParser.parse("March 30, 2003 02:40:00.000");
            date8 = dateTimeParser.parse("October 26, 2003 05:30:45.000");
        } finally {
            dateTimeParser.setTimeZone(defaultZone);
            TimeZone.setDefault(defaultZone);
        }
        calAmPm1 = Calendar.getInstance();
        calAmPm1.setTime(dateAmPm1);
        calAmPm2 = Calendar.getInstance();
        calAmPm2.setTime(dateAmPm2);
        calAmPm3 = Calendar.getInstance();
        calAmPm3.setTime(dateAmPm3);
        calAmPm4 = Calendar.getInstance();
        calAmPm4.setTime(dateAmPm4);
        cal1 = Calendar.getInstance();
        cal1.setTime(date1);
        cal2 = Calendar.getInstance();
        cal2.setTime(date2);
        try {
            TimeZone.setDefault(zone);
            cal3 = Calendar.getInstance();
            cal3.setTime(date3);
            cal4 = Calendar.getInstance();
            cal4.setTime(date4);
            cal5 = Calendar.getInstance();
            cal5.setTime(date5);
            cal6 = Calendar.getInstance();
            cal6.setTime(date6);
            cal7 = Calendar.getInstance();
            cal7.setTime(date7);
            cal8 = Calendar.getInstance();
            cal8.setTime(date8);
        } finally {
            TimeZone.setDefault(defaultZone);
        }
    }

    @Test
    public void testAddDays() throws Exception {
        Date result = DateUtils.addDays(BASE_DATE, 0);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2000, 6, 5, 4, 3, 2, 1);

        result = DateUtils.addDays(BASE_DATE, 1);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2000, 6, 6, 4, 3, 2, 1);

        result = DateUtils.addDays(BASE_DATE, -1);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2000, 6, 4, 4, 3, 2, 1);

        assertThrows(NullPointerException.class, () -> DateUtils.addDays(null, 0));
    }

    @Test
    public void testAddHours() throws Exception {
        Date result = DateUtils.addHours(BASE_DATE, 0);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2000, 6, 5, 4, 3, 2, 1);

        result = DateUtils.addHours(BASE_DATE, 1);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2000, 6, 5, 5, 3, 2, 1);

        result = DateUtils.addHours(BASE_DATE, -1);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2000, 6, 5, 3, 3, 2, 1);

        assertThrows(NullPointerException.class, () -> DateUtils.addHours(null, 0));
    }

    @Test
    public void testAddMilliseconds() throws Exception {
        Date result = DateUtils.addMilliseconds(BASE_DATE, 0);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2000, 6, 5, 4, 3, 2, 1);

        result = DateUtils.addMilliseconds(BASE_DATE, 1);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2000, 6, 5, 4, 3, 2, 2);

        result = DateUtils.addMilliseconds(BASE_DATE, -1);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2000, 6, 5, 4, 3, 2, 0);

        assertThrows(NullPointerException.class, () -> DateUtils.addMilliseconds(null, 0));
    }

    @Test
    public void testAddMinutes() throws Exception {
        Date result = DateUtils.addMinutes(BASE_DATE, 0);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2000, 6, 5, 4, 3, 2, 1);

        result = DateUtils.addMinutes(BASE_DATE, 1);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2000, 6, 5, 4, 4, 2, 1);

        result = DateUtils.addMinutes(BASE_DATE, -1);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2000, 6, 5, 4, 2, 2, 1);

        assertThrows(NullPointerException.class, () -> DateUtils.addMinutes(null, 0));
    }

    @Test
    public void testAddMonths() throws Exception {
        Date result = DateUtils.addMonths(BASE_DATE, 0);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2000, 6, 5, 4, 3, 2, 1);

        result = DateUtils.addMonths(BASE_DATE, 1);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2000, 7, 5, 4, 3, 2, 1);

        result = DateUtils.addMonths(BASE_DATE, -1);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2000, 5, 5, 4, 3, 2, 1);

        assertThrows(NullPointerException.class, () -> DateUtils.addMonths(null, 0));
    }

    @Test
    public void testAddSeconds() throws Exception {
        Date result = DateUtils.addSeconds(BASE_DATE, 0);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2000, 6, 5, 4, 3, 2, 1);

        result = DateUtils.addSeconds(BASE_DATE, 1);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2000, 6, 5, 4, 3, 3, 1);

        result = DateUtils.addSeconds(BASE_DATE, -1);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2000, 6, 5, 4, 3, 1, 1);

        assertThrows(NullPointerException.class, () -> DateUtils.addSeconds(null, 0));
    }

    @Test
    public void testAddWeeks() throws Exception {
        Date result = DateUtils.addWeeks(BASE_DATE, 0);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2000, 6, 5, 4, 3, 2, 1);

        result = DateUtils.addWeeks(BASE_DATE, 1);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2000, 6, 12, 4, 3, 2, 1);

        result = DateUtils.addWeeks(BASE_DATE, -1);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);      // july
        assertDate(result, 2000, 5, 28, 4, 3, 2, 1);   // june

        assertThrows(NullPointerException.class, () -> DateUtils.addMonths(null, 0));
    }

    @Test
    public void testAddYears() throws Exception {
        Date result = DateUtils.addYears(BASE_DATE, 0);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2000, 6, 5, 4, 3, 2, 1);

        result = DateUtils.addYears(BASE_DATE, 1);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2001, 6, 5, 4, 3, 2, 1);

        result = DateUtils.addYears(BASE_DATE, -1);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 1999, 6, 5, 4, 3, 2, 1);

        assertThrows(NullPointerException.class, () -> DateUtils.addYears(null, 0));
    }

    /**
     * Tests various values with the ceiling method
     *
     * @throws Exception so we don't have to catch it
     */
    @Test
    public void testCeil() throws Exception {
        // test javadoc
        assertEquals(dateTimeParser.parse("March 28, 2002 14:00:00.000"),
                DateUtils.ceiling(
                            dateTimeParser.parse("March 28, 2002 13:45:01.231"),
                        Calendar.HOUR),
                "ceiling javadoc-1 failed");
        assertEquals(dateTimeParser.parse("April 1, 2002 00:00:00.000"),
                DateUtils.ceiling(
                            dateTimeParser.parse("March 28, 2002 13:45:01.231"),
                        Calendar.MONTH),
                "ceiling javadoc-2 failed");

        // tests public static Date ceiling(Date date, int field)
        assertEquals(dateParser.parse("January 1, 2003"),
                DateUtils.ceiling(date1, Calendar.YEAR),
                "ceiling year-1 failed");
        assertEquals(dateParser.parse("January 1, 2002"),
                DateUtils.ceiling(date2, Calendar.YEAR),
                "ceiling year-2 failed");
        assertEquals(dateParser.parse("March 1, 2002"),
                DateUtils.ceiling(date1, Calendar.MONTH),
                "ceiling month-1 failed");
        assertEquals(dateParser.parse("December 1, 2001"),
                DateUtils.ceiling(date2, Calendar.MONTH),
                "ceiling month-2 failed");
        assertEquals(dateParser.parse("February 16, 2002"),
                DateUtils.ceiling(date1, DateUtils.SEMI_MONTH),
                "ceiling semimonth-1 failed");
        assertEquals(dateParser.parse("December 1, 2001"),
                DateUtils.ceiling(date2, DateUtils.SEMI_MONTH),
                "ceiling semimonth-2 failed");
        assertEquals(dateParser.parse("February 13, 2002"),
                DateUtils.ceiling(date1, Calendar.DATE),
                "ceiling date-1 failed");
        assertEquals(dateParser.parse("November 19, 2001"),
                DateUtils.ceiling(date2, Calendar.DATE),
                "ceiling date-2 failed");
        assertEquals(dateTimeParser.parse("February 12, 2002 13:00:00.000"),
                DateUtils.ceiling(date1, Calendar.HOUR),
                "ceiling hour-1 failed");
        assertEquals(dateTimeParser.parse("November 18, 2001 2:00:00.000"),
                DateUtils.ceiling(date2, Calendar.HOUR),
                "ceiling hour-2 failed");
        assertEquals(dateTimeParser.parse("February 12, 2002 12:35:00.000"),
                DateUtils.ceiling(date1, Calendar.MINUTE),
                "ceiling minute-1 failed");
        assertEquals(dateTimeParser.parse("November 18, 2001 1:24:00.000"),
                DateUtils.ceiling(date2, Calendar.MINUTE),
                "ceiling minute-2 failed");
        assertEquals(dateTimeParser.parse("February 12, 2002 12:34:57.000"),
                DateUtils.ceiling(date1, Calendar.SECOND),
                "ceiling second-1 failed");
        assertEquals(dateTimeParser.parse("November 18, 2001 1:23:12.000"),
                DateUtils.ceiling(date2, Calendar.SECOND),
                "ceiling second-2 failed");
        assertEquals(dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.ceiling(dateAmPm1, Calendar.AM_PM),
                "ceiling ampm-1 failed");
        assertEquals(dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.ceiling(dateAmPm2, Calendar.AM_PM),
                "ceiling ampm-2 failed");
        assertEquals(dateTimeParser.parse("February 4, 2002 00:00:00.000"),
                DateUtils.ceiling(dateAmPm3, Calendar.AM_PM),
                "ceiling ampm-3 failed");
        assertEquals(dateTimeParser.parse("February 4, 2002 00:00:00.000"),
                DateUtils.ceiling(dateAmPm4, Calendar.AM_PM),
                "ceiling ampm-4 failed");

     // tests public static Date ceiling(Object date, int field)
        assertEquals(dateParser.parse("January 1, 2003"),
                DateUtils.ceiling((Object) date1, Calendar.YEAR),
                "ceiling year-1 failed");
        assertEquals(dateParser.parse("January 1, 2002"),
                DateUtils.ceiling((Object) date2, Calendar.YEAR),
                "ceiling year-2 failed");
        assertEquals(dateParser.parse("March 1, 2002"),
                DateUtils.ceiling((Object) date1, Calendar.MONTH),
                "ceiling month-1 failed");
        assertEquals(dateParser.parse("December 1, 2001"),
                DateUtils.ceiling((Object) date2, Calendar.MONTH),
                "ceiling month-2 failed");
        assertEquals(dateParser.parse("February 16, 2002"),
                DateUtils.ceiling((Object) date1, DateUtils.SEMI_MONTH),
                "ceiling semimonth-1 failed");
        assertEquals(dateParser.parse("December 1, 2001"),
                DateUtils.ceiling((Object) date2, DateUtils.SEMI_MONTH),
                "ceiling semimonth-2 failed");
        assertEquals(dateParser.parse("February 13, 2002"),
                DateUtils.ceiling((Object) date1, Calendar.DATE),
                "ceiling date-1 failed");
        assertEquals(dateParser.parse("November 19, 2001"),
                DateUtils.ceiling((Object) date2, Calendar.DATE),
                "ceiling date-2 failed");
        assertEquals(dateTimeParser.parse("February 12, 2002 13:00:00.000"),
                DateUtils.ceiling((Object) date1, Calendar.HOUR),
                "ceiling hour-1 failed");
        assertEquals(dateTimeParser.parse("November 18, 2001 2:00:00.000"),
                DateUtils.ceiling((Object) date2, Calendar.HOUR),
                "ceiling hour-2 failed");
        assertEquals(dateTimeParser.parse("February 12, 2002 12:35:00.000"),
                DateUtils.ceiling((Object) date1, Calendar.MINUTE),
                "ceiling minute-1 failed");
        assertEquals(dateTimeParser.parse("November 18, 2001 1:24:00.000"),
                DateUtils.ceiling((Object) date2, Calendar.MINUTE),
                "ceiling minute-2 failed");
        assertEquals(dateTimeParser.parse("February 12, 2002 12:34:57.000"),
                DateUtils.ceiling((Object) date1, Calendar.SECOND),
                "ceiling second-1 failed");
        assertEquals(dateTimeParser.parse("November 18, 2001 1:23:12.000"),
                DateUtils.ceiling((Object) date2, Calendar.SECOND),
                "ceiling second-2 failed");
        assertEquals(dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.ceiling((Object) dateAmPm1, Calendar.AM_PM),
                "ceiling ampm-1 failed");
        assertEquals(dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.ceiling((Object) dateAmPm2, Calendar.AM_PM),
                "ceiling ampm-2 failed");
        assertEquals(dateTimeParser.parse("February 4, 2002 00:00:00.000"),
                DateUtils.ceiling((Object) dateAmPm3, Calendar.AM_PM),
                "ceiling ampm-3 failed");
        assertEquals(dateTimeParser.parse("February 4, 2002 00:00:00.000"),
                DateUtils.ceiling((Object) dateAmPm4, Calendar.AM_PM),
                "ceiling ampm-4 failed");

        assertEquals(dateTimeParser.parse("February 12, 2002 12:34:57.000"),
                DateUtils.ceiling((Object) cal1, Calendar.SECOND),
                "ceiling calendar second-1 failed");
        assertEquals(dateTimeParser.parse("November 18, 2001 1:23:12.000"),
                DateUtils.ceiling((Object) cal2, Calendar.SECOND),
                "ceiling calendar second-2 failed");

        assertEquals(dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.ceiling((Object) calAmPm1, Calendar.AM_PM),
                "ceiling ampm-1 failed");
        assertEquals(dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.ceiling((Object) calAmPm2, Calendar.AM_PM),
                "ceiling ampm-2 failed");
        assertEquals(dateTimeParser.parse("February 4, 2002 00:00:00.000"),
                DateUtils.ceiling((Object) calAmPm3, Calendar.AM_PM),
                "ceiling ampm-3 failed");
        assertEquals(dateTimeParser.parse("February 4, 2002 00:00:00.000"),
                DateUtils.ceiling((Object) calAmPm4, Calendar.AM_PM),
                "ceiling ampm-4 failed");

        assertThrows(NullPointerException.class, () -> DateUtils.ceiling((Date) null, Calendar.SECOND));
        assertThrows(NullPointerException.class, () -> DateUtils.ceiling((Calendar) null, Calendar.SECOND));
        assertThrows(NullPointerException.class, () -> DateUtils.ceiling((Object) null, Calendar.SECOND));
        assertThrows(ClassCastException.class, () -> DateUtils.ceiling("", Calendar.SECOND));
        assertThrows(IllegalArgumentException.class, () -> DateUtils.ceiling(date1, -9999));

        // Fix for https://issues.apache.org/bugzilla/show_bug.cgi?id=25560
        // Test ceiling across the beginning of daylight saving time
        try {
            TimeZone.setDefault(zone);
            dateTimeParser.setTimeZone(zone);

            assertEquals(dateTimeParser.parse("March 31, 2003 00:00:00.000"),
                    DateUtils.ceiling(date4, Calendar.DATE),
                    "ceiling MET date across DST change-over");
            assertEquals(dateTimeParser.parse("March 31, 2003 00:00:00.000"),
                    DateUtils.ceiling((Object) cal4, Calendar.DATE),
                    "ceiling MET date across DST change-over");
            assertEquals(dateTimeParser.parse("March 31, 2003 00:00:00.000"),
                    DateUtils.ceiling(date5, Calendar.DATE),
                    "ceiling MET date across DST change-over");
            assertEquals(dateTimeParser.parse("March 31, 2003 00:00:00.000"),
                    DateUtils.ceiling((Object) cal5, Calendar.DATE),
                    "ceiling MET date across DST change-over");
            assertEquals(dateTimeParser.parse("March 31, 2003 00:00:00.000"),
                    DateUtils.ceiling(date6, Calendar.DATE),
                    "ceiling MET date across DST change-over");
            assertEquals(dateTimeParser.parse("March 31, 2003 00:00:00.000"),
                    DateUtils.ceiling((Object) cal6, Calendar.DATE),
                    "ceiling MET date across DST change-over");
            assertEquals(dateTimeParser.parse("March 31, 2003 00:00:00.000"),
                    DateUtils.ceiling(date7, Calendar.DATE),
                    "ceiling MET date across DST change-over");
            assertEquals(dateTimeParser.parse("March 31, 2003 00:00:00.000"),
                    DateUtils.ceiling((Object) cal7, Calendar.DATE),
                    "ceiling MET date across DST change-over");

            assertEquals(dateTimeParser.parse("March 30, 2003 03:00:00.000"),
                    DateUtils.ceiling(date4, Calendar.HOUR_OF_DAY),
                    "ceiling MET date across DST change-over");
            assertEquals(dateTimeParser.parse("March 30, 2003 03:00:00.000"),
                    DateUtils.ceiling((Object) cal4, Calendar.HOUR_OF_DAY),
                    "ceiling MET date across DST change-over");
            assertEquals(dateTimeParser.parse("March 30, 2003 03:00:00.000"),
                    DateUtils.ceiling(date5, Calendar.HOUR_OF_DAY),
                    "ceiling MET date across DST change-over");
            assertEquals(dateTimeParser.parse("March 30, 2003 03:00:00.000"),
                    DateUtils.ceiling((Object) cal5, Calendar.HOUR_OF_DAY),
                    "ceiling MET date across DST change-over");
            assertEquals(dateTimeParser.parse("March 30, 2003 04:00:00.000"),
                    DateUtils.ceiling(date6, Calendar.HOUR_OF_DAY),
                    "ceiling MET date across DST change-over");
            assertEquals(dateTimeParser.parse("March 30, 2003 04:00:00.000"),
                    DateUtils.ceiling((Object) cal6, Calendar.HOUR_OF_DAY),
                    "ceiling MET date across DST change-over");
            assertEquals(dateTimeParser.parse("March 30, 2003 04:00:00.000"),
                    DateUtils.ceiling(date7, Calendar.HOUR_OF_DAY),
                    "ceiling MET date across DST change-over");
            assertEquals(dateTimeParser.parse("March 30, 2003 04:00:00.000"),
                    DateUtils.ceiling((Object) cal7, Calendar.HOUR_OF_DAY),
                    "ceiling MET date across DST change-over");

        } finally {
            TimeZone.setDefault(defaultZone);
            dateTimeParser.setTimeZone(defaultZone);
        }

     // Bug 31395, large dates
        final Date endOfTime = new Date(Long.MAX_VALUE); // fyi: Sun Aug 17 07:12:55 CET 292278994 -- 807 millis
        final GregorianCalendar endCal = new GregorianCalendar();
        endCal.setTime(endOfTime);
        assertThrows(ArithmeticException.class, () -> DateUtils.ceiling(endCal, Calendar.DATE));
        endCal.set(Calendar.YEAR, 280000001);
        assertThrows(ArithmeticException.class, () -> DateUtils.ceiling(endCal, Calendar.DATE));
        endCal.set(Calendar.YEAR, 280000000);
        final Calendar cal = DateUtils.ceiling(endCal, Calendar.DATE);
        assertEquals(0, cal.get(Calendar.HOUR));
    }

    @Test
    public void testConstructor() {
        assertNotNull(new DateUtils());
        final Constructor<?>[] cons = DateUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertTrue(Modifier.isPublic(cons[0].getModifiers()));
        assertTrue(Modifier.isPublic(DateUtils.class.getModifiers()));
        assertFalse(Modifier.isFinal(DateUtils.class.getModifiers()));
    }

    @Test
    public void testIsSameDay_Cal() {
        final GregorianCalendar cala = new GregorianCalendar(2004, 6, 9, 13, 45);
        final GregorianCalendar calb = new GregorianCalendar(2004, 6, 9, 13, 45);
        assertTrue(DateUtils.isSameDay(cala, calb));
        calb.add(Calendar.DAY_OF_YEAR, 1);
        assertFalse(DateUtils.isSameDay(cala, calb));
        cala.add(Calendar.DAY_OF_YEAR, 1);
        assertTrue(DateUtils.isSameDay(cala, calb));
        calb.add(Calendar.YEAR, 1);
        assertFalse(DateUtils.isSameDay(cala, calb));
    }

    @Test
    public void testIsSameDay_CalNotNullNull() {
        assertThrows(NullPointerException.class, () -> DateUtils.isSameDay(Calendar.getInstance(), null));
    }

    @Test
    public void testIsSameDay_CalNullNotNull() {
        assertThrows(NullPointerException.class, () -> DateUtils.isSameDay(null, Calendar.getInstance()));
    }

    @Test
    public void testIsSameDay_CalNullNull() {
        assertThrows(NullPointerException.class, () -> DateUtils.isSameDay((Calendar) null, null));
    }

    @Test
    public void testIsSameDay_Date() {
        Date datea = new GregorianCalendar(2004, 6, 9, 13, 45).getTime();
        Date dateb = new GregorianCalendar(2004, 6, 9, 13, 45).getTime();
        assertTrue(DateUtils.isSameDay(datea, dateb));
        dateb = new GregorianCalendar(2004, 6, 10, 13, 45).getTime();
        assertFalse(DateUtils.isSameDay(datea, dateb));
        datea = new GregorianCalendar(2004, 6, 10, 13, 45).getTime();
        assertTrue(DateUtils.isSameDay(datea, dateb));
        dateb = new GregorianCalendar(2005, 6, 10, 13, 45).getTime();
        assertFalse(DateUtils.isSameDay(datea, dateb));
    }

    @Test
    public void testIsSameDay_DateNotNullNull() {
        assertThrows(NullPointerException.class, () -> DateUtils.isSameDay(new Date(), null));
    }

    @Test
    public void testIsSameDay_DateNullNotNull() {
        assertThrows(NullPointerException.class, () -> DateUtils.isSameDay(null, new Date()));
    }

    @Test
    public void testIsSameDay_DateNullNull() {
        assertThrows(NullPointerException.class, () -> DateUtils.isSameDay((Date) null, null));
    }

    @Test
    public void testIsSameInstant_Cal() {
        final GregorianCalendar cala = new GregorianCalendar(TimeZone.getTimeZone("GMT+1"));
        final GregorianCalendar calb = new GregorianCalendar(TimeZone.getTimeZone("GMT-1"));
        cala.set(2004, Calendar.JULY, 9, 13, 45, 0);
        cala.set(Calendar.MILLISECOND, 0);
        calb.set(2004, Calendar.JULY, 9, 13, 45, 0);
        calb.set(Calendar.MILLISECOND, 0);
        assertFalse(DateUtils.isSameInstant(cala, calb));

        calb.set(2004, Calendar.JULY, 9, 11, 45, 0);
        assertTrue(DateUtils.isSameInstant(cala, calb));
    }

    @Test
    public void testIsSameInstant_CalNotNullNull() {
        assertThrows(NullPointerException.class, () -> DateUtils.isSameInstant(Calendar.getInstance(), null));
    }

    @Test
    public void testIsSameInstant_CalNullNotNull() {
        assertThrows(NullPointerException.class, () -> DateUtils.isSameInstant(null, Calendar.getInstance()));
    }

    @Test
    public void testIsSameInstant_CalNullNull() {
        assertThrows(NullPointerException.class, () -> DateUtils.isSameInstant((Calendar) null, null));
    }

    @Test
    public void testIsSameInstant_Date() {
        Date datea = new GregorianCalendar(2004, 6, 9, 13, 45).getTime();
        Date dateb = new GregorianCalendar(2004, 6, 9, 13, 45).getTime();
        assertTrue(DateUtils.isSameInstant(datea, dateb));
        dateb = new GregorianCalendar(2004, 6, 10, 13, 45).getTime();
        assertFalse(DateUtils.isSameInstant(datea, dateb));
        datea = new GregorianCalendar(2004, 6, 10, 13, 45).getTime();
        assertTrue(DateUtils.isSameInstant(datea, dateb));
        dateb = new GregorianCalendar(2005, 6, 10, 13, 45).getTime();
        assertFalse(DateUtils.isSameInstant(datea, dateb));
    }

    @Test
    public void testIsSameInstant_DateNotNullNull() {
        assertThrows(NullPointerException.class, () -> DateUtils.isSameInstant(new Date(), null));
    }

    @Test
    public void testIsSameInstant_DateNullNotNull() {
        assertThrows(NullPointerException.class, () -> DateUtils.isSameInstant(null, new Date()));
    }

    @Test
    public void testIsSameInstant_DateNullNull() {
        assertThrows(NullPointerException.class, () -> DateUtils.isSameInstant((Date) null, null));
    }

    @Test
    public void testIsSameLocalTime_Cal() {
        final GregorianCalendar cala = new GregorianCalendar(TimeZone.getTimeZone("GMT+1"));
        final GregorianCalendar calb = new GregorianCalendar(TimeZone.getTimeZone("GMT-1"));
        cala.set(2004, Calendar.JULY, 9, 13, 45, 0);
        cala.set(Calendar.MILLISECOND, 0);
        calb.set(2004, Calendar.JULY, 9, 13, 45, 0);
        calb.set(Calendar.MILLISECOND, 0);
        assertTrue(DateUtils.isSameLocalTime(cala, calb));

        final Calendar calc = Calendar.getInstance();
        final Calendar cald = Calendar.getInstance();
        calc.set(2004, Calendar.JULY, 9, 4,  0, 0);
        cald.set(2004, Calendar.JULY, 9, 16, 0, 0);
        calc.set(Calendar.MILLISECOND, 0);
        cald.set(Calendar.MILLISECOND, 0);
        assertFalse(DateUtils.isSameLocalTime(calc, cald), "LANG-677");

        calb.set(2004, Calendar.JULY, 9, 11, 45, 0);
        assertFalse(DateUtils.isSameLocalTime(cala, calb));
    }

    @Test
    public void testIsSameLocalTime_CalNotNullNull() {
        assertThrows(NullPointerException.class, () -> DateUtils.isSameLocalTime(Calendar.getInstance(), null));
    }

    @Test
    public void testIsSameLocalTime_CalNullNotNull() {
        assertThrows(NullPointerException.class, () -> DateUtils.isSameLocalTime(null, Calendar.getInstance()));
    }

    @Test
    public void testIsSameLocalTime_CalNullNull() {
        assertThrows(NullPointerException.class, () -> DateUtils.isSameLocalTime(null, null));
    }

    /**
     * Tests the iterator exceptions
     */
    @Test
    public void testIteratorEx() {
        assertThrows(IllegalArgumentException.class, () -> DateUtils.iterator(Calendar.getInstance(), -9999));
        assertThrows(NullPointerException.class, () -> DateUtils.iterator((Date) null, DateUtils.RANGE_WEEK_CENTER));
        assertThrows(NullPointerException.class, () -> DateUtils.iterator((Calendar) null, DateUtils.RANGE_WEEK_CENTER));
        assertThrows(NullPointerException.class, () -> DateUtils.iterator((Object) null, DateUtils.RANGE_WEEK_CENTER));
        assertThrows(ClassCastException.class, () -> DateUtils.iterator("", DateUtils.RANGE_WEEK_CENTER));
    }

    // https://issues.apache.org/jira/browse/LANG-530
    @SuppressWarnings("deprecation")
    @Test
    public void testLang530() throws ParseException {
        final Date d = new Date();
        final String isoDateStr = DateFormatUtils.ISO_DATETIME_TIME_ZONE_FORMAT.format(d);
        final Date d2 = DateUtils.parseDate(isoDateStr, DateFormatUtils.ISO_DATETIME_TIME_ZONE_FORMAT.getPattern());
        // the format loses milliseconds so have to reintroduce them
        assertEquals(d.getTime(), d2.getTime() + d.getTime() % 1000, "Date not equal to itself ISO formatted and parsed");
    }

    @Test
    public void testLANG799() throws ParseException {
        DateUtils.parseDateStrictly("09 abril 2008 23:55:38 GMT", new Locale("es"), "dd MMM yyyy HH:mm:ss zzz");
    }

    // Parse English date with German Locale
    @DefaultLocale(language = "de")
    @Test
    public void testLANG799_DE_FAIL() {
        assertThrows(ParseException.class, () -> DateUtils.parseDate("Wed, 09 Apr 2008 23:55:38 GMT", "EEE, dd MMM yyyy HH:mm:ss zzz"));
    }

    @DefaultLocale(language = "de")
    @Test
    public void testLANG799_DE_OK() throws ParseException {
        DateUtils.parseDate("Mi, 09 Apr 2008 23:55:38 GMT", "EEE, dd MMM yyyy HH:mm:ss zzz");
        DateUtils.parseDateStrictly("Mi, 09 Apr 2008 23:55:38 GMT", "EEE, dd MMM yyyy HH:mm:ss zzz");
    }

    // Parse German date with English Locale
    @DefaultLocale(language = "en")
    @Test
    public void testLANG799_EN_FAIL() {
        assertThrows(ParseException.class, () -> DateUtils.parseDate("Mi, 09 Apr 2008 23:55:38 GMT", "EEE, dd MMM yyyy HH:mm:ss zzz"));
    }

    @DefaultLocale(language = "en")
    @Test
    public void testLANG799_EN_OK() throws ParseException {
        DateUtils.parseDate("Wed, 09 Apr 2008 23:55:38 GMT", "EEE, dd MMM yyyy HH:mm:ss zzz");
        DateUtils.parseDateStrictly("Wed, 09 Apr 2008 23:55:38 GMT", "EEE, dd MMM yyyy HH:mm:ss zzz");
    }

    // Parse German date with English Locale, specifying German Locale override
    @DefaultLocale(language = "en")
    @Test
    public void testLANG799_EN_WITH_DE_LOCALE() throws ParseException {
        DateUtils.parseDate("Mi, 09 Apr 2008 23:55:38 GMT", Locale.GERMAN, "EEE, dd MMM yyyy HH:mm:ss zzz");
    }

    /**
     * Tests the calendar iterator for month-based ranges
     *
     * @throws Exception so we don't have to catch it
     */
    @Test
    public void testMonthIterator() throws Exception {
        Iterator<?> it = DateUtils.iterator(date1, DateUtils.RANGE_MONTH_SUNDAY);
        assertWeekIterator(it,
                dateParser.parse("January 27, 2002"),
                dateParser.parse("March 2, 2002"));

        it = DateUtils.iterator(date1, DateUtils.RANGE_MONTH_MONDAY);
        assertWeekIterator(it,
                dateParser.parse("January 28, 2002"),
                dateParser.parse("March 3, 2002"));

        it = DateUtils.iterator(date2, DateUtils.RANGE_MONTH_SUNDAY);
        assertWeekIterator(it,
                dateParser.parse("October 28, 2001"),
                dateParser.parse("December 1, 2001"));

        it = DateUtils.iterator(date2, DateUtils.RANGE_MONTH_MONDAY);
        assertWeekIterator(it,
                dateParser.parse("October 29, 2001"),
                dateParser.parse("December 2, 2001"));
    }

    @Test
    public void testParse_EmptyParsers() {
        assertThrows(ParseException.class, () -> DateUtils.parseDate("19721203"));
    }

    @Test
    public void testParse_NullParsers() {
        assertThrows(NullPointerException.class, () -> DateUtils.parseDate("19721203", (String[]) null));
    }

    @Test
    public void testParseDate() throws Exception {
        final GregorianCalendar cal = new GregorianCalendar(1972, 11, 3);
        String dateStr = "1972-12-03";
        final String[] parsers = {"yyyy'-'DDD", "yyyy'-'MM'-'dd", "yyyyMMdd"};
        Date date = DateUtils.parseDate(dateStr, parsers);
        assertEquals(cal.getTime(), date);

        dateStr = "1972-338";
        date = DateUtils.parseDate(dateStr, parsers);
        assertEquals(cal.getTime(), date);

        dateStr = "19721203";
        date = DateUtils.parseDate(dateStr, parsers);
        assertEquals(cal.getTime(), date);
    }

    @Test
    public void testParseDate_InvalidDateString() {
        final String[] parsers = {"yyyy'-'DDD", "yyyy'-'MM'-'dd", "yyyyMMdd"};
        assertThrows(ParseException.class, () -> DateUtils.parseDate("197212AB", parsers));
    }

    @Test
    public void testParseDate_NoDateString() {
        final String[] parsers = {"yyyy'-'DDD", "yyyy'-'MM'-'dd", "yyyyMMdd"};
        assertThrows(ParseException.class, () -> DateUtils.parseDate("PURPLE", parsers));
    }

    @Test
    public void testParseDate_Null() {
        final String[] parsers = {"yyyy'-'DDD", "yyyy'-'MM'-'dd", "yyyyMMdd"};
        assertThrows(NullPointerException.class, () -> DateUtils.parseDate(null, parsers));
    }

    // LANG-486
    @Test
    public void testParseDateWithLeniency() throws Exception {
        final GregorianCalendar cal = new GregorianCalendar(1998, 6, 30);
        final String dateStr = "02 942, 1996";
        final String[] parsers = {"MM DDD, yyyy"};

        final Date date = DateUtils.parseDate(dateStr, parsers);
        assertEquals(cal.getTime(), date);

        assertThrows(ParseException.class, () -> DateUtils.parseDateStrictly(dateStr, parsers));
    }

    /**
     * Tests various values with the round method
     *
     * @throws Exception so we don't have to catch it
     */
    @Test
    public void testRound() throws Exception {
        // tests for public static Date round(Date date, int field)
        assertEquals(dateParser.parse("January 1, 2002"),
                DateUtils.round(date1, Calendar.YEAR),
                "round year-1 failed");
        assertEquals(dateParser.parse("January 1, 2002"),
                DateUtils.round(date2, Calendar.YEAR),
                "round year-2 failed");
        assertEquals(dateParser.parse("February 1, 2002"),
                DateUtils.round(date1, Calendar.MONTH),
                "round month-1 failed");
        assertEquals(dateParser.parse("December 1, 2001"),
                DateUtils.round(date2, Calendar.MONTH),
                "round month-2 failed");
        assertEquals(dateParser.parse("February 1, 2002"),
                DateUtils.round(date0, DateUtils.SEMI_MONTH),
                "round semimonth-0 failed");
        assertEquals(dateParser.parse("February 16, 2002"),
                DateUtils.round(date1, DateUtils.SEMI_MONTH),
                "round semimonth-1 failed");
        assertEquals(dateParser.parse("November 16, 2001"),
                DateUtils.round(date2, DateUtils.SEMI_MONTH),
                "round semimonth-2 failed");


        assertEquals(dateParser.parse("February 13, 2002"),
                DateUtils.round(date1, Calendar.DATE),
                "round date-1 failed");
        assertEquals(dateParser.parse("November 18, 2001"),
                DateUtils.round(date2, Calendar.DATE),
                "round date-2 failed");
        assertEquals(dateTimeParser.parse("February 12, 2002 13:00:00.000"),
                DateUtils.round(date1, Calendar.HOUR),
                "round hour-1 failed");
        assertEquals(dateTimeParser.parse("November 18, 2001 1:00:00.000"),
                DateUtils.round(date2, Calendar.HOUR),
                "round hour-2 failed");
        assertEquals(dateTimeParser.parse("February 12, 2002 12:35:00.000"),
                DateUtils.round(date1, Calendar.MINUTE),
                "round minute-1 failed");
        assertEquals(dateTimeParser.parse("November 18, 2001 1:23:00.000"),
                DateUtils.round(date2, Calendar.MINUTE),
                "round minute-2 failed");
        assertEquals(dateTimeParser.parse("February 12, 2002 12:34:57.000"),
                DateUtils.round(date1, Calendar.SECOND),
                "round second-1 failed");
        assertEquals(dateTimeParser.parse("November 18, 2001 1:23:11.000"),
                DateUtils.round(date2, Calendar.SECOND),
                "round second-2 failed");
        assertEquals(dateTimeParser.parse("February 3, 2002 00:00:00.000"),
                DateUtils.round(dateAmPm1, Calendar.AM_PM),
                "round ampm-1 failed");
        assertEquals(dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.round(dateAmPm2, Calendar.AM_PM),
                "round ampm-2 failed");
        assertEquals(dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.round(dateAmPm3, Calendar.AM_PM),
                "round ampm-3 failed");
        assertEquals(dateTimeParser.parse("February 4, 2002 00:00:00.000"),
                DateUtils.round(dateAmPm4, Calendar.AM_PM),
                "round ampm-4 failed");

        // tests for public static Date round(Object date, int field)
        assertEquals(dateParser.parse("January 1, 2002"),
                DateUtils.round((Object) date1, Calendar.YEAR),
                "round year-1 failed");
        assertEquals(dateParser.parse("January 1, 2002"),
                DateUtils.round((Object) date2, Calendar.YEAR),
                "round year-2 failed");
        assertEquals(dateParser.parse("February 1, 2002"),
                DateUtils.round((Object) date1, Calendar.MONTH),
                "round month-1 failed");
        assertEquals(dateParser.parse("December 1, 2001"),
                DateUtils.round((Object) date2, Calendar.MONTH),
                "round month-2 failed");
        assertEquals(dateParser.parse("February 16, 2002"),
                DateUtils.round((Object) date1, DateUtils.SEMI_MONTH),
                "round semimonth-1 failed");
        assertEquals(dateParser.parse("November 16, 2001"),
                DateUtils.round((Object) date2, DateUtils.SEMI_MONTH),
                "round semimonth-2 failed");
        assertEquals(dateParser.parse("February 13, 2002"),
                DateUtils.round((Object) date1, Calendar.DATE),
                "round date-1 failed");
        assertEquals(dateParser.parse("November 18, 2001"),
                DateUtils.round((Object) date2, Calendar.DATE),
                "round date-2 failed");
        assertEquals(dateTimeParser.parse("February 12, 2002 13:00:00.000"),
                DateUtils.round((Object) date1, Calendar.HOUR),
                "round hour-1 failed");
        assertEquals(dateTimeParser.parse("November 18, 2001 1:00:00.000"),
                DateUtils.round((Object) date2, Calendar.HOUR),
                "round hour-2 failed");
        assertEquals(dateTimeParser.parse("February 12, 2002 12:35:00.000"),
                DateUtils.round((Object) date1, Calendar.MINUTE),
                "round minute-1 failed");
        assertEquals(dateTimeParser.parse("November 18, 2001 1:23:00.000"),
                DateUtils.round((Object) date2, Calendar.MINUTE),
                "round minute-2 failed");
        assertEquals(dateTimeParser.parse("February 12, 2002 12:34:57.000"),
                DateUtils.round((Object) date1, Calendar.SECOND),
                "round second-1 failed");
        assertEquals(dateTimeParser.parse("November 18, 2001 1:23:11.000"),
                DateUtils.round((Object) date2, Calendar.SECOND),
                "round second-2 failed");
        assertEquals(dateTimeParser.parse("February 12, 2002 12:34:57.000"),
                DateUtils.round((Object) cal1, Calendar.SECOND),
                "round calendar second-1 failed");
        assertEquals(dateTimeParser.parse("November 18, 2001 1:23:11.000"),
                DateUtils.round((Object) cal2, Calendar.SECOND),
                "round calendar second-2 failed");
        assertEquals(dateTimeParser.parse("February 3, 2002 00:00:00.000"),
                DateUtils.round((Object) dateAmPm1, Calendar.AM_PM),
                "round ampm-1 failed");
        assertEquals(dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.round((Object) dateAmPm2, Calendar.AM_PM),
                "round ampm-2 failed");
        assertEquals(dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.round((Object) dateAmPm3, Calendar.AM_PM),
                "round ampm-3 failed");
        assertEquals(dateTimeParser.parse("February 4, 2002 00:00:00.000"),
                DateUtils.round((Object) dateAmPm4, Calendar.AM_PM),
                "round ampm-4 failed");

        assertThrows(NullPointerException.class, () -> DateUtils.round((Date) null, Calendar.SECOND));
        assertThrows(NullPointerException.class, () -> DateUtils.round((Calendar) null, Calendar.SECOND));
        assertThrows(NullPointerException.class, () -> DateUtils.round((Object) null, Calendar.SECOND));
        assertThrows(ClassCastException.class, () -> DateUtils.round("", Calendar.SECOND));
        assertThrows(IllegalArgumentException.class, () -> DateUtils.round(date1, -9999));

        assertEquals(dateTimeParser.parse("February 3, 2002 00:00:00.000"),
                DateUtils.round((Object) calAmPm1, Calendar.AM_PM),
                "round ampm-1 failed");
        assertEquals(dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.round((Object) calAmPm2, Calendar.AM_PM),
                "round ampm-2 failed");
        assertEquals(dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.round((Object) calAmPm3, Calendar.AM_PM),
                "round ampm-3 failed");
        assertEquals(dateTimeParser.parse("February 4, 2002 00:00:00.000"),
                DateUtils.round((Object) calAmPm4, Calendar.AM_PM),
                "round ampm-4 failed");

        // Fix for https://issues.apache.org/bugzilla/show_bug.cgi?id=25560 / LANG-13
        // Test rounding across the beginning of daylight saving time
        try {
            TimeZone.setDefault(zone);
            dateTimeParser.setTimeZone(zone);
            assertEquals(dateTimeParser.parse("March 30, 2003 00:00:00.000"),
                    DateUtils.round(date4, Calendar.DATE),
                    "round MET date across DST change-over");
            assertEquals(dateTimeParser.parse("March 30, 2003 00:00:00.000"),
                    DateUtils.round((Object) cal4, Calendar.DATE),
                    "round MET date across DST change-over");
            assertEquals(dateTimeParser.parse("March 30, 2003 00:00:00.000"),
                    DateUtils.round(date5, Calendar.DATE),
                    "round MET date across DST change-over");
            assertEquals(dateTimeParser.parse("March 30, 2003 00:00:00.000"),
                    DateUtils.round((Object) cal5, Calendar.DATE),
                    "round MET date across DST change-over");
            assertEquals(dateTimeParser.parse("March 30, 2003 00:00:00.000"),
                    DateUtils.round(date6, Calendar.DATE),
                    "round MET date across DST change-over");
            assertEquals(dateTimeParser.parse("March 30, 2003 00:00:00.000"),
                    DateUtils.round((Object) cal6, Calendar.DATE),
                    "round MET date across DST change-over");
            assertEquals(dateTimeParser.parse("March 30, 2003 00:00:00.000"),
                    DateUtils.round(date7, Calendar.DATE),
                    "round MET date across DST change-over");
            assertEquals(dateTimeParser.parse("March 30, 2003 00:00:00.000"),
                    DateUtils.round((Object) cal7, Calendar.DATE),
                    "round MET date across DST change-over");

            assertEquals(dateTimeParser.parse("March 30, 2003 01:00:00.000"),
                    DateUtils.round(date4, Calendar.HOUR_OF_DAY),
                    "round MET date across DST change-over");
            assertEquals(dateTimeParser.parse("March 30, 2003 01:00:00.000"),
                    DateUtils.round((Object) cal4, Calendar.HOUR_OF_DAY),
                    "round MET date across DST change-over");
            assertEquals(dateTimeParser.parse("March 30, 2003 03:00:00.000"),
                    DateUtils.round(date5, Calendar.HOUR_OF_DAY),
                    "round MET date across DST change-over");
            assertEquals(dateTimeParser.parse("March 30, 2003 03:00:00.000"),
                    DateUtils.round((Object) cal5, Calendar.HOUR_OF_DAY),
                    "round MET date across DST change-over");
            assertEquals(dateTimeParser.parse("March 30, 2003 03:00:00.000"),
                    DateUtils.round(date6, Calendar.HOUR_OF_DAY),
                    "round MET date across DST change-over");
            assertEquals(dateTimeParser.parse("March 30, 2003 03:00:00.000"),
                    DateUtils.round((Object) cal6, Calendar.HOUR_OF_DAY),
                    "round MET date across DST change-over");
            assertEquals(dateTimeParser.parse("March 30, 2003 04:00:00.000"),
                    DateUtils.round(date7, Calendar.HOUR_OF_DAY),
                    "round MET date across DST change-over");
            assertEquals(dateTimeParser.parse("March 30, 2003 04:00:00.000"),
                    DateUtils.round((Object) cal7, Calendar.HOUR_OF_DAY),
                    "round MET date across DST change-over");
        } finally {
            TimeZone.setDefault(defaultZone);
            dateTimeParser.setTimeZone(defaultZone);
        }
    }

    /**
     * Tests the Changes Made by LANG-346 to the DateUtils.modify() private method invoked
     * by DateUtils.round().
     *
     * @throws Exception so we don't have to catch it
     */
    @Test
    public void testRoundLang346() throws Exception {
        final Calendar testCalendar = Calendar.getInstance();
        testCalendar.set(2007, Calendar.JULY, 2, 8, 8, 50);
        Date date = testCalendar.getTime();
        assertEquals(dateTimeParser.parse("July 2, 2007 08:09:00.000"),
                DateUtils.round(date, Calendar.MINUTE),
                "Minute Round Up Failed");

        testCalendar.set(2007, Calendar.JULY, 2, 8, 8, 20);
        date = testCalendar.getTime();
        assertEquals(dateTimeParser.parse("July 2, 2007 08:08:00.000"),
                DateUtils.round(date, Calendar.MINUTE),
                "Minute No Round Failed");

        testCalendar.set(2007, Calendar.JULY, 2, 8, 8, 50);
        testCalendar.set(Calendar.MILLISECOND, 600);
        date = testCalendar.getTime();

        assertEquals(dateTimeParser.parse("July 2, 2007 08:08:51.000"),
                DateUtils.round(date, Calendar.SECOND),
                "Second Round Up with 600 Milli Seconds Failed");

        testCalendar.set(2007, Calendar.JULY, 2, 8, 8, 50);
        testCalendar.set(Calendar.MILLISECOND, 200);
        date = testCalendar.getTime();
        assertEquals(dateTimeParser.parse("July 2, 2007 08:08:50.000"),
                DateUtils.round(date, Calendar.SECOND),
                "Second Round Down with 200 Milli Seconds Failed");

        testCalendar.set(2007, Calendar.JULY, 2, 8, 8, 20);
        testCalendar.set(Calendar.MILLISECOND, 600);
        date = testCalendar.getTime();
        assertEquals(dateTimeParser.parse("July 2, 2007 08:08:21.000"),
                DateUtils.round(date, Calendar.SECOND),
                "Second Round Up with 200 Milli Seconds Failed");

        testCalendar.set(2007, Calendar.JULY, 2, 8, 8, 20);
        testCalendar.set(Calendar.MILLISECOND, 200);
        date = testCalendar.getTime();
        assertEquals(dateTimeParser.parse("July 2, 2007 08:08:20.000"),
                DateUtils.round(date, Calendar.SECOND),
                "Second Round Down with 200 Milli Seconds Failed");

        testCalendar.set(2007, Calendar.JULY, 2, 8, 8, 50);
        date = testCalendar.getTime();
        assertEquals(dateTimeParser.parse("July 2, 2007 08:00:00.000"),
                DateUtils.round(date, Calendar.HOUR),
                "Hour Round Down Failed");

        testCalendar.set(2007, Calendar.JULY, 2, 8, 31, 50);
        date = testCalendar.getTime();
        assertEquals(dateTimeParser.parse("July 2, 2007 09:00:00.000"),
                DateUtils.round(date, Calendar.HOUR),
                "Hour Round Up Failed");
    }

    @Test
    public void testSetDays() throws Exception {
        Date result = DateUtils.setDays(BASE_DATE, 1);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2000, 6, 1, 4, 3, 2, 1);

        result = DateUtils.setDays(BASE_DATE, 29);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2000, 6, 29, 4, 3, 2, 1);

        final String outsideOfRangeAssertionMessage = "DateUtils.setDays did not throw an expected IllegalArgumentException for amount outside of range 1 to 31.";
        assertThrows(
                IllegalArgumentException.class,
                () -> DateUtils.setDays(BASE_DATE, 32),
                outsideOfRangeAssertionMessage);
        assertThrows(
                IllegalArgumentException.class,
                () -> DateUtils.setDays(BASE_DATE, 0),
                outsideOfRangeAssertionMessage);

        assertThrows(NullPointerException.class, () -> DateUtils.setDays(null, 1));
    }

    @Test
    public void testSetHours() throws Exception {
        Date result = DateUtils.setHours(BASE_DATE, 0);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2000, 6, 5, 0, 3, 2, 1);

        result = DateUtils.setHours(BASE_DATE, 23);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2000, 6, 5, 23, 3, 2, 1);

        final String outsideOfRangeAssertionMessage = "DateUtils.setHours did not throw an expected IllegalArgumentException for amount outside of range 0 to 23.";
        assertThrows(
                IllegalArgumentException.class,
                () -> DateUtils.setHours(BASE_DATE, 24),
                outsideOfRangeAssertionMessage);
        assertThrows(
                IllegalArgumentException.class,
                () -> DateUtils.setHours(BASE_DATE, -1),
                outsideOfRangeAssertionMessage);

        assertThrows(NullPointerException.class, () -> DateUtils.setHours(null, 0));
    }

    @Test
    public void testSetMilliseconds() throws Exception {
        Date result = DateUtils.setMilliseconds(BASE_DATE, 0);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2000, 6, 5, 4, 3, 2, 0);

        result = DateUtils.setMilliseconds(BASE_DATE, 999);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2000, 6, 5, 4, 3, 2, 999);

        final String outsideOfRangeAssertionMessage = "DateUtils.setMilliseconds did not throw an expected IllegalArgumentException for range outside of 0 to 999.";
        assertThrows(
                IllegalArgumentException.class,
                () -> DateUtils.setMilliseconds(BASE_DATE, 1000),
                outsideOfRangeAssertionMessage);
        assertThrows(
                IllegalArgumentException.class,
                () -> DateUtils.setMilliseconds(BASE_DATE, -1),
                outsideOfRangeAssertionMessage);

        assertThrows(NullPointerException.class, () -> DateUtils.setMilliseconds(null, 0));
    }

    @Test
    public void testSetMinutes() throws Exception {
        Date result = DateUtils.setMinutes(BASE_DATE, 0);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2000, 6, 5, 4, 0, 2, 1);

        result = DateUtils.setMinutes(BASE_DATE, 59);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2000, 6, 5, 4, 59, 2, 1);

        final String outsideOfRangeAssertionMessage = "DateUtils.setMinutes did not throw an expected IllegalArgumentException for amount outside of range 0 to 59.";
        assertThrows(
                IllegalArgumentException.class,
                () -> DateUtils.setMinutes(BASE_DATE, 60),
                outsideOfRangeAssertionMessage);
        assertThrows(
                IllegalArgumentException.class,
                () -> DateUtils.setMinutes(BASE_DATE, -1),
                outsideOfRangeAssertionMessage);

        assertThrows(NullPointerException.class, () -> DateUtils.setMinutes(null, 0));
    }

    @Test
    public void testSetMonths() throws Exception {
        Date result = DateUtils.setMonths(BASE_DATE, 5);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2000, 5, 5, 4, 3, 2, 1);

        result = DateUtils.setMonths(BASE_DATE, 1);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2000, 1, 5, 4, 3, 2, 1);

        result = DateUtils.setMonths(BASE_DATE, 0);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2000, 0, 5, 4, 3, 2, 1);

        final String outsideOfRangeAssertionMessage = "DateUtils.setMonths did not throw an expected IllegalArgumentException for amount outside of range 0 to 11.";
        assertThrows(
                IllegalArgumentException.class,
                () -> DateUtils.setMonths(BASE_DATE, 12),
                outsideOfRangeAssertionMessage);
        assertThrows(
                IllegalArgumentException.class,
                () -> DateUtils.setMonths(BASE_DATE, -1),
                outsideOfRangeAssertionMessage);

        assertThrows(NullPointerException.class, () -> DateUtils.setMonths(null, 0));
    }

    @Test
    public void testSetSeconds() throws Exception {
        Date result = DateUtils.setSeconds(BASE_DATE, 0);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2000, 6, 5, 4, 3, 0, 1);

        result = DateUtils.setSeconds(BASE_DATE, 59);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2000, 6, 5, 4, 3, 59, 1);

        final String outsideOfRangeAssertionMessage = "DateUtils.setSeconds did not throw an expected IllegalArgumentException for amount outside of range 0 to 59.";
        assertThrows(
                IllegalArgumentException.class,
                () -> DateUtils.setSeconds(BASE_DATE, 60),
                outsideOfRangeAssertionMessage);
        assertThrows(
                IllegalArgumentException.class,
                () -> DateUtils.setSeconds(BASE_DATE, -1),
                outsideOfRangeAssertionMessage);

        assertThrows(NullPointerException.class, () -> DateUtils.setSeconds(null, 0));
    }

    @Test
    public void testSetYears() throws Exception {
        Date result = DateUtils.setYears(BASE_DATE, 2000);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2000, 6, 5, 4, 3, 2, 1);

        result = DateUtils.setYears(BASE_DATE, 2008);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2008, 6, 5, 4, 3, 2, 1);

        result = DateUtils.setYears(BASE_DATE, 2005);
        assertNotSame(BASE_DATE, result);
        assertDate(BASE_DATE, 2000, 6, 5, 4, 3, 2, 1);
        assertDate(result, 2005, 6, 5, 4, 3, 2, 1);

        assertThrows(NullPointerException.class, () -> DateUtils.setYears(null, 0));
    }

    @Test
    public void testToCalendar() {
        assertEquals(date1, DateUtils.toCalendar(date1).getTime(), "Failed to convert to a Calendar and back");
        assertThrows(NullPointerException.class, () -> DateUtils.toCalendar(null));
    }

    @Test
    public void testToCalendarWithDateAndTimeZoneNotNull() {
        final Calendar c = DateUtils.toCalendar(date2, defaultZone);
        assertEquals(date2, c.getTime(), "Convert Date and TimeZone to a Calendar, but failed to get the Date back");
        assertEquals(defaultZone, c.getTimeZone(), "Convert Date and TimeZone to a Calendar, but failed to get the TimeZone back");
    }

    @Test
    public void testToCalendarWithDateAndTimeZoneNull() {
        assertThrows(NullPointerException.class, () -> DateUtils.toCalendar(null, null));
    }

    @Test
    public void testToCalendarWithDateNull() {
        assertThrows(NullPointerException.class, () -> DateUtils.toCalendar(null, zone));
    }

    @Test
    public void testToCalendarWithTimeZoneNull() {
        assertThrows(NullPointerException.class, () -> DateUtils.toCalendar(date1, null));
    }

    /**
     * Tests various values with the trunc method
     *
     * @throws Exception so we don't have to catch it
     */
    @Test
    public void testTruncate() throws Exception {
        // tests public static Date truncate(Date date, int field)
        assertEquals(dateParser.parse("January 1, 2002"),
                DateUtils.truncate(date1, Calendar.YEAR),
                "truncate year-1 failed");
        assertEquals(dateParser.parse("January 1, 2001"),
                DateUtils.truncate(date2, Calendar.YEAR),
                "truncate year-2 failed");
        assertEquals(dateParser.parse("February 1, 2002"),
                DateUtils.truncate(date1, Calendar.MONTH),
                "truncate month-1 failed");
        assertEquals(dateParser.parse("November 1, 2001"),
                DateUtils.truncate(date2, Calendar.MONTH),
                "truncate month-2 failed");
        assertEquals(dateParser.parse("February 1, 2002"),
                DateUtils.truncate(date1, DateUtils.SEMI_MONTH),
                "truncate semimonth-1 failed");
        assertEquals(dateParser.parse("November 16, 2001"),
                DateUtils.truncate(date2, DateUtils.SEMI_MONTH),
                "truncate semimonth-2 failed");
        assertEquals(dateParser.parse("February 12, 2002"),
                DateUtils.truncate(date1, Calendar.DATE),
                "truncate date-1 failed");
        assertEquals(dateParser.parse("November 18, 2001"),
                DateUtils.truncate(date2, Calendar.DATE),
                "truncate date-2 failed");
        assertEquals(dateTimeParser.parse("February 12, 2002 12:00:00.000"),
                DateUtils.truncate(date1, Calendar.HOUR),
                "truncate hour-1 failed");
        assertEquals(dateTimeParser.parse("November 18, 2001 1:00:00.000"),
                DateUtils.truncate(date2, Calendar.HOUR),
                "truncate hour-2 failed");
        assertEquals(dateTimeParser.parse("February 12, 2002 12:34:00.000"),
                DateUtils.truncate(date1, Calendar.MINUTE),
                "truncate minute-1 failed");
        assertEquals(dateTimeParser.parse("November 18, 2001 1:23:00.000"),
                DateUtils.truncate(date2, Calendar.MINUTE),
                "truncate minute-2 failed");
        assertEquals(dateTimeParser.parse("February 12, 2002 12:34:56.000"),
                DateUtils.truncate(date1, Calendar.SECOND),
                "truncate second-1 failed");
        assertEquals(dateTimeParser.parse("November 18, 2001 1:23:11.000"),
                DateUtils.truncate(date2, Calendar.SECOND),
                "truncate second-2 failed");
        assertEquals(dateTimeParser.parse("February 3, 2002 00:00:00.000"),
                DateUtils.truncate(dateAmPm1, Calendar.AM_PM),
                "truncate ampm-1 failed");
        assertEquals(dateTimeParser.parse("February 3, 2002 00:00:00.000"),
                DateUtils.truncate(dateAmPm2, Calendar.AM_PM),
                "truncate ampm-2 failed");
        assertEquals(dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.truncate(dateAmPm3, Calendar.AM_PM),
                "truncate ampm-3 failed");
        assertEquals(dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.truncate(dateAmPm4, Calendar.AM_PM),
                "truncate ampm-4 failed");

        // tests public static Date truncate(Object date, int field)
        assertEquals(dateParser.parse("January 1, 2002"),
                DateUtils.truncate((Object) date1, Calendar.YEAR),
                "truncate year-1 failed");
        assertEquals(dateParser.parse("January 1, 2001"),
                DateUtils.truncate((Object) date2, Calendar.YEAR),
                "truncate year-2 failed");
        assertEquals(dateParser.parse("February 1, 2002"),
                DateUtils.truncate((Object) date1, Calendar.MONTH),
                "truncate month-1 failed");
        assertEquals(dateParser.parse("November 1, 2001"),
                DateUtils.truncate((Object) date2, Calendar.MONTH),
                "truncate month-2 failed");
        assertEquals(dateParser.parse("February 1, 2002"),
                DateUtils.truncate((Object) date1, DateUtils.SEMI_MONTH),
                "truncate semimonth-1 failed");
        assertEquals(dateParser.parse("November 16, 2001"),
                DateUtils.truncate((Object) date2, DateUtils.SEMI_MONTH),
                "truncate semimonth-2 failed");
        assertEquals(dateParser.parse("February 12, 2002"),
                DateUtils.truncate((Object) date1, Calendar.DATE),
                "truncate date-1 failed");
        assertEquals(dateParser.parse("November 18, 2001"),
                DateUtils.truncate((Object) date2, Calendar.DATE),
                "truncate date-2 failed");
        assertEquals(dateTimeParser.parse("February 12, 2002 12:00:00.000"),
                DateUtils.truncate((Object) date1, Calendar.HOUR),
                "truncate hour-1 failed");
        assertEquals(dateTimeParser.parse("November 18, 2001 1:00:00.000"),
                DateUtils.truncate((Object) date2, Calendar.HOUR),
                "truncate hour-2 failed");
        assertEquals(dateTimeParser.parse("February 12, 2002 12:34:00.000"),
                DateUtils.truncate((Object) date1, Calendar.MINUTE),
                "truncate minute-1 failed");
        assertEquals(dateTimeParser.parse("November 18, 2001 1:23:00.000"),
                DateUtils.truncate((Object) date2, Calendar.MINUTE),
                "truncate minute-2 failed");
        assertEquals(dateTimeParser.parse("February 12, 2002 12:34:56.000"),
                DateUtils.truncate((Object) date1, Calendar.SECOND),
                "truncate second-1 failed");
        assertEquals(dateTimeParser.parse("November 18, 2001 1:23:11.000"),
                DateUtils.truncate((Object) date2, Calendar.SECOND),
                "truncate second-2 failed");
        assertEquals(dateTimeParser.parse("February 3, 2002 00:00:00.000"),
                DateUtils.truncate((Object) dateAmPm1, Calendar.AM_PM),
                "truncate ampm-1 failed");
        assertEquals(dateTimeParser.parse("February 3, 2002 00:00:00.000"),
                DateUtils.truncate((Object) dateAmPm2, Calendar.AM_PM),
                "truncate ampm-2 failed");
        assertEquals(dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.truncate((Object) dateAmPm3, Calendar.AM_PM),
                "truncate ampm-3 failed");
        assertEquals(dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.truncate((Object) dateAmPm4, Calendar.AM_PM),
                "truncate ampm-4 failed");

        assertEquals(dateTimeParser.parse("February 12, 2002 12:34:56.000"),
                DateUtils.truncate((Object) cal1, Calendar.SECOND),
                "truncate calendar second-1 failed");
        assertEquals(dateTimeParser.parse("November 18, 2001 1:23:11.000"),
                DateUtils.truncate((Object) cal2, Calendar.SECOND),
                "truncate calendar second-2 failed");

        assertEquals(dateTimeParser.parse("February 3, 2002 00:00:00.000"),
                DateUtils.truncate((Object) calAmPm1, Calendar.AM_PM),
                "truncate ampm-1 failed");
        assertEquals(dateTimeParser.parse("February 3, 2002 00:00:00.000"),
                DateUtils.truncate((Object) calAmPm2, Calendar.AM_PM),
                "truncate ampm-2 failed");
        assertEquals(dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.truncate((Object) calAmPm3, Calendar.AM_PM),
                "truncate ampm-3 failed");
        assertEquals(dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.truncate((Object) calAmPm4, Calendar.AM_PM),
                "truncate ampm-4 failed");

        assertThrows(NullPointerException.class, () -> DateUtils.truncate((Date) null, Calendar.SECOND));
        assertThrows(NullPointerException.class, () -> DateUtils.truncate((Calendar) null, Calendar.SECOND));
        assertThrows(NullPointerException.class, () -> DateUtils.truncate((Object) null, Calendar.SECOND));
        assertThrows(ClassCastException.class, () -> DateUtils.truncate("", Calendar.SECOND));

        // Fix for https://issues.apache.org/bugzilla/show_bug.cgi?id=25560
        // Test truncate across beginning of daylight saving time
        try {
            TimeZone.setDefault(zone);
            dateTimeParser.setTimeZone(zone);
            assertEquals(dateTimeParser.parse("March 30, 2003 00:00:00.000"),
                    DateUtils.truncate(date3, Calendar.DATE),
                    "truncate MET date across DST change-over");
            assertEquals(dateTimeParser.parse("March 30, 2003 00:00:00.000"),
                    DateUtils.truncate((Object) cal3, Calendar.DATE),
                    "truncate MET date across DST change-over");
            // Test truncate across end of daylight saving time
            assertEquals(dateTimeParser.parse("October 26, 2003 00:00:00.000"),
                    DateUtils.truncate(date8, Calendar.DATE),
                    "truncate MET date across DST change-over");
            assertEquals(dateTimeParser.parse("October 26, 2003 00:00:00.000"),
                    DateUtils.truncate((Object) cal8, Calendar.DATE),
                    "truncate MET date across DST change-over");
        } finally {
            TimeZone.setDefault(defaultZone);
            dateTimeParser.setTimeZone(defaultZone);
        }

        // Bug 31395, large dates
        final Date endOfTime = new Date(Long.MAX_VALUE); // fyi: Sun Aug 17 07:12:55 CET 292278994 -- 807 millis
        final GregorianCalendar endCal = new GregorianCalendar();
        endCal.setTime(endOfTime);
        assertThrows(ArithmeticException.class, () -> DateUtils.truncate(endCal, Calendar.DATE));
        endCal.set(Calendar.YEAR, 280000001);
        assertThrows(ArithmeticException.class, () -> DateUtils.truncate(endCal, Calendar.DATE));
        endCal.set(Calendar.YEAR, 280000000);
        final Calendar cal = DateUtils.truncate(endCal, Calendar.DATE);
        assertEquals(0, cal.get(Calendar.HOUR));
    }

    /**
     * Tests for LANG-59
     *
     * see https://issues.apache.org/jira/browse/LANG-59
     */
    @Test
    public void testTruncateLang59() {
        try {
            // Set TimeZone to Mountain Time
            final TimeZone denverZone = TimeZone.getTimeZone("America/Denver");
            TimeZone.setDefault(denverZone);
            final DateFormat format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS XXX");
            format.setTimeZone(denverZone);

            final Date oct31_01MDT = new Date(1099206000000L);

            final Date oct31MDT = new Date(oct31_01MDT.getTime() - 3600000L); // - 1 hour
            final Date oct31_01_02MDT = new Date(oct31_01MDT.getTime() + 120000L);  // + 2 minutes
            final Date oct31_01_02_03MDT = new Date(oct31_01_02MDT.getTime() + 3000L);    // + 3 seconds
            final Date oct31_01_02_03_04MDT = new Date(oct31_01_02_03MDT.getTime() + 4L);       // + 4 milliseconds

            assertEquals("2004-10-31 00:00:00.000 -06:00", format.format(oct31MDT), "Check 00:00:00.000");
            assertEquals("2004-10-31 01:00:00.000 -06:00", format.format(oct31_01MDT), "Check 01:00:00.000");
            assertEquals("2004-10-31 01:02:00.000 -06:00", format.format(oct31_01_02MDT), "Check 01:02:00.000");
            assertEquals("2004-10-31 01:02:03.000 -06:00", format.format(oct31_01_02_03MDT), "Check 01:02:03.000");
            assertEquals("2004-10-31 01:02:03.004 -06:00", format.format(oct31_01_02_03_04MDT), "Check 01:02:03.004");

            // ------- Demonstrate Problem -------
            final Calendar gval = Calendar.getInstance();
            gval.setTime(new Date(oct31_01MDT.getTime()));
            gval.set(Calendar.MINUTE, gval.get(Calendar.MINUTE)); // set minutes to the same value
            assertEquals(gval.getTime().getTime(), oct31_01MDT.getTime() + 3600000L, "Demonstrate Problem");

            // ---------- Test Truncate ----------
            assertEquals(oct31_01_02_03_04MDT, DateUtils.truncate(oct31_01_02_03_04MDT, Calendar.MILLISECOND),
                    "Truncate Calendar.MILLISECOND");

            assertEquals(oct31_01_02_03MDT, DateUtils.truncate(oct31_01_02_03_04MDT, Calendar.SECOND),
                    "Truncate Calendar.SECOND");

            assertEquals(oct31_01_02MDT, DateUtils.truncate(oct31_01_02_03_04MDT, Calendar.MINUTE),
                    "Truncate Calendar.MINUTE");

            assertEquals(oct31_01MDT, DateUtils.truncate(oct31_01_02_03_04MDT, Calendar.HOUR_OF_DAY),
                    "Truncate Calendar.HOUR_OF_DAY");

            assertEquals(oct31_01MDT, DateUtils.truncate(oct31_01_02_03_04MDT, Calendar.HOUR),
                    "Truncate Calendar.HOUR");

            assertEquals(oct31MDT, DateUtils.truncate(oct31_01_02_03_04MDT, Calendar.DATE),
                    "Truncate Calendar.DATE");

            // ---------- Test Round (down) ----------
            assertEquals(oct31_01_02_03_04MDT, DateUtils.round(oct31_01_02_03_04MDT, Calendar.MILLISECOND),
                    "Round Calendar.MILLISECOND");

            assertEquals(oct31_01_02_03MDT, DateUtils.round(oct31_01_02_03_04MDT, Calendar.SECOND),
                    "Round Calendar.SECOND");

            assertEquals(oct31_01_02MDT, DateUtils.round(oct31_01_02_03_04MDT, Calendar.MINUTE),
                    "Round Calendar.MINUTE");

            assertEquals(oct31_01MDT, DateUtils.round(oct31_01_02_03_04MDT, Calendar.HOUR_OF_DAY),
                    "Round Calendar.HOUR_OF_DAY");

            assertEquals(oct31_01MDT, DateUtils.round(oct31_01_02_03_04MDT, Calendar.HOUR),
                    "Round Calendar.HOUR");

            assertEquals(oct31MDT, DateUtils.round(oct31_01_02_03_04MDT, Calendar.DATE),
                    "Round Calendar.DATE");
        } finally {
            // restore default time zone
            TimeZone.setDefault(defaultZone);
        }
    }

    /**
     * Tests the calendar iterator for week ranges
     */
    @Test
    public void testWeekIterator() {
        final Calendar now = Calendar.getInstance();
        for (int i = 0; i< 7; i++) {
            final Calendar today = DateUtils.truncate(now, Calendar.DATE);
            final Calendar sunday = DateUtils.truncate(now, Calendar.DATE);
            sunday.add(Calendar.DATE, 1 - sunday.get(Calendar.DAY_OF_WEEK));
            final Calendar monday = DateUtils.truncate(now, Calendar.DATE);
            if (monday.get(Calendar.DAY_OF_WEEK) == 1) {
                //This is sunday... roll back 6 days
                monday.add(Calendar.DATE, -6);
            } else {
                monday.add(Calendar.DATE, 2 - monday.get(Calendar.DAY_OF_WEEK));
            }
            final Calendar centered = DateUtils.truncate(now, Calendar.DATE);
            centered.add(Calendar.DATE, -3);

            Iterator<?> it = DateUtils.iterator(now, DateUtils.RANGE_WEEK_SUNDAY);
            assertWeekIterator(it, sunday);
            it = DateUtils.iterator(now, DateUtils.RANGE_WEEK_MONDAY);
            assertWeekIterator(it, monday);
            it = DateUtils.iterator(now, DateUtils.RANGE_WEEK_RELATIVE);
            assertWeekIterator(it, today);
            it = DateUtils.iterator(now, DateUtils.RANGE_WEEK_CENTER);
            assertWeekIterator(it, centered);

            it = DateUtils.iterator((Object) now, DateUtils.RANGE_WEEK_CENTER);
            assertWeekIterator(it, centered);
            final Iterator<?> it2 = DateUtils.iterator((Object) now.getTime(), DateUtils.RANGE_WEEK_CENTER);
            assertWeekIterator(it2, centered);
            assertThrows(NoSuchElementException.class, it2::next);
            final Iterator<?> it3 = DateUtils.iterator(now, DateUtils.RANGE_WEEK_CENTER);
            it3.next();
            assertThrows(UnsupportedOperationException.class, it3::remove);

            now.add(Calendar.DATE, 1);
        }
    }

}

