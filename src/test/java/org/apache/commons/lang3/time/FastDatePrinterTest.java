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
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.Serializable;
import java.text.FieldPosition;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Locale;
import java.util.TimeZone;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.SerializationUtils;
import org.junit.jupiter.api.Test;
import org.junitpioneer.jupiter.DefaultLocale;
import org.junitpioneer.jupiter.DefaultTimeZone;

/**
 * Unit tests {@link org.apache.commons.lang3.time.FastDatePrinter}.
 *
 * @since 3.0
 */
public class FastDatePrinterTest extends AbstractLangTest {

    private enum Expected1806 {
        India(INDIA, "+05", "+0530", "+05:30"), Greenwich(TimeZones.GMT, "Z", "Z", "Z"), NewYork(
                NEW_YORK, "-05", "-0500", "-05:00");

        final TimeZone zone;

        final String one;
        final String two;
        final String three;
        Expected1806(final TimeZone zone, final String one, final String two, final String three) {
            this.zone = zone;
            this.one = one;
            this.two = two;
            this.three = three;
        }
    }
    private static final String YYYY_MM_DD = "yyyy/MM/dd";
    private static final TimeZone NEW_YORK = TimeZone.getTimeZone("America/New_York");
    private static final TimeZone INDIA = TimeZone.getTimeZone("Asia/Calcutta");

    private static final Locale SWEDEN = new Locale("sv", "SE");

    private static Calendar initializeCalendar(final TimeZone tz) {
        final Calendar cal = Calendar.getInstance(tz);
        cal.set(Calendar.YEAR, 2001);
        cal.set(Calendar.MONTH, 1); // not daylight savings
        cal.set(Calendar.DAY_OF_MONTH, 4);
        cal.set(Calendar.HOUR_OF_DAY, 12);
        cal.set(Calendar.MINUTE, 8);
        cal.set(Calendar.SECOND, 56);
        cal.set(Calendar.MILLISECOND, 235);
        return cal;
    }

    private DatePrinter getDateInstance(final int dateStyle, final Locale locale) {
        return getInstance(FormatCache.getPatternForStyle(Integer.valueOf(dateStyle), null, locale), TimeZone.getDefault(), Locale.getDefault());
    }

    DatePrinter getInstance(final String format) {
        return getInstance(format, TimeZone.getDefault(), Locale.getDefault());
    }

    private DatePrinter getInstance(final String format, final Locale locale) {
        return getInstance(format, TimeZone.getDefault(), locale);
    }

    private DatePrinter getInstance(final String format, final TimeZone timeZone) {
        return getInstance(format, timeZone, Locale.getDefault());
    }

    /**
     * Override this method in derived tests to change the construction of instances
     * @param format the format string to use
     * @param timeZone the time zone to use
     * @param locale the locale to use
     * @return the DatePrinter to use for testing
     */
    protected DatePrinter getInstance(final String format, final TimeZone timeZone, final Locale locale) {
        return new FastDatePrinter(format, timeZone, locale);
    }

    @Test
    public void test1806() {
        for (final Expected1806 trial : Expected1806.values()) {
            final Calendar cal = initializeCalendar(trial.zone);

            DatePrinter printer = getInstance("X", trial.zone);
            assertEquals(trial.one, printer.format(cal));

            printer = getInstance("XX", trial.zone);
            assertEquals(trial.two, printer.format(cal));

            printer = getInstance("XXX", trial.zone);
            assertEquals(trial.three, printer.format(cal));
        }
    }
    @Test
    public void test1806Argument() {
        assertThrows(IllegalArgumentException.class, () -> getInstance("XXXX"));
    }

    @Test
    public void testAppendableOptions() {
        final DatePrinter format = getInstance("yyyy-MM-dd HH:mm:ss.SSS Z", TimeZones.GMT);
        final Calendar calendar = Calendar.getInstance();
        final StringBuilder sb = new StringBuilder();
        final String expected = format.format(calendar, sb).toString();
        sb.setLength(0);

        final Date date = calendar.getTime();
        assertEquals(expected, format.format(date, sb).toString());
        sb.setLength(0);

        final long epoch = date.getTime();
        assertEquals(expected, format.format(epoch, sb).toString());
    }

    @Test
    public void testDayNumberOfWeek() {
        final DatePrinter printer = getInstance("u");
        final Calendar calendar = Calendar.getInstance();

        calendar.set(Calendar.DAY_OF_WEEK, Calendar.MONDAY);
        assertEquals("1", printer.format(calendar.getTime()));

        calendar.set(Calendar.DAY_OF_WEEK, Calendar.SATURDAY);
        assertEquals("6", printer.format(calendar.getTime()));

        calendar.set(Calendar.DAY_OF_WEEK, Calendar.SUNDAY);
        assertEquals("7", printer.format(calendar.getTime()));
    }

    @Test
    public void testEquals() {
        final DatePrinter printer1= getInstance(YYYY_MM_DD);
        final DatePrinter printer2= getInstance(YYYY_MM_DD);

        assertEquals(printer1, printer2);
        assertEquals(printer1.hashCode(), printer2.hashCode());

        assertNotEquals(printer1, new Object());
    }

    @DefaultLocale(language = "en", country = "US")
    @DefaultTimeZone("America/New_York")
    @Test
    public void testFormat() {
        final GregorianCalendar cal1 = new GregorianCalendar(2003, 0, 10, 15, 33, 20);
        final GregorianCalendar cal2 = new GregorianCalendar(2003, 6, 10, 9, 0, 0);
        final Date date1 = cal1.getTime();
        final Date date2 = cal2.getTime();
        final long millis1 = date1.getTime();
        final long millis2 = date2.getTime();

        DatePrinter fdf = getInstance("yyyy-MM-dd'T'HH:mm:ss");
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
        assertEquals(sdf.format(date1), fdf.format(date1));
        assertEquals("2003-01-10T15:33:20", fdf.format(date1));
        assertEquals("2003-01-10T15:33:20", fdf.format(cal1));
        assertEquals("2003-01-10T15:33:20", fdf.format(millis1));
        assertEquals("2003-07-10T09:00:00", fdf.format(date2));
        assertEquals("2003-07-10T09:00:00", fdf.format(cal2));
        assertEquals("2003-07-10T09:00:00", fdf.format(millis2));

        fdf = getInstance("Z");
        assertEquals("-0500", fdf.format(date1));
        assertEquals("-0500", fdf.format(cal1));
        assertEquals("-0500", fdf.format(millis1));

        assertEquals("-0400", fdf.format(date2));
        assertEquals("-0400", fdf.format(cal2));
        assertEquals("-0400", fdf.format(millis2));

        fdf = getInstance("ZZ");
        assertEquals("-05:00", fdf.format(date1));
        assertEquals("-05:00", fdf.format(cal1));
        assertEquals("-05:00", fdf.format(millis1));

        assertEquals("-04:00", fdf.format(date2));
        assertEquals("-04:00", fdf.format(cal2));
        assertEquals("-04:00", fdf.format(millis2));

        final String pattern = "GGGG GGG GG G yyyy yyy yy y MMMM MMM MM M LLLL LLL LL L" +
                " dddd ddd dd d DDDD DDD DD D EEEE EEE EE E aaaa aaa aa a zzzz zzz zz z";
        fdf = getInstance(pattern);
        sdf = new SimpleDateFormat(pattern);
        // SDF bug fix starting with Java 7
        assertEquals(sdf.format(date1).replace("2003 03 03 03", "2003 2003 03 2003"), fdf.format(date1));
        assertEquals(sdf.format(date2).replace("2003 03 03 03", "2003 2003 03 2003"), fdf.format(date2));
    }

    @Test
    public void testHourFormats() {
        final Calendar calendar = Calendar.getInstance();
        calendar.clear();
        final DatePrinter printer = getInstance("K k H h");

        calendar.set(Calendar.HOUR_OF_DAY, 0);
        assertEquals("0 24 0 12", printer.format(calendar));

        calendar.set(Calendar.HOUR_OF_DAY, 12);
        assertEquals("0 12 12 12", printer.format(calendar));

        calendar.set(Calendar.HOUR_OF_DAY, 23);
        assertEquals("11 23 23 11", printer.format(calendar));
    }

    @Test
    public void testLang1103() {
        final Calendar cal = Calendar.getInstance(SWEDEN);
        cal.set(Calendar.DAY_OF_MONTH, 2);

        assertEquals("2", getInstance("d", SWEDEN).format(cal));
        assertEquals("02", getInstance("dd", SWEDEN).format(cal));
        assertEquals("002", getInstance("ddd", SWEDEN).format(cal));
        assertEquals("0002", getInstance("dddd", SWEDEN).format(cal));
        assertEquals("00002", getInstance("ddddd", SWEDEN).format(cal));
    }

    @Test
    public void testLang303() {
        final Calendar cal = Calendar.getInstance();
        cal.set(2004, Calendar.DECEMBER, 31);

        DatePrinter format = getInstance(YYYY_MM_DD);
        final String output = format.format(cal);

        format = SerializationUtils.deserialize(SerializationUtils.serialize((Serializable) format));
        assertEquals(output, format.format(cal));
    }

    @Test
    public void testLang538() {
        // more commonly constructed with: cal = new GregorianCalendar(2009, 9, 16, 8, 42, 16)
        // for the unit test to work in any time zone, constructing with GMT-8 rather than default locale time zone
        final GregorianCalendar cal = new GregorianCalendar(TimeZone.getTimeZone("GMT-8"));
        cal.clear();
        cal.set(2009, Calendar.OCTOBER, 16, 8, 42, 16);

        final DatePrinter format = getInstance("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'", TimeZones.GMT);
        assertEquals("2009-10-16T16:42:16.000Z", format.format(cal.getTime()), "dateTime");
        assertEquals("2009-10-16T16:42:16.000Z", format.format(cal), "dateTime");
    }

    @Test
    public void testLang645() {
        final Locale locale = new Locale("sv", "SE");

        final Calendar cal = Calendar.getInstance();
        cal.set(2010, Calendar.JANUARY, 1, 12, 0, 0);
        final Date d = cal.getTime();

        final DatePrinter fdf = getInstance("EEEE', week 'ww", locale);

        assertEquals("fredag, week 53", fdf.format(d));
    }

    /**
     * According to LANG-916 (https://issues.apache.org/jira/browse/LANG-916),
     * the format method did contain a bug: it did not use the TimeZone data.
     *
     * This method test that the bug is fixed.
     */
    @Test
    public void testLang916() {

        final Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("Europe/Paris"));
        cal.clear();
        cal.set(2009, 9, 16, 8, 42, 16);

        // calendar fast.
        {
            final String value = FastDateFormat.getInstance("yyyy-MM-dd'T'HH:mm:ss Z", TimeZone.getTimeZone("Europe/Paris")).format(cal);
            assertEquals("2009-10-16T08:42:16 +0200", value, "calendar");
        }
        {
            final String value = FastDateFormat.getInstance("yyyy-MM-dd'T'HH:mm:ss Z", TimeZone.getTimeZone("Asia/Kolkata")).format(cal);
            assertEquals("2009-10-16T12:12:16 +0530", value, "calendar");
        }
        {
            final String value = FastDateFormat.getInstance("yyyy-MM-dd'T'HH:mm:ss Z", TimeZone.getTimeZone("Europe/London")).format(cal);
            assertEquals("2009-10-16T07:42:16 +0100", value, "calendar");
        }
    }

    @Test
    public void testLocaleMatches() {
        final DatePrinter printer= getInstance(YYYY_MM_DD, SWEDEN);
        assertEquals(SWEDEN, printer.getLocale());
    }

    /**
     * Tests that pre-1000AD years get padded with yyyy
     */
    @Test
    public void testLowYearPadding() {
        final Calendar cal = Calendar.getInstance();
        final DatePrinter format = getInstance(YYYY_MM_DD);

        cal.set(1, Calendar.JANUARY, 1);
        assertEquals("0001/01/01", format.format(cal));
        cal.set(10, Calendar.JANUARY, 1);
        assertEquals("0010/01/01", format.format(cal));
        cal.set(100, Calendar.JANUARY, 1);
        assertEquals("0100/01/01", format.format(cal));
        cal.set(999, Calendar.JANUARY, 1);
        assertEquals("0999/01/01", format.format(cal));
    }

    /**
     * Show Bug #39410 is solved
     */
    @Test
    public void testMilleniumBug() {
        final Calendar cal = Calendar.getInstance();
        final DatePrinter format = getInstance("dd.MM.yyyy");

        cal.set(1000, Calendar.JANUARY, 1);
        assertEquals("01.01.1000", format.format(cal));
    }

    @Test
    public void testPatternMatches() {
        final DatePrinter printer= getInstance(YYYY_MM_DD);
        assertEquals(YYYY_MM_DD, printer.getPattern());
    }

    /**
     * Test case for {@link FastDateParser#FastDateParser(String, TimeZone, Locale)}.
     */
    @Test
    public void testShortDateStyleWithLocales() {
        final Locale usLocale = Locale.US;
        final Locale swedishLocale = new Locale("sv", "SE");
        final Calendar cal = Calendar.getInstance();
        cal.set(2004, Calendar.FEBRUARY, 3);
        DatePrinter fdf = getDateInstance(FastDateFormat.SHORT, usLocale);
        assertEquals("2/3/04", fdf.format(cal));

        fdf = getDateInstance(FastDateFormat.SHORT, swedishLocale);
        assertEquals("2004-02-03", fdf.format(cal));

    }

    /**
     * testLowYearPadding showed that the date was buggy
     * This test confirms it, getting 366 back as a date
     */
    @Test
    public void testSimpleDate() {
        final Calendar cal = Calendar.getInstance();
        final DatePrinter format = getInstance(YYYY_MM_DD);

        cal.set(2004, Calendar.DECEMBER, 31);
        assertEquals("2004/12/31", format.format(cal));
        cal.set(999, Calendar.DECEMBER, 31);
        assertEquals("0999/12/31", format.format(cal));
        cal.set(1, Calendar.MARCH, 2);
        assertEquals("0001/03/02", format.format(cal));
    }

    @SuppressWarnings("deprecation")
    @Test
    public void testStringBufferOptions() {
        final DatePrinter format = getInstance("yyyy-MM-dd HH:mm:ss.SSS Z", TimeZones.GMT);
        final Calendar calendar = Calendar.getInstance();
        final StringBuffer sb = new StringBuffer();
        final String expected = format.format(calendar, sb, new FieldPosition(0)).toString();
        sb.setLength(0);
        assertEquals(expected, format.format(calendar, sb).toString());
        sb.setLength(0);

        final Date date = calendar.getTime();
        assertEquals(expected, format.format(date, sb, new FieldPosition(0)).toString());
        sb.setLength(0);
        assertEquals(expected, format.format(date, sb).toString());
        sb.setLength(0);

        final long epoch = date.getTime();
        assertEquals(expected, format.format(epoch, sb, new FieldPosition(0)).toString());
        sb.setLength(0);
        assertEquals(expected, format.format(epoch, sb).toString());
    }

    @DefaultTimeZone("UTC")
    @Test
    public void testTimeZoneAsZ() {
        final Calendar c = Calendar.getInstance(FastTimeZone.getGmtTimeZone());
        final FastDateFormat noColonFormat = FastDateFormat.getInstance("Z");
        assertEquals("+0000", noColonFormat.format(c));

        final FastDateFormat isoFormat = FastDateFormat.getInstance("ZZ");
        assertEquals("Z", isoFormat.format(c));

        final FastDateFormat colonFormat = FastDateFormat.getInstance("ZZZ");
        assertEquals("+00:00", colonFormat.format(c));
    }

    @Test
    public void testTimeZoneMatches() {
        final DatePrinter printer= getInstance(YYYY_MM_DD, NEW_YORK);
        assertEquals(NEW_YORK, printer.getTimeZone());
    }

    @Test
    public void testToStringContainsName() {
        final DatePrinter printer= getInstance(YYYY_MM_DD);
        assertTrue(printer.toString().startsWith("FastDate"));
    }

    @DefaultLocale(language = "en", country = "US")
    @DefaultTimeZone("America/New_York")
    @Test
    public void testWeekYear() {
        final GregorianCalendar cal = new GregorianCalendar(2020, 12, 31, 0, 0, 0);
        final DatePrinter printer4Digits = getInstance("YYYY");
        final DatePrinter printer4DigitsFallback = getInstance("YYY");
        final DatePrinter printer2Digits = getInstance("YY");
        final DatePrinter printer4DigitAnotherFallback = getInstance("Y");
        assertEquals("2021", printer4Digits.format(cal));
        assertEquals("2021", printer4DigitsFallback.format(cal));
        assertEquals("2021", printer4DigitAnotherFallback.format(cal));
        assertEquals("21", printer2Digits.format(cal));
    }
}
