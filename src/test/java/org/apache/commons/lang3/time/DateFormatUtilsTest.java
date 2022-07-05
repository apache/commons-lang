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
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.text.ParseException;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;
import org.junitpioneer.jupiter.DefaultLocale;
import org.junitpioneer.jupiter.DefaultTimeZone;

/**
 * TestCase for DateFormatUtils.
 */
@SuppressWarnings("deprecation") // tests lots of deprecated items
public class DateFormatUtilsTest extends AbstractLangTest {
    private void assertFormats(final String expectedValue, final String pattern, final TimeZone timeZone, final Calendar cal) {
        assertEquals(expectedValue, DateFormatUtils.format(cal.getTime(), pattern, timeZone));
        assertEquals(expectedValue, DateFormatUtils.format(cal.getTime().getTime(), pattern, timeZone));
        assertEquals(expectedValue, DateFormatUtils.format(cal, pattern, timeZone));
    }

    private Calendar createFebruaryTestDate(final TimeZone timeZone) {
        final Calendar cal = Calendar.getInstance(timeZone);
        cal.set(2002, Calendar.FEBRUARY, 23, 9, 11, 12);
        return cal;
    }

    private Calendar createJuneTestDate(final TimeZone timeZone) {
        final Calendar cal = Calendar.getInstance(timeZone);
        cal.set(2003, Calendar.JUNE, 8, 10, 11, 12);
        return cal;
    }

    @Test
    public void testConstructor() {
        assertNotNull(new DateFormatUtils());
        final Constructor<?>[] cons = DateFormatUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertTrue(Modifier.isPublic(cons[0].getModifiers()));
        assertTrue(Modifier.isPublic(DateFormatUtils.class.getModifiers()));
        assertFalse(Modifier.isFinal(DateFormatUtils.class.getModifiers()));
    }

    @Test
    public void testDateISO() {
        testGmtMinus3("2002-02-23", DateFormatUtils.ISO_DATE_FORMAT.getPattern());
        testGmtMinus3("2002-02-23-03:00", DateFormatUtils.ISO_DATE_TIME_ZONE_FORMAT.getPattern());
        testUTC("2002-02-23Z", DateFormatUtils.ISO_DATE_TIME_ZONE_FORMAT.getPattern());
    }

    @Test
    public void testDateTimeISO() {
        testGmtMinus3("2002-02-23T09:11:12", DateFormatUtils.ISO_DATETIME_FORMAT.getPattern());
        testGmtMinus3("2002-02-23T09:11:12-03:00", DateFormatUtils.ISO_DATETIME_TIME_ZONE_FORMAT.getPattern());
        testUTC("2002-02-23T09:11:12Z", DateFormatUtils.ISO_DATETIME_TIME_ZONE_FORMAT.getPattern());
    }

    @Test
    public void testFormat() {
        final Calendar c = Calendar.getInstance(FastTimeZone.getGmtTimeZone());
        c.set(2005, Calendar.JANUARY, 1, 12, 0, 0);
        c.setTimeZone(TimeZone.getDefault());
        final StringBuilder buffer = new StringBuilder ();
        final int year = c.get(Calendar.YEAR);
        final int month = c.get(Calendar.MONTH) + 1;
        final int day = c.get(Calendar.DAY_OF_MONTH);
        final int hour = c.get(Calendar.HOUR_OF_DAY);
        buffer.append (year);
        buffer.append(month);
        buffer.append(day);
        buffer.append(hour);
        assertEquals(buffer.toString(), DateFormatUtils.format(c.getTime(), "yyyyMdH"));

        assertEquals(buffer.toString(), DateFormatUtils.format(c.getTime().getTime(), "yyyyMdH"));

        assertEquals(buffer.toString(), DateFormatUtils.format(c.getTime(), "yyyyMdH", Locale.US));

        assertEquals(buffer.toString(), DateFormatUtils.format(c.getTime().getTime(), "yyyyMdH", Locale.US));
    }

    @Test
    public void testFormatCalendar() {
        final Calendar c = Calendar.getInstance(FastTimeZone.getGmtTimeZone());
        c.set(2005, Calendar.JANUARY, 1, 12, 0, 0);
        c.setTimeZone(TimeZone.getDefault());
        final StringBuilder buffer = new StringBuilder ();
        final int year = c.get(Calendar.YEAR);
        final int month = c.get(Calendar.MONTH) + 1;
        final int day = c.get(Calendar.DAY_OF_MONTH);
        final int hour = c.get(Calendar.HOUR_OF_DAY);
        buffer.append (year);
        buffer.append(month);
        buffer.append(day);
        buffer.append(hour);
        assertEquals(buffer.toString(), DateFormatUtils.format(c, "yyyyMdH"));

        assertEquals(buffer.toString(), DateFormatUtils.format(c.getTime(), "yyyyMdH"));

        assertEquals(buffer.toString(), DateFormatUtils.format(c, "yyyyMdH", Locale.US));

        assertEquals(buffer.toString(), DateFormatUtils.format(c.getTime(), "yyyyMdH", Locale.US));
    }

    @Test
    public void testFormatUTC() {
        final Calendar c = Calendar.getInstance(FastTimeZone.getGmtTimeZone());
        c.set(2005, Calendar.JANUARY, 1, 12, 0, 0);
        assertEquals ("2005-01-01T12:00:00", DateFormatUtils.formatUTC(c.getTime(), DateFormatUtils.ISO_DATETIME_FORMAT.getPattern()));

        assertEquals ("2005-01-01T12:00:00", DateFormatUtils.formatUTC(c.getTime().getTime(), DateFormatUtils.ISO_DATETIME_FORMAT.getPattern()));

        assertEquals ("2005-01-01T12:00:00", DateFormatUtils.formatUTC(c.getTime(), DateFormatUtils.ISO_DATETIME_FORMAT.getPattern(), Locale.US));

        assertEquals ("2005-01-01T12:00:00", DateFormatUtils.formatUTC(c.getTime().getTime(), DateFormatUtils.ISO_DATETIME_FORMAT.getPattern(), Locale.US));
    }

    private void testGmtMinus3(final String expectedValue, final String pattern) {
        final TimeZone timeZone = TimeZone.getTimeZone("GMT-3");
        assertFormats(expectedValue, pattern, timeZone, createFebruaryTestDate(timeZone));
    }

    @Test
    public void testLANG1000() throws Exception {
        final String date = "2013-11-18T12:48:05Z";
        DateFormatUtils.ISO_DATETIME_TIME_ZONE_FORMAT.parse(date);
    }

    @Test
    public void testLANG1462() {
        final TimeZone timeZone = TimeZone.getTimeZone("GMT-3");
        final Calendar calendar = createJuneTestDate(timeZone);
        assertEquals("20030608101112", DateFormatUtils.format(calendar, "yyyyMMddHHmmss"));
        calendar.setTimeZone(TimeZone.getTimeZone("JST"));
        assertEquals("20030608221112", DateFormatUtils.format(calendar, "yyyyMMddHHmmss"));
    }

    @DefaultTimeZone("UTC")
    @Test
    public void testLang530() throws ParseException {
        final Date d = new Date();
        final String isoDateStr = DateFormatUtils.ISO_DATETIME_TIME_ZONE_FORMAT.format(d);
        final Date d2 = DateUtils.parseDate(isoDateStr, DateFormatUtils.ISO_DATETIME_TIME_ZONE_FORMAT.getPattern());
        // the format loses milliseconds so have to reintroduce them
        assertEquals(d.getTime(), d2.getTime() + d.getTime() % 1000, "Date not equal to itself ISO formatted and parsed");
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

        // Long.
        {
            final String value = DateFormatUtils.format(cal.getTimeInMillis(), DateFormatUtils.ISO_DATETIME_TIME_ZONE_FORMAT.getPattern(), TimeZone.getTimeZone("Europe/Paris"));
            assertEquals("2009-10-16T08:42:16+02:00", value, "long");
        }
        {
            final String value = DateFormatUtils.format(cal.getTimeInMillis(), DateFormatUtils.ISO_DATETIME_TIME_ZONE_FORMAT.getPattern(), TimeZone.getTimeZone("Asia/Kolkata"));
            assertEquals("2009-10-16T12:12:16+05:30", value, "long");
        }
        {
            final String value = DateFormatUtils.format(cal.getTimeInMillis(), DateFormatUtils.ISO_DATETIME_TIME_ZONE_FORMAT.getPattern(), TimeZone.getTimeZone("Europe/London"));
            assertEquals("2009-10-16T07:42:16+01:00", value, "long");
        }

        // Calendar.
        {
            final String value = DateFormatUtils.format(cal, DateFormatUtils.ISO_DATETIME_TIME_ZONE_FORMAT.getPattern(), TimeZone.getTimeZone("Europe/Paris"));
            assertEquals("2009-10-16T08:42:16+02:00", value, "calendar");
        }
        {
            final String value = DateFormatUtils.format(cal, DateFormatUtils.ISO_DATETIME_TIME_ZONE_FORMAT.getPattern(), TimeZone.getTimeZone("Asia/Kolkata"));
            assertEquals("2009-10-16T12:12:16+05:30", value, "calendar");
        }
        {
            final String value = DateFormatUtils.format(cal, DateFormatUtils.ISO_DATETIME_TIME_ZONE_FORMAT.getPattern(), TimeZone.getTimeZone("Europe/London"));
            assertEquals("2009-10-16T07:42:16+01:00", value, "calendar");
        }
    }

    @DefaultLocale(language = "en")
    @Test
    public void testSMTP() {
        TimeZone timeZone = TimeZone.getTimeZone("GMT-3");
        Calendar june = createJuneTestDate(timeZone);

        assertFormats("Sun, 08 Jun 2003 10:11:12 -0300", DateFormatUtils.SMTP_DATETIME_FORMAT.getPattern(),
                timeZone, june);

        timeZone = FastTimeZone.getGmtTimeZone();
        june = createJuneTestDate(timeZone);
        assertFormats("Sun, 08 Jun 2003 10:11:12 +0000", DateFormatUtils.SMTP_DATETIME_FORMAT.getPattern(),
                timeZone, june);
    }

    @Test
    public void testTimeISO() {
        testGmtMinus3("T09:11:12", DateFormatUtils.ISO_TIME_FORMAT.getPattern());
        testGmtMinus3("T09:11:12-03:00", DateFormatUtils.ISO_TIME_TIME_ZONE_FORMAT.getPattern());
        testUTC("T09:11:12Z", DateFormatUtils.ISO_TIME_TIME_ZONE_FORMAT.getPattern());
    }

    @Test
    public void testTimeNoTISO() {
        testGmtMinus3("09:11:12", DateFormatUtils.ISO_TIME_NO_T_FORMAT.getPattern());
        testGmtMinus3("09:11:12-03:00", DateFormatUtils.ISO_TIME_NO_T_TIME_ZONE_FORMAT.getPattern());
        testUTC("09:11:12Z", DateFormatUtils.ISO_TIME_NO_T_TIME_ZONE_FORMAT.getPattern());
    }

    private void testUTC(final String expectedValue, final String pattern) {
        final TimeZone timeZone = FastTimeZone.getGmtTimeZone();
        assertFormats(expectedValue, pattern, timeZone, createFebruaryTestDate(timeZone));
    }
}
