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

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.Serializable;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Locale;
import java.util.TimeZone;

import org.apache.commons.lang3.SerializationUtils;
import org.junit.Test;

/**
 * Unit tests {@link org.apache.commons.lang3.time.FastDatePrinter}.
 *
 * @version $Id$
 * @since 3.0
 */
public class FastDatePrinterTest {
    
    private static final String YYYY_MM_DD = "yyyy/MM/dd";
    private static final TimeZone NEW_YORK = TimeZone.getTimeZone("America/New_York");
    private static final Locale SWEDEN = new Locale("sv", "SE");

        DatePrinter getInstance(final String format) {
        return getInstance(format, TimeZone.getDefault(), Locale.getDefault());
    }

    private DatePrinter getDateInstance(final int dateStyle, final Locale locale) {
        return getInstance(FormatCache.getPatternForStyle(Integer.valueOf(dateStyle), null, locale), TimeZone.getDefault(), Locale.getDefault());
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
    public void testFormat() {
        final Locale realDefaultLocale = Locale.getDefault();
        final TimeZone realDefaultZone = TimeZone.getDefault();
        try {
            Locale.setDefault(Locale.US);
            TimeZone.setDefault(NEW_YORK);

            final GregorianCalendar cal1 = new GregorianCalendar(2003, 0, 10, 15, 33, 20);
            final GregorianCalendar cal2 = new GregorianCalendar(2003, 6, 10, 9, 00, 00);
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

            final String pattern = "GGGG GGG GG G yyyy yyy yy y MMMM MMM MM M" +
                " dddd ddd dd d DDDD DDD DD D EEEE EEE EE E aaaa aaa aa a zzzz zzz zz z";
            fdf = getInstance(pattern);
            sdf = new SimpleDateFormat(pattern);
            // SDF bug fix starting with Java 7
            assertEquals(sdf.format(date1).replaceAll("2003 03 03 03", "2003 2003 03 2003"), fdf.format(date1));
            assertEquals(sdf.format(date2).replaceAll("2003 03 03 03", "2003 2003 03 2003"), fdf.format(date2));
        } finally {
            Locale.setDefault(realDefaultLocale);
            TimeZone.setDefault(realDefaultZone);
        }
    }

    /**
     * Test case for {@link FastDateParser#FastDateParser(String, TimeZone, Locale)}.
     */
    @Test
    public void testShortDateStyleWithLocales() {
        final Locale usLocale = Locale.US;
        final Locale swedishLocale = new Locale("sv", "SE");
        final Calendar cal = Calendar.getInstance();
        cal.set(2004, 1, 3);
        DatePrinter fdf = getDateInstance(FastDateFormat.SHORT, usLocale);
        assertEquals("2/3/04", fdf.format(cal));

        fdf = getDateInstance(FastDateFormat.SHORT, swedishLocale);
        assertEquals("2004-02-03", fdf.format(cal));

    }

    /**
     * Tests that pre-1000AD years get padded with yyyy
     */
    @Test
    public void testLowYearPadding() {
        final Calendar cal = Calendar.getInstance();
        final DatePrinter format = getInstance(YYYY_MM_DD);

        cal.set(1,0,1);
        assertEquals("0001/01/01", format.format(cal));
        cal.set(10,0,1);
        assertEquals("0010/01/01", format.format(cal));
        cal.set(100,0,1);
        assertEquals("0100/01/01", format.format(cal));
        cal.set(999,0,1);
        assertEquals("0999/01/01", format.format(cal));
    }
    /**
     * Show Bug #39410 is solved
     */
    @Test
    public void testMilleniumBug() {
        final Calendar cal = Calendar.getInstance();
        final DatePrinter format = getInstance("dd.MM.yyyy");

        cal.set(1000,0,1);
        assertEquals("01.01.1000", format.format(cal));
    }

    /**
     * testLowYearPadding showed that the date was buggy
     * This test confirms it, getting 366 back as a date
     */
    @Test
    public void testSimpleDate() {
        final Calendar cal = Calendar.getInstance();
        final DatePrinter format = getInstance(YYYY_MM_DD);

        cal.set(2004,11,31);
        assertEquals("2004/12/31", format.format(cal));
        cal.set(999,11,31);
        assertEquals("0999/12/31", format.format(cal));
        cal.set(1,2,2);
        assertEquals("0001/03/02", format.format(cal));
    }

    @Test
    public void testLang303() {
        final Calendar cal = Calendar.getInstance();
        cal.set(2004, 11, 31);

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
        cal.set(2009, 9, 16, 8, 42, 16);

        final DatePrinter format = getInstance("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'", TimeZone.getTimeZone("GMT"));
        assertEquals("dateTime", "2009-10-16T16:42:16.000Z", format.format(cal.getTime()));
        assertEquals("dateTime", "2009-10-16T08:42:16.000Z", format.format(cal));
    }

    @Test
    public void testLang645() {
        final Locale locale = new Locale("sv", "SE");

        final Calendar cal = Calendar.getInstance();
        cal.set(2010, 0, 1, 12, 0, 0);
        final Date d = cal.getTime();

        final DatePrinter fdf = getInstance("EEEE', week 'ww", locale);

        assertEquals("fredag, week 53", fdf.format(d));
    }
    
    @Test
    public void testEquals() {
        final DatePrinter printer1= getInstance(YYYY_MM_DD);
        final DatePrinter printer2= getInstance(YYYY_MM_DD);

        assertEquals(printer1, printer2);
        assertEquals(printer1.hashCode(), printer2.hashCode());        

        assertFalse(printer1.equals(new Object()));
    }
    
    @Test
    public void testToStringContainsName() {
        final DatePrinter printer= getInstance(YYYY_MM_DD);
        assertTrue(printer.toString().startsWith("FastDate"));
    }
    
    @Test
    public void testPatternMatches() {
        final DatePrinter printer= getInstance(YYYY_MM_DD);
        assertEquals(YYYY_MM_DD, printer.getPattern());
    }
    
    @Test
    public void testLocaleMatches() {
        final DatePrinter printer= getInstance(YYYY_MM_DD, SWEDEN);
        assertEquals(SWEDEN, printer.getLocale());
    }
    
    @Test
    public void testTimeZoneMatches() {
        final DatePrinter printer= getInstance(YYYY_MM_DD, NEW_YORK);
        assertEquals(NEW_YORK, printer.getTimeZone());
    }
    
    @Test
    public void testCalendarTimezoneRespected() {
        final String[] availableZones = TimeZone.getAvailableIDs();
        final TimeZone currentZone = TimeZone.getDefault();
        
        TimeZone anotherZone = null;
        for (final String zone : availableZones) {
            if (!zone.equals(currentZone.getID())) {
                anotherZone = TimeZone.getTimeZone(zone);
            }
        }
        
        assertNotNull("Cannot find another timezone", anotherZone);
        
        final String pattern = "h:mma z";
        final Calendar cal = Calendar.getInstance(anotherZone);
        
        final SimpleDateFormat sdf = new SimpleDateFormat(pattern);
        sdf.setTimeZone(anotherZone);
        final String expectedValue = sdf.format(cal.getTime());
        final String actualValue = FastDateFormat.getInstance(pattern).format(cal);
        assertEquals(expectedValue, actualValue);
    }
}
