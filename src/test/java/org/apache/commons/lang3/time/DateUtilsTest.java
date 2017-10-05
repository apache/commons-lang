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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

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

import org.apache.commons.lang3.test.SystemDefaults;
import org.apache.commons.lang3.test.SystemDefaultsSwitch;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;

/**
 * Unit tests {@link org.apache.commons.lang3.time.DateUtils}.
 */
public class DateUtilsTest {

    private static Date BASE_DATE;

    @BeforeClass
    public static void classSetup() {
        final GregorianCalendar cal = new GregorianCalendar(2000, 6, 5, 4, 3, 2);
        cal.set(Calendar.MILLISECOND, 1);
        BASE_DATE = cal.getTime();
    }

    @Rule
    public SystemDefaultsSwitch defaults = new SystemDefaultsSwitch();

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

    @Before
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
        TimeZone.setDefault(zone);
        dateTimeParser.setTimeZone(zone);
        date3 = dateTimeParser.parse("March 30, 2003 05:30:45.000");
        date4 = dateTimeParser.parse("March 30, 2003 01:10:00.000");
        date5 = dateTimeParser.parse("March 30, 2003 01:40:00.000");
        date6 = dateTimeParser.parse("March 30, 2003 02:10:00.000");
        date7 = dateTimeParser.parse("March 30, 2003 02:40:00.000");
        date8 = dateTimeParser.parse("October 26, 2003 05:30:45.000");
        dateTimeParser.setTimeZone(defaultZone);
        TimeZone.setDefault(defaultZone);
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
        TimeZone.setDefault(defaultZone);
    }

    //-----------------------------------------------------------------------
    @Test
    public void testConstructor() {
        assertNotNull(new DateUtils());
        final Constructor<?>[] cons = DateUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertTrue(Modifier.isPublic(cons[0].getModifiers()));
        assertTrue(Modifier.isPublic(DateUtils.class.getModifiers()));
        assertFalse(Modifier.isFinal(DateUtils.class.getModifiers()));
    }

    //-----------------------------------------------------------------------
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

    @Test(expected = IllegalArgumentException.class)
    public void testIsSameDay_DateNullNull() throws Exception {
        DateUtils.isSameDay((Date) null, null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testIsSameDay_DateNullNotNull() throws Exception {
        DateUtils.isSameDay(null, new Date());
    }

    @Test(expected = IllegalArgumentException.class)
    public void testIsSameDay_DateNotNullNull() throws Exception {
        DateUtils.isSameDay(new Date(), null);
    }

    //-----------------------------------------------------------------------
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

    @Test(expected = IllegalArgumentException.class)
    public void testIsSameDay_CalNullNull() throws Exception {
        DateUtils.isSameDay((Calendar) null, null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testIsSameDay_CalNullNotNull() throws Exception {
        DateUtils.isSameDay(null, Calendar.getInstance());
    }

    @Test(expected = IllegalArgumentException.class)
    public void testIsSameDay_CalNotNullNull() throws Exception {
        DateUtils.isSameDay(Calendar.getInstance(), null);
    }

    //-----------------------------------------------------------------------
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

    @Test(expected = IllegalArgumentException.class)
    public void testIsSameInstant_DateNullNull() throws Exception {
        DateUtils.isSameInstant((Date) null, null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testIsSameInstant_DateNullNotNull() throws Exception {
        DateUtils.isSameInstant(null, new Date());
    }

    @Test(expected = IllegalArgumentException.class)
    public void testIsSameInstant_DateNotNullNull() throws Exception {
        DateUtils.isSameInstant(new Date(), null);
    }

    //-----------------------------------------------------------------------
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

    @Test(expected = IllegalArgumentException.class)
    public void testIsSameInstant_CalNullNull() throws Exception {
        DateUtils.isSameInstant((Calendar) null, null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testIsSameInstant_CalNullNotNull() throws Exception {
        DateUtils.isSameInstant(null, Calendar.getInstance());
    }

    @Test(expected = IllegalArgumentException.class)
    public void testIsSameInstant_CalNotNullNull() throws Exception {
        DateUtils.isSameInstant(Calendar.getInstance(), null);
    }

    //-----------------------------------------------------------------------
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
        assertFalse("LANG-677", DateUtils.isSameLocalTime(calc, cald));

        calb.set(2004, Calendar.JULY, 9, 11, 45, 0);
        assertFalse(DateUtils.isSameLocalTime(cala, calb));
    }

    @Test(expected = IllegalArgumentException.class)
    public void testIsSameLocalTime_CalNullNull() throws Exception {
        DateUtils.isSameLocalTime(null, null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testIsSameLocalTime_CalNullNotNull() throws Exception {
        DateUtils.isSameLocalTime(null, Calendar.getInstance());
    }

    @Test(expected = IllegalArgumentException.class)
    public void testIsSameLocalTime_CalNotNullNull() throws Exception {
        DateUtils.isSameLocalTime(Calendar.getInstance(), null);
    }

    //-----------------------------------------------------------------------
    @Test
    public void testParseDate() throws Exception {
        final GregorianCalendar cal = new GregorianCalendar(1972, 11, 3);
        String dateStr = "1972-12-03";
        final String[] parsers = new String[] {"yyyy'-'DDD", "yyyy'-'MM'-'dd", "yyyyMMdd"};
        Date date = DateUtils.parseDate(dateStr, parsers);
        assertEquals(cal.getTime(), date);

        dateStr = "1972-338";
        date = DateUtils.parseDate(dateStr, parsers);
        assertEquals(cal.getTime(), date);

        dateStr = "19721203";
        date = DateUtils.parseDate(dateStr, parsers);
        assertEquals(cal.getTime(), date);
    }

    @Test(expected = ParseException.class)
    public void testParseDate_NoDateString() throws Exception {
        final String[] parsers = new String[] {"yyyy'-'DDD", "yyyy'-'MM'-'dd", "yyyyMMdd"};
        DateUtils.parseDate("PURPLE", parsers);
    }

    @Test(expected = ParseException.class)
    public void testParseDate_InvalidDateString() throws Exception {
        final String[] parsers = new String[] {"yyyy'-'DDD", "yyyy'-'MM'-'dd", "yyyyMMdd"};
        DateUtils.parseDate("197212AB", parsers);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testParseDate_Null() throws Exception {
        final String[] parsers = new String[] {"yyyy'-'DDD", "yyyy'-'MM'-'dd", "yyyyMMdd"};
        DateUtils.parseDate(null, parsers);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testParse_NullParsers() throws Exception {
        DateUtils.parseDate("19721203", (String[]) null);
    }

    @Test(expected = ParseException.class)
    public void testParse_EmptyParsers() throws Exception {
        DateUtils.parseDate("19721203");
    }

    // LANG-486
    @Test
    public void testParseDateWithLeniency() throws Exception {
        final GregorianCalendar cal = new GregorianCalendar(1998, 6, 30);
        final String dateStr = "02 942, 1996";
        final String[] parsers = new String[] {"MM DDD, yyyy"};

        final Date date = DateUtils.parseDate(dateStr, parsers);
        assertEquals(cal.getTime(), date);

        try {
            DateUtils.parseDateStrictly(dateStr, parsers);
            fail();
        } catch (final ParseException ex) {}
    }

    //-----------------------------------------------------------------------
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
    }

    //-----------------------------------------------------------------------
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
    }

    //-----------------------------------------------------------------------
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
    }

    //-----------------------------------------------------------------------
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
    }

    //-----------------------------------------------------------------------
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
    }

    //-----------------------------------------------------------------------
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
    }

    //-----------------------------------------------------------------------
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
    }

    //-----------------------------------------------------------------------
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
    }

    // -----------------------------------------------------------------------
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
    }

    // -----------------------------------------------------------------------
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

        try {
            DateUtils.setMonths(BASE_DATE, 12);
            fail("DateUtils.setMonths did not throw an expected IllegalArgumentException.");
        } catch (final IllegalArgumentException e) {

        }
    }

    // -----------------------------------------------------------------------
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

        try {
            DateUtils.setDays(BASE_DATE, 32);
            fail("DateUtils.setDays did not throw an expected IllegalArgumentException.");
        } catch (final IllegalArgumentException e) {

        }
    }

    // -----------------------------------------------------------------------
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

        try {
            DateUtils.setHours(BASE_DATE, 24);
            fail("DateUtils.setHours did not throw an expected IllegalArgumentException.");
        } catch (final IllegalArgumentException e) {

        }
    }

    // -----------------------------------------------------------------------
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

        try {
            DateUtils.setMinutes(BASE_DATE, 60);
            fail("DateUtils.setMinutes did not throw an expected IllegalArgumentException.");
        } catch (final IllegalArgumentException e) {

        }
    }

    // -----------------------------------------------------------------------
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

        try {
            DateUtils.setSeconds(BASE_DATE, 60);
            fail("DateUtils.setSeconds did not throw an expected IllegalArgumentException.");
        } catch (final IllegalArgumentException e) {

        }
    }

    // -----------------------------------------------------------------------
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

        try {
            DateUtils.setMilliseconds(BASE_DATE, 1000);
            fail("DateUtils.setMilliseconds did not throw an expected IllegalArgumentException.");
        } catch (final IllegalArgumentException e) {

        }
    }

    //-----------------------------------------------------------------------
    private void assertDate(final Date date, final int year, final int month, final int day, final int hour, final int min, final int sec, final int mil) throws Exception {
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

    //-----------------------------------------------------------------------
    @Test
    public void testToCalendar() {
        assertEquals("Failed to convert to a Calendar and back", date1, DateUtils.toCalendar(date1).getTime());
        try {
            DateUtils.toCalendar(null);
            fail("Expected NullPointerException to be thrown");
        } catch(final NullPointerException npe) {
            // expected
        }
    }

    //-----------------------------------------------------------------------
    @Test(expected=NullPointerException.class)
    public void testToCalendarWithDateNull() {
        DateUtils.toCalendar(null, zone);
    }

    //-----------------------------------------------------------------------
    @Test(expected=NullPointerException.class)
    public void testToCalendarWithTimeZoneNull() {
        DateUtils.toCalendar(date1, null);
    }

    //-----------------------------------------------------------------------
    @Test
    public void testToCalendarWithDateAndTimeZoneNotNull() {
        final Calendar c = DateUtils.toCalendar(date2, defaultZone);
        assertEquals("Convert Date and TimeZone to a Calendar, but failed to get the Date back", date2, c.getTime());
        assertEquals("Convert Date and TimeZone to a Calendar, but failed to get the TimeZone back", defaultZone, c.getTimeZone());
    }

    //-----------------------------------------------------------------------
    @Test(expected=NullPointerException.class)
    public void testToCalendarWithDateAndTimeZoneNull() {
        DateUtils.toCalendar(null, null);
    }

    //-----------------------------------------------------------------------
    /**
     * Tests various values with the round method
     *
     * @throws java.lang.Exception so we don't have to catch it
     */
    @Test
    public void testRound() throws Exception {
        // tests for public static Date round(Date date, int field)
        assertEquals("round year-1 failed",
                dateParser.parse("January 1, 2002"),
                DateUtils.round(date1, Calendar.YEAR));
        assertEquals("round year-2 failed",
                dateParser.parse("January 1, 2002"),
                DateUtils.round(date2, Calendar.YEAR));
        assertEquals("round month-1 failed",
                dateParser.parse("February 1, 2002"),
                DateUtils.round(date1, Calendar.MONTH));
        assertEquals("round month-2 failed",
                dateParser.parse("December 1, 2001"),
                DateUtils.round(date2, Calendar.MONTH));
        assertEquals("round semimonth-0 failed",
                dateParser.parse("February 1, 2002"),
                DateUtils.round(date0, DateUtils.SEMI_MONTH));
        assertEquals("round semimonth-1 failed",
                dateParser.parse("February 16, 2002"),
                DateUtils.round(date1, DateUtils.SEMI_MONTH));
        assertEquals("round semimonth-2 failed",
                dateParser.parse("November 16, 2001"),
                DateUtils.round(date2, DateUtils.SEMI_MONTH));


        assertEquals("round date-1 failed",
                dateParser.parse("February 13, 2002"),
                DateUtils.round(date1, Calendar.DATE));
        assertEquals("round date-2 failed",
                dateParser.parse("November 18, 2001"),
                DateUtils.round(date2, Calendar.DATE));
        assertEquals("round hour-1 failed",
                dateTimeParser.parse("February 12, 2002 13:00:00.000"),
                DateUtils.round(date1, Calendar.HOUR));
        assertEquals("round hour-2 failed",
                dateTimeParser.parse("November 18, 2001 1:00:00.000"),
                DateUtils.round(date2, Calendar.HOUR));
        assertEquals("round minute-1 failed",
                dateTimeParser.parse("February 12, 2002 12:35:00.000"),
                DateUtils.round(date1, Calendar.MINUTE));
        assertEquals("round minute-2 failed",
                dateTimeParser.parse("November 18, 2001 1:23:00.000"),
                DateUtils.round(date2, Calendar.MINUTE));
        assertEquals("round second-1 failed",
                dateTimeParser.parse("February 12, 2002 12:34:57.000"),
                DateUtils.round(date1, Calendar.SECOND));
        assertEquals("round second-2 failed",
                dateTimeParser.parse("November 18, 2001 1:23:11.000"),
                DateUtils.round(date2, Calendar.SECOND));
        assertEquals("round ampm-1 failed",
                dateTimeParser.parse("February 3, 2002 00:00:00.000"),
                DateUtils.round(dateAmPm1, Calendar.AM_PM));
        assertEquals("round ampm-2 failed",
                dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.round(dateAmPm2, Calendar.AM_PM));
        assertEquals("round ampm-3 failed",
                dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.round(dateAmPm3, Calendar.AM_PM));
        assertEquals("round ampm-4 failed",
                dateTimeParser.parse("February 4, 2002 00:00:00.000"),
                DateUtils.round(dateAmPm4, Calendar.AM_PM));

        // tests for public static Date round(Object date, int field)
        assertEquals("round year-1 failed",
                dateParser.parse("January 1, 2002"),
                DateUtils.round((Object) date1, Calendar.YEAR));
        assertEquals("round year-2 failed",
                dateParser.parse("January 1, 2002"),
                DateUtils.round((Object) date2, Calendar.YEAR));
        assertEquals("round month-1 failed",
                dateParser.parse("February 1, 2002"),
                DateUtils.round((Object) date1, Calendar.MONTH));
        assertEquals("round month-2 failed",
                dateParser.parse("December 1, 2001"),
                DateUtils.round((Object) date2, Calendar.MONTH));
        assertEquals("round semimonth-1 failed",
                dateParser.parse("February 16, 2002"),
                DateUtils.round((Object) date1, DateUtils.SEMI_MONTH));
        assertEquals("round semimonth-2 failed",
                dateParser.parse("November 16, 2001"),
                DateUtils.round((Object) date2, DateUtils.SEMI_MONTH));
        assertEquals("round date-1 failed",
                dateParser.parse("February 13, 2002"),
                DateUtils.round((Object) date1, Calendar.DATE));
        assertEquals("round date-2 failed",
                dateParser.parse("November 18, 2001"),
                DateUtils.round((Object) date2, Calendar.DATE));
        assertEquals("round hour-1 failed",
                dateTimeParser.parse("February 12, 2002 13:00:00.000"),
                DateUtils.round((Object) date1, Calendar.HOUR));
        assertEquals("round hour-2 failed",
                dateTimeParser.parse("November 18, 2001 1:00:00.000"),
                DateUtils.round((Object) date2, Calendar.HOUR));
        assertEquals("round minute-1 failed",
                dateTimeParser.parse("February 12, 2002 12:35:00.000"),
                DateUtils.round((Object) date1, Calendar.MINUTE));
        assertEquals("round minute-2 failed",
                dateTimeParser.parse("November 18, 2001 1:23:00.000"),
                DateUtils.round((Object) date2, Calendar.MINUTE));
        assertEquals("round second-1 failed",
                dateTimeParser.parse("February 12, 2002 12:34:57.000"),
                DateUtils.round((Object) date1, Calendar.SECOND));
        assertEquals("round second-2 failed",
                dateTimeParser.parse("November 18, 2001 1:23:11.000"),
                DateUtils.round((Object) date2, Calendar.SECOND));
        assertEquals("round calendar second-1 failed",
                dateTimeParser.parse("February 12, 2002 12:34:57.000"),
                DateUtils.round((Object) cal1, Calendar.SECOND));
        assertEquals("round calendar second-2 failed",
                dateTimeParser.parse("November 18, 2001 1:23:11.000"),
                DateUtils.round((Object) cal2, Calendar.SECOND));
        assertEquals("round ampm-1 failed",
                dateTimeParser.parse("February 3, 2002 00:00:00.000"),
                DateUtils.round((Object) dateAmPm1, Calendar.AM_PM));
        assertEquals("round ampm-2 failed",
                dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.round((Object) dateAmPm2, Calendar.AM_PM));
        assertEquals("round ampm-3 failed",
                dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.round((Object) dateAmPm3, Calendar.AM_PM));
        assertEquals("round ampm-4 failed",
                dateTimeParser.parse("February 4, 2002 00:00:00.000"),
                DateUtils.round((Object) dateAmPm4, Calendar.AM_PM));

        try {
            DateUtils.round((Date) null, Calendar.SECOND);
            fail();
        } catch (final IllegalArgumentException ex) {}
        try {
            DateUtils.round((Calendar) null, Calendar.SECOND);
            fail();
        } catch (final IllegalArgumentException ex) {}
        try {
            DateUtils.round((Object) null, Calendar.SECOND);
            fail();
        } catch (final IllegalArgumentException ex) {}
        try {
            DateUtils.round("", Calendar.SECOND);
            fail();
        } catch (final ClassCastException ex) {}
        try {
            DateUtils.round(date1, -9999);
            fail();
        } catch(final IllegalArgumentException ex) {}

        assertEquals("round ampm-1 failed",
                dateTimeParser.parse("February 3, 2002 00:00:00.000"),
                DateUtils.round((Object) calAmPm1, Calendar.AM_PM));
        assertEquals("round ampm-2 failed",
                dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.round((Object) calAmPm2, Calendar.AM_PM));
        assertEquals("round ampm-3 failed",
                dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.round((Object) calAmPm3, Calendar.AM_PM));
        assertEquals("round ampm-4 failed",
                dateTimeParser.parse("February 4, 2002 00:00:00.000"),
                DateUtils.round((Object) calAmPm4, Calendar.AM_PM));

        // Fix for http://issues.apache.org/bugzilla/show_bug.cgi?id=25560 / LANG-13
        // Test rounding across the beginning of daylight saving time
        TimeZone.setDefault(zone);
        dateTimeParser.setTimeZone(zone);
        assertEquals("round MET date across DST change-over",
                dateTimeParser.parse("March 30, 2003 00:00:00.000"),
                DateUtils.round(date4, Calendar.DATE));
        assertEquals("round MET date across DST change-over",
                dateTimeParser.parse("March 30, 2003 00:00:00.000"),
                DateUtils.round((Object) cal4, Calendar.DATE));
        assertEquals("round MET date across DST change-over",
                dateTimeParser.parse("March 30, 2003 00:00:00.000"),
                DateUtils.round(date5, Calendar.DATE));
        assertEquals("round MET date across DST change-over",
                dateTimeParser.parse("March 30, 2003 00:00:00.000"),
                DateUtils.round((Object) cal5, Calendar.DATE));
        assertEquals("round MET date across DST change-over",
                dateTimeParser.parse("March 30, 2003 00:00:00.000"),
                DateUtils.round(date6, Calendar.DATE));
        assertEquals("round MET date across DST change-over",
                dateTimeParser.parse("March 30, 2003 00:00:00.000"),
                DateUtils.round((Object) cal6, Calendar.DATE));
        assertEquals("round MET date across DST change-over",
                dateTimeParser.parse("March 30, 2003 00:00:00.000"),
                DateUtils.round(date7, Calendar.DATE));
        assertEquals("round MET date across DST change-over",
                dateTimeParser.parse("March 30, 2003 00:00:00.000"),
                DateUtils.round((Object) cal7, Calendar.DATE));

        assertEquals("round MET date across DST change-over",
                dateTimeParser.parse("March 30, 2003 01:00:00.000"),
                DateUtils.round(date4, Calendar.HOUR_OF_DAY));
        assertEquals("round MET date across DST change-over",
                dateTimeParser.parse("March 30, 2003 01:00:00.000"),
                DateUtils.round((Object) cal4, Calendar.HOUR_OF_DAY));
        assertEquals("round MET date across DST change-over",
                dateTimeParser.parse("March 30, 2003 03:00:00.000"),
                DateUtils.round(date5, Calendar.HOUR_OF_DAY));
        assertEquals("round MET date across DST change-over",
                dateTimeParser.parse("March 30, 2003 03:00:00.000"),
                DateUtils.round((Object) cal5, Calendar.HOUR_OF_DAY));
        assertEquals("round MET date across DST change-over",
                dateTimeParser.parse("March 30, 2003 03:00:00.000"),
                DateUtils.round(date6, Calendar.HOUR_OF_DAY));
        assertEquals("round MET date across DST change-over",
                dateTimeParser.parse("March 30, 2003 03:00:00.000"),
                DateUtils.round((Object) cal6, Calendar.HOUR_OF_DAY));
        assertEquals("round MET date across DST change-over",
                dateTimeParser.parse("March 30, 2003 04:00:00.000"),
                DateUtils.round(date7, Calendar.HOUR_OF_DAY));
        assertEquals("round MET date across DST change-over",
                dateTimeParser.parse("March 30, 2003 04:00:00.000"),
                DateUtils.round((Object) cal7, Calendar.HOUR_OF_DAY));
        TimeZone.setDefault(defaultZone);
        dateTimeParser.setTimeZone(defaultZone);
    }

    /**
     * Tests the Changes Made by LANG-346 to the DateUtils.modify() private method invoked
     * by DateUtils.round().
     *
     * @throws java.lang.Exception so we don't have to catch it
     */
    @Test
    public void testRoundLang346() throws Exception {
        TimeZone.setDefault(defaultZone);
        dateTimeParser.setTimeZone(defaultZone);
        final Calendar testCalendar = Calendar.getInstance();
        testCalendar.set(2007, Calendar.JULY, 2, 8, 8, 50);
        Date date = testCalendar.getTime();
        assertEquals("Minute Round Up Failed",
                     dateTimeParser.parse("July 2, 2007 08:09:00.000"),
                     DateUtils.round(date, Calendar.MINUTE));

        testCalendar.set(2007, Calendar.JULY, 2, 8, 8, 20);
        date = testCalendar.getTime();
        assertEquals("Minute No Round Failed",
                     dateTimeParser.parse("July 2, 2007 08:08:00.000"),
                     DateUtils.round(date, Calendar.MINUTE));

        testCalendar.set(2007, Calendar.JULY, 2, 8, 8, 50);
        testCalendar.set(Calendar.MILLISECOND, 600);
        date = testCalendar.getTime();

        assertEquals("Second Round Up with 600 Milli Seconds Failed",
                     dateTimeParser.parse("July 2, 2007 08:08:51.000"),
                     DateUtils.round(date, Calendar.SECOND));

        testCalendar.set(2007, Calendar.JULY, 2, 8, 8, 50);
        testCalendar.set(Calendar.MILLISECOND, 200);
        date = testCalendar.getTime();
        assertEquals("Second Round Down with 200 Milli Seconds Failed",
                     dateTimeParser.parse("July 2, 2007 08:08:50.000"),
                     DateUtils.round(date, Calendar.SECOND));

        testCalendar.set(2007, Calendar.JULY, 2, 8, 8, 20);
        testCalendar.set(Calendar.MILLISECOND, 600);
        date = testCalendar.getTime();
        assertEquals("Second Round Up with 200 Milli Seconds Failed",
                     dateTimeParser.parse("July 2, 2007 08:08:21.000"),
                     DateUtils.round(date, Calendar.SECOND));

        testCalendar.set(2007, Calendar.JULY, 2, 8, 8, 20);
        testCalendar.set(Calendar.MILLISECOND, 200);
        date = testCalendar.getTime();
        assertEquals("Second Round Down with 200 Milli Seconds Failed",
                     dateTimeParser.parse("July 2, 2007 08:08:20.000"),
                     DateUtils.round(date, Calendar.SECOND));

        testCalendar.set(2007, Calendar.JULY, 2, 8, 8, 50);
        date = testCalendar.getTime();
        assertEquals("Hour Round Down Failed",
                     dateTimeParser.parse("July 2, 2007 08:00:00.000"),
                     DateUtils.round(date, Calendar.HOUR));

        testCalendar.set(2007, Calendar.JULY, 2, 8, 31, 50);
        date = testCalendar.getTime();
        assertEquals("Hour Round Up Failed",
                     dateTimeParser.parse("July 2, 2007 09:00:00.000"),
                     DateUtils.round(date, Calendar.HOUR));
    }

    /**
     * Tests various values with the trunc method
     *
     * @throws java.lang.Exception so we don't have to catch it
     */
    @Test
    public void testTruncate() throws Exception {
        // tests public static Date truncate(Date date, int field)
        assertEquals("truncate year-1 failed",
                dateParser.parse("January 1, 2002"),
                DateUtils.truncate(date1, Calendar.YEAR));
        assertEquals("truncate year-2 failed",
                dateParser.parse("January 1, 2001"),
                DateUtils.truncate(date2, Calendar.YEAR));
        assertEquals("truncate month-1 failed",
                dateParser.parse("February 1, 2002"),
                DateUtils.truncate(date1, Calendar.MONTH));
        assertEquals("truncate month-2 failed",
                dateParser.parse("November 1, 2001"),
                DateUtils.truncate(date2, Calendar.MONTH));
        assertEquals("truncate semimonth-1 failed",
                dateParser.parse("February 1, 2002"),
                DateUtils.truncate(date1, DateUtils.SEMI_MONTH));
        assertEquals("truncate semimonth-2 failed",
                dateParser.parse("November 16, 2001"),
                DateUtils.truncate(date2, DateUtils.SEMI_MONTH));
        assertEquals("truncate date-1 failed",
                dateParser.parse("February 12, 2002"),
                DateUtils.truncate(date1, Calendar.DATE));
        assertEquals("truncate date-2 failed",
                dateParser.parse("November 18, 2001"),
                DateUtils.truncate(date2, Calendar.DATE));
        assertEquals("truncate hour-1 failed",
                dateTimeParser.parse("February 12, 2002 12:00:00.000"),
                DateUtils.truncate(date1, Calendar.HOUR));
        assertEquals("truncate hour-2 failed",
                dateTimeParser.parse("November 18, 2001 1:00:00.000"),
                DateUtils.truncate(date2, Calendar.HOUR));
        assertEquals("truncate minute-1 failed",
                dateTimeParser.parse("February 12, 2002 12:34:00.000"),
                DateUtils.truncate(date1, Calendar.MINUTE));
        assertEquals("truncate minute-2 failed",
                dateTimeParser.parse("November 18, 2001 1:23:00.000"),
                DateUtils.truncate(date2, Calendar.MINUTE));
        assertEquals("truncate second-1 failed",
                dateTimeParser.parse("February 12, 2002 12:34:56.000"),
                DateUtils.truncate(date1, Calendar.SECOND));
        assertEquals("truncate second-2 failed",
                dateTimeParser.parse("November 18, 2001 1:23:11.000"),
                DateUtils.truncate(date2, Calendar.SECOND));
        assertEquals("truncate ampm-1 failed",
                dateTimeParser.parse("February 3, 2002 00:00:00.000"),
                DateUtils.truncate(dateAmPm1, Calendar.AM_PM));
        assertEquals("truncate ampm-2 failed",
                dateTimeParser.parse("February 3, 2002 00:00:00.000"),
                DateUtils.truncate(dateAmPm2, Calendar.AM_PM));
        assertEquals("truncate ampm-3 failed",
                dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.truncate(dateAmPm3, Calendar.AM_PM));
        assertEquals("truncate ampm-4 failed",
                dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.truncate(dateAmPm4, Calendar.AM_PM));

        // tests public static Date truncate(Object date, int field)
        assertEquals("truncate year-1 failed",
                dateParser.parse("January 1, 2002"),
                DateUtils.truncate((Object) date1, Calendar.YEAR));
        assertEquals("truncate year-2 failed",
                dateParser.parse("January 1, 2001"),
                DateUtils.truncate((Object) date2, Calendar.YEAR));
        assertEquals("truncate month-1 failed",
                dateParser.parse("February 1, 2002"),
                DateUtils.truncate((Object) date1, Calendar.MONTH));
        assertEquals("truncate month-2 failed",
                dateParser.parse("November 1, 2001"),
                DateUtils.truncate((Object) date2, Calendar.MONTH));
        assertEquals("truncate semimonth-1 failed",
                dateParser.parse("February 1, 2002"),
                DateUtils.truncate((Object) date1, DateUtils.SEMI_MONTH));
        assertEquals("truncate semimonth-2 failed",
                dateParser.parse("November 16, 2001"),
                DateUtils.truncate((Object) date2, DateUtils.SEMI_MONTH));
        assertEquals("truncate date-1 failed",
                dateParser.parse("February 12, 2002"),
                DateUtils.truncate((Object) date1, Calendar.DATE));
        assertEquals("truncate date-2 failed",
                dateParser.parse("November 18, 2001"),
                DateUtils.truncate((Object) date2, Calendar.DATE));
        assertEquals("truncate hour-1 failed",
                dateTimeParser.parse("February 12, 2002 12:00:00.000"),
                DateUtils.truncate((Object) date1, Calendar.HOUR));
        assertEquals("truncate hour-2 failed",
                dateTimeParser.parse("November 18, 2001 1:00:00.000"),
                DateUtils.truncate((Object) date2, Calendar.HOUR));
        assertEquals("truncate minute-1 failed",
                dateTimeParser.parse("February 12, 2002 12:34:00.000"),
                DateUtils.truncate((Object) date1, Calendar.MINUTE));
        assertEquals("truncate minute-2 failed",
                dateTimeParser.parse("November 18, 2001 1:23:00.000"),
                DateUtils.truncate((Object) date2, Calendar.MINUTE));
        assertEquals("truncate second-1 failed",
                dateTimeParser.parse("February 12, 2002 12:34:56.000"),
                DateUtils.truncate((Object) date1, Calendar.SECOND));
        assertEquals("truncate second-2 failed",
                dateTimeParser.parse("November 18, 2001 1:23:11.000"),
                DateUtils.truncate((Object) date2, Calendar.SECOND));
        assertEquals("truncate ampm-1 failed",
                dateTimeParser.parse("February 3, 2002 00:00:00.000"),
                DateUtils.truncate((Object) dateAmPm1, Calendar.AM_PM));
        assertEquals("truncate ampm-2 failed",
                dateTimeParser.parse("February 3, 2002 00:00:00.000"),
                DateUtils.truncate((Object) dateAmPm2, Calendar.AM_PM));
        assertEquals("truncate ampm-3 failed",
                dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.truncate((Object) dateAmPm3, Calendar.AM_PM));
        assertEquals("truncate ampm-4 failed",
                dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.truncate((Object) dateAmPm4, Calendar.AM_PM));

        assertEquals("truncate calendar second-1 failed",
                dateTimeParser.parse("February 12, 2002 12:34:56.000"),
                DateUtils.truncate((Object) cal1, Calendar.SECOND));
        assertEquals("truncate calendar second-2 failed",
                dateTimeParser.parse("November 18, 2001 1:23:11.000"),
                DateUtils.truncate((Object) cal2, Calendar.SECOND));

        assertEquals("truncate ampm-1 failed",
                dateTimeParser.parse("February 3, 2002 00:00:00.000"),
                DateUtils.truncate((Object) calAmPm1, Calendar.AM_PM));
        assertEquals("truncate ampm-2 failed",
                dateTimeParser.parse("February 3, 2002 00:00:00.000"),
                DateUtils.truncate((Object) calAmPm2, Calendar.AM_PM));
        assertEquals("truncate ampm-3 failed",
                dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.truncate((Object) calAmPm3, Calendar.AM_PM));
        assertEquals("truncate ampm-4 failed",
                dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.truncate((Object) calAmPm4, Calendar.AM_PM));

        try {
            DateUtils.truncate((Date) null, Calendar.SECOND);
            fail();
        } catch (final IllegalArgumentException ex) {}
        try {
            DateUtils.truncate((Calendar) null, Calendar.SECOND);
            fail();
        } catch (final IllegalArgumentException ex) {}
        try {
            DateUtils.truncate((Object) null, Calendar.SECOND);
            fail();
        } catch (final IllegalArgumentException ex) {}
        try {
            DateUtils.truncate("", Calendar.SECOND);
            fail();
        } catch (final ClassCastException ex) {}

        // Fix for http://issues.apache.org/bugzilla/show_bug.cgi?id=25560
        // Test truncate across beginning of daylight saving time
        TimeZone.setDefault(zone);
        dateTimeParser.setTimeZone(zone);
        assertEquals("truncate MET date across DST change-over",
                dateTimeParser.parse("March 30, 2003 00:00:00.000"),
                DateUtils.truncate(date3, Calendar.DATE));
        assertEquals("truncate MET date across DST change-over",
                dateTimeParser.parse("March 30, 2003 00:00:00.000"),
                DateUtils.truncate((Object) cal3, Calendar.DATE));
        // Test truncate across end of daylight saving time
        assertEquals("truncate MET date across DST change-over",
                dateTimeParser.parse("October 26, 2003 00:00:00.000"),
                DateUtils.truncate(date8, Calendar.DATE));
        assertEquals("truncate MET date across DST change-over",
                dateTimeParser.parse("October 26, 2003 00:00:00.000"),
                DateUtils.truncate((Object) cal8, Calendar.DATE));
        TimeZone.setDefault(defaultZone);
        dateTimeParser.setTimeZone(defaultZone);

        // Bug 31395, large dates
        final Date endOfTime = new Date(Long.MAX_VALUE); // fyi: Sun Aug 17 07:12:55 CET 292278994 -- 807 millis
        final GregorianCalendar endCal = new GregorianCalendar();
        endCal.setTime(endOfTime);
        try {
            DateUtils.truncate(endCal, Calendar.DATE);
            fail();
        } catch (final ArithmeticException ex) {}
        endCal.set(Calendar.YEAR, 280000001);
        try {
            DateUtils.truncate(endCal, Calendar.DATE);
            fail();
        } catch (final ArithmeticException ex) {}
        endCal.set(Calendar.YEAR, 280000000);
        final Calendar cal = DateUtils.truncate(endCal, Calendar.DATE);
        assertEquals(0, cal.get(Calendar.HOUR));
    }

    /**
     * Tests for LANG-59
     *
     * @throws java.lang.Exception so we don't have to catch it
     *
     * see http://issues.apache.org/jira/browse/LANG-59
     */
    @Test
    public void testTruncateLang59() throws Exception {
        // Set TimeZone to Mountain Time
        final TimeZone MST_MDT = TimeZone.getTimeZone("MST7MDT");
        TimeZone.setDefault(MST_MDT);
        final DateFormat format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS z");
        format.setTimeZone(MST_MDT);

        final Date oct31_01MDT = new Date(1099206000000L);

        final Date oct31MDT             = new Date(oct31_01MDT.getTime()       - 3600000L); // - 1 hour
        final Date oct31_01_02MDT       = new Date(oct31_01MDT.getTime()       + 120000L);  // + 2 minutes
        final Date oct31_01_02_03MDT    = new Date(oct31_01_02MDT.getTime()    + 3000L);    // + 3 seconds
        final Date oct31_01_02_03_04MDT = new Date(oct31_01_02_03MDT.getTime() + 4L);       // + 4 milliseconds

        assertEquals("Check 00:00:00.000", "2004-10-31 00:00:00.000 MDT", format.format(oct31MDT));
        assertEquals("Check 01:00:00.000", "2004-10-31 01:00:00.000 MDT", format.format(oct31_01MDT));
        assertEquals("Check 01:02:00.000", "2004-10-31 01:02:00.000 MDT", format.format(oct31_01_02MDT));
        assertEquals("Check 01:02:03.000", "2004-10-31 01:02:03.000 MDT", format.format(oct31_01_02_03MDT));
        assertEquals("Check 01:02:03.004", "2004-10-31 01:02:03.004 MDT", format.format(oct31_01_02_03_04MDT));

        // ------- Demonstrate Problem -------
        final Calendar gval = Calendar.getInstance();
        gval.setTime(new Date(oct31_01MDT.getTime()));
        gval.set(Calendar.MINUTE, gval.get(Calendar.MINUTE)); // set minutes to the same value
        assertEquals("Demonstrate Problem", gval.getTime().getTime(), oct31_01MDT.getTime() + 3600000L);

        // ---------- Test Truncate ----------
        assertEquals("Truncate Calendar.MILLISECOND",
                oct31_01_02_03_04MDT, DateUtils.truncate(oct31_01_02_03_04MDT, Calendar.MILLISECOND));

        assertEquals("Truncate Calendar.SECOND",
                   oct31_01_02_03MDT, DateUtils.truncate(oct31_01_02_03_04MDT, Calendar.SECOND));

        assertEquals("Truncate Calendar.MINUTE",
                      oct31_01_02MDT, DateUtils.truncate(oct31_01_02_03_04MDT, Calendar.MINUTE));

        assertEquals("Truncate Calendar.HOUR_OF_DAY",
                         oct31_01MDT, DateUtils.truncate(oct31_01_02_03_04MDT, Calendar.HOUR_OF_DAY));

        assertEquals("Truncate Calendar.HOUR",
                         oct31_01MDT, DateUtils.truncate(oct31_01_02_03_04MDT, Calendar.HOUR));

        assertEquals("Truncate Calendar.DATE",
                            oct31MDT, DateUtils.truncate(oct31_01_02_03_04MDT, Calendar.DATE));


        // ---------- Test Round (down) ----------
        assertEquals("Round Calendar.MILLISECOND",
                oct31_01_02_03_04MDT, DateUtils.round(oct31_01_02_03_04MDT, Calendar.MILLISECOND));

        assertEquals("Round Calendar.SECOND",
                   oct31_01_02_03MDT, DateUtils.round(oct31_01_02_03_04MDT, Calendar.SECOND));

        assertEquals("Round Calendar.MINUTE",
                      oct31_01_02MDT, DateUtils.round(oct31_01_02_03_04MDT, Calendar.MINUTE));

        assertEquals("Round Calendar.HOUR_OF_DAY",
                         oct31_01MDT, DateUtils.round(oct31_01_02_03_04MDT, Calendar.HOUR_OF_DAY));

        assertEquals("Round Calendar.HOUR",
                         oct31_01MDT, DateUtils.round(oct31_01_02_03_04MDT, Calendar.HOUR));

        assertEquals("Round Calendar.DATE",
                            oct31MDT, DateUtils.round(oct31_01_02_03_04MDT, Calendar.DATE));

        // restore default time zone
        TimeZone.setDefault(defaultZone);
    }

    // http://issues.apache.org/jira/browse/LANG-530
    @SuppressWarnings("deprecation")
    @Test
    public void testLang530() throws ParseException {
        final Date d = new Date();
        final String isoDateStr = DateFormatUtils.ISO_DATETIME_TIME_ZONE_FORMAT.format(d);
        final Date d2 = DateUtils.parseDate(isoDateStr, DateFormatUtils.ISO_DATETIME_TIME_ZONE_FORMAT.getPattern());
        // the format loses milliseconds so have to reintroduce them
        assertEquals("Date not equal to itself ISO formatted and parsed", d.getTime(), d2.getTime() + d.getTime() % 1000);
    }

    /**
     * Tests various values with the ceiling method
     *
     * @throws java.lang.Exception so we don't have to catch it
     */
    @Test
    public void testCeil() throws Exception {
        // test javadoc
        assertEquals("ceiling javadoc-1 failed",
                dateTimeParser.parse("March 28, 2002 14:00:00.000"),
                DateUtils.ceiling(
                    dateTimeParser.parse("March 28, 2002 13:45:01.231"),
                Calendar.HOUR));
        assertEquals("ceiling javadoc-2 failed",
                dateTimeParser.parse("April 1, 2002 00:00:00.000"),
                DateUtils.ceiling(
                    dateTimeParser.parse("March 28, 2002 13:45:01.231"),
                Calendar.MONTH));

        // tests public static Date ceiling(Date date, int field)
        assertEquals("ceiling year-1 failed",
                dateParser.parse("January 1, 2003"),
                DateUtils.ceiling(date1, Calendar.YEAR));
        assertEquals("ceiling year-2 failed",
                dateParser.parse("January 1, 2002"),
                DateUtils.ceiling(date2, Calendar.YEAR));
        assertEquals("ceiling month-1 failed",
                dateParser.parse("March 1, 2002"),
                DateUtils.ceiling(date1, Calendar.MONTH));
        assertEquals("ceiling month-2 failed",
                dateParser.parse("December 1, 2001"),
                DateUtils.ceiling(date2, Calendar.MONTH));
        assertEquals("ceiling semimonth-1 failed",
                dateParser.parse("February 16, 2002"),
                DateUtils.ceiling(date1, DateUtils.SEMI_MONTH));
        assertEquals("ceiling semimonth-2 failed",
                dateParser.parse("December 1, 2001"),
                DateUtils.ceiling(date2, DateUtils.SEMI_MONTH));
        assertEquals("ceiling date-1 failed",
                dateParser.parse("February 13, 2002"),
                DateUtils.ceiling(date1, Calendar.DATE));
        assertEquals("ceiling date-2 failed",
                dateParser.parse("November 19, 2001"),
                DateUtils.ceiling(date2, Calendar.DATE));
        assertEquals("ceiling hour-1 failed",
                dateTimeParser.parse("February 12, 2002 13:00:00.000"),
                DateUtils.ceiling(date1, Calendar.HOUR));
        assertEquals("ceiling hour-2 failed",
                dateTimeParser.parse("November 18, 2001 2:00:00.000"),
                DateUtils.ceiling(date2, Calendar.HOUR));
        assertEquals("ceiling minute-1 failed",
                dateTimeParser.parse("February 12, 2002 12:35:00.000"),
                DateUtils.ceiling(date1, Calendar.MINUTE));
        assertEquals("ceiling minute-2 failed",
                dateTimeParser.parse("November 18, 2001 1:24:00.000"),
                DateUtils.ceiling(date2, Calendar.MINUTE));
        assertEquals("ceiling second-1 failed",
                dateTimeParser.parse("February 12, 2002 12:34:57.000"),
                DateUtils.ceiling(date1, Calendar.SECOND));
        assertEquals("ceiling second-2 failed",
                dateTimeParser.parse("November 18, 2001 1:23:12.000"),
                DateUtils.ceiling(date2, Calendar.SECOND));
        assertEquals("ceiling ampm-1 failed",
                dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.ceiling(dateAmPm1, Calendar.AM_PM));
        assertEquals("ceiling ampm-2 failed",
                dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.ceiling(dateAmPm2, Calendar.AM_PM));
        assertEquals("ceiling ampm-3 failed",
                dateTimeParser.parse("February 4, 2002 00:00:00.000"),
                DateUtils.ceiling(dateAmPm3, Calendar.AM_PM));
        assertEquals("ceiling ampm-4 failed",
                dateTimeParser.parse("February 4, 2002 00:00:00.000"),
                DateUtils.ceiling(dateAmPm4, Calendar.AM_PM));

     // tests public static Date ceiling(Object date, int field)
        assertEquals("ceiling year-1 failed",
                dateParser.parse("January 1, 2003"),
                DateUtils.ceiling((Object) date1, Calendar.YEAR));
        assertEquals("ceiling year-2 failed",
                dateParser.parse("January 1, 2002"),
                DateUtils.ceiling((Object) date2, Calendar.YEAR));
        assertEquals("ceiling month-1 failed",
                dateParser.parse("March 1, 2002"),
                DateUtils.ceiling((Object) date1, Calendar.MONTH));
        assertEquals("ceiling month-2 failed",
                dateParser.parse("December 1, 2001"),
                DateUtils.ceiling((Object) date2, Calendar.MONTH));
        assertEquals("ceiling semimonth-1 failed",
                dateParser.parse("February 16, 2002"),
                DateUtils.ceiling((Object) date1, DateUtils.SEMI_MONTH));
        assertEquals("ceiling semimonth-2 failed",
                dateParser.parse("December 1, 2001"),
                DateUtils.ceiling((Object) date2, DateUtils.SEMI_MONTH));
        assertEquals("ceiling date-1 failed",
                dateParser.parse("February 13, 2002"),
                DateUtils.ceiling((Object) date1, Calendar.DATE));
        assertEquals("ceiling date-2 failed",
                dateParser.parse("November 19, 2001"),
                DateUtils.ceiling((Object) date2, Calendar.DATE));
        assertEquals("ceiling hour-1 failed",
                dateTimeParser.parse("February 12, 2002 13:00:00.000"),
                DateUtils.ceiling((Object) date1, Calendar.HOUR));
        assertEquals("ceiling hour-2 failed",
                dateTimeParser.parse("November 18, 2001 2:00:00.000"),
                DateUtils.ceiling((Object) date2, Calendar.HOUR));
        assertEquals("ceiling minute-1 failed",
                dateTimeParser.parse("February 12, 2002 12:35:00.000"),
                DateUtils.ceiling((Object) date1, Calendar.MINUTE));
        assertEquals("ceiling minute-2 failed",
                dateTimeParser.parse("November 18, 2001 1:24:00.000"),
                DateUtils.ceiling((Object) date2, Calendar.MINUTE));
        assertEquals("ceiling second-1 failed",
                dateTimeParser.parse("February 12, 2002 12:34:57.000"),
                DateUtils.ceiling((Object) date1, Calendar.SECOND));
        assertEquals("ceiling second-2 failed",
                dateTimeParser.parse("November 18, 2001 1:23:12.000"),
                DateUtils.ceiling((Object) date2, Calendar.SECOND));
        assertEquals("ceiling ampm-1 failed",
                dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.ceiling((Object) dateAmPm1, Calendar.AM_PM));
        assertEquals("ceiling ampm-2 failed",
                dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.ceiling((Object) dateAmPm2, Calendar.AM_PM));
        assertEquals("ceiling ampm-3 failed",
                dateTimeParser.parse("February 4, 2002 00:00:00.000"),
                DateUtils.ceiling((Object) dateAmPm3, Calendar.AM_PM));
        assertEquals("ceiling ampm-4 failed",
                dateTimeParser.parse("February 4, 2002 00:00:00.000"),
                DateUtils.ceiling((Object) dateAmPm4, Calendar.AM_PM));

        assertEquals("ceiling calendar second-1 failed",
                dateTimeParser.parse("February 12, 2002 12:34:57.000"),
                DateUtils.ceiling((Object) cal1, Calendar.SECOND));
        assertEquals("ceiling calendar second-2 failed",
                dateTimeParser.parse("November 18, 2001 1:23:12.000"),
                DateUtils.ceiling((Object) cal2, Calendar.SECOND));

        assertEquals("ceiling ampm-1 failed",
                dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.ceiling((Object) calAmPm1, Calendar.AM_PM));
        assertEquals("ceiling ampm-2 failed",
                dateTimeParser.parse("February 3, 2002 12:00:00.000"),
                DateUtils.ceiling((Object) calAmPm2, Calendar.AM_PM));
        assertEquals("ceiling ampm-3 failed",
                dateTimeParser.parse("February 4, 2002 00:00:00.000"),
                DateUtils.ceiling((Object) calAmPm3, Calendar.AM_PM));
        assertEquals("ceiling ampm-4 failed",
                dateTimeParser.parse("February 4, 2002 00:00:00.000"),
                DateUtils.ceiling((Object) calAmPm4, Calendar.AM_PM));

        try {
            DateUtils.ceiling((Date) null, Calendar.SECOND);
            fail();
        } catch (final IllegalArgumentException ex) {}
        try {
            DateUtils.ceiling((Calendar) null, Calendar.SECOND);
            fail();
        } catch (final IllegalArgumentException ex) {}
        try {
            DateUtils.ceiling((Object) null, Calendar.SECOND);
            fail();
        } catch (final IllegalArgumentException ex) {}
        try {
            DateUtils.ceiling("", Calendar.SECOND);
            fail();
        } catch (final ClassCastException ex) {}
        try {
            DateUtils.ceiling(date1, -9999);
            fail();
        } catch(final IllegalArgumentException ex) {}


        // Fix for http://issues.apache.org/bugzilla/show_bug.cgi?id=25560
        // Test ceiling across the beginning of daylight saving time
        TimeZone.setDefault(zone);
        dateTimeParser.setTimeZone(zone);

        assertEquals("ceiling MET date across DST change-over",
                dateTimeParser.parse("March 31, 2003 00:00:00.000"),
                DateUtils.ceiling(date4, Calendar.DATE));
        assertEquals("ceiling MET date across DST change-over",
                dateTimeParser.parse("March 31, 2003 00:00:00.000"),
                DateUtils.ceiling((Object) cal4, Calendar.DATE));
        assertEquals("ceiling MET date across DST change-over",
                dateTimeParser.parse("March 31, 2003 00:00:00.000"),
                DateUtils.ceiling(date5, Calendar.DATE));
        assertEquals("ceiling MET date across DST change-over",
                dateTimeParser.parse("March 31, 2003 00:00:00.000"),
                DateUtils.ceiling((Object) cal5, Calendar.DATE));
        assertEquals("ceiling MET date across DST change-over",
                dateTimeParser.parse("March 31, 2003 00:00:00.000"),
                DateUtils.ceiling(date6, Calendar.DATE));
        assertEquals("ceiling MET date across DST change-over",
                dateTimeParser.parse("March 31, 2003 00:00:00.000"),
                DateUtils.ceiling((Object) cal6, Calendar.DATE));
        assertEquals("ceiling MET date across DST change-over",
                dateTimeParser.parse("March 31, 2003 00:00:00.000"),
                DateUtils.ceiling(date7, Calendar.DATE));
        assertEquals("ceiling MET date across DST change-over",
                dateTimeParser.parse("March 31, 2003 00:00:00.000"),
                DateUtils.ceiling((Object) cal7, Calendar.DATE));

        assertEquals("ceiling MET date across DST change-over",
                dateTimeParser.parse("March 30, 2003 03:00:00.000"),
                DateUtils.ceiling(date4, Calendar.HOUR_OF_DAY));
        assertEquals("ceiling MET date across DST change-over",
                dateTimeParser.parse("March 30, 2003 03:00:00.000"),
                DateUtils.ceiling((Object) cal4, Calendar.HOUR_OF_DAY));
        assertEquals("ceiling MET date across DST change-over",
                dateTimeParser.parse("March 30, 2003 03:00:00.000"),
                DateUtils.ceiling(date5, Calendar.HOUR_OF_DAY));
        assertEquals("ceiling MET date across DST change-over",
                dateTimeParser.parse("March 30, 2003 03:00:00.000"),
                DateUtils.ceiling((Object) cal5, Calendar.HOUR_OF_DAY));
        assertEquals("ceiling MET date across DST change-over",
                dateTimeParser.parse("March 30, 2003 04:00:00.000"),
                DateUtils.ceiling(date6, Calendar.HOUR_OF_DAY));
        assertEquals("ceiling MET date across DST change-over",
                dateTimeParser.parse("March 30, 2003 04:00:00.000"),
                DateUtils.ceiling((Object) cal6, Calendar.HOUR_OF_DAY));
        assertEquals("ceiling MET date across DST change-over",
                dateTimeParser.parse("March 30, 2003 04:00:00.000"),
                DateUtils.ceiling(date7, Calendar.HOUR_OF_DAY));
        assertEquals("ceiling MET date across DST change-over",
                dateTimeParser.parse("March 30, 2003 04:00:00.000"),
                DateUtils.ceiling((Object) cal7, Calendar.HOUR_OF_DAY));
        TimeZone.setDefault(defaultZone);
        dateTimeParser.setTimeZone(defaultZone);

     // Bug 31395, large dates
        final Date endOfTime = new Date(Long.MAX_VALUE); // fyi: Sun Aug 17 07:12:55 CET 292278994 -- 807 millis
        final GregorianCalendar endCal = new GregorianCalendar();
        endCal.setTime(endOfTime);
        try {
            DateUtils.ceiling(endCal, Calendar.DATE);
            fail();
        } catch (final ArithmeticException ex) {}
        endCal.set(Calendar.YEAR, 280000001);
        try {
            DateUtils.ceiling(endCal, Calendar.DATE);
            fail();
        } catch (final ArithmeticException ex) {}
        endCal.set(Calendar.YEAR, 280000000);
        final Calendar cal = DateUtils.ceiling(endCal, Calendar.DATE);
        assertEquals(0, cal.get(Calendar.HOUR));
    }

    /**
     * Tests the iterator exceptions
     *
     * @throws java.lang.Exception so we don't have to catch it
     */
    @Test
    public void testIteratorEx() throws Exception {
        try {
            DateUtils.iterator(Calendar.getInstance(), -9999);
        } catch (final IllegalArgumentException ex) {}
        try {
            DateUtils.iterator((Date) null, DateUtils.RANGE_WEEK_CENTER);
            fail();
        } catch (final IllegalArgumentException ex) {}
        try {
            DateUtils.iterator((Calendar) null, DateUtils.RANGE_WEEK_CENTER);
            fail();
        } catch (final IllegalArgumentException ex) {}
        try {
            DateUtils.iterator((Object) null, DateUtils.RANGE_WEEK_CENTER);
            fail();
        } catch (final IllegalArgumentException ex) {}
        try {
            DateUtils.iterator("", DateUtils.RANGE_WEEK_CENTER);
            fail();
        } catch (final ClassCastException ex) {}
    }

    /**
     * Tests the calendar iterator for week ranges
     *
     * @throws java.lang.Exception so we don't have to catch it
     */
    @Test
    public void testWeekIterator() throws Exception {
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
            it = DateUtils.iterator((Object) now.getTime(), DateUtils.RANGE_WEEK_CENTER);
            assertWeekIterator(it, centered);
            try {
                it.next();
                fail();
            } catch (final NoSuchElementException ex) {}
            it = DateUtils.iterator(now, DateUtils.RANGE_WEEK_CENTER);
            it.next();
            try {
                it.remove();
            } catch( final UnsupportedOperationException ex) {}

            now.add(Calendar.DATE,1);
        }
    }

    /**
     * Tests the calendar iterator for month-based ranges
     *
     * @throws java.lang.Exception so we don't have to catch it
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

    @SystemDefaults(locale="en")
    @Test
    public void testLANG799_EN_OK() throws ParseException {
        DateUtils.parseDate("Wed, 09 Apr 2008 23:55:38 GMT", "EEE, dd MMM yyyy HH:mm:ss zzz");
        DateUtils.parseDateStrictly("Wed, 09 Apr 2008 23:55:38 GMT", "EEE, dd MMM yyyy HH:mm:ss zzz");
    }

    // Parse German date with English Locale
    @SystemDefaults(locale="en")
    @Test(expected = ParseException.class)
    public void testLANG799_EN_FAIL() throws ParseException {
        DateUtils.parseDate("Mi, 09 Apr 2008 23:55:38 GMT", "EEE, dd MMM yyyy HH:mm:ss zzz");
    }

    @SystemDefaults(locale="de")
    @Test
    public void testLANG799_DE_OK() throws ParseException {
        DateUtils.parseDate("Mi, 09 Apr 2008 23:55:38 GMT", "EEE, dd MMM yyyy HH:mm:ss zzz");
        DateUtils.parseDateStrictly("Mi, 09 Apr 2008 23:55:38 GMT", "EEE, dd MMM yyyy HH:mm:ss zzz");
    }

    // Parse English date with German Locale
    @SystemDefaults(locale="de")
    @Test(expected=ParseException.class)
    public void testLANG799_DE_FAIL() throws ParseException {
        DateUtils.parseDate("Wed, 09 Apr 2008 23:55:38 GMT", "EEE, dd MMM yyyy HH:mm:ss zzz");
    }

    // Parse German date with English Locale, specifying German Locale override
    @SystemDefaults(locale="en")
    @Test
    public void testLANG799_EN_WITH_DE_LOCALE() throws ParseException {
        DateUtils.parseDate("Mi, 09 Apr 2008 23:55:38 GMT", Locale.GERMAN, "EEE, dd MMM yyyy HH:mm:ss zzz");
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
     * Convenience method for when working with Date objects
     */
    private static void assertWeekIterator(final Iterator<?> it, final Date start, final Date end) {
        final Calendar calStart = Calendar.getInstance();
        calStart.setTime(start);
        final Calendar calEnd = Calendar.getInstance();
        calEnd.setTime(end);

        assertWeekIterator(it, calStart, calEnd);
    }

    /**
     * This checks that this is a 7 divisble iterator of Calendar objects
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

        assertFalse("There were " + count + " days in this iterator", count % 7 != 0);
        assertCalendarsEquals("", end, cal, 0);
    }

    /**
     * Used to check that Calendar objects are close enough
     * delta is in milliseconds
     */
    private static void assertCalendarsEquals(final String message, final Calendar cal1, final Calendar cal2, final long delta) {
        assertFalse(message + " expected " + cal1.getTime() + " but got " + cal2.getTime(),
                Math.abs(cal1.getTime().getTime() - cal2.getTime().getTime()) > delta);
    }

    @Test
    public void testLANG799() throws ParseException {
        DateUtils.parseDateStrictly("09 abril 2008 23:55:38 GMT", new Locale("es"), "dd MMM yyyy HH:mm:ss zzz");
    }
}

