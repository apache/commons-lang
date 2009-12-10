/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
// package org.apache.commons.lang;

import java.text.DateFormat;
import java.util.*;

import junit.framework.AssertionFailedError;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * Unit tests {@link org.apache.commons.lang3.CalendarUtils}.
 *
 * @author <a href="mailto:sergek@lokitech.com">Serge Knystautas</a>
 */
public class CalendarUtilsTest extends TestCase {
    DateFormat parser = null;
    Date date1 = null;
    Date date2 = null;

    public CalendarUtilsTest(String name) {
        super(name);
    }

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(CalendarUtilsTest.class);
        suite.setName("CalendarUtilsTest Tests");
        return suite;
    }

    protected void setUp() throws Exception {
        super.setUp();

        parser = new java.text.SimpleDateFormat("MMM dd, yyyy H:mm:ss.SSS");

        date1 = parser.parse("February 12, 2002 12:34:56.789");
        date2 = parser.parse("November 18, 2001 1:23:11.321");
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    //-----------------------------------------------------------------------


    /**
     * Tests various values with the round method
     */
    public void testRound() throws Exception {
        assertEquals("round year-1 failed",
                new Date("2002 January 1"),
                CalendarUtils.round(date1, Calendar.YEAR));
        assertEquals("round year-2 failed",
                new Date("2002 January 1"),
                CalendarUtils.round(date2, Calendar.YEAR));
        assertEquals("round month-1 failed",
                new Date("2002 February 1"),
                CalendarUtils.round(date1, Calendar.MONTH));
        assertEquals("round month-2 failed",
                new Date("2001 December 1"),
                CalendarUtils.round(date2, Calendar.MONTH));
        assertEquals("round semimonth-1 failed",
                new Date("2002 February 16"),
                CalendarUtils.round(date1, CalendarUtils.SEMI_MONTH));
        assertEquals("round semimonth-2 failed",
                new Date("2001 November 16"),
                CalendarUtils.round(date2, CalendarUtils.SEMI_MONTH));
        assertEquals("round date-1 failed",
                new Date("2002 February 13"),
                CalendarUtils.round(date1, Calendar.DATE));
        assertEquals("round date-2 failed",
                new Date("2001 November 18"),
                CalendarUtils.round(date2, Calendar.DATE));
        assertEquals("round hour-1 failed",
                parser.parse("February 12, 2002 13:00:00.000"),
                CalendarUtils.round(date1, Calendar.HOUR));
        assertEquals("round hour-2 failed",
                parser.parse("November 18, 2001 1:00:00.000"),
                CalendarUtils.round(date2, Calendar.HOUR));
        assertEquals("round minute-1 failed",
                parser.parse("February 12, 2002 12:35:00.000"),
                CalendarUtils.round(date1, Calendar.MINUTE));
        assertEquals("round minute-2 failed",
                parser.parse("November 18, 2001 1:23:00.000"),
                CalendarUtils.round(date2, Calendar.MINUTE));
        assertEquals("round second-1 failed",
                parser.parse("February 12, 2002 12:34:57.000"),
                CalendarUtils.round(date1, Calendar.SECOND));
        assertEquals("round second-2 failed",
                parser.parse("November 18, 2001 1:23:11.000"),
                CalendarUtils.round(date2, Calendar.SECOND));
    }

    /**
     * Tests various values with the trunc method
     */
    public void testTrunc() throws Exception {
        assertEquals("trunc year-1 failed",
                new Date("2002 January 1"),
                CalendarUtils.trunc(date1, Calendar.YEAR));
        assertEquals("trunc year-2 failed",
                new Date("2001 January 1"),
                CalendarUtils.trunc(date2, Calendar.YEAR));
        assertEquals("trunc month-1 failed",
                new Date("2002 February 1"),
                CalendarUtils.trunc(date1, Calendar.MONTH));
        assertEquals("trunc month-2 failed",
                new Date("2001 November 1"),
                CalendarUtils.trunc(date2, Calendar.MONTH));
        assertEquals("trunc semimonth-1 failed",
                new Date("2002 February 1"),
                CalendarUtils.trunc(date1, CalendarUtils.SEMI_MONTH));
        assertEquals("trunc semimonth-2 failed",
                new Date("2001 November 16"),
                CalendarUtils.trunc(date2, CalendarUtils.SEMI_MONTH));
        assertEquals("trunc date-1 failed",
                new Date("2002 February 12"),
                CalendarUtils.trunc(date1, Calendar.DATE));
        assertEquals("trunc date-2 failed",
                new Date("2001 November 18"),
                CalendarUtils.trunc(date2, Calendar.DATE));
        assertEquals("trunc hour-1 failed",
                parser.parse("February 12, 2002 12:00:00.000"),
                CalendarUtils.trunc(date1, Calendar.HOUR));
        assertEquals("trunc hour-2 failed",
                parser.parse("November 18, 2001 1:00:00.000"),
                CalendarUtils.trunc(date2, Calendar.HOUR));
        assertEquals("trunc minute-1 failed",
                parser.parse("February 12, 2002 12:34:00.000"),
                CalendarUtils.trunc(date1, Calendar.MINUTE));
        assertEquals("trunc minute-2 failed",
                parser.parse("November 18, 2001 1:23:00.000"),
                CalendarUtils.trunc(date2, Calendar.MINUTE));
        assertEquals("trunc second-1 failed",
                parser.parse("February 12, 2002 12:34:56.000"),
                CalendarUtils.trunc(date1, Calendar.SECOND));
        assertEquals("trunc second-2 failed",
                parser.parse("November 18, 2001 1:23:11.000"),
                CalendarUtils.trunc(date2, Calendar.SECOND));

    }

    /**
     * Tests the parse method, which is supposed to handle various strings
     * as flexibly as CVS supports.
     */
    public void testParse() throws Exception {
        //This is difficult to test since the "now" used in the
        //  parse function cannot be controlled.  We could possibly control
        //  it by trying before and after and making sure the value we expect
        //  is between the two values calculated.
        //For now we're just using the custom assertEquals that takes a delta

        Calendar now = null;

        now = Calendar.getInstance();
        now.add(Calendar.MINUTE, -1);
        assertEquals("parse 1 minute ago",
                now, CalendarUtils.parse("1 minute ago"), 50);
        now = Calendar.getInstance();
        now.add(Calendar.MINUTE, -8);
        assertEquals("parse 8 minutes ago",
                now, CalendarUtils.parse("8 minutes ago"), 50);

        now = Calendar.getInstance();
        now.add(Calendar.DATE, -1);
        assertEquals("parse yesterday",
                now, CalendarUtils.parse("yesterday"), 50);

        now = Calendar.getInstance();
        now.add(Calendar.DATE, 1);
        assertEquals("parse tomorrow",
                now, CalendarUtils.parse("tomorrow"), 50);

        now = Calendar.getInstance();
        //Sunday would be 1, Saturday would be 7, so we walk back up to 6 days.
        if (now.get(Calendar.DATE) == 1) {
            //If Sunday already, we go back a full week
            now.add(Calendar.DATE, -7);
        } else {
            now.add(Calendar.DATE, 1 - now.get(Calendar.DAY_OF_WEEK));
        }
        assertEquals("parse last Sunday",
                now, CalendarUtils.parse("last Sunday"), 50);

        now = Calendar.getInstance();
        now.add(Calendar.DATE, -7);
        assertEquals("parse last week",
                now, CalendarUtils.parse("last week"), 50);

        now = Calendar.getInstance();
        //January would be 0, December would be 11, so we walk back up to 11 months
        if (now.get(Calendar.MONTH) == 0) {
            //If January already, we go back a full year
            now.add(Calendar.MONTH, -12);
        } else {
            now.add(Calendar.MONTH, 0 - now.get(Calendar.MONTH));
        }
        assertEquals("parse last January",
                now, CalendarUtils.parse("last January"), 50);
    }

    /**
     * Tests the calendar iterator for week ranges
     */
    public void testWeekIterator() throws Exception {
        Calendar now = Calendar.getInstance();
        Calendar today = CalendarUtils.trunc(now, Calendar.DATE);
        Calendar sunday = CalendarUtils.trunc(now, Calendar.DATE);
        sunday.add(Calendar.DATE, 1 - sunday.get(Calendar.DAY_OF_WEEK));
        Calendar monday = CalendarUtils.trunc(now, Calendar.DATE);
        if (monday.get(Calendar.DATE) == 1) {
            //This is sunday... roll back 6 days
            monday.add(Calendar.DATE, -6);
        } else {
            monday.add(Calendar.DATE, 2 - monday.get(Calendar.DAY_OF_WEEK));
        }
        Calendar centered = CalendarUtils.trunc(now, Calendar.DATE);
        centered.add(Calendar.DATE, -3);

        Iterator it = CalendarUtils.getCalendarIterator(now, CalendarUtils.RANGE_WEEK_SUNDAY);
        assertWeekIterator(it, sunday);
        it = CalendarUtils.getCalendarIterator(now, CalendarUtils.RANGE_WEEK_MONDAY);
        assertWeekIterator(it, monday);
        it = CalendarUtils.getCalendarIterator(now, CalendarUtils.RANGE_WEEK_RELATIVE);
        assertWeekIterator(it, today);
        it = CalendarUtils.getCalendarIterator(now, CalendarUtils.RANGE_WEEK_CENTER);
        assertWeekIterator(it, centered);
    }

    /**
     * Tests the calendar iterator for month-based ranges
     */
    public void testMonthIterator() throws Exception {
        Iterator it = CalendarUtils.getCalendarIterator(date1, CalendarUtils.RANGE_MONTH_SUNDAY);
        assertWeekIterator(it,
                new Date("January 27, 2002"),
                new Date("March 2, 2002"));

        it = CalendarUtils.getCalendarIterator(date1, CalendarUtils.RANGE_MONTH_MONDAY);
        assertWeekIterator(it,
                new Date("January 28, 2002"),
                new Date("March 3, 2002"));

        it = CalendarUtils.getCalendarIterator(date2, CalendarUtils.RANGE_MONTH_SUNDAY);
        assertWeekIterator(it,
                new Date("October 28, 2001"),
                new Date("December 1, 2001"));

        it = CalendarUtils.getCalendarIterator(date2, CalendarUtils.RANGE_MONTH_MONDAY);
        assertWeekIterator(it,
                new Date("October 29, 2001"),
                new Date("December 2, 2001"));
    }

    /**
     * This checks that this is a 7 element iterator of Calendar objects
     * that are dates (no time), and exactly 1 day spaced after each other.
     */
    private static void assertWeekIterator(Iterator it, Calendar start) {
        Calendar end = (Calendar) start.clone();
        end.add(Calendar.DATE, 6);

        assertWeekIterator(it, start, end);
    }

    /**
     * Convenience method for when working with Date objects
     */
    private static void assertWeekIterator(Iterator it, Date start, Date end) {
        Calendar calStart = Calendar.getInstance();
        calStart.setTime(start);
        Calendar calEnd = Calendar.getInstance();
        calEnd.setTime(end);

        assertWeekIterator(it, calStart, calEnd);
    }

    /**
     * This checks that this is a 7 divisble iterator of Calendar objects
     * that are dates (no time), and exactly 1 day spaced after each other
     * (in addition to the proper start and stop dates)
     */
    private static void assertWeekIterator(Iterator it, Calendar start, Calendar end) {
        Calendar cal = (Calendar) it.next();
        assertEquals("", start, cal, 0);
        Calendar last = null;
        int count = 1;
        while (it.hasNext()) {
            //Check this is just a date (no time component)
            assertEquals("", cal, CalendarUtils.trunc(cal, Calendar.DATE), 0);

            last = cal;
            cal = (Calendar) it.next();
            count++;

            //Check that this is one day more than the last date
            last.add(Calendar.DATE, 1);
            assertEquals("", last, cal, 0);
        }
        if (count % 7 != 0) {
            throw new AssertionFailedError("There were " + count + " days in this iterator");
        }
        assertEquals("", end, cal, 0);
    }

    /**
     * Used to check that Calendar objects are close enough
     * delta is in milliseconds
     */
    public static void assertEquals(String message, Calendar cal1, Calendar cal2, long delta) {
        if (Math.abs(cal1.getTime().getTime() - cal2.getTime().getTime()) > delta) {
            throw new AssertionFailedError(
                    message + " expected " + cal1.getTime() + " but got " + cal2.getTime());
        }
    }
}

