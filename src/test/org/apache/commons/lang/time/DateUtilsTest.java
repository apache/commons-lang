/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002-2003 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution, if
 *    any, must include the following acknowlegement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowlegement may appear in the software itself,
 *    if and wherever such third-party acknowlegements normally appear.
 *
 * 4. The names "The Jakarta Project", "Commons", and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 */
package org.apache.commons.lang.time;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;

import junit.framework.AssertionFailedError;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * Unit tests {@link org.apache.commons.lang.CalendarUtils}.
 *
 * @author <a href="mailto:sergek@lokitech.com">Serge Knystautas</a>
 */
public class DateUtilsTest extends TestCase {
    DateFormat dateParser = null;
    DateFormat dateTimeParser = null;
    Date date1 = null;
    Date date2 = null;

    public DateUtilsTest(String name) {
        super(name);
    }

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
    	TestSuite suite = new TestSuite(DateUtilsTest.class);
    	suite.setName("CalendarUtilsTest Tests");
        return suite;
    }

    protected void setUp() throws Exception {
        super.setUp();

        dateParser = new SimpleDateFormat("MMM dd, yyyy");
        dateTimeParser = new SimpleDateFormat("MMM dd, yyyy H:mm:ss.SSS");

        date1 = dateTimeParser.parse("February 12, 2002 12:34:56.789");
        date2 = dateTimeParser.parse("November 18, 2001 1:23:11.321");
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
    }

    /**
     * Tests various values with the trunc method
     */
    public void testTrunc() throws Exception {
        assertEquals("trunc year-1 failed",
                dateParser.parse("January 1, 2002"),
                DateUtils.trunc(date1, Calendar.YEAR));
        assertEquals("trunc year-2 failed",
                dateParser.parse("January 1, 2001"),
                DateUtils.trunc(date2, Calendar.YEAR));
        assertEquals("trunc month-1 failed",
                dateParser.parse("February 1, 2002"),
                DateUtils.trunc(date1, Calendar.MONTH));
        assertEquals("trunc month-2 failed",
                dateParser.parse("November 1, 2001"),
                DateUtils.trunc(date2, Calendar.MONTH));
        assertEquals("trunc semimonth-1 failed",
                dateParser.parse("February 1, 2002"),
                DateUtils.trunc(date1, DateUtils.SEMI_MONTH));
        assertEquals("trunc semimonth-2 failed",
                dateParser.parse("November 16, 2001"),
                DateUtils.trunc(date2, DateUtils.SEMI_MONTH));
        assertEquals("trunc date-1 failed",
                dateParser.parse("February 12, 2002"),
                DateUtils.trunc(date1, Calendar.DATE));
        assertEquals("trunc date-2 failed",
                dateParser.parse("November 18, 2001"),
                DateUtils.trunc(date2, Calendar.DATE));
        assertEquals("trunc hour-1 failed",
                dateTimeParser.parse("February 12, 2002 12:00:00.000"),
                DateUtils.trunc(date1, Calendar.HOUR));
        assertEquals("trunc hour-2 failed",
                dateTimeParser.parse("November 18, 2001 1:00:00.000"),
                DateUtils.trunc(date2, Calendar.HOUR));
        assertEquals("trunc minute-1 failed",
                dateTimeParser.parse("February 12, 2002 12:34:00.000"),
                DateUtils.trunc(date1, Calendar.MINUTE));
        assertEquals("trunc minute-2 failed",
                dateTimeParser.parse("November 18, 2001 1:23:00.000"),
                DateUtils.trunc(date2, Calendar.MINUTE));
        assertEquals("trunc second-1 failed",
                dateTimeParser.parse("February 12, 2002 12:34:56.000"),
                DateUtils.trunc(date1, Calendar.SECOND));
        assertEquals("trunc second-2 failed",
                dateTimeParser.parse("November 18, 2001 1:23:11.000"),
                DateUtils.trunc(date2, Calendar.SECOND));

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
                now, DateUtils.parse("1 minute ago"), 50);
        now = Calendar.getInstance();
        now.add(Calendar.MINUTE, -8);
        assertEquals("parse 8 minutes ago",
                now, DateUtils.parse("8 minutes ago"), 50);

        now = Calendar.getInstance();
        now.add(Calendar.DATE, -1);
        assertEquals("parse yesterday",
                now, DateUtils.parse("yesterday"), 50);

        now = Calendar.getInstance();
        now.add(Calendar.DATE, 1);
        assertEquals("parse tomorrow",
                now, DateUtils.parse("tomorrow"), 50);

        now = Calendar.getInstance();
        //Sunday would be 1, Saturday would be 7, so we walk back up to 6 days.
        if (now.get(Calendar.DAY_OF_WEEK) == 1) {
            //If Sunday already, we go back a full week
            now.add(Calendar.DATE, -7);
        } else {
            now.add(Calendar.DATE, 1 - now.get(Calendar.DAY_OF_WEEK));
        }
        assertEquals("parse last Sunday",
                now, DateUtils.parse("last Sunday"), 50);

        now = Calendar.getInstance();
        now.add(Calendar.DATE, -7);
        assertEquals("parse last week",
                now, DateUtils.parse("last week"), 50);

        now = Calendar.getInstance();
        //January would be 0, December would be 11, so we walk back up to 11 months
        if (now.get(Calendar.MONTH) == 0) {
            //If January already, we go back a full year
            now.add(Calendar.MONTH, -12);
        } else {
            now.add(Calendar.MONTH, 0 - now.get(Calendar.MONTH));
        }
        assertEquals("parse last January",
                now, DateUtils.parse("last January"), 50);
    }

    /**
     * Tests the calendar iterator for week ranges
     */
    public void testWeekIterator() throws Exception {
        Calendar now = Calendar.getInstance();
        for (int i = 0; i< 7; i++) {
            Calendar today = DateUtils.trunc(now, Calendar.DATE);
            Calendar sunday = DateUtils.trunc(now, Calendar.DATE);
            sunday.add(Calendar.DATE, 1 - sunday.get(Calendar.DAY_OF_WEEK));
            Calendar monday = DateUtils.trunc(now, Calendar.DATE);
            if (monday.get(Calendar.DAY_OF_WEEK) == 1) {
                //This is sunday... roll back 6 days
                monday.add(Calendar.DATE, -6);
            } else {
                monday.add(Calendar.DATE, 2 - monday.get(Calendar.DAY_OF_WEEK));
            }
            Calendar centered = DateUtils.trunc(now, Calendar.DATE);
            centered.add(Calendar.DATE, -3);
            
            Iterator it = DateUtils.getCalendarIterator(now, DateUtils.RANGE_WEEK_SUNDAY);
            assertWeekIterator(it, sunday);
            it = DateUtils.getCalendarIterator(now, DateUtils.RANGE_WEEK_MONDAY);
            assertWeekIterator(it, monday);
            it = DateUtils.getCalendarIterator(now, DateUtils.RANGE_WEEK_RELATIVE);
            assertWeekIterator(it, today);
            it = DateUtils.getCalendarIterator(now, DateUtils.RANGE_WEEK_CENTER);
            assertWeekIterator(it, centered);
            now.add(Calendar.DATE,1);
        }
    }

    /**
     * Tests the calendar iterator for month-based ranges
     */
    public void testMonthIterator() throws Exception {
        Iterator it = DateUtils.getCalendarIterator(date1, DateUtils.RANGE_MONTH_SUNDAY);
        assertWeekIterator(it,
                dateParser.parse("January 27, 2002"),
                dateParser.parse("March 2, 2002"));

        it = DateUtils.getCalendarIterator(date1, DateUtils.RANGE_MONTH_MONDAY);
        assertWeekIterator(it,
                dateParser.parse("January 28, 2002"),
                dateParser.parse("March 3, 2002"));

        it = DateUtils.getCalendarIterator(date2, DateUtils.RANGE_MONTH_SUNDAY);
        assertWeekIterator(it,
                dateParser.parse("October 28, 2001"),
                dateParser.parse("December 1, 2001"));

        it = DateUtils.getCalendarIterator(date2, DateUtils.RANGE_MONTH_MONDAY);
        assertWeekIterator(it,
                dateParser.parse("October 29, 2001"),
                dateParser.parse("December 2, 2001"));
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
            assertEquals("", cal, DateUtils.trunc(cal, Calendar.DATE), 0);

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

