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
 *    any, must include the following acknowledgement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowledgement may appear in the software itself,
 *    if and wherever such third-party acknowledgements normally appear.
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

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.Locale;
import java.util.NoSuchElementException;

import junit.framework.AssertionFailedError;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * Unit tests {@link org.apache.commons.lang.CalendarUtils}.
 *
 * @author <a href="mailto:sergek@lokitech.com">Serge Knystautas</a>
 * @author <a href="mailto:steve@mungoknotwise.com">Steven Caswell</a>
 */
public class DateUtilsTest extends TestCase {
    DateFormat dateParser = null;
    DateFormat dateTimeParser = null;
    Date date1 = null;
    Date date2 = null;
    Calendar cal1 = null;
    Calendar cal2 = null;

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

        dateParser = new SimpleDateFormat("MMM dd, yyyy", Locale.ENGLISH);
        dateTimeParser = new SimpleDateFormat("MMM dd, yyyy H:mm:ss.SSS", Locale.ENGLISH);

        date1 = dateTimeParser.parse("February 12, 2002 12:34:56.789");
        date2 = dateTimeParser.parse("November 18, 2001 1:23:11.321");
        cal1 = Calendar.getInstance();
        cal1.setTime(date1);
        cal2 = Calendar.getInstance();
        cal2.setTime(date2);
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    //-----------------------------------------------------------------------
    public void testConstructor() {
        assertNotNull(new DateUtils());
        Constructor[] cons = DateUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertEquals(true, Modifier.isPublic(cons[0].getModifiers()));
        assertEquals(true, Modifier.isPublic(DateUtils.class.getModifiers()));
        assertEquals(false, Modifier.isFinal(DateUtils.class.getModifiers()));
    }
    
    //-----------------------------------------------------------------------
    /**
     * Tests various values with the round method
     */
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

        try {
            DateUtils.round((Date) null, Calendar.SECOND);
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            DateUtils.round((Calendar) null, Calendar.SECOND);
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            DateUtils.round((Object) null, Calendar.SECOND);
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            DateUtils.round("", Calendar.SECOND);
            fail();
        } catch (ClassCastException ex) {}
        try {
            DateUtils.round(date1, -9999);
            fail();
        } catch(IllegalArgumentException ex) {}
    }

    /**
     * Tests various values with the trunc method
     */
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
        
        assertEquals("truncate calendar second-1 failed",
                dateTimeParser.parse("February 12, 2002 12:34:56.000"),
                DateUtils.truncate((Object) cal1, Calendar.SECOND));
        assertEquals("truncate calendar second-2 failed",
                dateTimeParser.parse("November 18, 2001 1:23:11.000"),
                DateUtils.truncate((Object) cal2, Calendar.SECOND));
        
        try {
            DateUtils.truncate((Date) null, Calendar.SECOND);
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            DateUtils.truncate((Calendar) null, Calendar.SECOND);
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            DateUtils.truncate((Object) null, Calendar.SECOND);
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            DateUtils.truncate("", Calendar.SECOND);
            fail();
        } catch (ClassCastException ex) {}
    }

    // TODO: Decide whether this code is removed or goes into 2.1
    /*
     * Tests the parse method, which is supposed to handle various strings
     * as flexibly as CVS supports.
    public void testParseCVS() throws Exception {
        try {
            DateUtils.parseCVS(null);
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            DateUtils.parseCVS("gobbledegook");
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            DateUtils.parseCVS("ago");
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            DateUtils.parseCVS("1 junk ago");
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            DateUtils.parseCVS("1month ago");
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            DateUtils.parseCVS("last month");
            fail();
        } catch (IllegalArgumentException ex) {}


        //This is difficult to test since the "now" used in the
        //  parse function cannot be controlled.  We could possibly control
        //  it by trying before and after and making sure the value we expect
        //  is between the two values calculated.
        //For now we're just using the custom assertEquals that takes a delta

        Calendar now = null;

        // M/dd/yy H:mm:ss z
        now = Calendar.getInstance();
        now.set(Calendar.MILLISECOND, 0);
        assertEquals("parseCVS format M/dd/yy H:mm:ss z",
                  now, DateUtils.parseCVS(new SimpleDateFormat("M/dd/yy H:mm:ss z").format(now.getTime())), 50);
        // MMM d, yyyy h:mm a
        now = Calendar.getInstance();
        now.set(Calendar.MILLISECOND, 0);
        now.set(Calendar.SECOND, 0);
        assertEquals("parseCVS format MMM d, yyyy h:mm a",
                  now, DateUtils.parseCVS(new SimpleDateFormat("MMM d, yyyy h:mm a").format(now.getTime())), 50);
        // h:mm z
        //
        // This format is difficult to test using the current time because the
        // parseCVS method applies the default date of January 1, 1970 to the
        // parsed time. The most straightforward way to test the parse is to
        // pass in a known value, and test the output against this know value.
        // 
        now = Calendar.getInstance();
        now.setTime(new SimpleDateFormat("h:mm z").parse("16:30 GMT"));
        assertEquals("parseCVS format h:mm z 16:30 GMT", 
                  now, DateUtils.parseCVS("16:30 GMT"), 50);
        now = Calendar.getInstance();
        now.setTime(new SimpleDateFormat("h:mm z").parse("16:30 EST"));
        assertEquals("parseCVS format h:mm z 16:30 EST", 
                  now, DateUtils.parseCVS("16:30 EST"), 50);
        now = Calendar.getInstance();
        now.setTime(new SimpleDateFormat("h:mm z").parse("16:30 GMT-05:00"));
        assertEquals("parseCVS format h:mm z 16:30 GMT-05:00", 
                  now, DateUtils.parseCVS("16:30 GMT-05:00"), 50);
        now = Calendar.getInstance();
        now.setTime(new SimpleDateFormat("h:mm z").parse("16:30 GMT+01:00"));
        assertEquals("parseCVS format h:mm z 16:30 GMT+01:00", 
                  now, DateUtils.parseCVS("16:30 GMT+01:00"), 50);
        
        now = Calendar.getInstance();
        now.setTime(new SimpleDateFormat("h:mm z").parse("06:30 GMT"));
        assertEquals("parseCVS format h:mm z 06:30 GMT", 
                  now, DateUtils.parseCVS("06:30 GMT"), 50);
        now = Calendar.getInstance();
        now.setTime(new SimpleDateFormat("h:mm z").parse("06:30 EST"));
        assertEquals("parseCVS format h:mm z 06:30 EST", 
                  now, DateUtils.parseCVS("06:30 EST"), 50);
        now = Calendar.getInstance();
        now.setTime(new SimpleDateFormat("h:mm z").parse("06:30 GMT-05:00"));
        assertEquals("parseCVS format h:mm z 06:30 GMT-05:00", 
                  now, DateUtils.parseCVS("06:30 GMT-05:00"), 50);
        now = Calendar.getInstance();
        now.setTime(new SimpleDateFormat("h:mm z").parse("06:30 GMT+01:00"));
        assertEquals("parseCVS format h:mm z 06:30 GMT+01:00", 
                  now, DateUtils.parseCVS("06:30 GMT+01:00"), 50);
        
        now = Calendar.getInstance();
        now.add(Calendar.WEEK_OF_MONTH, -1);
        assertEquals("parseCVS a week ago",
                now, DateUtils.parseCVS("a week ago"), 50);
        now = Calendar.getInstance();
        now.add(Calendar.WEEK_OF_MONTH, -1);
        assertEquals("parseCVS an week ago",
                now, DateUtils.parseCVS("an week ago"), 50);
        
        now = Calendar.getInstance();
        now.add(Calendar.DAY_OF_MONTH, -14);
        assertEquals("parseCVS 1 fortnight ago",
                now, DateUtils.parseCVS("1 fortnight ago"), 50);
        now = Calendar.getInstance();
        now.add(Calendar.DAY_OF_MONTH, -14);
        assertEquals("parseCVS 1 fortnights ago",
                now, DateUtils.parseCVS("1 fortnights ago"), 50);
        
        now = Calendar.getInstance();
        now.add(Calendar.MINUTE, -1);
        assertEquals("parseCVS 1 minute ago",
                now, DateUtils.parseCVS("1 minute ago"), 50);
        now = Calendar.getInstance();
        now.add(Calendar.MINUTE, -8);
        assertEquals("parseCVS 8 minutes ago",
                now, DateUtils.parseCVS("8 minutes ago"), 50);

        now = Calendar.getInstance();
        now.add(Calendar.MILLISECOND, -1);
        assertEquals("parseCVS 1 millisecond ago",
                now, DateUtils.parseCVS("1 millisecond ago"), 50);
        now = Calendar.getInstance();
        now.add(Calendar.MILLISECOND, -100);
        assertEquals("parseCVS 1 milliseconds ago",
                now, DateUtils.parseCVS("100 milliseconds ago"), 50);
        now = Calendar.getInstance();
        now.add(Calendar.SECOND, -30);
        assertEquals("parseCVS 30 second ago",
                now, DateUtils.parseCVS("30 second ago"), 50);
        now = Calendar.getInstance();
        now.add(Calendar.SECOND, -30);
        assertEquals("parseCVS 30 seconds ago",
                now, DateUtils.parseCVS("30 seconds ago"), 50);
        now = Calendar.getInstance();
        now.add(Calendar.HOUR, -2);
        assertEquals("parseCVS 2 hour ago",
                now, DateUtils.parseCVS("2 hour ago"), 50);
        now = Calendar.getInstance();
        now.add(Calendar.HOUR, -2);
        assertEquals("parseCVS 2 hours ago",
                now, DateUtils.parseCVS("2 hours ago"), 50);
        now = Calendar.getInstance();
        now.add(Calendar.DAY_OF_MONTH, -2);
        assertEquals("parseCVS 2 day ago",
                now, DateUtils.parseCVS("2 day ago"), 50);
        now = Calendar.getInstance();
        now.add(Calendar.DAY_OF_MONTH, -2);
        assertEquals("parseCVS 2 days ago",
                now, DateUtils.parseCVS("2 days ago"), 50);
        now = Calendar.getInstance();
        now.add(Calendar.MONTH, -2);
        assertEquals("parseCVS 2 month ago",
                now, DateUtils.parseCVS("2 month ago"), 50);
        now = Calendar.getInstance();
        now.add(Calendar.MONTH, -2);
        assertEquals("parseCVS 2 months ago",
                now, DateUtils.parseCVS("2 months ago"), 50);
        now = Calendar.getInstance();
        now.add(Calendar.YEAR, -2);
        assertEquals("parseCVS 2 year ago",
                now, DateUtils.parseCVS("2 year ago"), 50);
        now = Calendar.getInstance();
        now.add(Calendar.YEAR, -2);
        assertEquals("parseCVS 2 years ago",
                now, DateUtils.parseCVS("2 years ago"), 50);
        
        now = Calendar.getInstance();
        now.add(Calendar.DATE, -1);
        assertEquals("parseCVS yesterday",
                now, DateUtils.parseCVS("yesterday"), 50);

        now = Calendar.getInstance();
        now.add(Calendar.DATE, 1);
        assertEquals("parseCVS tomorrow",
                now, DateUtils.parseCVS("tomorrow"), 50);

        now = Calendar.getInstance();
        //Sunday would be 1, Saturday would be 7, so we walk back up to 6 days.
        if (now.get(Calendar.DAY_OF_WEEK) == 1) {
            //If Sunday already, we go back a full week
            now.add(Calendar.DATE, -7);
        } else {
            now.add(Calendar.DATE, 1 - now.get(Calendar.DAY_OF_WEEK));
        }
        assertEquals("parseCVS last Sunday",
                now, DateUtils.parseCVS("last Sunday"), 50);

        now = Calendar.getInstance();
        now.add(Calendar.DATE, -7);
        assertEquals("parseCVS last week",
                now, DateUtils.parseCVS("last week"), 50);
        
        now = Calendar.getInstance();
        //January would be 0, December would be 11, so we walk back up to 11 months
        if (now.get(Calendar.MONTH) == 0) {
            //If January already, we go back a full year
            now.add(Calendar.MONTH, -12);
        } else {
            now.add(Calendar.MONTH, 0 - now.get(Calendar.MONTH));
        }
        assertEquals("parseCVS last January",
                now, DateUtils.parseCVS("last January"), 50);
    }
     */

    /**
     * Tests the iterator exceptions
     */
    public void testIteratorEx() throws Exception {
        try {
            DateUtils.iterator(Calendar.getInstance(), -9999);
        } catch (IllegalArgumentException ex) {}
        try {
            DateUtils.iterator((Date) null, DateUtils.RANGE_WEEK_CENTER);
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            DateUtils.iterator((Calendar) null, DateUtils.RANGE_WEEK_CENTER);
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            DateUtils.iterator((Object) null, DateUtils.RANGE_WEEK_CENTER);
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            DateUtils.iterator("", DateUtils.RANGE_WEEK_CENTER);
            fail();
        } catch (ClassCastException ex) {}
    }

    /**
     * Tests the calendar iterator for week ranges
     */
    public void testWeekIterator() throws Exception {
        Calendar now = Calendar.getInstance();
        for (int i = 0; i< 7; i++) {
            Calendar today = DateUtils.truncate(now, Calendar.DATE);
            Calendar sunday = DateUtils.truncate(now, Calendar.DATE);
            sunday.add(Calendar.DATE, 1 - sunday.get(Calendar.DAY_OF_WEEK));
            Calendar monday = DateUtils.truncate(now, Calendar.DATE);
            if (monday.get(Calendar.DAY_OF_WEEK) == 1) {
                //This is sunday... roll back 6 days
                monday.add(Calendar.DATE, -6);
            } else {
                monday.add(Calendar.DATE, 2 - monday.get(Calendar.DAY_OF_WEEK));
            }
            Calendar centered = DateUtils.truncate(now, Calendar.DATE);
            centered.add(Calendar.DATE, -3);
            
            Iterator it = DateUtils.iterator(now, DateUtils.RANGE_WEEK_SUNDAY);
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
            } catch (NoSuchElementException ex) {}
            it = DateUtils.iterator(now, DateUtils.RANGE_WEEK_CENTER);
            it.next();
            try {
                it.remove();
            } catch( UnsupportedOperationException ex) {}
            
            now.add(Calendar.DATE,1);
        }
    }
            
    /**
     * Tests the calendar iterator for month-based ranges
     */
    public void testMonthIterator() throws Exception {
        Iterator it = DateUtils.iterator(date1, DateUtils.RANGE_MONTH_SUNDAY);
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
            assertEquals("", cal, DateUtils.truncate(cal, Calendar.DATE), 0);

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

