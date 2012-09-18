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

import org.junit.Test;
import org.junit.Before;
import static org.junit.Assert.*;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;

/**
 * These Unit-tests will check all possible extremes when using some rounding-methods of DateUtils.
 * The extremes are tested at the switch-point in milliseconds
 * 
 * According to the implementation SEMI_MONTH will either round/truncate to the 1st or 16th
 * When rounding Calendar.MONTH it depends on the number of days within that month.
 * A month with 28 days will be rounded up from the 15th
 * A month with 29 or 30 days will be rounded up from the 16th
 * A month with 31 days will be rounded up from the 17th
 * 
 * @since 3.0
 * @version $Id$
 */
public class DateUtilsRoundingTest {

    DateFormat dateTimeParser;
    
    Date januaryOneDate;
    Date targetYearDate;
    //No targetMonths, these must be tested for every type of month(28-31 days)
    Date targetDateDate, targetDayOfMonthDate, targetAmDate, targetPmDate;
    Date targetHourOfDayDate, targetHourDate;
    Date targetMinuteDate;
    Date targetSecondDate;
    Date targetMilliSecondDate;

    Calendar januaryOneCalendar;
    FastDateFormat fdf = DateFormatUtils.ISO_DATETIME_FORMAT;


    @Before
    public void setUp() throws Exception {

        dateTimeParser = new SimpleDateFormat("MMM dd, yyyy H:mm:ss.SSS", Locale.ENGLISH);
        
        targetYearDate = dateTimeParser.parse("January 1, 2007 0:00:00.000");
        targetDateDate = targetDayOfMonthDate = dateTimeParser.parse("June 1, 2008 0:00:00.000");
        targetAmDate =  dateTimeParser.parse("June 1, 2008 0:00:00.000");
        targetPmDate = dateTimeParser.parse("June 1, 2008 12:00:00.000");
        targetHourDate = dateTimeParser.parse("June 1, 2008 8:00:00.000");
        targetHourOfDayDate = dateTimeParser.parse("June 1, 2008 8:00:00.000");
        targetMinuteDate =  dateTimeParser.parse("June 1, 2008 8:15:00.000");
        targetSecondDate =  dateTimeParser.parse("June 1, 2008 8:15:14.000");
        targetMilliSecondDate =  dateTimeParser.parse("June 1, 2008 8:15:14.231");
        
        januaryOneDate = dateTimeParser.parse("January 1, 2008 0:00:00.000");
        januaryOneCalendar = Calendar.getInstance();
        januaryOneCalendar.setTime(januaryOneDate);
    }

    /**
     * Tests DateUtils.round()-method with Calendar.Year
     * 
     * @throws Exception
     * @since 3.0
     */
    @Test
    public void testRoundYear() throws Exception {
        final int calendarField = Calendar.YEAR;
        Date roundedUpDate = dateTimeParser.parse("January 1, 2008 0:00:00.000");
        Date roundedDownDate = targetYearDate;
        Date lastRoundedDownDate = dateTimeParser.parse("June 30, 2007 23:59:59.999");
        baseRoundTest(roundedUpDate, roundedDownDate, lastRoundedDownDate,  calendarField);
    }
    
    /**
     * Tests DateUtils.round()-method with Calendar.MONTH
     * Includes rounding months with 28, 29, 30 and 31 days
     * Includes rounding to January 1
     * 
     * @throws Exception
     * @since 3.0
     */
    @Test
    public void testRoundMonth() throws Exception {
        final int calendarField = Calendar.MONTH;
        Date roundedUpDate, roundedDownDate, lastRoundedDownDate;
        Date minDate, maxDate;
        
        //month with 28 days
        roundedUpDate = dateTimeParser.parse("March 1, 2007 0:00:00.000");
        roundedDownDate = dateTimeParser.parse("February 1, 2007 0:00:00.000");
        lastRoundedDownDate = dateTimeParser.parse("February 14, 2007 23:59:59.999");
        baseRoundTest(roundedUpDate, roundedDownDate, lastRoundedDownDate,  calendarField);

        //month with 29 days
        roundedUpDate = dateTimeParser.parse("March 1, 2008 0:00:00.000");
        roundedDownDate = dateTimeParser.parse("February 1, 2008 0:00:00.000");
        lastRoundedDownDate = dateTimeParser.parse("February 15, 2008 23:59:59.999");
        baseRoundTest(roundedUpDate, roundedDownDate, lastRoundedDownDate,  calendarField);
        
        //month with 30 days
        roundedUpDate = dateTimeParser.parse("May 1, 2008 0:00:00.000");
        roundedDownDate = dateTimeParser.parse("April 1, 2008 0:00:00.000");
        lastRoundedDownDate = dateTimeParser.parse("April 15, 2008 23:59:59.999");
        baseRoundTest(roundedUpDate, roundedDownDate, lastRoundedDownDate,  calendarField);
        
        //month with 31 days
        roundedUpDate = dateTimeParser.parse("June 1, 2008 0:00:00.000");
        roundedDownDate = dateTimeParser.parse("May 1, 2008 0:00:00.000");
        lastRoundedDownDate = dateTimeParser.parse("May 16, 2008 23:59:59.999");
        baseRoundTest(roundedUpDate, roundedDownDate, lastRoundedDownDate,  calendarField);
        
        //round to January 1
        minDate = dateTimeParser.parse("December 17, 2007 00:00:00.000");
        maxDate = dateTimeParser.parse("January 16, 2008 23:59:59.999");
        roundToJanuaryFirst(minDate, maxDate, calendarField);
    }
    
    /**
     * Tests DateUtils.round()-method with DateUtils.SEMI_MONTH
     * Includes rounding months with 28, 29, 30 and 31 days, each with first and second half 
     * Includes rounding to January 1
     *      
     * @throws Exception
     * @since 3.0
     */
    @Test
    public void testRoundSemiMonth() throws Exception {
        final int calendarField = DateUtils.SEMI_MONTH;
        Date roundedUpDate, roundedDownDate, lastRoundedDownDate;
        Date minDate, maxDate;
        
        //month with 28 days (1)
        roundedUpDate = dateTimeParser.parse("February 16, 2007 0:00:00.000");
        roundedDownDate = dateTimeParser.parse("February 1, 2007 0:00:00.000");
        lastRoundedDownDate = dateTimeParser.parse("February 8, 2007 23:59:59.999");
        baseRoundTest(roundedUpDate, roundedDownDate, lastRoundedDownDate,  calendarField);

        //month with 28 days (2)
        roundedUpDate = dateTimeParser.parse("March 1, 2007 0:00:00.000");
        roundedDownDate = dateTimeParser.parse("February 16, 2007 0:00:00.000");
        lastRoundedDownDate = dateTimeParser.parse("February 23, 2007 23:59:59.999");
        baseRoundTest(roundedUpDate, roundedDownDate, lastRoundedDownDate,  calendarField);

        //month with 29 days (1)
        roundedUpDate = dateTimeParser.parse("February 16, 2008 0:00:00.000");
        roundedDownDate = dateTimeParser.parse("February 1, 2008 0:00:00.000");
        lastRoundedDownDate = dateTimeParser.parse("February 8, 2008 23:59:59.999");
        baseRoundTest(roundedUpDate, roundedDownDate, lastRoundedDownDate,  calendarField);
        
        //month with 29 days (2)
        roundedUpDate = dateTimeParser.parse("March 1, 2008 0:00:00.000");
        roundedDownDate = dateTimeParser.parse("February 16, 2008 0:00:00.000");
        lastRoundedDownDate = dateTimeParser.parse("February 23, 2008 23:59:59.999");
        baseRoundTest(roundedUpDate, roundedDownDate, lastRoundedDownDate,  calendarField);

        //month with 30 days (1)
        roundedUpDate = dateTimeParser.parse("April 16, 2008 0:00:00.000");
        roundedDownDate = dateTimeParser.parse("April 1, 2008 0:00:00.000");
        lastRoundedDownDate = dateTimeParser.parse("April 8, 2008 23:59:59.999");
        baseRoundTest(roundedUpDate, roundedDownDate, lastRoundedDownDate,  calendarField);

        //month with 30 days (2)
        roundedUpDate = dateTimeParser.parse("May 1, 2008 0:00:00.000");
        roundedDownDate = dateTimeParser.parse("April 16, 2008 0:00:00.000");
        lastRoundedDownDate = dateTimeParser.parse("April 23, 2008 23:59:59.999");
        baseRoundTest(roundedUpDate, roundedDownDate, lastRoundedDownDate,  calendarField);
        
        //month with 31 days (1)
        roundedUpDate = dateTimeParser.parse("May 16, 2008 0:00:00.000");
        roundedDownDate = dateTimeParser.parse("May 1, 2008 0:00:00.000");
        lastRoundedDownDate = dateTimeParser.parse("May 8, 2008 23:59:59.999");
        baseRoundTest(roundedUpDate, roundedDownDate, lastRoundedDownDate,  calendarField);

        //month with 31 days (2)
        roundedUpDate = dateTimeParser.parse("June 1, 2008 0:00:00.000");
        roundedDownDate = dateTimeParser.parse("May 16, 2008 0:00:00.000");
        lastRoundedDownDate = dateTimeParser.parse("May 23, 2008 23:59:59.999");
        baseRoundTest(roundedUpDate, roundedDownDate, lastRoundedDownDate,  calendarField);
        
        //round to January 1
        minDate = dateTimeParser.parse("December 24, 2007 00:00:00.000");
        maxDate = dateTimeParser.parse("January 8, 2008 23:59:59.999");
        roundToJanuaryFirst(minDate, maxDate, calendarField);
    }
    
    /**
     * Tests DateUtils.round()-method with Calendar.DATE
     * Includes rounding the extremes of one day 
     * Includes rounding to January 1
     * 
     * @throws Exception
     * @since 3.0
     */
    @Test
    public void testRoundDate() throws Exception {
        final int calendarField = Calendar.DATE;
        Date roundedUpDate, roundedDownDate, lastRoundedDownDate;
        Date minDate, maxDate;

        roundedUpDate = dateTimeParser.parse("June 2, 2008 0:00:00.000");
        roundedDownDate = targetDateDate;
        lastRoundedDownDate = dateTimeParser.parse("June 1, 2008 11:59:59.999");
        baseRoundTest(roundedUpDate, roundedDownDate, lastRoundedDownDate,  calendarField);
        
        //round to January 1
        minDate = dateTimeParser.parse("December 31, 2007 12:00:00.000");
        maxDate = dateTimeParser.parse("January 1, 2008 11:59:59.999");
        roundToJanuaryFirst(minDate, maxDate, calendarField);
    }
    
    /**
     * Tests DateUtils.round()-method with Calendar.DAY_OF_MONTH
     * Includes rounding the extremes of one day 
     * Includes rounding to January 1
     * 
     * @throws Exception
     * @since 3.0
     */
    @Test
    public void testRoundDayOfMonth() throws Exception {
        final int calendarField = Calendar.DAY_OF_MONTH;
        Date roundedUpDate, roundedDownDate, lastRoundedDownDate;
        Date minDate, maxDate;

        roundedUpDate = dateTimeParser.parse("June 2, 2008 0:00:00.000");
        roundedDownDate = targetDayOfMonthDate;
        lastRoundedDownDate = dateTimeParser.parse("June 1, 2008 11:59:59.999");
        baseRoundTest(roundedUpDate, roundedDownDate, lastRoundedDownDate,  calendarField);
        
        //round to January 1
        minDate = dateTimeParser.parse("December 31, 2007 12:00:00.000");
        maxDate = dateTimeParser.parse("January 1, 2008 11:59:59.999");
        roundToJanuaryFirst(minDate, maxDate, calendarField);
    }
    
    /**
     * Tests DateUtils.round()-method with Calendar.AM_PM
     * Includes rounding the extremes of both AM and PM of one day 
     * Includes rounding to January 1
     * 
     * @throws Exception
     * @since 3.0
     */
    @Test
    public void testRoundAmPm() throws Exception {
        final int calendarField = Calendar.AM_PM;
        Date roundedUpDate, roundedDownDate, lastRoundedDownDate;
        Date minDate, maxDate;

        //AM
        roundedUpDate = dateTimeParser.parse("June 1, 2008 12:00:00.000");
        roundedDownDate = targetAmDate;
        lastRoundedDownDate = dateTimeParser.parse("June 1, 2008 5:59:59.999");
        baseRoundTest(roundedUpDate, roundedDownDate, lastRoundedDownDate,  calendarField);

        //PM
        roundedUpDate = dateTimeParser.parse("June 2, 2008 0:00:00.000");
        roundedDownDate = targetPmDate;
        lastRoundedDownDate = dateTimeParser.parse("June 1, 2008 17:59:59.999");
        baseRoundTest(roundedUpDate, roundedDownDate, lastRoundedDownDate,  calendarField);

        //round to January 1
        minDate = dateTimeParser.parse("December 31, 2007 18:00:00.000");
        maxDate = dateTimeParser.parse("January 1, 2008 5:59:59.999");
        roundToJanuaryFirst(minDate, maxDate, calendarField);
    }
    
    /**
     * Tests DateUtils.round()-method with Calendar.HOUR_OF_DAY
     * Includes rounding the extremes of one hour 
     * Includes rounding to January 1
     * 
     * @throws Exception
     * @since 3.0
     */
    @Test
    public void testRoundHourOfDay() throws Exception {
        final int calendarField = Calendar.HOUR_OF_DAY;
        Date roundedUpDate, roundedDownDate, lastRoundedDownDate;
        Date minDate, maxDate;

        roundedUpDate = dateTimeParser.parse("June 1, 2008 9:00:00.000");
        roundedDownDate = targetHourOfDayDate;
        lastRoundedDownDate = dateTimeParser.parse("June 1, 2008 8:29:59.999");
        baseRoundTest(roundedUpDate, roundedDownDate, lastRoundedDownDate,  calendarField);
        
        //round to January 1
        minDate = dateTimeParser.parse("December 31, 2007 23:30:00.000");
        maxDate = dateTimeParser.parse("January 1, 2008 0:29:59.999");
        roundToJanuaryFirst(minDate, maxDate, calendarField);
    }
    
    /**
     * Tests DateUtils.round()-method with Calendar.HOUR
     * Includes rounding the extremes of one hour 
     * Includes rounding to January 1
     * 
     * @throws Exception
     * @since 3.0
     */
    @Test
    public void testRoundHour() throws Exception {
        final int calendarField = Calendar.HOUR;
        Date roundedUpDate, roundedDownDate, lastRoundedDownDate;
        Date minDate, maxDate;

        roundedUpDate = dateTimeParser.parse("June 1, 2008 9:00:00.000");
        roundedDownDate = targetHourDate;
        lastRoundedDownDate = dateTimeParser.parse("June 1, 2008 8:29:59.999");
        baseRoundTest(roundedUpDate, roundedDownDate, lastRoundedDownDate,  calendarField);
        
        //round to January 1
        minDate = dateTimeParser.parse("December 31, 2007 23:30:00.000");
        maxDate = dateTimeParser.parse("January 1, 2008 0:29:59.999");
        roundToJanuaryFirst(minDate, maxDate, calendarField);
    }
    
    /**
     * Tests DateUtils.round()-method with Calendar.MINUTE
     * Includes rounding the extremes of one minute 
     * Includes rounding to January 1
     * 
     * @throws Exception
     * @since 3.0
     */
    @Test
    public void testRoundMinute() throws Exception {
        final int calendarField = Calendar.MINUTE;
        Date roundedUpDate, roundedDownDate, lastRoundedDownDate;
        Date minDate, maxDate;

        roundedUpDate = dateTimeParser.parse("June 1, 2008 8:16:00.000");
        roundedDownDate = targetMinuteDate;
        lastRoundedDownDate = dateTimeParser.parse("June 1, 2008 8:15:29.999");
        baseRoundTest(roundedUpDate, roundedDownDate, lastRoundedDownDate,  calendarField);
        
        //round to January 1
        minDate = dateTimeParser.parse("December 31, 2007 23:59:30.000");
        maxDate = dateTimeParser.parse("January 1, 2008 0:00:29.999");
        roundToJanuaryFirst(minDate, maxDate, calendarField);
    }
    
    /**
     * Tests DateUtils.round()-method with Calendar.SECOND
     * Includes rounding the extremes of one second 
     * Includes rounding to January 1
     * 
     * @throws Exception
     * @since 3.0
     */
    @Test
    public void testRoundSecond() throws Exception {
        final int calendarField = Calendar.SECOND;
        Date roundedUpDate, roundedDownDate, lastRoundedDownDate;
        Date minDate, maxDate;

        roundedUpDate = dateTimeParser.parse("June 1, 2008 8:15:15.000");
        roundedDownDate = targetSecondDate;
        lastRoundedDownDate = dateTimeParser.parse("June 1, 2008 8:15:14.499");
        baseRoundTest(roundedUpDate, roundedDownDate, lastRoundedDownDate,  calendarField);
        
        //round to January 1
        minDate = dateTimeParser.parse("December 31, 2007 23:59:59.500");
        maxDate = dateTimeParser.parse("January 1, 2008 0:00:00.499");
        roundToJanuaryFirst(minDate, maxDate, calendarField);
    }
    
    /**
     * Tests DateUtils.round()-method with Calendar.MILLISECOND
     * Includes rounding the extremes of one second 
     * Includes rounding to January 1
     * 
     * @throws Exception
     * @since 3.0
     */
    @Test
    public void testRoundMilliSecond() throws Exception {
        final int calendarField = Calendar.MILLISECOND;
        Date roundedUpDate, roundedDownDate, lastRoundedDownDate;
        Date minDate, maxDate;

        roundedDownDate = lastRoundedDownDate = targetMilliSecondDate;
        roundedUpDate = dateTimeParser.parse("June 1, 2008 8:15:14.232");
        baseRoundTest(roundedUpDate, roundedDownDate, lastRoundedDownDate,  calendarField);
        
        //round to January 1
        minDate = maxDate = januaryOneDate;
        roundToJanuaryFirst(minDate, maxDate, calendarField);
    }
    
    /**
     * Test DateUtils.truncate()-method with Calendar.YEAR
     * 
     * @throws Exception
     * @since 3.0
     */
    @Test
    public void testTruncateYear() throws Exception {
        final int calendarField = Calendar.YEAR;
        Date lastTruncateDate = dateTimeParser.parse("December 31, 2007 23:59:59.999");
        baseTruncateTest(targetYearDate, lastTruncateDate, calendarField);
    }

    /**
     * Test DateUtils.truncate()-method with Calendar.MONTH
     * 
     * @throws Exception
     * @since 3.0
     */
    @Test
    public void testTruncateMonth() throws Exception {
        final int calendarField = Calendar.MONTH;
        Date truncatedDate = dateTimeParser.parse("March 1, 2008 0:00:00.000");
        Date lastTruncateDate = dateTimeParser.parse("March 31, 2008 23:59:59.999");
        baseTruncateTest(truncatedDate, lastTruncateDate, calendarField);
    }

    /**
     * Test DateUtils.truncate()-method with DateUtils.SEMI_MONTH
     * Includes truncating months with 28, 29, 30 and 31 days, each with first and second half
     * 
     * @throws Exception
     * @since 3.0
     */
    @Test
    public void testTruncateSemiMonth() throws Exception {
        final int calendarField = DateUtils.SEMI_MONTH;
        Date truncatedDate, lastTruncateDate;
        
        //month with 28 days (1)
        truncatedDate = dateTimeParser.parse("February 1, 2007 0:00:00.000");
        lastTruncateDate = dateTimeParser.parse("February 15, 2007 23:59:59.999");
        baseTruncateTest(truncatedDate, lastTruncateDate, calendarField);

        //month with 28 days (2)
        truncatedDate = dateTimeParser.parse("February 16, 2007 0:00:00.000");
        lastTruncateDate = dateTimeParser.parse("February 28, 2007 23:59:59.999");
        baseTruncateTest(truncatedDate, lastTruncateDate, calendarField);

        //month with 29 days (1)
        truncatedDate = dateTimeParser.parse("February 1, 2008 0:00:00.000");
        lastTruncateDate = dateTimeParser.parse("February 15, 2008 23:59:59.999");
        baseTruncateTest(truncatedDate, lastTruncateDate, calendarField);

        //month with 29 days (2)
        truncatedDate = dateTimeParser.parse("February 16, 2008 0:00:00.000");
        lastTruncateDate = dateTimeParser.parse("February 29, 2008 23:59:59.999");
        baseTruncateTest(truncatedDate, lastTruncateDate, calendarField);

        //month with 30 days (1)
        truncatedDate = dateTimeParser.parse("April 1, 2008 0:00:00.000");
        lastTruncateDate = dateTimeParser.parse("April 15, 2008 23:59:59.999");
        baseTruncateTest(truncatedDate, lastTruncateDate, calendarField);

        //month with 30 days (2)
        truncatedDate = dateTimeParser.parse("April 16, 2008 0:00:00.000");
        lastTruncateDate = dateTimeParser.parse("April 30, 2008 23:59:59.999");
        baseTruncateTest(truncatedDate, lastTruncateDate, calendarField);
        
        //month with 31 days (1)
        truncatedDate = dateTimeParser.parse("March 1, 2008 0:00:00.000");
        lastTruncateDate = dateTimeParser.parse("March 15, 2008 23:59:59.999");
        baseTruncateTest(truncatedDate, lastTruncateDate, calendarField);

        //month with 31 days (2)
        truncatedDate = dateTimeParser.parse("March 16, 2008 0:00:00.000");
        lastTruncateDate = dateTimeParser.parse("March 31, 2008 23:59:59.999");
        baseTruncateTest(truncatedDate, lastTruncateDate, calendarField);

    }

    /**
     * Test DateUtils.truncate()-method with Calendar.DATE
     * 
     * @throws Exception
     * @since 3.0
     */
    @Test
    public void testTruncateDate() throws Exception {
        final int calendarField = Calendar.DATE;
        Date lastTruncateDate = dateTimeParser.parse("June 1, 2008 23:59:59.999");
        baseTruncateTest(targetDateDate, lastTruncateDate, calendarField);
    }
    
    /**
     * Test DateUtils.truncate()-method with Calendar.DAY_OF_MONTH
     * 
     * @throws Exception
     * @since 3.0
     */
    @Test
    public void testTruncateDayOfMonth() throws Exception {
        final int calendarField = Calendar.DAY_OF_MONTH;
        Date lastTruncateDate = dateTimeParser.parse("June 1, 2008 23:59:59.999");
        baseTruncateTest(targetDayOfMonthDate, lastTruncateDate, calendarField);
    }
    
    /**
     * Test DateUtils.truncate()-method with Calendar.AM_PM
     * Includes truncating the extremes of both AM and PM of one day 
     * 
     * @throws Exception
     * @since 3.0
     */
    @Test
    public void testTruncateAmPm() throws Exception {
        final int calendarField = Calendar.AM_PM;
        
        //AM
        Date lastTruncateDate = dateTimeParser.parse("June 1, 2008 11:59:59.999");
        baseTruncateTest(targetAmDate, lastTruncateDate, calendarField);

        //PM
        lastTruncateDate = dateTimeParser.parse("June 1, 2008 23:59:59.999");
        baseTruncateTest(targetPmDate, lastTruncateDate, calendarField);
    }
    
    /**
     * Test DateUtils.truncate()-method with Calendar.HOUR
     * 
     * @throws Exception
     * @since 3.0
     */
    @Test
    public void testTruncateHour() throws Exception {
        final int calendarField = Calendar.HOUR;
        Date lastTruncateDate = dateTimeParser.parse("June 1, 2008 8:59:59.999");
        baseTruncateTest(targetHourDate, lastTruncateDate, calendarField);
    }
    
    /**
     * Test DateUtils.truncate()-method with Calendar.HOUR_OF_DAY
     * 
     * @throws Exception
     * @since 3.0
     */
    @Test
    public void testTruncateHourOfDay() throws Exception {
        final int calendarField = Calendar.HOUR_OF_DAY;
        Date lastTruncateDate = dateTimeParser.parse("June 1, 2008 8:59:59.999");
        baseTruncateTest(targetHourOfDayDate, lastTruncateDate, calendarField);
    }
    
    /**
     * Test DateUtils.truncate()-method with Calendar.MINUTE
     * 
     * @throws Exception
     * @since 3.0
     */
    @Test
    public void testTruncateMinute() throws Exception {
        final int calendarField = Calendar.MINUTE;
        Date lastTruncateDate = dateTimeParser.parse("June 1, 2008 8:15:59.999");
        baseTruncateTest(targetMinuteDate, lastTruncateDate, calendarField);
    }
    
    /**
     * Test DateUtils.truncate()-method with Calendar.SECOND
     * 
     * @throws Exception
     * @since 3.0
     */
    @Test
    public void testTruncateSecond() throws Exception {
        final int calendarField = Calendar.SECOND;
        Date lastTruncateDate = dateTimeParser.parse("June 1, 2008 8:15:14.999");
        baseTruncateTest(targetSecondDate, lastTruncateDate, calendarField);
    }
    
    /**
     * Test DateUtils.truncate()-method with Calendar.SECOND
     * 
     * @throws Exception
     * @since 3.0
     */
    @Test
    public void testTruncateMilliSecond() throws Exception {
        final int calendarField = Calendar.MILLISECOND;
        baseTruncateTest(targetMilliSecondDate, targetMilliSecondDate, calendarField);
    }
        
    /**
     * When using this basetest all extremes are tested.<br> 
     * It will test the Date, Calendar and Object-implementation<br>
     * lastRoundDownDate should round down to roundedDownDate<br>
     * lastRoundDownDate + 1 millisecond should round up to roundedUpDate
     * 
     * @param roundedUpDate the next rounded date after <strong>roundedDownDate</strong> when using <strong>calendarField</strong>
     * @param roundedDownDate the result if <strong>lastRoundDownDate</strong> was rounded with <strong>calendarField</strong>
     * @param lastRoundDownDate rounding this value with <strong>calendarField</strong> will result in <strong>roundedDownDate</strong>
     * @param calendarField
     * @since 3.0
     */
    protected void baseRoundTest(final Date roundedUpDate, final Date roundedDownDate, final Date lastRoundDownDate, final int calendarField) {
        Date firstRoundUpDate = DateUtils.addMilliseconds(lastRoundDownDate, 1);
        
        //Date-comparison
        assertEquals(roundedDownDate, DateUtils.round(roundedDownDate, calendarField));
        assertEquals(roundedUpDate, DateUtils.round(roundedUpDate, calendarField));
        assertEquals(roundedDownDate, DateUtils.round(lastRoundDownDate, calendarField));
        assertEquals(roundedUpDate, DateUtils.round(firstRoundUpDate, calendarField));
        
        //Calendar-initiations
        Calendar roundedUpCalendar, roundedDownCalendar, lastRoundDownCalendar, firstRoundUpCalendar; 
        roundedDownCalendar = Calendar.getInstance();
        roundedUpCalendar = Calendar.getInstance();
        lastRoundDownCalendar = Calendar.getInstance();
        firstRoundUpCalendar = Calendar.getInstance();
        roundedDownCalendar.setTime(roundedDownDate);
        roundedUpCalendar.setTime(roundedUpDate);
        lastRoundDownCalendar.setTime(lastRoundDownDate);
        firstRoundUpCalendar.setTime(firstRoundUpDate);

        //Calendar-comparison
        assertEquals(roundedDownCalendar, DateUtils.round(roundedDownCalendar, calendarField));
        assertEquals(roundedUpCalendar, DateUtils.round(roundedUpCalendar, calendarField));
        assertEquals(roundedDownCalendar, DateUtils.round(lastRoundDownCalendar, calendarField));
        assertEquals(roundedUpCalendar, DateUtils.round(firstRoundUpCalendar, calendarField));

        //Object-comparison
        assertEquals(roundedDownDate, DateUtils.round((Object) roundedDownDate, calendarField));
        assertEquals(roundedUpDate, DateUtils.round((Object) roundedUpDate, calendarField));
        assertEquals(roundedDownDate, DateUtils.round((Object) lastRoundDownDate, calendarField));
        assertEquals(roundedUpDate, DateUtils.round((Object) firstRoundUpDate, calendarField));
        assertEquals(roundedDownDate, DateUtils.round((Object) roundedDownCalendar, calendarField));
        assertEquals(roundedUpDate, DateUtils.round((Object) roundedUpCalendar, calendarField));
        assertEquals(roundedDownDate, DateUtils.round((Object) lastRoundDownDate, calendarField));
        assertEquals(roundedUpDate, DateUtils.round((Object) firstRoundUpDate, calendarField));
    }
    
    /**
     * When using this basetest all extremes are tested.<br> 
     * It will test the Date, Calendar and Object-implementation<br>
     * lastTruncateDate should round down to truncatedDate<br>
     * lastTruncateDate + 1 millisecond should never round down to truncatedDate
     * 
     * @param truncatedDate expected Date when <strong>lastTruncateDate</strong> is truncated with <strong>calendarField</strong>
     * @param lastTruncateDate the last possible Date which will truncate to <strong>truncatedDate</strong> with <strong>calendarField</strong>
     * @param calendarField a Calendar.field value
     * @since 3.0
     */
    protected void baseTruncateTest(final Date truncatedDate, final Date lastTruncateDate, final int calendarField) {
        Date nextTruncateDate = DateUtils.addMilliseconds(lastTruncateDate, 1);
        
        //Date-comparison
        assertEquals("Truncating "+ fdf.format(truncatedDate) +" as Date with CalendarField-value "+ calendarField +" must return itself", truncatedDate, DateUtils.truncate(truncatedDate, calendarField));
        assertEquals(truncatedDate, DateUtils.truncate(lastTruncateDate, calendarField));
        assertFalse(fdf.format(lastTruncateDate) +" is not an extreme when truncating as Date with CalendarField-value "+ calendarField, truncatedDate.equals(DateUtils.truncate(nextTruncateDate, calendarField)));
        
        //Calendar-initiations
        Calendar truncatedCalendar, lastTruncateCalendar, nextTruncateCalendar; 
        truncatedCalendar = Calendar.getInstance();
        lastTruncateCalendar = Calendar.getInstance();
        nextTruncateCalendar = Calendar.getInstance();
        truncatedCalendar.setTime(truncatedDate);
        lastTruncateCalendar.setTime(lastTruncateDate);
        nextTruncateCalendar.setTime(nextTruncateDate);

        //Calendar-comparison
        assertEquals("Truncating "+ fdf.format(truncatedCalendar) +" as Calendar with CalendarField-value "+ calendarField +" must return itself", truncatedCalendar, DateUtils.truncate(truncatedCalendar, calendarField));
        assertEquals(truncatedCalendar, DateUtils.truncate(lastTruncateCalendar, calendarField));
        assertFalse(fdf.format(lastTruncateCalendar) +" is not an extreme when truncating as Calendar with CalendarField-value "+ calendarField, truncatedCalendar.equals(DateUtils.truncate(nextTruncateCalendar, calendarField)));

        //Object-comparison
        assertEquals("Truncating "+ fdf.format(truncatedDate) +" as Date cast to Object with CalendarField-value "+ calendarField +" must return itself as Date", truncatedDate, DateUtils.truncate((Object) truncatedDate, calendarField));
        assertEquals(truncatedDate, DateUtils.truncate((Object) lastTruncateDate, calendarField));
        assertFalse(fdf.format(lastTruncateDate) +" is not an extreme when truncating as Date cast to Object with CalendarField-value "+ calendarField, truncatedDate.equals(DateUtils.truncate((Object) nextTruncateDate, calendarField)));
        assertEquals("Truncating "+ fdf.format(truncatedCalendar) +" as Calendar cast to Object with CalendarField-value "+ calendarField +" must return itself as Date", truncatedDate, DateUtils.truncate((Object) truncatedCalendar, calendarField));
        assertEquals(truncatedDate, DateUtils.truncate((Object) lastTruncateCalendar, calendarField));
        assertFalse(fdf.format(lastTruncateCalendar) +" is not an extreme when truncating as Calendar cast to Object with CalendarField-value "+ calendarField, truncatedDate.equals(DateUtils.truncate((Object) nextTruncateCalendar, calendarField)));
    }
    
    /**
     * 
     * Any January 1 could be considered as the ultimate extreme.
     * Instead of comparing the results if the input has a difference of 1 millisecond we check the output to be exactly January first. 
     * 
     * @param minDate
     * @param maxDate
     * @param calendarField
     * @since 3.0
     */
    protected void roundToJanuaryFirst(Date minDate, Date maxDate, int calendarField) {
        assertEquals("Rounding "+ fdf.format(januaryOneDate) +" as Date with CalendarField-value "+ calendarField +" must return itself", januaryOneDate, DateUtils.round(januaryOneDate, calendarField));
        assertEquals(januaryOneDate, DateUtils.round(minDate, calendarField));
        assertEquals(januaryOneDate, DateUtils.round(maxDate, calendarField));
        
        Calendar minCalendar = Calendar.getInstance();
        minCalendar.setTime(minDate);
        Calendar maxCalendar = Calendar.getInstance();
        maxCalendar.setTime(maxDate);
        assertEquals("Rounding "+ fdf.format(januaryOneCalendar) +" as Date with CalendarField-value "+ calendarField +" must return itself", januaryOneCalendar, DateUtils.round(januaryOneCalendar, calendarField));
        assertEquals(januaryOneCalendar, DateUtils.round(minCalendar, calendarField));
        assertEquals(januaryOneCalendar, DateUtils.round(maxCalendar, calendarField));

        Date toPrevRoundDate = DateUtils.addMilliseconds(minDate, -1);
        Date toNextRoundDate = DateUtils.addMilliseconds(maxDate, 1);
        assertFalse(fdf.format(minDate) +" is not an lower-extreme when rounding as Date with CalendarField-value "+ calendarField, januaryOneDate.equals(DateUtils.round(toPrevRoundDate, calendarField)));
        assertFalse(fdf.format(maxDate) +" is not an upper-extreme when rounding as Date with CalendarField-value "+ calendarField, januaryOneDate.equals(DateUtils.round(toNextRoundDate, calendarField)));
        
        Calendar toPrevRoundCalendar = Calendar.getInstance();
        toPrevRoundCalendar.setTime(toPrevRoundDate);
        Calendar toNextRoundCalendar = Calendar.getInstance();
        toNextRoundCalendar.setTime(toNextRoundDate);
        assertFalse(fdf.format(minCalendar) +" is not an lower-extreme when rounding as Date with CalendarField-value "+ calendarField, januaryOneDate.equals(DateUtils.round(toPrevRoundDate, calendarField)));
        assertFalse(fdf.format(maxCalendar) +" is not an upper-extreme when rounding as Date with CalendarField-value "+ calendarField, januaryOneDate.equals(DateUtils.round(toNextRoundDate, calendarField)));
    }
}
