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
import java.util.Calendar;
import java.util.Date;

public class DateUtilsFragmentTest {

    private static final int months = 7;   // second final prime before 12
    private static final int days = 23;    // second final prime before 31 (and valid)
    private static final int hours = 19;   // second final prime before 24
    private static final int minutes = 53; // second final prime before 60
    private static final int seconds = 47; // third final prime before 60
    private static final int millis = 991; // second final prime before 1000

    private Date aDate;
    private Calendar aCalendar;


    @Before
    public void setUp() {
        aCalendar = Calendar.getInstance();
        aCalendar.set(2005, months, days, hours, minutes, seconds);
        aCalendar.set(Calendar.MILLISECOND, millis);
        aDate = aCalendar.getTime();
    }
    
    @Test
    public void testNullDate() {
        try {
            DateUtils.getFragmentInMilliseconds((Date) null, Calendar.MILLISECOND);
            fail();
        } catch(IllegalArgumentException iae) {}

        try {
            DateUtils.getFragmentInSeconds((Date) null, Calendar.MILLISECOND);
            fail();
        } catch(IllegalArgumentException iae) {}

        try {
            DateUtils.getFragmentInMinutes((Date) null, Calendar.MILLISECOND);
            fail();
        } catch(IllegalArgumentException iae) {}

        try {
            DateUtils.getFragmentInHours((Date) null, Calendar.MILLISECOND);
            fail();
        } catch(IllegalArgumentException iae) {}

        try {
            DateUtils.getFragmentInDays((Date) null, Calendar.MILLISECOND);
            fail();
        } catch(IllegalArgumentException iae) {}
    }

    @Test
    public void testNullCalendar() {
        try {
            DateUtils.getFragmentInMilliseconds((Calendar) null, Calendar.MILLISECOND);
            fail();
        } catch(IllegalArgumentException iae) {}

        try {
            DateUtils.getFragmentInSeconds((Calendar) null, Calendar.MILLISECOND);
            fail();
        } catch(IllegalArgumentException iae) {}

        try {
            DateUtils.getFragmentInMinutes((Calendar) null, Calendar.MILLISECOND);
            fail();
        } catch(IllegalArgumentException iae) {}

        try {
            DateUtils.getFragmentInHours((Calendar) null, Calendar.MILLISECOND);
            fail();
        } catch(IllegalArgumentException iae) {}

        try {
            DateUtils.getFragmentInDays((Calendar) null, Calendar.MILLISECOND);
            fail();
        } catch(IllegalArgumentException iae) {}
    }
    
    @Test
    public void testInvalidFragmentWithDate() {
        try {
            DateUtils.getFragmentInMilliseconds(aDate, 0);
            fail();
        } catch(IllegalArgumentException iae) {}

        try {
            DateUtils.getFragmentInSeconds(aDate, 0);
            fail();
        } catch(IllegalArgumentException iae) {}

        try {
            DateUtils.getFragmentInMinutes(aDate, 0);
            fail();
        } catch(IllegalArgumentException iae) {}

        try {
            DateUtils.getFragmentInHours(aDate, 0);
            fail();
        } catch(IllegalArgumentException iae) {}

        try {
            DateUtils.getFragmentInDays(aDate, 0);
            fail();
        } catch(IllegalArgumentException iae) {}
    }

    @Test
    public void testInvalidFragmentWithCalendar() {
        try {
            DateUtils.getFragmentInMilliseconds(aCalendar, 0);
            fail();
        } catch(IllegalArgumentException iae) {}

        try {
            DateUtils.getFragmentInSeconds(aCalendar, 0);
            fail();
        } catch(IllegalArgumentException iae) {}

        try {
            DateUtils.getFragmentInMinutes(aCalendar, 0);
            fail();
        } catch(IllegalArgumentException iae) {}

        try {
            DateUtils.getFragmentInHours(aCalendar, 0);
            fail();
        } catch(IllegalArgumentException iae) {}

        try {
            DateUtils.getFragmentInDays(aCalendar, 0);
            fail();
        } catch(IllegalArgumentException iae) {}
    }

    @Test
    public void testMillisecondFragmentInLargerUnitWithDate() {
        assertEquals(0, DateUtils.getFragmentInMilliseconds(aDate, Calendar.MILLISECOND));
        assertEquals(0, DateUtils.getFragmentInSeconds(aDate, Calendar.MILLISECOND));
        assertEquals(0, DateUtils.getFragmentInMinutes(aDate, Calendar.MILLISECOND));
        assertEquals(0, DateUtils.getFragmentInHours(aDate, Calendar.MILLISECOND));
        assertEquals(0, DateUtils.getFragmentInDays(aDate, Calendar.MILLISECOND));
    }

    @Test
    public void testMillisecondFragmentInLargerUnitWithCalendar() {
        assertEquals(0, DateUtils.getFragmentInMilliseconds(aCalendar, Calendar.MILLISECOND));
        assertEquals(0, DateUtils.getFragmentInSeconds(aCalendar, Calendar.MILLISECOND));
        assertEquals(0, DateUtils.getFragmentInMinutes(aCalendar, Calendar.MILLISECOND));
        assertEquals(0, DateUtils.getFragmentInHours(aCalendar, Calendar.MILLISECOND));
        assertEquals(0, DateUtils.getFragmentInDays(aCalendar, Calendar.MILLISECOND));
    }
    
    @Test
    public void testSecondFragmentInLargerUnitWithDate() {
        assertEquals(0, DateUtils.getFragmentInSeconds(aDate, Calendar.SECOND));
        assertEquals(0, DateUtils.getFragmentInMinutes(aDate, Calendar.SECOND));
        assertEquals(0, DateUtils.getFragmentInHours(aDate, Calendar.SECOND));
        assertEquals(0, DateUtils.getFragmentInDays(aDate, Calendar.SECOND));
    }

    @Test
    public void testSecondFragmentInLargerUnitWithCalendar() {
        assertEquals(0, DateUtils.getFragmentInSeconds(aCalendar, Calendar.SECOND));
        assertEquals(0, DateUtils.getFragmentInMinutes(aCalendar, Calendar.SECOND));
        assertEquals(0, DateUtils.getFragmentInHours(aCalendar, Calendar.SECOND));
        assertEquals(0, DateUtils.getFragmentInDays(aCalendar, Calendar.SECOND));
    }
    
    @Test
    public void testMinuteFragmentInLargerUnitWithDate() {
        assertEquals(0, DateUtils.getFragmentInMinutes(aDate, Calendar.MINUTE));
        assertEquals(0, DateUtils.getFragmentInHours(aDate, Calendar.MINUTE));
        assertEquals(0, DateUtils.getFragmentInDays(aDate, Calendar.MINUTE));
    }

    @Test
    public void testMinuteFragmentInLargerUnitWithCalendar() {
        assertEquals(0, DateUtils.getFragmentInMinutes(aCalendar, Calendar.MINUTE));
        assertEquals(0, DateUtils.getFragmentInHours(aCalendar, Calendar.MINUTE));
        assertEquals(0, DateUtils.getFragmentInDays(aCalendar, Calendar.MINUTE));
    }

    @Test
    public void testHourOfDayFragmentInLargerUnitWithDate() {
        assertEquals(0, DateUtils.getFragmentInHours(aDate, Calendar.HOUR_OF_DAY));
        assertEquals(0, DateUtils.getFragmentInDays(aDate, Calendar.HOUR_OF_DAY));
    }

    @Test
    public void testHourOfDayFragmentInLargerUnitWithCalendar() {
        assertEquals(0, DateUtils.getFragmentInHours(aCalendar, Calendar.HOUR_OF_DAY));
        assertEquals(0, DateUtils.getFragmentInDays(aCalendar, Calendar.HOUR_OF_DAY));
    }

    @Test
    public void testDayOfYearFragmentInLargerUnitWithDate() {
        assertEquals(0, DateUtils.getFragmentInDays(aDate, Calendar.DAY_OF_YEAR));
    }

    @Test
    public void testDayOfYearFragmentInLargerUnitWithCalendar() {
        assertEquals(0, DateUtils.getFragmentInDays(aCalendar, Calendar.DAY_OF_YEAR));
    }

    @Test
    public void testDateFragmentInLargerUnitWithDate() {
        assertEquals(0, DateUtils.getFragmentInDays(aDate, Calendar.DATE));
    }

    @Test
    public void testDateFragmentInLargerUnitWithCalendar() {
        assertEquals(0, DateUtils.getFragmentInDays(aCalendar, Calendar.DATE));
    }

    //Calendar.SECOND as useful fragment
    
    @Test
    public void testMillisecondsOfSecondWithDate() {
        long testResult = DateUtils.getFragmentInMilliseconds(aDate, Calendar.SECOND);
        assertEquals(millis, testResult);
    }

    @Test
    public void testMillisecondsOfSecondWithCalendar() {
        long testResult = DateUtils.getFragmentInMilliseconds(aCalendar, Calendar.SECOND);
        assertEquals(millis, testResult);
        assertEquals(aCalendar.get(Calendar.MILLISECOND), testResult);
    }

    //Calendar.MINUTE as useful fragment

    @Test
    public void testMillisecondsOfMinuteWithDate() {
        long testResult = DateUtils.getFragmentInMilliseconds(aDate, Calendar.MINUTE);
        assertEquals(millis + (seconds * DateUtils.MILLIS_PER_SECOND), testResult);
    }

    @Test
    public void testMillisecondsOfMinuteWithCalender() {
        long testResult = DateUtils.getFragmentInMilliseconds(aCalendar, Calendar.MINUTE);
        assertEquals(millis + (seconds * DateUtils.MILLIS_PER_SECOND), testResult);
    }

    @Test
    public void testSecondsofMinuteWithDate() {
        long testResult = DateUtils.getFragmentInSeconds(aDate, Calendar.MINUTE);
        assertEquals(seconds, testResult);
    }

    @Test
    public void testSecondsofMinuteWithCalendar() {
        long testResult = DateUtils.getFragmentInSeconds(aCalendar, Calendar.MINUTE);
        assertEquals(seconds, testResult);
        assertEquals(aCalendar.get(Calendar.SECOND), testResult);
    }

    //Calendar.HOUR_OF_DAY as useful fragment
    
    @Test
    public void testMillisecondsOfHourWithDate() {
        long testResult = DateUtils.getFragmentInMilliseconds(aDate, Calendar.HOUR_OF_DAY);
        assertEquals(millis + (seconds * DateUtils.MILLIS_PER_SECOND) + (minutes * DateUtils.MILLIS_PER_MINUTE), testResult);
    }
    
    @Test
    public void testMillisecondsOfHourWithCalendar() {
        long testResult = DateUtils.getFragmentInMilliseconds(aCalendar, Calendar.HOUR_OF_DAY);
        assertEquals(millis + (seconds * DateUtils.MILLIS_PER_SECOND) + (minutes * DateUtils.MILLIS_PER_MINUTE), testResult);
    }

    @Test
    public void testSecondsofHourWithDate() {
        long testResult = DateUtils.getFragmentInSeconds(aDate, Calendar.HOUR_OF_DAY);
        assertEquals(
                seconds
                        + (minutes
                                * DateUtils.MILLIS_PER_MINUTE / DateUtils.MILLIS_PER_SECOND),
                testResult);
    }

    @Test
    public void testSecondsofHourWithCalendar() {
        long testResult = DateUtils.getFragmentInSeconds(aCalendar, Calendar.HOUR_OF_DAY);
        assertEquals(
                seconds
                        + (minutes
                                * DateUtils.MILLIS_PER_MINUTE / DateUtils.MILLIS_PER_SECOND),
                testResult);
    }

    @Test
    public void testMinutesOfHourWithDate() {
        long testResult = DateUtils.getFragmentInMinutes(aDate, Calendar.HOUR_OF_DAY);
        assertEquals(minutes, testResult);
    }

    @Test
    public void testMinutesOfHourWithCalendar() {
        long testResult = DateUtils.getFragmentInMinutes(aCalendar, Calendar.HOUR_OF_DAY);
        assertEquals(minutes, testResult);
    }

    //Calendar.DATE and Calendar.DAY_OF_YEAR as useful fragment
    @Test
    public void testMillisecondsOfDayWithDate() {
        long testresult = DateUtils.getFragmentInMilliseconds(aDate, Calendar.DATE);
        long expectedValue = millis + (seconds * DateUtils.MILLIS_PER_SECOND) + (minutes * DateUtils.MILLIS_PER_MINUTE) + (hours * DateUtils.MILLIS_PER_HOUR); 
        assertEquals(expectedValue, testresult);
        testresult = DateUtils.getFragmentInMilliseconds(aDate, Calendar.DAY_OF_YEAR);
        assertEquals(expectedValue, testresult);
    }
    
    @Test
    public void testMillisecondsOfDayWithCalendar() {
        long testresult = DateUtils.getFragmentInMilliseconds(aCalendar, Calendar.DATE);
        long expectedValue = millis + (seconds * DateUtils.MILLIS_PER_SECOND) + (minutes * DateUtils.MILLIS_PER_MINUTE) + (hours * DateUtils.MILLIS_PER_HOUR); 
        assertEquals(expectedValue, testresult);
        testresult = DateUtils.getFragmentInMilliseconds(aCalendar, Calendar.DAY_OF_YEAR);
        assertEquals(expectedValue, testresult);
    }

    @Test
    public void testSecondsOfDayWithDate() {
        long testresult = DateUtils.getFragmentInSeconds(aDate, Calendar.DATE);
        long expectedValue = seconds + ((minutes * DateUtils.MILLIS_PER_MINUTE) + (hours * DateUtils.MILLIS_PER_HOUR))/ DateUtils.MILLIS_PER_SECOND;
        assertEquals(expectedValue, testresult);
        testresult = DateUtils.getFragmentInSeconds(aDate, Calendar.DAY_OF_YEAR);
        assertEquals(expectedValue, testresult);
    }

    @Test
    public void testSecondsOfDayWithCalendar() {
        long testresult = DateUtils.getFragmentInSeconds(aCalendar, Calendar.DATE);
        long expectedValue = seconds + ((minutes * DateUtils.MILLIS_PER_MINUTE) + (hours * DateUtils.MILLIS_PER_HOUR))/ DateUtils.MILLIS_PER_SECOND;
        assertEquals(expectedValue, testresult);
        testresult = DateUtils.getFragmentInSeconds(aCalendar, Calendar.DAY_OF_YEAR);
        assertEquals(expectedValue, testresult);
    }

    @Test
    public void testMinutesOfDayWithDate() {
        long testResult = DateUtils.getFragmentInMinutes(aDate, Calendar.DATE);
        long expectedValue = minutes + ((hours * DateUtils.MILLIS_PER_HOUR))/ DateUtils.MILLIS_PER_MINUTE; 
        assertEquals(expectedValue,testResult);
        testResult = DateUtils.getFragmentInMinutes(aDate, Calendar.DAY_OF_YEAR);
        assertEquals(expectedValue,testResult);
    }

    @Test
    public void testMinutesOfDayWithCalendar() {
        long testResult = DateUtils.getFragmentInMinutes(aCalendar, Calendar.DATE);
        long expectedValue = minutes + ((hours * DateUtils.MILLIS_PER_HOUR))/ DateUtils.MILLIS_PER_MINUTE; 
        assertEquals(expectedValue, testResult);
        testResult = DateUtils.getFragmentInMinutes(aCalendar, Calendar.DAY_OF_YEAR);
        assertEquals(expectedValue, testResult);
    }
    
    @Test
    public void testHoursOfDayWithDate() {
        long testResult = DateUtils.getFragmentInHours(aDate, Calendar.DATE);
        long expectedValue = hours; 
        assertEquals(expectedValue,testResult);
        testResult = DateUtils.getFragmentInHours(aDate, Calendar.DAY_OF_YEAR);
        assertEquals(expectedValue,testResult);
    }

    @Test
    public void testHoursOfDayWithCalendar() {
        long testResult = DateUtils.getFragmentInHours(aCalendar, Calendar.DATE);
        long expectedValue = hours; 
        assertEquals(expectedValue, testResult);
        testResult = DateUtils.getFragmentInHours(aCalendar, Calendar.DAY_OF_YEAR);
        assertEquals(expectedValue, testResult);
    }
    
    
    //Calendar.MONTH as useful fragment
    @Test
    public void testMillisecondsOfMonthWithDate() {
        long testResult = DateUtils.getFragmentInMilliseconds(aDate, Calendar.MONTH);
        assertEquals(millis + (seconds * DateUtils.MILLIS_PER_SECOND) + (minutes * DateUtils.MILLIS_PER_MINUTE)
                                + (hours * DateUtils.MILLIS_PER_HOUR) + (days * DateUtils.MILLIS_PER_DAY),
                testResult);
    }

    @Test
    public void testMillisecondsOfMonthWithCalendar() {
        long testResult = DateUtils.getFragmentInMilliseconds(aCalendar, Calendar.MONTH);
        assertEquals(millis + (seconds * DateUtils.MILLIS_PER_SECOND) + (minutes * DateUtils.MILLIS_PER_MINUTE)
                + (hours * DateUtils.MILLIS_PER_HOUR) + (days * DateUtils.MILLIS_PER_DAY),
testResult);
    }
    
    @Test
    public void testSecondsOfMonthWithDate() {
        long testResult = DateUtils.getFragmentInSeconds(aDate, Calendar.MONTH);
        assertEquals(
                seconds
                        + ((minutes * DateUtils.MILLIS_PER_MINUTE)
                                + (hours * DateUtils.MILLIS_PER_HOUR) + (days * DateUtils.MILLIS_PER_DAY))
                        / DateUtils.MILLIS_PER_SECOND,
                testResult);
    }

    @Test
    public void testSecondsOfMonthWithCalendar() {
        long testResult = DateUtils.getFragmentInSeconds(aCalendar, Calendar.MONTH);
        assertEquals(
                seconds
                        + ((minutes * DateUtils.MILLIS_PER_MINUTE)
                                + (hours * DateUtils.MILLIS_PER_HOUR) + (days * DateUtils.MILLIS_PER_DAY))
                        / DateUtils.MILLIS_PER_SECOND,
                testResult);
    }

    @Test
    public void testMinutesOfMonthWithDate() {
        long testResult = DateUtils.getFragmentInMinutes(aDate, Calendar.MONTH);
        assertEquals(minutes
                                + ((hours * DateUtils.MILLIS_PER_HOUR) + (days * DateUtils.MILLIS_PER_DAY))
                        / DateUtils.MILLIS_PER_MINUTE,
                testResult);
    }

    @Test
    public void testMinutesOfMonthWithCalendar() {
        long testResult = DateUtils.getFragmentInMinutes(aCalendar, Calendar.MONTH);
        assertEquals( minutes  +((hours * DateUtils.MILLIS_PER_HOUR) + (days * DateUtils.MILLIS_PER_DAY))
                        / DateUtils.MILLIS_PER_MINUTE,
                testResult);
    }

    @Test
    public void testHoursOfMonthWithDate() {
        long testResult = DateUtils.getFragmentInHours(aDate, Calendar.MONTH);
        assertEquals(hours + ((days * DateUtils.MILLIS_PER_DAY))
                        / DateUtils.MILLIS_PER_HOUR,
                testResult);
    }

    @Test
    public void testHoursOfMonthWithCalendar() {
        long testResult = DateUtils.getFragmentInHours(aCalendar, Calendar.MONTH);
        assertEquals( hours +((days * DateUtils.MILLIS_PER_DAY))
                        / DateUtils.MILLIS_PER_HOUR,
                testResult);
    }
    
    //Calendar.YEAR as useful fragment
    @Test
    public void testMillisecondsOfYearWithDate() {
        long testResult = DateUtils.getFragmentInMilliseconds(aDate, Calendar.YEAR);
        Calendar cal = Calendar.getInstance();
        cal.setTime(aDate);
        assertEquals(millis + (seconds * DateUtils.MILLIS_PER_SECOND) + (minutes * DateUtils.MILLIS_PER_MINUTE)
                                + (hours * DateUtils.MILLIS_PER_HOUR) + (cal.get(Calendar.DAY_OF_YEAR) * DateUtils.MILLIS_PER_DAY),
                testResult);
    }

    @Test
    public void testMillisecondsOfYearWithCalendar() {
        long testResult = DateUtils.getFragmentInMilliseconds(aCalendar, Calendar.YEAR);
        assertEquals(millis + (seconds * DateUtils.MILLIS_PER_SECOND) + (minutes * DateUtils.MILLIS_PER_MINUTE)
                + (hours * DateUtils.MILLIS_PER_HOUR) + (aCalendar.get(Calendar.DAY_OF_YEAR) * DateUtils.MILLIS_PER_DAY),
testResult);
    }
    
    @Test
    public void testSecondsOfYearWithDate() {
        long testResult = DateUtils.getFragmentInSeconds(aDate, Calendar.YEAR);
        Calendar cal = Calendar.getInstance();
        cal.setTime(aDate);
        assertEquals(
                seconds
                        + ((minutes * DateUtils.MILLIS_PER_MINUTE)
                                + (hours * DateUtils.MILLIS_PER_HOUR) + (cal.get(Calendar.DAY_OF_YEAR) * DateUtils.MILLIS_PER_DAY))
                        / DateUtils.MILLIS_PER_SECOND,
                testResult);
    }

    @Test
    public void testSecondsOfYearWithCalendar() {
        long testResult = DateUtils.getFragmentInSeconds(aCalendar, Calendar.YEAR);
        assertEquals(
                seconds
                        + ((minutes * DateUtils.MILLIS_PER_MINUTE)
                                + (hours * DateUtils.MILLIS_PER_HOUR) + (aCalendar.get(Calendar.DAY_OF_YEAR) * DateUtils.MILLIS_PER_DAY))
                        / DateUtils.MILLIS_PER_SECOND,
                testResult);
    }

    @Test
    public void testMinutesOfYearWithDate() {
        long testResult = DateUtils.getFragmentInMinutes(aDate, Calendar.YEAR);
        Calendar cal = Calendar.getInstance();
        cal.setTime(aDate);
        assertEquals(minutes
                                + ((hours * DateUtils.MILLIS_PER_HOUR) + (cal.get(Calendar.DAY_OF_YEAR) * DateUtils.MILLIS_PER_DAY))
                        / DateUtils.MILLIS_PER_MINUTE,
                testResult);
    }

    @Test
    public void testMinutesOfYearWithCalendar() {
        long testResult = DateUtils.getFragmentInMinutes(aCalendar, Calendar.YEAR);
        assertEquals( minutes  +((hours * DateUtils.MILLIS_PER_HOUR) + (aCalendar.get(Calendar.DAY_OF_YEAR) * DateUtils.MILLIS_PER_DAY))
                        / DateUtils.MILLIS_PER_MINUTE,
                testResult);
    }

    @Test
    public void testHoursOfYearWithDate() {
        long testResult = DateUtils.getFragmentInHours(aDate, Calendar.YEAR);
        Calendar cal = Calendar.getInstance();
        cal.setTime(aDate);
        assertEquals(hours + ((cal.get(Calendar.DAY_OF_YEAR) * DateUtils.MILLIS_PER_DAY))
                        / DateUtils.MILLIS_PER_HOUR,
                testResult);
    }

    @Test
    public void testHoursOfYearWithCalendar() {
        long testResult = DateUtils.getFragmentInHours(aCalendar, Calendar.YEAR);
        assertEquals( hours +((aCalendar.get(Calendar.DAY_OF_YEAR) * DateUtils.MILLIS_PER_DAY))
                        / DateUtils.MILLIS_PER_HOUR,
                testResult);
    }
}
