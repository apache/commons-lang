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
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.Calendar;
import java.util.Date;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class DateUtilsFragmentTest extends AbstractLangTest {

    private static final int months = 7;   // second final prime before 12
    private static final int days = 23;    // second final prime before 31 (and valid)
    private static final int hours = 19;   // second final prime before 24
    private static final int minutes = 53; // second final prime before 60
    private static final int seconds = 47; // third final prime before 60
    private static final int millis = 991; // second final prime before 1000

    private Date aDate;
    private Calendar aCalendar;


    @BeforeEach
    public void setUp() {
        aCalendar = Calendar.getInstance();
        aCalendar.set(2005, months, days, hours, minutes, seconds);
        aCalendar.set(Calendar.MILLISECOND, millis);
        aDate = aCalendar.getTime();
    }

    @Test
    public void testDateFragmentInLargerUnitWithCalendar() {
        assertEquals(0, DateUtils.getFragmentInDays(aCalendar, Calendar.DATE));
    }

    @Test
    public void testDateFragmentInLargerUnitWithDate() {
        assertEquals(0, DateUtils.getFragmentInDays(aDate, Calendar.DATE));
    }

    @Test
    public void testDayOfYearFragmentInLargerUnitWithCalendar() {
        assertEquals(0, DateUtils.getFragmentInDays(aCalendar, Calendar.DAY_OF_YEAR));
    }

    @Test
    public void testDayOfYearFragmentInLargerUnitWithDate() {
        assertEquals(0, DateUtils.getFragmentInDays(aDate, Calendar.DAY_OF_YEAR));
    }

    @Test
    public void testDaysOfMonthWithCalendar() {
        final long testResult = DateUtils.getFragmentInDays(aCalendar, Calendar.MONTH);
        assertEquals(days, testResult);
    }

    @Test
    public void testDaysOfMonthWithDate() {
        final long testResult = DateUtils.getFragmentInDays(aDate, Calendar.MONTH);
        final Calendar cal = Calendar.getInstance();
        cal.setTime(aDate);
        assertEquals(cal.get(Calendar.DAY_OF_MONTH), testResult);
    }

    @Test
    public void testDaysOfYearWithCalendar() {
        final long testResult = DateUtils.getFragmentInDays(aCalendar, Calendar.YEAR);
        assertEquals(aCalendar.get(Calendar.DAY_OF_YEAR), testResult);
    }

    @Test
    public void testDaysOfYearWithDate() {
        final long testResult = DateUtils.getFragmentInDays(aDate, Calendar.YEAR);
        final Calendar cal = Calendar.getInstance();
        cal.setTime(aDate);
        assertEquals(cal.get(Calendar.DAY_OF_YEAR), testResult);
    }

    @Test
    public void testHourOfDayFragmentInLargerUnitWithCalendar() {
        assertEquals(0, DateUtils.getFragmentInHours(aCalendar, Calendar.HOUR_OF_DAY));
        assertEquals(0, DateUtils.getFragmentInDays(aCalendar, Calendar.HOUR_OF_DAY));
    }

    @Test
    public void testHourOfDayFragmentInLargerUnitWithDate() {
        assertEquals(0, DateUtils.getFragmentInHours(aDate, Calendar.HOUR_OF_DAY));
        assertEquals(0, DateUtils.getFragmentInDays(aDate, Calendar.HOUR_OF_DAY));
    }

    @Test
    public void testHoursOfDayWithCalendar() {
        long testResult = DateUtils.getFragmentInHours(aCalendar, Calendar.DATE);
        final long expectedValue = hours;
        assertEquals(expectedValue, testResult);
        testResult = DateUtils.getFragmentInHours(aCalendar, Calendar.DAY_OF_YEAR);
        assertEquals(expectedValue, testResult);
    }

    @Test
    public void testHoursOfDayWithDate() {
        long testResult = DateUtils.getFragmentInHours(aDate, Calendar.DATE);
        final long expectedValue = hours;
        assertEquals(expectedValue, testResult);
        testResult = DateUtils.getFragmentInHours(aDate, Calendar.DAY_OF_YEAR);
        assertEquals(expectedValue, testResult);
    }

    @Test
    public void testHoursOfMonthWithCalendar() {
        final long testResult = DateUtils.getFragmentInHours(aCalendar, Calendar.MONTH);
        assertEquals( hours +(((days - 1) * DateUtils.MILLIS_PER_DAY))
                        / DateUtils.MILLIS_PER_HOUR,
                testResult);
    }

    @Test
    public void testHoursOfMonthWithDate() {
        final long testResult = DateUtils.getFragmentInHours(aDate, Calendar.MONTH);
        assertEquals(hours + (((days - 1) * DateUtils.MILLIS_PER_DAY))
                        / DateUtils.MILLIS_PER_HOUR,
                testResult);
    }

    @Test
    public void testHoursOfYearWithCalendar() {
        final long testResult = DateUtils.getFragmentInHours(aCalendar, Calendar.YEAR);
        assertEquals( hours +(((aCalendar.get(Calendar.DAY_OF_YEAR) - 1) * DateUtils.MILLIS_PER_DAY))
                        / DateUtils.MILLIS_PER_HOUR,
                testResult);
    }

    @Test
    public void testHoursOfYearWithDate() {
        final long testResult = DateUtils.getFragmentInHours(aDate, Calendar.YEAR);
        final Calendar cal = Calendar.getInstance();
        cal.setTime(aDate);
        assertEquals(hours + (((cal.get(Calendar.DAY_OF_YEAR) - 1) * DateUtils.MILLIS_PER_DAY))
                        / DateUtils.MILLIS_PER_HOUR,
                testResult);
    }

    //Calendar.SECOND as useful fragment

    @Test
    public void testInvalidFragmentWithCalendar() {
        assertThrows(IllegalArgumentException.class, () -> DateUtils.getFragmentInMilliseconds(aCalendar, 0));
        assertThrows(IllegalArgumentException.class, () -> DateUtils.getFragmentInSeconds(aCalendar, 0));
        assertThrows(IllegalArgumentException.class, () -> DateUtils.getFragmentInMinutes(aCalendar, 0));
        assertThrows(IllegalArgumentException.class, () -> DateUtils.getFragmentInHours(aCalendar, 0));
        assertThrows(IllegalArgumentException.class, () -> DateUtils.getFragmentInDays(aCalendar, 0));
    }

    @Test
    public void testInvalidFragmentWithDate() {
        assertThrows(IllegalArgumentException.class, () -> DateUtils.getFragmentInMilliseconds(aDate, 0));
        assertThrows(IllegalArgumentException.class, () -> DateUtils.getFragmentInSeconds(aDate, 0));
        assertThrows(IllegalArgumentException.class, () -> DateUtils.getFragmentInMinutes(aDate, 0));
        assertThrows(IllegalArgumentException.class, () -> DateUtils.getFragmentInHours(aDate, 0));
        assertThrows(IllegalArgumentException.class, () -> DateUtils.getFragmentInDays(aDate, 0));
    }

    //Calendar.MINUTE as useful fragment

    @Test
    public void testMillisecondFragmentInLargerUnitWithCalendar() {
        assertEquals(0, DateUtils.getFragmentInMilliseconds(aCalendar, Calendar.MILLISECOND));
        assertEquals(0, DateUtils.getFragmentInSeconds(aCalendar, Calendar.MILLISECOND));
        assertEquals(0, DateUtils.getFragmentInMinutes(aCalendar, Calendar.MILLISECOND));
        assertEquals(0, DateUtils.getFragmentInHours(aCalendar, Calendar.MILLISECOND));
        assertEquals(0, DateUtils.getFragmentInDays(aCalendar, Calendar.MILLISECOND));
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
    public void testMillisecondsOfDayWithCalendar() {
        long testresult = DateUtils.getFragmentInMilliseconds(aCalendar, Calendar.DATE);
        final long expectedValue = millis + (seconds * DateUtils.MILLIS_PER_SECOND) + (minutes * DateUtils.MILLIS_PER_MINUTE) + (hours * DateUtils.MILLIS_PER_HOUR);
        assertEquals(expectedValue, testresult);
        testresult = DateUtils.getFragmentInMilliseconds(aCalendar, Calendar.DAY_OF_YEAR);
        assertEquals(expectedValue, testresult);
    }

    //Calendar.DATE and Calendar.DAY_OF_YEAR as useful fragment
    @Test
    public void testMillisecondsOfDayWithDate() {
        long testresult = DateUtils.getFragmentInMilliseconds(aDate, Calendar.DATE);
        final long expectedValue = millis + (seconds * DateUtils.MILLIS_PER_SECOND) + (minutes * DateUtils.MILLIS_PER_MINUTE) + (hours * DateUtils.MILLIS_PER_HOUR);
        assertEquals(expectedValue, testresult);
        testresult = DateUtils.getFragmentInMilliseconds(aDate, Calendar.DAY_OF_YEAR);
        assertEquals(expectedValue, testresult);
    }

    //Calendar.HOUR_OF_DAY as useful fragment

    @Test
    public void testMillisecondsOfHourWithCalendar() {
        final long testResult = DateUtils.getFragmentInMilliseconds(aCalendar, Calendar.HOUR_OF_DAY);
        assertEquals(millis + (seconds * DateUtils.MILLIS_PER_SECOND) + (minutes * DateUtils.MILLIS_PER_MINUTE), testResult);
    }

    @Test
    public void testMillisecondsOfHourWithDate() {
        final long testResult = DateUtils.getFragmentInMilliseconds(aDate, Calendar.HOUR_OF_DAY);
        assertEquals(millis + (seconds * DateUtils.MILLIS_PER_SECOND) + (minutes * DateUtils.MILLIS_PER_MINUTE), testResult);
    }

    @Test
    public void testMillisecondsOfMinuteWithCalender() {
        final long testResult = DateUtils.getFragmentInMilliseconds(aCalendar, Calendar.MINUTE);
        assertEquals(millis + (seconds * DateUtils.MILLIS_PER_SECOND), testResult);
    }

    @Test
    public void testMillisecondsOfMinuteWithDate() {
        final long testResult = DateUtils.getFragmentInMilliseconds(aDate, Calendar.MINUTE);
        assertEquals(millis + (seconds * DateUtils.MILLIS_PER_SECOND), testResult);
    }

    @Test
    public void testMillisecondsOfMonthWithCalendar() {
        final long testResult = DateUtils.getFragmentInMilliseconds(aCalendar, Calendar.MONTH);
        assertEquals(millis + (seconds * DateUtils.MILLIS_PER_SECOND) + (minutes * DateUtils.MILLIS_PER_MINUTE)
                + (hours * DateUtils.MILLIS_PER_HOUR) + ((days - 1) * DateUtils.MILLIS_PER_DAY),
testResult);
    }

    //Calendar.MONTH as useful fragment
    @Test
    public void testMillisecondsOfMonthWithDate() {
        final long testResult = DateUtils.getFragmentInMilliseconds(aDate, Calendar.MONTH);
        assertEquals(millis + (seconds * DateUtils.MILLIS_PER_SECOND) + (minutes * DateUtils.MILLIS_PER_MINUTE)
                                + (hours * DateUtils.MILLIS_PER_HOUR) + ((days - 1) * DateUtils.MILLIS_PER_DAY),
                testResult);
    }

    @Test
    public void testMillisecondsOfSecondWithCalendar() {
        final long testResult = DateUtils.getFragmentInMilliseconds(aCalendar, Calendar.SECOND);
        assertEquals(millis, testResult);
        assertEquals(aCalendar.get(Calendar.MILLISECOND), testResult);
    }

    @Test
    public void testMillisecondsOfSecondWithDate() {
        final long testResult = DateUtils.getFragmentInMilliseconds(aDate, Calendar.SECOND);
        assertEquals(millis, testResult);
    }

    @Test
    public void testMillisecondsOfYearWithCalendar() {
        final long testResult = DateUtils.getFragmentInMilliseconds(aCalendar, Calendar.YEAR);
        assertEquals(millis + (seconds * DateUtils.MILLIS_PER_SECOND) + (minutes * DateUtils.MILLIS_PER_MINUTE)
                + (hours * DateUtils.MILLIS_PER_HOUR) + ((aCalendar.get(Calendar.DAY_OF_YEAR) - 1) * DateUtils.MILLIS_PER_DAY),
testResult);
    }

    //Calendar.YEAR as useful fragment
    @Test
    public void testMillisecondsOfYearWithDate() {
        final long testResult = DateUtils.getFragmentInMilliseconds(aDate, Calendar.YEAR);
        final Calendar cal = Calendar.getInstance();
        cal.setTime(aDate);
        assertEquals(millis + (seconds * DateUtils.MILLIS_PER_SECOND) + (minutes * DateUtils.MILLIS_PER_MINUTE)
                                + (hours * DateUtils.MILLIS_PER_HOUR) + ((cal.get(Calendar.DAY_OF_YEAR) - 1)* DateUtils.MILLIS_PER_DAY),
                testResult);
    }

    @Test
    public void testMinuteFragmentInLargerUnitWithCalendar() {
        assertEquals(0, DateUtils.getFragmentInMinutes(aCalendar, Calendar.MINUTE));
        assertEquals(0, DateUtils.getFragmentInHours(aCalendar, Calendar.MINUTE));
        assertEquals(0, DateUtils.getFragmentInDays(aCalendar, Calendar.MINUTE));
    }

    @Test
    public void testMinuteFragmentInLargerUnitWithDate() {
        assertEquals(0, DateUtils.getFragmentInMinutes(aDate, Calendar.MINUTE));
        assertEquals(0, DateUtils.getFragmentInHours(aDate, Calendar.MINUTE));
        assertEquals(0, DateUtils.getFragmentInDays(aDate, Calendar.MINUTE));
    }

    @Test
    public void testMinutesOfDayWithCalendar() {
        long testResult = DateUtils.getFragmentInMinutes(aCalendar, Calendar.DATE);
        final long expectedValue = minutes + ((hours * DateUtils.MILLIS_PER_HOUR))/ DateUtils.MILLIS_PER_MINUTE;
        assertEquals(expectedValue, testResult);
        testResult = DateUtils.getFragmentInMinutes(aCalendar, Calendar.DAY_OF_YEAR);
        assertEquals(expectedValue, testResult);
    }

    @Test
    public void testMinutesOfDayWithDate() {
        long testResult = DateUtils.getFragmentInMinutes(aDate, Calendar.DATE);
        final long expectedValue = minutes + ((hours * DateUtils.MILLIS_PER_HOUR))/ DateUtils.MILLIS_PER_MINUTE;
        assertEquals(expectedValue, testResult);
        testResult = DateUtils.getFragmentInMinutes(aDate, Calendar.DAY_OF_YEAR);
        assertEquals(expectedValue, testResult);
    }


    @Test
    public void testMinutesOfHourWithCalendar() {
        final long testResult = DateUtils.getFragmentInMinutes(aCalendar, Calendar.HOUR_OF_DAY);
        assertEquals(minutes, testResult);
    }

    @Test
    public void testMinutesOfHourWithDate() {
        final long testResult = DateUtils.getFragmentInMinutes(aDate, Calendar.HOUR_OF_DAY);
        assertEquals(minutes, testResult);
    }

    @Test
    public void testMinutesOfMonthWithCalendar() {
        final long testResult = DateUtils.getFragmentInMinutes(aCalendar, Calendar.MONTH);
        assertEquals( minutes  +((hours * DateUtils.MILLIS_PER_HOUR) + ((days - 1) * DateUtils.MILLIS_PER_DAY))
                        / DateUtils.MILLIS_PER_MINUTE,
                testResult);
    }

    @Test
    public void testMinutesOfMonthWithDate() {
        final long testResult = DateUtils.getFragmentInMinutes(aDate, Calendar.MONTH);
        assertEquals(minutes
                                + ((hours * DateUtils.MILLIS_PER_HOUR) + ((days - 1) * DateUtils.MILLIS_PER_DAY))
                        / DateUtils.MILLIS_PER_MINUTE,
                testResult);
    }

    @Test
    public void testMinutesOfYearWithCalendar() {
        final long testResult = DateUtils.getFragmentInMinutes(aCalendar, Calendar.YEAR);
        assertEquals( minutes  +((hours * DateUtils.MILLIS_PER_HOUR) + ((aCalendar.get(Calendar.DAY_OF_YEAR) - 1) * DateUtils.MILLIS_PER_DAY))
                        / DateUtils.MILLIS_PER_MINUTE,
                testResult);
    }

    @Test
    public void testMinutesOfYearWithDate() {
        final long testResult = DateUtils.getFragmentInMinutes(aDate, Calendar.YEAR);
        final Calendar cal = Calendar.getInstance();
        cal.setTime(aDate);
        assertEquals(minutes
                                + ((hours * DateUtils.MILLIS_PER_HOUR) + ((cal.get(Calendar.DAY_OF_YEAR) - 1) * DateUtils.MILLIS_PER_DAY))
                        / DateUtils.MILLIS_PER_MINUTE,
                testResult);
    }

    @Test
    public void testMinutesOfYearWithWrongOffsetBugWithCalendar() {
        final Calendar c = Calendar.getInstance();
        c.set(Calendar.MONTH, Calendar.JANUARY);
        c.set(Calendar.DAY_OF_YEAR, 1);
        c.set(Calendar.HOUR_OF_DAY, 0);
        c.set(Calendar.MINUTE, 0);
        c.set(Calendar.SECOND, 0);
        c.set(Calendar.MILLISECOND, 0);
        final long testResult = DateUtils.getFragmentInMinutes(c, Calendar.YEAR);
        assertEquals( 0, testResult);
    }

    @Test
    public void testNullCalendar() {
        assertThrows(NullPointerException.class, () -> DateUtils.getFragmentInMilliseconds((Calendar) null, Calendar.MILLISECOND));
        assertThrows(NullPointerException.class, () -> DateUtils.getFragmentInSeconds((Calendar) null, Calendar.MILLISECOND));
        assertThrows(NullPointerException.class, () -> DateUtils.getFragmentInMinutes((Calendar) null, Calendar.MILLISECOND));
        assertThrows(NullPointerException.class, () -> DateUtils.getFragmentInHours((Calendar) null, Calendar.MILLISECOND));
        assertThrows(NullPointerException.class, () -> DateUtils.getFragmentInDays((Calendar) null, Calendar.MILLISECOND));
    }

    @Test
    public void testNullDate() {
        assertThrows(NullPointerException.class, () -> DateUtils.getFragmentInMilliseconds((Date) null, Calendar.MILLISECOND));
        assertThrows(NullPointerException.class, () -> DateUtils.getFragmentInSeconds((Date) null, Calendar.MILLISECOND));
        assertThrows(NullPointerException.class, () -> DateUtils.getFragmentInMinutes((Date) null, Calendar.MILLISECOND));
        assertThrows(NullPointerException.class, () -> DateUtils.getFragmentInHours((Date) null, Calendar.MILLISECOND));
        assertThrows(NullPointerException.class, () -> DateUtils.getFragmentInDays((Date) null, Calendar.MILLISECOND));
    }

    @Test
    public void testSecondFragmentInLargerUnitWithCalendar() {
        assertEquals(0, DateUtils.getFragmentInSeconds(aCalendar, Calendar.SECOND));
        assertEquals(0, DateUtils.getFragmentInMinutes(aCalendar, Calendar.SECOND));
        assertEquals(0, DateUtils.getFragmentInHours(aCalendar, Calendar.SECOND));
        assertEquals(0, DateUtils.getFragmentInDays(aCalendar, Calendar.SECOND));
    }

    @Test
    public void testSecondFragmentInLargerUnitWithDate() {
        assertEquals(0, DateUtils.getFragmentInSeconds(aDate, Calendar.SECOND));
        assertEquals(0, DateUtils.getFragmentInMinutes(aDate, Calendar.SECOND));
        assertEquals(0, DateUtils.getFragmentInHours(aDate, Calendar.SECOND));
        assertEquals(0, DateUtils.getFragmentInDays(aDate, Calendar.SECOND));
    }

    @Test
    public void testSecondsOfDayWithCalendar() {
        long testresult = DateUtils.getFragmentInSeconds(aCalendar, Calendar.DATE);
        final long expectedValue = seconds + ((minutes * DateUtils.MILLIS_PER_MINUTE) + (hours * DateUtils.MILLIS_PER_HOUR))/ DateUtils.MILLIS_PER_SECOND;
        assertEquals(expectedValue, testresult);
        testresult = DateUtils.getFragmentInSeconds(aCalendar, Calendar.DAY_OF_YEAR);
        assertEquals(expectedValue, testresult);
    }

    @Test
    public void testSecondsOfDayWithDate() {
        long testresult = DateUtils.getFragmentInSeconds(aDate, Calendar.DATE);
        final long expectedValue = seconds + ((minutes * DateUtils.MILLIS_PER_MINUTE) + (hours * DateUtils.MILLIS_PER_HOUR))/ DateUtils.MILLIS_PER_SECOND;
        assertEquals(expectedValue, testresult);
        testresult = DateUtils.getFragmentInSeconds(aDate, Calendar.DAY_OF_YEAR);
        assertEquals(expectedValue, testresult);
    }

    @Test
    public void testSecondsofHourWithCalendar() {
        final long testResult = DateUtils.getFragmentInSeconds(aCalendar, Calendar.HOUR_OF_DAY);
        assertEquals(
                seconds
                        + (minutes
                                * DateUtils.MILLIS_PER_MINUTE / DateUtils.MILLIS_PER_SECOND),
                testResult);
    }

    @Test
    public void testSecondsofHourWithDate() {
        final long testResult = DateUtils.getFragmentInSeconds(aDate, Calendar.HOUR_OF_DAY);
        assertEquals(
                seconds
                        + (minutes
                                * DateUtils.MILLIS_PER_MINUTE / DateUtils.MILLIS_PER_SECOND),
                testResult);
    }

    @Test
    public void testSecondsofMinuteWithCalendar() {
        final long testResult = DateUtils.getFragmentInSeconds(aCalendar, Calendar.MINUTE);
        assertEquals(seconds, testResult);
        assertEquals(aCalendar.get(Calendar.SECOND), testResult);
    }

    @Test
    public void testSecondsofMinuteWithDate() {
        final long testResult = DateUtils.getFragmentInSeconds(aDate, Calendar.MINUTE);
        assertEquals(seconds, testResult);
    }

    @Test
    public void testSecondsOfMonthWithCalendar() {
        final long testResult = DateUtils.getFragmentInSeconds(aCalendar, Calendar.MONTH);
        assertEquals(
                seconds
                        + ((minutes * DateUtils.MILLIS_PER_MINUTE)
                                + (hours * DateUtils.MILLIS_PER_HOUR) + ((days - 1) * DateUtils.MILLIS_PER_DAY))
                        / DateUtils.MILLIS_PER_SECOND,
                testResult);
    }

    @Test
    public void testSecondsOfMonthWithDate() {
        final long testResult = DateUtils.getFragmentInSeconds(aDate, Calendar.MONTH);
        assertEquals(
                seconds
                        + ((minutes * DateUtils.MILLIS_PER_MINUTE)
                                + (hours * DateUtils.MILLIS_PER_HOUR) + ((days - 1) * DateUtils.MILLIS_PER_DAY))
                        / DateUtils.MILLIS_PER_SECOND,
                testResult);
    }

    @Test
    public void testSecondsOfYearWithCalendar() {
        final long testResult = DateUtils.getFragmentInSeconds(aCalendar, Calendar.YEAR);
        assertEquals(
                seconds
                        + ((minutes * DateUtils.MILLIS_PER_MINUTE)
                                + (hours * DateUtils.MILLIS_PER_HOUR) + ((aCalendar.get(Calendar.DAY_OF_YEAR) - 1) * DateUtils.MILLIS_PER_DAY))
                        / DateUtils.MILLIS_PER_SECOND,
                testResult);
    }

    @Test
    public void testSecondsOfYearWithDate() {
        final long testResult = DateUtils.getFragmentInSeconds(aDate, Calendar.YEAR);
        final Calendar cal = Calendar.getInstance();
        cal.setTime(aDate);
        assertEquals(
                seconds
                        + ((minutes * DateUtils.MILLIS_PER_MINUTE)
                                + (hours * DateUtils.MILLIS_PER_HOUR) + ((cal.get(Calendar.DAY_OF_YEAR) - 1) * DateUtils.MILLIS_PER_DAY))
                        / DateUtils.MILLIS_PER_SECOND,
                testResult);
    }
}
