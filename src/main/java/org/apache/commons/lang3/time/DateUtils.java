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

import java.text.ParseException;
import java.text.ParsePosition;
import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.Locale;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.TimeZone;
import java.util.concurrent.TimeUnit;

import org.apache.commons.lang3.LocaleUtils;

/**
 * A suite of utilities surrounding the use of the
 * {@link java.util.Calendar} and {@link java.util.Date} object.
 *
 * <p>DateUtils contains a lot of common methods considering manipulations
 * of Dates or Calendars. Some methods require some extra explanation.
 * The truncate, ceiling and round methods could be considered the Math.floor(),
 * Math.ceil() or Math.round versions for dates
 * This way date-fields will be ignored in bottom-up order.
 * As a complement to these methods we've introduced some fragment-methods.
 * With these methods the Date-fields will be ignored in top-down order.
 * Since a date without a year is not a valid date, you have to decide in what
 * kind of date-field you want your result, for instance milliseconds or days.
 * </p>
 * <p>
 * Several methods are provided for adding to {@link Date} objects, of the form
 * {@code addXXX(Date date, int amount)}. It is important to note these methods
 * use a {@link Calendar} internally (with default time zone and locale) and may
 * be affected by changes to daylight saving time (DST).
 * </p>
 *
 * @since 2.0
 */
public class DateUtils {

    /**
     * Number of milliseconds in a standard second.
     * @since 2.1
     */
    public static final long MILLIS_PER_SECOND = 1000;
    /**
     * Number of milliseconds in a standard minute.
     * @since 2.1
     */
    public static final long MILLIS_PER_MINUTE = 60 * MILLIS_PER_SECOND;
    /**
     * Number of milliseconds in a standard hour.
     * @since 2.1
     */
    public static final long MILLIS_PER_HOUR = 60 * MILLIS_PER_MINUTE;
    /**
     * Number of milliseconds in a standard day.
     * @since 2.1
     */
    public static final long MILLIS_PER_DAY = 24 * MILLIS_PER_HOUR;

    /**
     * This is half a month, so this represents whether a date is in the top
     * or bottom half of the month.
     */
    public static final int SEMI_MONTH = 1001;

    private static final int[][] fields = {
            {Calendar.MILLISECOND},
            {Calendar.SECOND},
            {Calendar.MINUTE},
            {Calendar.HOUR_OF_DAY, Calendar.HOUR},
            {Calendar.DATE, Calendar.DAY_OF_MONTH, Calendar.AM_PM
                /* Calendar.DAY_OF_YEAR, Calendar.DAY_OF_WEEK, Calendar.DAY_OF_WEEK_IN_MONTH */
            },
            {Calendar.MONTH, SEMI_MONTH},
            {Calendar.YEAR},
            {Calendar.ERA}};

    /**
     * A week range, starting on Sunday.
     */
    public static final int RANGE_WEEK_SUNDAY = 1;
    /**
     * A week range, starting on Monday.
     */
    public static final int RANGE_WEEK_MONDAY = 2;
    /**
     * A week range, starting on the day focused.
     */
    public static final int RANGE_WEEK_RELATIVE = 3;
    /**
     * A week range, centered around the day focused.
     */
    public static final int RANGE_WEEK_CENTER = 4;
    /**
     * A month range, the week starting on Sunday.
     */
    public static final int RANGE_MONTH_SUNDAY = 5;
    /**
     * A month range, the week starting on Monday.
     */
    public static final int RANGE_MONTH_MONDAY = 6;

    /**
     * Calendar modification types.
     */
    private enum ModifyType {
        /**
         * Truncation.
         */
        TRUNCATE,

        /**
         * Rounding.
         */
        ROUND,

        /**
         * Ceiling.
         */
        CEILING
    }

    /**
     * {@link DateUtils} instances should NOT be constructed in
     * standard programming. Instead, the static methods on the class should
     * be used, such as {@code DateUtils.parseDate(str);}.
     *
     * <p>This constructor is public to permit tools that require a JavaBean
     * instance to operate.</p>
     */
    public DateUtils() {
    }

    /**
     * Checks if two date objects are on the same day ignoring time.
     *
     * <p>28 Mar 2002 13:45 and 28 Mar 2002 06:01 would return true.
     * 28 Mar 2002 13:45 and 12 Mar 2002 13:45 would return false.
     * </p>
     *
     * @param date1  the first date, not altered, not null
     * @param date2  the second date, not altered, not null
     * @return true if they represent the same day
     * @throws NullPointerException if either date is {@code null}
     * @since 2.1
     */
    public static boolean isSameDay(final Date date1, final Date date2) {
        return isSameDay(toCalendar(date1), toCalendar(date2));
    }

    /**
     * Checks if two calendar objects are on the same day ignoring time.
     *
     * <p>28 Mar 2002 13:45 and 28 Mar 2002 06:01 would return true.
     * 28 Mar 2002 13:45 and 12 Mar 2002 13:45 would return false.
     * </p>
     *
     * @param cal1  the first calendar, not altered, not null
     * @param cal2  the second calendar, not altered, not null
     * @return true if they represent the same day
     * @throws NullPointerException if either calendar is {@code null}
     * @since 2.1
     */
    public static boolean isSameDay(final Calendar cal1, final Calendar cal2) {
        Objects.requireNonNull(cal1, "cal1");
        Objects.requireNonNull(cal2, "cal2");
        return cal1.get(Calendar.ERA) == cal2.get(Calendar.ERA) &&
                cal1.get(Calendar.YEAR) == cal2.get(Calendar.YEAR) &&
                cal1.get(Calendar.DAY_OF_YEAR) == cal2.get(Calendar.DAY_OF_YEAR);
    }

    /**
     * Checks if two date objects represent the same instant in time.
     *
     * <p>This method compares the long millisecond time of the two objects.</p>
     *
     * @param date1  the first date, not altered, not null
     * @param date2  the second date, not altered, not null
     * @return true if they represent the same millisecond instant
     * @throws NullPointerException if either date is {@code null}
     * @since 2.1
     */
    public static boolean isSameInstant(final Date date1, final Date date2) {
        Objects.requireNonNull(date1, "date1");
        Objects.requireNonNull(date2, "date2");
        return date1.getTime() == date2.getTime();
    }

    /**
     * Checks if two calendar objects represent the same instant in time.
     *
     * <p>This method compares the long millisecond time of the two objects.</p>
     *
     * @param cal1  the first calendar, not altered, not null
     * @param cal2  the second calendar, not altered, not null
     * @return true if they represent the same millisecond instant
     * @throws NullPointerException if either date is {@code null}
     * @since 2.1
     */
    public static boolean isSameInstant(final Calendar cal1, final Calendar cal2) {
        Objects.requireNonNull(cal1, "cal1");
        Objects.requireNonNull(cal2, "cal2");
        return cal1.getTime().getTime() == cal2.getTime().getTime();
    }

    /**
     * Checks if two calendar objects represent the same local time.
     *
     * <p>This method compares the values of the fields of the two objects.
     * In addition, both calendars must be the same of the same type.</p>
     *
     * @param cal1  the first calendar, not altered, not null
     * @param cal2  the second calendar, not altered, not null
     * @return true if they represent the same millisecond instant
     * @throws NullPointerException if either date is {@code null}
     * @since 2.1
     */
    public static boolean isSameLocalTime(final Calendar cal1, final Calendar cal2) {
        Objects.requireNonNull(cal1, "cal1");
        Objects.requireNonNull(cal2, "cal2");
        return cal1.get(Calendar.MILLISECOND) == cal2.get(Calendar.MILLISECOND) &&
                cal1.get(Calendar.SECOND) == cal2.get(Calendar.SECOND) &&
                cal1.get(Calendar.MINUTE) == cal2.get(Calendar.MINUTE) &&
                cal1.get(Calendar.HOUR_OF_DAY) == cal2.get(Calendar.HOUR_OF_DAY) &&
                cal1.get(Calendar.DAY_OF_YEAR) == cal2.get(Calendar.DAY_OF_YEAR) &&
                cal1.get(Calendar.YEAR) == cal2.get(Calendar.YEAR) &&
                cal1.get(Calendar.ERA) == cal2.get(Calendar.ERA) &&
                cal1.getClass() == cal2.getClass();
    }

    /**
     * Parses a string representing a date by trying a variety of different parsers.
     *
     * <p>The parse will try each parse pattern in turn.
     * A parse is only deemed successful if it parses the whole of the input string.
     * If no parse patterns match, a ParseException is thrown.</p>
     * The parser will be lenient toward the parsed date.
     *
     * @param str  the date to parse, not null
     * @param parsePatterns  the date format patterns to use, see SimpleDateFormat, not null
     * @return the parsed date
     * @throws NullPointerException if the date string or pattern array is null
     * @throws ParseException if none of the date patterns were suitable (or there were none)
     */
    public static Date parseDate(final String str, final String... parsePatterns) throws ParseException {
        return parseDate(str, null, parsePatterns);
    }

    /**
     * Parses a string representing a date by trying a variety of different parsers,
     * using the default date format symbols for the given locale.
     *
     * <p>The parse will try each parse pattern in turn.
     * A parse is only deemed successful if it parses the whole of the input string.
     * If no parse patterns match, a ParseException is thrown.</p>
     * The parser will be lenient toward the parsed date.
     *
     * @param str  the date to parse, not null
     * @param locale the locale whose date format symbols should be used. If {@code null},
     * the system locale is used (as per {@link #parseDate(String, String...)}).
     * @param parsePatterns  the date format patterns to use, see SimpleDateFormat, not null
     * @return the parsed date
     * @throws NullPointerException if the date string or pattern array is null
     * @throws ParseException if none of the date patterns were suitable (or there were none)
     * @since 3.2
     */
    public static Date parseDate(final String str, final Locale locale, final String... parsePatterns) throws ParseException {
        return parseDateWithLeniency(str, locale, parsePatterns, true);
    }

    /**
     * Parses a string representing a date by trying a variety of different parsers.
     *
     * <p>The parse will try each parse pattern in turn.
     * A parse is only deemed successful if it parses the whole of the input string.
     * If no parse patterns match, a ParseException is thrown.</p>
     * The parser parses strictly - it does not allow for dates such as "February 942, 1996".
     *
     * @param str  the date to parse, not null
     * @param parsePatterns  the date format patterns to use, see SimpleDateFormat, not null
     * @return the parsed date
     * @throws NullPointerException if the date string or pattern array is null
     * @throws ParseException if none of the date patterns were suitable
     * @since 2.5
     */
    public static Date parseDateStrictly(final String str, final String... parsePatterns) throws ParseException {
        return parseDateStrictly(str, null, parsePatterns);
    }

    /**
     * Parses a string representing a date by trying a variety of different parsers,
     * using the default date format symbols for the given locale..
     *
     * <p>The parse will try each parse pattern in turn.
     * A parse is only deemed successful if it parses the whole of the input string.
     * If no parse patterns match, a ParseException is thrown.</p>
     * The parser parses strictly - it does not allow for dates such as "February 942, 1996".
     *
     * @param str  the date to parse, not null
     * @param locale the locale whose date format symbols should be used. If {@code null},
     * the system locale is used (as per {@link #parseDateStrictly(String, String...)}).
     * @param parsePatterns  the date format patterns to use, see SimpleDateFormat, not null
     * @return the parsed date
     * @throws NullPointerException if the date string or pattern array is null
     * @throws ParseException if none of the date patterns were suitable
     * @since 3.2
     */
    public static Date parseDateStrictly(final String str, final Locale locale, final String... parsePatterns) throws ParseException {
        return parseDateWithLeniency(str, locale, parsePatterns, false);
    }

    /**
     * Parses a string representing a date by trying a variety of different parsers.
     *
     * <p>The parse will try each parse pattern in turn.
     * A parse is only deemed successful if it parses the whole of the input string.
     * If no parse patterns match, a ParseException is thrown.</p>
     *
     * @param dateStr  the date to parse, not null
     * @param locale the locale to use when interpreting the pattern, can be null in which
     * case the default system locale is used
     * @param parsePatterns  the date format patterns to use, see SimpleDateFormat, not null
     * @param lenient Specify whether or not date/time parsing is to be lenient.
     * @return the parsed date
     * @throws NullPointerException if the date string or pattern array is null
     * @throws ParseException if none of the date patterns were suitable
     * @see java.util.Calendar#isLenient()
     */
    private static Date parseDateWithLeniency(final String dateStr, final Locale locale, final String[] parsePatterns,
        final boolean lenient) throws ParseException {
        Objects.requireNonNull(dateStr, "str");
        Objects.requireNonNull(parsePatterns, "parsePatterns");

        final TimeZone tz = TimeZone.getDefault();
        final Locale lcl = LocaleUtils.toLocale(locale);
        final ParsePosition pos = new ParsePosition(0);
        final Calendar calendar = Calendar.getInstance(tz, lcl);
        calendar.setLenient(lenient);

        for (final String parsePattern : parsePatterns) {
            final FastDateParser fdp = new FastDateParser(parsePattern, tz, lcl);
            calendar.clear();
            try {
                if (fdp.parse(dateStr, pos, calendar) && pos.getIndex() == dateStr.length()) {
                    return calendar.getTime();
                }
            } catch (final IllegalArgumentException ignored) {
                // leniency is preventing calendar from being set
            }
            pos.setIndex(0);
        }
        throw new ParseException("Unable to parse the date: " + dateStr, -1);
    }

    /**
     * Adds a number of years to a date returning a new object.
     * The original {@link Date} is unchanged.
     *
     * @param date  the date, not null
     * @param amount  the amount to add, may be negative
     * @return the new {@link Date} with the amount added
     * @throws NullPointerException if the date is null
     */
    public static Date addYears(final Date date, final int amount) {
        return add(date, Calendar.YEAR, amount);
    }

    /**
     * Adds a number of months to a date returning a new object.
     * The original {@link Date} is unchanged.
     *
     * @param date  the date, not null
     * @param amount  the amount to add, may be negative
     * @return the new {@link Date} with the amount added
     * @throws NullPointerException if the date is null
     */
    public static Date addMonths(final Date date, final int amount) {
        return add(date, Calendar.MONTH, amount);
    }

    /**
     * Adds a number of weeks to a date returning a new object.
     * The original {@link Date} is unchanged.
     *
     * @param date  the date, not null
     * @param amount  the amount to add, may be negative
     * @return the new {@link Date} with the amount added
     * @throws NullPointerException if the date is null
     */
    public static Date addWeeks(final Date date, final int amount) {
        return add(date, Calendar.WEEK_OF_YEAR, amount);
    }

    /**
     * Adds a number of days to a date returning a new object.
     * The original {@link Date} is unchanged.
     *
     * @param date  the date, not null
     * @param amount  the amount to add, may be negative
     * @return the new {@link Date} with the amount added
     * @throws NullPointerException if the date is null
     */
    public static Date addDays(final Date date, final int amount) {
        return add(date, Calendar.DAY_OF_MONTH, amount);
    }

    /**
     * Adds a number of hours to a date returning a new object.
     * The original {@link Date} is unchanged.
     *
     * @param date  the date, not null
     * @param amount  the amount to add, may be negative
     * @return the new {@link Date} with the amount added
     * @throws NullPointerException if the date is null
     */
    public static Date addHours(final Date date, final int amount) {
        return add(date, Calendar.HOUR_OF_DAY, amount);
    }

    /**
     * Adds a number of minutes to a date returning a new object.
     * The original {@link Date} is unchanged.
     *
     * @param date  the date, not null
     * @param amount  the amount to add, may be negative
     * @return the new {@link Date} with the amount added
     * @throws NullPointerException if the date is null
     */
    public static Date addMinutes(final Date date, final int amount) {
        return add(date, Calendar.MINUTE, amount);
    }

    /**
     * Adds a number of seconds to a date returning a new object.
     * The original {@link Date} is unchanged.
     *
     * @param date  the date, not null
     * @param amount  the amount to add, may be negative
     * @return the new {@link Date} with the amount added
     * @throws NullPointerException if the date is null
     */
    public static Date addSeconds(final Date date, final int amount) {
        return add(date, Calendar.SECOND, amount);
    }

    /**
     * Adds a number of milliseconds to a date returning a new object.
     * The original {@link Date} is unchanged.
     *
     * @param date  the date, not null
     * @param amount  the amount to add, may be negative
     * @return the new {@link Date} with the amount added
     * @throws NullPointerException if the date is null
     */
    public static Date addMilliseconds(final Date date, final int amount) {
        return add(date, Calendar.MILLISECOND, amount);
    }

    /**
     * Adds to a date returning a new object.
     * The original {@link Date} is unchanged.
     *
     * @param date  the date, not null
     * @param calendarField  the calendar field to add to
     * @param amount  the amount to add, may be negative
     * @return the new {@link Date} with the amount added
     * @throws NullPointerException if the date is null
     */
    private static Date add(final Date date, final int calendarField, final int amount) {
        validateDateNotNull(date);
        final Calendar c = Calendar.getInstance();
        c.setTime(date);
        c.add(calendarField, amount);
        return c.getTime();
    }

    /**
     * Sets the years field to a date returning a new object.
     * The original {@link Date} is unchanged.
     *
     * @param date  the date, not null
     * @param amount the amount to set
     * @return a new {@link Date} set with the specified value
     * @throws NullPointerException if the date is null
     * @since 2.4
     */
    public static Date setYears(final Date date, final int amount) {
        return set(date, Calendar.YEAR, amount);
    }

    /**
     * Sets the months field to a date returning a new object.
     * The original {@link Date} is unchanged.
     *
     * @param date  the date, not null
     * @param amount the amount to set
     * @return a new {@link Date} set with the specified value
     * @throws NullPointerException if the date is null
     * @throws IllegalArgumentException if {@code amount} is not in the range
     *  {@code 0 <= amount <= 11}
     * @since 2.4
     */
    public static Date setMonths(final Date date, final int amount) {
        return set(date, Calendar.MONTH, amount);
    }

    /**
     * Sets the day of month field to a date returning a new object.
     * The original {@link Date} is unchanged.
     *
     * @param date  the date, not null
     * @param amount the amount to set
     * @return a new {@link Date} set with the specified value
     * @throws NullPointerException if the date is null
     * @throws IllegalArgumentException if {@code amount} is not in the range
     *  {@code 1 <= amount <= 31}
     * @since 2.4
     */
    public static Date setDays(final Date date, final int amount) {
        return set(date, Calendar.DAY_OF_MONTH, amount);
    }

    /**
     * Sets the hours field to a date returning a new object.  Hours range
     * from  0-23.
     * The original {@link Date} is unchanged.
     *
     * @param date  the date, not null
     * @param amount the amount to set
     * @return a new {@link Date} set with the specified value
     * @throws NullPointerException if the date is null
     * @throws IllegalArgumentException if {@code amount} is not in the range
     *  {@code 0 <= amount <= 23}
     * @since 2.4
     */
    public static Date setHours(final Date date, final int amount) {
        return set(date, Calendar.HOUR_OF_DAY, amount);
    }

    /**
     * Sets the minute field to a date returning a new object.
     * The original {@link Date} is unchanged.
     *
     * @param date  the date, not null
     * @param amount the amount to set
     * @return a new {@link Date} set with the specified value
     * @throws NullPointerException if the date is null
     * @throws IllegalArgumentException if {@code amount} is not in the range
     *  {@code 0 <= amount <= 59}
     * @since 2.4
     */
    public static Date setMinutes(final Date date, final int amount) {
        return set(date, Calendar.MINUTE, amount);
    }

    /**
     * Sets the seconds field to a date returning a new object.
     * The original {@link Date} is unchanged.
     *
     * @param date  the date, not null
     * @param amount the amount to set
     * @return a new {@link Date} set with the specified value
     * @throws NullPointerException if the date is null
     * @throws IllegalArgumentException if {@code amount} is not in the range
     *  {@code 0 <= amount <= 59}
     * @since 2.4
     */
    public static Date setSeconds(final Date date, final int amount) {
        return set(date, Calendar.SECOND, amount);
    }

    /**
     * Sets the milliseconds field to a date returning a new object.
     * The original {@link Date} is unchanged.
     *
     * @param date  the date, not null
     * @param amount the amount to set
     * @return a new {@link Date} set with the specified value
     * @throws NullPointerException if the date is null
     * @throws IllegalArgumentException if {@code amount} is not in the range
     *  {@code 0 <= amount <= 999}
     * @since 2.4
     */
    public static Date setMilliseconds(final Date date, final int amount) {
        return set(date, Calendar.MILLISECOND, amount);
    }

    /**
     * Sets the specified field to a date returning a new object.
     * This does not use a lenient calendar.
     * The original {@link Date} is unchanged.
     *
     * @param date  the date, not null
     * @param calendarField  the {@link Calendar} field to set the amount to
     * @param amount the amount to set
     * @return a new {@link Date} set with the specified value
     * @throws NullPointerException if the date is null
     * @since 2.4
     */
    private static Date set(final Date date, final int calendarField, final int amount) {
        validateDateNotNull(date);
        // getInstance() returns a new object, so this method is thread safe.
        final Calendar c = Calendar.getInstance();
        c.setLenient(false);
        c.setTime(date);
        c.set(calendarField, amount);
        return c.getTime();
    }

    /**
     * Converts a {@link Date} into a {@link Calendar}.
     *
     * @param date the date to convert to a Calendar
     * @return the created Calendar
     * @throws NullPointerException if null is passed in
     * @since 3.0
     */
    public static Calendar toCalendar(final Date date) {
        final Calendar c = Calendar.getInstance();
        c.setTime(Objects.requireNonNull(date, "date"));
        return c;
    }

    /**
     * Converts a {@link Date} of a given {@link TimeZone} into a {@link Calendar}
     * @param date the date to convert to a Calendar
     * @param tz the time zone of the {@code date}
     * @return the created Calendar
     * @throws NullPointerException if {@code date} or {@code tz} is null
     */
    public static Calendar toCalendar(final Date date, final TimeZone tz) {
        final Calendar c = Calendar.getInstance(tz);
        c.setTime(Objects.requireNonNull(date, "date"));
        return c;
    }

    /**
     * Rounds a date, leaving the field specified as the most
     * significant field.
     *
     * <p>For example, if you had the date-time of 28 Mar 2002
     * 13:45:01.231, if this was passed with HOUR, it would return
     * 28 Mar 2002 14:00:00.000. If this was passed with MONTH, it
     * would return 1 April 2002 0:00:00.000.</p>
     *
     * <p>For a date in a time zone that handles the change to daylight
     * saving time, rounding to Calendar.HOUR_OF_DAY will behave as follows.
     * Suppose daylight saving time begins at 02:00 on March 30. Rounding a
     * date that crosses this time would produce the following values:
     * </p>
     * <ul>
     * <li>March 30, 2003 01:10 rounds to March 30, 2003 01:00</li>
     * <li>March 30, 2003 01:40 rounds to March 30, 2003 03:00</li>
     * <li>March 30, 2003 02:10 rounds to March 30, 2003 03:00</li>
     * <li>March 30, 2003 02:40 rounds to March 30, 2003 04:00</li>
     * </ul>
     *
     * @param date  the date to work with, not null
     * @param field  the field from {@link Calendar} or {@code SEMI_MONTH}
     * @return the different rounded date, not null
     * @throws NullPointerException if the date is null
     * @throws ArithmeticException if the year is over 280 million
     */
    public static Date round(final Date date, final int field) {
        return modify(toCalendar(date), field, ModifyType.ROUND).getTime();
    }

    /**
     * Rounds a date, leaving the field specified as the most
     * significant field.
     *
     * <p>For example, if you had the date-time of 28 Mar 2002
     * 13:45:01.231, if this was passed with HOUR, it would return
     * 28 Mar 2002 14:00:00.000. If this was passed with MONTH, it
     * would return 1 April 2002 0:00:00.000.</p>
     *
     * <p>For a date in a time zone that handles the change to daylight
     * saving time, rounding to Calendar.HOUR_OF_DAY will behave as follows.
     * Suppose daylight saving time begins at 02:00 on March 30. Rounding a
     * date that crosses this time would produce the following values:
     * </p>
     * <ul>
     * <li>March 30, 2003 01:10 rounds to March 30, 2003 01:00</li>
     * <li>March 30, 2003 01:40 rounds to March 30, 2003 03:00</li>
     * <li>March 30, 2003 02:10 rounds to March 30, 2003 03:00</li>
     * <li>March 30, 2003 02:40 rounds to March 30, 2003 04:00</li>
     * </ul>
     *
     * @param calendar  the date to work with, not null
     * @param field  the field from {@link Calendar} or {@code SEMI_MONTH}
     * @return the different rounded date, not null
     * @throws NullPointerException if the date is {@code null}
     * @throws ArithmeticException if the year is over 280 million
     */
    public static Calendar round(final Calendar calendar, final int field) {
        Objects.requireNonNull(calendar, "calendar");
        return modify((Calendar) calendar.clone(), field, ModifyType.ROUND);
    }

    /**
     * Rounds a date, leaving the field specified as the most
     * significant field.
     *
     * <p>For example, if you had the date-time of 28 Mar 2002
     * 13:45:01.231, if this was passed with HOUR, it would return
     * 28 Mar 2002 14:00:00.000. If this was passed with MONTH, it
     * would return 1 April 2002 0:00:00.000.</p>
     *
     * <p>For a date in a time zone that handles the change to daylight
     * saving time, rounding to Calendar.HOUR_OF_DAY will behave as follows.
     * Suppose daylight saving time begins at 02:00 on March 30. Rounding a
     * date that crosses this time would produce the following values:
     * </p>
     * <ul>
     * <li>March 30, 2003 01:10 rounds to March 30, 2003 01:00</li>
     * <li>March 30, 2003 01:40 rounds to March 30, 2003 03:00</li>
     * <li>March 30, 2003 02:10 rounds to March 30, 2003 03:00</li>
     * <li>March 30, 2003 02:40 rounds to March 30, 2003 04:00</li>
     * </ul>
     *
     * @param date  the date to work with, either {@link Date} or {@link Calendar}, not null
     * @param field  the field from {@link Calendar} or {@code SEMI_MONTH}
     * @return the different rounded date, not null
     * @throws NullPointerException if the date is {@code null}
     * @throws ClassCastException if the object type is not a {@link Date} or {@link Calendar}
     * @throws ArithmeticException if the year is over 280 million
     */
    public static Date round(final Object date, final int field) {
        Objects.requireNonNull(date, "date");
        if (date instanceof Date) {
            return round((Date) date, field);
        }
        if (date instanceof Calendar) {
            return round((Calendar) date, field).getTime();
        }
        throw new ClassCastException("Could not round " + date);
    }

    /**
     * Truncates a date, leaving the field specified as the most
     * significant field.
     *
     * <p>For example, if you had the date-time of 28 Mar 2002
     * 13:45:01.231, if you passed with HOUR, it would return 28 Mar
     * 2002 13:00:00.000.  If this was passed with MONTH, it would
     * return 1 Mar 2002 0:00:00.000.</p>
     *
     * @param date  the date to work with, not null
     * @param field  the field from {@link Calendar} or {@code SEMI_MONTH}
     * @return the different truncated date, not null
     * @throws NullPointerException if the date is {@code null}
     * @throws ArithmeticException if the year is over 280 million
     */
    public static Date truncate(final Date date, final int field) {
        return modify(toCalendar(date), field, ModifyType.TRUNCATE).getTime();
    }

    /**
     * Truncates a date, leaving the field specified as the most
     * significant field.
     *
     * <p>For example, if you had the date-time of 28 Mar 2002
     * 13:45:01.231, if you passed with HOUR, it would return 28 Mar
     * 2002 13:00:00.000.  If this was passed with MONTH, it would
     * return 1 Mar 2002 0:00:00.000.</p>
     *
     * @param date  the date to work with, not null
     * @param field  the field from {@link Calendar} or {@code SEMI_MONTH}
     * @return the different truncated date, not null
     * @throws NullPointerException if the date is {@code null}
     * @throws ArithmeticException if the year is over 280 million
     */
    public static Calendar truncate(final Calendar date, final int field) {
        Objects.requireNonNull(date, "date");
        return modify((Calendar) date.clone(), field, ModifyType.TRUNCATE);
    }

    /**
     * Truncates a date, leaving the field specified as the most
     * significant field.
     *
     * <p>For example, if you had the date-time of 28 Mar 2002
     * 13:45:01.231, if you passed with HOUR, it would return 28 Mar
     * 2002 13:00:00.000.  If this was passed with MONTH, it would
     * return 1 Mar 2002 0:00:00.000.</p>
     *
     * @param date  the date to work with, either {@link Date} or {@link Calendar}, not null
     * @param field  the field from {@link Calendar} or {@code SEMI_MONTH}
     * @return the different truncated date, not null
     * @throws NullPointerException if the date is {@code null}
     * @throws ClassCastException if the object type is not a {@link Date} or {@link Calendar}
     * @throws ArithmeticException if the year is over 280 million
     */
    public static Date truncate(final Object date, final int field) {
        Objects.requireNonNull(date, "date");
        if (date instanceof Date) {
            return truncate((Date) date, field);
        }
        if (date instanceof Calendar) {
            return truncate((Calendar) date, field).getTime();
        }
        throw new ClassCastException("Could not truncate " + date);
    }

    /**
     * Gets a date ceiling, leaving the field specified as the most
     * significant field.
     *
     * <p>For example, if you had the date-time of 28 Mar 2002
     * 13:45:01.231, if you passed with HOUR, it would return 28 Mar
     * 2002 14:00:00.000.  If this was passed with MONTH, it would
     * return 1 Apr 2002 0:00:00.000.</p>
     *
     * @param date  the date to work with, not null
     * @param field  the field from {@link Calendar} or {@code SEMI_MONTH}
     * @return the different ceil date, not null
     * @throws NullPointerException if the date is {@code null}
     * @throws ArithmeticException if the year is over 280 million
     * @since 2.5
     */
    public static Date ceiling(final Date date, final int field) {
        return modify(toCalendar(date), field, ModifyType.CEILING).getTime();
    }

    /**
     * Gets a date ceiling, leaving the field specified as the most
     * significant field.
     *
     * <p>For example, if you had the date-time of 28 Mar 2002
     * 13:45:01.231, if you passed with HOUR, it would return 28 Mar
     * 2002 14:00:00.000.  If this was passed with MONTH, it would
     * return 1 Apr 2002 0:00:00.000.</p>
     *
     * @param calendar  the date to work with, not null
     * @param field  the field from {@link Calendar} or {@code SEMI_MONTH}
     * @return the different ceil date, not null
     * @throws NullPointerException if the date is {@code null}
     * @throws ArithmeticException if the year is over 280 million
     * @since 2.5
     */
    public static Calendar ceiling(final Calendar calendar, final int field) {
        Objects.requireNonNull(calendar, "calendar");
        return modify((Calendar) calendar.clone(), field, ModifyType.CEILING);
    }

    /**
     * Gets a date ceiling, leaving the field specified as the most
     * significant field.
     *
     * <p>For example, if you had the date-time of 28 Mar 2002
     * 13:45:01.231, if you passed with HOUR, it would return 28 Mar
     * 2002 14:00:00.000.  If this was passed with MONTH, it would
     * return 1 Apr 2002 0:00:00.000.</p>
     *
     * @param date  the date to work with, either {@link Date} or {@link Calendar}, not null
     * @param field  the field from {@link Calendar} or {@code SEMI_MONTH}
     * @return the different ceil date, not null
     * @throws NullPointerException if the date is {@code null}
     * @throws ClassCastException if the object type is not a {@link Date} or {@link Calendar}
     * @throws ArithmeticException if the year is over 280 million
     * @since 2.5
     */
    public static Date ceiling(final Object date, final int field) {
        Objects.requireNonNull(date, "date");
        if (date instanceof Date) {
            return ceiling((Date) date, field);
        }
        if (date instanceof Calendar) {
            return ceiling((Calendar) date, field).getTime();
        }
        throw new ClassCastException("Could not find ceiling of for type: " + date.getClass());
    }

    /**
     * Internal calculation method.
     *
     * @param val  the calendar, not null
     * @param field  the field constant
     * @param modType  type to truncate, round or ceiling
     * @return the given calendar
     * @throws ArithmeticException if the year is over 280 million
     */
    private static Calendar modify(final Calendar val, final int field, final ModifyType modType) {
        if (val.get(Calendar.YEAR) > 280000000) {
            throw new ArithmeticException("Calendar value too large for accurate calculations");
        }

        if (field == Calendar.MILLISECOND) {
            return val;
        }

        // Fix for LANG-59 START
        // see https://issues.apache.org/jira/browse/LANG-59
        //
        // Manually truncate milliseconds, seconds and minutes, rather than using
        // Calendar methods.

        final Date date = val.getTime();
        long time = date.getTime();
        boolean done = false;

        // truncate milliseconds
        final int millisecs = val.get(Calendar.MILLISECOND);
        if (ModifyType.TRUNCATE == modType || millisecs < 500) {
            time = time - millisecs;
        }
        if (field == Calendar.SECOND) {
            done = true;
        }

        // truncate seconds
        final int seconds = val.get(Calendar.SECOND);
        if (!done && (ModifyType.TRUNCATE == modType || seconds < 30)) {
            time = time - (seconds * 1000L);
        }
        if (field == Calendar.MINUTE) {
            done = true;
        }

        // truncate minutes
        final int minutes = val.get(Calendar.MINUTE);
        if (!done && (ModifyType.TRUNCATE == modType || minutes < 30)) {
            time = time - (minutes * 60000L);
        }

        // reset time
        if (date.getTime() != time) {
            date.setTime(time);
            val.setTime(date);
        }
        // Fix for LANG-59 END

        boolean roundUp = false;
        for (final int[] aField : fields) {
            for (final int element : aField) {
                if (element == field) {
                    //This is our field... we stop looping
                    if (modType == ModifyType.CEILING || modType == ModifyType.ROUND && roundUp) {
                        if (field == SEMI_MONTH) {
                            //This is a special case that's hard to generalize
                            //If the date is 1, we round up to 16, otherwise
                            //  we subtract 15 days and add 1 month
                            if (val.get(Calendar.DATE) == 1) {
                                val.add(Calendar.DATE, 15);
                            } else {
                                val.add(Calendar.DATE, -15);
                                val.add(Calendar.MONTH, 1);
                            }
                        // Fix for LANG-440 START
                        } else if (field == Calendar.AM_PM) {
                            // This is a special case
                            // If the time is 0, we round up to 12, otherwise
                            //  we subtract 12 hours and add 1 day
                            if (val.get(Calendar.HOUR_OF_DAY) == 0) {
                                val.add(Calendar.HOUR_OF_DAY, 12);
                            } else {
                                val.add(Calendar.HOUR_OF_DAY, -12);
                                val.add(Calendar.DATE, 1);
                            }
                            // Fix for LANG-440 END
                        } else {
                            //We need at add one to this field since the
                            //  last number causes us to round up
                            val.add(aField[0], 1);
                        }
                    }
                    return val;
                }
            }
            //We have various fields that are not easy roundings
            int offset = 0;
            boolean offsetSet = false;
            //These are special types of fields that require different rounding rules
            switch (field) {
                case SEMI_MONTH:
                    if (aField[0] == Calendar.DATE) {
                        //If we're going to drop the DATE field's value,
                        //  we want to do this our own way.
                        //We need to subtract 1 since the date has a minimum of 1
                        offset = val.get(Calendar.DATE) - 1;
                        //If we're above 15 days adjustment, that means we're in the
                        //  bottom half of the month and should stay accordingly.
                        if (offset >= 15) {
                            offset -= 15;
                        }
                        //Record whether we're in the top or bottom half of that range
                        roundUp = offset > 7;
                        offsetSet = true;
                    }
                    break;
                case Calendar.AM_PM:
                    if (aField[0] == Calendar.HOUR_OF_DAY) {
                        //If we're going to drop the HOUR field's value,
                        //  we want to do this our own way.
                        offset = val.get(Calendar.HOUR_OF_DAY);
                        if (offset >= 12) {
                            offset -= 12;
                        }
                        roundUp = offset >= 6;
                        offsetSet = true;
                    }
                    break;
                default:
                    break;
            }
            if (!offsetSet) {
                final int min = val.getActualMinimum(aField[0]);
                final int max = val.getActualMaximum(aField[0]);
                //Calculate the offset from the minimum allowed value
                offset = val.get(aField[0]) - min;
                //Set roundUp if this is more than half way between the minimum and maximum
                roundUp = offset > ((max - min) / 2);
            }
            //We need to remove this field
            if (offset != 0) {
                val.set(aField[0], val.get(aField[0]) - offset);
            }
        }
        throw new IllegalArgumentException("The field " + field + " is not supported");
    }

    /**
     * Constructs an {@link Iterator} over each day in a date
     * range defined by a focus date and range style.
     *
     * <p>For instance, passing Thursday, July 4, 2002 and a
     * {@code RANGE_MONTH_SUNDAY} will return an {@link Iterator}
     * that starts with Sunday, June 30, 2002 and ends with Saturday, August 3,
     * 2002, returning a Calendar instance for each intermediate day.</p>
     *
     * <p>This method provides an iterator that returns Calendar objects.
     * The days are progressed using {@link Calendar#add(int, int)}.</p>
     *
     * @param focus  the date to work with, not null
     * @param rangeStyle  the style constant to use. Must be one of
     * {@link DateUtils#RANGE_MONTH_SUNDAY},
     * {@link DateUtils#RANGE_MONTH_MONDAY},
     * {@link DateUtils#RANGE_WEEK_SUNDAY},
     * {@link DateUtils#RANGE_WEEK_MONDAY},
     * {@link DateUtils#RANGE_WEEK_RELATIVE},
     * {@link DateUtils#RANGE_WEEK_CENTER}
     * @return the date iterator, not null, not null
     * @throws NullPointerException if the date is {@code null}
     * @throws IllegalArgumentException if the rangeStyle is invalid
     */
    public static Iterator<Calendar> iterator(final Date focus, final int rangeStyle) {
        return iterator(toCalendar(focus), rangeStyle);
    }

    /**
     * Constructs an {@link Iterator} over each day in a date
     * range defined by a focus date and range style.
     *
     * <p>For instance, passing Thursday, July 4, 2002 and a
     * {@code RANGE_MONTH_SUNDAY} will return an {@link Iterator}
     * that starts with Sunday, June 30, 2002 and ends with Saturday, August 3,
     * 2002, returning a Calendar instance for each intermediate day.</p>
     *
     * <p>This method provides an iterator that returns Calendar objects.
     * The days are progressed using {@link Calendar#add(int, int)}.</p>
     *
     * @param calendar  the date to work with, not null
     * @param rangeStyle  the style constant to use. Must be one of
     * {@link DateUtils#RANGE_MONTH_SUNDAY},
     * {@link DateUtils#RANGE_MONTH_MONDAY},
     * {@link DateUtils#RANGE_WEEK_SUNDAY},
     * {@link DateUtils#RANGE_WEEK_MONDAY},
     * {@link DateUtils#RANGE_WEEK_RELATIVE},
     * {@link DateUtils#RANGE_WEEK_CENTER}
     * @return the date iterator, not null
     * @throws NullPointerException if calendar is {@code null}
     * @throws IllegalArgumentException if the rangeStyle is invalid
     */
    public static Iterator<Calendar> iterator(final Calendar calendar, final int rangeStyle) {
        Objects.requireNonNull(calendar, "calendar");
        final Calendar start;
        final Calendar end;
        int startCutoff = Calendar.SUNDAY;
        int endCutoff = Calendar.SATURDAY;
        switch (rangeStyle) {
            case RANGE_MONTH_SUNDAY:
            case RANGE_MONTH_MONDAY:
                //Set start to the first of the month
                start = truncate(calendar, Calendar.MONTH);
                //Set end to the last of the month
                end = (Calendar) start.clone();
                end.add(Calendar.MONTH, 1);
                end.add(Calendar.DATE, -1);
                //Loop start back to the previous sunday or monday
                if (rangeStyle == RANGE_MONTH_MONDAY) {
                    startCutoff = Calendar.MONDAY;
                    endCutoff = Calendar.SUNDAY;
                }
                break;
            case RANGE_WEEK_SUNDAY:
            case RANGE_WEEK_MONDAY:
            case RANGE_WEEK_RELATIVE:
            case RANGE_WEEK_CENTER:
                //Set start and end to the current date
                start = truncate(calendar, Calendar.DATE);
                end = truncate(calendar, Calendar.DATE);
                switch (rangeStyle) {
                    case RANGE_WEEK_SUNDAY:
                        //already set by default
                        break;
                    case RANGE_WEEK_MONDAY:
                        startCutoff = Calendar.MONDAY;
                        endCutoff = Calendar.SUNDAY;
                        break;
                    case RANGE_WEEK_RELATIVE:
                        startCutoff = calendar.get(Calendar.DAY_OF_WEEK);
                        endCutoff = startCutoff - 1;
                        break;
                    case RANGE_WEEK_CENTER:
                        startCutoff = calendar.get(Calendar.DAY_OF_WEEK) - 3;
                        endCutoff = calendar.get(Calendar.DAY_OF_WEEK) + 3;
                        break;
                    default:
                        break;
                }
                break;
            default:
                throw new IllegalArgumentException("The range style " + rangeStyle + " is not valid.");
        }
        if (startCutoff < Calendar.SUNDAY) {
            startCutoff += 7;
        }
        if (startCutoff > Calendar.SATURDAY) {
            startCutoff -= 7;
        }
        if (endCutoff < Calendar.SUNDAY) {
            endCutoff += 7;
        }
        if (endCutoff > Calendar.SATURDAY) {
            endCutoff -= 7;
        }
        while (start.get(Calendar.DAY_OF_WEEK) != startCutoff) {
            start.add(Calendar.DATE, -1);
        }
        while (end.get(Calendar.DAY_OF_WEEK) != endCutoff) {
            end.add(Calendar.DATE, 1);
        }
        return new DateIterator(start, end);
    }

    /**
     * Constructs an {@link Iterator} over each day in a date
     * range defined by a focus date and range style.
     *
     * <p>For instance, passing Thursday, July 4, 2002 and a
     * {@code RANGE_MONTH_SUNDAY} will return an {@link Iterator}
     * that starts with Sunday, June 30, 2002 and ends with Saturday, August 3,
     * 2002, returning a Calendar instance for each intermediate day.</p>
     *
     * @param calendar  the date to work with, either {@link Date} or {@link Calendar}, not null
     * @param rangeStyle  the style constant to use. Must be one of the range
     * styles listed for the {@link #iterator(Calendar, int)} method.
     * @return the date iterator, not null
     * @throws NullPointerException if the date is {@code null}
     * @throws ClassCastException if the object type is not a {@link Date} or {@link Calendar}
     */
    public static Iterator<?> iterator(final Object calendar, final int rangeStyle) {
        Objects.requireNonNull(calendar, "calendar");
        if (calendar instanceof Date) {
            return iterator((Date) calendar, rangeStyle);
        }
        if (calendar instanceof Calendar) {
            return iterator((Calendar) calendar, rangeStyle);
        }
        throw new ClassCastException("Could not iterate based on " + calendar);
    }

    /**
     * Returns the number of milliseconds within the
     * fragment. All datefields greater than the fragment will be ignored.
     *
     * <p>Asking the milliseconds of any date will only return the number of milliseconds
     * of the current second (resulting in a number between 0 and 999). This
     * method will retrieve the number of milliseconds for any fragment.
     * For example, if you want to calculate the number of milliseconds past today,
     * your fragment is Calendar.DATE or Calendar.DAY_OF_YEAR. The result will
     * be all milliseconds of the past hour(s), minutes(s) and second(s).</p>
     *
     * <p>Valid fragments are: Calendar.YEAR, Calendar.MONTH, both
     * Calendar.DAY_OF_YEAR and Calendar.DATE, Calendar.HOUR_OF_DAY,
     * Calendar.MINUTE, Calendar.SECOND and Calendar.MILLISECOND
     * A fragment less than or equal to a SECOND field will return 0.</p>
     *
     * <ul>
     *  <li>January 1, 2008 7:15:10.538 with Calendar.SECOND as fragment will return 538</li>
     *  <li>January 6, 2008 7:15:10.538 with Calendar.SECOND as fragment will return 538</li>
     *  <li>January 6, 2008 7:15:10.538 with Calendar.MINUTE as fragment will return 10538 (10*1000 + 538)</li>
     *  <li>January 16, 2008 7:15:10.538 with Calendar.MILLISECOND as fragment will return 0
     *   (a millisecond cannot be split in milliseconds)</li>
     * </ul>
     *
     * @param date the date to work with, not null
     * @param fragment the {@link Calendar} field part of date to calculate
     * @return number of milliseconds within the fragment of date
     * @throws NullPointerException if the date is {@code null}
     * @throws IllegalArgumentException if the fragment is not supported
     * @since 2.4
     */
    public static long getFragmentInMilliseconds(final Date date, final int fragment) {
        return getFragment(date, fragment, TimeUnit.MILLISECONDS);
    }

    /**
     * Returns the number of seconds within the
     * fragment. All datefields greater than the fragment will be ignored.
     *
     * <p>Asking the seconds of any date will only return the number of seconds
     * of the current minute (resulting in a number between 0 and 59). This
     * method will retrieve the number of seconds for any fragment.
     * For example, if you want to calculate the number of seconds past today,
     * your fragment is Calendar.DATE or Calendar.DAY_OF_YEAR. The result will
     * be all seconds of the past hour(s) and minutes(s).</p>
     *
     * <p>Valid fragments are: Calendar.YEAR, Calendar.MONTH, both
     * Calendar.DAY_OF_YEAR and Calendar.DATE, Calendar.HOUR_OF_DAY,
     * Calendar.MINUTE, Calendar.SECOND and Calendar.MILLISECOND
     * A fragment less than or equal to a SECOND field will return 0.</p>
     *
     * <ul>
     *  <li>January 1, 2008 7:15:10.538 with Calendar.MINUTE as fragment will return 10
     *   (equivalent to deprecated date.getSeconds())</li>
     *  <li>January 6, 2008 7:15:10.538 with Calendar.MINUTE as fragment will return 10
     *   (equivalent to deprecated date.getSeconds())</li>
     *  <li>January 6, 2008 7:15:10.538 with Calendar.DAY_OF_YEAR as fragment will return 26110
     *   (7*3600 + 15*60 + 10)</li>
     *  <li>January 16, 2008 7:15:10.538 with Calendar.MILLISECOND as fragment will return 0
     *   (a millisecond cannot be split in seconds)</li>
     * </ul>
     *
     * @param date the date to work with, not null
     * @param fragment the {@link Calendar} field part of date to calculate
     * @return number of seconds within the fragment of date
     * @throws NullPointerException if the date is {@code null}
     * @throws IllegalArgumentException if the fragment is not supported
     * @since 2.4
     */
    public static long getFragmentInSeconds(final Date date, final int fragment) {
        return getFragment(date, fragment, TimeUnit.SECONDS);
    }

    /**
     * Returns the number of minutes within the
     * fragment. All datefields greater than the fragment will be ignored.
     *
     * <p>Asking the minutes of any date will only return the number of minutes
     * of the current hour (resulting in a number between 0 and 59). This
     * method will retrieve the number of minutes for any fragment.
     * For example, if you want to calculate the number of minutes past this month,
     * your fragment is Calendar.MONTH. The result will be all minutes of the
     * past day(s) and hour(s).</p>
     *
     * <p>Valid fragments are: Calendar.YEAR, Calendar.MONTH, both
     * Calendar.DAY_OF_YEAR and Calendar.DATE, Calendar.HOUR_OF_DAY,
     * Calendar.MINUTE, Calendar.SECOND and Calendar.MILLISECOND
     * A fragment less than or equal to a MINUTE field will return 0.</p>
     *
     * <ul>
     *  <li>January 1, 2008 7:15:10.538 with Calendar.HOUR_OF_DAY as fragment will return 15
     *   (equivalent to deprecated date.getMinutes())</li>
     *  <li>January 6, 2008 7:15:10.538 with Calendar.HOUR_OF_DAY as fragment will return 15
     *   (equivalent to deprecated date.getMinutes())</li>
     *  <li>January 1, 2008 7:15:10.538 with Calendar.MONTH as fragment will return 15</li>
     *  <li>January 6, 2008 7:15:10.538 with Calendar.MONTH as fragment will return 435 (7*60 + 15)</li>
     *  <li>January 16, 2008 7:15:10.538 with Calendar.MILLISECOND as fragment will return 0
     *   (a millisecond cannot be split in minutes)</li>
     * </ul>
     *
     * @param date the date to work with, not null
     * @param fragment the {@link Calendar} field part of date to calculate
     * @return number of minutes within the fragment of date
     * @throws NullPointerException if the date is {@code null}
     * @throws IllegalArgumentException if the fragment is not supported
     * @since 2.4
     */
    public static long getFragmentInMinutes(final Date date, final int fragment) {
        return getFragment(date, fragment, TimeUnit.MINUTES);
    }

    /**
     * Returns the number of hours within the
     * fragment. All datefields greater than the fragment will be ignored.
     *
     * <p>Asking the hours of any date will only return the number of hours
     * of the current day (resulting in a number between 0 and 23). This
     * method will retrieve the number of hours for any fragment.
     * For example, if you want to calculate the number of hours past this month,
     * your fragment is Calendar.MONTH. The result will be all hours of the
     * past day(s).</p>
     *
     * <p>Valid fragments are: Calendar.YEAR, Calendar.MONTH, both
     * Calendar.DAY_OF_YEAR and Calendar.DATE, Calendar.HOUR_OF_DAY,
     * Calendar.MINUTE, Calendar.SECOND and Calendar.MILLISECOND
     * A fragment less than or equal to a HOUR field will return 0.</p>
     *
     * <ul>
     *  <li>January 1, 2008 7:15:10.538 with Calendar.DAY_OF_YEAR as fragment will return 7
     *   (equivalent to deprecated date.getHours())</li>
     *  <li>January 6, 2008 7:15:10.538 with Calendar.DAY_OF_YEAR as fragment will return 7
     *   (equivalent to deprecated date.getHours())</li>
     *  <li>January 1, 2008 7:15:10.538 with Calendar.MONTH as fragment will return 7</li>
     *  <li>January 6, 2008 7:15:10.538 with Calendar.MONTH as fragment will return 127 (5*24 + 7)</li>
     *  <li>January 16, 2008 7:15:10.538 with Calendar.MILLISECOND as fragment will return 0
     *   (a millisecond cannot be split in hours)</li>
     * </ul>
     *
     * @param date the date to work with, not null
     * @param fragment the {@link Calendar} field part of date to calculate
     * @return number of hours within the fragment of date
     * @throws NullPointerException if the date is {@code null}
     * @throws IllegalArgumentException if the fragment is not supported
     * @since 2.4
     */
    public static long getFragmentInHours(final Date date, final int fragment) {
        return getFragment(date, fragment, TimeUnit.HOURS);
    }

    /**
     * Returns the number of days within the
     * fragment. All datefields greater than the fragment will be ignored.
     *
     * <p>Asking the days of any date will only return the number of days
     * of the current month (resulting in a number between 1 and 31). This
     * method will retrieve the number of days for any fragment.
     * For example, if you want to calculate the number of days past this year,
     * your fragment is Calendar.YEAR. The result will be all days of the
     * past month(s).</p>
     *
     * <p>Valid fragments are: Calendar.YEAR, Calendar.MONTH, both
     * Calendar.DAY_OF_YEAR and Calendar.DATE, Calendar.HOUR_OF_DAY,
     * Calendar.MINUTE, Calendar.SECOND and Calendar.MILLISECOND
     * A fragment less than or equal to a DAY field will return 0.</p>
     *
     * <ul>
     *  <li>January 28, 2008 with Calendar.MONTH as fragment will return 28
     *   (equivalent to deprecated date.getDay())</li>
     *  <li>February 28, 2008 with Calendar.MONTH as fragment will return 28
     *   (equivalent to deprecated date.getDay())</li>
     *  <li>January 28, 2008 with Calendar.YEAR as fragment will return 28</li>
     *  <li>February 28, 2008 with Calendar.YEAR as fragment will return 59</li>
     *  <li>January 28, 2008 with Calendar.MILLISECOND as fragment will return 0
     *   (a millisecond cannot be split in days)</li>
     * </ul>
     *
     * @param date the date to work with, not null
     * @param fragment the {@link Calendar} field part of date to calculate
     * @return number of days  within the fragment of date
     * @throws NullPointerException if the date is {@code null}
     * @throws IllegalArgumentException if the fragment is not supported
     * @since 2.4
     */
    public static long getFragmentInDays(final Date date, final int fragment) {
        return getFragment(date, fragment, TimeUnit.DAYS);
    }

    /**
     * Returns the number of milliseconds within the
     * fragment. All datefields greater than the fragment will be ignored.
     *
     * <p>Asking the milliseconds of any date will only return the number of milliseconds
     * of the current second (resulting in a number between 0 and 999). This
     * method will retrieve the number of milliseconds for any fragment.
     * For example, if you want to calculate the number of seconds past today,
     * your fragment is Calendar.DATE or Calendar.DAY_OF_YEAR. The result will
     * be all seconds of the past hour(s), minutes(s) and second(s).</p>
     *
     * <p>Valid fragments are: Calendar.YEAR, Calendar.MONTH, both
     * Calendar.DAY_OF_YEAR and Calendar.DATE, Calendar.HOUR_OF_DAY,
     * Calendar.MINUTE, Calendar.SECOND and Calendar.MILLISECOND
     * A fragment less than or equal to a MILLISECOND field will return 0.</p>
     *
     * <ul>
     *  <li>January 1, 2008 7:15:10.538 with Calendar.SECOND as fragment will return 538
     *   (equivalent to calendar.get(Calendar.MILLISECOND))</li>
     *  <li>January 6, 2008 7:15:10.538 with Calendar.SECOND as fragment will return 538
     *   (equivalent to calendar.get(Calendar.MILLISECOND))</li>
     *  <li>January 6, 2008 7:15:10.538 with Calendar.MINUTE as fragment will return 10538
     *   (10*1000 + 538)</li>
     *  <li>January 16, 2008 7:15:10.538 with Calendar.MILLISECOND as fragment will return 0
     *   (a millisecond cannot be split in milliseconds)</li>
     * </ul>
     *
     * @param calendar the calendar to work with, not null
     * @param fragment the {@link Calendar} field part of calendar to calculate
     * @return number of milliseconds within the fragment of date
     * @throws NullPointerException if the date is {@code null} or
     * fragment is not supported
     * @since 2.4
     */
  public static long getFragmentInMilliseconds(final Calendar calendar, final int fragment) {
    return getFragment(calendar, fragment, TimeUnit.MILLISECONDS);
  }
    /**
     * Returns the number of seconds within the
     * fragment. All datefields greater than the fragment will be ignored.
     *
     * <p>Asking the seconds of any date will only return the number of seconds
     * of the current minute (resulting in a number between 0 and 59). This
     * method will retrieve the number of seconds for any fragment.
     * For example, if you want to calculate the number of seconds past today,
     * your fragment is Calendar.DATE or Calendar.DAY_OF_YEAR. The result will
     * be all seconds of the past hour(s) and minutes(s).</p>
     *
     * <p>Valid fragments are: Calendar.YEAR, Calendar.MONTH, both
     * Calendar.DAY_OF_YEAR and Calendar.DATE, Calendar.HOUR_OF_DAY,
     * Calendar.MINUTE, Calendar.SECOND and Calendar.MILLISECOND
     * A fragment less than or equal to a SECOND field will return 0.</p>
     *
     * <ul>
     *  <li>January 1, 2008 7:15:10.538 with Calendar.MINUTE as fragment will return 10
     *   (equivalent to calendar.get(Calendar.SECOND))</li>
     *  <li>January 6, 2008 7:15:10.538 with Calendar.MINUTE as fragment will return 10
     *   (equivalent to calendar.get(Calendar.SECOND))</li>
     *  <li>January 6, 2008 7:15:10.538 with Calendar.DAY_OF_YEAR as fragment will return 26110
     *   (7*3600 + 15*60 + 10)</li>
     *  <li>January 16, 2008 7:15:10.538 with Calendar.MILLISECOND as fragment will return 0
     *   (a millisecond cannot be split in seconds)</li>
     * </ul>
     *
     * @param calendar the calendar to work with, not null
     * @param fragment the {@link Calendar} field part of calendar to calculate
     * @return number of seconds within the fragment of date
     * @throws NullPointerException if the date is {@code null} or
     * fragment is not supported
     * @since 2.4
     */
    public static long getFragmentInSeconds(final Calendar calendar, final int fragment) {
        return getFragment(calendar, fragment, TimeUnit.SECONDS);
    }

    /**
     * Returns the number of minutes within the
     * fragment. All datefields greater than the fragment will be ignored.
     *
     * <p>Asking the minutes of any date will only return the number of minutes
     * of the current hour (resulting in a number between 0 and 59). This
     * method will retrieve the number of minutes for any fragment.
     * For example, if you want to calculate the number of minutes past this month,
     * your fragment is Calendar.MONTH. The result will be all minutes of the
     * past day(s) and hour(s).</p>
     *
     * <p>Valid fragments are: Calendar.YEAR, Calendar.MONTH, both
     * Calendar.DAY_OF_YEAR and Calendar.DATE, Calendar.HOUR_OF_DAY,
     * Calendar.MINUTE, Calendar.SECOND and Calendar.MILLISECOND
     * A fragment less than or equal to a MINUTE field will return 0.</p>
     *
     * <ul>
     *  <li>January 1, 2008 7:15:10.538 with Calendar.HOUR_OF_DAY as fragment will return 15
     *   (equivalent to calendar.get(Calendar.MINUTES))</li>
     *  <li>January 6, 2008 7:15:10.538 with Calendar.HOUR_OF_DAY as fragment will return 15
     *   (equivalent to calendar.get(Calendar.MINUTES))</li>
     *  <li>January 1, 2008 7:15:10.538 with Calendar.MONTH as fragment will return 15</li>
     *  <li>January 6, 2008 7:15:10.538 with Calendar.MONTH as fragment will return 435 (7*60 + 15)</li>
     *  <li>January 16, 2008 7:15:10.538 with Calendar.MILLISECOND as fragment will return 0
     *   (a millisecond cannot be split in minutes)</li>
     * </ul>
     *
     * @param calendar the calendar to work with, not null
     * @param fragment the {@link Calendar} field part of calendar to calculate
     * @return number of minutes within the fragment of date
     * @throws NullPointerException if the date is {@code null} or
     * fragment is not supported
     * @since 2.4
     */
    public static long getFragmentInMinutes(final Calendar calendar, final int fragment) {
        return getFragment(calendar, fragment, TimeUnit.MINUTES);
    }

    /**
     * Returns the number of hours within the
     * fragment. All datefields greater than the fragment will be ignored.
     *
     * <p>Asking the hours of any date will only return the number of hours
     * of the current day (resulting in a number between 0 and 23). This
     * method will retrieve the number of hours for any fragment.
     * For example, if you want to calculate the number of hours past this month,
     * your fragment is Calendar.MONTH. The result will be all hours of the
     * past day(s).</p>
     *
     * <p>Valid fragments are: Calendar.YEAR, Calendar.MONTH, both
     * Calendar.DAY_OF_YEAR and Calendar.DATE, Calendar.HOUR_OF_DAY,
     * Calendar.MINUTE, Calendar.SECOND and Calendar.MILLISECOND
     * A fragment less than or equal to a HOUR field will return 0.</p>
     *
     * <ul>
     *  <li>January 1, 2008 7:15:10.538 with Calendar.DAY_OF_YEAR as fragment will return 7
     *   (equivalent to calendar.get(Calendar.HOUR_OF_DAY))</li>
     *  <li>January 6, 2008 7:15:10.538 with Calendar.DAY_OF_YEAR as fragment will return 7
     *   (equivalent to calendar.get(Calendar.HOUR_OF_DAY))</li>
     *  <li>January 1, 2008 7:15:10.538 with Calendar.MONTH as fragment will return 7</li>
     *  <li>January 6, 2008 7:15:10.538 with Calendar.MONTH as fragment will return 127 (5*24 + 7)</li>
     *  <li>January 16, 2008 7:15:10.538 with Calendar.MILLISECOND as fragment will return 0
     *   (a millisecond cannot be split in hours)</li>
     * </ul>
     *
     * @param calendar the calendar to work with, not null
     * @param fragment the {@link Calendar} field part of calendar to calculate
     * @return number of hours within the fragment of date
     * @throws NullPointerException if the date is {@code null} or
     * fragment is not supported
     * @since 2.4
     */
    public static long getFragmentInHours(final Calendar calendar, final int fragment) {
        return getFragment(calendar, fragment, TimeUnit.HOURS);
    }

    /**
     * Returns the number of days within the
     * fragment. All datefields greater than the fragment will be ignored.
     *
     * <p>Asking the days of any date will only return the number of days
     * of the current month (resulting in a number between 1 and 31). This
     * method will retrieve the number of days for any fragment.
     * For example, if you want to calculate the number of days past this year,
     * your fragment is Calendar.YEAR. The result will be all days of the
     * past month(s).</p>
     *
     * <p>Valid fragments are: Calendar.YEAR, Calendar.MONTH, both
     * Calendar.DAY_OF_YEAR and Calendar.DATE, Calendar.HOUR_OF_DAY,
     * Calendar.MINUTE, Calendar.SECOND and Calendar.MILLISECOND
     * A fragment less than or equal to a DAY field will return 0.</p>
     *
     * <ul>
     *  <li>January 28, 2008 with Calendar.MONTH as fragment will return 28
     *   (equivalent to calendar.get(Calendar.DAY_OF_MONTH))</li>
     *  <li>February 28, 2008 with Calendar.MONTH as fragment will return 28
     *   (equivalent to calendar.get(Calendar.DAY_OF_MONTH))</li>
     *  <li>January 28, 2008 with Calendar.YEAR as fragment will return 28
     *   (equivalent to calendar.get(Calendar.DAY_OF_YEAR))</li>
     *  <li>February 28, 2008 with Calendar.YEAR as fragment will return 59
     *   (equivalent to calendar.get(Calendar.DAY_OF_YEAR))</li>
     *  <li>January 28, 2008 with Calendar.MILLISECOND as fragment will return 0
     *   (a millisecond cannot be split in days)</li>
     * </ul>
     *
     * @param calendar the calendar to work with, not null
     * @param fragment the {@link Calendar} field part of calendar to calculate
     * @return number of days within the fragment of date
     * @throws NullPointerException if the date is {@code null} or
     * fragment is not supported
     * @since 2.4
     */
    public static long getFragmentInDays(final Calendar calendar, final int fragment) {
        return getFragment(calendar, fragment, TimeUnit.DAYS);
    }

    /**
     * Gets a Date fragment for any unit.
     *
     * @param date the date to work with, not null
     * @param fragment the Calendar field part of date to calculate
     * @param unit the time unit
     * @return number of units within the fragment of the date
     * @throws NullPointerException if the date is {@code null}
     * @throws IllegalArgumentException if fragment is not supported
     * @since 2.4
     */
    private static long getFragment(final Date date, final int fragment, final TimeUnit unit) {
        validateDateNotNull(date);
        final Calendar calendar = Calendar.getInstance();
        calendar.setTime(date);
        return getFragment(calendar, fragment, unit);
    }

    /**
     * Gets a Calendar fragment for any unit.
     *
     * @param calendar the calendar to work with, not null
     * @param fragment the Calendar field part of calendar to calculate
     * @param unit the time unit
     * @return number of units within the fragment of the calendar
     * @throws NullPointerException if the date is {@code null} or
     * fragment is not supported
     * @since 2.4
     */
    private static long getFragment(final Calendar calendar, final int fragment, final TimeUnit unit) {
        Objects.requireNonNull(calendar, "calendar");
        long result = 0;
        final int offset = (unit == TimeUnit.DAYS) ? 0 : 1;

        // Fragments bigger than a day require a breakdown to days
        switch (fragment) {
            case Calendar.YEAR:
                result += unit.convert(calendar.get(Calendar.DAY_OF_YEAR) - offset, TimeUnit.DAYS);
                break;
            case Calendar.MONTH:
                result += unit.convert(calendar.get(Calendar.DAY_OF_MONTH) - offset, TimeUnit.DAYS);
                break;
            default:
                break;
        }

        switch (fragment) {
            // Number of days already calculated for these cases
            case Calendar.YEAR:
            case Calendar.MONTH:

            // The rest of the valid cases
            case Calendar.DAY_OF_YEAR:
            case Calendar.DATE:
                result += unit.convert(calendar.get(Calendar.HOUR_OF_DAY), TimeUnit.HOURS);
                //$FALL-THROUGH$
            case Calendar.HOUR_OF_DAY:
                result += unit.convert(calendar.get(Calendar.MINUTE), TimeUnit.MINUTES);
                //$FALL-THROUGH$
            case Calendar.MINUTE:
                result += unit.convert(calendar.get(Calendar.SECOND), TimeUnit.SECONDS);
                //$FALL-THROUGH$
            case Calendar.SECOND:
                result += unit.convert(calendar.get(Calendar.MILLISECOND), TimeUnit.MILLISECONDS);
                break;
            case Calendar.MILLISECOND: break; //never useful
                default: throw new IllegalArgumentException("The fragment " + fragment + " is not supported");
        }
        return result;
    }

    /**
     * Determines if two calendars are equal up to no more than the specified
     * most significant field.
     *
     * @param cal1 the first calendar, not {@code null}
     * @param cal2 the second calendar, not {@code null}
     * @param field the field from {@link Calendar}
     * @return {@code true} if equal; otherwise {@code false}
     * @throws NullPointerException if any argument is {@code null}
     * @see #truncate(Calendar, int)
     * @see #truncatedEquals(Date, Date, int)
     * @since 3.0
     */
    public static boolean truncatedEquals(final Calendar cal1, final Calendar cal2, final int field) {
        return truncatedCompareTo(cal1, cal2, field) == 0;
    }

    /**
     * Determines if two dates are equal up to no more than the specified
     * most significant field.
     *
     * @param date1 the first date, not {@code null}
     * @param date2 the second date, not {@code null}
     * @param field the field from {@link Calendar}
     * @return {@code true} if equal; otherwise {@code false}
     * @throws NullPointerException if any argument is {@code null}
     * @see #truncate(Date, int)
     * @see #truncatedEquals(Calendar, Calendar, int)
     * @since 3.0
     */
    public static boolean truncatedEquals(final Date date1, final Date date2, final int field) {
        return truncatedCompareTo(date1, date2, field) == 0;
    }

    /**
     * Determines how two calendars compare up to no more than the specified
     * most significant field.
     *
     * @param cal1 the first calendar, not {@code null}
     * @param cal2 the second calendar, not {@code null}
     * @param field the field from {@link Calendar}
     * @return a negative integer, zero, or a positive integer as the first
     * calendar is less than, equal to, or greater than the second.
     * @throws NullPointerException if any argument is {@code null}
     * @see #truncate(Calendar, int)
     * @see #truncatedCompareTo(Date, Date, int)
     * @since 3.0
     */
    public static int truncatedCompareTo(final Calendar cal1, final Calendar cal2, final int field) {
        final Calendar truncatedCal1 = truncate(cal1, field);
        final Calendar truncatedCal2 = truncate(cal2, field);
        return truncatedCal1.compareTo(truncatedCal2);
    }

    /**
     * Determines how two dates compare up to no more than the specified
     * most significant field.
     *
     * @param date1 the first date, not {@code null}
     * @param date2 the second date, not {@code null}
     * @param field the field from {@link Calendar}
     * @return a negative integer, zero, or a positive integer as the first
     * date is less than, equal to, or greater than the second.
     * @throws NullPointerException if any argument is {@code null}
     * @see #truncate(Calendar, int)
     * @see #truncatedCompareTo(Date, Date, int)
     * @since 3.0
     */
    public static int truncatedCompareTo(final Date date1, final Date date2, final int field) {
        final Date truncatedDate1 = truncate(date1, field);
        final Date truncatedDate2 = truncate(date2, field);
        return truncatedDate1.compareTo(truncatedDate2);
    }

    /**
     * @param date Date to validate.
     * @throws NullPointerException if {@code date == null}
     */
    private static void validateDateNotNull(final Date date) {
        Objects.requireNonNull(date, "date");
    }

    /**
     * Date iterator.
     */
    static class DateIterator implements Iterator<Calendar> {
        private final Calendar endFinal;
        private final Calendar spot;

        /**
         * Constructs a DateIterator that ranges from one date to another.
         *
         * @param startFinal start date (inclusive)
         * @param endFinal end date (inclusive)
         */
        DateIterator(final Calendar startFinal, final Calendar endFinal) {
            this.endFinal = endFinal;
            spot = startFinal;
            spot.add(Calendar.DATE, -1);
        }

        /**
         * Has the iterator not reached the end date yet?
         *
         * @return {@code true} if the iterator has yet to reach the end date
         */
        @Override
        public boolean hasNext() {
            return spot.before(endFinal);
        }

        /**
         * Returns the next calendar in the iteration
         *
         * @return Object calendar for the next date
         */
        @Override
        public Calendar next() {
            if (spot.equals(endFinal)) {
                throw new NoSuchElementException();
            }
            spot.add(Calendar.DATE, 1);
            return (Calendar) spot.clone();
        }

        /**
         * Always throws UnsupportedOperationException.
         *
         * @throws UnsupportedOperationException Always thrown.
         * @see java.util.Iterator#remove()
         */
        @Override
        public void remove() {
            throw new UnsupportedOperationException();
        }
    }

}
