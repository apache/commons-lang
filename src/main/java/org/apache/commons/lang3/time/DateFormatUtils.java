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

import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

/**
 * Date and time formatting utilities and constants.
 *
 * <p>Formatting is performed using the thread-safe
 * {@link org.apache.commons.lang3.time.FastDateFormat} class.</p>
 *
 * <p>Note that the JDK has a bug wherein calling Calendar.get(int) will
 * override any previously called Calendar.clear() calls. See LANG-755.</p>
 *
 * <p>Note that when using capital YYYY instead of lowercase yyyy, the formatter
 * will assume current year as week year is not supported. See {@link java.util.GregorianCalendar}
 * Week Year section for an explanation on the difference between calendar and week years.</p>
 *
 * @since 2.0
 */
public class DateFormatUtils {

    /**
     * The UTC time zone (often referred to as GMT).
     * This is private as it is mutable.
     */
    private static final TimeZone UTC_TIME_ZONE = FastTimeZone.getGmtTimeZone();

    /**
     * ISO 8601 formatter for date-time without time zone.
     *
     * <p>
     * The format used is {@code yyyy-MM-dd'T'HH:mm:ss}. This format uses the
     * default TimeZone in effect at the time of loading DateFormatUtils class.
     * </p>
     *
     * @since 3.5
     */
    public static final FastDateFormat ISO_8601_EXTENDED_DATETIME_FORMAT
            = FastDateFormat.getInstance("yyyy-MM-dd'T'HH:mm:ss");

    /**
     * @deprecated - as of 4.0, ISO_DATETIME_FORMAT will be replaced by ISO_8601_EXTENDED_DATETIME_FORMAT.
     */
    @Deprecated
    public static final FastDateFormat ISO_DATETIME_FORMAT = ISO_8601_EXTENDED_DATETIME_FORMAT;

    /**
     * ISO 8601 formatter for date-time with time zone.
     *
     * <p>
     * The format used is {@code yyyy-MM-dd'T'HH:mm:ssZZ}. This format uses the
     * default TimeZone in effect at the time of loading DateFormatUtils class.
     * </p>
     *
     * @since 3.5
     */
    public static final FastDateFormat ISO_8601_EXTENDED_DATETIME_TIME_ZONE_FORMAT
            = FastDateFormat.getInstance("yyyy-MM-dd'T'HH:mm:ssZZ");

    /**
     * @deprecated - as of 4.0, ISO_DATETIME_TIME_ZONE_FORMAT will be replaced by ISO_8601_EXTENDED_DATETIME_TIME_ZONE_FORMAT.
     */
    @Deprecated
    public static final FastDateFormat ISO_DATETIME_TIME_ZONE_FORMAT = ISO_8601_EXTENDED_DATETIME_TIME_ZONE_FORMAT;

    /**
     * ISO 8601 formatter for date without time zone.
     *
     * <p>
     * The format used is {@code yyyy-MM-dd}. This format uses the
     * default TimeZone in effect at the time of loading DateFormatUtils class.
     * </p>
     *
     * @since 3.5
     */
    public static final FastDateFormat ISO_8601_EXTENDED_DATE_FORMAT
            = FastDateFormat.getInstance("yyyy-MM-dd");

    /**
     * @deprecated - as of 4.0, ISO_DATE_FORMAT will be replaced by ISO_8601_EXTENDED_DATE_FORMAT.
     */
    @Deprecated
    public static final FastDateFormat ISO_DATE_FORMAT = ISO_8601_EXTENDED_DATE_FORMAT;

    /**
     * ISO 8601-like formatter for date with time zone.
     *
     * <p>
     * The format used is {@code yyyy-MM-ddZZ}. This pattern does not comply
     * with the formal ISO 8601 specification as the standard does not allow
     * a time zone  without a time. This format uses the default TimeZone in
     * effect at the time of loading DateFormatUtils class.
     * </p>
     *
     * @deprecated - as of 4.0, ISO_DATE_TIME_ZONE_FORMAT will be removed.
     */
    @Deprecated
    public static final FastDateFormat ISO_DATE_TIME_ZONE_FORMAT
            = FastDateFormat.getInstance("yyyy-MM-ddZZ");

    /**
     * Non-compliant formatter for time without time zone (ISO 8601 does not
     * prefix 'T' for standalone time value).
     *
     * <p>
     * The format used is {@code 'T'HH:mm:ss}. This format uses the default
     * TimeZone in effect at the time of loading DateFormatUtils class.
     * </p>
     *
     * @deprecated - as of 4.0, ISO_TIME_FORMAT will be removed.
     */
    @Deprecated
    public static final FastDateFormat ISO_TIME_FORMAT
            = FastDateFormat.getInstance("'T'HH:mm:ss");

    /**
     * Non-compliant formatter for time with time zone (ISO 8601 does not
     * prefix 'T' for standalone time value).
     *
     * <p>
     * The format used is {@code 'T'HH:mm:ssZZ}. This format uses the default
     * TimeZone in effect at the time of loading DateFormatUtils class.
     * </p>
     *
     * @deprecated - as of 4.0, ISO_TIME_TIME_ZONE_FORMAT will be removed.
     */
    @Deprecated
    public static final FastDateFormat ISO_TIME_TIME_ZONE_FORMAT
            = FastDateFormat.getInstance("'T'HH:mm:ssZZ");

    /**
     * ISO 8601 formatter for time without time zone.
     *
     * <p>
     * The format used is {@code HH:mm:ss}. This format uses the default
     * TimeZone in effect at the time of loading DateFormatUtils class.
     * </p>
     *
     * @since 3.5
     */
    public static final FastDateFormat ISO_8601_EXTENDED_TIME_FORMAT
            = FastDateFormat.getInstance("HH:mm:ss");

    /**
     * @deprecated - as of 4.0, ISO_TIME_NO_T_FORMAT will be replaced by ISO_8601_EXTENDED_TIME_FORMAT.
     */
    @Deprecated
    public static final FastDateFormat ISO_TIME_NO_T_FORMAT = ISO_8601_EXTENDED_TIME_FORMAT;

    /**
     * ISO 8601 formatter for time with time zone.
     *
     * <p>
     * The format used is {@code HH:mm:ssZZ}. This format uses the default
     * TimeZone in effect at the time of loading DateFormatUtils class.
     * </p>
     *
     * @since 3.5
     */
    public static final FastDateFormat ISO_8601_EXTENDED_TIME_TIME_ZONE_FORMAT
            = FastDateFormat.getInstance("HH:mm:ssZZ");

    /**
     * @deprecated - as of 4.0, ISO_TIME_NO_T_TIME_ZONE_FORMAT will be replaced by ISO_8601_EXTENDED_TIME_TIME_ZONE_FORMAT.
     */
    @Deprecated
    public static final FastDateFormat ISO_TIME_NO_T_TIME_ZONE_FORMAT = ISO_8601_EXTENDED_TIME_TIME_ZONE_FORMAT;

    /**
     * SMTP (and probably other) date headers.
     *
     * <p>
     * The format used is {@code EEE, dd MMM yyyy HH:mm:ss Z} in US locale.
     * This format uses the default TimeZone in effect at the time of loading
     * DateFormatUtils class.
     * </p>
     */
    public static final FastDateFormat SMTP_DATETIME_FORMAT
            = FastDateFormat.getInstance("EEE, dd MMM yyyy HH:mm:ss Z", Locale.US);

    /**
     * DateFormatUtils instances should NOT be constructed in standard programming.
     *
     * <p>This constructor is public to permit tools that require a JavaBean instance
     * to operate.</p>
     */
    public DateFormatUtils() {
    }

    /**
     * Formats a calendar into a specific pattern. The TimeZone from the calendar
     * will be used for formatting.
     *
     * @param calendar  the calendar to format, not null
     * @param pattern  the pattern to use to format the calendar, not null
     * @return the formatted calendar
     * @see FastDateFormat#format(Calendar)
     * @since 2.4
     */
    public static String format(final Calendar calendar, final String pattern) {
        return format(calendar, pattern, getTimeZone(calendar), null);
    }

    /**
     * Formats a calendar into a specific pattern in a locale. The TimeZone from the calendar
     * will be used for formatting.
     *
     * @param calendar  the calendar to format, not null
     * @param pattern  the pattern to use to format the calendar, not null
     * @param locale  the locale to use, may be {@code null}
     * @return the formatted calendar
     * @see FastDateFormat#format(Calendar)
     * @since 2.4
     */
    public static String format(final Calendar calendar, final String pattern, final Locale locale) {
        return format(calendar, pattern, getTimeZone(calendar), locale);
    }

    /**
     * Formats a calendar into a specific pattern in a time zone.
     *
     * @param calendar  the calendar to format, not null
     * @param pattern  the pattern to use to format the calendar, not null
     * @param timeZone  the time zone  to use, may be {@code null}
     * @return the formatted calendar
     * @see FastDateFormat#format(Calendar)
     * @since 2.4
     */
    public static String format(final Calendar calendar, final String pattern, final TimeZone timeZone) {
        return format(calendar, pattern, timeZone, null);
    }

    /**
     * Formats a calendar into a specific pattern in a time zone and locale.
     *
     * @param calendar  the calendar to format, not null
     * @param pattern  the pattern to use to format the calendar, not null
     * @param timeZone  the time zone  to use, may be {@code null}
     * @param locale  the locale to use, may be {@code null}
     * @return the formatted calendar
     * @see FastDateFormat#format(Calendar)
     * @since 2.4
     */
    public static String format(final Calendar calendar, final String pattern, final TimeZone timeZone, final Locale locale) {
        final FastDateFormat df = FastDateFormat.getInstance(pattern, timeZone, locale);
        return df.format(calendar);
    }

    /**
     * Formats a date/time into a specific pattern.
     *
     * @param date  the date to format, not null
     * @param pattern  the pattern to use to format the date, not null
     * @return the formatted date
     */
    public static String format(final Date date, final String pattern) {
        return format(date, pattern, null, null);
    }

    /**
     * Formats a date/time into a specific pattern in a locale.
     *
     * @param date  the date to format, not null
     * @param pattern  the pattern to use to format the date, not null
     * @param locale  the locale to use, may be {@code null}
     * @return the formatted date
     */
    public static String format(final Date date, final String pattern, final Locale locale) {
        return format(date, pattern, null, locale);
    }

    /**
     * Formats a date/time into a specific pattern in a time zone.
     *
     * @param date  the date to format, not null
     * @param pattern  the pattern to use to format the date, not null
     * @param timeZone  the time zone  to use, may be {@code null}
     * @return the formatted date
     */
    public static String format(final Date date, final String pattern, final TimeZone timeZone) {
        return format(date, pattern, timeZone, null);
    }

    /**
     * Formats a date/time into a specific pattern in a time zone and locale.
     *
     * @param date  the date to format, not null
     * @param pattern  the pattern to use to format the date, not null, not null
     * @param timeZone  the time zone  to use, may be {@code null}
     * @param locale  the locale to use, may be {@code null}
     * @return the formatted date
     */
    public static String format(final Date date, final String pattern, final TimeZone timeZone, final Locale locale) {
        final FastDateFormat df = FastDateFormat.getInstance(pattern, timeZone, locale);
        return df.format(date);
    }

    /**
     * Formats a date/time into a specific pattern.
     *
     * @param millis  the date to format expressed in milliseconds
     * @param pattern  the pattern to use to format the date, not null
     * @return the formatted date
     */
    public static String format(final long millis, final String pattern) {
        return format(new Date(millis), pattern, null, null);
    }

    /**
     * Formats a date/time into a specific pattern in a locale.
     *
     * @param millis  the date to format expressed in milliseconds
     * @param pattern  the pattern to use to format the date, not null
     * @param locale  the locale to use, may be {@code null}
     * @return the formatted date
     */
    public static String format(final long millis, final String pattern, final Locale locale) {
        return format(new Date(millis), pattern, null, locale);
    }

    /**
     * Formats a date/time into a specific pattern in a time zone.
     *
     * @param millis  the time expressed in milliseconds
     * @param pattern  the pattern to use to format the date, not null
     * @param timeZone  the time zone  to use, may be {@code null}
     * @return the formatted date
     */
    public static String format(final long millis, final String pattern, final TimeZone timeZone) {
        return format(new Date(millis), pattern, timeZone, null);
    }

    /**
     * Formats a date/time into a specific pattern in a time zone and locale.
     *
     * @param millis  the date to format expressed in milliseconds
     * @param pattern  the pattern to use to format the date, not null
     * @param timeZone  the time zone  to use, may be {@code null}
     * @param locale  the locale to use, may be {@code null}
     * @return the formatted date
     */
    public static String format(final long millis, final String pattern, final TimeZone timeZone, final Locale locale) {
        return format(new Date(millis), pattern, timeZone, locale);
    }

    /**
     * Formats a date/time into a specific pattern using the UTC time zone.
     *
     * @param date  the date to format, not null
     * @param pattern  the pattern to use to format the date, not null
     * @return the formatted date
     */
    public static String formatUTC(final Date date, final String pattern) {
        return format(date, pattern, UTC_TIME_ZONE, null);
    }

    /**
     * Formats a date/time into a specific pattern using the UTC time zone.
     *
     * @param date  the date to format, not null
     * @param pattern  the pattern to use to format the date, not null
     * @param locale  the locale to use, may be {@code null}
     * @return the formatted date
     */
    public static String formatUTC(final Date date, final String pattern, final Locale locale) {
        return format(date, pattern, UTC_TIME_ZONE, locale);
    }

    /**
     * Formats a date/time into a specific pattern using the UTC time zone.
     *
     * @param millis  the date to format expressed in milliseconds
     * @param pattern  the pattern to use to format the date, not null
     * @return the formatted date
     */
    public static String formatUTC(final long millis, final String pattern) {
        return format(new Date(millis), pattern, UTC_TIME_ZONE, null);
    }

    /**
     * Formats a date/time into a specific pattern using the UTC time zone.
     *
     * @param millis  the date to format expressed in milliseconds
     * @param pattern  the pattern to use to format the date, not null
     * @param locale  the locale to use, may be {@code null}
     * @return the formatted date
     */
    public static String formatUTC(final long millis, final String pattern, final Locale locale) {
        return format(new Date(millis), pattern, UTC_TIME_ZONE, locale);
    }

    private static TimeZone getTimeZone(final Calendar calendar) {
        return calendar == null ? null : calendar.getTimeZone();
    }

}
