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

import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

/**
 * Date and time formatting utilites and constants.
 * <p>
 * Formatting is performed using the
 * {@link org.apache.commons.lang.time.FastDateFormat} class.
 *
 * @author Apache Ant - DateUtils
 * @author <a href="mailto:sbailliez@apache.org">Stephane Bailliez</a>
 * @author <a href="mailto:stefan.bodewig@epost.de">Stefan Bodewig</a>
 * @author Stephen Colebourne
 * @since 2.0
 * @version $Id: DateFormatUtils.java,v 1.1 2003/06/08 23:14:23 scolebourne Exp $
 */
public final class DateFormatUtils {

    /**
     * ISO8601 formatter for date-time witout timezone.
     * The format used is <tt>yyyy-MM-dd'T'HH:mm:ss</tt>.
     */
    public static final FastDateFormat ISO_DATETIME_FORMAT
            = FastDateFormat.getInstance("yyyy-MM-dd'T'HH:mm:ss");

    /**
     * ISO8601 formatter for date-time with timezone.
     * The format used is <tt>yyyy-MM-dd'T'HH:mm:ssZZ</tt>.
     */
    public static final FastDateFormat ISO_DATETIME_TIMEZONE_FORMAT
            = FastDateFormat.getInstance("yyyy-MM-dd'T'HH:mm:ssZZ");

    /**
     * ISO8601 formatter for date without timezone.
     * The format used is <tt>yyyy-MM-dd</tt>.
     */
    public static final FastDateFormat ISO_DATE_FORMAT
            = FastDateFormat.getInstance("yyyy-MM-dd");

    /**
     * ISO8601-like formatter for date with timezone.
     * The format used is <tt>yyyy-MM-ddZZ</tt>.
     * This pattern does not comply with the formal ISO8601 specification
     * as the standard does not allow a timezone without a time.
     */
    public static final FastDateFormat ISO_DATE_TIMEZONE_FORMAT
            = FastDateFormat.getInstance("yyyy-MM-ddZZ");

    /**
     * ISO8601 formatter for time without timezone.
     * The format used is <tt>'T'HH:mm:ss</tt>.
     */
    public static final FastDateFormat ISO_TIME_FORMAT
            = FastDateFormat.getInstance("'T'HH:mm:ss");

    /**
     * ISO8601 formatter for time with timezone.
     * The format used is <tt>'T'HH:mm:ssZZ</tt>.
     */
    public static final FastDateFormat ISO_TIME_TIMEZONE_FORMAT
            = FastDateFormat.getInstance("'T'HH:mm:ssZZ");

    /**
     * ISO8601-like formatter for time without timezone.
     * The format used is <tt>HH:mm:ss</tt>.
     * This pattern does not comply with the formal ISO8601 specification
     * as the standard requires the 'T' prefix for times.
     */
    public static final FastDateFormat ISO_TIME_NO_T_FORMAT
            = FastDateFormat.getInstance("HH:mm:ss");

    /**
     * ISO8601-like formatter for time with timezone.
     * The format used is <tt>HH:mm:ssZZ</tt>.
     * This pattern does not comply with the formal ISO8601 specification
     * as the standard requires the 'T' prefix for times.
     */
    public static final FastDateFormat ISO_TIME_NO_T_TIMEZONE_FORMAT
            = FastDateFormat.getInstance("HH:mm:ssZZ");

    /**
     * SMTP (and probably other) date headers.
     * The format used is <tt>EEE, dd MMM yyyy HH:mm:ss Z</tt> in US locale.
     */
    public static final FastDateFormat SMTP_DATETIME_FORMAT
            = FastDateFormat.getInstance("EEE, dd MMM yyyy HH:mm:ss Z", Locale.US);

    //-----------------------------------------------------------------------
    /**
     * DateFormatUtils instances should NOT be constructed in standard programming.
     * <p>
     * This constructor is public to permit tools that require a JavaBean instance
     * to operate.
     */
    public DateFormatUtils() {
    }

    /**
     * Format a date/time into a specific pattern using the UTC timezone.
     * 
     * @param millis  the date to format expressed in milliseconds
     * @param pattern  the pattern to use to format the date
     * @return the formatted date
     */
    public static String formatUTC(long millis, String pattern) {
        return format(new Date(millis), pattern, DateUtils.UTC_TIMEZONE, null);
    }

    /**
     * Format a date/time into a specific pattern using the UTC timezone.
     * 
     * @param date  the date to format
     * @param pattern  the pattern to use to format the date
     * @return the formatted date
     */
    public static String formatUTC(Date date, String pattern) {
        return format(date, pattern, DateUtils.UTC_TIMEZONE, null);
    }
    
    /**
     * Format a date/time into a specific pattern using the UTC timezone.
     * 
     * @param millis  the date to format expressed in milliseconds
     * @param pattern  the pattern to use to format the date
     * @param locale  the locale to use, may be null
     * @return the formatted date
     */
    public static String formatUTC(long millis, String pattern, Locale locale) {
        return format(new Date(millis), pattern, DateUtils.UTC_TIMEZONE, locale);
    }

    /**
     * Format a date/time into a specific pattern using the UTC timezone.
     * 
     * @param date  the date to format
     * @param pattern  the pattern to use to format the date
     * @param locale  the locale to use, may be null
     * @return the formatted date
     */
    public static String formatUTC(Date date, String pattern, Locale locale) {
        return format(date, pattern, DateUtils.UTC_TIMEZONE, locale);
    }
    
    /**
     * Format a date/time into a specific pattern.
     * 
     * @param millis  the date to format expressed in milliseconds
     * @param pattern  the pattern to use to format the date
     * @return the formatted date
     */
    public static String format(long millis, String pattern) {
        return format(new Date(millis), pattern, null, null);
    }

    /**
     * Format a date/time into a specific pattern.
     * 
     * @param date  the date to format
     * @param pattern  the pattern to use to format the date
     * @return the formatted date
     */
    public static String format(Date date, String pattern) {
        return format(date, pattern, null, null);
    }
    
    /**
     * Format a date/time into a specific pattern in a timezone.
     * 
     * @param millis  the time expressed in milliseconds
     * @param pattern  the pattern to use to format the date
     * @param timeZone  the timezone to use, may be null
     * @return the formatted date
     */
    public static String format(long millis, String pattern, TimeZone timeZone) {
        return format(new Date(millis), pattern, timeZone, null);
    }

    /**
     * Format a date/time into a specific pattern in a timezone.
     * 
     * @param date  the date to format
     * @param pattern  the pattern to use to format the date
     * @param timeZone  the timezone to use, may be null
     * @return the formatted date
     */
    public static String format(Date date, String pattern, TimeZone timeZone) {
        return format(date, pattern, timeZone, null);
    }

    /**
     * Format a date/time into a specific pattern in a locale.
     * 
     * @param millis  the date to format expressed in milliseconds
     * @param pattern  the pattern to use to format the date
     * @param locale  the locale to use, may be null
     * @return the formatted date
     */
    public static String format(long millis, String pattern, Locale locale) {
        return format(new Date(millis), pattern, null, locale);
    }

    /**
     * Format a date/time into a specific pattern in a locale.
     * 
     * @param date  the date to format
     * @param pattern  the pattern to use to format the date
     * @param locale  the locale to use, may be null
     * @return the formatted date
     */
    public static String format(Date date, String pattern, Locale locale) {
        return format(date, pattern, null, locale);
    }

    /**
     * Format a date/time into a specific pattern in a timezone and locale.
     * 
     * @param millis  the date to format expressed in milliseconds
     * @param pattern  the pattern to use to format the date
     * @param timeZone  the timezone to use, may be null
     * @param locale  the locale to use, may be null
     * @return the formatted date
     */
    public static String format(long millis, String pattern, TimeZone timeZone, Locale locale) {
        return format(new Date(millis), pattern, timeZone, locale);
    }

    /**
     * Format a date/time into a specific pattern in a timezone and locale.
     * 
     * @param date  the date to format
     * @param pattern  the pattern to use to format the date
     * @param timeZone  the timezone to use, may be null
     * @param locale  the locale to use, may be null
     * @return the formatted date
     */
    public static String format(Date date, String pattern, TimeZone timeZone, Locale locale) {
        FastDateFormat df = FastDateFormat.getInstance(pattern, timeZone, locale);
        return df.format(date);
    }

}
