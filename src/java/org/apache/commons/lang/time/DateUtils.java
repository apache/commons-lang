/*
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002 The Apache Software Foundation.  All rights
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
 * 4. The names "The Jakarta Project", "Ant", and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Group.
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

import java.text.ChoiceFormat;
import java.text.DateFormat;
import java.text.MessageFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

/**
 * Helper methods to deal with date/time formatting. [Relies heavily on
 * code taken from the DateUtils class of the jakarata-ant project.]
 *
 * @author <a href="mailto:sbailliez@apache.org">Stephane Bailliez</a>
 * @author <a href="mailto:stefan.bodewig@epost.de">Stefan Bodewig</a>
 *
 * @since Lang 2.0
 *
 * @version $Revision: 1.1 $
 */
public final class DateUtils {

    /**
     * ISO8601-like pattern for date-time. It does not support timezone.
     *  <tt>yyyy-MM-ddTHH:mm:ss</tt>
     */
    public static final String ISO8601_DATETIME_PATTERN
            = "yyyy-MM-dd'T'HH:mm:ss";

    /**
     * ISO8601-like pattern for date. <tt>yyyy-MM-dd</tt>
     */
    public static final String ISO8601_DATE_PATTERN
            = "yyyy-MM-dd";

    /**
     * ISO8601-like pattern for time.  <tt>HH:mm:ss</tt>
     */
    public static final String ISO8601_TIME_PATTERN
            = "HH:mm:ss";

    /**
     * Format used for SMTP (and probably other) Date headers.
     */
    public static final DateFormat DATE_HEADER_FORMAT
        = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss ", Locale.US);


// code from Magesh moved from DefaultLogger and slightly modified
    private static final MessageFormat MINUTE_SECONDS
            = new MessageFormat("{0}{1}");

    private static final double[] LIMITS = {0, 1, 2};

    private static final String[] MINUTES_PART =
            {"", "1 minute ", "{0,number} minutes "};

    private static final String[] SECONDS_PART =
            {"0 seconds", "1 second", "{1,number} seconds"};

    private static final ChoiceFormat MINUTES_FORMAT =
            new ChoiceFormat(LIMITS, MINUTES_PART);

    private static final ChoiceFormat SECONDS_FORMAT =
            new ChoiceFormat(LIMITS, SECONDS_PART);

    static {
        MINUTE_SECONDS.setFormat(0, MINUTES_FORMAT);
        MINUTE_SECONDS.setFormat(1, SECONDS_FORMAT);
    }

    /** public constructor */
    /// TODO: Insert note that this should not be used
    public DateUtils() {
    }


    /**
     * Format a date/time into a specific pattern.
     * @param date the date to format expressed in milliseconds.
     * @param pattern the pattern to use to format the date.
     * @return the formatted date.
     */
    public static String format(long date, String pattern) {
        return format(new Date(date), pattern);
    }


    /**
     * Format a date/time into a specific pattern.
     * @param date the date to format expressed in milliseconds.
     * @param pattern the pattern to use to format the date.
     * @return the formatted date.
     */
    public static String format(Date date, String pattern) {
        DateFormat df = createDateFormat(pattern);
        return df.format(date);
    }


    /**
     * Format an elapsed time into a plurialization correct string.
     * It is limited only to report elapsed time in minutes and
     * seconds and has the following behavior.
     * <ul>
     * <li>minutes are not displayed when 0. (ie: "45 seconds")</li>
     * <li>seconds are always displayed in plural form (ie "0 seconds" or
     * "10 seconds") except for 1 (ie "1 second")</li>
     * </ul>
     * @param time the elapsed time to report in milliseconds.
     * @return the formatted text in minutes/seconds.
     */
    public static String formatElapsedTime(long millis) {
        long seconds = millis / 1000;
        long minutes = seconds / 60;
        Object[] args = {new Long(minutes), new Long(seconds % 60)};
        return MINUTE_SECONDS.format(args);
    }

    /**
     * return a lenient date format set to GMT time zone.
     * @param pattern the pattern used for date/time formatting.
     * @return the configured format for this pattern.
     */
    private static DateFormat createDateFormat(String pattern) {
        SimpleDateFormat sdf = new SimpleDateFormat(pattern);
        TimeZone gmt = TimeZone.getTimeZone("GMT");
        sdf.setTimeZone(gmt);
        sdf.setLenient(true);
        return sdf;
    }

    /**
     * Calculate the phase of the moon for a given date.
     *
     * <p>Code heavily influenced by hacklib.c in <a
     * href="http://www.nethack.org/">Nethack</a></p>
     *
     * <p>The Algorithm:
     *
     * <pre>
     * moon period = 29.53058 days ~= 30, year = 365.2422 days
     *
     * days moon phase advances on first day of year compared to preceding year
     *  = 365.2422 - 12*29.53058 ~= 11
     *
     * years in Metonic cycle (time until same phases fall on the same days of
     *  the month) = 18.6 ~= 19
     *
     * moon phase on first day of year (epact) ~= (11*(year%19) + 18) % 30
     *  (18 as initial condition for 1900)
     *
     * current phase in days = first day phase + days elapsed in year
     *
     * 6 moons ~= 177 days
     * 177 ~= 8 reported phases * 22
     * + 11/22 for rounding
     * </pre>
     *
     * @return The phase of the moon as a number between 0 and 7 with
     *         0 meaning new moon and 4 meaning full moon.
     *
     * @since 1.2, Ant 1.5
     */
    public static int getPhaseOfMoon(Calendar cal) {
        int dayOfTheYear = cal.get(Calendar.DAY_OF_YEAR);
        int yearInMetonicCycle = ((cal.get(Calendar.YEAR) - 1900) % 19) + 1;
        int epact = (11 * yearInMetonicCycle + 18) % 30;
        if ((epact == 25 && yearInMetonicCycle > 11) || epact == 24) {
            epact++;
        }
        return (((((dayOfTheYear + epact) * 6) + 11) % 177) / 22) & 7;
    }

    /**
     * Returns the current Date in a format suitable for a SMTP date
     * header.
     *
     * @since Ant 1.5.2
     */
    public static String getDateForHeader() {
        Calendar cal = Calendar.getInstance();
        TimeZone tz = cal.getTimeZone();
        int offset = tz.getOffset(cal.get(Calendar.ERA),
                                  cal.get(Calendar.YEAR),
                                  cal.get(Calendar.MONTH),
                                  cal.get(Calendar.DAY_OF_MONTH),
                                  cal.get(Calendar.DAY_OF_WEEK),
                                  cal.get(Calendar.MILLISECOND));
        StringBuffer tzMarker = new StringBuffer(offset < 0 ? "-" : "+");
        offset = Math.abs(offset);
        int hours = offset / (60 * 60 * 1000);
        int minutes = offset / (60 * 1000) - 60 * hours;
        if (hours < 10) {
            tzMarker.append("0");
        }
        tzMarker.append(hours);
        if (minutes < 10) {
            tzMarker.append("0");
        }
        tzMarker.append(minutes);
        return DATE_HEADER_FORMAT.format(cal.getTime()) + tzMarker.toString();
    }
}
