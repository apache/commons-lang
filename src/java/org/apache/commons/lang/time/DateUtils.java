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
import java.text.DateFormatSymbols;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Iterator;
import java.util.Locale;
import java.util.NoSuchElementException;
import java.util.TimeZone;

/**
 * <p>A suite of utilities surrounding the use of the
 * {@link java.util.Calendar} and {@link java.util.Date} object.</p>
 *
 * @author <a href="mailto:sergek@lokitech.com">Serge Knystautas</a>
 * @author Stephen Colebourne
 * @author Janek Bogucki
 * @author <a href="mailto:ggregory@seagullsw.com">Gary Gregory</a>
 * @since 2.0
 * @version $Id: DateUtils.java,v 1.10 2003/08/06 21:13:39 ggregory Exp $
 */
public class DateUtils {
    
    /**
     * The UTC time zone  (often referred to as GMT).
     */
    public static final TimeZone UTC_TIME_ZONE = TimeZone.getTimeZone("GMT");
    /**
     * Number of milliseconds in a standard second.
     */
    public static final int MILLIS_IN_SECOND = 1000;
    /**
     * Number of milliseconds in a standard minute.
     */
    public static final int MILLIS_IN_MINUTE = 60 * 1000;
    /**
     * Number of milliseconds in a standard hour.
     */
    public static final int MILLIS_IN_HOUR = 60 * 60 * 1000;
    /**
     * Number of milliseconds in a standard day.
     */
    public static final int MILLIS_IN_DAY = 24 * 60 * 60 * 1000;

    /**
     * This is half a month, so this represents whether a date is in the top
     * or bottom half of the month.
     */
    public final static int SEMI_MONTH = 1001;

    private static final int[][] fields = {
            {Calendar.MILLISECOND},
            {Calendar.SECOND},
            {Calendar.MINUTE},
            {Calendar.HOUR_OF_DAY, Calendar.HOUR},
            {Calendar.DATE, Calendar.DAY_OF_MONTH, Calendar.AM_PM /* Calendar.DAY_OF_YEAR, Calendar.DAY_OF_WEEK, Calendar.DAY_OF_WEEK_IN_MONTH */},
            {Calendar.MONTH, DateUtils.SEMI_MONTH},
            {Calendar.YEAR},
            {Calendar.ERA}};

    private static DateFormat[] dateFormats = {
        //3/31/92 10:00:07 PST
        new SimpleDateFormat("M/dd/yy h:mm:ss z"),
        //January 23, 1987 10:05pm
        new SimpleDateFormat("MMM d, yyyy h:mm a"),
        //22:00 GMT
        new SimpleDateFormat("h:mm z")};

    /**
     * A week range, starting on Sunday.
     */
    public final static int RANGE_WEEK_SUNDAY = 1;

    /**
     * A week range, starting on Monday.
     */
    public final static int RANGE_WEEK_MONDAY = 2;

    /**
     * A week range, starting on the day focused.
     */
    public final static int RANGE_WEEK_RELATIVE = 3;

    /**
     * A week range, centered around the day focused.
     */
    public final static int RANGE_WEEK_CENTER = 4;

    /**
     * A month range, the week starting on Sunday.
     */
    public final static int RANGE_MONTH_SUNDAY = 5;

    /**
     * A month range, the week starting on Monday.
     */
    public final static int RANGE_MONTH_MONDAY = 6;

    /**
     * <p><code>DateUtils<code> instances should NOT be constructed in
     * standard programming. Instead, the class should be used as
     * <code>DateUtils.parse(str);</code>.</p>
     *
     * <p>This constructor is public to permit tools that require a JavaBean
     * instance to operate.</p>
     */
    public DateUtils() {
    }

    //-----------------------------------------------------------------------
    /**
     * <p>Round this date, leaving the field specified as the most
     * significant field.</p>
     *
     * <p>For example, if you had the datetime of 28 Mar 2002
     * 13:45:01.231, if this was passed with HOUR, it would return
     * 28 Mar 2002 14:00:00.000. If this was passed with MONTH, it
     * would return 1 April 2002 0:00:00.000.</p>
     * 
     * @param date  the date to work with
     * @param field  the field from <code>Calendar</code>
     *  or <code>SEMI_MONTH</code>
     * @return the rounded date
     * @throws IllegalArgumentException if the date is <code>null</code>
     */
    public static Date round(Date date, int field) {
        if (date == null) {
            throw new IllegalArgumentException("The date must not be null");
        }
        GregorianCalendar gval = new GregorianCalendar();
        gval.setTime(date);
        modify(gval, field, true);
        return gval.getTime();
    }

    /**
     * <p>Round this date, leaving the field specified as the most
     * significant field.</p>
     *
     * <p>For example, if you had the datetime of 28 Mar 2002
     * 13:45:01.231, if this was passed with HOUR, it would return
     * 28 Mar 2002 14:00:00.000. If this was passed with MONTH, it
     * would return 1 April 2002 0:00:00.000.</p>
     * 
     * @param date  the date to work with
     * @param field  the field from <code>Calendar</code>
     *  or <code>SEMI_MONTH</code>
     * @return the rounded date (a different object)
     * @throws IllegalArgumentException if the date is <code>null</code>
     */
    public static Calendar round(Calendar date, int field) {
        if (date == null) {
            throw new IllegalArgumentException("The date must not be null");
        }
        Calendar rounded = (Calendar) date.clone();
        modify(rounded, field, true);
        return rounded;
    }

    /**
     * <p>Round this date, leaving the field specified as the most
     * significant field.</p>
     *
     * <p>For example, if you had the datetime of 28 Mar 2002
     * 13:45:01.231, if this was passed with HOUR, it would return
     * 28 Mar 2002 14:00:00.000. If this was passed with MONTH, it
     * would return 1 April 2002 0:00:00.000.</p>
     * 
     * @param date  the date to work with, either Date or Calendar
     * @param field  the field from <code>Calendar</code>
     *  or <code>SEMI_MONTH</code>
     * @return the rounded date
     * @throws IllegalArgumentException if the date is <code>null</code>
     * @throws ClassCastException if the object type is not a <code>Date</code>
     *  or <code>Calendar</code>
     */
    public static Date round(Object date, int field) {
        if (date == null) {
            throw new IllegalArgumentException("The date must not be null");
        }
        if (date instanceof Date) {
            return round((Date) date, field);
        } else if (date instanceof Calendar) {
            return round((Calendar) date, field).getTime();
        } else {
            throw new ClassCastException("Could not round " + date);
        }
    }

    //-----------------------------------------------------------------------
    /**
     * <p>Truncate this date, leaving the field specified as the most
     * significant field.</p>
     *
     * <p>For example, if you had the datetime of 28 Mar 2002
     * 13:45:01.231, if you passed with HOUR, it would return 28 Mar
     * 2002 13:00:00.000.  If this was passed with MONTH, it would
     * return 1 Mar 2002 0:00:00.000.</p>
     * 
     * @param date  the date to work with
     * @param field  the field from <code>Calendar</code>
     *  or <code>SEMI_MONTH</code>
     * @return the rounded date
     * @throws IllegalArgumentException if the date is <code>null</code>
     */
    public static Date truncate(Date date, int field) {
        if (date == null) {
            throw new IllegalArgumentException("The date must not be null");
        }
        GregorianCalendar gval = new GregorianCalendar();
        gval.setTime(date);
        modify(gval, field, false);
        return gval.getTime();
    }

    /**
     * <p>Truncate this date, leaving the field specified as the most
     * significant field.</p>
     *
     * <p>For example, if you had the datetime of 28 Mar 2002
     * 13:45:01.231, if you passed with HOUR, it would return 28 Mar
     * 2002 13:00:00.000.  If this was passed with MONTH, it would
     * return 1 Mar 2002 0:00:00.000.</p>
     * 
     * @param date  the date to work with
     * @param field  the field from <code>Calendar</code>
     *  or <code>SEMI_MONTH</code>
     * @return the rounded date (a different object)
     * @throws IllegalArgumentException if the date is <code>null</code>
     */
    public static Calendar truncate(Calendar date, int field) {
        if (date == null) {
            throw new IllegalArgumentException("The date must not be null");
        }
        Calendar truncated = (Calendar) date.clone();
        modify(truncated, field, false);
        return truncated;
    }

    /**
     * <p>Truncate this date, leaving the field specified as the most
     * significant field.</p>
     *
     * <p>For example, if you had the datetime of 28 Mar 2002
     * 13:45:01.231, if you passed with HOUR, it would return 28 Mar
     * 2002 13:00:00.000.  If this was passed with MONTH, it would
     * return 1 Mar 2002 0:00:00.000.</p>
     * 
     * @param date  the date to work with, either <code>Date</code>
     *  or <code>Calendar</code>
     * @param field  the field from <code>Calendar</code>
     *  or <code>SEMI_MONTH</code>
     * @return the rounded date
     * @throws IllegalArgumentException if the date
     *  is <code>null</code>
     * @throws ClassCastException if the object type is not a
     *  <code>Date</code> or <code>Calendar</code>
     */
    public static Date truncate(Object date, int field) {
        if (date == null) {
            throw new IllegalArgumentException("The date must not be null");
        }
        if (date instanceof Date) {
            return truncate((Date) date, field);
        } else if (date instanceof Calendar) {
            return truncate((Calendar) date, field).getTime();
        } else {
            throw new ClassCastException("Could not truncate " + date);
        }
    }

    //-----------------------------------------------------------------------
    /**
     * <p>Internal calculation method.</p>
     * 
     * @param val  the calendar
     * @param field  the field constant
     * @param round  true to round, false to truncate
     */
    private static void modify(Calendar val, int field, boolean round) {
        boolean roundUp = false;
        for (int i = 0; i < fields.length; i++) {
            for (int j = 0; j < fields[i].length; j++) {
                if (fields[i][j] == field) {
                    //This is our field... we stop looping
                    if (round && roundUp) {
                        if (field == DateUtils.SEMI_MONTH) {
                            //This is a special case that's hard to generalize
                            //If the date is 1, we round up to 16, otherwise
                            //  we subtract 15 days and add 1 month
                            if (val.get(Calendar.DATE) == 1) {
                                val.add(Calendar.DATE, 15);
                            } else {
                                val.add(Calendar.DATE, -15);
                                val.add(Calendar.MONTH, 1);
                            }
                        } else {
                            //We need at add one to this field since the
                            //  last number causes us to round up
                            val.add(fields[i][0], 1);
                        }
                    }
                    return;
                }
            }
            //We have various fields that are not easy roundings
            int offset = 0;
            boolean offsetSet = false;
            //These are special types of fields that require different rounding rules
            switch (field) {
                case DateUtils.SEMI_MONTH:
                    if (fields[i][0] == Calendar.DATE) {
                        //If we're going to drop the DATE field's value,
                        //  we want to do this our own way.
                        //We need to subtrace 1 since the date has a minimum of 1
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
                    if (fields[i][0] == Calendar.HOUR) {
                        //If we're going to drop the HOUR field's value,
                        //  we want to do this our own way.
                        offset = val.get(Calendar.HOUR);
                        if (offset >= 12) {
                            offset -= 12;
                        }
                        roundUp = offset > 6;
                        offsetSet = true;
                    }
                    break;
            }
            if (!offsetSet) {
                int min = val.getActualMinimum(fields[i][0]);
                int max = val.getActualMaximum(fields[i][0]);
                //Calculate the offset from the minimum allowed value
                offset = val.get(fields[i][0]) - min;
                //Set roundUp if this is more than half way between the minimum and maximum
                roundUp = offset > ((max - min) / 2);
            }
            //We need to remove this field
            val.add(fields[i][0], -offset);
        }
        throw new IllegalArgumentException("The field " + field + " is not supported");

    }

    //-----------------------------------------------------------------------
    /**
     * <p>Parses a date string formatted in CVS format.</p>
     * 
     * @param dateStr  the date to parse
     * @return the parsed date
     * @throws IllegalArgumentException if the date cannot be parsed
     */
    public static Calendar parseCVS(String dateStr) {
        return parseCVS(dateStr, Locale.getDefault());
    }

    /**
     * <p>Parses a date string formatted in CVS format.</p>
     * 
     * @param dateStr  the date to parse
     * @param locale  the locale to parse in
     * @return the parsed date
     * @throws IllegalArgumentException if the date is null or cannot be parsed
     */
    public static Calendar parseCVS(String dateStr, Locale locale) {
        if (dateStr == null) {
            throw new IllegalArgumentException("The date must not be null");
        }
        //Get the symbol names
        DateFormatSymbols symbols = new DateFormatSymbols(locale);

        //Prep the string to parse
        String value = dateStr.toLowerCase().trim();

        //Get the current date/time
        Calendar now = Calendar.getInstance();
        if (value.endsWith(" ago")) {
            //If this was a date that was "ago" the current time...
            //Strip out the ' ago' part
            value = value.substring(0, value.length() - 4);

            //Split the value and unit
            int start = value.indexOf(" ");
            if (start < 0) {
                throw new IllegalArgumentException("Could not find space in between value and unit");
            }
            String unit = value.substring(start + 1);
            value = value.substring(0, start);
            //We support "a week", so we need to parse the value as "a"
            int val = 0;
            if (value.equals("a") || value.equals("an")) {
                val = 1;
            } else {
                val = Integer.parseInt(value);
            }

            //Determine the unit
            if (unit.equals("milliseconds") || unit.equals("millisecond")) {
                now.add(Calendar.MILLISECOND, -val);
            } else if (unit.equals("seconds") || unit.equals("second")) {
                now.add(Calendar.SECOND, -val);
            } else if (unit.equals("minutes") || unit.equals("minute")) {
                now.add(Calendar.MINUTE, -val);
            } else if (unit.equals("hours") || unit.equals("hour")) {
                now.add(Calendar.HOUR, -val);
            } else if (unit.equals("days") || unit.equals("day")) {
                now.add(Calendar.DATE, -val);
            } else if (unit.equals("weeks") || unit.equals("week")) {
                now.add(Calendar.DATE, -val * 7);
            } else if (unit.equals("fortnights") || unit.equals("fortnight")) {
                now.add(Calendar.DATE, -val * 14);
            } else if (unit.equals("months") || unit.equals("month")) {
                now.add(Calendar.MONTH, -val);
            } else if (unit.equals("years") || unit.equals("year")) {
                now.add(Calendar.YEAR, -val);
            } else {
                throw new IllegalArgumentException("We do not understand that many units ago");
            }
            return now;
        } else if (value.startsWith("last ")) {
            //If this was the last time a certain field was met
            //Strip out the 'last ' part
            value = value.substring(5);
            //Get the current date/time
            String[] strings = symbols.getWeekdays();
            for (int i = 0; i < strings.length; i++) {
                if (value.equalsIgnoreCase(strings[i])) {
                    //How many days after Sunday
                    int daysAgo = now.get(Calendar.DAY_OF_WEEK) - i;
                    if (daysAgo <= 0) {
                        daysAgo += 7;
                    }
                    now.add(Calendar.DATE, -daysAgo);
                    return now;
                }
            }
            strings = symbols.getMonths();
            for (int i = 0; i < strings.length; i++) {
                if (value.equalsIgnoreCase(strings[i])) {
                    //How many days after January
                    int monthsAgo = now.get(Calendar.MONTH) - i;
                    if (monthsAgo <= 0) {
                        monthsAgo += 12;
                    }
                    now.add(Calendar.MONTH, -monthsAgo);
                    return now;
                }
            }
            if (value.equals("week")) {
                now.add(Calendar.DATE, -7);
                return now;
            }
            throw new IllegalArgumentException("We do not understand that last units");
        } else if (value.equals("yesterday")) {
            now.add(Calendar.DATE, -1);
            return now;
        } else if (value.equals("tomorrow")) {
            now.add(Calendar.DATE, 1);
            return now;
        }
        //Try to parse the date a number of different ways
        for (int i = 0; i < dateFormats.length; i++) {
            try {
                Date datetime = dateFormats[i].parse(dateStr);
                Calendar cal = Calendar.getInstance();
                cal.setTime(datetime);
                return cal;
            } catch (ParseException pe) {
                //we ignore this and just keep trying
            }
        }

        throw new IllegalArgumentException("Unable to parse '" + dateStr + "'.");
    }

    //-----------------------------------------------------------------------
    /**
     * <p>This constructs an <code>Iterator</code> that will
     * start and stop over a date range based on the focused
     * date and the range style.</p>
     *
     * <p>For instance, passing Thursday, July 4, 2002 and a
     * <code>RANGE_MONTH_SUNDAY</code> will return an
     * <code>Iterator</code> that starts with Sunday, June 30,
     * 2002 and ends with Saturday, August 3, 2002.
     * 
     * @param focus  the date to work with
     * @param rangeStyle  the style constant to use. Must be one of the range
     * styles listed for the {@link #iterator(Calendar, int)} method.
     *
     * @return the date iterator
     * @throws IllegalArgumentException if the date is <code>null</code> or if
     * the rangeStyle is not 
     */
    public static Iterator iterator(Date focus, int rangeStyle) {
        if (focus == null) {
            throw new IllegalArgumentException("The date must not be null");
        }
        GregorianCalendar gval = new GregorianCalendar();
        gval.setTime(focus);
        return iterator(gval, rangeStyle);
    }

    /**
     * <p>This constructs an <code>Iterator</code> that will
     * start and stop over a date range based on the focused
     * date and the range style.</p>
     *
     * <p>For instance, passing Thursday, July 4, 2002 and a
     * <code>RANGE_MONTH_SUNDAY</code> will return an
     * <code>Iterator</code> that starts with Sunday, June 30,
     * 2002 and ends with Saturday, August 3, 2002.
     * 
     * @param focus  the date to work with
     * @param rangeStyle  the style constant to use. Must be one of
     * {@link DateUtils#RANGE_MONTH_SUNDAY}, 
     * {@link DateUtils#RANGE_MONTH_MONDAY},
     * {@link DateUtils#RANGE_WEEK_SUNDAY},
     * {@link DateUtils#RANGE_WEEK_MONDAY},
     * {@link DateUtils#RANGE_WEEK_RELATIVE},
     * {@link DateUtils#RANGE_WEEK_CENTER}
     * @return the date iterator
     * @throws IllegalArgumentException if the date is <code>null</code>
     */
    public static Iterator iterator(Calendar focus, int rangeStyle) {
        if (focus == null) {
            throw new IllegalArgumentException("The date must not be null");
        }
        Calendar start = null;
        Calendar end = null;
        int startCutoff = Calendar.SUNDAY;
        int endCutoff = Calendar.SATURDAY;
        switch (rangeStyle) {
            case RANGE_MONTH_SUNDAY:
            case RANGE_MONTH_MONDAY:
                //Set start to the first of the month
                start = truncate(focus, Calendar.MONTH);
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
                start = truncate(focus, Calendar.DATE);
                end = truncate(focus, Calendar.DATE);
                switch (rangeStyle) {
                    case RANGE_WEEK_SUNDAY:
                        //already set by default
                        break;
                    case RANGE_WEEK_MONDAY:
                        startCutoff = Calendar.MONDAY;
                        endCutoff = Calendar.SUNDAY;
                        break;
                    case RANGE_WEEK_RELATIVE:
                        startCutoff = focus.get(Calendar.DAY_OF_WEEK);
                        endCutoff = startCutoff - 1;
                        break;
                    case RANGE_WEEK_CENTER:
                        startCutoff = focus.get(Calendar.DAY_OF_WEEK) - 3;
                        endCutoff = focus.get(Calendar.DAY_OF_WEEK) + 3;
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
     * <p>This constructs an <code>Iterator</code> that will
     * start and stop over a date range based on the focused
     * date and the range style.</p>
     *
     * <p>For instance, passing Thursday, July 4, 2002 and a
     * <code>RANGE_MONTH_SUNDAY</code> will return an
     * <code>Iterator</code> that starts with Sunday, June 30,
     * 2002 and ends with Saturday, August 3, 2002.</p>
     * 
     * @param focus  the date to work with, either
     *  <code>Date</code> or <code>Calendar</code>
     * @param rangeStyle  the style constant to use. Must be one of the range
     * styles listed for the {@link #iterator(Calendar, int)} method.
     * @return the date iterator
     * @throws IllegalArgumentException if the date
     *  is <code>null</code>
     * @throws ClassCastException if the object type is
     *  not a <code>Date</code> or <code>Calendar</code>
     */
    public static Iterator iterator(Object focus, int rangeStyle) {
        if (focus == null) {
            throw new IllegalArgumentException("The date must not be null");
        }
        if (focus instanceof Date) {
            return iterator((Date) focus, rangeStyle);
        } else if (focus instanceof Calendar) {
            return iterator((Calendar) focus, rangeStyle);
        } else {
            throw new ClassCastException("Could not iterate based on " + focus);
        }
    }

    /**
     * <p>Date iterator.</p>
     */
    static class DateIterator implements Iterator {
        private final Calendar endFinal;
        private final Calendar spot;
        
        DateIterator(Calendar startFinal, Calendar endFinal) {
            super();
            this.endFinal = endFinal;
            spot = startFinal;
            spot.add(Calendar.DATE, -1);
        }

        public boolean hasNext() {
            return spot.before(endFinal);
        }

        public Object next() {
            if (spot.equals(endFinal)) {
                throw new NoSuchElementException();
            }
            spot.add(Calendar.DATE, 1);
            return spot.clone();
        }

        public void remove() {
            throw new UnsupportedOperationException();
        }
    }

}
