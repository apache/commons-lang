/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
// package org.apache.commons.lang;

import java.text.*;
import java.util.*;

/**
 * A suite of utilities surrounding the use of the Calendar and Date object.
 *
 * @author <a href="mailto:sergek@lokitech.com">Serge Knystautas</a>
 */
public class CalendarUtils {

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
            {Calendar.MONTH, CalendarUtils.SEMI_MONTH},
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
     * See the other round method.  Works with a Date object.
     */
    public static Date round(Date val, int field) {
        GregorianCalendar gval = new GregorianCalendar();
        gval.setTime(val);
        modify(gval, field, true);
        return gval.getTime();
    }

    /**
     * Round this date, leaving the field specified as the most significant
     * field.  For example, if you had the datetime of 28 Mar 2002
     * 13:45:01.231, if this was passed with HOUR, it would return 28 Mar
     * 2002 14:00:00.000.  If this was passed with MONTH, it would return
     * 1 April 2002 0:00:00.000.
     */
    public static Calendar round(Calendar val, int field) {
        Calendar rounded = (Calendar) val.clone();
        modify(rounded, field, true);
        return rounded;
    }

    /**
     * See the other round method.  Works with an Object, trying to
     * use it as either a Date or Calendar.
     */
    public static Date round(Object val, int field) {
        if (val instanceof Date) {
            return round((Date) val, field);
        } else if (val instanceof Calendar) {
            return round((Calendar) val, field).getTime();
        } else {
            throw new ClassCastException("Could not round " + val);
        }
    }

    /**
     * See the other trunc method.  Works with a Date.
     */
    public static Date trunc(Date val, int field) {
        GregorianCalendar gval = new GregorianCalendar();
        gval.setTime(val);
        modify(gval, field, false);
        return gval.getTime();
    }

    /**
     * Truncate this date, leaving the field specified as the most significant
     * field.  For example, if you had the datetime of 28 Mar 2002
     * 13:45:01.231, if you passed with HOUR, it would return 28 Mar
     * 2002 13:00:00.000.  If this was passed with MONTH, it would return
     * 1 Mar 2002 0:00:00.000.
     */
    public static Calendar trunc(Calendar val, int field) {
        Calendar truncated = (Calendar) val.clone();
        modify(truncated, field, false);
        return truncated;
    }

    /**
     * See the other trunc method.  Works with an Object, trying to
     * use it as either a Date or Calendar.
     */
    public static Date trunc(Object val, int field) {
        if (val instanceof Date) {
            return trunc((Date) val, field);
        } else if (val instanceof Calendar) {
            return trunc((Calendar) val, field).getTime();
        } else {
            throw new ClassCastException("Could not trunc " + val);
        }
    }

    private static void modify(Calendar val, int field, boolean round) {
        boolean roundUp = false;
        for (int i = 0; i < fields.length; i++) {
            for (int j = 0; j < fields[i].length; j++) {
                if (fields[i][j] == field) {
                    //This is our field... we stop looping
                    if (round && roundUp) {
                        if (field == CalendarUtils.SEMI_MONTH) {
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
                case CalendarUtils.SEMI_MONTH:
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
        throw new RuntimeException("We do not support that field.");

    }

    /**
     * Parses strings the way that CVS supports it... very human readable
     */
    public static Calendar parse(String original) {
        return parse(original, Locale.getDefault());
    }

    /**
     * Parses strings the way that CVS supports it... very human readable
     */
    public static Calendar parse(String original, Locale locale) {
        //Get the symbol names
        DateFormatSymbols symbols = new DateFormatSymbols(locale);

        //Prep the string to parse
        String value = original.toLowerCase().trim();

        //Get the current date/time
        Calendar now = Calendar.getInstance();
        if (value.endsWith(" ago")) {
            //If this was a date that was "ago" the current time...
            //Strip out the ' ago' part
            value = value.substring(0, value.length() - 4);

            //Split the value and unit
            int start = value.indexOf(" ");
            if (start < 0) {
                throw new RuntimeException("Could not find space in between value and unit");
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
                throw new RuntimeException("We do not understand that many units ago");
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
                Date datetime = dateFormats[i].parse(original);
                Calendar cal = Calendar.getInstance();
                cal.setTime(datetime);
                return cal;
            } catch (ParseException pe) {
                //we ignore this and just keep trying
            }
        }

        throw new RuntimeException("Unable to parse '" + original + "'.");
    }

    /**
     * This constructs an Iterator that will start and stop over a date
     * range based on the focused date and the range style.  For instance,
     * passing Thursday, July 4, 2002 and a RANGE_MONTH_SUNDAY will return
     * an Iterator that starts with Sunday, June 30, 2002 and ends with
     * Saturday, August 3, 2002.
     */
    public static Iterator getCalendarIterator(Calendar focus, int rangeStyle) {
        Calendar start = null;
        Calendar end = null;
        int startCutoff = Calendar.SUNDAY;
        int endCutoff = Calendar.SATURDAY;
        switch (rangeStyle) {
            case RANGE_MONTH_SUNDAY:
            case RANGE_MONTH_MONDAY:
                //Set start to the first of the month
                start = trunc(focus, Calendar.MONTH);
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
                start = trunc(focus, Calendar.DATE);
                end = trunc(focus, Calendar.DATE);
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
                throw new RuntimeException("The range style " + rangeStyle + " is not valid.");
        }
        if (startCutoff < Calendar.SUNDAY) {
            startCutoff += 7;
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
        final Calendar startFinal = start;
        final Calendar endFinal = end;
        Iterator it = new Iterator() {
            Calendar spot = null;
            {
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
        };
        return it;
    }

    /**
     * See the other getCalendarIterator.  Works with a Date.
     */
    public static Iterator getCalendarIterator(Date focus, int rangeStyle) {
        GregorianCalendar gval = new GregorianCalendar();
        gval.setTime(focus);
        return getCalendarIterator(gval, rangeStyle);
    }

    /**
     * See the other getCalendarIterator.  Works with an Object, trying
     * to use it as a Date or Calendar.
     */
    public static Iterator getCalendarIterator(Object focus, int rangeStyle) {
        if (focus instanceof Date) {
            return getCalendarIterator((Date) focus, rangeStyle);
        } else if (focus instanceof Calendar) {
            return getCalendarIterator((Calendar) focus, rangeStyle);
        } else {
            throw new ClassCastException("Could not iterate based on " + focus);
        }
    }

}
