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
package org.apache.commons.lang3;

import java.text.DateFormat;
import java.text.DateFormatSymbols;
import java.text.ParseException;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;

/*
 * This class was created to hold the parseCVS method extracted from DateUtils in commons-lang.
 * The code was originally submitted by Serge Knystautas sergek@lokitech.com. It was never
 * fully implemented, and has been moved to the sandbox for further development. Recent discussion
 * from the commons-dev mailing list:
 * http://marc.theaimsgroup.com/?l=jakarta-commons-dev&m=108904098032038&w=2
 * Moving the code to the sandbox satisfies bug is a temporary solution to
 * http://issues.apache.org/bugzilla/show_bug.cgi?id=22172 but this issue needs to be considered
 * when the class/method is further developed.
 */
public class DateFormatter {

    /**
     * <p>Parses a date string formatted in CVS format.</p>
     * 
     * @param dateStr  the date to parse
     * @return the parsed date
     * @throws IllegalArgumentException if the date cannot be parsed
     */
    public static Calendar parseCVS(String dateStr) {
        if (dateStr == null) {
            throw new IllegalArgumentException("The date must not be null");
        }
        //Get the symbol names
        DateFormatSymbols symbols = new DateFormatSymbols(Locale.ENGLISH);

        DateFormat[] dateFormats = new DateFormat[0];

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

}
