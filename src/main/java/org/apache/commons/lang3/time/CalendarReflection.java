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

import java.lang.reflect.Method;
import java.util.Calendar;
import java.util.GregorianCalendar;

import org.apache.commons.lang3.exception.ExceptionUtils;

/**
 * Use reflection to access java 1.7 methods in Calendar.  This allows compilation with 1.6 compiler.
 */
class CalendarReflection {

    private static final Method IS_WEEK_DATE_SUPPORTED = getCalendarMethod("isWeekDateSupported");
    private static final Method GET_WEEK_YEAR = getCalendarMethod("getWeekYear");

    private static Method getCalendarMethod(String methodName, Class<?>... argTypes) {
        try {
            Method m = Calendar.class.getMethod(methodName, argTypes);
            return m;
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Does this calendar instance support week date?
     * @param calendar The calendar instance.
     * @return false, if runtime is less than java 1.7; otherwise, the result of calendar.isWeekDateSupported().
     */
    static boolean isWeekDateSupported(Calendar calendar) {
        try {
            return IS_WEEK_DATE_SUPPORTED!=null && ((Boolean)IS_WEEK_DATE_SUPPORTED.invoke(calendar)).booleanValue();
        } catch (Exception e) {
            return ExceptionUtils.<Boolean>rethrow(e);
        }
    }

    /**
     * Invoke getWeekYear() method of calendar instance.
     * <p>
     * If runtime is 1.7 or better and calendar instance support week year,
     * return the value from invocation of getWeekYear().
     * <p>
     * If runtime is less than 1.7, and calendar is an instance of
     * GregorianCalendar, return an approximation of the week year.
     * (Approximation is good for all years after the Julian to Gregorian
     * cutover.)
     * <p>
     * Otherwise, return the calendar instance year value.
     *
     * @param calendar The calendar instance.
     * @return the week year or year value.
     */
    public static int getWeekYear(Calendar calendar) {
        try {
            if (isWeekDateSupported(calendar)) {
                return (Integer) GET_WEEK_YEAR.invoke(calendar);
            }
        } catch (Exception e) {
            return ExceptionUtils.<Integer> rethrow(e);
        }

        int year = calendar.get(Calendar.YEAR);
        if (IS_WEEK_DATE_SUPPORTED == null && calendar instanceof GregorianCalendar) {
            // not perfect, won't work before gregorian cutover
            // good enough for most business use.
            switch (calendar.get(Calendar.MONTH)) {
            case Calendar.JANUARY:
                if (calendar.get(Calendar.WEEK_OF_YEAR) >= 52) {
                    --year;
                }
                break;
            case Calendar.DECEMBER:
                if (calendar.get(Calendar.WEEK_OF_YEAR) == 1) {
                    ++year;
                }
                break;
            }
        }
        return year;
    }
}
