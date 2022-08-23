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

import java.text.FieldPosition;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Locale;
import java.util.TimeZone;

/**
 * DatePrinter is the "missing" interface for the format methods of
 * {@link java.text.DateFormat}. You can obtain an object implementing this
 * interface by using one of the FastDateFormat factory methods.
 * <p>
 * Warning: Since binary compatible methods may be added to this interface in any
 * release, developers are not expected to implement this interface.
 *
 * @since 3.2
 */
public interface DatePrinter {

    /**
     * Formats a millisecond {@code long} value.
     *
     * @param millis  the millisecond value to format
     * @return the formatted string
     * @since 2.1
     */
    String format(long millis);

    /**
     * Formats a {@link Date} object using a {@link GregorianCalendar}.
     *
     * @param date  the date to format
     * @return the formatted string
     */
    String format(Date date);

    /**
     * Formats a {@link Calendar} object.
     * The TimeZone set on the Calendar is only used to adjust the time offset.
     * The TimeZone specified during the construction of the Parser will determine the TimeZone
     * used in the formatted string.
     *
     * @param calendar  the calendar to format.
     * @return the formatted string
     */
    String format(Calendar calendar);

    /**
     * Formats a millisecond {@code long} value into the
     * supplied {@link StringBuffer}.
     *
     * @param millis  the millisecond value to format
     * @param buf  the buffer to format into
     * @return the specified string buffer
     * @deprecated Use {{@link #format(long, Appendable)}.
     */
    @Deprecated
    StringBuffer format(long millis, StringBuffer buf);

    /**
     * Formats a {@link Date} object into the
     * supplied {@link StringBuffer} using a {@link GregorianCalendar}.
     *
     * @param date  the date to format
     * @param buf  the buffer to format into
     * @return the specified string buffer
     * @deprecated Use {{@link #format(Date, Appendable)}.
     */
    @Deprecated
    StringBuffer format(Date date, StringBuffer buf);

    /**
     * Formats a {@link Calendar} object into the supplied {@link StringBuffer}.
     * The TimeZone set on the Calendar is only used to adjust the time offset.
     * The TimeZone specified during the construction of the Parser will determine the TimeZone
     * used in the formatted string.
     *
     * @param calendar  the calendar to format
     * @param buf  the buffer to format into
     * @return the specified string buffer
     * @deprecated Use {{@link #format(Calendar, Appendable)}.
     */
    @Deprecated
    StringBuffer format(Calendar calendar, StringBuffer buf);

    /**
     * Formats a millisecond {@code long} value into the
     * supplied {@link Appendable}.
     *
     * @param millis  the millisecond value to format
     * @param buf  the buffer to format into
     * @param <B> the Appendable class type, usually StringBuilder or StringBuffer.
     * @return the specified string buffer
     * @since 3.5
     */
    <B extends Appendable> B format(long millis, B buf);

    /**
     * Formats a {@link Date} object into the
     * supplied {@link Appendable} using a {@link GregorianCalendar}.
     *
     * @param date  the date to format
     * @param buf  the buffer to format into
     * @param <B> the Appendable class type, usually StringBuilder or StringBuffer.
     * @return the specified string buffer
     * @since 3.5
     */
    <B extends Appendable> B format(Date date, B buf);

    /**
     * Formats a {@link Calendar} object into the supplied {@link Appendable}.
     * The TimeZone set on the Calendar is only used to adjust the time offset.
     * The TimeZone specified during the construction of the Parser will determine the TimeZone
     * used in the formatted string.
     *
     * @param calendar  the calendar to format
     * @param buf  the buffer to format into
     * @param <B> the Appendable class type, usually StringBuilder or StringBuffer.
     * @return the specified string buffer
     * @since 3.5
     */
    <B extends Appendable> B format(Calendar calendar, B buf);


    // Accessors
    /**
     * Gets the pattern used by this printer.
     *
     * @return the pattern, {@link java.text.SimpleDateFormat} compatible
     */
    String getPattern();

    /**
     * Gets the time zone used by this printer.
     *
     * <p>This zone is always used for {@link Date} printing.</p>
     *
     * @return the time zone
     */
    TimeZone getTimeZone();

    /**
     * Gets the locale used by this printer.
     *
     * @return the locale
     */
    Locale getLocale();

    /**
     * Formats a {@link Date}, {@link Calendar} or
     * {@link Long} (milliseconds) object.
     *
     * @param obj  the object to format
     * @param toAppendTo  the buffer to append to
     * @param pos  the position - ignored
     * @return the buffer passed in
     * @see java.text.DateFormat#format(Object, StringBuffer, FieldPosition)
     */
    StringBuffer format(Object obj, StringBuffer toAppendTo, FieldPosition pos);
}
