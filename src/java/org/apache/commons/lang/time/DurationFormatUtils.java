/*
 * Copyright 2002-2004 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang.time;

/**
 * <p>Duration formatting utilities and constants.</p>
 *
 * @author Apache Ant - DateUtils
 * @author <a href="mailto:sbailliez@apache.org">Stephane Bailliez</a>
 * @author <a href="mailto:stefan.bodewig@epost.de">Stefan Bodewig</a>
 * @author Stephen Colebourne
 * @author <a href="mailto:ggregory@seagullsw.com">Gary Gregory</a>
 * @since 2.0
 * @version $Id: DurationFormatUtils.java,v 1.9 2004/02/18 22:56:42 ggregory Exp $
 */
class DurationFormatUtils {
    // TODO: Make class public once methods can fully select which fields to output

    /**
     * <p>Pattern used with <code>FastDateFormat</code> and <code>SimpleDateFormat </code> for the ISO8601 
     * date time extended format used in durations.</p>
     * 
     * @see org.apache.commons.lang.time.FastDateFormat
     * @see java.text.SimpleDateFormat
     */
    public static final String ISO_EXTENDED_FORMAT_PATTERN = "'P'yyyy'Y'M'M'd'DT'H'H'm'M's.S'S'";

    /**
     * <p>ISO8601 formatter for the date time extended format used in durations, 
     * with XML Schema durations particularly in mind.</p>
     * 
     * <p>This format represents the Gregorian year, month, day, hour, minute, and second components defined 
     * in section 5.5.3.2 of ISO 8601, respectively. These components are ordered in their significance by their order 
     * of appearance i.e. as year, month, day, hour, minute, and second.</p>
     * 
     * <p>The ISO8601 extended format P<i>n</i>Y<i>n</i>M<i>n</i>DT<i>n</i>H<i>n</i>M<i>n</i>S, where <i>n</i>Y 
     * represents the number of years, <i>n</i>M the number of months, <i>n</i>D the number of days, 
     * 'T' is the date/time separator, <i>n</i>H the number of hours, <i>n</i>M the number of minutes and 
     * <i>n</i>S the number of seconds. The number of seconds can include decimal digits to arbitrary precision.</p>
     * 
     * @see #ISO_EXTENDED_FORMAT_PATTERN
     * @see <a href="http://www.w3.org/TR/xmlschema-2/#duration">http://www.w3.org/TR/xmlschema-2/#duration</a>
     */
    public static final FastDateFormat ISO_EXTENDED_FORMAT =
        FastDateFormat.getInstance(ISO_EXTENDED_FORMAT_PATTERN);

    /**
     * <p>Get the time gap as a string.</p>
     * 
     * <p>The format used is ISO8601-like:
     * <i>hours</i>:<i>minutes</i>:<i>seconds</i>.<i>milliseconds</i>.</p>
     * 
     * @param millis  the duration to format
     * @return the time as a String
     */
    public static String formatISO(long millis) {
        int hours, minutes, seconds, milliseconds;
        hours = (int) (millis / DateUtils.MILLIS_PER_HOUR);
        millis = millis - (hours * DateUtils.MILLIS_PER_HOUR);
        minutes = (int) (millis / DateUtils.MILLIS_PER_MINUTE);
        millis = millis - (minutes * DateUtils.MILLIS_PER_MINUTE);
        seconds = (int) (millis / DateUtils.MILLIS_PER_SECOND);
        millis = millis - (seconds * DateUtils.MILLIS_PER_SECOND);
        milliseconds = (int) millis;

        StringBuffer buf = new StringBuffer(32);
        buf.append(hours);
        buf.append(':');
        buf.append((char) (minutes / 10 + '0'));
        buf.append((char) (minutes % 10 + '0'));
        buf.append(':');
        buf.append((char) (seconds / 10 + '0'));
        buf.append((char) (seconds % 10 + '0'));
        buf.append('.');
        if (milliseconds < 10) {
            buf.append('0').append('0');
        } else if (milliseconds < 100) {
            buf.append('0');
        }
        buf.append(milliseconds);
        return buf.toString();
    }

    /**
     * <p>Format an elapsed time into a plurialization correct string.</p>
     * 
     * @param millis  the elapsed time to report in milliseconds
     * @param suppressLeadingZeroElements suppresses leading 0 elements
     * @param suppressTrailingZeroElements suppresses trailing 0 elements
     * @return the formatted text in days/hours/minutes/seconds
     */
    public static String formatWords(
        long millis,
        boolean suppressLeadingZeroElements,
        boolean suppressTrailingZeroElements) {
        long[] values = new long[4];
        values[0] = millis / DateUtils.MILLIS_PER_DAY;
        values[1] = (millis / DateUtils.MILLIS_PER_HOUR) % 24;
        values[2] = (millis / DateUtils.MILLIS_PER_MINUTE) % 60;
        values[3] = (millis / DateUtils.MILLIS_PER_SECOND) % 60;
        String[] fieldsOne = { " day ", " hour ", " minute ", " second" };
        String[] fieldsPlural = { " days ", " hours ", " minutes ", " seconds" };

        StringBuffer buf = new StringBuffer(64);
        boolean valueOutput = false;

        for (int i = 0; i < 4; i++) {
            long value = values[i];
            if (value == 0) {
                // handle zero
                if (valueOutput) {
                    if (suppressTrailingZeroElements == false) {
                        buf.append('0').append(fieldsPlural[i]);
                    }
                } else {
                    if (suppressLeadingZeroElements == false) {
                        buf.append('0').append(fieldsPlural[i]);
                    }
                }
            } else if (value == 1) {
                // one
                valueOutput = true;
                buf.append('1').append(fieldsOne[i]);
            } else {
                // other
                valueOutput = true;
                buf.append(value).append(fieldsPlural[i]);
            }
        }

        return buf.toString().trim();
    }

    /**
     * <p>DurationFormatUtils instances should NOT be constructed in standard programming.</p>
     *
     * <p>This constructor is public to permit tools that require a JavaBean instance
     * to operate.</p>
     */
    public DurationFormatUtils() {
    }

}
