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

import org.apache.commons.lang.StringUtils;

import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

/**
 * <p>Duration formatting utilities and constants. The following table describes the tokens 
 * used in the pattern language for formatting. </p>
 * <table border="1">
 *  <tr><th>character</th><th>duration element</th></tr>
 *  <tr><td>y</td><td>years</td></tr>
 *  <tr><td>M</td><td>months</td></tr>
 *  <tr><td>d</td><td>days</td></tr>
 *  <tr><td>H</td><td>hours</td></tr>
 *  <tr><td>m</td><td>minutes</td></tr>
 *  <tr><td>s</td><td>seconds</td></tr>
 *  <tr><td>S</td><td>milliseconds</td></tr>
 * </table>
 *
 * @author Apache Ant - DateUtils
 * @author <a href="mailto:sbailliez@apache.org">Stephane Bailliez</a>
 * @author <a href="mailto:stefan.bodewig@epost.de">Stefan Bodewig</a>
 * @author Stephen Colebourne
 * @author <a href="mailto:ggregory@seagullsw.com">Gary Gregory</a>
 * @author Henri Yandell
 * @since 2.1
 * @version $Id: DurationFormatUtils.java,v 1.20 2004/10/02 01:40:30 bayard Exp $
 */
public class DurationFormatUtils {

    /**
     * <p>DurationFormatUtils instances should NOT be constructed in standard programming.</p>
     *
     * <p>This constructor is public to permit tools that require a JavaBean instance
     * to operate.</p>
     */
    public DurationFormatUtils() {
    }

    /**
     * <p>Pattern used with <code>FastDateFormat</code> and <code>SimpleDateFormat</code> for the ISO8601 
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
//    public static final FastDateFormat ISO_EXTENDED_FORMAT =
//        FastDateFormat.getInstance(ISO_EXTENDED_FORMAT_PATTERN);

    /**
     * <p>Get the time gap as a string.</p>
     * 
     * <p>The format used is ISO8601-like:
     * <i>H</i>:<i>m</i>:<i>s</i>.<i>S</i>.</p>
     * 
     * @param millis  the duration to format
     * @return the time as a String
     */
    public static String formatISO(long millis) {
        return format(millis, "H:mm:ss.SSS");
    }

    public static String format(long millis) {
        return format(millis, ISO_EXTENDED_FORMAT_PATTERN, false, TimeZone.getDefault() );
    }
    public static String format(long startMillis, long endMillis) {
        return format(startMillis, endMillis, ISO_EXTENDED_FORMAT_PATTERN, false, TimeZone.getDefault() );
    }


    /**
     * <p>Get the time gap as a string, using the specified format, and padding with zeros and 
     * using the default timezone.</p>
     * 
     * @param millis  the duration to format
     * @param format  the way in which to format the duration
     * @return the time as a String
     */
    public static String format(long millis, String format) {
        return format(millis, format, true, TimeZone.getDefault());
    }
    /**
     * <p>Get the time gap as a string, using the specified format.
     * Padding the left hand side of numbers with zeroes is optional and 
     * the timezone may be specified. 
     * 
     * @param millis  the duration to format
     * @param format  the way in which to format the duration
     * @param padWithZeros whether to pad the left hand side of numbers with 0's
     * @param timezone the millis are defined in
     * @return the time as a String
     */
    public static String format(long millis, String format, boolean padWithZeros, TimeZone timezone) {

        if(millis >= 28 * DateUtils.MILLIS_PER_DAY) {
            Calendar c = Calendar.getInstance(timezone);
            c.set(1970, 0, 1, 0, 0, 0);
            c.set(Calendar.MILLISECOND, 0);
            return format(c.getTime().getTime(), millis, format, padWithZeros, timezone);
        }

        Token[] tokens = lexx(format);

        int years        = 0;
        int months       = 0;
        int days         = 0;
        int hours        = 0;
        int minutes      = 0;
        int seconds      = 0;
        int milliseconds = 0;

        /*  This will never be evaluated
        if(Token.containsTokenWithValue(tokens, y) ) {
            years = (int) (millis / DateUtils.MILLIS_PER_YEAR);
            millis = millis - (years * DateUtils.MILLIS_PER_YEAR);
        }
        if(Token.containsTokenWithValue(tokens, M) ) {
            months = (int) (millis / DateUtils.MILLIS_PER_MONTH);
            millis = millis - (months * DateUtils.MILLIS_PER_MONTH);
            // as MONTH * 12 != YEAR, this fixes issues
            if(months == 12) {
                years++;
                months = 0;
            }
        }
        */
        if(Token.containsTokenWithValue(tokens, d) ) {
            days = (int) (millis / DateUtils.MILLIS_PER_DAY);
            millis = millis - (days * DateUtils.MILLIS_PER_DAY);
        }
        if(Token.containsTokenWithValue(tokens, H) ) {
            hours = (int) (millis / DateUtils.MILLIS_PER_HOUR);
            millis = millis - (hours * DateUtils.MILLIS_PER_HOUR);
        }
        if(Token.containsTokenWithValue(tokens, m) ) {
            minutes = (int) (millis / DateUtils.MILLIS_PER_MINUTE);
            millis = millis - (minutes * DateUtils.MILLIS_PER_MINUTE);
        }
        if(Token.containsTokenWithValue(tokens, s) ) {
            seconds = (int) (millis / DateUtils.MILLIS_PER_SECOND);
            millis = millis - (seconds * DateUtils.MILLIS_PER_SECOND);
        }
        if(Token.containsTokenWithValue(tokens, S) ) {
            milliseconds = (int) millis;
        }

        return formatDuration(tokens, years, months, days, hours, minutes, seconds, milliseconds, padWithZeros);
    }


    static String formatDuration(Token[] tokens, int years, int months, int days, int hours, 
                                 int minutes, int seconds, int milliseconds, boolean padWithZeros) 
    { 
        StringBuffer buffer = new StringBuffer();
        int sz = tokens.length;
        for(int i=0; i<sz; i++) {
            Token token = tokens[i];
            Object value = token.getValue();
            int count = token.getCount();
            if(value instanceof StringBuffer) {
                buffer.append(value.toString());
            } else {
                if(value == y) {
                    buffer.append( padWithZeros ? StringUtils.leftPad(""+years, count, "0") : ""+years ); 
                } else
                if(value == M) {
                    buffer.append( padWithZeros ? StringUtils.leftPad(""+months, count, "0") : ""+months ); 
                } else
                if(value == d) {
                    buffer.append( padWithZeros ? StringUtils.leftPad(""+days, count, "0") : ""+days ); 
                } else
                if(value == H) {
                    buffer.append( padWithZeros ? StringUtils.leftPad(""+hours, count, "0") : ""+hours ); 
                } else
                if(value == m) {
                    buffer.append( padWithZeros ? StringUtils.leftPad(""+minutes, count, "0") : ""+minutes ); 
                } else
                if(value == s) {
                    buffer.append( padWithZeros ? StringUtils.leftPad(""+seconds, count, "0") : ""+seconds ); 
                } else
                if(value == S) {
                    buffer.append( padWithZeros ? StringUtils.leftPad(""+milliseconds, count, "0") : ""+milliseconds ); 
                }
            }
        }
        
        return buffer.toString();
    }

    /**
     * <p>Get the time gap as a string, using the specified format.
     * Padding the left hand side of numbers with zeroes is optional.
     * 
     * @param startMillis  the start of the duration
     * @param endMillis  the end of the duration
     * @param format  the way in which to format the duration
     * @return the time as a String
     */
    public static String format(long startMillis, long endMillis, String format) {
        return format(startMillis, endMillis, format, true, TimeZone.getDefault());
    }
    /**
     * <p>Get the time gap as a string, using the specified format.
     * Padding the left hand side of numbers with zeroes is optional and 
     * the timezone may be specified. 
     * 
     * @param startMillis  the start of the duration
     * @param endMillis  the end of the duration
     * @param format  the way in which to format the duration
     * @param padWithZeros whether to pad the left hand side of numbers with 0's
     * @param timezone the millis are defined in
     * @return the time as a String
     */
    public static String format(long startMillis, long endMillis, String format, boolean padWithZeros, TimeZone timezone) {

        long millis = endMillis - startMillis;
        if(millis < 28 * DateUtils.MILLIS_PER_DAY) {
            return format(millis, format, padWithZeros, timezone);
        }

        Token[] tokens = lexx(format);

        // timezones get funky around 0, so normalizing everything to GMT 
        // stops the hours being off
        Calendar start = Calendar.getInstance(timezone);
        start.setTime(new Date(startMillis));
        Calendar end = Calendar.getInstance(timezone);
        end.setTime(new Date(endMillis));

        // initial estimates
        int years = end.get(Calendar.YEAR) - start.get(Calendar.YEAR);
        int months = end.get(Calendar.MONTH) - start.get(Calendar.MONTH);
        // each initial estimate is adjusted in case it is under 0
        while(months < 0) {
            months += 12;
            years -= 1;
        }
        int days = end.get(Calendar.DAY_OF_MONTH) - start.get(Calendar.DAY_OF_MONTH);
        while(days < 0) {
            days += 31;  // such overshooting is taken care of later on
            months -= 1;
        }
        int hours = end.get(Calendar.HOUR_OF_DAY) - start.get(Calendar.HOUR_OF_DAY);
        while(hours < 0) {
            hours += 24;
            days -= 1;
        }
        int minutes = end.get(Calendar.MINUTE) - start.get(Calendar.MINUTE);
        while(minutes < 0) {
            minutes += 60;
            hours -= 1;
        }
        int seconds = end.get(Calendar.SECOND) - start.get(Calendar.SECOND);
        while(seconds < 0) {
            seconds += 60;
            minutes -= 1;
        }
        int milliseconds = end.get(Calendar.MILLISECOND) - start.get(Calendar.MILLISECOND);
        while(milliseconds < 0) {
            milliseconds += 1000;
            seconds -= 1;
        }

        // take estimates off of end to see if we can equal start, when it overshoots recalculate
        milliseconds -= reduceAndCorrect( start, end, Calendar.MILLISECOND, milliseconds );
        seconds -= reduceAndCorrect( start, end, Calendar.SECOND, seconds );
        minutes -= reduceAndCorrect( start, end, Calendar.MINUTE, minutes );
        hours -= reduceAndCorrect( start, end, Calendar.HOUR_OF_DAY, hours );
        days -= reduceAndCorrect( start, end, Calendar.DAY_OF_MONTH, days );
        months -= reduceAndCorrect( start, end, Calendar.MONTH, months );
        years -= reduceAndCorrect( start, end, Calendar.YEAR, years );

        // This next block of code adds in values that 
        // aren't requested. This allows the user to ask for the 
        // number of months and get the real count and not just 0->11.
        if(!Token.containsTokenWithValue(tokens, y) ) {
            if(Token.containsTokenWithValue(tokens, M) ) {
                months += 12 * years;
                years = 0;
            } else {
                // TODO: this is a bit weak, needs work to know about leap years
                days += 365 * years;
                years = 0;
            }
        }
        if(!Token.containsTokenWithValue(tokens, M) ) {
            days += end.get(Calendar.DAY_OF_YEAR) - start.get(Calendar.DAY_OF_YEAR);
            months = 0;
        }
        if(!Token.containsTokenWithValue(tokens, d) ) {
            hours += 24 * days;
            days = 0;
        }
        if(!Token.containsTokenWithValue(tokens, H) ) {
            minutes += 60 * hours;
            hours = 0;
        }
        if(!Token.containsTokenWithValue(tokens, m) ) {
            seconds += 60 * minutes;
            minutes = 0;
        }
        if(!Token.containsTokenWithValue(tokens, s) ) {
            milliseconds += 1000 * seconds;
            seconds = 0;
        }

        return formatDuration(tokens, years, months, days, hours, minutes, seconds, milliseconds, padWithZeros);
    }

    // Reduces by difference, then if it overshot, calculates the overshot amount and 
    // fixes and returns the amount to change by
    static int reduceAndCorrect(Calendar start, Calendar end, int field, int difference) {
        end.add( field, -1 * difference );
        int endValue = end.get(field);
        int startValue = start.get(field);
        if(endValue < startValue) {
            int newdiff = startValue - endValue;
            end.add( field, newdiff );
            return newdiff;
        } else {
            return 0;
        }
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

        // This method is generally replacable by the format method, but 
        // there are a series of tweaks and special cases that require 
        // trickery to replicate.
        String duration = format(millis, "d' days 'H' hours 'm' minutes 's' seconds'");
        if(suppressLeadingZeroElements) {
            // this is a temporary marker on the front. Like ^ in regexp.
            duration = " " + duration;
            String tmp = StringUtils.replaceOnce(duration, " 0 days", "");
            if(tmp.length() != duration.length()) {
                duration = tmp;
                tmp = StringUtils.replaceOnce(duration, " 0 hours", "");
                if(tmp.length() != duration.length()) {
                    duration = tmp;
                    tmp = StringUtils.replaceOnce(duration, " 0 minutes", "");
                    duration = tmp;
                    if(tmp.length() != duration.length()) {
                        duration = StringUtils.replaceOnce(tmp, " 0 seconds", "");
                    }
                }
            }
            if(duration.length() != 0) {
                // strip the space off again
                duration = duration.substring(1);
            }
        }
        if(suppressTrailingZeroElements) {
            String tmp = StringUtils.replaceOnce(duration, " 0 seconds", "");
            if(tmp.length() != duration.length()) {
                duration = tmp;
                tmp = StringUtils.replaceOnce(duration, " 0 minutes", "");
                if(tmp.length() != duration.length()) {
                    duration = tmp;
                    tmp = StringUtils.replaceOnce(duration, " 0 hours", "");
                    if(tmp.length() != duration.length()) {
                        duration = StringUtils.replaceOnce(tmp, " 0 days", "");
                    }
                }
            }
        }
        // handle plurals
        duration = StringUtils.replaceOnce(duration, "1 seconds", "1 second");
        duration = StringUtils.replaceOnce(duration, "1 minutes", "1 minute");
        duration = StringUtils.replaceOnce(duration, "1 hours", "1 hour");
        duration = StringUtils.replaceOnce(duration, "1 days", "1 day");
        return duration;
    }

    static final Object y = "y";
    static final Object M = "M";
    static final Object d = "d";
    static final Object H = "H";
    static final Object m = "m";
    static final Object s = "s";
    static final Object S = "S";
    
    static Token[] lexx(String format) {
        char[] array = format.toCharArray();
        java.util.ArrayList list = new java.util.ArrayList(array.length);

        boolean inLiteral = false;
        StringBuffer buffer = null;
        Token previous = null;
        int sz = array.length;
        for(int i=0; i<sz; i++) {
            char ch = array[i];
            if(inLiteral && ch != '\'') {
                buffer.append(ch);
                continue;
            }
            Object value = null;
            switch(ch) {
                // TODO: Need to handle escaping of '
                case '\'' : 
                  if(inLiteral) {
                      buffer = null;
                      inLiteral = false;
                  } else {
                      buffer = new StringBuffer();
                      list.add(new Token(buffer));
                      inLiteral = true;
                  }
                  break;
                case 'y'  : value = y; break;
                case 'M'  : value = M; break;
                case 'd'  : value = d; break;
                case 'H'  : value = H; break;
                case 'm'  : value = m; break;
                case 's'  : value = s; break;
                case 'S'  : value = S; break;
                default   : 
                  if(buffer == null) {
                      buffer = new StringBuffer();
                      list.add(new Token(buffer));
                  }
                  buffer.append(ch);
            }

            if(value != null) {
                if(previous != null && previous.getValue() == value) {
                    previous.increment();
                } else {
                    Token token = new Token(value);
                    list.add(token); 
                    previous = token;
                }
                buffer = null; 
            }
        }
        return (Token[]) list.toArray( new Token[0] );
    }

}

// Represents an element of the format-mini-language.
class Token {

    // will only work for the tokens, not for stringbuffers/numbers
    static boolean containsTokenWithValue(Token[] tokens, Object value) {
        int sz = tokens.length;
        for(int i=0; i<sz; i++) {
            if(tokens[i].getValue() == value) {
                return true;
            }
        }
        return false;
    }

    private Object value;
    private int count;

    public Token(Object value) {
        this.value = value;
        this.count = 1;
    }

    Token(Object value, int count) {
        this.value = value;
        this.count = count;
    }

    public void increment() { 
        count++;
    }

    public int getCount() {
        return count;
    }

    public Object getValue() {
        return value;
    }

    public boolean equals(Object obj2) {
        if(obj2 instanceof Token) {
            Token tok2 = (Token) obj2;
            if(this.value.getClass() != tok2.value.getClass()) {
                return false;
            }
            if(this.count != tok2.count) {
                return false;
            }
            if(this.value instanceof StringBuffer) {
                return this.value.toString().equals(tok2.value.toString());
            } else
            if(this.value instanceof Number) {
                return this.value.equals(tok2.value);
            } else {
                return this.value == tok2.value;
            }
        } else {
            return false;
        }
    }

    public String toString() {
        return StringUtils.repeat(this.value.toString(), this.count);
    }

}
