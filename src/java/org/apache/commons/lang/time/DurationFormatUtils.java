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

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.mutable.MutableInt;

/**
 * <p>Duration formatting utilities and constants.</p>
 *
 * @author Apache Ant - DateUtils
 * @author <a href="mailto:sbailliez@apache.org">Stephane Bailliez</a>
 * @author <a href="mailto:stefan.bodewig@epost.de">Stefan Bodewig</a>
 * @author Stephen Colebourne
 * @author <a href="mailto:ggregory@seagullsw.com">Gary Gregory</a>
 * @since 2.0
 * @version $Id: DurationFormatUtils.java,v 1.11 2004/08/27 06:45:25 bayard Exp $
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
    public static final FastDateFormat ISO_EXTENDED_FORMAT =
        FastDateFormat.getInstance(ISO_EXTENDED_FORMAT_PATTERN);

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
    /**
     * <p>Get the time gap as a string, using the specified format.</p>
     * <table border="1">
     *  <tr><th>character</th><th>duration element</th></tr>
     *  <tr><td>y</td><td>years (aka 365 days)</td></tr>
     *  <tr><td>M</td><td>months (aka year/12)</td></tr>
     *  <tr><td>d</td><td>days</td></tr>
     *  <tr><td>H</td><td>hours</td></tr>
     *  <tr><td>m</td><td>minutes</td></tr>
     *  <tr><td>s</td><td>seconds</td></tr>
     *  <tr><td>S</td><td>milliseconds</td></tr>
     * </table>
     * 
     * @param millis  the duration to format
     * @param format  the way iin which to format the duration
     * @return the time as a String
     */
    public static String format(long millis, String format) {
        StringBuffer buffer = new StringBuffer();
        Token[] tokens = lexx(format);
        int sz = tokens.length;

        int years        = 0;
        int months       = 0;
        int days         = 0;
        int hours        = 0;
        int minutes      = 0;
        int seconds      = 0;
        int milliseconds = 0;

        if(Token.containsTokenWithValue(tokens, y) ) {
            years = (int) (millis / DateUtils.MILLIS_PER_YEAR);
            millis = millis - (years * DateUtils.MILLIS_PER_YEAR);
        }
        if(Token.containsTokenWithValue(tokens, M) ) {
            months = (int) (millis / DateUtils.MILLIS_PER_MONTH);
            millis = millis - (months * DateUtils.MILLIS_PER_MONTH);
        }
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


        for(int i=0; i<sz; i++) {
            Token token = tokens[i];
            Object value = token.getValue();
            int count = token.getCount();
            if(value instanceof StringBuffer) {
                buffer.append(value.toString());
            } else {
                if(value == y) {
                    buffer.append( StringUtils.leftPad(""+years, count, "0") ); 
                } else
                if(value == M) {
                    buffer.append( StringUtils.leftPad(""+months, count, "0") ); 
                } else
                if(value == d) {
                    buffer.append( StringUtils.leftPad(""+days, count, "0") ); 
                } else
                if(value == H) {
                    buffer.append( StringUtils.leftPad(""+hours, count, "0") ); 
                } else
                if(value == m) {
                    buffer.append( StringUtils.leftPad(""+minutes, count, "0") ); 
                } else
                if(value == s) {
                    buffer.append( StringUtils.leftPad(""+seconds, count, "0") ); 
                } else
                if(value == S) {
                    buffer.append( StringUtils.leftPad(""+milliseconds, count, "0") ); 
                }
            }
        }
        
        return buffer.toString();
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
