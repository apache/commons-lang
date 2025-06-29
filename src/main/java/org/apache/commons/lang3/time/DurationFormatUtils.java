/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang3.time;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Objects;
import java.util.TimeZone;
import java.util.stream.Stream;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.Strings;
import org.apache.commons.lang3.Validate;

/**
 * Duration formatting utilities and constants. The following table describes the tokens
 * used in the pattern language for formatting.
 * <table border="1">
 *  <caption>Pattern Tokens</caption>
 *  <tr><th>character</th><th>duration element</th></tr>
 *  <tr><td>y</td><td>years</td></tr>
 *  <tr><td>M</td><td>months</td></tr>
 *  <tr><td>d</td><td>days</td></tr>
 *  <tr><td>H</td><td>hours</td></tr>
 *  <tr><td>m</td><td>minutes</td></tr>
 *  <tr><td>s</td><td>seconds</td></tr>
 *  <tr><td>S</td><td>milliseconds</td></tr>
 *  <tr><td>'text'</td><td>arbitrary text content</td></tr>
 * </table>
 *
 * <strong>Note: It's not currently possible to include a single-quote in a format.</strong>
 * <br>
 * Token values are printed using decimal digits.
 * A token character can be repeated to ensure that the field occupies a certain minimum
 * size. Values will be left-padded with 0 unless padding is disabled in the method invocation.
 * <br>
 * Tokens can be marked as optional by surrounding them with brackets [ ]. These tokens will
 * only be printed if the token value is non-zero. Literals within optional blocks will only be
 * printed if the preceding non-literal token is non-zero. Leading optional literals will only
 * be printed if the following non-literal is non-zero.
 * Multiple optional blocks can be used to group literals with the desired token.
 * <p>
 * Notes on Optional Tokens:<br>
 * <strong>Multiple optional tokens without literals can result in impossible to understand output.</strong><br>
 * <strong>Patterns where all tokens are optional can produce empty strings.</strong><br>
 * (See examples below)
 * </p>
 * <br>
 * <table border="1">
 * <caption>Example Output</caption>
 * <tr><th>pattern</th><th>Duration.ofDays(1)</th><th>Duration.ofHours(1)</th><th>Duration.ofMinutes(1)</th><th>Duration.ZERO</th></tr>
 * <tr><td>d'd'H'h'm'm's's'</td><td>1d0h0m0s</td><td>0d1h0m0s</td><td>0d0h1m0s</td><td>0d0h0m0s</td></tr>
 * <tr><td>d'd'[H'h'm'm']s's'</td><td>1d0s</td><td>0d1h0s</td><td>0d1m0s</td><td>0d0s</td></tr>
 * <tr><td>[d'd'H'h'm'm']s's'</td><td>1d0s</td><td>1h0s</td><td>1m0s</td><td>0s</td></tr>
 * <tr><td>[d'd'H'h'm'm's's']</td><td>1d</td><td>1h</td><td>1m</td><td></td></tr>
 * <tr><td>['{'d'}']HH':'mm</td><td>{1}00:00</td><td>01:00</td><td>00:01</td><td>00:00</td></tr>
 * <tr><td>['{'dd'}']['&lt;'HH'&gt;']['('mm')']</td><td>{01}</td><td>&lt;01&gt;</td><td>(00)</td><td></td></tr>
 * <tr><td>[dHms]</td><td>1</td><td>1</td><td>1</td><td></td></tr>
 * </table>
 * <strong>Note: Optional blocks cannot be nested.</strong>
 *
 * @since 2.1
 */
public class DurationFormatUtils {

    /**
     * Element that is parsed from the format pattern.
     */
    static final class Token {

        /** Empty array. */
        private static final Token[] EMPTY_ARRAY = {};

        /**
         * Helper method to determine if a set of tokens contain a value
         *
         * @param tokens set to look in
         * @param value to look for
         * @return boolean {@code true} if contained
         */
        static boolean containsTokenWithValue(final Token[] tokens, final Object value) {
            return Stream.of(tokens).anyMatch(token -> token.getValue() == value);
        }

        private final CharSequence value;
        private int count;
        private int optionalIndex = -1;

        /**
         * Wraps a token around a value. A value would be something like a 'Y'.
         *
         * @param value value to wrap, non-null.
         * @param optional whether the token is optional
         * @param optionalIndex the index of the optional token within the pattern
         */
        Token(final CharSequence value, final boolean optional, final int optionalIndex) {
            this.value = Objects.requireNonNull(value, "value");
            this.count = 1;
            if (optional) {
                this.optionalIndex = optionalIndex;
            }
        }

        /**
         * Supports equality of this Token to another Token.
         *
         * @param obj2 Object to consider equality of
         * @return boolean {@code true} if equal
         */
        @Override
        public boolean equals(final Object obj2) {
            if (obj2 instanceof Token) {
                final Token tok2 = (Token) obj2;
                if (this.value.getClass() != tok2.value.getClass()) {
                    return false;
                }
                if (this.count != tok2.count) {
                    return false;
                }
                if (this.value instanceof StringBuilder) {
                    return this.value.toString().equals(tok2.value.toString());
                }
                if (this.value instanceof Number) {
                    return this.value.equals(tok2.value);
                }
                return this.value == tok2.value;
            }
            return false;
        }

        /**
         * Gets the current number of values represented
         *
         * @return int number of values represented
         */
        int getCount() {
            return count;
        }

        /**
         * Gets the particular value this token represents.
         *
         * @return Object value, non-null.
         */
        Object getValue() {
            return value;
        }

        /**
         * Returns a hash code for the token equal to the
         * hash code for the token's value. Thus 'TT' and 'TTTT'
         * will have the same hash code.
         *
         * @return The hash code for the token
         */
        @Override
        public int hashCode() {
            return this.value.hashCode();
        }

        /**
         * Adds another one of the value
         */
        void increment() {
            count++;
        }

        /**
         * Represents this token as a String.
         *
         * @return String representation of the token
         */
        @Override
        public String toString() {
            return StringUtils.repeat(this.value.toString(), this.count);
        }
    }

    private static final int MINUTES_PER_HOUR = 60;

    private static final int SECONDS_PER_MINUTES = 60;

    private static final int HOURS_PER_DAY = 24;

    /**
     * Pattern used with {@link FastDateFormat} and {@link SimpleDateFormat}
     * for the ISO 8601 period format used in durations.
     *
     * @see org.apache.commons.lang3.time.FastDateFormat
     * @see java.text.SimpleDateFormat
     */
    public static final String ISO_EXTENDED_FORMAT_PATTERN = "'P'yyyy'Y'M'M'd'DT'H'H'm'M's.SSS'S'";

    static final String y = "y";

    static final String M = "M";

    static final String d = "d";

    static final String H = "H";

    static final String m = "m";

    static final String s = "s";

    static final String S = "S";

    /**
     * The internal method to do the formatting.
     *
     * @param tokens  the tokens
     * @param years  the number of years
     * @param months  the number of months
     * @param days  the number of days
     * @param hours  the number of hours
     * @param minutes  the number of minutes
     * @param seconds  the number of seconds
     * @param milliseconds  the number of millis
     * @param padWithZeros  whether to pad
     * @return the formatted string
     */
    static String format(final Token[] tokens, final long years, final long months, final long days, final long hours, final long minutes,
            final long seconds,
            final long milliseconds, final boolean padWithZeros) {
        final StringBuilder buffer = new StringBuilder();
        boolean lastOutputSeconds = false;
        boolean lastOutputZero = false;
        int optionalStart = -1;
        boolean firstOptionalNonLiteral = false;
        int optionalIndex = -1;
        boolean inOptional = false;
        for (final Token token : tokens) {
            final Object value = token.getValue();
            final boolean isLiteral = value instanceof StringBuilder;
            final int count = token.getCount();
            if (optionalIndex != token.optionalIndex) {
              optionalIndex = token.optionalIndex;
              if (optionalIndex > -1) {
                //entering new optional block
                optionalStart = buffer.length();
                lastOutputZero = false;
                inOptional = true;
                firstOptionalNonLiteral = false;
              } else {
                //leaving optional block
                inOptional = false;
              }
            }
            if (isLiteral) {
                if (!inOptional || !lastOutputZero) {
                    buffer.append(value.toString());
                }
            } else if (value.equals(y)) {
                lastOutputSeconds = false;
                lastOutputZero = years == 0;
                if (!inOptional || !lastOutputZero) {
                    buffer.append(paddedValue(years, padWithZeros, count));
                }
            } else if (value.equals(M)) {
                lastOutputSeconds = false;
                lastOutputZero = months == 0;
                if (!inOptional || !lastOutputZero) {
                    buffer.append(paddedValue(months, padWithZeros, count));
                }
            } else if (value.equals(d)) {
                lastOutputSeconds = false;
                lastOutputZero = days == 0;
                if (!inOptional || !lastOutputZero) {
                    buffer.append(paddedValue(days, padWithZeros, count));
                }
            } else if (value.equals(H)) {
                lastOutputSeconds = false;
                lastOutputZero = hours == 0;
                if (!inOptional || !lastOutputZero) {
                    buffer.append(paddedValue(hours, padWithZeros, count));
                }
            } else if (value.equals(m)) {
                lastOutputSeconds = false;
                lastOutputZero = minutes == 0;
                if (!inOptional || !lastOutputZero) {
                    buffer.append(paddedValue(minutes, padWithZeros, count));
                }
            } else if (value.equals(s)) {
                lastOutputSeconds = true;
                lastOutputZero = seconds == 0;
                if (!inOptional || !lastOutputZero) {
                    buffer.append(paddedValue(seconds, padWithZeros, count));
                }
            } else if (value.equals(S)) {
                lastOutputZero = milliseconds == 0;
                if (!inOptional || !lastOutputZero) {
                    if (lastOutputSeconds) {
                        // ensure at least 3 digits are displayed even if padding is not selected
                        final int width = padWithZeros ? Math.max(3, count) : 3;
                        buffer.append(paddedValue(milliseconds, true, width));
                    } else {
                        buffer.append(paddedValue(milliseconds, padWithZeros, count));
                    }
                }
                lastOutputSeconds = false;
            }
            //as soon as we hit first nonliteral in optional, check for literal prefix
            if (inOptional && !isLiteral && !firstOptionalNonLiteral) {
                firstOptionalNonLiteral = true;
                if (lastOutputZero) {
                    buffer.delete(optionalStart, buffer.length());
                }
            }
        }
        return buffer.toString();
    }

    /**
     * Formats the time gap as a string, using the specified format, and padding with zeros.
     *
     * <p>This method formats durations using the days and lower fields of the
     * format pattern. Months and larger are not used.</p>
     *
     * @param durationMillis  the duration to format
     * @param format  the way in which to format the duration, not null
     * @return the formatted duration, not null
     * @throws IllegalArgumentException if durationMillis is negative
     */
    public static String formatDuration(final long durationMillis, final String format) {
        return formatDuration(durationMillis, format, true);
    }

    /**
     * Formats the time gap as a string, using the specified format.
     * Padding the left-hand side side of numbers with zeroes is optional.
     *
     * <p>This method formats durations using the days and lower fields of the
     * format pattern. Months and larger are not used.</p>
     *
     * @param durationMillis  the duration to format
     * @param format  the way in which to format the duration, not null
     * @param padWithZeros  whether to pad the left-hand side side of numbers with 0's
     * @return the formatted duration, not null
     * @throws IllegalArgumentException if durationMillis is negative
     */
    public static String formatDuration(final long durationMillis, final String format, final boolean padWithZeros) {
        Validate.inclusiveBetween(0, Long.MAX_VALUE, durationMillis, "durationMillis must not be negative");

        final Token[] tokens = lexx(format);

        long days = 0;
        long hours = 0;
        long minutes = 0;
        long seconds = 0;
        long milliseconds = durationMillis;

        if (Token.containsTokenWithValue(tokens, d)) {
            days = milliseconds / DateUtils.MILLIS_PER_DAY;
            milliseconds -= days * DateUtils.MILLIS_PER_DAY;
        }
        if (Token.containsTokenWithValue(tokens, H)) {
            hours = milliseconds / DateUtils.MILLIS_PER_HOUR;
            milliseconds -= hours * DateUtils.MILLIS_PER_HOUR;
        }
        if (Token.containsTokenWithValue(tokens, m)) {
            minutes = milliseconds / DateUtils.MILLIS_PER_MINUTE;
            milliseconds -= minutes * DateUtils.MILLIS_PER_MINUTE;
        }
        if (Token.containsTokenWithValue(tokens, s)) {
            seconds = milliseconds / DateUtils.MILLIS_PER_SECOND;
            milliseconds -= seconds * DateUtils.MILLIS_PER_SECOND;
        }

        return format(tokens, 0, 0, days, hours, minutes, seconds, milliseconds, padWithZeros);
    }

    /**
     * Formats the time gap as a string.
     *
     * <p>The format used is ISO 8601-like: {@code HH:mm:ss.SSS}.</p>
     *
     * @param durationMillis  the duration to format
     * @return the formatted duration, not null
     * @throws IllegalArgumentException if durationMillis is negative
     */
    public static String formatDurationHMS(final long durationMillis) {
        return formatDuration(durationMillis, "HH:mm:ss.SSS");
    }

    /**
     * Formats the time gap as a string.
     *
     * <p>The format used is the ISO 8601 period format.</p>
     *
     * <p>This method formats durations using the days and lower fields of the
     * ISO format pattern, such as P7D6TH5M4.321S.</p>
     *
     * @param durationMillis  the duration to format
     * @return the formatted duration, not null
     * @throws IllegalArgumentException if durationMillis is negative
     */
    public static String formatDurationISO(final long durationMillis) {
        return formatDuration(durationMillis, ISO_EXTENDED_FORMAT_PATTERN, false);
    }

    /**
     * Formats an elapsed time into a pluralization correct string.
     *
     * <p>This method formats durations using the days and lower fields of the
     * format pattern. Months and larger are not used.</p>
     *
     * @param durationMillis  the elapsed time to report in milliseconds
     * @param suppressLeadingZeroElements  suppresses leading 0 elements
     * @param suppressTrailingZeroElements  suppresses trailing 0 elements
     * @return the formatted text in days/hours/minutes/seconds, not null
     * @throws IllegalArgumentException if durationMillis is negative
     */
    public static String formatDurationWords(
        final long durationMillis,
        final boolean suppressLeadingZeroElements,
        final boolean suppressTrailingZeroElements) {

        // This method is generally replaceable by the format method, but
        // there are a series of tweaks and special cases that require
        // trickery to replicate.
        String duration = formatDuration(durationMillis, "d' days 'H' hours 'm' minutes 's' seconds'");
        if (suppressLeadingZeroElements) {
            // this is a temporary marker on the front. Like ^ in regexp.
            duration = " " + duration;
            final String text = duration;
            String tmp = Strings.CS.replaceOnce(text, " 0 days", StringUtils.EMPTY);
            if (tmp.length() != duration.length()) {
                duration = tmp;
                final String text1 = duration;
                tmp = Strings.CS.replaceOnce(text1, " 0 hours", StringUtils.EMPTY);
                if (tmp.length() != duration.length()) {
                    duration = tmp;
                    final String text2 = duration;
                    tmp = Strings.CS.replaceOnce(text2, " 0 minutes", StringUtils.EMPTY);
                    duration = tmp;
                }
            }
            if (!duration.isEmpty()) {
                // strip the space off again
                duration = duration.substring(1);
            }
        }
        if (suppressTrailingZeroElements) {
            final String text = duration;
            String tmp = Strings.CS.replaceOnce(text, " 0 seconds", StringUtils.EMPTY);
            if (tmp.length() != duration.length()) {
                duration = tmp;
                final String text1 = duration;
                tmp = Strings.CS.replaceOnce(text1, " 0 minutes", StringUtils.EMPTY);
                if (tmp.length() != duration.length()) {
                    duration = tmp;
                    final String text2 = duration;
                    tmp = Strings.CS.replaceOnce(text2, " 0 hours", StringUtils.EMPTY);
                    if (tmp.length() != duration.length()) {
                        final String text3 = tmp;
                        duration = Strings.CS.replaceOnce(text3, " 0 days", StringUtils.EMPTY);
                    }
                }
            }
        }
        // handle plurals
        duration = " " + duration;
        final String text = duration;
        duration = Strings.CS.replaceOnce(text, " 1 seconds", " 1 second");
        final String text1 = duration;
        duration = Strings.CS.replaceOnce(text1, " 1 minutes", " 1 minute");
        final String text2 = duration;
        duration = Strings.CS.replaceOnce(text2, " 1 hours", " 1 hour");
        final String text3 = duration;
        duration = Strings.CS.replaceOnce(text3, " 1 days", " 1 day");
        return duration.trim();
    }

    /**
     * Formats the time gap as a string, using the specified format.
     * Padding the left-hand side side of numbers with zeroes is optional.
     *
     * @param startMillis  the start of the duration
     * @param endMillis  the end of the duration
     * @param format  the way in which to format the duration, not null
     * @return the formatted duration, not null
     * @throws IllegalArgumentException if startMillis is greater than endMillis
     */
    public static String formatPeriod(final long startMillis, final long endMillis, final String format) {
        return formatPeriod(startMillis, endMillis, format, true, TimeZone.getDefault());
    }

    /**
     * <p>Formats the time gap as a string, using the specified format.
     * Padding the left-hand side side of numbers with zeroes is optional and
     * the time zone may be specified.
     *
     * <p>When calculating the difference between months/days, it chooses to
     * calculate months first. So when working out the number of months and
     * days between January 15th and March 10th, it choose 1 month and
     * 23 days gained by choosing January-&gt;February = 1 month and then
     * calculating days forwards, and not the 1 month and 26 days gained by
     * choosing March -&gt; February = 1 month and then calculating days
     * backwards.</p>
     *
     * <p>For more control, the <a href="https://www.joda.org/joda-time/">Joda-Time</a>
     * library is recommended.</p>
     *
     * @param startMillis  the start of the duration
     * @param endMillis  the end of the duration
     * @param format  the way in which to format the duration, not null
     * @param padWithZeros  whether to pad the left-hand side side of numbers with 0's
     * @param timezone  the millis are defined in
     * @return the formatted duration, not null
     * @throws IllegalArgumentException if startMillis is greater than endMillis
     */
    public static String formatPeriod(final long startMillis, final long endMillis, final String format, final boolean padWithZeros,
            final TimeZone timezone) {
        Validate.isTrue(startMillis <= endMillis, "startMillis must not be greater than endMillis");

        // Used to optimize for differences under 28 days and
        // called formatDuration(millis, format); however this did not work
        // over leap years.
        // TODO: Compare performance to see if anything was lost by
        // losing this optimization.

        final Token[] tokens = lexx(format);

        // time zones get funky around 0, so normalizing everything to GMT
        // stops the hours being off
        final Calendar start = Calendar.getInstance(timezone);
        start.setTime(new Date(startMillis));
        final Calendar end = Calendar.getInstance(timezone);
        end.setTime(new Date(endMillis));

        // initial estimates
        long milliseconds = end.get(Calendar.MILLISECOND) - start.get(Calendar.MILLISECOND);
        int seconds = end.get(Calendar.SECOND) - start.get(Calendar.SECOND);
        int minutes = end.get(Calendar.MINUTE) - start.get(Calendar.MINUTE);
        int hours = end.get(Calendar.HOUR_OF_DAY) - start.get(Calendar.HOUR_OF_DAY);
        int days = end.get(Calendar.DAY_OF_MONTH) - start.get(Calendar.DAY_OF_MONTH);
        int months = end.get(Calendar.MONTH) - start.get(Calendar.MONTH);
        int years = end.get(Calendar.YEAR) - start.get(Calendar.YEAR);

        // each initial estimate is adjusted in case it is under 0
        while (milliseconds < 0) {
            milliseconds += DateUtils.MILLIS_PER_SECOND;
            seconds -= 1;
        }
        while (seconds < 0) {
            seconds += SECONDS_PER_MINUTES;
            minutes -= 1;
        }
        while (minutes < 0) {
            minutes += MINUTES_PER_HOUR;
            hours -= 1;
        }
        while (hours < 0) {
            hours += HOURS_PER_DAY;
            days -= 1;
        }

        if (Token.containsTokenWithValue(tokens, M)) {
            while (days < 0) {
                days += start.getActualMaximum(Calendar.DAY_OF_MONTH);
                months -= 1;
                start.add(Calendar.MONTH, 1);
            }

            while (months < 0) {
                months += 12;
                years -= 1;
            }

            if (!Token.containsTokenWithValue(tokens, y) && years != 0) {
                while (years != 0) {
                    months += 12 * years;
                    years = 0;
                }
            }
        } else {
            // there are no M's in the format string

            if (!Token.containsTokenWithValue(tokens, y)) {
                int target = end.get(Calendar.YEAR);
                if (months < 0) {
                    // target is end-year -1
                    target -= 1;
                }

                while (start.get(Calendar.YEAR) != target) {
                    days += start.getActualMaximum(Calendar.DAY_OF_YEAR) - start.get(Calendar.DAY_OF_YEAR);

                    // Not sure I grok why this is needed, but the brutal tests show it is
                    if (start instanceof GregorianCalendar &&
                            start.get(Calendar.MONTH) == Calendar.FEBRUARY &&
                            start.get(Calendar.DAY_OF_MONTH) == 29) {
                        days += 1;
                    }

                    start.add(Calendar.YEAR, 1);

                    days += start.get(Calendar.DAY_OF_YEAR);
                }

                years = 0;
            }

            while (start.get(Calendar.MONTH) != end.get(Calendar.MONTH)) {
                days += start.getActualMaximum(Calendar.DAY_OF_MONTH);
                start.add(Calendar.MONTH, 1);
            }

            months = 0;

            while (days < 0) {
                days += start.getActualMaximum(Calendar.DAY_OF_MONTH);
                months -= 1;
                start.add(Calendar.MONTH, 1);
            }

        }

        // The rest of this code adds in values that
        // aren't requested. This allows the user to ask for the
        // number of months and get the real count and not just 0->11.

        if (!Token.containsTokenWithValue(tokens, d)) {
            hours += HOURS_PER_DAY * days;
            days = 0;
        }
        if (!Token.containsTokenWithValue(tokens, H)) {
            minutes += MINUTES_PER_HOUR * hours;
            hours = 0;
        }
        if (!Token.containsTokenWithValue(tokens, m)) {
            seconds += SECONDS_PER_MINUTES * minutes;
            minutes = 0;
        }
        if (!Token.containsTokenWithValue(tokens, s)) {
            milliseconds += DateUtils.MILLIS_PER_SECOND * seconds;
            seconds = 0;
        }

        return format(tokens, years, months, days, hours, minutes, seconds, milliseconds, padWithZeros);
    }

    /**
     * Formats the time gap as a string.
     *
     * <p>The format used is the ISO 8601 period format.</p>
     *
     * @param startMillis  the start of the duration to format
     * @param endMillis  the end of the duration to format
     * @return the formatted duration, not null
     * @throws IllegalArgumentException if startMillis is greater than endMillis
     */
    public static String formatPeriodISO(final long startMillis, final long endMillis) {
        return formatPeriod(startMillis, endMillis, ISO_EXTENDED_FORMAT_PATTERN, false, TimeZone.getDefault());
    }

    /**
     * Parses a classic date format string into Tokens
     *
     * @param format  the format to parse, not null
     * @return array of Token[]
     */
    static Token[] lexx(final String format) {
        final ArrayList<Token> list = new ArrayList<>(format.length());

        boolean inLiteral = false;
        // Although the buffer is stored in a Token, the Tokens are only
        // used internally, so cannot be accessed by other threads
        StringBuilder buffer = null;
        Token previous = null;
        boolean inOptional = false;
        int optionalIndex = -1;
        for (int i = 0; i < format.length(); i++) {
            final char ch = format.charAt(i);
            if (inLiteral && ch != '\'') {
                buffer.append(ch); // buffer can't be null if inLiteral is true
                continue;
            }
            String value = null;
            switch (ch) {
            // TODO: Need to handle escaping of '
            case '[':
                if (inOptional) {
                    throw new IllegalArgumentException("Nested optional block at index: " + i);
                }
                optionalIndex++;
                inOptional = true;
                break;
            case ']':
                if (!inOptional) {
                    throw new IllegalArgumentException("Attempting to close unopened optional block at index: " + i);
                }
                inOptional = false;
                break;
            case '\'':
                if (inLiteral) {
                    buffer = null;
                    inLiteral = false;
                } else {
                    buffer = new StringBuilder();
                    list.add(new Token(buffer, inOptional, optionalIndex));
                    inLiteral = true;
                }
                break;
            case 'y':
                value = y;
                break;
            case 'M':
                value = M;
                break;
            case 'd':
                value = d;
                break;
            case 'H':
                value = H;
                break;
            case 'm':
                value = m;
                break;
            case 's':
                value = s;
                break;
            case 'S':
                value = S;
                break;
            default:
                if (buffer == null) {
                    buffer = new StringBuilder();
                    list.add(new Token(buffer, inOptional, optionalIndex));
                }
                buffer.append(ch);
            }

            if (value != null) {
                if (previous != null && previous.getValue().equals(value)) {
                    previous.increment();
                } else {
                    final Token token = new Token(value, inOptional, optionalIndex);
                    list.add(token);
                    previous = token;
                }
                buffer = null;
            }
        }
        if (inLiteral) { // i.e. we have not found the end of the literal
            throw new IllegalArgumentException("Unmatched quote in format: " + format);
        }
        if (inOptional) { // i.e. we have not found the end of the literal
            throw new IllegalArgumentException("Unmatched optional in format: " + format);
        }
        return list.toArray(Token.EMPTY_ARRAY);
    }

    /**
     * Converts a {@code long} to a {@link String} with optional
     * zero padding.
     *
     * @param value the value to convert
     * @param padWithZeros whether to pad with zeroes
     * @param count the size to pad to (ignored if {@code padWithZeros} is false)
     * @return the string result
     */
    private static String paddedValue(final long value, final boolean padWithZeros, final int count) {
        final String longString = Long.toString(value);
        return padWithZeros ? StringUtils.leftPad(longString, count, '0') : longString;
    }

    /**
     * DurationFormatUtils instances should NOT be constructed in standard programming.
     *
     * <p>This constructor is public to permit tools that require a JavaBean instance
     * to operate.</p>
     *
     * @deprecated TODO Make private in 4.0.
     */
    @Deprecated
    public DurationFormatUtils() {
        // empty
    }

}
