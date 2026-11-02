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
import java.util.Objects;
import java.util.TimeZone;
import java.util.stream.Stream;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.Strings;
import org.apache.commons.lang3.Validate;

/**
 * Duration formatting utilities and constants. The following table describes the tokens
 * used in the pattern language for formatting.
 *
 * <table>
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
 *  <tr><td>''</td><td>literal single quote (apostrophe)</td></tr>
 * </table>
 *
 * A literal single quote is represented by a pair of consecutive single quotes ({@code ''}).
 * <p>
 * Token values are printed using decimal digits.
 * A token character can be repeated to ensure that the field occupies a certain minimum
 * size. Values will be left-padded with 0 unless padding is disabled in the method invocation.
 * </p>
 * <p>
 * Tokens can be marked as optional by surrounding them with brackets [ ]. These tokens will
 * only be printed if the token value is non-zero. Literals within optional blocks will only be
 * printed if the preceding non-literal token is non-zero. Leading optional literals will only
 * be printed if the following non-literal is non-zero.
 * Multiple optional blocks can be used to group literals with the desired token.
 * </p>
 * <p>
 * Notes on Optional Tokens:
 * </p>
 * <p>
 * <strong>Multiple optional tokens without literals can result in impossible to understand output.</strong>
 * </p>
 * <p>
 * <strong>Patterns where all tokens are optional can produce empty strings.</strong>
 * </p>
 * <p>
 * See the following examples.
 * </p>
 *
 * <table>
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
 * <p>
 * <strong>Note: Optional blocks cannot be nested.</strong>
 * </p>
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
         * Determines whether a set of tokens contains a duration unit.
         *
         * @param tokens set to search.
         * @param value duration unit to find.
         * @return boolean {@code true} if contained.
         */
        static boolean containsTokenWithValue(final Token[] tokens, final Unit value) {
            return Stream.of(tokens).anyMatch(token -> token.getValue() == value);
        }

        private final Unit value;
        private final StringBuilder literal;
        private int count;
        private final int optionalIndex;

        /**
         * Wraps a token around literal text.
         *
         * @param literal literal text to wrap, non-null.
         * @param optional whether the token is optional.
         * @param optionalIndex The index of the optional token within the pattern.
         */
        Token(final StringBuilder literal, final boolean optional, final int optionalIndex) {
            this.value = null;
            this.literal = Objects.requireNonNull(literal, "literal");
            this.count = 1;
            this.optionalIndex = optional ? optionalIndex : -1;
        }

        /**
         * Wraps a token around a duration unit.
         *
         * @param value duration unit to wrap, non-null.
         * @param optional whether the token is optional.
         * @param optionalIndex The index of the optional token within the pattern.
         */
        Token(final Unit value, final boolean optional, final int optionalIndex) {
            this.value = Objects.requireNonNull(value, "value");
            this.literal = null;
            this.count = 1;
            this.optionalIndex = optional ? optionalIndex : -1;
        }

        /**
         * Supports equality of this Token to another Token.
         *
         * @param obj2 Object to consider equality of.
         * @return boolean {@code true} if equal.
         */
        @Override
        public boolean equals(final Object obj2) {
            if (obj2 instanceof Token) {
                final Token tok2 = (Token) obj2;
                return value == tok2.value && count == tok2.count
                        && Objects.equals(literal == null ? null : literal.toString(), tok2.literal == null ? null : tok2.literal.toString());
            }
            return false;
        }

        /**
         * Gets the current number of values represented.
         *
         * @return int number of values represented.
         */
        int getCount() {
            return count;
        }

        /**
         * Gets the duration unit this token represents.
         *
         * @return the duration unit, or null for a literal token.
         */
        Unit getValue() {
            return value;
        }

        /**
         * Computes a hash code from the duration unit or literal text, excluding the repetition count.
         *
         * @return The hash code for the token.
         */
        @Override
        public int hashCode() {
            return value != null ? value.hashCode() : literal.toString().hashCode();
        }

        /**
         * Adds another one of the value.
         */
        void increment() {
            count++;
        }

        /**
         * Represents this token as a String.
         *
         * @return String representation of the token.
         */
        @Override
        public String toString() {
            return StringUtils.repeat(value != null ? value.toString() : literal.toString(), count);
        }
    }

    /**
     * Duration units recognized in format patterns.
     */
    enum Unit {
        YEAR("y"), MONTH("M"), DAY("d"), HOUR("H"), MINUTE("m"), SECOND("s"), MILLISECOND("S");

        private final String symbol;

        Unit(final String symbol) {
            this.symbol = symbol;
        }

        @Override
        public String toString() {
            return symbol;
        }
    }

    private static final int MINUTES_PER_HOUR = 60;

    private static final int SECONDS_PER_MINUTES = 60;

    private static final int HOURS_PER_DAY = 24;

    /**
     * Pattern used with {@link FastDateFormat} and {@link SimpleDateFormat} for the ISO 8601 period format used in durations.
     *
     * @see org.apache.commons.lang3.time.FastDateFormat
     * @see java.text.SimpleDateFormat
     */
    public static final String ISO_EXTENDED_FORMAT_PATTERN = "'P'yyyy'Y'M'M'd'DT'H'H'm'M's.SSS'S'";

    /**
     * The internal method to do the formatting.
     *
     * @param tokens  The tokens.
     * @param years  The number of years.
     * @param months  The number of months.
     * @param days  The number of days.
     * @param hours  The number of hours.
     * @param minutes  The number of minutes.
     * @param seconds  The number of seconds.
     * @param milliseconds  The number of milliseconds.
     * @param padWithZeros  whether to pad.
     * @return The formatted string.
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
            final Unit unit = token.getValue();
            final boolean isLiteral = token.literal != null;
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
                    buffer.append(token.literal);
                }
            } else {
                final long value;
                switch (unit) {
                case YEAR:
                    value = years;
                    break;
                case MONTH:
                    value = months;
                    break;
                case DAY:
                    value = days;
                    break;
                case HOUR:
                    value = hours;
                    break;
                case MINUTE:
                    value = minutes;
                    break;
                case SECOND:
                    value = seconds;
                    break;
                case MILLISECOND:
                    value = milliseconds;
                    break;
                default:
                    throw new IllegalStateException("Unexpected duration unit: " + unit);
                }
                lastOutputZero = value == 0;
                if (!inOptional || !lastOutputZero) {
                    if (unit == Unit.MILLISECOND && lastOutputSeconds) {
                        // ensure at least 3 digits are displayed even if padding is not selected
                        final int width = padWithZeros ? Math.max(3, count) : 3;
                        buffer.append(paddedValue(value, true, width));
                    } else {
                        buffer.append(paddedValue(value, padWithZeros, count));
                    }
                }
                lastOutputSeconds = unit == Unit.SECOND;
            }
            // as soon as we hit first nonliteral in optional, check for literal prefix
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
     * <p>
     * This method formats durations using the days and lower fields of the format pattern. Months and larger are not used.
     * </p>
     *
     * @param durationMillis The duration to format.
     * @param format         The way in which to format the duration, not null.
     * @return The formatted duration, not null.
     * @throws IllegalArgumentException Thrown if durationMillis is negative.
     */
    public static String formatDuration(final long durationMillis, final String format) {
        return formatDuration(durationMillis, format, true);
    }

    /**
     * Formats the time gap as a string, using the specified format. Padding the left-hand side of numbers with zeroes is optional.
     * <p>
     * This method formats durations using the days and lower fields of the format pattern. Months and larger are not used.
     * </p>
     *
     * @param durationMillis The duration to format.
     * @param format         The way in which to format the duration, not null.
     * @param padWithZeros   whether to pad the left-hand side of numbers with 0's.
     * @return The formatted duration, not null.
     * @throws IllegalArgumentException Thrown if durationMillis is negative.
     */
    public static String formatDuration(final long durationMillis, final String format, final boolean padWithZeros) {
        Validate.inclusiveBetween(0, Long.MAX_VALUE, durationMillis, "durationMillis must not be negative");
        final Token[] tokens = lexx(format);
        long days = 0;
        long hours = 0;
        long minutes = 0;
        long seconds = 0;
        long milliseconds = durationMillis;
        if (Token.containsTokenWithValue(tokens, Unit.DAY)) {
            days = milliseconds / DateUtils.MILLIS_PER_DAY;
            milliseconds -= days * DateUtils.MILLIS_PER_DAY;
        }
        if (Token.containsTokenWithValue(tokens, Unit.HOUR)) {
            hours = milliseconds / DateUtils.MILLIS_PER_HOUR;
            milliseconds -= hours * DateUtils.MILLIS_PER_HOUR;
        }
        if (Token.containsTokenWithValue(tokens, Unit.MINUTE)) {
            minutes = milliseconds / DateUtils.MILLIS_PER_MINUTE;
            milliseconds -= minutes * DateUtils.MILLIS_PER_MINUTE;
        }
        if (Token.containsTokenWithValue(tokens, Unit.SECOND)) {
            seconds = milliseconds / DateUtils.MILLIS_PER_SECOND;
            milliseconds -= seconds * DateUtils.MILLIS_PER_SECOND;
        }
        return format(tokens, 0, 0, days, hours, minutes, seconds, milliseconds, padWithZeros);
    }

    /**
     * Formats the time gap as a string.
     * <p>
     * The format used is ISO 8601-like: {@code HH:mm:ss.SSS}.
     * </p>
     *
     * @param durationMillis The duration to format.
     * @return The formatted duration, not null.
     * @throws IllegalArgumentException Thrown if durationMillis is negative.
     */
    public static String formatDurationHMS(final long durationMillis) {
        return formatDuration(durationMillis, "HH:mm:ss.SSS");
    }

    /**
     * Formats the time gap as a string.
     * <p>
     * The format used is the ISO 8601 period format.
     * </p>
     * <p>
     * This method formats durations using the days and lower fields of the ISO format pattern, such as P7D6TH5M4.321S.
     * </p>
     *
     * @param durationMillis The duration to format.
     * @return The formatted duration, not null.
     * @throws IllegalArgumentException Thrown if durationMillis is negative.
     */
    public static String formatDurationISO(final long durationMillis) {
        return formatDuration(durationMillis, ISO_EXTENDED_FORMAT_PATTERN, false);
    }

    /**
     * Formats an elapsed time into a pluralization correct string.
     * <p>
     * This method formats durations using the days and lower fields of the format pattern. Months and larger are not used.
     * </p>
     *
     * @param durationMillis               The elapsed time to report in milliseconds.
     * @param suppressLeadingZeroElements  suppresses leading 0 elements.
     * @param suppressTrailingZeroElements suppresses trailing 0 elements.
     * @return The formatted text in days/hours/minutes/seconds, not null.
     * @throws IllegalArgumentException Thrown if durationMillis is negative.
     */
    public static String formatDurationWords(final long durationMillis, final boolean suppressLeadingZeroElements, final boolean suppressTrailingZeroElements) {
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
     * Formats the time gap as a string, using the specified format. Padding the left-hand side of numbers with zeroes is optional.
     *
     * @param startMillis The start of the duration.
     * @param endMillis   The end of the duration.
     * @param format      The way in which to format the duration, not null.
     * @return The formatted duration, not null.
     * @throws IllegalArgumentException Thrown if startMillis is greater than endMillis.
     */
    public static String formatPeriod(final long startMillis, final long endMillis, final String format) {
        return formatPeriod(startMillis, endMillis, format, true, TimeZone.getDefault());
    }

    /**
     * <p>
     * Formats the time gap as a string, using the specified format. Padding the left-hand side of numbers with zeroes is optional and the time zone may be
     * specified.
     * <p>
     * When calculating the difference between months/days, it chooses to calculate months first, borrowing the length of the month in which the period starts
     * when the number of days would otherwise be negative. So when working out the number of months and days between January 15th and March 10th, it chooses
     * 1 month and 26 days gained by choosing March -&gt; February = 1 month and then calculating days backwards, and not the 1 month and 23 days gained by
     * choosing January-&gt;February = 1 month and then calculating days forwards, as computed by {@link java.time.Period#between}.
     * </p>
     * <p>
     * For more control, the <a href="https://www.joda.org/joda-time/">Joda-Time</a> library is recommended.
     * </p>
     *
     * @param startMillis  The start of the duration.
     * @param endMillis    The end of the duration.
     * @param format       The way in which to format the duration, not null.
     * @param padWithZeros whether to pad the left-hand side of numbers with 0's.
     * @param timezone     The millis are defined in.
     * @return The formatted duration, not null.
     * @throws IllegalArgumentException Thrown if startMillis is greater than endMillis.
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
        long seconds = end.get(Calendar.SECOND) - start.get(Calendar.SECOND);
        long minutes = end.get(Calendar.MINUTE) - start.get(Calendar.MINUTE);
        long hours = end.get(Calendar.HOUR_OF_DAY) - start.get(Calendar.HOUR_OF_DAY);
        final boolean calendarUnits = Token.containsTokenWithValue(tokens, Unit.YEAR) || Token.containsTokenWithValue(tokens, Unit.MONTH);
        // Without years or months, count local dates directly instead of visiting every intervening month.
        long days = calendarUnits ? end.get(Calendar.DAY_OF_MONTH) - start.get(Calendar.DAY_OF_MONTH) : localEpochDay(end) - localEpochDay(start);
        long months = end.get(Calendar.MONTH) - start.get(Calendar.MONTH);
        long years = end.get(Calendar.YEAR) - start.get(Calendar.YEAR);
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
        if (calendarUnits) {
            while (days < 0) {
                days += start.getActualMaximum(Calendar.DAY_OF_MONTH);
                months -= 1;
                start.add(Calendar.MONTH, 1);
            }
            while (months < 0) {
                months += 12;
                years -= 1;
            }
            if (!Token.containsTokenWithValue(tokens, Unit.YEAR) && years != 0) {
                while (years != 0) {
                    months += 12 * years;
                    years = 0;
                }
            }
            if (!Token.containsTokenWithValue(tokens, Unit.MONTH)) {
                while (months > 0) {
                    days += start.getActualMaximum(Calendar.DAY_OF_MONTH);
                    months -= 1;
                    start.add(Calendar.MONTH, 1);
                }
            }
        } else {
            months = 0;
            years = 0;
        }
        // The rest of this code adds in values that
        // aren't requested. This allows the user to ask for the
        // number of months and get the real count and not just 0->11.
        if (!Token.containsTokenWithValue(tokens, Unit.DAY)) {
            hours += HOURS_PER_DAY * days;
            days = 0;
        }
        if (!Token.containsTokenWithValue(tokens, Unit.HOUR)) {
            minutes += MINUTES_PER_HOUR * hours;
            hours = 0;
        }
        if (!Token.containsTokenWithValue(tokens, Unit.MINUTE)) {
            seconds += SECONDS_PER_MINUTES * minutes;
            minutes = 0;
        }
        if (!Token.containsTokenWithValue(tokens, Unit.SECOND)) {
            milliseconds += DateUtils.MILLIS_PER_SECOND * seconds;
            seconds = 0;
        }
        return format(tokens, years, months, days, hours, minutes, seconds, milliseconds, padWithZeros);
    }

    /**
     * Formats the time gap as a string.
     * <p>
     * The format used is the ISO 8601 period format.
     * </p>
     *
     * @param startMillis The start of the duration to format.
     * @param endMillis   The end of the duration to format.
     * @return The formatted duration, not null.
     * @throws IllegalArgumentException Thrown if startMillis is greater than endMillis.
     */
    public static String formatPeriodISO(final long startMillis, final long endMillis) {
        return formatPeriod(startMillis, endMillis, ISO_EXTENDED_FORMAT_PATTERN, false, TimeZone.getDefault());
    }

    /**
     * Parses a classic date format string into Tokens
     *
     * @param format  The format to parse, not null.
     * @return array of Token[].
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
            Unit value = null;
            switch (ch) {
            case '[':
                if (inOptional) {
                    throw new IllegalArgumentException("Nested optional block at index: " + i);
                }
                optionalIndex++;
                inOptional = true;
                previous = null;
                break;
            case ']':
                if (!inOptional) {
                    throw new IllegalArgumentException("Attempting to close unopened optional block at index: " + i);
                }
                inOptional = false;
                previous = null;
                break;
            case '\'':
                if (inLiteral) {
                    if (i + 1 < format.length() && format.charAt(i + 1) == '\'') {
                        // escaped quote '' ? append literal apostrophe, stay in literal
                        buffer.append('\'');
                        i++;
                    } else {
                        // end of literal
                        buffer = null;
                        inLiteral = false;
                    }
                } else if (i + 1 < format.length() && format.charAt(i + 1) == '\'') {
                    // standalone '' outside a literal ? emit a single apostrophe
                    buffer = new StringBuilder("'");
                    list.add(new Token(buffer, inOptional, optionalIndex));
                    buffer = null;
                    i++;
                } else {
                    buffer = new StringBuilder();
                    list.add(new Token(buffer, inOptional, optionalIndex));
                    inLiteral = true;
                }
                previous = null;
                break;
            case 'y':
                value = Unit.YEAR;
                break;
            case 'M':
                value = Unit.MONTH;
                break;
            case 'd':
                value = Unit.DAY;
                break;
            case 'H':
                value = Unit.HOUR;
                break;
            case 'm':
                value = Unit.MINUTE;
                break;
            case 's':
                value = Unit.SECOND;
                break;
            case 'S':
                value = Unit.MILLISECOND;
                break;
            default:
                if (buffer == null) {
                    buffer = new StringBuilder();
                    list.add(new Token(buffer, inOptional, optionalIndex));
                }
                buffer.append(ch);
                previous = null;
            }
            if (value != null) {
                if (previous != null && previous.getValue() == value) {
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
     * Computes the local epoch day without overflowing at either end of the millisecond range.
     *
     * @param calendar the calendar to convert.
     * @return the local epoch day.
     */
    private static long localEpochDay(final Calendar calendar) {
        final long millis = calendar.getTimeInMillis();
        final long offset = (long) calendar.get(Calendar.ZONE_OFFSET) + calendar.get(Calendar.DST_OFFSET);
        return Math.floorDiv(millis, DateUtils.MILLIS_PER_DAY)
                + Math.floorDiv(Math.floorMod(millis, DateUtils.MILLIS_PER_DAY) + offset, DateUtils.MILLIS_PER_DAY);
    }

    /**
     * Converts a {@code long} to a {@link String} with optional zero padding.
     *
     * @param value        The value to convert.
     * @param padWithZeros whether to pad with zeroes.
     * @param count        The size to pad to (ignored if {@code padWithZeros} is false).
     * @return The string result.
     */
    private static String paddedValue(final long value, final boolean padWithZeros, final int count) {
        final String longString = Long.toString(value);
        return padWithZeros ? StringUtils.leftPad(longString, count, '0') : longString;
    }

    /**
     * DurationFormatUtils instances should NOT be constructed in standard programming.
     * <p>
     * This constructor is public to permit tools that require a JavaBean instance to operate.
     * </p>
     *
     * @deprecated TODO Make private in 4.0.
     */
    @Deprecated
    public DurationFormatUtils() {
        // empty
    }

}
