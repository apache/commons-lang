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

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.text.DateFormatSymbols;
import java.text.ParseException;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TimeZone;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import org.apache.commons.lang3.ArraySorter;
import org.apache.commons.lang3.CharUtils;
import org.apache.commons.lang3.LocaleUtils;

/**
 * FastDateParser is a fast and thread-safe version of {@link java.text.SimpleDateFormat}.
 *
 * <p>
 * To obtain a proxy to a FastDateParser, use {@link FastDateFormat#getInstance(String, TimeZone, Locale)} or another variation of the factory methods of
 * {@link FastDateFormat}.
 * </p>
 *
 * <p>
 * Since FastDateParser is thread safe, you can use a static member instance:
 * </p>
 * {@code
 *     private static final DateParser DATE_PARSER = FastDateFormat.getInstance("yyyy-MM-dd");
 * }
 *
 * <p>
 * This class can be used as a direct replacement for {@link SimpleDateFormat} in most parsing situations. This class is especially useful in multi-threaded
 * server environments. {@link SimpleDateFormat} is not thread-safe in any JDK version, nor will it be as Sun has closed the
 * <a href="https://bugs.openjdk.org/browse/JDK-4228335">bug</a>/RFE.
 * </p>
 *
 * <p>
 * Only parsing is supported by this class, but all patterns are compatible with SimpleDateFormat.
 * </p>
 *
 * <p>
 * The class operates in lenient mode, so for example a time of 90 minutes is treated as 1 hour 30 minutes.
 * </p>
 *
 * <p>
 * Timing tests indicate this class is as about as fast as SimpleDateFormat in single thread applications and about 25% faster in multi-thread applications.
 * </p>
 *
 * @since 3.2
 * @see FastDatePrinter
 */
public class FastDateParser implements DateParser, Serializable {

    /**
     * A strategy that handles a text field in the parsing pattern
     */
    private static final class CaseInsensitiveTextStrategy extends PatternStrategy {

        private final int field;
        private final Locale locale;
        private final Map<String, Integer> lKeyValues;

        /**
         * Constructs a Strategy that parses a Text field
         *
         * @param field            The Calendar field
         * @param definingCalendar The Calendar to use
         * @param locale           The Locale to use
         */
        CaseInsensitiveTextStrategy(final int field, final Calendar definingCalendar, final Locale locale) {
            this.field = field;
            this.locale = LocaleUtils.toLocale(locale);

            final StringBuilder regex = new StringBuilder();
            regex.append("((?iu)");
            lKeyValues = appendDisplayNames(definingCalendar, locale, field, regex);
            regex.setLength(regex.length() - 1);
            regex.append(")");
            createPattern(regex);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        void setCalendar(final FastDateParser parser, final Calendar calendar, final String value) {
            final String lowerCase = value.toLowerCase(locale);
            Integer iVal = lKeyValues.get(lowerCase);
            if (iVal == null) {
                // match missing the optional trailing period
                iVal = lKeyValues.get(lowerCase + '.');
            }
            // LANG-1669: Mimic fix done in OpenJDK 17 to resolve issue with parsing newly supported day periods added in OpenJDK 16
            if (Calendar.AM_PM != this.field || iVal <= 1) {
                calendar.set(field, iVal.intValue());
            }
        }

        /**
         * Converts this instance to a handy debug string.
         *
         * @since 3.12.0
         */
        @Override
        public String toString() {
            return "CaseInsensitiveTextStrategy [field=" + field + ", locale=" + locale + ", lKeyValues=" + lKeyValues + ", pattern=" + pattern + "]";
        }
    }

    /**
     * A strategy that copies the static or quoted field in the parsing pattern
     */
    private static final class CopyQuotedStrategy extends Strategy {

        private final String formatField;

        /**
         * Constructs a Strategy that ensures the formatField has literal text
         *
         * @param formatField The literal text to match
         */
        CopyQuotedStrategy(final String formatField) {
            this.formatField = formatField;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        boolean isNumber() {
            return false;
        }

        @Override
        boolean parse(final FastDateParser parser, final Calendar calendar, final String source, final ParsePosition pos, final int maxWidth) {
            for (int idx = 0; idx < formatField.length(); ++idx) {
                final int sIdx = idx + pos.getIndex();
                if (sIdx == source.length()) {
                    pos.setErrorIndex(sIdx);
                    return false;
                }
                if (formatField.charAt(idx) != source.charAt(sIdx)) {
                    pos.setErrorIndex(sIdx);
                    return false;
                }
            }
            pos.setIndex(formatField.length() + pos.getIndex());
            return true;
        }

        /**
         * Converts this instance to a handy debug string.
         *
         * @since 3.12.0
         */
        @Override
        public String toString() {
            return "CopyQuotedStrategy [formatField=" + formatField + "]";
        }
    }

    private static final class ISO8601TimeZoneStrategy extends PatternStrategy {
        // Z, +hh, -hh, +hhmm, -hhmm, +hh:mm or -hh:mm

        private static final Strategy ISO_8601_1_STRATEGY = new ISO8601TimeZoneStrategy("(Z|(?:[+-]\\d{2}))");

        private static final Strategy ISO_8601_2_STRATEGY = new ISO8601TimeZoneStrategy("(Z|(?:[+-]\\d{2}\\d{2}))");

        private static final Strategy ISO_8601_3_STRATEGY = new ISO8601TimeZoneStrategy("(Z|(?:[+-]\\d{2}(?::)\\d{2}))");
        /**
         * Factory method for ISO8601TimeZoneStrategies.
         *
         * @param tokenLen a token indicating the length of the TimeZone String to be formatted.
         * @return a ISO8601TimeZoneStrategy that can format TimeZone String of length {@code tokenLen}. If no such strategy exists, an IllegalArgumentException
         *         will be thrown.
         */
        static Strategy getStrategy(final int tokenLen) {
            switch (tokenLen) {
            case 1:
                return ISO_8601_1_STRATEGY;
            case 2:
                return ISO_8601_2_STRATEGY;
            case 3:
                return ISO_8601_3_STRATEGY;
            default:
                throw new IllegalArgumentException("invalid number of X");
            }
        }
        /**
         * Constructs a Strategy that parses a TimeZone
         *
         * @param pattern The Pattern
         */
        ISO8601TimeZoneStrategy(final String pattern) {
            createPattern(pattern);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        void setCalendar(final FastDateParser parser, final Calendar calendar, final String value) {
            calendar.setTimeZone(FastTimeZone.getGmtTimeZone(value));
        }
    }

    /**
     * A strategy that handles a number field in the parsing pattern
     */
    private static class NumberStrategy extends Strategy {

        private final int field;

        /**
         * Constructs a Strategy that parses a Number field
         *
         * @param field The Calendar field
         */
        NumberStrategy(final int field) {
            this.field = field;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        boolean isNumber() {
            return true;
        }

        /**
         * Make any modifications to parsed integer
         *
         * @param parser The parser
         * @param iValue The parsed integer
         * @return The modified value
         */
        int modify(final FastDateParser parser, final int iValue) {
            return iValue;
        }

        @Override
        boolean parse(final FastDateParser parser, final Calendar calendar, final String source, final ParsePosition pos, final int maxWidth) {
            int idx = pos.getIndex();
            int last = source.length();

            if (maxWidth == 0) {
                // if no maxWidth, strip leading white space
                for (; idx < last; ++idx) {
                    final char c = source.charAt(idx);
                    if (!Character.isWhitespace(c)) {
                        break;
                    }
                }
                pos.setIndex(idx);
            } else {
                final int end = idx + maxWidth;
                if (last > end) {
                    last = end;
                }
            }

            for (; idx < last; ++idx) {
                final char c = source.charAt(idx);
                if (!Character.isDigit(c)) {
                    break;
                }
            }

            if (pos.getIndex() == idx) {
                pos.setErrorIndex(idx);
                return false;
            }

            final int value = Integer.parseInt(source.substring(pos.getIndex(), idx));
            pos.setIndex(idx);

            calendar.set(field, modify(parser, value));
            return true;
        }

        /**
         * Converts this instance to a handy debug string.
         *
         * @since 3.12.0
         */
        @Override
        public String toString() {
            return "NumberStrategy [field=" + field + "]";
        }
    }

    /**
     * A strategy to parse a single field from the parsing pattern
     */
    private abstract static class PatternStrategy extends Strategy {

        Pattern pattern;

        void createPattern(final String regex) {
            this.pattern = Pattern.compile(regex);
        }

        void createPattern(final StringBuilder regex) {
            createPattern(regex.toString());
        }

        /**
         * Is this field a number? The default implementation returns false.
         *
         * @return true, if field is a number
         */
        @Override
        boolean isNumber() {
            return false;
        }

        @Override
        boolean parse(final FastDateParser parser, final Calendar calendar, final String source, final ParsePosition pos, final int maxWidth) {
            final Matcher matcher = pattern.matcher(source.substring(pos.getIndex()));
            if (!matcher.lookingAt()) {
                pos.setErrorIndex(pos.getIndex());
                return false;
            }
            pos.setIndex(pos.getIndex() + matcher.end(1));
            setCalendar(parser, calendar, matcher.group(1));
            return true;
        }

        abstract void setCalendar(FastDateParser parser, Calendar calendar, String value);

        /**
         * Converts this instance to a handy debug string.
         *
         * @since 3.12.0
         */
        @Override
        public String toString() {
            return getClass().getSimpleName() + " [pattern=" + pattern + "]";
        }

    }

    /**
     * A strategy to parse a single field from the parsing pattern
     */
    private abstract static class Strategy {

        /**
         * Is this field a number? The default implementation returns false.
         *
         * @return true, if field is a number
         */
        boolean isNumber() {
            return false;
        }

        abstract boolean parse(FastDateParser parser, Calendar calendar, String source, ParsePosition pos, int maxWidth);
    }

    /**
     * Holds strategy and field width
     */
    private static final class StrategyAndWidth {

        final Strategy strategy;
        final int width;

        StrategyAndWidth(final Strategy strategy, final int width) {
            this.strategy = Objects.requireNonNull(strategy, "strategy");
            this.width = width;
        }

        int getMaxWidth(final ListIterator<StrategyAndWidth> lt) {
            if (!strategy.isNumber() || !lt.hasNext()) {
                return 0;
            }
            final Strategy nextStrategy = lt.next().strategy;
            lt.previous();
            return nextStrategy.isNumber() ? width : 0;
        }

        @Override
        public String toString() {
            return "StrategyAndWidth [strategy=" + strategy + ", width=" + width + "]";
        }
    }

    /**
     * Parse format into Strategies
     */
    private final class StrategyParser {
        private final Calendar definingCalendar;
        private int currentIdx;

        StrategyParser(final Calendar definingCalendar) {
            this.definingCalendar = Objects.requireNonNull(definingCalendar, "definingCalendar");
        }

        StrategyAndWidth getNextStrategy() {
            if (currentIdx >= pattern.length()) {
                return null;
            }
            final char c = pattern.charAt(currentIdx);
            if (CharUtils.isAsciiAlpha(c)) {
                return letterPattern(c);
            }
            return literal();
        }

        private StrategyAndWidth letterPattern(final char c) {
            final int begin = currentIdx;
            while (++currentIdx < pattern.length()) {
                if (pattern.charAt(currentIdx) != c) {
                    break;
                }
            }
            final int width = currentIdx - begin;
            return new StrategyAndWidth(getStrategy(c, width, definingCalendar), width);
        }

        private StrategyAndWidth literal() {
            boolean activeQuote = false;

            final StringBuilder sb = new StringBuilder();
            while (currentIdx < pattern.length()) {
                final char c = pattern.charAt(currentIdx);
                if (!activeQuote && CharUtils.isAsciiAlpha(c)) {
                    break;
                }
                if (c == '\'' && (++currentIdx == pattern.length() || pattern.charAt(currentIdx) != '\'')) {
                    activeQuote = !activeQuote;
                    continue;
                }
                ++currentIdx;
                sb.append(c);
            }
            if (activeQuote) {
                throw new IllegalArgumentException("Unterminated quote");
            }
            final String formatField = sb.toString();
            return new StrategyAndWidth(new CopyQuotedStrategy(formatField), formatField.length());
        }
    }

    /**
     * A strategy that handles a time zone field in the parsing pattern
     */
    static class TimeZoneStrategy extends PatternStrategy {
        private static final class TzInfo {
            final TimeZone zone;
            final int dstOffset;

            TzInfo(final TimeZone tz, final boolean useDst) {
                zone = tz;
                dstOffset = useDst ? tz.getDSTSavings() : 0;
            }

            @Override
            public String toString() {
                return "TzInfo [zone=" + zone + ", dstOffset=" + dstOffset + "]";
            }
        }
        private static final String RFC_822_TIME_ZONE = "[+-]\\d{4}";

        private static final String GMT_OPTION = TimeZones.GMT_ID + "[+-]\\d{1,2}:\\d{2}";

        /**
         * Index of zone id from {@link DateFormatSymbols#getZoneStrings()}.
         */
        private static final int ID = 0;

        private final Locale locale;

        /**
         * Using lower case only or upper case only will cause problems with some Locales like Turkey, Armenia, Colognian and also depending on the Java
         * version. For details, see https://garygregory.wordpress.com/2015/11/03/java-lowercase-conversion-turkey/
         */
        private final Map<String, TzInfo> tzNames = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);

        /**
         * Constructs a Strategy that parses a TimeZone
         *
         * @param locale The Locale
         */
        TimeZoneStrategy(final Locale locale) {
            this.locale = LocaleUtils.toLocale(locale);

            final StringBuilder sb = new StringBuilder();
            sb.append("((?iu)" + RFC_822_TIME_ZONE + "|" + GMT_OPTION);

            final Set<String> sorted = new TreeSet<>(LONGER_FIRST_LOWERCASE);

            // Order is undefined.
            // TODO Use of getZoneStrings() is discouraged per its Javadoc.
            final String[][] zones = DateFormatSymbols.getInstance(locale).getZoneStrings();
            for (final String[] zoneNames : zones) {
                // offset 0 is the time zone ID and is not localized
                final String tzId = zoneNames[ID];
                if (tzId.equalsIgnoreCase(TimeZones.GMT_ID)) {
                    continue;
                }
                final TimeZone tz = TimeZone.getTimeZone(tzId);
                // offset 1 is long standard name
                // offset 2 is short standard name
                final TzInfo standard = new TzInfo(tz, false);
                TzInfo tzInfo = standard;
                for (int i = 1; i < zoneNames.length; ++i) {
                    switch (i) {
                    case 3: // offset 3 is long daylight savings (or summertime) name
                            // offset 4 is the short summertime name
                        tzInfo = new TzInfo(tz, true);
                        break;
                    case 5: // offset 5 starts additional names, probably standard time
                        tzInfo = standard;
                        break;
                    default:
                        break;
                    }
                    final String zoneName = zoneNames[i];
                    // ignore the data associated with duplicates supplied in the additional names
                    if (zoneName != null && sorted.add(zoneName)) {
                        tzNames.put(zoneName, tzInfo);
                    }
                }
            }
            // Order is undefined.
            for (final String tzId : ArraySorter.sort(TimeZone.getAvailableIDs())) {
                if (tzId.equalsIgnoreCase(TimeZones.GMT_ID)) {
                    continue;
                }
                final TimeZone tz = TimeZone.getTimeZone(tzId);
                final String zoneName = tz.getDisplayName(locale);
                if (sorted.add(zoneName)) {
                    tzNames.put(zoneName, new TzInfo(tz, tz.observesDaylightTime()));
                }
            }
            // order the regex alternatives with longer strings first, greedy
            // match will ensure the longest string will be consumed
            sorted.forEach(zoneName -> simpleQuote(sb.append('|'), zoneName));
            sb.append(")");
            createPattern(sb);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        void setCalendar(final FastDateParser parser, final Calendar calendar, final String timeZone) {
            final TimeZone tz = FastTimeZone.getGmtTimeZone(timeZone);
            if (tz != null) {
                calendar.setTimeZone(tz);
            } else {
                TzInfo tzInfo = tzNames.get(timeZone);
                if (tzInfo == null) {
                    // match missing the optional trailing period
                    tzInfo = tzNames.get(timeZone + '.');
                    if (tzInfo == null) {
                        // show chars in case this is multiple byte character issue
                        final char[] charArray = timeZone.toCharArray();
                        throw new IllegalStateException(String.format("Can't find time zone '%s' (%d %s) in %s", timeZone, charArray.length,
                                Arrays.toString(charArray), new TreeSet<>(tzNames.keySet())));
                    }
                }
                calendar.set(Calendar.DST_OFFSET, tzInfo.dstOffset);
                calendar.set(Calendar.ZONE_OFFSET, tzInfo.zone.getRawOffset());
            }
        }

        /**
         * Converts this instance to a handy debug string.
         *
         * @since 3.12.0
         */
        @Override
        public String toString() {
            return "TimeZoneStrategy [locale=" + locale + ", tzNames=" + tzNames + ", pattern=" + pattern + "]";
        }

    }

    /**
     * Required for serialization support.
     *
     * @see java.io.Serializable
     */
    private static final long serialVersionUID = 3L;

    static final Locale JAPANESE_IMPERIAL = new Locale("ja", "JP", "JP");

    /**
     * comparator used to sort regex alternatives. Alternatives should be ordered longer first, and shorter last. ('february' before 'feb'). All entries must be
     * lower-case by locale.
     */
    private static final Comparator<String> LONGER_FIRST_LOWERCASE = Comparator.reverseOrder();

    // helper classes to parse the format string

    @SuppressWarnings("unchecked") // OK because we are creating an array with no entries
    private static final ConcurrentMap<Locale, Strategy>[] CACHES = new ConcurrentMap[Calendar.FIELD_COUNT];

    private static final Strategy ABBREVIATED_YEAR_STRATEGY = new NumberStrategy(Calendar.YEAR) {
        /**
         * {@inheritDoc}
         */
        @Override
        int modify(final FastDateParser parser, final int iValue) {
            return iValue < 100 ? parser.adjustYear(iValue) : iValue;
        }
    };

    private static final Strategy NUMBER_MONTH_STRATEGY = new NumberStrategy(Calendar.MONTH) {
        @Override
        int modify(final FastDateParser parser, final int iValue) {
            return iValue - 1;
        }
    };

    private static final Strategy LITERAL_YEAR_STRATEGY = new NumberStrategy(Calendar.YEAR);

    private static final Strategy WEEK_OF_YEAR_STRATEGY = new NumberStrategy(Calendar.WEEK_OF_YEAR);

    private static final Strategy WEEK_OF_MONTH_STRATEGY = new NumberStrategy(Calendar.WEEK_OF_MONTH);

    private static final Strategy DAY_OF_YEAR_STRATEGY = new NumberStrategy(Calendar.DAY_OF_YEAR);

    private static final Strategy DAY_OF_MONTH_STRATEGY = new NumberStrategy(Calendar.DAY_OF_MONTH);

    private static final Strategy DAY_OF_WEEK_STRATEGY = new NumberStrategy(Calendar.DAY_OF_WEEK) {
        @Override
        int modify(final FastDateParser parser, final int iValue) {
            return iValue == 7 ? Calendar.SUNDAY : iValue + 1;
        }
    };

    private static final Strategy DAY_OF_WEEK_IN_MONTH_STRATEGY = new NumberStrategy(Calendar.DAY_OF_WEEK_IN_MONTH);

    private static final Strategy HOUR_OF_DAY_STRATEGY = new NumberStrategy(Calendar.HOUR_OF_DAY);

    private static final Strategy HOUR24_OF_DAY_STRATEGY = new NumberStrategy(Calendar.HOUR_OF_DAY) {
        @Override
        int modify(final FastDateParser parser, final int iValue) {
            return iValue == 24 ? 0 : iValue;
        }
    };

    private static final Strategy HOUR12_STRATEGY = new NumberStrategy(Calendar.HOUR) {
        @Override
        int modify(final FastDateParser parser, final int iValue) {
            return iValue == 12 ? 0 : iValue;
        }
    };

    private static final Strategy HOUR_STRATEGY = new NumberStrategy(Calendar.HOUR);

    private static final Strategy MINUTE_STRATEGY = new NumberStrategy(Calendar.MINUTE);

    private static final Strategy SECOND_STRATEGY = new NumberStrategy(Calendar.SECOND);

    private static final Strategy MILLISECOND_STRATEGY = new NumberStrategy(Calendar.MILLISECOND);

    /**
     * Gets the short and long values displayed for a field
     *
     * @param calendar The calendar to obtain the short and long values
     * @param locale   The locale of display names
     * @param field    The field of interest
     * @param regex    The regular expression to build
     * @return The map of string display names to field values
     */
    private static Map<String, Integer> appendDisplayNames(final Calendar calendar, final Locale locale, final int field, final StringBuilder regex) {
        Objects.requireNonNull(calendar, "calendar");
        final Map<String, Integer> values = new HashMap<>();
        final Locale actualLocale = LocaleUtils.toLocale(locale);
        final Map<String, Integer> displayNames = calendar.getDisplayNames(field, Calendar.ALL_STYLES, actualLocale);
        final TreeSet<String> sorted = new TreeSet<>(LONGER_FIRST_LOWERCASE);
        displayNames.forEach((k, v) -> {
            final String keyLc = k.toLowerCase(actualLocale);
            if (sorted.add(keyLc)) {
                values.put(keyLc, v);
            }
        });
        sorted.forEach(symbol -> simpleQuote(regex, symbol).append('|'));
        return values;
    }

    /**
     * Clears the cache.
     */
    static void clear() {
        Stream.of(CACHES).filter(Objects::nonNull).forEach(ConcurrentMap::clear);
    }

    /**
     * Gets a cache of Strategies for a particular field
     *
     * @param field The Calendar field
     * @return a cache of Locale to Strategy
     */
    private static ConcurrentMap<Locale, Strategy> getCache(final int field) {
        synchronized (CACHES) {
            if (CACHES[field] == null) {
                CACHES[field] = new ConcurrentHashMap<>(3);
            }
            return CACHES[field];
        }
    }

    private static StringBuilder simpleQuote(final StringBuilder sb, final String value) {
        for (int i = 0; i < value.length(); ++i) {
            final char c = value.charAt(i);
            switch (c) {
            case '\\':
            case '^':
            case '$':
            case '.':
            case '|':
            case '?':
            case '*':
            case '+':
            case '(':
            case ')':
            case '[':
            case '{':
                sb.append('\\');
                // falls-through
            default:
                sb.append(c);
            }
        }
        if (sb.charAt(sb.length() - 1) == '.') {
            // trailing '.' is optional
            sb.append('?');
        }
        return sb;
    }

    /** Input pattern. */
    private final String pattern;

    /** Input TimeZone. */
    private final TimeZone timeZone;

    /** Input Locale. */
    private final Locale locale;

    /**
     * Century from Date.
     */
    private final int century;

    /**
     * Start year from Date.
     */
    private final int startYear;

    /** Initialized from Calendar. */
    private transient List<StrategyAndWidth> patterns;

    /**
     * Constructs a new FastDateParser.
     *
     * Use {@link FastDateFormat#getInstance(String, TimeZone, Locale)} or another variation of the factory methods of {@link FastDateFormat} to get a cached
     * FastDateParser instance.
     *
     * @param pattern  non-null {@link java.text.SimpleDateFormat} compatible pattern
     * @param timeZone non-null time zone to use
     * @param locale   non-null locale
     */
    protected FastDateParser(final String pattern, final TimeZone timeZone, final Locale locale) {
        this(pattern, timeZone, locale, null);
    }

    /**
     * Constructs a new FastDateParser.
     *
     * @param pattern      non-null {@link java.text.SimpleDateFormat} compatible pattern
     * @param timeZone     non-null time zone to use
     * @param locale       locale, null maps to the default Locale.
     * @param centuryStart The start of the century for 2 digit year parsing
     * @since 3.5
     */
    protected FastDateParser(final String pattern, final TimeZone timeZone, final Locale locale, final Date centuryStart) {
        this.pattern = Objects.requireNonNull(pattern, "pattern");
        this.timeZone = Objects.requireNonNull(timeZone, "timeZone");
        this.locale = LocaleUtils.toLocale(locale);
        final Calendar definingCalendar = Calendar.getInstance(timeZone, this.locale);
        final int centuryStartYear;
        if (centuryStart != null) {
            definingCalendar.setTime(centuryStart);
            centuryStartYear = definingCalendar.get(Calendar.YEAR);
        } else if (this.locale.equals(JAPANESE_IMPERIAL)) {
            centuryStartYear = 0;
        } else {
            // from 80 years ago to 20 years from now
            definingCalendar.setTime(new Date());
            centuryStartYear = definingCalendar.get(Calendar.YEAR) - 80;
        }
        century = centuryStartYear / 100 * 100;
        startYear = centuryStartYear - century;
        init(definingCalendar);
    }

    /**
     * Adjusts dates to be within appropriate century
     *
     * @param twoDigitYear The year to adjust
     * @return A value between centuryStart(inclusive) to centuryStart+100(exclusive)
     */
    private int adjustYear(final int twoDigitYear) {
        final int trial = century + twoDigitYear;
        return twoDigitYear >= startYear ? trial : trial + 100;
    }

    /**
     * Compares another object for equality with this object.
     *
     * @param obj the object to compare to
     * @return {@code true}if equal to this instance
     */
    @Override
    public boolean equals(final Object obj) {
        if (!(obj instanceof FastDateParser)) {
            return false;
        }
        final FastDateParser other = (FastDateParser) obj;
        return pattern.equals(other.pattern) && timeZone.equals(other.timeZone) && locale.equals(other.locale);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.apache.commons.lang3.time.DateParser#getLocale()
     */
    @Override
    public Locale getLocale() {
        return locale;
    }

    /**
     * Constructs a Strategy that parses a Text field
     *
     * @param field            The Calendar field
     * @param definingCalendar The calendar to obtain the short and long values
     * @return a TextStrategy for the field and Locale
     */
    private Strategy getLocaleSpecificStrategy(final int field, final Calendar definingCalendar) {
        final ConcurrentMap<Locale, Strategy> cache = getCache(field);
        return cache.computeIfAbsent(locale,
                k -> field == Calendar.ZONE_OFFSET ? new TimeZoneStrategy(locale) : new CaseInsensitiveTextStrategy(field, definingCalendar, locale));
    }

    /*
     * (non-Javadoc)
     *
     * @see org.apache.commons.lang3.time.DateParser#getPattern()
     */
    @Override
    public String getPattern() {
        return pattern;
    }
    /**
     * Gets a Strategy given a field from a SimpleDateFormat pattern
     *
     * @param f                A sub-sequence of the SimpleDateFormat pattern
     * @param width            formatting width
     * @param definingCalendar The calendar to obtain the short and long values
     * @return The Strategy that will handle parsing for the field
     */
    private Strategy getStrategy(final char f, final int width, final Calendar definingCalendar) {
        switch (f) {
        case 'D':
            return DAY_OF_YEAR_STRATEGY;
        case 'E':
            return getLocaleSpecificStrategy(Calendar.DAY_OF_WEEK, definingCalendar);
        case 'F':
            return DAY_OF_WEEK_IN_MONTH_STRATEGY;
        case 'G':
            return getLocaleSpecificStrategy(Calendar.ERA, definingCalendar);
        case 'H': // Hour in day (0-23)
            return HOUR_OF_DAY_STRATEGY;
        case 'K': // Hour in am/pm (0-11)
            return HOUR_STRATEGY;
        case 'M':
        case 'L':
            return width >= 3 ? getLocaleSpecificStrategy(Calendar.MONTH, definingCalendar) : NUMBER_MONTH_STRATEGY;
        case 'S':
            return MILLISECOND_STRATEGY;
        case 'W':
            return WEEK_OF_MONTH_STRATEGY;
        case 'a':
            return getLocaleSpecificStrategy(Calendar.AM_PM, definingCalendar);
        case 'd':
            return DAY_OF_MONTH_STRATEGY;
        case 'h': // Hour in am/pm (1-12), i.e. midday/midnight is 12, not 0
            return HOUR12_STRATEGY;
        case 'k': // Hour in day (1-24), i.e. midnight is 24, not 0
            return HOUR24_OF_DAY_STRATEGY;
        case 'm':
            return MINUTE_STRATEGY;
        case 's':
            return SECOND_STRATEGY;
        case 'u':
            return DAY_OF_WEEK_STRATEGY;
        case 'w':
            return WEEK_OF_YEAR_STRATEGY;
        case 'y':
        case 'Y':
            return width > 2 ? LITERAL_YEAR_STRATEGY : ABBREVIATED_YEAR_STRATEGY;
        case 'X':
            return ISO8601TimeZoneStrategy.getStrategy(width);
        case 'Z':
            if (width == 2) {
                return ISO8601TimeZoneStrategy.ISO_8601_3_STRATEGY;
            }
            // falls-through
        case 'z':
            return getLocaleSpecificStrategy(Calendar.ZONE_OFFSET, definingCalendar);
        default:
            throw new IllegalArgumentException("Format '" + f + "' not supported");
        }
    }
    /*
     * (non-Javadoc)
     *
     * @see org.apache.commons.lang3.time.DateParser#getTimeZone()
     */
    @Override
    public TimeZone getTimeZone() {
        return timeZone;
    }
    /**
     * Returns a hash code compatible with equals.
     *
     * @return a hash code compatible with equals
     */
    @Override
    public int hashCode() {
        return pattern.hashCode() + 13 * (timeZone.hashCode() + 13 * locale.hashCode());
    }
    /**
     * Initializes derived fields from defining fields. This is called from constructor and from readObject (de-serialization)
     *
     * @param definingCalendar the {@link java.util.Calendar} instance used to initialize this FastDateParser
     */
    private void init(final Calendar definingCalendar) {
        patterns = new ArrayList<>();

        final StrategyParser strategyParser = new StrategyParser(definingCalendar);
        for (;;) {
            final StrategyAndWidth field = strategyParser.getNextStrategy();
            if (field == null) {
                break;
            }
            patterns.add(field);
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.apache.commons.lang3.time.DateParser#parse(String)
     */
    @Override
    public Date parse(final String source) throws ParseException {
        final ParsePosition pp = new ParsePosition(0);
        final Date date = parse(source, pp);
        if (date == null) {
            // Add a note regarding supported date range
            if (locale.equals(JAPANESE_IMPERIAL)) {
                throw new ParseException("(The " + locale + " locale does not support dates before 1868 AD)\nUnparseable date: \"" + source,
                        pp.getErrorIndex());
            }
            throw new ParseException("Unparseable date: " + source, pp.getErrorIndex());
        }
        return date;
    }
    /**
     * This implementation updates the ParsePosition if the parse succeeds. However, it sets the error index to the position before the failed field unlike the
     * method {@link java.text.SimpleDateFormat#parse(String, ParsePosition)} which sets the error index to after the failed field.
     * <p>
     * To determine if the parse has succeeded, the caller must check if the current parse position given by {@link ParsePosition#getIndex()} has been updated.
     * If the input buffer has been fully parsed, then the index will point to just after the end of the input buffer.
     * </p>
     *
     * @see org.apache.commons.lang3.time.DateParser#parse(String, java.text.ParsePosition)
     */
    @Override
    public Date parse(final String source, final ParsePosition pos) {
        // timing tests indicate getting new instance is 19% faster than cloning
        final Calendar cal = Calendar.getInstance(timeZone, locale);
        cal.clear();
        return parse(source, pos, cal) ? cal.getTime() : null;
    }
    /**
     * Parses a formatted date string according to the format. Updates the Calendar with parsed fields. Upon success, the ParsePosition index is updated to
     * indicate how much of the source text was consumed. Not all source text needs to be consumed. Upon parse failure, ParsePosition error index is updated to
     * the offset of the source text which does not match the supplied format.
     *
     * @param source   The text to parse.
     * @param pos      On input, the position in the source to start parsing, on output, updated position.
     * @param calendar The calendar into which to set parsed fields.
     * @return true, if source has been parsed (pos parsePosition is updated); otherwise false (and pos errorIndex is updated)
     * @throws IllegalArgumentException when Calendar has been set to be not lenient, and a parsed field is out of range.
     */
    @Override
    public boolean parse(final String source, final ParsePosition pos, final Calendar calendar) {
        final ListIterator<StrategyAndWidth> lt = patterns.listIterator();
        while (lt.hasNext()) {
            final StrategyAndWidth strategyAndWidth = lt.next();
            final int maxWidth = strategyAndWidth.getMaxWidth(lt);
            if (!strategyAndWidth.strategy.parse(this, calendar, source, pos, maxWidth)) {
                return false;
            }
        }
        return true;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.apache.commons.lang3.time.DateParser#parseObject(String)
     */
    @Override
    public Object parseObject(final String source) throws ParseException {
        return parse(source);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.apache.commons.lang3.time.DateParser#parseObject(String, java.text.ParsePosition)
     */
    @Override
    public Object parseObject(final String source, final ParsePosition pos) {
        return parse(source, pos);
    }

    // Serializing
    /**
     * Creates the object after serialization. This implementation reinitializes the transient properties.
     *
     * @param in ObjectInputStream from which the object is being deserialized.
     * @throws IOException            if there is an IO issue.
     * @throws ClassNotFoundException if a class cannot be found.
     */
    private void readObject(final ObjectInputStream in) throws IOException, ClassNotFoundException {
        in.defaultReadObject();
        final Calendar definingCalendar = Calendar.getInstance(timeZone, locale);
        init(definingCalendar);
    }

    /**
     * Gets a string version of this formatter.
     *
     * @return a debugging string
     */
    @Override
    public String toString() {
        return "FastDateParser[" + pattern + ", " + locale + ", " + timeZone.getID() + "]";
    }

    /**
     * Converts all state of this instance to a String handy for debugging.
     *
     * @return a string.
     * @since 3.12.0
     */
    public String toStringAll() {
        return "FastDateParser [pattern=" + pattern + ", timeZone=" + timeZone + ", locale=" + locale + ", century=" + century + ", startYear=" + startYear
                + ", patterns=" + patterns + "]";
    }
}
