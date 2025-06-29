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

import java.text.DateFormat;
import java.text.Format;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Locale;
import java.util.Objects;
import java.util.TimeZone;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.apache.commons.lang3.LocaleUtils;

/**
 * FormatCache is a cache and factory for {@link Format}s.
 *
 * @param <F> The Format type.
 * @since 3.0
 */
// TODO: Before making public move from getDateTimeInstance(Integer, ...) to int; or some other approach.
abstract class AbstractFormatCache<F extends Format> {

    /**
     * Helper class to hold multipart Map keys as arrays.
     */
    private static final class ArrayKey {

        private static int computeHashCode(final Object[] keys) {
            final int prime = 31;
            int result = 1;
            result = prime * result + Arrays.hashCode(keys);
            return result;
        }

        private final Object[] keys;
        private final int hashCode;

        /**
         * Constructs an instance of {@link MultipartKey} to hold the specified objects.
         *
         * @param keys the set of objects that make up the key.  Each key may be null.
         */
        ArrayKey(final Object... keys) {
            this.keys = keys;
            this.hashCode = computeHashCode(keys);
        }

        @Override
        public boolean equals(final Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            final ArrayKey other = (ArrayKey) obj;
            return Arrays.deepEquals(keys, other.keys);
        }

        @Override
        public int hashCode() {
            return hashCode;
        }

    }

    /**
     * No date or no time.  Used in same parameters as DateFormat.SHORT or DateFormat.LONG
     */
    static final int NONE = -1;

    private static final ConcurrentMap<ArrayKey, String> dateTimeInstanceCache = new ConcurrentHashMap<>(7);

    /**
     * Clears the cache.
     */
    static void clear() {
        dateTimeInstanceCache.clear();
    }

    /**
     * Gets a date/time format for the specified styles and locale.
     *
     * @param dateStyle  date style: FULL, LONG, MEDIUM, or SHORT, null indicates no date in format
     * @param timeStyle  time style: FULL, LONG, MEDIUM, or SHORT, null indicates no time in format
     * @param locale  The non-null locale of the desired format
     * @return a localized standard date/time format
     * @throws IllegalArgumentException if the Locale has no date/time pattern defined
     */
    // package protected, for access from test code; do not make public or protected
    static String getPatternForStyle(final Integer dateStyle, final Integer timeStyle, final Locale locale) {
        final Locale safeLocale = LocaleUtils.toLocale(locale);
        final ArrayKey key = new ArrayKey(dateStyle, timeStyle, safeLocale);
        return dateTimeInstanceCache.computeIfAbsent(key, k -> {
            try {
                final DateFormat formatter;
                if (dateStyle == null) {
                    formatter = DateFormat.getTimeInstance(timeStyle.intValue(), safeLocale);
                } else if (timeStyle == null) {
                    formatter = DateFormat.getDateInstance(dateStyle.intValue(), safeLocale);
                } else {
                    formatter = DateFormat.getDateTimeInstance(dateStyle.intValue(), timeStyle.intValue(), safeLocale);
                }
                return ((SimpleDateFormat) formatter).toPattern();
            } catch (final ClassCastException ex) {
                throw new IllegalArgumentException("No date time pattern for locale: " + safeLocale);
            }
        });
    }

    private final ConcurrentMap<ArrayKey, F> instanceCache = new ConcurrentHashMap<>(7);

    /**
     * Clears the cache.
     */
    void clearInstance() {
        instanceCache.clear();
    }

    /**
     * Create a format instance using the specified pattern, time zone
     * and locale.
     *
     * @param pattern  {@link java.text.SimpleDateFormat} compatible pattern, this will not be null.
     * @param timeZone  time zone, this will not be null.
     * @param locale  locale, this will not be null.
     * @return a pattern based date/time formatter
     * @throws IllegalArgumentException if pattern is invalid
     *  or {@code null}
     */
    protected abstract F createInstance(String pattern, TimeZone timeZone, Locale locale);

    /**
     * Gets a date formatter instance using the specified style,
     * time zone and locale.
     *
     * @param dateStyle  date style: FULL, LONG, MEDIUM, or SHORT
     * @param timeZone  optional time zone, overrides time zone of
     *  formatted date, null means use default Locale
     * @param locale  optional locale, overrides system locale
     * @return a localized standard date/time formatter
     * @throws IllegalArgumentException if the Locale has no date/time
     *  pattern defined
     */
    // package protected, for access from FastDateFormat; do not make public or protected
    F getDateInstance(final int dateStyle, final TimeZone timeZone, final Locale locale) {
        return getDateTimeInstance(Integer.valueOf(dateStyle), null, timeZone, locale);
    }

    /**
     * Gets a date/time formatter instance using the specified style,
     * time zone and locale.
     *
     * @param dateStyle  date style: FULL, LONG, MEDIUM, or SHORT
     * @param timeStyle  time style: FULL, LONG, MEDIUM, or SHORT
     * @param timeZone  optional time zone, overrides time zone of
     *  formatted date, null means use default Locale
     * @param locale  optional locale, overrides system locale
     * @return a localized standard date/time formatter
     * @throws IllegalArgumentException if the Locale has no date/time
     *  pattern defined
     */
    // package protected, for access from FastDateFormat; do not make public or protected
    F getDateTimeInstance(final int dateStyle, final int timeStyle, final TimeZone timeZone, final Locale locale) {
        return getDateTimeInstance(Integer.valueOf(dateStyle), Integer.valueOf(timeStyle), timeZone, locale);
    }

    /**
     * Gets a date/time formatter instance using the specified style,
     * time zone and locale.
     *
     * @param dateStyle  date style: FULL, LONG, MEDIUM, or SHORT, null indicates no date in format
     * @param timeStyle  time style: FULL, LONG, MEDIUM, or SHORT, null indicates no time in format
     * @param timeZone  optional time zone, overrides time zone of
     *  formatted date, null means use default Locale
     * @param locale  optional locale, overrides system locale
     * @return a localized standard date/time formatter
     * @throws IllegalArgumentException if the Locale has no date/time
     *  pattern defined
     */
    // This must remain private, see LANG-884
    private F getDateTimeInstance(final Integer dateStyle, final Integer timeStyle, final TimeZone timeZone, Locale locale) {
        locale = LocaleUtils.toLocale(locale);
        final String pattern = getPatternForStyle(dateStyle, timeStyle, locale);
        return getInstance(pattern, timeZone, locale);
    }

    /**
     * Gets a formatter instance using the default pattern in the
     * default time zone and locale.
     *
     * @return a date/time formatter
     */
    public F getInstance() {
        return getDateTimeInstance(DateFormat.SHORT, DateFormat.SHORT, TimeZone.getDefault(), Locale.getDefault());
    }

    /**
     * Gets a formatter instance using the specified pattern, time zone
     * and locale.
     *
     * @param pattern  {@link java.text.SimpleDateFormat} compatible
     *  pattern, non-null
     * @param timeZone  the time zone, null means use the default TimeZone
     * @param locale  the locale, null means use the default Locale
     * @return a pattern based date/time formatter
     * @throws NullPointerException if pattern is {@code null}
     * @throws IllegalArgumentException if pattern is invalid
     */
    public F getInstance(final String pattern, final TimeZone timeZone, final Locale locale) {
        Objects.requireNonNull(pattern, "pattern");
        final TimeZone actualTimeZone = TimeZones.toTimeZone(timeZone);
        final Locale actualLocale = LocaleUtils.toLocale(locale);
        final ArrayKey key = new ArrayKey(pattern, actualTimeZone, actualLocale);
        return instanceCache.computeIfAbsent(key, k -> createInstance(pattern, actualTimeZone, actualLocale));
    }

    /**
     * Gets a time formatter instance using the specified style,
     * time zone and locale.
     *
     * @param timeStyle  time style: FULL, LONG, MEDIUM, or SHORT
     * @param timeZone  optional time zone, overrides time zone of
     *  formatted date, null means use default Locale
     * @param locale  optional locale, overrides system locale
     * @return a localized standard date/time formatter
     * @throws IllegalArgumentException if the Locale has no date/time
     *  pattern defined
     */
    // package protected, for access from FastDateFormat; do not make public or protected
    F getTimeInstance(final int timeStyle, final TimeZone timeZone, final Locale locale) {
        return getDateTimeInstance(null, Integer.valueOf(timeStyle), timeZone, locale);
    }

}
