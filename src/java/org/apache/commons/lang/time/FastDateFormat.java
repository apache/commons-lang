/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002-2003 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution, if
 *    any, must include the following acknowlegement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowlegement may appear in the software itself,
 *    if and wherever such third-party acknowlegements normally appear.
 *
 * 4. The names "The Jakarta Project", "Commons", and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 */
package org.apache.commons.lang.time;

import java.io.Serializable;
import java.text.DateFormat;
import java.text.DateFormatSymbols;
import java.text.FieldPosition;
import java.text.Format;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;

/**
 * FastDateFormat is similar to {@link java.text.SimpleDateFormat}, but 
 * faster and thread-safe.
 * <p>
 * Only formatting is supported, but all patterns are compatible with
 * SimpleDateFormat.
 * <p>
 * NOTE: Code originally taken from the open source TreeTrove project.
 *
 * @author Brian S O'Neill
 * @author Sean Schofield
 * @author Gary Gregory
 * @author Stephen Colebourne
 * @since 2.0
 * @version $Id: FastDateFormat.java,v 1.5 2003/05/21 23:39:53 scolebourne Exp $
 */
public class FastDateFormat extends Format {
    
    /** FULL date or time style */
    public static final int FULL = SimpleDateFormat.FULL;
    /** LONG date or time style */
    public static final int LONG = SimpleDateFormat.LONG;
    /** MEDIUM date or time style */
    public static final int MEDIUM = SimpleDateFormat.MEDIUM;
    /** SHORT date or time style */
    public static final int SHORT = SimpleDateFormat.SHORT;
    
    // package scoped as used by inner class
    static final double LOG_10 = Math.log(10);

    private static String cDefaultPattern;
    private static TimeZone cDefaultTimeZone = TimeZone.getDefault();

    private static Map cInstanceCache = new HashMap(7);
    private static Map cDateInstanceCache = new HashMap(7);
    private static Map cTimeInstanceCache = new HashMap(7);
    private static Map cDateTimeInstanceCache = new HashMap(7);
    private static Map cTimeZoneDisplayCache = new HashMap(7);

    /** The pattern */
    private final String mPattern;
    /** The time zone */
    private final TimeZone mTimeZone;
    /** The locale */
    private final Locale mLocale;
    /** The parsed rules */
    private final Rule[] mRules;
    /** The estimated maximum length */
    private final int mMaxLengthEstimate;

    //-----------------------------------------------------------------------
    /**
     * Gets a formatter instance using the default pattern in the default locale.
     * 
     * @return a date/time formatter
     */
    public static FastDateFormat getInstance() {
        return getInstance(getDefaultPattern(), null, null);
    }

    /**
     * Gets a formatter instance using the specified pattern in the default locale.
     * 
     * @param pattern  {@link java.text.SimpleDateFormat} compatible pattern
     * @return a pattern based date/time formatter
     * @throws IllegalArgumentException if pattern is invalid
     */
    public static FastDateFormat getInstance(String pattern) {
        return getInstance(pattern, null, null);
    }

    /**
     * Gets a formatter instance using the specified pattern and time zone.
     * 
     * @param pattern  {@link java.text.SimpleDateFormat} compatible pattern
     * @param timeZone  optional time zone, overrides time zone of formatted date
     * @return a pattern based date/time formatter
     * @throws IllegalArgumentException if pattern is invalid
     */
    public static FastDateFormat getInstance(String pattern, TimeZone timeZone) {
        return getInstance(pattern, timeZone, null);
    }

    /**
     * Gets a formatter instance using the specified pattern and locale.
     * 
     * @param pattern  {@link java.text.SimpleDateFormat} compatible pattern
     * @param locale  optional locale, overrides system locale
     * @return a pattern based date/time formatter
     * @throws IllegalArgumentException if pattern is invalid
     */
    public static FastDateFormat getInstance(String pattern, Locale locale) {
        return getInstance(pattern, null, locale);
    }

    /**
     * Gets a formatter instance using the specified pattern, time zone and locale.
     * 
     * @param pattern  {@link java.text.SimpleDateFormat} compatible pattern
     * @param timeZone  optional time zone, overrides time zone of formatted date
     * @param locale  optional locale, overrides system locale
     * @return a pattern based date/time formatter
     * @throws IllegalArgumentException if pattern is invalid
     */
    public static synchronized FastDateFormat getInstance(String pattern, TimeZone timeZone, Locale locale) {
        Object key = pattern;
        if (timeZone != null) {
            key = new Pair(key, timeZone);
        }
        if (locale != null) {
            key = new Pair(key, locale);
        }

        FastDateFormat format = (FastDateFormat) cInstanceCache.get(key);
        if (format == null) {
            if (locale == null) {
                locale = Locale.getDefault();
            }

            format = new FastDateFormat(pattern, timeZone, locale, new DateFormatSymbols(locale));
            cInstanceCache.put(key, format);
        }
        return format;
    }

    /**
     * Gets a date formatter instance using the specified style, time zone and locale.
     * 
     * @param style  date style: FULL, LONG, MEDIUM, or SHORT
     * @param timeZone  optional time zone, overrides time zone of formatted date
     * @param locale  optional locale, overrides system locale
     * @return a localized standard date formatter
     * @throws IllegalArgumentException if the Locale has no date pattern defined
     */
    public static synchronized FastDateFormat getDateInstance(int style, TimeZone timeZone, Locale locale) {
        Object key = new Integer(style);
        if (timeZone != null) {
            key = new Pair(key, timeZone);
        }
        if (locale == null) {
            key = new Pair(key, locale);
        }

        FastDateFormat format = (FastDateFormat) cDateInstanceCache.get(key);
        if (format == null) {
            if (locale == null) {
                locale = Locale.getDefault();
            }

            try {
                SimpleDateFormat formatter = (SimpleDateFormat) DateFormat.getDateInstance(style, locale);
                String pattern = formatter.toPattern();
                format = getInstance(pattern, timeZone, locale);
                cDateInstanceCache.put(key, format);
                
            } catch (ClassCastException ex) {
                throw new IllegalArgumentException("No date pattern for locale: " + locale);
            }
        }
        return format;
    }

    /**
     * Gets a time formatter instance using the specified style, time zone and locale.
     * 
     * @param style  time style: FULL, LONG, MEDIUM, or SHORT
     * @param timeZone  optional time zone, overrides time zone of formatted time
     * @param locale  optional locale, overrides system locale
     * @return a localized standard time formatter
     * @throws IllegalArgumentException if the Locale has no time pattern defined
     */
    public static synchronized FastDateFormat getTimeInstance(int style, TimeZone timeZone, Locale locale) {
        Object key = new Integer(style);
        if (timeZone != null) {
            key = new Pair(key, timeZone);
        }
        if (locale != null) {
            key = new Pair(key, locale);
        }

        FastDateFormat format = (FastDateFormat) cTimeInstanceCache.get(key);
        if (format == null) {
            if (locale == null) {
                locale = Locale.getDefault();
            }

            try {
                SimpleDateFormat formatter = (SimpleDateFormat) DateFormat.getTimeInstance(style, locale);
                String pattern = formatter.toPattern();
                format = getInstance(pattern, timeZone, locale);
                cTimeInstanceCache.put(key, format);
            
            } catch (ClassCastException ex) {
                throw new IllegalArgumentException("No date pattern for locale: " + locale);
            }
        }
        return format;
    }

    /**
     * Gets a date/time formatter instance using the specified style, time zone and locale.
     * 
     * @param dateStyle  date style: FULL, LONG, MEDIUM, or SHORT
     * @param timeStyle  time style: FULL, LONG, MEDIUM, or SHORT
     * @param timeZone  optional time zone, overrides time zone of formatted date
     * @param locale  optional locale, overrides system locale
     * @return a localized standard date/time formatter
     * @throws IllegalArgumentException if the Locale has no date/time pattern defined
     */
    public static synchronized FastDateFormat getDateTimeInstance(
            int dateStyle, int timeStyle, TimeZone timeZone, Locale locale) {

        Object key = new Pair(new Integer(dateStyle), new Integer(timeStyle));
        if (timeZone != null) {
            key = new Pair(key, timeZone);
        }
        if (locale != null) {
            key = new Pair(key, locale);
        }

        FastDateFormat format = (FastDateFormat) cDateTimeInstanceCache.get(key);
        if (format == null) {
            if (locale == null) {
                locale = Locale.getDefault();
            }

            try {
                SimpleDateFormat formatter = (SimpleDateFormat) DateFormat.getDateTimeInstance(dateStyle, timeStyle, locale);
                String pattern = formatter.toPattern();
                format = getInstance(pattern, timeZone, locale);
                cDateTimeInstanceCache.put(key, format);
                
            } catch (ClassCastException ex) {
                throw new IllegalArgumentException("No date time pattern for locale: " + locale);
            }
        }
        return format;
    }

    //-----------------------------------------------------------------------
    /**
     * Gets the time zone display name, using a cache for performance.
     * 
     * @param tz  the zone to query
     * @param daylight  true if daylight savings
     * @param style  the style to use TimeZone.LONG or TimeZone.SHORT
     * @param locale  the locale to use
     * @return the textual name of the time zone
     */
    static synchronized String getTimeZoneDisplay(TimeZone tz, boolean daylight, int style, Locale locale) {
        Object key = new TimeZoneDisplayKey(tz, daylight, style, locale);
        String value = (String) cTimeZoneDisplayCache.get(key);
        if (value == null) {
            // This is a very slow call, so cache the results.
            value = tz.getDisplayName(daylight, style, locale);
            cTimeZoneDisplayCache.put(key, value);
        }
        return value;
    }

    /**
     * Gets the default pattern.
     * 
     * @return the default pattern
     */
    private static synchronized String getDefaultPattern() {
        if (cDefaultPattern == null) {
            cDefaultPattern = new SimpleDateFormat().toPattern();
        }
        return cDefaultPattern;
    }

    /**
     * Returns a list of Rules given a pattern.
     * 
     * @param pattern  the pattern to parse
     * @param timeZone  the time zone to use
     * @param locale  the locale to use
     * @param symbols  the symbols to use
     * @return a List of Rule objects
     * @throws IllegalArgumentException if pattern is invalid
     */
    private static List parse(String pattern, TimeZone timeZone, Locale locale, DateFormatSymbols symbols) {
        List rules = new ArrayList();

        String[] ERAs = symbols.getEras();
        String[] months = symbols.getMonths();
        String[] shortMonths = symbols.getShortMonths();
        String[] weekdays = symbols.getWeekdays();
        String[] shortWeekdays = symbols.getShortWeekdays();
        String[] AmPmStrings = symbols.getAmPmStrings();

        int length = pattern.length();
        int[] indexRef = new int[1];

        for (int i = 0; i < length; i++) {
            indexRef[0] = i;
            String token = parseToken(pattern, indexRef);
            i = indexRef[0];

            int tokenLen = token.length();
            if (tokenLen == 0) {
                break;
            }

            Rule rule;
            char c = token.charAt(0);

            switch (c) {
            case 'G': // era designator (text)
                rule = new TextField(Calendar.ERA, ERAs);
                break;
            case 'y': // year (number)
                if (tokenLen >= 4) {
                    rule = new UnpaddedNumberField(Calendar.YEAR);
                } else {
                    rule = new TwoDigitYearField();
                }
                break;
            case 'M': // month in year (text and number)
                if (tokenLen >= 4) {
                    rule = new TextField(Calendar.MONTH, months);
                } else if (tokenLen == 3) {
                    rule = new TextField(Calendar.MONTH, shortMonths);
                } else if (tokenLen == 2) {
                    rule = new TwoDigitMonthField();
                } else {
                    rule = new UnpaddedMonthField();
                }
                break;
            case 'd': // day in month (number)
                rule = selectNumberRule(Calendar.DAY_OF_MONTH, tokenLen);
                break;
            case 'h': // hour in am/pm (number, 1..12)
                rule = new TwelveHourField(selectNumberRule(Calendar.HOUR, tokenLen));
                break;
            case 'H': // hour in day (number, 0..23)
                rule = selectNumberRule(Calendar.HOUR_OF_DAY, tokenLen);
                break;
            case 'm': // minute in hour (number)
                rule = selectNumberRule(Calendar.MINUTE, tokenLen);
                break;
            case 's': // second in minute (number)
                rule = selectNumberRule(Calendar.SECOND, tokenLen);
                break;
            case 'S': // millisecond (number)
                rule = selectNumberRule(Calendar.MILLISECOND, tokenLen);
                break;
            case 'E': // day in week (text)
                rule = new TextField(Calendar.DAY_OF_WEEK, tokenLen < 4 ? shortWeekdays : weekdays);
                break;
            case 'D': // day in year (number)
                rule = selectNumberRule(Calendar.DAY_OF_YEAR, tokenLen);
                break;
            case 'F': // day of week in month (number)
                rule = selectNumberRule(Calendar.DAY_OF_WEEK_IN_MONTH, tokenLen);
                break;
            case 'w': // week in year (number)
                rule = selectNumberRule(Calendar.WEEK_OF_YEAR, tokenLen);
                break;
            case 'W': // week in month (number)
                rule = selectNumberRule(Calendar.WEEK_OF_MONTH, tokenLen);
                break;
            case 'a': // am/pm marker (text)
                rule = new TextField(Calendar.AM_PM, AmPmStrings);
                break;
            case 'k': // hour in day (1..24)
                rule = new TwentyFourHourField(selectNumberRule(Calendar.HOUR_OF_DAY, tokenLen));
                break;
            case 'K': // hour in am/pm (0..11)
                rule = selectNumberRule(Calendar.HOUR, tokenLen);
                break;
            case 'z': // time zone (text)
                if (tokenLen >= 4) {
                    rule = new TimeZoneRule(timeZone, locale, TimeZone.LONG);
                } else {
                    rule = new TimeZoneRule(timeZone, locale, TimeZone.SHORT);
                }
                break;
            case '\'': // literal text
                String sub = token.substring(1);
                if (sub.length() == 1) {
                    rule = new CharacterLiteral(sub.charAt(0));
                } else {
                    rule = new StringLiteral(new String(sub));
                }
                break;
            default:
                throw new IllegalArgumentException("Illegal pattern component: " + token);
            }

            rules.add(rule);
        }

        return rules;
    }

    /**
     * Performs the parsing of tokens.
     * 
     * @param pattern  the pattern
     * @param indexRef  index references
     * @return parsed token
     */
    private static String parseToken(String pattern, int[] indexRef) {
        StringBuffer buf = new StringBuffer();

        int i = indexRef[0];
        int length = pattern.length();

        char c = pattern.charAt(i);
        if (c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z') {
            // Scan a run of the same character, which indicates a time
            // pattern.
            buf.append(c);

            while (i + 1 < length) {
                char peek = pattern.charAt(i + 1);
                if (peek == c) {
                    buf.append(c);
                    i++;
                } else {
                    break;
                }
            }
        } else {
            // This will identify token as text.
            buf.append('\'');

            boolean inLiteral = false;

            for (; i < length; i++) {
                c = pattern.charAt(i);

                if (c == '\'') {
                    if (i + 1 < length && pattern.charAt(i + 1) == '\'') {
                        // '' is treated as escaped '
                        i++;
                        buf.append(c);
                    } else {
                        inLiteral = !inLiteral;
                    }
                } else if (!inLiteral &&
                         (c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z')) {
                    i--;
                    break;
                } else {
                    buf.append(c);
                }
            }
        }

        indexRef[0] = i;
        return buf.toString();
    }

    /**
     * Gets an appropriate rule for the padding required.
     * 
     * @param field  the field to get a rule for
     * @param padding  the padding required
     * @return a new rule with the correct padding
     */
    private static NumberRule selectNumberRule(int field, int padding) {
        switch (padding) {
        case 1:
            return new UnpaddedNumberField(field);
        case 2:
            return new TwoDigitNumberField(field);
        default:
            return new PaddedNumberField(field, padding);
        }
    }

    //-----------------------------------------------------------------------
    /**
     * Constructs a new FastDateFormat.
     * 
     * @param pattern  {@link java.text.SimpleDateFormat} compatible pattern
     * @param timeZone  optional time zone, overrides time zone of formatted date
     * @param locale  optional locale, overrides system locale
     * @param symbols  optional date format symbols, overrides symbols for provided locale
     * @throws IllegalArgumentException if pattern is invalid
     */
    private FastDateFormat(String pattern, TimeZone timeZone, Locale locale, DateFormatSymbols symbols) {
        if (locale == null) {
            locale = Locale.getDefault();
        }

        mPattern = pattern;
        mTimeZone = timeZone;
        mLocale = locale;

        if (symbols == null) {
            symbols = new DateFormatSymbols(locale);
        }

        List rulesList = parse(pattern, timeZone, locale, symbols);
        mRules = (Rule[]) rulesList.toArray(new Rule[rulesList.size()]);

        int len = 0;
        for (int i=mRules.length; --i >= 0; ) {
            len += mRules[i].estimateLength();
        }

        mMaxLengthEstimate = len;
    }

    //-----------------------------------------------------------------------
    /**
     * Format either a Date or a Calendar object.
     * 
     * @param obj  the object to format
     * @param toAppendTo  the buffer to append to
     * @param pos  the position - ignored
     * @return the buffer passed in
     */
    public StringBuffer format(Object obj, StringBuffer toAppendTo, FieldPosition pos) {
        if (obj instanceof Date) {
            return format((Date) obj, toAppendTo);
        } else if (obj instanceof Calendar) {
            return format((Calendar) obj, toAppendTo);
        } else {
            throw new IllegalArgumentException("Unknown class: " +
                (obj == null ? "<null>" : obj.getClass().getName()));
        }
    }

    /**
     * Formats a Date object.
     * 
     * @param date  the date to format
     * @return the formatted string
     */
    public String format(Date date) {
        Calendar c = new GregorianCalendar(cDefaultTimeZone);
        c.setTime(date);
        if (mTimeZone != null) {
            c.setTimeZone(mTimeZone);
        }
        return applyRules(c, new StringBuffer(mMaxLengthEstimate)).toString();
    }

    /**
     * Formats a Calendar object.
     * 
     * @param calendar  the calendar to format
     * @return the formatted string
     */
    public String format(Calendar calendar) {
        return format(calendar, new StringBuffer(mMaxLengthEstimate)).toString();
    }

    /**
     * Formats a Date object into the supplied StringBuffer.
     * 
     * @param date  the date to format
     * @param buf  the buffer to format into
     * @return the specified string buffer
     */
    public StringBuffer format(Date date, StringBuffer buf) {
        Calendar c = new GregorianCalendar(cDefaultTimeZone);
        c.setTime(date);
        if (mTimeZone != null) {
            c.setTimeZone(mTimeZone);
        }
        return applyRules(c, buf);
    }

    /**
     * Formats a Calendar object into the supplied StringBuffer.
     * 
     * @param calendar  the calendar to format
     * @param buf  the buffer to format into
     * @return the specified string buffer
     */
    public StringBuffer format(Calendar calendar, StringBuffer buf) {
        if (mTimeZone != null) {
            calendar = (Calendar)calendar.clone();
            calendar.setTimeZone(mTimeZone);
        }
        return applyRules(calendar, buf);
    }

    /**
     * Performs the formatting by applying the rules to the specified calendar.
     * 
     * @param calendar  the calendar to format
     * @param buf  the buffer to format into
     * @return the specified string buffer
     */
    private StringBuffer applyRules(Calendar calendar, StringBuffer buf) {
        Rule[] rules = mRules;
        int len = mRules.length;
        for (int i = 0; i < len; i++) {
            rules[i].appendTo(buf, calendar);
        }
        return buf;
    }

    //-----------------------------------------------------------------------
    /**
     * Parsing not supported.
     * 
     * @param source  the string to parse
     * @param pos  the parsing position
     * @return null as not supported
     */
    public Object parseObject(String source, ParsePosition pos) {
        pos.setIndex(0);
        pos.setErrorIndex(0);
        return null;
    }
    
    //-----------------------------------------------------------------------
    /**
     * Gets the pattern used by this formatter.
     * 
     * @return the pattern, {@link java.text.SimpleDateFormat} compatible
     */
    public String getPattern() {
        return mPattern;
    }

    /**
     * Gets the time zone used by this formatter, or null if time zone of
     * formatted dates is used instead.
     * 
     * @return the time zone
     */
    public TimeZone getTimeZone() {
        return mTimeZone;
    }

    /**
     * Gets the locale used by this formatter.
     * 
     * @return the locale
     */
    public Locale getLocale() {
        return mLocale;
    }

    /**
     * Gets  an estimate for the maximum string length that the formatter will produce.
     * The actual formatted length will almost always be less than or equal to this amount.
     * 
     * @return the maximum formatted length
     */
    public int getMaxLengthEstimate() {
        return mMaxLengthEstimate;
    }

    //-----------------------------------------------------------------------
    /**
     * Inner class defining a rule.
     */
    private interface Rule {
        int estimateLength();
        void appendTo(StringBuffer buffer, Calendar calendar);
    }

    /**
     * Inner class defining a numeric rule.
     */
    private interface NumberRule extends Rule {
        void appendTo(StringBuffer buffer, int value);
    }

    /**
     * Inner class to output a constant single character.
     */
    private static class CharacterLiteral implements Rule {
        private final char mValue;

        CharacterLiteral(char value) {
            mValue = value;
        }

        public int estimateLength() {
            return 1;
        }

        public void appendTo(StringBuffer buffer, Calendar calendar) {
            buffer.append(mValue);
        }
    }

    /**
     * Inner class to output a constant string.
     */
    private static class StringLiteral implements Rule {
        private final String mValue;

        StringLiteral(String value) {
            mValue = value;
        }

        public int estimateLength() {
            return mValue.length();
        }

        public void appendTo(StringBuffer buffer, Calendar calendar) {
            buffer.append(mValue);
        }
    }

    /**
     * Inner class to output one of a set of values.
     */
    private static class TextField implements Rule {
        private final int mField;
        private final String[] mValues;

        TextField(int field, String[] values) {
            mField = field;
            mValues = values;
        }

        public int estimateLength() {
            int max = 0;
            for (int i=mValues.length; --i >= 0; ) {
                int len = mValues[i].length();
                if (len > max) {
                    max = len;
                }
            }
            return max;
        }

        public void appendTo(StringBuffer buffer, Calendar calendar) {
            buffer.append(mValues[calendar.get(mField)]);
        }
    }

    /**
     * Inner class to output an unpadded number.
     */
    private static class UnpaddedNumberField implements NumberRule {
        private final int mField;

        UnpaddedNumberField(int field) {
            mField = field;
        }

        public int estimateLength() {
            return 4;
        }

        public void appendTo(StringBuffer buffer, Calendar calendar) {
            appendTo(buffer, calendar.get(mField));
        }

        public final void appendTo(StringBuffer buffer, int value) {
            if (value < 10) {
                buffer.append((char)(value + '0'));
            }
            else if (value < 100) {
                buffer.append((char)(value / 10 + '0'));
                buffer.append((char)(value % 10 + '0'));
            }
            else {
                buffer.append(Integer.toString(value));
            }
        }
    }

    /**
     * Inner class to output an unpadded month.
     */
    private static class UnpaddedMonthField implements NumberRule {
        UnpaddedMonthField() {
        }

        public int estimateLength() {
            return 2;
        }

        public void appendTo(StringBuffer buffer, Calendar calendar) {
            appendTo(buffer, calendar.get(Calendar.MONTH) + 1);
        }

        public final void appendTo(StringBuffer buffer, int value) {
            if (value < 10) {
                buffer.append((char)(value + '0'));
            }
            else {
                buffer.append((char)(value / 10 + '0'));
                buffer.append((char)(value % 10 + '0'));
            }
        }
    }

    /**
     * Inner class to output a padded number.
     */
    private static class PaddedNumberField implements NumberRule {
        private final int mField;
        private final int mSize;

        PaddedNumberField(int field, int size) {
            if (size < 3) {
                // Should use UnpaddedNumberField or TwoDigitNumberField.
                throw new IllegalArgumentException();
            }
            mField = field;
            mSize = size;
        }

        public int estimateLength() {
            return 4;
        }

        public void appendTo(StringBuffer buffer, Calendar calendar) {
            appendTo(buffer, calendar.get(mField));
        }

        public final void appendTo(StringBuffer buffer, int value) {
            if (value < 100) {
                for (int i = mSize; --i >= 2; ) {
                    buffer.append('0');
                }
                buffer.append((char)(value / 10 + '0'));
                buffer.append((char)(value % 10 + '0'));
            }
            else {
                int digits;
                if (value < 1000) {
                    digits = 3;
                }
                else {
                    digits = (int)(Math.log(value) / LOG_10) + 1;
                }
                for (int i = mSize; --i >= digits; ) {
                    buffer.append('0');
                }
                buffer.append(Integer.toString(value));
            }
        }
    }

    /**
     * Inner class to output a two digit number.
     */
    private static class TwoDigitNumberField implements NumberRule {
        private final int mField;

        TwoDigitNumberField(int field) {
            mField = field;
        }

        public int estimateLength() {
            return 2;
        }

        public void appendTo(StringBuffer buffer, Calendar calendar) {
            appendTo(buffer, calendar.get(mField));
        }

        public final void appendTo(StringBuffer buffer, int value) {
            if (value < 100) {
                buffer.append((char)(value / 10 + '0'));
                buffer.append((char)(value % 10 + '0'));
            }
            else {
                buffer.append(Integer.toString(value));
            }
        }
    }

    /**
     * Inner class to output a two digit year.
     */
    private static class TwoDigitYearField implements NumberRule {
        TwoDigitYearField() {
        }

        public int estimateLength() {
            return 2;
        }

        public void appendTo(StringBuffer buffer, Calendar calendar) {
            appendTo(buffer, calendar.get(Calendar.YEAR) % 100);
        }

        public final void appendTo(StringBuffer buffer, int value) {
            buffer.append((char)(value / 10 + '0'));
            buffer.append((char)(value % 10 + '0'));
        }
    }

    /**
     * Inner class to output a two digit month.
     */
    private static class TwoDigitMonthField implements NumberRule {
        TwoDigitMonthField() {
        }

        public int estimateLength() {
            return 2;
        }

        public void appendTo(StringBuffer buffer, Calendar calendar) {
            appendTo(buffer, calendar.get(Calendar.MONTH) + 1);
        }

        public final void appendTo(StringBuffer buffer, int value) {
            buffer.append((char)(value / 10 + '0'));
            buffer.append((char)(value % 10 + '0'));
        }
    }

    /**
     * Inner class to output the twelve hour field.
     */
    private static class TwelveHourField implements NumberRule {
        private final NumberRule mRule;

        TwelveHourField(NumberRule rule) {
            mRule = rule;
        }

        public int estimateLength() {
            return mRule.estimateLength();
        }

        public void appendTo(StringBuffer buffer, Calendar calendar) {
            int value = calendar.get(Calendar.HOUR);
            if (value == 0) {
                value = calendar.getLeastMaximum(Calendar.HOUR) + 1;
            }
            mRule.appendTo(buffer, value);
        }

        public void appendTo(StringBuffer buffer, int value) {
            mRule.appendTo(buffer, value);
        }
    }

    /**
     * Inner class to output the twenty four hour field.
     */
    private static class TwentyFourHourField implements NumberRule {
        private final NumberRule mRule;

        TwentyFourHourField(NumberRule rule) {
            mRule = rule;
        }

        public int estimateLength() {
            return mRule.estimateLength();
        }

        public void appendTo(StringBuffer buffer, Calendar calendar) {
            int value = calendar.get(Calendar.HOUR_OF_DAY);
            if (value == 0) {
                value = calendar.getMaximum(Calendar.HOUR_OF_DAY) + 1;
            }
            mRule.appendTo(buffer, value);
        }

        public void appendTo(StringBuffer buffer, int value) {
            mRule.appendTo(buffer, value);
        }
    }

    /**
     * Inner class to output a time zone.
     */
    private static class TimeZoneRule implements Rule {
        private final TimeZone mTimeZone;
        private final Locale mLocale;
        private final int mStyle;
        private final String mStandard;
        private final String mDaylight;

        TimeZoneRule(TimeZone timeZone, Locale locale, int style) {
            mTimeZone = timeZone;
            mLocale = locale;
            mStyle = style;

            if (timeZone != null) {
                mStandard = getTimeZoneDisplay(timeZone, false, style, locale);
                mDaylight = getTimeZoneDisplay(timeZone, true, style, locale);
            }
            else {
                mStandard = null;
                mDaylight = null;
            }
        }

        public int estimateLength() {
            if (mTimeZone != null) {
                return Math.max(mStandard.length(), mDaylight.length());
            }
            else if (mStyle == TimeZone.SHORT) {
                return 4;
            }
            else {
                return 40;
            }
        }

        public void appendTo(StringBuffer buffer, Calendar calendar) {
            TimeZone timeZone;
            if ((timeZone = mTimeZone) != null) {
                if (timeZone.useDaylightTime() &&
                    calendar.get(Calendar.DST_OFFSET) != 0) {

                    buffer.append(mDaylight);
                }
                else {
                    buffer.append(mStandard);
                }
            }
            else {
                timeZone = calendar.getTimeZone();
                if (timeZone.useDaylightTime() &&
                    calendar.get(Calendar.DST_OFFSET) != 0) {

                    buffer.append(getTimeZoneDisplay
                                  (timeZone, true, mStyle, mLocale));
                }
                else {
                    buffer.append(getTimeZoneDisplay
                                  (timeZone, false, mStyle, mLocale));
                }
            }
        }
    }

    // ----------------------------------------------------------------------------------
    /**
     * Inner class that acts as a compound key for time zone names.
     */
    private static class TimeZoneDisplayKey {
        private final TimeZone mTimeZone;
        private final int mStyle;
        private final Locale mLocale;

        TimeZoneDisplayKey(TimeZone timeZone,
                           boolean daylight, int style, Locale locale) {
            mTimeZone = timeZone;
            if (daylight) {
                style |= 0x80000000;
            }
            mStyle = style;
            mLocale = locale;
        }

        public int hashCode() {
            return mStyle * 31 + mLocale.hashCode();
        }

        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj instanceof TimeZoneDisplayKey) {
                TimeZoneDisplayKey other = (TimeZoneDisplayKey)obj;
                return
                    mTimeZone.equals(other.mTimeZone) &&
                    mStyle == other.mStyle &&
                    mLocale.equals(other.mLocale);
            }
            return false;
        }
    }

    // ----------------------------------------------------------------------------------
    /**
     * Helper class for creating compound objects.  One use for this class is to create a
     * hashtable key out of multiple objects.
     */
    private static class Pair implements Comparable, Serializable {
        private final Object mObj1;
        private final Object mObj2;

        public Pair(Object obj1, Object obj2) {
            mObj1 = obj1;
            mObj2 = obj2;
        }

        public int compareTo(Object obj) {
            if (this == obj) {
                return 0;
            }

            Pair other = (Pair)obj;

            Object a = mObj1;
            Object b = other.mObj1;

            firstTest: {
                if (a == null) {
                    if (b != null) {
                        return 1;
                    }
                    // Both a and b are null.
                    break firstTest;
                }
                else {
                    if (b == null) {
                        return -1;
                    }
                }

                int result = ((Comparable)a).compareTo(b);

                if (result != 0) {
                    return result;
                }
            }

            a = mObj2;
            b = other.mObj2;

            if (a == null) {
                if (b != null) {
                    return 1;
                }
                // Both a and b are null.
                return 0;
            }
            else {
                if (b == null) {
                    return -1;
                }
            }

            return ((Comparable)a).compareTo(b);
        }

        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }

            if (!(obj instanceof Pair)) {
                return false;
            }

            Pair key = (Pair)obj;

            return
                (mObj1 == null ?
                 key.mObj1 == null : mObj1.equals(key.mObj1)) &&
                (mObj2 == null ?
                 key.mObj2 == null : mObj2.equals(key.mObj2));
        }

        public int hashCode() {
            return
                (mObj1 == null ? 0 : mObj1.hashCode()) +
                (mObj2 == null ? 0 : mObj2.hashCode());
        }

        public String toString() {
            return "[" + mObj1 + ':' + mObj2 + ']';
        }
    }

}
