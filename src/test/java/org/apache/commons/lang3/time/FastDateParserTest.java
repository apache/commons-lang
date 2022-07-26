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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.Serializable;
import java.text.ParseException;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;
import java.util.stream.Stream;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.LocaleUtils;
import org.apache.commons.lang3.SerializationUtils;
import org.apache.commons.lang3.SystemUtils;
import org.apache.commons.lang3.function.TriFunction;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Unit tests {@link org.apache.commons.lang3.time.FastDateParser}.
 *
 * @since 3.2
 */
public class FastDateParserTest extends AbstractLangTest {

    private enum Expected1806 {
        India(INDIA, "+05", "+0530", "+05:30", true), Greenwich(TimeZones.GMT, "Z", "Z", "Z", false),
        NewYork(NEW_YORK, "-05", "-0500", "-05:00", false);

        final TimeZone zone;

        final String one;
        final String two;
        final String three;
        final long offset;

        Expected1806(final TimeZone zone, final String one, final String two, final String three,
            final boolean hasHalfHourOffset) {
            this.zone = zone;
            this.one = one;
            this.two = two;
            this.three = three;
            this.offset = hasHalfHourOffset ? 30 * 60 * 1000 : 0;
        }
    }

    static final String DATE_PARSER_PARAMETERS = "dateParserParameters";

    static final String SHORT_FORMAT_NOERA = "y/M/d/h/a/m/s/E";

    static final String LONG_FORMAT_NOERA = "yyyy/MMMM/dddd/hhhh/mmmm/ss/aaaa/EEEE";
    static final String SHORT_FORMAT = "G/" + SHORT_FORMAT_NOERA;
    static final String LONG_FORMAT = "GGGG/" + LONG_FORMAT_NOERA;

    private static final String yMdHmsSZ = "yyyy-MM-dd'T'HH:mm:ss.SSS Z";
    private static final String DMY_DOT = "dd.MM.yyyy";
    private static final String YMD_SLASH = "yyyy/MM/dd";
    private static final String MDY_DASH = "MM-DD-yyyy";
    private static final String MDY_SLASH = "MM/DD/yyyy";

    private static final TimeZone REYKJAVIK = TimeZone.getTimeZone("Atlantic/Reykjavik");
    private static final TimeZone NEW_YORK = TimeZone.getTimeZone("America/New_York");
    private static final TimeZone INDIA = TimeZone.getTimeZone("Asia/Calcutta");

    private static final Locale SWEDEN = new Locale("sv", "SE");

    static void checkParse(final Locale locale, final Calendar cal, final SimpleDateFormat simpleDateFormat,
            final DateParser dateParser) {
        final String formattedDate = simpleDateFormat.format(cal.getTime());
        checkParse(locale, simpleDateFormat, dateParser, formattedDate, formattedDate);
        checkParse(locale, simpleDateFormat, dateParser, formattedDate.toLowerCase(locale), formattedDate);
        checkParse(locale, simpleDateFormat, dateParser, formattedDate.toUpperCase(locale), formattedDate);
    }

    static void checkParse(final Locale locale, final SimpleDateFormat simpleDateFormat, final DateParser dateParser,
        final String formattedDate, final String originalFormattedDate) {
        try {
            final Date expectedTime = simpleDateFormat.parse(formattedDate);
            final Date actualTime = dateParser.parse(formattedDate);
            assertEquals(expectedTime, actualTime,
                "locale: " + locale + ", formattedDate: '" + formattedDate + "', originalFormattedDate: '"
                    + originalFormattedDate + ", simpleDateFormat.pattern: '" + simpleDateFormat + "', Java: "
                    + SystemUtils.JAVA_RUNTIME_VERSION + "\n");
        } catch (final Exception e) {
            fail("locale: " + locale + ", formattedDate: '" + formattedDate + "', error : " + e + "\n", e);
        }
    }

    static Stream<Arguments> dateParserParameters() {
        return Stream.of(
        // @formatter:off
            Arguments.of((TriFunction<String, TimeZone, Locale, DateParser>) (format, timeZone, locale)
                -> new FastDateParser(format, timeZone, locale, null)),
            Arguments.of((TriFunction<String, TimeZone, Locale, DateParser>) FastDateFormat::getInstance)
        // @formatter:on
        );
    }

    private static Calendar initializeCalendar(final TimeZone timeZone) {
        final Calendar cal = Calendar.getInstance(timeZone);
        cal.set(Calendar.YEAR, 2001);
        cal.set(Calendar.MONTH, 1); // not daylight savings
        cal.set(Calendar.DAY_OF_MONTH, 4);
        cal.set(Calendar.HOUR_OF_DAY, 12);
        cal.set(Calendar.MINUTE, 8);
        cal.set(Calendar.SECOND, 56);
        cal.set(Calendar.MILLISECOND, 235);
        return cal;
    }

    private final TriFunction<String, TimeZone, Locale, DateParser> dateParserProvider = (format, timeZone,
            locale) -> new FastDateParser(format, timeZone, locale, null);

    private DateParser getDateInstance(final int dateStyle, final Locale locale) {
        return getInstance(null, FormatCache.getPatternForStyle(Integer.valueOf(dateStyle), null, locale),
            TimeZone.getDefault(), Locale.getDefault());
    }

    private Calendar getEraStart(int year, final TimeZone zone, final Locale locale) {
        final Calendar cal = Calendar.getInstance(zone, locale);
        cal.clear();

        // https://docs.oracle.com/javase/8/docs/technotes/guides/intl/calendar.doc.html
        if (locale.equals(FastDateParser.JAPANESE_IMPERIAL)) {
            if (year < 1868) {
                cal.set(Calendar.ERA, 0);
                cal.set(Calendar.YEAR, 1868 - year);
            }
        } else {
            if (year < 0) {
                cal.set(Calendar.ERA, GregorianCalendar.BC);
                year = -year;
            }
            cal.set(Calendar.YEAR, year / 100 * 100);
        }
        return cal;
    }

    DateParser getInstance(final String format) {
        return getInstance(null, format, TimeZone.getDefault(), Locale.getDefault());
    }

    DateParser getInstance(final String format, final Locale locale) {
        return getInstance(null, format, TimeZone.getDefault(), locale);
    }

    private DateParser getInstance(final String format, final TimeZone timeZone) {
        return getInstance(null, format, timeZone, Locale.getDefault());
    }

    /**
     * Override this method in derived tests to change the construction of instances
     *
     * @param dpProvider TODO
     * @param format the format string to use
     * @param timeZone the time zone to use
     * @param locale the locale to use
     *
     * @return the DateParser instance to use for testing
     */
    protected DateParser getInstance(final TriFunction<String, TimeZone, Locale, DateParser> dpProvider,
        final String format, final TimeZone timeZone, final Locale locale) {
        return (dpProvider == null ? this.dateParserProvider : dpProvider).apply(format, timeZone, locale);
    }

    @ParameterizedTest
    @MethodSource(DATE_PARSER_PARAMETERS)
    public void test_Equality_Hash(final TriFunction<String, TimeZone, Locale, DateParser> dpProvider) {
        final DateParser[] parsers = {getInstance(dpProvider, yMdHmsSZ, NEW_YORK, Locale.US),
            getInstance(dpProvider, DMY_DOT, NEW_YORK, Locale.US),
            getInstance(dpProvider, YMD_SLASH, NEW_YORK, Locale.US),
            getInstance(dpProvider, MDY_DASH, NEW_YORK, Locale.US),
            getInstance(dpProvider, MDY_SLASH, NEW_YORK, Locale.US),
            getInstance(dpProvider, MDY_SLASH, REYKJAVIK, Locale.US),
            getInstance(dpProvider, MDY_SLASH, REYKJAVIK, SWEDEN)};

        final Map<DateParser, Integer> map = new HashMap<>();
        int i = 0;
        for (final DateParser parser : parsers) {
            map.put(parser, Integer.valueOf(i++));
        }

        i = 0;
        for (final DateParser parser : parsers) {
            assertEquals(i++, map.get(parser).intValue());
        }
    }

    @Test
    public void test1806() throws ParseException {
        final String formatStub = "yyyy-MM-dd'T'HH:mm:ss.SSS";
        final String dateStub = "2001-02-04T12:08:56.235";

        for (final Expected1806 trial : Expected1806.values()) {
            final Calendar cal = initializeCalendar(trial.zone);

            final String message = trial.zone.getDisplayName() + ";";

            DateParser parser = getInstance(formatStub + "X", trial.zone);
            assertEquals(cal.getTime().getTime(), parser.parse(dateStub + trial.one).getTime() - trial.offset,
                message + trial.one);

            parser = getInstance(formatStub + "XX", trial.zone);
            assertEquals(cal.getTime(), parser.parse(dateStub + trial.two), message + trial.two);

            parser = getInstance(formatStub + "XXX", trial.zone);
            assertEquals(cal.getTime(), parser.parse(dateStub + trial.three), message + trial.three);
        }
    }

    @Test
    public void test1806Argument() {
        assertThrows(IllegalArgumentException.class, () -> getInstance("XXXX"));
    }

    @ParameterizedTest
    @MethodSource(DATE_PARSER_PARAMETERS)
    public void testAmPm(final TriFunction<String, TimeZone, Locale, DateParser> dpProvider) throws ParseException {
        final Calendar cal = Calendar.getInstance(NEW_YORK, Locale.US);
        cal.clear();

        final DateParser h = getInstance(dpProvider, "yyyy-MM-dd hh a mm:ss", NEW_YORK, Locale.US);
        final DateParser K = getInstance(dpProvider, "yyyy-MM-dd KK a mm:ss", NEW_YORK, Locale.US);
        final DateParser k = getInstance(dpProvider, "yyyy-MM-dd kk:mm:ss", NEW_YORK, Locale.US);
        final DateParser H = getInstance(dpProvider, "yyyy-MM-dd HH:mm:ss", NEW_YORK, Locale.US);

        cal.set(2010, Calendar.AUGUST, 1, 0, 33, 20);
        assertEquals(cal.getTime(), h.parse("2010-08-01 12 AM 33:20"));
        assertEquals(cal.getTime(), K.parse("2010-08-01 0 AM 33:20"));
        assertEquals(cal.getTime(), k.parse("2010-08-01 00:33:20"));
        assertEquals(cal.getTime(), H.parse("2010-08-01 00:33:20"));

        cal.set(2010, Calendar.AUGUST, 1, 3, 33, 20);
        assertEquals(cal.getTime(), h.parse("2010-08-01 3 AM 33:20"));
        assertEquals(cal.getTime(), K.parse("2010-08-01 3 AM 33:20"));
        assertEquals(cal.getTime(), k.parse("2010-08-01 03:33:20"));
        assertEquals(cal.getTime(), H.parse("2010-08-01 03:33:20"));

        cal.set(2010, Calendar.AUGUST, 1, 15, 33, 20);
        assertEquals(cal.getTime(), h.parse("2010-08-01 3 PM 33:20"));
        assertEquals(cal.getTime(), K.parse("2010-08-01 3 PM 33:20"));
        assertEquals(cal.getTime(), k.parse("2010-08-01 15:33:20"));
        assertEquals(cal.getTime(), H.parse("2010-08-01 15:33:20"));

        cal.set(2010, Calendar.AUGUST, 1, 12, 33, 20);
        assertEquals(cal.getTime(), h.parse("2010-08-01 12 PM 33:20"));
        assertEquals(cal.getTime(), K.parse("2010-08-01 0 PM 33:20"));
        assertEquals(cal.getTime(), k.parse("2010-08-01 12:33:20"));
        assertEquals(cal.getTime(), H.parse("2010-08-01 12:33:20"));
    }

    @Test
    public void testDayNumberOfWeek() throws ParseException {
        final DateParser parser = getInstance("u");
        final Calendar calendar = Calendar.getInstance();

        calendar.setTime(parser.parse("1"));
        assertEquals(Calendar.MONDAY, calendar.get(Calendar.DAY_OF_WEEK));

        calendar.setTime(parser.parse("6"));
        assertEquals(Calendar.SATURDAY, calendar.get(Calendar.DAY_OF_WEEK));

        calendar.setTime(parser.parse("7"));
        assertEquals(Calendar.SUNDAY, calendar.get(Calendar.DAY_OF_WEEK));
    }

    @ParameterizedTest
    @MethodSource(DATE_PARSER_PARAMETERS)
    public void testDayOf(final TriFunction<String, TimeZone, Locale, DateParser> dpProvider) throws ParseException {
        final Calendar cal = Calendar.getInstance(NEW_YORK, Locale.US);
        cal.clear();
        cal.set(2003, Calendar.FEBRUARY, 10);

        final DateParser fdf = getInstance(dpProvider, "W w F D y", NEW_YORK, Locale.US);
        assertEquals(cal.getTime(), fdf.parse("3 7 2 41 03"));
    }

    @Test
    public void testEquals() {
        final DateParser parser1 = getInstance(YMD_SLASH);
        final DateParser parser2 = getInstance(YMD_SLASH);

        assertEquals(parser1, parser2);
        assertEquals(parser1.hashCode(), parser2.hashCode());

        assertNotEquals(parser1, new Object());
    }

    @Test
    public void testJpLocales() {

        final Calendar cal = Calendar.getInstance(TimeZones.GMT);
        cal.clear();
        cal.set(2003, Calendar.FEBRUARY, 10);
        cal.set(Calendar.ERA, GregorianCalendar.BC);

        final Locale locale = LocaleUtils.toLocale("zh");
        // ja_JP_JP cannot handle dates before 1868 properly

        final SimpleDateFormat sdf = new SimpleDateFormat(LONG_FORMAT, locale);
        final DateParser fdf = getInstance(LONG_FORMAT, locale);

        // If parsing fails, a ParseException will be thrown and the test will fail
        checkParse(locale, cal, sdf, fdf);
    }

    @ParameterizedTest
    @MethodSource(DATE_PARSER_PARAMETERS)
    public void testLANG_831(final TriFunction<String, TimeZone, Locale, DateParser> dpProvider) throws Exception {
        testSdfAndFdp(dpProvider, "M E", "3  Tue", true);
    }

    @ParameterizedTest
    @MethodSource(DATE_PARSER_PARAMETERS)
    public void testLANG_832(final TriFunction<String, TimeZone, Locale, DateParser> dpProvider) throws Exception {
        testSdfAndFdp(dpProvider, "'d'd", "d3", false); // OK
        testSdfAndFdp(dpProvider, "'d'd'", "d3", true); // should fail (unterminated quote)
    }

    @ParameterizedTest
    @MethodSource(DATE_PARSER_PARAMETERS)
    public void testLang1121(final TriFunction<String, TimeZone, Locale, DateParser> dpProvider) throws ParseException {
        final TimeZone kst = TimeZone.getTimeZone("KST");
        final DateParser fdp = getInstance(dpProvider, "yyyyMMdd", kst, Locale.KOREA);

        assertThrows(ParseException.class, () -> fdp.parse("2015"));

        // Wed Apr 29 00:00:00 KST 2015
        Date actual = fdp.parse("20150429");
        final Calendar cal = Calendar.getInstance(kst, Locale.KOREA);
        cal.clear();
        cal.set(2015, 3, 29);
        Date expected = cal.getTime();
        assertEquals(expected, actual);

        final SimpleDateFormat df = new SimpleDateFormat("yyyyMMdd", Locale.KOREA);
        df.setTimeZone(kst);
        expected = df.parse("20150429113100");

        // Thu Mar 16 00:00:00 KST 81724
        actual = fdp.parse("20150429113100");
        assertEquals(expected, actual);
    }

    @ParameterizedTest
    @MethodSource(DATE_PARSER_PARAMETERS)
    public void testLang1380(final TriFunction<String, TimeZone, Locale, DateParser> dpProvider) throws ParseException {
        final Calendar expected = Calendar.getInstance(TimeZones.GMT, Locale.FRANCE);
        expected.clear();
        expected.set(2014, Calendar.APRIL, 14);

        final DateParser fdp = getInstance(dpProvider, "dd MMM yyyy", TimeZones.GMT, Locale.FRANCE);
        assertEquals(expected.getTime(), fdp.parse("14 avril 2014"));
        assertEquals(expected.getTime(), fdp.parse("14 avr. 2014"));
        assertEquals(expected.getTime(), fdp.parse("14 avr 2014"));
    }

    @Test
    public void testLang303() throws ParseException {
        DateParser parser = getInstance(YMD_SLASH);
        final Calendar cal = Calendar.getInstance();
        cal.set(2004, Calendar.DECEMBER, 31);

        final Date date = parser.parse("2004/11/31");

        parser = SerializationUtils.deserialize(SerializationUtils.serialize((Serializable) parser));
        assertEquals(date, parser.parse("2004/11/31"));
    }

    @Test
    public void testLang538() throws ParseException {
        final DateParser parser = getInstance("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'", TimeZones.GMT);

        final Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT-8"));
        cal.clear();
        cal.set(2009, Calendar.OCTOBER, 16, 8, 42, 16);

        assertEquals(cal.getTime(), parser.parse("2009-10-16T16:42:16.000Z"));
    }

    @ParameterizedTest
    @MethodSource(DATE_PARSER_PARAMETERS)
    public void testLang996(final TriFunction<String, TimeZone, Locale, DateParser> dpProvider) throws ParseException {
        final Calendar expected = Calendar.getInstance(NEW_YORK, Locale.US);
        expected.clear();
        expected.set(2014, Calendar.MAY, 14);

        final DateParser fdp = getInstance(dpProvider, "ddMMMyyyy", NEW_YORK, Locale.US);
        assertEquals(expected.getTime(), fdp.parse("14may2014"));
        assertEquals(expected.getTime(), fdp.parse("14MAY2014"));
        assertEquals(expected.getTime(), fdp.parse("14May2014"));
    }

    @Test
    public void testLocaleMatches() {
        final DateParser parser = getInstance(yMdHmsSZ, SWEDEN);
        assertEquals(SWEDEN, parser.getLocale());
    }

    /**
     * Tests that pre-1000AD years get padded with yyyy
     *
     * @throws ParseException so we don't have to catch it
     */
    @Test
    public void testLowYearPadding() throws ParseException {
        final DateParser parser = getInstance(YMD_SLASH);
        final Calendar cal = Calendar.getInstance();
        cal.clear();

        cal.set(1, Calendar.JANUARY, 1);
        assertEquals(cal.getTime(), parser.parse("0001/01/01"));
        cal.set(10, Calendar.JANUARY, 1);
        assertEquals(cal.getTime(), parser.parse("0010/01/01"));
        cal.set(100, Calendar.JANUARY, 1);
        assertEquals(cal.getTime(), parser.parse("0100/01/01"));
        cal.set(999, Calendar.JANUARY, 1);
        assertEquals(cal.getTime(), parser.parse("0999/01/01"));
    }

    @Test
    public void testMilleniumBug() throws ParseException {
        final DateParser parser = getInstance(DMY_DOT);
        final Calendar cal = Calendar.getInstance();
        cal.clear();

        cal.set(1000, Calendar.JANUARY, 1);
        assertEquals(cal.getTime(), parser.parse("01.01.1000"));
    }

    @ParameterizedTest
    @MethodSource(DATE_PARSER_PARAMETERS)
    public void testParseLongShort(final TriFunction<String, TimeZone, Locale, DateParser> dpProvider)
        throws ParseException {
        final Calendar cal = Calendar.getInstance(NEW_YORK, Locale.US);
        cal.clear();
        cal.set(2003, Calendar.FEBRUARY, 10, 15, 33, 20);
        cal.set(Calendar.MILLISECOND, 989);
        cal.setTimeZone(NEW_YORK);

        DateParser fdf = getInstance(dpProvider, "yyyy GGGG MMMM dddd aaaa EEEE HHHH mmmm ssss SSSS ZZZZ", NEW_YORK,
            Locale.US);

        assertEquals(cal.getTime(), fdf.parse("2003 AD February 0010 PM Monday 0015 0033 0020 0989 GMT-05:00"));
        cal.set(Calendar.ERA, GregorianCalendar.BC);

        final Date parse = fdf.parse("2003 BC February 0010 PM Saturday 0015 0033 0020 0989 GMT-05:00");
        assertEquals(cal.getTime(), parse);

        fdf = getInstance(null, "y G M d a E H m s S Z", NEW_YORK, Locale.US);
        assertEquals(cal.getTime(), fdf.parse("03 BC 2 10 PM Sat 15 33 20 989 -0500"));

        cal.set(Calendar.ERA, GregorianCalendar.AD);
        assertEquals(cal.getTime(), fdf.parse("03 AD 2 10 PM Saturday 15 33 20 989 -0500"));
    }

    @ParameterizedTest
    @MethodSource(DATE_PARSER_PARAMETERS)
    public void testParseNumerics(final TriFunction<String, TimeZone, Locale, DateParser> dpProvider)
        throws ParseException {
        final Calendar cal = Calendar.getInstance(NEW_YORK, Locale.US);
        cal.clear();
        cal.set(2003, Calendar.FEBRUARY, 10, 15, 33, 20);
        cal.set(Calendar.MILLISECOND, 989);

        final DateParser fdf = getInstance(dpProvider, "yyyyMMddHHmmssSSS", NEW_YORK, Locale.US);
        assertEquals(cal.getTime(), fdf.parse("20030210153320989"));
    }

    @Test
    public void testParseOffset() {
        final DateParser parser = getInstance(YMD_SLASH);
        final Date date = parser.parse("Today is 2015/07/04", new ParsePosition(9));

        final Calendar cal = Calendar.getInstance();
        cal.clear();
        cal.set(2015, Calendar.JULY, 4);
        assertEquals(cal.getTime(), date);
    }

    @Test
    // Check that all Locales can parse the formats we use
    public void testParses() throws Exception {
        for (final String format : new String[] {LONG_FORMAT, SHORT_FORMAT}) {
            for (final Locale locale : Locale.getAvailableLocales()) {
                for (final TimeZone timeZone : new TimeZone[] {NEW_YORK, REYKJAVIK, TimeZones.GMT}) {
                    for (final int year : new int[] {2003, 1940, 1868, 1867, 1, -1, -1940}) {
                        final Calendar cal = getEraStart(year, timeZone, locale);
                        final Date centuryStart = cal.getTime();

                        cal.set(Calendar.MONTH, 1);
                        cal.set(Calendar.DAY_OF_MONTH, 10);
                        final Date in = cal.getTime();

                        final FastDateParser fastDateParser = new FastDateParser(format, timeZone, locale,
                            centuryStart);
                        validateSdfFormatFdpParseEquality(format, locale, timeZone, fastDateParser, in, year,
                            centuryStart);
                    }
                }
            }
        }
    }

    /**
     * Fails on Java 16 Early Access build 25 and above, last tested with build 36.
     */
    @Test
    public void testParsesKnownJava16Ea25Failure() throws Exception {
        final String format = LONG_FORMAT;
        final int year = 2003;
        final Locale locale = new Locale.Builder().setLanguage("sq").setRegion("MK").build();
        assertEquals("sq_MK", locale.toString());
        assertNotNull(locale);
        final TimeZone timeZone = NEW_YORK;
        final Calendar cal = getEraStart(year, timeZone, locale);
        final Date centuryStart = cal.getTime();

        cal.set(Calendar.MONTH, 1);
        cal.set(Calendar.DAY_OF_MONTH, 10);
        final Date in = cal.getTime();

        final FastDateParser fastDateParser = new FastDateParser(format, timeZone, locale, centuryStart);
        validateSdfFormatFdpParseEquality(format, locale, timeZone, fastDateParser, in, year, centuryStart);
    }

    @ParameterizedTest
    @MethodSource(DATE_PARSER_PARAMETERS)
    public void testParseZone(final TriFunction<String, TimeZone, Locale, DateParser> dpProvider)
        throws ParseException {
        final Calendar cal = Calendar.getInstance(NEW_YORK, Locale.US);
        cal.clear();
        cal.set(2003, Calendar.JULY, 10, 16, 33, 20);

        final DateParser fdf = getInstance(dpProvider, yMdHmsSZ, NEW_YORK, Locale.US);

        assertEquals(cal.getTime(), fdf.parse("2003-07-10T15:33:20.000 -0500"));
        assertEquals(cal.getTime(), fdf.parse("2003-07-10T15:33:20.000 GMT-05:00"));
        assertEquals(cal.getTime(), fdf.parse("2003-07-10T16:33:20.000 Eastern Daylight Time"));
        assertEquals(cal.getTime(), fdf.parse("2003-07-10T16:33:20.000 EDT"));

        cal.setTimeZone(TimeZone.getTimeZone("GMT-3"));
        cal.set(2003, Calendar.FEBRUARY, 10, 9, 0, 0);

        assertEquals(cal.getTime(), fdf.parse("2003-02-10T09:00:00.000 -0300"));

        cal.setTimeZone(TimeZone.getTimeZone("GMT+5"));
        cal.set(2003, Calendar.FEBRUARY, 10, 15, 5, 6);

        assertEquals(cal.getTime(), fdf.parse("2003-02-10T15:05:06.000 +0500"));
    }

    @Test
    public void testPatternMatches() {
        final DateParser parser = getInstance(yMdHmsSZ);
        assertEquals(yMdHmsSZ, parser.getPattern());
    }

    @ParameterizedTest
    @MethodSource(DATE_PARSER_PARAMETERS)
    public void testQuotes(final TriFunction<String, TimeZone, Locale, DateParser> dpProvider) throws ParseException {
        final Calendar cal = Calendar.getInstance(NEW_YORK, Locale.US);
        cal.clear();
        cal.set(2003, Calendar.FEBRUARY, 10, 15, 33, 20);
        cal.set(Calendar.MILLISECOND, 989);

        final DateParser fdf = getInstance(dpProvider, "''yyyyMMdd'A''B'HHmmssSSS''", NEW_YORK, Locale.US);
        assertEquals(cal.getTime(), fdf.parse("'20030210A'B153320989'"));
    }

    private void testSdfAndFdp(final TriFunction<String, TimeZone, Locale, DateParser> dbProvider, final String format,
        final String date, final boolean shouldFail) throws Exception {
        Date dfdp = null;
        Date dsdf = null;
        Throwable f = null;
        Throwable s = null;

        try {
            final SimpleDateFormat sdf = new SimpleDateFormat(format, Locale.US);
            sdf.setTimeZone(NEW_YORK);
            dsdf = sdf.parse(date);
            assertFalse(shouldFail, "Expected SDF failure, but got " + dsdf + " for [" + format + ", " + date + "]");
        } catch (final Exception e) {
            s = e;
            if (!shouldFail) {
                throw e;
            }
        }

        try {
            final DateParser fdp = getInstance(dbProvider, format, NEW_YORK, Locale.US);
            dfdp = fdp.parse(date);
            assertFalse(shouldFail, "Expected FDF failure, but got " + dfdp + " for [" + format + ", " + date + "]");
        } catch (final Exception e) {
            f = e;
            if (!shouldFail) {
                throw e;
            }
        }
        // SDF and FDF should produce equivalent results
        assertEquals((f == null), (s == null), "Should both or neither throw Exceptions");
        assertEquals(dsdf, dfdp, "Parsed dates should be equal");
    }

    /**
     * Test case for {@link FastDateParser#FastDateParser(String, TimeZone, Locale)}.
     *
     * @throws ParseException so we don't have to catch it
     */
    @Test
    public void testShortDateStyleWithLocales() throws ParseException {
        DateParser fdf = getDateInstance(FastDateFormat.SHORT, Locale.US);
        final Calendar cal = Calendar.getInstance();
        cal.clear();

        cal.set(2004, Calendar.FEBRUARY, 3);
        assertEquals(cal.getTime(), fdf.parse("2/3/04"));

        fdf = getDateInstance(FastDateFormat.SHORT, SWEDEN);
        assertEquals(cal.getTime(), fdf.parse("2004-02-03"));
    }

    @ParameterizedTest
    @MethodSource(DATE_PARSER_PARAMETERS)
    public void testSpecialCharacters(final TriFunction<String, TimeZone, Locale, DateParser> dpProvider)
        throws Exception {
        testSdfAndFdp(dpProvider, "q", "", true); // bad pattern character (at present)
        testSdfAndFdp(dpProvider, "Q", "", true); // bad pattern character
        testSdfAndFdp(dpProvider, "$", "$", false); // OK
        testSdfAndFdp(dpProvider, "?.d", "?.12", false); // OK
        testSdfAndFdp(dpProvider, "''yyyyMMdd'A''B'HHmmssSSS''", "'20030210A'B153320989'", false); // OK
        testSdfAndFdp(dpProvider, "''''yyyyMMdd'A''B'HHmmssSSS''", "''20030210A'B153320989'", false); // OK
        testSdfAndFdp(dpProvider, "'$\\Ed'", "$\\Ed", false); // OK

        // quoted characters are case-sensitive
        testSdfAndFdp(dpProvider, "'QED'", "QED", false);
        testSdfAndFdp(dpProvider, "'QED'", "qed", true);
        // case-sensitive after insensitive Month field
        testSdfAndFdp(dpProvider, "yyyy-MM-dd 'QED'", "2003-02-10 QED", false);
        testSdfAndFdp(dpProvider, "yyyy-MM-dd 'QED'", "2003-02-10 qed", true);
    }

    @Test
    public void testTimeZoneMatches() {
        final DateParser parser = getInstance(yMdHmsSZ, REYKJAVIK);
        assertEquals(REYKJAVIK, parser.getTimeZone());
    }

    @Test
    public void testToStringContainsName() {
        final DateParser parser = getInstance(YMD_SLASH);
        assertTrue(parser.toString().startsWith("FastDate"));
    }

    // we cannot use historic dates to test time zone parsing, some time zones have second offsets
    // as well as hours and minutes which makes the z formats a low fidelity round trip
    @Test
    public void testTzParses() throws Exception {
        // Check that all Locales can parse the time formats we use
        for (final Locale locale : Locale.getAvailableLocales()) {
            final FastDateParser fdp = new FastDateParser("yyyy/MM/dd z", TimeZone.getDefault(), locale);

            for (final TimeZone timeZone : new TimeZone[] {NEW_YORK, REYKJAVIK, TimeZones.GMT}) {
                final Calendar cal = Calendar.getInstance(timeZone, locale);
                cal.clear();
                cal.set(Calendar.YEAR, 2000);
                cal.set(Calendar.MONTH, 1);
                cal.set(Calendar.DAY_OF_MONTH, 10);
                final Date expected = cal.getTime();

                final Date actual = fdp.parse("2000/02/10 " + timeZone.getDisplayName(locale));
                assertEquals(expected, actual, "timeZone:" + timeZone.getID() + " locale:" + locale.getDisplayName());
            }
        }
    }

    private void validateSdfFormatFdpParseEquality(final String formatStr, final Locale locale, final TimeZone timeZone,
        final FastDateParser dateParser, final Date inDate, final int year, final Date csDate) throws ParseException {
        final SimpleDateFormat sdf = new SimpleDateFormat(formatStr, locale);
        sdf.setTimeZone(timeZone);
        if (formatStr.equals(SHORT_FORMAT)) {
            sdf.set2DigitYearStart(csDate);
        }
        final String fmt = sdf.format(inDate);
//        System.out.printf("[Java %s] Date: '%s' formatted with '%s' -> '%s'%n", SystemUtils.JAVA_RUNTIME_VERSION, inDate,
//            formatStr, fmt);
        try {
            final Date out = dateParser.parse(fmt);
            assertEquals(inDate, out, "format: '" + formatStr + "', locale: '" + locale + "', time zone: '"
                + timeZone.getID() + "', year: " + year + ", parse: '" + fmt);
        } catch (final ParseException pe) {
            if (year >= 1868 || !locale.getCountry().equals("JP")) {
                // LANG-978
                throw pe;
            }
        }
    }
}

