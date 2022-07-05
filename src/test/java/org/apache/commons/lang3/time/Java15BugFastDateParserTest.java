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

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.Locale;
import java.util.TimeZone;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.function.TriFunction;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * These tests fail on Java 15 due to a bug which was only fixed for Java 16.
 * <ul>
 * <li>https://bugs.openjdk.java.net/browse/JDK-8248434</li>
 * <li>https://bugs.openjdk.java.net/browse/JDK-8248655</li>
 * </ul>
 */
public class Java15BugFastDateParserTest extends AbstractLangTest {

    /** @see org.apache.commons.lang3.time.FastDateParserTest#dateParserParameters() */
    private static final String DATE_PARSER_PARAMETERS = "org.apache.commons.lang3.time.FastDateParserTest#dateParserParameters()";

    @Test
    public void java15BuggyLocaleTest() throws ParseException {
        final String buggyLocaleName = "ff_LR_#Adlm";
        Locale buggyLocale = null;
        for (final Locale locale : Locale.getAvailableLocales()) {
            if (buggyLocaleName.equals(locale.toString())) {
                buggyLocale = locale;
                break;
            }
        }
        if (buggyLocale == null) {
            return;
        }
        testSingleLocale(buggyLocale);
    }

    @Test
    public void java15BuggyLocaleTestAll() throws ParseException {
        for (final Locale locale : Locale.getAvailableLocales()) {
            testSingleLocale(locale);
        }
    }

    private void testLocales(final TriFunction<String, TimeZone, Locale, DateParser> dbProvider, final String format,
        final boolean eraBC) throws Exception {

        final Calendar cal = Calendar.getInstance(TimeZones.GMT);
        cal.clear();
        cal.set(2003, Calendar.FEBRUARY, 10);
        if (eraBC) {
            cal.set(Calendar.ERA, GregorianCalendar.BC);
        }

        for (final Locale locale : Locale.getAvailableLocales()) {
            // ja_JP_JP cannot handle dates before 1868 properly
            if (eraBC && locale.equals(FastDateParser.JAPANESE_IMPERIAL)) {
                continue;
            }
            final SimpleDateFormat sdf = new SimpleDateFormat(format, locale);
            final DateParser fdf = dbProvider.apply(format, TimeZone.getDefault(), locale);

            // If parsing fails, a ParseException will be thrown and the test will fail
            FastDateParserTest.checkParse(locale, cal, sdf, fdf);
        }
    }

    @ParameterizedTest
    @MethodSource(DATE_PARSER_PARAMETERS)
    public void testLocales_Long_AD(final TriFunction<String, TimeZone, Locale, DateParser> dpProvider)
        throws Exception {
        testLocales(dpProvider, FastDateParserTest.LONG_FORMAT, false);
    }

    @ParameterizedTest
    @MethodSource(DATE_PARSER_PARAMETERS)
    public void testLocales_Long_BC(final TriFunction<String, TimeZone, Locale, DateParser> dpProvider)
        throws Exception {
        testLocales(dpProvider, FastDateParserTest.LONG_FORMAT, true);
    }

    @ParameterizedTest
    @MethodSource(DATE_PARSER_PARAMETERS)
    public void testLocales_LongNoEra_AD(final TriFunction<String, TimeZone, Locale, DateParser> dpProvider)
        throws Exception {
        testLocales(dpProvider, FastDateParserTest.LONG_FORMAT_NOERA, false);
    }

    @ParameterizedTest
    @MethodSource(DATE_PARSER_PARAMETERS)
    public void testLocales_LongNoEra_BC(final TriFunction<String, TimeZone, Locale, DateParser> dpProvider)
        throws Exception {
        testLocales(dpProvider, FastDateParserTest.LONG_FORMAT_NOERA, true);
    }

    @ParameterizedTest
    @MethodSource(DATE_PARSER_PARAMETERS)
    public void testLocales_Short_AD(final TriFunction<String, TimeZone, Locale, DateParser> dpProvider)
        throws Exception {
        testLocales(dpProvider, FastDateParserTest.SHORT_FORMAT, false);
    }

    @ParameterizedTest
    @MethodSource(DATE_PARSER_PARAMETERS)
    public void testLocales_Short_BC(final TriFunction<String, TimeZone, Locale, DateParser> dpProvider)
        throws Exception {
        testLocales(dpProvider, FastDateParserTest.SHORT_FORMAT, true);
    }

    @ParameterizedTest
    @MethodSource(DATE_PARSER_PARAMETERS)
    public void testLocales_ShortNoEra_AD(final TriFunction<String, TimeZone, Locale, DateParser> dpProvider)
        throws Exception {
        testLocales(dpProvider, FastDateParserTest.SHORT_FORMAT_NOERA, false);
    }

    @ParameterizedTest
    @MethodSource(DATE_PARSER_PARAMETERS)
    public void testLocales_ShortNoEra_BC(final TriFunction<String, TimeZone, Locale, DateParser> dpProvider)
        throws Exception {
        testLocales(dpProvider, FastDateParserTest.SHORT_FORMAT_NOERA, true);
    }

    private void testSingleLocale(final Locale locale) throws ParseException {
        final Calendar cal = Calendar.getInstance(TimeZones.GMT);
        cal.clear();
        cal.set(2003, Calendar.FEBRUARY, 10);
        final SimpleDateFormat sdf = new SimpleDateFormat(FastDateParserTest.LONG_FORMAT, locale);
        final String formattedDate = sdf.format(cal.getTime());
        sdf.parse(formattedDate);
        sdf.parse(formattedDate.toUpperCase(locale));
        sdf.parse(formattedDate.toLowerCase(locale));
    }

}
