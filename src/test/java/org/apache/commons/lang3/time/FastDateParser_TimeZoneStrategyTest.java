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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.fail;
import static org.junit.jupiter.api.Assumptions.assumeFalse;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import java.text.DateFormatSymbols;
import java.text.ParseException;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;
import java.util.TimeZone;
import java.util.concurrent.atomic.AtomicInteger;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.ArraySorter;
import org.apache.commons.lang3.JavaVersion;
import org.apache.commons.lang3.LocaleUtils;
import org.apache.commons.lang3.SystemUtils;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.junitpioneer.jupiter.DefaultLocale;
import org.junitpioneer.jupiter.DefaultTimeZone;
import org.junitpioneer.jupiter.ReadsDefaultLocale;
import org.junitpioneer.jupiter.ReadsDefaultTimeZone;

/**
 * Tests {@link FastDateParser}.
 */
/* Make test reproducible */ @DefaultLocale(language = "en")
/* Make test reproducible */ @DefaultTimeZone(TimeZones.GMT_ID)
/* Make test reproducible */ @ReadsDefaultLocale
/* Make test reproducible */ @ReadsDefaultTimeZone
class FastDateParser_TimeZoneStrategyTest extends AbstractLangTest {

    private static final List<Locale> Java11Failures = new ArrayList<>();
    private static final List<Locale> Java17Failures = new ArrayList<>();
    private static final AtomicInteger fails = new AtomicInteger();

    @AfterAll
    public static void afterAll() {
        if (!Java17Failures.isEmpty()) {
            System.err.printf("Actual failures on Java 17+: %,d%n%s%n", Java17Failures.size(), Java17Failures);
        }
        if (!Java11Failures.isEmpty()) {
            System.err.printf("Actual failures on Java 11: %,d%n%s%n", Java11Failures.size(), Java11Failures);
        }
    }

    static Set<Entry<String, String>> getZoneIdStream() {
        return ZoneId.SHORT_IDS.entrySet();
    }

    private String[][] getZoneStringsSorted(final Locale locale) {
        return ArraySorter.sort(DateFormatSymbols.getInstance(locale).getZoneStrings(), Comparator.comparing(array -> array[0]));
    }

    /**
     * Tests that known short {@link ZoneId}s still parse since all short IDs are deprecated starting in Java 25, but are not removed.
     *
     * TODO: Why don't all short IDs parse, even on Java 8?
     *
     * @throws ParseException Thrown on test failure.
     */
    @ParameterizedTest
    @ValueSource(strings = { "ACT", "CST" })
    void testJava25DeprecatedZoneId(final String shortId) throws ParseException {
        final FastDateParser parser = new FastDateParser("dd.MM.yyyy HH:mm:ss z", TimeZone.getTimeZone(shortId), Locale.getDefault());
        final Date date1 = parser.parse("26.10.2014 02:00:00 " + shortId);
        // 1) parsing returns a value and doesn't throw.
        assertNotNull(date1);
        // 2) Something reasonable, note that getYear() subtracts 1900.
        assertEquals(2014, date1.getYear() + 1900);
    }

    /**
     * Tests that {@link ZoneId#SHORT_IDS} keys and values still works as they are deprecated starting in Java 25, but not removed yet.
     *
     * TODO: Why don't all short IDs parse, even on Java 8?
     *
     * @throws ParseException Thrown on test failure.
     */
    @Disabled
    @ParameterizedTest
    @MethodSource("getZoneIdStream")
    void testJava25DeprecatedZoneIds(final Map.Entry<String, String> entry) throws ParseException {
        final FastDateParser parser = new FastDateParser("dd.MM.yyyy HH:mm:ss z", TimeZone.getDefault(), Locale.GERMAN);
        final Date date1 = parser.parse("26.10.2014 02:00:00 " + entry.getKey());
        final Date date2 = parser.parse("26.10.2014 02:00:00 " + entry.getValue());
        assertNotEquals(date1.getTime(), date2.getTime());
    }

    @Test
    void testLang1219() throws ParseException {
        final FastDateParser parser = new FastDateParser("dd.MM.yyyy HH:mm:ss z", TimeZone.getDefault(), Locale.GERMAN);
        final Date summer = parser.parse("26.10.2014 02:00:00 MESZ");
        final Date standard = parser.parse("26.10.2014 02:00:00 MEZ");
        assertNotEquals(summer.getTime(), standard.getTime());
    }

    @ParameterizedTest
    @MethodSource("org.apache.commons.lang3.LocaleUtils#availableLocaleList()")
    void testTimeZoneStrategy_DateFormatSymbols(final Locale locale) {
        testTimeZoneStrategyPattern_DateFormatSymbols_getZoneStrings(locale);
    }
    @ParameterizedTest
    @MethodSource("org.apache.commons.lang3.LocaleUtils#availableLocaleList()")
    void testTimeZoneStrategy_TimeZone(final Locale locale) {
        testTimeZoneStrategyPattern_TimeZone_getAvailableIDs(locale);
    }

    private void testTimeZoneStrategyPattern(final String languageTag, final String source) throws ParseException {
        final Locale locale = Locale.forLanguageTag(languageTag);
        final TimeZone timeZone = TimeZones.getTimeZone("Etc/UTC");
        assumeFalse(LocaleUtils.isLanguageUndetermined(locale), () -> toFailureMessage(locale, languageTag, timeZone));
        assumeTrue(LocaleUtils.isAvailableLocale(locale), () -> toFailureMessage(locale, languageTag, timeZone));
        final FastDateParser parser = new FastDateParser("z", timeZone, locale);
        parser.parse(source);
        testTimeZoneStrategyPattern_TimeZone_getAvailableIDs(locale);
    }

    private void testTimeZoneStrategyPattern_DateFormatSymbols_getZoneStrings(final Locale locale) {
        Objects.requireNonNull(locale, "locale");
        assumeFalse(LocaleUtils.isLanguageUndetermined(locale), () -> toFailureMessage(locale, null, null));
        assumeTrue(LocaleUtils.isAvailableLocale(locale), () -> toFailureMessage(locale, null, null));

        final String[][] zones = getZoneStringsSorted(locale);
        for (final String[] zone : zones) {
            for (int zIndex = 1; zIndex < zone.length; ++zIndex) {
                final String tzDisplay = zone[zIndex];
                if (tzDisplay == null) {
                    break;
                }
                final TimeZone timeZone = TimeZone.getDefault();
                final FastDateParser parser = new FastDateParser("z", timeZone, locale);
                // An exception will be thrown and the test will fail if parsing isn't successful
                try {
                    parser.parse(tzDisplay);
                } catch (final ParseException e) {
                    // Hack Start
                    // See failures on GitHub Actions builds for Java 17.
                    final String localeStr = locale.toString();
                    if (SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_17)
                            && (localeStr.contains("_") || "Coordinated Universal Time".equals(tzDisplay)
                                    || "sommartid – Atyrau".equals(tzDisplay))) {
                        Java17Failures.add(locale);
                        // Mark as an assumption failure instead of a hard fail
                        System.err.printf(
                                "[%,d][%s] Java %s %s - Mark as an assumption failure instead of a hard fail: locale = '%s', parse = '%s'%n",
                                fails.incrementAndGet(),
                                Thread.currentThread().getName(),
                                SystemUtils.JAVA_VENDOR,
                                SystemUtils.JAVA_VM_VERSION,
                                localeStr, tzDisplay);
                        assumeTrue(false, localeStr);
                        continue;
                    }
                    if (SystemUtils.IS_JAVA_11
                            && (localeStr.contains("_") || "Coordinated Universal Time".equals(tzDisplay))) {
                        Java11Failures.add(locale);
                        // Mark as an assumption failure instead of a hard fail
                        System.err.printf(
                                "[%,d][%s] Java %s %s - Mark as an assumption failure instead of a hard fail: locale = '%s', parse = '%s'%n",
                                fails.incrementAndGet(),
                                Thread.currentThread().getName(),
                                SystemUtils.JAVA_VENDOR,
                                SystemUtils.JAVA_VM_VERSION,
                                localeStr, tzDisplay);
                        assumeTrue(false, localeStr);
                        continue;
                    }
                    // Hack End
                    fail(String.format("%s: with locale = %s, zIndex = %,d, tzDisplay = '%s', parser = '%s'", e,
                            localeStr, zIndex, tzDisplay, parser), e);
                }
            }
        }
    }

    /**
     * Breaks randomly on GitHub for Locale "pt_PT", TimeZone "Etc/UTC" if we do not check if the Locale's language is "undetermined".
     *
     * @throws ParseException
     */
    private void testTimeZoneStrategyPattern_TimeZone_getAvailableIDs(final Locale locale) {
        Objects.requireNonNull(locale, "locale");
        assumeFalse(LocaleUtils.isLanguageUndetermined(locale), () -> toFailureMessage(locale, null, null));
        assumeTrue(LocaleUtils.isAvailableLocale(locale), () -> toFailureMessage(locale, null, null));
        for (final String id : TimeZones.SORTED_AVAILABLE_IDS) {
            final TimeZone timeZone = TimeZones.getTimeZone(id);
            final String displayName = timeZone.getDisplayName(locale);
            final FastDateParser parser = new FastDateParser("z", timeZone, locale);
            try {
                parser.parse(displayName);
            } catch (final ParseException e) {
                // Missing "Zulu" or something else in broken JDK's GH builds?
                // Call LocaleUtils again
                fail(String.format("%s: with id = '%s', displayName = '%s', %s, parser = '%s'", e, id, displayName,
                        toFailureMessage(locale, null, timeZone), parser.toStringAll()), e);
            }
        }
    }

    @Test
    void testTimeZoneStrategyPattern_zh_HK_Hans() throws ParseException {
        testTimeZoneStrategyPattern("zh_HK_#Hans", "?????????");
    }

    /**
     * Breaks randomly on GitHub CI for Locale "pt_PT", TimeZone "Etc/UTC" if we do not check if the Locale's language is "undetermined".
     *
     * <pre>{@code
     * java.text.ParseException: Unparseable date: Horário do Meridiano de Greenwich: with tzDefault =
     * sun.util.calendar.ZoneInfo[id="Etc/UTC",offset=0,dstSavings=0,useDaylight=false,transitions=0,lastRule=null], locale = pt_LU, zones[][] size = '601',
     * zone[] size = '7', zIndex = 3, tzDisplay = 'Horário do Meridiano de Greenwich'
     * }</pre>
     *
     * @throws ParseException Test failure
     */
    @Test
    void testTimeZoneStrategyPatternPortugal_PT() throws ParseException {
        testTimeZoneStrategyPattern("pt_PT", "Horário do Meridiano de Greenwich");
    }

    /**
     * Breaks randomly on GitHub CI for Java 25 and Locale "pt_ST", TimeZone "Hora padrão de Damasco".
     *
     * @throws ParseException Test failure
     */
    @Test
    void testTimeZoneStrategyPatternPortugal_ST() throws ParseException {
        testTimeZoneStrategyPattern("pt_ST", "Hora padrão de Damasco");
    }

    /**
     * Breaks randomly on GitHub CI for Java 25 and Locale "pt_TL", TimeZone "Hora padrão de Damasco".
     *
     * @throws ParseException Test failure
     */
    @Test
    void testTimeZoneStrategyPatternPortugal_TL() throws ParseException {
        testTimeZoneStrategyPattern("pt_TL", "Hora padrão de Damasco");
    }

    /**
     * Breaks randomly on GitHub for Locale "sr_ME_#Cyrl", TimeZone "Etc/UTC" if we do not check if the Locale's language is "undetermined".
     *
     * <pre>{@code
     * java.text.ParseException: Unparseable date: Srednje vreme po Griniču: with tzDefault = sun.util.calendar.ZoneInfo[id="Etc/UTC",
     * offset=0,dstSavings=0,useDaylight=false,transitions=0,lastRule=null], locale = sr_ME_#Cyrl, zones[][] size = '601',
     * zone[] size = '7', zIndex = 3, tzDisplay = 'Srednje vreme po Griniču'
     * }</pre>
     *
     * @throws ParseException Test failure
     */
    @Test
    void testTimeZoneStrategyPatternSuriname() throws ParseException {
        testTimeZoneStrategyPattern("sr_ME_#Cyrl", "Srednje vreme po Griniču");
    }

    private String toFailureMessage(final Locale locale, final String languageTag, final TimeZone timeZone) {
        return String.format("locale = %s, languageTag = '%s', isAvailableLocale = %s, isLanguageUndetermined = %s, timeZone = %s", languageTag, locale,
                LocaleUtils.isAvailableLocale(locale), LocaleUtils.isLanguageUndetermined(locale), TimeZones.toTimeZone(timeZone));
    }
}
