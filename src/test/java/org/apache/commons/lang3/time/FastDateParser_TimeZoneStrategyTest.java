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

import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.fail;
import static org.junit.jupiter.api.Assumptions.assumeFalse;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import java.text.ParseException;
import java.util.Date;
import java.util.Locale;
import java.util.Objects;
import java.util.TimeZone;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.LocaleUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.junitpioneer.jupiter.DefaultLocale;
import org.junitpioneer.jupiter.DefaultTimeZone;
import org.junitpioneer.jupiter.ReadsDefaultLocale;
import org.junitpioneer.jupiter.ReadsDefaultTimeZone;

/* make test reproducible */ @DefaultLocale("en_US")
/* make test reproducible */ @DefaultTimeZone(TimeZones.GMT_ID)
/* make test reproducible */ @ReadsDefaultLocale
/* make test reproducible */ @ReadsDefaultTimeZone
public class FastDateParser_TimeZoneStrategyTest extends AbstractLangTest {

    @Test
    public void testLang1219() throws ParseException {
        final FastDateParser parser = new FastDateParser("dd.MM.yyyy HH:mm:ss z", TimeZone.getDefault(), Locale.GERMAN);
        final Date summer = parser.parse("26.10.2014 02:00:00 MESZ");
        final Date standard = parser.parse("26.10.2014 02:00:00 MEZ");
        assertNotEquals(summer.getTime(), standard.getTime());
    }

    @ParameterizedTest
    @MethodSource("java.util.Locale#getAvailableLocales")
    public void testTimeZoneStrategyPattern(final Locale locale) {
        testTimeZoneStrategyPattern(locale, TimeZones.GMT);
    }

    /**
     * Breaks randomly on GitHub for Locale "pt_PT", TimeZone "Etc/UTC" if we do not check if the Locale's language is "undetermined".
     * 
     * @throws ParseException
     */
    private void testTimeZoneStrategyPattern(final Locale locale, final TimeZone tzDefault) {
        Objects.requireNonNull(locale, "locale");
        Objects.requireNonNull(tzDefault, "tzDefault");
        assumeFalse(LocaleUtils.isLanguageUndetermined(locale), () -> toFailureMessage(locale, null));
        assumeTrue(LocaleUtils.isAvailableLocale(locale), () -> toFailureMessage(locale, null));
        final FastDateParser parser = new FastDateParser("z", tzDefault, locale);

        for (final String id : TimeZone.getAvailableIDs()) {
            final TimeZone timeZone = TimeZone.getTimeZone(id);
            final String displayName = timeZone.getDisplayName(locale);
            try {
                parser.parse(displayName);
            } catch (ParseException e) {
                // Missing "Zulu" or something else in broken JDK's GH builds?
                fail(String.format("%s: with tzDefault = %s, locale = %s, id = %s, timeZone = %s, displayName = %s, parser = '%s'", e, tzDefault, locale,
                        displayName, id, timeZone, displayName, parser.toStringAll()), e);
            }
        }

// The above replaces what's below.
//
// Calling getZoneStrings() is not recommended in the Javadoc but not deprecated.
//
//        final String[][] zones = DateFormatSymbols.getInstance(locale).getZoneStrings();
//        for (final String[] zone : zones) {
//            for (int zIndex = 1; zIndex < zone.length; ++zIndex) {
//                final String tzDisplay = zone[zIndex];
//                if (tzDisplay == null) {
//                    break;
//                }
//                // An exception will be thrown and the test will fail if parsing isn't successful
//                try {
//                    parser.parse(tzDisplay);
//                } catch (ParseException e) {
//                    // Missing "Zulu" or something else in broken JDK's GH builds?
//                    final ByteArrayOutputStream zonesOut = new ByteArrayOutputStream();
//                    final PrintStream zonesPs = new PrintStream(zonesOut);
//                    final AtomicInteger i = new AtomicInteger();
//                    Stream.of(zones).forEach(zoneArray -> zonesPs.printf("[%,d] %s%n", i.getAndIncrement(), Arrays.toString(zoneArray)));
//                    fail(String.format(
//                            "%s: with tzDefault = %s, locale = %s, zones[][] size = '%s', zIndex = %,d, tzDisplay = '%s', parser = '%s', zones size = %,d, zones = %s",
//                            e, tzDefault, locale, zone.length, zIndex, tzDisplay, parser.toStringAll(), zones.length, zonesOut), e);
//                }
//            }
//        }
    }

    private void testTimeZoneStrategyPattern(final String languageTag, final String source) throws ParseException {
        final Locale locale = Locale.forLanguageTag(languageTag);
        assumeFalse(LocaleUtils.isLanguageUndetermined(locale), () -> toFailureMessage(locale, languageTag));
        assumeTrue(LocaleUtils.isAvailableLocale(locale), () -> toFailureMessage(locale, languageTag));
        final TimeZone tzDefault = TimeZone.getTimeZone("Etc/UTC");
        final FastDateParser parser = new FastDateParser("z", tzDefault, locale);
        parser.parse(source);
        testTimeZoneStrategyPattern(locale, tzDefault);
    }

    private String toFailureMessage(final Locale locale, final String languageTag) {
        return String.format("locale = %s, languageTag = '%s', isAvailableLocale = %s, isLanguageUndetermined = %s", languageTag, locale,
                LocaleUtils.isAvailableLocale(locale), LocaleUtils.isLanguageUndetermined(locale));
    }

    /**
     * Breaks randomly on GitHub for Locale "pt_PT", TimeZone "Etc/UTC" if we do not check if the Locale's language is "undetermined".
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
    public void testTimeZoneStrategyPatternPortugal() throws ParseException {
        testTimeZoneStrategyPattern("pt_PT", "Horário do Meridiano de Greenwich");
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
    public void testTimeZoneStrategyPatternSuriname() throws ParseException {
        testTimeZoneStrategyPattern("sr_ME_#Cyrl", "Srednje vreme po Griniču");
    }
}
