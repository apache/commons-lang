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
package org.apache.commons.lang3;

import static org.apache.commons.lang3.JavaVersion.JAVA_1_4;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Set;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Unit tests for {@link LocaleUtils}.
 */
public class LocaleUtilsTest extends AbstractLangTest {

    private static final Locale LOCALE_EN = new Locale("en", "");
    private static final Locale LOCALE_EN_US = new Locale("en", "US");
    private static final Locale LOCALE_EN_US_ZZZZ = new Locale("en", "US", "ZZZZ");
    private static final Locale LOCALE_FR = new Locale("fr", "");
    private static final Locale LOCALE_FR_CA = new Locale("fr", "CA");
    private static final Locale LOCALE_QQ = new Locale("qq", "");
    private static final Locale LOCALE_QQ_ZZ = new Locale("qq", "ZZ");

    @BeforeEach
    public void setUp() {
        // Testing #LANG-304. Must be called before availableLocaleSet is called.
        LocaleUtils.isAvailableLocale(Locale.getDefault());
    }

    /**
     * Test that constructors are public, and work, etc.
     */
    @Test
    public void testConstructor() {
        assertNotNull(new LocaleUtils());
        final Constructor<?>[] cons = LocaleUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertTrue(Modifier.isPublic(cons[0].getModifiers()));
        assertTrue(Modifier.isPublic(LocaleUtils.class.getModifiers()));
        assertFalse(Modifier.isFinal(LocaleUtils.class.getModifiers()));
    }

    /**
     * Pass in a valid language, test toLocale.
     *
     * @param language  the language string
     */
    private static void assertValidToLocale(final String language) {
        final Locale locale = LocaleUtils.toLocale(language);
        assertNotNull(locale, "valid locale");
        assertEquals(language, locale.getLanguage());
        //country and variant are empty
        assertTrue(StringUtils.isEmpty(locale.getCountry()));
        assertTrue(StringUtils.isEmpty(locale.getVariant()));
    }

    /**
     * Pass in a valid language, test toLocale.
     *
     * @param localeString to pass to toLocale()
     * @param language of the resulting Locale
     * @param country of the resulting Locale
     */
    private static void assertValidToLocale(final String localeString, final String language, final String country) {
        final Locale locale = LocaleUtils.toLocale(localeString);
        assertNotNull(locale, "valid locale");
        assertEquals(language, locale.getLanguage());
        assertEquals(country, locale.getCountry());
        //variant is empty
        assertTrue(StringUtils.isEmpty(locale.getVariant()));
    }

    /**
     * Pass in a valid language, test toLocale.
     *
     * @param localeString to pass to toLocale()
     * @param language of the resulting Locale
     * @param country of the resulting Locale
     * @param variant of the resulting Locale
     */
    private static void assertValidToLocale(
            final String localeString, final String language,
            final String country, final String variant) {
        final Locale locale = LocaleUtils.toLocale(localeString);
        assertNotNull(locale, "valid locale");
        assertEquals(language, locale.getLanguage());
        assertEquals(country, locale.getCountry());
        assertEquals(variant, locale.getVariant());
    }

    /**
     * Test toLocale(Locale) method.
     */
    @Test
    public void testToLocale_Locale_defaults() {
        assertNull(LocaleUtils.toLocale((String) null));
        assertEquals(Locale.getDefault(), LocaleUtils.toLocale((Locale) null));
        assertEquals(Locale.getDefault(), LocaleUtils.toLocale(Locale.getDefault()));
    }

    /**
     * Test toLocale(Locale) method.
     */
    @ParameterizedTest
    @MethodSource("java.util.Locale#getAvailableLocales")
    public void testToLocales(final Locale actualLocale) {
        assertEquals(actualLocale, LocaleUtils.toLocale(actualLocale));
    }

    /**
     * Test toLocale(String) method.
     */
    @Test
    public void testToLocale_1Part() {
        assertNull(LocaleUtils.toLocale((String) null));

        assertValidToLocale("us");
        assertValidToLocale("fr");
        assertValidToLocale("de");
        assertValidToLocale("zh");
        // Valid format but lang doesn't exist, should make instance anyway
        assertValidToLocale("qq");
        // LANG-941: JDK 8 introduced the empty locale as one of the default locales
        assertValidToLocale("");

        assertThrows(IllegalArgumentException.class, () -> LocaleUtils.toLocale("Us"), "Should fail if not lowercase");
        assertThrows(IllegalArgumentException.class, () -> LocaleUtils.toLocale("uS"), "Should fail if not lowercase");
        assertThrows(IllegalArgumentException.class, () -> LocaleUtils.toLocale("u#"), "Should fail if not lowercase");
        assertThrows(
                IllegalArgumentException.class, () -> LocaleUtils.toLocale("u"), "Must be 2 chars if less than 5");
        assertThrows(
                IllegalArgumentException.class, () -> LocaleUtils.toLocale("uu_U"), "Must be 2 chars if less than 5");
    }

    /**
     * Test toLocale() method.
     */
    @Test
    public void testToLocale_2Part() {
        assertValidToLocale("us_EN", "us", "EN");
        assertValidToLocale("us-EN", "us", "EN");
        //valid though doesn't exist
        assertValidToLocale("us_ZH", "us", "ZH");

        assertThrows(
                IllegalArgumentException.class,
                () -> LocaleUtils.toLocale("us_En"),
                "Should fail second part not uppercase");
        assertThrows(
                IllegalArgumentException.class,
                () -> LocaleUtils.toLocale("us_en"),
                "Should fail second part not uppercase");
        assertThrows(
                IllegalArgumentException.class,
                () -> LocaleUtils.toLocale("us_eN"),
                "Should fail second part not uppercase");
        assertThrows(
                IllegalArgumentException.class,
                () -> LocaleUtils.toLocale("uS_EN"),
                "Should fail first part not lowercase");
        assertThrows(
                IllegalArgumentException.class,
                () -> LocaleUtils.toLocale("us_E3"),
                "Should fail second part not uppercase");
    }

    /**
     * Test toLocale() method.
     */
    @Test
    public void testToLocale_3Part() {
        assertValidToLocale("us_EN_A", "us", "EN", "A");
        assertValidToLocale("us-EN-A", "us", "EN", "A");
        // this isn't pretty, but was caused by a jdk bug it seems
        // https://bugs.java.com/bugdatabase/view_bug.do?bug_id=4210525
        if (SystemUtils.isJavaVersionAtLeast(JAVA_1_4)) {
            assertValidToLocale("us_EN_a", "us", "EN", "a");
            assertValidToLocale("us_EN_SFsafdFDsdfF", "us", "EN", "SFsafdFDsdfF");
        } else {
            assertValidToLocale("us_EN_a", "us", "EN", "A");
            assertValidToLocale("us_EN_SFsafdFDsdfF", "us", "EN", "SFSAFDFDSDFF");
        }

        assertThrows(
                IllegalArgumentException.class, () -> LocaleUtils.toLocale("us_EN-a"), "Should fail as no consistent delimiter");
        assertThrows(
                IllegalArgumentException.class, () -> LocaleUtils.toLocale("uu_UU_"), "Must be 3, 5 or 7+ in length");
    }

    /**
     * Helper method for local lookups.
     *
     * @param locale  the input locale
     * @param defaultLocale  the input default locale
     * @param expected  expected results
     */
    private static void assertLocaleLookupList(final Locale locale, final Locale defaultLocale, final Locale[] expected) {
        final List<Locale> localeList = defaultLocale == null ?
                LocaleUtils.localeLookupList(locale) :
                LocaleUtils.localeLookupList(locale, defaultLocale);

        assertEquals(expected.length, localeList.size());
        assertEquals(Arrays.asList(expected), localeList);
        assertUnmodifiableCollection(localeList);
    }

    /**
     * Test localeLookupList() method.
     */
    @Test
    public void testLocaleLookupList_Locale() {
        assertLocaleLookupList(null, null, new Locale[0]);
        assertLocaleLookupList(LOCALE_QQ, null, new Locale[]{LOCALE_QQ});
        assertLocaleLookupList(LOCALE_EN, null, new Locale[]{LOCALE_EN});
        assertLocaleLookupList(LOCALE_EN, null, new Locale[]{LOCALE_EN});
        assertLocaleLookupList(LOCALE_EN_US, null,
            new Locale[] {
                LOCALE_EN_US,
                LOCALE_EN});
        assertLocaleLookupList(LOCALE_EN_US_ZZZZ, null,
            new Locale[] {
                LOCALE_EN_US_ZZZZ,
                LOCALE_EN_US,
                LOCALE_EN});
    }

    /**
     * Test localeLookupList() method.
     */
    @Test
    public void testLocaleLookupList_LocaleLocale() {
        assertLocaleLookupList(LOCALE_QQ, LOCALE_QQ,
                new Locale[]{LOCALE_QQ});
        assertLocaleLookupList(LOCALE_EN, LOCALE_EN,
                new Locale[]{LOCALE_EN});

        assertLocaleLookupList(LOCALE_EN_US, LOCALE_EN_US,
            new Locale[]{
                LOCALE_EN_US,
                LOCALE_EN});
        assertLocaleLookupList(LOCALE_EN_US, LOCALE_QQ,
            new Locale[] {
                LOCALE_EN_US,
                LOCALE_EN,
                LOCALE_QQ});
        assertLocaleLookupList(LOCALE_EN_US, LOCALE_QQ_ZZ,
            new Locale[] {
                LOCALE_EN_US,
                LOCALE_EN,
                LOCALE_QQ_ZZ});

        assertLocaleLookupList(LOCALE_EN_US_ZZZZ, null,
            new Locale[] {
                LOCALE_EN_US_ZZZZ,
                LOCALE_EN_US,
                LOCALE_EN});
        assertLocaleLookupList(LOCALE_EN_US_ZZZZ, LOCALE_EN_US_ZZZZ,
            new Locale[] {
                LOCALE_EN_US_ZZZZ,
                LOCALE_EN_US,
                LOCALE_EN});
        assertLocaleLookupList(LOCALE_EN_US_ZZZZ, LOCALE_QQ,
            new Locale[] {
                LOCALE_EN_US_ZZZZ,
                LOCALE_EN_US,
                LOCALE_EN,
                LOCALE_QQ});
        assertLocaleLookupList(LOCALE_EN_US_ZZZZ, LOCALE_QQ_ZZ,
            new Locale[] {
                LOCALE_EN_US_ZZZZ,
                LOCALE_EN_US,
                LOCALE_EN,
                LOCALE_QQ_ZZ});
        assertLocaleLookupList(LOCALE_FR_CA, LOCALE_EN,
            new Locale[] {
                LOCALE_FR_CA,
                LOCALE_FR,
                LOCALE_EN});
    }

    /**
     * Test availableLocaleList() method.
     */
    @Test
    public void testAvailableLocaleList() {
        final List<Locale> list = LocaleUtils.availableLocaleList();
        final List<Locale> list2 = LocaleUtils.availableLocaleList();
        assertNotNull(list);
        assertSame(list, list2);
        assertUnmodifiableCollection(list);

        final Locale[] jdkLocaleArray = Locale.getAvailableLocales();
        final List<Locale> jdkLocaleList = Arrays.asList(jdkLocaleArray);
        assertEquals(jdkLocaleList, list);
    }

    /**
     * Test availableLocaleSet() method.
     */
    @Test
    public void testAvailableLocaleSet() {
        final Set<Locale> set = LocaleUtils.availableLocaleSet();
        final Set<Locale> set2 = LocaleUtils.availableLocaleSet();
        assertNotNull(set);
        assertSame(set, set2);
        assertUnmodifiableCollection(set);

        final Locale[] jdkLocaleArray = Locale.getAvailableLocales();
        final List<Locale> jdkLocaleList = Arrays.asList(jdkLocaleArray);
        final Set<Locale> jdkLocaleSet = new HashSet<>(jdkLocaleList);
        assertEquals(jdkLocaleSet, set);
    }

    /**
     * Test availableLocaleSet() method.
     */
    @SuppressWarnings("boxing") // JUnit4 does not support primitive equality testing apart from long
    @Test
    public void testIsAvailableLocale() {
        final Set<Locale> set = LocaleUtils.availableLocaleSet();
        assertEquals(set.contains(LOCALE_EN), LocaleUtils.isAvailableLocale(LOCALE_EN));
        assertEquals(set.contains(LOCALE_EN_US), LocaleUtils.isAvailableLocale(LOCALE_EN_US));
        assertEquals(set.contains(LOCALE_EN_US_ZZZZ), LocaleUtils.isAvailableLocale(LOCALE_EN_US_ZZZZ));
        assertEquals(set.contains(LOCALE_FR), LocaleUtils.isAvailableLocale(LOCALE_FR));
        assertEquals(set.contains(LOCALE_FR_CA), LocaleUtils.isAvailableLocale(LOCALE_FR_CA));
        assertEquals(set.contains(LOCALE_QQ), LocaleUtils.isAvailableLocale(LOCALE_QQ));
        assertEquals(set.contains(LOCALE_QQ_ZZ), LocaleUtils.isAvailableLocale(LOCALE_QQ_ZZ));
    }

    /**
     * Test for 3-chars locale, further details at LANG-915
     *
     */
    @Test
    public void testThreeCharsLocale() {
        for (final String str : Arrays.asList("udm", "tet")) {
            final Locale locale = LocaleUtils.toLocale(str);
            assertNotNull(locale);
            assertEquals(str, locale.getLanguage());
            assertTrue(StringUtils.isBlank(locale.getCountry()));
            assertEquals(new Locale(str), locale);
        }
    }

    /**
     * Make sure the language by country is correct. It checks that
     * the LocaleUtils.languagesByCountry(country) call contains the
     * array of languages passed in. It may contain more due to JVM
     * variations.
     *
     * @param country
     * @param languages array of languages that should be returned
     */
    private static void assertLanguageByCountry(final String country, final String[] languages) {
        final List<Locale> list = LocaleUtils.languagesByCountry(country);
        final List<Locale> list2 = LocaleUtils.languagesByCountry(country);
        assertNotNull(list);
        assertSame(list, list2);
        //search through languages
        for (final String language : languages) {
            final Iterator<Locale> iterator = list.iterator();
            boolean found = false;
            // see if it was returned by the set
            while (iterator.hasNext()) {
                final Locale locale = iterator.next();
                // should have an en empty variant
                assertTrue(StringUtils.isEmpty(locale.getVariant()));
                assertEquals(country, locale.getCountry());
                if (language.equals(locale.getLanguage())) {
                    found = true;
                    break;
                }
            }
            assertTrue(found, "Could not find language: " + language + " for country: " + country);
        }
        assertUnmodifiableCollection(list);
    }

    /**
     * Test languagesByCountry() method.
     */
    @Test
    public void testLanguagesByCountry() {
        assertLanguageByCountry(null, new String[0]);
        assertLanguageByCountry("GB", new String[]{"en"});
        assertLanguageByCountry("ZZ", new String[0]);
        assertLanguageByCountry("CH", new String[]{"fr", "de", "it"});
    }

    /**
     * Make sure the country by language is correct. It checks that
     * the LocaleUtils.countryByLanguage(language) call contains the
     * array of countries passed in. It may contain more due to JVM
     * variations.
     *
     *
     * @param language
     * @param countries array of countries that should be returned
     */
    private static void assertCountriesByLanguage(final String language, final String[] countries) {
        final List<Locale> list = LocaleUtils.countriesByLanguage(language);
        final List<Locale> list2 = LocaleUtils.countriesByLanguage(language);
        assertNotNull(list);
        assertSame(list, list2);
        //search through languages
        for (final String country : countries) {
            final Iterator<Locale> iterator = list.iterator();
            boolean found = false;
            // see if it was returned by the set
            while (iterator.hasNext()) {
                final Locale locale = iterator.next();
                // should have an en empty variant
                assertTrue(StringUtils.isEmpty(locale.getVariant()));
                assertEquals(language, locale.getLanguage());
                if (country.equals(locale.getCountry())) {
                    found = true;
                    break;
                }
            }
            assertTrue(found, "Could not find language: " + country + " for country: " + language);
        }
        assertUnmodifiableCollection(list);
    }

    /**
     * Test countriesByLanguage() method.
     */
    @Test
    public void testCountriesByLanguage() {
        assertCountriesByLanguage(null, new String[0]);
        assertCountriesByLanguage("de", new String[]{"DE", "CH", "AT", "LU"});
        assertCountriesByLanguage("zz", new String[0]);
        assertCountriesByLanguage("it", new String[]{"IT", "CH"});
    }

    /**
     * @param coll  the collection to check
     */
    private static void assertUnmodifiableCollection(final Collection<?> coll) {
        assertThrows(UnsupportedOperationException.class, () -> coll.add(null));
    }

    /**
     * Tests #LANG-328 - only language+variant
     */
    @Test
    public void testLang328() {
        assertValidToLocale("fr__P", "fr", "", "P");
        assertValidToLocale("fr__POSIX", "fr", "", "POSIX");
    }

    @Test
    public void testLanguageAndUNM49Numeric3AreaCodeLang1312() {
        assertValidToLocale("en_001", "en", "001");
        assertValidToLocale("en_150", "en", "150");
        assertValidToLocale("ar_001", "ar", "001");

        // LANG-1312
        assertValidToLocale("en_001_GB", "en", "001", "GB");
        assertValidToLocale("en_150_US", "en", "150", "US");
    }

    /**
     * Tests #LANG-865, strings starting with an underscore.
     */
    @Test
    public void testLang865() {
        assertValidToLocale("_GB", "", "GB", "");
        assertValidToLocale("_GB_P", "", "GB", "P");
        assertValidToLocale("_GB_POSIX", "", "GB", "POSIX");
        assertThrows(
                IllegalArgumentException.class,
                () -> LocaleUtils.toLocale("_G"),
                "Must be at least 3 chars if starts with underscore");
        assertThrows(
                IllegalArgumentException.class,
                () -> LocaleUtils.toLocale("_Gb"),
                "Must be uppercase if starts with underscore");
        assertThrows(
                IllegalArgumentException.class,
                () -> LocaleUtils.toLocale("_gB"),
                "Must be uppercase if starts with underscore");
        assertThrows(
                IllegalArgumentException.class,
                () -> LocaleUtils.toLocale("_1B"),
                "Must be letter if starts with underscore");
        assertThrows(
                IllegalArgumentException.class,
                () -> LocaleUtils.toLocale("_G1"),
                "Must be letter if starts with underscore");
        assertThrows(
                IllegalArgumentException.class,
                () -> LocaleUtils.toLocale("_GB_"),
                "Must be at least 5 chars if starts with underscore");
        assertThrows(
                IllegalArgumentException.class,
                () -> LocaleUtils.toLocale("_GBAP"),
                "Must have underscore after the country if starts with underscore and is at least 5 chars");
    }

    @ParameterizedTest
    @MethodSource("java.util.Locale#getAvailableLocales")
    public void testParseAllLocales(final Locale actualLocale) {
        // Check if it's possible to recreate the Locale using just the standard constructor
        final Locale locale = new Locale(actualLocale.getLanguage(), actualLocale.getCountry(), actualLocale.getVariant());
        if (actualLocale.equals(locale)) { // it is possible for LocaleUtils.toLocale to handle these Locales
            final String str = actualLocale.toString();
            // Look for the script/extension suffix
            int suff = str.indexOf("_#");
            if (suff == - 1) {
                suff = str.indexOf("#");
            }
            String localeStr = str;
            if (suff >= 0) { // we have a suffix
                assertThrows(IllegalArgumentException.class, () -> LocaleUtils.toLocale(str));
                // try without suffix
                localeStr = str.substring(0, suff);
            }
            final Locale loc = LocaleUtils.toLocale(localeStr);
            assertEquals(actualLocale, loc);
        }
    }
}
