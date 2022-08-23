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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.function.Predicate;
import java.util.stream.Collectors;

/**
 * Operations to assist when working with a {@link Locale}.
 *
 * <p>This class tries to handle {@code null} input gracefully.
 * An exception will not be thrown for a {@code null} input.
 * Each method documents its behavior in more detail.</p>
 *
 * @since 2.2
 */
public class LocaleUtils {
    private static final char UNDERSCORE = '_';
    private static final char DASH = '-';

    // class to avoid synchronization (Init on demand)
    static class SyncAvoid {
        /** Unmodifiable list of available locales. */
        private static final List<Locale> AVAILABLE_LOCALE_LIST;
        /** Unmodifiable set of available locales. */
        private static final Set<Locale> AVAILABLE_LOCALE_SET;

        static {
            final List<Locale> list = new ArrayList<>(Arrays.asList(Locale.getAvailableLocales()));  // extra safe
            AVAILABLE_LOCALE_LIST = Collections.unmodifiableList(list);
            AVAILABLE_LOCALE_SET = Collections.unmodifiableSet(new HashSet<>(list));
        }
    }

    /** Concurrent map of language locales by country. */
    private static final ConcurrentMap<String, List<Locale>> cLanguagesByCountry =
        new ConcurrentHashMap<>();

    /** Concurrent map of country locales by language. */
    private static final ConcurrentMap<String, List<Locale>> cCountriesByLanguage =
        new ConcurrentHashMap<>();

    /**
     * Obtains an unmodifiable list of installed locales.
     *
     * <p>This method is a wrapper around {@link Locale#getAvailableLocales()}.
     * It is more efficient, as the JDK method must create a new array each
     * time it is called.</p>
     *
     * @return the unmodifiable list of available locales
     */
    public static List<Locale> availableLocaleList() {
        return SyncAvoid.AVAILABLE_LOCALE_LIST;
    }

    private static List<Locale> availableLocaleList(final Predicate<Locale> predicate) {
        return availableLocaleList().stream().filter(predicate).collect(Collectors.toList());
    }

    /**
     * Obtains an unmodifiable set of installed locales.
     *
     * <p>This method is a wrapper around {@link Locale#getAvailableLocales()}.
     * It is more efficient, as the JDK method must create a new array each
     * time it is called.</p>
     *
     * @return the unmodifiable set of available locales
     */
    public static Set<Locale> availableLocaleSet() {
        return SyncAvoid.AVAILABLE_LOCALE_SET;
    }

    /**
     * Obtains the list of countries supported for a given language.
     *
     * <p>This method takes a language code and searches to find the
     * countries available for that language. Variant locales are removed.</p>
     *
     * @param languageCode  the 2 letter language code, null returns empty
     * @return an unmodifiable List of Locale objects, not null
     */
    public static List<Locale> countriesByLanguage(final String languageCode) {
        if (languageCode == null) {
            return Collections.emptyList();
        }
        return cCountriesByLanguage.computeIfAbsent(languageCode, lc -> Collections.unmodifiableList(
            availableLocaleList(locale -> languageCode.equals(locale.getLanguage()) && !locale.getCountry().isEmpty() && locale.getVariant().isEmpty())));
    }

    /**
     * Checks if the locale specified is in the set of available locales.
     *
     * @param locale the Locale object to check if it is available
     * @return true if the locale is a known locale
     */
    public static boolean isAvailableLocale(final Locale locale) {
        return availableLocaleSet().contains(locale);
    }

    /**
     * Checks whether the given String is a ISO 3166 alpha-2 country code.
     *
     * @param str the String to check
     * @return true, is the given String is a ISO 3166 compliant country code.
     */
    private static boolean isISO3166CountryCode(final String str) {
        return StringUtils.isAllUpperCase(str) && str.length() == 2;
    }

    /**
     * Checks whether the given String is a ISO 639 compliant language code.
     *
     * @param str the String to check.
     * @return true, if the given String is a ISO 639 compliant language code.
     */
    private static boolean isISO639LanguageCode(final String str) {
        return StringUtils.isAllLowerCase(str) && (str.length() == 2 || str.length() == 3);
    }

    /**
     * Checks whether the given String is a UN M.49 numeric area code.
     *
     * @param str the String to check
     * @return true, is the given String is a UN M.49 numeric area code.
     */
    private static boolean isNumericAreaCode(final String str) {
        return StringUtils.isNumeric(str) && str.length() == 3;
    }

    /**
     * Obtains the list of languages supported for a given country.
     *
     * <p>This method takes a country code and searches to find the
     * languages available for that country. Variant locales are removed.</p>
     *
     * @param countryCode  the 2-letter country code, null returns empty
     * @return an unmodifiable List of Locale objects, not null
     */
    public static List<Locale> languagesByCountry(final String countryCode) {
        if (countryCode == null) {
            return Collections.emptyList();
        }
        return cLanguagesByCountry.computeIfAbsent(countryCode,
            k -> Collections.unmodifiableList(availableLocaleList(locale -> countryCode.equals(locale.getCountry()) && locale.getVariant().isEmpty())));
    }

    /**
     * Obtains the list of locales to search through when performing
     * a locale search.
     *
     * <pre>
     * localeLookupList(Locale("fr", "CA", "xxx"))
     *   = [Locale("fr", "CA", "xxx"), Locale("fr", "CA"), Locale("fr")]
     * </pre>
     *
     * @param locale  the locale to start from
     * @return the unmodifiable list of Locale objects, 0 being locale, not null
     */
    public static List<Locale> localeLookupList(final Locale locale) {
        return localeLookupList(locale, locale);
    }

    /**
     * Obtains the list of locales to search through when performing
     * a locale search.
     *
     * <pre>
     * localeLookupList(Locale("fr", "CA", "xxx"), Locale("en"))
     *   = [Locale("fr", "CA", "xxx"), Locale("fr", "CA"), Locale("fr"), Locale("en"]
     * </pre>
     *
     * <p>The result list begins with the most specific locale, then the
     * next more general and so on, finishing with the default locale.
     * The list will never contain the same locale twice.</p>
     *
     * @param locale  the locale to start from, null returns empty list
     * @param defaultLocale  the default locale to use if no other is found
     * @return the unmodifiable list of Locale objects, 0 being locale, not null
     */
    public static List<Locale> localeLookupList(final Locale locale, final Locale defaultLocale) {
        final List<Locale> list = new ArrayList<>(4);
        if (locale != null) {
            list.add(locale);
            if (!locale.getVariant().isEmpty()) {
                list.add(new Locale(locale.getLanguage(), locale.getCountry()));
            }
            if (!locale.getCountry().isEmpty()) {
                list.add(new Locale(locale.getLanguage(), StringUtils.EMPTY));
            }
            if (!list.contains(defaultLocale)) {
                list.add(defaultLocale);
            }
        }
        return Collections.unmodifiableList(list);
    }

    /**
     * Tries to parse a locale from the given String.
     *
     * @param str the String to parse a locale from.
     * @return a Locale instance parsed from the given String.
     * @throws IllegalArgumentException if the given String can not be parsed.
     */
    private static Locale parseLocale(final String str) {
        if (isISO639LanguageCode(str)) {
            return new Locale(str);
        }

        final String[] segments = str.indexOf(UNDERSCORE) != -1
            ? str.split(String.valueOf(UNDERSCORE), -1)
            : str.split(String.valueOf(DASH), -1);
        final String language = segments[0];
        if (segments.length == 2) {
            final String country = segments[1];
            if (isISO639LanguageCode(language) && isISO3166CountryCode(country) ||
                    isNumericAreaCode(country)) {
                return new Locale(language, country);
            }
        } else if (segments.length == 3) {
            final String country = segments[1];
            final String variant = segments[2];
            if (isISO639LanguageCode(language) &&
                    (country.isEmpty() || isISO3166CountryCode(country) || isNumericAreaCode(country)) &&
                    !variant.isEmpty()) {
                return new Locale(language, country, variant);
            }
        }
        throw new IllegalArgumentException("Invalid locale format: " + str);
    }

    /**
     * Returns the given locale if non-{@code null}, otherwise {@link Locale#getDefault()}.
     *
     * @param locale a locale or {@code null}.
     * @return the given locale if non-{@code null}, otherwise {@link Locale#getDefault()}.
     * @since 3.12.0
     */
    public static Locale toLocale(final Locale locale) {
        return locale != null ? locale : Locale.getDefault();
    }

    /**
     * Converts a String to a Locale.
     *
     * <p>This method takes the string format of a locale and creates the
     * locale object from it.</p>
     *
     * <pre>
     *   LocaleUtils.toLocale("")           = new Locale("", "")
     *   LocaleUtils.toLocale("en")         = new Locale("en", "")
     *   LocaleUtils.toLocale("en_GB")      = new Locale("en", "GB")
     *   LocaleUtils.toLocale("en-GB")      = new Locale("en", "GB")
     *   LocaleUtils.toLocale("en_001")     = new Locale("en", "001")
     *   LocaleUtils.toLocale("en_GB_xxx")  = new Locale("en", "GB", "xxx")   (#)
     * </pre>
     *
     * <p>(#) The behavior of the JDK variant constructor changed between JDK1.3 and JDK1.4.
     * In JDK1.3, the constructor upper cases the variant, in JDK1.4, it doesn't.
     * Thus, the result from getVariant() may vary depending on your JDK.</p>
     *
     * <p>This method validates the input strictly.
     * The language code must be lowercase.
     * The country code must be uppercase.
     * The separator must be an underscore or a dash.
     * The length must be correct.
     * </p>
     *
     * @param str  the locale String to convert, null returns null
     * @return a Locale, null if null input
     * @throws IllegalArgumentException if the string is an invalid format
     * @see Locale#forLanguageTag(String)
     */
    public static Locale toLocale(final String str) {
        if (str == null) {
            // TODO Should this return the default locale?
            return null;
        }
        if (str.isEmpty()) { // LANG-941 - JDK 8 introduced an empty locale where all fields are blank
            return new Locale(StringUtils.EMPTY, StringUtils.EMPTY);
        }
        if (str.contains("#")) { // LANG-879 - Cannot handle Java 7 script & extensions
            throw new IllegalArgumentException("Invalid locale format: " + str);
        }
        final int len = str.length();
        if (len < 2) {
            throw new IllegalArgumentException("Invalid locale format: " + str);
        }
        final char ch0 = str.charAt(0);
        if (ch0 == UNDERSCORE || ch0 == DASH) {
            if (len < 3) {
                throw new IllegalArgumentException("Invalid locale format: " + str);
            }
            final char ch1 = str.charAt(1);
            final char ch2 = str.charAt(2);
            if (!Character.isUpperCase(ch1) || !Character.isUpperCase(ch2)) {
                throw new IllegalArgumentException("Invalid locale format: " + str);
            }
            if (len == 3) {
                return new Locale(StringUtils.EMPTY, str.substring(1, 3));
            }
            if (len < 5) {
                throw new IllegalArgumentException("Invalid locale format: " + str);
            }
            if (str.charAt(3) != ch0) {
                throw new IllegalArgumentException("Invalid locale format: " + str);
            }
            return new Locale(StringUtils.EMPTY, str.substring(1, 3), str.substring(4));
        }

        return parseLocale(str);
    }

    /**
     * {@link LocaleUtils} instances should NOT be constructed in standard programming.
     * Instead, the class should be used as {@code LocaleUtils.toLocale("en_GB");}.
     *
     * <p>This constructor is public to permit tools that require a JavaBean instance
     * to operate.</p>
     */
    public LocaleUtils() {
    }

}
