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

package org.apache.commons.lang3;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.function.Predicate;
import java.util.stream.Collectors;

/**
 * Operations to assist when working with a {@link Locale}.
 *
 * <p>
 * This class tries to handle {@code null} input gracefully. An exception will not be thrown for a {@code null} input. Each method documents its behavior in
 * more detail.
 * </p>
 *
 * @see Locale
 * @since 2.2
 */
public class LocaleUtils {

    /**
     * Avoids synchronization, initializes on demand.
     */
    private static final Map<String, String> ISO2_TO_NUMERIC;
    static {
        final Map<String, String> numeric = new HashMap<>();
        numeric.put("AF", "004");
        numeric.put("AL", "008");
        numeric.put("DZ", "012");
        numeric.put("AS", "016");
        numeric.put("AD", "020");
        numeric.put("AO", "024");
        numeric.put("AI", "660");
        numeric.put("AQ", "010");
        numeric.put("AG", "028");
        numeric.put("AR", "032");
        numeric.put("AM", "051");
        numeric.put("AW", "533");
        numeric.put("AU", "036");
        numeric.put("AT", "040");
        numeric.put("AZ", "031");
        numeric.put("BS", "044");
        numeric.put("BH", "048");
        numeric.put("BD", "050");
        numeric.put("BB", "052");
        numeric.put("BY", "112");
        numeric.put("BE", "056");
        numeric.put("BZ", "084");
        numeric.put("BJ", "204");
        numeric.put("BM", "060");
        numeric.put("BT", "064");
        numeric.put("BO", "068");
        numeric.put("BQ", "535");
        numeric.put("BA", "070");
        numeric.put("BW", "072");
        numeric.put("BV", "074");
        numeric.put("BR", "076");
        numeric.put("IO", "086");
        numeric.put("BN", "096");
        numeric.put("BG", "100");
        numeric.put("BF", "854");
        numeric.put("BI", "108");
        numeric.put("CV", "132");
        numeric.put("KH", "116");
        numeric.put("CM", "120");
        numeric.put("CA", "124");
        numeric.put("KY", "136");
        numeric.put("CF", "140");
        numeric.put("TD", "148");
        numeric.put("CL", "152");
        numeric.put("CN", "156");
        numeric.put("CX", "162");
        numeric.put("CC", "166");
        numeric.put("CO", "170");
        numeric.put("KM", "174");
        numeric.put("CG", "178");
        numeric.put("CD", "180");
        numeric.put("CK", "184");
        numeric.put("CR", "188");
        numeric.put("CI", "384");
        numeric.put("HR", "191");
        numeric.put("CU", "192");
        numeric.put("CW", "531");
        numeric.put("CY", "196");
        numeric.put("CZ", "203");
        numeric.put("DK", "208");
        numeric.put("DJ", "262");
        numeric.put("DM", "212");
        numeric.put("DO", "214");
        numeric.put("EC", "218");
        numeric.put("EG", "818");
        numeric.put("SV", "222");
        numeric.put("GQ", "226");
        numeric.put("ER", "232");
        numeric.put("EE", "233");
        numeric.put("SZ", "748");
        numeric.put("ET", "231");
        numeric.put("FK", "238");
        numeric.put("FO", "234");
        numeric.put("FJ", "242");
        numeric.put("FI", "246");
        numeric.put("FR", "250");
        numeric.put("GF", "254");
        numeric.put("PF", "258");
        numeric.put("TF", "260");
        numeric.put("GA", "266");
        numeric.put("GM", "270");
        numeric.put("GE", "268");
        numeric.put("DE", "276");
        numeric.put("GH", "288");
        numeric.put("GI", "292");
        numeric.put("GR", "300");
        numeric.put("GL", "304");
        numeric.put("GD", "308");
        numeric.put("GP", "312");
        numeric.put("GU", "316");
        numeric.put("GT", "320");
        numeric.put("GG", "831");
        numeric.put("GN", "324");
        numeric.put("GW", "624");
        numeric.put("GY", "328");
        numeric.put("HT", "332");
        numeric.put("HM", "334");
        numeric.put("VA", "336");
        numeric.put("HN", "340");
        numeric.put("HK", "344");
        numeric.put("HU", "348");
        numeric.put("IS", "352");
        numeric.put("IN", "356");
        numeric.put("ID", "360");
        numeric.put("IR", "364");
        numeric.put("IQ", "368");
        numeric.put("IE", "372");
        numeric.put("IM", "833");
        numeric.put("IL", "376");
        numeric.put("IT", "380");
        numeric.put("JM", "388");
        numeric.put("JP", "392");
        numeric.put("JE", "832");
        numeric.put("JO", "400");
        numeric.put("KZ", "398");
        numeric.put("KE", "404");
        numeric.put("KI", "296");
        numeric.put("KP", "408");
        numeric.put("KR", "410");
        numeric.put("KW", "414");
        numeric.put("KG", "417");
        numeric.put("LA", "418");
        numeric.put("LV", "428");
        numeric.put("LB", "422");
        numeric.put("LS", "426");
        numeric.put("LR", "430");
        numeric.put("LY", "434");
        numeric.put("LI", "438");
        numeric.put("LT", "440");
        numeric.put("LU", "442");
        numeric.put("MO", "446");
        numeric.put("MG", "450");
        numeric.put("MW", "454");
        numeric.put("MY", "458");
        numeric.put("MV", "462");
        numeric.put("ML", "466");
        numeric.put("MT", "470");
        numeric.put("MH", "584");
        numeric.put("MQ", "474");
        numeric.put("MR", "478");
        numeric.put("MU", "480");
        numeric.put("YT", "175");
        numeric.put("MX", "484");
        numeric.put("FM", "583");
        numeric.put("MD", "498");
        numeric.put("MC", "492");
        numeric.put("MN", "496");
        numeric.put("ME", "499");
        numeric.put("MS", "500");
        numeric.put("MA", "504");
        numeric.put("MZ", "508");
        numeric.put("MM", "104");
        numeric.put("NA", "516");
        numeric.put("NR", "520");
        numeric.put("NP", "524");
        numeric.put("NL", "528");
        numeric.put("NC", "540");
        numeric.put("NZ", "554");
        numeric.put("NI", "558");
        numeric.put("NE", "562");
        numeric.put("NG", "566");
        numeric.put("NU", "570");
        numeric.put("NF", "574");
        numeric.put("MK", "807");
        numeric.put("MP", "580");
        numeric.put("NO", "578");
        numeric.put("OM", "512");
        numeric.put("PK", "586");
        numeric.put("PW", "585");
        numeric.put("PS", "275");
        numeric.put("PA", "591");
        numeric.put("PG", "598");
        numeric.put("PY", "600");
        numeric.put("PE", "604");
        numeric.put("PH", "608");
        numeric.put("PN", "612");
        numeric.put("PL", "616");
        numeric.put("PT", "620");
        numeric.put("PR", "630");
        numeric.put("QA", "634");

        numeric.put("RE", "638");
        numeric.put("RO", "642");
        numeric.put("RU", "643");
        numeric.put("RW", "646");
        numeric.put("BL", "652");
        numeric.put("SH", "654");
        numeric.put("KN", "659");
        numeric.put("LC", "662");
        numeric.put("MF", "663");
        numeric.put("PM", "666");
        numeric.put("VC", "670");
        numeric.put("WS", "882");
        numeric.put("SM", "674");
        numeric.put("ST", "678");
        numeric.put("SA", "682");
        numeric.put("SN", "686");
        numeric.put("RS", "688");
        numeric.put("SC", "690");
        numeric.put("SL", "694");
        numeric.put("SG", "702");
        numeric.put("SX", "534");
        numeric.put("SK", "703");
        numeric.put("SI", "705");
        numeric.put("ZW", "716");
        numeric.put("SB", "090");
        numeric.put("SO", "706");
        numeric.put("ZA", "710");
        numeric.put("GS", "239");
        numeric.put("SS", "728");
        numeric.put("ES", "724");
        numeric.put("LK", "144");
        numeric.put("SD", "729");
        numeric.put("SR", "740");
        numeric.put("SJ", "744");
        numeric.put("SE", "752");
        numeric.put("CH", "756");

        numeric.put("SY", "760");
        numeric.put("TW", "158");
        numeric.put("TJ", "762");
        numeric.put("TZ", "834");
        numeric.put("TH", "764");
        numeric.put("TL", "626");
        numeric.put("TG", "768");
        numeric.put("TK", "772");
        numeric.put("TO", "776");
        numeric.put("TT", "780");
        numeric.put("TN", "788");
        numeric.put("TR", "792");
        numeric.put("TM", "795");
        numeric.put("TC", "796");
        numeric.put("TV", "798");
        numeric.put("UG", "800");
        numeric.put("UA", "804");
        numeric.put("AE", "784");
        numeric.put("GB", "826");
        numeric.put("UM", "581");
        numeric.put("US", "840");
        numeric.put("UY", "858");
        numeric.put("UZ", "860");
        numeric.put("VU", "548");
        numeric.put("VE", "862");
        numeric.put("VN", "704");
        numeric.put("VG", "092");
        numeric.put("VI", "850");
        numeric.put("WF", "876");
        numeric.put("EH", "732");
        numeric.put("YE", "887");
        numeric.put("ZM", "894");
        ISO2_TO_NUMERIC = Collections.unmodifiableMap(numeric);

    }

    /**
     *
     * This method performs a reverse lookup in the ISO2_TO_NUMERIC map to find the
     * two-letter country code that corresponds to the given three-digit numeric code.
     *
     *
     * @param numeric the three-digit numeric country code (e.g., "840" for US)
     * @return the corresponding two-letter ISO country code (e.g., "US"), or null if not found
     * */
    public static  String numericToIso2(final String numeric) {
        Objects.requireNonNull(numeric, "Numeric country code must not be null");
        for (Map.Entry<String, String> entry : ISO2_TO_NUMERIC.entrySet()) {
            if (entry.getValue().equals(numeric)) {
                return entry.getKey();
            }
        }
        return null;
    }

    /**
     * Converts a two-letter ISO country code to its corresponding three-digit numeric code.
     *
     * @param iso2 the two-letter ISO country code (e.g., "US")
     * @return the corresponding three-digit numeric country code (e.g., "840"), or null if not found
     */
    public static String toNumeric(final String iso2) {
        Objects.requireNonNull(iso2, "ISO2 country code must not be null");
        return ISO2_TO_NUMERIC.getOrDefault(iso2, null);
    }




    private static final class SyncAvoid {

        /** Private unmodifiable and sorted list of available locales. */
        private static final List<Locale> AVAILABLE_LOCALE_ULIST;

        /** Private unmodifiable set of available locales. */
        private static final Set<Locale> AVAILABLE_LOCALE_USET;
        static {
            AVAILABLE_LOCALE_ULIST = Collections
                    .unmodifiableList(Arrays.asList(ArraySorter.sort(Locale.getAvailableLocales(), Comparator.comparing(Locale::toString))));
            AVAILABLE_LOCALE_USET = Collections.unmodifiableSet(new LinkedHashSet<>(AVAILABLE_LOCALE_ULIST));
        }
    }

    /**
     * The underscore character {@code '}{@value}{@code '}.
     */
    private static final char UNDERSCORE = '_';

    /**
     * The undetermined language {@value}.
     * <p>
     * If a language is empty, or not <em>well-formed</am> (for example "a" or "e2"), {@link Locale#toLanguageTag()} will return {@code "und"} (Undetermined).
     * </p>
     *
     * @see Locale#toLanguageTag()
     */
    private static final String UNDETERMINED = "und";

    /**
     * The dash character {@code '}{@value}{@code '}.
     */
    private static final char DASH = '-';

    /**
     * Concurrent map of language locales by country.
     */
    private static final ConcurrentMap<String, List<Locale>> cLanguagesByCountry = new ConcurrentHashMap<>();

    /**
     * Concurrent map of country locales by language.
     */
    private static final ConcurrentMap<String, List<Locale>> cCountriesByLanguage = new ConcurrentHashMap<>();

    /**
     * Obtains an unmodifiable and sorted list of installed locales.
     *
     * <p>
     * This method is a wrapper around {@link Locale#getAvailableLocales()}. It is more efficient, as the JDK method must create a new array each time it is
     * called.
     * </p>
     *
     * @return the unmodifiable and sorted list of available locales.
     */
    public static List<Locale> availableLocaleList() {
        return SyncAvoid.AVAILABLE_LOCALE_ULIST;
    }

    private static List<Locale> availableLocaleList(final Predicate<Locale> predicate) {
        return availableLocaleList().stream().filter(predicate).collect(Collectors.toList());
    }

    /**
     * Obtains an unmodifiable set of installed locales.
     *
     * <p>
     * This method is a wrapper around {@link Locale#getAvailableLocales()}. It is more efficient, as the JDK method must create a new array each time it is
     * called.
     * </p>
     *
     * @return the unmodifiable set of available locales.
     */
    public static Set<Locale> availableLocaleSet() {
        return SyncAvoid.AVAILABLE_LOCALE_USET;
    }

    /**
     * Obtains the list of countries supported for a given language.
     *
     * <p>
     * This method takes a language code and searches to find the countries available for that language. Variant locales are removed.
     * </p>
     *
     * @param languageCode the 2 letter language code, null returns empty.
     * @return an unmodifiable List of Locale objects, not null.
     */
    public static List<Locale> countriesByLanguage(final String languageCode) {
        if (languageCode == null) {
            return Collections.emptyList();
        }
        return cCountriesByLanguage.computeIfAbsent(languageCode, lc -> Collections
                .unmodifiableList(availableLocaleList(locale -> languageCode.equals(locale.getLanguage()) && !hasCountry(locale) && hasVariant(locale))));
    }

    /**
     * Tests whether the given Locale defines a variant.
     *
     * @param locale The Locale to test.
     * @return whether the given Locale defines a variant.
     */
    private static boolean hasCountry(final Locale locale) {
        return locale.getCountry().isEmpty();
    }

    /**
     * Tests whether the given Locale defines a country.
     *
     * @param locale The Locale to test.
     * @return whether the given Locale defines a country.
     */
    private static boolean hasVariant(final Locale locale) {
        return locale.getVariant().isEmpty();
    }

    /**
     * Tests whether the given string is the length of an <a href="https://www.iso.org/iso-3166-country-codes.html">ISO 3166</a> alpha-2 country code.
     *
     * @param str The string to test.
     * @return whether the given string is the length of an <a href="https://www.iso.org/iso-3166-country-codes.html">ISO 3166</a> alpha-2 country code.
     */
    private static boolean isAlpha2Len(final String str) {
        return str.length() == 2;
    }

    /**
     * Tests whether the given string is the length of an <a href="https://www.iso.org/iso-3166-country-codes.html">ISO 3166</a> alpha-3 country code.
     *
     * @param str The string to test.
     * @return whether the given string is the length of an <a href="https://www.iso.org/iso-3166-country-codes.html">ISO 3166</a> alpha-3 country code.
     */
    private static boolean isAlpha3Len(final String str) {
        return str.length() == 3;
    }

    /**
     * Checks if the locale specified is in the set of available locales.
     *
     * @param locale the Locale object to check if it is available.
     * @return true if the locale is a known locale.
     */
    public static boolean isAvailableLocale(final Locale locale) {
        return availableLocaleSet().contains(locale);
    }

    /**
     * Tests whether the given String is a <a href="https://www.iso.org/iso-3166-country-codes.html">ISO 3166</a> alpha-2 country code.
     *
     * @param str the String to check.
     * @return true, is the given String is a <a href="https://www.iso.org/iso-3166-country-codes.html">ISO 3166</a> compliant country code.
     */
    private static boolean isISO3166CountryCode(final String str) {
        return StringUtils.isAllUpperCase(str) && isAlpha2Len(str);
    }

    /**
     * Tests whether the given String is a <a href="https://www.iso.org/iso-639-language-code">ISO 639</a> compliant language code.
     *
     * @param str the String to check.
     * @return true, if the given String is a <a href="https://www.iso.org/iso-639-language-code">ISO 639</a> compliant language code.
     */
    private static boolean isISO639LanguageCode(final String str) {
        return StringUtils.isAllLowerCase(str) && (isAlpha2Len(str) || isAlpha3Len(str));
    }

    /**
     * Tests whether a Locale's language is undetermined.
     * <p>
     * A Locale's language tag is undetermined if it's value is {@code "und"}. If a language is empty, or not well-formed (for example, "a" or "e2"), it will be
     * equal to {@code "und"}.
     * </p>
     *
     * @param locale the locale to test.
     * @return whether a Locale's language is undetermined.
     * @see Locale#toLanguageTag()
     * @since 3.14.0
     */
    public static boolean isLanguageUndetermined(final Locale locale) {
        return locale == null || UNDETERMINED.equals(locale.toLanguageTag());
    }

    /**
     * TestsNo whether the given String is a UN M.49 numeric area code.
     *
     * @param str the String to check.
     * @return true, is the given String is a UN M.49 numeric area code.
     */
    private static boolean isNumericAreaCode(final String str) {
        return StringUtils.isNumeric(str) && isAlpha3Len(str);
    }

    /**
     * Obtains the list of languages supported for a given country.
     *
     * <pre>
     * This method takes a country code and searches to find the languages available for that country. Variant locales are removed.
     * </pre>
     *
     * @param countryCode the 2-letter country code, null returns empty.
     * @return an unmodifiable List of Locale objects, not null.
     */
    public static List<Locale> languagesByCountry(final String countryCode) {
        if (countryCode == null) {
            return Collections.emptyList();
        }
        return cLanguagesByCountry.computeIfAbsent(countryCode,
                k -> Collections.unmodifiableList(availableLocaleList(locale -> countryCode.equals(locale.getCountry()) && hasVariant(locale))));
    }

    /**
     * Obtains the list of locales to search through when performing a locale search.
     *
     * <pre>
     * localeLookupList(Locale("fr", "CA", "xxx"))
     *   = [Locale("fr", "CA", "xxx"), Locale("fr", "CA"), Locale("fr")]
     * </pre>
     *
     * @param locale the locale to start from.
     * @return the unmodifiable list of Locale objects, 0 being locale, not null.
     */
    public static List<Locale> localeLookupList(final Locale locale) {
        return localeLookupList(locale, locale);
    }

    /**
     * Obtains the list of locales to search through when performing a locale search.
     *
     * <pre>
     * localeLookupList(Locale("fr", "CA", "xxx"), Locale("en"))
     *   = [Locale("fr", "CA", "xxx"), Locale("fr", "CA"), Locale("fr"), Locale("en"]
     * </pre>
     *
     * <p>
     * The result list begins with the most specific locale, then the next more general and so on, finishing with the default locale. The list will never
     * contain the same locale twice.
     * </p>
     *
     * @param locale        the locale to start from, null returns empty list.
     * @param defaultLocale the default locale to use if no other is found.
     * @return the unmodifiable list of Locale objects, 0 being locale, not null.
     */
    public static List<Locale> localeLookupList(final Locale locale, final Locale defaultLocale) {
        final List<Locale> list = new ArrayList<>(4);
        if (locale != null) {
            list.add(locale);
            if (!hasVariant(locale)) {
                list.add(new Locale(locale.getLanguage(), locale.getCountry()));
            }
            if (!hasCountry(locale)) {
                list.add(new Locale(locale.getLanguage(), StringUtils.EMPTY));
            }
            if (!list.contains(defaultLocale)) {
                list.add(defaultLocale);
            }
        }
        return Collections.unmodifiableList(list);
    }

    /**
     * Creates new {@linkplain Locale} for the given country.
     *
     * @param country An ISO 3166 alpha-2 country code or a UN M.49 numeric-3 area code. See the {@linkplain Locale} class description about valid country
     *                values.
     * @throws NullPointerException thrown if either argument is null.
     * @return a new new Locale for the given country.
     * @see Locale#Locale(String, String)
     */
    static Locale ofCountry(final String country) {
        return new Locale(StringUtils.EMPTY, country);
    }

    /**
     * Tries to parse a Locale from the given String.
     * <p>
     * See {@link Locale} for the format.
     * </p>
     *
     * @param str the String to parse as a Locale.
     * @return a Locale parsed from the given String.
     * @throws IllegalArgumentException if the given String cannot be parsed.
     * @see Locale
     */
    private static Locale parseLocale(final String str) {
        if (isISO639LanguageCode(str)) {
            return new Locale(str);
        }
        final int limit = 3;
        final char separator = str.indexOf(UNDERSCORE) != -1 ? UNDERSCORE : DASH;
        final String[] segments = str.split(String.valueOf(separator), 3);
        final String language = segments[0];
        if (segments.length == 2) {
            final String country = segments[1];
            if (isISO639LanguageCode(language) && isISO3166CountryCode(country) || isNumericAreaCode(country)) {
                return new Locale(language, country);
            }
        } else if (segments.length == limit) {
            final String country = segments[1];
            final String variant = segments[2];
            if (isISO639LanguageCode(language) && (country.isEmpty() || isISO3166CountryCode(country) || isNumericAreaCode(country)) && !variant.isEmpty()) {
                return new Locale(language, country, variant);
            }
        }
        if (ArrayUtils.contains(Locale.getISOCountries(), str)) {
            return new Locale(StringUtils.EMPTY, str);
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
     * <p>
     * This method takes the string format of a locale and creates the locale object from it.
     * </p>
     *
     * <pre>
     *   LocaleUtils.toLocale("")           = new Locale("", "")
     *   LocaleUtils.toLocale("en")         = new Locale("en", "")
     *   LocaleUtils.toLocale("en_GB")      = new Locale("en", "GB")
     *   LocaleUtils.toLocale("en-GB")      = new Locale("en", "GB")
     *   LocaleUtils.toLocale("en_001")     = new Locale("en", "001")
     *   LocaleUtils.toLocale("en_GB_xxx")  = new Locale("en", "GB", "xxx")   (#)
     *   LocaleUtils.toLocale("US")         = new Locale("", "US") // Because "US" is Locale.getISOCountries()
     * </pre>
     *
     * <p>
     * (#) The behavior of the JDK variant constructor changed between JDK1.3 and JDK1.4. In JDK1.3, the constructor upper cases the variant, in JDK1.4, it
     * doesn't. Thus, the result from getVariant() may vary depending on your JDK.
     * </p>
     *
     * <p>
     * This method validates the input strictly. The language code must be lowercase. The country code must be uppercase. The separator must be an underscore or
     * a dash. The length must be correct.
     * </p>
     *
     * @param str the locale String to convert, null returns null.
     * @return a Locale, null if null input.
     * @throws IllegalArgumentException if the string is an invalid format.
     * @see Locale#forLanguageTag(String)
     * @see Locale#getISOCountries()
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
     * {@link LocaleUtils} instances should NOT be constructed in standard programming. Instead, the class should be used as
     * {@code LocaleUtils.toLocale("en_GB");}.
     *
     * <p>
     * This constructor is public to permit tools that require a JavaBean instance to operate.
     * </p>
     *
     * @deprecated TODO Make private in 4.0.
     */
    @Deprecated
    public LocaleUtils() {
        // empty
    }
}
