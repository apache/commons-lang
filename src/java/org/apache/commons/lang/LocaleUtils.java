/*
 * Copyright 2005 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

/**
 * <p>Operations to assist when working with a Locale.</p>
 *
 * <p>This class tries to handle <code>null</code> input gracefully.
 * An exception will not be thrown for a <code>null</code> input.
 * Each method documents its behaviour in more detail.</p>
 *
 * @author Stephen Colebourne
 * @since 2.2
 * @version $Id$
 */
public class LocaleUtils {

    /**
     * <p><code>LocaleUtils</code> instances should NOT be constructed in standard programming.
     * Instead, the class should be used as <code>LocaleUtils.toLocale("en_GB");</code>.</p>
     *
     * <p>This constructor is public to permit tools that require a JavaBean instance
     * to operate.</p>
     */
    public LocaleUtils() {
      super();
    }

    //-----------------------------------------------------------------------
    /**
     * <p>Converts a String to a Locale.</p>
     *
     * <p>This method takes the string format of a locale and creates the
     * locale object from it.</p>
     *
     * <pre>
     *   LocaleUtils.toLocale("en")     = new Locale("en", "")
     *   LocaleUtils.toLocale("en_GB")  = new Locale("en", "GB")
     *   LocaleUtils.toLocale("en_GB_xxx")  = new Locale("en", "GB", "xxx")
     * </pre>
     *
     * @param str  the locale String to convert, null returns null
     * @return a Locale
     * @throws IllegalArgumentException if the string is an invalid format
     */
    public static Locale toLocale(String str) {
        if (str == null) {
            return null;
        }
        if (str.length() != 2 &&
            str.length() != 5 &&
            str.length() < 7) {
            throw new IllegalArgumentException("Invalid locale format: " + str);
        }
        if (Character.isLowerCase(str.charAt(0)) == false ||
            Character.isLowerCase(str.charAt(1)) == false) {
            throw new IllegalArgumentException("Invalid locale format: " + str);
        }
        if (str.length() == 2) {
            return new Locale(str, "");
        } else {
            if (Character.isUpperCase(str.charAt(3)) == false ||
                Character.isUpperCase(str.charAt(4)) == false) {
                throw new IllegalArgumentException("Invalid locale format: " + str);
            }
            if (str.length() == 5) {
                return new Locale(str.substring(0, 2), str.substring(3, 5));
            } else {
                return new Locale(str.substring(0, 2), str.substring(3, 5), str.substring(6));
            }
        }
    }

    //-----------------------------------------------------------------------
    /**
     * <p>Obtains the list of locales to search through when performing
     * a locale search.</p>
     *
     * <pre>
     * localeLookupList(Locale("fr","CA","xxx"))
     *   = [Locale("fr","CA","xxx"), Locale("fr","CA"), Locale("fr")]
     * </pre>
     *
     * @param locale  the locale to start from
     * @return the list of Locale objects, 0 being locale
     */
    public static List localeLookupList(Locale locale) {
        return localeLookupList(locale, locale);
    }

    //-----------------------------------------------------------------------
    /**
     * <p>Obtains the list of locales to search through when performing
     * a locale search.</p>
     *
     * <pre>
     * localeLookupList(Locale("fr", "CA", "xxx"), Locale("en"))
     *   = [Locale("fr","CA","xxx"), Locale("fr","CA"), Locale("fr"), Locale("en"]
     * </pre>
     *
     * <p>This method takes a country code and searches to find the
     * languages available for that country. Variant locales are removed.</p>
     *
     * @param locale  the locale to start from, null returns empty list
     * @param defaultLocale  the default locale to use if no other is found
     * @return the list of Locale objects, 0 being locale
     */
    public static List localeLookupList(Locale locale, Locale defaultLocale) {
        List list = new ArrayList(4);
        if (locale != null) {
            list.add(locale);
            if (locale.getVariant().length() > 0) {
                list.add(new Locale(locale.getLanguage(), locale.getCountry()));
            }
            if (locale.getCountry().length() > 0) {
                list.add(new Locale(locale.getLanguage(), ""));
            }
            if (list.contains(defaultLocale) == false) {
                list.add(defaultLocale);
            }
        }
        return list;
    }

    //-----------------------------------------------------------------------
    /**
     * <p>Obtains the set of languages supported for a given country.</p>
     
     * <p>This method takes a country code and searches to find the
     * languages available for that country. Variant locales are removed.</p>
     *
     * @param countryCode  the 2 letter country code, null returns empty
     * @return a Set of Locale objects
     */
    public static Set languagesByCountry(String countryCode) {
        Set set = new HashSet();
        Locale[] array = Locale.getAvailableLocales();
        if (countryCode != null) {
            for (int i = 0; i < array.length; i++) {
                if (countryCode.equals(array[i].getCountry()) &&
                        array[i].getVariant().length() == 0) {
                    set.add(array[i]);
                }
            }
        }
        return set;
    }

    //-----------------------------------------------------------------------
    /**
     * <p>Obtains the set of countries supported for a given language.</p>
     * 
     * <p>This method takes a language code and searches to find the
     * countries available for that language. Variant locales are removed.</p>
     *
     * @param languageCode  the 2 letter language code, null returns empty
     * @return a Set of Locale objects
     */
    public static Set countriesByLanguage(String languageCode) {
        Set set = new HashSet();
        Locale[] array = Locale.getAvailableLocales();
        if (languageCode != null) {
            for (int i = 0; i < array.length; i++) {
                if (languageCode.equals(array[i].getLanguage()) &&
                        array[i].getVariant().length() == 0) {
                    set.add(array[i]);
                }
            }
        }
        return set;
    }

}
