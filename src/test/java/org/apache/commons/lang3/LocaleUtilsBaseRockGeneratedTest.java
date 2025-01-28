package org.apache.commons.lang3;

import org.apache.commons.lang3.LocaleUtils;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import org.junit.jupiter.api.Test;
import java.util.Collections;
import org.junit.jupiter.params.ParameterizedTest;
import static org.mockito.ArgumentMatchers.eq;
import java.util.Locale;
import java.util.concurrent.ConcurrentMap;
import java.util.function.Predicate;
import java.util.Set;
import org.apache.commons.lang3.LocaleUtils;
import java.util.Comparator;
import java.util.ArrayList;
import org.junit.jupiter.params.provider.CsvSource;
import static org.junit.jupiter.api.Assertions.*;
import java.util.stream.Collectors;
import java.util.concurrent.ConcurrentHashMap;
import static org.mockito.Mockito.*;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import org.junit.jupiter.api.Disabled;

public class LocaleUtilsBaseRockGeneratedTest {

    //BaseRock generated method id: ${testAvailableLocaleList}, hash: AD33B32B1059CEDB627F384EA5F529F1
    @Test
    public void testAvailableLocaleList() {
        List<Locale> locales = LocaleUtils.availableLocaleList();
        assertNotNull(locales);
        assertFalse(locales.isEmpty());
        assertTrue(locales.contains(Locale.US));
    }

    //BaseRock generated method id: ${testAvailableLocaleSet}, hash: 4273DAD3EF613065560888FA67438421
    @Test
    public void testAvailableLocaleSet() {
        Set<Locale> locales = LocaleUtils.availableLocaleSet();
        assertNotNull(locales);
        assertFalse(locales.isEmpty());
        assertTrue(locales.contains(Locale.US));
    }

    //BaseRock generated method id: ${testCountriesByLanguage}, hash: A37A07B75D5CDACFC271D596F1F21612
    @ParameterizedTest
    @CsvSource({ "en, 1", "fr, 1", "de, 1" })
    public void testCountriesByLanguage(String languageCode, int expectedSize) {
        List<Locale> countries = LocaleUtils.countriesByLanguage(languageCode);
        assertNotNull(countries);
        assertTrue(countries.size() >= expectedSize);
    }

    //BaseRock generated method id: ${testCountriesByLanguageWithNull}, hash: 1640B2A0AE5EC3A370CDD8DFDDDC40FD
    @Test
    public void testCountriesByLanguageWithNull() {
        List<Locale> countries = LocaleUtils.countriesByLanguage(null);
        assertNotNull(countries);
        assertTrue(countries.isEmpty());
    }

    //BaseRock generated method id: ${testIsAvailableLocale}, hash: 926750CA4E8AB76C3137F4B78D15C5CB
    @Test
    public void testIsAvailableLocale() {
        assertTrue(LocaleUtils.isAvailableLocale(Locale.US));
        assertFalse(LocaleUtils.isAvailableLocale(new Locale("xx", "XX")));
    }

    //BaseRock generated method id: ${testLanguagesByCountry}, hash: 46F557D61EE7F78CAF5DD5180F8A48AF
    @Disabled()
    @Test
    public void testLanguagesByCountry() {
        List<Locale> languages = LocaleUtils.languagesByCountry("US");
        assertNotNull(languages);
        assertFalse(languages.isEmpty());
        assertTrue(languages.contains(Locale.ENGLISH));
    }

    //BaseRock generated method id: ${testLanguagesByCountryWithNull}, hash: FDCC7B725DE8CC3D112737222E087D67
    @Test
    public void testLanguagesByCountryWithNull() {
        List<Locale> languages = LocaleUtils.languagesByCountry(null);
        assertNotNull(languages);
        assertTrue(languages.isEmpty());
    }

    //BaseRock generated method id: ${testLocaleLookupList}, hash: 3FBE19575B63EE44FA4494BECF62191F
    @Test
    public void testLocaleLookupList() {
        List<Locale> list = LocaleUtils.localeLookupList(Locale.FRANCE);
        assertNotNull(list);
        assertTrue(list.contains(Locale.FRANCE));
        assertTrue(list.contains(Locale.FRENCH));
    }

    //BaseRock generated method id: ${testLocaleLookupListWithNull}, hash: 0B35FA7B3F4F30317D4EC13766909430
    @Test
    public void testLocaleLookupListWithNull() {
        List<Locale> list = LocaleUtils.localeLookupList(null);
        assertNotNull(list);
        assertTrue(list.isEmpty());
    }

    //BaseRock generated method id: ${testToLocale}, hash: 216FB71DAC342673FE7C345675B48E42
    @Test
    public void testToLocale() {
        assertEquals(Locale.US, LocaleUtils.toLocale("en_US"));
        assertEquals(new Locale("en"), LocaleUtils.toLocale("en"));
        assertThrows(IllegalArgumentException.class, () -> LocaleUtils.toLocale("invalidLocale"));
    }

    //BaseRock generated method id: ${testToLocaleWithNull}, hash: 1710F9301B3E17BF1AF21F6AA0FD97AC
    @Test
    public void testToLocaleWithNull() {
        assertNull(LocaleUtils.toLocale((String) null));
    }

    //BaseRock generated method id: ${testIsLanguageUndetermined}, hash: 7AD8E89C1646D0DD3CD1AE54B61664B3
    @Test
    public void testIsLanguageUndetermined() {
        assertTrue(LocaleUtils.isLanguageUndetermined(null));
        assertTrue(LocaleUtils.isLanguageUndetermined(new Locale("und")));
        assertFalse(LocaleUtils.isLanguageUndetermined(Locale.ENGLISH));
    }
}
