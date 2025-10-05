package org.apache.commons.lang3;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class CountryUtilsTest {

    @Test
    void testKnownEntries() {
        assertEquals("USA", CountryUtils.getIso3("US"));
        assertEquals("840", CountryUtils.getNumericCode("US"));
        assertEquals("United States of America", CountryUtils.getCountryName("US"));
        assertEquals("+1", CountryUtils.getPhoneCode("US"));

        assertEquals("GBR", CountryUtils.getIso3("GB"));
        assertTrue(CountryUtils.getPhoneCode("GB").contains("+44"));
        assertEquals("GBR", CountryUtils.getIso3("gb")); // case-insensitivity

        assertEquals("PAK", CountryUtils.getIso3("PK"));
        assertEquals("+92", CountryUtils.getPhoneCode("PK"));

        assertEquals("IND", CountryUtils.getIso3("IN"));
        assertEquals("+91", CountryUtils.getPhoneCode("IN"));

        assertEquals("FRA", CountryUtils.getIso3("FR"));
        assertEquals("+33", CountryUtils.getPhoneCode("FR"));
    }

    @Test
    void testUnknownAndNull() {
        assertEquals("", CountryUtils.getIso3("ZZ"));
        assertEquals("", CountryUtils.getNumericCode("ZZ"));
        assertEquals("", CountryUtils.getCountryName("ZZ"));
        assertEquals("", CountryUtils.getPhoneCode("ZZ"));

        assertEquals("", CountryUtils.getIso3(null));
        assertEquals("", CountryUtils.getPhoneCode(null));
    }
    @Test
    void testDatasetSize() {
        // Ensure we have a reasonably complete dataset (best-effort). Adjust threshold if you add/remove entries.
        int known = CountryUtils.knownCount();
        assertTrue(known >= 180, "Expected dataset to contain many countries; actual: " + known);
    }
}
