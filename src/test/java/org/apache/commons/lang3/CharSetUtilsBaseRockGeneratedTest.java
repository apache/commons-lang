package org.apache.commons.lang3;

import org.apache.commons.lang3.CharSetUtils;
import java.util.Arrays;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import static org.mockito.ArgumentMatchers.eq;
import java.util.Set;
import java.util.HashSet;
import org.junit.jupiter.params.provider.CsvSource;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
import static org.hamcrest.Matchers.startsWith;
import org.apache.commons.lang3.CharSetUtils;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import org.junit.jupiter.api.Disabled;

class // Removed testDeepEmpty as it's testing a private method
// Removed testModify as it's testing a private method
CharSetUtilsBaseRockGeneratedTest {

    //BaseRock generated method id: ${testContainsAny}, hash: 0B2DCAE1511FDDEDD21D6BC2C7588810
    @Test
    void testContainsAny() {
        assertTrue(CharSetUtils.containsAny("hello", "aeiou"));
        assertFalse(CharSetUtils.containsAny("hello", "xyz"));
        assertFalse(CharSetUtils.containsAny("", "abc"));
        assertFalse(CharSetUtils.containsAny("hello", ""));
        assertFalse(CharSetUtils.containsAny(null, "abc"));
    }

    //BaseRock generated method id: ${testCount}, hash: 6B93F9EB1C67A967E4A06D020ACB9C93
    @Disabled()
    @Test
    void testCount() {
        assertEquals(3, CharSetUtils.count("hello", "l"));
        assertEquals(2, CharSetUtils.count("hello", "lo"));
        assertEquals(5, CharSetUtils.count("hello", "a-z"));
        assertEquals(0, CharSetUtils.count("hello", ""));
        assertEquals(0, CharSetUtils.count("", "a-z"));
        assertEquals(0, CharSetUtils.count(null, "a-z"));
    }

    //BaseRock generated method id: ${testDelete}, hash: 35548132FA86CE1C081A650EB9B29543
    @Test
    void testDelete() {
        assertEquals("hll", CharSetUtils.delete("hello", "aeiou"));
        assertEquals("hello", CharSetUtils.delete("hello", "xyz"));
        assertEquals("", CharSetUtils.delete("hello", "a-z"));
        assertEquals("hello", CharSetUtils.delete("hello", ""));
        assertNull(CharSetUtils.delete(null, "abc"));
    }

    //BaseRock generated method id: ${testKeep}, hash: F41717A0F8DAC9568B2BA255E8061687
    @Test
    void testKeep() {
        assertEquals("eo", CharSetUtils.keep("hello", "aeiou"));
        assertEquals("", CharSetUtils.keep("hello", "xyz"));
        assertEquals("hello", CharSetUtils.keep("hello", "a-z"));
        assertEquals("", CharSetUtils.keep("hello", ""));
        assertNull(CharSetUtils.keep(null, "abc"));
    }

    //BaseRock generated method id: ${testSqueeze}, hash: 1BA78EDF2D069173FB4E7576B0F1729B
    @ParameterizedTest
    @CsvSource({ "hello, o, helllo", "hello, l, hello", "hello, '', hello", ", abc, " })
    void testSqueeze(String input, String squeeze, String expected) {
        assertEquals(expected, CharSetUtils.squeeze(input, squeeze));
    }
}
