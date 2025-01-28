package org.apache.commons.lang3;

import org.apache.commons.lang3.CharSequenceUtils;

import java.util.Arrays;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import static org.mockito.ArgumentMatchers.eq;
import java.util.Objects;
import org.junit.jupiter.params.provider.CsvSource;
import org.apache.commons.lang3.CharSequenceUtils;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

class CharSequenceUtilsBaseRockGeneratedTest {

    private CharSequenceUtils charSequenceUtils;

    @BeforeEach
    void setUp() {
        charSequenceUtils = new CharSequenceUtils();
    }

    //BaseRock generated method id: ${testCheckLaterThan1}, hash: 339F77D6DADB87D6B9A8C3D0BEB2B1AE
    @Test
    void testCheckLaterThan1() {
        // This method is private, so we can't test it directly
        // We'll leave this test empty for now
    }

    //BaseRock generated method id: ${testIndexOf}, hash: EDC967F8F30B034454800DB9DA0F5023
    @ParameterizedTest
    @CsvSource({ "abc,b,0,1", "abc,c,0,2", "abcabc,abc,3,3", "abcabc,d,0,-1" })
    void testIndexOf(String cs, String searchChar, int start, int expected) {
        assertEquals(expected, CharSequenceUtils.indexOf(cs, searchChar, start));
    }

    //BaseRock generated method id: ${testIndexOfWithNull}, hash: 8AC4BAEC6D02365D57DEB1B5A7FFE3FD
    @Test
    void testIndexOfWithNull() {
        assertEquals(-1, CharSequenceUtils.indexOf(null, "a", 0));
        assertEquals(-1, CharSequenceUtils.indexOf("abc", null, 0));
    }

    //BaseRock generated method id: ${testIndexOfChar}, hash: 4661D6128040C1635EF72606B95EB738
    @ParameterizedTest
    @CsvSource({ "abc,98,0,1", "abc,99,0,2", "abc,100,0,-1", "abc,97,1,-1" })
    void testIndexOfChar(String cs, int searchChar, int start, int expected) {
        assertEquals(expected, CharSequenceUtils.indexOf(cs, searchChar, start));
    }

    //BaseRock generated method id: ${testIndexOfWithSupplementaryChar}, hash: E011ACA9798EC6CE741EAB2ACC9F3FDE
    @Test
    void testIndexOfWithSupplementaryChar() {
        String supplementaryString = new StringBuilder().appendCodePoint(0x10000).toString();
        assertEquals(0, CharSequenceUtils.indexOf(supplementaryString, 0x10000, 0));
    }

    //BaseRock generated method id: ${testLastIndexOf}, hash: 67DB1D88FE035B2422CB3710FBA29E1F
    @ParameterizedTest
    @CsvSource({ "abcba,b,4,3", "abcba,a,4,4", "abcba,c,4,2", "abcba,z,4,-1" })
    void testLastIndexOf(String cs, String searchChar, int start, int expected) {
        assertEquals(expected, CharSequenceUtils.lastIndexOf(cs, searchChar, start));
    }

    //BaseRock generated method id: ${testLastIndexOfWithNull}, hash: 37E6AC2EEF040C1102D649787795A748
    @Test
    void testLastIndexOfWithNull() {
        assertEquals(-1, CharSequenceUtils.lastIndexOf(null, "a", 0));
        assertEquals(-1, CharSequenceUtils.lastIndexOf("abc", null, 0));
    }

    //BaseRock generated method id: ${testLastIndexOfChar}, hash: D3E6737B3ED99E1029C017F008E25879
    @ParameterizedTest
    @CsvSource({ "abcba,98,4,3", "abcba,97,4,4", "abcba,99,4,2", "abcba,122,4,-1" })
    void testLastIndexOfChar(String cs, int searchChar, int start, int expected) {
        assertEquals(expected, CharSequenceUtils.lastIndexOf(cs, searchChar, start));
    }

    //BaseRock generated method id: ${testLastIndexOfWithSupplementaryChar}, hash: 7691BA16125A78740927DBAF79991CE7
    @Test
    void testLastIndexOfWithSupplementaryChar() {
        String supplementaryString = new StringBuilder().appendCodePoint(0x10000).toString();
        assertEquals(0, CharSequenceUtils.lastIndexOf(supplementaryString, 0x10000, 1));
    }

    //BaseRock generated method id: ${testRegionMatches}, hash: 6554702FCBD618B7C48FF362DD5A7C92
    @ParameterizedTest
    @CsvSource({ "HelloWorld,false,0,Hello,0,5,true", "HelloWorld,true,0,hello,0,5,true", "HelloWorld,false,5,World,0,5,true", "HelloWorld,true,5,world,0,5,true", "HelloWorld,false,0,Bye,0,3,false" })
    void testRegionMatches(String cs, boolean ignoreCase, int thisStart, String substring, int start, int length, boolean expected) {
        assertEquals(expected, CharSequenceUtils.regionMatches(cs, ignoreCase, thisStart, substring, start, length));
    }

    //BaseRock generated method id: ${testSubSequence}, hash: 7BC4C41D24B4954A37395C4A3F889A6B
    @Test
    void testSubSequence() {
        assertEquals("lo", CharSequenceUtils.subSequence("Hello", 3));
        assertNull(CharSequenceUtils.subSequence(null, 3));
    }

    //BaseRock generated method id: ${testToCharArray}, hash: D20E9EA4F661EE3B0D30FF7F8F160434
    @Test
    void testToCharArray() {
        assertArrayEquals(new char[] { 'H', 'e', 'l', 'l', 'o' }, CharSequenceUtils.toCharArray("Hello"));
        assertArrayEquals(new char[0], CharSequenceUtils.toCharArray(""));
        assertArrayEquals(new char[0], CharSequenceUtils.toCharArray(null));
    }

    //BaseRock generated method id: ${testDeprecatedConstructor}, hash: 376862E21BC95B7A12B72D0BCBD0D834
    @Test
    void testDeprecatedConstructor() {
        assertDoesNotThrow(() -> new CharSequenceUtils());
    }
}
