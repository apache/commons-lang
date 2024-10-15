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

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.allOf;
import static org.hamcrest.Matchers.greaterThanOrEqualTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.lessThan;
import static org.hamcrest.Matchers.lessThanOrEqualTo;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Random;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Tests {@link RandomStringUtils}.
 */
public class RandomStringUtilsTest extends AbstractLangTest {

    private static final int LOOP_COUNT = 1_000;

    static Stream<RandomStringUtils> randomProvider() {
        return Stream.of(RandomStringUtils.secure(), RandomStringUtils.secureStrong(), RandomStringUtils.insecure());
    }

    /**
     * Computes Chi-Square statistic given observed and expected counts
     *
     * @param observed array of observed frequency counts
     * @param expected array of expected frequency counts
     */
    private double chiSquare(final int[] expected, final int[] observed) {
        double sumSq = 0.0d;
        for (int i = 0; i < observed.length; i++) {
            final double dev = observed[i] - expected[i];
            sumSq += dev * dev / expected[i];
        }
        return sumSq;
    }

    /**
     * Test for LANG-1286. Creates situation where old code would overflow a char and result in a code point outside the specified range.
     */
    @Test
    public void testCharOverflow() {
        final int start = Character.MAX_VALUE;
        final int end = Integer.MAX_VALUE;

        @SuppressWarnings("serial")
        final Random fixedRandom = new Random() {
            @Override
            public int nextInt(final int n) {
                // Prevents selection of 'start' as the character
                return super.nextInt(n - 1) + 1;
            }
        };

        final String result = RandomStringUtils.random(2, start, end, false, false, null, fixedRandom);
        final int c = result.codePointAt(0);
        assertTrue(c >= start && c < end, String.format("Character '%d' not in range [%d,%d).", c, start, end));
    }

    @Test
    public void testConstructor() {
        assertNotNull(new RandomStringUtils());
    }

    @Test
    public void testExceptionsRandom() {
        assertThrows(IllegalArgumentException.class, () -> RandomStringUtils.random(-1));
        assertThrows(IllegalArgumentException.class, () -> RandomStringUtils.random(-1, true, true));
        assertThrows(IllegalArgumentException.class, () -> RandomStringUtils.random(-1, new char[] { 'a' }));
        assertThrows(IllegalArgumentException.class, () -> RandomStringUtils.random(1, new char[0]));
        assertThrows(IllegalArgumentException.class, () -> RandomStringUtils.random(-1, ""));
        assertThrows(IllegalArgumentException.class, () -> RandomStringUtils.random(-1, (String) null));
        assertThrows(IllegalArgumentException.class, () -> RandomStringUtils.random(-1, 'a', 'z', false, false));
        assertThrows(IllegalArgumentException.class, () -> RandomStringUtils.random(-1, 'a', 'z', false, false, new char[] { 'a' }));
        assertThrows(IllegalArgumentException.class, () -> RandomStringUtils.random(-1, 'a', 'z', false, false, new char[] { 'a' }, new Random()));
        assertThrows(IllegalArgumentException.class, () -> RandomStringUtils.random(8, 32, 48, false, true));
        assertThrows(IllegalArgumentException.class, () -> RandomStringUtils.random(8, 32, 65, true, false));
        assertThrows(IllegalArgumentException.class, () -> RandomStringUtils.random(1, Integer.MIN_VALUE, -10, false, false, null));
    }

    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testExceptionsRandom(final RandomStringUtils rsu) {
        assertThrows(IllegalArgumentException.class, () -> rsu.next(-1));
        assertThrows(IllegalArgumentException.class, () -> rsu.next(-1, true, true));
        assertThrows(IllegalArgumentException.class, () -> rsu.next(-1, new char[] { 'a' }));
        assertThrows(IllegalArgumentException.class, () -> rsu.next(1, new char[0]));
        assertThrows(IllegalArgumentException.class, () -> rsu.next(-1, ""));
        assertThrows(IllegalArgumentException.class, () -> rsu.next(-1, (String) null));
        assertThrows(IllegalArgumentException.class, () -> rsu.next(-1, 'a', 'z', false, false));
        assertThrows(IllegalArgumentException.class, () -> rsu.next(-1, 'a', 'z', false, false, new char[] { 'a' }));
        assertThrows(IllegalArgumentException.class, () -> rsu.next(8, 32, 48, false, true));
        assertThrows(IllegalArgumentException.class, () -> rsu.next(8, 32, 65, true, false));
        assertThrows(IllegalArgumentException.class, () -> rsu.next(1, Integer.MIN_VALUE, -10, false, false, null));
    }

    @Test
    public void testExceptionsRandomAlphabetic() {
        assertThrows(IllegalArgumentException.class, () -> RandomStringUtils.randomAlphabetic(-1));
    }

    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testExceptionsRandomAlphabetic(final RandomStringUtils rsu) {
        assertThrows(IllegalArgumentException.class, () -> rsu.nextAlphabetic(-1));
    }

    @Test
    public void testExceptionsRandomAscii() {
        assertThrows(IllegalArgumentException.class, () -> RandomStringUtils.randomAscii(-1));
    }

    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testExceptionsRandomAscii(final RandomStringUtils rsu) {
        assertThrows(IllegalArgumentException.class, () -> rsu.nextAscii(-1));
    }

    @Test
    public void testExceptionsRandomGraph() {
        assertThrows(IllegalArgumentException.class, () -> RandomStringUtils.randomGraph(-1));
    }

    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testExceptionsRandomGraph(final RandomStringUtils rsu) {
        assertThrows(IllegalArgumentException.class, () -> rsu.nextGraph(-1));
    }

    @Test
    public void testExceptionsRandomNumeric() {
        assertThrows(IllegalArgumentException.class, () -> RandomStringUtils.randomNumeric(-1));
    }

    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testExceptionsRandomNumeric(final RandomStringUtils rsu) {
        assertThrows(IllegalArgumentException.class, () -> rsu.nextNumeric(-1));
    }

    @Test
    public void testExceptionsRandomPrint() {
        assertThrows(IllegalArgumentException.class, () -> RandomStringUtils.randomPrint(-1));
    }

    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testExceptionsRandomPrint(final RandomStringUtils rsu) {
        assertThrows(IllegalArgumentException.class, () -> rsu.nextPrint(-1));
    }

    /**
     * Test homogeneity of random strings generated -- i.e., test that characters show up with expected frequencies in generated strings. Will fail randomly
     * about 1 in 100,000 times. Repeated failures indicate a problem.
     *
     * @param rsu the instance to test.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testHomogeneity(final RandomStringUtils rsu) {
        final String set = "abc";
        final char[] chars = set.toCharArray();
        final int[] counts = { 0, 0, 0 };
        final int[] expected = { 200, 200, 200 };
        for (int i = 0; i < 100; i++) {
            final String gen = rsu.next(6, chars);
            for (int j = 0; j < 6; j++) {
                switch (gen.charAt(j)) {
                case 'a': {
                    counts[0]++;
                    break;
                }
                case 'b': {
                    counts[1]++;
                    break;
                }
                case 'c': {
                    counts[2]++;
                    break;
                }
                default: {
                    fail("generated character not in set");
                }
                }
            }
        }
        // Perform chi-square test with degrees of freedom = 3-1 = 2, testing at 1e-5 level.
        // This expects a failure rate of 1 in 100,000.
        // critical value: from scipy.stats import chi2; chi2(2).isf(1e-5)
        assertThat("test homogeneity -- will fail about 1 in 100,000 times", chiSquare(expected, counts), lessThan(23.025850929940457d));
    }

    /**
     * Checks if the string got by {@link RandomStringUtils#random(int)} can be converted to UTF-8 and back without loss.
     *
     * @see <a href="https://issues.apache.org/jira/browse/LANG-100">LANG-100</a>
     */
    @Test
    public void testLang100() {
        final int size = 5000;
        final Charset charset = StandardCharsets.UTF_8;
        final String orig = RandomStringUtils.random(size);
        final byte[] bytes = orig.getBytes(charset);
        final String copy = new String(bytes, charset);

        // for a verbose compare:
        for (int i = 0; i < orig.length() && i < copy.length(); i++) {
            final char o = orig.charAt(i);
            final char c = copy.charAt(i);
            assertEquals(o, c, "differs at " + i + "(" + Integer.toHexString(Character.valueOf(o).hashCode()) + ","
                    + Integer.toHexString(Character.valueOf(c).hashCode()) + ")");
        }
        // compare length also
        assertEquals(orig.length(), copy.length());
        // just to be complete
        assertEquals(orig, copy);
    }

    /**
     * Checks if the string got by {@link RandomStringUtils#random(int)} can be converted to UTF-8 and back without loss.
     *
     * @param rsu the instance to test
     * @see <a href="https://issues.apache.org/jira/browse/LANG-100">LANG-100</a>
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testLang100(final RandomStringUtils rsu) {
        final int size = 5000;
        final Charset charset = StandardCharsets.UTF_8;
        final String orig = rsu.next(size);
        final byte[] bytes = orig.getBytes(charset);
        final String copy = new String(bytes, charset);

        // for a verbose compare:
        for (int i = 0; i < orig.length() && i < copy.length(); i++) {
            final char o = orig.charAt(i);
            final char c = copy.charAt(i);
            assertEquals(o, c, "differs at " + i + "(" + Integer.toHexString(Character.valueOf(o).hashCode()) + ","
                    + Integer.toHexString(Character.valueOf(c).hashCode()) + ")");
        }
        // compare length also
        assertEquals(orig.length(), copy.length());
        // just to be complete
        assertEquals(orig, copy);
    }

    @Test
    public void testLANG805() {
        final long seedMillis = System.currentTimeMillis();
        assertEquals("aaa", RandomStringUtils.random(3, 0, 0, false, false, new char[] { 'a' }, new Random(seedMillis)));
    }

    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testLANG807(final RandomStringUtils rsu) {
        final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> rsu.next(3, 5, 5, false, false));
        final String msg = ex.getMessage();
        assertTrue(msg.contains("start"), "Message (" + msg + ") must contain 'start'");
        assertTrue(msg.contains("end"), "Message (" + msg + ") must contain 'end'");
    }

    /**
     * Test {@code RandomStringUtils.random} works appropriately when letters=true
     * and the range does not only include ASCII letters.
     * Fails with probability less than 2^-40 (in practice this never happens).
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    void testNonASCIILetters(final RandomStringUtils rsu) {
        // Check that the following create a string with 10 characters 0x4e00 (a non-ASCII letter)
        String r1 = rsu.next(10, 0x4e00, 0x4e01, true, false);
        assertEquals(10, r1.length(), "wrong length");
        for (int i = 0; i < r1.length(); i++) {
            assertEquals(0x4e00, r1.charAt(i), "characters not all equal to 0x4e00");
        }

        // Same with both letters=true and numbers=true
        r1 = rsu.next(10, 0x4e00, 0x4e01, true, true);
        assertEquals(10, r1.length(), "wrong length");
        for (int i = 0; i < r1.length(); i++) {
            assertEquals(0x4e00, r1.charAt(i), "characters not all equal to 0x4e00");
        }

        // Check that at least one letter is not ASCII
        boolean found = false;
        r1 = rsu.next(40, 'F', 0x3000, true, false);
        assertEquals(40, r1.length(), "wrong length");
        for (int i = 0; i < r1.length(); i++) {
            assertTrue(Character.isLetter(r1.charAt(i)), "characters not all letters");
            if (r1.charAt(i) > 0x7f) {
                found = true;
            }
        }
        assertTrue(found, "no non-ASCII letter generated");
    }

    /**
     * Test {@code RandomStringUtils.random} works appropriately when numbers=true
     * and the range does not only include ASCII numbers/digits.
     * Fails with probability less than 2^-40 (in practice this never happens).
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    void testNonASCIINumbers(final RandomStringUtils rsu) {
        // Check that the following create a string with 10 characters 0x0660 (a non-ASCII digit)
        String r1 = rsu.next(10, 0x0660, 0x0661, false, true);
        assertEquals(10, r1.length(), "wrong length");
        for (int i = 0; i < r1.length(); i++) {
            assertEquals(0x0660, r1.charAt(i), "characters not all equal to 0x0660");
        }

        // Same with both letters=true and numbers=true
        r1 = rsu.next(10, 0x0660, 0x0661, true, true);
        assertEquals(10, r1.length(), "wrong length");
        for (int i = 0; i < r1.length(); i++) {
            assertEquals(0x0660, r1.charAt(i), "characters not all equal to 0x0660");
        }

        // Check that at least one letter is not ASCII
        boolean found = false;
        r1 = rsu.next(40, 'F', 0x3000, false, true);
        assertEquals(40, r1.length(), "wrong length");
        for (int i = 0; i < r1.length(); i++) {
            assertTrue(Character.isDigit(r1.charAt(i)), "characters not all numbers");
            if (r1.charAt(i) > 0x7f) {
                found = true;
            }
        }
        assertTrue(found, "no non-ASCII number generated");
    }

    /**
     * Make sure boundary alpha characters are generated by randomAlphabetic This test will fail randomly with probability = 4 * (51/52)**1000 ~ 1.58E-8
     */
    @Test
    public void testRandomAlphabetic() {
        final char[] testChars = { 'a', 'z', 'A', 'Z' };
        final boolean[] found = { false, false, false, false };
        for (int i = 0; i < LOOP_COUNT; i++) {
            final String randString = RandomStringUtils.randomAlphabetic(10);
            for (int j = 0; j < testChars.length; j++) {
                if (randString.indexOf(testChars[j]) > 0) {
                    found[j] = true;
                }
            }
        }
        for (int i = 0; i < testChars.length; i++) {
            assertTrue(found[i], "alphanumeric character not generated in 1000 attempts: " + testChars[i] + " -- repeated failures indicate a problem ");
        }
    }

    /**
     * Make sure boundary alpha characters are generated by randomAlphabetic This test will fail randomly with probability = 4 * (51/52)**1000 ~ 1.58E-8
     *
     * @param rsu the instance to test
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testRandomAlphabetic(final RandomStringUtils rsu) {
        final char[] testChars = { 'a', 'z', 'A', 'Z' };
        final boolean[] found = { false, false, false, false };
        for (int i = 0; i < LOOP_COUNT; i++) {
            final String randString = rsu.nextAlphabetic(10);
            for (int j = 0; j < testChars.length; j++) {
                if (randString.indexOf(testChars[j]) > 0) {
                    found[j] = true;
                }
            }
        }
        for (int i = 0; i < testChars.length; i++) {
            assertTrue(found[i], "alphanumeric character not generated in 1000 attempts: " + testChars[i] + " -- repeated failures indicate a problem ");
        }
    }

    @Test
    public void testRandomAlphabeticRange() {
        final int expectedMinLengthInclusive = 1;
        final int expectedMaxLengthExclusive = 11;
        final String pattern = "^\\p{Alpha}{" + expectedMinLengthInclusive + ',' + expectedMaxLengthExclusive + "}$";

        int maxCreatedLength = expectedMinLengthInclusive;
        int minCreatedLength = expectedMaxLengthExclusive - 1;
        for (int i = 0; i < LOOP_COUNT; i++) {
            final String s = RandomStringUtils.randomAlphabetic(expectedMinLengthInclusive, expectedMaxLengthExclusive);
            assertThat("within range", s.length(), allOf(greaterThanOrEqualTo(expectedMinLengthInclusive), lessThanOrEqualTo(expectedMaxLengthExclusive - 1)));
            assertTrue(s.matches(pattern), s);

            if (s.length() < minCreatedLength) {
                minCreatedLength = s.length();
            }

            if (s.length() > maxCreatedLength) {
                maxCreatedLength = s.length();
            }
        }
        assertThat("min generated, may fail randomly rarely", minCreatedLength, is(expectedMinLengthInclusive));
        assertThat("max generated, may fail randomly rarely", maxCreatedLength, is(expectedMaxLengthExclusive - 1));
    }

    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testRandomAlphabeticRange(final RandomStringUtils rsu) {
        final int expectedMinLengthInclusive = 1;
        final int expectedMaxLengthExclusive = 11;
        final String pattern = "^\\p{Alpha}{" + expectedMinLengthInclusive + ',' + expectedMaxLengthExclusive + "}$";

        int maxCreatedLength = expectedMinLengthInclusive;
        int minCreatedLength = expectedMaxLengthExclusive - 1;
        for (int i = 0; i < LOOP_COUNT; i++) {
            final String s = rsu.nextAlphabetic(expectedMinLengthInclusive, expectedMaxLengthExclusive);
            assertThat("within range", s.length(), allOf(greaterThanOrEqualTo(expectedMinLengthInclusive), lessThanOrEqualTo(expectedMaxLengthExclusive - 1)));
            assertTrue(s.matches(pattern), s);

            if (s.length() < minCreatedLength) {
                minCreatedLength = s.length();
            }

            if (s.length() > maxCreatedLength) {
                maxCreatedLength = s.length();
            }
        }
        assertThat("min generated, may fail randomly rarely", minCreatedLength, is(expectedMinLengthInclusive));
        assertThat("max generated, may fail randomly rarely", maxCreatedLength, is(expectedMaxLengthExclusive - 1));
    }

    /**
     * Make sure boundary alphanumeric characters are generated by randomAlphaNumeric This test will fail randomly with probability = 6 * (61/62)**1000 ~ 5.2E-7
     */
    @Test
    public void testRandomAlphaNumeric() {
        final char[] testChars = { 'a', 'z', 'A', 'Z', '0', '9' };
        final boolean[] found = { false, false, false, false, false, false };
        for (int i = 0; i < LOOP_COUNT; i++) {
            final String randString = RandomStringUtils.randomAlphanumeric(10);
            for (int j = 0; j < testChars.length; j++) {
                if (randString.indexOf(testChars[j]) > 0) {
                    found[j] = true;
                }
            }
        }
        for (int i = 0; i < testChars.length; i++) {
            assertTrue(found[i], "alphanumeric character not generated in 1000 attempts: " + testChars[i] + " -- repeated failures indicate a problem ");
        }
    }

    /**
     * Make sure boundary alphanumeric characters are generated by randomAlphaNumeric This test will fail randomly with probability = 6 * (61/62)**1000 ~ 5.2E-7
     *
     * @param rsu the instance to test
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testRandomAlphaNumeric(final RandomStringUtils rsu) {
        final char[] testChars = { 'a', 'z', 'A', 'Z', '0', '9' };
        final boolean[] found = { false, false, false, false, false, false };
        for (int i = 0; i < LOOP_COUNT; i++) {
            final String randString = rsu.nextAlphanumeric(10);
            for (int j = 0; j < testChars.length; j++) {
                if (randString.indexOf(testChars[j]) > 0) {
                    found[j] = true;
                }
            }
        }
        for (int i = 0; i < testChars.length; i++) {
            assertTrue(found[i], "alphanumeric character not generated in 1000 attempts: " + testChars[i] + " -- repeated failures indicate a problem ");
        }
    }

    @Test
    public void testRandomAlphanumericRange() {
        final int expectedMinLengthInclusive = 1;
        final int expectedMaxLengthExclusive = 11;
        final String pattern = "^\\p{Alnum}{" + expectedMinLengthInclusive + ',' + expectedMaxLengthExclusive + "}$";

        int maxCreatedLength = expectedMinLengthInclusive;
        int minCreatedLength = expectedMaxLengthExclusive - 1;
        for (int i = 0; i < LOOP_COUNT; i++) {
            final String s = RandomStringUtils.randomAlphanumeric(expectedMinLengthInclusive, expectedMaxLengthExclusive);
            assertThat("within range", s.length(), allOf(greaterThanOrEqualTo(expectedMinLengthInclusive), lessThanOrEqualTo(expectedMaxLengthExclusive - 1)));
            assertTrue(s.matches(pattern), s);

            if (s.length() < minCreatedLength) {
                minCreatedLength = s.length();
            }

            if (s.length() > maxCreatedLength) {
                maxCreatedLength = s.length();
            }
        }
        assertThat("min generated, may fail randomly rarely", minCreatedLength, is(expectedMinLengthInclusive));
        assertThat("max generated, may fail randomly rarely", maxCreatedLength, is(expectedMaxLengthExclusive - 1));
    }

    /**
     * Test the implementation
     *
     * @param rsu the instance to test.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testRandomApis(final RandomStringUtils rsu) {
        String r1 = rsu.next(50);
        assertEquals(50, r1.length(), "random(50) length");
        String r2 = rsu.next(50);
        assertEquals(50, r2.length(), "random(50) length");
        assertNotEquals(r1, r2, "!r1.equals(r2)");

        r1 = rsu.nextAscii(50);
        assertEquals(50, r1.length(), "randomAscii(50) length");
        for (int i = 0; i < r1.length(); i++) {
            assertThat("char >= 32 && <= 127", (int) r1.charAt(i), allOf(greaterThanOrEqualTo(32), lessThanOrEqualTo(127)));
        }
        r2 = rsu.nextAscii(50);
        assertNotEquals(r1, r2, "!r1.equals(r2)");

        r1 = rsu.nextAlphabetic(50);
        assertEquals(50, r1.length(), "randomAlphabetic(50)");
        for (int i = 0; i < r1.length(); i++) {
            assertTrue(Character.isLetter(r1.charAt(i)) && !Character.isDigit(r1.charAt(i)), "r1 contains alphabetic");
        }
        r2 = rsu.nextAlphabetic(50);
        assertNotEquals(r1, r2, "!r1.equals(r2)");

        r1 = rsu.nextAlphanumeric(50);
        assertEquals(50, r1.length(), "randomAlphanumeric(50)");
        for (int i = 0; i < r1.length(); i++) {
            assertTrue(Character.isLetterOrDigit(r1.charAt(i)), "r1 contains alphanumeric");
        }
        r2 = rsu.nextAlphabetic(50);
        assertNotEquals(r1, r2, "!r1.equals(r2)");

        r1 = rsu.nextGraph(50);
        assertEquals(50, r1.length(), "randomGraph(50) length");
        for (int i = 0; i < r1.length(); i++) {
            assertTrue(r1.charAt(i) >= 33 && r1.charAt(i) <= 126, "char between 33 and 126");
        }
        r2 = rsu.nextGraph(50);
        assertNotEquals(r1, r2, "!r1.equals(r2)");

        r1 = rsu.nextNumeric(50);
        assertEquals(50, r1.length(), "randomNumeric(50)");
        for (int i = 0; i < r1.length(); i++) {
            assertTrue(Character.isDigit(r1.charAt(i)) && !Character.isLetter(r1.charAt(i)), "r1 contains numeric");
        }
        r2 = rsu.nextNumeric(50);
        assertNotEquals(r1, r2, "!r1.equals(r2)");

        r1 = rsu.nextPrint(50);
        assertEquals(50, r1.length(), "randomPrint(50) length");
        for (int i = 0; i < r1.length(); i++) {
            assertTrue(r1.charAt(i) >= 32 && r1.charAt(i) <= 126, "char between 32 and 126");
        }
        r2 = rsu.nextPrint(50);
        assertNotEquals(r1, r2, "!r1.equals(r2)");

        String set = "abcdefg";
        r1 = rsu.next(50, set);
        assertEquals(50, r1.length(), "random(50, \"abcdefg\")");
        for (int i = 0; i < r1.length(); i++) {
            assertTrue(set.indexOf(r1.charAt(i)) > -1, "random char in set");
        }
        r2 = rsu.next(50, set);
        assertNotEquals(r1, r2, "!r1.equals(r2)");

        r1 = rsu.next(50, (String) null);
        assertEquals(50, r1.length(), "random(50) length");
        r2 = rsu.next(50, (String) null);
        assertEquals(50, r2.length(), "random(50) length");
        assertNotEquals(r1, r2, "!r1.equals(r2)");

        set = "stuvwxyz";
        r1 = rsu.next(50, set.toCharArray());
        assertEquals(50, r1.length(), "random(50, \"stuvwxyz\")");
        for (int i = 0; i < r1.length(); i++) {
            assertTrue(set.indexOf(r1.charAt(i)) > -1, "random char in set");
        }
        r2 = rsu.next(50, set);
        assertNotEquals(r1, r2, "!r1.equals(r2)");

        r1 = rsu.next(50, (char[]) null);
        assertEquals(50, r1.length(), "random(50) length");
        r2 = rsu.next(50, (char[]) null);
        assertEquals(50, r2.length(), "random(50) length");
        assertNotEquals(r1, r2, "!r1.equals(r2)");

        r1 = rsu.next(0);
        assertEquals("", r1, "random(0).equals(\"\")");
    }

    /**
     * Make sure 32 and 127 are generated by randomNumeric This test will fail randomly with probability = 2*(95/96)**1000 ~ 5.7E-5
     *
     * @param rsu the instance to test
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testRandomAscii(final RandomStringUtils rsu) {
        final char[] testChars = { (char) 32, (char) 126 };
        final boolean[] found = { false, false };
        // Test failures have been observed on GitHub builds with a 100 limit.
        for (int i = 0; i < LOOP_COUNT; i++) {
            final String randString = rsu.nextAscii(10);
            for (int j = 0; j < testChars.length; j++) {
                if (randString.indexOf(testChars[j]) > 0) {
                    found[j] = true;
                }
            }
        }
        for (int i = 0; i < testChars.length; i++) {
            assertTrue(found[i], "ascii character not generated in 1000 attempts: " + (int) testChars[i] + " -- repeated failures indicate a problem");
        }
    }

    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testRandomAsciiRange(final RandomStringUtils rsu) {
        final int expectedMinLengthInclusive = 1;
        final int expectedMaxLengthExclusive = 11;
        final String pattern = "^\\p{ASCII}{" + expectedMinLengthInclusive + ',' + expectedMaxLengthExclusive + "}$";

        int maxCreatedLength = expectedMinLengthInclusive;
        int minCreatedLength = expectedMaxLengthExclusive - 1;
        for (int i = 0; i < LOOP_COUNT; i++) {
            final String s = rsu.nextAscii(expectedMinLengthInclusive, expectedMaxLengthExclusive);
            assertThat("within range", s.length(), allOf(greaterThanOrEqualTo(expectedMinLengthInclusive), lessThanOrEqualTo(expectedMaxLengthExclusive - 1)));
            assertTrue(s.matches(pattern), s);

            if (s.length() < minCreatedLength) {
                minCreatedLength = s.length();
            }

            if (s.length() > maxCreatedLength) {
                maxCreatedLength = s.length();
            }
        }
        assertThat("min generated, may fail randomly rarely", minCreatedLength, is(expectedMinLengthInclusive));
        assertThat("max generated, may fail randomly rarely", maxCreatedLength, is(expectedMaxLengthExclusive - 1));
    }

    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testRandomGraphRange(final RandomStringUtils rsu) {
        final int expectedMinLengthInclusive = 1;
        final int expectedMaxLengthExclusive = 11;
        final String pattern = "^\\p{Graph}{" + expectedMinLengthInclusive + ',' + expectedMaxLengthExclusive + "}$";

        int maxCreatedLength = expectedMinLengthInclusive;
        int minCreatedLength = expectedMaxLengthExclusive - 1;
        for (int i = 0; i < LOOP_COUNT; i++) {
            final String s = rsu.nextGraph(expectedMinLengthInclusive, expectedMaxLengthExclusive);
            assertThat("within range", s.length(), allOf(greaterThanOrEqualTo(expectedMinLengthInclusive), lessThanOrEqualTo(expectedMaxLengthExclusive - 1)));
            assertTrue(s.matches(pattern), s);

            if (s.length() < minCreatedLength) {
                minCreatedLength = s.length();
            }

            if (s.length() > maxCreatedLength) {
                maxCreatedLength = s.length();
            }
        }
        assertThat("min generated, may fail randomly rarely", minCreatedLength, is(expectedMinLengthInclusive));
        assertThat("max generated, may fail randomly rarely", maxCreatedLength, is(expectedMaxLengthExclusive - 1));
    }

    /**
     * Make sure '0' and '9' are generated by randomNumeric This test will fail randomly with probability = 2 * (9/10)**1000 ~ 3.5E-46
     *
     * @param rsu the instance to test
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testRandomNumeric(final RandomStringUtils rsu) {
        final char[] testChars = { '0', '9' };
        final boolean[] found = { false, false };
        for (int i = 0; i < LOOP_COUNT; i++) {
            final String randString = rsu.nextNumeric(10);
            for (int j = 0; j < testChars.length; j++) {
                if (randString.indexOf(testChars[j]) > 0) {
                    found[j] = true;
                }
            }
        }
        for (int i = 0; i < testChars.length; i++) {
            assertTrue(found[i], "digit not generated in 1000 attempts: " + testChars[i] + " -- repeated failures indicate a problem ");
        }
    }

    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testRandomNumericRange(final RandomStringUtils rsu) {
        final int expectedMinLengthInclusive = 1;
        final int expectedMaxLengthExclusive = 11;
        final String pattern = "^\\p{Digit}{" + expectedMinLengthInclusive + ',' + expectedMaxLengthExclusive + "}$";

        int maxCreatedLength = expectedMinLengthInclusive;
        int minCreatedLength = expectedMaxLengthExclusive - 1;
        for (int i = 0; i < LOOP_COUNT; i++) {
            final String s = rsu.nextNumeric(expectedMinLengthInclusive, expectedMaxLengthExclusive);
            assertThat("within range", s.length(), allOf(greaterThanOrEqualTo(expectedMinLengthInclusive), lessThanOrEqualTo(expectedMaxLengthExclusive - 1)));
            assertTrue(s.matches(pattern), s);

            if (s.length() < minCreatedLength) {
                minCreatedLength = s.length();
            }

            if (s.length() > maxCreatedLength) {
                maxCreatedLength = s.length();
            }
        }
        assertThat("min generated, may fail randomly rarely", minCreatedLength, is(expectedMinLengthInclusive));
        assertThat("max generated, may fail randomly rarely", maxCreatedLength, is(expectedMaxLengthExclusive - 1));
    }

    @Test
    public void testRandomParameter() {
        final long seedMillis = System.currentTimeMillis();
        final String r1 = RandomStringUtils.random(50, 0, 0, true, true, null, new Random(seedMillis));
        final String r2 = RandomStringUtils.random(50, 0, 0, true, true, null, new Random(seedMillis));
        assertEquals(r1, r2, "r1.equals(r2)");
    }

    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testRandomPrintRange(final RandomStringUtils rsu) {
        final int expectedMinLengthInclusive = 1;
        final int expectedMaxLengthExclusive = 11;
        final String pattern = "^\\p{Print}{" + expectedMinLengthInclusive + ',' + expectedMaxLengthExclusive + "}$";

        int maxCreatedLength = expectedMinLengthInclusive;
        int minCreatedLength = expectedMaxLengthExclusive - 1;
        for (int i = 0; i < LOOP_COUNT; i++) {
            final String s = rsu.nextPrint(expectedMinLengthInclusive, expectedMaxLengthExclusive);
            assertThat("within range", s.length(), allOf(greaterThanOrEqualTo(expectedMinLengthInclusive), lessThanOrEqualTo(expectedMaxLengthExclusive - 1)));
            assertTrue(s.matches(pattern), s);

            if (s.length() < minCreatedLength) {
                minCreatedLength = s.length();
            }

            if (s.length() > maxCreatedLength) {
                maxCreatedLength = s.length();
            }
        }
        assertThat("min generated, may fail randomly rarely", minCreatedLength, is(expectedMinLengthInclusive));
        assertThat("max generated, may fail randomly rarely", maxCreatedLength, is(expectedMaxLengthExclusive - 1));
    }

    /**
     * Test {@code RandomStringUtils.random} works appropriately when chars specified.
     *
     * @param rsu the instance to test.
     */
    @ParameterizedTest
    @MethodSource("randomProvider")
    public void testRandomWithChars(final RandomStringUtils rsu) {
        final char[] digitChars = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9'};

        String r1, r2, r3;

        r1 = rsu.next(50, 0, 0, true, true, digitChars);
        assertEquals(50, r1.length(), "randomNumeric(50)");
        for (int i = 0; i < r1.length(); i++) {
            assertTrue(
                    Character.isDigit(r1.charAt(i)) && !Character.isLetter(r1.charAt(i)),
                    "r1 contains numeric");
        }
        r2 = rsu.nextNumeric(50);
        assertNotEquals(r1, r2);

        r3 = rsu.next(50, 0, 0, true, true, digitChars);
        assertNotEquals(r1, r3);
        assertNotEquals(r2, r3);
    }
}
