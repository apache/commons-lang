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

import static org.hamcrest.Matchers.allOf;
import static org.hamcrest.Matchers.greaterThanOrEqualTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.lessThanOrEqualTo;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.nio.charset.Charset;
import java.util.Random;

import org.junit.Test;

/**
 * Unit tests {@link org.apache.commons.lang3.RandomStringUtils}.
 */
public class RandomStringUtilsTest {

    //-----------------------------------------------------------------------
    @Test
    public void testConstructor() {
        assertNotNull(new RandomStringUtils());
        final Constructor<?>[] cons = RandomStringUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertTrue(Modifier.isPublic(cons[0].getModifiers()));
        assertTrue(Modifier.isPublic(RandomStringUtils.class.getModifiers()));
        assertFalse(Modifier.isFinal(RandomStringUtils.class.getModifiers()));
    }
    
    //-----------------------------------------------------------------------
    /**
     * Test the implementation
     */
    @Test
    public void testRandomStringUtils() {
        String r1 = RandomStringUtils.random(50);
        assertEquals("random(50) length", 50, r1.length());
        String r2 = RandomStringUtils.random(50);
        assertEquals("random(50) length", 50, r2.length());
        assertTrue("!r1.equals(r2)", !r1.equals(r2));
        
        r1 = RandomStringUtils.randomAscii(50);
        assertEquals("randomAscii(50) length", 50, r1.length());
        for(int i = 0; i < r1.length(); i++) {
            assertTrue("char between 32 and 127", r1.charAt(i) >= 32 && r1.charAt(i) <= 127);
        }        
        r2 = RandomStringUtils.randomAscii(50);
        assertTrue("!r1.equals(r2)", !r1.equals(r2));

        r1 = RandomStringUtils.randomAlphabetic(50);
        assertEquals("randomAlphabetic(50)", 50, r1.length());
        for(int i = 0; i < r1.length(); i++) {
            assertTrue("r1 contains alphabetic", Character.isLetter(r1.charAt(i)) && !Character.isDigit(r1.charAt(i)));
        }
        r2 = RandomStringUtils.randomAlphabetic(50);
        assertTrue("!r1.equals(r2)", !r1.equals(r2));
        
        r1 = RandomStringUtils.randomAlphanumeric(50);
        assertEquals("randomAlphanumeric(50)", 50, r1.length());
        for(int i = 0; i < r1.length(); i++) {
            assertTrue("r1 contains alphanumeric", Character.isLetterOrDigit(r1.charAt(i)));
        }
        r2 = RandomStringUtils.randomAlphabetic(50);
        assertTrue("!r1.equals(r2)", !r1.equals(r2));
        
        r1 = RandomStringUtils.randomGraph(50);
        assertEquals("randomGraph(50) length", 50, r1.length());
        for(int i = 0; i < r1.length(); i++) {
            assertTrue("char between 33 and 126", r1.charAt(i) >= 33 && r1.charAt(i) <= 126);
        }
        r2 = RandomStringUtils.randomGraph(50);
        assertTrue("!r1.equals(r2)", !r1.equals(r2));
        
        r1 = RandomStringUtils.randomNumeric(50);
        assertEquals("randomNumeric(50)", 50, r1.length());
        for(int i = 0; i < r1.length(); i++) {
            assertTrue("r1 contains numeric", Character.isDigit(r1.charAt(i)) && !Character.isLetter(r1.charAt(i)));
        }
        r2 = RandomStringUtils.randomNumeric(50);
        assertTrue("!r1.equals(r2)", !r1.equals(r2));
        
        r1 = RandomStringUtils.randomPrint(50);
        assertEquals("randomPrint(50) length", 50, r1.length());
        for(int i = 0; i < r1.length(); i++) {
            assertTrue("char between 32 and 126", r1.charAt(i) >= 32 && r1.charAt(i) <= 126);
        }
        r2 = RandomStringUtils.randomPrint(50);
        assertTrue("!r1.equals(r2)", !r1.equals(r2));
        
        String set = "abcdefg";
        r1 = RandomStringUtils.random(50, set);
        assertEquals("random(50, \"abcdefg\")", 50, r1.length());
        for(int i = 0; i < r1.length(); i++) {
            assertTrue("random char in set", set.indexOf(r1.charAt(i)) > -1);
        }
        r2 = RandomStringUtils.random(50, set);
        assertTrue("!r1.equals(r2)", !r1.equals(r2));
        
        r1 = RandomStringUtils.random(50, (String) null);
        assertEquals("random(50) length", 50, r1.length());
        r2 = RandomStringUtils.random(50, (String) null);
        assertEquals("random(50) length", 50, r2.length());
        assertTrue("!r1.equals(r2)", !r1.equals(r2));
        
        set = "stuvwxyz";
        r1 = RandomStringUtils.random(50, set.toCharArray());
        assertEquals("random(50, \"stuvwxyz\")", 50, r1.length());
        for(int i = 0; i < r1.length(); i++) {
            assertTrue("random char in set", set.indexOf(r1.charAt(i)) > -1);
        }
        r2 = RandomStringUtils.random(50, set);
        assertTrue("!r1.equals(r2)", !r1.equals(r2));
        
        r1 = RandomStringUtils.random(50, (char[]) null);
        assertEquals("random(50) length", 50, r1.length());
        r2 = RandomStringUtils.random(50, (char[]) null);
        assertEquals("random(50) length", 50, r2.length());
        assertTrue("!r1.equals(r2)", !r1.equals(r2));

        final long seed = System.currentTimeMillis();
        r1 = RandomStringUtils.random(50,0,0,true,true,null,new Random(seed));
        r2 = RandomStringUtils.random(50,0,0,true,true,null,new Random(seed));
        assertEquals("r1.equals(r2)", r1, r2);

        r1 = RandomStringUtils.random(0);
        assertEquals("random(0).equals(\"\")", "", r1);
    }

    @Test
    public void testLANG805() {
        final long seed = System.currentTimeMillis();
        assertEquals("aaa", RandomStringUtils.random(3,0,0,false,false,new char[]{'a'},new Random(seed)));
    }

    @Test
    public void testLANG807() {
        try {
            RandomStringUtils.random(3,5,5,false,false);
            fail("Expected IllegalArgumentException");
        } catch (final IllegalArgumentException ex) { // distinguish from Random#nextInt message
            final String msg = ex.getMessage();
            assertTrue("Message (" + msg + ") must contain 'start'", msg.contains("start"));
            assertTrue("Message (" + msg + ") must contain 'end'", msg.contains("end"));
        }
    }

    @Test
    public void testExceptions() {
        final char[] DUMMY = new char[]{'a'}; // valid char array
        try {
            RandomStringUtils.random(-1);
            fail();
        } catch (final IllegalArgumentException ex) {}
        try {
            RandomStringUtils.random(-1, true, true);
            fail();
        } catch (final IllegalArgumentException ex) {}
        try {
            RandomStringUtils.random(-1, DUMMY);
            fail();
        } catch (final IllegalArgumentException ex) {}
        try {
            RandomStringUtils.random(1, new char[0]); // must not provide empty array => IAE
            fail();
        } catch (final IllegalArgumentException ex) {}
        try {
            RandomStringUtils.random(-1, "");
            fail();
        } catch (final IllegalArgumentException ex) {}
        try {
            RandomStringUtils.random(-1, (String)null);
            fail();
        } catch (final IllegalArgumentException ex) {}
        try {
            RandomStringUtils.random(-1, 'a', 'z', false, false);
            fail();
        } catch (final IllegalArgumentException ex) {}
        try {
            RandomStringUtils.random(-1, 'a', 'z', false, false, DUMMY);
            fail();
        } catch (final IllegalArgumentException ex) {}
        try {
            RandomStringUtils.random(-1, 'a', 'z', false, false, DUMMY, new Random());
            fail();
        } catch (final IllegalArgumentException ex) {}
        try {
            RandomStringUtils.random(8, 32, 48, false, true);
            fail();
        } catch (final IllegalArgumentException ex) {}
        try {
            RandomStringUtils.random(8, 32, 65, true, false);
            fail();
        } catch (final IllegalArgumentException ex) {}
    }
    
    /**
     * Make sure boundary alphanumeric characters are generated by randomAlphaNumeric
     * This test will fail randomly with probability = 6 * (61/62)**1000 ~ 5.2E-7
     */  
    @Test
    public void testRandomAlphaNumeric() {
        final char[] testChars = {'a', 'z', 'A', 'Z', '0', '9'};
        final boolean[] found = {false, false, false, false, false, false};
        for (int i = 0; i < 100; i++) {
            final String randString = RandomStringUtils.randomAlphanumeric(10);
            for (int j = 0; j < testChars.length; j++) {
                if (randString.indexOf(testChars[j]) > 0) {
                    found[j] = true;
                }
            }
        }
        for (int i = 0; i < testChars.length; i++) {
            if (!found[i]) {
                fail("alphanumeric character not generated in 1000 attempts: " 
                   + testChars[i] +" -- repeated failures indicate a problem ");
            }
        }
    }
    
    /**
     * Make sure '0' and '9' are generated by randomNumeric
     * This test will fail randomly with probability = 2 * (9/10)**1000 ~ 3.5E-46
     */  
    @Test
    public void testRandomNumeric() {
        final char[] testChars = {'0','9'};
        final boolean[] found = {false, false};
        for (int i = 0; i < 100; i++) {
            final String randString = RandomStringUtils.randomNumeric(10);
            for (int j = 0; j < testChars.length; j++) {
                if (randString.indexOf(testChars[j]) > 0) {
                    found[j] = true;
                }
            }
        }
        for (int i = 0; i < testChars.length; i++) {
            if (!found[i]) {
                fail("digit not generated in 1000 attempts: " 
                   + testChars[i] +" -- repeated failures indicate a problem ");
            }
        }  
    }
    
    /**
     * Make sure boundary alpha characters are generated by randomAlphabetic
     * This test will fail randomly with probability = 4 * (51/52)**1000 ~ 1.58E-8
     */  
    @Test
    public void testRandomAlphabetic() {
        final char[] testChars = {'a', 'z', 'A', 'Z'};
        final boolean[] found = {false, false, false, false};
        for (int i = 0; i < 100; i++) {
            final String randString = RandomStringUtils.randomAlphabetic(10);
            for (int j = 0; j < testChars.length; j++) {
                if (randString.indexOf(testChars[j]) > 0) {
                    found[j] = true;
                }
            }
        }
        for (int i = 0; i < testChars.length; i++) {
            if (!found[i]) {
                fail("alphanumeric character not generated in 1000 attempts: " 
                   + testChars[i] +" -- repeated failures indicate a problem ");
            }
        }
    }
    
    /**
     * Make sure 32 and 127 are generated by randomNumeric
     * This test will fail randomly with probability = 2*(95/96)**1000 ~ 5.7E-5
     */  
    @Test
    public void testRandomAscii() {
        final char[] testChars = {(char) 32, (char) 126};
        final boolean[] found = {false, false};
        for (int i = 0; i < 100; i++) {
            final String randString = RandomStringUtils.randomAscii(10);
            for (int j = 0; j < testChars.length; j++) {
                if (randString.indexOf(testChars[j]) > 0) {
                    found[j] = true;
                }
            }
        }
        for (int i = 0; i < testChars.length; i++) {
            if (!found[i]) {
                fail("ascii character not generated in 1000 attempts: " 
                + (int) testChars[i] + 
                 " -- repeated failures indicate a problem");
            }
        }  
    }

    @Test
    public void testRandomAsciiRange() {
        final int expectedMinLengthInclusive = 1;
        final int expectedMaxLengthExclusive = 11;
        final String pattern = "^\\p{ASCII}{" + expectedMinLengthInclusive + ',' + expectedMaxLengthExclusive + "}$";

        int maxCreatedLength = expectedMinLengthInclusive;
        int minCreatedLength = expectedMaxLengthExclusive - 1;
        for (int i = 0; i < 1000; i++) {
            final String s = RandomStringUtils.randomAscii(expectedMinLengthInclusive, expectedMaxLengthExclusive);
            assertThat("within range", s.length(), allOf(greaterThanOrEqualTo(expectedMinLengthInclusive), lessThanOrEqualTo(expectedMaxLengthExclusive - 1)));
            assertTrue(s, s.matches(pattern));

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
    public void testRandomAlphabeticRange() {
        final int expectedMinLengthInclusive = 1;
        final int expectedMaxLengthExclusive = 11;
        final String pattern = "^\\p{Alpha}{" + expectedMinLengthInclusive + ',' + expectedMaxLengthExclusive + "}$";

        int maxCreatedLength = expectedMinLengthInclusive;
        int minCreatedLength = expectedMaxLengthExclusive - 1;
        for (int i = 0; i < 1000; i++) {
            final String s = RandomStringUtils.randomAlphabetic(expectedMinLengthInclusive, expectedMaxLengthExclusive);
            assertThat("within range", s.length(), allOf(greaterThanOrEqualTo(expectedMinLengthInclusive), lessThanOrEqualTo(expectedMaxLengthExclusive - 1)));
            assertTrue(s, s.matches(pattern));

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
    public void testRandomAlphanumericRange() {
        final int expectedMinLengthInclusive = 1;
        final int expectedMaxLengthExclusive = 11;
        final String pattern = "^\\p{Alnum}{" + expectedMinLengthInclusive + ',' + expectedMaxLengthExclusive + "}$";

        int maxCreatedLength = expectedMinLengthInclusive;
        int minCreatedLength = expectedMaxLengthExclusive - 1;
        for (int i = 0; i < 1000; i++) {
            final String s = RandomStringUtils.randomAlphanumeric(expectedMinLengthInclusive, expectedMaxLengthExclusive);
            assertThat("within range", s.length(), allOf(greaterThanOrEqualTo(expectedMinLengthInclusive), lessThanOrEqualTo(expectedMaxLengthExclusive - 1)));
            assertTrue(s, s.matches(pattern));

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
    public void testRandomGraphRange() {
        final int expectedMinLengthInclusive = 1;
        final int expectedMaxLengthExclusive = 11;
        final String pattern = "^\\p{Graph}{" + expectedMinLengthInclusive + ',' + expectedMaxLengthExclusive + "}$";

        int maxCreatedLength = expectedMinLengthInclusive;
        int minCreatedLength = expectedMaxLengthExclusive - 1;
        for (int i = 0; i < 1000; i++) {
            final String s = RandomStringUtils.randomGraph(expectedMinLengthInclusive, expectedMaxLengthExclusive);
            assertThat("within range", s.length(), allOf(greaterThanOrEqualTo(expectedMinLengthInclusive), lessThanOrEqualTo(expectedMaxLengthExclusive - 1)));
            assertTrue(s, s.matches(pattern));

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
    public void testRandomNumericRange() {
        final int expectedMinLengthInclusive = 1;
        final int expectedMaxLengthExclusive = 11;
        final String pattern = "^\\p{Digit}{" + expectedMinLengthInclusive + ',' + expectedMaxLengthExclusive + "}$";

        int maxCreatedLength = expectedMinLengthInclusive;
        int minCreatedLength = expectedMaxLengthExclusive - 1;
        for (int i = 0; i < 1000; i++) {
            final String s = RandomStringUtils.randomNumeric(expectedMinLengthInclusive, expectedMaxLengthExclusive);
            assertThat("within range", s.length(), allOf(greaterThanOrEqualTo(expectedMinLengthInclusive), lessThanOrEqualTo(expectedMaxLengthExclusive - 1)));
            assertTrue(s, s.matches(pattern));

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
    public void testRandomPrintRange() {
        final int expectedMinLengthInclusive = 1;
        final int expectedMaxLengthExclusive = 11;
        final String pattern = "^\\p{Print}{" + expectedMinLengthInclusive + ',' + expectedMaxLengthExclusive + "}$";

        int maxCreatedLength = expectedMinLengthInclusive;
        int minCreatedLength = expectedMaxLengthExclusive - 1;
        for (int i = 0; i < 1000; i++) {
            final String s = RandomStringUtils.randomPrint(expectedMinLengthInclusive, expectedMaxLengthExclusive);
            assertThat("within range", s.length(), allOf(greaterThanOrEqualTo(expectedMinLengthInclusive), lessThanOrEqualTo(expectedMaxLengthExclusive - 1)));
            assertTrue(s, s.matches(pattern));

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
     * Test homogeneity of random strings generated --
     * i.e., test that characters show up with expected frequencies
     * in generated strings.  Will fail randomly about 1 in 1000 times.
     * Repeated failures indicate a problem.
     */
    @Test
    public void testRandomStringUtilsHomog() {
        final String set = "abc";
        final char[] chars = set.toCharArray();
        String gen = "";
        final int[] counts = {0,0,0};
        final int[] expected = {200,200,200};
        for (int i = 0; i< 100; i++) {
           gen = RandomStringUtils.random(6,chars);
           for (int j = 0; j < 6; j++) {
               switch (gen.charAt(j)) {
                   case 'a': {counts[0]++; break;}
                   case 'b': {counts[1]++; break;}
                   case 'c': {counts[2]++; break;}
                   default: {fail("generated character not in set");}
               }
           }
        } 
        // Perform chi-square test with df = 3-1 = 2, testing at .001 level
        assertTrue("test homogeneity -- will fail about 1 in 1000 times",
            chiSquare(expected,counts) < 13.82);  
    }
    
    /**
     * Computes Chi-Square statistic given observed and expected counts
     * @param observed array of observed frequency counts
     * @param expected array of expected frequency counts
     */
    private double chiSquare(final int[] expected, final int[] observed) {
        double sumSq = 0.0d;
        double dev = 0.0d;
        for (int i = 0; i < observed.length; i++) {
            dev = observed[i] - expected[i];
            sumSq += dev * dev / expected[i];
        }
        return sumSq;
    }           

    /**
     * Checks if the string got by {@link RandomStringUtils#random(int)}
     * can be converted to UTF-8 and back without loss.
     *
     * @see <a href="http://issues.apache.org/jira/browse/LANG-100">LANG-100</a>
     */
    @Test
    public void testLang100() {
        final int size = 5000;
        final Charset charset = Charset.forName("UTF-8");
        final String orig = RandomStringUtils.random(size);
        final byte[] bytes = orig.getBytes(charset);
        final String copy = new String(bytes, charset);

        // for a verbose compare:
        for (int i=0; i < orig.length() && i < copy.length(); i++) {
            final char o = orig.charAt(i);
            final char c = copy.charAt(i);
            assertEquals("differs at " + i + "(" + Integer.toHexString(new Character(o).hashCode()) + "," +
            Integer.toHexString(new Character(c).hashCode()) + ")", o, c);
        }
        // compare length also
        assertEquals(orig.length(), copy.length());
        // just to be complete
        assertEquals(orig, copy);
    }
    
    
    /**
     * Test for LANG-1286. Creates situation where old code would
     * overflow a char and result in a code point outside the specified
     * range.
     * 
     * @throws Exception
     */
    @Test
    public void testCharOverflow() throws Exception {
        int start = Character.MAX_VALUE;
        int end = Integer.MAX_VALUE;
        
        @SuppressWarnings("serial")
        Random fixedRandom = new Random() {
            @Override
            public int nextInt(int n) {
                // Prevents selection of 'start' as the character
                return super.nextInt(n - 1) + 1;
            }
        };
        
        String result = RandomStringUtils.random(2, start, end, false, false, null, fixedRandom);
        int c = result.codePointAt(0);
        assertTrue(String.format("Character '%d' not in range [%d,%d).", c, start, end), c >= start && c < end);
    }
}

