/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002-2003 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution, if
 *    any, must include the following acknowlegement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowlegement may appear in the software itself,
 *    if and wherever such third-party acknowlegements normally appear.
 *
 * 4. The names "The Jakarta Project", "Commons", and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 */
package org.apache.commons.lang;

import java.util.Random;

import junit.framework.*;
import junit.textui.TestRunner;

/**
 * Unit tests {@link org.apache.commons.lang.RandomStringUtils}.
 *
 * @author <a href="mailto:steven@caswell.name">Steven Caswell</a>
 * @author <a href="mailto:ridesmet@users.sourceforge.net">Ringo De Smet</a>
 * @author Phil Steitz
 * @version $Id: RandomStringUtilsTest.java,v 1.7 2003/06/09 21:36:03 scolebourne Exp $
 */
public class RandomStringUtilsTest extends junit.framework.TestCase {
    /**
     * Construct a new instance of RandomStringUtilsTest with the specified name
     */
    public RandomStringUtilsTest(String name) {
        super(name);
    }

    public static Test suite() {
    	TestSuite suite = new TestSuite(RandomStringUtilsTest.class);
    	suite.setName("RandomStringUtils Tests");
        return suite;
    }
    
    /**
     * Set up instance variables required by this test case.
     */
    public void setUp() {
    }
    
    /**
     * Tear down instance variables required by this test case.
     */
    public void tearDown() {
    }
    
    /**
     * Test the implementation
     */
    public void testRandomStringUtils() {
        String r1 = RandomStringUtils.random(50);
        assertEquals("random(50) length", 50, r1.length());
        String r2 = RandomStringUtils.random(50);
        assertEquals("random(50) length", 50, r2.length());
        assertTrue("!r1.equals(r2)", !r1.equals(r2));
        
        r1 = RandomStringUtils.randomAscii(50);
        assertEquals("randomAscii(50) length", 50, r1.length());
        for(int i = 0; i < r1.length(); i++) {
            assertTrue("char between 32 and 127", (int) r1.charAt(i) >= 32 && (int) r1.charAt(i) <= 127);
        }        
        r2 = RandomStringUtils.randomAscii(50);
        assertTrue("!r1.equals(r2)", !r1.equals(r2));

        r1 = RandomStringUtils.randomAlphabetic(50);
        assertEquals("randomAlphabetic(50)", 50, r1.length());
        for(int i = 0; i < r1.length(); i++) {
            assertEquals("r1 contains alphabetic", true, Character.isLetter(r1.charAt(i)) && !Character.isDigit(r1.charAt(i)));
        }
        r2 = RandomStringUtils.randomAlphabetic(50);
        assertTrue("!r1.equals(r2)", !r1.equals(r2));
        
        r1 = RandomStringUtils.randomAlphanumeric(50);
        assertEquals("randomAlphanumeric(50)", 50, r1.length());
        for(int i = 0; i < r1.length(); i++) {
            assertEquals("r1 contains alphanumeric", true, Character.isLetterOrDigit(r1.charAt(i)));
        }
        r2 = RandomStringUtils.randomAlphabetic(50);
        assertTrue("!r1.equals(r2)", !r1.equals(r2));
        
        r1 = RandomStringUtils.randomNumeric(50);
        assertEquals("randomNumeric(50)", 50, r1.length());
        for(int i = 0; i < r1.length(); i++) {
            assertEquals("r1 contains numeric", true, Character.isDigit(r1.charAt(i)) && !Character.isLetter(r1.charAt(i)));
        }
        r2 = RandomStringUtils.randomNumeric(50);
        assertTrue("!r1.equals(r2)", !r1.equals(r2));
        
        String set = "abcdefg";
        r1 = RandomStringUtils.random(50, set);
        assertEquals("random(50, \"abcdefg\")", 50, r1.length());
        for(int i = 0; i < r1.length(); i++) {
            assertTrue("random char in set", set.indexOf(r1.charAt(i)) > -1);
        }
        r2 = RandomStringUtils.random(50, set);
        assertTrue("!r1.equals(r2)", !r1.equals(r2));
        
        set = "stuvwxyz";
        r1 = RandomStringUtils.random(50, set.toCharArray());
        assertEquals("random(50, \"stuvwxyz\")", 50, r1.length());
        for(int i = 0; i < r1.length(); i++) {
            assertTrue("random char in set", set.indexOf(r1.charAt(i)) > -1);
        }
        r2 = RandomStringUtils.random(50, set);
        assertTrue("!r1.equals(r2)", !r1.equals(r2));

        long seed = System.currentTimeMillis();
        r1 = RandomStringUtils.random(50,0,0,true,true,null,new Random(seed));
        r2 = RandomStringUtils.random(50,0,0,true,true,null,new Random(seed));
        assertEquals("r1.equals(r2)", r1, r2);

        r1 = RandomStringUtils.random(0);
        assertEquals("random(0).equals(\"\")", "", r1);

        Exception e = null;
        try {
            r1 = RandomStringUtils.random(-1);
        } catch (Exception e2) {
            e = e2;
        }
        assertNotNull("random(<0) throws exception", e);
    }
    
    /**
     * Make sure boundary alphanumeric characters are generated by randomAlphaNumeric
     * This test will fail randomly with probability = 6 * (61/62)**1000 ~ 5.2E-7
     */  
    public void testRandomAlphaNumeric() {
        char[] testChars = {'a', 'z', 'A', 'Z', '0', '9'};
        boolean[] found = {false, false, false, false, false, false};
        for (int i = 0; i < 100; i++) {
            String randString = RandomStringUtils.randomAlphanumeric(10);
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
    public void testRandomNumeric() {
        char[] testChars = {'0','9'};
        boolean[] found = {false, false};
        for (int i = 0; i < 100; i++) {
            String randString = RandomStringUtils.randomNumeric(10);
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
    public void testRandomAlphabetic() {
        char[] testChars = {'a', 'z', 'A', 'Z'};
        boolean[] found = {false, false, false, false};
        for (int i = 0; i < 100; i++) {
            String randString = RandomStringUtils.randomAlphabetic(10);
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
    public void testRandomAscii() {
        char[] testChars = {(char) 32, (char) 126};
        boolean[] found = {false, false};
        for (int i = 0; i < 100; i++) {
            String randString = RandomStringUtils.randomAscii(10);
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
    
    /** 
     * Test homogeneity of random strings generated --
     * i.e., test that characters show up with expected frequencies
     * in generated strings.  Will fail randomly about 1 in 1000 times.
     * Repeated failures indicate a problem.
     */
    public void testRandomStringUtilsHomog() {
        String set = "abc";
        char[] chars = set.toCharArray();
        String gen = "";
        int[] counts = {0,0,0};
        int[] expected = {200,200,200};
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
     * @param expected array of exptected frequency counts
     */
    private double chiSquare(int[] expected, int[] observed) {
        double sumSq = 0.0d;
        double dev = 0.0d;
        for (int i = 0; i< observed.length; i++) {
            dev = (double)(observed[i] - expected[i]);
            sumSq += dev*dev/(double)expected[i];
        }
        return sumSq;
    }           
        

    public static void main(String args[]) {
        TestRunner.run(suite());
    }
}

