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
package org.apache.commons.lang.math;

import java.util.Random;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * Test cases for the {@link RandomUtils} class.
 *
 * @author <a href="mailto:phil@steitz.com">Phil Steitz</a>
 * @version $Revision: 1.3 $ $Date: 2003/06/09 21:36:03 $
 */

public final class RandomUtilsTest extends TestCase {

    public RandomUtilsTest(String name) {
        super(name);
    }
    
    public void setUp() {
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(RandomUtilsTest.class);
        suite.setName("RandomUtils Tests");
        return suite;
    }
    
    /** test distribution of nextInt() */
    public void testNextInt() {
        tstNextInt(null);
    }
    
    /** test distribution of nextInt(Random) */
    public void testNextInt2() {
        Random rnd = new Random();
        rnd.setSeed(System.currentTimeMillis());
        tstNextInt(rnd);
    } 
    
    /** 
     * Generate 1000 values for nextInt(bound) and compare
     * the observed frequency counts to expected counts using
     * a chi-square test.
     * @param rnd Random to use if not null
     */
    private void tstNextInt(Random rnd) {
        int bound = 0;
        int result = 0;
        // test boundary condition: n = Integer.MAX_VALUE;
        bound = Integer.MAX_VALUE;
        if (rnd == null) {
            result = RandomUtils.nextInt(bound);
        } else {
            result = RandomUtils.nextInt(rnd,bound);
        }      
        assertTrue("result less than bound",result < bound);
        assertTrue("result non-negative",result >= 0);
        
        // test uniformity -- use Chi-Square test at .01 level
        bound = 4;
        int[] expected = new int[] {250,250,250,250};
        int[] observed = new int[] {0,0,0,0};
        for (int i = 0; i < 1000; i ++) {
            if (rnd == null) {
                result = RandomUtils.nextInt(bound);
            } else {
                result = RandomUtils.nextInt(rnd,bound);
            }     
            assertTrue(result < bound);
            assertTrue(result >= 0);
            observed[result]++;
        } 
        /* Use ChiSquare dist with df = 4-1 = 3, alpha = .001
         * Change to 11.34 for alpha = .01   
         */
        assertTrue(
            "chi-square test -- will fail about 1 in 1000 times",
            chiSquare(expected,observed) < 16.27);                                                            
    }  
    
    /** test distribution of nextLong() */
    public void testNextLong() {
        tstNextLong(null);
    }
    
    /** test distribution of nextLong(Random) BROKEN
     *  contract of nextLong(Random) is different from
     * nextLong() */
    public void testNextLong2() {
        Random rnd = new Random();
        rnd.setSeed(System.currentTimeMillis());
        tstNextLong(rnd);
    }
     
    /** 
     * Generate 1000 values for nextLong and check that
     * p(value < long.MAXVALUE/2) ~ 0.5. Use chi-square test
     * with df = 2-1 = 1  
     * @param rnd Random to use if not null
     */
    private void tstNextLong(Random rnd) {
        int[] expected = new int[] {500,500};
        int[] observed = new int[] {0,0};
        long result = 0;
        long midPoint = Long.MAX_VALUE/2;
        for (int i = 0; i < 1000; i ++) {
            if (rnd == null) {
                result = Math.abs(RandomUtils.nextLong());
            } else {
                result = Math.abs(RandomUtils.nextLong(rnd));
            }  
            if (result < midPoint) {
                observed[0]++;
            } else {
                observed[1]++;
            }
        }
        /* Use ChiSquare dist with df = 2-1 = 1, alpha = .001
         * Change to 6.64 for alpha = .01  
         */ 
        assertTrue(
            "chi-square test -- will fail about 1 in 1000 times",
            chiSquare(expected,observed) < 10.83); 
    }
        
    
    /** test distribution of nextBoolean() */
    public void testNextBoolean() {
        tstNextBoolean(null);
    }
    
    /** test distribution of nextBoolean(Random) */
    public void testNextBoolean2() {
        Random rnd = new Random();
        rnd.setSeed(System.currentTimeMillis());
        tstNextBoolean(rnd);
    }
    
    /** 
     * Generate 1000 values for nextBoolean and check that
     * p(value = false) ~ 0.5. Use chi-square test
     * with df = 2-1 = 1  
     * @param rnd Random to use if not null
     */
    private void tstNextBoolean(Random rnd) {
        int[] expected = new int[] {500,500};
        int[] observed = new int[] {0,0};
        boolean result = false;
        for (int i = 0; i < 1000; i ++) {
            if (rnd == null) {
                result = RandomUtils.nextBoolean();
            } else {
                result = RandomUtils.nextBoolean(rnd);
            }     
            if (result) {
                observed[0]++;
            } else {
                observed[1]++;
            }
        }
        /* Use ChiSquare dist with df = 2-1 = 1, alpha = .001
         * Change to 6.64 for alpha = .01 
         */
        assertTrue(
            "chi-square test -- will fail about 1 in 1000 times",
            chiSquare(expected,observed) < 10.83 );  
    }
    
    /** test distribution of nextFloat() */
    public void testNextFloat() {
        tstNextFloat(null);
    }
    
    /** test distribution of nextFloat(Random) */
    public void testNextFloat2() {
        Random rnd = new Random();
        rnd.setSeed(System.currentTimeMillis());
        tstNextFloat(rnd);
    }
    
    /** 
     * Generate 1000 values for nextFloat and check that
     * p(value < 0.5) ~ 0.5. Use chi-square test
     * with df = 2-1 = 1  
     * @param rnd Random to use if not null
     */
    private void tstNextFloat(Random rnd) {
        int[] expected = new int[] {500,500};
        int[] observed = new int[] {0,0};
        float result = 0;
        for (int i = 0; i < 1000; i ++) {
            if (rnd == null) {
                result = RandomUtils.nextFloat();
            } else {
                result = RandomUtils.nextFloat(rnd);
            }     
            if (result < 0.5) {
                observed[0]++;
            } else {
                observed[1]++;
            }
        }
        /* Use ChiSquare dist with df = 2-1 = 1, alpha = .001
         * Change to 6.64 for alpha = .01 
         */
        assertTrue(
            "chi-square test -- will fail about 1 in 1000 times",
            chiSquare(expected,observed) < 10.83);  
    }
    
    /** test distribution of nextDouble() */
    public void testNextDouble() {
        tstNextDouble(null);
    }
    
    /** test distribution of nextDouble(Random) */
    public void testNextDouble2() {
        Random rnd = new Random();
        rnd.setSeed(System.currentTimeMillis());
        tstNextDouble(rnd);
    }
    
    /** 
     * Generate 1000 values for nextFloat and check that
     * p(value < 0.5) ~ 0.5. Use chi-square test
     * with df = 2-1 = 1  
     * @param rnd Random to use if not null
     */
    private void tstNextDouble(Random rnd) {
        int[] expected = new int[] {500,500};
        int[] observed = new int[] {0,0};
        double result = 0;
        for (int i = 0; i < 1000; i ++) {
            if (rnd == null) {
                result = RandomUtils.nextDouble();
            } else {
                result = RandomUtils.nextDouble(rnd);
            }     
            if (result < 0.5) {
                observed[0]++;
            } else {
                observed[1]++;
            }
        }
        /* Use ChiSquare dist with df = 2-1 = 1, alpha = .001
         * Change to 6.64 for alpha = .01 
         */
        assertTrue(
            "chi-square test -- will fail about 1 in 1000 times",
            chiSquare(expected,observed) < 10.83);  
    }
    
    /** make sure that setSeed fails */
    public void testSetSeed() {
        try {
            RandomUtils.JVM_RANDOM.setSeed(1000);
            fail("expecting UnsupportedOperationException");
        } catch (UnsupportedOperationException ex) {
            ;
        }
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

}

