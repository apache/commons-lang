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
package org.apache.commons.lang.math;

import java.util.Random;

import junit.framework.TestCase;

/**
 * Test cases for the {@link RandomUtils} class.
 *
 * @author <a href="mailto:phil@steitz.com">Phil Steitz</a>
 * @version $Revision$ $Date$
 */

public final class RandomUtilsFreqTest extends TestCase {

    public RandomUtilsFreqTest(String name) {
        super(name);
    }

    public void testNextIntBound(){
        tstNextInt(10);
        tstNextInt(1<<8);
        tstNextInt((1<<8)+1);
        tstNextInt((1<<8)-1);
        tstNextInt(1<<30);
        tstNextInt((1<<30)-1);
        tstNextInt((1<<30)+1);
        Random rnd = new Random();
        for(int i=0;i<10;i++){
            tstNextInt(rnd.nextInt(Integer.MAX_VALUE));
        }
    }

    public void testNextLongBound(){
        tstNextLong(Integer.MAX_VALUE-1);
        tstNextLong(Integer.MAX_VALUE);
        tstNextLong((long)Integer.MAX_VALUE+1);
        tstNextLong(Long.MAX_VALUE/1024);
        tstNextLong(Long.MAX_VALUE/920);
        tstNextLong(Long.MAX_VALUE/1000);
        tstNextLong(Long.MAX_VALUE/512);
        tstNextLong(Long.MAX_VALUE/64);
        tstNextLong(Long.MAX_VALUE-1);
        tstNextLong(Long.MAX_VALUE);
        Random rnd = new Random();
        for(int i=0;i<10;i++){
            tstNextLong(rnd.nextInt(Integer.MAX_VALUE));
        }
        for(int i=0;i<10;i++){
            tstNextLong(rnd.nextLong() & 0x7fffffffffffffffL);
        }
    }
    
    /** 
     * Generate 1000 values for nextInt(bound) and compare
     * the observed frequency counts to expected counts using
     * a chi-square test.
     * @param bound upper bound to use
     */
    private void tstNextInt(int bound) {
        assertTrue(bound+" Must be non-negative",bound>=0);
        int result = 0;
        Random rnd = new Random();
        // test uniformity -- use Chi-Square test at .01 level
        int[] expected = new int[] {500,500};
        int[] observed = new int[] {0,0};
        int[] observed2 = new int[] {0,0};
        for (int i = 0; i < 1000; i ++) {
            result = rnd.nextInt(bound);
            assertTrue(result+" Must be non-negative",result>=0);
            assertTrue(result+" Must be less than bound: "+bound,result<bound);
            if (result < bound/2) {
                observed[0]++;
            } else {
                observed[1]++;
            }
            observed2[result%2]++;
        } 
        /* Use ChiSquare dist with df = 2-1 = 1, alpha = .001
         * Change to 6.64 for alpha = .01  
         */ 
        double chiSquare = chiSquare(expected,observed);
        assertTrue(
            "mid point chi-square test -- will fail about 1 in 1000 times: "+chiSquare,
                chiSquare < 10.83);
        chiSquare = chiSquare(expected,observed2);
        assertTrue(
                "odd/even chi-square test -- will fail about 1 in 1000 times: "+chiSquare,
                chiSquare < 10.83);
    }  

    /** 
     * Generate 1000 values for nextInt(bound) and compare
     * the observed frequency counts to expected counts using
     * a chi-square test.
     * @param bound upper bound to use
     */
    private void tstNextLong(long bound) {
        // Distribution
        int[] expected = new int[] {500,500};
        int[] observed = new int[] {0,0};
        // Even/Odd
        int[] expected2 = new int[] {500,500};
        int[] observed2 = new int[] {0,0};
        long result = 0;
        long midPoint = bound/2;
        for (int i = 0; i < 1000; i ++) {
            result = JVMRandom.nextLong(bound);
            assertTrue(result+" Must be non-negative",result>=0);
            assertTrue(result+" Must be less than bound: "+bound,result<bound);
            if (result < midPoint) {
                observed[0]++;
            } else {
                observed[1]++;
            }
            if (result % 2 == 0) {
               observed2[0]++;
            } else {
               observed2[1]++;
            }
        }
        /* Use ChiSquare dist with df = 2-1 = 1, alpha = .001
         * Change to 6.64 for alpha = .01  
         */ 
        final double chiSquare = chiSquare(expected,observed);
        assertTrue(
            "mid point chi-square test -- will fail about 1 in 1000 times: "
                +chiSquare+":"+observed[0]+","+observed[1],
            chiSquare < 10.83); 
        final double oddEven = chiSquare(expected2,observed2);
        assertTrue(
                "odd/even chi-square test -- will fail about 1 in 1000 times: "
                +oddEven+":"+observed2[0]+","+observed2[1],
                oddEven < 10.83); 
    }

    /**
     * Computes Chi-Square statistic given observed and expected counts
     * @param observed array of observed frequency counts
     * @param expected array of expected frequency counts
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
