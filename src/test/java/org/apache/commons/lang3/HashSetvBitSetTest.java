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

import java.util.BitSet;
import java.util.HashSet;

import org.junit.Assert;
import org.junit.Test;

/**
 * Test to show whether using BitSet for removeAll() methods is faster than using HashSet.
 */
public class HashSetvBitSetTest {

    private static final int LOOPS = 2000; // number of times to invoke methods

    @Test
    public void testTimes() {
        timeHashSet(10); // warmup
        timeBitSet(10); // warmup
        printTimes(0);
        printTimes(5);
        printTimes(10);
        printTimes(200);
        printTimes(50);
        printTimes(100);
        printTimes(1000);
        printTimes(2000);
    }
    private void printTimes(int count) {
        long hashSet = timeHashSet(count);
        long bitSet = timeBitSet(count);
        // If percent is less than 100, then bitset is faster
        System.out.println("Ratio="+(bitSet*100/hashSet)+"% count="+count+" hash="+hashSet+" bits="+bitSet);
    }

    private static long timeHashSet(int count) {
        int [] result = new int[0];
        long start = System.nanoTime();
        for (int i = 0; i < LOOPS; i++) {
            result = testHashSet(count);
        }
        long elapsed = System.nanoTime() - start;
        Assert.assertEquals(count, result.length);
        return elapsed;
    }

    private static long timeBitSet(int count) {
        int [] result = new int[0];
        long start = System.nanoTime();
        for (int i = 0; i < LOOPS; i++) {
            result = testBitSet(count);
        }
        long elapsed = System.nanoTime() - start;
        Assert.assertEquals(count, result.length);
        return elapsed;
    }

    @SuppressWarnings("boxing")
    private static int[] testHashSet(int count) {
        HashSet<Integer> toRemove = new HashSet<Integer>();
            int found = 0;
            for (int i = 0; i < count; i++) {
                toRemove.add(found++);
            }
            return extractIndices(toRemove);
        }
    
    private static int[] testBitSet(int count) {
        BitSet toRemove = new BitSet();
        int found = 0;
        for (int i = 0; i < count; i++) {
            toRemove.set(found++);
        }
        return extractIndices(toRemove);
    }
    

    private static int[] extractIndices(HashSet<Integer> coll) {
        int[] result = new int[coll.size()];
        int i = 0;
        for (Integer index : coll) {
            result[i++] = index.intValue();
        }
        return result;
    }

    private static int[] extractIndices(BitSet coll) {
        int[] result = new int[coll.cardinality()];
        int i = 0;
        int j=0;
        while((j=coll.nextSetBit(j)) != -1) {
            result[i++] = j++;            
        }
        return result;
    }
}