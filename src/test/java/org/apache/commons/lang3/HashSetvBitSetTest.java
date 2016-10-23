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
    private static final int LOOPS2 = 10000;

    @Test
    public void testTimes() {
        timeHashSet(10); // warmup
        timeBitSet(10); // warmup
        long timeDiff = printTimes(0);
        timeDiff += printTimes(5);
        timeDiff += printTimes(10);
        timeDiff += printTimes(200);
        timeDiff += printTimes(50);
        timeDiff += printTimes(100);
        timeDiff += printTimes(1000);
        timeDiff += printTimes(2000);
        Assert.assertTrue(timeDiff <= 0);
    }

    /**
     * @return bitSet - HashSet
     */
    private long printTimes(final int count) {
        final long hashSet = timeHashSet(count);
        final long bitSet = timeBitSet(count);
        // If percent is less than 100, then bitset is faster
        System.out.println("Ratio="+(bitSet*100/hashSet)+"% count="+count+" hash="+hashSet+" bits="+bitSet);
        return bitSet - hashSet;
    }

    private static long timeHashSet(final int count) {
        int [] result = new int[0];
        final long start = System.nanoTime();
        for (int i = 0; i < LOOPS; i++) {
            result = testHashSet(count);
        }
        final long elapsed = System.nanoTime() - start;
        Assert.assertEquals(count, result.length);
        return elapsed;
    }

    private static long timeBitSet(final int count) {
        int [] result = new int[0];
        final long start = System.nanoTime();
        for (int i = 0; i < LOOPS; i++) {
            result = testBitSet(count);
        }
        final long elapsed = System.nanoTime() - start;
        Assert.assertEquals(count, result.length);
        return elapsed;
    }

    @SuppressWarnings("boxing")
    private static int[] testHashSet(final int count) {
        final HashSet<Integer> toRemove = new HashSet<>();
            int found = 0;
            for (int i = 0; i < count; i++) {
                toRemove.add(found++);
            }
            return extractIndices(toRemove);
        }
    
    private static int[] testBitSet(final int count) {
        final BitSet toRemove = new BitSet();
        int found = 0;
        for (int i = 0; i < count; i++) {
            toRemove.set(found++);
        }
        return extractIndices(toRemove);
    }
    

    private static int[] extractIndices(final HashSet<Integer> coll) {
        final int[] result = new int[coll.size()];
        int i = 0;
        for (final Integer index : coll) {
            result[i++] = index.intValue();
        }
        return result;
    }

    private static int[] extractIndices(final BitSet coll) {
        final int[] result = new int[coll.cardinality()];
        int i = 0;
        int j=0;
        while((j=coll.nextSetBit(j)) != -1) {
            result[i++] = j++;            
        }
        return result;
    }
    
    @Test
    public void testTimesExtractOrBitset() {
        final BitSet toRemove = new BitSet();
        final int[] array = new int[100];
        toRemove.set(10, 20);
        timeBitSetRemoveAll(array, toRemove); // warmup
        timeExtractRemoveAll(array, toRemove); // warmup
        long timeDiff = printTimes(100,1);
        timeDiff += printTimes(100,10);
        timeDiff += printTimes(100,50);
        timeDiff += printTimes(100,100);
        timeDiff += printTimes(1000,10);
        timeDiff += printTimes(1000,100);
        timeDiff += printTimes(1000,500);
        timeDiff += printTimes(1000,1000);
        Assert.assertTrue(timeDiff <= 0);
    }

    private long printTimes(final int arraySize, final int bitSetSize) {
        final int[] array = new int[arraySize];
        final BitSet remove = new BitSet();
        for (int i = 0; i < bitSetSize; i++) {
            remove.set(i);
        }
        final long bitSet = timeBitSetRemoveAll(array, remove );
        final long extract = timeExtractRemoveAll(array, remove);
        // If percent is less than 100, then direct use of bitset is faster
        System.out.println("Ratio="+(bitSet*100/extract)+"% array="+array.length+" count="+remove.cardinality()+" extract="+extract+" bitset="+bitSet);
        return bitSet - extract;
    }

    private long timeBitSetRemoveAll(final int[] array, final BitSet toRemove) {
        int[] output = new int[0];
        final long start = System.nanoTime();
        for(int i = 0; i < LOOPS2; i++){
            output = (int[]) ArrayUtils.removeAll(array, toRemove);            
        }
        final long end = System.nanoTime();
        Assert.assertEquals(array.length-toRemove.cardinality(), output.length);
        return end - start;
    }
    
    private long timeExtractRemoveAll(final int[] array, final BitSet toRemove) {
        int[] output = new int[0];
        final long start = System.nanoTime();
        for(int i = 0; i < LOOPS2; i++){
            final int[] extractIndices = extractIndices(toRemove);
            output = (int[]) ArrayUtils.removeAll((Object)array, extractIndices);
        }
        final long end = System.nanoTime();
        Assert.assertEquals(array.length-toRemove.cardinality(), output.length);
        return end - start;
    }
    
}