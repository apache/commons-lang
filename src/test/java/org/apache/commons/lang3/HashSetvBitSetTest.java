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
import java.util.concurrent.TimeUnit;

import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.State;

/**
 * Test to show whether using BitSet for removeAll() methods is faster than using HashSet.
 */
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
public class HashSetvBitSetTest extends AbstractLangTest {

    private static final int numberOfElementsToCompute = 10;

    @Benchmark
    public int[] testHashSet() {
        final HashSet<Integer> toRemove = new HashSet<>();
        int found = 0;
        for (int i = 0; i < numberOfElementsToCompute; i++) {
            toRemove.add(found++);
        }
        return extractIndices(toRemove);
    }

    @Benchmark
    public int[] testBitSet() {
        final BitSet toRemove = new BitSet();
        int found = 0;
        for (int i = 0; i < numberOfElementsToCompute; i++) {
            toRemove.set(found++);
        }
        return extractIndices(toRemove);
    }

    @Benchmark
    public int[] timeBitSetRemoveAll() {
        final BitSet toRemove = new BitSet();
        final int[] array = new int[100];
        toRemove.set(10, 20);
        return (int[]) ArrayUtils.removeAll(array, toRemove);
    }

    @Benchmark
    public int[] timeExtractRemoveAll() {
        final BitSet toRemove = new BitSet();
        final int[] array = new int[100];
        toRemove.set(10, 20);
        final int[] extractIndices = extractIndices(toRemove);
        return (int[]) ArrayUtils.removeAll((Object) array, extractIndices);
    }

    // --- utility methods
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
        while ((j=coll.nextSetBit(j)) != -1) {
            result[i++] = j++;
        }
        return result;
    }
}
