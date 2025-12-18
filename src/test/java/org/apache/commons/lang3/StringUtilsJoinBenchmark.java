/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang3;

import java.util.Random;
import java.util.concurrent.TimeUnit;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Param;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.Warmup;

public class StringUtilsJoinBenchmark {

    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.NANOSECONDS)
    @State(Scope.Thread)
    @Fork(1)
    @Warmup(iterations = 2, time = 1)
    @Measurement(iterations = 3, time = 1)
    public static class IntArrayBenchmark {

        @Param({ "10", "100", "1000", "10000" })
        private int size;

        private int[] intArray;

        @Setup
        public void setup() {
            intArray = new int[size];
            Random random = new Random(235);
            for (int i = 0; i < size; i++) {
                intArray[i] = random.nextInt(10000);
            }
        }

        @Benchmark
        public String testJoinIntWithoutInitCapacity() {
            return StringUtilsWithoutInitCapacity.join(intArray, ',', 0, intArray.length);
        }

        @Benchmark
        public String testJoinIntWithInitCapacity() {
            return StringUtilsWithInitCapacity.join(intArray, ',', 0, intArray.length);
        }
    }

    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.NANOSECONDS)
    @State(Scope.Thread)
    @Fork(1)
    @Warmup(iterations = 2, time = 1)
    @Measurement(iterations = 3, time = 1)
    public static class BooleanArrayBenchmark {

        private boolean[] boolArray;

        @Setup
        public void setup() {
            boolArray = new boolean[10000];
            for (int i = 0; i < boolArray.length; i++) {
                boolArray[i] = (i % 2 == 0);
            }
        }

        @Benchmark
        public String testJoinBooleanInitArrayLength()  {
            return StringUtilsWithoutInitCapacity.join(boolArray, ',', 0, 10);
        }

        @Benchmark
        public String testJoinBooleanInitNoOfItems() {
            return StringUtilsWithInitCapacity.join(boolArray, ',', 0, 10);
        }
    }

    public static class StringUtilsWithInitCapacity {

        public static final String EMPTY = "";

        public static String join(final boolean[] array, final char delimiter, final int startIndex, final int endIndex) {
            if (array == null) {
                return null;
            }
            final int noOfItems = endIndex - startIndex;
            if (noOfItems <= 0) {
                return EMPTY;
            }
            final StringBuilder stringBuilder = new StringBuilder(noOfItems * 5 + noOfItems - 1);
            stringBuilder.append(array[startIndex]);
            for (int i = startIndex + 1; i < endIndex; i++) {
                stringBuilder
                    .append(delimiter)
                    .append(array[i]);
            }
            return stringBuilder.toString();
        }

        public static String join(final int[] array, final char delimiter, final int startIndex, final int endIndex) {
            if (array == null) {
                return null;
            }
            final int noOfItems = endIndex - startIndex;
            if (noOfItems <= 0) {
                return EMPTY;
            }
            final StringBuilder stringBuilder = new StringBuilder(noOfItems * 4);
            stringBuilder.append(array[startIndex]);
            for (int i = startIndex + 1; i < endIndex; i++) {
                stringBuilder
                    .append(delimiter)
                    .append(array[i]);
            }
            return stringBuilder.toString();
        }
    }

    public static class StringUtilsWithoutInitCapacity {

        public static final String EMPTY = "";

        public static String join(final boolean[] array, final char delimiter, final int startIndex, final int endIndex) {
            if (array == null) {
                return null;
            }
            if (endIndex - startIndex <= 0) {
                return EMPTY;
            }
            final StringBuilder stringBuilder = new StringBuilder(array.length * 5 + array.length - 1);
            for (int i = startIndex; i < endIndex; i++) {
                stringBuilder
                    .append(array[i])
                    .append(delimiter);
            }
            return stringBuilder.substring(0, stringBuilder.length() - 1);
        }

        public static String join(final int[] array, final char delimiter, final int startIndex, final int endIndex) {
            if (array == null) {
                return null;
            }
            if (endIndex - startIndex <= 0) {
                return EMPTY;
            }
            final StringBuilder stringBuilder = new StringBuilder();
            for (int i = startIndex; i < endIndex; i++) {
                stringBuilder
                    .append(array[i])
                    .append(delimiter);
            }
            return stringBuilder.substring(0, stringBuilder.length() - 1);
        }
    }
}