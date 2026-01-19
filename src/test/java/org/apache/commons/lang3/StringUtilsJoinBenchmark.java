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

/**
 * <p>
 * JMH benchmark results for {@code StringUtils.join} on Java 21, executed without JaCoCo instrumentation.
 * </p>
 * <pre>
 * mvn clean test -P benchmark -Dbenchmark=org.apache.commons.lang3.StringUtilsJoinBenchmark -P '!jacoco'
 * </pre>
 *
 * <pre>
 * Benchmark                                                              (size)  Mode  Cnt       Score       Error  Units
 * StringUtilsJoinBenchmark.BooleanArrayBenchmark.testJoinBoolean_3_20_0      10  avgt    5      20.744 ±     0.254  ns/op
 * StringUtilsJoinBenchmark.BooleanArrayBenchmark.testJoinBoolean_3_20_0     100  avgt    5      63.396 ±     0.933  ns/op
 * StringUtilsJoinBenchmark.BooleanArrayBenchmark.testJoinBoolean_3_20_0    1000  avgt    5     245.837 ±    96.959  ns/op
 * StringUtilsJoinBenchmark.BooleanArrayBenchmark.testJoinBoolean_3_20_0   10000  avgt    5     391.794 ±     1.479  ns/op
 * StringUtilsJoinBenchmark.BooleanArrayBenchmark.testJoinBoolean_3_21_0      10  avgt    5      53.083 ±    10.429  ns/op
 * StringUtilsJoinBenchmark.BooleanArrayBenchmark.testJoinBoolean_3_21_0     100  avgt    5      49.502 ±     3.611  ns/op
 * StringUtilsJoinBenchmark.BooleanArrayBenchmark.testJoinBoolean_3_21_0    1000  avgt    5      52.685 ±    12.080  ns/op
 * StringUtilsJoinBenchmark.BooleanArrayBenchmark.testJoinBoolean_3_21_0   10000  avgt    5      52.111 ±     8.522  ns/op
 *
 * StringUtilsJoinBenchmark.ByteArrayBenchmark.testJoinByte_3_20_0            10  avgt    5     111.913 ±    34.777  ns/op
 * StringUtilsJoinBenchmark.ByteArrayBenchmark.testJoinByte_3_20_0           100  avgt    5     227.867 ±     1.022  ns/op
 * StringUtilsJoinBenchmark.ByteArrayBenchmark.testJoinByte_3_20_0          1000  avgt    5    2512.815 ±     7.439  ns/op
 * StringUtilsJoinBenchmark.ByteArrayBenchmark.testJoinByte_3_20_0         10000  avgt    5   24995.528 ±   371.981  ns/op
 * StringUtilsJoinBenchmark.ByteArrayBenchmark.testJoinByte_3_21_0            10  avgt    5      39.934 ±    17.696  ns/op
 * StringUtilsJoinBenchmark.ByteArrayBenchmark.testJoinByte_3_21_0           100  avgt    5     208.709 ±     0.630  ns/op
 * StringUtilsJoinBenchmark.ByteArrayBenchmark.testJoinByte_3_21_0          1000  avgt    5    2028.011 ±    15.023  ns/op
 * StringUtilsJoinBenchmark.ByteArrayBenchmark.testJoinByte_3_21_0         10000  avgt    5   20696.625 ±   334.290  ns/op
 *
 * StringUtilsJoinBenchmark.CharArrayBenchmark.testJoinChar_3_20_0            10  avgt    5      27.610 ±     1.302  ns/op
 * StringUtilsJoinBenchmark.CharArrayBenchmark.testJoinChar_3_20_0           100  avgt    5     180.165 ±     3.730  ns/op
 * StringUtilsJoinBenchmark.CharArrayBenchmark.testJoinChar_3_20_0          1000  avgt    5    1464.739 ±    41.632  ns/op
 * StringUtilsJoinBenchmark.CharArrayBenchmark.testJoinChar_3_20_0         10000  avgt    5   13706.722 ±   208.715  ns/op
 * StringUtilsJoinBenchmark.CharArrayBenchmark.testJoinChar_3_21_0            10  avgt    5      24.202 ±     3.130  ns/op
 * StringUtilsJoinBenchmark.CharArrayBenchmark.testJoinChar_3_21_0           100  avgt    5     150.218 ±     2.129  ns/op
 * StringUtilsJoinBenchmark.CharArrayBenchmark.testJoinChar_3_21_0          1000  avgt    5    1357.945 ±     8.898  ns/op
 * StringUtilsJoinBenchmark.CharArrayBenchmark.testJoinChar_3_21_0         10000  avgt    5   13152.872 ±   447.029  ns/op
 *
 * StringUtilsJoinBenchmark.DoubleArrayBenchmark.testJoinDouble_3_20_0        10  avgt    5     327.488 ±     3.735  ns/op
 * StringUtilsJoinBenchmark.DoubleArrayBenchmark.testJoinDouble_3_20_0       100  avgt    5    3246.046 ±    24.045  ns/op
 * StringUtilsJoinBenchmark.DoubleArrayBenchmark.testJoinDouble_3_20_0      1000  avgt    5   33334.269 ±  1910.838  ns/op
 * StringUtilsJoinBenchmark.DoubleArrayBenchmark.testJoinDouble_3_20_0     10000  avgt    5  330373.734 ± 10610.857  ns/op
 * StringUtilsJoinBenchmark.DoubleArrayBenchmark.testJoinDouble_3_21_0        10  avgt    5     338.655 ±    15.557  ns/op
 * StringUtilsJoinBenchmark.DoubleArrayBenchmark.testJoinDouble_3_21_0       100  avgt    5    3141.778 ±    65.095  ns/op
 * StringUtilsJoinBenchmark.DoubleArrayBenchmark.testJoinDouble_3_21_0      1000  avgt    5   31349.168 ±   194.029  ns/op
 * StringUtilsJoinBenchmark.DoubleArrayBenchmark.testJoinDouble_3_21_0     10000  avgt    5  315213.960 ± 26935.888  ns/op
 *
 * StringUtilsJoinBenchmark.FloatArrayBenchmark.testJoinFloat_3_20_0          10  avgt    5     224.880 ±     5.634  ns/op
 * StringUtilsJoinBenchmark.FloatArrayBenchmark.testJoinFloat_3_20_0         100  avgt    5    2451.727 ±    22.667  ns/op
 * StringUtilsJoinBenchmark.FloatArrayBenchmark.testJoinFloat_3_20_0        1000  avgt    5   31433.944 ±  2287.593  ns/op
 * StringUtilsJoinBenchmark.FloatArrayBenchmark.testJoinFloat_3_20_0       10000  avgt    5  374632.760 ± 50931.471  ns/op
 * StringUtilsJoinBenchmark.FloatArrayBenchmark.testJoinFloat_3_21_0          10  avgt    5     254.568 ±     9.830  ns/op
 * StringUtilsJoinBenchmark.FloatArrayBenchmark.testJoinFloat_3_21_0         100  avgt    5    2916.277 ±   105.372  ns/op
 * StringUtilsJoinBenchmark.FloatArrayBenchmark.testJoinFloat_3_21_0        1000  avgt    5   35840.570 ±  2000.700  ns/op
 * StringUtilsJoinBenchmark.FloatArrayBenchmark.testJoinFloat_3_21_0       10000  avgt    5  293352.232 ± 20630.992  ns/op
 *
 * StringUtilsJoinBenchmark.IntArrayBenchmark.testJoinInt_3_20_0              10  avgt    5      84.855 ±     2.455  ns/op
 * StringUtilsJoinBenchmark.IntArrayBenchmark.testJoinInt_3_20_0             100  avgt    5     796.464 ±    13.394  ns/op
 * StringUtilsJoinBenchmark.IntArrayBenchmark.testJoinInt_3_20_0            1000  avgt    5    9734.390 ±   407.508  ns/op
 * StringUtilsJoinBenchmark.IntArrayBenchmark.testJoinInt_3_20_0           10000  avgt    5   98763.469 ± 11627.264  ns/op
 * StringUtilsJoinBenchmark.IntArrayBenchmark.testJoinInt_3_21_0              10  avgt    5      74.442 ±     0.558  ns/op
 * StringUtilsJoinBenchmark.IntArrayBenchmark.testJoinInt_3_21_0             100  avgt    5     754.152 ±     2.570  ns/op
 * StringUtilsJoinBenchmark.IntArrayBenchmark.testJoinInt_3_21_0            1000  avgt    5    7786.939 ±   105.617  ns/op
 * StringUtilsJoinBenchmark.IntArrayBenchmark.testJoinInt_3_21_0           10000  avgt    5   78220.569 ±  2943.719  ns/op
 *
 * StringUtilsJoinBenchmark.LongArrayBenchmark.testJoinLong_3_20_0            10  avgt    5     160.601 ±     0.316  ns/op
 * StringUtilsJoinBenchmark.LongArrayBenchmark.testJoinLong_3_20_0           100  avgt    5    1458.751 ±    34.769  ns/op
 * StringUtilsJoinBenchmark.LongArrayBenchmark.testJoinLong_3_20_0          1000  avgt    5   15690.854 ±   203.858  ns/op
 * StringUtilsJoinBenchmark.LongArrayBenchmark.testJoinLong_3_20_0         10000  avgt    5  158644.000 ±  1108.395  ns/op
 * StringUtilsJoinBenchmark.LongArrayBenchmark.testJoinLong_3_21_0            10  avgt    5     150.858 ±     2.435  ns/op
 * StringUtilsJoinBenchmark.LongArrayBenchmark.testJoinLong_3_21_0           100  avgt    5    1375.840 ±    14.656  ns/op
 * StringUtilsJoinBenchmark.LongArrayBenchmark.testJoinLong_3_21_0          1000  avgt    5   14564.621 ±   278.250  ns/op
 * StringUtilsJoinBenchmark.LongArrayBenchmark.testJoinLong_3_21_0         10000  avgt    5  152825.668 ±   877.466  ns/op
 *
 * StringUtilsJoinBenchmark.ShortArrayBenchmark.testJoinShort_3_20_0          10  avgt    5      45.996 ±     4.032  ns/op
 * StringUtilsJoinBenchmark.ShortArrayBenchmark.testJoinShort_3_20_0         100  avgt    5     421.166 ±    10.521  ns/op
 * StringUtilsJoinBenchmark.ShortArrayBenchmark.testJoinShort_3_20_0        1000  avgt    5    5396.685 ±    22.345  ns/op
 * StringUtilsJoinBenchmark.ShortArrayBenchmark.testJoinShort_3_20_0       10000  avgt    5   59389.744 ±   795.609  ns/op
 * StringUtilsJoinBenchmark.ShortArrayBenchmark.testJoinShort_3_21_0          10  avgt    5      51.526 ±     8.371  ns/op
 * StringUtilsJoinBenchmark.ShortArrayBenchmark.testJoinShort_3_21_0         100  avgt    5     392.907 ±    30.972  ns/op
 * StringUtilsJoinBenchmark.ShortArrayBenchmark.testJoinShort_3_21_0        1000  avgt    5    4869.440 ±    57.132  ns/op
 * StringUtilsJoinBenchmark.ShortArrayBenchmark.testJoinShort_3_21_0       10000  avgt    5   51744.074 ±   121.176  ns/op
 * </pre>
 */
public class StringUtilsJoinBenchmark {

    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.NANOSECONDS)
    @State(Scope.Thread)
    @Fork(1)
    @Warmup(iterations = WARMUP_ITERATIONS, time = WARMUP_TIME)
    @Measurement(iterations = MEASUREMENT_ITERATIONS, time = MEASUREMENT_TIME)
    public static class BooleanArrayBenchmark {

        @Param({ "10", "100", "1000", "10000" })
        private int size;
        private boolean[] array;

        @Setup
        public void setup() {
            array = new boolean[size];
            for (int i = 0; i < array.length; i++) {
                array[i] = i % 2 == 0;
            }
        }

        @Benchmark
        public String testJoinBoolean_3_20_0() {
            return StringUtils_3_20_0.join(array, ',', 0, 10);
        }

        @Benchmark
        public String testJoinBoolean_3_21_0() {
            return StringUtils_3_21_0.join(array, ',', 0, 10);
        }
    }
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.NANOSECONDS)
    @State(Scope.Thread)
    @Fork(1)
    @Warmup(iterations = WARMUP_ITERATIONS, time = WARMUP_TIME)
    @Measurement(iterations = MEASUREMENT_ITERATIONS, time = MEASUREMENT_TIME)
    public static class ByteArrayBenchmark {

        @Param({ "10", "100", "1000", "10000" })
        private int size;
        private byte[] array;

        @Setup
        public void setup() {
            array = new byte[size];
            final Random random = newRandom();
            for (int i = 0; i < size; i++) {
                array[i] = (byte) random.nextInt(Byte.MAX_VALUE);
            }
        }

        @Benchmark
        public String testJoinByte_3_20_0() {
            return StringUtils_3_20_0.join(array, ',', 0, array.length);
        }

        @Benchmark
        public String testJoinByte_3_21_0() {
            return StringUtils_3_21_0.join(array, ',', 0, array.length);
        }
    }
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.NANOSECONDS)
    @State(Scope.Thread)
    @Fork(1)
    @Warmup(iterations = WARMUP_ITERATIONS, time = WARMUP_TIME)
    @Measurement(iterations = MEASUREMENT_ITERATIONS, time = MEASUREMENT_TIME)
    public static class CharArrayBenchmark {

        @Param({ "10", "100", "1000", "10000" })
        private int size;
        private char[] array;

        @Setup
        public void setup() {
            array = new char[size];
            final Random random = newRandom();
            for (int i = 0; i < size; i++) {
                array[i] = (char) random.nextInt(Character.MAX_VALUE);
            }
        }

        @Benchmark
        public String testJoinChar_3_20_0() {
            return StringUtils_3_20_0.join(array, ',', 0, array.length);
        }

        @Benchmark
        public String testJoinChar_3_21_0() {
            return StringUtils_3_21_0.join(array, ',', 0, array.length);
        }
    }
    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.NANOSECONDS)
    @State(Scope.Thread)
    @Fork(1)
    @Warmup(iterations = WARMUP_ITERATIONS, time = WARMUP_TIME)
    @Measurement(iterations = MEASUREMENT_ITERATIONS, time = MEASUREMENT_TIME)
    public static class DoubleArrayBenchmark {

        @Param({ "10", "100", "1000", "10000" })
        private int size;
        private double[] array;

        @Setup
        public void setup() {
            array = new double[size];
            final Random random = newRandom();
            for (int i = 0; i < size; i++) {
                array[i] = random.nextDouble();
            }
        }

        @Benchmark
        public String testJoinDouble_3_20_0() {
            return StringUtils_3_20_0.join(array, ',', 0, array.length);
        }

        @Benchmark
        public String testJoinDouble_3_21_0() {
            return StringUtils_3_21_0.join(array, ',', 0, array.length);
        }
    }

    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.NANOSECONDS)
    @State(Scope.Thread)
    @Fork(1)
    @Warmup(iterations = WARMUP_ITERATIONS, time = WARMUP_TIME)
    @Measurement(iterations = MEASUREMENT_ITERATIONS, time = MEASUREMENT_TIME)
    public static class FloatArrayBenchmark {

        @Param({ "10", "100", "1000", "10000" })
        private int size;
        private float[] array;

        @Setup
        public void setup() {
            array = new float[size];
            final Random random = newRandom();
            for (int i = 0; i < size; i++) {
                array[i] = random.nextFloat();
            }
        }

        @Benchmark
        public String testJoinFloat_3_21_0() {
            return StringUtils_3_21_0.join(array, ',', 0, array.length);
        }

        @Benchmark
        public String testJoinFloat3_20_0() {
            return StringUtils_3_20_0.join(array, ',', 0, array.length);
        }
    }

    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.NANOSECONDS)
    @State(Scope.Thread)
    @Fork(1)
    @Warmup(iterations = WARMUP_ITERATIONS, time = WARMUP_TIME)
    @Measurement(iterations = MEASUREMENT_ITERATIONS, time = MEASUREMENT_TIME)
    public static class IntArrayBenchmark {

        @Param({ "10", "100", "1000", "10000" })
        private int size;
        private int[] array;

        @Setup
        public void setup() {
            array = new int[size];
            final Random random = newRandom();
            for (int i = 0; i < size; i++) {
                array[i] = random.nextInt();
            }
        }

        @Benchmark
        public String testJoinInt_3_20_0() {
            return StringUtils_3_20_0.join(array, ',', 0, array.length);
        }

        @Benchmark
        public String testJoinInt_3_21_0() {
            return StringUtils_3_21_0.join(array, ',', 0, array.length);
        }
    }

    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.NANOSECONDS)
    @State(Scope.Thread)
    @Fork(1)
    @Warmup(iterations = WARMUP_ITERATIONS, time = WARMUP_TIME)
    @Measurement(iterations = MEASUREMENT_ITERATIONS, time = MEASUREMENT_TIME)
    public static class LongArrayBenchmark {

        @Param({ "10", "100", "1000", "10000" })
        private int size;
        private long[] array;

        @Setup
        public void setup() {
            array = new long[size];
            final Random random = newRandom();
            for (int i = 0; i < size; i++) {
                array[i] = random.nextLong();
            }
        }

        @Benchmark
        public String testJoinLong_3_20_0() {
            return StringUtils_3_20_0.join(array, ',', 0, array.length);
        }

        @Benchmark
        public String testJoinLong_3_21_0() {
            return StringUtils_3_21_0.join(array, ',', 0, array.length);
        }
    }

    @BenchmarkMode(Mode.AverageTime)
    @OutputTimeUnit(TimeUnit.NANOSECONDS)
    @State(Scope.Thread)
    @Fork(1)
    @Warmup(iterations = WARMUP_ITERATIONS, time = WARMUP_TIME)
    @Measurement(iterations = MEASUREMENT_ITERATIONS, time = MEASUREMENT_TIME)
    public static class ShortArrayBenchmark {

        @Param({ "10", "100", "1000", "10000" })
        private int size;
        private short[] array;

        @Setup
        public void setup() {
            array = new short[size];
            final Random random = newRandom();
            for (int i = 0; i < size; i++) {
                array[i] = (short) random.nextInt(Short.MAX_VALUE);
            }
        }

        @Benchmark
        public String testJoinShort_3_20_0() {
            return StringUtils_3_20_0.join(array, ',', 0, array.length);
        }

        @Benchmark
        public String testJoinShort_3_21_0() {
            return StringUtils_3_21_0.join(array, ',', 0, array.length);
        }
    }

    public static class StringUtils_3_20_0 {

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
                stringBuilder.append(array[i]).append(delimiter);
            }
            return stringBuilder.substring(0, stringBuilder.length() - 1);
        }

        public static String join(final byte[] array, final char delimiter, final int startIndex, final int endIndex) {
            if (array == null) {
                return null;
            }
            if (endIndex - startIndex <= 0) {
                return EMPTY;
            }
            final StringBuilder stringBuilder = new StringBuilder();
            for (int i = startIndex; i < endIndex; i++) {
                stringBuilder.append(array[i]).append(delimiter);
            }
            return stringBuilder.substring(0, stringBuilder.length() - 1);
        }

        public static String join(final char[] array, final char delimiter, final int startIndex, final int endIndex) {
            if (array == null) {
                return null;
            }
            if (endIndex - startIndex <= 0) {
                return EMPTY;
            }
            final StringBuilder stringBuilder = new StringBuilder(array.length * 2 - 1);
            for (int i = startIndex; i < endIndex; i++) {
                stringBuilder.append(array[i]).append(delimiter);
            }
            return stringBuilder.substring(0, stringBuilder.length() - 1);
        }

        public static String join(final double[] array, final char delimiter, final int startIndex, final int endIndex) {
            if (array == null) {
                return null;
            }
            if (endIndex - startIndex <= 0) {
                return EMPTY;
            }
            final StringBuilder stringBuilder = new StringBuilder();
            for (int i = startIndex; i < endIndex; i++) {
                stringBuilder.append(array[i]).append(delimiter);
            }
            return stringBuilder.substring(0, stringBuilder.length() - 1);
        }

        public static String join(final float[] array, final char delimiter, final int startIndex, final int endIndex) {
            if (array == null) {
                return null;
            }
            if (endIndex - startIndex <= 0) {
                return EMPTY;
            }
            final StringBuilder stringBuilder = new StringBuilder();
            for (int i = startIndex; i < endIndex; i++) {
                stringBuilder.append(array[i]).append(delimiter);
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
                stringBuilder.append(array[i]).append(delimiter);
            }
            return stringBuilder.substring(0, stringBuilder.length() - 1);
        }

        public static String join(final long[] array, final char delimiter, final int startIndex, final int endIndex) {
            if (array == null) {
                return null;
            }
            if (endIndex - startIndex <= 0) {
                return EMPTY;
            }
            final StringBuilder stringBuilder = new StringBuilder();
            for (int i = startIndex; i < endIndex; i++) {
                stringBuilder.append(array[i]).append(delimiter);
            }
            return stringBuilder.substring(0, stringBuilder.length() - 1);
        }

        public static String join(final short[] array, final char delimiter, final int startIndex, final int endIndex) {
            if (array == null) {
                return null;
            }
            if (endIndex - startIndex <= 0) {
                return EMPTY;
            }
            final StringBuilder stringBuilder = new StringBuilder();
            for (int i = startIndex; i < endIndex; i++) {
                stringBuilder.append(array[i]).append(delimiter);
            }
            return stringBuilder.substring(0, stringBuilder.length() - 1);
        }
    }

    public static class StringUtils_3_21_0 {

        public static final String EMPTY = "";

        private static StringBuilder capacity(final int count, final byte maxElementChars) {
            return new StringBuilder(count * maxElementChars + count - 1);
        }

        public static String join(final boolean[] array, final char delimiter, final int startIndex, final int endIndex) {
            if (array == null) {
                return null;
            }
            final int count = endIndex - startIndex;
            if (count <= 0) {
                return EMPTY;
            }
            final byte maxElementChars = 5; // "false".length()
            final StringBuilder stringBuilder = capacity(count, maxElementChars);
            stringBuilder.append(array[startIndex]);
            for (int i = startIndex + 1; i < endIndex; i++) {
                stringBuilder.append(delimiter).append(array[i]);
            }
            return stringBuilder.toString();
        }

        public static String join(final byte[] array, final char delimiter, final int startIndex, final int endIndex) {
            if (array == null) {
                return null;
            }
            final int count = endIndex - startIndex;
            if (count <= 0) {
                return EMPTY;
            }
            final byte maxElementChars = 4; // "-128"
            final StringBuilder stringBuilder = capacity(count, maxElementChars);
            stringBuilder.append(array[startIndex]);
            for (int i = startIndex + 1; i < endIndex; i++) {
                stringBuilder.append(delimiter).append(array[i]);
            }
            return stringBuilder.toString();
        }

        public static String join(final char[] array, final char delimiter, final int startIndex, final int endIndex) {
            if (array == null) {
                return null;
            }
            final int count = endIndex - startIndex;
            if (count <= 0) {
                return EMPTY;
            }
            final byte maxElementChars = 1;
            final StringBuilder stringBuilder = capacity(count, maxElementChars);
            stringBuilder.append(array[startIndex]);
            for (int i = startIndex + 1; i < endIndex; i++) {
                stringBuilder.append(delimiter).append(array[i]);
            }
            return stringBuilder.toString();
        }

        public static String join(final double[] array, final char delimiter, final int startIndex, final int endIndex) {
            if (array == null) {
                return null;
            }
            final int count = endIndex - startIndex;
            if (count <= 0) {
                return EMPTY;
            }
            final byte maxElementChars = 22; // "1.7976931348623157E308"
            final StringBuilder stringBuilder = capacity(count, maxElementChars);
            stringBuilder.append(array[startIndex]);
            for (int i = startIndex + 1; i < endIndex; i++) {
                stringBuilder.append(delimiter).append(array[i]);
            }
            return stringBuilder.toString();
        }

        public static String join(final float[] array, final char delimiter, final int startIndex, final int endIndex) {
            if (array == null) {
                return null;
            }
            final int count = endIndex - startIndex;
            if (count <= 0) {
                return EMPTY;
            }
            final byte maxElementChars = 12; // "3.4028235E38"
            final StringBuilder stringBuilder = capacity(count, maxElementChars);
            stringBuilder.append(array[startIndex]);
            for (int i = startIndex + 1; i < endIndex; i++) {
                stringBuilder.append(delimiter).append(array[i]);
            }
            return stringBuilder.toString();
        }

        public static String join(final int[] array, final char delimiter, final int startIndex, final int endIndex) {
            if (array == null) {
                return null;
            }
            final int count = endIndex - startIndex;
            if (count <= 0) {
                return EMPTY;
            }
            final byte maxElementChars = 11; // "-2147483648"
            final StringBuilder stringBuilder = capacity(count, maxElementChars);
            stringBuilder.append(array[startIndex]);
            for (int i = startIndex + 1; i < endIndex; i++) {
                stringBuilder.append(delimiter).append(array[i]);
            }
            return stringBuilder.toString();
        }

        public static String join(final long[] array, final char delimiter, final int startIndex, final int endIndex) {
            if (array == null) {
                return null;
            }
            final int count = endIndex - startIndex;
            if (count <= 0) {
                return EMPTY;
            }
            final byte maxElementChars = 20; // "-9223372036854775808"
            final StringBuilder stringBuilder = capacity(count, maxElementChars);
            stringBuilder.append(array[startIndex]);
            for (int i = startIndex + 1; i < endIndex; i++) {
                stringBuilder.append(delimiter).append(array[i]);
            }
            return stringBuilder.toString();
        }

        public static String join(final short[] array, final char delimiter, final int startIndex, final int endIndex) {
            if (array == null) {
                return null;
            }
            final int count = endIndex - startIndex;
            if (count <= 0) {
                return EMPTY;
            }
            final byte maxElementChars = 6; // "-32768"
            final StringBuilder stringBuilder = capacity(count, maxElementChars);
            stringBuilder.append(array[startIndex]);
            for (int i = startIndex + 1; i < endIndex; i++) {
                stringBuilder.append(delimiter).append(array[i]);
            }
            return stringBuilder.toString();
        }
    }

    private static final int WARMUP_ITERATIONS = 3;

    private static final int WARMUP_TIME = 1;

    private static final int MEASUREMENT_ITERATIONS = 5;

    private static final int MEASUREMENT_TIME = 1;

    private static Random newRandom() {
        return new Random(235);
    }
}