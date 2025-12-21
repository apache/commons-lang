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

import java.util.concurrent.TimeUnit;

import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Level;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Param;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.Warmup;

/**
 * Benchmark comparing the old and new implementations of CharSequenceUtils methods.
 *
 * <p>
 * Run with:
 * </p>
 *
 * <pre>
 * mvn -P benchmark clean test -Dbenchmark=org.apache.commons.lang3.CharSequenceUtilsBenchmark
 * </pre>
 * <p>
 * Results:
 * </p>
 *
 * <pre>
Benchmark                                               (charSequenceType)  (length)  Mode  Cnt     Score    Error  Units
CharSequenceUtilsBenchmark.benchmarkToCharArrayCurrent              String        10  avgt    5     1.626 ±  0.011  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayCurrent              String        50  avgt    5     2.741 ±  0.029  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayCurrent              String       100  avgt    5     4.235 ±  0.038  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayCurrent              String       500  avgt    5    17.713 ±  0.273  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayCurrent              String      1000  avgt    5    34.692 ±  1.752  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayCurrent       StringBuilder        10  avgt    5     1.963 ±  0.047  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayCurrent       StringBuilder        50  avgt    5     4.085 ±  0.042  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayCurrent       StringBuilder       100  avgt    5     5.978 ±  0.177  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayCurrent       StringBuilder       500  avgt    5    25.616 ±  1.621  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayCurrent       StringBuilder      1000  avgt    5    53.749 ±  0.420  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayCurrent        StringBuffer        10  avgt    5     7.239 ±  0.149  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayCurrent        StringBuffer        50  avgt    5     9.061 ±  0.187  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayCurrent        StringBuffer       100  avgt    5    10.281 ±  0.055  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayCurrent        StringBuffer       500  avgt    5    29.647 ±  0.420  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayCurrent        StringBuffer      1000  avgt    5    56.203 ±  0.505  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayNew                  String        10  avgt    5     1.657 ±  0.030  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayNew                  String        50  avgt    5     2.771 ±  0.094  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayNew                  String       100  avgt    5     4.281 ±  0.036  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayNew                  String       500  avgt    5    17.744 ±  0.091  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayNew                  String      1000  avgt    5    34.224 ±  0.251  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayNew           StringBuilder        10  avgt    5     1.962 ±  0.128  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayNew           StringBuilder        50  avgt    5     4.101 ±  0.035  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayNew           StringBuilder       100  avgt    5     5.984 ±  0.062  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayNew           StringBuilder       500  avgt    5    25.448 ±  0.152  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayNew           StringBuilder      1000  avgt    5    54.531 ±  0.559  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayNew            StringBuffer        10  avgt    5     7.260 ±  0.175  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayNew            StringBuffer        50  avgt    5     8.537 ±  0.101  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayNew            StringBuffer       100  avgt    5    10.502 ±  0.143  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayNew            StringBuffer       500  avgt    5    29.584 ±  0.339  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayNew            StringBuffer      1000  avgt    5    56.751 ±  0.983  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayOld                  String        10  avgt    5     1.656 ±  0.231  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayOld                  String        50  avgt    5     2.770 ±  0.222  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayOld                  String       100  avgt    5     4.298 ±  0.198  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayOld                  String       500  avgt    5    18.023 ±  0.203  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayOld                  String      1000  avgt    5    35.053 ±  1.467  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayOld           StringBuilder        10  avgt    5     3.164 ±  0.062  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayOld           StringBuilder        50  avgt    5     8.907 ±  0.185  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayOld           StringBuilder       100  avgt    5    15.801 ±  0.104  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayOld           StringBuilder       500  avgt    5    77.203 ±  0.460  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayOld           StringBuilder      1000  avgt    5   164.064 ±  2.506  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayOld            StringBuffer        10  avgt    5    28.981 ±  0.307  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayOld            StringBuffer        50  avgt    5   126.285 ±  1.688  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayOld            StringBuffer       100  avgt    5   250.584 ±  5.639  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayOld            StringBuffer       500  avgt    5  1231.478 ± 51.296  ns/op
CharSequenceUtilsBenchmark.benchmarkToCharArrayOld            StringBuffer      1000  avgt    5  2453.553 ± 54.004  ns/op
 * </pre>
 *
 */
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
@Fork(1)
@Warmup(iterations = 3, time = 1)
@Measurement(iterations = 5, time = 1)
public class CharSequenceUtilsBenchmark {

    /**
     * New optimized implementation of toCharArray.
     */
    public static char[] toCharArrayNew(final CharSequence source) {
        final int len = StringUtils.length(source);
        if (len == 0) {
            return ArrayUtils.EMPTY_CHAR_ARRAY;
        }
        if (source instanceof String) {
            return ((String) source).toCharArray();
        }
        // NEW: Uses bulk getChars() for StringBuilder/StringBuffer
        if (source instanceof StringBuilder) {
            final char[] array = new char[len];
            ((StringBuilder) source).getChars(0, len, array, 0);
            return array;
        }
        if (source instanceof StringBuffer) {
            final char[] array = new char[len];
            ((StringBuffer) source).getChars(0, len, array, 0);
            return array;
        }
        final char[] array = new char[len];
        for (int i = 0; i < len; i++) {
            array[i] = source.charAt(i);
        }
        return array;
    }

    /**
     * Old implementation of toCharArray.
     */
    public static char[] toCharArrayOld(final CharSequence source) {
        final int len = StringUtils.length(source);
        if (len == 0) {
            return ArrayUtils.EMPTY_CHAR_ARRAY;
        }
        if (source instanceof String) {
            return ((String) source).toCharArray();
        }
        // OLD: Always uses charAt() loop, even for StringBuilder/StringBuffer
        final char[] array = new char[len];
        for (int i = 0; i < len; i++) {
            array[i] = source.charAt(i);
        }
        return array;
    }

    @Param({ "10", "50", "100", "500", "1000" })
    public int length;
    @Param({ "String", "StringBuilder", "StringBuffer" })
    public String charSequenceType;
    private CharSequence testSequence;

    @Benchmark
    public char[] benchmarkToCharArrayCurrent() {
        return CharSequenceUtils.toCharArray(testSequence);
    }
    @Benchmark
    public char[] benchmarkToCharArrayNew() {
        return toCharArrayNew(testSequence);
    }

    @Benchmark
    public char[] benchmarkToCharArrayOld() {
        return toCharArrayOld(testSequence);
    }

    @Setup(Level.Trial)
    public void setup() {
        final StringBuilder sb = new StringBuilder(length);
        for (int i = 0; i < length; i++) {
            sb.append((char) ('a' + i % 26));
        }
        final String content = sb.toString();
        switch (charSequenceType) {
        case "String":
            testSequence = content;
            break;
        case "StringBuilder":
            testSequence = new StringBuilder(content);
            break;
        case "StringBuffer":
            testSequence = new StringBuffer(content);
            break;
        }
    }
}
