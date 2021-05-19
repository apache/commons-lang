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

import java.util.concurrent.TimeUnit;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.State;

@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
public class CharSequenceUtilsIndexOfDoubleLoopPerformanceTest {

    private static final String cs = buildTestString();

    private static final int start = 0;
    private static final String searchChar = "gg";
    private static final int limit = cs.length() - searchChar.length();
    private static final char char0 = searchChar.charAt(0);
    private static final int len2 = searchChar.length();

    private static String buildTestString() {
        StringBuilder stringBuilder = new StringBuilder();
        for (int i = 0; i < 1000; i++) {
            for (char c = 'a'; c <= 'z'; c++) {
                stringBuilder.append(c);
            }
        }
        return stringBuilder.toString();
    }

    private static boolean checkLaterThan1(final CharSequence cs, final CharSequence searchChar, final int len2,
                                           final int start1) {
        for (int i = 1, j = len2 - 1; i <= j; i++, j--) {
            if (cs.charAt(start1 + i) != searchChar.charAt(i)
                    ||
                    cs.charAt(start1 + j) != searchChar.charAt(j)
            ) {
                return false;
            }
        }
        return true;
    }

    @Benchmark
    public int checkWithSingleLoop() {
        for (int i = start; i <= limit; i++) {
            if (cs.charAt(i) == char0 && checkLaterThan1(cs, searchChar, len2, i)) {
                return i;
            }
        }
        return -1;
    }

    @Benchmark
    public int checkWithDoubleLoop() {
        int i = start;
        while (true) {
            while (cs.charAt(i) != char0) {
                i++;
                if (i > limit) {
                    return -1;
                }
            }
            if (checkLaterThan1(cs, searchChar, len2, i)) {
                return i;
            } else {
                i++;
                if (i > limit) {
                    return -1;
                }
            }
        }
    }
}
