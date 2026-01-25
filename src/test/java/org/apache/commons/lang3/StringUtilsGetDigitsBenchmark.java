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

import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.TimeUnit;

import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Level;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Param;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.State;

@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
public class StringUtilsGetDigitsBenchmark {

    public static String getDigitsManually(final String str) {
        if (StringUtils.isEmpty(str)) {
            return str;
        }
        final int len = str.length();
        final char[] buffer = new char[len];
        int count = 0;
        for (int i = 0; i < len; i++) {
            final char tempChar = str.charAt(i);
            if (Character.isDigit(tempChar)) {
                buffer[count++] = tempChar;
            }
        }
        return new String(buffer, 0, count);
    }
    public static String getDigitsWithBuilder(final String str) {
        if (StringUtils.isEmpty(str)) {
            return str;
        }
        final int sz = str.length();
        final StringBuilder strDigits = new StringBuilder(sz);
        for (int i = 0; i < sz; i++) {
            final char tempChar = str.charAt(i);
            if (Character.isDigit(tempChar)) {
                strDigits.append(tempChar);
            }
        }
        return strDigits.toString();
    }

    @Param({ "10", "100", "1000", "10000" })
    public int length;

    private String testStr;

    @Setup(Level.Invocation)
    public void setup() {
        final StringBuilder sb = new StringBuilder(length);
        for (int i = 0; i < length; i++) {
            final int code = ThreadLocalRandom.current().nextInt(33, 127);
            sb.append((char) code);
        }
        testStr = sb.toString();
    }

    @Benchmark
    public String testGetDigitsManually() {
        return getDigitsManually(testStr);
    }

    @Benchmark
    public String testGetDigitsWithBuilder() {
        return getDigitsWithBuilder(testStr);
    }
}
