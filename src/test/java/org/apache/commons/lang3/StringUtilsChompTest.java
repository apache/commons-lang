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
import java.util.function.UnaryOperator;
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
import org.openjdk.jmh.infra.Blackhole;
import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.isEmpty;

@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
@Warmup(iterations = 5, time = 10, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgs = {"-server", "-Xms2048M", "-Xms2048M"})
public class StringUtilsChompTest {
    /** Define the strings to chomp. */
    public enum ChompString {
        NULL(null),
        CHAR0(""),
        CHAR0_CR("\r"),
        CHAR0_LF("\n"),
        CHAR0_CR_LF("\r\n"),
        CHAR1("a"),
        CHAR1_CR("a\r"),
        CHAR1_LF("a\n"),
        CHAR1_CR_LF("a\r\n"),
        CHAR2("ab"),
        CHAR2_CR("ab\r"),
        CHAR2_LF("ab\n"),
        CHAR2_CR_LF("ab\r\n"),
        CHAR1024(StringUtils.repeat("a", 1024)),
        CHAR1024_CR(StringUtils.repeat("a", 1024) + "\r"),
        CHAR1024_LF(StringUtils.repeat("a", 1024) + "\n"),
        CHAR1024_CR_LF(StringUtils.repeat("a", 1024) + "\r\n"),
        CHAR10240(StringUtils.repeat("a", 10240)),
        CHAR10240_CR(StringUtils.repeat("a", 10240) + "\r"),
        CHAR10240_LF(StringUtils.repeat("a", 10240) + "\n"),
        CHAR10240_CR_LF(StringUtils.repeat("a", 10240) + "\r\n"),
        CHAR102400(StringUtils.repeat("a", 102400)),
        CHAR102400_CR(StringUtils.repeat("a", 102400) + "\r"),
        CHAR102400_LF(StringUtils.repeat("a", 102400) + "\n"),
        CHAR102400_CR_LF(StringUtils.repeat("a", 102400) + "\r\n");

        /** The string data. */
        final String string;

        /**
         * Create an instance.
         *
         * @param string the string data
         */
        ChompString(String string) {
            this.string = string;
        }
    }

    /** The benchmark data to chomp. */
    @State(Scope.Benchmark)
    public static class ChompData {
        /** The data to chomp. */
        @Param
        private ChompString data;

        /**
         * Gets the data.
         *
         * @return the data
         */
        public String getData() {
            return data.string;
        }
    }

    /** The chomp method to benchmark. */
    @State(Scope.Benchmark)
    public static class ChompMethod {
        /** The method name. */
        @Param({"old", "new"})
        private String name;

        /** The method. */
        private UnaryOperator<String> method;

        /**
         * Gets the method.
         *
         * @return the method
         */
        public UnaryOperator<String> getMethod() {
            return method;
        }

        /** Setup the chomp method. */
        @Setup
        public void setup() {
            if ("old".equals(name)) {
                method = StringUtilsChompTest::chompOld;
            } else {
                method = StringUtils::chomp;
            }
        }
    }

    /**
     * Benchmark a single chomp of a string.
     *
     * @param data the data
     * @param method the method
     * @return the chomped string
     */
    @Benchmark
    public String singleString(ChompData data, ChompMethod method) {
        return method.getMethod().apply(data.getData());
    }

    private static final String[] strings = buildStrings();

    private static String[] buildStrings() {
        String[] res = new String[128 * 128];
        for (int i = 0; i < 128; i++) {
            for (int j = 0; j < 128; j++) {
                StringBuilder stringBuilder = new StringBuilder();
                stringBuilder.append((char) i);
                stringBuilder.append((char) j);
                res[i * 128 + j] = stringBuilder.toString();
            }
        }
        return res;
    }

    @Benchmark
    public void test_Random_Strings_Old(Blackhole blackhole) {
        for (String au : strings) {
            blackhole.consume(chompOld(au));
        }
    }

    @Benchmark
    public void test_Random_Strings_New(Blackhole blackhole) {
        for (String au : strings) {
            blackhole.consume(StringUtils.chomp(au));
        }
    }

    //-----

    public static String chompOld(final String str) {
        if (isEmpty(str)) {
            return str;
        }

        if (str.length() == 1) {
            final char ch = str.charAt(0);
            if (ch == CharUtils.CR || ch == CharUtils.LF) {
                return EMPTY;
            }
            return str;
        }

        int lastIdx = str.length() - 1;
        final char last = str.charAt(lastIdx);

        if (last == CharUtils.LF) {
            if (str.charAt(lastIdx - 1) == CharUtils.CR) {
                lastIdx--;
            }
        } else if (last != CharUtils.CR) {
            lastIdx++;
        }
        return str.substring(0, lastIdx);
    }
}
