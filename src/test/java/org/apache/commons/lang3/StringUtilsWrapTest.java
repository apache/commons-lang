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

import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.Warmup;
import org.openjdk.jmh.infra.Blackhole;

import java.util.concurrent.TimeUnit;

import static org.apache.commons.lang3.StringUtils.isEmpty;

/**
 * Test to show whether using BitSet for removeAll() methods is faster than using HashSet.
 */
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgs = {"-server", "-Xms512M", "-Xmx512M"})
public class StringUtilsWrapTest {
    static String[] strings = buildStrings();

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
    public String test00Old() {
        return wrapOld("a", "b");
    }

    @Benchmark
    public String test00New() {
        return wrapNew("a", "b");
    }

    @Benchmark
    public String test01Old() {
        return wrapOld("aaaaaaaaaa", "b");
    }

    @Benchmark
    public String test01New() {
        return wrapNew("aaaaaaaaaa", "b");
    }

    @Benchmark
    public String test10Old() {
        return wrapIfMissingOld("aaaaaaaaaa", "b");
    }

    @Benchmark
    public String test10New() {
        return wrapIfMissingNew("aaaaaaaaaa", "b");
    }

    @Benchmark
    public String test11Old() {
        return wrapIfMissingOld("ab", "b");
    }

    @Benchmark
    public String test11New() {
        return wrapIfMissingNew("ab", "b");
    }

    @Benchmark
    public String test12ld() {
        return wrapIfMissingOld("bb", "b");
    }

    @Benchmark
    public String test12New() {
        return wrapIfMissingNew("bb", "b");
    }

    @Benchmark
    public String test20Old() {
        return wrapIfMissingOld("aaaaaaaaaa", 'b');
    }

    @Benchmark
    public String test20New() {
        return wrapIfMissingNew("aaaaaaaaaa", 'b');
    }

    @Benchmark
    public String test21Old() {
        return wrapIfMissingOld("ab", 'b');
    }

    @Benchmark
    public String test21New() {
        return wrapIfMissingNew("ab", 'b');
    }

    @Benchmark
    public String test22Old() {
        return wrapIfMissingOld("bb", 'b');
    }

    @Benchmark
    public String test22New() {
        return wrapIfMissingNew("bb", 'b');
    }

    @Benchmark
    public void testsOld(Blackhole blackhole) {
        for (int i = 0; i < 128; i++) {
            for (String au : strings) {
                blackhole.consume(wrapIfMissingOld(au, (char) i));
            }
        }
    }

    @Benchmark
    public void testsNew(Blackhole blackhole) {
        for (int i = 0; i < 128; i++) {
            for (String au : strings) {
                blackhole.consume(wrapIfMissingNew(au, (char) i));
            }
        }
    }

    //-----

    public static String wrapOld(final String str, final String wrapWith) {

        if (isEmpty(str) || isEmpty(wrapWith)) {
            return str;
        }

        return wrapWith.concat(str).concat(wrapWith);
    }

    public static String wrapNew(final String str, final String wrapWith) {

        if (isEmpty(str) || isEmpty(wrapWith)) {
            return str;
        }

        return wrapWith + str + wrapWith;
    }

    public static String wrapIfMissingOld(final String str, final char wrapWith) {
        if (isEmpty(str) || wrapWith == CharUtils.NUL) {
            return str;
        }
        final boolean wrapStart = str.charAt(0) != wrapWith;
        final boolean wrapEnd = str.charAt(str.length() - 1) != wrapWith;
        if (!wrapStart && !wrapEnd) {
            return str;
        }

        final StringBuilder builder = new StringBuilder(str.length() + 2);
        if (wrapStart) {
            builder.append(wrapWith);
        }
        builder.append(str);
        if (wrapEnd) {
            builder.append(wrapWith);
        }
        return builder.toString();
    }

    public static String wrapIfMissingNew(final String str, final char wrapWith) {
        if (isEmpty(str) || wrapWith == CharUtils.NUL) {
            return str;
        }
        final boolean wrapStart = str.charAt(0) != wrapWith;
        final boolean wrapEnd = str.charAt(str.length() - 1) != wrapWith;
        if (wrapStart) {
            if (wrapEnd) {
                return wrapWith + str + wrapWith;
            } else {
                return wrapWith + str;
            }
        } else {
            if (wrapEnd) {
                return str + wrapWith;
            } else {
                return str;
            }
        }
    }

    public static String wrapIfMissingOld(final String str, final String wrapWith) {
        if (isEmpty(str) || isEmpty(wrapWith)) {
            return str;
        }

        final boolean wrapStart = !str.startsWith(wrapWith);
        final boolean wrapEnd = !str.endsWith(wrapWith);
        if (!wrapStart && !wrapEnd) {
            return str;
        }

        final StringBuilder builder = new StringBuilder(str.length() + wrapWith.length() + wrapWith.length());
        if (wrapStart) {
            builder.append(wrapWith);
        }
        builder.append(str);
        if (wrapEnd) {
            builder.append(wrapWith);
        }
        return builder.toString();
    }

    public static String wrapIfMissingNew(final String str, final String wrapWith) {
        if (isEmpty(str) || isEmpty(wrapWith)) {
            return str;
        }

        final boolean wrapStart = !str.startsWith(wrapWith);
        final boolean wrapEnd = !str.endsWith(wrapWith);
        if (wrapStart) {
            if (wrapEnd) {
                return wrapWith + str + wrapWith;
            } else {
                return wrapWith + str;
            }
        } else {
            if (wrapEnd) {
                return str + wrapWith;
            } else {
                return str;
            }
        }
    }
}
