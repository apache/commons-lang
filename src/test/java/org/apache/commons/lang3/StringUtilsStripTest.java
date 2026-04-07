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

import static org.apache.commons.lang3.StringUtils.INDEX_NOT_FOUND;
import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.apache.commons.lang3.StringUtils.length;
import static org.apache.commons.lang3.StringUtils.stripEnd;
import static org.apache.commons.lang3.StringUtils.stripStart;

/**
 * Test to show whether using BitSet for removeAll() methods is faster than using HashSet.
 */
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgs = {"-server", "-Xms512M", "-Xmx512M"})
public class StringUtilsStripTest {
    static final String string0 = "a";
    static final String string1 = "           a";
    static final String string2 = "a           ";
    static final String string3 = "           a           ";
    static final String string4 = " aaaaaaaaaa";
    static final String string5 = "aaaaaaaaaa ";
    static final String string6 = " aaaaaaaaaa ";
    static final String[] strings = buildStrings();

    private static String[] buildStrings() {
        String[] res = new String[128 * 128 * 128];
        for (int i = 0; i < 128; i++) {
            for (int j = 0; j < 128; j++) {
                for (int k = 0; k < 128; k++) {
                    StringBuilder stringBuilder = new StringBuilder();
                    stringBuilder.append((char) i);
                    stringBuilder.append((char) j);
                    stringBuilder.append((char) k);

                    res[128 * 128 * i + 128 * j + k] = stringBuilder.toString();
                }
            }
        }
        return res;
    }

    @Benchmark
    public String test00Old() {
        return stripOld(string0, " ");
    }

    @Benchmark
    public String test01Old() {
        return stripOld(string0, null);
    }

    @Benchmark
    public String test00New() {
        return stripNew(string0, " ");
    }

    @Benchmark
    public String test01New() {
        return stripNew(string0, null);
    }

    @Benchmark
    public String test10Old() {
        return stripOld(string1, " ");
    }

    @Benchmark
    public String test11Old() {
        return stripOld(string1, null);
    }

    @Benchmark
    public String test10New() {
        return stripNew(string1, " ");
    }

    @Benchmark
    public String test11New() {
        return stripNew(string1, null);
    }

    @Benchmark
    public String test20Old() {
        return stripOld(string2, " ");
    }

    @Benchmark
    public String test21Old() {
        return stripOld(string2, null);
    }

    @Benchmark
    public String test20New() {
        return stripNew(string2, " ");
    }

    @Benchmark
    public String test21New() {
        return stripNew(string2, null);
    }

    @Benchmark
    public String test30Old() {
        return stripOld(string3, " ");
    }

    @Benchmark
    public String test31Old() {
        return stripOld(string3, null);
    }

    @Benchmark
    public String test30New() {
        return stripNew(string3, " ");
    }

    @Benchmark
    public String test31New() {
        return stripNew(string3, null);
    }

    @Benchmark
    public String test40Old() {
        return stripOld(string4, " ");
    }

    @Benchmark
    public String test41Old() {
        return stripOld(string4, null);
    }

    @Benchmark
    public String test40New() {
        return stripNew(string4, " ");
    }

    @Benchmark
    public String test41New() {
        return stripNew(string4, null);
    }

    @Benchmark
    public String test50Old() {
        return stripOld(string5, " ");
    }

    @Benchmark
    public String test51Old() {
        return stripOld(string5, null);
    }

    @Benchmark
    public String test50New() {
        return stripNew(string5, " ");
    }

    @Benchmark
    public String test51New() {
        return stripNew(string5, null);
    }

    @Benchmark
    public String test60Old() {
        return stripOld(string6, " ");
    }

    @Benchmark
    public String test61Old() {
        return stripOld(string6, null);
    }

    @Benchmark
    public String test60New() {
        return stripNew(string6, " ");
    }

    @Benchmark
    public String test61New() {
        return stripNew(string6, null);
    }

    @Benchmark
    public void testStrings0Old(Blackhole blackhole) {
        for (String au : strings) {
            blackhole.consume(stripOld(au, " "));
        }
    }

    @Benchmark
    public void testStrings1Old(Blackhole blackhole) {
        for (String au : strings) {
            blackhole.consume(stripOld(au, null));
        }
    }

    @Benchmark
    public void testStrings0New(Blackhole blackhole) {
        for (String au : strings) {
            blackhole.consume(stripNew(au, " "));
        }
    }

    @Benchmark
    public void testStrings1New(Blackhole blackhole) {
        for (String au : strings) {
            blackhole.consume(stripNew(au, null));
        }
    }

    //-----

    public static String stripNew(String str, final String stripChars) {
        int end = length(str);
        if (end == 0) {
            return str;
        }
        int start = 0;
        if (stripChars == null) {
            while (start != end && Character.isWhitespace(str.charAt(start))) {
                start++;
            }
            while (end != start && Character.isWhitespace(str.charAt(end - 1))) {
                end--;
            }
        } else if (stripChars.isEmpty()) {
            return str;
        } else {
            while (start != end && stripChars.indexOf(str.charAt(start)) != INDEX_NOT_FOUND) {
                start++;
            }
            while (end != start && stripChars.indexOf(str.charAt(end - 1)) != INDEX_NOT_FOUND) {
                end--;
            }
        }
        return str.substring(start, end);
    }

    public static String stripOld(String str, final String stripChars) {
        if (isEmpty(str)) {
            return str;
        }
        str = stripStart(str, stripChars);
        return stripEnd(str, stripChars);
    }
}
