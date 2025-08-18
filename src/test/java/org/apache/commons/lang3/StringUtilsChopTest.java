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

import java.util.concurrent.TimeUnit;

import static org.apache.commons.lang3.StringUtils.EMPTY;

/**
 * Test to show whether using BitSet for removeAll() methods is faster than using HashSet.
 */
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgs = {"-server", "-Xms512M", "-Xmx512M"})
public class StringUtilsChopTest {
    static final String string1 = "aaa ";
    static final String string2 = "aaa\r\n";
    static final String string3 = "a";
    static final String string4 = "aaa";

    @Benchmark
    public String test1Old() {
        return chopOld(string1);
    }

    @Benchmark
    public String test2Old() {
        return chopOld(string2);
    }

    @Benchmark
    public String test3Old() {
        return chopOld(string3);
    }

    @Benchmark
    public String test4Old() {
        return chopOld(string4);
    }

    @Benchmark
    public String test1New() {
        return chopNew(string1);
    }

    @Benchmark
    public String test2New() {
        return chopNew(string2);
    }

    @Benchmark
    public String test3New() {
        return chopNew(string3);
    }

    @Benchmark
    public String test4New() {
        return chopNew(string4);
    }

    @Benchmark
    public String test1New2() {
        return chopNew2(string1);
    }

    @Benchmark
    public String test2New2() {
        return chopNew2(string2);
    }

    @Benchmark
    public String test3New2() {
        return chopNew2(string3);
    }

    @Benchmark
    public String test4New2() {
        return chopNew2(string4);
    }

    //-----

    public static String chopOld(final String str) {
        if (str == null) {
            return null;
        }
        final int strLen = str.length();
        if (strLen < 2) {
            return EMPTY;
        }
        final int lastIdx = strLen - 1;
        final String ret = str.substring(0, lastIdx);
        final char last = str.charAt(lastIdx);
        if (last == CharUtils.LF && ret.charAt(lastIdx - 1) == CharUtils.CR) {
            return ret.substring(0, lastIdx - 1);
        }
        return ret;
    }

    public static String chopNew(final String str) {
        if (str == null) {
            return null;
        }
        final int strLen = str.length();
        if (strLen < 2) {
            return EMPTY;
        }
        int lastIdx = strLen - 1;
        if (str.charAt(lastIdx) == CharUtils.LF && str.charAt(lastIdx - 1) == CharUtils.CR) {
            --lastIdx;
        }
        return str.substring(0, lastIdx);
    }

    public static String chopNew2(final String str) {
        if (str == null) {
            return null;
        }
        final int strLen = str.length();
        if (strLen < 2) {
            return EMPTY;
        }
        final int lastIdx = strLen - 1;
        if (str.charAt(lastIdx) != CharUtils.LF || str.charAt(lastIdx - 1) != CharUtils.CR) {
            return str.substring(0, lastIdx);
        }
        return str.substring(0, lastIdx - 1);
    }
}
