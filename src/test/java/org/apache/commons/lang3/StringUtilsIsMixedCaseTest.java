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
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.Warmup;
import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.apache.commons.lang3.StringUtils.length;

@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgs = {"-server", "-Xms512M", "-Xmx512M"})
public class StringUtilsIsMixedCaseTest {
    static final String string0 = StringUtils.repeat(' ', 100);
    static final String string1 = StringUtils.repeat('a', 100);
    static final String string2 = StringUtils.repeat('A', 100);
    static final String string3 = string1 + string2;

    @Benchmark
    public boolean test0Old() {
        return isMixedCaseOld(string0);
    }

    @Benchmark
    public boolean test0New() {
        return isMixedCaseNew(string0);
    }

    @Benchmark
    public boolean test0New2() {
        return isMixedCaseNew2(string0);
    }

    @Benchmark
    public boolean test1Old() {
        return isMixedCaseOld(string1);
    }

    @Benchmark
    public boolean test1New() {
        return isMixedCaseNew(string1);
    }

    @Benchmark
    public boolean test1New2() {
        return isMixedCaseNew2(string1);
    }

    @Benchmark
    public boolean test2Old() {
        return isMixedCaseOld(string2);
    }

    @Benchmark
    public boolean test2New() {
        return isMixedCaseNew(string2);
    }

    @Benchmark
    public boolean test2New2() {
        return isMixedCaseNew2(string2);
    }

    @Benchmark
    public boolean test3Old() {
        return isMixedCaseOld(string3);
    }

    @Benchmark
    public boolean test3New() {
        return isMixedCaseNew(string3);
    }

    @Benchmark
    public boolean test3New2() {
        return isMixedCaseNew2(string3);
    }

    //-----

    public static boolean isMixedCaseOld(final CharSequence cs) {
        if (isEmpty(cs) || cs.length() == 1) {
            return false;
        }
        boolean containsUppercase = false;
        boolean containsLowercase = false;
        final int sz = cs.length();
        for (int i = 0; i < sz; i++) {
            if (containsUppercase && containsLowercase) {
                return true;
            } else if (Character.isUpperCase(cs.charAt(i))) {
                containsUppercase = true;
            } else if (Character.isLowerCase(cs.charAt(i))) {
                containsLowercase = true;
            }
        }
        return containsUppercase && containsLowercase;
    }

    public static boolean isMixedCaseNew(final CharSequence cs) {
        final int sz = length(cs);
        if (sz <= 1) {
            return false;
        }
        boolean containsUppercase = false;
        int i = 0;
        while (i < sz) {
            final char nowChar = cs.charAt(i);
            ++i;
            if (Character.isUpperCase(nowChar)) {
                containsUppercase = true;
                break;
            } else if (Character.isLowerCase(nowChar)) {
                break;
            }
        }
        if (i == sz) {
            return false;
        }
        if (containsUppercase) {
            for (; i < sz; ++i) {
                if (Character.isLowerCase(cs.charAt(i))) {
                    return true;
                }
            }
            return false;
        }
        for (; i < sz; ++i) {
            if (Character.isUpperCase(cs.charAt(i))) {
                return true;
            }
        }
        return false;
    }

    public static boolean isMixedCaseNew2(final CharSequence cs) {
        final int sz = length(cs);
        if (sz <= 1) {
            return false;
        }
        boolean containsUppercase = false;
        boolean containsLowercase = false;
        for (int i = 0; i < sz; ++i) {
            final char nowChar = cs.charAt(i);
            if (containsUppercase || (containsUppercase = Character.isUpperCase(nowChar))) {
                if (containsLowercase) {
                    return true;
                }
            } else if (containsLowercase || (containsLowercase = Character.isLowerCase(nowChar))) {
                if (containsUppercase) {
                    return true;
                }
            }
        }
        return false;
    }
}
