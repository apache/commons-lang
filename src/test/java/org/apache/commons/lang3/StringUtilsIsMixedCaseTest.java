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
import org.openjdk.jmh.annotations.Warmup;

/**
 * Test to show the better performance of the new implementation of isMixedCase
 */
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
@Warmup(iterations = 5, time = 10)
public class StringUtilsIsMixedCaseTest {

    public static final String LOWER_CASE_LETTERS = "abcdefghijklmnopqrstuvwxyz";
    public static final String END_MATCH = "at the enD";
    public static final String Middle_MATCH = "at tHe Mid";
    public static final String EARLY_MATCH = "At tHe beginning";

    public static boolean oldIsMixedCase(final CharSequence cs) {
        if (StringUtils.isEmpty(cs) || cs.length() == 1) {
            return false;
        }
        boolean containsUppercase = false;
        boolean containsLowercase = false;
        final int sz = cs.length();
        for (int i = 0; i < sz; i++) {
            if (containsUppercase && containsLowercase) {
                return true;
            }
            if (Character.isUpperCase(cs.charAt(i))) {
                containsUppercase = true;
            } else if (Character.isLowerCase(cs.charAt(i))) {
                containsLowercase = true;
            }
        }
        return containsUppercase && containsLowercase;
    }

    @Benchmark
    public boolean newIsMixedCaseBeginningMatch() {
        return StringUtils.isMixedCase(EARLY_MATCH);
    }

    @Benchmark
    public boolean newIsMixedCaseEndMatch() {
        return StringUtils.isMixedCase(END_MATCH);
    }

    @Benchmark
    public boolean newIsMixedCaseMiddleMatch() {
        return StringUtils.isMixedCase(Middle_MATCH);
    }

    @Benchmark
    public boolean newIsMixedCaseNoneMatch() {
        return StringUtils.isMixedCase(LOWER_CASE_LETTERS);
    }

    @Benchmark
    public boolean oldIsMixedCaseBeginningMatch() {
        return oldIsMixedCase(EARLY_MATCH);
    }

    @Benchmark
    public boolean oldIsMixedCaseEndMatch() {
        return oldIsMixedCase(END_MATCH);
    }

    @Benchmark
    public boolean oldIsMixedCaseMiddleMatch() {
        return oldIsMixedCase(Middle_MATCH);
    }

    @Benchmark
    public boolean oldIsMixedCaseNoneMatch() {
        return oldIsMixedCase(LOWER_CASE_LETTERS);
    }
}
