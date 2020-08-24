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
package org.apache.commons.lang3.math;

import java.util.Random;
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

@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
@Warmup(iterations = 5, time = 10, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgs = {"-server", "-Xms2048M", "-Xms2048M"})
public class FractionPowPerformanceTest {
    private Random random = new Random(0);
    private int[] a = buildInts(1000, 8);
    private int[] b = buildInts(1000, 8);
    private int[] c = buildInts(1000, 8);

    private int[] buildInts(int length, int bound) {
        int[] res = new int[length];
        for (int i = 0; i < length; i++) {
            res[i] = random.nextInt(bound);
        }
        return res;
    }

    @Benchmark
    public Fraction[] testNew() {
        final int length = a.length;
        Fraction[] res = new Fraction[length];
        for (int i = 0; i < length; i++) {
            res[i] = Fraction.getFraction(a[i], b[i]+1).pow(c[i]);
        }
        return res;
    }

    @Benchmark
    public Fraction[] testOld() {
        final int length = a.length;
        Fraction[] res = new Fraction[length];
        for (int i = 0; i < length; i++) {
            res[i] = Fraction.getFraction(a[i], b[i]+1).powOld(c[i]);
        }
        return res;
    }
}
