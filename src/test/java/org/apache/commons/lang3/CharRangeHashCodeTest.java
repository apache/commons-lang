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

import java.util.Objects;

import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Level;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.Warmup;
import org.openjdk.jmh.infra.Blackhole;

/**
 * Test to show whether using Bitwise for hashCode() methods is faster than using Objects.hash.
 */
@State(Scope.Thread)
@BenchmarkMode(Mode.Throughput)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(0)
public class CharRangeHashCodeTest {
    private CharRange normalRange;
    private CharRange negatedRange;
    private CharRange singleCharRange;
    private CharRange extremeRange;

    @Setup(Level.Trial)
    public void initTestData() {
        normalRange = CharRange.isIn('a', 'z');
        negatedRange = CharRange.isNotIn('0', '9');
        singleCharRange = CharRange.isIn('x', 'x');
        extremeRange = CharRange.isIn(Character.MAX_VALUE, (char) (Character.MAX_VALUE - 100));
    }

    private int hashCodeWithObjectsHash(CharRange range) {
        return Objects.hash(range.getEnd(), range.isNegated(), range.getStart());
    }

    private int hashCodeWithBitwise(CharRange range) {
        final int charCombined = (range.getStart() << 16) | (range.getEnd() & 0xFFFF);
        return charCombined ^ (range.isNegated() ? 0x80000000 : 0);
    }

    @Benchmark
    public void hashCode_ObjectsHash(Blackhole blackhole) {
        blackhole.consume(hashCodeWithObjectsHash(normalRange));
        blackhole.consume(hashCodeWithObjectsHash(negatedRange));
        blackhole.consume(hashCodeWithObjectsHash(singleCharRange));
        blackhole.consume(hashCodeWithObjectsHash(extremeRange));
    }

    @Benchmark
    public void hashCode_Bitwise(Blackhole blackhole) {
        blackhole.consume(hashCodeWithBitwise(normalRange));
        blackhole.consume(hashCodeWithBitwise(negatedRange));
        blackhole.consume(hashCodeWithBitwise(singleCharRange));
        blackhole.consume(hashCodeWithBitwise(extremeRange));
    }
}