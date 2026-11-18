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

import java.lang.reflect.Array;
import java.util.concurrent.TimeUnit;

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

/**
 * Benchmarks {@link ClassUtils#getShortClassName(String)} with valid JVM class names.
 *
 * <p>
 * Compare a baseline revision from before the optimization with the updated code in separate checkouts. Copy this benchmark source unchanged into both
 * checkouts, including the baseline if it does not contain the benchmark. Use the same JDK, machine, and JVM options, and record both source revisions
 * with the results. Run the checkouts sequentially to avoid interference between measurements.
 * </p>
 *
 * <p>
 * Run these POSIX shell commands from the baseline checkout, then repeat them from the updated checkout with
 * {@code benchmark_result=/tmp/classutils-after.json}. The JSON files are outside {@code target} so that a clean build preserves the comparison results.
 * Ensure {@code java} and Maven use the same JDK. The GC profiler reports allocation per operation as {@code gc.alloc.rate.norm}, in bytes per operation.
 * </p>
 *
 * <pre>
 * benchmark_result=/tmp/classutils-before.json
 * mvn -B -ntp -Pbenchmark clean test-compile dependency:build-classpath -Dmdep.outputFile=target/benchmark-classpath.txt
 * java -cp "target/test-classes:target/classes:$(cat target/benchmark-classpath.txt)" \
 *   org.openjdk.jmh.Main org.apache.commons.lang3.ClassUtilsGetShortClassNameBenchmark \
 *   -prof gc -jvmArgs "-Xms256m -Xmx256m" -rf json -rff "$benchmark_result"
 * </pre>
 *
 * <p>
 * Each parameter combination uses three JVM forks, with five one-second warmup iterations and five one-second measurement iterations per fork.
 * Shortened runs are useful for checking the benchmark, but use the full configuration and inspect measurement uncertainty before reporting timing
 * differences. Compare execution time and allocation for matching parameter combinations; an improvement for deeply nested arrays does not imply
 * an improvement for ordinary class names or shallow arrays.
 * </p>
 */
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
@Fork(3)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
public class ClassUtilsGetShortClassNameBenchmark {

    /**
     * The array depth, or zero for a non-array class.
     */
    @Param({ "0", "1", "2", "32", "255" })
    public int dimensions;

    /**
     * The primitive, reference, or nested class used as the array element type.
     */
    @Param({ "int", "java.lang.String", "java.util.Map$Entry" })
    public String elementType;

    private String className;

    /**
     * Constructs a benchmark state.
     */
    public ClassUtilsGetShortClassNameBenchmark() {
        // Empty.
    }

    /**
     * Gets the short class name of the prepared input.
     *
     * @return the short class name.
     */
    @Benchmark
    public String getShortClassName() {
        return ClassUtils.getShortClassName(className);
    }

    /**
     * Prepares a valid JVM class name outside the measured operation.
     *
     * @throws ClassNotFoundException if the element type cannot be loaded.
     */
    @Setup
    public void setup() throws ClassNotFoundException {
        final Class<?> elementClass = ClassUtils.getClass(elementType);
        className = dimensions == 0 ? elementClass.getName() : Array.newInstance(elementClass, new int[dimensions]).getClass().getName();
    }
}
