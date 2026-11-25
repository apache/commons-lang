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
 * Benchmarks {@link StringUtils#replaceEach(String, String[], String[])} with different match distributions.
 * Grouped inputs place frequent ampersands before the other matching characters; interleaved inputs mix all five characters.
 * Sparse, absent, and single-pattern cases measure paths that do not repeatedly search for distant matches.
 *
 * <p>
 * Run the same benchmark in baseline and candidate checkouts with the same JDK and JVM options. For example, in a POSIX shell:
 * </p>
 * <pre>
 * mvn -B -ntp -Pbenchmark clean test-compile dependency:build-classpath -Dmdep.outputFile=target/benchmark-classpath.txt
 * java -cp "target/test-classes:target/classes:$(cat target/benchmark-classpath.txt)" \
 *   org.openjdk.jmh.Main StringUtilsReplaceEachBenchmark -prof gc -jvmArgs "-Xms256m -Xmx256m" \
 *   -rf json -rff /tmp/replace-each-before.json
 * </pre>
 * <p>
 * Use a separate result file for the candidate. Compare both time and bytes allocated per operation, including the unaffected cases.
 * </p>
 *
 * @since 3.22.1
 */
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
@Fork(2)
@Warmup(iterations = 3, time = 1)
@Measurement(iterations = 5, time = 1)
public class StringUtilsReplaceEachBenchmark {

    /**
     * The distribution of matching characters in the input.
     *
     * @since 3.22.1
     */
    @Param({ "GROUPED", "UTF16_GROUPED", "INTERLEAVED", "SPARSE", "NO_MATCH", "SINGLE_PATTERN" })
    public String distribution;

    /**
     * The number of UTF-16 code units in the input.
     *
     * @since 3.22.1
     */
    @Param({ "64", "4096", "65536" })
    public int length;

    private String[] replacements;
    private String[] searches;
    private String text;

    /**
     * Constructs a benchmark state.
     *
     * @since 3.22.1
     */
    public StringUtilsReplaceEachBenchmark() {
        // Empty.
    }

    /**
     * Replaces the prepared input, including all per-call search bookkeeping and output allocation.
     *
     * @return the replaced text.
     * @since 3.22.1
     */
    @Benchmark
    public String replaceEach() {
        return StringUtils.replaceEach(text, searches, replacements);
    }

    /**
     * Prepares the input and replacement rules outside the measured operation.
     *
     * @since 3.22.1
     */
    @Setup
    public void setup() {
        searches = new String[] { "&", "<", ">", "\"", "'" };
        replacements = new String[] { "&amp;", "&lt;", "&gt;", "&quot;", "&apos;" };
        final String unit;
        switch (distribution) {
        case "GROUPED":
        case "SINGLE_PATTERN":
            unit = "word &";
            break;
        case "UTF16_GROUPED":
            unit = "\u4e2d\u6587 &";
            break;
        case "INTERLEAVED":
            unit = "word &<>\"'";
            break;
        default:
            unit = "word ";
            break;
        }
        final StringBuilder builder = new StringBuilder(length + unit.length());
        while (builder.length() < length) {
            builder.append(unit);
        }
        builder.setLength(length);
        if (distribution.endsWith("GROUPED")) {
            builder.replace(length - 4, length, "<>\"'");
        } else if ("SPARSE".equals(distribution)) {
            builder.setCharAt(length - 1, '&');
        } else if ("SINGLE_PATTERN".equals(distribution)) {
            searches = new String[] { "&" };
            replacements = new String[] { "&amp;" };
        }
        text = builder.toString();
    }
}
