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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Verifies that speculative replacement capacity does not exhaust a heap that can hold the actual result.
 */
class StringsReplaceMemoryTest extends AbstractLangTest {

    /**
     * Runs the allocation workload without requiring JUnit in the child JVM.
     */
    public static final class MemoryProbe {

        private static void check(final Strings strings, final String replacement, final String search, final int max) {
            final String result = strings.replace("ab", search, replacement, max);
            if (!result.equals(replacement)) {
                throw new AssertionError("The full-input replacement must equal the replacement string");
            }
        }

        private static void checkMultipleMatches(final Strings strings, final String replacement, final String search, final int max) {
            final String result = strings.replace("ab!ab", search, replacement, max);
            if (result.length() != 2 * replacement.length() + 1 || !result.startsWith(replacement) || !result.endsWith(replacement)
                    || result.charAt(replacement.length()) != '!') {
                throw new AssertionError("Both replacements and the intervening text must be preserved");
            }
        }

        private static void checkSparseMatch(final Strings strings, final String replacement, final String search, final int max) {
            // Many matches could fit by length, but only one actually occurs.
            final String suffix = StringUtils.repeat('z', 126);
            final String result = strings.replace("ab" + suffix, search, replacement, max);
            if (result.length() != replacement.length() + suffix.length() || !result.startsWith(replacement) || !result.endsWith(suffix)) {
                throw new AssertionError("The replacement and unmatched suffix must be preserved");
            }
        }

        /**
         * Replaces strings with Latin-1 and UTF-16 data in a bounded heap, checking full result contents.
         *
         * @param args whether to ignore case and the maximum replacement count.
         */
        public static void main(final String[] args) {
            final boolean ignoreCase = Boolean.parseBoolean(args[0]);
            final int max = Integer.parseInt(args[1]);
            final Strings strings = ignoreCase ? Strings.CI : Strings.CS;
            final String search = ignoreCase ? "AB" : "ab";
            for (final char ch : new char[] { 'x', '\u0101' }) {
                // The former 16-match estimate needs over 64 MB even with compact strings; actual results fit on Java 8 as well.
                // Leave headroom for the two-match UTF-16 result with Serial GC.
                final String replacement = StringUtils.repeat(ch, 4_250_000);
                check(strings, replacement, search, max);
                checkSparseMatch(strings, replacement, search, max);
                checkMultipleMatches(strings, replacement, search, max);
            }
            System.out.println("Completed replacement memory checks");
        }
    }

    @ParameterizedTest
    @CsvSource({ "false, -1", "false, 64", "true, -1", "true, 64" })
    void testLargeReplacementWithBoundedHeap(final boolean ignoreCase, final int max, @TempDir final Path tempDir) throws Exception {
        final Path java = Paths.get(System.getProperty("java.home"), "bin", SystemUtils.IS_OS_WINDOWS ? "java.exe" : "java");
        final String classPath = Paths.get(MemoryProbe.class.getProtectionDomain().getCodeSource().getLocation().toURI()) + File.pathSeparator
                + Paths.get(Strings.class.getProtectionDomain().getCodeSource().getLocation().toURI());
        final Path log = tempDir.resolve("memory-probe.log");
        final Process process = new ProcessBuilder(java.toString(), "-Xmx64m", "-cp", classPath, MemoryProbe.class.getName(),
                Boolean.toString(ignoreCase), Integer.toString(max)).redirectErrorStream(true).redirectOutput(log.toFile()).start();
        try {
            assertTrue(process.waitFor(60, TimeUnit.SECONDS), "The memory probe must finish within its time limit");
            final String output = new String(Files.readAllBytes(log), StandardCharsets.UTF_8);
            assertEquals(0, process.exitValue(), output);
            assertTrue(output.contains("Completed replacement memory checks"), output);
        } finally {
            process.destroyForcibly();
            assertTrue(process.waitFor(10, TimeUnit.SECONDS), "The child JVM must terminate even if the test fails");
        }
    }
}
