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

package org.apache.commons.lang3.builder;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.concurrent.TimeUnit;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.SystemUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Tests that abandoned direct outputs remain collectable when a visited object refers back to their output buffer.
 *
 * <p>
 * Direct style calls retain traversal state between {@code appendStart()} and {@code appendEnd()} using weak buffer keys. Weak keys alone cannot release an
 * abandoned buffer if the associated traversal strongly retains a visited object that points back to that buffer. Weak visited references must break this
 * retention path even when the caller never invokes {@code appendEnd()}.
 * </p>
 *
 * <p>
 * A child JVM allocates more buffer capacity than its bounded heap can retain. Successful completion demonstrates that abandoned outputs can be collected under
 * allocation pressure, without depending on explicit garbage collection or risking the main test runner's heap.
 * </p>
 */
class ToStringBuilderTraversalMemoryTest extends AbstractLangTest {

    /**
     * Supplies a standalone entry point for the child JVM's allocation workload.
     *
     * <p>
     * All workload helpers are nested here so the child needs only the project's main and test classes, without invoking JUnit. The parent test provides the
     * heap limit and checks both the exit status and a completion marker.
     * </p>
     */
    public static final class MemoryProbe {

        /**
         * Supplies a reflectively visited field that strongly references the output buffer.
         *
         * <p>
         * Retaining this object in a traversal's visited set would also retain the buffer used as the direct traversal map's weak key.
         * </p>
         */
        private static final class BufferLink {

            @SuppressWarnings("unused")
            private final StringBuffer buffer;

            /**
             * Creates the back reference that exposes strong retention by a visited set.
             *
             * @param buffer the output buffer that must become collectable after the iteration.
             */
            private BufferLink(final StringBuffer buffer) {
                this.buffer = buffer;
            }
        }

        /**
         * Renders the back reference while isolating traversal retention from root registration and JDK implementation fields.
         */
        private static final class DirectStyle extends RecursiveToStringStyle {

            private static final long serialVersionUID = 1L;

            /**
             * Disables the prefixes that register the root, so an omitted {@code appendEnd()} tests only abandoned traversal state.
             */
            private DirectStyle() {
                // Avoid registering an abandoned root: this test concerns visited objects, not the older root registry behavior.
                setUseClassName(false);
                setUseIdentityHashCode(false);
            }

            /**
             * Excludes buffer internals from reflection while allowing {@link BufferLink} to be visited.
             *
             * <p>
             * The buffer is rendered using its own {@code toString()}, avoiding reflective access to JDK implementation fields. The link still holds the strong
             * back reference needed to expose a retention regression.
             * </p>
             *
             * @param type the class considered for recursive reflection.
             * @return {@code false} for {@link StringBuffer}, otherwise the superclass decision.
             */
            @Override
            protected boolean accept(final Class<?> type) {
                return type != StringBuffer.class && super.accept(type);
            }
        }

        /**
         * Creates 20,000 abandoned outputs whose total buffer capacity exceeds the child JVM's 32 MB heap.
         *
         * <p>
         * Every iteration opens a direct traversal, visits an object that points back to its buffer, and deliberately omits {@code appendEnd()}. The root
         * registry must remain empty so it cannot explain retained buffers. With weak visited references, allocation pressure allows old outputs to be
         * collected and the workload prints its completion marker; strong retention exhausts the bounded heap before completion.
         * </p>
         *
         * @param args unused.
         */
        public static void main(final String[] args) {
            final DirectStyle style = new DirectStyle();
            for (int i = 0; i < 20_000; i++) {
                final StringBuffer buffer = new StringBuffer(4096);
                style.appendStart(buffer, new Object());
                style.append(buffer, "link", new BufferLink(buffer), Boolean.TRUE);
                // Deliberately omit appendEnd(). A visited object refers back to the weakly registered output buffer.
                if (!ToStringStyle.getRegistry().isEmpty()) {
                    throw new AssertionError("The root registry must not retain these outputs");
                }
            }
            System.out.println("Completed 20000 abandoned outputs");
        }
    }

    /**
     * Tests that abandoned buffer back references do not prevent the allocation workload from completing within a 32 MB heap.
     *
     * <p>
     * The child uses the current Java runtime and the main and test class locations. Exit status zero and the completion marker establish that the entire
     * workload ran successfully. A timeout and forced cleanup bound failures, and captured child output makes memory exhaustion diagnosable.
     * </p>
     *
     * @param tempDir the directory for the child's combined standard output and error log.
     * @throws Exception if class locations cannot be resolved, the process or log cannot be accessed, or waiting is interrupted.
     */
    @Test
    void testAbandonedOutputWithBufferBackReferenceIsCollectable(@TempDir final Path tempDir) throws Exception {
        final Path java = Paths.get(System.getProperty("java.home"), "bin", SystemUtils.IS_OS_WINDOWS ? "java.exe" : "java");
        final String classPath = Paths.get(MemoryProbe.class.getProtectionDomain().getCodeSource().getLocation().toURI()) + File.pathSeparator
                + Paths.get(ToStringBuilder.class.getProtectionDomain().getCodeSource().getLocation().toURI());
        final Path log = tempDir.resolve("memory-probe.log");
        final Process process = new ProcessBuilder(java.toString(), "-Xmx32m", "-cp", classPath, MemoryProbe.class.getName()).redirectErrorStream(true)
                .redirectOutput(log.toFile()).start();
        try {
            assertTrue(process.waitFor(60, TimeUnit.SECONDS), "The memory probe must finish within its time limit");
            final String output = new String(Files.readAllBytes(log), StandardCharsets.UTF_8);
            assertEquals(0, process.exitValue(), output);
            assertTrue(output.contains("Completed 20000 abandoned outputs"), output);
        } finally {
            process.destroyForcibly();
            assertTrue(process.waitFor(10, TimeUnit.SECONDS), "The child JVM must terminate even if the test fails");
        }
    }
}
