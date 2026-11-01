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
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Map;
import java.util.concurrent.BrokenBarrierException;
import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Tests isolation of visited state between threads and between successive outputs on the same thread.
 *
 * <p>
 * Two workers render the same object concurrently. Each output must detail its first occurrence and abbreviate its second occurrence, regardless of whether the
 * other worker has already visited that object. A barrier inside the rendering callback forces the traversals to overlap instead of relying on thread
 * scheduling to expose shared state.
 * </p>
 *
 * <p>
 * The workers repeat the operation with their own style instances to expose state left on reused threads. Only one worker keeps an unrelated root registered,
 * so the assertions also check that root registration and traversal cleanup remain local to each thread.
 * </p>
 */
class ToStringBuilderTraversalConcurrencyTest extends AbstractLangTest {

    /**
     * Waits for both workers to reach the shared object's rendering callback.
     *
     * <p>
     * A timeout turns a missing callback into a bounded test failure. For example, state shared across threads could make one worker abbreviate the object
     * immediately and skip the callback, leaving the other worker waiting. Interruptions preserve the interrupt flag for executor shutdown.
     * </p>
     *
     * @param barrier the barrier shared by the two workers.
     * @throws IllegalStateException if synchronization is interrupted, times out, or the barrier is broken.
     */
    private static void awaitOtherTraversal(final CyclicBarrier barrier) {
        try {
            barrier.await(10, TimeUnit.SECONDS);
        } catch (final InterruptedException ex) {
            Thread.currentThread().interrupt();
            throw new IllegalStateException(ex);
        } catch (final BrokenBarrierException | TimeoutException ex) {
            throw new IllegalStateException(ex);
        }
    }

    /**
     * Creates a worker's style with a synchronization point after the shared object is marked visited and before it is rendered reflectively.
     *
     * <p>
     * Each worker needs its own style because multiline indentation is mutable. The barrier coordinates traversal state without introducing a separate race
     * through shared formatting state.
     * </p>
     *
     * @param multiline whether to use the multiline recursive style.
     * @param shared    the object whose rendering must overlap on both workers.
     * @param barrier   the barrier shared by the workers.
     * @return a style that pauses when rendering the shared object in detail.
     */
    private static RecursiveToStringStyle createStyle(final boolean multiline, final Object shared, final CyclicBarrier barrier) {
        if (multiline) {
            return new MultilineRecursiveToStringStyle() {

                private static final long serialVersionUID = 1L;

                /**
                 * {@inheritDoc}
                 *
                 * <p>
                 * Waits for the other worker before rendering the shared object so both traversals are active at the same time.
                 * </p>
                 */
                @Override
                public void appendDetail(final StringBuffer buffer, final String fieldName, final Object value) {
                    if (value == shared) {
                        awaitOtherTraversal(barrier);
                    }
                    super.appendDetail(buffer, fieldName, value);
                }
            };
        }
        return new RecursiveToStringStyle() {

            private static final long serialVersionUID = 1L;

            /**
             * {@inheritDoc}
             *
             * <p>
             * Waits for the other worker before rendering the shared object so both traversals are active at the same time.
             * </p>
             */
            @Override
            public void appendDetail(final StringBuffer buffer, final String fieldName, final Object value) {
                if (value == shared) {
                    awaitOtherTraversal(barrier);
                }
                super.appendDetail(buffer, fieldName, value);
            }
        };
    }

    /**
     * Renders 100 outputs on one worker and checks their contents and thread-local cleanup.
     *
     * <p>
     * Each output contains the shared object's identity twice, with a detailed first occurrence and an abbreviated second occurrence. Repeating the assertions
     * on the same thread detects visited marks leaking from a previous output. The optional unrelated root must survive each completed output, then be removed
     * when this worker finishes.
     * </p>
     *
     * @param multiline whether to use the multiline recursive style.
     * @param shared    the object rendered twice in each output and shared with the other worker.
     * @param barrier   the synchronization point used for each output's first occurrence.
     * @param abandoned whether to keep an unrelated abandoned builder's root registered on this worker.
     */
    private static void renderOnWorker(final boolean multiline, final Object shared, final CyclicBarrier barrier, final boolean abandoned) {
        final Object orphan = new Object();
        if (abandoned) {
            new ReflectionToStringBuilder(orphan, new RecursiveToStringStyle());
        }
        try {
            // Each worker reuses its own style; multiline indentation is mutable and must not be shared between threads.
            final RecursiveToStringStyle style = createStyle(multiline, shared, barrier);
            final String identity = ObjectUtils.identityToString(shared);
            for (int i = 0; i < 100; i++) {
                final String output = new ToStringBuilder(new Object[0], style).append("first", shared).append("second", shared).toString();
                assertEquals(2, StringUtils.countMatches(output, identity));
                assertEquals(1, StringUtils.countMatches(output, identity + "["));
                assertTrue(output.contains("first=" + identity + "["));
                assertNull(ToStringStyle.getActiveTraversal());
                final Map<?, ?> direct = ToStringStyle.getDirectTraversals().get();
                assertTrue(direct == null || direct.isEmpty());
                assertEquals(abandoned ? 1 : 0, ToStringStyle.getRegistry().size());
                if (abandoned) {
                    assertTrue(ToStringStyle.getRegistry().containsKey(orphan));
                }
            }
        } finally {
            ToStringStyle.unregister(orphan);
        }
        assertTrue(ToStringStyle.getRegistry().isEmpty(), "A reused worker must not retain completed roots");
    }

    /**
     * Tests that two overlapping traversals can render the same object independently on reused worker threads.
     *
     * <p>
     * One worker keeps an unrelated root registered and the other does not. Both must produce a fresh detailed first occurrence in every output. Bounded waits
     * and executor shutdown ensure that a traversal regression cannot leave the test waiting indefinitely or retain worker threads.
     * </p>
     *
     * @param multiline whether to exercise the multiline recursive style instead of the single-line recursive style.
     * @throws Exception if a worker fails, a wait times out, or the test thread is interrupted.
     */
    @ParameterizedTest(name = "multiline={0}")
    @ValueSource(booleans = { false, true })
    void testOverlappingTraversalsOnReusedWorkers(final boolean multiline) throws Exception {
        final Object shared = new Object();
        final CyclicBarrier barrier = new CyclicBarrier(2);
        final ExecutorService pool = Executors.newFixedThreadPool(2);
        try {
            // Both threads pause after marking the same object visited, before either completes its nested rendering.
            final Future<?> withOrphan = pool.submit(() -> renderOnWorker(multiline, shared, barrier, true));
            final Future<?> withoutOrphan = pool.submit(() -> renderOnWorker(multiline, shared, barrier, false));
            withOrphan.get(30, TimeUnit.SECONDS);
            withoutOrphan.get(30, TimeUnit.SECONDS);
        } finally {
            pool.shutdownNow();
            assertTrue(pool.awaitTermination(10, TimeUnit.SECONDS), "Worker threads must terminate even if an assertion fails");
        }
    }
}
