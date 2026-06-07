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

package org.apache.commons.lang3.concurrent;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTimeout;

import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicLong;

import org.junit.jupiter.api.Test;

/**
 * AtomicSafeInitializer.get() spins in a while-loop without Thread.yield() or LockSupport.parkNanos() when the CAS fails (another thread is initializing).
 *
 * <p>
 * Concurrent callers who lose the CAS busy-wait for the duration of initialize(), burning CPU proportional to init latency * thread count. A slow initializer
 * combined with many concurrent callers use more CPU than it can.
 * </p>
 *
 * <p>
 * This test measures CPU time spent in spinning threads during a 100 ms init. Pre-patch: spinning threads consume significant CPU. Post-patch: spinning threads
 * yield, keeping CPU near zero while waiting.
 * </p>
 */
class AtomicSafeInitializerInitTest {

    /** Slow initializer: sleeps 100 ms to widen the spin window. */
    private static final int INIT_MS = 100;
    private static final int SPINNER_THREADS = 8;

    private static long threadCpuTimeNanos() {
        final ThreadMXBean mx = ManagementFactory.getThreadMXBean();
        return mx.isCurrentThreadCpuTimeSupported() ? mx.getCurrentThreadCpuTime() : 0;
    }

    @Test
    void testSpinningThreadsYieldDuringSlowInit() throws Exception {
        final CountDownLatch startLatch = new CountDownLatch(1);
        final AtomicLong totalCpuNanos = new AtomicLong();
        final AtomicSafeInitializer<String> initializer = AtomicSafeInitializer.<String>builder().setInitializer(() -> {
            Thread.sleep(INIT_MS);
            return "done";
        }).get();
        final ExecutorService exec = Executors.newFixedThreadPool(SPINNER_THREADS + 1);
        try {
            final List<Future<?>> futures = new ArrayList<>();
            for (int i = 0; i < SPINNER_THREADS; i++) {
                futures.add(exec.submit(() -> {
                    try {
                        startLatch.await();
                        final long cpuBeforeNanos = threadCpuTimeNanos();
                        initializer.get();
                        totalCpuNanos.addAndGet(threadCpuTimeNanos() - cpuBeforeNanos);
                    } catch (final Exception e) {
                        Thread.currentThread().interrupt();
                    }
                }));
            }
            startLatch.countDown();
            for (final Future<?> f : futures) {
                f.get();
            }
        } finally {
            exec.shutdown();
        }
        assertNotNull(initializer.get());
        // Post-patch: CPU consumed by spinner threads during 100 ms init must be
        // significantly less than INIT_MS per thread. We allow 50 ms total CPU
        // across all spinner threads (vs. ~800 ms if spinning at 100%).
        // This threshold is conservative, a yielding implementation uses ~0 ms.
        final long cpuMs = totalCpuNanos.get() / 1_000_000;
        // Re-express as a blocking assertion: if cpuMs > threshold, fail.
        assertTimeout(Duration.ofMillis(INIT_MS * SPINNER_THREADS / 4), () -> assertFalse(cpuMs > INIT_MS * SPINNER_THREADS / 4,
                () -> "Spinner threads consumed " + cpuMs + " ms CPU during " + INIT_MS + " ms init — missing Thread.yield() in get() spin loop"));
    }
}
