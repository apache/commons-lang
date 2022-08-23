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
package org.apache.commons.lang3.concurrent;

import java.util.concurrent.atomic.AtomicLong;

/**
 * A simple implementation of the <a
 * href="https://martinfowler.com/bliki/CircuitBreaker.html">Circuit Breaker</a> pattern
 * that opens if the requested increment amount is greater than a given threshold.
 *
 * <p>
 * It contains an internal counter that starts in zero, and each call increments the counter by a given amount.
 * If the threshold is zero, the circuit breaker will be in a permanent <em>open</em> state.
 * </p>
 *
 * <p>
 * An example of use case could be a memory circuit breaker.
 * </p>
 *
 * <pre>
 * long threshold = 10L;
 * ThresholdCircuitBreaker breaker = new ThresholdCircuitBreaker(10L);
 * ...
 * public void handleRequest(Request request) {
 *     long memoryUsed = estimateMemoryUsage(request);
 *     if (breaker.incrementAndCheckState(memoryUsed)) {
 *         // actually handle this request
 *     } else {
 *         // do something else, e.g. send an error code
 *     }
 * }
 * </pre>
 *
 * <p>#Thread safe#</p>
 * @since 3.5
 */
public class ThresholdCircuitBreaker extends AbstractCircuitBreaker<Long> {
    /**
     * The initial value of the internal counter.
     */
    private static final long INITIAL_COUNT = 0L;

    /**
     * The threshold.
     */
    private final long threshold;

    /**
     * Controls the amount used.
     */
    private final AtomicLong used;

    /**
     * Creates a new instance of {@link ThresholdCircuitBreaker} and initializes the threshold.
     *
     * @param threshold the threshold.
     */
    public ThresholdCircuitBreaker(final long threshold) {
        this.used = new AtomicLong(INITIAL_COUNT);
        this.threshold = threshold;
    }

    /**
     * Gets the threshold.
     *
     * @return the threshold
     */
    public long getThreshold() {
        return threshold;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean checkState() {
        return isOpen();
    }

    /**
     * {@inheritDoc}
     *
     * <p>Resets the internal counter back to its initial value (zero).</p>
     */
    @Override
    public void close() {
        super.close();
        this.used.set(INITIAL_COUNT);
    }

    /**
     * {@inheritDoc}
     *
     * <p>If the threshold is zero, the circuit breaker will be in a permanent <em>open</em> state.</p>
     */
    @Override
    public boolean incrementAndCheckState(final Long increment) {
        if (threshold == 0) {
            open();
        }

        final long used = this.used.addAndGet(increment);
        if (used > threshold) {
            open();
        }

        return checkState();
    }

}
