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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.function.Consumer;

import org.apache.commons.lang3.concurrent.BaseCircuitBreaker.State;
import org.junit.jupiter.api.Test;

/**
 * Test class for {@code SimpleThresholdCircuitBreaker}.
 */
public class SimpleThresholdCircuitBreakerTest {

    /**
     * Threshold used in tests.
     */
    private static final long threshold = 10L;

    private static final long zeroThreshold = 0L;

    /**
     * Tests that the threshold is working as expected when incremented and no exception is thrown.
     */
    @Test
    public void testThreshold() {
        final SimpleThresholdCircuitBreaker circuit = new SimpleThresholdCircuitBreaker(threshold);
        circuit.incrementAndCheckState(9L);
        assertFalse(circuit.incrementAndCheckState(1L), "Circuit opened before reaching the threshold");
    }

    /**
     * Tests that exceeding the threshold raises an exception.
     */
    @Test
    public void testThresholdCircuitBreakingException() {
        final SimpleThresholdCircuitBreaker circuit = new SimpleThresholdCircuitBreaker(threshold);
        circuit.incrementAndCheckState(9L);
        assertTrue(circuit.incrementAndCheckState(2L), "The circuit was supposed to be open after increment above the threshold");
    }

    /**
     * Test that when threshold is zero, the circuit breaker is always open.
     */
    @Test
    public void testThresholdEqualsZero() {
        final SimpleThresholdCircuitBreaker circuit = new SimpleThresholdCircuitBreaker(zeroThreshold);
        assertTrue(circuit.incrementAndCheckState(0L), "When the threshold is zero, the circuit is supposed to be always open");
    }

    /**
     * Tests that closing a {@code SimpleThresholdCircuitBreaker} resets the internal counter.
     */
    @Test
    public void testClosingSimpleThresholdCircuitBreaker() {
        final SimpleThresholdCircuitBreaker circuit = new SimpleThresholdCircuitBreaker(threshold);
        circuit.incrementAndCheckState(9L);
        circuit.close();
        // now the internal counter is back at zero, not 9 anymore. So it is safe to increment 9 again
        assertFalse(circuit.incrementAndCheckState(9L), "Internal counter was not reset back to zero");
    }

    /**
     * Tests that we can get the threshold value correctly.
     */
    @Test
    public void testGettingThreshold() {
        final SimpleThresholdCircuitBreaker circuit = new SimpleThresholdCircuitBreaker(threshold);
        assertEquals(Long.valueOf(threshold), Long.valueOf(circuit.getThreshold()), "Wrong value of threshold");
    }

    private static class MyConsumer implements Consumer<State> {
        public State state;
        @Override
        public void accept(State state) {
            this.state = state;
        }
    }

    /**
     * Test the circuit breaker using a {@code Consumer}.
     */
    @Test
    public void testUsingConsumer() {
        final MyConsumer consumer = new MyConsumer();
        final SimpleThresholdCircuitBreaker circuit = new SimpleThresholdCircuitBreaker(consumer, threshold);
        assertNull(consumer.state);
        circuit.incrementAndCheckState(threshold + 1);
        assertEquals(State.OPEN, consumer.state);
    }
}
