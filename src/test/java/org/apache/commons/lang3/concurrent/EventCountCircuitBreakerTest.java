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

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.ArrayUtils;
import org.junit.jupiter.api.Test;

/**
 * Test class for {@code EventCountCircuitBreaker}.
 */
public class EventCountCircuitBreakerTest extends AbstractLangTest {
    /** Constant for the opening threshold. */
    private static final int OPENING_THRESHOLD = 10;

    /** Constant for the closing threshold. */
    private static final int CLOSING_THRESHOLD = 5;

    /** Constant for the factor for converting nanoseconds. */
    private static final long NANO_FACTOR = 1000L * 1000L * 1000L;

    /**
     * Tests that time units are correctly taken into account by constructors.
     */
    @Test
    public void testIntervalCalculation() {
        final EventCountCircuitBreaker breaker = new EventCountCircuitBreaker(OPENING_THRESHOLD, 1,
                TimeUnit.SECONDS, CLOSING_THRESHOLD, 2, TimeUnit.MILLISECONDS);
        assertEquals(NANO_FACTOR, breaker.getOpeningInterval(), "Wrong opening interval");
        assertEquals(2 * NANO_FACTOR / 1000, breaker.getClosingInterval(), "Wrong closing interval");
    }

    /**
     * Tests that the closing interval is the same as the opening interval if it is not
     * specified.
     */
    @Test
    public void testDefaultClosingInterval() {
        final EventCountCircuitBreaker breaker = new EventCountCircuitBreaker(OPENING_THRESHOLD, 1,
                TimeUnit.SECONDS, CLOSING_THRESHOLD);
        assertEquals(NANO_FACTOR, breaker.getClosingInterval(), "Wrong closing interval");
    }

    /**
     * Tests that the closing threshold is the same as the opening threshold if not
     * specified otherwise.
     */
    @Test
    public void testDefaultClosingThreshold() {
        final EventCountCircuitBreaker breaker = new EventCountCircuitBreaker(OPENING_THRESHOLD, 1,
                TimeUnit.SECONDS);
        assertEquals(NANO_FACTOR, breaker.getClosingInterval(), "Wrong closing interval");
        assertEquals(OPENING_THRESHOLD, breaker.getClosingThreshold(), "Wrong closing threshold");
    }

    /**
     * Tests that a circuit breaker is closed after its creation.
     */
    @Test
    public void testInitiallyClosed() {
        final EventCountCircuitBreaker breaker = new EventCountCircuitBreaker(OPENING_THRESHOLD, 1,
                TimeUnit.SECONDS);
        assertFalse(breaker.isOpen(), "Open");
        assertTrue(breaker.isClosed(), "Not closed");
    }

    /**
     * Tests whether the current time is correctly determined.
     */
    @Test
    public void testNow() {
        final EventCountCircuitBreaker breaker = new EventCountCircuitBreaker(OPENING_THRESHOLD, 1,
                TimeUnit.SECONDS);
        final long nowNanos = breaker.nanoTime();
        final long deltaNanos = Math.abs(System.nanoTime() - nowNanos);
        assertTrue(deltaNanos < 100_000, String.format("Delta %,d ns to current time too large", deltaNanos));
    }

    /**
     * Tests that the circuit breaker stays closed if the number of received events stays
     * below the threshold.
     */
    @Test
    public void testNotOpeningUnderThreshold() {
        long startTime = 1000;
        final EventCountCircuitBreakerTestImpl breaker = new EventCountCircuitBreakerTestImpl(OPENING_THRESHOLD, 1,
                TimeUnit.SECONDS, CLOSING_THRESHOLD, 1, TimeUnit.SECONDS);
        for (int i = 0; i < OPENING_THRESHOLD - 1; i++) {
            assertTrue(breaker.at(startTime).incrementAndCheckState(), "In open state");
            startTime++;
        }
        assertTrue(breaker.isClosed(), "Not closed");
    }

    /**
     * Tests that the circuit breaker stays closed if there are a number of received
     * events, but not in a single check interval.
     */
    @Test
    public void testNotOpeningCheckIntervalExceeded() {
        long startTime = 0L;
        final long timeIncrement = 3 * NANO_FACTOR / (2 * OPENING_THRESHOLD);
        final EventCountCircuitBreakerTestImpl breaker = new EventCountCircuitBreakerTestImpl(OPENING_THRESHOLD, 1,
                TimeUnit.SECONDS, CLOSING_THRESHOLD, 1, TimeUnit.SECONDS);
        for (int i = 0; i < 5 * OPENING_THRESHOLD; i++) {
            assertTrue(breaker.at(startTime).incrementAndCheckState(), "In open state");
            startTime += timeIncrement;
        }
        assertTrue(breaker.isClosed(), "Not closed");
    }

    /**
     * Tests that the circuit breaker opens if all conditions are met.
     */
    @Test
    public void testOpeningWhenThresholdReached() {
        long startTime = 0;
        final long timeIncrement = NANO_FACTOR / OPENING_THRESHOLD - 1;
        final EventCountCircuitBreakerTestImpl breaker = new EventCountCircuitBreakerTestImpl(OPENING_THRESHOLD, 1,
                TimeUnit.SECONDS, CLOSING_THRESHOLD, 1, TimeUnit.SECONDS);
        boolean open = false;
        for (int i = 0; i < OPENING_THRESHOLD + 1; i++) {
            open = !breaker.at(startTime).incrementAndCheckState();
            startTime += timeIncrement;
        }
        assertTrue(open, "Not open");
        assertFalse(breaker.isClosed(), "Closed");
    }

    /**
     * Tests that the circuit breaker opens if all conditions are met when using
     * {@link EventCountCircuitBreaker#incrementAndCheckState(Integer increment)}.
     */
    @Test
    public void testOpeningWhenThresholdReachedThroughBatch() {
        final long timeIncrement = NANO_FACTOR / OPENING_THRESHOLD - 1;
        final EventCountCircuitBreakerTestImpl breaker = new EventCountCircuitBreakerTestImpl(OPENING_THRESHOLD, 1,
            TimeUnit.SECONDS, CLOSING_THRESHOLD, 1, TimeUnit.SECONDS);
        final long startTime = timeIncrement * (OPENING_THRESHOLD + 1);
        final boolean open = !breaker.at(startTime).incrementAndCheckState(OPENING_THRESHOLD + 1);
        assertTrue(open, "Not open");
        assertFalse(breaker.isClosed(), "Closed");
    }

    /**
     * Tests that an open circuit breaker does not close itself when the number of events
     * received is over the threshold.
     */
    @Test
    public void testNotClosingOverThreshold() {
        final EventCountCircuitBreakerTestImpl breaker = new EventCountCircuitBreakerTestImpl(OPENING_THRESHOLD,
                10, TimeUnit.SECONDS, CLOSING_THRESHOLD, 1, TimeUnit.SECONDS);
        long startTime = 0;
        breaker.open();
        for (int i = 0; i <= CLOSING_THRESHOLD; i++) {
            assertFalse(breaker.at(startTime).incrementAndCheckState(), "Not open");
            startTime += 1000;
        }
        assertFalse(breaker.at(startTime + NANO_FACTOR).incrementAndCheckState(), "Closed in new interval");
        assertTrue(breaker.isOpen(), "Not open at end");
    }

    /**
     * Tests that the circuit breaker closes automatically if the number of events
     * received goes under the closing threshold.
     */
    @Test
    public void testClosingWhenThresholdReached() {
        final EventCountCircuitBreakerTestImpl breaker = new EventCountCircuitBreakerTestImpl(OPENING_THRESHOLD,
                10, TimeUnit.SECONDS, CLOSING_THRESHOLD, 1, TimeUnit.SECONDS);
        breaker.open();
        breaker.at(1000).incrementAndCheckState();
        assertFalse(breaker.at(2000).checkState(), "Already closed");
        assertFalse(breaker.at(NANO_FACTOR).checkState(), "Closed at interval end");
        assertTrue(breaker.at(NANO_FACTOR + 1).checkState(), "Not closed after interval end");
        assertTrue(breaker.isClosed(), "Not closed at end");
    }

    /**
     * Tests whether an explicit open operation fully initializes the internal check data
     * object. Otherwise, the circuit breaker may close itself directly afterwards.
     */
    @Test
    public void testOpenStartsNewCheckInterval() {
        final EventCountCircuitBreakerTestImpl breaker = new EventCountCircuitBreakerTestImpl(OPENING_THRESHOLD, 2,
                TimeUnit.SECONDS, CLOSING_THRESHOLD, 1, TimeUnit.SECONDS);
        breaker.at(NANO_FACTOR - 1000).open();
        assertTrue(breaker.isOpen(), "Not open");
        assertFalse(breaker.at(NANO_FACTOR + 100).checkState(), "Already closed");
    }

    /**
     * Tests whether a new check interval is started if the circuit breaker has a
     * transition to open state.
     */
    @Test
    public void testAutomaticOpenStartsNewCheckInterval() {
        final EventCountCircuitBreakerTestImpl breaker = new EventCountCircuitBreakerTestImpl(OPENING_THRESHOLD, 2,
                TimeUnit.SECONDS, CLOSING_THRESHOLD, 1, TimeUnit.SECONDS);
        long time = 10 * NANO_FACTOR;
        for (int i = 0; i <= OPENING_THRESHOLD; i++) {
            breaker.at(time++).incrementAndCheckState();
        }
        assertTrue(breaker.isOpen(), "Not open");
        time += NANO_FACTOR - 1000;
        assertFalse(breaker.at(time).incrementAndCheckState(), "Already closed");
        time += 1001;
        assertTrue(breaker.at(time).checkState(), "Not closed in time interval");
    }

    /**
     * Tests whether the circuit breaker can be closed explicitly.
     */
    @Test
    public void testClose() {
        final EventCountCircuitBreakerTestImpl breaker = new EventCountCircuitBreakerTestImpl(OPENING_THRESHOLD, 2,
                TimeUnit.SECONDS, CLOSING_THRESHOLD, 1, TimeUnit.SECONDS);
        long time = 0;
        for (int i = 0; i <= OPENING_THRESHOLD; i++, time += 1000) {
            breaker.at(time).incrementAndCheckState();
        }
        assertTrue(breaker.isOpen(), "Not open");
        breaker.close();
        assertTrue(breaker.isClosed(), "Not closed");
        assertTrue(breaker.at(time + 1000).incrementAndCheckState(), "Open again");
    }

    /**
     * Tests whether events are generated when the state is changed.
     */
    @Test
    public void testChangeEvents() {
        final EventCountCircuitBreaker breaker = new EventCountCircuitBreaker(OPENING_THRESHOLD, 1,
                TimeUnit.SECONDS);
        final ChangeListener listener = new ChangeListener(breaker);
        breaker.addChangeListener(listener);
        breaker.open();
        breaker.close();
        listener.verify(Boolean.TRUE, Boolean.FALSE);
    }

    /**
     * Tests whether a change listener can be removed.
     */
    @Test
    public void testRemoveChangeListener() {
        final EventCountCircuitBreaker breaker = new EventCountCircuitBreaker(OPENING_THRESHOLD, 1,
                TimeUnit.SECONDS);
        final ChangeListener listener = new ChangeListener(breaker);
        breaker.addChangeListener(listener);
        breaker.open();
        breaker.removeChangeListener(listener);
        breaker.close();
        listener.verify(Boolean.TRUE);
    }

    /**
     * Tests that a state transition triggered by multiple threads is handled correctly.
     * Only the first transition should cause an event to be sent.
     */
    @Test
    public void testStateTransitionGuarded() throws InterruptedException {
        final EventCountCircuitBreaker breaker = new EventCountCircuitBreaker(OPENING_THRESHOLD, 1,
                TimeUnit.SECONDS);
        final ChangeListener listener = new ChangeListener(breaker);
        breaker.addChangeListener(listener);

        final int threadCount = 128;
        final CountDownLatch latch = new CountDownLatch(1);
        final Thread[] threads = new Thread[threadCount];
        for (int i = 0; i < threadCount; i++) {
            threads[i] = new Thread() {
                @Override
                public void run() {
                    try {
                        latch.await();
                    } catch (final InterruptedException iex) {
                        // ignore
                    }
                    breaker.open();
                }
            };
            threads[i].start();
        }
        latch.countDown();
        for (final Thread thread : threads) {
            thread.join();
        }
        listener.verify(Boolean.TRUE);
    }

    /**
     * Tests that automatic state transitions generate change events as well.
     */
    @Test
    public void testChangeEventsGeneratedByAutomaticTransitions() {
        final EventCountCircuitBreakerTestImpl breaker = new EventCountCircuitBreakerTestImpl(OPENING_THRESHOLD, 2,
                TimeUnit.SECONDS, CLOSING_THRESHOLD, 1, TimeUnit.SECONDS);
        final ChangeListener listener = new ChangeListener(breaker);
        breaker.addChangeListener(listener);
        long time = 0;
        for (int i = 0; i <= OPENING_THRESHOLD; i++, time += 1000) {
            breaker.at(time).incrementAndCheckState();
        }
        breaker.at(NANO_FACTOR + 1).checkState();
        breaker.at(3 * NANO_FACTOR).checkState();
        listener.verify(Boolean.TRUE, Boolean.FALSE);
    }

    /**
     * A test implementation of {@code EventCountCircuitBreaker} which supports mocking the timer.
     * This is useful for the creation of deterministic tests for switching the circuit
     * breaker's state.
     */
    private static class EventCountCircuitBreakerTestImpl extends EventCountCircuitBreaker {
        /** The current time in nanoseconds. */
        private long currentTime;

        EventCountCircuitBreakerTestImpl(final int openingThreshold, final long openingInterval,
                                                final TimeUnit openingUnit, final int closingThreshold, final long closingInterval,
                                                final TimeUnit closingUnit) {
            super(openingThreshold, openingInterval, openingUnit, closingThreshold,
                    closingInterval, closingUnit);
        }

        /**
         * Sets the current time to be used by this test object for the next operation.
         *
         * @param time the time to set
         * @return a reference to this object
         */
        public EventCountCircuitBreakerTestImpl at(final long time) {
            currentTime = time;
            return this;
        }

        /**
         * {@inheritDoc} This implementation returns the value passed to the {@code at()}
         * method.
         */
        @Override
        long nanoTime() {
            return currentTime;
        }
    }

    /**
     * A test change listener for checking whether correct change events are generated.
     */
    private static class ChangeListener implements PropertyChangeListener {
        /** The expected event source. */
        private final Object expectedSource;

        /** A list with the updated values extracted from received change events. */
        private final List<Boolean> changedValues;

        /**
         * Creates a new instance of {@code ChangeListener} and sets the expected event
         * source.
         *
         * @param source the expected event source
         */
        ChangeListener(final Object source) {
            expectedSource = source;
            changedValues = new ArrayList<>();
        }

        @Override
        public void propertyChange(final PropertyChangeEvent evt) {
            assertEquals(expectedSource, evt.getSource(), "Wrong event source");
            assertEquals("open", evt.getPropertyName(), "Wrong property name");
            final Boolean newValue = (Boolean) evt.getNewValue();
            final Boolean oldValue = (Boolean) evt.getOldValue();
            assertNotEquals(newValue, oldValue, "Old and new value are equal");
            changedValues.add(newValue);
        }

        /**
         * Verifies that change events for the expected values have been received.
         *
         * @param values the expected values
         */
        public void verify(final Boolean... values) {
            assertArrayEquals(values, changedValues.toArray(ArrayUtils.EMPTY_BOOLEAN_OBJECT_ARRAY));
        }
    }
}
