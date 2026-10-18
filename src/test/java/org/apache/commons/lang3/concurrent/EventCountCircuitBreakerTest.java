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

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.ArrayUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Test class for {@code EventCountCircuitBreaker}.
 */
class EventCountCircuitBreakerTest extends AbstractLangTest {

    /**
     * A test change listener for checking whether correct change events are generated.
     */
    private static final class ChangeListener implements PropertyChangeListener {

        /** The expected event source. */
        private final Object expectedSource;

        /** A list with the updated values extracted from received change events. */
        private final List<Boolean> changedValues;

        /**
         * Creates a new instance of {@code ChangeListener} and sets the expected event
         * source.
         *
         * @param source The expected event source
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
         * @param values The expected values
         */
        public void verify(final Boolean... values) {
            assertArrayEquals(values, changedValues.toArray(ArrayUtils.EMPTY_BOOLEAN_OBJECT_ARRAY));
        }
    }

    /**
     * A test implementation of {@code EventCountCircuitBreaker} which supports mocking the timer.
     * This is useful for the creation of deterministic tests for switching the circuit
     * breaker's state.
     */
    private static final class EventCountCircuitBreakerTestImpl extends EventCountCircuitBreaker {

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
         * @param time The time to set
         * @return A reference to this object
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

        /**
         * Sets the event count near the long boundary without executing billions of increments.
         *
         * @param count The event count
         * @throws ReflectiveOperationException if the interval data cannot be initialized
         */
        @SuppressWarnings("unchecked")
        private void setEventCount(final long count) throws ReflectiveOperationException {
            final Field field = EventCountCircuitBreaker.class.getDeclaredField("checkIntervalData");
            field.setAccessible(true);
            final AtomicReference<Object> intervalData = (AtomicReference<Object>) field.get(this);
            final Constructor<?> constructor = intervalData.get().getClass().getDeclaredConstructor(long.class, long.class);
            constructor.setAccessible(true);
            intervalData.set(constructor.newInstance(count, currentTime));
        }
    }

    /** Constant for the opening threshold. */
    private static final int OPENING_THRESHOLD = 10;

    /** Constant for the closing threshold. */
    private static final int CLOSING_THRESHOLD = 5;

    /** Constant for the factor for converting nanoseconds. */
    private static final long NANO_FACTOR = 1000L * 1000L * 1000L;

    /**
     * Tests whether a new check interval is started if the circuit breaker has a
     * transition to open state.
     */
    @Test
    void testAutomaticOpenStartsNewCheckInterval() {
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
     * Tests whether events are generated when the state is changed.
     */
    @Test
    void testChangeEvents() {
        final EventCountCircuitBreaker breaker = new EventCountCircuitBreaker(OPENING_THRESHOLD, 1,
                TimeUnit.SECONDS);
        final ChangeListener listener = new ChangeListener(breaker);
        breaker.addChangeListener(listener);
        breaker.open();
        breaker.close();
        listener.verify(Boolean.TRUE, Boolean.FALSE);
    }

    /**
     * Tests that automatic state transitions generate change events as well.
     */
    @Test
    void testChangeEventsGeneratedByAutomaticTransitions() {
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
     * Tests whether the circuit breaker can be closed explicitly.
     */
    @Test
    void testClose() {
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
     * Tests that the circuit breaker closes automatically if the number of events
     * received goes under the closing threshold.
     */
    @Test
    void testClosingWhenThresholdReached() {
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
     * Tests that the closing interval is the same as the opening interval if it is not
     * specified.
     */
    @Test
    void testDefaultClosingInterval() {
        final EventCountCircuitBreaker breaker = new EventCountCircuitBreaker(OPENING_THRESHOLD, 1,
                TimeUnit.SECONDS, CLOSING_THRESHOLD);
        assertEquals(NANO_FACTOR, breaker.getClosingInterval(), "Wrong closing interval");
    }

    /**
     * Tests that the closing threshold is the same as the opening threshold if not
     * specified otherwise.
     */
    @Test
    void testDefaultClosingThreshold() {
        final EventCountCircuitBreaker breaker = new EventCountCircuitBreaker(OPENING_THRESHOLD, 1,
                TimeUnit.SECONDS);
        assertEquals(NANO_FACTOR, breaker.getClosingInterval(), "Wrong closing interval");
        assertEquals(OPENING_THRESHOLD, breaker.getClosingThreshold(), "Wrong closing threshold");
    }

    /**
     * Tests that a circuit breaker is closed after its creation.
     */
    @Test
    void testInitiallyClosed() {
        final EventCountCircuitBreaker breaker = new EventCountCircuitBreaker(OPENING_THRESHOLD, 1,
                TimeUnit.SECONDS);
        assertFalse(breaker.isOpen(), "Open");
        assertTrue(breaker.isClosed(), "Not closed");
    }

    /**
     * Tests that time units are correctly taken into account by constructors.
     */
    @Test
    void testIntervalCalculation() {
        final EventCountCircuitBreaker breaker = new EventCountCircuitBreaker(OPENING_THRESHOLD, 1,
                TimeUnit.SECONDS, CLOSING_THRESHOLD, 2, TimeUnit.MILLISECONDS);
        assertEquals(NANO_FACTOR, breaker.getOpeningInterval(), "Wrong opening interval");
        assertEquals(2 * NANO_FACTOR / 1000, breaker.getClosingInterval(), "Wrong closing interval");
    }

    /**
     * Tests that a negative increment is rejected: the event count must only move toward
     * the opening threshold.
     */
    @Test
    void testNegativeIncrementRejected() {
        final EventCountCircuitBreaker breaker = new EventCountCircuitBreaker(OPENING_THRESHOLD, 1, TimeUnit.SECONDS);
        assertThrows(IllegalArgumentException.class, () -> breaker.incrementAndCheckState(-1), "Negative increments must be rejected");
    }

    /**
     * Tests that an open circuit breaker does not close itself when the number of events
     * received is over the threshold.
     */
    @Test
    void testNotClosingOverThreshold() {
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
     * Tests that an open circuit breaker does not close itself when the event count of the
     * check interval exceeds the range of an int.
     */
    @Test
    void testNotClosingWhenEventCountOverflows() {
        final EventCountCircuitBreakerTestImpl breaker = new EventCountCircuitBreakerTestImpl(OPENING_THRESHOLD,
                10, TimeUnit.SECONDS, CLOSING_THRESHOLD, 1, TimeUnit.SECONDS);
        breaker.at(0).open();
        assertFalse(breaker.at(1000).incrementAndCheckState(CLOSING_THRESHOLD), "Not open");
        assertFalse(breaker.at(2000).incrementAndCheckState(Integer.MAX_VALUE), "Not open");
        assertFalse(breaker.at(NANO_FACTOR + 3000).incrementAndCheckState(), "Closed in new interval");
        assertTrue(breaker.isOpen(), "Not open at end");
    }

    /**
     * Tests that saturation prevents closing after heavy load and still allows recovery after a quiet interval.
     *
     * @param count The event count before incrementing
     * @throws ReflectiveOperationException if the interval data cannot be initialized
     */
    @ParameterizedTest
    @ValueSource(longs = {Long.MAX_VALUE - Integer.MAX_VALUE, Long.MAX_VALUE - Integer.MAX_VALUE + 1, Long.MAX_VALUE - 1, Long.MAX_VALUE})
    void testNotClosingWhenEventCountSaturates(final long count) throws ReflectiveOperationException {
        final EventCountCircuitBreakerTestImpl breaker = new EventCountCircuitBreakerTestImpl(OPENING_THRESHOLD,
                10, TimeUnit.SECONDS, CLOSING_THRESHOLD, 1, TimeUnit.SECONDS);
        breaker.at(0).open();
        breaker.setEventCount(count);
        assertFalse(breaker.at(1000).incrementAndCheckState(Integer.MAX_VALUE), "Not open");
        assertFalse(breaker.at(2000).checkState(), "Closed after a zero increment");
        assertFalse(breaker.at(3000).incrementAndCheckState(), "Not open after another event");
        assertFalse(breaker.at(4000).incrementAndCheckState(Integer.MAX_VALUE), "Not open after another batch");
        assertFalse(breaker.at(NANO_FACTOR + 1).checkState(), "Closed after a saturated interval");
        assertTrue(breaker.at(2 * NANO_FACTOR + 2).checkState(), "Not closed after a quiet interval");
    }

    /**
     * Tests that the circuit breaker stays closed if there are a number of received
     * events, but not in a single check interval.
     */
    @Test
    void testNotOpeningCheckIntervalExceeded() {
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
     * Tests that the circuit breaker stays closed if the number of received events stays
     * below the threshold.
     */
    @Test
    void testNotOpeningUnderThreshold() {
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
     * Tests whether the current time is correctly determined.
     */
    @Test
    void testNow() {
        final EventCountCircuitBreaker breaker = new EventCountCircuitBreaker(OPENING_THRESHOLD, 1,
                TimeUnit.SECONDS);
        final long nowNanos = breaker.nanoTime();
        final long deltaNanos = Math.abs(System.nanoTime() - nowNanos);
        assertTrue(deltaNanos < 100_000, String.format("Delta %,d ns to current time too large", deltaNanos));
    }

    /**
     * Tests that the circuit breaker opens when the event count of the check interval
     * exceeds the range of an int.
     */
    @Test
    void testOpeningWhenEventCountOverflows() {
        final EventCountCircuitBreakerTestImpl breaker = new EventCountCircuitBreakerTestImpl(OPENING_THRESHOLD, 1,
            TimeUnit.SECONDS, CLOSING_THRESHOLD, 1, TimeUnit.SECONDS);
        assertTrue(breaker.at(1000).incrementAndCheckState(OPENING_THRESHOLD / 2), "Not closed");
        assertFalse(breaker.at(2000).incrementAndCheckState(Integer.MAX_VALUE), "Not open");
        assertTrue(breaker.isOpen(), "Not open at end");
    }

    /**
     * Tests that the circuit breaker opens if all conditions are met.
     */
    @Test
    void testOpeningWhenThresholdReached() {
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
    void testOpeningWhenThresholdReachedThroughBatch() {
        final long timeIncrement = NANO_FACTOR / OPENING_THRESHOLD - 1;
        final EventCountCircuitBreakerTestImpl breaker = new EventCountCircuitBreakerTestImpl(OPENING_THRESHOLD, 1,
            TimeUnit.SECONDS, CLOSING_THRESHOLD, 1, TimeUnit.SECONDS);
        final long startTime = timeIncrement * (OPENING_THRESHOLD + 1);
        final boolean open = !breaker.at(startTime).incrementAndCheckState(OPENING_THRESHOLD + 1);
        assertTrue(open, "Not open");
        assertFalse(breaker.isClosed(), "Closed");
    }

    /**
     * Tests that the maximum int opening threshold is exceeded without wrapping the event count.
     */
    @Test
    void testOpeningWithMaximumThreshold() {
        final EventCountCircuitBreakerTestImpl breaker = new EventCountCircuitBreakerTestImpl(Integer.MAX_VALUE, 1,
                TimeUnit.SECONDS, CLOSING_THRESHOLD, 1, TimeUnit.SECONDS);
        assertTrue(breaker.at(1000).incrementAndCheckState(Integer.MAX_VALUE), "Opened at the threshold");
        assertFalse(breaker.at(2000).incrementAndCheckState(), "Not open above the threshold");
        assertTrue(breaker.isOpen(), "Not open at end");
    }

    /**
     * Tests whether an explicit open operation fully initializes the internal check data
     * object. Otherwise, the circuit breaker may close itself directly afterwards.
     */
    @Test
    void testOpenStartsNewCheckInterval() {
        final EventCountCircuitBreakerTestImpl breaker = new EventCountCircuitBreakerTestImpl(OPENING_THRESHOLD, 2,
                TimeUnit.SECONDS, CLOSING_THRESHOLD, 1, TimeUnit.SECONDS);
        breaker.at(NANO_FACTOR - 1000).open();
        assertTrue(breaker.isOpen(), "Not open");
        assertFalse(breaker.at(NANO_FACTOR + 100).checkState(), "Already closed");
    }

    /**
     * Tests whether a change listener can be removed.
     */
    @Test
    void testRemoveChangeListener() {
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
    void testStateTransitionGuarded() throws InterruptedException {
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
}
