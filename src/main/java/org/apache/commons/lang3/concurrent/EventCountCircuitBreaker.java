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

import java.beans.PropertyChangeListener;
import java.util.EnumMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

/**
 * A simple implementation of the <a
 * href="https://martinfowler.com/bliki/CircuitBreaker.html">Circuit Breaker</a> pattern
 * that counts specific events.
 *
 * <p>
 * A <em>circuit breaker</em> can be used to protect an application against unreliable
 * services or unexpected load. A newly created {@link EventCountCircuitBreaker} object is
 * initially in state <em>closed</em> meaning that no problem has been detected. When the
 * application encounters specific events (like errors or service timeouts), it tells the
 * circuit breaker to increment an internal counter. If the number of events reported in a
 * specific time interval exceeds a configurable threshold, the circuit breaker changes
 * into state <em>open</em>. This means that there is a problem with the associated sub
 * system; the application should no longer call it, but give it some time to settle down.
 * The circuit breaker can be configured to switch back to <em>closed</em> state after a
 * certain time frame if the number of events received goes below a threshold.
 * </p>
 * <p>
 * When a {@link EventCountCircuitBreaker} object is constructed the following parameters
 * can be provided:
 * </p>
 * <ul>
 * <li>A threshold for the number of events that causes a state transition to
 * <em>open</em> state. If more events are received in the configured check interval, the
 * circuit breaker switches to <em>open</em> state.</li>
 * <li>The interval for checks whether the circuit breaker should open. So it is possible
 * to specify something like "The circuit breaker should open if more than 10 errors are
 * encountered in a minute."</li>
 * <li>The same parameters can be specified for automatically closing the circuit breaker
 * again, as in "If the number of requests goes down to 100 per minute, the circuit
 * breaker should close itself again". Depending on the use case, it may make sense to use
 * a slightly lower threshold for closing the circuit breaker than for opening it to avoid
 * continuously flipping when the number of events received is close to the threshold.</li>
 * </ul>
 * <p>
 * This class supports the following typical use cases:
 * </p>
 * <p>
 * <strong>Protecting against load peaks</strong>
 * </p>
 * <p>
 * Imagine you have a server which can handle a certain number of requests per minute.
 * Suddenly, the number of requests increases significantly - maybe because a connected
 * partner system is going mad or due to a denial of service attack. A
 * {@link EventCountCircuitBreaker} can be configured to stop the application from
 * processing requests when a sudden peak load is detected and to start request processing
 * again when things calm down. The following code fragment shows a typical example of
 * such a scenario. Here the {@link EventCountCircuitBreaker} allows up to 1000 requests
 * per minute before it interferes. When the load goes down again to 800 requests per
 * second it switches back to state <em>closed</em>:
 * </p>
 *
 * <pre>
 * EventCountCircuitBreaker breaker = new EventCountCircuitBreaker(1000, 1, TimeUnit.MINUTE, 800);
 * ...
 * public void handleRequest(Request request) {
 *     if (breaker.incrementAndCheckState()) {
 *         // actually handle this request
 *     } else {
 *         // do something else, e.g. send an error code
 *     }
 * }
 * </pre>
 * <p>
 * <strong>Deal with an unreliable service</strong>
 * </p>
 * <p>
 * In this scenario, an application uses an external service which may fail from time to
 * time. If there are too many errors, the service is considered down and should not be
 * called for a while. This can be achieved using the following pattern - in this concrete
 * example we accept up to 5 errors in 2 minutes; if this limit is reached, the service is
 * given a rest time of 10 minutes:
 * </p>
 *
 * <pre>
 * EventCountCircuitBreaker breaker = new EventCountCircuitBreaker(5, 2, TimeUnit.MINUTE, 5, 10, TimeUnit.MINUTE);
 * ...
 * public void handleRequest(Request request) {
 *     if (breaker.checkState()) {
 *         try {
 *             service.doSomething();
 *         } catch (ServiceException ex) {
 *             breaker.incrementAndCheckState();
 *         }
 *     } else {
 *         // return an error code, use an alternative service, etc.
 *     }
 * }
 * </pre>
 * <p>
 * In addition to automatic state transitions, the state of a circuit breaker can be
 * changed manually using the methods {@link #open()} and {@link #close()}. It is also
 * possible to register {@link PropertyChangeListener} objects that get notified whenever
 * a state transition occurs. This is useful, for instance to directly react on a freshly
 * detected error condition.
 * </p>
 * <p>
 * <em>Implementation notes:</em>
 * </p>
 * <ul>
 * <li>This implementation uses non-blocking algorithms to update the internal counter and
 * state. This should be pretty efficient if there is not too much contention.</li>
 * <li>This implementation is not intended to operate as a high-precision timer in very
 * short check intervals. It is deliberately kept simple to avoid complex and
 * time-consuming state checks. It should work well in time intervals from a few seconds
 * up to minutes and longer. If the intervals become too short, there might be race
 * conditions causing spurious state transitions.</li>
 * <li>The handling of check intervals is a bit simplistic. Therefore, there is no
 * guarantee that the circuit breaker is triggered at a specific point in time; there may
 * be some delay (less than a check interval).</li>
 * </ul>
 * @since 3.5
 */
public class EventCountCircuitBreaker extends AbstractCircuitBreaker<Integer> {

    /** A map for accessing the strategy objects for the different states. */
    private static final Map<State, StateStrategy> STRATEGY_MAP = createStrategyMap();

    /** Stores information about the current check interval. */
    private final AtomicReference<CheckIntervalData> checkIntervalData;

    /** The threshold for opening the circuit breaker. */
    private final int openingThreshold;

    /** The time interval for opening the circuit breaker. */
    private final long openingInterval;

    /** The threshold for closing the circuit breaker. */
    private final int closingThreshold;

    /** The time interval for closing the circuit breaker. */
    private final long closingInterval;

    /**
     * Creates a new instance of {@link EventCountCircuitBreaker} and initializes all properties for
     * opening and closing it based on threshold values for events occurring in specific
     * intervals.
     *
     * @param openingThreshold the threshold for opening the circuit breaker; if this
     * number of events is received in the time span determined by the opening interval,
     * the circuit breaker is opened
     * @param openingInterval the interval for opening the circuit breaker
     * @param openingUnit the {@link TimeUnit} defining the opening interval
     * @param closingThreshold the threshold for closing the circuit breaker; if the
     * number of events received in the time span determined by the closing interval goes
     * below this threshold, the circuit breaker is closed again
     * @param closingInterval the interval for closing the circuit breaker
     * @param closingUnit the {@link TimeUnit} defining the closing interval
     */
    public EventCountCircuitBreaker(final int openingThreshold, final long openingInterval,
                                    final TimeUnit openingUnit, final int closingThreshold, final long closingInterval,
                                    final TimeUnit closingUnit) {
        checkIntervalData = new AtomicReference<>(new CheckIntervalData(0, 0));
        this.openingThreshold = openingThreshold;
        this.openingInterval = openingUnit.toNanos(openingInterval);
        this.closingThreshold = closingThreshold;
        this.closingInterval = closingUnit.toNanos(closingInterval);
    }

    /**
     * Creates a new instance of {@link EventCountCircuitBreaker} with the same interval for opening
     * and closing checks.
     *
     * @param openingThreshold the threshold for opening the circuit breaker; if this
     * number of events is received in the time span determined by the check interval, the
     * circuit breaker is opened
     * @param checkInterval the check interval for opening or closing the circuit breaker
     * @param checkUnit the {@link TimeUnit} defining the check interval
     * @param closingThreshold the threshold for closing the circuit breaker; if the
     * number of events received in the time span determined by the check interval goes
     * below this threshold, the circuit breaker is closed again
     */
    public EventCountCircuitBreaker(final int openingThreshold, final long checkInterval, final TimeUnit checkUnit,
                                    final int closingThreshold) {
        this(openingThreshold, checkInterval, checkUnit, closingThreshold, checkInterval,
                checkUnit);
    }

    /**
     * Creates a new instance of {@link EventCountCircuitBreaker} which uses the same parameters for
     * opening and closing checks.
     *
     * @param threshold the threshold for changing the status of the circuit breaker; if
     * the number of events received in a check interval is greater than this value, the
     * circuit breaker is opened; if it is lower than this value, it is closed again
     * @param checkInterval the check interval for opening or closing the circuit breaker
     * @param checkUnit the {@link TimeUnit} defining the check interval
     */
    public EventCountCircuitBreaker(final int threshold, final long checkInterval, final TimeUnit checkUnit) {
        this(threshold, checkInterval, checkUnit, threshold);
    }

    /**
     * Returns the threshold value for opening the circuit breaker. If this number of
     * events is received in the time span determined by the opening interval, the circuit
     * breaker is opened.
     *
     * @return the opening threshold
     */
    public int getOpeningThreshold() {
        return openingThreshold;
    }

    /**
     * Returns the interval (in nanoseconds) for checking for the opening threshold.
     *
     * @return the opening check interval
     */
    public long getOpeningInterval() {
        return openingInterval;
    }

    /**
     * Returns the threshold value for closing the circuit breaker. If the number of
     * events received in the time span determined by the closing interval goes below this
     * threshold, the circuit breaker is closed again.
     *
     * @return the closing threshold
     */
    public int getClosingThreshold() {
        return closingThreshold;
    }

    /**
     * Returns the interval (in nanoseconds) for checking for the closing threshold.
     *
     * @return the opening check interval
     */
    public long getClosingInterval() {
        return closingInterval;
    }

    /**
     * {@inheritDoc} This implementation checks the internal event counter against the
     * threshold values and the check intervals. This may cause a state change of this
     * circuit breaker.
     */
    @Override
    public boolean checkState() {
        return performStateCheck(0);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean incrementAndCheckState(final Integer increment) {
        return performStateCheck(increment);
    }

    /**
     * Increments the monitored value by <strong>1</strong> and performs a check of the current state of this
     * circuit breaker. This method works like {@link #checkState()}, but the monitored
     * value is incremented before the state check is performed.
     *
     * @return <strong>true</strong> if the circuit breaker is now closed;
     * <strong>false</strong> otherwise
     */
    public boolean incrementAndCheckState() {
        return incrementAndCheckState(1);
    }

    /**
     * {@inheritDoc} This circuit breaker may close itself again if the number of events
     * received during a check interval goes below the closing threshold. If this circuit
     * breaker is already open, this method has no effect, except that a new check
     * interval is started.
     */
    @Override
    public void open() {
        super.open();
        checkIntervalData.set(new CheckIntervalData(0, nanoTime()));
    }

    /**
     * {@inheritDoc} A new check interval is started. If too many events are received in
     * this interval, the circuit breaker changes again to state open. If this circuit
     * breaker is already closed, this method has no effect, except that a new check
     * interval is started.
     */
    @Override
    public void close() {
        super.close();
        checkIntervalData.set(new CheckIntervalData(0, nanoTime()));
    }

    /**
     * Actually checks the state of this circuit breaker and executes a state transition
     * if necessary.
     *
     * @param increment the increment for the internal counter
     * @return a flag whether the circuit breaker is now closed
     */
    private boolean performStateCheck(final int increment) {
        CheckIntervalData currentData;
        CheckIntervalData nextData;
        State currentState;

        do {
            final long time = nanoTime();
            currentState = state.get();
            currentData = checkIntervalData.get();
            nextData = nextCheckIntervalData(increment, currentData, currentState, time);
        } while (!updateCheckIntervalData(currentData, nextData));

        // This might cause a race condition if other changes happen in between!
        // Refer to the header comment!
        if (stateStrategy(currentState).isStateTransition(this, currentData, nextData)) {
            currentState = currentState.oppositeState();
            changeStateAndStartNewCheckInterval(currentState);
        }
        return !isOpen(currentState);
    }

    /**
     * Updates the {@link CheckIntervalData} object. The current data object is replaced
     * by the one modified by the last check. The return value indicates whether this was
     * successful. If it is <strong>false</strong>, another thread interfered, and the
     * whole operation has to be redone.
     *
     * @param currentData the current check data object
     * @param nextData the replacing check data object
     * @return a flag whether the update was successful
     */
    private boolean updateCheckIntervalData(final CheckIntervalData currentData,
            final CheckIntervalData nextData) {
        return currentData == nextData
                || checkIntervalData.compareAndSet(currentData, nextData);
    }

    /**
     * Changes the state of this circuit breaker and also initializes a new
     * {@link CheckIntervalData} object.
     *
     * @param newState the new state to be set
     */
    private void changeStateAndStartNewCheckInterval(final State newState) {
        changeState(newState);
        checkIntervalData.set(new CheckIntervalData(0, nanoTime()));
    }

    /**
     * Calculates the next {@link CheckIntervalData} object based on the current data and
     * the current state. The next data object takes the counter increment and the current
     * time into account.
     *
     * @param increment the increment for the internal counter
     * @param currentData the current check data object
     * @param currentState the current state of the circuit breaker
     * @param time the current time
     * @return the updated {@link CheckIntervalData} object
     */
    private CheckIntervalData nextCheckIntervalData(final int increment,
            final CheckIntervalData currentData, final State currentState, final long time) {
        final CheckIntervalData nextData;
        if (stateStrategy(currentState).isCheckIntervalFinished(this, currentData, time)) {
            nextData = new CheckIntervalData(increment, time);
        } else {
            nextData = currentData.increment(increment);
        }
        return nextData;
    }

    /**
     * Returns the current time in nanoseconds. This method is used to obtain the current
     * time. This is needed to calculate the check intervals correctly.
     *
     * @return the current time in nanoseconds
     */
    long nanoTime() {
        return System.nanoTime();
    }

    /**
     * Returns the {@link StateStrategy} object responsible for the given state.
     *
     * @param state the state
     * @return the corresponding {@link StateStrategy}
     * @throws CircuitBreakingException if the strategy cannot be resolved
     */
    private static StateStrategy stateStrategy(final State state) {
        return STRATEGY_MAP.get(state);
    }

    /**
     * Creates the map with strategy objects. It allows access for a strategy for a given
     * state.
     *
     * @return the strategy map
     */
    private static Map<State, StateStrategy> createStrategyMap() {
        final Map<State, StateStrategy> map = new EnumMap<>(State.class);
        map.put(State.CLOSED, new StateStrategyClosed());
        map.put(State.OPEN, new StateStrategyOpen());
        return map;
    }

    /**
     * An internally used data class holding information about the checks performed by
     * this class. Basically, the number of received events and the start time of the
     * current check interval are stored.
     */
    private static class CheckIntervalData {
        /** The counter for events. */
        private final int eventCount;

        /** The start time of the current check interval. */
        private final long checkIntervalStart;

        /**
         * Creates a new instance of {@link CheckIntervalData}.
         *
         * @param count the current count value
         * @param intervalStart the start time of the check interval
         */
        CheckIntervalData(final int count, final long intervalStart) {
            eventCount = count;
            checkIntervalStart = intervalStart;
        }

        /**
         * Returns the event counter.
         *
         * @return the number of received events
         */
        public int getEventCount() {
            return eventCount;
        }

        /**
         * Returns the start time of the current check interval.
         *
         * @return the check interval start time
         */
        public long getCheckIntervalStart() {
            return checkIntervalStart;
        }

        /**
         * Returns a new instance of {@link CheckIntervalData} with the event counter
         * incremented by the given delta. If the delta is 0, this object is returned.
         *
         * @param delta the delta
         * @return the updated instance
         */
        public CheckIntervalData increment(final int delta) {
            return (delta == 0) ? this : new CheckIntervalData(getEventCount() + delta,
                    getCheckIntervalStart());
        }
    }

    /**
     * Internally used class for executing check logic based on the current state of the
     * circuit breaker. Having this logic extracted into special classes avoids complex
     * if-then-else cascades.
     */
    private abstract static class StateStrategy {
        /**
         * Returns a flag whether the end of the current check interval is reached.
         *
         * @param breaker the {@link CircuitBreaker}
         * @param currentData the current state object
         * @param now the current time
         * @return a flag whether the end of the current check interval is reached
         */
        public boolean isCheckIntervalFinished(final EventCountCircuitBreaker breaker,
                final CheckIntervalData currentData, final long now) {
            return now - currentData.getCheckIntervalStart() > fetchCheckInterval(breaker);
        }

        /**
         * Checks whether the specified {@link CheckIntervalData} objects indicate that a
         * state transition should occur. Here the logic which checks for thresholds
         * depending on the current state is implemented.
         *
         * @param breaker the {@link CircuitBreaker}
         * @param currentData the current {@link CheckIntervalData} object
         * @param nextData the updated {@link CheckIntervalData} object
         * @return a flag whether a state transition should be performed
         */
        public abstract boolean isStateTransition(EventCountCircuitBreaker breaker,
                CheckIntervalData currentData, CheckIntervalData nextData);

        /**
         * Obtains the check interval to applied for the represented state from the given
         * {@link CircuitBreaker}.
         *
         * @param breaker the {@link CircuitBreaker}
         * @return the check interval to be applied
         */
        protected abstract long fetchCheckInterval(EventCountCircuitBreaker breaker);
    }

    /**
     * A specialized {@link StateStrategy} implementation for the state closed.
     */
    private static class StateStrategyClosed extends StateStrategy {

        /**
         * {@inheritDoc}
         */
        @Override
        public boolean isStateTransition(final EventCountCircuitBreaker breaker,
                final CheckIntervalData currentData, final CheckIntervalData nextData) {
            return nextData.getEventCount() > breaker.getOpeningThreshold();
        }

        /**
         * {@inheritDoc}
         */
        @Override
        protected long fetchCheckInterval(final EventCountCircuitBreaker breaker) {
            return breaker.getOpeningInterval();
        }
    }

    /**
     * A specialized {@link StateStrategy} implementation for the state open.
     */
    private static class StateStrategyOpen extends StateStrategy {
        /**
         * {@inheritDoc}
         */
        @Override
        public boolean isStateTransition(final EventCountCircuitBreaker breaker,
                final CheckIntervalData currentData, final CheckIntervalData nextData) {
            return nextData.getCheckIntervalStart() != currentData
                    .getCheckIntervalStart()
                    && currentData.getEventCount() < breaker.getClosingThreshold();
        }

        /**
         * {@inheritDoc}
         */
        @Override
        protected long fetchCheckInterval(final EventCountCircuitBreaker breaker) {
            return breaker.getClosingInterval();
        }
    }

}
