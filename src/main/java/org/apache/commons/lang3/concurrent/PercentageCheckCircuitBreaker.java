package org.apache.commons.lang3.concurrent;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;

/**
 * <p>
 * A simple implementation of the <a
 * href="http://martinfowler.com/bliki/CircuitBreaker.html">Circuit Breaker</a> pattern
 * that counts specific events.
 * </p>
 * <p>
 * A <em>circuit breaker</em> can be used to protect an application against unreliable
 * services or unexpected load. A newly created {@code PercentageCheckCircuitBreaker} object is
 * initially in state <em>closed</em> meaning that no problem has been detected. When the
 * application encounters specific events (like errors or service timeouts), it tells the
 * circuit breaker to increment the internal counters.
 * If the number of events reported results in the higher than acceptable percentage for a window size,
 * the circuit breaker changes into state <em>open</em>. This means that there is a problem with the associated sub
 * system; the application should no longer call it, but give it some time to settle down.
 * The circuit breaker can be configured to switch back to <em>closed</em> state after a
 * certain time frame.
 * </p>
 * <p>
 *
 * <strong>Deal with an unreliable service</strong>
 * </p>
 * <p>
 * In this scenario, an application uses an external service which may fail from time to
 * time. If there are too many errors, the service is considered down and should not be
 * called for a while. This can be achieved using the following pattern - in this concrete
 * example we accept up to 80 percentage exceptions in the window size of 1000 if this limit is reached,
 * the service is given a rest time of 10 minutes.
 * The circuit breaker reset in 20 minutes.
 *
 * </p>
 *
 * <pre>
 * final PercentageCheckCircuitBreaker breaker = new PercentageCheckCircuitBreaker(80.0, 1000, 20, TimeUnit.MINUTE, 10, TimeUnit.MINUTE);
 * ...
 * public void handleRequest(Request request) {
 *     if (breaker.checkState()) {
 *         try {
 *             service.doSomething();
 *             breaker.incrementSuccessAndCheckState();
 *         } catch (ServiceException ex) {
 *             breaker.incrementFailureAndCheckState();
 *         }
 *     } else {
 *         // return an error code, use an alternative service, etc.
 *     }
 * }
 * </pre>
 * <p>
 * <strong>In case when number of times you need to call the service is known already</strong>
 * </p>
 * <p>
 * Imagine you know that how many times you need to call the service.
 * If there are too many calls, the service is considered down and should not be
 * called for a while. This can be achieved using the following pattern - in this concrete
 * example we accept up to 80 percentage calls in the window size of 1000 if this limit is reached,
 * the service is given a rest time of 10 minutes.
 * The circuit breaker reset in 20 minutes.
 * </p>
 *
 * <pre>
 * final PercentageCheckCircuitBreaker breaker = new PercentageCheckCircuitBreaker(80.0, 1000, 20, TimeUnit.MINUTE, 10, TimeUnit.MINUTE);
 * ...
 * public void handleRequest(Request request) {
 *     if (breaker.incrementAndCheckState(1)) {
 *         // actually handle this request
 *     } else {
 *         // do something else, e.g. send an error code
 *     }
 * }
 * </pre>
 * <p>
 *
 * @since 3.5
 */
public class PercentageCheckCircuitBreaker implements CircuitBreaker<Integer> {

    private static final int INITIAL_COUNT = 0;

    private enum State {OPEN, CLOSED}

    private final AtomicReference<State> state = new AtomicReference<>(State.CLOSED);
    private final AtomicInteger successCount = new AtomicInteger(INITIAL_COUNT);
    private final AtomicInteger failureCount = new AtomicInteger(INITIAL_COUNT);
    private final AtomicInteger used = new AtomicInteger(INITIAL_COUNT);
    private final AtomicLong windowResetTimeInNanos = new AtomicLong(INITIAL_COUNT);
    private final AtomicLong closingTimeInNanos = new AtomicLong(INITIAL_COUNT);

    private final double acceptablePercentage;
    private final int windowSize;
    private final long windowResetInterval;
    private final long closingInterval;

    /**
     * Creates a new instance of {@code PercentageCheckCircuitBreaker}.
     *
     * @param acceptablePercentage the maximum percentage allowed in the given window.
     * @param windowSize the window size for evaluating percentage
     * @param windowResetInterval the check interval for resetting the circuit breaker
     * @param windowResetUnit the {@code TimeUnit} defining the reset interval
     * @param closingInterval the check interval for closing the circuit breaker after opening
     * @param closingUnit the {@code TimeUnit} defining the closing interval
     */
    public PercentageCheckCircuitBreaker(final double acceptablePercentage,
                                         final int windowSize,
                                         final long windowResetInterval,
                                         final TimeUnit windowResetUnit,
                                         final long closingInterval,
                                         final TimeUnit closingUnit) {
        this.acceptablePercentage = acceptablePercentage;
        this.windowSize = windowSize;
        this.windowResetInterval = windowResetUnit.toNanos(windowResetInterval);
        this.closingInterval = closingUnit.toNanos(closingInterval);
    }

    /**
     * Returns the current open state of this circuit breaker. A return value of
     * <strong>true</strong> means that the circuit breaker is currently open indicating a
     * problem in the monitored sub system.
     *
     * @return the current open state of this circuit breaker
     */
    @Override
    public boolean isOpen() {
        return !checkState();
    }

    /**
     * Returns the current closed state of this circuit breaker. A return value of
     * <strong>true</strong> means that the circuit breaker is currently closed. This
     * means that everything is okay with the monitored sub system.
     *
     * @return the current closed state of this circuit breaker
     */
    @Override
    public boolean isClosed() {
        return checkState();
    }

    /**
     * Checks the state of this circuit breaker and changes it if necessary. The return
     * value indicates whether the circuit breaker is now in state {@code CLOSED}; a value
     * of <strong>true</strong> typically means that the current operation can continue.
     *
     * @return <strong>true</strong> if the circuit breaker is now closed;
     * <strong>false</strong> otherwise
     */
    @Override
    public boolean checkState() {
        performStateCheck(used.get(), windowSize);

        final int total = successCount.get() + failureCount.get();
        if (total >= windowSize) {
            performStateCheck(failureCount.get(), total);
        }

        return state.get() == State.CLOSED;
    }

    /**
     * Closes this circuit breaker. Its state is changed to closed. If this circuit
     * breaker is already closed, this method has no effect.
     */
    @Override
    public void close() {
        state.set(State.CLOSED);
    }

    /**
     * Opens this circuit breaker. Its state is changed to open.
     */
    @Override
    public void open() {
        resetCounters();
        state.set(State.OPEN);
    }

    /**
     * Increments the monitored value and performs a check of the current state of this
     * circuit breaker. This method works like {@link #checkState()}, but the monitored
     * value is incremented before the state check is performed.
     */
    @Override
    public boolean incrementAndCheckState(final Integer increment) {
        this.used.addAndGet(increment);

        return checkState();
    }

    /**
     * Increments the success monitored value and performs a check of the current state of this
     * circuit breaker. This method works like {@link #checkState()}, but the monitored
     * value is incremented before the state check is performed.
     */
    public boolean incrementSuccessAndCheckState() {
        successCount.incrementAndGet();

        return checkState();
    }

    /**
     * Increments the failure monitored value and performs a check of the current state of this
     * circuit breaker. This method works like {@link #checkState()}, but the monitored
     * value is incremented before the state check is performed.
     */
    public boolean incrementFailureAndCheckState() {
        failureCount.incrementAndGet();

        return checkState();
    }

    private void performStateCheck(final int count,
                                   final int totalCount) {
        final double percentage = (count * 100.0) / totalCount;
        final boolean percentageGreaterThanAcceptable = percentage >= acceptablePercentage;

        if (percentageGreaterThanAcceptable) {
            closingTimeInNanos.set(System.nanoTime() + closingInterval);
            open();
        }

        if (closingTimeInNanos.get() <= System.nanoTime()) {
            closingTimeInNanos.set(INITIAL_COUNT);
            close();
        }

        performWindowReset();
    }

    private void performWindowReset() {
        if (windowResetTimeInNanos.get() == INITIAL_COUNT) {
            windowResetTimeInNanos.set(System.nanoTime() + windowResetInterval);
        } else if (windowResetTimeInNanos.get() <= System.nanoTime()) {
            reset();
            close();
        }
    }

    private void reset() {
        windowResetTimeInNanos.set(INITIAL_COUNT);
        closingTimeInNanos.set(INITIAL_COUNT);
        resetCounters();
    }

    private void resetCounters() {
        successCount.set(INITIAL_COUNT);
        failureCount.set(INITIAL_COUNT);
        used.set(INITIAL_COUNT);
    }
}
