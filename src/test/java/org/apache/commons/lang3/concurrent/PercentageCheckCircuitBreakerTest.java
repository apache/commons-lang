package org.apache.commons.lang3.concurrent;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.Test;

class PercentageCheckCircuitBreakerTest {

    private static final long RESET_TIME = 10;
    private static final long CLOSING_TIME = 5;

    @Test
    void whenFailurePercentageHigherThanAcceptablePercentageThenCircuitOpen() {
        final PercentageCheckCircuitBreaker breaker = new PercentageCheckCircuitBreaker(80,
                                                                                        5,
                                                                                        RESET_TIME,
                                                                                        TimeUnit.SECONDS,
                                                                                        CLOSING_TIME,
                                                                                        TimeUnit.SECONDS);

        breaker.incrementSuccessAndCheckState();
        breaker.incrementFailureAndCheckState();
        breaker.incrementFailureAndCheckState();
        breaker.incrementFailureAndCheckState();
        breaker.incrementFailureAndCheckState();

        assertTrue(breaker.isOpen());
        assertFalse(breaker.isClosed());
    }

    @Test
    void whenFailurePercentageEqualToAcceptablePercentageThenCircuitOpen() {
        final PercentageCheckCircuitBreaker breaker = new PercentageCheckCircuitBreaker(50.0,
                                                                                        6,
                                                                                        RESET_TIME,
                                                                                        TimeUnit.SECONDS,
                                                                                        CLOSING_TIME,
                                                                                        TimeUnit.SECONDS);

        breaker.incrementSuccessAndCheckState();
        breaker.incrementSuccessAndCheckState();
        breaker.incrementSuccessAndCheckState();
        breaker.incrementFailureAndCheckState();
        breaker.incrementFailureAndCheckState();
        breaker.incrementFailureAndCheckState();

        assertTrue(breaker.isOpen());
        assertFalse(breaker.isClosed());
    }

    @Test
    void whenFailurePercentageLessThanAcceptablePercentageThenCircuitClosed() {
        final PercentageCheckCircuitBreaker breaker = new PercentageCheckCircuitBreaker(80.0,
                                                                                        5,
                                                                                        RESET_TIME,
                                                                                        TimeUnit.SECONDS,
                                                                                        CLOSING_TIME,
                                                                                        TimeUnit.SECONDS);

        breaker.incrementSuccessAndCheckState();
        breaker.incrementSuccessAndCheckState();
        breaker.incrementSuccessAndCheckState();
        breaker.incrementSuccessAndCheckState();
        breaker.incrementFailureAndCheckState();

        assertTrue(breaker.isClosed());
        assertFalse(breaker.isOpen());
    }

    @Test
    void whenPercentageHigherThanAcceptablePercentageThenCircuitOpen() {
        final PercentageCheckCircuitBreaker breaker = new PercentageCheckCircuitBreaker(80,
                                                                                        2,
                                                                                        RESET_TIME,
                                                                                        TimeUnit.SECONDS,
                                                                                        CLOSING_TIME,
                                                                                        TimeUnit.SECONDS);

        breaker.incrementAndCheckState(1);
        breaker.incrementAndCheckState(1);

        assertTrue(breaker.isOpen());
        assertFalse(breaker.isClosed());
    }

    @Test
    void whenPercentageEqualToAcceptablePercentageThenCircuitOpen() {
        final PercentageCheckCircuitBreaker breaker = new PercentageCheckCircuitBreaker(50,
                                                                                        2,
                                                                                        RESET_TIME,
                                                                                        TimeUnit.SECONDS,
                                                                                        CLOSING_TIME,
                                                                                        TimeUnit.SECONDS);

        breaker.incrementAndCheckState(1);

        assertTrue(breaker.isOpen());
        assertFalse(breaker.isClosed());
    }

    @Test
    void whenPercentageLessThanAcceptablePercentageThenCircuitClosed() {
        final PercentageCheckCircuitBreaker breaker = new PercentageCheckCircuitBreaker(80,
                                                                                        2,
                                                                                        RESET_TIME,
                                                                                        TimeUnit.SECONDS,
                                                                                        CLOSING_TIME,
                                                                                        TimeUnit.SECONDS);

        breaker.incrementAndCheckState(1);

        assertTrue(breaker.isClosed());
        assertFalse(breaker.isOpen());
    }

    @Test
    void whenClosingTimeFinishedThenCircuitClosed() throws InterruptedException {
        final PercentageCheckCircuitBreaker breaker = new PercentageCheckCircuitBreaker(80,
                                                                                        2,
                                                                                        RESET_TIME,
                                                                                        TimeUnit.SECONDS,
                                                                                        CLOSING_TIME,
                                                                                        TimeUnit.SECONDS);
        breaker.incrementFailureAndCheckState();
        breaker.incrementFailureAndCheckState();

        assertTrue(breaker.isOpen());
        assertFalse(breaker.isClosed());

        Thread.sleep(6000);

        assertTrue(breaker.isClosed());
        assertFalse(breaker.isOpen());
    }

    @Test
    void whenResetTimeOccurredThenCircuitClosed() throws InterruptedException {
        final PercentageCheckCircuitBreaker breaker = new PercentageCheckCircuitBreaker(80,
                                                                                        2,
                                                                                        2,
                                                                                        TimeUnit.SECONDS,
                                                                                        10,
                                                                                        TimeUnit.SECONDS);
        breaker.incrementFailureAndCheckState();
        breaker.incrementFailureAndCheckState();

        assertTrue(breaker.isOpen());
        assertFalse(breaker.isClosed());

        Thread.sleep(3000);

        assertTrue(breaker.isClosed());
        assertFalse(breaker.isOpen());
    }
}
