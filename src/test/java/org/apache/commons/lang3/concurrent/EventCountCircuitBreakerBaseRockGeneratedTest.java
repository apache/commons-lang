package org.apache.commons.lang3.concurrent;

import org.apache.commons.lang3.concurrent.EventCountCircuitBreaker;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.provider.CsvSource;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.params.ParameterizedTest;
import java.util.concurrent.TimeUnit;
import static org.mockito.Mockito.*;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

class EventCountCircuitBreakerBaseRockGeneratedTest {

    private EventCountCircuitBreaker circuitBreaker;

    @BeforeEach
    void setUp() {
        circuitBreaker = spy(new EventCountCircuitBreaker(5, 1000, TimeUnit.MILLISECONDS, 3));
    }

    //BaseRock generated method id: ${testConstructorWithThreeParameters}, hash: 3D8E3DDED4077D0945CDABB5A2E58194
    @Test
    void testConstructorWithThreeParameters() {
        EventCountCircuitBreaker breaker = new EventCountCircuitBreaker(10, 2000, TimeUnit.MILLISECONDS);
        assertEquals(10, breaker.getOpeningThreshold());
        assertEquals(10, breaker.getClosingThreshold());
        assertEquals(TimeUnit.MILLISECONDS.toNanos(2000), breaker.getOpeningInterval());
        assertEquals(TimeUnit.MILLISECONDS.toNanos(2000), breaker.getClosingInterval());
    }

    //BaseRock generated method id: ${testConstructorWithFourParameters}, hash: 57A15DC25FA5E63280C2624CA0D4E66F
    @Test
    void testConstructorWithFourParameters() {
        EventCountCircuitBreaker breaker = new EventCountCircuitBreaker(10, 2000, TimeUnit.MILLISECONDS, 5);
        assertEquals(10, breaker.getOpeningThreshold());
        assertEquals(5, breaker.getClosingThreshold());
        assertEquals(TimeUnit.MILLISECONDS.toNanos(2000), breaker.getOpeningInterval());
        assertEquals(TimeUnit.MILLISECONDS.toNanos(2000), breaker.getClosingInterval());
    }

    //BaseRock generated method id: ${testConstructorWithSixParameters}, hash: BD4B80A9EFACCAD376B8B89B2924B7D0
    @Test
    void testConstructorWithSixParameters() {
        EventCountCircuitBreaker breaker = new EventCountCircuitBreaker(10, 2000, TimeUnit.MILLISECONDS, 5, 1000, TimeUnit.MILLISECONDS);
        assertEquals(10, breaker.getOpeningThreshold());
        assertEquals(5, breaker.getClosingThreshold());
        assertEquals(TimeUnit.MILLISECONDS.toNanos(2000), breaker.getOpeningInterval());
        assertEquals(TimeUnit.MILLISECONDS.toNanos(1000), breaker.getClosingInterval());
    }

    //BaseRock generated method id: ${testCheckState}, hash: 7D55A53BC8251477BC9CEF8559984AC0
    @Test
    void testCheckState() {
        assertTrue(circuitBreaker.checkState());
    }

    //BaseRock generated method id: ${testClose}, hash: E6DD1EE5497FA059E0FDD5DCEE21FD3C
    @Test
    void testClose() {
        circuitBreaker.open();
        circuitBreaker.close();
        assertTrue(circuitBreaker.isClosed());
    }

    //BaseRock generated method id: ${testOpen}, hash: D113918C1FF46ECB162498D65F010C70
    @Test
    void testOpen() {
        circuitBreaker.open();
        assertTrue(circuitBreaker.isOpen());
    }

    //BaseRock generated method id: ${testIncrementAndCheckState}, hash: 8E40CE690AE6851D2C5B9A7D9D23BAFC
    @Test
    void testIncrementAndCheckState() {
        assertTrue(circuitBreaker.incrementAndCheckState());
    }

    //BaseRock generated method id: ${testIncrementAndCheckStateWithDifferentValues}, hash: D3860CC38E73F3BC9C90174EDB936953
    @ParameterizedTest
    @CsvSource({ "1, true", "5, true", "6, false" })
    void testIncrementAndCheckStateWithDifferentValues(int increment, boolean expected) {
        for (int i = 0; i < increment; i++) {
            circuitBreaker.incrementAndCheckState();
        }
        assertEquals(expected, circuitBreaker.checkState());
    }

    //BaseRock generated method id: ${testStateTransitionFromClosedToOpen}, hash: 77ADDF5993163ADA3B8D87D47207AD71
    @Test
    void testStateTransitionFromClosedToOpen() {
        for (int i = 0; i < 5; i++) {
            assertTrue(circuitBreaker.incrementAndCheckState());
        }
        assertFalse(circuitBreaker.incrementAndCheckState());
        assertTrue(circuitBreaker.isOpen());
    }

    //BaseRock generated method id: ${testStateTransitionFromOpenToClosed}, hash: F9D58B5299E9F2DD85B77B634DBF68D7
    @Test
    void testStateTransitionFromOpenToClosed() throws InterruptedException {
        // Open the circuit breaker
        for (int i = 0; i < 6; i++) {
            circuitBreaker.incrementAndCheckState();
        }
        assertTrue(circuitBreaker.isOpen());
        // Wait for the closing interval to pass
        Thread.sleep(1100);
        // The circuit breaker should now be closed
        assertTrue(circuitBreaker.checkState());
        assertTrue(circuitBreaker.isClosed());
    }

    //BaseRock generated method id: ${testNanoTime}, hash: 28A785A68B922ADB26F5ABE8C85FD1D3
    @Test
    void testNanoTime() {
        long time = System.nanoTime();
        when(circuitBreaker.nanoTime()).thenReturn(time);
        assertEquals(time, circuitBreaker.nanoTime());
    }
}
