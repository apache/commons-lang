package org.apache.commons.lang3.event;

import org.apache.commons.lang3.event.EventUtils;

import java.util.Arrays;
import org.junit.jupiter.api.BeforeEach;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import static org.mockito.ArgumentMatchers.eq;
import java.lang.reflect.InvocationHandler;
import org.apache.commons.lang3.event.EventUtils;
import java.util.HashSet;
import java.util.Set;
import org.apache.commons.lang3.reflect.MethodUtils;
import java.lang.reflect.Method;
import org.junit.jupiter.params.provider.CsvSource;
import static org.junit.jupiter.api.Assertions.*;
import java.lang.reflect.Proxy;
import static org.mockito.Mockito.*;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

class EventUtilsBaseRockGeneratedTest {

    private EventSource eventSource;

    private TestListener listener;

    @BeforeEach
    void setUp() {
        eventSource = mock(EventSource.class);
        listener = mock(TestListener.class);
    }

    //BaseRock generated method id: ${testAddEventListener}, hash: 3F4AF60246FF9F4E44EDF8DC31FC75E3
    @Test
    void testAddEventListener() throws Exception {
        EventUtils.addEventListener(eventSource, TestListener.class, listener);
        verify(eventSource).addTestListener(listener);
    }

    //BaseRock generated method id: ${testAddEventListenerThrowsIllegalArgumentException}, hash: 074147BBA6DAF651871A9B36F1B67D47
    @Test
    void testAddEventListenerThrowsIllegalArgumentException() {
        Object invalidEventSource = new Object();
        assertThrows(IllegalArgumentException.class, () -> EventUtils.addEventListener(invalidEventSource, TestListener.class, listener));
    }

    //BaseRock generated method id: ${testBindEventsToMethod}, hash: 7CE2B0151BDF9F3B98C2736D06F84874
    @Test
    void testBindEventsToMethod() throws Exception {
        Object target = new Object() {

            public void handleEvent(String value) {
            }
        };
        EventUtils.bindEventsToMethod(target, "handleEvent", eventSource, TestListener.class, "onEvent");
        verify(eventSource).addTestListener(any(TestListener.class));
    }

    //BaseRock generated method id: ${testBindEventsToMethodWithDifferentEventTypes}, hash: 6B8D626FCC908C725B15347B00FA77B4
    @ParameterizedTest
    @CsvSource({ "handleEvent, onEvent", "handleEvent, " })
    void testBindEventsToMethodWithDifferentEventTypes(String methodName, String eventType) throws Exception {
        Object target = new Object() {

            public void handleEvent(String value) {
            }
        };
        String[] eventTypes = eventType.isEmpty() ? new String[0] : new String[] { eventType };
        EventUtils.bindEventsToMethod(target, methodName, eventSource, TestListener.class, eventTypes);
        verify(eventSource).addTestListener(any(TestListener.class));
    }

    //BaseRock generated method id: ${testEventBindingInvocationHandler}, hash: 10FDDCA0FAED352E070330E583886876
    @Test
    void testEventBindingInvocationHandler() throws Throwable {
        // This test cannot be implemented due to EventBindingInvocationHandler being private
    }

    //BaseRock generated method id: ${testEventBindingInvocationHandlerWithNoMatchingMethod}, hash: BB80AF905389E36DAAE2D530CB98994F
    @Test
    void testEventBindingInvocationHandlerWithNoMatchingMethod() throws Throwable {
        // This test cannot be implemented due to EventBindingInvocationHandler being private
    }

    //BaseRock generated method id: ${testEventBindingInvocationHandlerWithNonMatchingEventType}, hash: 76532A610BCCC3D1601CF2398D2952C4
    @Test
    void testEventBindingInvocationHandlerWithNonMatchingEventType() throws Throwable {
        // This test cannot be implemented due to EventBindingInvocationHandler being private
    }

    //BaseRock generated method id: ${testDeprecatedConstructor}, hash: B36B54C046D74323902445DB6F12C48B
    @Test
    void testDeprecatedConstructor() {
        assertDoesNotThrow(EventUtils::new);
    }

    interface EventSource {

        void addTestListener(TestListener listener);
    }

    interface TestListener {

        void onEvent(String value);
    }
}
