package org.apache.commons.lang3.event;

import org.apache.commons.lang3.event.EventListenerSupport;

import java.io.ObjectOutputStream;
import org.junit.jupiter.params.provider.MethodSource;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import org.junit.jupiter.api.Test;
import java.lang.reflect.InvocationHandler;
import java.util.concurrent.CopyOnWriteArrayList;
import org.apache.commons.lang3.function.FailableConsumer;
import java.io.*;
import org.apache.commons.lang3.exception.ExceptionUtils;
import java.lang.reflect.Method;
import java.io.Serializable;
import org.apache.commons.lang3.Validate;
import org.mockito.Mockito;
import static org.mockito.Mockito.*;
import java.io.IOException;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.params.ParameterizedTest;
import static org.mockito.ArgumentMatchers.eq;
import java.util.Objects;
import org.apache.commons.lang3.ArrayUtils;
import java.util.stream.Stream;
import java.lang.reflect.InvocationTargetException;
import org.apache.commons.lang3.event.EventListenerSupport;
import java.util.ArrayList;
import static org.junit.jupiter.api.Assertions.*;
import java.lang.reflect.Proxy;
import org.junit.jupiter.params.provider.Arguments;
import java.io.ObjectInputStream;
import java.io.ByteArrayOutputStream;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

class EventListenerSupportBaseRockGeneratedTest {

    private EventListenerSupport<TestListener> eventListenerSupport;

    private TestListener mockListener;

    interface TestListener {

        void testMethod();

        void exceptionThrowingMethod() throws Exception;
    }

    @BeforeEach
    void setUp() {
        eventListenerSupport = EventListenerSupport.create(TestListener.class);
        mockListener = mock(TestListener.class);
    }

    //BaseRock generated method id: ${testCreate}, hash: F9F52EF21A48805F5BEC1BE7F3ADED99
    @Test
    void testCreate() {
        assertNotNull(eventListenerSupport);
    }

    //BaseRock generated method id: ${testAddListener}, hash: 72AAE11F197B3F926057FD0957713204
    @Test
    void testAddListener() {
        eventListenerSupport.addListener(mockListener);
        assertEquals(1, eventListenerSupport.getListenerCount());
    }

    //BaseRock generated method id: ${testAddListenerWithDuplicate}, hash: A41229A626A72AEC7FAAAC074219C18A
    @Test
    void testAddListenerWithDuplicate() {
        eventListenerSupport.addListener(mockListener);
        eventListenerSupport.addListener(mockListener, true);
        assertEquals(2, eventListenerSupport.getListenerCount());
    }

    //BaseRock generated method id: ${testAddListenerWithoutDuplicate}, hash: 98BF37B955F753647931F13115E98AE8
    @Test
    void testAddListenerWithoutDuplicate() {
        eventListenerSupport.addListener(mockListener);
        eventListenerSupport.addListener(mockListener, false);
        assertEquals(1, eventListenerSupport.getListenerCount());
    }

    //BaseRock generated method id: ${testRemoveListener}, hash: 0ADF72BF7ADE751C6CE44A67382923D6
    @Test
    void testRemoveListener() {
        eventListenerSupport.addListener(mockListener);
        eventListenerSupport.removeListener(mockListener);
        assertEquals(0, eventListenerSupport.getListenerCount());
    }

    //BaseRock generated method id: ${testGetListeners}, hash: 723B70B13C5703B7A92E8608C5329535
    @Test
    void testGetListeners() {
        eventListenerSupport.addListener(mockListener);
        TestListener[] listeners = eventListenerSupport.getListeners();
        assertEquals(1, listeners.length);
        assertSame(mockListener, listeners[0]);
    }

    //BaseRock generated method id: ${testFire}, hash: F33790C3E2A77DAFF6BF78BFFA8CC40B
    @Test
    void testFire() {
        eventListenerSupport.addListener(mockListener);
        eventListenerSupport.fire().testMethod();
        verify(mockListener, times(1)).testMethod();
    }

    //BaseRock generated method id: ${testFireWithMultipleListeners}, hash: BAE83F8582C7EBA93A7B9905D5B6AC01
    @Test
    void testFireWithMultipleListeners() {
        TestListener mockListener2 = mock(TestListener.class);
        eventListenerSupport.addListener(mockListener);
        eventListenerSupport.addListener(mockListener2);
        eventListenerSupport.fire().testMethod();
        verify(mockListener, times(1)).testMethod();
        verify(mockListener2, times(1)).testMethod();
    }

    //BaseRock generated method id: ${testFireWithException}, hash: 25278ADDB261C39607FEFCE7F77D085B
    @Test
    void testFireWithException() {
        eventListenerSupport.addListener(mockListener);
        doThrow(new RuntimeException("Test exception")).when(mockListener).testMethod();
        assertDoesNotThrow(() -> eventListenerSupport.fire().testMethod());
    }

    //BaseRock generated method id: ${testSerializationDeserialization}, hash: 29FBABEBE8D976C448E3E1817C175B0D
    @Test
    void testSerializationDeserialization() throws IOException, ClassNotFoundException {
        eventListenerSupport.addListener(mockListener);
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try (ObjectOutputStream oos = new ObjectOutputStream(baos)) {
            oos.writeObject(eventListenerSupport);
        }
        try (ObjectInputStream ois = new ObjectInputStream(new ByteArrayInputStream(baos.toByteArray()))) {
            EventListenerSupport<TestListener> deserializedSupport = (EventListenerSupport<TestListener>) ois.readObject();
            assertNotNull(deserializedSupport);
            assertEquals(eventListenerSupport.getListenerCount(), deserializedSupport.getListenerCount());
        }
    }

    //BaseRock generated method id: ${testInvalidConstructorArguments}, hash: C23E9F7CB48A198C9D5D642D4C083786
    @ParameterizedTest
    @MethodSource("provideInvalidConstructorArguments")
    void testInvalidConstructorArguments(Class<?> listenerInterface, ClassLoader classLoader, Class<? extends Throwable> expectedExceptionClass) {
        assertThrows(expectedExceptionClass, () -> new EventListenerSupport<>(listenerInterface, classLoader));
    }

    private static Stream<Arguments> provideInvalidConstructorArguments() {
        return Stream.of(Arguments.of(null, Thread.currentThread().getContextClassLoader(), NullPointerException.class), Arguments.of(TestListener.class, null, NullPointerException.class), Arguments.of(String.class, Thread.currentThread().getContextClassLoader(), IllegalArgumentException.class));
    }

    //BaseRock generated method id: ${testNullListenerAddition}, hash: 68858DA44A5C57CAE5C90CB900423652
    @Test
    void testNullListenerAddition() {
        assertThrows(NullPointerException.class, () -> eventListenerSupport.addListener(null));
    }

    //BaseRock generated method id: ${testNullListenerRemoval}, hash: 34AA2B4C1667C05D432C1DC5B42CB32A
    @Test
    void testNullListenerRemoval() {
        assertThrows(NullPointerException.class, () -> eventListenerSupport.removeListener(null));
    }

    //BaseRock generated method id: ${testProxyInvocationHandlerWithCustomExceptionHandler}, hash: 6DEF8B7AB4CA5F8DAA0B11233A6443B1
    @Test
    void testProxyInvocationHandlerWithCustomExceptionHandler() throws Exception {
        EventListenerSupport<TestListener> customSupport = new EventListenerSupport<TestListener>(TestListener.class) {

            @Override
            protected InvocationHandler createInvocationHandler() {
                return new ProxyInvocationHandler(t -> {
                    throw new RuntimeException("Custom handler", t);
                });
            }
        };
        TestListener exceptionThrowingListener = mock(TestListener.class);
        doThrow(new Exception("Test exception")).when(exceptionThrowingListener).exceptionThrowingMethod();
        customSupport.addListener(exceptionThrowingListener);
        assertThrows(RuntimeException.class, () -> customSupport.fire().exceptionThrowingMethod());
    }
}
