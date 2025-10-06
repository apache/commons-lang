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

package org.apache.commons.lang3.event;

import static org.apache.commons.lang3.LangAssertions.assertNullPointerException;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.UndeclaredThrowableException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.commons.lang3.function.FailableConsumer;
import org.easymock.EasyMock;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link EventListenerSupport}.
 */
class EventListenerSupportTest extends AbstractLangTest {

    private void addDeregisterListener(final EventListenerSupport<VetoableChangeListener> listenerSupport) {
        listenerSupport.addListener(new VetoableChangeListener() {

            @Override
            public void vetoableChange(final PropertyChangeEvent e) {
                listenerSupport.removeListener(this);
            }
        });
    }

    private VetoableChangeListener createListener(final List<VetoableChangeListener> calledListeners) {
        return new VetoableChangeListener() {

            @Override
            public void vetoableChange(final PropertyChangeEvent e) {
                calledListeners.add(this);
            }
        };
    }

    @Test
    void testAddListenerNoDuplicates() {
        final EventListenerSupport<VetoableChangeListener> listenerSupport = EventListenerSupport.create(VetoableChangeListener.class);
        final VetoableChangeListener[] listeners = listenerSupport.getListeners();
        assertEquals(0, listeners.length);
        assertEquals(VetoableChangeListener.class, listeners.getClass().getComponentType());
        final VetoableChangeListener[] empty = listeners;
        // for fun, show that the same empty instance is used
        assertSame(empty, listenerSupport.getListeners());
        final VetoableChangeListener listener1 = EasyMock.createNiceMock(VetoableChangeListener.class);
        listenerSupport.addListener(listener1);
        assertEquals(1, listenerSupport.getListeners().length);
        listenerSupport.addListener(listener1, false);
        assertEquals(1, listenerSupport.getListeners().length);
        listenerSupport.removeListener(listener1);
        assertSame(empty, listenerSupport.getListeners());
    }

    @Test
    void testAddNullListener() {
        final EventListenerSupport<VetoableChangeListener> listenerSupport = EventListenerSupport.create(VetoableChangeListener.class);
        assertNullPointerException(() -> listenerSupport.addListener(null));
    }

    @Test
    void testCreateWithNonInterfaceParameter() {
        assertThrows(IllegalArgumentException.class, () -> EventListenerSupport.create(String.class));
    }

    @Test
    void testCreateWithNullParameter() {
        assertNullPointerException(() -> EventListenerSupport.create(null));
    }

    @Test
    void testEventDispatchOrder() throws PropertyVetoException {
        final EventListenerSupport<VetoableChangeListener> listenerSupport = EventListenerSupport.create(VetoableChangeListener.class);
        final List<VetoableChangeListener> calledListeners = new ArrayList<>();
        final VetoableChangeListener listener1 = createListener(calledListeners);
        final VetoableChangeListener listener2 = createListener(calledListeners);
        listenerSupport.addListener(listener1);
        listenerSupport.addListener(listener2);
        listenerSupport.fire().vetoableChange(new PropertyChangeEvent(new Date(), "Day", 4, 5));
        assertEquals(calledListeners.size(), 2);
        assertSame(calledListeners.get(0), listener1);
        assertSame(calledListeners.get(1), listener2);
    }

    @Test
    void testGetListeners() {
        final EventListenerSupport<VetoableChangeListener> listenerSupport = EventListenerSupport.create(VetoableChangeListener.class);
        final VetoableChangeListener[] listeners = listenerSupport.getListeners();
        assertEquals(0, listeners.length);
        assertEquals(VetoableChangeListener.class, listeners.getClass().getComponentType());
        final VetoableChangeListener[] empty = listeners;
        // for fun, show that the same empty instance is used
        assertSame(empty, listenerSupport.getListeners());
        final VetoableChangeListener listener1 = EasyMock.createNiceMock(VetoableChangeListener.class);
        listenerSupport.addListener(listener1);
        assertEquals(1, listenerSupport.getListeners().length);
        final VetoableChangeListener listener2 = EasyMock.createNiceMock(VetoableChangeListener.class);
        listenerSupport.addListener(listener2);
        assertEquals(2, listenerSupport.getListeners().length);
        listenerSupport.removeListener(listener1);
        assertEquals(1, listenerSupport.getListeners().length);
        listenerSupport.removeListener(listener2);
        assertSame(empty, listenerSupport.getListeners());
    }

    @Test
    void testRemoveListenerDuringEvent() throws PropertyVetoException {
        final EventListenerSupport<VetoableChangeListener> listenerSupport = EventListenerSupport.create(VetoableChangeListener.class);
        for (int i = 0; i < 10; ++i) {
            addDeregisterListener(listenerSupport);
        }
        assertEquals(listenerSupport.getListenerCount(), 10);
        listenerSupport.fire().vetoableChange(new PropertyChangeEvent(new Date(), "Day", 4, 5));
        assertEquals(listenerSupport.getListenerCount(), 0);
    }

    @Test
    void testRemoveNullListener() {
        final EventListenerSupport<VetoableChangeListener> listenerSupport = EventListenerSupport.create(VetoableChangeListener.class);
        assertNullPointerException(() -> listenerSupport.removeListener(null));
    }

    @Test
    void testSerialization() throws IOException, ClassNotFoundException, PropertyVetoException {
        final EventListenerSupport<VetoableChangeListener> listenerSupport = EventListenerSupport.create(VetoableChangeListener.class);
        listenerSupport.addListener(Function.identity()::apply);
        listenerSupport.addListener(EasyMock.createNiceMock(VetoableChangeListener.class));
        // serialize:
        final ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        try (ObjectOutputStream objectOutputStream = new ObjectOutputStream(outputStream)) {
            objectOutputStream.writeObject(listenerSupport);
        }
        // deserialize:
        @SuppressWarnings("unchecked")
        final EventListenerSupport<VetoableChangeListener> deserializedListenerSupport = (EventListenerSupport<VetoableChangeListener>) new ObjectInputStream(
                new ByteArrayInputStream(outputStream.toByteArray())).readObject();
        // make sure we get a listener array back, of the correct component type, and that it contains only the serializable mock
        final VetoableChangeListener[] listeners = deserializedListenerSupport.getListeners();
        assertEquals(VetoableChangeListener.class, listeners.getClass().getComponentType());
        assertEquals(1, listeners.length);
        // now verify that the mock still receives events; we can infer that the proxy was correctly reconstituted
        final VetoableChangeListener listener = listeners[0];
        final PropertyChangeEvent evt = new PropertyChangeEvent(new Date(), "Day", 7, 9);
        listener.vetoableChange(evt);
        EasyMock.replay(listener);
        deserializedListenerSupport.fire().vetoableChange(evt);
        EasyMock.verify(listener);
        // remove listener and verify we get an empty array of listeners
        deserializedListenerSupport.removeListener(listener);
        assertEquals(0, deserializedListenerSupport.getListeners().length);
    }

    @Test
    void testSubclassInvocationHandling() throws PropertyVetoException {
        final EventListenerSupport<VetoableChangeListener> eventListenerSupport = new EventListenerSupport<VetoableChangeListener>(
                VetoableChangeListener.class) {

            private static final long serialVersionUID = 1L;

            @Override
            protected java.lang.reflect.InvocationHandler createInvocationHandler() {
                return new ProxyInvocationHandler() {

                    @Override
                    public Object invoke(final Object proxy, final Method method, final Object[] args)
                            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {
                        return "vetoableChange".equals(method.getName()) && "Hour".equals(((PropertyChangeEvent) args[0]).getPropertyName()) ? null
                                : super.invoke(proxy, method, args);
                    }
                };
            }
        };
        final VetoableChangeListener listener = EasyMock.createNiceMock(VetoableChangeListener.class);
        eventListenerSupport.addListener(listener);
        final Object source = new Date();
        final PropertyChangeEvent ignore = new PropertyChangeEvent(source, "Hour", 5, 6);
        final PropertyChangeEvent respond = new PropertyChangeEvent(source, "Day", 6, 7);
        listener.vetoableChange(respond);
        EasyMock.replay(listener);
        eventListenerSupport.fire().vetoableChange(ignore);
        eventListenerSupport.fire().vetoableChange(respond);
        EasyMock.verify(listener);
    }

    /**
     * Tests that throwing an exception from a listener stops calling the remaining listeners.
     */
    @Test
    void testThrowingListener() {
        final AtomicInteger count = new AtomicInteger();
        final EventListenerSupport<VetoableChangeListener> listenerSupport = EventListenerSupport.create(VetoableChangeListener.class);
        final int vetoLimit = 1;
        final int listenerCount = 10;
        for (int i = 0; i < listenerCount; ++i) {
            listenerSupport.addListener(evt -> {
                if (count.incrementAndGet() > vetoLimit) {
                    throw new PropertyVetoException(count.toString(), evt);
                }
            });
        }
        assertEquals(listenerCount, listenerSupport.getListenerCount());
        assertEquals(0, count.get());
        final Exception e = assertThrows(UndeclaredThrowableException.class,
                () -> listenerSupport.fire().vetoableChange(new PropertyChangeEvent(new Date(), "Day", 0, 1)));
        final Throwable rootCause = ExceptionUtils.getRootCause(e);
        assertInstanceOf(PropertyVetoException.class, rootCause);
        assertEquals(vetoLimit + 1, count.get());
    }

    /**
     * Tests that throwing an exception from a listener continues calling the remaining listeners.
     */
    @Test
    void testThrowingListenerContinues() throws PropertyVetoException {
        final AtomicInteger count = new AtomicInteger();
        final EventListenerSupport<VetoableChangeListener> listenerSupport = new EventListenerSupport<VetoableChangeListener>(VetoableChangeListener.class) {
            @Override
            protected InvocationHandler createInvocationHandler() {
                return new ProxyInvocationHandler(FailableConsumer.nop());
            }
        };
        final int vetoLimit = 1;
        final int listenerCount = 10;
        for (int i = 0; i < listenerCount; ++i) {
            listenerSupport.addListener(evt -> {
                if (count.incrementAndGet() > vetoLimit) {
                    throw new PropertyVetoException(count.toString(), evt);
                }
            });
        }
        assertEquals(listenerCount, listenerSupport.getListenerCount());
        assertEquals(0, count.get());
        listenerSupport.fire().vetoableChange(new PropertyChangeEvent(new Date(), "Day", 0, 1));
        assertEquals(listenerCount, count.get());
    }

}
