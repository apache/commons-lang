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

package org.apache.commons.lang3.event;

import static org.junit.jupiter.api.Assertions.assertEquals;
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
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.easymock.EasyMock;
import org.junit.jupiter.api.Test;

/**
 * @since 3.0
 */
public class EventListenerSupportTest {

    @Test
    public void testAddListenerNoDuplicates() {
        final EventListenerSupport<VetoableChangeListener> listenerSupport = EventListenerSupport.create(VetoableChangeListener.class);

        final VetoableChangeListener[] listeners = listenerSupport.getListeners();
        assertEquals(0, listeners.length);
        assertEquals(VetoableChangeListener.class, listeners.getClass().getComponentType());
        final VetoableChangeListener[] empty = listeners;
        //for fun, show that the same empty instance is used
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
    public void testAddNullListener() {
        final EventListenerSupport<VetoableChangeListener> listenerSupport = EventListenerSupport.create(VetoableChangeListener.class);
        assertThrows(NullPointerException.class, () -> listenerSupport.addListener(null));
    }

    @Test
    public void testRemoveNullListener() {
        final EventListenerSupport<VetoableChangeListener> listenerSupport = EventListenerSupport.create(VetoableChangeListener.class);
        assertThrows(NullPointerException.class, () -> listenerSupport.removeListener(null));
    }

    @Test
    public void testEventDispatchOrder() throws PropertyVetoException {
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
    public void testCreateWithNonInterfaceParameter() {
        assertThrows(IllegalArgumentException.class, () -> EventListenerSupport.create(String.class));
    }

    @Test
    public void testCreateWithNullParameter() {
        assertThrows(NullPointerException.class, () -> EventListenerSupport.create(null));
    }

    @Test
    public void testRemoveListenerDuringEvent() throws PropertyVetoException {
        final EventListenerSupport<VetoableChangeListener> listenerSupport = EventListenerSupport.create(VetoableChangeListener.class);
        for (int i = 0; i < 10; ++i) {
            addDeregisterListener(listenerSupport);
        }
        assertEquals(listenerSupport.getListenerCount(), 10);
        listenerSupport.fire().vetoableChange(new PropertyChangeEvent(new Date(), "Day", 4, 5));
        assertEquals(listenerSupport.getListenerCount(), 0);
    }

    @Test
    public void testGetListeners() {
        final EventListenerSupport<VetoableChangeListener> listenerSupport = EventListenerSupport.create(VetoableChangeListener.class);

        final VetoableChangeListener[] listeners = listenerSupport.getListeners();
        assertEquals(0, listeners.length);
        assertEquals(VetoableChangeListener.class, listeners.getClass().getComponentType());
        final VetoableChangeListener[] empty = listeners;
        //for fun, show that the same empty instance is used
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
    public void testSerialization() throws IOException, ClassNotFoundException, PropertyVetoException {
        final EventListenerSupport<VetoableChangeListener> listenerSupport = EventListenerSupport.create(VetoableChangeListener.class);
        listenerSupport.addListener(new VetoableChangeListener() {

            @Override
            public void vetoableChange(final PropertyChangeEvent e) {
            }
        });
        listenerSupport.addListener(EasyMock.createNiceMock(VetoableChangeListener.class));

        //serialize:
        final ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        final ObjectOutputStream objectOutputStream = new ObjectOutputStream(outputStream);

        objectOutputStream.writeObject(listenerSupport);
        objectOutputStream.close();

        //deserialize:
        @SuppressWarnings("unchecked")
        final
        EventListenerSupport<VetoableChangeListener> deserializedListenerSupport = (EventListenerSupport<VetoableChangeListener>) new ObjectInputStream(
                new ByteArrayInputStream(outputStream.toByteArray())).readObject();

        //make sure we get a listener array back, of the correct component type, and that it contains only the serializable mock
        final VetoableChangeListener[] listeners = deserializedListenerSupport.getListeners();
        assertEquals(VetoableChangeListener.class, listeners.getClass().getComponentType());
        assertEquals(1, listeners.length);

        //now verify that the mock still receives events; we can infer that the proxy was correctly reconstituted
        final VetoableChangeListener listener = listeners[0];
        final PropertyChangeEvent evt = new PropertyChangeEvent(new Date(), "Day", 7, 9);
        listener.vetoableChange(evt);
        EasyMock.replay(listener);
        deserializedListenerSupport.fire().vetoableChange(evt);
        EasyMock.verify(listener);

        //remove listener and verify we get an empty array of listeners
        deserializedListenerSupport.removeListener(listener);
        assertEquals(0, deserializedListenerSupport.getListeners().length);
    }

    @Test
    public void testSubclassInvocationHandling() throws PropertyVetoException {

        final
        EventListenerSupport<VetoableChangeListener> eventListenerSupport = new EventListenerSupport<VetoableChangeListener>(
                VetoableChangeListener.class) {
            private static final long serialVersionUID = 1L;

            @Override
            protected java.lang.reflect.InvocationHandler createInvocationHandler() {
                return new ProxyInvocationHandler() {
                    /**
                     * {@inheritDoc}
                     */
                    @Override
                    public Object invoke(final Object proxy, final Method method, final Object[] args)
                            throws Throwable {
                        return "vetoableChange".equals(method.getName())
                                && "Hour".equals(((PropertyChangeEvent) args[0]).getPropertyName()) ? null
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
}
