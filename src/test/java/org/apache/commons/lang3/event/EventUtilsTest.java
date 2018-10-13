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
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.VetoableChangeListener;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Proxy;
import java.util.Date;
import java.util.Map;
import java.util.TreeMap;

import javax.naming.event.ObjectChangeListener;

import org.junit.jupiter.api.Test;

/**
 * @since 3.0
 */
public class EventUtilsTest {
    @Test
    public void testConstructor() {
        assertNotNull(new EventUtils());
        final Constructor<?>[] cons = EventUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertTrue(Modifier.isPublic(cons[0].getModifiers()));
        assertTrue(Modifier.isPublic(EventUtils.class.getModifiers()));
        assertFalse(Modifier.isFinal(EventUtils.class.getModifiers()));
    }

    @Test
    public void testAddEventListener() {
        final PropertyChangeSource src = new PropertyChangeSource();
        final EventCountingInvociationHandler handler = new EventCountingInvociationHandler();
        final PropertyChangeListener listener = handler.createListener(PropertyChangeListener.class);
        assertEquals(0, handler.getEventCount("propertyChange"));
        EventUtils.addEventListener(src, PropertyChangeListener.class, listener);
        assertEquals(0, handler.getEventCount("propertyChange"));
        src.setProperty("newValue");
        assertEquals(1, handler.getEventCount("propertyChange"));
    }

    @Test
    public void testAddEventListenerWithNoAddMethod() {
        final PropertyChangeSource src = new PropertyChangeSource();
        final EventCountingInvociationHandler handler = new EventCountingInvociationHandler();
        final ObjectChangeListener listener = handler.createListener(ObjectChangeListener.class);
        IllegalArgumentException e =
                assertThrows(IllegalArgumentException.class, () -> EventUtils.addEventListener(src, ObjectChangeListener.class, listener));
        assertEquals("Class " + src.getClass().getName() + " does not have a public add" + ObjectChangeListener.class.getSimpleName() + " method which takes a parameter of type " + ObjectChangeListener.class.getName() + ".",
                e.getMessage());
    }

    @Test
    public void testAddEventListenerThrowsException() {
        final ExceptionEventSource src = new ExceptionEventSource();
        assertThrows(RuntimeException.class, () ->
            EventUtils.addEventListener(src, PropertyChangeListener.class, new PropertyChangeListener() {
                @Override
                public void propertyChange(final PropertyChangeEvent e) {
                    // Do nothing!
                }
            })
        );
    }

    @Test
    public void testAddEventListenerWithPrivateAddMethod() {
        final PropertyChangeSource src = new PropertyChangeSource();
        final EventCountingInvociationHandler handler = new EventCountingInvociationHandler();
        final VetoableChangeListener listener = handler.createListener(VetoableChangeListener.class);
        IllegalArgumentException e =
                assertThrows(IllegalArgumentException.class, () -> EventUtils.addEventListener(src, VetoableChangeListener.class, listener));
        assertEquals("Class " + src.getClass().getName() + " does not have a public add" + VetoableChangeListener.class.getSimpleName() + " method which takes a parameter of type " + VetoableChangeListener.class.getName() + ".",
                e.getMessage());
    }

    @Test
    public void testBindEventsToMethod() {
        final PropertyChangeSource src = new PropertyChangeSource();
        final EventCounter counter = new EventCounter();
        EventUtils.bindEventsToMethod(counter, "eventOccurred", src, PropertyChangeListener.class);
        assertEquals(0, counter.getCount());
        src.setProperty("newValue");
        assertEquals(1, counter.getCount());
    }


    @Test
    public void testBindEventsToMethodWithEvent() {
        final PropertyChangeSource src = new PropertyChangeSource();
        final EventCounterWithEvent counter = new EventCounterWithEvent();
        EventUtils.bindEventsToMethod(counter, "eventOccurred", src, PropertyChangeListener.class);
        assertEquals(0, counter.getCount());
        src.setProperty("newValue");
        assertEquals(1, counter.getCount());
    }


    @Test
    public void testBindFilteredEventsToMethod() {
        final MultipleEventSource src = new MultipleEventSource();
        final EventCounter counter = new EventCounter();
        EventUtils.bindEventsToMethod(counter, "eventOccurred", src, MultipleEventListener.class, "event1");
        assertEquals(0, counter.getCount());
        src.listeners.fire().event1(new PropertyChangeEvent(new Date(), "Day", Integer.valueOf(0), Integer.valueOf(1)));
        assertEquals(1, counter.getCount());
        src.listeners.fire().event2(new PropertyChangeEvent(new Date(), "Day", Integer.valueOf(1), Integer.valueOf(2)));
        assertEquals(1, counter.getCount());
    }

    public interface MultipleEventListener {
        void event1(PropertyChangeEvent e);

        void event2(PropertyChangeEvent e);
    }

    public static class EventCounter {
        private int count;

        public void eventOccurred() {
            count++;
        }

        public int getCount() {
            return count;
        }
    }

    public static class EventCounterWithEvent {
        private int count;

        public void eventOccurred(final PropertyChangeEvent e) {
            count++;
        }

        public int getCount() {
            return count;
        }
    }


    private static class EventCountingInvociationHandler implements InvocationHandler {
        private final Map<String, Integer> eventCounts = new TreeMap<>();

        public <L> L createListener(final Class<L> listenerType) {
            return listenerType.cast(Proxy.newProxyInstance(Thread.currentThread().getContextClassLoader(),
                    new Class[]{listenerType},
                    this));
        }

        public int getEventCount(final String eventName) {
            final Integer count = eventCounts.get(eventName);
            return count == null ? 0 : count.intValue();
        }

        @Override
        public Object invoke(final Object proxy, final Method method, final Object[] args) {
            final Integer count = eventCounts.get(method.getName());
            if (count == null) {
                eventCounts.put(method.getName(), Integer.valueOf(1));
            } else {
                eventCounts.put(method.getName(), Integer.valueOf(count.intValue() + 1));
            }
            return null;
        }
    }

    public static class MultipleEventSource {
        private final EventListenerSupport<MultipleEventListener> listeners = EventListenerSupport.create(MultipleEventListener.class);

        public void addMultipleEventListener(final MultipleEventListener listener) {
            listeners.addListener(listener);
        }
    }

    public static class ExceptionEventSource {
        public void addPropertyChangeListener(final PropertyChangeListener listener) {
            throw new RuntimeException();
        }
    }

    public static class PropertyChangeSource {
        private final EventListenerSupport<PropertyChangeListener> listeners = EventListenerSupport.create(PropertyChangeListener.class);

        private String property;

        public void setProperty(final String property) {
            final String oldValue = this.property;
            this.property = property;
            listeners.fire().propertyChange(new PropertyChangeEvent(this, "property", oldValue, property));
        }

        protected void addVetoableChangeListener(final VetoableChangeListener listener) {
            // Do nothing!
        }

        public void addPropertyChangeListener(final PropertyChangeListener listener) {
            listeners.addListener(listener);
        }

        public void removePropertyChangeListener(final PropertyChangeListener listener) {
            listeners.removeListener(listener);
        }
    }
}
