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

import junit.framework.TestCase;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.VetoableChangeListener;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.Map;
import java.util.TreeMap;

/**
 * @since 3.0
 * @version $Id$
 */
public class EventUtilsTest extends TestCase
{
    public void testAddEventListener()
    {
        final PropertyChangeSource src = new PropertyChangeSource();
        EventCountingInvociationHandler handler = new EventCountingInvociationHandler();
        PropertyChangeListener listener = handler.createListener(PropertyChangeListener.class);
        assertEquals(0, handler.getEventCount("propertyChange"));
        EventUtils.addEventListener(src, PropertyChangeListener.class, listener);
        assertEquals(0, handler.getEventCount("propertyChange"));
        src.setProperty("newValue");
        assertEquals(1, handler.getEventCount("propertyChange"));
    }

    public void testAddEventListenerWithNoAddMethod()
    {
        final PropertyChangeSource src = new PropertyChangeSource();
        EventCountingInvociationHandler handler = new EventCountingInvociationHandler();
        ActionListener listener = handler.createListener(ActionListener.class);
        try
        {
            EventUtils.addEventListener(src, ActionListener.class, listener);
            fail("Should not be allowed to add a listener to an object that doesn't support it.");
        }
        catch (IllegalArgumentException e)
        {
            assertEquals("Class " + src.getClass().getName() + " does not have a public add" + ActionListener.class.getSimpleName() + " method which takes a parameter of type " + ActionListener.class.getName() + ".", e.getMessage());
        }
    }

    public void testAddEventListenerThrowsException()
    {
        final ExceptionEventSource src = new ExceptionEventSource();
        try
        {
            EventUtils.addEventListener(src, ActionListener.class, new ActionListener()
            {
                public void actionPerformed(ActionEvent e)
                {
                    // Do nothing!
                }
            });
            fail("Add method should have thrown an exception, so method should fail.");
        }
        catch (RuntimeException e)
        {

        }
    }

    public void testAddEventListenerWithPrivateAddMethod()
    {
        final PropertyChangeSource src = new PropertyChangeSource();
        EventCountingInvociationHandler handler = new EventCountingInvociationHandler();
        VetoableChangeListener listener = handler.createListener(VetoableChangeListener.class);
        try
        {
            EventUtils.addEventListener(src, VetoableChangeListener.class, listener);
            fail("Should not be allowed to add a listener to an object that doesn't support it.");
        }
        catch (IllegalArgumentException e)
        {
            assertEquals("Class " + src.getClass().getName() + " does not have a public add" + VetoableChangeListener.class.getSimpleName() + " method which takes a parameter of type " + VetoableChangeListener.class.getName() + ".", e.getMessage());
        }
    }

    public void testBindEventsToMethod()
    {
        final PropertyChangeSource src = new PropertyChangeSource();
        final EventCounter counter = new EventCounter();
        EventUtils.bindEventsToMethod(counter, "eventOccurred", src, PropertyChangeListener.class);
        assertEquals(0, counter.getCount());
        src.setProperty("newValue");
        assertEquals(1, counter.getCount());
    }


    public void testBindEventsToMethodWithEvent()
    {
        final PropertyChangeSource src = new PropertyChangeSource();
        final EventCounterWithEvent counter = new EventCounterWithEvent();
        EventUtils.bindEventsToMethod(counter, "eventOccurred", src, PropertyChangeListener.class);
        assertEquals(0, counter.getCount());
        src.setProperty("newValue");
        assertEquals(1, counter.getCount());
    }


    public void testBindFilteredEventsToMethod()
    {
        final MultipleEventSource src = new MultipleEventSource();
        final EventCounter counter = new EventCounter();
        EventUtils.bindEventsToMethod(counter, "eventOccurred", src, MultipleEventListener.class, "event1");
        assertEquals(0, counter.getCount());
        src.listeners.fire().event1(new ActionEvent(src, ActionEvent.ACTION_PERFORMED, "event1"));
        assertEquals(1, counter.getCount());
        src.listeners.fire().event2(new ActionEvent(src, ActionEvent.ACTION_PERFORMED, "event2"));
        assertEquals(1, counter.getCount());
    }

    public static interface MultipleEventListener
    {
        public void event1(ActionEvent e);

        public void event2(ActionEvent e);
    }

    public static class EventCounter
    {
        private int count;

        public void eventOccurred()
        {
            count++;
        }

        public int getCount()
        {
            return count;
        }
    }

    public static class EventCounterWithEvent
    {
        private int count;

        public void eventOccurred(PropertyChangeEvent e)
        {
            count++;
        }

        public int getCount()
        {
            return count;
        }
    }


    private static class EventCountingInvociationHandler implements InvocationHandler
    {
        private Map<String, Integer> eventCounts = new TreeMap<String, Integer>();

        public <L> L createListener(Class<L> listenerType)
        {
            return listenerType.cast(Proxy.newProxyInstance(Thread.currentThread().getContextClassLoader(),
                    new Class[]{listenerType},
                    this));
        }

        public int getEventCount(String eventName)
        {
            Integer count = eventCounts.get(eventName);
            return count == null ? 0 : count;
        }

        public Object invoke(Object proxy, Method method, Object[] args) throws Throwable
        {
            Integer count = eventCounts.get(method.getName());
            if (count == null)
            {
                eventCounts.put(method.getName(), 1);
            }
            else
            {
                eventCounts.put(method.getName(), count + 1);
            }
            return null;
        }
    }

    public static class MultipleEventSource
    {
        private EventListenerSupport<MultipleEventListener> listeners = EventListenerSupport.create(MultipleEventListener.class);

        public void addMultipleEventListener(MultipleEventListener listener)
        {
            listeners.addListener(listener);
        }
    }

    public static class ExceptionEventSource
    {
        public void addActionListener(ActionListener listener)
        {
            throw new RuntimeException();
        }
    }

    public static class PropertyChangeSource
    {
        private EventListenerSupport<PropertyChangeListener> listeners = EventListenerSupport.create(PropertyChangeListener.class);

        private String property;

        public void setProperty(String property)
        {
            String oldValue = this.property;
            this.property = property;
            listeners.fire().propertyChange(new PropertyChangeEvent(this, "property", oldValue, property));
        }

        protected void addVetoableChangeListener(VetoableChangeListener listener)
        {
            // Do nothing!
        }

        public void addPropertyChangeListener(PropertyChangeListener listener)
        {
            listeners.addListener(listener);
        }

        public void removePropertyChangeListener(PropertyChangeListener listener)
        {
            listeners.removeListener(listener);
        }
    }
}
