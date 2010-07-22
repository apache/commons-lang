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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.Map;
import java.util.TreeMap;

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

    public void testBindEventsToMethod()
    {
        final PropertyChangeSource src = new PropertyChangeSource();
        final EventCounter counter = new EventCounter();
        EventUtils.bindEventsToMethod(counter, "eventOccurred", src, PropertyChangeListener.class);
        assertEquals(0, counter.getCount());
        src.setProperty("newValue");
        assertEquals(1, counter.getCount());
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

    public static class PropertyChangeSource
    {
        private EventListenerSupport<PropertyChangeListener> listeners = EventListenerSupport.create(PropertyChangeListener.class);

        private String property;

        public void setProperty(String property)
        {
            String oldValue = this.property;
            this.property = property;
            listeners.fire().propertyChange(new PropertyChangeEvent(this, "property", "oldValue", property));
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
