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
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import org.easymock.EasyMock;

/**
 * @since 3.0
 * @version $Id$
 */
public class EventListenerSupportTest extends TestCase
{
    public void testAddNullListener()
    {
        EventListenerSupport<ActionListener> listenerSupport = EventListenerSupport.create(ActionListener.class);
        try
        {
            listenerSupport.addListener(null);
            fail("Should not be able to add a null listener.");
        }
        catch (NullPointerException e)
        {

        }
    }

    public void testRemoveNullListener()
    {
        EventListenerSupport<ActionListener> listenerSupport = EventListenerSupport.create(ActionListener.class);
        try
        {
            listenerSupport.removeListener(null);
            fail("Should not be able to remove a null listener.");
        }
        catch (NullPointerException e)
        {

        }
    }

    public void testEventDispatchOrder()
    {
        EventListenerSupport<ActionListener> listenerSupport = EventListenerSupport.create(ActionListener.class);
        final List<ActionListener> calledListeners = new ArrayList<ActionListener>();

        final ActionListener listener1 = createListener(calledListeners);
        final ActionListener listener2 = createListener(calledListeners);
        listenerSupport.addListener(listener1);
        listenerSupport.addListener(listener2);
        listenerSupport.fire().actionPerformed(new ActionEvent("Hello", 0, "Hello"));
        assertEquals(calledListeners.size(), 2);
        assertSame(calledListeners.get(0), listener1);
        assertSame(calledListeners.get(1), listener2);
    }

    public void testCreateWithNonInterfaceParameter()
    {
        try
        {
            EventListenerSupport.create(String.class);
            fail("Should not be able to create using non-interface class.");
        }
        catch (IllegalArgumentException e)
        {

        }
    }

    public void testCreateWithNullParameter()
    {
        try
        {
            EventListenerSupport.create(null);
            fail("Should not be able to create using null class.");
        }
        catch (NullPointerException e)
        {

        }
    }

    public void testRemoveListenerDuringEvent()
    {
        final EventListenerSupport<ActionListener> listenerSupport = EventListenerSupport.create(ActionListener.class);
        for (int i = 0; i < 10; ++i)
        {
            addDeregisterListener(listenerSupport);
        }
        assertEquals(listenerSupport.getListenerCount(), 10);
        listenerSupport.fire().actionPerformed(new ActionEvent("Hello", 0, "Hello"));
        assertEquals(listenerSupport.getListenerCount(), 0);
    }

    public void testGetListeners() {
        final EventListenerSupport<ActionListener> listenerSupport = EventListenerSupport.create(ActionListener.class);

        ActionListener[] listeners = listenerSupport.getListeners();
        assertEquals(0, listeners.length);
        assertEquals(ActionListener.class, listeners.getClass().getComponentType());
        ActionListener[] empty = listeners;
        //for fun, show that the same empty instance is used 
        assertSame(empty, listenerSupport.getListeners());

        ActionListener listener1 = EasyMock.createNiceMock(ActionListener.class);
        listenerSupport.addListener(listener1);
        assertEquals(1, listenerSupport.getListeners().length);
        ActionListener listener2 = EasyMock.createNiceMock(ActionListener.class);
        listenerSupport.addListener(listener2);
        assertEquals(2, listenerSupport.getListeners().length);
        listenerSupport.removeListener(listener1);
        assertEquals(1, listenerSupport.getListeners().length);
        listenerSupport.removeListener(listener2);
        assertSame(empty, listenerSupport.getListeners());
    }

    public void testSerialization() throws IOException, ClassNotFoundException {
        EventListenerSupport<ActionListener> listenerSupport = EventListenerSupport.create(ActionListener.class);
        listenerSupport.addListener(new ActionListener() {
            
            public void actionPerformed(ActionEvent e) {
            }
        });
        listenerSupport.addListener(EasyMock.createNiceMock(ActionListener.class));

        //serialize:
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        ObjectOutputStream objectOutputStream = new ObjectOutputStream(outputStream);

        objectOutputStream.writeObject(listenerSupport);
        objectOutputStream.close();

        //deserialize:
        @SuppressWarnings("unchecked")
        EventListenerSupport<ActionListener> deserializedListenerSupport = (EventListenerSupport<ActionListener>) new ObjectInputStream(
                new ByteArrayInputStream(outputStream.toByteArray())).readObject();

        //make sure we get a listener array back, of the correct component type, and that it contains only the serializable mock
        ActionListener[] listeners = deserializedListenerSupport.getListeners();
        assertEquals(ActionListener.class, listeners.getClass().getComponentType());
        assertEquals(1, listeners.length);

        //now verify that the mock still receives events; we can infer that the proxy was correctly reconstituted
        ActionListener listener = listeners[0];
        ActionEvent evt = new ActionEvent(new Object(), 666, "sit");
        listener.actionPerformed(evt);
        EasyMock.replay(listener);
        deserializedListenerSupport.fire().actionPerformed(evt);
        EasyMock.verify(listener);

        //remove listener and verify we get an empty array of listeners
        deserializedListenerSupport.removeListener(listener);
        assertEquals(0, deserializedListenerSupport.getListeners().length);
    }

    public void testSubclassInvocationHandling() {

        @SuppressWarnings("serial")
        EventListenerSupport<ActionListener> eventListenerSupport = new EventListenerSupport<ActionListener>(
                ActionListener.class) {
            protected java.lang.reflect.InvocationHandler createInvocationHandler() {
                return new ProxyInvocationHandler() {
                    /**
                     * {@inheritDoc}
                     */
                    @Override
                    public Object invoke(Object proxy, Method method, Object[] args)
                            throws Throwable {
                        return "actionPerformed".equals(method.getName())
                                && "ignore".equals(((ActionEvent) args[0]).getActionCommand()) ? null
                                : super.invoke(proxy, method, args);
                    }
                };
            };
        };

        ActionListener listener = EasyMock.createNiceMock(ActionListener.class);
        eventListenerSupport.addListener(listener);
        Object source = new Object();
        ActionEvent ignore = new ActionEvent(source, 0, "ignore");
        ActionEvent respond = new ActionEvent(source, 1, "respond");
        listener.actionPerformed(respond);
        EasyMock.replay(listener);
        eventListenerSupport.fire().actionPerformed(ignore);
        eventListenerSupport.fire().actionPerformed(respond);
        EasyMock.verify(listener);
    }

    private void addDeregisterListener(final EventListenerSupport<ActionListener> listenerSupport)
    {
        listenerSupport.addListener(new ActionListener()
        {
            public void actionPerformed(ActionEvent e)
            {
                listenerSupport.removeListener(this);
            }
        });
    }

    private ActionListener createListener(final List<ActionListener> calledListeners)
    {
        return new ActionListener()
        {
            public void actionPerformed(ActionEvent e)
            {
                calledListeners.add(this);
            }
        };
    }
}
