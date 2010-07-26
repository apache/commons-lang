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
import java.util.ArrayList;
import java.util.List;

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
