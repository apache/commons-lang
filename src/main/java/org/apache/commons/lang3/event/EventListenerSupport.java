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

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import org.apache.commons.lang3.Validate;

/**
 * An EventListenerSupport object can be used to manage a list of event 
 * listeners of a particular type. The class provides 
 * {@link #addListener(Object)} and {@link #removeListener(Object)} methods
 * for registering listeners, as well as a {@link #fire()} method for firing
 * events to the listeners.
 * 
 * <p/>
 * To use this class, suppose you want to support ActionEvents.  You would do:
 * <code><pre>
 * public class MyActionEventSource
 * {
 *   private EventListenerSupport<ActionListener> actionListeners = 
 *       EventListenerSupport.create(ActionListener.class);
 *
 *   public void someMethodThatFiresAction()
 *   {
 *     ActionEvent e = new ActionEvent(this, ActionEvent.ACTION_PERFORMED, "somethingCool");
 *     actionListeners.fire().actionPerformed(e);
 *   }
 * }
 * </pre></code>
 *
 * @param <L> the type of event listener that is supported by this proxy.
 *
 * @since 3.0
 * @version $Id$
 */
public class EventListenerSupport<L>
{
    /**
    * The list used to hold the registered listeners. This list is 
    * intentionally a thread-safe copy-on-write-array so that traversals over
    * the list of listeners will be atomic.
    */
    private final List<L> listeners = new CopyOnWriteArrayList<L>();
    
    /**
     * The proxy representing the collection of listeners. Calls to this proxy 
     * object will sent to all registered listeners.
     */
    private final L proxy;

    /**
     * Creates an EventListenerSupport object which supports the specified 
     * listener type.
     *
     * @param listenerInterface the type of listener interface that will receive
     *        events posted using this class.
     * 
     * @return an EventListenerSupport object which supports the specified 
     *         listener type.
     *         
     * @throws NullPointerException if <code>listenerInterface</code> is 
     *         <code>null</code>.
     * @throws IllegalArgumentException if <code>listenerInterface</code> is
     *         not an interface.
     */
    public static <T> EventListenerSupport<T> create(Class<T> listenerInterface)
    {
        return new EventListenerSupport<T>(listenerInterface);
    }

    /**
     * Creates an EventListenerSupport object which supports the provided 
     * listener interface.
     *
     * @param listenerInterface the type of listener interface that will receive
     *        events posted using this class.
     * 
     * @throws NullPointerException if <code>listenerInterface</code> is 
     *         <code>null</code>.
     * @throws IllegalArgumentException if <code>listenerInterface</code> is
     *         not an interface.
     */
    public EventListenerSupport(Class<L> listenerInterface)
    {
        this(listenerInterface, Thread.currentThread().getContextClassLoader());
    }

    /**
     * Creates an EventListenerSupport object which supports the provided 
     * listener interface using the specified class loader to create the JDK 
     * dynamic proxy.
     *
     * @param listenerInterface the listener interface.
     * @param classLoader       the class loader.
     * 
     * @throws NullPointerException if <code>listenerInterface</code> or
     *         <code>classLoader</code> is <code>null</code>.
     * @throws IllegalArgumentException if <code>listenerInterface</code> is
     *         not an interface.
     */
    public EventListenerSupport(Class<L> listenerInterface, ClassLoader classLoader)
    {
        Validate.notNull(listenerInterface, "Listener interface cannot be null.");
        Validate.notNull(classLoader, "ClassLoader cannot be null.");
        Validate.isTrue(listenerInterface.isInterface(), 
            "Class {0} is not an interface", 
            listenerInterface.getName());
        proxy = listenerInterface.cast(Proxy.newProxyInstance(classLoader, 
                new Class[]{listenerInterface},
                new ProxyInvocationHandler()));
    }

    /**
     * Returns a proxy object which can be used to call listener methods on all 
     * of the registered event listeners. All calls made to this proxy will be
     * forwarded to all registered listeners.
     *
     * @return a proxy object which can be used to call listener methods on all 
     * of the registered event listeners
     */
    public L fire()
    {
        return proxy;
    }

//**********************************************************************************************************************
// Other Methods
//**********************************************************************************************************************

    /**
     * Registers an event listener.
     *
     * @param listener the event listener (may not be <code>null</code>).
     * 
     * @throws NullPointerException if <code>listener</code> is 
     *         <code>null</code>.
     */
    public void addListener(L listener)
    {
        Validate.notNull(listener, "Listener object cannot be null.");
        listeners.add(listener);
    }

    /**
     * Returns the number of registered listeners.
     *
     * @return the number of registered listeners.
     */
    int getListenerCount()
    {
        return listeners.size();
    }

    /**
     * Unregisters an event listener.
     *
     * @param listener the event listener (may not be <code>null</code>).
     * 
     * @throws NullPointerException if <code>listener</code> is 
     *         <code>null</code>.
     */
    public void removeListener(L listener)
    {
        Validate.notNull(listener, "Listener object cannot be null.");
        listeners.remove(listener);
    }

    /**
     * An invocation handler used to dispatch the event(s) to all the listeners.
     */
    private class ProxyInvocationHandler implements InvocationHandler
    {
        /**
         * Propagates the method call to all registered listeners in place of
         * the proxy listener object.
         * 
         * @param proxy the proxy object representing a listener on which the 
         *        invocation was called.
         * @param method the listener method that will be called on all of the
         *        listeners.
         * @param args event arguments to propogate to the listeners.
         */
        public Object invoke(Object proxy, Method method, Object[] args) 
            throws Throwable
        {
            for (L listener : listeners)
            {
                method.invoke(listener, args);
            }
            return null;
        }
    }
}
