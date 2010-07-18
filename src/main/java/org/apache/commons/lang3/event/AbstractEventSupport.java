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

import java.io.Serializable;
import java.util.EventListener;
import java.util.EventObject;
import java.util.Iterator;
import java.util.concurrent.CopyOnWriteArrayList;

import org.apache.commons.lang3.Validate;

/**
 * <p>
 * The AbstractEventSupport class provides an abstract base class framework for
 * managing {@link EventListener} objects and for firing {@link EventObject}s to
 * those listeners. The class provides the ability to register a "source" object
 * that should be used as the source of the events, as well as the ability to 
 * register and unregister listeners in a thread-safe manner. The class also 
 * provides support for iterating over the registered listeners.
 * </p>
 * 
 * <p>
 * Subclasses of the AbstractEventSupport class are designed to implement 
 * methods for firing events. The implementations can make use of both the 
 * iterable functionality and use the associated source object as the source 
 * of the events.
 * </p>
 * 
 * <p>
 * Example:
 * <code><pre>
 * public WindowSupport extends AbstractEventSupport<WindowListener> {
 *     
 *     public WindowSupport(Window source) {
 *         super(source);
 *     }
 *     
 *     public void fireWindowOpened(int windowId, int oldState, int newState) {
 *         WindowEvent windowEvent = 
 *             new WindowEvent((Window) getSource(), windowId, oldState, newState);
 *         for (WindowListener listener : this) 
 *         {
 *             listener.windowOpened(windowEvent);
 *         }
 *     }
 * }
 * </pre></code>
 * </p>
 * 
 * @author <a href="mailto:mwooten.dev@gmail.com">Michael Wooten</a>
 *
 * @param <L> the subclass of {@link EventListener} that this event support
 *        class can register.
 * 
 * @since 3.0
 */
public abstract class AbstractEventSupport<L extends EventListener> 
    implements EventSupport<L>, Iterable<L>, Serializable {

    /**
     * The serialization unique version identifier.
     */
    private static final long serialVersionUID = 20100310L;

    /**
     * The list used to hold the registered listeners. This list is 
     * intentionally a thread-safe copy-on-write-array so that traversals over
     * the list of listeners will be atomic.
     */
    private final CopyOnWriteArrayList<L> listeners;
    
    /**
     * The object registered as the source of events fired to the listeners.
     */
    private final Object source;
    
    /**
     * Constructs a new AbstractEventSupport object and associates it with the
     * object that will be used as the source of all events sent to the 
     * listeners.
     * 
     * @param source the object that will be used as the source of all events
     *        posted to the listeners.
     *        
     * @throws NullPointerException if <code>source</code> is 
     *         <code>null</code>.
     */
    protected AbstractEventSupport(Object source) {
        Validate.notNull(source, "source cannot be null");
        this.source = source;
        this.listeners = new CopyOnWriteArrayList<L>();
    }
    
    /**
     * Registers a listener to receive events posted the by the supported class.
     * 
     * @param listener the listener to register for posted events. Values of 
     *        <code>null</code> will be ignored.
     */
    public void addListener(L listener) {
        if (listener != null)
        {
            this.listeners.add(listener);
        }
    }
    
    /**
     * Unregisters a listener from receiving events posted by the supported 
     * class.
     * 
     * @param listener the listener to stop posting events to. Values of 
     *        <code>null</code> will be ignored.
     */
    public void removeListener(L listener) {
        if (listener != null)
        {
            this.listeners.remove(listener);
        }
    }
    
    /**
     * Returns a reference to the object registered as the source of events 
     * broadcast to the listeners.
     * 
     * @return the object that was initially registered to be the source of all
     *         events sent to the listeners.
     */
    public Object getSource() {
        return this.source;
    }
    
    /**
     * Returns an iterator over the current list of listeners. This iterator is
     * immutable and does not support {@link Iterator#remove()} operations.
     * 
     * @return an iterator to iterate over the currently registered listeners.
     */
    public Iterator<L> iterator() {
        return this.listeners.iterator();
    }
}
