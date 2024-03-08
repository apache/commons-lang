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
package org.apache.commons.lang3.concurrent;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Base class for circuit breakers.
 *
 * @param <T> the type of the value monitored by this circuit breaker
 * @since 3.5
 */
public abstract class AbstractCircuitBreaker<T> implements CircuitBreaker<T> {

    /**
     * An internal enumeration representing the different states of a circuit
     * breaker. This class also contains some logic for performing state
     * transitions. This is done to avoid complex if-conditions in the code of
     * {@link CircuitBreaker}.
     */
    protected enum State {

        /** The closed state. */
        CLOSED {
            /**
             * {@inheritDoc}
             */
            @Override
            public State oppositeState() {
                return OPEN;
            }
        },

        /** The open state. */
        OPEN {
            /**
             * {@inheritDoc}
             */
            @Override
            public State oppositeState() {
                return CLOSED;
            }
        };

        /**
         * Returns the opposite state to the represented state. This is useful
         * for flipping the current state.
         *
         * @return the opposite state
         */
        public abstract State oppositeState();
    }

    /**
     * The name of the <em>open</em> property as it is passed to registered
     * change listeners.
     */
    public static final String PROPERTY_NAME = "open";

    /**
     * Converts the given state value to a boolean <em>open</em> property.
     *
     * @param state the state to be converted
     * @return the boolean open flag
     */
    protected static boolean isOpen(final State state) {
        return state == State.OPEN;
    }

    /** The current state of this circuit breaker. */
    protected final AtomicReference<State> state = new AtomicReference<>(State.CLOSED);

    /**
     * Monitor for usages of change support to enable lazy initialization of that field.
     * This lets us require the {@code java.desktop} module statically, significantly reducing
     * the jlink-ed size of all downstream apache commons libraries.
     */
    private final Object changeSupportMonitor = new Object();

    /** An object for managing change listeners registered at this instance. */
    private PropertyChangeSupport changeSupport;

    /**
     * Creates an {@link AbstractCircuitBreaker}. It also creates an internal {@link PropertyChangeSupport}.
     */
    public AbstractCircuitBreaker() {
        changeSupport = null;
    }

    /**
     * Adds a change listener to this circuit breaker. This listener is notified whenever
     * the state of this circuit breaker changes. If the listener is
     * <strong>null</strong>, it is silently ignored.
     *
     * <p>
     * If the {@code java.desktop} module is not present in the JRE,
     * calling this method will throw an exception.
     * </p>
     *
     * @param listener the listener to be added
     */
    public void addChangeListener(final PropertyChangeListener listener) {
        synchronized (changeSupportMonitor) {
            if (changeSupport == null) {
                changeSupport = new PropertyChangeSupport(this);
            }

            changeSupport.addPropertyChangeListener(listener);
        }
    }

    /**
     * Changes the internal state of this circuit breaker. If there is actually a change
     * of the state value, all registered change listeners are notified.
     *
     * @param newState the new state to be set
     */
    protected void changeState(final State newState) {
        if (state.compareAndSet(newState.oppositeState(), newState)) {
            synchronized (changeSupportMonitor) {
                if (changeSupport != null) {
                    changeSupport.firePropertyChange(PROPERTY_NAME, !isOpen(newState), isOpen(newState));
                }
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public abstract boolean checkState();

    /**
     * {@inheritDoc}
     */
    @Override
    public void close() {
        changeState(State.CLOSED);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public abstract boolean incrementAndCheckState(T increment);

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isClosed() {
        return !isOpen();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isOpen() {
        return isOpen(state.get());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void open() {
        changeState(State.OPEN);
    }

    /**
     * Removes the specified change listener from this circuit breaker.
     *
     * @param listener the listener to be removed
     */
    public void removeChangeListener(final PropertyChangeListener listener) {
        synchronized (changeSupportMonitor) {
            if (changeSupport != null) {
                changeSupport.removePropertyChangeListener(listener);
            }
        }
    }

}
