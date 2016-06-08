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
     * The name of the <em>open</em> property as it is passed to registered
     * change listeners.
     */
    public static final String PROPERTY_NAME = "open";

    /** The current state of this circuit breaker. */
    protected final AtomicReference<State> state = new AtomicReference<State>(State.CLOSED);

    /** An object for managing change listeners registered at this instance. */
    private final PropertyChangeSupport changeSupport;

    /**
     * Creates an {@code AbstractCircuitBreaker}. It also creates an internal {@code PropertyChangeSupport}.
     */
    public AbstractCircuitBreaker() {
        changeSupport = new PropertyChangeSupport(this);
    }

    /**
     * {@inheritDoc}
     */
    public boolean isOpen() {
        return isOpen(state.get());
    }

    /**
     * {@inheritDoc}
     */
    public boolean isClosed() {
        return !isOpen();
    }

    /**
     * {@inheritDoc}
     */
    public abstract boolean checkState();

    /**
     * {@inheritDoc}
     */
    public abstract boolean incrementAndCheckState(T increment);

    /**
     * {@inheritDoc}
     */
    public void close() {
        changeState(State.CLOSED);
    }

    /**
     * {@inheritDoc}
     */
    public void open() {
        changeState(State.OPEN);
    }

    /**
     * Converts the given state value to a boolean <em>open</em> property.
     *
     * @param state the state to be converted
     * @return the boolean open flag
     */
    protected static boolean isOpen(State state) {
        return state == State.OPEN;
    }

    /**
     * Changes the internal state of this circuit breaker. If there is actually a change
     * of the state value, all registered change listeners are notified.
     *
     * @param newState the new state to be set
     */
    protected void changeState(State newState) {
        if (state.compareAndSet(newState.oppositeState(), newState)) {
            changeSupport.firePropertyChange(PROPERTY_NAME, !isOpen(newState), isOpen(newState));
        }
    }

    /**
     * Adds a change listener to this circuit breaker. This listener is notified whenever
     * the state of this circuit breaker changes. If the listener is
     * <strong>null</strong>, it is silently ignored.
     *
     * @param listener the listener to be added
     */
    public void addChangeListener(PropertyChangeListener listener) {
        changeSupport.addPropertyChangeListener(listener);
    }

    /**
     * Removes the specified change listener from this circuit breaker.
     *
     * @param listener the listener to be removed
     */
    public void removeChangeListener(PropertyChangeListener listener) {
        changeSupport.removePropertyChangeListener(listener);
    }

    /**
     * An internal enumeration representing the different states of a circuit
     * breaker. This class also contains some logic for performing state
     * transitions. This is done to avoid complex if-conditions in the code of
     * {@code CircuitBreaker}.
     */
    protected static enum State {
        CLOSED {
            /**
             * {@inheritDoc}
             */
            @Override
            public State oppositeState() {
                return OPEN;
            }
        },

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

}
