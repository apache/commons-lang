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

import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;


/**
 * Base class for circuit breakers.
 *
 * @param <T> the type of the value monitored by this circuit breaker
 * @since 3.11
 */
public abstract class BaseCircuitBreaker<T> implements CircuitBreaker<T> {

    /** The current state of this circuit breaker. */
    protected final AtomicReference<State> state = new AtomicReference<>(State.CLOSED);

    /**
     * Consumer called every time the circuit breaker state changes if not {@code null}.
     */
    private final Consumer<State> consumer;

    /**
     * Creates an {@code AbstractCircuitBreaker}.
     * @param consumer a consumer called every time the circuit breaker state changes
     */
    public BaseCircuitBreaker(Consumer<State> consumer) {
        this.consumer = consumer;
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
    public boolean isClosed() {
        return !isOpen();
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
    public abstract boolean incrementAndCheckState(T increment);

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
    public void open() {
        changeState(State.OPEN);
    }

    /**
     * Converts the given state value to a boolean <em>open</em> property.
     *
     * @param state the state to be converted
     * @return the boolean open flag
     */
    protected static boolean isOpen(final State state) {
        return state == State.OPEN;
    }

    /**
     * Changes the internal state of this circuit breaker. If there is actually a change
     * of the state value, the consumer of this circuit breaker will be called with the
     * value of the new state.
     *
     * @param newState the new state to be set
     */
    protected void changeState(final State newState) {
        if (state.compareAndSet(newState.oppositeState(), newState)) {
            if (this.consumer != null) {
                this.consumer.accept(newState);
            }
        }
    }

    /**
     * An internal enumeration representing the different states of a circuit
     * breaker. This class also contains some logic for performing state
     * transitions. This is done to avoid complex if-conditions in the code of
     * {@code CircuitBreaker}.
     */
    protected enum State {
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
