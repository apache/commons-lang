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

/**
 * An interface describing a <a
 * href="https://martinfowler.com/bliki/CircuitBreaker.html">Circuit Breaker</a> component.
 *
 * <p>
 * A <em>circuit breaker</em> can be used to protect an application against unreliable
 * services or unexpected load. It typically monitors a specific resource. As long as this
 * resource works as expected, it stays in state <em>closed</em>, meaning that the
 * resource can be used. If problems are encountered when using the resource, the circuit
 * breaker can switch into state <em>open</em>; then access to this resource is
 * prohibited. Depending on a concrete implementation, it is possible that the circuit
 * breaker switches back to state <em>closed</em> when the resource becomes available
 * again.
 * </p>
 * <p>
 * This interface defines a generic protocol of a circuit breaker component. It should be
 * sufficiently generic to be applied to multiple different use cases.
 * </p>
 *
 * @param <T> the type of the value monitored by this circuit breaker
 * @since 3.5
 */
public interface CircuitBreaker<T> {
    /**
     * Returns the current open state of this circuit breaker. A return value of
     * <strong>true</strong> means that the circuit breaker is currently open indicating a
     * problem in the monitored sub system.
     *
     * @return the current open state of this circuit breaker
     */
    boolean isOpen();

    /**
     * Returns the current closed state of this circuit breaker. A return value of
     * <strong>true</strong> means that the circuit breaker is currently closed. This
     * means that everything is okay with the monitored sub system.
     *
     * @return the current closed state of this circuit breaker
     */
    boolean isClosed();

    /**
     * Checks the state of this circuit breaker and changes it if necessary. The return
     * value indicates whether the circuit breaker is now in state <em>closed</em>; a value
     * of <strong>true</strong> typically means that the current operation can continue.
     *
     * @return <strong>true</strong> if the circuit breaker is now closed;
     * <strong>false</strong> otherwise
     */
    boolean checkState();

    /**
     * Closes this circuit breaker. Its state is changed to closed. If this circuit
     * breaker is already closed, this method has no effect.
     */
    void close();

    /**
     * Opens this circuit breaker. Its state is changed to open. Depending on a concrete
     * implementation, it may close itself again if the monitored sub system becomes
     * available. If this circuit breaker is already open, this method has no effect.
     */
    void open();

    /**
     * Increments the monitored value and performs a check of the current state of this
     * circuit breaker. This method works like {@link #checkState()}, but the monitored
     * value is incremented before the state check is performed.
     *
     * @param increment value to increment in the monitored value of the circuit breaker
     * @return <strong>true</strong> if the circuit breaker is now closed;
     * <strong>false</strong> otherwise
     */
    boolean incrementAndCheckState(T increment);
}
