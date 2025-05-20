/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang3.concurrent;

import java.util.concurrent.atomic.AtomicReference;

import org.apache.commons.lang3.function.FailableConsumer;
import org.apache.commons.lang3.function.FailableSupplier;

/**
 * A specialized implementation of the {@link ConcurrentInitializer} interface
 * based on an {@link AtomicReference} variable.
 *
 * <p>
 * This class maintains a member field of type {@link AtomicReference}. It
 * implements the following algorithm to create and initialize an object in its
 * {@link #get()} method:
 * </p>
 * <ul>
 * <li>First it is checked whether the {@link AtomicReference} variable contains
 * already a value. If this is the case, the value is directly returned.</li>
 * <li>Otherwise the {@link #initialize()} method is called. This method must be
 * defined in concrete subclasses to actually create the managed object.</li>
 * <li>After the object was created by {@link #initialize()} it is checked
 * whether the {@link AtomicReference} variable is still undefined. This has to
 * be done because in the meantime another thread may have initialized the
 * object. If the reference is still empty, the newly created object is stored
 * in it and returned by this method.</li>
 * <li>Otherwise the value stored in the {@link AtomicReference} is returned.</li>
 * </ul>
 * <p>
 * Because atomic variables are used this class does not need any
 * synchronization. So there is no danger of deadlock, and access to the managed
 * object is efficient. However, if multiple threads access the {@code
 * AtomicInitializer} object before it has been initialized almost at the same
 * time, it can happen that {@link #initialize()} is called multiple times. The
 * algorithm outlined above guarantees that {@link #get()} always returns the
 * same object though.
 * </p>
 * <p>
 * Compared with the {@link LazyInitializer} class, this class can be more
 * efficient because it does not need synchronization. The drawback is that the
 * {@link #initialize()} method can be called multiple times which may be
 * problematic if the creation of the managed object is expensive. As a rule of
 * thumb this initializer implementation is preferable if there are not too many
 * threads involved and the probability that multiple threads access an
 * uninitialized object is small. If there is high parallelism,
 * {@link LazyInitializer} is more appropriate.
 * </p>
 *
 * @param <T> the type of the object managed by this initializer class
 * @since 3.0
 */
public class AtomicInitializer<T> extends AbstractConcurrentInitializer<T, ConcurrentException> {

    /**
     * Builds a new instance.
     *
     * @param <T> The type of results supplied by this builder.
     * @param <I> The type of the initializer managed by this builder.
     * @since 3.14.0
     */
    public static class Builder<I extends AtomicInitializer<T>, T> extends AbstractBuilder<I, T, Builder<I, T>, ConcurrentException> {

        /**
         * Constructs a new instance.
         */
        public Builder() {
            // empty
        }

        @SuppressWarnings("unchecked")
        @Override
        public I get() {
            return (I) new AtomicInitializer(getInitializer(), getCloser());
        }

    }

    private static final Object NO_INIT = new Object();

    /**
     * Creates a new builder.
     *
     * @param <T> the type of object to build.
     * @return a new builder.
     * @since 3.14.0
     */
    public static <T> Builder<AtomicInitializer<T>, T> builder() {
        return new Builder<>();
    }

    /** Holds the reference to the managed object. */
    private final AtomicReference<T> reference = new AtomicReference<>(getNoInit());

    /**
     * Constructs a new instance.
     */
    public AtomicInitializer() {
        // empty
    }

    /**
     * Constructs a new instance.
     *
     * @param initializer the initializer supplier called by {@link #initialize()}.
     * @param closer the closer consumer called by {@link #close()}.
     */
    private AtomicInitializer(final FailableSupplier<T, ConcurrentException> initializer, final FailableConsumer<T, ConcurrentException> closer) {
        super(initializer, closer);
    }

    /**
     * Returns the object managed by this initializer. The object is created if
     * it is not available yet and stored internally. This method always returns
     * the same object.
     *
     * @return the object created by this {@link AtomicInitializer}
     * @throws ConcurrentException if an error occurred during initialization of
     * the object
     */
    @Override
    public T get() throws ConcurrentException {
        T result = reference.get();

        if (result == getNoInit()) {
            result = initialize();
            if (!reference.compareAndSet(getNoInit(), result)) {
                // another thread has initialized the reference
                result = reference.get();
            }
        }

        return result;
    }

    /** Gets the internal no-init object cast for this instance. */
    @SuppressWarnings("unchecked")
    private T getNoInit() {
        return (T) NO_INIT;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected ConcurrentException getTypedException(final Exception e) {
        return new ConcurrentException(e);
    }

    /**
     * Tests whether this instance is initialized. Once initialized, always returns true.
     *
     * @return whether this instance is initialized. Once initialized, always returns true.
     * @since 3.14.0
     */
    @Override
    public boolean isInitialized() {
        return reference.get() != NO_INIT;
    }
}
