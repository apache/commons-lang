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
 * A specialized {@link ConcurrentInitializer} implementation which is similar
 * to {@link AtomicInitializer}, but ensures that the {@link #initialize()}
 * method is called only once.
 *
 * <p>
 * As {@link AtomicInitializer} this class is based on atomic variables, so it
 * can create an object under concurrent access without synchronization.
 * However, it implements an additional check to guarantee that the
 * {@link #initialize()} method which actually creates the object cannot be
 * called multiple times.
 * </p>
 * <p>
 * Because of this additional check this implementation is slightly less
 * efficient than {@link AtomicInitializer}, but if the object creation in the
 * {@code initialize()} method is expensive or if multiple invocations of
 * {@code initialize()} are problematic, it is the better alternative.
 * </p>
 * <p>
 * From its semantics this class has the same properties as
 * {@link LazyInitializer}. It is a &quot;save&quot; implementation of the lazy
 * initializer pattern. Comparing both classes in terms of efficiency is
 * difficult because which one is faster depends on multiple factors. Because
 * {@link AtomicSafeInitializer} does not use synchronization at all it probably
 * outruns {@link LazyInitializer}, at least under low or moderate concurrent
 * access. Developers should run their own benchmarks on the expected target
 * platform to decide which implementation is suitable for their specific use
 * case.
 * </p>
 *
 * @param <T> the type of the object managed by this initializer class
 * @since 3.0
 */
public class AtomicSafeInitializer<T> extends AbstractConcurrentInitializer<T, ConcurrentException> {

    /**
     * Builds a new instance.
     *
     * @param <T> The type of results supplied by this builder.
     * @param <I> The type of the initializer managed by this builder.
     * @since 3.14.0
     */
    public static class Builder<I extends AtomicSafeInitializer<T>, T> extends AbstractBuilder<I, T, Builder<I, T>, ConcurrentException> {

        /**
         * Constructs a new instance.
         */
        public Builder() {
            // empty
        }

        @SuppressWarnings("unchecked")
        @Override
        public I get() {
            return (I) new AtomicSafeInitializer(getInitializer(), getCloser());
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
    public static <T> Builder<AtomicSafeInitializer<T>, T> builder() {
        return new Builder<>();
    }

    /** A guard which ensures that initialize() is called only once. */
    private final AtomicReference<AtomicSafeInitializer<T>> factory = new AtomicReference<>();

    /** Holds the reference to the managed object. */
    private final AtomicReference<T> reference = new AtomicReference<>(getNoInit());

    /**
     * Constructs a new instance.
     */
    public AtomicSafeInitializer() {
        // empty
    }

    /**
     * Constructs a new instance.
     *
     * @param initializer the initializer supplier called by {@link #initialize()}.
     * @param closer the closer consumer called by {@link #close()}.
     */
    private AtomicSafeInitializer(final FailableSupplier<T, ConcurrentException> initializer, final FailableConsumer<T, ConcurrentException> closer) {
        super(initializer, closer);
    }

    /**
     * Gets (and initialize, if not initialized yet) the required object
     *
     * @return lazily initialized object
     * @throws ConcurrentException if the initialization of the object causes an
     * exception
     */
    @Override
    public final T get() throws ConcurrentException {
        T result;

        while ((result = reference.get()) == getNoInit()) {
            if (factory.compareAndSet(null, this)) {
                reference.set(initialize());
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
