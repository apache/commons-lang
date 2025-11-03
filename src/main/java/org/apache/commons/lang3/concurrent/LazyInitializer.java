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

import org.apache.commons.lang3.function.FailableConsumer;
import org.apache.commons.lang3.function.FailableSupplier;

/**
 * This class provides a generic implementation of the lazy initialization pattern.
 *
 * <p>
 * Sometimes an application has to deal with an object only under certain circumstances, e.g. when the user selects a specific menu item or if a special event
 * is received. If the creation of the object is costly or the consumption of memory or other system resources is significant, it may make sense to defer the
 * creation of this object until it is really needed. This is a use case for the lazy initialization pattern.
 * </p>
 * <p>
 * This abstract base class provides an implementation of the double-check idiom for an instance field as discussed in Joshua Bloch's "Effective Java", 2nd
 * edition, item 71. The class already implements all necessary synchronization. A concrete subclass has to implement the {@code initialize()} method, which
 * actually creates the wrapped data object.
 * </p>
 * <p>
 * As an usage example consider that we have a class {@code ComplexObject} whose instantiation is a complex operation. In order to apply lazy initialization to
 * this class, a subclass of {@link LazyInitializer} has to be created:
 * </p>
 *
 * <pre>{@code
 * public class ComplexObjectInitializer extends LazyInitializer<ComplexObject> {
 *     &#064;Override
 *     protected ComplexObject initialize() {
 *         return new ComplexObject();
 *     }
 * }
 * }
 * </pre>
 *
 * <p>
 * Access to the data object is provided through the {@code get()} method. So, code that wants to obtain the {@code ComplexObject} instance would simply look
 * like this:
 * </p>
 *
 * <pre>
 * // Create an instance of the lazy initializer
 * ComplexObjectInitializer initializer = new ComplexObjectInitializer();
 * ...
 * // When the object is actually needed:
 * ComplexObject cobj = initializer.get();
 * </pre>
 *
 * <p>
 * If multiple threads call the {@code get()} method when the object has not yet been created, they are blocked until initialization completes. The algorithm
 * guarantees that only a single instance of the wrapped object class is created, which is passed to all callers. Once initialized, calls to the {@code get()}
 * method are pretty fast because no synchronization is needed (only an access to a <strong>volatile</strong> member field).
 * </p>
 *
 * @param <T> the type of the object managed by the initializer.
 * @since 3.0
 */
public class LazyInitializer<T> extends AbstractConcurrentInitializer<T, ConcurrentException> {

    /**
     * Builds a new instance.
     *
     * @param <T> The type of results supplied by this builder.
     * @param <I> The type of the initializer managed by this builder.
     * @since 3.14.0
     */
    public static class Builder<I extends LazyInitializer<T>, T> extends AbstractBuilder<I, T, Builder<I, T>, ConcurrentException> {

        /**
         * Constructs a new instance.
         */
        public Builder() {
            // empty
        }

        @SuppressWarnings("unchecked")
        @Override
        public I get() {
            return (I) new LazyInitializer(getInitializer(), getCloser());
        }

    }

    /**
     * A unique value indicating an un-initialized instance.
     */
    private static final Object NO_INIT = new Object();

    /**
     * Creates a new builder.
     *
     * @param <T> the type of object to build.
     * @return a new builder.
     * @since 3.14.0
     */
    public static <T> Builder<LazyInitializer<T>, T> builder() {
        return new Builder<>();
    }

    /** Stores the managed object. */
    @SuppressWarnings("unchecked")
    private volatile T object = (T) NO_INIT;

    /**
     * Constructs a new instance.
     */
    public LazyInitializer() {
        // empty
    }

    /**
     * Constructs a new instance.
     *
     * @param initializer the initializer supplier called by {@link #initialize()}.
     * @param closer the closer consumer called by {@link #close()}.
     */
    private LazyInitializer(final FailableSupplier<T, ConcurrentException> initializer, final FailableConsumer<T, ConcurrentException> closer) {
        super(initializer, closer);
    }

    /**
     * Gets the object wrapped by this instance. On first access the object is created. After that it is cached and can be accessed pretty fast.
     *
     * @return the object initialized by this {@link LazyInitializer}.
     * @throws ConcurrentException if an error occurred during initialization of the object.
     */
    @Override
    public T get() throws ConcurrentException {
        // use a temporary variable to reduce the number of reads of the
        // volatile field
        T result = object;
        if (result == NO_INIT) {
            synchronized (this) {
                result = object;
                if (result == NO_INIT) {
                    object = result = initialize();
                }
            }
        }
        return result;
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
        return object != NO_INIT;
    }

}
