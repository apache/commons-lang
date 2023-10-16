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

import java.util.Objects;

import org.apache.commons.lang3.builder.AbstractSupplier;
import org.apache.commons.lang3.function.FailableConsumer;
import org.apache.commons.lang3.function.FailableSupplier;

/**
 * Abstracts and defines operations for ConcurrentInitializer implementations.
 *
 * @param <T> the type of the object managed by this initializer class.
 * @param <E> The exception type thrown by {@link #initialize()}.
 * @since 3.14.0
 */
public abstract class AbstractConcurrentInitializer<T, E extends Exception> implements ConcurrentInitializer<T> {

    /**
     * Builds a new instance for subclasses.
     *
     * @param <T> the type of the object managed by the initializer class.
     * @param <I> the type of the initializer class.
     * @param <B> the type of builder.
     * @param <E> The exception type thrown by {@link #initialize()}.
     */
    public abstract static class AbstractBuilder<I extends AbstractConcurrentInitializer<T, E>, T, B extends AbstractBuilder<I, T, B, E>, E extends Exception>
            extends AbstractSupplier<I, B, E> {

        /**
         * Closer consumer called by {@link #close()}.
         */
        private FailableConsumer<T, E> closer = FailableConsumer.nop();

        /**
         * Initializer supplier called by {@link #initialize()}.
         */
        private FailableSupplier<T, E> initializer = FailableSupplier.nul();

        /**
         * Gets the closer consumer called by {@link #close()}.
         *
         * @return the closer consumer called by {@link #close()}.
         */
        public FailableConsumer<T, E> getCloser() {
            return closer;
        }

        /**
         * Gets the initializer supplier called by {@link #initialize()}.
         *
         * @return the initializer supplier called by {@link #initialize()}.
         */
        public FailableSupplier<T, E> getInitializer() {
            return initializer;
        }

        /**
         * Sets the closer consumer called by {@link #close()}.
         *
         * @param closer the consumer called by {@link #close()}.
         * @return this
         */
        public B setCloser(final FailableConsumer<T, E> closer) {
            this.closer = closer != null ? closer : FailableConsumer.nop();
            return asThis();
        }

        /**
         * Sets the initializer supplier called by {@link #initialize()}.
         *
         * @param initializer the supplier called by {@link #initialize()}.
         * @return this
         */
        public B setInitializer(final FailableSupplier<T, E> initializer) {
            this.initializer = initializer != null ? initializer : FailableSupplier.nul();
            return asThis();
        }

    }

    /**
     * Closer consumer called by {@link #close()}.
     */
    private final FailableConsumer<? super T, E> closer;

    /**
     * Initializer supplier called by {@link #initialize()}.
     */
    private final FailableSupplier<? extends T, E> initializer;

    /**
     * Constructs a new instance.
     */
    public AbstractConcurrentInitializer() {
        this(FailableSupplier.nul(), FailableConsumer.nop());
    }

    /**
     * Constructs a new instance.
     *
     * @param initializer the initializer supplier called by {@link #initialize()}.
     * @param closer the closer consumer called by {@link #close()}.
     */
    AbstractConcurrentInitializer(final FailableSupplier<? extends T, E> initializer, final FailableConsumer<? super T, E> closer) {
        this.closer = Objects.requireNonNull(closer, "closer");
        this.initializer = Objects.requireNonNull(initializer, "initializer");
    }

    /**
     * Calls the closer with the manager object.
     *
     * @throws E Thrown by the closer.
     * @since 3.14.0
     */
    public void close() throws E {
        if (isInitialized()) {
            try {
                closer.accept(get());
            } catch (final ConcurrentException e) {
                // Should not happen
                throw new IllegalStateException(e);
            }
        }
    }

    /**
     * Creates and initializes the object managed by this {@code
     * ConcurrentInitializer}. This method is called by {@link #get()} when the object is accessed for the first time. An implementation can focus on the
     * creation of the object. No synchronization is needed, as this is already handled by {@code get()}.
     * <p>
     * Subclasses and clients that do not provide an initializer are expected to implement this method.
     * </p>
     *
     * @return the managed data object
     * @throws E if an error occurs during object creation
     */
    protected T initialize() throws E {
        return initializer.get();
    }

    /**
     * Returns true if initialization has been completed. If initialization threw an exception this will return false, but it will return true if a subsequent
     * call to initialize completes successfully. If the implementation of ConcurrentInitializer can initialize multiple objects, this will only return true if
     * all objects have been initialized.
     *
     * @return true if all initialization is complete, otherwise false
     */
    protected abstract boolean isInitialized();

}
