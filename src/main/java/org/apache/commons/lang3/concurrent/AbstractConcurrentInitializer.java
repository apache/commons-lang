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
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.commons.lang3.function.FailableConsumer;
import org.apache.commons.lang3.function.FailableSupplier;

/**
 * Abstracts and defines operations for {@link ConcurrentInitializer} implementations.
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
        private FailableConsumer<T, ? extends Exception> closer = FailableConsumer.nop();

        /**
         * Initializer supplier called by {@link #initialize()}.
         */
        private FailableSupplier<T, ? extends Exception> initializer = FailableSupplier.nul();

        /**
         * Constructs a new instance.
         */
        public AbstractBuilder() {
            // empty
        }

        /**
         * Gets the closer consumer called by {@link #close()}.
         *
         * @return the closer consumer called by {@link #close()}.
         */
        public FailableConsumer<T, ? extends Exception> getCloser() {
            return closer;
        }

        /**
         * Gets the initializer supplier called by {@link #initialize()}.
         *
         * @return the initializer supplier called by {@link #initialize()}.
         */
        public FailableSupplier<T, ? extends Exception> getInitializer() {
            return initializer;
        }

        /**
         * Sets the closer consumer called by {@link #close()}.
         *
         * @param closer the consumer called by {@link #close()}.
         * @return {@code this} instance.
         */
        public B setCloser(final FailableConsumer<T, ? extends Exception> closer) {
            this.closer = closer != null ? closer : FailableConsumer.nop();
            return asThis();
        }

        /**
         * Sets the initializer supplier called by {@link #initialize()}.
         *
         * @param initializer the supplier called by {@link #initialize()}.
         * @return {@code this} instance.
         */
        public B setInitializer(final FailableSupplier<T, ? extends Exception> initializer) {
            this.initializer = initializer != null ? initializer : FailableSupplier.nul();
            return asThis();
        }

    }

    /**
     * Closer consumer called by {@link #close()}.
     */
    private final FailableConsumer<? super T, ? extends Exception> closer;

    /**
     * Initializer supplier called by {@link #initialize()}.
     */
    private final FailableSupplier<? extends T, ? extends Exception> initializer;

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
    AbstractConcurrentInitializer(final FailableSupplier<? extends T, ? extends Exception> initializer, final FailableConsumer<? super T, ? extends Exception> closer) {
        this.closer = Objects.requireNonNull(closer, "closer");
        this.initializer = Objects.requireNonNull(initializer, "initializer");
    }

    /**
     * Calls the closer with the manager object.
     *
     * @throws ConcurrentException Thrown by the closer.
     * @since 3.14.0
     */
    public void close() throws ConcurrentException {
        if (isInitialized()) {
            try {
                closer.accept(get());
            } catch (final Exception e) {
                // This intentionally does not duplicate the logic in initialize
                // or care about the generic type E.
                //
                // initialize may run inside a Future and it does not make sense
                // to wrap an exception stored inside a Future. However close()
                // always runs on the current thread so it always wraps in a
                // ConcurrentException
                throw new ConcurrentException(ExceptionUtils.throwUnchecked(e));
            }
        }
    }

    /**
     * Gets an Exception with a type of E as defined by a concrete subclass of this class.
     *
     * @param e The actual exception that was thrown
     * @return a new exception with the actual type of E, that wraps e.
     */
    protected abstract E getTypedException(Exception e);

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
    @SuppressWarnings("unchecked")
    protected T initialize() throws E {
        try {
            return initializer.get();
        } catch (final Exception e) {
            // Do this first so we don't pass a RuntimeException or Error into an exception constructor
            ExceptionUtils.throwUnchecked(e);

            // Depending on the subclass of AbstractConcurrentInitializer E can be Exception or ConcurrentException
            // if E is Exception the if statement below will always be true, and the new Exception object created
            // in getTypedException will never be thrown. If E is ConcurrentException and the if statement is false
            // we throw the ConcurrentException returned from getTypedException, which wraps the original exception.
            final E typedException = getTypedException(e);
            if (typedException.getClass().isAssignableFrom(e.getClass())) {
                throw (E) e;
            }
            throw typedException;
        }
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
