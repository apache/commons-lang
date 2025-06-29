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

import java.util.concurrent.Callable;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import org.apache.commons.lang3.function.FailableConsumer;
import org.apache.commons.lang3.function.FailableSupplier;

/**
 * A class that allows complex initialization operations in a background task.
 *
 * <p>
 * Applications often have to do some expensive initialization steps when they
 * are started, e.g. constructing a connection to a database, reading a
 * configuration file, etc. Doing these things in parallel can enhance
 * performance as the CPU load can be improved. However, when access to the
 * resources initialized in a background thread is actually required,
 * synchronization has to be performed to ensure that their initialization is
 * complete.
 * </p>
 * <p>
 * This abstract base class provides support for this use case. A concrete
 * subclass must implement the {@link #initialize()} method. Here an arbitrary
 * initialization can be implemented, and a result object can be returned. With
 * this method in place the basic usage of this class is as follows (where
 * {@code MyBackgroundInitializer} is a concrete subclass):
 * </p>
 *
 * <pre>
 * MyBackgroundInitializer initializer = new MyBackgroundInitializer();
 * initializer.start();
 * // Now do some other things. Initialization runs in a parallel thread
 * ...
 * // Wait for the end of initialization and access the result object
 * Object result = initializer.get();
 * </pre>
 *
 * <p>
 * After the construction of a {@link BackgroundInitializer} object its
 * {@link #start()} method has to be called. This starts the background
 * processing. The application can now continue to do other things. When it
 * needs access to the object produced by the {@link BackgroundInitializer} it
 * calls its {@link #get()} method. If initialization is already complete,
 * {@link #get()} returns the result object immediately. Otherwise it blocks
 * until the result object is fully constructed.
 * </p>
 * <p>
 * {@link BackgroundInitializer} is a thin wrapper around a {@link Future}
 * object and uses an {@link ExecutorService} for running the background
 * initialization task. It is possible to pass in an {@link ExecutorService} at
 * construction time or set one using {@code setExternalExecutor()} before
 * {@code start()} was called. Then this object is used to spawn the background
 * task. If no {@link ExecutorService} has been provided, {@code
 * BackgroundInitializer} creates a temporary {@link ExecutorService} and
 * destroys it when initialization is complete.
 * </p>
 * <p>
 * The methods provided by {@link BackgroundInitializer} provide for minimal
 * interaction with the wrapped {@link Future} object. It is also possible to
 * obtain the {@link Future} object directly. Then the enhanced functionality
 * offered by {@link Future} can be used, e.g. to check whether the background
 * operation is complete or to cancel the operation.
 * </p>
 *
 * @param <T> the type of the object managed by this initializer class
 * @since 3.0
 */
public class BackgroundInitializer<T> extends AbstractConcurrentInitializer<T, Exception> {

    /**
     * Builds a new instance.
     *
     * @param <T> The type of results supplied by this builder.
     * @param <I> The type of the initializer managed by this builder.
     * @since 3.14.0
     */
    public static class Builder<I extends BackgroundInitializer<T>, T> extends AbstractBuilder<I, T, Builder<I, T>, Exception> {

        /**
         * The external executor service for executing tasks. null is a permitted value.
         */
        private ExecutorService externalExecutor;

        /**
         * Constructs a new instance.
         */
        public Builder() {
            // empty
        }

        @SuppressWarnings("unchecked")
        @Override
        public I get() {
            return (I) new BackgroundInitializer(getInitializer(), getCloser(), externalExecutor);
        }

        /**
         * Sets the external executor service for executing tasks. null is a permitted value.
         *
         * @see org.apache.commons.lang3.concurrent.BackgroundInitializer#setExternalExecutor(ExecutorService)
         * @param externalExecutor the {@link ExecutorService} to be used.
         * @return {@code this} instance.
         */
        public Builder<I, T> setExternalExecutor(final ExecutorService externalExecutor) {
            this.externalExecutor = externalExecutor;
            return asThis();
        }

    }

    private final class InitializationTask implements Callable<T> {
        /** Stores the executor service to be destroyed at the end. */
        private final ExecutorService execFinally;

        /**
         * Creates a new instance of {@link InitializationTask} and initializes
         * it with the {@link ExecutorService} to be destroyed at the end.
         *
         * @param exec the {@link ExecutorService}
         */
        InitializationTask(final ExecutorService exec) {
            execFinally = exec;
        }

        /**
         * Initiates initialization and returns the result.
         *
         * @return the result object
         * @throws Exception if an error occurs
         */
        @Override
        public T call() throws Exception {
            try {
                return initialize();
            } finally {
                if (execFinally != null) {
                    execFinally.shutdown();
                }
            }
        }
    }

    /**
     * Creates a new builder.
     *
     * @param <T> the type of object to build.
     * @return a new builder.
     * @since 3.14.0
     */
    public static <T> Builder<BackgroundInitializer<T>, T> builder() {
        return new Builder<>();
    }

    /** The external executor service for executing tasks. */
    private ExecutorService externalExecutor; // @GuardedBy("this")

    /** A reference to the executor service that is actually used. */
    private ExecutorService executor; // @GuardedBy("this")

    /** Stores the handle to the background task. */
    private Future<T> future;  // @GuardedBy("this")

    /**
     * Creates a new instance of {@link BackgroundInitializer}. No external
     * {@link ExecutorService} is used.
     */
    protected BackgroundInitializer() {
        this(null);
    }

    /**
     * Creates a new instance of {@link BackgroundInitializer} and initializes
     * it with the given {@link ExecutorService}. If the {@link ExecutorService}
     * is not null, the background task for initializing this object will be
     * scheduled at this service. Otherwise a new temporary {@code
     * ExecutorService} is created.
     *
     * @param exec an external {@link ExecutorService} to be used for task.
     * execution
     */
    protected BackgroundInitializer(final ExecutorService exec) {
        setExternalExecutor(exec);
    }

    /**
     * Constructs a new instance.
     *
     * @param initializer the initializer supplier called by {@link #initialize()}.
     * @param closer the closer consumer called by {@link #close()}.
     * @param exec the {@link ExecutorService} to be used @see #setExternalExecutor(ExecutorService)
     */
    private BackgroundInitializer(final FailableSupplier<T, ConcurrentException> initializer, final FailableConsumer<T, ConcurrentException> closer, final ExecutorService exec) {
        super(initializer, closer);
        setExternalExecutor(exec);
    }

    /**
     * Creates the {@link ExecutorService} to be used. This method is called if
     * no {@link ExecutorService} was provided at construction time.
     *
     * @return the {@link ExecutorService} to be used.
     */
    private ExecutorService createExecutor() {
        return Executors.newFixedThreadPool(getTaskCount());
    }

    /**
     * Creates a task for the background initialization. The {@link Callable}
     * object returned by this method is passed to the {@link ExecutorService}.
     * This implementation returns a task that invokes the {@link #initialize()}
     * method. If a temporary {@link ExecutorService} is used, it is destroyed
     * at the end of the task.
     *
     * @param execDestroy the {@link ExecutorService} to be destroyed by the
     * task.
     * @return a task for the background initialization.
     */
    private Callable<T> createTask(final ExecutorService execDestroy) {
        return new InitializationTask(execDestroy);
    }

    /**
     * Gets the result of the background initialization. This method blocks
     * until initialization is complete. If the background processing caused a
     * runtime exception, it is directly thrown by this method. Checked
     * exceptions, including {@link InterruptedException} are wrapped in a
     * {@link ConcurrentException}. Calling this method before {@link #start()}
     * was called causes an {@link IllegalStateException} exception to be
     * thrown.
     *
     * @return the object produced by this initializer.
     * @throws ConcurrentException if a checked exception occurred during
     * background processing.
     * @throws IllegalStateException if {@link #start()} has not been called.
     */
    @Override
    public T get() throws ConcurrentException {
        try {
            return getFuture().get();
        } catch (final ExecutionException execex) {
            ConcurrentUtils.handleCause(execex);
            return null; // should not be reached
        } catch (final InterruptedException iex) {
            // reset interrupted state
            Thread.currentThread().interrupt();
            throw new ConcurrentException(iex);
        }
    }

    /**
     * Gets the {@link ExecutorService} that is actually used for executing
     * the background task. This method can be called after {@link #start()}
     * (before {@code start()} it returns <strong>null</strong>). If an external executor
     * was set, this is also the active executor. Otherwise this method returns
     * the temporary executor that was created by this object.
     *
     * @return the {@link ExecutorService} for executing the background task.
     */
    protected final synchronized ExecutorService getActiveExecutor() {
        return executor;
    }

    /**
     * Gets the external {@link ExecutorService} to be used by this class.
     *
     * @return the {@link ExecutorService}.
     */
    public final synchronized ExecutorService getExternalExecutor() {
        return externalExecutor;
    }

    /**
     * Gets the {@link Future} object that was created when {@link #start()}
     * was called. Therefore this method can only be called after {@code
     * start()}.
     *
     * @return the {@link Future} object wrapped by this initializer.
     * @throws IllegalStateException if {@link #start()} has not been called.
     */
    public synchronized Future<T> getFuture() {
        if (future == null) {
            throw new IllegalStateException("start() must be called first!");
        }
        return future;
    }

    /**
     * Gets the number of background tasks to be created for this
     * initializer. This information is evaluated when a temporary {@code
     * ExecutorService} is created. This base implementation returns 1. Derived
     * classes that do more complex background processing can override it. This
     * method is called from a synchronized block by the {@link #start()}
     * method. Therefore overriding methods should be careful with obtaining
     * other locks and return as fast as possible.
     *
     * @return the number of background tasks required by this initializer.
     */
    protected int getTaskCount() {
        return 1;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Exception getTypedException(final Exception e) {
        //This Exception object will be used for type comparison in AbstractConcurrentInitializer.initialize but not thrown
        return new Exception(e);
    }

    /**
     * Tests whether this instance is initialized. Once initialized, always returns true.
     * If initialization failed then the failure will be cached and this will never return
     * true.
     *
     * @return true if initialization completed successfully, otherwise false.
     * @since 3.14.0
     */
    @Override
    public synchronized boolean isInitialized() {
        if (future == null || !future.isDone()) {
            return false;
        }
        try {
            future.get();
            return true;
        } catch (CancellationException | ExecutionException | InterruptedException e) {
            return false;
        }
    }

    /**
     * Returns a flag whether this {@link BackgroundInitializer} has already
     * been started.
     *
     * @return a flag whether the {@link #start()} method has already been
     * called.
     */
    public synchronized boolean isStarted() {
        return future != null;
    }

    /**
     * Sets an {@link ExecutorService} to be used by this class. The {@code
     * ExecutorService} passed to this method is used for executing the
     * background task. Thus it is possible to re-use an already existing
     * {@link ExecutorService} or to use a specially configured one. If no
     * {@link ExecutorService} is set, this instance creates a temporary one and
     * destroys it after background initialization is complete. Note that this
     * method must be called before {@link #start()}; otherwise an exception is
     * thrown.
     *
     * @param externalExecutor the {@link ExecutorService} to be used.
     * @throws IllegalStateException if this initializer has already been
     * started.
     */
    public final synchronized void setExternalExecutor(final ExecutorService externalExecutor) {
        if (isStarted()) {
            throw new IllegalStateException("Cannot set ExecutorService after start()!");
        }
        this.externalExecutor = externalExecutor;
    }

    /**
     * Starts the background initialization. With this method the initializer
     * becomes active and invokes the {@link #initialize()} method in a
     * background task. A {@link BackgroundInitializer} can be started exactly
     * once. The return value of this method determines whether the start was
     * successful: only the first invocation of this method returns <strong>true</strong>,
     * following invocations will return <strong>false</strong>.
     *
     * @return a flag whether the initializer could be started successfully.
     */
    public synchronized boolean start() {
        // Not yet started?
        if (!isStarted()) {
            // Determine the executor to use and whether a temporary one has to be created.
            final ExecutorService tempExec;
            executor = getExternalExecutor();
            if (executor == null) {
                executor = tempExec = createExecutor();
            } else {
                tempExec = null;
            }
            future = executor.submit(createTask(tempExec));
            return true;
        }
        return false;
    }
}
