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

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ExecutorService;

/**
 * A specialized {@link BackgroundInitializer} implementation that can deal with
 * multiple background initialization tasks.
 *
 * <p>
 * This class has a similar purpose as {@link BackgroundInitializer}. However,
 * it is not limited to a single background initialization task. Rather it
 * manages an arbitrary number of {@link BackgroundInitializer} objects,
 * executes them, and waits until they are completely initialized. This is
 * useful for applications that have to perform multiple initialization tasks
 * that can run in parallel (i.e. that do not depend on each other). This class
 * takes care about the management of an {@link ExecutorService} and shares it
 * with the {@link BackgroundInitializer} objects it is responsible for; so the
 * using application need not bother with these details.
 * </p>
 * <p>
 * The typical usage scenario for {@link MultiBackgroundInitializer} is as
 * follows:
 * </p>
 * <ul>
 * <li>Create a new instance of the class. Optionally pass in a pre-configured
 * {@link ExecutorService}. Alternatively {@link MultiBackgroundInitializer} can
 * create a temporary {@link ExecutorService} and delete it after initialization
 * is complete.</li>
 * <li>Create specialized {@link BackgroundInitializer} objects for the
 * initialization tasks to be performed and add them to the {@code
 * MultiBackgroundInitializer} using the
 * {@link #addInitializer(String, BackgroundInitializer)} method.</li>
 * <li>After all initializers have been added, call the {@link #start()} method.
 * </li>
 * <li>When access to the result objects produced by the {@code
 * BackgroundInitializer} objects is needed call the {@link #get()} method. The
 * object returned here provides access to all result objects created during
 * initialization. It also stores information about exceptions that have
 * occurred.</li>
 * </ul>
 * <p>
 * {@link MultiBackgroundInitializer} starts a special controller task that
 * starts all {@link BackgroundInitializer} objects added to the instance.
 * Before the an initializer is started it is checked whether this initializer
 * already has an {@link ExecutorService} set. If this is the case, this {@code
 * ExecutorService} is used for running the background task. Otherwise the
 * current {@link ExecutorService} of this {@link MultiBackgroundInitializer} is
 * shared with the initializer.
 * </p>
 * <p>
 * The easiest way of using this class is to let it deal with the management of
 * an {@link ExecutorService} itself: If no external {@link ExecutorService} is
 * provided, the class creates a temporary {@link ExecutorService} (that is
 * capable of executing all background tasks in parallel) and destroys it at the
 * end of background processing.
 * </p>
 * <p>
 * Alternatively an external {@link ExecutorService} can be provided - either at
 * construction time or later by calling the
 * {@link #setExternalExecutor(ExecutorService)} method. In this case all
 * background tasks are scheduled at this external {@link ExecutorService}.
 * <strong>Important note:</strong> When using an external {@code
 * ExecutorService} be sure that the number of threads managed by the service is
 * large enough. Otherwise a deadlock can happen! This is the case in the
 * following scenario: {@link MultiBackgroundInitializer} starts a task that
 * starts all registered {@link BackgroundInitializer} objects and waits for
 * their completion. If for instance a single threaded {@link ExecutorService}
 * is used, none of the background tasks can be executed, and the task created
 * by {@link MultiBackgroundInitializer} waits forever.
 * </p>
 *
 * @since 3.0
 */
public class MultiBackgroundInitializer
        extends
        BackgroundInitializer<MultiBackgroundInitializer.MultiBackgroundInitializerResults> {

    /** A map with the child initializers. */
    private final Map<String, BackgroundInitializer<?>> childInitializers = new HashMap<>();

    /**
     * Creates a new instance of {@link MultiBackgroundInitializer}.
     */
    public MultiBackgroundInitializer() {
    }

    /**
     * Creates a new instance of {@link MultiBackgroundInitializer} and
     * initializes it with the given external {@link ExecutorService}.
     *
     * @param exec the {@link ExecutorService} for executing the background
     * tasks
     */
    public MultiBackgroundInitializer(final ExecutorService exec) {
        super(exec);
    }

    /**
     * Adds a new {@link BackgroundInitializer} to this object. When this
     * {@link MultiBackgroundInitializer} is started, the given initializer will
     * be processed. This method must not be called after {@link #start()} has
     * been invoked.
     *
     * @param name the name of the initializer (must not be <b>null</b>)
     * @param backgroundInitializer the {@link BackgroundInitializer} to add (must not be
     * <b>null</b>)
     * @throws NullPointerException if either {@code name} or {@code backgroundInitializer}
     *         is {@code null}
     * @throws IllegalStateException if {@code start()} has already been called
     */
    public void addInitializer(final String name, final BackgroundInitializer<?> backgroundInitializer) {
        Objects.requireNonNull(name, "name");
        Objects.requireNonNull(backgroundInitializer, "backgroundInitializer");

        synchronized (this) {
            if (isStarted()) {
                throw new IllegalStateException("addInitializer() must not be called after start()!");
            }
            childInitializers.put(name, backgroundInitializer);
        }
    }

    /**
     * Returns the number of tasks needed for executing all child {@code
     * BackgroundInitializer} objects in parallel. This implementation sums up
     * the required tasks for all child initializers (which is necessary if one
     * of the child initializers is itself a {@link MultiBackgroundInitializer}
     * ). Then it adds 1 for the control task that waits for the completion of
     * the children.
     *
     * @return the number of tasks required for background processing
     */
    @Override
    protected int getTaskCount() {
        return 1 + childInitializers.values().stream().mapToInt(BackgroundInitializer::getTaskCount).sum();
    }

    /**
     * Creates the results object. This implementation starts all child {@code
     * BackgroundInitializer} objects. Then it collects their results and
     * creates a {@link MultiBackgroundInitializerResults} object with this
     * data. If a child initializer throws a checked exceptions, it is added to
     * the results object. Unchecked exceptions are propagated.
     *
     * @return the results object
     * @throws Exception if an error occurs
     */
    @Override
    protected MultiBackgroundInitializerResults initialize() throws Exception {
        final Map<String, BackgroundInitializer<?>> inits;
        synchronized (this) {
            // create a snapshot to operate on
            inits = new HashMap<>(childInitializers);
        }

        // start the child initializers
        final ExecutorService exec = getActiveExecutor();
        inits.values().forEach(bi -> {
            if (bi.getExternalExecutor() == null) {
                // share the executor service if necessary
                bi.setExternalExecutor(exec);
            }
            bi.start();
        });

        // collect the results
        final Map<String, Object> results = new HashMap<>();
        final Map<String, ConcurrentException> excepts = new HashMap<>();
        inits.forEach((k, v) -> {
            try {
                results.put(k, v.get());
            } catch (final ConcurrentException cex) {
                excepts.put(k, cex);
            }
        });

        return new MultiBackgroundInitializerResults(inits, results, excepts);
    }

    /**
     * A data class for storing the results of the background initialization
     * performed by {@link MultiBackgroundInitializer}. Objects of this inner
     * class are returned by {@link MultiBackgroundInitializer#initialize()}.
     * They allow access to all result objects produced by the
     * {@link BackgroundInitializer} objects managed by the owning instance. It
     * is also possible to retrieve status information about single
     * {@link BackgroundInitializer}s, i.e. whether they completed normally or
     * caused an exception.
     */
    public static class MultiBackgroundInitializerResults {
        /** A map with the child initializers. */
        private final Map<String, BackgroundInitializer<?>> initializers;

        /** A map with the result objects. */
        private final Map<String, Object> resultObjects;

        /** A map with the exceptions. */
        private final Map<String, ConcurrentException> exceptions;

        /**
         * Creates a new instance of {@link MultiBackgroundInitializerResults}
         * and initializes it with maps for the {@link BackgroundInitializer}
         * objects, their result objects and the exceptions thrown by them.
         *
         * @param inits the {@link BackgroundInitializer} objects
         * @param results the result objects
         * @param excepts the exceptions
         */
        private MultiBackgroundInitializerResults(
                final Map<String, BackgroundInitializer<?>> inits,
                final Map<String, Object> results,
                final Map<String, ConcurrentException> excepts) {
            initializers = inits;
            resultObjects = results;
            exceptions = excepts;
        }

        /**
         * Returns the {@link BackgroundInitializer} with the given name. If the
         * name cannot be resolved, an exception is thrown.
         *
         * @param name the name of the {@link BackgroundInitializer}
         * @return the {@link BackgroundInitializer} with this name
         * @throws NoSuchElementException if the name cannot be resolved
         */
        public BackgroundInitializer<?> getInitializer(final String name) {
            return checkName(name);
        }

        /**
         * Returns the result object produced by the {@code
         * BackgroundInitializer} with the given name. This is the object
         * returned by the initializer's {@code initialize()} method. If this
         * {@link BackgroundInitializer} caused an exception, <b>null</b> is
         * returned. If the name cannot be resolved, an exception is thrown.
         *
         * @param name the name of the {@link BackgroundInitializer}
         * @return the result object produced by this {@code
         * BackgroundInitializer}
         * @throws NoSuchElementException if the name cannot be resolved
         */
        public Object getResultObject(final String name) {
            checkName(name);
            return resultObjects.get(name);
        }

        /**
         * Returns a flag whether the {@link BackgroundInitializer} with the
         * given name caused an exception.
         *
         * @param name the name of the {@link BackgroundInitializer}
         * @return a flag whether this initializer caused an exception
         * @throws NoSuchElementException if the name cannot be resolved
         */
        public boolean isException(final String name) {
            checkName(name);
            return exceptions.containsKey(name);
        }

        /**
         * Returns the {@link ConcurrentException} object that was thrown by the
         * {@link BackgroundInitializer} with the given name. If this
         * initializer did not throw an exception, the return value is
         * <b>null</b>. If the name cannot be resolved, an exception is thrown.
         *
         * @param name the name of the {@link BackgroundInitializer}
         * @return the exception thrown by this initializer
         * @throws NoSuchElementException if the name cannot be resolved
         */
        public ConcurrentException getException(final String name) {
            checkName(name);
            return exceptions.get(name);
        }

        /**
         * Returns a set with the names of all {@link BackgroundInitializer}
         * objects managed by the {@link MultiBackgroundInitializer}.
         *
         * @return an (unmodifiable) set with the names of the managed {@code
         * BackgroundInitializer} objects
         */
        public Set<String> initializerNames() {
            return Collections.unmodifiableSet(initializers.keySet());
        }

        /**
         * Returns a flag whether the whole initialization was successful. This
         * is the case if no child initializer has thrown an exception.
         *
         * @return a flag whether the initialization was successful
         */
        public boolean isSuccessful() {
            return exceptions.isEmpty();
        }

        /**
         * Checks whether an initializer with the given name exists. If not,
         * throws an exception. If it exists, the associated child initializer
         * is returned.
         *
         * @param name the name to check
         * @return the initializer with this name
         * @throws NoSuchElementException if the name is unknown
         */
        private BackgroundInitializer<?> checkName(final String name) {
            final BackgroundInitializer<?> init = initializers.get(name);
            if (init == null) {
                throw new NoSuchElementException(
                        "No child initializer with name " + name);
            }

            return init;
        }
    }
}
