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

import java.util.concurrent.CancellationException;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.FutureTask;
import java.util.function.Function;

import org.apache.commons.lang3.exception.ExceptionUtils;

/**
 * Definition of an interface for a wrapper around a calculation that takes a single parameter and returns a result. The
 * results for the calculation will be cached for future requests.
 *
 * <p>
 * This is not a fully functional cache: it is unbounded, and there is no way of limiting or removing results once they
 * have been generated. In particular, note the exception-caching default: unless the {@code recalculate} constructor
 * option is set to {@code true}, the <em>first</em> exception thrown by a calculation for a given parameter is cached
 * and rethrown for every future call with that parameter for the lifetime of this instance - a single transient
 * failure permanently poisons that key. Set {@code recalculate} to {@code true} to retry failed calculations on
 * subsequent calls instead.
 * </p>
 * <p>
 * Thanks go to Brian Goetz, Tim Peierls and the members of JCP JSR-166 Expert Group for coming up with the
 * original implementation of the class. It was also published within Java Concurrency in Practice as a sample.
 * </p>
 *
 * @param <I> The type of the input to the calculation
 * @param <O> The type of the output of the calculation
 * @since 3.6
 */
public class Memoizer<I, O> implements Computable<I, O> {

    private final ConcurrentMap<I, Future<O>> cache = new ConcurrentHashMap<>();
    private final Function<? super I, FutureTask<O>> mappingFunction;
    private final boolean recalculate;

    /**
     * Constructs a Memoizer for the provided Computable calculation.
     *
     * <p>
     * If a calculation throws an exception for any reason, this exception will be cached and returned for all future
     * calls with the provided parameter.
     * </p>
     *
     * @param computable The computation whose results should be memorized
     */
    public Memoizer(final Computable<I, O> computable) {
        this(computable, false);
    }

    /**
     * Constructs a Memoizer for the provided Computable calculation, with the option of whether a Computation that
     * experiences an error should recalculate on subsequent calls or return the same cached exception.
     *
     * @param computable The computation whose results should be memorized
     * @param recalculate determines whether the computation should be recalculated on subsequent calls if the previous call
     *        failed
     */
    public Memoizer(final Computable<I, O> computable, final boolean recalculate) {
        this.recalculate = recalculate;
        this.mappingFunction = k -> new FutureTask<>(() -> computable.compute(k));
    }

    /**
     * Constructs a Memoizer for the provided Function calculation.
     *
     * <p>
     * If a calculation throws an exception for any reason, this exception will be cached and returned for all future
     * calls with the provided parameter.
     * </p>
     *
     * @param function The function whose results should be memorized
     * @since 2.13.0
     */
    public Memoizer(final Function<I, O> function) {
        this(function, false);
    }

    /**
     * Constructs a Memoizer for the provided Function calculation, with the option of whether a Function that
     * experiences an error should recalculate on subsequent calls or return the same cached exception.
     *
     * @param function The computation whose results should be memorized
     * @param recalculate determines whether the computation should be recalculated on subsequent calls if the previous call
     *        failed
     * @since 2.13.0
     */
     public Memoizer(final Function<I, O> function, final boolean recalculate) {
        this.recalculate = recalculate;
        this.mappingFunction = k -> new FutureTask<>(() -> function.apply(k));
    }

    /**
     * This method will return the result of the calculation and cache it, if it has not previously been calculated.
     *
     * <p>
     * This cache will also cache exceptions that occur during the computation if the {@code recalculate} parameter in the
     * constructor was set to {@code false}, or not set: the first exception thrown for a given argument is rethrown for
     * every future call with that argument. Otherwise, if an exception happened on the previous calculation,
     * the method will attempt again to generate a value.
     * </p>
     * <p>
     * The calculation for a given argument runs at most once per cached entry and executes <em>outside</em> any internal
     * lock of the backing map (the pattern published in <em>Java Concurrency in Practice</em>): a slow calculation for
     * one key does not block calls for unrelated keys, and a calculation may itself use this Memoizer without
     * deadlocking. Concurrent callers for the same argument wait on the same {@link Future}.
     * </p>
     *
     * @param arg The argument for the calculation
     * @return The result of the calculation
     * @throws InterruptedException thrown if the calculation is interrupted
     */
    @Override
    public O compute(final I arg) throws InterruptedException {
        while (true) {
            Future<O> future = cache.get(arg);
            if (future == null) {
                final FutureTask<O> futureTask = mappingFunction.apply(arg);
                future = cache.putIfAbsent(arg, futureTask);
                if (future == null) {
                    // This thread won the race to install the task: run the user computation here,
                    // outside the ConcurrentHashMap's internal locks. Losing threads (and later
                    // callers) block on futureTask.get() instead of on a map bin lock.
                    future = futureTask;
                    futureTask.run();
                }
            }
            try {
                return future.get();
            } catch (final CancellationException e) {
                cache.remove(arg, future);
            } catch (final ExecutionException e) {
                if (recalculate) {
                    cache.remove(arg, future);
                }
                throw launderException(e.getCause());
            }
        }
    }

    /**
     * This method launders a Throwable to either a RuntimeException, Error or any other Exception wrapped in an
     * IllegalStateException.
     *
     * @param throwable The throwable to laundered
     * @return A RuntimeException, Error or an IllegalStateException
     */
    private RuntimeException launderException(final Throwable throwable) {
        throw new IllegalStateException("Unchecked exception", ExceptionUtils.throwUnchecked(throwable));
    }
}
