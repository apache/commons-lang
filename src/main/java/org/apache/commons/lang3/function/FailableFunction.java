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

package org.apache.commons.lang3.function;

import java.util.Objects;
import java.util.function.Function;

/**
 * A functional interface like {@link Function} that declares a {@link Throwable}.
 *
 * @param <T> Input type 1.
 * @param <R> Return type.
 * @param <E> The kind of thrown exception or error.
 * @since 3.11
 */
@FunctionalInterface
public interface FailableFunction<T, R, E extends Throwable> {

    /** NOP singleton */
    @SuppressWarnings("rawtypes")
    FailableFunction NOP = t -> null;

    /**
     * Returns a function that always returns its input argument.
     *
     * @param <T> the type of the input and output objects to the function
     * @param <E> The kind of thrown exception or error.
     * @return a function that always returns its input argument
     */
    static <T, E extends Throwable> FailableFunction<T, T, E> identity() {
        return t -> t;
    }

    /**
     * Returns The NOP singleton.
     *
     * @param <T> Consumed type 1.
     * @param <R> Return type.
     * @param <E> The kind of thrown exception or error.
     * @return The NOP singleton.
     */
    static <T, R, E extends Throwable> FailableFunction<T, R, E> nop() {
        return NOP;
    }

    /**
     * Returns a composed {@link FailableFunction} like {@link Function#andThen(Function)}.
     *
     * @param <V> the output type of the {@code after} function, and of the composed function.
     * @return a composed {@link FailableFunction} like {@link Function#andThen(Function)}.
     * @param after the operation to perform after this one.
     * @throws NullPointerException when {@code after} is null.
     */
    default <V> FailableFunction<T, V, E> andThen(final FailableFunction<? super R, ? extends V, E> after) {
        Objects.requireNonNull(after);
        return (final T t) -> after.apply(apply(t));
    }

    /**
     * Applies this function.
     *
     * @param input the input for the function
     * @return the result of the function
     * @throws E Thrown when the function fails.
     */
    R apply(T input) throws E;

    /**
     * Returns a composed {@link FailableFunction} like {@link Function#compose(Function)}.
     *
     * @param <V> the input type to the {@code before} function, and to the composed function.
     * @param before the operator to apply before this one.
     * @return a composed {@link FailableFunction} like {@link Function#compose(Function)}.
     * @throws NullPointerException if before is null.
     * @see #andThen(FailableFunction)
     */
    default <V> FailableFunction<V, R, E> compose(final FailableFunction<? super V, ? extends T, E> before) {
        Objects.requireNonNull(before);
        return (final V v) -> apply(before.apply(v));
    }
}
