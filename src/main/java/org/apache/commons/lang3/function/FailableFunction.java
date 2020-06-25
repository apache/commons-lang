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
 * A functional interface like {@link Function} that declares a {@code Throwable}.
 *
 * @param <T> Input type 1.
 * @param <R> Return type.
 * @param <E> Thrown exception.
 * @since 3.11
 */
@FunctionalInterface
public interface FailableFunction<T, R, E extends Throwable> {

    /** NOP singleton */
    @SuppressWarnings("rawtypes")
    final FailableFunction NOP = t -> null;

    /**
     * Returns The NOP singleton.
     *
     * @param <T> Consumed type 1.
     * @param <R> Return type.
     * @param <E> Thrown exception.
     * @return The NOP singleton.
     */
    static <T, R, E extends Throwable> FailableFunction<T, R, E> nop() {
        return NOP;
    }

    /**
     * Returns a composed {@code FailableFunction} like {@link Function#andThen(Function)}.
     *
     * @param <V> the output type of the {@code after} function, and of the composed function.
     * @return a composed {@code FailableFunction} like {@link Function#andThen(Function)}.
     * @param after the operation to perform after this one.
     * @throws NullPointerException when {@code after} is null.
     * @throws E Thrown when a consumer fails.
     */
    default <V> FailableFunction<T, V, E> andThen(final FailableFunction<? super R, ? extends V, E> after) throws E {
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

}
