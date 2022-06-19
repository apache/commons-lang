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
import java.util.function.BiFunction;
import java.util.function.Function;

/**
 * A functional interface like {@link BiFunction} that declares a {@link Throwable}.
 *
 * @param <T> Input type 1.
 * @param <U> Input type 2.
 * @param <R> Return type.
 * @param <E> The kind of thrown exception or error.
 * @since 3.11
 */
@FunctionalInterface
public interface FailableBiFunction<T, U, R, E extends Throwable> {

    /** NOP singleton */
    @SuppressWarnings("rawtypes")
    FailableBiFunction NOP = (t, u) -> null;

    /**
     * Returns The NOP singleton.
     *
     * @param <T> Consumed type 1.
     * @param <U> Consumed type 2.
     * @param <R> Return type.
     * @param <E> The kind of thrown exception or error.
     * @return The NOP singleton.
     */
    static <T, U, R, E extends Throwable> FailableBiFunction<T, U, R, E> nop() {
        return NOP;
    }

    /**
     * Returns a composed {@link FailableBiFunction} that like {@link BiFunction#andThen(Function)}.
     *
     * @param <V> the output type of the {@code after} function, and of the composed function.
     * @param after the operation to perform after this one.
     * @return a composed {@link FailableBiFunction} that like {@link BiFunction#andThen(Function)}.
     * @throws NullPointerException when {@code after} is null.
     */
    default <V> FailableBiFunction<T, U, V, E> andThen(final FailableFunction<? super R, ? extends V, E> after) {
        Objects.requireNonNull(after);
        return (final T t, final U u) -> after.apply(apply(t, u));
    }

    /**
     * Applies this function.
     *
     * @param input1 the first input for the function
     * @param input2 the second input for the function
     * @return the result of the function
     * @throws E Thrown when the function fails.
     */
    R apply(T input1, U input2) throws E;
}
