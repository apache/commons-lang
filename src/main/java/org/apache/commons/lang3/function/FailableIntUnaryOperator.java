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
import java.util.function.IntUnaryOperator;

/**
 * A functional interface like {@link IntUnaryOperator} that declares a {@link Throwable}.
 *
 * @param <E> The kind of thrown exception or error.
 * @since 3.11
 */
public interface FailableIntUnaryOperator<E extends Throwable> {

    /** NOP singleton */
    @SuppressWarnings("rawtypes")
    FailableIntUnaryOperator NOP = t -> 0;

    /**
     * Returns a unary operator that always returns its input argument.
     *
     * @param <E> The kind of thrown exception or error.
     * @return a unary operator that always returns its input argument
     */
    static <E extends Throwable> FailableIntUnaryOperator<E> identity() {
        return t -> t;
    }

    /**
     * Returns The NOP singleton.
     *
     * @param <E> The kind of thrown exception or error.
     * @return The NOP singleton.
     */
    static <E extends Throwable> FailableIntUnaryOperator<E> nop() {
        return NOP;
    }

    /**
     * Returns a composed {@link FailableDoubleUnaryOperator} like {@link IntUnaryOperator#andThen(IntUnaryOperator)}.
     *
     * @param after the operator to apply after this one.
     * @return a composed {@link FailableIntUnaryOperator} like {@link IntUnaryOperator#andThen(IntUnaryOperator)}.
     * @throws NullPointerException if after is null.
     * @see #compose(FailableIntUnaryOperator)
     */
    default FailableIntUnaryOperator<E> andThen(final FailableIntUnaryOperator<E> after) {
        Objects.requireNonNull(after);
        return (final int t) -> after.applyAsInt(applyAsInt(t));
    }

    /**
     * Applies this operator to the given operand.
     *
     * @param operand the operand
     * @return the operator result
     * @throws E Thrown when a consumer fails.
     */
    int applyAsInt(int operand) throws E;

    /**
     * Returns a composed {@link FailableIntUnaryOperator} like {@link IntUnaryOperator#compose(IntUnaryOperator)}.
     *
     * @param before the operator to apply before this one.
     * @return a composed {@link FailableIntUnaryOperator} like {@link IntUnaryOperator#compose(IntUnaryOperator)}.
     * @throws NullPointerException if before is null.
     * @see #andThen(FailableIntUnaryOperator)
     */
    default FailableIntUnaryOperator<E> compose(final FailableIntUnaryOperator<E> before) {
        Objects.requireNonNull(before);
        return (final int v) -> applyAsInt(before.applyAsInt(v));
    }
}
