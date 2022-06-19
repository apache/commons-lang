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
import java.util.function.BiPredicate;

/**
 * A functional interface like {@link BiPredicate} that declares a {@link Throwable}.
 *
 * @param <T> Predicate type 1.
 * @param <U> Predicate type 2.
 * @param <E> The kind of thrown exception or error.
 * @since 3.11
 */
@FunctionalInterface
public interface FailableBiPredicate<T, U, E extends Throwable> {

    /** FALSE singleton */
    @SuppressWarnings("rawtypes")
    FailableBiPredicate FALSE = (t, u) -> false;

    /** TRUE singleton */
    @SuppressWarnings("rawtypes")
    FailableBiPredicate TRUE = (t, u) -> true;

    /**
     * Returns The FALSE singleton.
     *
     * @param <T> Consumed type 1.
     * @param <U> Consumed type 2.
     * @param <E> The kind of thrown exception or error.
     * @return The NOP singleton.
     */
    static <T, U, E extends Throwable> FailableBiPredicate<T, U, E> falsePredicate() {
        return FALSE;
    }

    /**
     * Returns The TRUE singleton.
     *
     * @param <T> Consumed type 1.
     * @param <U> Consumed type 2.
     * @param <E> The kind of thrown exception or error.
     * @return The NOP singleton.
     */
    static <T, U, E extends Throwable> FailableBiPredicate<T, U, E> truePredicate() {
        return TRUE;
    }

    /**
     * Returns a composed {@link FailableBiPredicate} like {@link BiPredicate#and(BiPredicate)}.
     *
     * @param other a predicate that will be logically-ANDed with this predicate.
     * @return a composed {@link FailableBiPredicate} like {@link BiPredicate#and(BiPredicate)}.
     * @throws NullPointerException if other is null
     */
    default FailableBiPredicate<T, U, E> and(final FailableBiPredicate<? super T, ? super U, E> other) {
        Objects.requireNonNull(other);
        return (final T t, final U u) -> test(t, u) && other.test(t, u);
    }

    /**
     * Returns a predicate that negates this predicate.
     *
     * @return a predicate that negates this predicate.
     */
    default FailableBiPredicate<T, U, E> negate() {
        return (final T t, final U u) -> !test(t, u);
    }

    /**
     * Returns a composed {@link FailableBiPredicate} like {@link BiPredicate#and(BiPredicate)}.
     *
     * @param other a predicate that will be logically-ORed with this predicate.
     * @return a composed {@link FailableBiPredicate} like {@link BiPredicate#and(BiPredicate)}.
     * @throws NullPointerException if other is null
     */
    default FailableBiPredicate<T, U, E> or(final FailableBiPredicate<? super T, ? super U, E> other) {
        Objects.requireNonNull(other);
        return (final T t, final U u) -> test(t, u) || other.test(t, u);
    }

    /**
     * Tests the predicate.
     *
     * @param object1 the first object to test the predicate on
     * @param object2 the second object to test the predicate on
     * @return the predicate's evaluation
     * @throws E Thrown when this predicate fails.
     */
    boolean test(T object1, U object2) throws E;
}
