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

import java.util.function.LongPredicate;

/**
 * A functional interface like {@link LongPredicate} that declares a {@code Throwable}.
 *
 * @param <E> Thrown exception.
 * @since 3.11
 */
@FunctionalInterface
public interface FailableLongPredicate<E extends Throwable> {

    /** FALSE singleton */
    @SuppressWarnings("rawtypes")
    FailableLongPredicate FALSE = t -> false;

    /** TRUE singleton */
    @SuppressWarnings("rawtypes")
    FailableLongPredicate TRUE = t -> true;

    /**
     * Returns The FALSE singleton.
     *
     * @param <E> Thrown exception.
     * @return The NOP singleton.
     */
    static <E extends Throwable> FailableLongPredicate<E> falsePredicate() {
        return FALSE;
    }

    /**
     * Returns The FALSE TRUE.
     *
     * @param <E> Thrown exception.
     * @return The NOP singleton.
     */
    static <E extends Throwable> FailableLongPredicate<E> truePredicate() {
        return TRUE;
    }

    /**
     * Returns a predicate that negates this predicate.
     *
     * @return a predicate that negates this predicate.
     * @throws E Thrown when this predicate fails.
     */
    default FailableLongPredicate<E> negate() throws E {
        return t -> !test(t);
    }

    /**
     * Tests the predicate.
     *
     * @param value the parameter for the predicate to accept.
     * @return {@code true} if the input argument matches the predicate, {@code false} otherwise.
     * @throws E Thrown when the consumer fails.
     */
    boolean test(long value) throws E;
}
