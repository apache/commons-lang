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

import java.util.function.BiPredicate;

/**
 * This is a checked predicate of two arguments
 * @param <T> the type of the first argument of the predicate
 * @param <U> the type of the second argument of the predicate
 * @param <E> the type of an exception thrown by the predicate
 * @since 3.9
 */
@FunctionalInterface
public interface CheckedBiPredicate<T, U, E extends Exception> {

    /**
     * Evaluates the predicate for the given arguments.
     *
     * @param t the first argument of the predicate
     * @param u the second argument of the predicate
     * @return true if the arguments matches the predicate, otherwise false
     * @throws E the exception thrown by this method
     * @since 3.9
     */
    boolean test(T t, U u) throws E;

    /**
     * create an instance of BiPredicate with the same behaviour like a checked version
     *
     * @param checkedBiPredicate an instance of CheckedBiPredicate which will be mapped to {@link BiPredicate}
     * @param <T>                the type of the first argument of the checked predicate
     * @param <U>                the type of the second argument of the checked predicate
     * @param <E>                the type of an exception thrown by the checked predicate
     * @return an instance of BiPredicate
     * @since 3.9
     */
    static <T, U, E extends Exception> BiPredicate<T, U> unchecked(CheckedBiPredicate<T, U, E> checkedBiPredicate) {
        return (t, u) -> {
            try {
                return checkedBiPredicate.test(t, u);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        };
    }

}
