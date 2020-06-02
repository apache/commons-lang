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

import java.util.function.Predicate;

/**
 * Represents a predicate of a single argument
 * @param <T> the type of the input to the CheckedPredicate
 * @since 3.9
 */
@FunctionalInterface
public interface CheckedPredicate<T, E extends Exception> {

    /**
     *
     * @param argument the argument which will be tested
     * @return true if the input matches the predicate, otherwise false
     * @throws E the type of exception thrown by the predicate
     * @since 3.9
     */
    boolean test(T argument) throws E;

    /**
     * @param checkedPredicate the instance of CheckedPredicate which will be mapped to {@link java.util.function.Predicate}
     * @param <T> the type of the argument
     * @param <E> the type of exception thrown by checkedPredicate
     * @return a predicate which can throw a wrapped checked exception
     * @Since 3.9
     */
    static <T, E extends Exception> Predicate<T> unchecked(CheckedPredicate<T, E> checkedPredicate) {
        return argument -> {
            try {
               return  (checkedPredicate.test(argument));
            } catch (Exception e) {
                throw  new RuntimeException(e);
            }
        };
    }

}
