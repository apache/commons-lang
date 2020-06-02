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

import java.util.function.BiFunction;

/**
 * Represents a function which takes two arguments and returns a result.
 * @param <T> the type of first argument
 * @param <U> the type of second argument
 * @param <R> the type of result
 * @param <E> the type of checked exception thrown by this function
 * @since 3.9
 */
@FunctionalInterface
public interface CheckedBiFunction<T, U, R, E extends Exception> {

    /**
     * @param argument1 a first argument for a given function
     * @param argument2 a second argument for a given function
     * @return a result of a given function
     * @throws E a checked exception
     * @since 3.9
     */
    R apply(T argument1, U argument2) throws E;

    /**
     *
     * @param checkedFunction an object which will be mapped to {@link java.util.function.BiFunction}
     * @param <T> the type of a first function argument
     * @param <U> the type of a second function argument
     * @param <R> the type of a function result
     * @param <E> the type of an exception which can be thrown by checkedFunction
     * @return an instance of unchecked BiFunction
     * @since 3.9
     */
    static <T, U, R, E extends Exception> BiFunction<T, U, R> unchecked (CheckedBiFunction<T, U, R, E> checkedFunction) {
        return (a, b) -> {
            try {
               return checkedFunction.apply(a, b);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        };
    }

}
