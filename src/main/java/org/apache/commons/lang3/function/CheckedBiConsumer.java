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

import java.util.function.BiConsumer;

/**
 * Represents a function which takes two arguments and returns nothing
 * @param <T> the type of the first argument
 * @param <U> the type of the second argument
 * @param <E> the type of exception thrown by function
 * @since 3.9
 */
@FunctionalInterface
public interface CheckedBiConsumer<T, U, E extends Exception> {

    /**
     * Performs this operations against two arguments
     * @param argument1 the first input argument
     * @param argument2 the second input argument
     * @throws E a exception thrown during method execution
     * @since 3.9
     */
    void accept (T argument1, U argument2) throws E;

    /**
     *
     * @param checkedBiConsumer the CheckedBiConsumer which will be mapped to {@link java.util.function.BiConsumer}
     * @param <T> the first argument for BiConsumer
     * @param <U> the second argument for BiConsumer
     * @param <E> the exception thrown by an instance of CheckedBiConsumer
     * @return an instance of BiConsumer
     * @since 3.9
     */
    static <T, U, E extends Exception>  BiConsumer<T, U>  unchecked(CheckedBiConsumer<T, U, E> checkedBiConsumer) {
        return (arg1, arg2) -> {
            try {
                checkedBiConsumer.accept(arg1, arg2);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        };
    }
}
