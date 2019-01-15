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


import java.util.function.BinaryOperator;

/**
 * Represent an operation upon two argument with the same type,producing  a result with the same type or throwing an checked exception
 * @param <T> the type of arguments and a result
 * @param <E> the type of exception thrown by the apply function
 * @since 3.9
 */
@FunctionalInterface
public interface CheckedBiOperator<T, E extends Exception> extends CheckedBiFunction<T, T, T, E> {

    /**
     * @param checkedBiOperator an object which will be mapped to {@link java.util.function.BinaryOperator}
     * @param <T> the type of functions arguments and a result
     * @param <E> the type of an exception which can be thrown by checkedBiOperator
     * @return an instance of unchecked BiFunction
     * @since 3.9
     */
    static <T, E extends Exception> BinaryOperator<T> unchecked (CheckedBiOperator<T, E> checkedBiOperator) {
        return (a, b) -> {
            try {
                return checkedBiOperator.apply(a, b);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        };
    }
}
