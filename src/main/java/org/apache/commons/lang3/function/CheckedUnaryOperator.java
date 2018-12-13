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

import java.util.function.UnaryOperator;

/**
 * Represents a function which accepts a single argument and returns a result which has the same type
 *
 * @param <T> the type of an argument and a result
 * @param <E> the type of an exception which can be produced by the apply method
 * @since 3.9
 */
@FunctionalInterface
public interface CheckedUnaryOperator<T, E extends Exception> extends CheckedFunction<T, T, E> {

    /**
     * @param checkedUnaryOperator an unaryOperator which which will be mapped to {@link java.util.function.UnaryOperator}
     * @param <T>                  the type of function argument
     * @param <E>                  the exception of the function
     * @return function which can throw a wrapped checked exception into the runtime exception
     * @since 3.9
     */
    static <T, E extends Exception> UnaryOperator<T> unchecked(CheckedUnaryOperator<T, E> checkedUnaryOperator) {
        return arg -> {
            try {
                return checkedUnaryOperator.apply(arg);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        };
    }

}
