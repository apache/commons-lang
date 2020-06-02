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
import java.util.function.Function;

/**
 * Represents a function which accepts a single argument and returns a result or throws an exception (which can be checked)
 * @param <T> the type of an argument
 * @param <R> the type of a result
 * @param <E> the type of exception thrown by function
 * @since 3.9
 */
@FunctionalInterface
public interface CheckedFunction<T, R, E extends Exception> {

    /**
     * @param argument the argument for a given function
     * @return a result of the given function
     * @throws E the type of an exception thrown
     * @since 3.9
     */
    R apply(T argument) throws E;

    /**
     * @param checkedFunction the function which will be mapped to {@link java.util.function.Function}
     * @param <T> the type of the function argument
     * @param <R> the result of the function
     * @param <E> the checked exception which can be thrown by wrapped function
     * @return a function which can throw a wrapped checked exception into a runtime exception
     * @since 3.9
     */
    static <T, R, E extends Exception> Function<T, R> unchecked (CheckedFunction<T, R, E> checkedFunction) {
        return argument -> {
            try {
                return checkedFunction.apply(argument);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        };
    }

}
