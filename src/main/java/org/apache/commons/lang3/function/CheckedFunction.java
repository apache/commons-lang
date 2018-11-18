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

public interface CheckedFunction<T, R, E extends Exception> {

    /**
     * @param argument an argument for a given function
     * @return result of a given function
     * @throws E an exception
     */
    R apply(T argument) throws E;

    /**
     * @param checkedFunction a function which returns
     * @param <T> a type of function argument
     * @param <R> a result of function
     * @param <E> a checked exception which can be thrown by wrapped function
     * @return function which throws a wrapped checked exception into a runtime exception
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
