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
 * A functional interface like {@link Function} that declares a {@code Throwable}.
 *
 * @param <T> Input type 1.
 * @param <R> Return type.
 * @param <E> Thrown exception.
 * @since 3.11
 */
@FunctionalInterface
public interface FailableFunction<T, R, E extends Throwable> {

    /** NOP singleton */
    @SuppressWarnings("rawtypes")
    final FailableFunction NOP = t -> null;

    /**
     * Returns The NOP singleton.
     *
     * @param <T> Consumed type 1.
     * @param <R> Return type.
     * @param <E> Thrown exception.
     * @return The NOP singleton.
     */
    static <T, R, E extends Throwable> FailableFunction<T, R, E> nop() {
        return NOP;
    }

    /**
     * Applies this function.
     *
     * @param input the input for the function
     * @return the result of the function
     * @throws E Thrown when the function fails.
     */
    R apply(T input) throws E;
}
