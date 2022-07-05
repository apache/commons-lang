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

import java.util.function.DoubleFunction;

/**
 * A functional interface like {@link DoubleFunction} that declares a {@link Throwable}.
 *
 * @param <R> Return type.
 * @param <E> The kind of thrown exception or error.
 * @since 3.11
 */
@FunctionalInterface
public interface FailableDoubleFunction<R, E extends Throwable> {

    /** NOP singleton */
    @SuppressWarnings("rawtypes")
    FailableDoubleFunction NOP = t -> null;

    /**
     * Returns The NOP singleton.
     *
     * @param <R> Return type.
     * @param <E> The kind of thrown exception or error.
     * @return The NOP singleton.
     */
    static <R, E extends Throwable> FailableDoubleFunction<R, E> nop() {
        return NOP;
    }

    /**
     * Applies this function.
     *
     * @param input the input for the function
     * @return the result of the function
     * @throws E Thrown when the function fails.
     */
    R apply(double input) throws E;
}
