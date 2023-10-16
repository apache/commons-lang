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

import java.util.function.Supplier;

/**
 * A functional interface like {@link Supplier} that declares a {@link Throwable}.
 *
 * @param <T> Return type.
 * @param <E> The kind of thrown exception or error.
 * @since 3.11
 */
@FunctionalInterface
public interface FailableSupplier<T, E extends Throwable> {

    /**
     * Returns the singleton supplier that always returns null.
     * <p>
     * This supplier never throws an exception.
     * </p>
     *
     * @since 3.14.0
     */
    @SuppressWarnings("rawtypes")
    FailableSupplier NUL = () -> null;

    /**
     * Returns the singleton supplier that always returns null.
     * <p>
     * This supplier never throws an exception.
     * </p>
     *
     * @param <T> Supplied type.
     * @param <E> The kind of thrown exception or error.
     * @return The NUL singleton.
     * @since 3.14.0
     */
    @SuppressWarnings("unchecked")
    static <T, E extends Exception> FailableSupplier<T, E> nul() {
        return NUL;
    }

    /**
     * Supplies an object
     *
     * @return a result
     * @throws E if the supplier fails
     */
    T get() throws E;
}
