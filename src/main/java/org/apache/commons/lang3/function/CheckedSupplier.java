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
 * Represents a supplier of results which can thrown a checked exception
 * @param <T> the type of a result
 * @param <E> the type of exception thrown
 * @since 3.9
 */
@FunctionalInterface
public interface CheckedSupplier<T, E extends Exception> {

    /**
     * @return a result
     * @throws E the type of an exception thrown
     * @since 3.9
     */
    T get() throws E;

    /**
     *
     * @param checkedSupplier the instance of CheckedSupplier which will be mapped to {@link java.util.function.Supplier}
     * @param <T> the return type from supplier
     * @param <E> the the type of exception thrown by CheckerSupplier
     * @return an instance of supplier which can throw a wrapped checked exception
     * since 3.9
     */
    static <T, E extends Exception> Supplier<T> unchecked(CheckedSupplier<T, E> checkedSupplier) {
        return () -> {
            try {
                return checkedSupplier.get();
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        };
    }
}
