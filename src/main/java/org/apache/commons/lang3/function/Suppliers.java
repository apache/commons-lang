/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
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
 * Helps use {@link Supplier}.
 *
 * @since 3.13.0
 */
public class Suppliers {

    /**
     * Returns the singleton supplier that always returns null.
     * <p>
     * This supplier never throws an exception.
     * </p>
     */
    @SuppressWarnings("rawtypes")
    private static Supplier NUL = () -> null;

    /**
     * Null-safe call to {@link Supplier#get()}.
     *
     * @param <T> the type of results supplied by this supplier.
     * @param supplier the supplier or null.
     * @return Result of {@link Supplier#get()} or null.
     */
    public static <T> T get(final Supplier<T> supplier) {
        return supplier == null ? null : supplier.get();
    }

    /**
     * Gets the singleton supplier that always returns null.
     * <p>
     * This supplier never throws an exception.
     * </p>
     *
     * @param <T> Supplied type.
     * @return The NUL singleton.
     * @since 3.14.0
     */
    @SuppressWarnings("unchecked")
    public static <T> Supplier<T> nul() {
        return NUL;
    }

    /**
     * Make private in 4.0.
     *
     * @deprecated TODO Make private in 4.0.
     */
    @Deprecated
    public Suppliers() {
        // empty
    }
}
