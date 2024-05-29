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
 * Factory for {@link Function}.
 *
 * @since 3.14.0
 */
public final class Functions {

    /**
     * Applies the {@link Function} on the object if the function is not {@code null}. Otherwise, does nothing and returns {@code null}.
     *
     * @param function the function to apply.
     * @param object   the object to apply the function.
     *
     * @param <T>      the type of the argument the function applies.
     * @param <R>      the type of the result the function returns.
     * @return the value the function returns if the function is not {@code null}; {@code null} otherwise.
     * @since 3.15.0
     */
    public static <T, R> R apply(final Function<T, R> function, final T object) {
        return function != null ? function.apply(object) : null;
    }

    /**
     * Starts a fluent chain like {@code function(foo::bar).andThen(...).andThen(...).apply(...);}
     *
     * @param <T>      Input type.
     * @param <R>      Return type.
     * @param function the argument to return.
     * @return the argument
     */
    public static <T, R> Function<T, R> function(final Function<T, R> function) {
        return function;
    }

    private Functions() {
        // no instances needed.
    }
}
