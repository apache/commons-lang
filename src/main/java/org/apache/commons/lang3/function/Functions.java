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

import java.util.Objects;
import java.util.Optional;
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
     * @param <T>      the type of the argument the function applies.
     * @param <R>      the type of the result the function returns.
     * @return the value the function returns if the function is not {@code null}; {@code null} otherwise.
     * @since 3.15.0
     */
    public static <T, R> R apply(final Function<T, R> function, final T object) {
        return function != null ? function.apply(object) : null;
    }

    /**
     * Applies a value to a function if the value isn't {@code null}, otherwise the method returns {@code null}. If the value isn't {@code null} then return the
     * result of the applying function.
     *
     * <pre>{@code
     * Functions.applyNonNull("a", String::toUpperCase)  = "A"
     * Functions.applyNonNull(null, String::toUpperCase) = null
     * Functions.applyNonNull("a", s -> null)            = null
     * }</pre>
     * <p>
     * Useful when working with expressions that may return {@code null} as it allows a single-line expression without using temporary local variables or
     * evaluating expressions twice. Provides an alternative to using {@link Optional} that is shorter and has less allocation.
     * </p>
     *
     * @param <T>    The type of the input of this method and the function.
     * @param <R>    The type of the result of the function and this method.
     * @param value  The value to apply the function to, may be {@code null}.
     * @param mapper The function to apply, must not be {@code null}.
     * @return The result of the function (which may be {@code null}) or {@code null} if the input value is {@code null}.
     * @see #applyNonNull(Object, Function, Function)
     * @see #applyNonNull(Object, Function, Function, Function)
     * @since 3.19.0
     */
    public static <T, R> R applyNonNull(final T value, final Function<? super T, ? extends R> mapper) {
        return value != null ? Objects.requireNonNull(mapper, "mapper").apply(value) : null;
    }

    /**
     * Applies values to a chain of functions, where a {@code null} can short-circuit each step. A function is only applied if the previous value is not
     * {@code null}, otherwise this method returns {@code null}.
     *
     * <pre>{@code
     * Functions.applyNonNull(" a ", String::toUpperCase, String::trim) = "A"
     * Functions.applyNonNull(null, String::toUpperCase, String::trim)  = null
     * Functions.applyNonNull(" a ", s -> null, String::trim)           = null
     * Functions.applyNonNull(" a ", String::toUpperCase, s -> null)    = null
     * }</pre>
     * <p>
     * Useful when working with expressions that may return {@code null} as it allows a single-line expression without using temporary local variables or
     * evaluating expressions twice. Provides an alternative to using {@link Optional} that is shorter and has less allocation.
     * </p>
     *
     * @param <T>     The type of the input of this method and the first function.
     * @param <U>     The type of the result of the first function and the input to the second function.
     * @param <R>     The type of the result of the second function and this method.
     * @param value1  The value to apply the functions to, may be {@code null}.
     * @param mapper1 The first function to apply, must not be {@code null}.
     * @param mapper2 The second function to apply, must not be {@code null}.
     * @return The result of the final function (which may be {@code null}) or {@code null} if the input value or any intermediate value is {@code null}.
     * @see #applyNonNull(Object, Function)
     * @see #applyNonNull(Object, Function, Function, Function)
     * @since 3.19.0
     */
    public static <T, U, R> R applyNonNull(final T value1, final Function<? super T, ? extends U> mapper1, final Function<? super U, ? extends R> mapper2) {
        return applyNonNull(applyNonNull(value1, mapper1), mapper2);
    }

    /**
     * Applies values to a chain of functions, where a {@code null} can short-circuit each step. A function is only applied if the previous value is not
     * {@code null}, otherwise this method returns {@code null}.
     *
     * <pre>{@code
     * Functions.applyNonNull(" abc ", String::toUpperCase, String::trim, StringUtils::reverse) = "CBA"
     * Functions.applyNonNull(null, String::toUpperCase, String::trim, StringUtils::reverse)    = null
     * Functions.applyNonNull(" abc ", s -> null, String::trim, StringUtils::reverse)           = null
     * Functions.applyNonNull(" abc ", String::toUpperCase, s -> null, StringUtils::reverse)    = null
     * Functions.applyNonNull(" abc ", String::toUpperCase, String::trim, s -> null)            = null
     * }</pre>
     * <p>
     * Useful when working with expressions that may return {@code null} as it allows a single-line expression without using temporary local variables or
     * evaluating expressions twice. Provides an alternative to using {@link Optional} that is shorter and has less allocation.
     * </p>
     *
     * @param <T>     The type of the input of this method and the first function.
     * @param <U>     The type of the result of the first function and the input to the second function.
     * @param <V>     The type of the result of the second function and the input to the third function.
     * @param <R>     The type of the result of the third function and this method.
     * @param value1  The value to apply the first function, may be {@code null}.
     * @param mapper1 The first function to apply, must not be {@code null}.
     * @param mapper2 The second function to apply, must not be {@code null}.
     * @param mapper3 The third function to apply, must not be {@code null}.
     * @return The result of the final function (which may be {@code null}) or {@code null} if the input value or any intermediate value is {@code null}.
     * @see #applyNonNull(Object, Function)
     * @see #applyNonNull(Object, Function, Function)
     * @since 3.19.0
     */
    public static <T, U, V, R> R applyNonNull(final T value1, final Function<? super T, ? extends U> mapper1, final Function<? super U, ? extends V> mapper2,
            final Function<? super V, ? extends R> mapper3) {
        return applyNonNull(applyNonNull(applyNonNull(value1, mapper1), mapper2), mapper3);
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
