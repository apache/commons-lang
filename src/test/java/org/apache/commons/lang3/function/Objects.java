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

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.apache.commons.lang3.ObjectUtils;

/**
 * This class provides some replacements for the corresponding methods in
 * {@link java.util.Objects}. The replacements have the advantage, that they are properly
 * annotated with {@link Nullable}, and/or {@link Nonnull}, so they let the
 * compiler know what their respective results are.
 *
 * The various {@code requireNonNull} methods are particularly handy, when
 * dealing with external code, that a) doesn't support the {@link Nonnull}
 * annotation, or if you know for other reasons, that an object is non-null.
 * Take for example, a {@link java.util.Map map}, that you have filled with
 * non-null values. So, in your opinion, the following should be perfectly
 * valid code:
 * <pre>
 *   final Map&lt;String,Object&gt; map = getMapOfNonNullValues();
 *   final @Nonnull Object o = map.get("SomeKey");
 * </pre>
 * However, if your Java compiler *does* null analysis, it will reject this
 * example as invalid, because {@link java.util.Map#get(Object)} might return
 * a null value. As a workaround, you can use this:
 * <pre>
 *   import static org.apache.commons.lang3.function.Objects.requireNonNull;
 *
 *   final Map&lt;String,Object&gt; map = getMapOfNonNullValues();
 *   final @Nonnull Object o = requireNonNull(map.get("SomeKey"));
 * </pre>
 *
 * This class is somewhat redundant with regards to {@link ObjectUtils}.
 * For example, {@link #requireNonNull(Object, Object)} is almost equivalent
 * with {@link ObjectUtils#defaultIfNull(Object, Object)}. However, it isn't
 * quite the same, because the latter can, in fact, return null. The former
 * can't, and the Java compiler confirms this.(An alternative to redundancy
 * would have been to change the {@code ObjectUtils} class. However, that
 * would mean loosing upwards compatibility, and we don't do that.)
 *
 * @since 3.12.0
 */
public class Objects {

    /**
     * Checks, whether the given object is non-null. If so, returns the non-null
     * object as a result value. Otherwise, a NullPointerException is thrown.
     * @param <T> The type of parameter {@code value}, also the result type.
     * @param value The value, which is being checked.
     * @return The given input value, if it was found to be non-null.
     * @throws NullPointerException The input value was null.
     * @see java.util.Objects#requireNonNull(Object)
     */
    public static <T> @Nonnull T requireNonNull(@Nullable final T value) throws NullPointerException {
        return requireNonNull(value, "The value must not be null.");
    }

    /**
     * Checks, whether the given object is non-null. If so, returns the non-null
     * object as a result value. Otherwise, invokes the given {@link Supplier},
     * and returns the suppliers result value.
     * @param <T> The type of parameter {@code value}, also the result type of
     *    the default value supplier, and of the method itself.
     * @param <E> The type of exception, that the {@code default value supplier},
     *    may throw.
     * @param value The value, which is being checked.
     * @param defaultValueSupplier The supplier, which returns the default value. This default
     *   value <em>must</em> be non-null. The supplier will only be invoked, if
     *   necessary. (If the {@code value} parameter is null, that is.)
     * @return The given input value, if it was found to be non-null. Otherwise,
     *   the value, that has been returned by the default value supplier.
     * @see #requireNonNull(Object)
     * @see #requireNonNull(Object, String)
     * @see #requireNonNull(Object, Supplier)
     * @throws NullPointerException The default value supplier is null, or the default
     *   value supplier has returned null.
     */
    public static <T, E extends Throwable> @Nonnull T requireNonNull(@Nullable final T value, @Nonnull final FailableSupplier<T, E> defaultValueSupplier) throws NullPointerException {
        if (value == null) {
            final FailableSupplier<T, ?> supplier = requireNonNull(defaultValueSupplier, "The supplier must not be null");
            final T defaultValue;
            try {
                defaultValue = supplier.get();
            } catch (final Throwable t) {
                throw Failable.rethrow(t);
            }
            return requireNonNull(defaultValue, "The supplier must not return null.");
        }
        return value;
    }

    /**
     * Checks, whether the given object is non-null. If so, returns the non-null
     * object as a result value. Otherwise, a NullPointerException is thrown.
     * @param <T> The type of parameter {@code value}, also the result type.
     * @param value The value, which is being checked.
     * @param msg A string, which is being used as the exceptions message, if the
     *   check fails.
     * @return The given input value, if it was found to be non-null.
     * @throws NullPointerException The input value was null.
     * @see java.util.Objects#requireNonNull(Object, String)
     * @see #requireNonNull(Object, Supplier)
     */
    public static <T> @Nonnull T requireNonNull(@Nullable final T value, @Nonnull final String msg) throws NullPointerException {
        if (value == null) {
            throw new NullPointerException(msg);
        }
        return value;
    }

    /**
     * Checks, whether the given object is non-null. If so, returns the non-null
     * object as a result value. Otherwise, a NullPointerException is thrown.
     * @param <T> The type of parameter {@code value}, also the result type.
     * @param value The value, which is being checked.
     * @param msgSupplier A supplier, which creates the exception message, if the check fails.
     *     This supplier will only be invoked, if necessary.
     * @return The given input value, if it was found to be non-null.
     * @throws NullPointerException The input value was null.
     * @see java.util.Objects#requireNonNull(Object, String)
     * @see #requireNonNull(Object, String)
     */
    public static <T> @Nonnull T requireNonNull(@Nullable final T value, @Nonnull final Supplier<String> msgSupplier) throws NullPointerException {
        if (value == null) {
            throw new NullPointerException(msgSupplier.get());
        }
        return value;
    }

    /**
     * Checks, whether the given object is non-null. If so, returns the non-null
     * object as a result value. Otherwise, a NullPointerException is thrown.
     * @param <T> The type of parameter {@code value}, also the result type.
     * @param value The value, which is being checked.
     * @param defaultValue The default value, which is being returned, if the
     *   check fails, and the {@code value} is null.
     * @throws NullPointerException The input value, and the default value are null.
     * @return The given input value, if it was found to be non-null.
     * @see java.util.Objects#requireNonNull(Object)
     */
    public static <T> @Nonnull T requireNonNull(@Nullable final T value, @Nonnull final T defaultValue) throws NullPointerException {
        return value == null ? requireNonNull(defaultValue) : value;
    }
}
