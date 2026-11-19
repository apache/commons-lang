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

import java.util.function.BooleanSupplier;
import java.util.function.DoubleSupplier;
import java.util.function.IntSupplier;
import java.util.function.LongSupplier;
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
     * The singleton supplier that always returns {@code true}.
     */
    private static final BooleanSupplier TRUE_SUPPLIER = () -> true;

    /**
     * The singleton supplier that always returns {@code false}.
     */
    private static final BooleanSupplier FALSE_SUPPLIER = () -> false;

    /**
     * Gets the result of {@link Supplier#get()}, or null if the supplier is null.
     *
     * @param <T> The type of results supplied by this supplier.
     * @param supplier The supplier or null.
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
     * Creates a {@link Supplier} that always returns the given value.
     *
     * @param value The value to be returned by the supplier.
     * @param <T>   Supplied type.
     * @return the supplier that always returns the given value.
     * @see #supplyBoolean(boolean)
     * @see #supplyByte(byte)
     * @see #supplyDouble(double)
     * @see #supplyInt(int)
     * @see #supplyLong(long)
     * @since 3.22.0
     */
    public static <T> Supplier<T> supply(final T value) {
        return () -> value;
    }

    /**
     * Gets a {@link BooleanSupplier} that always returns the given value.
     *
     * <p>
     * The returned supplier is a singleton.
     * </p>
     *
     * @param value The value to be returned by the supplier.
     * @return the supplier that always returns the given value.
     * @see #supply(Object)
     * @see #supplyByte(byte)
     * @see #supplyDouble(double)
     * @see #supplyInt(int)
     * @see #supplyLong(long)
     * @since 3.22.0
     */
    public static BooleanSupplier supplyBoolean(final boolean value) {
        return value ? TRUE_SUPPLIER : FALSE_SUPPLIER;
    }

    /**
     * Creates a {@link ByteSupplier} that always returns the given value.
     *
     * @param value The value to be returned by the supplier.
     * @return the supplier that always returns the given value.
     * @see #supply(Object)
     * @see #supplyBoolean(boolean)
     * @see #supplyDouble(double)
     * @see #supplyInt(int)
     * @see #supplyLong(long)
     * @since 3.22.0
     */
    public static ByteSupplier supplyByte(final byte value) {
        return () -> value;
    }

    /**
     * Creates a {@link DoubleSupplier} that always returns the given value.
     *
     * @param value The value to be returned by the supplier.
     * @return the supplier that always returns the given value.
     * @see #supply(Object)
     * @see #supplyBoolean(boolean)
     * @see #supplyByte(byte)
     * @see #supplyInt(int)
     * @see #supplyLong(long)
     * @since 3.22.0
     */
    public static DoubleSupplier supplyDouble(final double value) {
        return () -> value;
    }

    /**
     * Creates an {@link IntSupplier} that always returns the given value.
     *
     * @param value The value to be returned by the supplier.
     * @return the supplier that always returns the given value.
     * @see #supply(Object)
     * @see #supplyBoolean(boolean)
     * @see #supplyByte(byte)
     * @see #supplyDouble(double)
     * @see #supplyLong(long)
     * @since 3.22.0
     */
    public static IntSupplier supplyInt(final int value) {
        return () -> value;
    }

    /**
     * Creates a {@link LongSupplier} that always returns the given value.
     *
     * @param value The value to be returned by the supplier.
     * @return the supplier that always returns the given value.
     * @see #supply(Object)
     * @see #supplyBoolean(boolean)
     * @see #supplyByte(byte)
     * @see #supplyDouble(double)
     * @see #supplyInt(int)
     * @since 3.22.0
     */
    public static LongSupplier supplyLong(final long value) {
        return () -> value;
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
