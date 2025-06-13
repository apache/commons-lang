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
package org.apache.commons.lang3.builder;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.function.Supplier;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.ObjectUtils;

/**
 * Assists in implementing {@link Diffable#diff(Object)} methods.
 *
 * <p>
 * To use this class, write code as follows:
 * </p>
 *
 * <pre>{@code
 * public class Person implements Diffable<Person> {
 *   String name;
 *   int age;
 *   boolean smoker;
 *
 *   ...
 *
 *   public DiffResult<Person> diff(Person obj) {
 *     // No need for null check, as NullPointerException correct if obj is null
 *     return DiffBuilder.<Person>builder()
 *         .setLeft(this)
 *         .setRight(obj)
 *         .setStyle(ToStringStyle.SHORT_PREFIX_STYLE)
 *         .build()
 *       .append("name", this.name, obj.name)
 *       .append("age", this.age, obj.age)
 *       .append("smoker", this.smoker, obj.smoker)
 *       .build();
 *   }
 * }
 * }</pre>
 *
 * <p>
 * The {@link ToStringStyle} passed to the constructor is embedded in the returned {@link DiffResult} and influences the style of the
 * {@code DiffResult.toString()} method. This style choice can be overridden by calling {@link DiffResult#toString(ToStringStyle)}.
 * </p>
 * <p>
 * See {@link ReflectionDiffBuilder} for a reflection based version of this class.
 * </p>
 *
 * @param <T> type of the left and right object.
 * @see Diffable
 * @see Diff
 * @see DiffResult
 * @see ToStringStyle
 * @see ReflectionDiffBuilder
 * @since 3.3
 */
public class DiffBuilder<T> implements Builder<DiffResult<T>> {

    /**
     * Constructs a new instance.
     *
     * @param <T> type of the left and right object.
     * @since 3.15.0
     */
    public static final class Builder<T> {

        private T left;
        private T right;
        private ToStringStyle style;
        private boolean testObjectsEquals = true;
        private String toStringFormat = TO_STRING_FORMAT;

        /**
         * Constructs a new instance.
         */
        public Builder() {
            // empty
        }

        /**
         * Builds a new configured {@link DiffBuilder}.
         *
         * @return a new configured {@link DiffBuilder}.
         */
        public DiffBuilder<T> build() {
            return new DiffBuilder<>(left, right, style, testObjectsEquals, toStringFormat);
        }

        /**
         * Sets the left object.
         *
         * @param left the left object.
         * @return {@code this} instance.
         */
        public Builder<T> setLeft(final T left) {
            this.left = left;
            return this;
        }

        /**
         * Sets the right object.
         *
         * @param right the left object.
         * @return {@code this} instance.
         */
        public Builder<T> setRight(final T right) {
            this.right = right;
            return this;
        }

        /**
         * Sets the style will to use when outputting the objects, {@code null} uses the default.
         *
         * @param style the style to use when outputting the objects, {@code null} uses the default.
         * @return {@code this} instance.
         */
        public Builder<T> setStyle(final ToStringStyle style) {
            this.style = style != null ? style : ToStringStyle.DEFAULT_STYLE;
            return this;
        }

        /**
         * Sets whether to test if left and right are the same or equal. All of the append(fieldName, left, right) methods will abort without creating a field
         * {@link Diff} if the trivially equal test is enabled and returns true. The result of this test is never changed throughout the life of this
         * {@link DiffBuilder}.
         *
         * @param testObjectsEquals If true, this will test if lhs and rhs are the same or equal. All of the append(fieldName, left, right) methods will abort
         *                          without creating a field {@link Diff} if the trivially equal test is enabled and returns true. The result of this test is
         *                          never changed throughout the life of this {@link DiffBuilder}.
         * @return {@code this} instance.
         */
        public Builder<T> setTestObjectsEquals(final boolean testObjectsEquals) {
            this.testObjectsEquals = testObjectsEquals;
            return this;
        }

        /**
         * Sets the two-argument format string for {@link String#format(String, Object...)}, for example {@code "%s differs from %s"}.
         *
         * @param toStringFormat {@code null} uses the default.
         * @return {@code this} instance.
         */
        public Builder<T> setToStringFormat(final String toStringFormat) {
            this.toStringFormat = toStringFormat != null ? toStringFormat : TO_STRING_FORMAT;
            return this;
        }
    }

    private static final class SDiff<T> extends Diff<T> {

        private static final long serialVersionUID = 1L;
        private final SerializableSupplier<T> leftSupplier;
        private final SerializableSupplier<T> rightSupplier;

        private SDiff(final String fieldName, final SerializableSupplier<T> leftSupplier, final SerializableSupplier<T> rightSupplier, final Class<T> type) {
            super(fieldName, type);
            this.leftSupplier = Objects.requireNonNull(leftSupplier);
            this.rightSupplier = Objects.requireNonNull(rightSupplier);
        }

        @Override
        public T getLeft() {
            return leftSupplier.get();
        }

        @Override
        public T getRight() {
            return rightSupplier.get();
        }

    }

    /**
     * Private interface while we still have to support serialization.
     *
     * @param <T> the type of results supplied by this supplier.
     */
    private interface SerializableSupplier<T> extends Supplier<T>, Serializable {
        // empty
    }

    static final String TO_STRING_FORMAT = "%s differs from %s";

    /**
     * Constructs a new {@link Builder}.
     *
     * @param <T> type of the left and right object.
     * @return a new {@link Builder}.
     * @since 3.15.0
     */
    public static <T> Builder<T> builder() {
        return new Builder<>();
    }

    private final List<Diff<?>> diffs;
    private final boolean equals;
    private final T left;
    private final T right;
    private final ToStringStyle style;
    private final String toStringFormat;

    /**
     * Constructs a builder for the specified objects with the specified style.
     *
     * <p>
     * If {@code lhs == rhs} or {@code lhs.equals(rhs)} then the builder will not evaluate any calls to {@code append(...)} and will return an empty
     * {@link DiffResult} when {@link #build()} is executed.
     * </p>
     *
     * <p>
     * This delegates to {@link #DiffBuilder(Object, Object, ToStringStyle, boolean)} with the testTriviallyEqual flag enabled.
     * </p>
     *
     * @param left  {@code this} object
     * @param right the object to diff against
     * @param style the style to use when outputting the objects, {@code null} uses the default
     * @throws NullPointerException if {@code lhs} or {@code rhs} is {@code null}
     * @deprecated Use {@link Builder}.
     */
    @Deprecated
    public DiffBuilder(final T left, final T right, final ToStringStyle style) {
        this(left, right, style, true);
    }

    /**
     * Constructs a builder for the specified objects with the specified style.
     *
     * <p>
     * If {@code lhs == rhs} or {@code lhs.equals(rhs)} then the builder will not evaluate any calls to {@code append(...)} and will return an empty
     * {@link DiffResult} when {@link #build()} is executed.
     * </p>
     *
     * @param left              {@code this} object
     * @param right             the object to diff against
     * @param style             the style to use when outputting the objects, {@code null} uses the default
     * @param testObjectsEquals If true, this will test if lhs and rhs are the same or equal. All of the append(fieldName, lhs, rhs) methods will abort without
     *                          creating a field {@link Diff} if the trivially equal test is enabled and returns true. The result of this test is never changed
     *                          throughout the life of this {@link DiffBuilder}.
     * @throws NullPointerException if {@code lhs} or {@code rhs} is {@code null}
     * @since 3.4
     * @deprecated Use {@link Builder}.
     */
    @Deprecated
    public DiffBuilder(final T left, final T right, final ToStringStyle style, final boolean testObjectsEquals) {
        this(left, right, style, testObjectsEquals, TO_STRING_FORMAT);
    }

    private DiffBuilder(final T left, final T right, final ToStringStyle style, final boolean testObjectsEquals, final String toStringFormat) {
        this.left = Objects.requireNonNull(left, "left");
        this.right = Objects.requireNonNull(right, "right");
        this.diffs = new ArrayList<>();
        this.toStringFormat = toStringFormat;
        this.style = style != null ? style : ToStringStyle.DEFAULT_STYLE;
        // Don't compare any fields if objects equal
        this.equals = testObjectsEquals && Objects.equals(left, right);
    }

    private <F> DiffBuilder<T> add(final String fieldName, final SerializableSupplier<F> left, final SerializableSupplier<F> right, final Class<F> type) {
        diffs.add(new SDiff<>(fieldName, left, right, type));
        return this;
    }

    /**
     * Tests if two {@code boolean}s are equal.
     *
     * @param fieldName the field name
     * @param lhs       the left-hand side {@code boolean}
     * @param rhs       the right-hand side {@code boolean}
     * @return {@code this} instance.
     * @throws NullPointerException if field name is {@code null}
     */
    public DiffBuilder<T> append(final String fieldName, final boolean lhs, final boolean rhs) {
        return equals || lhs == rhs ? this : add(fieldName, () -> Boolean.valueOf(lhs), () -> Boolean.valueOf(rhs), Boolean.class);
    }

    /**
     * Tests if two {@code boolean[]}s are equal.
     *
     * @param fieldName the field name
     * @param lhs       the left-hand side {@code boolean[]}
     * @param rhs       the right-hand side {@code boolean[]}
     * @return {@code this} instance.
     * @throws NullPointerException if field name is {@code null}
     */
    public DiffBuilder<T> append(final String fieldName, final boolean[] lhs, final boolean[] rhs) {
        return equals || Arrays.equals(lhs, rhs) ? this : add(fieldName, () -> ArrayUtils.toObject(lhs), () -> ArrayUtils.toObject(rhs), Boolean[].class);
    }

    /**
     * Tests if two {@code byte}s are equal.
     *
     * @param fieldName the field name
     * @param lhs       the left-hand side {@code byte}
     * @param rhs       the right-hand side {@code byte}
     * @return {@code this} instance.
     * @throws NullPointerException if field name is {@code null}
     */
    public DiffBuilder<T> append(final String fieldName, final byte lhs, final byte rhs) {
        return equals || lhs == rhs ? this : add(fieldName, () -> Byte.valueOf(lhs), () -> Byte.valueOf(rhs), Byte.class);
    }

    /**
     * Tests if two {@code byte[]}s are equal.
     *
     * @param fieldName the field name
     * @param lhs       the left-hand side {@code byte[]}
     * @param rhs       the right-hand side {@code byte[]}
     * @return {@code this} instance.
     * @throws NullPointerException if field name is {@code null}
     */
    public DiffBuilder<T> append(final String fieldName, final byte[] lhs, final byte[] rhs) {
        return equals || Arrays.equals(lhs, rhs) ? this : add(fieldName, () -> ArrayUtils.toObject(lhs), () -> ArrayUtils.toObject(rhs), Byte[].class);
    }

    /**
     * Tests if two {@code char}s are equal.
     *
     * @param fieldName the field name
     * @param lhs       the left-hand side {@code char}
     * @param rhs       the right-hand side {@code char}
     * @return {@code this} instance.
     * @throws NullPointerException if field name is {@code null}
     */
    public DiffBuilder<T> append(final String fieldName, final char lhs, final char rhs) {
        return equals || lhs == rhs ? this : add(fieldName, () -> Character.valueOf(lhs), () -> Character.valueOf(rhs), Character.class);
    }

    /**
     * Tests if two {@code char[]}s are equal.
     *
     * @param fieldName the field name
     * @param lhs       the left-hand side {@code char[]}
     * @param rhs       the right-hand side {@code char[]}
     * @return {@code this} instance.
     * @throws NullPointerException if field name is {@code null}
     */
    public DiffBuilder<T> append(final String fieldName, final char[] lhs, final char[] rhs) {
        return equals || Arrays.equals(lhs, rhs) ? this : add(fieldName, () -> ArrayUtils.toObject(lhs), () -> ArrayUtils.toObject(rhs), Character[].class);
    }

    /**
     * Appends diffs from another {@link DiffResult}.
     *
     * <p>
     * Useful this method to compare properties which are themselves Diffable and would like to know which specific part of it is different.
     * </p>
     *
     * <pre>{@code
     * public class Person implements Diffable<Person> {
     *   String name;
     *   Address address; // implements Diffable<Address>
     *
     *   ...
     *
     *   public DiffResult diff(Person obj) {
     *     return new DiffBuilder(this, obj, ToStringStyle.SHORT_PREFIX_STYLE)
     *       .append("name", this.name, obj.name)
     *       .append("address", this.address.diff(obj.address))
     *       .build();
     *   }
     * }
     * }
     * </pre>
     *
     * @param fieldName  the field name
     * @param diffResult the {@link DiffResult} to append
     * @return {@code this} instance.
     * @throws NullPointerException if field name is {@code null} or diffResult is {@code null}
     * @since 3.5
     */
    public DiffBuilder<T> append(final String fieldName, final DiffResult<?> diffResult) {
        Objects.requireNonNull(diffResult, "diffResult");
        if (equals) {
            return this;
        }
        diffResult.getDiffs().forEach(diff -> append(fieldName + "." + diff.getFieldName(), diff.getLeft(), diff.getRight()));
        return this;
    }

    /**
     * Tests if two {@code double}s are equal.
     *
     * @param fieldName the field name
     * @param lhs       the left-hand side {@code double}
     * @param rhs       the right-hand side {@code double}
     * @return {@code this} instance.
     * @throws NullPointerException if field name is {@code null}
     */
    public DiffBuilder<T> append(final String fieldName, final double lhs, final double rhs) {
        return equals || Double.doubleToLongBits(lhs) == Double.doubleToLongBits(rhs) ? this
                : add(fieldName, () -> Double.valueOf(lhs), () -> Double.valueOf(rhs), Double.class);
    }

    /**
     * Tests if two {@code double[]}s are equal.
     *
     * @param fieldName the field name
     * @param lhs       the left-hand side {@code double[]}
     * @param rhs       the right-hand side {@code double[]}
     * @return {@code this} instance.
     * @throws NullPointerException if field name is {@code null}
     */
    public DiffBuilder<T> append(final String fieldName, final double[] lhs, final double[] rhs) {
        return equals || Arrays.equals(lhs, rhs) ? this : add(fieldName, () -> ArrayUtils.toObject(lhs), () -> ArrayUtils.toObject(rhs), Double[].class);
    }

    /**
     * Test if two {@code float}s are equal.
     *
     * @param fieldName the field name
     * @param lhs       the left-hand side {@code float}
     * @param rhs       the right-hand side {@code float}
     * @return {@code this} instance.
     * @throws NullPointerException if field name is {@code null}
     */
    public DiffBuilder<T> append(final String fieldName, final float lhs, final float rhs) {
        return equals || Float.floatToIntBits(lhs) == Float.floatToIntBits(rhs) ? this
                : add(fieldName, () -> Float.valueOf(lhs), () -> Float.valueOf(rhs), Float.class);
    }

    /**
     * Tests if two {@code float[]}s are equal.
     *
     * @param fieldName the field name
     * @param lhs       the left-hand side {@code float[]}
     * @param rhs       the right-hand side {@code float[]}
     * @return {@code this} instance.
     * @throws NullPointerException if field name is {@code null}
     */
    public DiffBuilder<T> append(final String fieldName, final float[] lhs, final float[] rhs) {
        return equals || Arrays.equals(lhs, rhs) ? this : add(fieldName, () -> ArrayUtils.toObject(lhs), () -> ArrayUtils.toObject(rhs), Float[].class);
    }

    /**
     * Tests if two {@code int}s are equal.
     *
     * @param fieldName the field name
     * @param lhs       the left-hand side {@code int}
     * @param rhs       the right-hand side {@code int}
     * @return {@code this} instance.
     * @throws NullPointerException if field name is {@code null}
     */
    public DiffBuilder<T> append(final String fieldName, final int lhs, final int rhs) {
        return equals || lhs == rhs ? this : add(fieldName, () -> Integer.valueOf(lhs), () -> Integer.valueOf(rhs), Integer.class);
    }

    /**
     * Tests if two {@code int[]}s are equal.
     *
     * @param fieldName the field name
     * @param lhs       the left-hand side {@code int[]}
     * @param rhs       the right-hand side {@code int[]}
     * @return {@code this} instance.
     * @throws NullPointerException if field name is {@code null}
     */
    public DiffBuilder<T> append(final String fieldName, final int[] lhs, final int[] rhs) {
        return equals || Arrays.equals(lhs, rhs) ? this : add(fieldName, () -> ArrayUtils.toObject(lhs), () -> ArrayUtils.toObject(rhs), Integer[].class);
    }

    /**
     * Tests if two {@code long}s are equal.
     *
     * @param fieldName the field name
     * @param lhs       the left-hand side {@code long}
     * @param rhs       the right-hand side {@code long}
     * @return {@code this} instance.
     * @throws NullPointerException if field name is {@code null}
     */
    public DiffBuilder<T> append(final String fieldName, final long lhs, final long rhs) {
        return equals || lhs == rhs ? this : add(fieldName, () -> Long.valueOf(lhs), () -> Long.valueOf(rhs), Long.class);
    }

    /**
     * Tests if two {@code long[]}s are equal.
     *
     * @param fieldName the field name
     * @param lhs       the left-hand side {@code long[]}
     * @param rhs       the right-hand side {@code long[]}
     * @return {@code this} instance.
     * @throws NullPointerException if field name is {@code null}
     */
    public DiffBuilder<T> append(final String fieldName, final long[] lhs, final long[] rhs) {
        return equals || Arrays.equals(lhs, rhs) ? this : add(fieldName, () -> ArrayUtils.toObject(lhs), () -> ArrayUtils.toObject(rhs), Long[].class);
    }

    /**
     * Tests if two {@link Objects}s are equal.
     *
     * @param fieldName the field name
     * @param lhs       the left-hand side {@link Object}
     * @param rhs       the right-hand side {@link Object}
     * @return {@code this} instance.
     * @throws NullPointerException if field name is {@code null}
     */
    public DiffBuilder<T> append(final String fieldName, final Object lhs, final Object rhs) {
        if (equals || lhs == rhs) {
            return this;
        }
        // rhs cannot be null, as lhs != rhs
        final Object test = lhs != null ? lhs : rhs;
        if (ObjectUtils.isArray(test)) {
            if (test instanceof boolean[]) {
                return append(fieldName, (boolean[]) lhs, (boolean[]) rhs);
            }
            if (test instanceof byte[]) {
                return append(fieldName, (byte[]) lhs, (byte[]) rhs);
            }
            if (test instanceof char[]) {
                return append(fieldName, (char[]) lhs, (char[]) rhs);
            }
            if (test instanceof double[]) {
                return append(fieldName, (double[]) lhs, (double[]) rhs);
            }
            if (test instanceof float[]) {
                return append(fieldName, (float[]) lhs, (float[]) rhs);
            }
            if (test instanceof int[]) {
                return append(fieldName, (int[]) lhs, (int[]) rhs);
            }
            if (test instanceof long[]) {
                return append(fieldName, (long[]) lhs, (long[]) rhs);
            }
            if (test instanceof short[]) {
                return append(fieldName, (short[]) lhs, (short[]) rhs);
            }
            return append(fieldName, (Object[]) lhs, (Object[]) rhs);
        }
        // Not array type
        return Objects.equals(lhs, rhs) ? this : add(fieldName, () -> lhs, () -> rhs, Object.class);
    }

    /**
     * Tests if two {@code Object[]}s are equal.
     *
     * @param fieldName the field name
     * @param lhs       the left-hand side {@code Object[]}
     * @param rhs       the right-hand side {@code Object[]}
     * @return {@code this} instance.
     * @throws NullPointerException if field name is {@code null}
     */
    public DiffBuilder<T> append(final String fieldName, final Object[] lhs, final Object[] rhs) {
        return equals || Arrays.equals(lhs, rhs) ? this : add(fieldName, () -> lhs, () -> rhs, Object[].class);
    }

    /**
     * Tests if two {@code short}s are equal.
     *
     * @param fieldName the field name
     * @param lhs       the left-hand side {@code short}
     * @param rhs       the right-hand side {@code short}
     * @return {@code this} instance.
     * @throws NullPointerException if field name is {@code null}
     */
    public DiffBuilder<T> append(final String fieldName, final short lhs, final short rhs) {
        return equals || lhs == rhs ? this : add(fieldName, () -> Short.valueOf(lhs), () -> Short.valueOf(rhs), Short.class);
    }

    /**
     * Tests if two {@code short[]}s are equal.
     *
     * @param fieldName the field name
     * @param lhs       the left-hand side {@code short[]}
     * @param rhs       the right-hand side {@code short[]}
     * @return {@code this} instance.
     * @throws NullPointerException if field name is {@code null}
     */
    public DiffBuilder<T> append(final String fieldName, final short[] lhs, final short[] rhs) {
        return equals || Arrays.equals(lhs, rhs) ? this : add(fieldName, () -> ArrayUtils.toObject(lhs), () -> ArrayUtils.toObject(rhs), Short[].class);
    }

    /**
     * Builds a {@link DiffResult} based on the differences appended to this builder.
     *
     * @return a {@link DiffResult} containing the differences between the two objects.
     */
    @Override
    public DiffResult<T> build() {
        return new DiffResult<>(left, right, diffs, style, toStringFormat);
    }

    /**
     * Gets the left object.
     *
     * @return the left object.
     */
    T getLeft() {
        return left;
    }

    /**
     * Gets the right object.
     *
     * @return the right object.
     */
    T getRight() {
        return right;
    }

}
