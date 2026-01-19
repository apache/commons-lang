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

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.Objects;

import org.apache.commons.lang3.ArraySorter;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.ClassUtils;
import org.apache.commons.lang3.reflect.FieldUtils;

/**
 * Assists in implementing {@link Diffable#diff(Object)} methods.
 *
 * <p>
 * All non-static, non-transient fields (including inherited fields) of the objects to diff are discovered using reflection and compared for differences.
 * </p>
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
 *   ...
 *
 *   public DiffResult<Person> diff(Person obj) {
 *     // No need for null check, as NullPointerException correct if obj is null
 *     return ReflectionDiffBuilder.<Person>builder()
 *       .setDiffBuilder(DiffBuilder.<Person>builder()
 *           .setLeft(this)
 *           .setRight(obj)
 *           .setStyle(ToStringStyle.SHORT_PREFIX_STYLE)
 *           .build())
 *       .setExcludeFieldNames("userName", "password")
 *       .build()  // -> ReflectionDiffBuilder
 *       .build(); // -> DiffResult
 *   }
 * }
 * }</pre>
 *
 * <p>
 * The {@link ToStringStyle} passed to the constructor is embedded in the returned {@link DiffResult} and influences the style of the
 * {@code DiffResult.toString()} method. This style choice can be overridden by calling {@link DiffResult#toString(ToStringStyle)}.
 * </p>
 * <p>
 * See {@link DiffBuilder} for a non-reflection based version of this class.
 * </p>
 *
 * @param <T> type of the left and right object to diff.
 * @see Diffable
 * @see Diff
 * @see DiffResult
 * @see ToStringStyle
 * @see DiffBuilder
 * @since 3.6
 */
public class ReflectionDiffBuilder<T> implements Builder<DiffResult<T>> {

    /**
     * Constructs a new instance.
     *
     * @param <T> type of the left and right object.
     * @since 3.15.0
     */
    public static final class Builder<T> {

        private String[] excludeFieldNames = ArrayUtils.EMPTY_STRING_ARRAY;
        private DiffBuilder<T> diffBuilder;

        /**
         * Constructs a new instance.
         */
        public Builder() {
            // empty
        }

        /**
         * Builds a new configured {@link ReflectionDiffBuilder}.
         *
         * @return a new configured {@link ReflectionDiffBuilder}.
         */
        public ReflectionDiffBuilder<T> build() {
            return new ReflectionDiffBuilder<>(diffBuilder, excludeFieldNames);
        }

        /**
         * Sets the DiffBuilder.
         *
         * @param diffBuilder the DiffBuilder.
         * @return {@code this} instance.
         */
        public Builder<T> setDiffBuilder(final DiffBuilder<T> diffBuilder) {
            this.diffBuilder = diffBuilder;
            return this;
        }

        /**
         * Sets field names to exclude from output. Intended for fields like {@code "password"} or {@code "lastModificationDate"}.
         *
         * @param excludeFieldNames field names to exclude.
         * @return {@code this} instance.
         */
        public Builder<T> setExcludeFieldNames(final String... excludeFieldNames) {
            this.excludeFieldNames = toExcludeFieldNames(excludeFieldNames);
            return this;
        }

    }

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

    private static String[] toExcludeFieldNames(final String[] excludeFieldNames) {
        if (excludeFieldNames == null) {
            return ArrayUtils.EMPTY_STRING_ARRAY;
        }
        // clone and remove nulls
        return ArraySorter.sort(ReflectionToStringBuilder.toNoNullStringArray(excludeFieldNames));
    }

    private final DiffBuilder<T> diffBuilder;

    /**
     * Field names to exclude from output. Intended for fields like {@code "password"} or {@code "lastModificationDate"}.
     */
    private String[] excludeFieldNames;

    /**
     * Constructs a new instance.
     *
     * @param diffBuilder a non-null DiffBuilder.
     * @param excludeFieldNames a non-null String array.
     * @throw NullPointerException Thrown on null input.
     */
    private ReflectionDiffBuilder(final DiffBuilder<T> diffBuilder, final String[] excludeFieldNames) {
        this.diffBuilder = Objects.requireNonNull(diffBuilder, "diffBuilder");
        this.excludeFieldNames = Objects.requireNonNull(excludeFieldNames, "excludeFieldNames");
    }

    /**
     * Constructs a builder for the specified objects with the specified style.
     *
     * <p>
     * If {@code left == right} or {@code left.equals(right)} then the builder will not evaluate any calls to {@code append(...)} and will return an empty
     * {@link DiffResult} when {@link #build()} is executed.
     * </p>
     *
     * @param left  {@code this} object.
     * @param right the object to diff against.
     * @param style the style will use when outputting the objects, {@code null} uses the default
     * @throws IllegalArgumentException if {@code left} or {@code right} is {@code null}.
     * @deprecated Use {@link Builder}.
     */
    @Deprecated
    public ReflectionDiffBuilder(final T left, final T right, final ToStringStyle style) {
        this(DiffBuilder.<T>builder().setLeft(left).setRight(right).setStyle(style).build(), ArrayUtils.EMPTY_STRING_ARRAY);
    }

    private boolean accept(final Field field) {
        if (field.getName().indexOf(ClassUtils.INNER_CLASS_SEPARATOR_CHAR) != -1 || Modifier.isTransient(field.getModifiers())
                || Modifier.isStatic(field.getModifiers()) || Arrays.binarySearch(excludeFieldNames, field.getName()) >= 0) {
            // Rejected.
            return false;
        }
        return !field.isAnnotationPresent(DiffExclude.class);
    }

    /**
     * Appends fields using reflection.
     *
     * @throws SecurityException if an underlying accessible object's method denies the request.
     * @see SecurityManager#checkPermission
     */
    private void appendFields(final Class<?> clazz) {
        for (final Field field : FieldUtils.getAllFields(clazz)) {
            if (accept(field)) {
                try {
                    diffBuilder.append(field.getName(), readField(field, getLeft()), readField(field, getRight()));
                } catch (final IllegalAccessException e) {
                    // this can't happen. Would get a Security exception instead
                    // throw a runtime exception in case the impossible happens.
                    throw new IllegalArgumentException("Unexpected IllegalAccessException: " + e.getMessage(), e);
                }
            }
        }
    }

    /**
     * {@inheritDoc}
     *
     * @throws SecurityException if an underlying accessible object's method denies the request.
     * @see SecurityManager#checkPermission
     */
    @Override
    public DiffResult<T> build() {
        if (getLeft().equals(getRight())) {
            return diffBuilder.build();
        }
        appendFields(getLeft().getClass());
        return diffBuilder.build();
    }

    /**
     * Gets the field names that should be excluded from the diff.
     *
     * @return the excludeFieldNames.
     * @since 3.13.0
     */
    public String[] getExcludeFieldNames() {
        return excludeFieldNames.clone();
    }

    private T getLeft() {
        return diffBuilder.getLeft();
    }

    private T getRight() {
        return diffBuilder.getRight();
    }

    /**
     * Reads a {@link Field}, forcing access if needed.
     *
     * @param field  the field to use.
     * @param target the object to call on, may be {@code null} for {@code static} fields.
     * @return the field value.
     * @throws NullPointerException   if the field is {@code null}.
     * @throws IllegalAccessException if the field is not made accessible.
     * @throws SecurityException      if an underlying accessible object's method denies the request.
     * @see SecurityManager#checkPermission
     */
    private Object readField(final Field field, final Object target) throws IllegalAccessException {
        return FieldUtils.readField(field, target, true);
    }

    /**
     * Sets the field names to exclude.
     *
     * @param excludeFieldNames The field names to exclude from the diff or {@code null}.
     * @return {@code this} instance.
     * @since 3.13.0
     * @deprecated Use {@link Builder#setExcludeFieldNames(String[])}.
     */
    @Deprecated
    public ReflectionDiffBuilder<T> setExcludeFieldNames(final String... excludeFieldNames) {
        this.excludeFieldNames = toExcludeFieldNames(excludeFieldNames);
        return this;
    }

}
