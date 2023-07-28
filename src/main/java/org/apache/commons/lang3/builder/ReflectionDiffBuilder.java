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
package org.apache.commons.lang3.builder;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.Arrays;

import org.apache.commons.lang3.ArraySorter;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.ClassUtils;
import org.apache.commons.lang3.reflect.FieldUtils;

/**
 * Assists in implementing {@link Diffable#diff(Object)} methods.
 *
 * <p>
 * All non-static, non-transient fields (including inherited fields)
 * of the objects to diff are discovered using reflection and compared
 * for differences.
 * </p>
 *
 * <p>
 * To use this class, write code as follows:
 * </p>
 *
 * <pre>
 * public class Person implements Diffable&lt;Person&gt; {
 *   String name;
 *   int age;
 *   boolean smoker;
 *   ...
 *
 *   public DiffResult diff(Person obj) {
 *     // No need for null check, as NullPointerException correct if obj is null
 *     return new ReflectionDiffBuilder(this, obj, ToStringStyle.SHORT_PREFIX_STYLE)
 *       .build();
 *   }
 * }
 * </pre>
 *
 * <p>
 * The {@link ToStringStyle} passed to the constructor is embedded in the
 * returned {@link DiffResult} and influences the style of the
 * {@code DiffResult.toString()} method. This style choice can be overridden by
 * calling {@link DiffResult#toString(ToStringStyle)}.
 * </p>
 * <p>
 * See {@link DiffBuilder} for a non-reflection based version of this class.
 * </p>
 * @param <T>
 *            type of the left and right object to diff.
 * @see Diffable
 * @see Diff
 * @see DiffResult
 * @see ToStringStyle
 * @see DiffBuilder
 * @since 3.6
 */
public class ReflectionDiffBuilder<T> implements Builder<DiffResult<T>> {

    private final T left;
    private final T right;
    private final DiffBuilder<T> diffBuilder;

    /**
     * Field names to exclude from output. Intended for fields like {@code "password"} or {@code "lastModificationDate"}.
     *
     * @since 3.13.0
     */
    private String[] excludeFieldNames;

    /**
     * Constructs a builder for the specified objects with the specified style.
     *
     * <p>
     * If {@code lhs == rhs} or {@code lhs.equals(rhs)} then the builder will
     * not evaluate any calls to {@code append(...)} and will return an empty
     * {@link DiffResult} when {@link #build()} is executed.
     * </p>
     * @param lhs
     *            {@code this} object
     * @param rhs
     *            the object to diff against
     * @param style
     *            the style will use when outputting the objects, {@code null}
     *            uses the default
     * @throws IllegalArgumentException
     *             if {@code lhs} or {@code rhs} is {@code null}
     */
    public ReflectionDiffBuilder(final T lhs, final T rhs, final ToStringStyle style) {
        this.left = lhs;
        this.right = rhs;
        this.diffBuilder = new DiffBuilder<>(lhs, rhs, style);
    }

    /**
     * Gets the field names that should be excluded from the diff.
     *
     * @return Returns the excludeFieldNames.
     * @since 3.13.0
     */
    public String[] getExcludeFieldNames() {
        return this.excludeFieldNames.clone();
    }


    /**
     * Sets the field names to exclude.
     *
     * @param excludeFieldNamesParam
     *            The field names to exclude from the diff or {@code null}.
     * @return {@code this}
     * @since 3.13.0
     */
    public ReflectionDiffBuilder<T> setExcludeFieldNames(final String... excludeFieldNamesParam) {
        if (excludeFieldNamesParam == null) {
            this.excludeFieldNames = ArrayUtils.EMPTY_STRING_ARRAY;
        } else {
            // clone and remove nulls
            this.excludeFieldNames = ArraySorter.sort(ReflectionToStringBuilder.toNoNullStringArray(excludeFieldNamesParam));
        }
        return this;
    }

    @Override
    public DiffResult<T> build() {
        if (left.equals(right)) {
            return diffBuilder.build();
        }

        appendFields(left.getClass());
        return diffBuilder.build();
    }

    private void appendFields(final Class<?> clazz) {
        for (final Field field : FieldUtils.getAllFields(clazz)) {
            if (accept(field)) {
                try {
                    diffBuilder.append(field.getName(), FieldUtils.readField(field, left, true), FieldUtils.readField(field, right, true));
                } catch (final IllegalAccessException e) {
                    // this can't happen. Would get a Security exception instead
                    // throw a runtime exception in case the impossible happens.
                    throw new IllegalArgumentException("Unexpected IllegalAccessException: " + e.getMessage(), e);
                }
            }
        }
    }

    private boolean accept(final Field field) {
        if (field.getName().indexOf(ClassUtils.INNER_CLASS_SEPARATOR_CHAR) != -1) {
            return false;
        }
        if (Modifier.isTransient(field.getModifiers())) {
            return false;
        }
        if (Modifier.isStatic(field.getModifiers())) {
            return false;
        }
        if (this.excludeFieldNames != null
                && Arrays.binarySearch(this.excludeFieldNames, field.getName()) >= 0) {
            // Reject fields from the getExcludeFieldNames list.
            return false;
        }
        return !field.isAnnotationPresent(DiffExclude.class);
    }

}
