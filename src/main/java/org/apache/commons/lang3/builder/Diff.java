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

import java.lang.reflect.Type;

import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.reflect.TypeUtils;
import org.apache.commons.lang3.tuple.Pair;

/**
 * A {@link Diff} contains the differences between two {@link Diffable} class
 * fields.
 *
 * <p>
 * Typically, {@link Diff}s are retrieved by using a {@link DiffBuilder} to
 * produce a {@link DiffResult}, containing the differences between two objects.
 * </p>
 *
 * @param <T>
 *            The type of object contained within this {@link Diff}. Differences
 *            between primitive objects are stored as their Object wrapper
 *            equivalent.
 * @since 3.3
 */
public abstract class Diff<T> extends Pair<T, T> {

    private static final long serialVersionUID = 1L;

    /** The field type. */
    private final Type type;

    /** The field name. */
    private final String fieldName;

    /**
     * Constructs a new {@link Diff} for the given field name.
     *
     * @param fieldName
     *            the field name
     */
    protected Diff(final String fieldName) {
        this.type = ObjectUtils.defaultIfNull(
                TypeUtils.getTypeArguments(getClass(), Diff.class).get(
                        Diff.class.getTypeParameters()[0]), Object.class);
        this.fieldName = fieldName;
    }

    /**
     * Gets the type of the field.
     *
     * @return the field type
     */
    public final Type getType() {
        return type;
    }

    /**
     * Gets the name of the field.
     *
     * @return the field name
     */
    public final String getFieldName() {
        return fieldName;
    }

    /**
     * Returns a {@link String} representation of the {@link Diff}, with the
     * following format:
     *
     * <pre>
     * [fieldname: left-value, right-value]
     * </pre>
     *
     * @return the string representation
     */
    @Override
    public final String toString() {
        return String.format("[%s: %s, %s]", fieldName, getLeft(), getRight());
    }

    /**
     * Throws {@link UnsupportedOperationException}.
     *
     * @param value
     *            ignored
     * @return nothing
     */
    @Override
    public final T setValue(final T value) {
        throw new UnsupportedOperationException("Cannot alter Diff object.");
    }
}
