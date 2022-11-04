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

package org.apache.commons.lang3.mutable;

import java.io.Serializable;
import java.util.Objects;

/**
 * A mutable {@link Object} wrapper.
 *
 * @param <T> the type to set and get
 * @since 2.1
 */
public class MutableObject<T> implements Mutable<T>, Serializable {

    /**
     * Required for serialization support.
     *
     * @see java.io.Serializable
     */
    private static final long serialVersionUID = 86241875189L;

    /** The mutable value. */
    private T value;

    /**
     * Constructs a new MutableObject with the default value of {@code null}.
     */
    public MutableObject() {
    }

    /**
     * Constructs a new MutableObject with the specified value.
     *
     * @param value  the initial value to store
     */
    public MutableObject(final T value) {
        this.value = value;
    }

    /**
     * Gets the value.
     *
     * @return the value, may be null
     */
    @Override
    public T getValue() {
        return this.value;
    }

    /**
     * Sets the value.
     *
     * @param value  the value to set
     */
    @Override
    public void setValue(final T value) {
        this.value = value;
    }

    /**
     * Compares this object against the specified object. The result is {@code true} if and only if the argument
     * is not {@code null} and is a {@link MutableObject} object that contains the same {@link T}
     * value as this object.
     *
     * @param obj  the object to compare with, {@code null} returns {@code false}
     * @return  {@code true} if the objects are the same;
     *          {@code true} if the objects have equivalent {@code value} fields;
     *          {@code false} otherwise.
     */
    @Override
    public boolean equals(final Object obj) {
        if (obj == null) {
            return false;
        }
        if (this == obj) {
            return true;
        }
        if (this.getClass() == obj.getClass()) {
            final MutableObject<?> that = (MutableObject<?>) obj;
            return this.value.equals(that.value);
        }
        return false;
    }

    /**
     * Returns the value's hash code or {@code 0} if the value is {@code null}.
     *
     * @return the value's hash code or {@code 0} if the value is {@code null}.
     */
    @Override
    public int hashCode() {
        return Objects.hashCode(value);
    }

    /**
     * Returns the String value of this mutable.
     *
     * @return the mutable value as a string
     */
    @Override
    public String toString() {
        return Objects.toString(value);
    }

}
