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

package org.apache.commons.lang3.mutable;

import java.util.function.Supplier;

/**
 * Provides mutable access to a value.
 * <p>
 * {@link Mutable} is used as a generic interface to the implementations in this package.
 * </p>
 * <p>
 * A typical use case would be to enable a primitive or string to be passed to a method and allow that method to effectively change the value of the
 * primitive/object. Another use case is to store a frequently changing primitive in a collection (for example a total in a map) without needing to create new
 * Integer/Long wrapper objects.
 * </p>
 *
 * @param <T> the type to wrap.
 * @since 2.1
 */
public interface Mutable<T> extends Supplier<T> {

    /**
     * Gets the value of this mutable.
     *
     * @return the stored value.
     * @since 3.18.0
     */
    default T get() {
        return getValue();
    }

    /**
     * Gets the value of this mutable.
     *
     * @return the stored value
     * @deprecated Use {@link #get()}.
     */
    @Deprecated
    T getValue();

    /**
     * Sets the value of this mutable.
     *
     * @param value the value to store
     * @throws NullPointerException if the object is null and null is invalid.
     * @throws ClassCastException   if the type is invalid.
     */
    void setValue(T value);
}
