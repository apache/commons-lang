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

import org.apache.commons.lang3.function.FailableSupplier;

/**
 * Abstracts supplying an instance of {@code T}. Use to implement the builder pattern.
 *
 * @param <T> The type of results supplied by this supplier.
 * @param <B> the type of builder.
 * @param <E> The kind of thrown exception or error.
 * @since 3.14.0
 */
public abstract class AbstractSupplier<T, B extends AbstractSupplier<T, B, E>, E extends Throwable> implements FailableSupplier<T, E> {

    /**
     * Constructs a new instance.
     */
    public AbstractSupplier() {
        // empty
    }

    /**
     * Returns this instance typed as the subclass type {@code B}.
     * <p>
     * This is the same as the expression:
     * </p>
     * <pre>
     * (B) this
     * </pre>
     *
     * @return this instance typed as the subclass type {@code B}.
     */
    @SuppressWarnings("unchecked")
    protected B asThis() {
        return (B) this;
    }

}
