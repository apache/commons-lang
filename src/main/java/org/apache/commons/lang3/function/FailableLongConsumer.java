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

import java.util.Objects;
import java.util.function.LongConsumer;

/**
 * A functional interface like {@link LongConsumer} that declares a {@link Throwable}.
 *
 * @param <E> The kind of thrown exception or error.
 * @since 3.11
 */
@FunctionalInterface
public interface FailableLongConsumer<E extends Throwable> {

    /** NOP singleton */
    @SuppressWarnings("rawtypes")
    FailableLongConsumer NOP = t -> {/* NOP */};

    /**
     * Returns The NOP singleton.
     *
     * @param <E> The kind of thrown exception or error.
     * @return The NOP singleton.
     */
    static <E extends Throwable> FailableLongConsumer<E> nop() {
        return NOP;
    }

    /**
     * Accepts the consumer.
     *
     * @param object the parameter for the consumable to accept
     * @throws E Thrown when the consumer fails.
     */
    void accept(long object) throws E;

    /**
     * Returns a composed {@link FailableLongConsumer} like {@link LongConsumer#andThen(LongConsumer)}.
     *
     * @param after the operation to perform after this one.
     * @return a composed {@link FailableLongConsumer} like {@link LongConsumer#andThen(LongConsumer)}.
     * @throws NullPointerException if {@code after} is null
     */
    default FailableLongConsumer<E> andThen(final FailableLongConsumer<E> after) {
        Objects.requireNonNull(after);
        return (final long t) -> {
            accept(t);
            after.accept(t);
        };
    }
}
