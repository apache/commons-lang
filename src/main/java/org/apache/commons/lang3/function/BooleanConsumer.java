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
import java.util.function.IntConsumer;

/**
 * A functional interface like {@link IntConsumer} but for {@code boolean}.
 *
 * @see IntConsumer
 * @since 3.13.0
 */
@FunctionalInterface
public interface BooleanConsumer {

    /** NOP singleton */
    BooleanConsumer NOP = t -> {/* NOP */};

    /**
     * Returns The NOP singleton.
     *
     * @return The NOP singleton.
     */
    static BooleanConsumer nop() {
        return NOP;
    }

    /**
     * Performs this operation on the given argument.
     *
     * @param value the input argument
     */
    void accept(boolean value);

    /**
     * Returns a composed {@link BooleanConsumer} that performs, in sequence, this operation followed by the {@code after}
     * operation. If performing either operation throws an exception, it is relayed to the caller of the composed operation.
     * If performing this operation throws an exception, the {@code after} operation will not be performed.
     *
     * @param after the operation to perform after this operation
     * @return a composed {@link BooleanConsumer} that performs in sequence this operation followed by the {@code after}
     *         operation
     * @throws NullPointerException if {@code after} is null
     */
    default BooleanConsumer andThen(final BooleanConsumer after) {
        Objects.requireNonNull(after);
        return (final boolean t) -> {
            accept(t);
            after.accept(t);
        };
    }
}
