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

import java.util.Objects;
import java.util.function.IntConsumer;

/**
 * A functional interface like {@link IntConsumer} but for {@code byte}.
 *
 * @see IntConsumer
 * @since 3.19.0
 */
@FunctionalInterface
public interface ByteConsumer {

    /** NOP singleton */
    ByteConsumer NOP = t -> {
        /* NOP */ };

    /**
     * Gets the NOP singleton.
     *
     * @return The NOP singleton.
     */
    static ByteConsumer nop() {
        return NOP;
    }

    /**
     * Accepts the given arguments.
     *
     * @param value the input argument
     */
    void accept(byte value);

    /**
     * Returns a composed {@link ByteConsumer} that performs, in sequence, this operation followed by the {@code after} operation. If performing either
     * operation throws an exception, it is relayed to the caller of the composed operation. If performing this operation throws an exception, the {@code after}
     * operation will not be performed.
     *
     * @param after the operation to perform after this operation
     * @return a composed {@link ByteConsumer} that performs in sequence this operation followed by the {@code after} operation
     * @throws NullPointerException if {@code after} is null
     */
    default ByteConsumer andThen(final ByteConsumer after) {
        Objects.requireNonNull(after);
        return (final byte t) -> {
            accept(t);
            after.accept(t);
        };
    }
}
