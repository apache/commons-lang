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

import java.util.function.ObjLongConsumer;

/**
 * A functional interface like {@link ObjLongConsumer} that declares a {@link Throwable}.
 *
 * @param <T> the type of the object argument to the operation.
 * @param <E> The kind of thrown exception or error.
 * @since 3.11
 */
@FunctionalInterface
public interface FailableObjLongConsumer<T, E extends Throwable> {

    /** NOP singleton */
    @SuppressWarnings("rawtypes")
    FailableObjLongConsumer NOP = (t, u) -> {/* NOP */};

    /**
     * Returns The NOP singleton.
     *
     * @param <T> the type of the object argument to the operation.
     * @param <E> The kind of thrown exception or error.
     * @return The NOP singleton.
     */
    static <T, E extends Throwable> FailableObjLongConsumer<T, E> nop() {
        return NOP;
    }

    /**
     * Accepts the consumer.
     *
     * @param object the object parameter for the consumable to accept.
     * @param value the long parameter for the consumable to accept.
     * @throws E Thrown when the consumer fails.
     */
    void accept(T object, long value) throws E;
}
