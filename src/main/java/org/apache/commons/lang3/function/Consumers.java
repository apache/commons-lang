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

import java.util.function.Consumer;
import java.util.function.Function;

/**
 * Provides {@link Consumer} instances.
 *
 * @since 3.13.0
 */
public class Consumers {

    /** NOP singleton. */
    @SuppressWarnings("rawtypes")
    private static final Consumer NOP = Function.identity()::apply;

    /**
     * Applies the given {@link Consumer} action to the object if the consumer is not {@code null}. Otherwise, does nothing.
     *
     * @param consumer the consumer to consume.
     * @param object   the object to be consumed.
     *
     * @param <T>      the type of the argument the consumer accepts.
     * @since 3.15.0
     */
    public static <T> void accept(final Consumer<T> consumer, final T object) {
        if (consumer != null) {
            consumer.accept(object);
        }
    }

    /**
     * Gets the NOP Consumer singleton.
     *
     * @param <T> type type to consume.
     * @return the NOP Consumer singleton.
     */
    @SuppressWarnings("unchecked")
    public static <T> Consumer<T> nop() {
        return NOP;
    }

    private Consumers() {
        // No instances.
    }
}
