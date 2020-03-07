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

import org.apache.commons.lang3.Functions;

import java.util.function.LongConsumer;

/**
 * Represents an operation that accepts a single input argument and returns no
 * result. This is the {@code long}-consuming primitive specialization for
 * {@link Functions.FailableConsumer}.
 *
 * <p>This is a functional interface whose functional method is {@link #accept(long)}.
 *
 * <p>An exception will be thrown if an error occurs.
 *
 * @param <T> the type of exception to be thrown
 *
 * @see LongConsumer
 * @see Functions.FailableConsumer
 * @since 3.10
 */
public interface FailableLongConsumer<T extends Throwable> {
    /**
     * Accepts the consumer.
     *
     * @param value the parameter for the consumable to accept
     * @throws T if the consumer fails
     */
    void accept(long value) throws T;
}
