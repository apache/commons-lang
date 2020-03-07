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

import java.util.function.IntToLongFunction;

/**
 * Represents a function that accepts an int-valued argument and produces a
 * long-valued result.  This is the {@code int}-to-{@code long} primitive
 * specialization for {@link Functions.FailableFunction}.
 *
 * <p>This is a functional interface whose functional method is
 * {@link #applyAsLong(int)}.
 *
 * @param <T> the type of exception to be thrown
 *
 * @see IntToLongFunction
 * @see Functions.FailableFunction
 * @since 3.10
 */
@FunctionalInterface
public interface FailableIntToLongFunction<T extends Throwable> {
    /**
     * Applies this function to the given argument.
     *
     * @param value the function argument
     * @return the function result
     * @throws T if the function fails
     */
    long applyAsLong(int value) throws T;
}
