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

/**
 * Represents a function that accepts two arguments and produces a result.
 *
 * <p>This is a functional interface whose functional method is {@link #apply(Object, Object)}.
 *
 * <p>An exception will be thrown if an error occurs.
 *
 * @param <I1> the type of the first argument to the operation
 * @param <I2> the type of the second argument to the operation
 * @param <O>  the type of the returned value
 * @param <T>  the type of exception to be thrown
 *
 * @see java.util.function.BiFunction
 * @since 3.10
 */
@FunctionalInterface
public interface FailableBiFunction<I1, I2, O, T extends Throwable> {
    /**
     * Apply the function.
     *
     * @param input1 the first input for the function
     * @param input2 the second input for the function
     * @return the result of the function
     * @throws T if the function fails
     */
    O apply(I1 input1, I2 input2) throws T;
}
