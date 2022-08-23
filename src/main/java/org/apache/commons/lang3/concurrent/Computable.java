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
package org.apache.commons.lang3.concurrent;

import org.apache.commons.lang3.function.FailableFunction;

/**
 * Definition of an interface for a wrapper around a calculation that takes a single parameter and returns a result.
 *
 * <p>This interface allows for wrapping a calculation into a class so that it maybe passed around an application.</p>
 *
 * <p>See also {@code FailableFunction<I, O, InterruptedException>}.</p>
 *
 * @param <I> the type of the input to the calculation
 * @param <O> the type of the output of the calculation
 * @see FailableFunction
 * @since 3.6
 */
public interface Computable<I, O> {

    /**
     * This method carries out the given operation with the provided argument.
     *
     * @param arg
     *            the argument for the calculation
     * @return the result of the calculation
     * @throws InterruptedException
     *             thrown if the calculation is interrupted
     */
    O compute(I arg) throws InterruptedException;
}
