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
 * Represents a predicate (boolean-valued function) of two arguments.
 *
 * <p>This is a functional interface whose functional method is {@link #test(Object, Object)}.
 *
 * <p>An exception will be thrown if an error occurs.
 *
 * @param <O1> the type of the first argument to the operation
 * @param <O2> the type of the second argument to the operation
 * @param <T>  the type of exception to be thrown
 *
 * @see java.util.function.BiPredicate
 * @since 3.10
 */
@FunctionalInterface
public interface FailableBiPredicate<O1, O2, T extends Throwable> {
    /**
     * Test the predicate.
     *
     * @param object1 the first object to test the predicate on
     * @param object2 the second object to test the predicate on
     * @return the predicate's evaluation
     * @throws T if the predicate fails
     */
    boolean test(O1 object1, O2 object2) throws T;
}
