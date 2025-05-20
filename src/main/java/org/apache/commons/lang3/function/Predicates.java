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

import java.util.function.Predicate;

/**
 * Factory for {@link Predicate}.
 *
 * @since 3.18.0
 */
public class Predicates {

    private static final Predicate<?> ALWAYS_TRUE = t -> true;
    private static final Predicate<?> ALWAYS_FALSE = t -> false;

    /**
     * Returns the Predicate singleton that always returns false.
     *
     * @param <T> the type of the input to the predicate.
     * @return the Predicate singleton.
     */
    @SuppressWarnings("unchecked")
    // method name cannot be "false".
    public static <T> Predicate<T> falsePredicate() {
        return (Predicate<T>) ALWAYS_FALSE;
    }

    /**
     * Returns the Predicate singleton that always returns true.
     *
     * @param <T> the type of the input to the predicate.
     * @return the Predicate singleton.
     */
    @SuppressWarnings("unchecked")
    // method name cannot be "true".
    public static <T> Predicate<T> truePredicate() {
        return (Predicate<T>) ALWAYS_TRUE;
    }

    /**
     * No instances needed.
     */
    private Predicates() {
        // empty
    }
}
