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

import java.util.function.BiConsumer;

/**
 * A functional interface like {@link BiConsumer} that declares a {@code Throwable}.
 *
 * @param <O1> Consumed type 1.
 * @param <O2> Consumed type 2.
 * @param <T> Thrown exception.
 * @since 3.11
 */
@FunctionalInterface
public interface FailableBiConsumer<O1, O2, T extends Throwable> {

    /**
     * Accepts the consumer.
     *
     * @param object1 the first parameter for the consumable to accept
     * @param object2 the second parameter for the consumable to accept
     * @throws T Thrown when the consumer fails.
     */
    void accept(O1 object1, O2 object2) throws T;
}
