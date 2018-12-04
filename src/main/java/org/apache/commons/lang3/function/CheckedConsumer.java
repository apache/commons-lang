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

/**
 * Represents a function which accept a single argument and returns no result or throws a checked exception
 * @param <T> the type of an argument
 * @param <E> the type of exception thrown by function
 * @since 3.9
 */
@FunctionalInterface
public interface CheckedConsumer<T, E extends Exception> {

    /**
     *
     * @param argument the input argument
     * @throws E - the type of exception thrown by this function
     * @since 3.9
     */
    void accept(T argument) throws E;

    /**
     *
     * @param checkedConsumer an instance of this interface which will be mapped to {@link java.util.function.Consumer}
     * @param <T> the type of argument of a consumer
     * @param <E> the type of checked exception thrown by CheckedConsumer instance
     * @return an instance of Consumer which can throw a wrapped checked exception
     * @since 3.9
     */
    static <T, E extends Exception> Consumer<T> unchecked(CheckedConsumer<T, E> checkedConsumer) {
        return arg -> {
          try {
              checkedConsumer.accept(arg);
          } catch (Exception e) {
              throw new RuntimeException(e);
          }
        };
    }

}
