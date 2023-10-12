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

/**
 * Abstracts and defines operations for ConcurrentInitializer implementations.
 *
 * @param <T> the type of the object managed by this initializer class.
 * @param <E> The exception type thrown by {@link #initialize()}.
 * @since 3.14.0
 */
public abstract class AbstractConcurrentInitializer<T, E extends Exception> implements ConcurrentInitializer<T> {

    /**
     * Creates and initializes the object managed by this {@code
     * ConcurrentInitializer}. This method is called by {@link #get()} when the object is accessed for the first time. An implementation can focus on the
     * creation of the object. No synchronization is needed, as this is already handled by {@code get()}.
     *
     * @return the managed data object
     * @throws E if an error occurs during object creation
     */
    protected abstract T initialize() throws E;

}
