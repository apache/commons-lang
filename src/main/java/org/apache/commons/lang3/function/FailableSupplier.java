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
 * Represents a supplier of results.
 *
 * <p>There is no requirement that a new or distinct result be returned each
 * time the supplier is invoked.
 *
 * <p>This is a functional interface whose functional method is {@link #get()}.
 *
 * <p>An exception will be thrown if an error occurs.
 *
 * @param <R> the type of results produced by this supplier
 * @param <T> the type of exception to be thrown
 *
 * @see java.util.function.Supplier
 * @since 3.10
 */
@FunctionalInterface
public interface FailableSupplier<R, T extends Throwable> {
    /**
     * Supplies an object
     * @return the suppliers result
     * @throws T if the supplier fails
     */
    R get() throws T;
}
