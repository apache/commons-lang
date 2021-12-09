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

import java.util.Collection;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang3.exception.UncheckedInterruptedException;

/**
 * An {@link Future} that throws unchecked instead checked exceptions.
 *
 * @param <V> The result type returned by this Future's {@link #get()} and {@link #get(long, TimeUnit)} methods.
 * @see Future
 * @see Exception
 * @since 3.13.0
 */
public interface UncheckedFuture<V> extends Future<V> {

    /**
     * Maps the given instances as unchecked.
     *
     * @param <T> The result type returned by the Futures' {@link #get()} and {@link #get(long, TimeUnit)} methods.
     *
     * @param futures The Futures to uncheck.
     * @return a new stream.
     */
    static <T> Stream<UncheckedFuture<T>> map(final Collection<Future<T>> futures) {
        return futures.stream().map(UncheckedFuture::on);
    }

    /**
     * Maps the given instances as unchecked.
     *
     * @param <T> The result type returned by the Futures' {@link #get()} and {@link #get(long, TimeUnit)} methods.
     *
     * @param futures The Futures to uncheck.
     * @return a new collection.
     */
    static <T> Collection<UncheckedFuture<T>> on(final Collection<Future<T>> futures) {
        return map(futures).collect(Collectors.toList());
    }

    /**
     * Creates a new instance on the given Future.
     *
     * @param <T> The result type returned by this Future's {@link #get()} and {@link #get(long, TimeUnit)} methods.
     *
     * @param future The Future to uncheck.
     * @return a new instance.
     */
    static <T> UncheckedFuture<T> on(final Future<T> future) {
        return new UncheckedFutureImpl<>(future);
    }

    /**
     * Gets per {@link Future#get()} but rethrows checked exceptions as unchecked.
     * <p>
     * The default mapping from checked to unchecked is:
     * </p>
     * <ul>
     * <li>{@link InterruptedException} \u2192 {@link UncheckedInterruptedException}</li>
     * <li>{@link ExecutionException} \u2192 {@link UncheckedExecutionException}</li>
     * </ul>
     */
    @Override
    V get();

    /**
     * Gets per {@link Future#get(long, TimeUnit)} but rethrows checked exceptions as unchecked.
     * <p>
     * The default mapping from checked to unchecked is:
     * </p>
     * <ul>
     * <li>{@link InterruptedException} \u2192 {@link UncheckedInterruptedException}</li>
     * <li>{@link ExecutionException} \u2192 {@link UncheckedExecutionException}</li>
     * <li>{@link TimeoutException} \u2192 {@link UncheckedTimeoutException}</li>
     * </ul>
     */
    @Override
    V get(long timeout, TimeUnit unit);

}
