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

import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.apache.commons.lang3.exception.UncheckedInterruptedException;

/**
 * An {@link Future} implementation that throws unchecked instead of checked exceptions.
 *
 * @param <V> The result type returned by this UncheckedFuture's {@link #get()} and {@link #get(long, TimeUnit)} methods.
 * @see Future
 * @since 3.13.0
 */
class UncheckedFutureImpl<V> extends AbstractFutureProxy<V> implements UncheckedFuture<V> {

    UncheckedFutureImpl(final Future<V> future) {
        super(future);
    }

    @Override
    public V get() {
        try {
            return super.get();
        } catch (final InterruptedException e) {
            throw new UncheckedInterruptedException(e);
        } catch (final ExecutionException e) {
            throw new UncheckedExecutionException(e);
        }
    }

    @Override
    public V get(final long timeout, final TimeUnit unit) {
        try {
            return super.get(timeout, unit);
        } catch (final InterruptedException e) {
            throw new UncheckedInterruptedException(e);
        } catch (final ExecutionException e) {
            throw new UncheckedExecutionException(e);
        } catch (final TimeoutException e) {
            throw new UncheckedTimeoutException(e);
        }
    }

}
