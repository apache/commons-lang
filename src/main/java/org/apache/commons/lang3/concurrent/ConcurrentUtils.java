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
package org.apache.commons.lang3.concurrent;

import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import org.apache.commons.lang3.Validate;
import org.apache.commons.lang3.exception.ExceptionUtils;

/**
 * A utility class providing functionality related to the {@code
 * java.util.concurrent} package.
 *
 * @since 3.0
 */
public class ConcurrentUtils {

    /**
     * A specialized {@link Future} implementation which wraps a constant value.
     * @param <T> the type of the value wrapped by this class
     */
    static final class ConstantFuture<T> implements Future<T> {
        /** The constant value. */
        private final T value;

        /**
         * Creates a new instance of {@link ConstantFuture} and initializes it
         * with the constant value.
         *
         * @param value the value (may be <strong>null</strong>)
         */
        ConstantFuture(final T value) {
            this.value = value;
        }

        /**
         * {@inheritDoc} The cancel operation is not supported. This
         * implementation always returns <strong>false</strong>.
         */
        @Override
        public boolean cancel(final boolean mayInterruptIfRunning) {
            return false;
        }

        /**
         * {@inheritDoc} This implementation just returns the constant value.
         */
        @Override
        public T get() {
            return value;
        }

        /**
         * {@inheritDoc} This implementation just returns the constant value; it
         * does not block, therefore the timeout has no meaning.
         */
        @Override
        public T get(final long timeout, final TimeUnit unit) {
            return value;
        }

        /**
         * {@inheritDoc} This implementation always returns <strong>false</strong>; there
         * is no background process which could be cancelled.
         */
        @Override
        public boolean isCancelled() {
            return false;
        }

        /**
         * {@inheritDoc} This implementation always returns <strong>true</strong> because
         * the constant object managed by this {@link Future} implementation is
         * always available.
         */
        @Override
        public boolean isDone() {
            return true;
        }
    }

    /**
     * Tests whether the specified {@link Throwable} is a checked exception. If
     * not, an exception is thrown.
     *
     * @param ex the {@link Throwable} to check
     * @return a flag whether the passed in exception is a checked exception
     * @throws IllegalArgumentException if the {@link Throwable} is not a
     * checked exception
     */
    static Throwable checkedException(final Throwable ex) {
        Validate.isTrue(ExceptionUtils.isChecked(ex), "Not a checked exception: %s", ex);
        return ex;
    }

    /**
     * Gets an implementation of {@link Future} that is immediately done
     * and returns the specified constant value.
     *
     * <p>
     * This can be useful to return a simple constant immediately from the
     * concurrent processing, perhaps as part of avoiding nulls.
     * A constant future can also be useful in testing.
     * </p>
     *
     * @param <T> the type of the value used by this {@link Future} object
     * @param value  the constant value to return, may be null
     * @return an instance of Future that will return the value, never null
     */
    public static <T> Future<T> constantFuture(final T value) {
        return new ConstantFuture<>(value);
    }

    /**
     * Checks if a concurrent map contains a key and creates a corresponding
     * value if not. This method first checks the presence of the key in the
     * given map. If it is already contained, its value is returned. Otherwise
     * the {@code get()} method of the passed in {@link ConcurrentInitializer}
     * is called. With the resulting object
     * {@link #putIfAbsent(ConcurrentMap, Object, Object)} is called. This
     * handles the case that in the meantime another thread has added the key to
     * the map. Both the map and the initializer can be <strong>null</strong>; in this
     * case this method simply returns <strong>null</strong>.
     *
     * @param <K> the type of the keys of the map
     * @param <V> the type of the values of the map
     * @param map the map to be modified
     * @param key the key of the value to be added
     * @param init the {@link ConcurrentInitializer} for creating the value
     * @return the value stored in the map after this operation; this may or may
     * not be the object created by the {@link ConcurrentInitializer}
     * @throws ConcurrentException if the initializer throws an exception
     */
    public static <K, V> V createIfAbsent(final ConcurrentMap<K, V> map, final K key,
            final ConcurrentInitializer<V> init) throws ConcurrentException {
        if (map == null || init == null) {
            return null;
        }

        final V value = map.get(key);
        if (value == null) {
            return putIfAbsent(map, key, init.get());
        }
        return value;
    }

    /**
     * Checks if a concurrent map contains a key and creates a corresponding
     * value if not, suppressing checked exceptions. This method calls
     * {@code createIfAbsent()}. If a {@link ConcurrentException} is thrown, it
     * is caught and re-thrown as a {@link ConcurrentRuntimeException}.
     *
     * @param <K> the type of the keys of the map
     * @param <V> the type of the values of the map
     * @param map the map to be modified
     * @param key the key of the value to be added
     * @param init the {@link ConcurrentInitializer} for creating the value
     * @return the value stored in the map after this operation; this may or may
     * not be the object created by the {@link ConcurrentInitializer}
     * @throws ConcurrentRuntimeException if the initializer throws an exception
     */
    public static <K, V> V createIfAbsentUnchecked(final ConcurrentMap<K, V> map,
            final K key, final ConcurrentInitializer<V> init) {
        try {
            return createIfAbsent(map, key, init);
        } catch (final ConcurrentException cex) {
            throw new ConcurrentRuntimeException(cex.getCause());
        }
    }

    /**
     * Inspects the cause of the specified {@link ExecutionException} and
     * creates a {@link ConcurrentException} with the checked cause if
     * necessary. This method performs the following checks on the cause of the
     * passed in exception:
     * <ul>
     * <li>If the passed in exception is <strong>null</strong> or the cause is
     * <strong>null</strong>, this method returns <strong>null</strong>.</li>
     * <li>If the cause is a runtime exception, it is directly thrown.</li>
     * <li>If the cause is an error, it is directly thrown, too.</li>
     * <li>In any other case the cause is a checked exception. The method then
     * creates a {@link ConcurrentException}, initializes it with the cause, and
     * returns it.</li>
     * </ul>
     *
     * @param ex the exception to be processed
     * @return a {@link ConcurrentException} with the checked cause
     */
    public static ConcurrentException extractCause(final ExecutionException ex) {
        if (ex == null || ex.getCause() == null) {
            return null;
        }
        ExceptionUtils.throwUnchecked(ex.getCause());
        return new ConcurrentException(ex.getMessage(), ex.getCause());
    }

    /**
     * Inspects the cause of the specified {@link ExecutionException} and
     * creates a {@link ConcurrentRuntimeException} with the checked cause if
     * necessary. This method works exactly like
     * {@link #extractCause(ExecutionException)}. The only difference is that
     * the cause of the specified {@link ExecutionException} is extracted as a
     * runtime exception. This is an alternative for client code that does not
     * want to deal with checked exceptions.
     *
     * @param ex the exception to be processed
     * @return a {@link ConcurrentRuntimeException} with the checked cause
     */
    public static ConcurrentRuntimeException extractCauseUnchecked(final ExecutionException ex) {
        if (ex == null || ex.getCause() == null) {
            return null;
        }
        ExceptionUtils.throwUnchecked(ex.getCause());
        return new ConcurrentRuntimeException(ex.getMessage(), ex.getCause());
    }

    /**
     * Handles the specified {@link ExecutionException}. This method calls
     * {@link #extractCause(ExecutionException)} for obtaining the cause of the
     * exception - which might already cause an unchecked exception or an error
     * being thrown. If the cause is a checked exception however, it is wrapped
     * in a {@link ConcurrentException}, which is thrown. If the passed in
     * exception is <strong>null</strong> or has no cause, the method simply returns
     * without throwing an exception.
     *
     * @param ex the exception to be handled
     * @throws ConcurrentException if the cause of the {@code
     * ExecutionException} is a checked exception
     */
    public static void handleCause(final ExecutionException ex) throws ConcurrentException {
        final ConcurrentException cause = extractCause(ex);
        if (cause != null) {
            throw cause;
        }
    }

    /**
     * Handles the specified {@link ExecutionException} and transforms it into a
     * runtime exception. This method works exactly like
     * {@link #handleCause(ExecutionException)}, but instead of a
     * {@link ConcurrentException} it throws a
     * {@link ConcurrentRuntimeException}. This is an alternative for client
     * code that does not want to deal with checked exceptions.
     *
     * @param ex the exception to be handled
     * @throws ConcurrentRuntimeException if the cause of the {@code
     * ExecutionException} is a checked exception; this exception is then
     * wrapped in the thrown runtime exception
     */
    public static void handleCauseUnchecked(final ExecutionException ex) {
        final ConcurrentRuntimeException cause = extractCauseUnchecked(ex);
        if (cause != null) {
            throw cause;
        }
    }

    /**
     * Invokes the specified {@link ConcurrentInitializer} and returns the
     * object produced by the initializer. This method just invokes the {@code
     * get()} method of the given {@link ConcurrentInitializer}. It is
     * <strong>null</strong>-safe: if the argument is <strong>null</strong>, result is also
     * <strong>null</strong>.
     *
     * @param <T> the type of the object produced by the initializer
     * @param initializer the {@link ConcurrentInitializer} to be invoked
     * @return the object managed by the {@link ConcurrentInitializer}
     * @throws ConcurrentException if the {@link ConcurrentInitializer} throws
     * an exception
     */
    public static <T> T initialize(final ConcurrentInitializer<T> initializer)
            throws ConcurrentException {
        return initializer != null ? initializer.get() : null;
    }

    /**
     * Invokes the specified {@link ConcurrentInitializer} and transforms
     * occurring exceptions to runtime exceptions. This method works like
     * {@link #initialize(ConcurrentInitializer)}, but if the {@code
     * ConcurrentInitializer} throws a {@link ConcurrentException}, it is
     * caught, and the cause is wrapped in a {@link ConcurrentRuntimeException}.
     * So client code does not have to deal with checked exceptions.
     *
     * @param <T> the type of the object produced by the initializer
     * @param initializer the {@link ConcurrentInitializer} to be invoked
     * @return the object managed by the {@link ConcurrentInitializer}
     * @throws ConcurrentRuntimeException if the initializer throws an exception
     */
    public static <T> T initializeUnchecked(final ConcurrentInitializer<T> initializer) {
        try {
            return initialize(initializer);
        } catch (final ConcurrentException cex) {
            throw new ConcurrentRuntimeException(cex.getCause());
        }
    }

    /**
     * Puts a value in the specified {@link ConcurrentMap} if the key is not yet
     * present. This method works similar to the {@code putIfAbsent()} method of
     * the {@link ConcurrentMap} interface, but the value returned is different.
     * Basically, this method is equivalent to the following code fragment:
     *
     * <pre>
     * if (!map.containsKey(key)) {
     *     map.put(key, value);
     *     return value;
     * } else {
     *     return map.get(key);
     * }
     * </pre>
     *
     * <p>
     * except that the action is performed atomically. So this method always
     * returns the value which is stored in the map.
     * </p>
     * <p>
     * This method is <strong>null</strong>-safe: It accepts a <strong>null</strong> map as input
     * without throwing an exception. In this case the return value is
     * <strong>null</strong>, too.
     * </p>
     *
     * @param <K> the type of the keys of the map
     * @param <V> the type of the values of the map
     * @param map the map to be modified
     * @param key the key of the value to be added
     * @param value the value to be added
     * @return the value stored in the map after this operation
     */
    public static <K, V> V putIfAbsent(final ConcurrentMap<K, V> map, final K key, final V value) {
        if (map == null) {
            return null;
        }
        final V result = map.putIfAbsent(key, value);
        return result != null ? result : value;
    }

    /**
     * Private constructor so that no instances can be created. This class
     * contains only static utility methods.
     */
    private ConcurrentUtils() {
    }

}
