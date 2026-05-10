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

package org.apache.commons.lang3;

import java.io.Closeable;
import java.util.function.Consumer;

import org.apache.commons.lang3.function.Consumers;
import org.apache.commons.lang3.function.FailableConsumer;

/**
 * Static operations on {@link AutoCloseable}.
 * <p>
 * For {@link Closeable}-specific methods, see Apache Commons IO's
 * <a href="https://commons.apache.org/proper/commons-io/apidocs/org/apache/commons/io/IOUtils.html">IOUtils</a>.
 * </p>
 *
 * @since 3.21.0
 */
public class AutoCloseables {

    /**
     * Closes the given {@link AutoCloseable} as a null-safe operation.
     *
     * @param closeable The resource to close, may be null.
     * @throws Exception if an error occurs.
     */
    public static void close(final AutoCloseable closeable) throws Exception {
        if (closeable != null) {
            closeable.close();
        }
    }

    /**
     * Closes the given {@link AutoCloseable} as a null-safe operation.
     *
     * @param closeable The resource to close, may be null.
     * @param consumer  Consume the Exception thrown by {@link AutoCloseable#close()}.
     * @throws Exception As thrown by the consumer.
     */
    public static void close(final AutoCloseable closeable, final FailableConsumer<Exception, Exception> consumer) throws Exception {
        if (closeable != null) {
            try {
                closeable.close();
            } catch (final Exception e) {
                FailableConsumer.accept(consumer, e);
            }
        }
    }

    /**
     * Closes an {@link AutoCloseable}, never throwing an {@link Exception}.
     * <p>
     * Equivalent to {@link AutoCloseable#close()}, except any exceptions will be ignored.
     * </p>
     *
     * @param closeable the objects to close, may be null or already closed.
     * @see Throwable#addSuppressed(Throwable)
     */
    public static void closeQuietly(final AutoCloseable closeable) {
        closeQuietly(closeable, (Consumer<Exception>) null);
    }

    /**
     * Closes the given {@link AutoCloseable} as a null-safe operation while consuming Exception by the given {@code consumer}.
     *
     * @param closeable The resource to close, may be null.
     * @param consumer  Consumes the Exception thrown by {@link AutoCloseable#close()}.
     */
    public static void closeQuietly(final AutoCloseable closeable, final Consumer<Exception> consumer) {
        if (closeable != null) {
            try {
                closeable.close();
            } catch (final Exception e) {
                Consumers.accept(consumer, e);
            }
        }
    }

    /**
     * Closes an iterable of {@link AutoCloseable}, never throwing an {@link Exception}.
     * <p>
     * Equivalent calling {@link AutoCloseable#close()} on each element, except any exceptions will be ignored.
     * </p>
     *
     * @param closeables the objects to close, may be null or already closed.
     * @see #closeQuietly(AutoCloseable)
     */
    public static void closeQuietly(final Iterable<AutoCloseable> closeables) {
        if (closeables != null) {
            closeables.forEach(AutoCloseables::closeQuietly);
        }
    }

    /**
     * Closes a {@link Closeable} unconditionally and adds any exception thrown by the {@code close()} to the given Throwable.
     * <p>
     * For example:
     * </p>
     *
     * <pre>
     * AutoCloseable autoCloseable = ...;
     * try {
     *     // process autoCloseable.
     * } catch (Exception e) {
     *     // Handle exception.
     *     throw AutoCloseables.closeQuietlySuppress(autoCloseable, e);
     * }
     * </pre>
     * <p>
     * Also consider using a try-with-resources statement where appropriate.
     * </p>
     *
     * @param <T>       The Throwable type.
     * @param closeable The object to close, may be null or already closed.
     * @param throwable Add the exception throw by the closeable to the given Throwable.
     * @return The given Throwable.
     * @see Throwable#addSuppressed(Throwable)
     */
    public static <T extends Throwable> T closeQuietlySuppress(final Closeable closeable, final T throwable) {
        closeQuietly(closeable, throwable::addSuppressed);
        return throwable;
    }

    /**
     * No instances needed.
     */
    private AutoCloseables() {
        // empty
    }
}
