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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.Closeable;
import java.io.IOException;
import java.util.Arrays;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import org.junit.jupiter.api.Test;

/**
 * Tests {@link AutoCloseables}.
 */
class AutoCloseablesTest extends AbstractLangTest {

    /** An AutoCloseable that always throws on close(). */
    private static class ThrowingCloseable implements Closeable {

        private final IOException exception;

        ThrowingCloseable(final IOException exception) {
            this.exception = exception;
        }

        @Override
        public void close() throws IOException {
            throw exception;
        }
    }

    /** An AutoCloseable that tracks whether close() was called. */
    private static class TrackingCloseable implements Closeable {

        private final AtomicBoolean closed = new AtomicBoolean();

        @Override
        public void close() {
            closed.set(true);
        }

        boolean isClosed() {
            return closed.get();
        }
    }

    @Test
    void testClose_closesResource() throws Exception {
        final TrackingCloseable tc = new TrackingCloseable();
        AutoCloseables.close(tc);
        assertTrue(tc.isClosed(), "Expected resource to be closed");
    }

    @Test
    void testClose_null_doesNotThrow() throws Exception {
        AutoCloseables.close((AutoCloseable) null);
    }

    @Test
    void testClose_propagatesException() {
        final IOException cause = new IOException("boom");
        final ThrowingCloseable tc = new ThrowingCloseable(cause);
        final Exception thrown = assertThrows(Exception.class, () -> AutoCloseables.close(tc));
        assertEquals(cause, thrown);
    }

    @Test
    void testClose_withConsumer_closesResource() throws Exception {
        final TrackingCloseable tc = new TrackingCloseable();
        AutoCloseables.close(tc, null);
        assertTrue(tc.isClosed(), "Expected resource to be closed");
    }

    @Test
    void testClose_withConsumer_exceptionDeliveredToConsumer() throws Exception {
        final IOException cause = new IOException("boom");
        final ThrowingCloseable tc = new ThrowingCloseable(cause);
        final AtomicReference<Exception> received = new AtomicReference<>();
        AutoCloseables.close(tc, received::set);
        assertEquals(cause, received.get(), "Consumer should have received the exception");
    }

    @Test
    void testClose_withConsumer_null_doesNotThrow() throws Exception {
        AutoCloseables.close(null, null);
    }

    @Test
    void testCloseQuietly_closesResource() {
        final TrackingCloseable tc = new TrackingCloseable();
        AutoCloseables.closeQuietly(tc);
        assertTrue(tc.isClosed(), "Expected resource to be closed");
    }

    @Test
    void testCloseQuietly_iterable_closesAllResources() {
        final TrackingCloseable tc1 = new TrackingCloseable();
        final TrackingCloseable tc2 = new TrackingCloseable();
        AutoCloseables.closeQuietly(Arrays.asList(tc1, tc2));
        assertTrue(tc1.isClosed(), "Expected first resource to be closed");
        assertTrue(tc2.isClosed(), "Expected second resource to be closed");
    }

    @Test
    void testCloseQuietly_iterable_null_doesNotThrow() {
        AutoCloseables.closeQuietly((Iterable<AutoCloseable>) null);
    }

    @Test
    void testCloseQuietly_iterable_swallowsExceptions() {
        final ThrowingCloseable tc1 = new ThrowingCloseable(new IOException("first"));
        final ThrowingCloseable tc2 = new ThrowingCloseable(new IOException("second"));
        AutoCloseables.closeQuietly(Arrays.asList(tc1, tc2));
    }

    @Test
    void testCloseQuietly_null_doesNotThrow() {
        AutoCloseables.closeQuietly((AutoCloseable) null);
    }

    @Test
    void testCloseQuietly_swallowsException() {
        final ThrowingCloseable tc = new ThrowingCloseable(new IOException("ignored"));
        AutoCloseables.closeQuietly(tc);
    }

    @Test
    void testCloseQuietly_withConsumer_exceptionDeliveredToConsumer() {
        final IOException cause = new IOException("boom");
        final ThrowingCloseable tc = new ThrowingCloseable(cause);
        final AtomicReference<Exception> received = new AtomicReference<>();
        AutoCloseables.closeQuietly(tc, received::set);
        assertEquals(cause, received.get(), "Consumer should have received the exception");
    }

    @Test
    void testCloseQuietly_withConsumer_null_doesNotThrow() {
        AutoCloseables.closeQuietly(null, (java.util.function.Consumer<Exception>) null);
    }

    @Test
    void testCloseQuietly_withNullConsumer_swallowsException() {
        final ThrowingCloseable tc = new ThrowingCloseable(new IOException("ignored"));
        AutoCloseables.closeQuietly(tc, (java.util.function.Consumer<Exception>) null);
    }

    @Test
    void testCloseQuietlySuppress_addsSuppressedException() {
        final IOException closeException = new IOException("close failure");
        final ThrowingCloseable tc = new ThrowingCloseable(closeException);
        final RuntimeException primary = new RuntimeException("primary");
        final RuntimeException result = AutoCloseables.closeQuietlySuppress(tc, primary);
        assertEquals(primary, result);
        assertNotNull(result.getSuppressed());
        assertEquals(1, result.getSuppressed().length);
        assertEquals(closeException, result.getSuppressed()[0]);
    }

    @Test
    void testCloseQuietlySuppress_noException_returnsThrowable() throws Exception {
        final TrackingCloseable tc = new TrackingCloseable();
        final RuntimeException primary = new RuntimeException("primary");
        final RuntimeException result = AutoCloseables.closeQuietlySuppress(tc, primary);
        assertEquals(primary, result);
        assertTrue(tc.isClosed());
        assertEquals(0, result.getSuppressed().length, "No suppressed exceptions expected");
    }

    @Test
    void testCloseQuietlySuppress_null_returnsThrowable() {
        final RuntimeException primary = new RuntimeException("primary");
        final RuntimeException result = AutoCloseables.closeQuietlySuppress(null, primary);
        assertEquals(primary, result, "Should return the given throwable unchanged");
        assertEquals(0, result.getSuppressed().length, "No suppressed exceptions expected");
    }
}
