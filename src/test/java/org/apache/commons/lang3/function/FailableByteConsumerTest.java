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

import static org.apache.commons.lang3.LangAssertions.assertNullPointerException;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.IOException;
import java.util.concurrent.atomic.AtomicInteger;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link FailableByteConsumer}.
 */
class FailableByteConsumerTest extends AbstractLangTest {

    @Test
    void testAccept_invokesConsumer() throws IOException {
        final AtomicInteger ref = new AtomicInteger();
        final FailableByteConsumer<IOException> consumer = v -> ref.set(v);
        consumer.accept((byte) 42);
        assertEquals(42, ref.get());
    }

    @Test
    void testAccept_throwsException() {
        final IOException expected = new IOException("fail");
        final FailableByteConsumer<IOException> consumer = v -> {
            throw expected;
        };
        final IOException thrown = assertThrows(IOException.class, () -> consumer.accept((byte) 1));
        assertEquals(expected, thrown);
    }

    @Test
    void testAndThen_bothInvoked() throws IOException {
        final AtomicInteger ref1 = new AtomicInteger();
        final AtomicInteger ref2 = new AtomicInteger();
        final FailableByteConsumer<IOException> first = v -> ref1.set(v);
        final FailableByteConsumer<IOException> second = v -> ref2.set(v);
        first.andThen(second).accept((byte) 7);
        assertEquals(7, ref1.get());
        assertEquals(7, ref2.get());
    }

    @Test
    void testAndThen_firstThrows_secondNotInvoked() {
        final IOException expected = new IOException("first");
        final AtomicInteger ref = new AtomicInteger();
        final FailableByteConsumer<IOException> throwing = v -> {
            throw expected;
        };
        final FailableByteConsumer<IOException> second = v -> ref.set(v);
        final IOException thrown = assertThrows(IOException.class, () -> throwing.andThen(second).accept((byte) 1));
        assertEquals(expected, thrown);
        assertEquals(0, ref.get());
    }

    @Test
    void testAndThen_nullAfter_throwsNullPointerException() throws Throwable {
        final FailableByteConsumer<IOException> consumer = v -> {
        };
        assertNullPointerException(() -> consumer.andThen(null));
    }

    @Test
    void testNop_acceptDoesNothing() throws Throwable {
        FailableByteConsumer.nop().accept((byte) 1);
    }

    @Test
    void testNop_returnsNonNull() {
        assertNotNull(FailableByteConsumer.nop());
    }
}
