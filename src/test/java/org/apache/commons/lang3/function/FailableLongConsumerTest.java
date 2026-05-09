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
import java.util.concurrent.atomic.AtomicLong;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link FailableLongConsumer}.
 */
class FailableLongConsumerTest extends AbstractLangTest {

    @Test
    void testAccept_invokesConsumer() throws IOException {
        final AtomicLong ref = new AtomicLong();
        final FailableLongConsumer<IOException> consumer = ref::set;
        consumer.accept(100L);
        assertEquals(100L, ref.get());
    }

    @Test
    void testAccept_throwsException() {
        final IOException expected = new IOException("fail");
        final FailableLongConsumer<IOException> consumer = v -> {
            throw expected;
        };
        final IOException thrown = assertThrows(IOException.class, () -> consumer.accept(1L));
        assertEquals(expected, thrown);
    }

    @Test
    void testAndThen_bothInvoked() throws IOException {
        final AtomicLong ref1 = new AtomicLong();
        final AtomicLong ref2 = new AtomicLong();
        final FailableLongConsumer<IOException> first = ref1::set;
        final FailableLongConsumer<IOException> second = ref2::set;
        first.andThen(second).accept(99L);
        assertEquals(99L, ref1.get());
        assertEquals(99L, ref2.get());
    }

    @Test
    void testAndThen_firstThrows_secondNotInvoked() {
        final IOException expected = new IOException("first");
        final AtomicLong ref = new AtomicLong(-1L);
        final FailableLongConsumer<IOException> throwing = v -> {
            throw expected;
        };
        final FailableLongConsumer<IOException> second = ref::set;
        final IOException thrown = assertThrows(IOException.class, () -> throwing.andThen(second).accept(1L));
        assertEquals(expected, thrown);
        assertEquals(-1L, ref.get());
    }

    @Test
    void testAndThen_nullAfter_throwsNullPointerException() throws Throwable {
        final FailableLongConsumer<IOException> consumer = v -> {
        };
        assertNullPointerException(() -> consumer.andThen(null));
    }

    @Test
    void testNop_acceptDoesNothing() throws Throwable {
        FailableLongConsumer.nop().accept(1L);
    }

    @Test
    void testNop_returnsNonNull() {
        assertNotNull(FailableLongConsumer.nop());
    }
}
