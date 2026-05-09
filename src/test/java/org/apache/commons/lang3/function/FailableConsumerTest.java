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
import java.util.concurrent.atomic.AtomicReference;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link FailableConsumer}.
 */
class FailableConsumerTest extends AbstractLangTest {

    @Test
    void testAccept_invokesConsumer() throws IOException {
        final AtomicReference<String> ref = new AtomicReference<>();
        final FailableConsumer<String, IOException> consumer = ref::set;
        consumer.accept("world");
        assertEquals("world", ref.get());
    }

    @Test
    void testAccept_throwsCheckedExceptionOnFailure() {
        final IOException expected = new IOException("fail");
        final FailableConsumer<String, IOException> consumer = s -> {
            throw expected;
        };
        final IOException thrown = assertThrows(IOException.class, () -> consumer.accept("x"));
        assertEquals(expected, thrown);
    }

    @Test
    void testAndThen_bothInvoked() throws IOException {
        final AtomicReference<String> ref1 = new AtomicReference<>();
        final AtomicReference<String> ref2 = new AtomicReference<>();
        final FailableConsumer<String, IOException> first = ref1::set;
        final FailableConsumer<String, IOException> second = ref2::set;
        first.andThen(second).accept("value");
        assertEquals("value", ref1.get());
        assertEquals("value", ref2.get());
    }

    @Test
    void testAndThen_firstThrows_secondNotInvoked() {
        final IOException expected = new IOException("first");
        final AtomicReference<String> ref = new AtomicReference<>();
        final FailableConsumer<String, IOException> throwing = s -> {
            throw expected;
        };
        final FailableConsumer<String, IOException> second = ref::set;
        final IOException thrown = assertThrows(IOException.class, () -> throwing.andThen(second).accept("x"));
        assertEquals(expected, thrown);
        assertEquals(null, ref.get(), "Second consumer should not have been invoked");
    }

    @Test
    void testAndThen_nullAfter_throwsNullPointerException() throws Throwable {
        final FailableConsumer<String, IOException> consumer = s -> {
        };
        assertNullPointerException(() -> consumer.andThen(null));
    }

    @Test
    void testAndThen_secondThrows_propagatesException() {
        final IOException expected = new IOException("second");
        final AtomicReference<String> ref = new AtomicReference<>();
        final FailableConsumer<String, IOException> first = ref::set;
        final FailableConsumer<String, IOException> throwing = s -> {
            throw expected;
        };
        final IOException thrown = assertThrows(IOException.class, () -> first.andThen(throwing).accept("v"));
        assertEquals(expected, thrown);
        assertEquals("v", ref.get(), "First consumer should have been invoked");
    }

    @Test
    void testNop_acceptDoesNothing() throws Throwable {
        FailableConsumer.nop().accept("anything");
    }

    @Test
    void testNop_returnsNonNull() {
        assertNotNull(FailableConsumer.nop());
    }

    @Test
    void testStaticAccept_consumerThrows_propagatesException() {
        final IOException expected = new IOException("boom");
        final FailableConsumer<String, IOException> throwing = s -> {
            throw expected;
        };
        final IOException thrown = assertThrows(IOException.class, () -> FailableConsumer.accept(throwing, "x"));
        assertEquals(expected, thrown);
    }

    @Test
    void testStaticAccept_nonNullConsumer_invokesConsumer() throws Exception {
        final AtomicReference<String> ref = new AtomicReference<>();
        FailableConsumer.accept(ref::set, "hello");
        assertEquals("hello", ref.get());
    }

    @Test
    void testStaticAccept_nullConsumer_doesNothing() throws Exception {
        FailableConsumer.accept(null, "value");
    }
}
