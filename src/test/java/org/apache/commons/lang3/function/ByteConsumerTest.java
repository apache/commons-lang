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

import java.util.concurrent.atomic.AtomicInteger;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link ByteConsumer}.
 */
class ByteConsumerTest extends AbstractLangTest {

    private static final byte B0 = (byte) 0;
    private static final byte B1 = (byte) 1;

    private ByteConsumer accept(final ByteConsumer consumer, final byte expected) {
        consumer.accept(expected);
        return consumer;
    }

    @Test
    void testAccept() {
        final AtomicInteger ref = new AtomicInteger();
        accept(v -> ref.lazySet(v), B1);
        assertEquals(1, ref.get());
        accept(v -> ref.lazySet(v), B0);
        assertEquals(0, ref.get());
    }

    @Test
    void testAndThen() throws Throwable {
        final ByteConsumer nop = ByteConsumer.nop();
        nop.andThen(nop);
        // Documented in Javadoc edge-case.
        assertNullPointerException(() -> nop.andThen(null));

        final AtomicInteger ref1 = new AtomicInteger();
        final AtomicInteger ref2 = new AtomicInteger();

        final ByteConsumer bc = ref1::lazySet;
        final ByteConsumer composite = bc.andThen(ref2::lazySet);

        composite.accept(B1);
        assertEquals(1, ref1.get());
        assertEquals(1, ref2.get());

        composite.accept(B0);
        assertEquals(0, ref1.get());
        assertEquals(0, ref2.get());

        // Check order
        final ByteConsumer bad = value -> {
            throw new IllegalStateException();
        };
        final ByteConsumer badComposite = bad.andThen(ref2::lazySet);

        Assertions.assertThrows(IllegalStateException.class, () -> badComposite.accept(B1));
        assertEquals(0, ref2.get(), "Second consumer should not be invoked");
    }

}
