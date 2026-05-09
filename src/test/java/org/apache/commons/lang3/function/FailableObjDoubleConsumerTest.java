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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.IOException;
import java.util.concurrent.atomic.AtomicReference;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link FailableObjDoubleConsumer}.
 */
class FailableObjDoubleConsumerTest extends AbstractLangTest {

    @Test
    void testAccept_invokesConsumer() throws IOException {
        final AtomicReference<String> objRef = new AtomicReference<>();
        final AtomicReference<Double> valRef = new AtomicReference<>();
        final FailableObjDoubleConsumer<String, IOException> consumer = (o, v) -> {
            objRef.set(o);
            valRef.set(v);
        };
        consumer.accept("hello", 3.14);
        assertEquals("hello", objRef.get());
        assertEquals(3.14, valRef.get());
    }

    @Test
    void testAccept_throwsException() {
        final IOException expected = new IOException("fail");
        final FailableObjDoubleConsumer<String, IOException> consumer = (o, v) -> {
            throw expected;
        };
        final IOException thrown = assertThrows(IOException.class, () -> consumer.accept("x", 1.0));
        assertEquals(expected, thrown);
    }

    @Test
    void testNop_acceptDoesNothing() throws Throwable {
        FailableObjDoubleConsumer.nop().accept("x", 1.0);
    }

    @Test
    void testNop_returnsNonNull() {
        assertNotNull(FailableObjDoubleConsumer.nop());
    }
}
