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
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.IOException;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link FailableLongFunction}.
 */
class FailableLongFunctionTest extends AbstractLangTest {

    @Test
    void testApply_returnsResult() throws IOException {
        final FailableLongFunction<String, IOException> f = v -> "v=" + v;
        assertEquals("v=5", f.apply(5L));
    }

    @Test
    void testApply_throwsException() {
        final IOException expected = new IOException("fail");
        final FailableLongFunction<String, IOException> f = v -> {
            throw expected;
        };
        final IOException thrown = assertThrows(IOException.class, () -> f.apply(1L));
        assertEquals(expected, thrown);
    }

    @Test
    void testNop_applyReturnsNull() throws Throwable {
        assertNull(FailableLongFunction.nop().apply(1L));
    }

    @Test
    void testNop_returnsNonNull() {
        assertNotNull(FailableLongFunction.nop());
    }
}
