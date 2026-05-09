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

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link FailableIntToLongFunction}.
 */
class FailableIntToLongFunctionTest extends AbstractLangTest {

    @Test
    void testApplyAsLong_returnsResult() throws IOException {
        final FailableIntToLongFunction<IOException> f = v -> (long) v * 1_000_000L;
        assertEquals(3_000_000L, f.applyAsLong(3));
    }

    @Test
    void testApplyAsLong_throwsException() {
        final IOException expected = new IOException("fail");
        final FailableIntToLongFunction<IOException> f = v -> {
            throw expected;
        };
        final IOException thrown = assertThrows(IOException.class, () -> f.applyAsLong(1));
        assertEquals(expected, thrown);
    }

    @Test
    void testNop_applyAsLongReturnsZero() throws Throwable {
        assertEquals(0L, FailableIntToLongFunction.nop().applyAsLong(1));
    }

    @Test
    void testNop_returnsNonNull() {
        assertNotNull(FailableIntToLongFunction.nop());
    }
}
