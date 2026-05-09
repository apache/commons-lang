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
 * Tests {@link FailableToIntBiFunction}.
 */
class FailableToIntBiFunctionTest extends AbstractLangTest {

    @Test
    void testApplyAsInt_returnsResult() throws IOException {
        final FailableToIntBiFunction<String, String, IOException> f = (a, b) -> a.length() + b.length();
        assertEquals(5, f.applyAsInt("ab", "abc"));
    }

    @Test
    void testApplyAsInt_throwsException() {
        final IOException expected = new IOException("fail");
        final FailableToIntBiFunction<String, String, IOException> f = (a, b) -> {
            throw expected;
        };
        final IOException thrown = assertThrows(IOException.class, () -> f.applyAsInt("a", "b"));
        assertEquals(expected, thrown);
    }

    @Test
    void testNop_applyAsIntReturnsZero() throws Throwable {
        assertEquals(0, FailableToIntBiFunction.nop().applyAsInt("a", "b"));
    }

    @Test
    void testNop_returnsNonNull() {
        assertNotNull(FailableToIntBiFunction.nop());
    }
}
