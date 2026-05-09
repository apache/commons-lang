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
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.IOException;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link FailableBiFunction}.
 */
class FailableBiFunctionTest extends AbstractLangTest {

    @Test
    void testAndThen_composesCorrectly() throws IOException {
        final FailableBiFunction<Integer, Integer, Integer, IOException> sum = (a, b) -> a + b;
        final FailableFunction<Integer, String, IOException> toStr = i -> "result:" + i;
        assertEquals("result:3", sum.andThen(toStr).apply(1, 2));
    }

    @Test
    void testAndThen_firstThrows_secondNotInvoked() {
        final IOException expected = new IOException("first");
        final FailableBiFunction<String, String, String, IOException> f = (a, b) -> {
            throw expected;
        };
        final FailableFunction<String, String, IOException> after = s -> s + "!";
        final IOException thrown = assertThrows(IOException.class, () -> f.andThen(after).apply("a", "b"));
        assertEquals(expected, thrown);
    }

    @Test
    void testAndThen_nullAfter_throwsNullPointerException() throws Throwable {
        final FailableBiFunction<String, String, String, IOException> f = (a, b) -> a + b;
        assertNullPointerException(() -> f.andThen(null));
    }

    @Test
    void testApply_returnsResult() throws IOException {
        final FailableBiFunction<String, String, String, IOException> f = (a, b) -> a + b;
        assertEquals("ab", f.apply("a", "b"));
    }

    @Test
    void testApply_throwsException() {
        final IOException expected = new IOException("fail");
        final FailableBiFunction<String, String, String, IOException> f = (a, b) -> {
            throw expected;
        };
        final IOException thrown = assertThrows(IOException.class, () -> f.apply("a", "b"));
        assertEquals(expected, thrown);
    }

    @Test
    void testNop_applyReturnsNull() throws Throwable {
        assertNull(FailableBiFunction.nop().apply("a", "b"));
    }

    @Test
    void testNop_returnsNonNull() {
        assertNotNull(FailableBiFunction.nop());
    }
}
