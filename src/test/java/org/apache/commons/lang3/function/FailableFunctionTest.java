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
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.IOException;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link FailableFunction}.
 */
class FailableFunctionTest extends AbstractLangTest {

    @Test
    void testAndThen_composesCorrectly() throws IOException {
        final FailableFunction<String, Integer, IOException> length = String::length;
        final FailableFunction<Integer, String, IOException> toStr = i -> "len=" + i;
        assertEquals("len=5", length.andThen(toStr).apply("hello"));
    }

    @Test
    void testAndThen_nullAfter_throwsNullPointerException() throws Throwable {
        final FailableFunction<String, String, IOException> f = s -> s;
        assertNullPointerException(() -> f.andThen(null));
    }

    @Test
    void testApply_returnsResult() throws IOException {
        final FailableFunction<String, Integer, IOException> f = String::length;
        assertEquals(5, f.apply("hello"));
    }

    @Test
    void testApply_throwsException() {
        final IOException expected = new IOException("fail");
        final FailableFunction<String, String, IOException> f = s -> {
            throw expected;
        };
        final IOException thrown = assertThrows(IOException.class, () -> f.apply("x"));
        assertEquals(expected, thrown);
    }

    @Test
    void testCompose_composesCorrectly() throws IOException {
        final FailableFunction<String, String, IOException> upper = String::toUpperCase;
        final FailableFunction<String, String, IOException> trim = String::trim;
        assertEquals("HELLO", upper.compose(trim).apply("  hello  "));
    }

    @Test
    void testCompose_nullBefore_throwsNullPointerException() throws Throwable {
        final FailableFunction<String, String, IOException> f = s -> s;
        assertNullPointerException(() -> f.compose(null));
    }

    @Test
    void testFunction_returnsArgument() throws IOException {
        final FailableFunction<String, String, IOException> f = String::toUpperCase;
        final FailableFunction<String, String, IOException> wrapped = FailableFunction.function(f);
        assertEquals("HELLO", wrapped.apply("hello"));
    }

    @Test
    void testIdentity_returnsInput() throws IOException {
        final String input = "hello";
        assertSame(input, FailableFunction.<String, IOException>identity().apply(input));
    }

    @Test
    void testNop_applyReturnsNull() throws Throwable {
        assertNull(FailableFunction.nop().apply("x"));
    }

    @Test
    void testNop_returnsNonNull() {
        assertNotNull(FailableFunction.nop());
    }
}
