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

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link FailableIntUnaryOperator}.
 */
class FailableIntUnaryOperatorTest extends AbstractLangTest {

    @Test
    void testAndThen_composesCorrectly() throws IOException {
        final FailableIntUnaryOperator<IOException> times2 = v -> v * 2;
        final FailableIntUnaryOperator<IOException> plus1 = v -> v + 1;
        assertEquals(7, times2.andThen(plus1).applyAsInt(3));
    }

    @Test
    void testAndThen_nullAfter_throwsNullPointerException() throws Throwable {
        final FailableIntUnaryOperator<IOException> op = v -> v;
        assertNullPointerException(() -> op.andThen(null));
    }

    @Test
    void testApplyAsInt_returnsResult() throws IOException {
        final FailableIntUnaryOperator<IOException> op = v -> v * 3;
        assertEquals(15, op.applyAsInt(5));
    }

    @Test
    void testApplyAsInt_throwsException() {
        final IOException expected = new IOException("fail");
        final FailableIntUnaryOperator<IOException> op = v -> {
            throw expected;
        };
        final IOException thrown = assertThrows(IOException.class, () -> op.applyAsInt(1));
        assertEquals(expected, thrown);
    }

    @Test
    void testCompose_composesCorrectly() throws IOException {
        final FailableIntUnaryOperator<IOException> times2 = v -> v * 2;
        final FailableIntUnaryOperator<IOException> plus1 = v -> v + 1;
        assertEquals(8, times2.compose(plus1).applyAsInt(3));
    }

    @Test
    void testCompose_nullBefore_throwsNullPointerException() throws Throwable {
        final FailableIntUnaryOperator<IOException> op = v -> v;
        assertNullPointerException(() -> op.compose(null));
    }

    @Test
    void testIdentity_returnsInput() throws IOException {
        assertEquals(7, FailableIntUnaryOperator.<IOException>identity().applyAsInt(7));
    }

    @Test
    void testNop_applyAsIntReturnsZero() throws Throwable {
        assertEquals(0, FailableIntUnaryOperator.nop().applyAsInt(5));
    }

    @Test
    void testNop_returnsNonNull() {
        assertNotNull(FailableIntUnaryOperator.nop());
    }
}
