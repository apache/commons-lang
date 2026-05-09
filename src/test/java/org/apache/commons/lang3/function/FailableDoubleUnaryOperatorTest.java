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
 * Tests {@link FailableDoubleUnaryOperator}.
 */
class FailableDoubleUnaryOperatorTest extends AbstractLangTest {

    @Test
    void testAndThen_composesCorrectly() throws IOException {
        final FailableDoubleUnaryOperator<IOException> times2 = v -> v * 2;
        final FailableDoubleUnaryOperator<IOException> plus1 = v -> v + 1;
        assertEquals(7.0, times2.andThen(plus1).applyAsDouble(3.0));
    }

    @Test
    void testAndThen_nullAfter_throwsNullPointerException() throws Throwable {
        final FailableDoubleUnaryOperator<IOException> op = v -> v;
        assertNullPointerException(() -> op.andThen(null));
    }

    @Test
    void testApplyAsDouble_returnsResult() throws IOException {
        final FailableDoubleUnaryOperator<IOException> op = v -> v * 2;
        assertEquals(6.0, op.applyAsDouble(3.0));
    }

    @Test
    void testApplyAsDouble_throwsException() {
        final IOException expected = new IOException("fail");
        final FailableDoubleUnaryOperator<IOException> op = v -> {
            throw expected;
        };
        final IOException thrown = assertThrows(IOException.class, () -> op.applyAsDouble(1.0));
        assertEquals(expected, thrown);
    }

    @Test
    void testCompose_composesCorrectly() throws IOException {
        final FailableDoubleUnaryOperator<IOException> times2 = v -> v * 2;
        final FailableDoubleUnaryOperator<IOException> plus1 = v -> v + 1;
        assertEquals(8.0, times2.compose(plus1).applyAsDouble(3.0));
    }

    @Test
    void testCompose_nullBefore_throwsNullPointerException() throws Throwable {
        final FailableDoubleUnaryOperator<IOException> op = v -> v;
        assertNullPointerException(() -> op.compose(null));
    }

    @Test
    void testIdentity_returnsInput() throws IOException {
        assertEquals(3.14, FailableDoubleUnaryOperator.<IOException>identity().applyAsDouble(3.14));
    }

    @Test
    void testNop_applyAsDoubleReturnsZero() throws Throwable {
        assertEquals(0d, FailableDoubleUnaryOperator.nop().applyAsDouble(5.0));
    }

    @Test
    void testNop_returnsNonNull() {
        assertNotNull(FailableDoubleUnaryOperator.nop());
    }
}
