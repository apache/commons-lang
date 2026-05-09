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
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link FailableIntPredicate}.
 */
class FailableIntPredicateTest extends AbstractLangTest {

    @Test
    void testAnd_bothTrue_returnsTrue() throws IOException {
        final FailableIntPredicate<IOException> p = v -> true;
        assertTrue(p.and(v -> true).test(1));
    }

    @Test
    void testAnd_firstFalse_returnsFalse() throws IOException {
        final FailableIntPredicate<IOException> p = v -> false;
        assertFalse(p.and(v -> true).test(1));
    }

    @Test
    void testAnd_nullOther_throwsNullPointerException() throws Throwable {
        final FailableIntPredicate<IOException> p = v -> true;
        assertNullPointerException(() -> p.and(null));
    }

    @Test
    void testFalsePredicate_returnsFalse() throws Throwable {
        assertFalse(FailableIntPredicate.falsePredicate().test(1));
    }

    @Test
    void testNegate_invertsResult() throws IOException {
        final FailableIntPredicate<IOException> p = v -> true;
        assertFalse(p.negate().test(1));
        assertTrue(p.negate().negate().test(1));
    }

    @Test
    void testOr_bothFalse_returnsFalse() throws IOException {
        final FailableIntPredicate<IOException> p = v -> false;
        assertFalse(p.or(v -> false).test(1));
    }

    @Test
    void testOr_nullOther_throwsNullPointerException() throws Throwable {
        final FailableIntPredicate<IOException> p = v -> false;
        assertNullPointerException(() -> p.or(null));
    }

    @Test
    void testOr_oneTrue_returnsTrue() throws IOException {
        final FailableIntPredicate<IOException> p = v -> false;
        assertTrue(p.or(v -> true).test(1));
    }

    @Test
    void testTest_evaluatesCorrectly() throws IOException {
        final FailableIntPredicate<IOException> positive = v -> v > 0;
        assertTrue(positive.test(1));
        assertFalse(positive.test(-1));
    }

    @Test
    void testTest_throwsException() {
        final IOException expected = new IOException("fail");
        final FailableIntPredicate<IOException> p = v -> {
            throw expected;
        };
        final IOException thrown = assertThrows(IOException.class, () -> p.test(1));
        assertEquals(expected, thrown);
    }

    @Test
    void testTruePredicate_returnsTrue() throws Throwable {
        assertTrue(FailableIntPredicate.truePredicate().test(1));
    }
}
