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
 * Tests {@link FailableBiPredicate}.
 */
class FailableBiPredicateTest extends AbstractLangTest {

    @Test
    void testAnd_bothTrue_returnsTrue() throws IOException {
        final FailableBiPredicate<String, String, IOException> p = (a, b) -> true;
        assertTrue(p.and((a, b) -> true).test("x", "y"));
    }

    @Test
    void testAnd_firstFalse_returnsFalse() throws IOException {
        final FailableBiPredicate<String, String, IOException> p = (a, b) -> false;
        assertFalse(p.and((a, b) -> true).test("x", "y"));
    }

    @Test
    void testAnd_nullOther_throwsNullPointerException() throws Throwable {
        final FailableBiPredicate<String, String, IOException> p = (a, b) -> true;
        assertNullPointerException(() -> p.and(null));
    }

    @Test
    void testFalsePredicate_returnsFalse() throws Throwable {
        assertFalse(FailableBiPredicate.falsePredicate().test("a", "b"));
    }

    @Test
    void testNegate_invertsResult() throws IOException {
        final FailableBiPredicate<String, String, IOException> p = (a, b) -> true;
        assertFalse(p.negate().test("x", "y"));
        assertTrue(p.negate().negate().test("x", "y"));
    }

    @Test
    void testOr_bothFalse_returnsFalse() throws IOException {
        final FailableBiPredicate<String, String, IOException> p = (a, b) -> false;
        assertFalse(p.or((a, b) -> false).test("x", "y"));
    }

    @Test
    void testOr_nullOther_throwsNullPointerException() throws Throwable {
        final FailableBiPredicate<String, String, IOException> p = (a, b) -> false;
        assertNullPointerException(() -> p.or(null));
    }

    @Test
    void testOr_oneTrue_returnsTrue() throws IOException {
        final FailableBiPredicate<String, String, IOException> p = (a, b) -> false;
        assertTrue(p.or((a, b) -> true).test("x", "y"));
    }

    @Test
    void testTest_evaluatesCorrectly() throws IOException {
        final FailableBiPredicate<String, String, IOException> eq = String::equals;
        assertTrue(eq.test("x", "x"));
        assertFalse(eq.test("x", "y"));
    }

    @Test
    void testTest_throwsException() {
        final IOException expected = new IOException("fail");
        final FailableBiPredicate<String, String, IOException> p = (a, b) -> {
            throw expected;
        };
        final IOException thrown = assertThrows(IOException.class, () -> p.test("a", "b"));
        assertEquals(expected, thrown);
    }

    @Test
    void testTruePredicate_returnsTrue() throws Throwable {
        assertTrue(FailableBiPredicate.truePredicate().test("a", "b"));
    }
}
