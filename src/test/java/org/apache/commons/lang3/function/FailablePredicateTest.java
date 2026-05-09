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
 * Tests {@link FailablePredicate}.
 */
class FailablePredicateTest extends AbstractLangTest {

    @Test
    void testAnd_bothTrue_returnsTrue() throws IOException {
        final FailablePredicate<String, IOException> p = s -> true;
        assertTrue(p.and(s -> true).test("x"));
    }

    @Test
    void testAnd_firstFalse_returnsFalse() throws IOException {
        final FailablePredicate<String, IOException> p = s -> false;
        assertFalse(p.and(s -> true).test("x"));
    }

    @Test
    void testAnd_nullOther_throwsNullPointerException() throws Throwable {
        final FailablePredicate<String, IOException> p = s -> true;
        assertNullPointerException(() -> p.and(null));
    }

    @Test
    void testFalsePredicate_returnsFalse() throws Throwable {
        assertFalse(FailablePredicate.falsePredicate().test("anything"));
    }

    @Test
    void testNegate_invertsResult() throws IOException {
        final FailablePredicate<String, IOException> p = s -> true;
        assertFalse(p.negate().test("x"));
        assertTrue(p.negate().negate().test("x"));
    }

    @Test
    void testOr_bothFalse_returnsFalse() throws IOException {
        final FailablePredicate<String, IOException> p = s -> false;
        assertFalse(p.or(s -> false).test("x"));
    }

    @Test
    void testOr_nullOther_throwsNullPointerException() throws Throwable {
        final FailablePredicate<String, IOException> p = s -> false;
        assertNullPointerException(() -> p.or(null));
    }

    @Test
    void testOr_oneTrue_returnsTrue() throws IOException {
        final FailablePredicate<String, IOException> p = s -> false;
        assertTrue(p.or(s -> true).test("x"));
    }

    @Test
    void testTest_evaluatesCorrectly() throws IOException {
        final FailablePredicate<String, IOException> nonEmpty = s -> !s.isEmpty();
        assertTrue(nonEmpty.test("hello"));
        assertFalse(nonEmpty.test(""));
    }

    @Test
    void testTest_throwsException() {
        final IOException expected = new IOException("fail");
        final FailablePredicate<String, IOException> p = s -> {
            throw expected;
        };
        final IOException thrown = assertThrows(IOException.class, () -> p.test("x"));
        assertEquals(expected, thrown);
    }

    @Test
    void testTruePredicate_returnsTrue() throws Throwable {
        assertTrue(FailablePredicate.truePredicate().test("anything"));
    }
}
