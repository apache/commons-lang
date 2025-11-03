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

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.function.Predicate;

import org.junit.jupiter.api.Test;

/**
 * Tests {@link Predicates}.
 */
class PredicatesTest {

    @Test
    void testFalsePredicate() {
        assertFalse(Predicates.falsePredicate().test(null));
        assertFalse(Predicates.falsePredicate().test(new Object()));
        final Predicate<String> stringPredicate = Predicates.falsePredicate();
        assertFalse(stringPredicate.test(null));
        assertFalse(stringPredicate.test(""));
    }

    @Test
    void testTruePredicate() {
        assertTrue(Predicates.truePredicate().test(null));
        assertTrue(Predicates.truePredicate().test(new Object()));
        final Predicate<String> stringPredicate = Predicates.truePredicate();
        assertTrue(stringPredicate.test(null));
        assertTrue(stringPredicate.test(""));
    }
}
