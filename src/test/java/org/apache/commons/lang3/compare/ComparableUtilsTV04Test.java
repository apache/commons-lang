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

package org.apache.commons.lang3.compare;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;

class ComparableUtilsTV04Test {
    @Test void comparisonBuilderCoversOrderingAndEquality() {
        ComparableUtils.ComparableCheckBuilder<Integer> c = ComparableUtils.is(5);
        assertAll(() -> assertTrue(c.equalTo(5)), () -> assertFalse(c.equalTo(4)),
                () -> assertTrue(c.greaterThan(4)), () -> assertFalse(c.greaterThan(5)),
                () -> assertTrue(c.greaterThanOrEqualTo(5)), () -> assertTrue(c.lessThan(6)),
                () -> assertFalse(c.lessThan(5)), () -> assertTrue(c.lessThanOrEqualTo(5)));
    }
    @Test void betweenAcceptsEitherEndpointOrderAndExclusiveRejectsEndpoints() {
        assertTrue(ComparableUtils.is(5).between(1, 9));
        assertTrue(ComparableUtils.is(5).between(9, 1));
        assertTrue(ComparableUtils.is(5).between(5, 9));
        assertFalse(ComparableUtils.is(5).betweenExclusive(5, 9));
        assertTrue(ComparableUtils.is(5).betweenExclusive(1, 9));
    }
    @Test void nullBaseNeverSatisfiesPredicates() {
        ComparableUtils.ComparableCheckBuilder<Integer> c = ComparableUtils.is(null);
        assertAll(() -> assertFalse(c.equalTo(1)), () -> assertFalse(c.greaterThan(1)),
                () -> assertFalse(c.greaterThanOrEqualTo(1)), () -> assertFalse(c.lessThan(1)),
                () -> assertFalse(c.lessThanOrEqualTo(1)), () -> assertFalse(c.between(0, 2)));
    }
    @Test void predicateFactoriesEvaluateTheirInput() {
        assertTrue(ComparableUtils.gt(3).test(4));
        assertFalse(ComparableUtils.gt(3).test(3));
        assertTrue(ComparableUtils.le(3).test(3));
        assertTrue(ComparableUtils.between(1, 4).test(2));
        assertFalse(ComparableUtils.betweenExclusive(1, 4).test(4));
    }
}
