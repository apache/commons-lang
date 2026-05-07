/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.commons.lang3;

import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;

/**
 * NaN bypass in primitive double range validators.
 * <p>
 * Validate.inclusiveBetween(double,double,double) and Validate.exclusiveBetween(double,double,double) use ordered comparisons (&lt; > &lt;= >=). All ordered
 * comparisons involving NaN return false, so Double.NaN passes any range check silently. A caller expecting NaN to be rejected (for example, for quota or
 * monetary validation) receives no exception.
 * </p>
 * <p>
 * Pre-patch: tests below FAIL (no exception thrown). Post-patch: tests below PASS (IllegalArgumentException thrown for NaN).
 * </p>
 */
class ValidateDoublesTest extends AbstractLangTest {

    @Test
    void exclusiveBetweenRejectsNaN() {
        assertThrows(IllegalArgumentException.class, () -> Validate.exclusiveBetween(0.0, 10.0, Double.NaN),
                "NaN should be rejected by exclusiveBetween but is silently accepted");
    }

    @Test
    void exclusiveBetweenWithMessageRejectsNaN() {
        assertThrows(IllegalArgumentException.class, () -> Validate.exclusiveBetween(0.0, 10.0, Double.NaN, "value must be in (0,10)"),
                "NaN should be rejected by exclusiveBetween(with message)");
    }

    @Test
    void inclusiveBetweenRejectsNaN() {
        assertThrows(IllegalArgumentException.class, () -> Validate.inclusiveBetween(0.0, 10.0, Double.NaN),
                "NaN should be rejected by inclusiveBetween but is silently accepted");
    }

    @Test
    void inclusiveBetweenRejectsNegativeInfinity() {
        assertThrows(IllegalArgumentException.class, () -> Validate.inclusiveBetween(0.0, 10.0, Double.NEGATIVE_INFINITY),
                "Positive infinity beyond range end should be rejected");
    }

    @Test
    void inclusiveBetweenRejectsPositiveInfinity() {
        assertThrows(IllegalArgumentException.class, () -> Validate.inclusiveBetween(0.0, 10.0, Double.POSITIVE_INFINITY),
                "Positive infinity beyond range end should be rejected");
    }

    @Test
    void inclusiveBetweenWithMessageRejectsNaN() {
        assertThrows(IllegalArgumentException.class, () -> Validate.inclusiveBetween(0.0, 10.0, Double.NaN, "value must be in [0,10]"),
                "NaN should be rejected by inclusiveBetween(with message)");
    }

    @Test
    void validFiniteValuesStillAccepted() {
        Validate.inclusiveBetween(0.0, 10.0, 5.0);
        Validate.inclusiveBetween(Double.NEGATIVE_INFINITY, Double.POSITIVE_INFINITY, 0.0);
        Validate.exclusiveBetween(0.0, 10.0, 5.0);
        Validate.exclusiveBetween(Double.NEGATIVE_INFINITY, Double.POSITIVE_INFINITY, 0.0);
    }
}
