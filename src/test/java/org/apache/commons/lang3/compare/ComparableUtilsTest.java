/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang3.compare;

import static org.apache.commons.lang3.compare.ComparableUtils.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.math.BigDecimal;
import java.time.Instant;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.DisplayNameGeneration;
import org.junit.jupiter.api.DisplayNameGenerator;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

@DisplayNameGeneration(DisplayNameGenerator.ReplaceUnderscores.class)
public class ComparableUtilsTest extends AbstractLangTest {

    @Nested
    class A_is_1 {

        @DisplayName("B is 0 (B < A)")
        @Nested
        class B_is_0 {

            @DisplayName("C is 0 ([B=C] < A)")
            @Nested
            class C_is_0 {

                BigDecimal c = BigDecimal.ZERO;

                @Test
                void between_returns_false() {
                    assertFalse(is(a).between(b, c));
                }

                @Test
                void betweenExclusive_returns_false() {
                    assertFalse(is(a).betweenExclusive(b, c));
                }

                @Test
                void static_between_returns_false() {
                    assertFalse(ComparableUtils.between(b, c).test(a));
                }

                @Test
                void static_betweenExclusive_returns_false() {
                    assertFalse(ComparableUtils.betweenExclusive(b, c).test(a));
                }

            }

            @DisplayName("C is 1 (B < A = C)")
            @Nested
            class C_is_1 {

                BigDecimal c = BigDecimal.ONE;

                @Test
                void between_returns_true() {
                    assertTrue(is(a).between(b, c));
                }

                @Test
                void betweenExclusive_returns_false() {
                    assertFalse(is(a).betweenExclusive(b, c));
                }

                @Test
                void static_between_returns_true() {
                    assertTrue(ComparableUtils.between(b, c).test(a));
                }

                @Test
                void static_betweenExclusive_returns_false() {
                    assertFalse(ComparableUtils.betweenExclusive(b, c).test(a));
                }
            }

            @DisplayName("C is 10 (B < A < C)")
            @Nested
            class C_is_10 {

                BigDecimal c = BigDecimal.TEN;

                @Test
                void between_returns_true() {
                    assertTrue(is(a).between(b, c));
                }

                @Test
                void betweenExclusive_returns_true() {
                    assertTrue(is(a).betweenExclusive(b, c));
                }

                @Test
                void static_between_returns_true() {
                    assertTrue(ComparableUtils.between(b, c).test(a));
                }

                @Test
                void static_betweenExclusive_returns_true() {
                    assertTrue(ComparableUtils.betweenExclusive(b, c).test(a));
                }
            }

            BigDecimal b = BigDecimal.ZERO;

            @Test
            void equalTo_returns_false() {
                assertFalse(is(a).equalTo(b));
            }

            @Test
            void greaterThan_returns_true() {
                assertTrue(is(a).greaterThan(b));
            }

            @Test
            void greaterThanOrEqualTo_returns_true() {
                assertTrue(is(a).greaterThanOrEqualTo(b));
            }

            @Test
            void lessThan_returns_false() {
                assertFalse(is(a).lessThan(b));
            }

            @Test
            void lessThanOrEqualTo_returns_false() {
                assertFalse(is(a).lessThanOrEqualTo(b));
            }

            @Test
            void static_gt_returns_true() {
                assertTrue(ComparableUtils.gt(b).test(a));
            }

            @Test
            void static_ge_returns_true() {
                assertTrue(ComparableUtils.ge(b).test(a));
            }

            @Test
            void static_lt_returns_false() {
                assertFalse(ComparableUtils.lt(b).test(a));
            }

            @Test
            void static_le_returns_false() {
                assertFalse(ComparableUtils.le(b).test(a));
            }
        }

        @DisplayName("B is 1 (B = A)")
        @Nested
        class B_is_1 {

            @DisplayName("C is 0 (B = A > C)")
            @Nested
            class C_is_0 {

                BigDecimal c = BigDecimal.ZERO;

                @Test
                void between_returns_true() {
                    assertTrue(is(a).between(b, c));
                }

                @Test
                void betweenExclusive_returns_false() {
                    assertFalse(is(a).betweenExclusive(b, c));
                }

                @Test
                void static_between_returns_true() {
                    assertTrue(ComparableUtils.between(b, c).test(a));
                }

                @Test
                void static_betweenExclusive_returns_false() {
                    assertFalse(ComparableUtils.betweenExclusive(b, c).test(a));
                }
            }

            @DisplayName("C is 1 (B = A = C)")
            @Nested
            class C_is_1 {

                BigDecimal c = BigDecimal.ONE;

                @Test
                void between_returns_true() {
                    assertTrue(is(a).between(b, c));
                }

                @Test
                void betweenExclusive_returns_false() {
                    assertFalse(is(a).betweenExclusive(b, c));
                }

                @Test
                void static_between_returns_true() {
                    assertTrue(ComparableUtils.between(b, c).test(a));
                }

                @Test
                void static_betweenExclusive_returns_false() {
                    assertFalse(ComparableUtils.betweenExclusive(b, c).test(a));
                }
            }

            @DisplayName("C is 10 (B = A < C)")
            @Nested
            class C_is_10 {

                BigDecimal c = BigDecimal.TEN;

                @Test
                void between_returns_true() {
                    assertTrue(is(a).between(b, c));
                }

                @Test
                void betweenExclusive_returns_false() {
                    assertFalse(is(a).betweenExclusive(b, c));
                }

                @Test
                void static_between_returns_true() {
                    assertTrue(ComparableUtils.between(b, c).test(a));
                }

                @Test
                void static_betweenExclusive_returns_false() {
                    assertFalse(ComparableUtils.betweenExclusive(b, c).test(a));
                }
            }

            BigDecimal b = BigDecimal.ONE;

            @Test
            void equalTo_returns_true() {
                assertTrue(is(a).equalTo(b));
            }

            @Test
            void greaterThan_returns_false() {
                assertFalse(is(a).greaterThan(b));
            }

            @Test
            void greaterThanOrEqualTo_returns_true() {
                assertTrue(is(a).greaterThanOrEqualTo(b));
            }

            @Test
            void lessThan_returns_false() {
                assertFalse(is(a).lessThan(b));
            }

            @Test
            void lessThanOrEqualTo_returns_true() {
                assertTrue(is(a).lessThanOrEqualTo(b));
            }

            @Test
            void static_gt_returns_false() {
                assertFalse(ComparableUtils.gt(b).test(a));
            }

            @Test
            void static_ge_returns_true() {
                assertTrue(ComparableUtils.ge(b).test(a));
            }

            @Test
            void static_lt_returns_false() {
                assertFalse(ComparableUtils.lt(b).test(a));
            }

            @Test
            void static_le_returns_true() {
                assertTrue(ComparableUtils.le(b).test(a));
            }
        }

        @DisplayName("B is 10 (B > A)")
        @Nested
        class B_is_10 {

            @DisplayName("C is 0 (B > A > C)")
            @Nested
            class C_is_0 {

                BigDecimal c = BigDecimal.ZERO;

                @Test
                void between_returns_true() {
                    assertTrue(is(a).between(b, c));
                }

                @Test
                void betweenExclusive_returns_true() {
                    assertTrue(is(a).betweenExclusive(b, c));
                }

                @Test
                void static_between_returns_true() {
                    assertTrue(ComparableUtils.between(b, c).test(a));
                }

                @Test
                void static_betweenExclusive_returns_true() {
                    assertTrue(ComparableUtils.betweenExclusive(b, c).test(a));
                }
            }

            @DisplayName("C is 1 (B > A = C)")
            @Nested
            class C_is_1 {

                BigDecimal c = BigDecimal.ONE;

                @Test
                void between_returns_true() {
                    assertTrue(is(a).between(b, c));
                }

                @Test
                void betweenExclusive_returns_false() {
                    assertFalse(is(a).betweenExclusive(b, c));
                }

                @Test
                void static_between_returns_true() {
                    assertTrue(ComparableUtils.between(b, c).test(a));
                }

                @Test
                void static_betweenExclusive_returns_false() {
                    assertFalse(ComparableUtils.betweenExclusive(b, c).test(a));
                }
            }

            @DisplayName("C is 10 ([B,C] > A)")
            @Nested
            class C_is_10 {

                BigDecimal c = BigDecimal.TEN;

                @Test
                void between_returns_false() {
                    assertFalse(is(a).between(b, c));
                }

                @Test
                void betweenExclusive_returns_false() {
                    assertFalse(is(a).betweenExclusive(b, c));
                }

                @Test
                void static_between_returns_false() {
                    assertFalse(ComparableUtils.between(b, c).test(a));
                }

                @Test
                void static_betweenExclusive_returns_false() {
                    assertFalse(ComparableUtils.betweenExclusive(b, c).test(a));
                }
            }

            BigDecimal b = BigDecimal.TEN;

            @Test
            void equalTo_returns_false() {
                assertFalse(is(a).equalTo(b));
            }

            @Test
            void greaterThan_returns_false() {
                assertFalse(is(a).greaterThan(b));
            }

            @Test
            void greaterThanOrEqualTo_returns_false() {
                assertFalse(is(a).greaterThanOrEqualTo(b));
            }

            @Test
            void lessThan_returns_true() {
                assertTrue(is(a).lessThan(b));
            }

            @Test
            void lessThanOrEqualTo_returns_true() {
                assertTrue(is(a).lessThanOrEqualTo(b));
            }

            @Test
            void static_gt_returns_false() {
                assertFalse(ComparableUtils.gt(b).test(a));
            }

            @Test
            void static_ge_returns_false() {
                assertFalse(ComparableUtils.ge(b).test(a));
            }

            @Test
            void static_lt_returns_true() {
                assertTrue(ComparableUtils.lt(b).test(a));
            }

            @Test
            void static_le_returns_true() {
                assertTrue(ComparableUtils.le(b).test(a));
            }
        }

        BigDecimal a = BigDecimal.ONE;
    }

    @Test
    public void testMax() {
        assertEquals(Instant.MAX, ComparableUtils.max(Instant.MAX, Instant.MAX));
        assertEquals(Instant.MIN, ComparableUtils.max(Instant.MIN, Instant.MIN));
        assertEquals(Instant.MAX, ComparableUtils.max(Instant.MIN, Instant.MAX));
        assertEquals(Instant.MAX, ComparableUtils.max(Instant.MAX, Instant.MIN));
        //
        assertEquals(Integer.MIN_VALUE, ComparableUtils.max(Integer.valueOf(Integer.MIN_VALUE), Integer.valueOf(Integer.MIN_VALUE)));
        assertEquals(Integer.MAX_VALUE, ComparableUtils.max(Integer.valueOf(Integer.MAX_VALUE), Integer.valueOf(Integer.MAX_VALUE)));
        assertEquals(Integer.MAX_VALUE, ComparableUtils.max(Integer.valueOf(Integer.MIN_VALUE), Integer.valueOf(Integer.MAX_VALUE)));
        assertEquals(Integer.MAX_VALUE, ComparableUtils.max(Integer.valueOf(Integer.MAX_VALUE), Integer.valueOf(Integer.MIN_VALUE)));
        //
        assertEquals(Instant.MAX, ComparableUtils.max(null, Instant.MAX));
        assertEquals(Instant.MAX, ComparableUtils.max(Instant.MAX, null));
    }

    @Test
    public void testMin() {
        assertEquals(Instant.MAX, ComparableUtils.min(Instant.MAX, Instant.MAX));
        assertEquals(Instant.MIN, ComparableUtils.min(Instant.MIN, Instant.MIN));
        assertEquals(Instant.MIN, ComparableUtils.min(Instant.MIN, Instant.MAX));
        assertEquals(Instant.MIN, ComparableUtils.min(Instant.MAX, Instant.MIN));
        //
        assertEquals(Integer.MIN_VALUE, ComparableUtils.min(Integer.valueOf(Integer.MIN_VALUE), Integer.valueOf(Integer.MIN_VALUE)));
        assertEquals(Integer.MAX_VALUE, ComparableUtils.min(Integer.valueOf(Integer.MAX_VALUE), Integer.valueOf(Integer.MAX_VALUE)));
        assertEquals(Integer.MIN_VALUE, ComparableUtils.min(Integer.valueOf(Integer.MIN_VALUE), Integer.valueOf(Integer.MAX_VALUE)));
        assertEquals(Integer.MIN_VALUE, ComparableUtils.min(Integer.valueOf(Integer.MAX_VALUE), Integer.valueOf(Integer.MIN_VALUE)));
        //
        assertEquals(Instant.MAX, ComparableUtils.min(null, Instant.MAX));
        assertEquals(Instant.MAX, ComparableUtils.min(Instant.MAX, null));
    }
}
