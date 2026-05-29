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

package org.apache.commons.lang3.time;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.time.Instant;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link Instants}.
 */
class InstantsTest extends AbstractLangTest {

    /**
     * An Instant far in the future whose epoch-millis overflow {@code long}: seconds chosen so that {@code seconds * 1_000} exceeds {@link Long#MAX_VALUE}.
     */
    private static final Instant INSTANT_FAR_FUTURE = Instant.ofEpochSecond(Long.MAX_VALUE / 1_000 + 1_000);
    /**
     * An Instant far in the past whose epoch-millis underflow {@code long}: seconds chosen so that {@code seconds * 1_000} is less than {@link Long#MIN_VALUE}.
     */
    private static final Instant INSTANT_FAR_PAST = Instant.ofEpochSecond(Long.MIN_VALUE / 1_000 - 1_000);

    @Test
    void testToEpochMillisEpoch() {
        assertEquals(0L, Instants.toEpochMillis(Instant.EPOCH));
    }

    @Test
    void testToEpochMillisMaxValue() {
        assertEquals(Long.MAX_VALUE, Instants.toEpochMillis(Instant.ofEpochMilli(Long.MAX_VALUE)));
    }

    @Test
    void testToEpochMillisMinValue() {
        assertEquals(Long.MIN_VALUE, Instants.toEpochMillis(Instant.ofEpochMilli(Long.MIN_VALUE)));
    }

    @Test
    void testToEpochMillisNegativeMillis() {
        final Instant instant = Instant.ofEpochMilli(-1_000_000L);
        assertEquals(-1_000_000L, Instants.toEpochMillis(instant));
    }

    @Test
    void testToEpochMillisNormalInstant() {
        final Instant instant = Instant.ofEpochMilli(1_000_000L);
        assertEquals(1_000_000L, Instants.toEpochMillis(instant));
    }

    @Test
    void testToEpochMillisNow() {
        final Instant now = Instant.now();
        assertEquals(now.toEpochMilli(), Instants.toEpochMillis(now));
    }

    @Test
    void testToEpochMillisOverflowReturnsMaxValue() {
        assertEquals(Long.MAX_VALUE, Instants.toEpochMillis(INSTANT_FAR_FUTURE));
    }

    @Test
    void testToEpochMillisUnderflowReturnsMinValue() {
        assertEquals(Long.MIN_VALUE, Instants.toEpochMillis(INSTANT_FAR_PAST));
    }
}
