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
import static org.junit.jupiter.api.Assertions.assertTrue;

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
        assertEquals(-1_000_000L, Instants.toEpochMillis(Instant.ofEpochMilli(-1_000_000L)));
    }

    @Test
    void testToEpochMillisNormalInstant() {
        assertEquals(1_000_000L, Instants.toEpochMillis(Instant.ofEpochMilli(1_000_000L)));
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

    /**
     * Epoch instant (time zero): milliseconds since epoch should be positive and equal to
     * roughly the current time in millis (with a generous lower bound).
     */
    @Test
    void testToMillisSinceEpoch() {
        // As of 2026, ~1.7 trillion ms have elapsed since epoch.
        assertTrue(Instants.toMillisSince(Instant.EPOCH) > 0);
    }

    /**
     * A future instant one second from now; milliseconds since it should be negative (roughly -1_000).
     */
    @Test
    void testToMillisSinceFutureInstantIsNegative() {
        final Instant oneSecondFuture = Instant.now().plusMillis(1_000);
        final long millis = Instants.toMillisSince(oneSecondFuture);
        // Duration from a future instant to now is negative.
        assertTrue(millis <= -900, "Expected millis <= -900 but was " + millis);
    }

    /**
     * {@link Instant#MAX} (positive epoch second): the huge negative duration from Instant.MAX to now
     * overflows {@code long} millis; the bound is {@link Long#MAX_VALUE} because the instant's epoch
     * second is positive.
     */
    @Test
    void testToMillisSinceInstantMaxOverflowReturnsMaxValue() {
        assertEquals(Long.MAX_VALUE, Instants.toMillisSince(Instant.MAX));
    }

    /**
     * {@link Instant#MIN} (negative epoch second): the huge positive duration from Instant.MIN to now
     * overflows {@code long} millis; the bound is {@link Long#MIN_VALUE} because the instant's epoch
     * second is negative.
     */
    @Test
    void testToMillisSinceInstantMinOverflowReturnsMinValue() {
        assertEquals(Long.MIN_VALUE, Instants.toMillisSince(Instant.MIN));
    }

    /**
     * Instant.now(); milliseconds since should be very close to zero (within a generous tolerance).
     */
    @Test
    void testToMillisSinceNowIsNearZero() {
        final Instant now = Instant.now();
        final long millis = Instants.toMillisSince(now);
        // Allow a generous 5_000 ms window for slow test environments.
        assertTrue(millis >= 0 && millis < 5_000, "Expected millis in [0, 5000) but was " + millis);
    }

    /**
     * A past instant one second ago; milliseconds since it should be approximately 1_000.
     */
    @Test
    void testToMillisSincePastInstantIsPositive() {
        final Instant oneSecondAgo = Instant.now().minusMillis(1_000);
        final long millis = Instants.toMillisSince(oneSecondAgo);
        // Should be at least 1_000 ms (wall time may have advanced slightly more).
        assertTrue(millis >= 1_000, "Expected millis >= 1000 but was " + millis);
    }
}
