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
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * Tests for {@link ImmutableTimeZone}.
 */
class ImmutableTimeZoneTest extends AbstractLangTest {

    private static final TimeZone NEW_YORK = TimeZone.getTimeZone("America/New_York");
    private static final TimeZone UTC = TimeZone.getTimeZone("UTC");
    private ImmutableTimeZone newYorkImmutable;
    private ImmutableTimeZone utcImmutable;

    @BeforeEach
    void setUp() {
        utcImmutable = new ImmutableTimeZone(UTC);
        newYorkImmutable = new ImmutableTimeZone(NEW_YORK);
    }

    @Test
    void testCloneReturnsSameInstance() {
        // Immutable singleton: clone() must return 'this'
        assertSame(utcImmutable, utcImmutable.clone());
        assertSame(newYorkImmutable, newYorkImmutable.clone());
    }

    @Test
    void testConstructorNullThrowsNullPointerException() {
        assertThrows(NullPointerException.class, () -> new ImmutableTimeZone(null));
    }

    @Test
    void testGetDisplayNameDelegatesToWrappedTimeZone() {
        assertEquals(UTC.getDisplayName(false, TimeZone.LONG, Locale.US), utcImmutable.getDisplayName(false, TimeZone.LONG, Locale.US));
        assertEquals(NEW_YORK.getDisplayName(true, TimeZone.SHORT, Locale.US), newYorkImmutable.getDisplayName(true, TimeZone.SHORT, Locale.US));
    }

    @Test
    void testGetDSTSavingsDelegatesToWrappedTimeZone() {
        assertEquals(UTC.getDSTSavings(), utcImmutable.getDSTSavings());
        assertEquals(NEW_YORK.getDSTSavings(), newYorkImmutable.getDSTSavings());
    }

    @Test
    void testGetIDDelegatesToWrappedTimeZone() {
        assertEquals(UTC.getID(), utcImmutable.getID());
        assertEquals(NEW_YORK.getID(), newYorkImmutable.getID());
    }

    @Test
    void testGetOffsetEraYearMonthDayDelegatesToWrappedTimeZone() {
        // era=1 (AD), year=2024, month=0 (Jan), day=1, dayOfWeek=2 (Mon), milliseconds=0
        assertEquals(UTC.getOffset(1, 2024, 0, 1, 2, 0), utcImmutable.getOffset(1, 2024, 0, 1, 2, 0));
        assertEquals(NEW_YORK.getOffset(1, 2024, 0, 1, 2, 0), newYorkImmutable.getOffset(1, 2024, 0, 1, 2, 0));
    }

    @Test
    void testGetOffsetLongDelegatesToWrappedTimeZone() {
        final long now = System.currentTimeMillis();
        assertEquals(UTC.getOffset(now), utcImmutable.getOffset(now));
        assertEquals(NEW_YORK.getOffset(now), newYorkImmutable.getOffset(now));
    }

    @Test
    void testGetRawOffsetDelegatesToWrappedTimeZone() {
        assertEquals(UTC.getRawOffset(), utcImmutable.getRawOffset());
        assertEquals(NEW_YORK.getRawOffset(), newYorkImmutable.getRawOffset());
    }

    @Test
    void testHasSameRulesDelegatesToWrappedTimeZone() {
        assertTrue(utcImmutable.hasSameRules(UTC));
        assertTrue(newYorkImmutable.hasSameRules(NEW_YORK));
    }

    @Test
    void testInDaylightTimeDelegatesToWrappedTimeZone() {
        final Date date = new Date();
        assertEquals(UTC.inDaylightTime(date), utcImmutable.inDaylightTime(date));
        assertEquals(NEW_YORK.inDaylightTime(date), newYorkImmutable.inDaylightTime(date));
    }

    @Test
    void testObservesDaylightTimeDelegatesToWrappedTimeZone() {
        assertEquals(UTC.observesDaylightTime(), utcImmutable.observesDaylightTime());
        assertEquals(NEW_YORK.observesDaylightTime(), newYorkImmutable.observesDaylightTime());
    }

    @Test
    void testSetIDThrowsUnsupportedOperationException() {
        assertThrows(UnsupportedOperationException.class, () -> utcImmutable.setID("GMT"));
    }

    @Test
    void testSetRawOffsetThrowsUnsupportedOperationException() {
        assertThrows(UnsupportedOperationException.class, () -> utcImmutable.setRawOffset(0));
    }

    @Test
    void testToStringDelegatesToWrappedTimeZone() {
        assertEquals(UTC.toString(), utcImmutable.toString());
        assertEquals(NEW_YORK.toString(), newYorkImmutable.toString());
    }

    @Test
    void testToZoneIdDelegatesToWrappedTimeZone() {
        assertEquals(UTC.toZoneId(), utcImmutable.toZoneId());
        assertEquals(NEW_YORK.toZoneId(), newYorkImmutable.toZoneId());
    }

    @Test
    void testToZoneIdNotNull() {
        assertNotNull(utcImmutable.toZoneId());
        assertNotNull(newYorkImmutable.toZoneId());
    }

    @Test
    void testUseDaylightTimeDelegatesToWrappedTimeZone() {
        assertEquals(UTC.useDaylightTime(), utcImmutable.useDaylightTime());
        assertEquals(NEW_YORK.useDaylightTime(), newYorkImmutable.useDaylightTime());
    }
}
