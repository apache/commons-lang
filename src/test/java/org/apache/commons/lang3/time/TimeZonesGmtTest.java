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

import java.util.TimeZone;

import org.apache.commons.lang3.SerializationUtils;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link ImmutableTimeZone} and {@link TimeZones#GMT}.
 * <p>
 * TimeZones.GMT is a public static final mutable TimeZone. Callers can call setRawOffset() / setID() to globally corrupt time zone behavior.
 * </p>
 * <p>
 * Pre-patch: setRawOffset() / setID() succeed silently and mutate the shared singleton.
 * </p>
 * <p>
 * Post-patch: setRawOffset() / setID() throw UnsupportedOperationException.
 * </p>
 */
public class TimeZonesGmtTest {

    @Test
    public void testGmtTimeZoneIsImmutableSetRawOffset() {
        assertThrows(UnsupportedOperationException.class, () -> TimeZones.GMT.setRawOffset(3600000));
    }

    @Test
    public void testGmtTimeZoneIsImmutableSetId() {
        assertThrows(UnsupportedOperationException.class, () -> TimeZones.GMT.setID("Etc/UTC"));
    }

    @Test
    public void testGmtOffsetRemainsZeroAfterAttemptedMutation() {
        assertThrows(UnsupportedOperationException.class, () -> TimeZones.GMT.setRawOffset(3600000));
        assertEquals(0, TimeZones.GMT.getRawOffset());
    }

    @Test
    public void testGmtIdRemainsGmtAfterAttemptedMutation() {
        assertThrows(UnsupportedOperationException.class, () -> TimeZones.GMT.setID("Etc/UTC"));
        assertEquals("GMT", TimeZones.GMT.getID());
    }

    @Test
    public void testCloneReturnsSelfForImmutableSingleton() {
        // For an immutable singleton, clone() should return the same instance,
        // and there must be no need to defensively copy the value.
        final Object copy = TimeZones.GMT.clone();
        assertSame(TimeZones.GMT, copy, "clone() of an immutable singleton should return the same instance");
    }

    @Test
    public void testSerializedFormUsesNamedClass() throws Exception {
        // Wire-format hazard guard: ensure the implementation class is a named static
        // nested type, not an anonymous inner class whose synthetic name (TimeZones$1)
        // is brittle across non-functional code reorderings.
        assertEquals(ImmutableTimeZone.class.getName(), TimeZones.GMT.getClass().getName());
    }

    @Test
    public void testSerializationRoundTripPreservesId() throws Exception {
        // Round-trip via Serializable to confirm the type is wire-format friendly.
        final TimeZone roundtrip = SerializationUtils.roundtrip(TimeZones.GMT);
        assertNotNull(roundtrip, "Round-tripped TimeZone must not be null");
        assertEquals("GMT", roundtrip.getID(), "Round-tripped TimeZone id must remain \"GMT\"");
        assertEquals(0, roundtrip.getRawOffset(), "Round-tripped TimeZone offset must remain 0");
    }
}
