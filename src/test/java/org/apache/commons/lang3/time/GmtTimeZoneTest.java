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

import static org.apache.commons.lang3.LangAssertions.assertIllegalArgumentException;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Tests for GmtTimeZone
 */
class GmtTimeZoneTest extends AbstractLangTest {

    @Test
    void testGetID() {
        assertEquals("GMT+00:00", new GmtTimeZone(false, 0, 0).getID());
        assertEquals("GMT+01:02", new GmtTimeZone(false, 1, 2).getID());
        assertEquals("GMT+11:22", new GmtTimeZone(false, 11, 22).getID());
        assertEquals("GMT-01:02", new GmtTimeZone(true, 1, 2).getID());
        assertEquals("GMT-11:22", new GmtTimeZone(true, 11, 22).getID());
    }

    @Test
    void testGetOffset() {
        assertEquals(0, new GmtTimeZone(false, 0, 0).getOffset(234304));
        assertEquals(-(6 * 60 + 30) * 60 * 1000,
                new GmtTimeZone(true, 6, 30).getOffset(1, 1, 1, 1, 1, 1));
    }

    @Test
    void testGetRawOffset() {
        assertEquals(0, new GmtTimeZone(false, 0, 0).getRawOffset());
    }

    @Test
    void testHoursInRange() {
        assertEquals(23 * 60 * 60 * 1000, new GmtTimeZone(false, 23, 0).getRawOffset());
    }

    @Test
    void testHoursOutOfRange() {
        assertIllegalArgumentException(() -> new GmtTimeZone(false, 24, 0));
    }

    @Test
    void testInDaylightTime() {
        assertFalse(new GmtTimeZone(false, 0, 0).useDaylightTime());
    }

    @Test
    void testMinutesInRange() {
        assertEquals(59 * 60 * 1000, new GmtTimeZone(false, 0, 59).getRawOffset());
    }

    @Test
    void testMinutesOutOfRange() {
        assertIllegalArgumentException(() -> new GmtTimeZone(false, 0, 60));
    }

    @Test
    void testSetRawOffset() {
        assertThrows(UnsupportedOperationException.class, () -> new GmtTimeZone(false, 0, 0).setRawOffset(0));
    }

    @Test
    void testToString() {
        assertEquals("[GmtTimeZone id=\"GMT-12:00\",offset=-43200000]",
            new GmtTimeZone(true, 12, 0).toString());
    }

    @Test
    void testUseDaylightTime() {
        assertFalse(new GmtTimeZone(false, 0, 0).useDaylightTime());
    }
}
