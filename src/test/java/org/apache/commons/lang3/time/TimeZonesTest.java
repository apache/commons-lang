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

package org.apache.commons.lang3.time;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.TimeZone;

import org.junit.jupiter.api.Test;

/**
 * Tests {@link TimeZones}.
 */
public class TimeZonesTest {

    static final String TIME_ZONE_GET_AVAILABLE_IDS = "java.util.TimeZone#getAvailableIDs()";

    @Test
    public void testToTimeZone() {
        assertEquals(TimeZone.getDefault(), TimeZones.toTimeZone(null));
        assertEquals(TimeZone.getDefault(), TimeZones.toTimeZone(TimeZone.getDefault()));
        assertEquals(TimeZones.GMT, TimeZones.toTimeZone(TimeZones.GMT));
    }
}
