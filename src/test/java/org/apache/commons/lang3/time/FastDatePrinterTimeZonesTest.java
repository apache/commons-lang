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

import static org.junit.Assert.assertEquals;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.List;
import java.util.TimeZone;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

@RunWith(Parameterized.class)
public class FastDatePrinterTimeZonesTest {

    private static final String PATTERN = "h:mma z";

    @Parameterized.Parameters
    public static Collection<TimeZone> data() {
        final String[] zoneIds = TimeZone.getAvailableIDs();
        final List<TimeZone> timeZones = new ArrayList<>();
        for (final String zoneId : zoneIds) {
            timeZones.add(TimeZone.getTimeZone(zoneId));
        }
        return timeZones;
    }

    private final TimeZone timeZone;

    public FastDatePrinterTimeZonesTest(final TimeZone timeZone) {
        this.timeZone = timeZone;
    }

    @Test
    public void testCalendarTimezoneRespected() {
        final Calendar cal = Calendar.getInstance(timeZone);

        final SimpleDateFormat sdf = new SimpleDateFormat(PATTERN);
        sdf.setTimeZone(timeZone);
        final String expectedValue = sdf.format(cal.getTime());
        final String actualValue = FastDateFormat.getInstance(PATTERN, this.timeZone).format(cal);
        assertEquals(expectedValue, actualValue);
    }

}
