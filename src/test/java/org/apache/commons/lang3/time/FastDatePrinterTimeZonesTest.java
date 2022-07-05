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

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.TimeZone;
import java.util.stream.Stream;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

public class FastDatePrinterTimeZonesTest extends AbstractLangTest {

    private static final String PATTERN = "h:mma z";

    public static Stream<TimeZone> data() {
        return Stream.of(TimeZone.getAvailableIDs()).map(TimeZone::getTimeZone);
    }

    @ParameterizedTest
    @MethodSource("data")
    public void testCalendarTimezoneRespected(final TimeZone timeZone) {
        final Calendar cal = Calendar.getInstance(timeZone);

        final SimpleDateFormat sdf = new SimpleDateFormat(PATTERN);
        sdf.setTimeZone(timeZone);
        final String expectedValue = sdf.format(cal.getTime());
        final String actualValue = FastDateFormat.getInstance(PATTERN, timeZone).format(cal);
        assertEquals(expectedValue, actualValue);
    }

}
