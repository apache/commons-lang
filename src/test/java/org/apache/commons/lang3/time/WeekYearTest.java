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

import java.text.ParsePosition;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.Locale;
import java.util.TimeZone;
import java.util.stream.Stream;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

public class WeekYearTest extends AbstractLangTest {

    public static Stream<Arguments> data() {
        return Stream.of(
            Arguments.of(new GregorianCalendar( 2005, Calendar.JANUARY, 1), "2004-W53-6"),
            Arguments.of(new GregorianCalendar( 2005, Calendar.JANUARY, 2), "2004-W53-7"),
            Arguments.of(new GregorianCalendar( 2005, Calendar.DECEMBER, 31), "2005-W52-6"),
            Arguments.of(new GregorianCalendar( 2007, Calendar.JANUARY, 1), "2007-W01-1"),
            Arguments.of(new GregorianCalendar( 2007, Calendar.DECEMBER, 30), "2007-W52-7"),
            Arguments.of(new GregorianCalendar( 2007, Calendar.DECEMBER, 31), "2008-W01-1"),
            Arguments.of(new GregorianCalendar( 2008, Calendar.JANUARY, 1), "2008-W01-2"),
            Arguments.of(new GregorianCalendar( 2008, Calendar.DECEMBER, 28), "2008-W52-7"),
            Arguments.of(new GregorianCalendar( 2008, Calendar.DECEMBER, 29), "2009-W01-1"),
            Arguments.of(new GregorianCalendar( 2008, Calendar.DECEMBER, 30), "2009-W01-2"),
            Arguments.of(new GregorianCalendar( 2008, Calendar.DECEMBER, 31), "2009-W01-3"),
            Arguments.of(new GregorianCalendar( 2009, Calendar.JANUARY, 1), "2009-W01-4"),
            Arguments.of(new GregorianCalendar( 2009, Calendar.DECEMBER, 31), "2009-W53-4"),
            Arguments.of(new GregorianCalendar( 2010, Calendar.JANUARY, 1), "2009-W53-5"),
            Arguments.of(new GregorianCalendar( 2010, Calendar.JANUARY, 2), "2009-W53-6"),
            Arguments.of(new GregorianCalendar( 2010, Calendar.JANUARY, 3), "2009-W53-7")
        );
    }

    @ParameterizedTest
    @MethodSource("data")
    public void testParser(final Calendar vulgar, final String isoForm) {
        final DateParser parser = new FastDateParser("YYYY-'W'ww-u", TimeZone.getDefault(), Locale.getDefault());

        final Calendar cal = Calendar.getInstance();
        cal.setMinimalDaysInFirstWeek(4);
        cal.setFirstDayOfWeek(Calendar.MONDAY);
        cal.clear();

        parser.parse(isoForm, new ParsePosition(0), cal);
        assertEquals(vulgar.getTime(), cal.getTime());
    }

    @ParameterizedTest
    @MethodSource("data")
    public void testPrinter(final Calendar vulgar, final String isoForm) {
        final FastDatePrinter printer = new FastDatePrinter("YYYY-'W'ww-u", TimeZone.getDefault(), Locale.getDefault());

        vulgar.setMinimalDaysInFirstWeek(4);
        vulgar.setFirstDayOfWeek(Calendar.MONDAY);

        assertEquals(isoForm, printer.format(vulgar));
    }
}
