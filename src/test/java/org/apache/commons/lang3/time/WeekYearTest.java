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

import java.text.ParseException;
import java.text.ParsePosition;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.GregorianCalendar;
import java.util.Locale;
import java.util.TimeZone;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

@RunWith(Parameterized.class)
public class WeekYearTest {

    @Parameters(name = "{index}: {3}")
    public static Collection<Object[]> data() {
        return Arrays
                .asList(new Object[][] {
                    { 2005, Calendar.JANUARY, 1, "2004-W53-6" },
                    { 2005, Calendar.JANUARY, 2, "2004-W53-7" },
                    { 2005, Calendar.DECEMBER, 31, "2005-W52-6" },
                    { 2007, Calendar.JANUARY, 1, "2007-W01-1" },
                    { 2007, Calendar.DECEMBER, 30, "2007-W52-7" },
                    { 2007, Calendar.DECEMBER, 31, "2008-W01-1" },
                    { 2008, Calendar.JANUARY, 1, "2008-W01-2" },
                    { 2008, Calendar.DECEMBER, 28, "2008-W52-7" },
                    { 2008, Calendar.DECEMBER, 29, "2009-W01-1" },
                    { 2008, Calendar.DECEMBER, 30, "2009-W01-2" },
                    { 2008, Calendar.DECEMBER, 31, "2009-W01-3" },
                    { 2009, Calendar.JANUARY, 1, "2009-W01-4" },
                    { 2009, Calendar.DECEMBER, 31, "2009-W53-4" },
                    { 2010, Calendar.JANUARY, 1, "2009-W53-5" },
                    { 2010, Calendar.JANUARY, 2, "2009-W53-6" },
                    { 2010, Calendar.JANUARY, 3, "2009-W53-7" }
                });
    }

    final Calendar vulgar;
    final String isoForm;

    public WeekYearTest(final int year, final int month, final int day, final String isoForm) {
        vulgar = new GregorianCalendar(year, month, day);
        this.isoForm = isoForm;
    }

    @Test
    public void testParser() throws ParseException {
        final DateParser parser = new FastDateParser("YYYY-'W'ww-u", TimeZone.getDefault(), Locale.getDefault());

        final Calendar cal = Calendar.getInstance();
        cal.setMinimalDaysInFirstWeek(4);
        cal.setFirstDayOfWeek(Calendar.MONDAY);
        cal.clear();

        parser.parse(isoForm, new ParsePosition(0), cal);
        assertEquals(vulgar.getTime(), cal.getTime());
    }

    @Test
    public void testPrinter() {
        final FastDatePrinter printer = new FastDatePrinter("YYYY-'W'ww-u", TimeZone.getDefault(), Locale.getDefault());

        vulgar.setMinimalDaysInFirstWeek(4);
        vulgar.setFirstDayOfWeek(Calendar.MONDAY);

        assertEquals(isoForm, printer.format(vulgar));
    }
}
