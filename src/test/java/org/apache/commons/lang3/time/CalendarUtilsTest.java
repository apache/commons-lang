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

import java.util.Calendar;
import java.util.Locale;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

public class CalendarUtilsTest extends AbstractLangTest {

    @Test
    public void testGetDayOfMonth() {
        assertEquals(Calendar.getInstance().get(Calendar.DAY_OF_MONTH), CalendarUtils.INSTANCE.getDayOfMonth());
    }

    @Test
    public void testGetDayOfYear() {
        assertEquals(Calendar.getInstance().get(Calendar.DAY_OF_YEAR), CalendarUtils.INSTANCE.getDayOfYear());
    }

    @Test
    public void testGetMonth() {
        assertEquals(Calendar.getInstance().get(Calendar.MONTH), CalendarUtils.INSTANCE.getMonth());
    }

    @Test
    public void testGetStandaloneLongMonthNames() {
        final String[] monthNames = CalendarUtils.getInstance(Locale.GERMAN).getStandaloneLongMonthNames();
        assertEquals(12, monthNames.length);
        assertEquals("Januar", monthNames[0]);
        assertEquals("Februar", monthNames[1]);
        assertEquals("M\u00e4rz", monthNames[2]);
        assertEquals("April", monthNames[3]);
        assertEquals("Mai", monthNames[4]);
        assertEquals("Juni", monthNames[5]);
        assertEquals("Juli", monthNames[6]);
        assertEquals("August", monthNames[7]);
        assertEquals("September", monthNames[8]);
        assertEquals("Oktober", monthNames[9]);
        assertEquals("November", monthNames[10]);
        assertEquals("Dezember", monthNames[11]);
    }

    @Test
    public void testGetStandaloneShortMonthNames() {
        final String[] monthNames = CalendarUtils.getInstance(Locale.GERMAN).getStandaloneShortMonthNames();
        assertEquals(12, monthNames.length);
        assertEquals("Jan", monthNames[0]);
        assertEquals("Feb", monthNames[1]);
        assertEquals("M\u00e4r", monthNames[2]);
        assertEquals("Apr", monthNames[3]);
        assertEquals("Mai", monthNames[4]);
        assertEquals("Jun", monthNames[5]);
        assertEquals("Jul", monthNames[6]);
        assertEquals("Aug", monthNames[7]);
        assertEquals("Sep", monthNames[8]);
        assertEquals("Okt", monthNames[9]);
        assertEquals("Nov", monthNames[10]);
        assertEquals("Dez", monthNames[11]);
    }

    @Test
    public void testGetYear() {
        assertEquals(Calendar.getInstance().get(Calendar.YEAR), CalendarUtils.INSTANCE.getYear());
    }


}
