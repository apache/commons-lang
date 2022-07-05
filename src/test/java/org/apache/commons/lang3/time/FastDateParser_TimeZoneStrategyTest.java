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

import static org.junit.jupiter.api.Assertions.assertNotEquals;

import java.text.DateFormatSymbols;
import java.text.ParseException;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

public class FastDateParser_TimeZoneStrategyTest extends AbstractLangTest {

    @Test
    void testLang1219() throws ParseException {
        final FastDateParser parser = new FastDateParser("dd.MM.yyyy HH:mm:ss z", TimeZone.getDefault(), Locale.GERMAN);

        final Date summer = parser.parse("26.10.2014 02:00:00 MESZ");
        final Date standard = parser.parse("26.10.2014 02:00:00 MEZ");
        assertNotEquals(summer.getTime(), standard.getTime());
    }

    @ParameterizedTest
    @MethodSource("java.util.Locale#getAvailableLocales")
    void testTimeZoneStrategyPattern(final Locale locale) throws ParseException {
        final FastDateParser parser = new FastDateParser("z", TimeZone.getDefault(), locale);
        final String[][] zones = DateFormatSymbols.getInstance(locale).getZoneStrings();
        for (final String[] zone : zones) {
            for (int t = 1; t < zone.length; ++t) {
                final String tzDisplay = zone[t];
                if (tzDisplay == null) {
                    break;
                }
                // An exception will be thrown and the test will fail if parsing isn't successful
                parser.parse(tzDisplay);
            }
        }
    }
}
