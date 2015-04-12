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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.text.DateFormatSymbols;
import java.util.Locale;
import java.util.regex.Pattern;

import org.junit.Test;

public class FastDateParser_TimeZoneStrategyTest {

    @Test
    public void testTimeZoneStrategyPattern() {
        Pattern tz = Pattern.compile(FastDateParser.TimeZoneStrategy.TZ_DATABASE);
        assertFalse(tz.matcher("GMT-1234").matches());

        for (Locale locale : Locale.getAvailableLocales()) {
            final String[][] zones = DateFormatSymbols.getInstance(locale).getZoneStrings();
            for (final String[] zone : zones) {
                for (String zoneExpr : zone) {
                    assertTrue(locale.getDisplayName() + ":" + zoneExpr, tz.matcher(zoneExpr).matches());
                }
            }
        }
    }
}
