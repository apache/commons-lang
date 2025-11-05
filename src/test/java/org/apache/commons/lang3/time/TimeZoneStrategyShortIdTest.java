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

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.time.ZoneId;
import java.util.Map;

import org.apache.commons.lang3.time.FastDateParser.TimeZoneStrategy;
import org.junit.jupiter.api.condition.EnabledForJreRange;
import org.junit.jupiter.api.condition.EnabledOnJre;
import org.junit.jupiter.api.condition.JRE;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Tests {@link TimeZoneStrategy} with and without {@link ZoneId#SHORT_IDS}.
 */
public class TimeZoneStrategyShortIdTest {

    @ParameterizedTest
    @EnabledForJreRange(max = JRE.JAVA_24)
    @MethodSource("org.apache.commons.lang3.time.FastDateParser_TimeZoneStrategyTest#getZoneIdStream")
    void testJava24DownInvalidId(final Map.Entry<String, String> entry) {
        assertFalse(TimeZoneStrategy.skipTimeZone(entry.getKey(), false));
    }

    @ParameterizedTest
    @EnabledForJreRange(max = JRE.JAVA_24)
    @MethodSource("org.apache.commons.lang3.time.FastDateParser_TimeZoneStrategyTest#getZoneIdStream")
    void testJava24DownInvalidIdCompatible(final Map.Entry<String, String> entry) {
        assertFalse(TimeZoneStrategy.skipTimeZone(entry.getKey(), true));
    }

    @ParameterizedTest
    @EnabledOnJre(JRE.JAVA_25)
    @MethodSource("org.apache.commons.lang3.time.FastDateParser_TimeZoneStrategyTest#getZoneIdStream")
    void testJava25InvalidId(final Map.Entry<String, String> entry) {
        assertFalse(TimeZoneStrategy.skipTimeZone(entry.getKey(), false));
    }

    @ParameterizedTest
    @EnabledOnJre(JRE.JAVA_25)
    @MethodSource("org.apache.commons.lang3.time.FastDateParser_TimeZoneStrategyTest#getZoneIdStream")
    void testJava25UpInvalidIdCompatible(final Map.Entry<String, String> entry) {
        assertTrue(TimeZoneStrategy.skipTimeZone(entry.getKey(), true));
    }
}
