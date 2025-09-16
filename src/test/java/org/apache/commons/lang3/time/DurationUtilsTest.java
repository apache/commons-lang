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

import static org.apache.commons.lang3.LangAssertions.assertNullPointerException;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.time.Duration;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.concurrent.TimeUnit;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.math.NumberUtils;
import org.junit.jupiter.api.Test;
import org.junitpioneer.jupiter.SetSystemProperty;
import org.junitpioneer.jupiter.SetSystemProperty.SetSystemProperties;

/**
 * Tests {@link DurationUtils}.
 */
class DurationUtilsTest extends AbstractLangTest {

    @Test
    @SetSystemProperties({
        @SetSystemProperty(key = "Seconds1", value = "1"),
        @SetSystemProperty(key = "Seconds2", value = "9223372036854775807") }) // Long.MAX_VALUE
    void testGet() {
        // ChronoUnit.SECONDS
        assertEquals(Duration.ofSeconds(0), DurationUtils.get(null, ChronoUnit.SECONDS, 0));
        assertEquals(Duration.ofSeconds(0), DurationUtils.get("", ChronoUnit.SECONDS, 0));
        assertEquals(Duration.ofSeconds(1), DurationUtils.get("Seconds1", ChronoUnit.SECONDS, 0));
        assertEquals(Duration.ofSeconds(Long.MAX_VALUE), DurationUtils.get("Seconds2", ChronoUnit.SECONDS, 0));
        // ChronoUnit.MILLIS
        assertEquals(Duration.ofMillis(0), DurationUtils.get(null, ChronoUnit.MILLIS, 0));
        assertEquals(Duration.ofMillis(0), DurationUtils.get("", ChronoUnit.MILLIS, 0));
        assertEquals(Duration.ofMillis(1), DurationUtils.get("Seconds1", ChronoUnit.MILLIS, 0));
        assertEquals(Duration.ofMillis(Long.MAX_VALUE), DurationUtils.get("Seconds2", ChronoUnit.MILLIS, 0));
    }

    @Test
    @SetSystemProperties({
        @SetSystemProperty(key = "Seconds1", value = "1"),
        @SetSystemProperty(key = "Seconds2", value = "9223372036854775807") }) // Long.MAX_VALUE
    void testGetMilliseconds() {
        assertEquals(Duration.ofMillis(0), DurationUtils.getMillis(null, 0));
        assertEquals(Duration.ofMillis(0), DurationUtils.getMillis("", 0));
        assertEquals(Duration.ofMillis(1), DurationUtils.getMillis("Seconds1", 0));
        assertEquals(Duration.ofMillis(Long.MAX_VALUE), DurationUtils.getMillis("Seconds2", 0));
    }

    @Test
    void testGetNanosOfMiili() {
        assertEquals(0, DurationUtils.getNanosOfMiili(null));
        assertEquals(0, DurationUtils.getNanosOfMiili(Duration.ZERO));
        assertEquals(1, DurationUtils.getNanosOfMiili(Duration.ofNanos(1)));
        assertEquals(10, DurationUtils.getNanosOfMiili(Duration.ofNanos(10)));
        assertEquals(100, DurationUtils.getNanosOfMiili(Duration.ofNanos(100)));
        assertEquals(1_000, DurationUtils.getNanosOfMiili(Duration.ofNanos(1_000)));
        assertEquals(10_000, DurationUtils.getNanosOfMiili(Duration.ofNanos(10_000)));
        assertEquals(100_000, DurationUtils.getNanosOfMiili(Duration.ofNanos(100_000)));
        assertEquals(0, DurationUtils.getNanosOfMiili(Duration.ofNanos(1_000_000)));
        assertEquals(1, DurationUtils.getNanosOfMiili(Duration.ofNanos(1_000_001)));
    }

    @Test
    void testGetNanosOfMilli() {
        assertEquals(0, DurationUtils.getNanosOfMilli(null));
        assertEquals(0, DurationUtils.getNanosOfMilli(Duration.ZERO));
        assertEquals(1, DurationUtils.getNanosOfMilli(Duration.ofNanos(1)));
        assertEquals(10, DurationUtils.getNanosOfMilli(Duration.ofNanos(10)));
        assertEquals(100, DurationUtils.getNanosOfMilli(Duration.ofNanos(100)));
        assertEquals(1_000, DurationUtils.getNanosOfMilli(Duration.ofNanos(1_000)));
        assertEquals(10_000, DurationUtils.getNanosOfMilli(Duration.ofNanos(10_000)));
        assertEquals(100_000, DurationUtils.getNanosOfMilli(Duration.ofNanos(100_000)));
        assertEquals(0, DurationUtils.getNanosOfMilli(Duration.ofNanos(1_000_000)));
        assertEquals(1, DurationUtils.getNanosOfMilli(Duration.ofNanos(1_000_001)));
    }

    @Test
    @SetSystemProperties({
        @SetSystemProperty(key = "Seconds1", value = "1"),
        @SetSystemProperty(key = "Seconds2", value = "9223372036854775807") }) // Long.MAX_VALUE
    void testGetSeconds() {
        assertEquals(Duration.ofSeconds(0), DurationUtils.getSeconds(null, 0));
        assertEquals(Duration.ofSeconds(0), DurationUtils.getSeconds("", 0));
        assertEquals(Duration.ofSeconds(1), DurationUtils.getSeconds("Seconds1", 0));
        assertEquals(Duration.ofSeconds(Long.MAX_VALUE), DurationUtils.getSeconds("Seconds2", 0));
    }

    @Test
    void testIsPositive() {
        assertFalse(DurationUtils.isPositive(Duration.ZERO));
        assertFalse(DurationUtils.isPositive(Duration.ofMillis(-1)));
        assertTrue(DurationUtils.isPositive(Duration.ofMillis(1)));
    }

    @Test
    void testLongToIntRangeFit() {
        assertEquals(0, DurationUtils.LONG_TO_INT_RANGE.fit(0L));
        //
        assertEquals(Integer.MIN_VALUE, DurationUtils.LONG_TO_INT_RANGE.fit(NumberUtils.LONG_INT_MIN_VALUE));
        assertEquals(Integer.MIN_VALUE, DurationUtils.LONG_TO_INT_RANGE.fit(NumberUtils.LONG_INT_MIN_VALUE - 1));
        assertEquals(Integer.MIN_VALUE, DurationUtils.LONG_TO_INT_RANGE.fit(NumberUtils.LONG_INT_MIN_VALUE - 2));
        assertEquals(Integer.MAX_VALUE, DurationUtils.LONG_TO_INT_RANGE.fit(NumberUtils.LONG_INT_MAX_VALUE));
        assertEquals(Integer.MAX_VALUE, DurationUtils.LONG_TO_INT_RANGE.fit(NumberUtils.LONG_INT_MAX_VALUE + 1));
        assertEquals(Integer.MAX_VALUE, DurationUtils.LONG_TO_INT_RANGE.fit(NumberUtils.LONG_INT_MAX_VALUE + 2));
        //
        assertEquals(Integer.MIN_VALUE, DurationUtils.LONG_TO_INT_RANGE.fit(Long.MIN_VALUE));
        assertEquals(Integer.MAX_VALUE, DurationUtils.LONG_TO_INT_RANGE.fit(Long.MAX_VALUE));
        //
        assertEquals(Short.MIN_VALUE, DurationUtils.LONG_TO_INT_RANGE.fit((long) Short.MIN_VALUE));
        assertEquals(Short.MAX_VALUE, DurationUtils.LONG_TO_INT_RANGE.fit((long) Short.MAX_VALUE));
    }

    @Test
    void testOfConsumer() {
        assertTrue(DurationUtils.of(start -> assertTrue(start.compareTo(Instant.now()) <= 0)).compareTo(Duration.ZERO) >= 0);
        final Instant before = Instant.now();
        DurationUtils.of(start -> assertTrue(start.compareTo(before) >= 0));
    }

    @Test
    void testOfRunnble() {
        assertTrue(DurationUtils.of(this::testSince).compareTo(Duration.ZERO) >= 0);
    }

    @Test
    void testOfRunnbleThrowing() {
        assertThrows(IOException.class, () -> DurationUtils.of(() -> {
            throw new IOException();
        }));
    }

    @Test
    void testSince() {
        assertTrue(DurationUtils.since(Instant.EPOCH).compareTo(Duration.ZERO) >= 0);
        assertTrue(DurationUtils.since(Instant.MIN).compareTo(Duration.ZERO) >= 0);
        assertTrue(DurationUtils.since(Instant.MAX).compareTo(Duration.ZERO) <= 0);
    }

    @Test
    void testToDuration() {
        assertEquals(Duration.ofDays(1), DurationUtils.toDuration(1, TimeUnit.DAYS));
        assertEquals(Duration.ofHours(1), DurationUtils.toDuration(1, TimeUnit.HOURS));
        assertEquals(Duration.ofMillis(1), DurationUtils.toDuration(1_000, TimeUnit.MICROSECONDS));
        assertEquals(Duration.ofMillis(1), DurationUtils.toDuration(1, TimeUnit.MILLISECONDS));
        assertEquals(Duration.ofMinutes(1), DurationUtils.toDuration(1, TimeUnit.MINUTES));
        assertEquals(Duration.ofNanos(1), DurationUtils.toDuration(1, TimeUnit.NANOSECONDS));
        assertEquals(Duration.ofSeconds(1), DurationUtils.toDuration(1, TimeUnit.SECONDS));
        assertEquals(1, DurationUtils.toDuration(1, TimeUnit.MILLISECONDS).toMillis());
        assertEquals(-1, DurationUtils.toDuration(-1, TimeUnit.MILLISECONDS).toMillis());
        assertEquals(0, DurationUtils.toDuration(0, TimeUnit.SECONDS).toMillis());
    }

    @Test
    void testToMillisInt() {
        assertEquals(0, DurationUtils.toMillisInt(Duration.ZERO));
        assertEquals(1, DurationUtils.toMillisInt(Duration.ofMillis(1)));
        //
        assertEquals(Integer.MIN_VALUE, DurationUtils.toMillisInt(Duration.ofMillis(Integer.MIN_VALUE)));
        assertEquals(Integer.MAX_VALUE, DurationUtils.toMillisInt(Duration.ofMillis(Integer.MAX_VALUE)));
        assertEquals(Integer.MAX_VALUE, DurationUtils.toMillisInt(Duration.ofMillis(NumberUtils.LONG_INT_MAX_VALUE + 1)));
        assertEquals(Integer.MAX_VALUE, DurationUtils.toMillisInt(Duration.ofMillis(NumberUtils.LONG_INT_MAX_VALUE + 2)));
        assertEquals(Integer.MIN_VALUE, DurationUtils.toMillisInt(Duration.ofMillis(NumberUtils.LONG_INT_MIN_VALUE - 1)));
        assertEquals(Integer.MIN_VALUE, DurationUtils.toMillisInt(Duration.ofMillis(NumberUtils.LONG_INT_MIN_VALUE - 2)));
        //
        assertEquals(Integer.MIN_VALUE, DurationUtils.toMillisInt(Duration.ofNanos(Long.MIN_VALUE)));
        assertEquals(Integer.MAX_VALUE, DurationUtils.toMillisInt(Duration.ofNanos(Long.MAX_VALUE)));
    }

    @Test
    void testToMillisIntNullDuration() {
        assertNullPointerException(() -> DurationUtils.toMillisInt(null));
    }

    @Test
    void testZeroIfNull() {
        assertEquals(Duration.ZERO, DurationUtils.zeroIfNull(null));
        assertEquals(Duration.ofDays(1), DurationUtils.zeroIfNull(Duration.ofDays(1)));
    }
}
