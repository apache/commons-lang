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

package org.apache.commons.lang3.builder;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.time.Duration;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.OffsetTime;
import java.time.Period;
import java.time.Year;
import java.time.YearMonth;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.chrono.HijrahDate;
import java.time.chrono.JapaneseDate;
import java.time.chrono.MinguoDate;
import java.time.chrono.ThaiBuddhistDate;
import java.time.temporal.Temporal;
import java.time.temporal.TemporalAccessor;
import java.time.temporal.TemporalAmount;
import java.time.temporal.TemporalField;
import java.time.temporal.TemporalUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Supplier;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.stream.IntStreams;
import org.junit.jupiter.api.Test;

/**
 * Tests that {@link EqualsBuilder} works using reflection when types that implement JRE interfaces like TemporalAccessor, TemporalAmout, and CharSequence work.
 */
class EqualsBuilderReflectJreImplementationTest extends AbstractLangTest {

    static class MyCharSequence implements CharSequence {

        private final char[] chars;

        MyCharSequence(final char[] chars) {
            this.chars = Arrays.copyOf(chars, chars.length);
        }

        MyCharSequence(final char[] chars, final int start, final int end) {
            this.chars = Arrays.copyOfRange(chars, start, end);
        }

        MyCharSequence(final String string) {
            this.chars = string.toCharArray();
        }

        @Override
        public char charAt(final int index) {
            return chars[index];
        }

        @Override
        public int length() {
            return chars.length;
        }

        @Override
        public CharSequence subSequence(final int start, final int end) {
            return new MyCharSequence(chars, start, end);
        }

        @Override
        public String toString() {
            return new String(chars);
        }
    }

    static class MyClass implements Cloneable {

        private final MyCharSequence charSequence;
        private final MyTemporal temporal;
        private final MyTemporalAccessor temporalAccessor;
        private final MyTemporalAmount temporalAmount;
        private final Object[] objects;
        private final List<Supplier<?>> list = new ArrayList<>();

        MyClass(final MyCharSequence charSequence, final MyTemporal temporal, final MyTemporalAccessor temporalAccessor,
                final MyTemporalAmount temporalAmount) {
            this.charSequence = charSequence;
            this.temporal = temporal;
            this.temporalAccessor = temporalAccessor;
            this.temporalAmount = temporalAmount;
            final int value = Integer.parseInt(charSequence.toString());
            final LocalDate localDate = LocalDate.ofEpochDay(value);
            final LocalTime localTime = LocalTime.of(value, value);
            final LocalDateTime localDateTime = LocalDateTime.of(localDate, localTime);
            final OffsetDateTime offsetDateTime = OffsetDateTime.of(localDateTime, ZoneOffset.UTC);
            final ZoneOffset zoneOffset = ZoneOffset.ofHours(value);
            this.objects = new Object[] {
                    // a Long
                    value,
                    // all concrete dates and times
                    localDate, HijrahDate.from(localDate), JapaneseDate.from(localDate), MinguoDate.from(localDate), ThaiBuddhistDate.from(localDate),
                    localDate, localTime, localDateTime, offsetDateTime, OffsetTime.of(localTime, zoneOffset), Year.of(value), YearMonth.of(value, value),
                    ZonedDateTime.of(localDateTime, zoneOffset), zoneOffset, ZoneId.of(zoneOffset.getId()) };
            IntStreams.range(100).forEach(i -> list.add(() -> charSequence));
        }

        @Override
        public String toString() {
            return String.format("%s[%s, %s, %s, %s, %s]", getClass().getSimpleName(), charSequence, temporal, temporalAccessor, temporalAmount,
                    Arrays.toString(objects));
        }
    }

    static class MyTemporal implements Temporal {

        private final String string;
        private final int value;
        private final Duration duration;
        private final Instant instant;
        private final Period period;

        MyTemporal(final String string) {
            this.string = string;
            this.value = Integer.parseInt(string);
            this.instant = Instant.ofEpochMilli(value);
            this.duration = Duration.between(instant, instant.plusMillis(value));
            this.period = Period.ofDays(value);
        }

        @Override
        public long getLong(final TemporalField field) {
            return instant.get(field);
        }

        @Override
        public boolean isSupported(final TemporalField field) {
            return instant.isSupported(field);
        }

        @Override
        public boolean isSupported(final TemporalUnit unit) {
            return instant.isSupported(unit);
        }

        @Override
        public Temporal plus(final long amountToAdd, final TemporalUnit unit) {
            return instant.plus(amountToAdd, unit);
        }

        @Override
        public String toString() {
            return String.format("%s[%s, %s, %s, %s]", getClass().getSimpleName(), string, instant, duration, period);
        }

        @Override
        public long until(final Temporal endExclusive, final TemporalUnit unit) {
            return instant.until(endExclusive, unit);
        }

        @Override
        public Temporal with(final TemporalField field, final long newValue) {
            return instant.with(field, newValue);
        }

    }

    static class MyTemporalAccessor implements TemporalAccessor {

        private final String string;
        private final int value;
        private final Instant instant;
        private final Duration duration;
        private final Period period;

        MyTemporalAccessor(final String string) {
            this.string = string;
            this.value = Integer.parseInt(string);
            this.instant = Instant.ofEpochMilli(value);
            this.duration = Duration.between(instant, instant.plusMillis(value));
            this.period = Period.ofDays(value);
        }

        @Override
        public long getLong(final TemporalField field) {
            return instant.get(field);
        }

        @Override
        public boolean isSupported(final TemporalField field) {
            return instant.isSupported(field);
        }

        @Override
        public String toString() {
            return String.format("%s[%s, %s, %s, %s]", getClass().getSimpleName(), string, instant, duration, period);
        }

    }

    static class MyTemporalAmount implements TemporalAmount {

        private final String string;
        private final int value;
        private final Instant instant;
        private final Duration duration;
        private final Period period;

        MyTemporalAmount(final String string) {
            this.string = string;
            this.value = Integer.parseInt(string);
            this.instant = Instant.ofEpochMilli(value);
            this.duration = Duration.between(instant, instant.plusMillis(value));
            this.period = Period.ofDays(value);
        }

        @Override
        public Temporal addTo(final Temporal temporal) {
            return duration.addTo(temporal);
        }

        @Override
        public long get(final TemporalUnit unit) {
            return duration.get(unit);
        }

        @Override
        public List<TemporalUnit> getUnits() {
            return duration.getUnits();
        }

        @Override
        public Temporal subtractFrom(final Temporal temporal) {
            return duration.subtractFrom(temporal);
        }

        @Override
        public String toString() {
            return String.format("%s[%s - %s - %s - %s]", getClass().getSimpleName(), string, instant, duration, period);
        }

    }

    @Test
    void testRecursive() {
        final MyClass o1 = new MyClass(new MyCharSequence("1"), new MyTemporal("2"), new MyTemporalAccessor("3"), new MyTemporalAmount("4"));
        // This gives you different instances of MyTemporalAccessor for 1 (and 2) that should be equals by reflection.
        final MyClass o1Bis = new MyClass(new MyCharSequence("1"), new MyTemporal("2"), new MyTemporalAccessor("3"), new MyTemporalAmount("4"));
        final MyClass o2 = new MyClass(new MyCharSequence("5"), new MyTemporal("6"), new MyTemporalAccessor("7"), new MyTemporalAmount("8"));
        final MyClass o2Bis = new MyClass(new MyCharSequence("5"), new MyTemporal("6"), new MyTemporalAccessor("7"), new MyTemporalAmount("8"));
        // MyTemporal
        assertTrue(new EqualsBuilder().setTestRecursive(true).append(new MyTemporal("1"), new MyTemporal("1")).isEquals());
        // MyTemporalAccessor
        assertTrue(new EqualsBuilder().setTestRecursive(true).append(new MyTemporalAccessor("1"), new MyTemporalAccessor("1")).isEquals());
        // MyCharSequence
        assertTrue(new EqualsBuilder().setTestRecursive(true).append(new MyCharSequence("1"), new MyCharSequence("1")).isEquals());
        // MyClass
        assertTrue(new EqualsBuilder().setTestRecursive(true).append(o1, o1).isEquals(), o1::toString);
        assertTrue(new EqualsBuilder().setTestRecursive(true).append(o1, o1Bis).isEquals(), o1::toString);
        assertTrue(new EqualsBuilder().setTestRecursive(true).append(o2, o2).isEquals(), o2::toString);
        assertTrue(new EqualsBuilder().setTestRecursive(true).append(o2, o2Bis).isEquals(), o2::toString);
        assertFalse(new EqualsBuilder().setTestRecursive(true).append(o1, o2).isEquals());
        assertFalse(new EqualsBuilder().setTestRecursive(true).append(o2, o1).isEquals());
    }

    @Test
    void testRetention() throws Exception {
        // The following should not retain memory.
        for (int i = 0; i < Integer.getInteger("testRetention", 10_000); i++) {
            final Class<?> clazz = TestClassBuilder.defineSimpleClass(getClass().getPackage().getName(), i);
            assertTrue(new EqualsBuilder().setTestRecursive(true).append(clazz.newInstance(), clazz.newInstance()).isEquals());
        }
        // some retention is checked in super's after().
    }

}
