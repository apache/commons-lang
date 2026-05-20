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

import java.time.ZoneId;
import java.util.Date;
import java.util.Locale;
import java.util.Objects;
import java.util.TimeZone;

/**
 * An immutable {@link TimeZone}.
 */
final class ImmutableTimeZone extends TimeZone {

    private static final long serialVersionUID = 1L;
    private final TimeZone timeZone;

    ImmutableTimeZone(final TimeZone timeZone) {
        this.timeZone = Objects.requireNonNull(timeZone, "timeZone");
    }

    @Override
    public Object clone() {
        // Immutable, so return this.
        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getDisplayName(final boolean daylight, final int style, final Locale locale) {
        return timeZone.getDisplayName(daylight, style, locale);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getDSTSavings() {
        return timeZone.getDSTSavings();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getID() {
        return timeZone.getID();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getOffset(final int era, final int year, final int month, final int day, final int dayOfWeek, final int milliseconds) {
        return timeZone.getOffset(era, year, month, day, dayOfWeek, milliseconds);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getOffset(final long date) {
        return timeZone.getOffset(date);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getRawOffset() {
        return timeZone.getRawOffset();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean hasSameRules(final TimeZone other) {
        return timeZone.hasSameRules(other);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean inDaylightTime(final Date date) {
        return timeZone.inDaylightTime(date);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean observesDaylightTime() {
        return timeZone.observesDaylightTime();
    }

    /**
     * Always throws {@link UnsupportedOperationException}.
     */
    @Override
    public void setID(final String ID) {
        throw new UnsupportedOperationException("This class is immutable.");
    }

    /**
     * Always throws {@link UnsupportedOperationException}.
     */
    @Override
    public void setRawOffset(final int offsetMillis) {
        throw new UnsupportedOperationException("This class is immutable.");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        return timeZone.toString();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ZoneId toZoneId() {
        return timeZone.toZoneId();
    }

    TimeZone unwrap() {
        return timeZone;
    }

    @Override
    public boolean useDaylightTime() {
        return timeZone.useDaylightTime();
    }
}
