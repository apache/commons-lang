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

import java.util.Date;
import java.util.Objects;
import java.util.SimpleTimeZone;
import java.util.TimeZone;

/**
 * Custom time zone that contains offset from GMT.
 *
 * @since 3.7
 */
class GmtTimeZone extends TimeZone {

    private static final int MILLISECONDS_PER_MINUTE = 60 * 1000;
    private static final int MINUTES_PER_HOUR = 60;
    private static final int HOURS_PER_DAY = 24;

    // Serializable!
    static final long serialVersionUID = 1L;

    private final SimpleTimeZone m_delegate;

    GmtTimeZone(final boolean negate, final int hours, final int minutes) {
        if (hours >= HOURS_PER_DAY) {
            throw new IllegalArgumentException(hours + " hours out of range");
        }
        if (minutes >= MINUTES_PER_HOUR) {
            throw new IllegalArgumentException(minutes + " minutes out of range");
        }
        final int milliseconds = (minutes + (hours * MINUTES_PER_HOUR)) * MILLISECONDS_PER_MINUTE;
        final int offset = negate ? -milliseconds : milliseconds;
        final String zoneId = twoDigits(
            twoDigits(new StringBuilder(9).append("GMT").append(negate ? '-' : '+'), hours)
                .append(':'), minutes).toString();

        m_delegate = new SimpleTimeZone(offset, zoneId);
    }

    private static StringBuilder twoDigits(final StringBuilder sb, final int n) {
        return sb.append((char) ('0' + (n / 10))).append((char) ('0' + (n % 10)));
    }

    @Override
    public int getOffset(final int era, final int year, final int month, final int day, final int dayOfWeek, final int milliseconds) {
        return m_delegate.getOffset(era, year, month, day, dayOfWeek, milliseconds);
    }

    @Override
    public void setRawOffset(final int offsetMillis) {
        throw new UnsupportedOperationException();
    }

    @Override
    public int getRawOffset() {
        return m_delegate.getRawOffset();
    }

    @Override
    public String getID() {
        return m_delegate.getID();
    }

    @Override
    public boolean useDaylightTime() {
        return m_delegate.useDaylightTime();
    }

    @Override
    public boolean inDaylightTime(final Date date) {
        return m_delegate.inDaylightTime(date);
    }

    @Override
    public String toString() {
        return "[GmtTimeZone id=\"" + getID() + "\",offset=" + getRawOffset() + ']';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        final GmtTimeZone that = (GmtTimeZone) o;
        return m_delegate.equals(that.m_delegate);
    }
    @Override
    public int hashCode() {
        return Objects.hash(getID(), m_delegate);
    }
}
