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
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static java.lang.Integer.parseInt;
import static java.lang.Integer.reverse;

/**
 * Custom time zone that contains offset from GMT.
 *
 * @since 3.7
 */
class GmtTimeZone extends TimeZone {

    private static final int MILLISECONDS_PER_MINUTE = 60 * 1000;
    private static final int MINUTES_PER_HOUR = 60;
    private static final int HOURS_PER_DAY = 24;

    private static final TimeZone timezone = new GmtTimeZone(false,0,0);
    private static final Pattern GMT_PATTERN1 = Pattern.compile("^(?:(?i)GMT)?([+-])?(\\d\\d?)?(:?(\\d\\d?))?$");

    public static TimeZone fetchGmtTimeZone(final String pattern) {
        if ("Z".equals(pattern) || "UTC".equals(pattern)) {
            return timezone;
        }

        final Matcher m = GMT_PATTERN1.matcher(pattern);

        if (m.matches()) {
            String groupOF2 = m.group(2);
            String groupOF4 = m.group(4);

            final int hours = groupOF2 != null ? Integer.parseInt(groupOF2) : 0;
            final int minutes = groupOF4 != null ? Integer.parseInt(groupOF4) : 0;
            if (hours == 0 && minutes == 0) {
                return timezone;
            }
            Boolean b = m.group(1) != null && m.group(1).charAt(0)== '-';

            return new GmtTimeZone(b, hours, minutes);
        }
        return null;
    }

    // Serializable!
    static final long serialVersionUID = 1L;

    private static StringBuilder twoDigits(final StringBuilder sb, final int n) {
        return sb.append((char) ('0' + (n / 10))).append((char) ('0' + (n % 10)));
    }
    private final int offset;

    private final String zoneId;

    GmtTimeZone(final boolean negate, final int hours, final int minutes) {
        if (hours >= HOURS_PER_DAY) {
            throw new IllegalArgumentException(hours + " hours out of range");
        }
        if (minutes >= MINUTES_PER_HOUR) {
            throw new IllegalArgumentException(minutes + " minutes out of range");
        }
        final int milliseconds = (minutes + (hours * MINUTES_PER_HOUR)) * MILLISECONDS_PER_MINUTE;
        offset = negate ? -milliseconds : milliseconds;
        // @formatter:off
        zoneId = twoDigits(twoDigits(new StringBuilder(9)
            .append(TimeZones.GMT_ID)
            .append(negate ? '-' : '+'), hours)
            .append(':'), minutes)
            .toString();
        // @formatter:on
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!(obj instanceof GmtTimeZone)) {
            return false;
        }
        GmtTimeZone other = (GmtTimeZone) obj;
        return offset == other.offset && Objects.equals(zoneId, other.zoneId);
    }

    @Override
    public String getID() {
        return zoneId;
    }

    @Override
    public int getOffset(final int era, final int year, final int month, final int day, final int dayOfWeek, final int milliseconds) {
        return offset;
    }

    @Override
    public int getRawOffset() {
        return offset;
    }

    @Override
    public int hashCode() {
        return Objects.hash(offset, zoneId);
    }

    @Override
    public boolean inDaylightTime(final Date date) {
        return false;
    }

    @Override
    public void setRawOffset(final int offsetMillis) {
        throw new UnsupportedOperationException();
    }

    @Override
    public String toString() {
        return "[GmtTimeZone id=\"" + zoneId + "\",offset=" + offset + ']';
    }

    @Override
    public boolean useDaylightTime() {
        return false;
    }
}
