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
import java.util.Map;
import java.util.TimeZone;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.lang3.JavaVersion;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.SystemUtils;

/**
 * Helps dealing with {@link java.util.TimeZone}s.
 *
 * @since 3.7
 */
public class TimeZones {

    private static final boolean AT_LEAST_JAVA_25 = SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_25);

    private static final Map<String, TimeZone> SHORT_IDS = new ConcurrentHashMap<>();

    /**
     * A public version of {@link java.util.TimeZone}'s package private {@code GMT_ID} field.
     */
    public static final String GMT_ID = "GMT";

    /**
     * The GMT time zone.
     *
     * @since 3.13.0
     */
    public static final TimeZone GMT = TimeZones.getTimeZone(GMT_ID);

    /**
     * Delegates to {@link TimeZone#getTimeZone(String)} with special behavior on Java 25.
     * <p>
     * On Java 25, this methods delegates once to {@link TimeZone#getTimeZone(String)} for each short ID to avoid logging deprecation warning to system error.
     * </p>
     * <p>
     * On Java 25, this message is of the form:
     * </p>
     *
     * <pre>
     * WARNING: Use of the three-letter time zone ID "the-short-id" is deprecated and it will be removed in a future release
     * </pre>
     *
     * @param id Same as {@link TimeZone#getTimeZone(String)}.
     * @return Same as {@link TimeZone#getTimeZone(String)}.
     * @since 3.20.0
     */
    public static TimeZone getTimeZone(final String id) {
        return AT_LEAST_JAVA_25 && ZoneId.SHORT_IDS.containsKey(id) ? SHORT_IDS.computeIfAbsent(id, TimeZone::getTimeZone) : TimeZone.getTimeZone(id);
    }

    /**
     * Returns the given TimeZone if non-{@code null}, otherwise {@link TimeZone#getDefault()}.
     *
     * @param timeZone a locale or {@code null}.
     * @return the given locale if non-{@code null}, otherwise {@link TimeZone#getDefault()}.
     * @since 3.13.0
     */
    public static TimeZone toTimeZone(final TimeZone timeZone) {
        return ObjectUtils.getIfNull(timeZone, TimeZone::getDefault);
    }

    /** Do not instantiate. */
    private TimeZones() {
    }
}
