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

import java.util.TimeZone;

import org.apache.commons.lang3.ObjectUtils;

/**
 * Helps to deal with {@link java.util.TimeZone}s.
 *
 * @since 3.7
 */
public class TimeZones {

    /** Do not instantiate. */
    private TimeZones() {
    }

    /**
     * A public version of {@link java.util.TimeZone}'s package private {@code GMT_ID} field.
     */
    public static final String GMT_ID = "GMT";

    /**
     * The GMT time zone.
     *
     * @since 3.13.0
     */
    public static final TimeZone GMT = TimeZone.getTimeZone(GMT_ID);

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

}
