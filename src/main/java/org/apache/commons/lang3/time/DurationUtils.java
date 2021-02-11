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

import java.time.Duration;
import java.util.Objects;

import org.apache.commons.lang3.Range;
import org.apache.commons.lang3.math.NumberUtils;

/**
 * Utilities for {@link Duration}.
 *
 * @since 3.12.0
 */
public class DurationUtils {

    /**
     * An Integer Range that accepts Longs.
     */
    static final Range<Long> LONG_TO_INT_RANGE = Range.between(
        NumberUtils.LONG_INT_MIN_VALUE,
        NumberUtils.LONG_INT_MAX_VALUE);

    /**
     * Converts a Duration to milliseconds bound to an int (instead of a long).
     * <p>
     * Handy for low-level APIs that take millisecond timeouts in ints rather than longs.
     * </p>
     * <ul>
     * <li>If the duration milliseconds are greater than {@link Integer#MAX_VALUE}, then return
     * {@link Integer#MAX_VALUE}.</li>
     * <li>If the duration milliseconds are lesser than {@link Integer#MIN_VALUE}, then return
     * {@link Integer#MIN_VALUE}.</li>
     * </ul>
     *
     * @param duration The duration to convert, not null.
     * @return int milliseconds.
     */
    public static int toMillisInt(final Duration duration) {
        Objects.requireNonNull(duration, "duration");
        // intValue() does not do a narrowing conversion here
        return DurationUtils.LONG_TO_INT_RANGE.fit(Long.valueOf(duration.toMillis())).intValue();
    }
}
