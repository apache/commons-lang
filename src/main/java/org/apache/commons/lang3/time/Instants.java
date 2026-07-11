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

import java.time.Instant;

/**
 * Works with {@link Instant}s.
 *
 * @since 3.21.0
 */
public class Instants {

    private static long toBound(final Instant instant, final long negBound, final long posBound) {
        return instant.getEpochSecond() < 0 ? negBound : posBound;
    }

    /**
     * Converts an Instant to milliseconds bound to a {@code long} without throwing {@link ArithmeticException}.
     * <ul>
     * <li>If the duration milliseconds are greater than {@link Long#MAX_VALUE}, then return {@link Long#MAX_VALUE}.</li>
     * <li>If the duration milliseconds are lesser than {@link Long#MIN_VALUE}, then return {@link Long#MIN_VALUE}.</li>
     * <li>If the instant is null, treat it as {@link Instant#EPOCH}.</li>
     * </ul>
     *
     * @param instant The instant to convert, not null.
     * @return long The given Instant in milliseconds.
     * @see Instant#toEpochMilli()
     * @see Long#MIN_VALUE
     * @see Long#MAX_VALUE
     */
    public static long toEpochMillis(final Instant instant) {
        final Instant instant2 = toInstant(instant);
        try {
            return instant2.toEpochMilli();
        } catch (final ArithmeticException e) {
            return toBound(instant2, Long.MIN_VALUE, Long.MAX_VALUE);
        }
    }

    /**
     * Returns the given non-null instant, or {@link Instant#EPOCH} if null.
     *
     * @param instant The instant to test, may be null.
     * @return The given non-null instant, or {@link Instant#EPOCH} if null.
     */
    public static Instant toInstant(final Instant instant) {
        return toInstant(instant, Instant.EPOCH);
    }

    /**
     * Returns the given non-null instant, or {@code defaultInstant} if instant is null.
     *
     * @param instant The instant to test, may be null.
     * @param defaultInstant The default instant to use if the given instant is null, may be null.
     * @return The given non-null instant, or {@code defaultInstant} if null.
     */
    public static Instant toInstant(final Instant instant, final Instant defaultInstant) {
        return instant != null ? instant : defaultInstant;
    }

    /**
     * Converts an Instant to milliseconds since that Instant bound to a {@code long} without throwing {@link ArithmeticException}.
     * <ul>
     * <li>If the duration milliseconds are greater than {@link Long#MAX_VALUE}, then return {@link Long#MAX_VALUE}.</li>
     * <li>If the duration milliseconds are lesser than {@link Long#MIN_VALUE}, then return {@link Long#MIN_VALUE}.</li>
     * <li>If the instant is null, treat it as {@link Instant#EPOCH}.</li>
     * </ul>
     *
     * @param instant The instant to convert, not null.
     * @return long The duration in milliseconds since the given Instant.
     */
    public static long toMillisSince(final Instant instant) {
        final Instant instant2 = toInstant(instant);
        try {
            return DurationUtils.since(instant2).toMillis();
        } catch (final ArithmeticException e) {
            return toBound(instant2, Long.MIN_VALUE, Long.MAX_VALUE);
        }
    }

    /**
     * No instances needed.
     */
    private Instants() {
        // empty.
    }
}
