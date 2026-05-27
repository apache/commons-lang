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

    /**
     * Converts an Instant to milliseconds bound to a {@code long} without throwing {@link ArithmeticException}.
     * <ul>
     * <li>If the duration milliseconds are greater than {@link Long#MAX_VALUE}, then return {@link Long#MAX_VALUE}.</li>
     * <li>If the duration milliseconds are lesser than {@link Long#MIN_VALUE}, then return {@link Long#MIN_VALUE}.</li>
     * </ul>
     *
     * @param instant The instant to convert, not null.
     * @return long milliseconds.
     * @see Instant#toEpochMilli()
     * @see Long#MIN_VALUE
     * @see Long#MAX_VALUE
     */
    public static long toEpochMillis(final Instant instant) {
        try {
            return instant.toEpochMilli();
        } catch (final ArithmeticException e) {
            return instant.getEpochSecond() < 0 ? Long.MIN_VALUE : Long.MAX_VALUE;
        }
    }

    /**
     * No instances needed.
     */
    private Instants() {
        // empty.
    }
}
