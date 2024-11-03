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
package org.apache.commons.lang3.stream;

import java.util.stream.IntStream;

/**
 * Factory for {@link IntStream}.
 * <p>
 * <small> Only a factory for now but could hold other functionality.</small>
 * </p>
 *
 * @since 3.13.0
 */
public class IntStreams {

    /**
     * Null-safe version of {@link IntStream#of(int[])}.
     *
     * @param values the elements of the new stream, may be {@code null}.
     * @return the new stream on {@code values} or {@link IntStream#empty()}.
     * @since 3.18.0
     */
    @SafeVarargs // Creating a stream from an array is safe
    public static IntStream of(final int... values) {
        return values == null ? IntStream.empty() : IntStream.of(values);
    }

    /**
     * Shorthand for {@code IntStream.range(0, i)}.
     *
     * @param endExclusive the exclusive upper bound.
     * @return a sequential {@link IntStream} for the range of {@code int} elements.
     */
    public static IntStream range(final int endExclusive) {
        return IntStream.range(0, endExclusive);
    }

    /**
     * Shorthand for {@code IntStream.rangeClosed(0, i)}.
     *
     * @param endInclusive the inclusive upper bound.
     * @return a sequential {@link IntStream} for the range of {@code int} elements.
     */
    public static IntStream rangeClosed(final int endInclusive) {
        return IntStream.rangeClosed(0, endInclusive);
    }

    /**
     * Make private in 4.0.
     *
     * @deprecated TODO Make private in 4.0.
     */
    @Deprecated
    public IntStreams() {
        // empty
    }
}
