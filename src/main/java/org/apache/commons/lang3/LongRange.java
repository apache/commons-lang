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

package org.apache.commons.lang3;

/**
 * Specializes {@link NumberRange} for {@link Long}s.
 *
 * <p>
 * This class is not designed to interoperate with other NumberRanges
 * </p>
 *
 * @since 3.13.0
 */
public final class LongRange extends NumberRange<Long> {

    private static final long serialVersionUID = 1L;

    /**
     * Creates a range with the specified minimum and maximum values (both inclusive).
     *
     * <p>
     * The range uses the natural ordering of the elements to determine where values lie in the range.
     * </p>
     *
     * <p>
     * The arguments may be passed in the order (min,max) or (max,min). The getMinimum and getMaximum methods will return the correct values.
     * </p>
     *
     * @param fromInclusive the first value that defines the edge of the range, inclusive.
     * @param toInclusive the second value that defines the edge of the range, inclusive.
     * @return the range object, not null.
     */
    public static LongRange of(final long fromInclusive, final long toInclusive) {
        return of(Long.valueOf(fromInclusive), Long.valueOf(toInclusive));
    }

    /**
     * Creates a range with the specified minimum and maximum values (both inclusive).
     *
     * <p>
     * The range uses the natural ordering of the elements to determine where values lie in the range.
     * </p>
     *
     * <p>
     * The arguments may be passed in the order (min,max) or (max,min). The getMinimum and getMaximum methods will return the correct values.
     * </p>
     *
     * @param fromInclusive the first value that defines the edge of the range, inclusive.
     * @param toInclusive the second value that defines the edge of the range, inclusive.
     * @return the range object, not null.
     * @throws IllegalArgumentException if either element is null.
     */
    public static LongRange of(final Long fromInclusive, final Long toInclusive) {
        return new LongRange(fromInclusive, toInclusive);
    }

    /**
     * Creates an instance.
     *
     * @param number1 the first element, not null
     * @param number2 the second element, not null
     * @throws NullPointerException when element1 is null.
     * @throws NullPointerException when element2 is null.
     */
    private LongRange(final Long number1, final Long number2) {
        super(number1, number2, null);
    }

}
