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

import java.util.Comparator;

/**
 * Specializes {@link Range} for {@link Number}s.
 * <p>
 * We only offer specializations for Integer, Long, and Double (like Java Streams).
 * </p>
 *
 * @param <N> The Number class.
 * @since 3.13.0
 */
public class NumberRange<N extends Number> extends Range<N> {

    private static final long serialVersionUID = 1L;

    /**
     * Creates an instance.
     *
     * @param number1 the first element, not null
     * @param number2 the second element, not null
     * @param comp the comparator to be used, null for natural ordering
     * @throws NullPointerException when element1 is null.
     * @throws NullPointerException when element2 is null.
     */
    public NumberRange(final N number1, final N number2, final Comparator<N> comp) {
        super(number1, number2, comp);
    }

}
