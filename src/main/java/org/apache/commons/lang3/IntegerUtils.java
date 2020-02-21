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
 * <p>Operations on int primitives and Integer objects.</p>
 * <p>
 * <p>This class tries to handle {@code null} input gracefully.
 * An exception will not be thrown for a {@code null} input.
 * Each method documents its behaviour in more detail.</p>
 * <p>
 * <p>#ThreadSafe#</p>
 *
 * @since 2.0
 */
public class IntegerUtils {

    /**
     * <p>{@code IntegerUtils} instances should NOT be constructed in standard programming.
     * Instead, the class should be used as {@code IntegerUtils.isPositive(10);}.</p>
     * <p>
     * <p>This constructor is public to permit tools that require a JavaBean instance
     * to operate.</p>
     */
    public IntegerUtils() {
        super();
    }

    // int Integer methods
    //-----------------------------------------------------------------------

    /**
     * <p>Checks if an {@code Integer} value is positive,
     * handling {@code null} by returning {@code false}.</p>
     *
     * @param integer the integer to check, {@code null} returns {@code false}
     * @return {@code true} only if the input is non-null and greater than 0
     */
    public static boolean isPositive(final Integer integer) {
        return integer != null && integer > 0;
    }

    /**
     * <p>Checks if an {@code Integer} value is not positive,
     * handling {@code null} by returning {@code true}.</p>
     *
     * @param integer the integer to check, {@code null} returns {@code true}
     * @return {@code true} if the input is non-null or lesser than 0
     */
    public static boolean isNotPositive(final Integer integer) {
        return !isPositive(integer);
    }

    /**
     * <p>Checks if an {@code Integer} value is negative,
     * handling {@code null} by returning {@code false}.</p>
     *
     * @param integer the integer to check, {@code null} returns {@code false}
     * @return {@code true} only if the input is non-null and lesser than 0
     */
    public static boolean isNegative(final Integer integer) {
        return integer != null && integer < 0;
    }

    /**
     * <p>Checks if an {@code Integer} value is not negative,
     * handling {@code null} by returning {@code true}.</p>
     *
     * @param integer the integer to check, {@code null} returns {@code true}
     * @return {@code true} if the input is non-null or greater than 0
     */
    public static boolean isNotNegative(final Integer integer) {
        return !isNegative(integer);
    }

    /**
     * <p>Checks if an {@code Integer} value is zero,
     * handling {@code null} by returning {@code false}.</p>
     *
     * @param integer the integer to check, {@code null} returns {@code false}
     * @return {@code true} only if the input is non-null and equals 0
     */
    public static boolean isZero(final Integer integer) {
        return integer != null && integer == 0;
    }

    /**
     * <p>Checks if an {@code Integer} value is not zero,
     * handling {@code null} by returning {@code true}.</p>
     *
     * @param integer the integer to check, {@code null} returns {@code true}
     * @return {@code true} if the input is null or not equals 0
     */
    public static boolean isNotZero(final Integer integer) {
        return !isZero(integer);
    }
}
