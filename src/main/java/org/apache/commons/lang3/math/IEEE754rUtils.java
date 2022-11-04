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
package org.apache.commons.lang3.math;

import java.util.Objects;

import org.apache.commons.lang3.Validate;

/**
 * Provides IEEE-754r variants of NumberUtils methods.
 *
 * <p>See: <a href="https://en.wikipedia.org/wiki/IEEE_754r">https://en.wikipedia.org/wiki/IEEE_754r</a></p>
 *
 * @since 2.4
 */
public class IEEE754rUtils {

     /**
     * Returns the minimum value in an array.
     *
     * @param array  an array, must not be null or empty
     * @return the minimum value in the array
     * @throws NullPointerException if {@code array} is {@code null}
     * @throws IllegalArgumentException if {@code array} is empty
     * @since 3.4 Changed signature from min(double[]) to min(double...)
     */
    public static double min(final double... array) {
        Objects.requireNonNull(array, "array");
        Validate.isTrue(array.length != 0, "Array cannot be empty.");

        // Finds and returns min
        double min = array[0];
        for (int i = 1; i < array.length; i++) {
            min = min(array[i], min);
        }

        return min;
    }

    /**
     * Returns the minimum value in an array.
     *
     * @param array  an array, must not be null or empty
     * @return the minimum value in the array
     * @throws NullPointerException if {@code array} is {@code null}
     * @throws IllegalArgumentException if {@code array} is empty
     * @since 3.4 Changed signature from min(float[]) to min(float...)
     */
    public static float min(final float... array) {
        Objects.requireNonNull(array, "array");
        Validate.isTrue(array.length != 0, "Array cannot be empty.");

        // Finds and returns min
        float min = array[0];
        for (int i = 1; i < array.length; i++) {
            min = min(array[i], min);
        }

        return min;
    }

    /**
     * Gets the minimum of three {@code double} values.
     *
     * <p>NaN is only returned if all numbers are NaN as per IEEE-754r.</p>
     *
     * @param a  value 1
     * @param b  value 2
     * @param c  value 3
     * @return  the smallest of the values
     */
    public static double min(final double a, final double b, final double c) {
        return min(min(a, b), c);
    }

    /**
     * Gets the minimum of two {@code double} values.
     *
     * <p>NaN is only returned if all numbers are NaN as per IEEE-754r.</p>
     *
     * @param a  value 1
     * @param b  value 2
     * @return  the smallest of the values
     */
    public static double min(final double a, final double b) {
        if (Double.isNaN(a)) {
            return b;
        }
        if (Double.isNaN(b)) {
            return a;
        }
        return Math.min(a, b);
    }

    /**
     * Gets the minimum of three {@code float} values.
     *
     * <p>NaN is only returned if all numbers are NaN as per IEEE-754r.</p>
     *
     * @param a  value 1
     * @param b  value 2
     * @param c  value 3
     * @return  the smallest of the values
     */
    public static float min(final float a, final float b, final float c) {
        return min(min(a, b), c);
    }

    /**
     * Gets the minimum of two {@code float} values.
     *
     * <p>NaN is only returned if all numbers are NaN as per IEEE-754r.</p>
     *
     * @param a  value 1
     * @param b  value 2
     * @return  the smallest of the values
     */
    public static float min(final float a, final float b) {
        if (Float.isNaN(a)) {
            return b;
        }
        if (Float.isNaN(b)) {
            return a;
        }
        return Math.min(a, b);
    }

    /**
     * Returns the maximum value in an array.
     *
     * @param array  an array, must not be null or empty
     * @return the minimum value in the array
     * @throws NullPointerException if {@code array} is {@code null}
     * @throws IllegalArgumentException if {@code array} is empty
     * @since 3.4 Changed signature from max(double[]) to max(double...)
     */
    public static double max(final double... array) {
        Objects.requireNonNull(array, "array");
        Validate.isTrue(array.length != 0, "Array cannot be empty.");

        // Finds and returns max
        double max = array[0];
        for (int j = 1; j < array.length; j++) {
            max = max(array[j], max);
        }

        return max;
    }

    /**
     * Returns the maximum value in an array.
     *
     * @param array  an array, must not be null or empty
     * @return the minimum value in the array
     * @throws NullPointerException if {@code array} is {@code null}
     * @throws IllegalArgumentException if {@code array} is empty
     * @since 3.4 Changed signature from max(float[]) to max(float...)
     */
    public static float max(final float... array) {
        Objects.requireNonNull(array, "array");
        Validate.isTrue(array.length != 0, "Array cannot be empty.");

        // Finds and returns max
        float max = array[0];
        for (int j = 1; j < array.length; j++) {
            max = max(array[j], max);
        }

        return max;
    }

    /**
     * Gets the maximum of three {@code double} values.
     *
     * <p>NaN is only returned if all numbers are NaN as per IEEE-754r.</p>
     *
     * @param a  value 1
     * @param b  value 2
     * @param c  value 3
     * @return  the largest of the values
     */
    public static double max(final double a, final double b, final double c) {
        return max(max(a, b), c);
    }

    /**
     * Gets the maximum of two {@code double} values.
     *
     * <p>NaN is only returned if all numbers are NaN as per IEEE-754r.</p>
     *
     * @param a  value 1
     * @param b  value 2
     * @return  the largest of the values
     */
    public static double max(final double a, final double b) {
        if (Double.isNaN(a)) {
            return b;
        }
        if (Double.isNaN(b)) {
            return a;
        }
        return Math.max(a, b);
    }

    /**
     * Gets the maximum of three {@code float} values.
     *
     * <p>NaN is only returned if all numbers are NaN as per IEEE-754r.</p>
     *
     * @param a  value 1
     * @param b  value 2
     * @param c  value 3
     * @return  the largest of the values
     */
    public static float max(final float a, final float b, final float c) {
        return max(max(a, b), c);
    }

    /**
     * Gets the maximum of two {@code float} values.
     *
     * <p>NaN is only returned if all numbers are NaN as per IEEE-754r.</p>
     *
     * @param a  value 1
     * @param b  value 2
     * @return  the largest of the values
     */
    public static float max(final float a, final float b) {
        if (Float.isNaN(a)) {
            return b;
        }
        if (Float.isNaN(b)) {
            return a;
        }
        return Math.max(a, b);
    }

}
