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
package org.apache.commons.lang3;

import java.util.Arrays;
import java.util.Comparator;

/**
 * Sorts and returns arrays in the fluent style.
 *
 * TODO For 4.0, rename to ArraySort, since we cover the sort() method here, see also ArrayFill.
 * @since 3.12.0
 */
public class ArraySorter {

    /**
     * Sorts the given array into ascending order and returns it.
     *
     * @param array the array to sort (may be null).
     * @return the given array.
     * @see Arrays#sort(byte[])
     */
    public static byte[] sort(final byte[] array) {
        if (array != null) {
            Arrays.sort(array);
        }
        return array;
    }

    /**
     * Sorts the given array into ascending order and returns it.
     *
     * @param array the array to sort (may be null).
     * @return the given array.
     * @see Arrays#sort(char[])
     */
    public static char[] sort(final char[] array) {
        if (array != null) {
            Arrays.sort(array);
        }
        return array;
    }

    /**
     * Sorts the given array into ascending order and returns it.
     *
     * @param array the array to sort (may be null).
     * @return the given array.
     * @see Arrays#sort(double[])
     */
    public static double[] sort(final double[] array) {
        if (array != null) {
            Arrays.sort(array);
        }
        return array;
    }

    /**
     * Sorts the given array into ascending order and returns it.
     *
     * @param array the array to sort (may be null).
     * @return the given array.
     * @see Arrays#sort(float[])
     */
    public static float[] sort(final float[] array) {
        if (array != null) {
            Arrays.sort(array);
        }
        return array;
    }

    /**
     * Sorts the given array into ascending order and returns it.
     *
     * @param array the array to sort (may be null).
     * @return the given array.
     * @see Arrays#sort(int[])
     */
    public static int[] sort(final int[] array) {
        if (array != null) {
            Arrays.sort(array);
        }
        return array;
    }

    /**
     * Sorts the given array into ascending order and returns it.
     *
     * @param array the array to sort (may be null).
     * @return the given array.
     * @see Arrays#sort(long[])
     */
    public static long[] sort(final long[] array) {
        if (array != null) {
            Arrays.sort(array);
        }
        return array;
    }

    /**
     * Sorts the given array into ascending order and returns it.
     *
     * @param array the array to sort (may be null).
     * @return the given array.
     * @see Arrays#sort(short[])
     */
    public static short[] sort(final short[] array) {
        if (array != null) {
            Arrays.sort(array);
        }
        return array;
    }

    /**
     * Sorts the given array into ascending order and returns it.
     *
     * @param <T> the array type.
     * @param array the array to sort (may be null).
     * @return the given array.
     * @see Arrays#sort(Object[])
     */
    public static <T> T[] sort(final T[] array) {
        if (array != null) {
            Arrays.sort(array);
        }
        return array;
    }

    /**
     * Sorts the given array into ascending order and returns it.
     *
     * @param <T> the array type.
     * @param array the array to sort (may be null).
     * @param comparator the comparator to determine the order of the array. A {@code null} value uses the elements'
     *        {@link Comparable natural ordering}.
     * @return the given array.
     * @see Arrays#sort(Object[])
     */
    public static <T> T[] sort(final T[] array, final Comparator<? super T> comparator) {
        if (array != null) {
            Arrays.sort(array, comparator);
        }
        return array;
    }

    /**
     * Constructs a new instance.
     *
     * @deprecated Will be removed in 4.0.0.
     */
    @Deprecated
    public ArraySorter() {
        // empty
    }

}
