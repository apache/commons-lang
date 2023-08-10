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

import java.util.Arrays;

/**
 * Fills and returns arrays in the fluent style.
 *
 * @since 3.14.0
 */
public final class ArrayFill {

    /**
     * Fills and returns the given array.
     *
     * @param a   the array to be filled.
     * @param val the value to be stored in all elements of the array.
     * @return the given array.
     * @see Arrays#fill(byte[],byte)
     */
    public static byte[] fill(final byte[] a, final byte val) {
        Arrays.fill(a, val);
        return a;
    }

    /**
     * Fills and returns the given array.
     *
     * @param a   the array to be filled.
     * @param val the value to be stored in all elements of the array.
     * @return the given array.
     * @see Arrays#fill(char[],char)
     */
    public static char[] fill(final char[] a, final char val) {
        Arrays.fill(a, val);
        return a;
    }

    /**
     * Fills and returns the given array.
     *
     * @param a   the array to be filled.
     * @param val the value to be stored in all elements of the array.
     * @return the given array.
     * @see Arrays#fill(double[],double)
     */
    public static double[] fill(final double[] a, final double val) {
        Arrays.fill(a, val);
        return a;
    }

    /**
     * Fills and returns the given array.
     *
     * @param a   the array to be filled.
     * @param val the value to be stored in all elements of the array.
     * @return the given array.
     * @see Arrays#fill(float[],float)
     */
    public static float[] fill(final float[] a, final float val) {
        Arrays.fill(a, val);
        return a;
    }

    /**
     * Fills and returns the given array.
     *
     * @param a   the array to be filled.
     * @param val the value to be stored in all elements of the array.
     * @return the given array.
     * @see Arrays#fill(int[],int)
     */
    public static int[] fill(final int[] a, final int val) {
        Arrays.fill(a, val);
        return a;
    }

    /**
     * Fills and returns the given array.
     *
     * @param a   the array to be filled.
     * @param val the value to be stored in all elements of the array.
     * @return the given array.
     * @see Arrays#fill(long[],long)
     */
    public static long[] fill(final long[] a, final long val) {
        Arrays.fill(a, val);
        return a;
    }

    /**
     * Fills and returns the given array.
     *
     * @param a   the array to be filled.
     * @param val the value to be stored in all elements of the array.
     * @return the given array.
     * @see Arrays#fill(short[],short)
     */
    public static short[] fill(final short[] a, final short val) {
        Arrays.fill(a, val);
        return a;
    }

    /**
     * Fills and returns the given array.
     *
     * @param <T> the array type.
     * @param a   the array to be filled.
     * @param val the value to be stored in all elements of the array.
     * @return the given array.
     * @see Arrays#fill(Object[],Object)
     */
    public static <T> T[] fill(final T[] a, final T val) {
        Arrays.fill(a, val);
        return a;
    }

    private ArrayFill() {
        // no instances
    }

}
