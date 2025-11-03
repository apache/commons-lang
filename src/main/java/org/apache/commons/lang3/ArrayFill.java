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
import java.util.function.IntFunction;

import org.apache.commons.lang3.function.FailableIntFunction;

/**
 * Fills and returns arrays in the fluent style.
 *
 * @since 3.14.0
 */
public final class ArrayFill {

    /**
     * Fills and returns the given array, assigning the given {@code boolean} value to each element of the array.
     *
     * @param a   the array to be filled (may be null).
     * @param val the value to be stored in all elements of the array.
     * @return the given array.
     * @see Arrays#fill(boolean[],boolean)
     * @since 3.18.0
     */
    public static boolean[] fill(final boolean[] a, final boolean val) {
        if (a != null) {
            Arrays.fill(a, val);
        }
        return a;
    }

    /**
     * Fills and returns the given array, assigning the given {@code byte} value to each element of the array.
     *
     * @param a   the array to be filled (may be null).
     * @param val the value to be stored in all elements of the array.
     * @return the given array.
     * @see Arrays#fill(byte[],byte)
     */
    public static byte[] fill(final byte[] a, final byte val) {
        if (a != null) {
            Arrays.fill(a, val);
        }
        return a;
    }

    /**
     * Fills and returns the given array, assigning the given {@code char} value to each element of the array.
     *
     * @param a   the array to be filled (may be null).
     * @param val the value to be stored in all elements of the array.
     * @return the given array.
     * @see Arrays#fill(char[],char)
     */
    public static char[] fill(final char[] a, final char val) {
        if (a != null) {
            Arrays.fill(a, val);
        }
        return a;
    }

    /**
     * Fills and returns the given array, assigning the given {@code double} value to each element of the array.
     *
     * @param a   the array to be filled (may be null).
     * @param val the value to be stored in all elements of the array.
     * @return the given array.
     * @see Arrays#fill(double[],double)
     */
    public static double[] fill(final double[] a, final double val) {
        if (a != null) {
            Arrays.fill(a, val);
        }
        return a;
    }

    /**
     * Fills and returns the given array, assigning the given {@code float} value to each element of the array.
     *
     * @param a   the array to be filled (may be null).
     * @param val the value to be stored in all elements of the array.
     * @return the given array.
     * @see Arrays#fill(float[],float)
     */
    public static float[] fill(final float[] a, final float val) {
        if (a != null) {
            Arrays.fill(a, val);
        }
        return a;
    }

    /**
     * Fills and returns the given array, assigning the given {@code int} value to each element of the array.
     *
     * @param a   the array to be filled (may be null).
     * @param val the value to be stored in all elements of the array.
     * @return the given array.
     * @see Arrays#fill(int[],int)
     */
    public static int[] fill(final int[] a, final int val) {
        if (a != null) {
            Arrays.fill(a, val);
        }
        return a;
    }

    /**
     * Fills and returns the given array, assigning the given {@code long} value to each element of the array.
     *
     * @param a   the array to be filled (may be null).
     * @param val the value to be stored in all elements of the array.
     * @return the given array.
     * @see Arrays#fill(long[],long)
     */
    public static long[] fill(final long[] a, final long val) {
        if (a != null) {
            Arrays.fill(a, val);
        }
        return a;
    }

    /**
     * Fills and returns the given array, assigning the given {@code short} value to each element of the array.
     *
     * @param a   the array to be filled (may be null).
     * @param val the value to be stored in all elements of the array.
     * @return the given array.
     * @see Arrays#fill(short[],short)
     */
    public static short[] fill(final short[] a, final short val) {
        if (a != null) {
            Arrays.fill(a, val);
        }
        return a;
    }

    /**
     * Fills and returns the given array, using the provided generator supplier to compute each element. Like
     * {@link Arrays#setAll(Object[], IntFunction)} with exception support.
     * <p>
     * If the generator supplier throws an exception, it is relayed to the caller and the array is left in an indeterminate
     * state.
     * </p>
     *
     * @param <T> type of elements of the array.
     * @param array array to be initialized.
     * @param generator a function accepting an index and producing the desired value for that position.
     * @return the input array
     * @param <E> The kind of thrown exception or error.
     * @throws E Thrown by the given {@code generator}.
     * @see Arrays#setAll(Object[], IntFunction)
     * @since 3.18.0
     */
    public static <T, E extends Throwable> T[] fill(final T[] array, final FailableIntFunction<? extends T, E> generator) throws E {
        if (array != null && generator != null) {
            for (int i = 0; i < array.length; i++) {
                array[i] = generator.apply(i);
            }
        }
        return array;
    }

    /**
     * Fills and returns the given array, assigning the given {@code T} value to each element of the array.
     *
     * @param <T> the array type.
     * @param a   the array to be filled (may be null).
     * @param val the value to be stored in all elements of the array.
     * @return the given array.
     * @see Arrays#fill(Object[],Object)
     */
    public static <T> T[] fill(final T[] a, final T val) {
        if (a != null) {
            Arrays.fill(a, val);
        }
        return a;
    }

    private ArrayFill() {
        // no instances
    }

}
