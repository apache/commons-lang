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

import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.security.SecureRandom;
import java.util.Arrays;
import java.util.BitSet;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;
import java.util.function.Function;
import java.util.function.IntFunction;
import java.util.function.Supplier;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.apache.commons.lang3.function.FailableFunction;
import org.apache.commons.lang3.math.NumberUtils;
import org.apache.commons.lang3.mutable.MutableInt;
import org.apache.commons.lang3.stream.IntStreams;
import org.apache.commons.lang3.stream.Streams;

/**
 * Operations on arrays, primitive arrays (like {@code int[]}) and
 * primitive wrapper arrays (like {@code Integer[]}).
 * <p>
 * This class tries to handle {@code null} input gracefully.
 * An exception will not be thrown for a {@code null}
 * array input. However, an Object array that contains a {@code null}
 * element may throw an exception. Each method documents its behavior.
 * </p>
 * <p>
 * #ThreadSafe#
 * </p>
 * @since 2.0
 */
public class ArrayUtils {

    /**
     * An empty immutable {@code boolean} array.
     */
    public static final boolean[] EMPTY_BOOLEAN_ARRAY = {};

    /**
     * An empty immutable {@link Boolean} array.
     */
    public static final Boolean[] EMPTY_BOOLEAN_OBJECT_ARRAY = {};

    /**
     * An empty immutable {@code byte} array.
     */
    public static final byte[] EMPTY_BYTE_ARRAY = {};

    /**
     * An empty immutable {@link Byte} array.
     */
    public static final Byte[] EMPTY_BYTE_OBJECT_ARRAY = {};

    /**
     * An empty immutable {@code char} array.
     */
    public static final char[] EMPTY_CHAR_ARRAY = {};

    /**
     * An empty immutable {@link Character} array.
     */
    public static final Character[] EMPTY_CHARACTER_OBJECT_ARRAY = {};

    /**
     * An empty immutable {@link Class} array.
     */
    public static final Class<?>[] EMPTY_CLASS_ARRAY = {};

    /**
     * An empty immutable {@code double} array.
     */
    public static final double[] EMPTY_DOUBLE_ARRAY = {};

    /**
     * An empty immutable {@link Double} array.
     */
    public static final Double[] EMPTY_DOUBLE_OBJECT_ARRAY = {};

    /**
     * An empty immutable {@link Field} array.
     *
     * @since 3.10
     */
    public static final Field[] EMPTY_FIELD_ARRAY = {};

    /**
     * An empty immutable {@code float} array.
     */
    public static final float[] EMPTY_FLOAT_ARRAY = {};

    /**
     * An empty immutable {@link Float} array.
     */
    public static final Float[] EMPTY_FLOAT_OBJECT_ARRAY = {};

    /**
     * An empty immutable {@code int} array.
     */
    public static final int[] EMPTY_INT_ARRAY = {};

    /**
     * An empty immutable {@link Integer} array.
     */
    public static final Integer[] EMPTY_INTEGER_OBJECT_ARRAY = {};

    /**
     * An empty immutable {@code long} array.
     */
    public static final long[] EMPTY_LONG_ARRAY = {};

    /**
     * An empty immutable {@link Long} array.
     */
    public static final Long[] EMPTY_LONG_OBJECT_ARRAY = {};

    /**
     * An empty immutable {@link Method} array.
     *
     * @since 3.10
     */
    public static final Method[] EMPTY_METHOD_ARRAY = {};

    /**
     * An empty immutable {@link Object} array.
     */
    public static final Object[] EMPTY_OBJECT_ARRAY = {};

    /**
     * An empty immutable {@code short} array.
     */
    public static final short[] EMPTY_SHORT_ARRAY = {};

    /**
     * An empty immutable {@link Short} array.
     */
    public static final Short[] EMPTY_SHORT_OBJECT_ARRAY = {};

    /**
     * An empty immutable {@link String} array.
     */
    public static final String[] EMPTY_STRING_ARRAY = {};

    /**
     * An empty immutable {@link Throwable} array.
     *
     * @since 3.10
     */
    public static final Throwable[] EMPTY_THROWABLE_ARRAY = {};

    /**
     * An empty immutable {@link Type} array.
     *
     * @since 3.10
     */
    public static final Type[] EMPTY_TYPE_ARRAY = {};

    /**
     * The index value when an element is not found in a list or array: {@code -1}.
     * This value is returned by methods in this class and can also be used in comparisons with values returned by
     * various method from {@link java.util.List}.
     */
    public static final int INDEX_NOT_FOUND = -1;

    /**
     * Copies the given array and adds the given element at the end of the new array.
     * <p>
     * The new array contains the same elements of the input
     * array plus the given element in the last position. The component type of
     * the new array is the same as that of the input array.
     * </p>
     * <p>
     * If the input array is {@code null}, a new one element array is returned
     * whose component type is the same as the element.
     * </p>
     * <pre>
     * ArrayUtils.add(null, true)          = [true]
     * ArrayUtils.add([true], false)       = [true, false]
     * ArrayUtils.add([true, false], true) = [true, false, true]
     * </pre>
     *
     * @param array  the array to copy and add the element to, may be {@code null}
     * @param element  the object to add at the last index of the new array
     * @return A new array containing the existing elements plus the new element
     * @since 2.1
     */
    public static boolean[] add(final boolean[] array, final boolean element) {
        final boolean[] newArray = (boolean[]) copyArrayGrow1(array, Boolean.TYPE);
        newArray[newArray.length - 1] = element;
        return newArray;
    }

    /**
     * Inserts the specified element at the specified position in the array.
     * Shifts the element currently at that position (if any) and any subsequent
     * elements to the right (adds one to their indices).
     * <p>
     * This method returns a new array with the same elements of the input
     * array plus the given element on the specified position. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <p>
     * If the input array is {@code null}, a new one element array is returned
     * whose component type is the same as the element.
     * </p>
     * <pre>
     * ArrayUtils.add(null, 0, true)          = [true]
     * ArrayUtils.add([true], 0, false)       = [false, true]
     * ArrayUtils.add([false], 1, true)       = [false, true]
     * ArrayUtils.add([true, false], 1, true) = [true, true, false]
     * </pre>
     *
     * @param array  the array to add the element to, may be {@code null}
     * @param index  the position of the new object
     * @param element  the object to add
     * @return A new array containing the existing elements and the new element
     * @throws IndexOutOfBoundsException if the index is out of range (index &lt; 0 || index &gt; array.length).
     * @deprecated this method has been superseded by {@link #insert(int, boolean[], boolean...)} and
     * may be removed in a future release. Please note the handling of {@code null} input arrays differs
     * in the new method: inserting {@code X} into a {@code null} array results in {@code null} not {@code X}.
     */
    @Deprecated
    public static boolean[] add(final boolean[] array, final int index, final boolean element) {
        return (boolean[]) add(array, index, Boolean.valueOf(element), Boolean.TYPE);
    }

    /**
     * Copies the given array and adds the given element at the end of the new array.
     * <p>
     * The new array contains the same elements of the input
     * array plus the given element in the last position. The component type of
     * the new array is the same as that of the input array.
     * </p>
     * <p>
     * If the input array is {@code null}, a new one element array is returned
     * whose component type is the same as the element.
     * </p>
     * <pre>
     * ArrayUtils.add(null, 0)   = [0]
     * ArrayUtils.add([1], 0)    = [1, 0]
     * ArrayUtils.add([1, 0], 1) = [1, 0, 1]
     * </pre>
     *
     * @param array  the array to copy and add the element to, may be {@code null}
     * @param element  the object to add at the last index of the new array
     * @return A new array containing the existing elements plus the new element
     * @since 2.1
     */
    public static byte[] add(final byte[] array, final byte element) {
        final byte[] newArray = (byte[]) copyArrayGrow1(array, Byte.TYPE);
        newArray[newArray.length - 1] = element;
        return newArray;
    }

    /**
     * Inserts the specified element at the specified position in the array.
     * Shifts the element currently at that position (if any) and any subsequent
     * elements to the right (adds one to their indices).
     * <p>
     * This method returns a new array with the same elements of the input
     * array plus the given element on the specified position. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <p>
     * If the input array is {@code null}, a new one element array is returned
     * whose component type is the same as the element.
     * </p>
     * <pre>
     * ArrayUtils.add([1], 0, 2)         = [2, 1]
     * ArrayUtils.add([2, 6], 2, 3)      = [2, 6, 3]
     * ArrayUtils.add([2, 6], 0, 1)      = [1, 2, 6]
     * ArrayUtils.add([2, 6, 3], 2, 1)   = [2, 6, 1, 3]
     * </pre>
     *
     * @param array  the array to add the element to, may be {@code null}
     * @param index  the position of the new object
     * @param element  the object to add
     * @return A new array containing the existing elements and the new element
     * @throws IndexOutOfBoundsException if the index is out of range
     * (index &lt; 0 || index &gt; array.length).
     * @deprecated this method has been superseded by {@link #insert(int, byte[], byte...)} and
     * may be removed in a future release. Please note the handling of {@code null} input arrays differs
     * in the new method: inserting {@code X} into a {@code null} array results in {@code null} not {@code X}.
     */
    @Deprecated
    public static byte[] add(final byte[] array, final int index, final byte element) {
        return (byte[]) add(array, index, Byte.valueOf(element), Byte.TYPE);
    }

    /**
     * Copies the given array and adds the given element at the end of the new array.
     * <p>
     * The new array contains the same elements of the input
     * array plus the given element in the last position. The component type of
     * the new array is the same as that of the input array.
     * </p>
     * <p>
     * If the input array is {@code null}, a new one element array is returned
     * whose component type is the same as the element.
     * </p>
     * <pre>
     * ArrayUtils.add(null, '0')       = ['0']
     * ArrayUtils.add(['1'], '0')      = ['1', '0']
     * ArrayUtils.add(['1', '0'], '1') = ['1', '0', '1']
     * </pre>
     *
     * @param array  the array to copy and add the element to, may be {@code null}
     * @param element  the object to add at the last index of the new array
     * @return A new array containing the existing elements plus the new element
     * @since 2.1
     */
    public static char[] add(final char[] array, final char element) {
        final char[] newArray = (char[]) copyArrayGrow1(array, Character.TYPE);
        newArray[newArray.length - 1] = element;
        return newArray;
    }

    /**
     * Inserts the specified element at the specified position in the array.
     * Shifts the element currently at that position (if any) and any subsequent
     * elements to the right (adds one to their indices).
     * <p>
     * This method returns a new array with the same elements of the input
     * array plus the given element on the specified position. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <p>
     * If the input array is {@code null}, a new one element array is returned
     * whose component type is the same as the element.
     * </p>
     * <pre>
     * ArrayUtils.add(null, 0, 'a')            = ['a']
     * ArrayUtils.add(['a'], 0, 'b')           = ['b', 'a']
     * ArrayUtils.add(['a', 'b'], 0, 'c')      = ['c', 'a', 'b']
     * ArrayUtils.add(['a', 'b'], 1, 'k')      = ['a', 'k', 'b']
     * ArrayUtils.add(['a', 'b', 'c'], 1, 't') = ['a', 't', 'b', 'c']
     * </pre>
     *
     * @param array  the array to add the element to, may be {@code null}
     * @param index  the position of the new object
     * @param element  the object to add
     * @return A new array containing the existing elements and the new element
     * @throws IndexOutOfBoundsException if the index is out of range
     * (index &lt; 0 || index &gt; array.length).
     * @deprecated this method has been superseded by {@link #insert(int, char[], char...)} and
     * may be removed in a future release. Please note the handling of {@code null} input arrays differs
     * in the new method: inserting {@code X} into a {@code null} array results in {@code null} not {@code X}.
     */
    @Deprecated
    public static char[] add(final char[] array, final int index, final char element) {
        return (char[]) add(array, index, Character.valueOf(element), Character.TYPE);
    }

    /**
     * Copies the given array and adds the given element at the end of the new array.
     *
     * <p>
     * The new array contains the same elements of the input
     * array plus the given element in the last position. The component type of
     * the new array is the same as that of the input array.
     * </p>
     * <p>
     * If the input array is {@code null}, a new one element array is returned
     * whose component type is the same as the element.
     * </p>
     * <pre>
     * ArrayUtils.add(null, 0)   = [0]
     * ArrayUtils.add([1], 0)    = [1, 0]
     * ArrayUtils.add([1, 0], 1) = [1, 0, 1]
     * </pre>
     *
     * @param array  the array to copy and add the element to, may be {@code null}
     * @param element  the object to add at the last index of the new array
     * @return A new array containing the existing elements plus the new element
     * @since 2.1
     */
    public static double[] add(final double[] array, final double element) {
        final double[] newArray = (double[]) copyArrayGrow1(array, Double.TYPE);
        newArray[newArray.length - 1] = element;
        return newArray;
    }

    /**
     * Inserts the specified element at the specified position in the array.
     * Shifts the element currently at that position (if any) and any subsequent
     * elements to the right (adds one to their indices).
     * <p>
     * This method returns a new array with the same elements of the input
     * array plus the given element on the specified position. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <p>
     * If the input array is {@code null}, a new one element array is returned
     * whose component type is the same as the element.
     * </p>
     * <pre>
     * ArrayUtils.add([1.1], 0, 2.2)              = [2.2, 1.1]
     * ArrayUtils.add([2.3, 6.4], 2, 10.5)        = [2.3, 6.4, 10.5]
     * ArrayUtils.add([2.6, 6.7], 0, -4.8)        = [-4.8, 2.6, 6.7]
     * ArrayUtils.add([2.9, 6.0, 0.3], 2, 1.0)    = [2.9, 6.0, 1.0, 0.3]
     * </pre>
     *
     * @param array  the array to add the element to, may be {@code null}
     * @param index  the position of the new object
     * @param element  the object to add
     * @return A new array containing the existing elements and the new element
     * @throws IndexOutOfBoundsException if the index is out of range
     * (index &lt; 0 || index &gt; array.length).
     * @deprecated this method has been superseded by {@link #insert(int, double[], double...)} and
     * may be removed in a future release. Please note the handling of {@code null} input arrays differs
     * in the new method: inserting {@code X} into a {@code null} array results in {@code null} not {@code X}.
     */
    @Deprecated
    public static double[] add(final double[] array, final int index, final double element) {
        return (double[]) add(array, index, Double.valueOf(element), Double.TYPE);
    }

    /**
     * Copies the given array and adds the given element at the end of the new array.
     * <p>
     * The new array contains the same elements of the input
     * array plus the given element in the last position. The component type of
     * the new array is the same as that of the input array.
     * </p>
     * <p>
     * If the input array is {@code null}, a new one element array is returned
     * whose component type is the same as the element.
     * </p>
     * <pre>
     * ArrayUtils.add(null, 0)   = [0]
     * ArrayUtils.add([1], 0)    = [1, 0]
     * ArrayUtils.add([1, 0], 1) = [1, 0, 1]
     * </pre>
     *
     * @param array  the array to copy and add the element to, may be {@code null}
     * @param element  the object to add at the last index of the new array
     * @return A new array containing the existing elements plus the new element
     * @since 2.1
     */
    public static float[] add(final float[] array, final float element) {
        final float[] newArray = (float[]) copyArrayGrow1(array, Float.TYPE);
        newArray[newArray.length - 1] = element;
        return newArray;
    }

    /**
     * Inserts the specified element at the specified position in the array.
     * Shifts the element currently at that position (if any) and any subsequent
     * elements to the right (adds one to their indices).
     * <p>
     * This method returns a new array with the same elements of the input
     * array plus the given element on the specified position. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <p>
     * If the input array is {@code null}, a new one element array is returned
     * whose component type is the same as the element.
     * </p>
     * <pre>
     * ArrayUtils.add([1.1f], 0, 2.2f)               = [2.2f, 1.1f]
     * ArrayUtils.add([2.3f, 6.4f], 2, 10.5f)        = [2.3f, 6.4f, 10.5f]
     * ArrayUtils.add([2.6f, 6.7f], 0, -4.8f)        = [-4.8f, 2.6f, 6.7f]
     * ArrayUtils.add([2.9f, 6.0f, 0.3f], 2, 1.0f)   = [2.9f, 6.0f, 1.0f, 0.3f]
     * </pre>
     *
     * @param array  the array to add the element to, may be {@code null}
     * @param index  the position of the new object
     * @param element  the object to add
     * @return A new array containing the existing elements and the new element
     * @throws IndexOutOfBoundsException if the index is out of range
     * (index &lt; 0 || index &gt; array.length).
     * @deprecated this method has been superseded by {@link #insert(int, float[], float...)} and
     * may be removed in a future release. Please note the handling of {@code null} input arrays differs
     * in the new method: inserting {@code X} into a {@code null} array results in {@code null} not {@code X}.
     */
    @Deprecated
    public static float[] add(final float[] array, final int index, final float element) {
        return (float[]) add(array, index, Float.valueOf(element), Float.TYPE);
    }

    /**
     * Copies the given array and adds the given element at the end of the new array.
     * <p>
     * The new array contains the same elements of the input
     * array plus the given element in the last position. The component type of
     * the new array is the same as that of the input array.
     * </p>
     * <p>
     * If the input array is {@code null}, a new one element array is returned
     * whose component type is the same as the element.
     * </p>
     * <pre>
     * ArrayUtils.add(null, 0)   = [0]
     * ArrayUtils.add([1], 0)    = [1, 0]
     * ArrayUtils.add([1, 0], 1) = [1, 0, 1]
     * </pre>
     *
     * @param array  the array to copy and add the element to, may be {@code null}
     * @param element  the object to add at the last index of the new array
     * @return A new array containing the existing elements plus the new element
     * @since 2.1
     */
    public static int[] add(final int[] array, final int element) {
        final int[] newArray = (int[]) copyArrayGrow1(array, Integer.TYPE);
        newArray[newArray.length - 1] = element;
        return newArray;
    }

    /**
     * Inserts the specified element at the specified position in the array.
     * Shifts the element currently at that position (if any) and any subsequent
     * elements to the right (adds one to their indices).
     * <p>
     * This method returns a new array with the same elements of the input
     * array plus the given element on the specified position. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <p>
     * If the input array is {@code null}, a new one element array is returned
     * whose component type is the same as the element.
     * </p>
     * <pre>
     * ArrayUtils.add([1], 0, 2)         = [2, 1]
     * ArrayUtils.add([2, 6], 2, 10)     = [2, 6, 10]
     * ArrayUtils.add([2, 6], 0, -4)     = [-4, 2, 6]
     * ArrayUtils.add([2, 6, 3], 2, 1)   = [2, 6, 1, 3]
     * </pre>
     *
     * @param array  the array to add the element to, may be {@code null}
     * @param index  the position of the new object
     * @param element  the object to add
     * @return A new array containing the existing elements and the new element
     * @throws IndexOutOfBoundsException if the index is out of range
     * (index &lt; 0 || index &gt; array.length).
     * @deprecated this method has been superseded by {@link #insert(int, int[], int...)} and
     * may be removed in a future release. Please note the handling of {@code null} input arrays differs
     * in the new method: inserting {@code X} into a {@code null} array results in {@code null} not {@code X}.
     */
    @Deprecated
    public static int[] add(final int[] array, final int index, final int element) {
        return (int[]) add(array, index, Integer.valueOf(element), Integer.TYPE);
    }

    /**
     * Inserts the specified element at the specified position in the array.
     * Shifts the element currently at that position (if any) and any subsequent
     * elements to the right (adds one to their indices).
     * <p>
     * This method returns a new array with the same elements of the input
     * array plus the given element on the specified position. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <p>
     * If the input array is {@code null}, a new one element array is returned
     * whose component type is the same as the element.
     * </p>
     * <pre>
     * ArrayUtils.add([1L], 0, 2L)           = [2L, 1L]
     * ArrayUtils.add([2L, 6L], 2, 10L)      = [2L, 6L, 10L]
     * ArrayUtils.add([2L, 6L], 0, -4L)      = [-4L, 2L, 6L]
     * ArrayUtils.add([2L, 6L, 3L], 2, 1L)   = [2L, 6L, 1L, 3L]
     * </pre>
     *
     * @param array  the array to add the element to, may be {@code null}
     * @param index  the position of the new object
     * @param element  the object to add
     * @return A new array containing the existing elements and the new element
     * @throws IndexOutOfBoundsException if the index is out of range
     * (index &lt; 0 || index &gt; array.length).
     * @deprecated this method has been superseded by {@link #insert(int, long[], long...)} and
     * may be removed in a future release. Please note the handling of {@code null} input arrays differs
     * in the new method: inserting {@code X} into a {@code null} array results in {@code null} not {@code X}.
     */
    @Deprecated
    public static long[] add(final long[] array, final int index, final long element) {
        return (long[]) add(array, index, Long.valueOf(element), Long.TYPE);
    }

    /**
     * Copies the given array and adds the given element at the end of the new array.
     * <p>
     * The new array contains the same elements of the input
     * array plus the given element in the last position. The component type of
     * the new array is the same as that of the input array.
     * </p>
     * <p>
     * If the input array is {@code null}, a new one element array is returned
     * whose component type is the same as the element.
     * </p>
     * <pre>
     * ArrayUtils.add(null, 0)   = [0]
     * ArrayUtils.add([1], 0)    = [1, 0]
     * ArrayUtils.add([1, 0], 1) = [1, 0, 1]
     * </pre>
     *
     * @param array  the array to copy and add the element to, may be {@code null}
     * @param element  the object to add at the last index of the new array
     * @return A new array containing the existing elements plus the new element
     * @since 2.1
     */
    public static long[] add(final long[] array, final long element) {
        final long[] newArray = (long[]) copyArrayGrow1(array, Long.TYPE);
        newArray[newArray.length - 1] = element;
        return newArray;
    }

    /**
     * Underlying implementation of add(array, index, element) methods.
     * The last parameter is the class, which may not equal element.getClass
     * for primitives.
     *
     * @param array  the array to add the element to, may be {@code null}
     * @param index  the position of the new object
     * @param element  the object to add
     * @param clazz the type of the element being added
     * @return A new array containing the existing elements and the new element
     */
    private static Object add(final Object array, final int index, final Object element, final Class<?> clazz) {
        if (array == null) {
            if (index != 0) {
                throw new IndexOutOfBoundsException("Index: " + index + ", Length: 0");
            }
            final Object joinedArray = Array.newInstance(clazz, 1);
            Array.set(joinedArray, 0, element);
            return joinedArray;
        }
        final int length = Array.getLength(array);
        if (index > length || index < 0) {
            throw new IndexOutOfBoundsException("Index: " + index + ", Length: " + length);
        }
        final Object result = arraycopy(array, 0, 0, index, () -> Array.newInstance(clazz, length + 1));
        Array.set(result, index, element);
        if (index < length) {
            System.arraycopy(array, index, result, index + 1, length - index);
        }
        return result;
    }

    /**
     * Inserts the specified element at the specified position in the array.
     * Shifts the element currently at that position (if any) and any subsequent
     * elements to the right (adds one to their indices).
     * <p>
     * This method returns a new array with the same elements of the input
     * array plus the given element on the specified position. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <p>
     * If the input array is {@code null}, a new one element array is returned
     * whose component type is the same as the element.
     * </p>
     * <pre>
     * ArrayUtils.add([1], 0, 2)         = [2, 1]
     * ArrayUtils.add([2, 6], 2, 10)     = [2, 6, 10]
     * ArrayUtils.add([2, 6], 0, -4)     = [-4, 2, 6]
     * ArrayUtils.add([2, 6, 3], 2, 1)   = [2, 6, 1, 3]
     * </pre>
     *
     * @param array  the array to add the element to, may be {@code null}
     * @param index  the position of the new object
     * @param element  the object to add
     * @return A new array containing the existing elements and the new element
     * @throws IndexOutOfBoundsException if the index is out of range
     * (index &lt; 0 || index &gt; array.length).
     * @deprecated this method has been superseded by {@link #insert(int, short[], short...)} and
     * may be removed in a future release. Please note the handling of {@code null} input arrays differs
     * in the new method: inserting {@code X} into a {@code null} array results in {@code null} not {@code X}.
     */
    @Deprecated
    public static short[] add(final short[] array, final int index, final short element) {
        return (short[]) add(array, index, Short.valueOf(element), Short.TYPE);
    }

    /**
     * Copies the given array and adds the given element at the end of the new array.
     * <p>
     * The new array contains the same elements of the input
     * array plus the given element in the last position. The component type of
     * the new array is the same as that of the input array.
     * </p>
     * <p>
     * If the input array is {@code null}, a new one element array is returned
     * whose component type is the same as the element.
     * </p>
     * <pre>
     * ArrayUtils.add(null, 0)   = [0]
     * ArrayUtils.add([1], 0)    = [1, 0]
     * ArrayUtils.add([1, 0], 1) = [1, 0, 1]
     * </pre>
     *
     * @param array  the array to copy and add the element to, may be {@code null}
     * @param element  the object to add at the last index of the new array
     * @return A new array containing the existing elements plus the new element
     * @since 2.1
     */
    public static short[] add(final short[] array, final short element) {
        final short[] newArray = (short[]) copyArrayGrow1(array, Short.TYPE);
        newArray[newArray.length - 1] = element;
        return newArray;
    }

    /**
     * Inserts the specified element at the specified position in the array.
     * Shifts the element currently at that position (if any) and any subsequent
     * elements to the right (adds one to their indices).
     * <p>
     * This method returns a new array with the same elements of the input
     * array plus the given element on the specified position. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <p>
     * If the input array is {@code null}, a new one element array is returned
     * whose component type is the same as the element.
     * </p>
     * <pre>
     * ArrayUtils.add(null, 0, null)      = IllegalArgumentException
     * ArrayUtils.add(null, 0, "a")       = ["a"]
     * ArrayUtils.add(["a"], 1, null)     = ["a", null]
     * ArrayUtils.add(["a"], 1, "b")      = ["a", "b"]
     * ArrayUtils.add(["a", "b"], 3, "c") = ["a", "b", "c"]
     * </pre>
     *
     * @param <T> the component type of the array
     * @param array  the array to add the element to, may be {@code null}
     * @param index  the position of the new object
     * @param element  the object to add
     * @return A new array containing the existing elements and the new element
     * @throws IndexOutOfBoundsException if the index is out of range (index &lt; 0 || index &gt; array.length).
     * @throws IllegalArgumentException if both array and element are null
     * @deprecated this method has been superseded by {@link #insert(int, Object[], Object...) insert(int, T[], T...)} and
     * may be removed in a future release. Please note the handling of {@code null} input arrays differs
     * in the new method: inserting {@code X} into a {@code null} array results in {@code null} not {@code X}.
     */
    @Deprecated
    public static <T> T[] add(final T[] array, final int index, final T element) {
        final Class<T> clazz;
        if (array != null) {
            clazz = getComponentType(array);
        } else if (element != null) {
            clazz = ObjectUtils.getClass(element);
        } else {
            throw new IllegalArgumentException("Array and element cannot both be null");
        }
        return (T[]) add(array, index, element, clazz);
    }

    /**
     * Copies the given array and adds the given element at the end of the new array.
     * <p>
     * The new array contains the same elements of the input
     * array plus the given element in the last position. The component type of
     * the new array is the same as that of the input array.
     * </p>
     * <p>
     * If the input array is {@code null}, a new one element array is returned
     * whose component type is the same as the element, unless the element itself is null,
     * in which case the return type is Object[]
     * </p>
     * <pre>
     * ArrayUtils.add(null, null)      = IllegalArgumentException
     * ArrayUtils.add(null, "a")       = ["a"]
     * ArrayUtils.add(["a"], null)     = ["a", null]
     * ArrayUtils.add(["a"], "b")      = ["a", "b"]
     * ArrayUtils.add(["a", "b"], "c") = ["a", "b", "c"]
     * </pre>
     *
     * @param <T> the component type of the array
     * @param array  the array to "add" the element to, may be {@code null}
     * @param element  the object to add, may be {@code null}
     * @return A new array containing the existing elements plus the new element
     * The returned array type will be that of the input array (unless null),
     * in which case it will have the same type as the element.
     * If both are null, an IllegalArgumentException is thrown
     * @throws IllegalArgumentException if both arguments are null
     * @since 2.1
     */
    public static <T> T[] add(final T[] array, final T element) {
        final Class<?> type;
        if (array != null) {
            type = array.getClass().getComponentType();
        } else if (element != null) {
            type = element.getClass();
        } else {
            throw new IllegalArgumentException("Arguments cannot both be null");
        }
        @SuppressWarnings("unchecked") // type must be T
        final
        T[] newArray = (T[]) copyArrayGrow1(array, type);
        newArray[newArray.length - 1] = element;
        return newArray;
    }

    /**
     * Adds all the elements of the given arrays into a new array.
     * <p>
     * The new array contains all of the element of {@code array1} followed
     * by all of the elements {@code array2}. When an array is returned, it is always
     * a new array.
     * </p>
     * <pre>
     * ArrayUtils.addAll(array1, null)   = cloned copy of array1
     * ArrayUtils.addAll(null, array2)   = cloned copy of array2
     * ArrayUtils.addAll([], [])         = []
     * ArrayUtils.addAll(null, null)     = null
     * </pre>
     *
     * @param array1  the first array whose elements are added to the new array.
     * @param array2  the second array whose elements are added to the new array.
     * @return The new boolean[] array or {@code null}.
     * @since 2.1
     */
    public static boolean[] addAll(final boolean[] array1, final boolean... array2) {
        if (array1 == null) {
            return clone(array2);
        }
        if (array2 == null) {
            return clone(array1);
        }
        final boolean[] joinedArray = new boolean[array1.length + array2.length];
        System.arraycopy(array1, 0, joinedArray, 0, array1.length);
        System.arraycopy(array2, 0, joinedArray, array1.length, array2.length);
        return joinedArray;
    }

    /**
     * Adds all the elements of the given arrays into a new array.
     * <p>
     * The new array contains all of the element of {@code array1} followed
     * by all of the elements {@code array2}. When an array is returned, it is always
     * a new array.
     * </p>
     * <pre>
     * ArrayUtils.addAll(array1, null)   = cloned copy of array1
     * ArrayUtils.addAll(null, array2)   = cloned copy of array2
     * ArrayUtils.addAll([], [])         = []
     * ArrayUtils.addAll(null, null)     = null
     * </pre>
     *
     * @param array1  the first array whose elements are added to the new array.
     * @param array2  the second array whose elements are added to the new array.
     * @return The new byte[] array or {@code null}.
     * @since 2.1
     */
    public static byte[] addAll(final byte[] array1, final byte... array2) {
        if (array1 == null) {
            return clone(array2);
        }
        if (array2 == null) {
            return clone(array1);
        }
        final byte[] joinedArray = new byte[array1.length + array2.length];
        System.arraycopy(array1, 0, joinedArray, 0, array1.length);
        System.arraycopy(array2, 0, joinedArray, array1.length, array2.length);
        return joinedArray;
    }

    /**
     * Adds all the elements of the given arrays into a new array.
     * <p>
     * The new array contains all of the element of {@code array1} followed
     * by all of the elements {@code array2}. When an array is returned, it is always
     * a new array.
     * </p>
     * <pre>
     * ArrayUtils.addAll(array1, null)   = cloned copy of array1
     * ArrayUtils.addAll(null, array2)   = cloned copy of array2
     * ArrayUtils.addAll([], [])         = []
     * ArrayUtils.addAll(null, null)     = null
     * </pre>
     *
     * @param array1  the first array whose elements are added to the new array.
     * @param array2  the second array whose elements are added to the new array.
     * @return The new char[] array or {@code null}.
     * @since 2.1
     */
    public static char[] addAll(final char[] array1, final char... array2) {
        if (array1 == null) {
            return clone(array2);
        }
        if (array2 == null) {
            return clone(array1);
        }
        final char[] joinedArray = new char[array1.length + array2.length];
        System.arraycopy(array1, 0, joinedArray, 0, array1.length);
        System.arraycopy(array2, 0, joinedArray, array1.length, array2.length);
        return joinedArray;
    }

    /**
     * Adds all the elements of the given arrays into a new array.
     * <p>
     * The new array contains all of the element of {@code array1} followed
     * by all of the elements {@code array2}. When an array is returned, it is always
     * a new array.
     * </p>
     * <pre>
     * ArrayUtils.addAll(array1, null)   = cloned copy of array1
     * ArrayUtils.addAll(null, array2)   = cloned copy of array2
     * ArrayUtils.addAll([], [])         = []
     * ArrayUtils.addAll(null, null)     = null
     * </pre>
     *
     * @param array1  the first array whose elements are added to the new array.
     * @param array2  the second array whose elements are added to the new array.
     * @return The new double[] array or {@code null}.
     * @since 2.1
     */
    public static double[] addAll(final double[] array1, final double... array2) {
        if (array1 == null) {
            return clone(array2);
        }
        if (array2 == null) {
            return clone(array1);
        }
        final double[] joinedArray = new double[array1.length + array2.length];
        System.arraycopy(array1, 0, joinedArray, 0, array1.length);
        System.arraycopy(array2, 0, joinedArray, array1.length, array2.length);
        return joinedArray;
    }

    /**
     * Adds all the elements of the given arrays into a new array.
     * <p>
     * The new array contains all of the element of {@code array1} followed
     * by all of the elements {@code array2}. When an array is returned, it is always
     * a new array.
     * </p>
     * <pre>
     * ArrayUtils.addAll(array1, null)   = cloned copy of array1
     * ArrayUtils.addAll(null, array2)   = cloned copy of array2
     * ArrayUtils.addAll([], [])         = []
     * ArrayUtils.addAll(null, null)     = null
     * </pre>
     *
     * @param array1  the first array whose elements are added to the new array.
     * @param array2  the second array whose elements are added to the new array.
     * @return The new float[] array or {@code null}.
     * @since 2.1
     */
    public static float[] addAll(final float[] array1, final float... array2) {
        if (array1 == null) {
            return clone(array2);
        }
        if (array2 == null) {
            return clone(array1);
        }
        final float[] joinedArray = new float[array1.length + array2.length];
        System.arraycopy(array1, 0, joinedArray, 0, array1.length);
        System.arraycopy(array2, 0, joinedArray, array1.length, array2.length);
        return joinedArray;
    }

    /**
     * Adds all the elements of the given arrays into a new array.
     * <p>
     * The new array contains all of the element of {@code array1} followed
     * by all of the elements {@code array2}. When an array is returned, it is always
     * a new array.
     * </p>
     * <pre>
     * ArrayUtils.addAll(array1, null)   = cloned copy of array1
     * ArrayUtils.addAll(null, array2)   = cloned copy of array2
     * ArrayUtils.addAll([], [])         = []
     * ArrayUtils.addAll(null, null)     = null
     * </pre>
     *
     * @param array1  the first array whose elements are added to the new array.
     * @param array2  the second array whose elements are added to the new array.
     * @return The new int[] array or {@code null}.
     * @since 2.1
     */
    public static int[] addAll(final int[] array1, final int... array2) {
        if (array1 == null) {
            return clone(array2);
        }
        if (array2 == null) {
            return clone(array1);
        }
        final int[] joinedArray = new int[array1.length + array2.length];
        System.arraycopy(array1, 0, joinedArray, 0, array1.length);
        System.arraycopy(array2, 0, joinedArray, array1.length, array2.length);
        return joinedArray;
    }

    /**
     * Adds all the elements of the given arrays into a new array.
     * <p>
     * The new array contains all of the element of {@code array1} followed
     * by all of the elements {@code array2}. When an array is returned, it is always
     * a new array.
     * </p>
     * <pre>
     * ArrayUtils.addAll(array1, null)   = cloned copy of array1
     * ArrayUtils.addAll(null, array2)   = cloned copy of array2
     * ArrayUtils.addAll([], [])         = []
     * ArrayUtils.addAll(null, null)     = null
     * </pre>
     *
     * @param array1  the first array whose elements are added to the new array.
     * @param array2  the second array whose elements are added to the new array.
     * @return The new long[] array or {@code null}.
     * @since 2.1
     */
    public static long[] addAll(final long[] array1, final long... array2) {
        if (array1 == null) {
            return clone(array2);
        }
        if (array2 == null) {
            return clone(array1);
        }
        final long[] joinedArray = new long[array1.length + array2.length];
        System.arraycopy(array1, 0, joinedArray, 0, array1.length);
        System.arraycopy(array2, 0, joinedArray, array1.length, array2.length);
        return joinedArray;
    }

    /**
     * Adds all the elements of the given arrays into a new array.
     * <p>
     * The new array contains all of the element of {@code array1} followed
     * by all of the elements {@code array2}. When an array is returned, it is always
     * a new array.
     * </p>
     * <pre>
     * ArrayUtils.addAll(array1, null)   = cloned copy of array1
     * ArrayUtils.addAll(null, array2)   = cloned copy of array2
     * ArrayUtils.addAll([], [])         = []
     * ArrayUtils.addAll(null, null)     = null
     * </pre>
     *
     * @param array1  the first array whose elements are added to the new array.
     * @param array2  the second array whose elements are added to the new array.
     * @return The new short[] array or {@code null}.
     * @since 2.1
     */
    public static short[] addAll(final short[] array1, final short... array2) {
        if (array1 == null) {
            return clone(array2);
        }
        if (array2 == null) {
            return clone(array1);
        }
        final short[] joinedArray = new short[array1.length + array2.length];
        System.arraycopy(array1, 0, joinedArray, 0, array1.length);
        System.arraycopy(array2, 0, joinedArray, array1.length, array2.length);
        return joinedArray;
    }

    /**
     * Adds all the elements of the given arrays into a new array.
     * <p>
     * The new array contains all of the element of {@code array1} followed
     * by all of the elements {@code array2}. When an array is returned, it is always
     * a new array.
     * </p>
     * <pre>
     * ArrayUtils.addAll(null, null)     = null
     * ArrayUtils.addAll(array1, null)   = cloned copy of array1
     * ArrayUtils.addAll(null, array2)   = cloned copy of array2
     * ArrayUtils.addAll([], [])         = []
     * ArrayUtils.addAll(null, null)     = null
     * ArrayUtils.addAll([null], [null]) = [null, null]
     * ArrayUtils.addAll(["a", "b", "c"], ["1", "2", "3"]) = ["a", "b", "c", "1", "2", "3"]
     * </pre>
     *
     * @param <T> the component type of the array
     * @param array1  the first array whose elements are added to the new array, may be {@code null}
     * @param array2  the second array whose elements are added to the new array, may be {@code null}
     * @return The new array, {@code null} if both arrays are {@code null}.
     *      The type of the new array is the type of the first array,
     *      unless the first array is null, in which case the type is the same as the second array.
     * @throws IllegalArgumentException if the array types are incompatible
     * @since 2.1
     */
    public static <T> T[] addAll(final T[] array1, @SuppressWarnings("unchecked") final T... array2) {
        if (array1 == null) {
            return clone(array2);
        }
        if (array2 == null) {
            return clone(array1);
        }
        final Class<T> type1 = getComponentType(array1);
        final T[] joinedArray = arraycopy(array1, 0, 0, array1.length, () -> newInstance(type1, array1.length + array2.length));
        try {
            System.arraycopy(array2, 0, joinedArray, array1.length, array2.length);
        } catch (final ArrayStoreException ase) {
            // Check if problem was due to incompatible types
            /*
             * We do this here, rather than before the copy because: - it would be a wasted check most of the time - safer, in case check turns out to be too
             * strict
             */
            final Class<?> type2 = array2.getClass().getComponentType();
            if (!type1.isAssignableFrom(type2)) {
                throw new IllegalArgumentException("Cannot store " + type2.getName() + " in an array of " + type1.getName(), ase);
            }
            throw ase; // No, so rethrow original
        }
        return joinedArray;
    }

    /**
     * Copies the given array and adds the given element at the beginning of the new array.
     * <p>
     * The new array contains the same elements of the input array plus the given element in the first position. The
     * component type of the new array is the same as that of the input array.
     * </p>
     * <p>
     * If the input array is {@code null}, a new one element array is returned whose component type is the same as the
     * element.
     * </p>
     * <pre>
     * ArrayUtils.addFirst(null, true)          = [true]
     * ArrayUtils.addFirst([true], false)       = [false, true]
     * ArrayUtils.addFirst([true, false], true) = [true, true, false]
     * </pre>
     *
     * @param array the array to "add" the element to, may be {@code null}.
     * @param element the object to add.
     * @return A new array containing the existing elements plus the new element The returned array type will be that of
     *         the input array (unless null), in which case it will have the same type as the element.
     * @since 3.10
     */
    public static boolean[] addFirst(final boolean[] array, final boolean element) {
        return array == null ? add(array, element) : insert(0, array, element);
    }

    /**
     * Copies the given array and adds the given element at the beginning of the new array.
     * <p>
     * The new array contains the same elements of the input array plus the given element in the first position. The
     * component type of the new array is the same as that of the input array.
     * </p>
     * <p>
     * If the input array is {@code null}, a new one element array is returned whose component type is the same as the
     * element.
     * </p>
     * <pre>
     * ArrayUtils.addFirst(null, 1)   = [1]
     * ArrayUtils.addFirst([1], 0)    = [0, 1]
     * ArrayUtils.addFirst([1, 0], 1) = [1, 1, 0]
     * </pre>
     *
     * @param array the array to "add" the element to, may be {@code null}.
     * @param element the object to add.
     * @return A new array containing the existing elements plus the new element The returned array type will be that of
     *         the input array (unless null), in which case it will have the same type as the element.
     * @since 3.10
     */
    public static byte[] addFirst(final byte[] array, final byte element) {
        return array == null ? add(array, element) : insert(0, array, element);
    }

    /**
     * Copies the given array and adds the given element at the beginning of the new array.
     * <p>
     * The new array contains the same elements of the input array plus the given element in the first position. The
     * component type of the new array is the same as that of the input array.
     * </p>
     * <p>
     * If the input array is {@code null}, a new one element array is returned whose component type is the same as the
     * element.
     * </p>
     * <pre>
     * ArrayUtils.addFirst(null, '1')       = ['1']
     * ArrayUtils.addFirst(['1'], '0')      = ['0', '1']
     * ArrayUtils.addFirst(['1', '0'], '1') = ['1', '1', '0']
     * </pre>
     *
     * @param array the array to "add" the element to, may be {@code null}.
     * @param element the object to add.
     * @return A new array containing the existing elements plus the new element The returned array type will be that of
     *         the input array (unless null), in which case it will have the same type as the element.
     * @since 3.10
     */
    public static char[] addFirst(final char[] array, final char element) {
        return array == null ? add(array, element) : insert(0, array, element);
    }

    /**
     * Copies the given array and adds the given element at the beginning of the new array.
     * <p>
     * The new array contains the same elements of the input array plus the given element in the first position. The
     * component type of the new array is the same as that of the input array.
     * </p>
     * <p>
     * If the input array is {@code null}, a new one element array is returned whose component type is the same as the
     * element.
     * </p>
     * <pre>
     * ArrayUtils.addFirst(null, 1)   = [1]
     * ArrayUtils.addFirst([1], 0)    = [0, 1]
     * ArrayUtils.addFirst([1, 0], 1) = [1, 1, 0]
     * </pre>
     *
     * @param array the array to "add" the element to, may be {@code null}.
     * @param element the object to add.
     * @return A new array containing the existing elements plus the new element The returned array type will be that of
     *         the input array (unless null), in which case it will have the same type as the element.
     * @since 3.10
     */
    public static double[] addFirst(final double[] array, final double element) {
        return array == null ? add(array, element) : insert(0, array, element);
    }

    /**
     * Copies the given array and adds the given element at the beginning of the new array.
     * <p>
     * The new array contains the same elements of the input array plus the given element in the first position. The
     * component type of the new array is the same as that of the input array.
     * </p>
     * <p>
     * If the input array is {@code null}, a new one element array is returned whose component type is the same as the
     * element.
     * </p>
     * <pre>
     * ArrayUtils.addFirst(null, 1)   = [1]
     * ArrayUtils.addFirst([1], 0)    = [0, 1]
     * ArrayUtils.addFirst([1, 0], 1) = [1, 1, 0]
     * </pre>
     *
     * @param array the array to "add" the element to, may be {@code null}.
     * @param element the object to add.
     * @return A new array containing the existing elements plus the new element The returned array type will be that of
     *         the input array (unless null), in which case it will have the same type as the element.
     * @since 3.10
     */
    public static float[] addFirst(final float[] array, final float element) {
        return array == null ? add(array, element) : insert(0, array, element);
    }

    /**
     * Copies the given array and adds the given element at the beginning of the new array.
     * <p>
     * The new array contains the same elements of the input array plus the given element in the first position. The
     * component type of the new array is the same as that of the input array.
     * </p>
     * <p>
     * If the input array is {@code null}, a new one element array is returned whose component type is the same as the
     * element.
     * </p>
     * <pre>
     * ArrayUtils.addFirst(null, 1)   = [1]
     * ArrayUtils.addFirst([1], 0)    = [0, 1]
     * ArrayUtils.addFirst([1, 0], 1) = [1, 1, 0]
     * </pre>
     *
     * @param array the array to "add" the element to, may be {@code null}.
     * @param element the object to add.
     * @return A new array containing the existing elements plus the new element The returned array type will be that of
     *         the input array (unless null), in which case it will have the same type as the element.
     * @since 3.10
     */
    public static int[] addFirst(final int[] array, final int element) {
        return array == null ? add(array, element) : insert(0, array, element);
    }

    /**
     * Copies the given array and adds the given element at the beginning of the new array.
     * <p>
     * The new array contains the same elements of the input array plus the given element in the first position. The
     * component type of the new array is the same as that of the input array.
     * </p>
     * <p>
     * If the input array is {@code null}, a new one element array is returned whose component type is the same as the
     * element.
     * </p>
     * <pre>
     * ArrayUtils.addFirst(null, 1)   = [1]
     * ArrayUtils.addFirst([1], 0)    = [0, 1]
     * ArrayUtils.addFirst([1, 0], 1) = [1, 1, 0]
     * </pre>
     *
     * @param array the array to "add" the element to, may be {@code null}.
     * @param element the object to add.
     * @return A new array containing the existing elements plus the new element The returned array type will be that of
     *         the input array (unless null), in which case it will have the same type as the element.
     * @since 3.10
     */
    public static long[] addFirst(final long[] array, final long element) {
        return array == null ? add(array, element) : insert(0, array, element);
    }

    /**
     * Copies the given array and adds the given element at the beginning of the new array.
     * <p>
     * The new array contains the same elements of the input array plus the given element in the first position. The
     * component type of the new array is the same as that of the input array.
     * </p>
     * <p>
     * If the input array is {@code null}, a new one element array is returned whose component type is the same as the
     * element.
     * </p>
     * <pre>
     * ArrayUtils.addFirst(null, 1)   = [1]
     * ArrayUtils.addFirst([1], 0)    = [0, 1]
     * ArrayUtils.addFirst([1, 0], 1) = [1, 1, 0]
     * </pre>
     *
     * @param array the array to "add" the element to, may be {@code null}.
     * @param element the object to add.
     * @return A new array containing the existing elements plus the new element The returned array type will be that of
     *         the input array (unless null), in which case it will have the same type as the element.
     * @since 3.10
     */
    public static short[] addFirst(final short[] array, final short element) {
        return array == null ? add(array, element) : insert(0, array, element);
    }

    /**
     * Copies the given array and adds the given element at the beginning of the new array.
     * <p>
     * The new array contains the same elements of the input array plus the given element in the first position. The
     * component type of the new array is the same as that of the input array.
     * </p>
     * <p>
     * If the input array is {@code null}, a new one element array is returned whose component type is the same as the
     * element, unless the element itself is null, in which case the return type is Object[]
     * </p>
     * <pre>
     * ArrayUtils.addFirst(null, null)      = IllegalArgumentException
     * ArrayUtils.addFirst(null, "a")       = ["a"]
     * ArrayUtils.addFirst(["a"], null)     = [null, "a"]
     * ArrayUtils.addFirst(["a"], "b")      = ["b", "a"]
     * ArrayUtils.addFirst(["a", "b"], "c") = ["c", "a", "b"]
     * </pre>
     *
     * @param <T> the component type of the array
     * @param array the array to "add" the element to, may be {@code null}
     * @param element the object to add, may be {@code null}
     * @return A new array containing the existing elements plus the new element The returned array type will be that of
     *         the input array (unless null), in which case it will have the same type as the element. If both are null,
     *         an IllegalArgumentException is thrown
     * @throws IllegalArgumentException if both arguments are null
     * @since 3.10
     */
    public static <T> T[] addFirst(final T[] array, final T element) {
        return array == null ? add(array, element) : insert(0, array, element);
    }

    /**
     * A fluent version of {@link System#arraycopy(Object, int, Object, int, int)} that returns the destination array.
     *
     * @param <T>       the type.
     * @param source    the source array.
     * @param sourcePos starting position in the source array.
     * @param destPos   starting position in the destination data.
     * @param length    the number of array elements to be copied.
     * @param allocator allocates the array to populate and return.
     * @return dest
     * @throws IndexOutOfBoundsException if copying would cause access of data outside array bounds.
     * @throws ArrayStoreException       if an element in the {@code src} array could not be stored into the {@code dest} array because of a type
     *                                   mismatch.
     * @throws NullPointerException      if either {@code src} or {@code dest} is {@code null}.
     * @since 3.15.0
     */
    public static <T> T arraycopy(final T source, final int sourcePos, final int destPos, final int length, final Function<Integer, T> allocator) {
        return arraycopy(source, sourcePos, allocator.apply(length), destPos, length);
    }

    /**
     * A fluent version of {@link System#arraycopy(Object, int, Object, int, int)} that returns the destination array.
     *
     * @param <T>       the type.
     * @param source    the source array.
     * @param sourcePos starting position in the source array.
     * @param destPos   starting position in the destination data.
     * @param length    the number of array elements to be copied.
     * @param allocator allocates the array to populate and return.
     * @return dest
     * @throws IndexOutOfBoundsException if copying would cause access of data outside array bounds.
     * @throws ArrayStoreException       if an element in the {@code src} array could not be stored into the {@code dest} array because of a type
     *                                   mismatch.
     * @throws NullPointerException      if either {@code src} or {@code dest} is {@code null}.
     * @since 3.15.0
     */
    public static <T> T arraycopy(final T source, final int sourcePos, final int destPos, final int length, final Supplier<T> allocator) {
        return arraycopy(source, sourcePos, allocator.get(), destPos, length);
    }

    /**
     * A fluent version of {@link System#arraycopy(Object, int, Object, int, int)} that returns the destination array.
     *
     * @param <T>       the type
     * @param source    the source array.
     * @param sourcePos starting position in the source array.
     * @param dest      the destination array.
     * @param destPos   starting position in the destination data.
     * @param length    the number of array elements to be copied.
     * @return dest
     * @throws IndexOutOfBoundsException if copying would cause access of data outside array bounds.
     * @throws ArrayStoreException       if an element in the {@code src} array could not be stored into the {@code dest} array because of a type
     *                                   mismatch.
     * @throws NullPointerException      if either {@code src} or {@code dest} is {@code null}.
     * @since 3.15.0
     */
    public static <T> T arraycopy(final T source, final int sourcePos, final T dest, final int destPos, final int length) {
        System.arraycopy(source, sourcePos, dest, destPos, length);
        return dest;
    }

    /**
     * Clones an array or returns {@code null}.
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param array the array to clone, may be {@code null}
     * @return the cloned array, {@code null} if {@code null} input
     */
    public static boolean[] clone(final boolean[] array) {
        return array != null ? array.clone() : null;
    }

    /**
     * Clones an array or returns {@code null}.
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param array the array to clone, may be {@code null}
     * @return the cloned array, {@code null} if {@code null} input
     */
    public static byte[] clone(final byte[] array) {
        return array != null ? array.clone() : null;
    }

    /**
     * Clones an array or returns {@code null}.
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param array the array to clone, may be {@code null}
     * @return the cloned array, {@code null} if {@code null} input
     */
    public static char[] clone(final char[] array) {
        return array != null ? array.clone() : null;
    }

    /**
     * Clones an array or returns {@code null}.
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param array the array to clone, may be {@code null}
     * @return the cloned array, {@code null} if {@code null} input
     */
    public static double[] clone(final double[] array) {
        return array != null ? array.clone() : null;
    }

    /**
     * Clones an array or returns {@code null}.
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param array the array to clone, may be {@code null}
     * @return the cloned array, {@code null} if {@code null} input
     */
    public static float[] clone(final float[] array) {
        return array != null ? array.clone() : null;
    }

    /**
     * Clones an array or returns {@code null}.
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param array the array to clone, may be {@code null}
     * @return the cloned array, {@code null} if {@code null} input
     */
    public static int[] clone(final int[] array) {
        return array != null ? array.clone() : null;
    }

    /**
     * Clones an array or returns {@code null}.
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param array the array to clone, may be {@code null}
     * @return the cloned array, {@code null} if {@code null} input
     */
    public static long[] clone(final long[] array) {
        return array != null ? array.clone() : null;
    }

    /**
     * Clones an array or returns {@code null}.
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param array the array to clone, may be {@code null}
     * @return the cloned array, {@code null} if {@code null} input
     */
    public static short[] clone(final short[] array) {
        return array != null ? array.clone() : null;
    }

    /**
     * Shallow clones an array or returns {@code null}.
     * <p>
     * The objects in the array are not cloned, thus there is no special handling for multi-dimensional arrays.
     * </p>
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param <T>   the component type of the array
     * @param array the array to shallow clone, may be {@code null}
     * @return the cloned array, {@code null} if {@code null} input
     */
    public static <T> T[] clone(final T[] array) {
        return array != null ? array.clone() : null;
    }

    /**
     * Checks if the value is in the given array.
     * <p>
     * The method returns {@code false} if a {@code null} array is passed in.
     * </p>
     *
     * @param array  the array to search
     * @param valueToFind  the value to find
     * @return {@code true} if the array contains the object
     */
    public static boolean contains(final boolean[] array, final boolean valueToFind) {
        return indexOf(array, valueToFind) != INDEX_NOT_FOUND;
    }

    /**
     * Checks if the value is in the given array.
     * <p>
     * The method returns {@code false} if a {@code null} array is passed in.
     * </p>
     * <p>
     * If the {@code array} elements you are searching implement {@link Comparator}, consider whether it is worth using
     * {@link Arrays#sort(byte[])} and {@link Arrays#binarySearch(byte[], byte)}.
     * </p>
     *
     * @param array  the array to search
     * @param valueToFind  the value to find
     * @return {@code true} if the array contains the object
     */
    public static boolean contains(final byte[] array, final byte valueToFind) {
        return indexOf(array, valueToFind) != INDEX_NOT_FOUND;
    }

    /**
     * Checks if the value is in the given array.
     * <p>
     * The method returns {@code false} if a {@code null} array is passed in.
     * </p>
     * <p>
     * If the {@code array} elements you are searching implement {@link Comparator}, consider whether it is worth using
     * {@link Arrays#sort(char[])} and {@link Arrays#binarySearch(char[], char)}.
     * </p>
     *
     * @param array  the array to search
     * @param valueToFind  the value to find
     * @return {@code true} if the array contains the object
     * @since 2.1
     */
    public static boolean contains(final char[] array, final char valueToFind) {
        return indexOf(array, valueToFind) != INDEX_NOT_FOUND;
    }

    /**
     * Checks if the value is in the given array.
     * <p>
     * The method returns {@code false} if a {@code null} array is passed in.
     * </p>
     * <p>
     * If the {@code array} elements you are searching implement {@link Comparator}, consider whether it is worth using
     * {@link Arrays#sort(double[])} and {@link Arrays#binarySearch(double[], double)}.
     * </p>
     *
     * @param array  the array to search
     * @param valueToFind  the value to find
     * @return {@code true} if the array contains the object
     */
    public static boolean contains(final double[] array, final double valueToFind) {
        return indexOf(array, valueToFind) != INDEX_NOT_FOUND;
    }

    /**
     * Checks if a value falling within the given tolerance is in the
     * given array.  If the array contains a value within the inclusive range
     * defined by (value - tolerance) to (value + tolerance).
     * <p>
     * The method returns {@code false} if a {@code null} array
     * is passed in.
     * </p>
     * <p>
     * If the {@code array} elements you are searching implement {@link Comparator}, consider whether it is worth using
     * {@link Arrays#sort(double[])} and {@link Arrays#binarySearch(double[], double)}.
     * </p>
     *
     * @param array  the array to search
     * @param valueToFind  the value to find
     * @param tolerance  the array contains the tolerance of the search
     * @return true if value falling within tolerance is in array
     */
    public static boolean contains(final double[] array, final double valueToFind, final double tolerance) {
        return indexOf(array, valueToFind, 0, tolerance) != INDEX_NOT_FOUND;
    }

    /**
     * Checks if the value is in the given array.
     * <p>
     * The method returns {@code false} if a {@code null} array is passed in.
     * </p>
     * <p>
     * If the {@code array} elements you are searching implement {@link Comparator}, consider whether it is worth using
     * {@link Arrays#sort(float[])} and {@link Arrays#binarySearch(float[], float)}.
     * </p>
     *
     * @param array  the array to search
     * @param valueToFind  the value to find
     * @return {@code true} if the array contains the object
     */
    public static boolean contains(final float[] array, final float valueToFind) {
        return indexOf(array, valueToFind) != INDEX_NOT_FOUND;
    }

    /**
     * Checks if the value is in the given array.
     * <p>
     * The method returns {@code false} if a {@code null} array is passed in.
     * </p>
     * <p>
     * If the {@code array} elements you are searching implement {@link Comparator}, consider whether it is worth using
     * {@link Arrays#sort(int[])} and {@link Arrays#binarySearch(int[], int)}.
     * </p>
     *
     * @param array  the array to search
     * @param valueToFind  the value to find
     * @return {@code true} if the array contains the object
     */
    public static boolean contains(final int[] array, final int valueToFind) {
        return indexOf(array, valueToFind) != INDEX_NOT_FOUND;
    }

    /**
     * Checks if the value is in the given array.
     * <p>
     * The method returns {@code false} if a {@code null} array is passed in.
     * </p>
     * <p>
     * If the {@code array} elements you are searching implement {@link Comparator}, consider whether it is worth using
     * {@link Arrays#sort(long[])} and {@link Arrays#binarySearch(long[], long)}.
     * </p>
     *
     * @param array  the array to search
     * @param valueToFind  the value to find
     * @return {@code true} if the array contains the object
     */
    public static boolean contains(final long[] array, final long valueToFind) {
        return indexOf(array, valueToFind) != INDEX_NOT_FOUND;
    }

    /**
     * Checks if the object is in the given array.
     * <p>
     * The method returns {@code false} if a {@code null} array is passed in.
     * </p>
     * <p>
     * If the {@code array} elements you are searching implement {@link Comparator}, consider whether it is worth using
     * {@link Arrays#sort(Object[], Comparator)} and {@link Arrays#binarySearch(Object[], Object)}.
     * </p>
     *
     * @param array  the array to search, may be {@code null}.
     * @param objectToFind  the object to find, may be {@code null}.
     * @return {@code true} if the array contains the object
     */
    public static boolean contains(final Object[] array, final Object objectToFind) {
        return indexOf(array, objectToFind) != INDEX_NOT_FOUND;
    }

    /**
     * Checks if the value is in the given array.
     * <p>
     * The method returns {@code false} if a {@code null} array is passed in.
     * </p>
     * <p>
     * If the {@code array} elements you are searching implement {@link Comparator}, consider whether it is worth using
     * {@link Arrays#sort(short[])} and {@link Arrays#binarySearch(short[], short)}.
     * </p>
     *
     * @param array  the array to search
     * @param valueToFind  the value to find
     * @return {@code true} if the array contains the object
     */
    public static boolean contains(final short[] array, final short valueToFind) {
        return indexOf(array, valueToFind) != INDEX_NOT_FOUND;
    }

    /**
     * Checks if any of the ints are in the given array.
     * <p>
     * The method returns {@code false} if a {@code null} array is passed in.
     * </p>
     * <p>
     * If the {@code array} elements you are searching implement {@link Comparator}, consider whether it is worth using
     * {@link Arrays#sort(int[])} and {@link Arrays#binarySearch(int[], int)}.
     * </p>
     *
     * @param array         the array to search
     * @param objectsToFind any of the ints to find
     * @return {@code true} if the array contains any of the ints
     * @since 3.18.0
     */
    public static boolean containsAny(final int[] array, final int... objectsToFind) {
        return IntStreams.of(objectsToFind).anyMatch(e -> contains(array, e));
    }

    /**
     * Checks if any of the objects are in the given array.
     * <p>
     * The method returns {@code false} if a {@code null} array is passed in.
     * </p>
     * <p>
     * If the {@code array} elements you are searching implement {@link Comparator}, consider whether it is worth using
     * {@link Arrays#sort(Object[], Comparator)} and {@link Arrays#binarySearch(Object[], Object)}.
     * </p>
     *
     * @param array         the array to search, may be {@code null}.
     * @param objectsToFind any of the objects to find, may be {@code null}.
     * @return {@code true} if the array contains any of the objects
     * @since 3.13.0
     */
    public static boolean containsAny(final Object[] array, final Object... objectsToFind) {
        return Streams.of(objectsToFind).anyMatch(e -> contains(array, e));
    }

    /**
     * Returns a copy of the given array of size 1 greater than the argument.
     * The last value of the array is left to the default value.
     *
     * @param array The array to copy, must not be {@code null}.
     * @param newArrayComponentType If {@code array} is {@code null}, create a
     * size 1 array of this type.
     * @return A new copy of the array of size 1 greater than the input.
     */
    private static Object copyArrayGrow1(final Object array, final Class<?> newArrayComponentType) {
        if (array != null) {
            final int arrayLength = Array.getLength(array);
            final Object newArray = Array.newInstance(array.getClass().getComponentType(), arrayLength + 1);
            System.arraycopy(array, 0, newArray, 0, arrayLength);
            return newArray;
        }
        return Array.newInstance(newArrayComponentType, 1);
    }

    /**
     * Gets the nTh element of an array or null if the index is out of bounds or the array is null.
     *
     * @param <T> The type of array elements.
     * @param array The array to index.
     * @param index The index
     * @return the nTh element of an array or null if the index is out of bounds or the array is null.
     * @since 3.11
     */
    public static <T> T get(final T[] array, final int index) {
        return get(array, index, null);
    }

    /**
     * Gets the nTh element of an array or a default value if the index is out of bounds.
     *
     * @param <T> The type of array elements.
     * @param array The array to index.
     * @param index The index
     * @param defaultValue The return value of the given index is out of bounds.
     * @return the nTh element of an array or a default value if the index is out of bounds.
     * @since 3.11
     */
    public static <T> T get(final T[] array, final int index, final T defaultValue) {
        return isArrayIndexValid(array, index) ? array[index] : defaultValue;
    }

    /**
     * Gets an array's component type.
     *
     * @param <T> The array type.
     * @param array The array.
     * @return The component type.
     * @since 3.13.0
     */
    public static <T> Class<T> getComponentType(final T[] array) {
        return ClassUtils.getComponentType(ObjectUtils.getClass(array));
    }

    /**
     * Returns the length of the specified array.
     * This method can deal with {@link Object} arrays and with primitive arrays.
     * <p>
     * If the input array is {@code null}, {@code 0} is returned.
     * </p>
     * <pre>
     * ArrayUtils.getLength(null)            = 0
     * ArrayUtils.getLength([])              = 0
     * ArrayUtils.getLength([null])          = 1
     * ArrayUtils.getLength([true, false])   = 2
     * ArrayUtils.getLength([1, 2, 3])       = 3
     * ArrayUtils.getLength(["a", "b", "c"]) = 3
     * </pre>
     *
     * @param array  the array to retrieve the length from, may be {@code null}.
     * @return The length of the array, or {@code 0} if the array is {@code null}
     * @throws IllegalArgumentException if the object argument is not an array.
     * @since 2.1
     */
    public static int getLength(final Object array) {
        return array != null ? Array.getLength(array) : 0;
    }

    /**
     * Gets a hash code for an array handling multidimensional arrays correctly.
     * <p>
     * Multi-dimensional primitive arrays are also handled correctly by this method.
     * </p>
     *
     * @param array  the array to get a hash code for, {@code null} returns zero
     * @return a hash code for the array
     */
    public static int hashCode(final Object array) {
        return new HashCodeBuilder().append(array).toHashCode();
    }

    static <K> void increment(final Map<K, MutableInt> occurrences, final K boxed) {
        occurrences.computeIfAbsent(boxed, k -> new MutableInt()).increment();
    }

    /**
     * Finds the indices of the given value in the array.
     * <p>
     * This method returns an empty BitSet for a {@code null} input array.
     * </p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @return a BitSet of all the indices of the value within the array,
     *  an empty BitSet if not found or {@code null} array input
     * @since 3.10
     */
    public static BitSet indexesOf(final boolean[] array, final boolean valueToFind) {
        return indexesOf(array, valueToFind, 0);
    }

    /**
     * Finds the indices of the given value in the array starting at the given index.
     * <p>
     * This method returns an empty BitSet for a {@code null} input array.
     * </p>
     * <p>
     * A negative startIndex is treated as zero. A startIndex larger than the array
     * length will return an empty BitSet ({@code -1}).
     * </p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @param startIndex  the index to start searching at
     * @return a BitSet of all the indices of the value within the array,
     *  an empty BitSet if not found or {@code null}
     *  array input
     * @since 3.10
     */
    public static BitSet indexesOf(final boolean[] array, final boolean valueToFind, int startIndex) {
        final BitSet bitSet = new BitSet();
        if (array == null) {
            return bitSet;
        }
        while (startIndex < array.length) {
            startIndex = indexOf(array, valueToFind, startIndex);
            if (startIndex == INDEX_NOT_FOUND) {
                break;
            }
            bitSet.set(startIndex);
            ++startIndex;
        }
        return bitSet;
    }

    /**
     * Finds the indices of the given value in the array.
     *
     * <p>This method returns an empty BitSet for a {@code null} input array.</p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @return a BitSet of all the indices of the value within the array,
     *  an empty BitSet if not found or {@code null} array input
     * @since 3.10
     */
    public static BitSet indexesOf(final byte[] array, final byte valueToFind) {
        return indexesOf(array, valueToFind, 0);
    }

    /**
     * Finds the indices of the given value in the array starting at the given index.
     *
     * <p>This method returns an empty BitSet for a {@code null} input array.</p>
     *
     * <p>A negative startIndex is treated as zero. A startIndex larger than the array
     * length will return an empty BitSet.</p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @param startIndex  the index to start searching at
     * @return a BitSet of all the indices of the value within the array,
     *  an empty BitSet if not found or {@code null} array input
     * @since 3.10
     */
    public static BitSet indexesOf(final byte[] array, final byte valueToFind, int startIndex) {
        final BitSet bitSet = new BitSet();
        if (array == null) {
            return bitSet;
        }
        while (startIndex < array.length) {
            startIndex = indexOf(array, valueToFind, startIndex);
            if (startIndex == INDEX_NOT_FOUND) {
                break;
            }
            bitSet.set(startIndex);
            ++startIndex;
        }

        return bitSet;
    }

    /**
     * Finds the indices of the given value in the array.
     *
     * <p>This method returns an empty BitSet for a {@code null} input array.</p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @return a BitSet of all the indices of the value within the array,
     *  an empty BitSet if not found or {@code null} array input
     * @since 3.10
     */
    public static BitSet indexesOf(final char[] array, final char valueToFind) {
        return indexesOf(array, valueToFind, 0);
    }

    /**
     * Finds the indices of the given value in the array starting at the given index.
     *
     * <p>This method returns an empty BitSet for a {@code null} input array.</p>
     *
     * <p>A negative startIndex is treated as zero. A startIndex larger than the array
     * length will return an empty BitSet.</p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @param startIndex  the index to start searching at
     * @return a BitSet of all the indices of the value within the array,
     *  an empty BitSet if not found or {@code null} array input
     * @since 3.10
     */
    public static BitSet indexesOf(final char[] array, final char valueToFind, int startIndex) {
        final BitSet bitSet = new BitSet();
        if (array == null) {
            return bitSet;
        }
        while (startIndex < array.length) {
            startIndex = indexOf(array, valueToFind, startIndex);
            if (startIndex == INDEX_NOT_FOUND) {
                break;
            }
            bitSet.set(startIndex);
            ++startIndex;
        }
        return bitSet;
    }

    /**
     * Finds the indices of the given value in the array.
     *
     * <p>This method returns empty BitSet for a {@code null} input array.</p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @return a BitSet of all the indices of the value within the array,
     *  an empty BitSet if not found or {@code null} array input
     * @since 3.10
     */
    public static BitSet indexesOf(final double[] array, final double valueToFind) {
        return indexesOf(array, valueToFind, 0);
    }

    /**
     * Finds the indices of the given value within a given tolerance in the array.
     *
     * <p>
     * This method will return all the indices of the value which fall between the region
     * defined by valueToFind - tolerance and valueToFind + tolerance, each time between the nearest integers.
     * </p>
     *
     * <p>This method returns an empty BitSet for a {@code null} input array.</p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @param tolerance tolerance of the search
     * @return a BitSet of all the indices of the value within the array,
     *  an empty BitSet if not found or {@code null} array input
     * @since 3.10
     */
    public static BitSet indexesOf(final double[] array, final double valueToFind, final double tolerance) {
        return indexesOf(array, valueToFind, 0, tolerance);
    }

    /**
     * Finds the indices of the given value in the array starting at the given index.
     *
     * <p>This method returns an empty BitSet for a {@code null} input array.</p>
     *
     * <p>A negative startIndex is treated as zero. A startIndex larger than the array
     * length will return an empty BitSet.</p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @param startIndex  the index to start searching at
     * @return a BitSet of the indices of the value within the array,
     *  an empty BitSet if not found or {@code null} array input
     * @since 3.10
     */
    public static BitSet indexesOf(final double[] array, final double valueToFind, int startIndex) {
        final BitSet bitSet = new BitSet();
        if (array == null) {
            return bitSet;
        }
        while (startIndex < array.length) {
            startIndex = indexOf(array, valueToFind, startIndex);
            if (startIndex == INDEX_NOT_FOUND) {
                break;
            }
            bitSet.set(startIndex);
            ++startIndex;
        }
        return bitSet;
    }

    /**
     * Finds the indices of the given value in the array starting at the given index.
     *
     * <p>
     * This method will return the indices of the values which fall between the region
     * defined by valueToFind - tolerance and valueToFind + tolerance, between the nearest integers.
     * </p>
     *
     * <p>This method returns an empty BitSet for a {@code null} input array.</p>
     *
     * <p>A negative startIndex is treated as zero. A startIndex larger than the array
     * length will return an empty BitSet.</p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @param startIndex  the index to start searching at
     * @param tolerance tolerance of the search
     * @return a BitSet of the indices of the value within the array,
     *  an empty BitSet if not found or {@code null} array input
     * @since 3.10
     */
    public static BitSet indexesOf(final double[] array, final double valueToFind, int startIndex, final double tolerance) {
        final BitSet bitSet = new BitSet();
        if (array == null) {
            return bitSet;
        }
        while (startIndex < array.length) {
            startIndex = indexOf(array, valueToFind, startIndex, tolerance);
            if (startIndex == INDEX_NOT_FOUND) {
                break;
            }
            bitSet.set(startIndex);
            ++startIndex;
        }
        return bitSet;
    }

    /**
     * Finds the indices of the given value in the array.
     *
     * <p>This method returns an empty BitSet for a {@code null} input array.</p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @return a BitSet of all the indices of the value within the array,
     *  an empty BitSet if not found or {@code null} array input
     * @since 3.10
     */
    public static BitSet indexesOf(final float[] array, final float valueToFind) {
        return indexesOf(array, valueToFind, 0);
    }

    /**
     * Finds the indices of the given value in the array starting at the given index.
     *
     * <p>This method returns an empty BitSet for a {@code null} input array.</p>
     *
     * <p>A negative startIndex is treated as zero. A startIndex larger than the array
     * length will return empty BitSet.</p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @param startIndex  the index to start searching at
     * @return a BitSet of all the indices of the value within the array,
     *  an empty BitSet if not found or {@code null} array input
     * @since 3.10
     */
    public static BitSet indexesOf(final float[] array, final float valueToFind, int startIndex) {
        final BitSet bitSet = new BitSet();
        if (array == null) {
            return bitSet;
        }
        while (startIndex < array.length) {
            startIndex = indexOf(array, valueToFind, startIndex);
            if (startIndex == INDEX_NOT_FOUND) {
                break;
            }
            bitSet.set(startIndex);
            ++startIndex;
        }
        return bitSet;
    }

    /**
     * Finds the indices of the given value in the array.
     *
     * <p>This method returns an empty BitSet for a {@code null} input array.</p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @return a BitSet of all the indices of the value within the array,
     *  an empty BitSet if not found or {@code null} array input
     * @since 3.10
     */
    public static BitSet indexesOf(final int[] array, final int valueToFind) {
        return indexesOf(array, valueToFind, 0);
    }

    /**
     * Finds the indices of the given value in the array starting at the given index.
     *
     * <p>This method returns an empty BitSet for a {@code null} input array.</p>
     *
     * <p>A negative startIndex is treated as zero. A startIndex larger than the array
     * length will return an empty BitSet.</p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @param startIndex  the index to start searching at
     * @return a BitSet of all the indices of the value within the array,
     *  an empty BitSet if not found or {@code null} array input
     * @since 3.10
     */
    public static BitSet indexesOf(final int[] array, final int valueToFind, int startIndex) {
        final BitSet bitSet = new BitSet();
        if (array == null) {
            return bitSet;
        }
        while (startIndex < array.length) {
            startIndex = indexOf(array, valueToFind, startIndex);

            if (startIndex == INDEX_NOT_FOUND) {
                break;
            }
            bitSet.set(startIndex);
            ++startIndex;
        }
        return bitSet;
    }

    /**
     * Finds the indices of the given value in the array.
     *
     * <p>This method returns an empty BitSet for a {@code null} input array.</p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @return a BitSet of all the indices of the value within the array,
     *  an empty BitSet if not found or {@code null} array input
     * @since 3.10
     */
    public static BitSet indexesOf(final long[] array, final long valueToFind) {
        return indexesOf(array, valueToFind, 0);
    }

    /**
     * Finds the indices of the given value in the array starting at the given index.
     *
     * <p>This method returns an empty BitSet for a {@code null} input array.</p>
     *
     * <p>A negative startIndex is treated as zero. A startIndex larger than the array
     * length will return an empty BitSet.</p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @param startIndex  the index to start searching at
     * @return a BitSet of all the indices of the value within the array,
     *  an empty BitSet if not found or {@code null} array input
     * @since 3.10
     */
    public static BitSet indexesOf(final long[] array, final long valueToFind, int startIndex) {
        final BitSet bitSet = new BitSet();
        if (array == null) {
            return bitSet;
        }
        while (startIndex < array.length) {
            startIndex = indexOf(array, valueToFind, startIndex);
            if (startIndex == INDEX_NOT_FOUND) {
                break;
            }
            bitSet.set(startIndex);
            ++startIndex;
        }
        return bitSet;
    }

    /**
     * Finds the indices of the given object in the array.
     *
     * <p>This method returns an empty BitSet for a {@code null} input array.</p>
     *
     * @param array  the array to search for the object, may be {@code null}.
     * @param objectToFind  the object to find, may be {@code null}.
     * @return a BitSet of all the indices of the object within the array,
     *  an empty BitSet if not found or {@code null} array input
     * @since 3.10
     */
    public static BitSet indexesOf(final Object[] array, final Object objectToFind) {
        return indexesOf(array, objectToFind, 0);
    }

    /**
     * Finds the indices of the given object in the array starting at the given index.
     *
     * <p>This method returns an empty BitSet for a {@code null} input array.</p>
     *
     * <p>A negative startIndex is treated as zero. A startIndex larger than the array
     * length will return an empty BitSet.</p>
     *
     * @param array  the array to search for the object, may be {@code null}.
     * @param objectToFind  the object to find, may be {@code null}.
     * @param startIndex  the index to start searching at
     * @return a BitSet of all the indices of the object within the array starting at the index,
     *  an empty BitSet if not found or {@code null} array input
     * @since 3.10
     */
    public static BitSet indexesOf(final Object[] array, final Object objectToFind, int startIndex) {
        final BitSet bitSet = new BitSet();
        if (array == null) {
            return bitSet;
        }
        while (startIndex < array.length) {
            startIndex = indexOf(array, objectToFind, startIndex);
            if (startIndex == INDEX_NOT_FOUND) {
                break;
            }
            bitSet.set(startIndex);
            ++startIndex;
        }
        return bitSet;
    }

    /**
     * Finds the indices of the given value in the array.
     *
     * <p>This method returns an empty BitSet for a {@code null} input array.</p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @return a BitSet of all the indices of the value within the array,
     *  an empty BitSet if not found or {@code null} array input
     * @since 3.10
     */
    public static BitSet indexesOf(final short[] array, final short valueToFind) {
        return indexesOf(array, valueToFind, 0);
    }

    /**
     * Finds the indices of the given value in the array starting at the given index.
     *
     * <p>This method returns an empty BitSet for a {@code null} input array.</p>
     *
     * <p>A negative startIndex is treated as zero. A startIndex larger than the array
     * length will return an empty BitSet.</p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @param startIndex  the index to start searching at
     * @return a BitSet of all the indices of the value within the array,
     *  an empty BitSet if not found or {@code null} array input
     * @since 3.10
     */
    public static BitSet indexesOf(final short[] array, final short valueToFind, int startIndex) {
        final BitSet bitSet = new BitSet();
        if (array == null) {
            return bitSet;
        }
        while (startIndex < array.length) {
            startIndex = indexOf(array, valueToFind, startIndex);
            if (startIndex == INDEX_NOT_FOUND) {
                break;
            }
            bitSet.set(startIndex);
            ++startIndex;
        }
        return bitSet;
    }

    /**
     * Finds the index of the given value in the array.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @return the index of the value within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     */
    public static int indexOf(final boolean[] array, final boolean valueToFind) {
        return indexOf(array, valueToFind, 0);
    }

    /**
     * Finds the index of the given value in the array starting at the given index.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     * <p>
     * A negative startIndex is treated as zero. A startIndex larger than the array
     * length will return {@link #INDEX_NOT_FOUND} ({@code -1}).
     * </p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @param startIndex  the index to start searching at
     * @return the index of the value within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null}
     *  array input
     */
    public static int indexOf(final boolean[] array, final boolean valueToFind, final int startIndex) {
        if (isEmpty(array)) {
            return INDEX_NOT_FOUND;
        }
        for (int i = max0(startIndex); i < array.length; i++) {
            if (valueToFind == array[i]) {
                return i;
            }
        }
        return INDEX_NOT_FOUND;
    }

    /**
     * Finds the index of the given value in the array.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @return the index of the value within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     */
    public static int indexOf(final byte[] array, final byte valueToFind) {
        return indexOf(array, valueToFind, 0);
    }

    /**
     * Finds the index of the given value in the array starting at the given index.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     * <p>
     * A negative startIndex is treated as zero. A startIndex larger than the array
     * length will return {@link #INDEX_NOT_FOUND} ({@code -1}).
     * </p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @param startIndex  the index to start searching at
     * @return the index of the value within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     */
    public static int indexOf(final byte[] array, final byte valueToFind, final int startIndex) {
        if (array == null) {
            return INDEX_NOT_FOUND;
        }
        for (int i = max0(startIndex); i < array.length; i++) {
            if (valueToFind == array[i]) {
                return i;
            }
        }
        return INDEX_NOT_FOUND;
    }

    /**
     * Finds the index of the given value in the array.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @return the index of the value within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     * @since 2.1
     */
    public static int indexOf(final char[] array, final char valueToFind) {
        return indexOf(array, valueToFind, 0);
    }

    /**
     * Finds the index of the given value in the array starting at the given index.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     * <p>
     * A negative startIndex is treated as zero. A startIndex larger than the array
     * length will return {@link #INDEX_NOT_FOUND} ({@code -1}).
     * </p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @param startIndex  the index to start searching at
     * @return the index of the value within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     * @since 2.1
     */
    public static int indexOf(final char[] array, final char valueToFind, final int startIndex) {
        if (array == null) {
            return INDEX_NOT_FOUND;
        }
        for (int i = max0(startIndex); i < array.length; i++) {
            if (valueToFind == array[i]) {
                return i;
            }
        }
        return INDEX_NOT_FOUND;
    }

    /**
     * Finds the index of the given value in the array.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @return the index of the value within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     */
    public static int indexOf(final double[] array, final double valueToFind) {
        return indexOf(array, valueToFind, 0);
    }

    /**
     * Finds the index of the given value within a given tolerance in the array.
     * This method will return the index of the first value which falls between the region
     * defined by valueToFind - tolerance and valueToFind + tolerance.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @param tolerance tolerance of the search
     * @return the index of the value within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     */
    public static int indexOf(final double[] array, final double valueToFind, final double tolerance) {
        return indexOf(array, valueToFind, 0, tolerance);
    }

    /**
     * Finds the index of the given value in the array starting at the given index.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     * <p>
     * A negative startIndex is treated as zero. A startIndex larger than the array
     * length will return {@link #INDEX_NOT_FOUND} ({@code -1}).
     * </p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @param startIndex  the index to start searching at
     * @return the index of the value within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     */
    public static int indexOf(final double[] array, final double valueToFind, final int startIndex) {
        if (isEmpty(array)) {
            return INDEX_NOT_FOUND;
        }
        final boolean searchNaN = Double.isNaN(valueToFind);
        for (int i = max0(startIndex); i < array.length; i++) {
            final double element = array[i];
            if (valueToFind == element || searchNaN && Double.isNaN(element)) {
                return i;
            }
        }
        return INDEX_NOT_FOUND;
    }

    /**
     * Finds the index of the given value in the array starting at the given index.
     * This method will return the index of the first value which falls between the region
     * defined by valueToFind - tolerance and valueToFind + tolerance.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     * <p>
     * A negative startIndex is treated as zero. A startIndex larger than the array
     * length will return {@link #INDEX_NOT_FOUND} ({@code -1}).
     * </p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @param startIndex  the index to start searching at
     * @param tolerance tolerance of the search
     * @return the index of the value within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     */
    public static int indexOf(final double[] array, final double valueToFind, final int startIndex, final double tolerance) {
        if (isEmpty(array)) {
            return INDEX_NOT_FOUND;
        }
        final double min = valueToFind - tolerance;
        final double max = valueToFind + tolerance;
        for (int i = max0(startIndex); i < array.length; i++) {
            if (array[i] >= min && array[i] <= max) {
                return i;
            }
        }
        return INDEX_NOT_FOUND;
    }

    /**
     * Finds the index of the given value in the array.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @return the index of the value within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     */
    public static int indexOf(final float[] array, final float valueToFind) {
        return indexOf(array, valueToFind, 0);
    }

    /**
     * Finds the index of the given value in the array starting at the given index.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     * <p>
     * A negative startIndex is treated as zero. A startIndex larger than the array
     * length will return {@link #INDEX_NOT_FOUND} ({@code -1}).
     * </p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @param startIndex  the index to start searching at
     * @return the index of the value within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     */
    public static int indexOf(final float[] array, final float valueToFind, final int startIndex) {
        if (isEmpty(array)) {
            return INDEX_NOT_FOUND;
        }
        final boolean searchNaN = Float.isNaN(valueToFind);
        for (int i = max0(startIndex); i < array.length; i++) {
            final float element = array[i];
            if (valueToFind == element || searchNaN && Float.isNaN(element)) {
                return i;
            }
        }
        return INDEX_NOT_FOUND;
    }

    /**
     * Finds the index of the given value in the array.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @return the index of the value within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     */
    public static int indexOf(final int[] array, final int valueToFind) {
        return indexOf(array, valueToFind, 0);
    }

    /**
     * Finds the index of the given value in the array starting at the given index.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     * <p>
     * A negative startIndex is treated as zero. A startIndex larger than the array
     * length will return {@link #INDEX_NOT_FOUND} ({@code -1}).
     * </p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @param startIndex  the index to start searching at
     * @return the index of the value within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     */
    public static int indexOf(final int[] array, final int valueToFind, final int startIndex) {
        if (array == null) {
            return INDEX_NOT_FOUND;
        }
        for (int i = max0(startIndex); i < array.length; i++) {
            if (valueToFind == array[i]) {
                return i;
            }
        }
        return INDEX_NOT_FOUND;
    }

    /**
     * Finds the index of the given value in the array.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     *
     * @param array the array to search for the object, may be {@code null}
     * @param valueToFind the value to find
     * @return the index of the value within the array, {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null}
     *         array input
     */
    public static int indexOf(final long[] array, final long valueToFind) {
        return indexOf(array, valueToFind, 0);
    }

    /**
     * Finds the index of the given value in the array starting at the given index.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     * <p>
     * A negative startIndex is treated as zero. A startIndex larger than the array
     * length will return {@link #INDEX_NOT_FOUND} ({@code -1}).
     * </p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @param startIndex  the index to start searching at
     * @return the index of the value within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     */
    public static int indexOf(final long[] array, final long valueToFind, final int startIndex) {
        if (array == null) {
            return INDEX_NOT_FOUND;
        }
        for (int i = max0(startIndex); i < array.length; i++) {
            if (valueToFind == array[i]) {
                return i;
            }
        }
        return INDEX_NOT_FOUND;
    }

    /**
     * Finds the index of the given object in the array.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     *
     * @param array  the array to search for the object, may be {@code null}.
     * @param objectToFind  the object to find, may be {@code null}.
     * @return the index of the object within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     */
    public static int indexOf(final Object[] array, final Object objectToFind) {
        return indexOf(array, objectToFind, 0);
    }

    /**
     * Finds the index of the given object in the array starting at the given index.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     * <p>
     * A negative startIndex is treated as zero. A startIndex larger than the array
     * length will return {@link #INDEX_NOT_FOUND} ({@code -1}).
     * </p>
     *
     * @param array  the array to search for the object, may be {@code null}.
     * @param objectToFind  the object to find, may be {@code null}.
     * @param startIndex  the index to start searching at
     * @return the index of the object within the array starting at the index,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     */
    public static int indexOf(final Object[] array, final Object objectToFind, int startIndex) {
        if (array == null) {
            return INDEX_NOT_FOUND;
        }
        startIndex = max0(startIndex);
        if (objectToFind == null) {
            for (int i = startIndex; i < array.length; i++) {
                if (array[i] == null) {
                    return i;
                }
            }
        } else {
            for (int i = startIndex; i < array.length; i++) {
                if (objectToFind.equals(array[i])) {
                    return i;
                }
            }
        }
        return INDEX_NOT_FOUND;
    }

    /**
     * Finds the index of the given value in the array.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @return the index of the value within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     */
    public static int indexOf(final short[] array, final short valueToFind) {
        return indexOf(array, valueToFind, 0);
    }

    /**
     * Finds the index of the given value in the array starting at the given index.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     * <p>
     * A negative startIndex is treated as zero. A startIndex larger than the array
     * length will return {@link #INDEX_NOT_FOUND} ({@code -1}).
     * </p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @param startIndex  the index to start searching at
     * @return the index of the value within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     */
    public static int indexOf(final short[] array, final short valueToFind, final int startIndex) {
        if (array == null) {
            return INDEX_NOT_FOUND;
        }
        for (int i = max0(startIndex); i < array.length; i++) {
            if (valueToFind == array[i]) {
                return i;
            }
        }
        return INDEX_NOT_FOUND;
    }

    /**
     * Inserts elements into an array at the given index (starting from zero).
     *
     * <p>When an array is returned, it is always a new array.</p>
     *
     * <pre>
     * ArrayUtils.insert(index, null, null)      = null
     * ArrayUtils.insert(index, array, null)     = cloned copy of 'array'
     * ArrayUtils.insert(index, null, values)    = null
     * </pre>
     *
     * @param index the position within {@code array} to insert the new values
     * @param array the array to insert the values into, may be {@code null}
     * @param values the new values to insert, may be {@code null}
     * @return The new array or {@code null} if the given array is {@code null}.
     * @throws IndexOutOfBoundsException if {@code array} is provided
     * and either {@code index < 0} or {@code index > array.length}
     * @since 3.6
     */
    public static boolean[] insert(final int index, final boolean[] array, final boolean... values) {
        if (array == null) {
            return null;
        }
        if (isEmpty(values)) {
            return clone(array);
        }
        if (index < 0 || index > array.length) {
            throw new IndexOutOfBoundsException("Index: " + index + ", Length: " + array.length);
        }
        final boolean[] result = new boolean[array.length + values.length];
        System.arraycopy(values, 0, result, index, values.length);
        if (index > 0) {
            System.arraycopy(array, 0, result, 0, index);
        }
        if (index < array.length) {
            System.arraycopy(array, index, result, index + values.length, array.length - index);
        }
        return result;
    }

    /**
     * Inserts elements into an array at the given index (starting from zero).
     *
     * <p>When an array is returned, it is always a new array.</p>
     *
     * <pre>
     * ArrayUtils.insert(index, null, null)      = null
     * ArrayUtils.insert(index, array, null)     = cloned copy of 'array'
     * ArrayUtils.insert(index, null, values)    = null
     * </pre>
     *
     * @param index the position within {@code array} to insert the new values
     * @param array the array to insert the values into, may be {@code null}
     * @param values the new values to insert, may be {@code null}
     * @return The new array or {@code null} if the given array is {@code null}.
     * @throws IndexOutOfBoundsException if {@code array} is provided
     * and either {@code index < 0} or {@code index > array.length}
     * @since 3.6
     */
    public static byte[] insert(final int index, final byte[] array, final byte... values) {
        if (array == null) {
            return null;
        }
        if (isEmpty(values)) {
            return clone(array);
        }
        if (index < 0 || index > array.length) {
            throw new IndexOutOfBoundsException("Index: " + index + ", Length: " + array.length);
        }
        final byte[] result = new byte[array.length + values.length];
        System.arraycopy(values, 0, result, index, values.length);
        if (index > 0) {
            System.arraycopy(array, 0, result, 0, index);
        }
        if (index < array.length) {
            System.arraycopy(array, index, result, index + values.length, array.length - index);
        }
        return result;
    }

    /**
     * Inserts elements into an array at the given index (starting from zero).
     *
     * <p>When an array is returned, it is always a new array.</p>
     *
     * <pre>
     * ArrayUtils.insert(index, null, null)      = null
     * ArrayUtils.insert(index, array, null)     = cloned copy of 'array'
     * ArrayUtils.insert(index, null, values)    = null
     * </pre>
     *
     * @param index the position within {@code array} to insert the new values
     * @param array the array to insert the values into, may be {@code null}
     * @param values the new values to insert, may be {@code null}
     * @return The new array or {@code null} if the given array is {@code null}.
     * @throws IndexOutOfBoundsException if {@code array} is provided
     * and either {@code index < 0} or {@code index > array.length}
     * @since 3.6
     */
    public static char[] insert(final int index, final char[] array, final char... values) {
        if (array == null) {
            return null;
        }
        if (isEmpty(values)) {
            return clone(array);
        }
        if (index < 0 || index > array.length) {
            throw new IndexOutOfBoundsException("Index: " + index + ", Length: " + array.length);
        }
        final char[] result = new char[array.length + values.length];
        System.arraycopy(values, 0, result, index, values.length);
        if (index > 0) {
            System.arraycopy(array, 0, result, 0, index);
        }
        if (index < array.length) {
            System.arraycopy(array, index, result, index + values.length, array.length - index);
        }
        return result;
    }

    /**
     * Inserts elements into an array at the given index (starting from zero).
     *
     * <p>When an array is returned, it is always a new array.</p>
     *
     * <pre>
     * ArrayUtils.insert(index, null, null)      = null
     * ArrayUtils.insert(index, array, null)     = cloned copy of 'array'
     * ArrayUtils.insert(index, null, values)    = null
     * </pre>
     *
     * @param index the position within {@code array} to insert the new values
     * @param array the array to insert the values into, may be {@code null}
     * @param values the new values to insert, may be {@code null}
     * @return The new array or {@code null} if the given array is {@code null}.
     * @throws IndexOutOfBoundsException if {@code array} is provided
     * and either {@code index < 0} or {@code index > array.length}
     * @since 3.6
     */
    public static double[] insert(final int index, final double[] array, final double... values) {
        if (array == null) {
            return null;
        }
        if (isEmpty(values)) {
            return clone(array);
        }
        if (index < 0 || index > array.length) {
            throw new IndexOutOfBoundsException("Index: " + index + ", Length: " + array.length);
        }
        final double[] result = new double[array.length + values.length];
        System.arraycopy(values, 0, result, index, values.length);
        if (index > 0) {
            System.arraycopy(array, 0, result, 0, index);
        }
        if (index < array.length) {
            System.arraycopy(array, index, result, index + values.length, array.length - index);
        }
        return result;
    }

    /**
     * Inserts elements into an array at the given index (starting from zero).
     *
     * <p>When an array is returned, it is always a new array.</p>
     *
     * <pre>
     * ArrayUtils.insert(index, null, null)      = null
     * ArrayUtils.insert(index, array, null)     = cloned copy of 'array'
     * ArrayUtils.insert(index, null, values)    = null
     * </pre>
     *
     * @param index the position within {@code array} to insert the new values
     * @param array the array to insert the values into, may be {@code null}
     * @param values the new values to insert, may be {@code null}
     * @return The new array or {@code null} if the given array is {@code null}.
     * @throws IndexOutOfBoundsException if {@code array} is provided
     * and either {@code index < 0} or {@code index > array.length}
     * @since 3.6
     */
    public static float[] insert(final int index, final float[] array, final float... values) {
        if (array == null) {
            return null;
        }
        if (isEmpty(values)) {
            return clone(array);
        }
        if (index < 0 || index > array.length) {
            throw new IndexOutOfBoundsException("Index: " + index + ", Length: " + array.length);
        }
        final float[] result = new float[array.length + values.length];
        System.arraycopy(values, 0, result, index, values.length);
        if (index > 0) {
            System.arraycopy(array, 0, result, 0, index);
        }
        if (index < array.length) {
            System.arraycopy(array, index, result, index + values.length, array.length - index);
        }
        return result;
    }

    /**
     * Inserts elements into an array at the given index (starting from zero).
     *
     * <p>When an array is returned, it is always a new array.</p>
     *
     * <pre>
     * ArrayUtils.insert(index, null, null)      = null
     * ArrayUtils.insert(index, array, null)     = cloned copy of 'array'
     * ArrayUtils.insert(index, null, values)    = null
     * </pre>
     *
     * @param index the position within {@code array} to insert the new values
     * @param array the array to insert the values into, may be {@code null}
     * @param values the new values to insert, may be {@code null}
     * @return The new array or {@code null} if the given array is {@code null}.
     * @throws IndexOutOfBoundsException if {@code array} is provided
     * and either {@code index < 0} or {@code index > array.length}
     * @since 3.6
     */
    public static int[] insert(final int index, final int[] array, final int... values) {
        if (array == null) {
            return null;
        }
        if (isEmpty(values)) {
            return clone(array);
        }
        if (index < 0 || index > array.length) {
            throw new IndexOutOfBoundsException("Index: " + index + ", Length: " + array.length);
        }
        final int[] result = new int[array.length + values.length];
        System.arraycopy(values, 0, result, index, values.length);
        if (index > 0) {
            System.arraycopy(array, 0, result, 0, index);
        }
        if (index < array.length) {
            System.arraycopy(array, index, result, index + values.length, array.length - index);
        }
        return result;
    }

    /**
     * Inserts elements into an array at the given index (starting from zero).
     *
     * <p>When an array is returned, it is always a new array.</p>
     *
     * <pre>
     * ArrayUtils.insert(index, null, null)      = null
     * ArrayUtils.insert(index, array, null)     = cloned copy of 'array'
     * ArrayUtils.insert(index, null, values)    = null
     * </pre>
     *
     * @param index the position within {@code array} to insert the new values
     * @param array the array to insert the values into, may be {@code null}
     * @param values the new values to insert, may be {@code null}
     * @return The new array or {@code null} if the given array is {@code null}.
     * @throws IndexOutOfBoundsException if {@code array} is provided
     * and either {@code index < 0} or {@code index > array.length}
     * @since 3.6
     */
    public static long[] insert(final int index, final long[] array, final long... values) {
        if (array == null) {
            return null;
        }
        if (isEmpty(values)) {
            return clone(array);
        }
        if (index < 0 || index > array.length) {
            throw new IndexOutOfBoundsException("Index: " + index + ", Length: " + array.length);
        }
        final long[] result = new long[array.length + values.length];
        System.arraycopy(values, 0, result, index, values.length);
        if (index > 0) {
            System.arraycopy(array, 0, result, 0, index);
        }
        if (index < array.length) {
            System.arraycopy(array, index, result, index + values.length, array.length - index);
        }
        return result;
    }

    /**
     * Inserts elements into an array at the given index (starting from zero).
     *
     * <p>When an array is returned, it is always a new array.</p>
     *
     * <pre>
     * ArrayUtils.insert(index, null, null)      = null
     * ArrayUtils.insert(index, array, null)     = cloned copy of 'array'
     * ArrayUtils.insert(index, null, values)    = null
     * </pre>
     *
     * @param index the position within {@code array} to insert the new values
     * @param array the array to insert the values into, may be {@code null}
     * @param values the new values to insert, may be {@code null}
     * @return The new array or {@code null} if the given array is {@code null}.
     * @throws IndexOutOfBoundsException if {@code array} is provided
     * and either {@code index < 0} or {@code index > array.length}
     * @since 3.6
     */
    public static short[] insert(final int index, final short[] array, final short... values) {
        if (array == null) {
            return null;
        }
        if (isEmpty(values)) {
            return clone(array);
        }
        if (index < 0 || index > array.length) {
            throw new IndexOutOfBoundsException("Index: " + index + ", Length: " + array.length);
        }
        final short[] result = new short[array.length + values.length];
        System.arraycopy(values, 0, result, index, values.length);
        if (index > 0) {
            System.arraycopy(array, 0, result, 0, index);
        }
        if (index < array.length) {
            System.arraycopy(array, index, result, index + values.length, array.length - index);
        }
        return result;
    }

    /**
     * Inserts elements into an array at the given index (starting from zero).
     *
     * <p>When an array is returned, it is always a new array.</p>
     *
     * <pre>
     * ArrayUtils.insert(index, null, null)      = null
     * ArrayUtils.insert(index, array, null)     = cloned copy of 'array'
     * ArrayUtils.insert(index, null, values)    = null
     * </pre>
     *
     * @param <T> The type of elements in {@code array} and {@code values}
     * @param index the position within {@code array} to insert the new values
     * @param array the array to insert the values into, may be {@code null}
     * @param values the new values to insert, may be {@code null}
     * @return The new array or {@code null} if the given array is {@code null}.
     * @throws IndexOutOfBoundsException if {@code array} is provided
     * and either {@code index < 0} or {@code index > array.length}
     * @since 3.6
     */
    @SafeVarargs
    public static <T> T[] insert(final int index, final T[] array, final T... values) {
        /*
         * Note on use of @SafeVarargs:
         *
         * By returning null when 'array' is null, we avoid returning the vararg
         * array to the caller. We also avoid relying on the type of the vararg
         * array, by inspecting the component type of 'array'.
         */
        if (array == null) {
            return null;
        }
        if (isEmpty(values)) {
            return clone(array);
        }
        if (index < 0 || index > array.length) {
            throw new IndexOutOfBoundsException("Index: " + index + ", Length: " + array.length);
        }
        final Class<T> type = getComponentType(array);
        final int length = array.length + values.length;
        final T[] result = newInstance(type, length);
        System.arraycopy(values, 0, result, index, values.length);
        if (index > 0) {
            System.arraycopy(array, 0, result, 0, index);
        }
        if (index < array.length) {
            System.arraycopy(array, index, result, index + values.length, array.length - index);
        }
        return result;
    }

    /**
     * Checks if an array is empty or {@code null}.
     *
     * @param array the array to test
     * @return {@code true} if the array is empty or {@code null}
     */
    private static boolean isArrayEmpty(final Object array) {
        return getLength(array) == 0;
    }

    /**
     * Returns whether a given array can safely be accessed at the given index.
     *
     * <pre>
     * ArrayUtils.isArrayIndexValid(null, 0)       = false
     * ArrayUtils.isArrayIndexValid([], 0)         = false
     * ArrayUtils.isArrayIndexValid(["a"], 0)      = true
     * </pre>
     *
     * @param <T> the component type of the array
     * @param array the array to inspect, may be {@code null}.
     * @param index the index of the array to be inspected
     * @return Whether the given index is safely-accessible in the given array
     * @since 3.8
     */
    public static <T> boolean isArrayIndexValid(final T[] array, final int index) {
        return index >= 0 && getLength(array) > index;
    }

    /**
     * Checks if an array of primitive booleans is empty or {@code null}.
     *
     * @param array  the array to test
     * @return {@code true} if the array is empty or {@code null}
     * @since 2.1
     */
    public static boolean isEmpty(final boolean[] array) {
        return isArrayEmpty(array);
    }

    /**
     * Checks if an array of primitive bytes is empty or {@code null}.
     *
     * @param array  the array to test
     * @return {@code true} if the array is empty or {@code null}
     * @since 2.1
     */
    public static boolean isEmpty(final byte[] array) {
        return isArrayEmpty(array);
    }

    /**
     * Checks if an array of primitive chars is empty or {@code null}.
     *
     * @param array  the array to test
     * @return {@code true} if the array is empty or {@code null}
     * @since 2.1
     */
    public static boolean isEmpty(final char[] array) {
        return isArrayEmpty(array);
    }

    /**
     * Checks if an array of primitive doubles is empty or {@code null}.
     *
     * @param array  the array to test
     * @return {@code true} if the array is empty or {@code null}
     * @since 2.1
     */
    public static boolean isEmpty(final double[] array) {
        return isArrayEmpty(array);
    }

    /**
     * Checks if an array of primitive floats is empty or {@code null}.
     *
     * @param array  the array to test
     * @return {@code true} if the array is empty or {@code null}
     * @since 2.1
     */
    public static boolean isEmpty(final float[] array) {
        return isArrayEmpty(array);
    }

    /**
     * Checks if an array of primitive ints is empty or {@code null}.
     *
     * @param array  the array to test
     * @return {@code true} if the array is empty or {@code null}
     * @since 2.1
     */
    public static boolean isEmpty(final int[] array) {
        return isArrayEmpty(array);
    }

    /**
     * Checks if an array of primitive longs is empty or {@code null}.
     *
     * @param array  the array to test
     * @return {@code true} if the array is empty or {@code null}
     * @since 2.1
     */
    public static boolean isEmpty(final long[] array) {
        return isArrayEmpty(array);
    }

    /**
     * Checks if an array of Objects is empty or {@code null}.
     *
     * @param array  the array to test
     * @return {@code true} if the array is empty or {@code null}
     * @since 2.1
     */
    public static boolean isEmpty(final Object[] array) {
        return isArrayEmpty(array);
    }

    /**
     * Checks if an array of primitive shorts is empty or {@code null}.
     *
     * @param array  the array to test
     * @return {@code true} if the array is empty or {@code null}
     * @since 2.1
     */
    public static boolean isEmpty(final short[] array) {
        return isArrayEmpty(array);
    }

    /**
     * Compares two arrays, using equals(), handling multidimensional arrays
     * correctly.
     * <p>
     * Multi-dimensional primitive arrays are also handled correctly by this method.
     * </p>
     *
     * @param array1  the left-hand side array to compare, may be {@code null}
     * @param array2  the right-hand side array to compare, may be {@code null}
     * @return {@code true} if the arrays are equal
     * @deprecated this method has been replaced by {@code java.util.Objects.deepEquals(Object, Object)} and will be
     * removed from future releases.
     */
    @Deprecated
    public static boolean isEquals(final Object array1, final Object array2) {
        return new EqualsBuilder().append(array1, array2).isEquals();
    }

    /**
     * Checks if an array of primitive booleans is not empty and not {@code null}.
     *
     * @param array  the array to test
     * @return {@code true} if the array is not empty and not {@code null}
     * @since 2.5
     */
    public static boolean isNotEmpty(final boolean[] array) {
        return !isEmpty(array);
    }

    /**
     * Checks if an array of primitive bytes is not empty and not {@code null}.
     *
     * @param array  the array to test
     * @return {@code true} if the array is not empty and not {@code null}
     * @since 2.5
     */
    public static boolean isNotEmpty(final byte[] array) {
        return !isEmpty(array);
    }

    /**
     * Checks if an array of primitive chars is not empty and not {@code null}.
     *
     * @param array  the array to test
     * @return {@code true} if the array is not empty and not {@code null}
     * @since 2.5
     */
    public static boolean isNotEmpty(final char[] array) {
        return !isEmpty(array);
    }

    /**
     * Checks if an array of primitive doubles is not empty and not {@code null}.
     *
     * @param array  the array to test
     * @return {@code true} if the array is not empty and not {@code null}
     * @since 2.5
     */
    public static boolean isNotEmpty(final double[] array) {
        return !isEmpty(array);
    }

    /**
     * Checks if an array of primitive floats is not empty and not {@code null}.
     *
     * @param array  the array to test
     * @return {@code true} if the array is not empty and not {@code null}
     * @since 2.5
     */
    public static boolean isNotEmpty(final float[] array) {
        return !isEmpty(array);
    }

    /**
     * Checks if an array of primitive ints is not empty and not {@code null}.
     *
     * @param array  the array to test
     * @return {@code true} if the array is not empty and not {@code null}
     * @since 2.5
     */
    public static boolean isNotEmpty(final int[] array) {
        return !isEmpty(array);
    }

    /**
     * Checks if an array of primitive longs is not empty and not {@code null}.
     *
     * @param array  the array to test
     * @return {@code true} if the array is not empty and not {@code null}
     * @since 2.5
     */
    public static boolean isNotEmpty(final long[] array) {
        return !isEmpty(array);
    }

    /**
     * Checks if an array of primitive shorts is not empty and not {@code null}.
     *
     * @param array  the array to test
     * @return {@code true} if the array is not empty and not {@code null}
     * @since 2.5
     */
    public static boolean isNotEmpty(final short[] array) {
        return !isEmpty(array);
    }

    /**
     * Checks if an array of Objects is not empty and not {@code null}.
     *
     * @param <T> the component type of the array
     * @param array  the array to test
     * @return {@code true} if the array is not empty and not {@code null}
     * @since 2.5
     */
     public static <T> boolean isNotEmpty(final T[] array) {
         return !isEmpty(array);
     }

    /**
     * Checks whether two arrays are the same length, treating
     * {@code null} arrays as length {@code 0}.
     *
     * @param array1 the first array, may be {@code null}
     * @param array2 the second array, may be {@code null}
     * @return {@code true} if length of arrays matches, treating
     *  {@code null} as an empty array
     */
    public static boolean isSameLength(final boolean[] array1, final boolean[] array2) {
        return getLength(array1) == getLength(array2);
    }

    /**
     * Checks whether two arrays are the same length, treating
     * {@code null} arrays as length {@code 0}.
     *
     * @param array1 the first array, may be {@code null}
     * @param array2 the second array, may be {@code null}
     * @return {@code true} if length of arrays matches, treating
     *  {@code null} as an empty array
     */
    public static boolean isSameLength(final byte[] array1, final byte[] array2) {
        return getLength(array1) == getLength(array2);
    }

    /**
     * Checks whether two arrays are the same length, treating
     * {@code null} arrays as length {@code 0}.
     *
     * @param array1 the first array, may be {@code null}
     * @param array2 the second array, may be {@code null}
     * @return {@code true} if length of arrays matches, treating
     *  {@code null} as an empty array
     */
    public static boolean isSameLength(final char[] array1, final char[] array2) {
        return getLength(array1) == getLength(array2);
    }

    /**
     * Checks whether two arrays are the same length, treating
     * {@code null} arrays as length {@code 0}.
     *
     * @param array1 the first array, may be {@code null}
     * @param array2 the second array, may be {@code null}
     * @return {@code true} if length of arrays matches, treating
     *  {@code null} as an empty array
     */
    public static boolean isSameLength(final double[] array1, final double[] array2) {
        return getLength(array1) == getLength(array2);
    }

    /**
     * Checks whether two arrays are the same length, treating
     * {@code null} arrays as length {@code 0}.
     *
     * @param array1 the first array, may be {@code null}
     * @param array2 the second array, may be {@code null}
     * @return {@code true} if length of arrays matches, treating
     *  {@code null} as an empty array
     */
    public static boolean isSameLength(final float[] array1, final float[] array2) {
        return getLength(array1) == getLength(array2);
    }

    /**
     * Checks whether two arrays are the same length, treating
     * {@code null} arrays as length {@code 0}.
     *
     * @param array1 the first array, may be {@code null}
     * @param array2 the second array, may be {@code null}
     * @return {@code true} if length of arrays matches, treating
     *  {@code null} as an empty array
     */
    public static boolean isSameLength(final int[] array1, final int[] array2) {
        return getLength(array1) == getLength(array2);
    }

    /**
     * Checks whether two arrays are the same length, treating
     * {@code null} arrays as length {@code 0}.
     *
     * @param array1 the first array, may be {@code null}
     * @param array2 the second array, may be {@code null}
     * @return {@code true} if length of arrays matches, treating
     *  {@code null} as an empty array
     */
    public static boolean isSameLength(final long[] array1, final long[] array2) {
        return getLength(array1) == getLength(array2);
    }

    /**
     * Checks whether two arrays are the same length, treating
     * {@code null} arrays as length {@code 0}.
     * <p>
     * Any multi-dimensional aspects of the arrays are ignored.
     * </p>
     *
     * @param array1 the first array, may be {@code null}
     * @param array2 the second array, may be {@code null}
     * @return {@code true} if length of arrays matches, treating
     *  {@code null} as an empty array
     * @since 3.11
     */
    public static boolean isSameLength(final Object array1, final Object array2) {
        return getLength(array1) == getLength(array2);
    }

    /**
     * Checks whether two arrays are the same length, treating
     * {@code null} arrays as length {@code 0}.
     * <p>
     * Any multi-dimensional aspects of the arrays are ignored.
     * </p>
     *
     * @param array1 the first array, may be {@code null}
     * @param array2 the second array, may be {@code null}
     * @return {@code true} if length of arrays matches, treating
     *  {@code null} as an empty array
     */
    public static boolean isSameLength(final Object[] array1, final Object[] array2) {
        return getLength(array1) == getLength(array2);
    }

    /**
     * Checks whether two arrays are the same length, treating
     * {@code null} arrays as length {@code 0}.
     *
     * @param array1 the first array, may be {@code null}
     * @param array2 the second array, may be {@code null}
     * @return {@code true} if length of arrays matches, treating
     *  {@code null} as an empty array
     */
    public static boolean isSameLength(final short[] array1, final short[] array2) {
        return getLength(array1) == getLength(array2);
    }

    /**
     * Checks whether two arrays are the same type taking into account
     * multidimensional arrays.
     *
     * @param array1 the first array, must not be {@code null}
     * @param array2 the second array, must not be {@code null}
     * @return {@code true} if type of arrays matches
     * @throws IllegalArgumentException if either array is {@code null}
     */
    public static boolean isSameType(final Object array1, final Object array2) {
        if (array1 == null || array2 == null) {
            throw new IllegalArgumentException("The Array must not be null");
        }
        return array1.getClass().getName().equals(array2.getClass().getName());
    }

    /**
     * This method checks whether the provided array is sorted according to natural ordering
     * ({@code false} before {@code true}).
     *
     * @param array the array to check
     * @return whether the array is sorted according to natural ordering
     * @since 3.4
     */
    public static boolean isSorted(final boolean[] array) {
        if (getLength(array) < 2) {
            return true;
        }
        boolean previous = array[0];
        final int n = array.length;
        for (int i = 1; i < n; i++) {
            final boolean current = array[i];
            if (BooleanUtils.compare(previous, current) > 0) {
                return false;
            }
            previous = current;
        }
        return true;
    }

    /**
     * Checks whether the provided array is sorted according to natural ordering.
     *
     * @param array the array to check
     * @return whether the array is sorted according to natural ordering
     * @since 3.4
     */
    public static boolean isSorted(final byte[] array) {
        if (getLength(array) < 2) {
            return true;
        }
        byte previous = array[0];
        final int n = array.length;
        for (int i = 1; i < n; i++) {
            final byte current = array[i];
            if (NumberUtils.compare(previous, current) > 0) {
                return false;
            }
            previous = current;
        }
        return true;
    }

    /**
     * Checks whether the provided array is sorted according to natural ordering.
     *
     * @param array the array to check
     * @return whether the array is sorted according to natural ordering
     * @since 3.4
     */
    public static boolean isSorted(final char[] array) {
        if (getLength(array) < 2) {
            return true;
        }
        char previous = array[0];
        final int n = array.length;
        for (int i = 1; i < n; i++) {
            final char current = array[i];
            if (CharUtils.compare(previous, current) > 0) {
                return false;
            }
            previous = current;
        }
        return true;
    }

    /**
     * This method checks whether the provided array is sorted according to natural ordering.
     *
     * @param array the array to check
     * @return whether the array is sorted according to natural ordering
     * @since 3.4
     */
    public static boolean isSorted(final double[] array) {
        if (getLength(array) < 2) {
            return true;
        }
        double previous = array[0];
        final int n = array.length;
        for (int i = 1; i < n; i++) {
            final double current = array[i];
            if (Double.compare(previous, current) > 0) {
                return false;
            }
            previous = current;
        }
        return true;
    }

    /**
     * This method checks whether the provided array is sorted according to natural ordering.
     *
     * @param array the array to check
     * @return whether the array is sorted according to natural ordering
     * @since 3.4
     */
    public static boolean isSorted(final float[] array) {
        if (getLength(array) < 2) {
            return true;
        }
        float previous = array[0];
        final int n = array.length;
        for (int i = 1; i < n; i++) {
            final float current = array[i];
            if (Float.compare(previous, current) > 0) {
                return false;
            }
            previous = current;
        }
        return true;
    }

    /**
     * This method checks whether the provided array is sorted according to natural ordering.
     *
     * @param array the array to check
     * @return whether the array is sorted according to natural ordering
     * @since 3.4
     */
    public static boolean isSorted(final int[] array) {
        if (getLength(array) < 2) {
            return true;
        }
        int previous = array[0];
        final int n = array.length;
        for (int i = 1; i < n; i++) {
            final int current = array[i];
            if (NumberUtils.compare(previous, current) > 0) {
                return false;
            }
            previous = current;
        }
        return true;
    }

    /**
     * This method checks whether the provided array is sorted according to natural ordering.
     *
     * @param array the array to check
     * @return whether the array is sorted according to natural ordering
     * @since 3.4
     */
    public static boolean isSorted(final long[] array) {
        if (getLength(array) < 2) {
            return true;
        }
        long previous = array[0];
        final int n = array.length;
        for (int i = 1; i < n; i++) {
            final long current = array[i];
            if (NumberUtils.compare(previous, current) > 0) {
                return false;
            }
            previous = current;
        }
        return true;
    }

    /**
     * This method checks whether the provided array is sorted according to natural ordering.
     *
     * @param array the array to check
     * @return whether the array is sorted according to natural ordering
     * @since 3.4
     */
    public static boolean isSorted(final short[] array) {
        if (getLength(array) < 2) {
            return true;
        }
        short previous = array[0];
        final int n = array.length;
        for (int i = 1; i < n; i++) {
            final short current = array[i];
            if (NumberUtils.compare(previous, current) > 0) {
                return false;
            }
            previous = current;
        }
        return true;
    }

    /**
     * This method checks whether the provided array is sorted according to the class's
     * {@code compareTo} method.
     *
     * @param array the array to check
     * @param <T> the datatype of the array to check, it must implement {@link Comparable}
     * @return whether the array is sorted
     * @since 3.4
     */
    public static <T extends Comparable<? super T>> boolean isSorted(final T[] array) {
        return isSorted(array, Comparable::compareTo);
    }

    /**
     * This method checks whether the provided array is sorted according to the provided {@link Comparator}.
     *
     * @param array the array to check
     * @param comparator the {@link Comparator} to compare over
     * @param <T> the datatype of the array
     * @return whether the array is sorted
     * @throws NullPointerException if {@code comparator} is {@code null}
     * @since 3.4
     */
    public static <T> boolean isSorted(final T[] array, final Comparator<T> comparator) {
        Objects.requireNonNull(comparator, "comparator");
        if (getLength(array) < 2) {
            return true;
        }
        T previous = array[0];
        final int n = array.length;
        for (int i = 1; i < n; i++) {
            final T current = array[i];
            if (comparator.compare(previous, current) > 0) {
                return false;
            }
            previous = current;
        }
        return true;
    }

    /**
     * Finds the last index of the given value within the array.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) if
     * {@code null} array input.
     * </p>
     *
     * @param array  the array to traverse backwards looking for the object, may be {@code null}
     * @param valueToFind  the object to find
     * @return the last index of the value within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     */
    public static int lastIndexOf(final boolean[] array, final boolean valueToFind) {
        return lastIndexOf(array, valueToFind, Integer.MAX_VALUE);
    }

    /**
     * Finds the last index of the given value in the array starting at the given index.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     * <p>
     * A negative startIndex will return {@link #INDEX_NOT_FOUND} ({@code -1}). A startIndex larger than
     * the array length will search from the end of the array.
     * </p>
     *
     * @param array  the array to traverse for looking for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @param startIndex  the start index to traverse backwards from
     * @return the last index of the value within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     */
    public static int lastIndexOf(final boolean[] array, final boolean valueToFind, int startIndex) {
        if (isEmpty(array) || startIndex < 0) {
            return INDEX_NOT_FOUND;
        }
        if (startIndex >= array.length) {
            startIndex = array.length - 1;
        }
        for (int i = startIndex; i >= 0; i--) {
            if (valueToFind == array[i]) {
                return i;
            }
        }
        return INDEX_NOT_FOUND;
    }

    /**
     * Finds the last index of the given value within the array.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     *
     * @param array  the array to traverse backwards looking for the object, may be {@code null}
     * @param valueToFind  the object to find
     * @return the last index of the value within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     */
    public static int lastIndexOf(final byte[] array, final byte valueToFind) {
        return lastIndexOf(array, valueToFind, Integer.MAX_VALUE);
    }

    /**
     * Finds the last index of the given value in the array starting at the given index.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     * <p>
     * A negative startIndex will return {@link #INDEX_NOT_FOUND} ({@code -1}). A startIndex larger than the
     * array length will search from the end of the array.
     * </p>
     *
     * @param array  the array to traverse for looking for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @param startIndex  the start index to traverse backwards from
     * @return the last index of the value within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     */
    public static int lastIndexOf(final byte[] array, final byte valueToFind, int startIndex) {
        if (array == null || startIndex < 0) {
            return INDEX_NOT_FOUND;
        }
        if (startIndex >= array.length) {
            startIndex = array.length - 1;
        }
        for (int i = startIndex; i >= 0; i--) {
            if (valueToFind == array[i]) {
                return i;
            }
        }
        return INDEX_NOT_FOUND;
    }

    /**
     * Finds the last index of the given value within the array.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     *
     * @param array  the array to traverse backwards looking for the object, may be {@code null}
     * @param valueToFind  the object to find
     * @return the last index of the value within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     * @since 2.1
     */
    public static int lastIndexOf(final char[] array, final char valueToFind) {
        return lastIndexOf(array, valueToFind, Integer.MAX_VALUE);
    }

    /**
     * Finds the last index of the given value in the array starting at the given index.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     * <p>
     * A negative startIndex will return {@link #INDEX_NOT_FOUND} ({@code -1}). A startIndex larger than the
     * array length will search from the end of the array.
     * </p>
     *
     * @param array  the array to traverse for looking for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @param startIndex  the start index to traverse backwards from
     * @return the last index of the value within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     * @since 2.1
     */
    public static int lastIndexOf(final char[] array, final char valueToFind, int startIndex) {
        if (array == null || startIndex < 0) {
            return INDEX_NOT_FOUND;
        }
        if (startIndex >= array.length) {
            startIndex = array.length - 1;
        }
        for (int i = startIndex; i >= 0; i--) {
            if (valueToFind == array[i]) {
                return i;
            }
        }
        return INDEX_NOT_FOUND;
    }

    /**
     * Finds the last index of the given value within the array.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     *
     * @param array  the array to traverse backwards looking for the object, may be {@code null}
     * @param valueToFind  the object to find
     * @return the last index of the value within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     */
    public static int lastIndexOf(final double[] array, final double valueToFind) {
        return lastIndexOf(array, valueToFind, Integer.MAX_VALUE);
    }

    /**
     * Finds the last index of the given value within a given tolerance in the array.
     * This method will return the index of the last value which falls between the region
     * defined by valueToFind - tolerance and valueToFind + tolerance.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     *
     * @param array  the array to search for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @param tolerance tolerance of the search
     * @return the index of the value within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     */
    public static int lastIndexOf(final double[] array, final double valueToFind, final double tolerance) {
        return lastIndexOf(array, valueToFind, Integer.MAX_VALUE, tolerance);
    }

    /**
     * Finds the last index of the given value in the array starting at the given index.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     * <p>
     * A negative startIndex will return {@link #INDEX_NOT_FOUND} ({@code -1}). A startIndex larger than the
     * array length will search from the end of the array.
     * </p>
     *
     * @param array  the array to traverse for looking for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @param startIndex  the start index to traverse backwards from
     * @return the last index of the value within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     */
    public static int lastIndexOf(final double[] array, final double valueToFind, int startIndex) {
        if (isEmpty(array) || startIndex < 0) {
            return INDEX_NOT_FOUND;
        }
        if (startIndex >= array.length) {
            startIndex = array.length - 1;
        }
        for (int i = startIndex; i >= 0; i--) {
            if (valueToFind == array[i]) {
                return i;
            }
        }
        return INDEX_NOT_FOUND;
    }

    /**
     * Finds the last index of the given value in the array starting at the given index.
     * This method will return the index of the last value which falls between the region
     * defined by valueToFind - tolerance and valueToFind + tolerance.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     * <p>
     * A negative startIndex will return {@link #INDEX_NOT_FOUND} ({@code -1}). A startIndex larger than the
     * array length will search from the end of the array.
     * </p>
     *
     * @param array  the array to traverse for looking for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @param startIndex  the start index to traverse backwards from
     * @param tolerance  search for value within plus/minus this amount
     * @return the last index of the value within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     */
    public static int lastIndexOf(final double[] array, final double valueToFind, int startIndex, final double tolerance) {
        if (isEmpty(array) || startIndex < 0) {
            return INDEX_NOT_FOUND;
        }
        if (startIndex >= array.length) {
            startIndex = array.length - 1;
        }
        final double min = valueToFind - tolerance;
        final double max = valueToFind + tolerance;
        for (int i = startIndex; i >= 0; i--) {
            if (array[i] >= min && array[i] <= max) {
                return i;
            }
        }
        return INDEX_NOT_FOUND;
    }

    /**
     * Finds the last index of the given value within the array.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     *
     * @param array  the array to traverse backwards looking for the object, may be {@code null}
     * @param valueToFind  the object to find
     * @return the last index of the value within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     */
    public static int lastIndexOf(final float[] array, final float valueToFind) {
        return lastIndexOf(array, valueToFind, Integer.MAX_VALUE);
    }

    /**
     * Finds the last index of the given value in the array starting at the given index.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     * <p>
     * A negative startIndex will return {@link #INDEX_NOT_FOUND} ({@code -1}). A startIndex larger than the
     * array length will search from the end of the array.
     * </p>
     *
     * @param array  the array to traverse for looking for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @param startIndex  the start index to traverse backwards from
     * @return the last index of the value within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     */
    public static int lastIndexOf(final float[] array, final float valueToFind, int startIndex) {
        if (isEmpty(array) || startIndex < 0) {
            return INDEX_NOT_FOUND;
        }
        if (startIndex >= array.length) {
            startIndex = array.length - 1;
        }
        for (int i = startIndex; i >= 0; i--) {
            if (valueToFind == array[i]) {
                return i;
            }
        }
        return INDEX_NOT_FOUND;
    }

    /**
     * Finds the last index of the given value within the array.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     *
     * @param array  the array to traverse backwards looking for the object, may be {@code null}
     * @param valueToFind  the object to find
     * @return the last index of the value within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     */
    public static int lastIndexOf(final int[] array, final int valueToFind) {
        return lastIndexOf(array, valueToFind, Integer.MAX_VALUE);
    }

    /**
     * Finds the last index of the given value in the array starting at the given index.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     * <p>
     * A negative startIndex will return {@link #INDEX_NOT_FOUND} ({@code -1}). A startIndex larger than the
     * array length will search from the end of the array.
     * </p>
     *
     * @param array  the array to traverse for looking for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @param startIndex  the start index to traverse backwards from
     * @return the last index of the value within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     */
    public static int lastIndexOf(final int[] array, final int valueToFind, int startIndex) {
        if (array == null || startIndex < 0) {
            return INDEX_NOT_FOUND;
        }
        if (startIndex >= array.length) {
            startIndex = array.length - 1;
        }
        for (int i = startIndex; i >= 0; i--) {
            if (valueToFind == array[i]) {
                return i;
            }
        }
        return INDEX_NOT_FOUND;
    }

    /**
     * Finds the last index of the given value within the array.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     *
     * @param array  the array to traverse backwards looking for the object, may be {@code null}
     * @param valueToFind  the object to find
     * @return the last index of the value within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     */
    public static int lastIndexOf(final long[] array, final long valueToFind) {
        return lastIndexOf(array, valueToFind, Integer.MAX_VALUE);
    }

    /**
     * Finds the last index of the given value in the array starting at the given index.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     * <p>
     * A negative startIndex will return {@link #INDEX_NOT_FOUND} ({@code -1}). A startIndex larger than the
     * array length will search from the end of the array.
     * </p>
     *
     * @param array  the array to traverse for looking for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @param startIndex  the start index to traverse backwards from
     * @return the last index of the value within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     */
    public static int lastIndexOf(final long[] array, final long valueToFind, int startIndex) {
        if (array == null || startIndex < 0) {
            return INDEX_NOT_FOUND;
        }
        if (startIndex >= array.length) {
            startIndex = array.length - 1;
        }
        for (int i = startIndex; i >= 0; i--) {
            if (valueToFind == array[i]) {
                return i;
            }
        }
        return INDEX_NOT_FOUND;
    }

    /**
     * Finds the last index of the given object within the array.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     *
     * @param array  the array to traverse backwards looking for the object, may be {@code null}
     * @param objectToFind  the object to find, may be {@code null}
     * @return the last index of the object within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     */
    public static int lastIndexOf(final Object[] array, final Object objectToFind) {
        return lastIndexOf(array, objectToFind, Integer.MAX_VALUE);
    }

    /**
     * Finds the last index of the given object in the array starting at the given index.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     * <p>
     * A negative startIndex will return {@link #INDEX_NOT_FOUND} ({@code -1}). A startIndex larger than
     * the array length will search from the end of the array.
     * </p>
     *
     * @param array  the array to traverse for looking for the object, may be {@code null}
     * @param objectToFind  the object to find, may be {@code null}
     * @param startIndex  the start index to traverse backwards from
     * @return the last index of the object within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     */
    public static int lastIndexOf(final Object[] array, final Object objectToFind, int startIndex) {
        if (array == null || startIndex < 0) {
            return INDEX_NOT_FOUND;
        }
        if (startIndex >= array.length) {
            startIndex = array.length - 1;
        }
        if (objectToFind == null) {
            for (int i = startIndex; i >= 0; i--) {
                if (array[i] == null) {
                    return i;
                }
            }
        } else if (array.getClass().getComponentType().isInstance(objectToFind)) {
            for (int i = startIndex; i >= 0; i--) {
                if (objectToFind.equals(array[i])) {
                    return i;
                }
            }
        }
        return INDEX_NOT_FOUND;
    }

    /**
     * Finds the last index of the given value within the array.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     *
     * @param array  the array to traverse backwards looking for the object, may be {@code null}
     * @param valueToFind  the object to find
     * @return the last index of the value within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     */
    public static int lastIndexOf(final short[] array, final short valueToFind) {
        return lastIndexOf(array, valueToFind, Integer.MAX_VALUE);
    }

    /**
     * Finds the last index of the given value in the array starting at the given index.
     * <p>
     * This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
     * </p>
     * <p>
     * A negative startIndex will return {@link #INDEX_NOT_FOUND} ({@code -1}). A startIndex larger than the
     * array length will search from the end of the array.
     * </p>
     *
     * @param array  the array to traverse for looking for the object, may be {@code null}
     * @param valueToFind  the value to find
     * @param startIndex  the start index to traverse backwards from
     * @return the last index of the value within the array,
     *  {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
     */
    public static int lastIndexOf(final short[] array, final short valueToFind, int startIndex) {
        if (array == null || startIndex < 0) {
            return INDEX_NOT_FOUND;
        }
        if (startIndex >= array.length) {
            startIndex = array.length - 1;
        }
        for (int i = startIndex; i >= 0; i--) {
            if (valueToFind == array[i]) {
                return i;
            }
        }
        return INDEX_NOT_FOUND;
    }

    /**
     * Maps elements from an array into elements of a new array of a given type, while mapping old elements to new elements.
     *
     * @param <T>           The input array type.
     * @param <R>           The output array type.
     * @param <E>           The type of exceptions thrown when the mapper function fails.
     * @param array         The input array.
     * @param componentType the component type of the result array.
     * @param mapper        a non-interfering, stateless function to apply to each element
     * @return a new array
     * @throws E Thrown when the mapper function fails.
     */
    private static <T, R, E extends Throwable> R[] map(final T[] array, final Class<R> componentType, final FailableFunction<? super T, ? extends R, E> mapper)
            throws E {
        return ArrayFill.fill(newInstance(componentType, array.length), i -> mapper.apply(array[i]));
    }

    private static int max0(final int other) {
        return Math.max(0, other);
    }

    /**
     * Delegates to {@link Array#newInstance(Class,int)} using generics.
     *
     * @param <T> The array type.
     * @param componentType The array class.
     * @param length the array length
     * @return The new array.
     * @throws NullPointerException if the specified {@code componentType} parameter is null.
     * @since 3.13.0
     */
    @SuppressWarnings("unchecked") // OK, because array and values are of type T
    public static <T> T[] newInstance(final Class<T> componentType, final int length) {
        return (T[]) Array.newInstance(componentType, length);
    }

    /**
     * Defensive programming technique to change a {@code null}
     * reference to an empty one.
     * <p>
     * This method returns a default array for a {@code null} input array.
     * </p>
     * <p>
     * As a memory optimizing technique an empty array passed in will be overridden with
     * the empty {@code public static} references in this class.
     * </p>
     *
     * @param <T> The array type.
     * @param array  the array to check for {@code null} or empty
     * @param defaultArray A default array, usually empty.
     * @return the same array, or defaultArray if {@code null} or empty input.
     * @since 3.15.0
     */
    public static <T> T[] nullTo(final T[] array, final T[] defaultArray) {
        return isEmpty(array) ? defaultArray : array;
    }

    /**
     * Defensive programming technique to change a {@code null}
     * reference to an empty one.
     * <p>
     * This method returns an empty array for a {@code null} input array.
     * </p>
     * <p>
     * As a memory optimizing technique an empty array passed in will be overridden with
     * the empty {@code public static} references in this class.
     * </p>
     *
     * @param array  the array to check for {@code null} or empty
     * @return the same array, {@code public static} empty array if {@code null} or empty input
     * @since 2.5
     */
    public static boolean[] nullToEmpty(final boolean[] array) {
        return isEmpty(array) ? EMPTY_BOOLEAN_ARRAY : array;
    }

    /**
     * Defensive programming technique to change a {@code null}
     * reference to an empty one.
     * <p>
     * This method returns an empty array for a {@code null} input array.
     * </p>
     * <p>
     * As a memory optimizing technique an empty array passed in will be overridden with
     * the empty {@code public static} references in this class.
     * </p>
     *
     * @param array  the array to check for {@code null} or empty
     * @return the same array, {@code public static} empty array if {@code null} or empty input
     * @since 2.5
     */
    public static Boolean[] nullToEmpty(final Boolean[] array) {
        return nullTo(array, EMPTY_BOOLEAN_OBJECT_ARRAY);
    }

    /**
     * Defensive programming technique to change a {@code null}
     * reference to an empty one.
     * <p>
     * This method returns an empty array for a {@code null} input array.
     * </p>
     * <p>
     * As a memory optimizing technique an empty array passed in will be overridden with
     * the empty {@code public static} references in this class.
     * </p>
     *
     * @param array  the array to check for {@code null} or empty
     * @return the same array, {@code public static} empty array if {@code null} or empty input
     * @since 2.5
     */
    public static byte[] nullToEmpty(final byte[] array) {
        return isEmpty(array) ? EMPTY_BYTE_ARRAY : array;
    }

    /**
     * Defensive programming technique to change a {@code null}
     * reference to an empty one.
     * <p>
     * This method returns an empty array for a {@code null} input array.
     * </p>
     * <p>
     * As a memory optimizing technique an empty array passed in will be overridden with
     * the empty {@code public static} references in this class.
     * </p>
     *
     * @param array  the array to check for {@code null} or empty
     * @return the same array, {@code public static} empty array if {@code null} or empty input
     * @since 2.5
     */
    public static Byte[] nullToEmpty(final Byte[] array) {
        return nullTo(array, EMPTY_BYTE_OBJECT_ARRAY);
    }

    /**
     * Defensive programming technique to change a {@code null}
     * reference to an empty one.
     * <p>
     * This method returns an empty array for a {@code null} input array.
     * </p>
     * <p>
     * As a memory optimizing technique an empty array passed in will be overridden with
     * the empty {@code public static} references in this class.
     * </p>
     *
     * @param array  the array to check for {@code null} or empty
     * @return the same array, {@code public static} empty array if {@code null} or empty input
     * @since 2.5
     */
    public static char[] nullToEmpty(final char[] array) {
        return isEmpty(array) ? EMPTY_CHAR_ARRAY : array;
    }

    /**
     * Defensive programming technique to change a {@code null}
     * reference to an empty one.
     * <p>
     * This method returns an empty array for a {@code null} input array.
     * </p>
     * <p>
     * As a memory optimizing technique an empty array passed in will be overridden with
     * the empty {@code public static} references in this class.
     * </p>
     *
     * @param array  the array to check for {@code null} or empty
     * @return the same array, {@code public static} empty array if {@code null} or empty input
     * @since 2.5
     */
    public static Character[] nullToEmpty(final Character[] array) {
        return nullTo(array, EMPTY_CHARACTER_OBJECT_ARRAY);
    }

    /**
     * Defensive programming technique to change a {@code null}
     * reference to an empty one.
     * <p>
     * This method returns an empty array for a {@code null} input array.
     * </p>
     * <p>
     * As a memory optimizing technique an empty array passed in will be overridden with
     * the empty {@code public static} references in this class.
     * </p>
     *
     * @param array  the array to check for {@code null} or empty
     * @return the same array, {@code public static} empty array if {@code null} or empty input
     * @since 3.2
     */
    public static Class<?>[] nullToEmpty(final Class<?>[] array) {
        return nullTo(array, EMPTY_CLASS_ARRAY);
    }

    /**
     * Defensive programming technique to change a {@code null}
     * reference to an empty one.
     * <p>
     * This method returns an empty array for a {@code null} input array.
     * </p>
     * <p>
     * As a memory optimizing technique an empty array passed in will be overridden with
     * the empty {@code public static} references in this class.
     * </p>
     *
     * @param array  the array to check for {@code null} or empty
     * @return the same array, {@code public static} empty array if {@code null} or empty input
     * @since 2.5
     */
    public static double[] nullToEmpty(final double[] array) {
        return isEmpty(array) ? EMPTY_DOUBLE_ARRAY : array;
    }

    /**
     * Defensive programming technique to change a {@code null}
     * reference to an empty one.
     * <p>
     * This method returns an empty array for a {@code null} input array.
     * </p>
     * <p>
     * As a memory optimizing technique an empty array passed in will be overridden with
     * the empty {@code public static} references in this class.
     * </p>
     *
     * @param array  the array to check for {@code null} or empty
     * @return the same array, {@code public static} empty array if {@code null} or empty input
     * @since 2.5
     */
    public static Double[] nullToEmpty(final Double[] array) {
        return nullTo(array, EMPTY_DOUBLE_OBJECT_ARRAY);
    }

    /**
     * Defensive programming technique to change a {@code null}
     * reference to an empty one.
     * <p>
     * This method returns an empty array for a {@code null} input array.
     * </p>
     * <p>
     * As a memory optimizing technique an empty array passed in will be overridden with
     * the empty {@code public static} references in this class.
     * </p>
     *
     * @param array  the array to check for {@code null} or empty
     * @return the same array, {@code public static} empty array if {@code null} or empty input
     * @since 2.5
     */
    public static float[] nullToEmpty(final float[] array) {
        return isEmpty(array) ? EMPTY_FLOAT_ARRAY : array;
    }

    /**
     * Defensive programming technique to change a {@code null}
     * reference to an empty one.
     * <p>
     * This method returns an empty array for a {@code null} input array.
     * </p>
     * <p>
     * As a memory optimizing technique an empty array passed in will be overridden with
     * the empty {@code public static} references in this class.
     * </p>
     *
     * @param array  the array to check for {@code null} or empty
     * @return the same array, {@code public static} empty array if {@code null} or empty input
     * @since 2.5
     */
    public static Float[] nullToEmpty(final Float[] array) {
        return nullTo(array, EMPTY_FLOAT_OBJECT_ARRAY);
    }

    /**
     * Defensive programming technique to change a {@code null}
     * reference to an empty one.
     * <p>
     * This method returns an empty array for a {@code null} input array.
     * </p>
     * <p>
     * As a memory optimizing technique an empty array passed in will be overridden with
     * the empty {@code public static} references in this class.
     * </p>
     *
     * @param array  the array to check for {@code null} or empty
     * @return the same array, {@code public static} empty array if {@code null} or empty input
     * @since 2.5
     */
    public static int[] nullToEmpty(final int[] array) {
        return isEmpty(array) ? EMPTY_INT_ARRAY : array;
    }

    /**
     * Defensive programming technique to change a {@code null}
     * reference to an empty one.
     * <p>
     * This method returns an empty array for a {@code null} input array.
     * </p>
     * <p>
     * As a memory optimizing technique an empty array passed in will be overridden with
     * the empty {@code public static} references in this class.
     * </p>
     *
     * @param array  the array to check for {@code null} or empty
     * @return the same array, {@code public static} empty array if {@code null} or empty input
     * @since 2.5
     */
    public static Integer[] nullToEmpty(final Integer[] array) {
        return nullTo(array, EMPTY_INTEGER_OBJECT_ARRAY);
    }

    /**
     * Defensive programming technique to change a {@code null}
     * reference to an empty one.
     * <p>
     * This method returns an empty array for a {@code null} input array.
     * </p>
     * <p>
     * As a memory optimizing technique an empty array passed in will be overridden with
     * the empty {@code public static} references in this class.
     * </p>
     *
     * @param array  the array to check for {@code null} or empty
     * @return the same array, {@code public static} empty array if {@code null} or empty input
     * @since 2.5
     */
    public static long[] nullToEmpty(final long[] array) {
        return isEmpty(array) ? EMPTY_LONG_ARRAY : array;
    }

    /**
     * Defensive programming technique to change a {@code null}
     * reference to an empty one.
     * <p>
     * This method returns an empty array for a {@code null} input array.
     * </p>
     * <p>
     * As a memory optimizing technique an empty array passed in will be overridden with
     * the empty {@code public static} references in this class.
     * </p>
     *
     * @param array  the array to check for {@code null} or empty
     * @return the same array, {@code public static} empty array if {@code null} or empty input
     * @since 2.5
     */
    public static Long[] nullToEmpty(final Long[] array) {
        return nullTo(array, EMPTY_LONG_OBJECT_ARRAY);
    }

    /**
     * Defensive programming technique to change a {@code null}
     * reference to an empty one.
     * <p>
     * This method returns an empty array for a {@code null} input array.
     * </p>
     * <p>
     * As a memory optimizing technique an empty array passed in will be overridden with
     * the empty {@code public static} references in this class.
     * </p>
     *
     * @param array  the array to check for {@code null} or empty
     * @return the same array, {@code public static} empty array if {@code null} or empty input
     * @since 2.5
     */
    public static Object[] nullToEmpty(final Object[] array) {
        return nullTo(array, EMPTY_OBJECT_ARRAY);
    }

    /**
     * Defensive programming technique to change a {@code null}
     * reference to an empty one.
     * <p>
     * This method returns an empty array for a {@code null} input array.
     * </p>
     * <p>
     * As a memory optimizing technique an empty array passed in will be overridden with
     * the empty {@code public static} references in this class.
     * </p>
     *
     * @param array  the array to check for {@code null} or empty
     * @return the same array, {@code public static} empty array if {@code null} or empty input
     * @since 2.5
     */
    public static short[] nullToEmpty(final short[] array) {
        return isEmpty(array) ? EMPTY_SHORT_ARRAY : array;
    }

    /**
     * Defensive programming technique to change a {@code null}
     * reference to an empty one.
     * <p>
     * This method returns an empty array for a {@code null} input array.
     * </p>
     * <p>
     * As a memory optimizing technique an empty array passed in will be overridden with
     * the empty {@code public static} references in this class.
     * </p>
     *
     * @param array  the array to check for {@code null} or empty
     * @return the same array, {@code public static} empty array if {@code null} or empty input
     * @since 2.5
     */
    public static Short[] nullToEmpty(final Short[] array) {
        return nullTo(array, EMPTY_SHORT_OBJECT_ARRAY);
    }

    /**
     * Defensive programming technique to change a {@code null}
     * reference to an empty one.
     * <p>
     * This method returns an empty array for a {@code null} input array.
     * </p>
     * <p>
     * As a memory optimizing technique an empty array passed in will be overridden with
     * the empty {@code public static} references in this class.
     * </p>
     *
     * @param array  the array to check for {@code null} or empty
     * @return the same array, {@code public static} empty array if {@code null} or empty input
     * @since 2.5
     */
    public static String[] nullToEmpty(final String[] array) {
        return nullTo(array, EMPTY_STRING_ARRAY);
    }

    /**
     * Defensive programming technique to change a {@code null}
     * reference to an empty one.
     * <p>
     * This method returns an empty array for a {@code null} input array.
     * </p>
     *
     * @param array  the array to check for {@code null} or empty
     * @param type   the class representation of the desired array
     * @param <T>  the class type
     * @return the same array, {@code public static} empty array if {@code null}
     * @throws IllegalArgumentException if the type argument is null
     * @since 3.5
     */
    public static <T> T[] nullToEmpty(final T[] array, final Class<T[]> type) {
        if (type == null) {
            throw new IllegalArgumentException("The type must not be null");
        }
        if (array == null) {
            return type.cast(Array.newInstance(type.getComponentType(), 0));
        }
        return array;
    }

    /**
     * Gets the {@link ThreadLocalRandom} for {@code shuffle} methods that don't take a {@link Random} argument.
     *
     * @return the current ThreadLocalRandom.
     */
    private static ThreadLocalRandom random() {
        return ThreadLocalRandom.current();
    }

    /**
     * Removes the element at the specified position from the specified array.
     * All subsequent elements are shifted to the left (subtracts one from
     * their indices).
     * <p>
     * This method returns a new array with the same elements of the input
     * array except the element on the specified position. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <p>
     * If the input array is {@code null}, an IndexOutOfBoundsException
     * will be thrown, because in that case no valid index can be specified.
     * </p>
     * <pre>
     * ArrayUtils.remove([true], 0)              = []
     * ArrayUtils.remove([true, false], 0)       = [false]
     * ArrayUtils.remove([true, false], 1)       = [true]
     * ArrayUtils.remove([true, true, false], 1) = [true, false]
     * </pre>
     *
     * @param array  the array to remove the element from, may not be {@code null}
     * @param index  the position of the element to be removed
     * @return A new array containing the existing elements except the element
     *         at the specified position.
     * @throws IndexOutOfBoundsException if the index is out of range
     * (index &lt; 0 || index &gt;= array.length), or if the array is {@code null}.
     * @since 2.1
     */
    public static boolean[] remove(final boolean[] array, final int index) {
        return (boolean[]) remove((Object) array, index);
    }

    /**
     * Removes the element at the specified position from the specified array.
     * All subsequent elements are shifted to the left (subtracts one from
     * their indices).
     * <p>
     * This method returns a new array with the same elements of the input
     * array except the element on the specified position. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <p>
     * If the input array is {@code null}, an IndexOutOfBoundsException
     * will be thrown, because in that case no valid index can be specified.
     * </p>
     * <pre>
     * ArrayUtils.remove([1], 0)          = []
     * ArrayUtils.remove([1, 0], 0)       = [0]
     * ArrayUtils.remove([1, 0], 1)       = [1]
     * ArrayUtils.remove([1, 0, 1], 1)    = [1, 1]
     * </pre>
     *
     * @param array  the array to remove the element from, may not be {@code null}
     * @param index  the position of the element to be removed
     * @return A new array containing the existing elements except the element
     *         at the specified position.
     * @throws IndexOutOfBoundsException if the index is out of range
     * (index &lt; 0 || index &gt;= array.length), or if the array is {@code null}.
     * @since 2.1
     */
    public static byte[] remove(final byte[] array, final int index) {
        return (byte[]) remove((Object) array, index);
    }

    /**
     * Removes the element at the specified position from the specified array.
     * All subsequent elements are shifted to the left (subtracts one from
     * their indices).
     * <p>
     * This method returns a new array with the same elements of the input
     * array except the element on the specified position. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <p>
     * If the input array is {@code null}, an IndexOutOfBoundsException
     * will be thrown, because in that case no valid index can be specified.
     * </p>
     * <pre>
     * ArrayUtils.remove(['a'], 0)           = []
     * ArrayUtils.remove(['a', 'b'], 0)      = ['b']
     * ArrayUtils.remove(['a', 'b'], 1)      = ['a']
     * ArrayUtils.remove(['a', 'b', 'c'], 1) = ['a', 'c']
     * </pre>
     *
     * @param array  the array to remove the element from, may not be {@code null}
     * @param index  the position of the element to be removed
     * @return A new array containing the existing elements except the element
     *         at the specified position.
     * @throws IndexOutOfBoundsException if the index is out of range
     * (index &lt; 0 || index &gt;= array.length), or if the array is {@code null}.
     * @since 2.1
     */
    public static char[] remove(final char[] array, final int index) {
        return (char[]) remove((Object) array, index);
    }

    /**
     * Removes the element at the specified position from the specified array.
     * All subsequent elements are shifted to the left (subtracts one from
     * their indices).
     * <p>
     * This method returns a new array with the same elements of the input
     * array except the element on the specified position. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <p>
     * If the input array is {@code null}, an IndexOutOfBoundsException
     * will be thrown, because in that case no valid index can be specified.
     * </p>
     * <pre>
     * ArrayUtils.remove([1.1], 0)           = []
     * ArrayUtils.remove([2.5, 6.0], 0)      = [6.0]
     * ArrayUtils.remove([2.5, 6.0], 1)      = [2.5]
     * ArrayUtils.remove([2.5, 6.0, 3.8], 1) = [2.5, 3.8]
     * </pre>
     *
     * @param array  the array to remove the element from, may not be {@code null}
     * @param index  the position of the element to be removed
     * @return A new array containing the existing elements except the element
     *         at the specified position.
     * @throws IndexOutOfBoundsException if the index is out of range
     * (index &lt; 0 || index &gt;= array.length), or if the array is {@code null}.
     * @since 2.1
     */
    public static double[] remove(final double[] array, final int index) {
        return (double[]) remove((Object) array, index);
    }

    /**
     * Removes the element at the specified position from the specified array.
     * All subsequent elements are shifted to the left (subtracts one from
     * their indices).
     * <p>
     * This method returns a new array with the same elements of the input
     * array except the element on the specified position. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <p>
     * If the input array is {@code null}, an IndexOutOfBoundsException
     * will be thrown, because in that case no valid index can be specified.
     * </p>
     * <pre>
     * ArrayUtils.remove([1.1], 0)           = []
     * ArrayUtils.remove([2.5, 6.0], 0)      = [6.0]
     * ArrayUtils.remove([2.5, 6.0], 1)      = [2.5]
     * ArrayUtils.remove([2.5, 6.0, 3.8], 1) = [2.5, 3.8]
     * </pre>
     *
     * @param array  the array to remove the element from, may not be {@code null}
     * @param index  the position of the element to be removed
     * @return A new array containing the existing elements except the element
     *         at the specified position.
     * @throws IndexOutOfBoundsException if the index is out of range
     * (index &lt; 0 || index &gt;= array.length), or if the array is {@code null}.
     * @since 2.1
     */
    public static float[] remove(final float[] array, final int index) {
        return (float[]) remove((Object) array, index);
    }

    /**
     * Removes the element at the specified position from the specified array.
     * All subsequent elements are shifted to the left (subtracts one from
     * their indices).
     * <p>
     * This method returns a new array with the same elements of the input
     * array except the element on the specified position. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <p>
     * If the input array is {@code null}, an IndexOutOfBoundsException
     * will be thrown, because in that case no valid index can be specified.
     * </p>
     * <pre>
     * ArrayUtils.remove([1], 0)         = []
     * ArrayUtils.remove([2, 6], 0)      = [6]
     * ArrayUtils.remove([2, 6], 1)      = [2]
     * ArrayUtils.remove([2, 6, 3], 1)   = [2, 3]
     * </pre>
     *
     * @param array  the array to remove the element from, may not be {@code null}
     * @param index  the position of the element to be removed
     * @return A new array containing the existing elements except the element
     *         at the specified position.
     * @throws IndexOutOfBoundsException if the index is out of range
     * (index &lt; 0 || index &gt;= array.length), or if the array is {@code null}.
     * @since 2.1
     */
    public static int[] remove(final int[] array, final int index) {
        return (int[]) remove((Object) array, index);
    }

    /**
     * Removes the element at the specified position from the specified array.
     * All subsequent elements are shifted to the left (subtracts one from
     * their indices).
     * <p>
     * This method returns a new array with the same elements of the input
     * array except the element on the specified position. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <p>
     * If the input array is {@code null}, an IndexOutOfBoundsException
     * will be thrown, because in that case no valid index can be specified.
     * </p>
     * <pre>
     * ArrayUtils.remove([1], 0)         = []
     * ArrayUtils.remove([2, 6], 0)      = [6]
     * ArrayUtils.remove([2, 6], 1)      = [2]
     * ArrayUtils.remove([2, 6, 3], 1)   = [2, 3]
     * </pre>
     *
     * @param array  the array to remove the element from, may not be {@code null}
     * @param index  the position of the element to be removed
     * @return A new array containing the existing elements except the element
     *         at the specified position.
     * @throws IndexOutOfBoundsException if the index is out of range
     * (index &lt; 0 || index &gt;= array.length), or if the array is {@code null}.
     * @since 2.1
     */
    public static long[] remove(final long[] array, final int index) {
        return (long[]) remove((Object) array, index);
    }

    /**
     * Removes the element at the specified position from the specified array.
     * All subsequent elements are shifted to the left (subtracts one from
     * their indices).
     * <p>
     * This method returns a new array with the same elements of the input
     * array except the element on the specified position. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <p>
     * If the input array is {@code null}, an IndexOutOfBoundsException
     * will be thrown, because in that case no valid index can be specified.
     * </p>
     *
     * @param array  the array to remove the element from, may not be {@code null}
     * @param index  the position of the element to be removed
     * @return A new array containing the existing elements except the element
     *         at the specified position.
     * @throws IndexOutOfBoundsException if the index is out of range
     * (index &lt; 0 || index &gt;= array.length), or if the array is {@code null}.
     * @since 2.1
     */
    private static Object remove(final Object array, final int index) {
        final int length = getLength(array);
        if (index < 0 || index >= length) {
            throw new IndexOutOfBoundsException("Index: " + index + ", Length: " + length);
        }
        final Object result = Array.newInstance(array.getClass().getComponentType(), length - 1);
        System.arraycopy(array, 0, result, 0, index);
        if (index < length - 1) {
            System.arraycopy(array, index + 1, result, index, length - index - 1);
        }
        return result;
    }

    /**
     * Removes the element at the specified position from the specified array.
     * All subsequent elements are shifted to the left (subtracts one from
     * their indices).
     * <p>
     * This method returns a new array with the same elements of the input
     * array except the element on the specified position. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <p>
     * If the input array is {@code null}, an IndexOutOfBoundsException
     * will be thrown, because in that case no valid index can be specified.
     * </p>
     * <pre>
     * ArrayUtils.remove([1], 0)         = []
     * ArrayUtils.remove([2, 6], 0)      = [6]
     * ArrayUtils.remove([2, 6], 1)      = [2]
     * ArrayUtils.remove([2, 6, 3], 1)   = [2, 3]
     * </pre>
     *
     * @param array  the array to remove the element from, may not be {@code null}
     * @param index  the position of the element to be removed
     * @return A new array containing the existing elements except the element
     *         at the specified position.
     * @throws IndexOutOfBoundsException if the index is out of range
     * (index &lt; 0 || index &gt;= array.length), or if the array is {@code null}.
     * @since 2.1
     */
    public static short[] remove(final short[] array, final int index) {
        return (short[]) remove((Object) array, index);
    }

    /**
     * Removes the element at the specified position from the specified array.
     * All subsequent elements are shifted to the left (subtracts one from
     * their indices).
     * <p>
     * This method returns a new array with the same elements of the input
     * array except the element on the specified position. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <p>
     * If the input array is {@code null}, an IndexOutOfBoundsException
     * will be thrown, because in that case no valid index can be specified.
     * </p>
     * <pre>
     * ArrayUtils.remove(["a"], 0)           = []
     * ArrayUtils.remove(["a", "b"], 0)      = ["b"]
     * ArrayUtils.remove(["a", "b"], 1)      = ["a"]
     * ArrayUtils.remove(["a", "b", "c"], 1) = ["a", "c"]
     * </pre>
     *
     * @param <T> the component type of the array
     * @param array  the array to remove the element from, may not be {@code null}
     * @param index  the position of the element to be removed
     * @return A new array containing the existing elements except the element
     *         at the specified position.
     * @throws IndexOutOfBoundsException if the index is out of range
     * (index &lt; 0 || index &gt;= array.length), or if the array is {@code null}.
     * @since 2.1
     */
    @SuppressWarnings("unchecked") // remove() always creates an array of the same type as its input
    public static <T> T[] remove(final T[] array, final int index) {
        return (T[]) remove((Object) array, index);
    }

    /**
     * Removes the elements at the specified positions from the specified array.
     * All remaining elements are shifted to the left.
     * <p>
     * This method returns a new array with the same elements of the input
     * array except those at the specified positions. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <p>
     * If the input array is {@code null}, an IndexOutOfBoundsException
     * will be thrown, because in that case no valid index can be specified.
     * </p>
     * <pre>
     * ArrayUtils.removeAll([true, false, true], 0, 2) = [false]
     * ArrayUtils.removeAll([true, false, true], 1, 2) = [true]
     * </pre>
     *
     * @param array   the array to remove the element from, may not be {@code null}
     * @param indices the positions of the elements to be removed
     * @return A new array containing the existing elements except those
     *         at the specified positions.
     * @throws IndexOutOfBoundsException if any index is out of range
     * (index &lt; 0 || index &gt;= array.length), or if the array is {@code null}.
     * @since 3.0.1
     */
    public static boolean[] removeAll(final boolean[] array, final int... indices) {
        return (boolean[]) removeAll((Object) array, indices);
    }

    /**
     * Removes the elements at the specified positions from the specified array.
     * All remaining elements are shifted to the left.
     * <p>
     * This method returns a new array with the same elements of the input
     * array except those at the specified positions. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <p>
     * If the input array is {@code null}, an IndexOutOfBoundsException
     * will be thrown, because in that case no valid index can be specified.
     * </p>
     * <pre>
     * ArrayUtils.removeAll([1], 0)             = []
     * ArrayUtils.removeAll([2, 6], 0)          = [6]
     * ArrayUtils.removeAll([2, 6], 0, 1)       = []
     * ArrayUtils.removeAll([2, 6, 3], 1, 2)    = [2]
     * ArrayUtils.removeAll([2, 6, 3], 0, 2)    = [6]
     * ArrayUtils.removeAll([2, 6, 3], 0, 1, 2) = []
     * </pre>
     *
     * @param array   the array to remove the element from, may not be {@code null}
     * @param indices the positions of the elements to be removed
     * @return A new array containing the existing elements except those
     *         at the specified positions.
     * @throws IndexOutOfBoundsException if any index is out of range
     * (index &lt; 0 || index &gt;= array.length), or if the array is {@code null}.
     * @since 3.0.1
     */
    public static byte[] removeAll(final byte[] array, final int... indices) {
        return (byte[]) removeAll((Object) array, indices);
    }

    /**
     * Removes the elements at the specified positions from the specified array.
     * All remaining elements are shifted to the left.
     * <p>
     * This method returns a new array with the same elements of the input
     * array except those at the specified positions. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <p>
     * If the input array is {@code null}, an IndexOutOfBoundsException
     * will be thrown, because in that case no valid index can be specified.
     * </p>
     * <pre>
     * ArrayUtils.removeAll([1], 0)             = []
     * ArrayUtils.removeAll([2, 6], 0)          = [6]
     * ArrayUtils.removeAll([2, 6], 0, 1)       = []
     * ArrayUtils.removeAll([2, 6, 3], 1, 2)    = [2]
     * ArrayUtils.removeAll([2, 6, 3], 0, 2)    = [6]
     * ArrayUtils.removeAll([2, 6, 3], 0, 1, 2) = []
     * </pre>
     *
     * @param array   the array to remove the element from, may not be {@code null}
     * @param indices the positions of the elements to be removed
     * @return A new array containing the existing elements except those
     *         at the specified positions.
     * @throws IndexOutOfBoundsException if any index is out of range
     * (index &lt; 0 || index &gt;= array.length), or if the array is {@code null}.
     * @since 3.0.1
     */
    public static char[] removeAll(final char[] array, final int... indices) {
        return (char[]) removeAll((Object) array, indices);
    }

    /**
     * Removes the elements at the specified positions from the specified array.
     * All remaining elements are shifted to the left.
     * <p>
     * This method returns a new array with the same elements of the input
     * array except those at the specified positions. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <p>
     * If the input array is {@code null}, an IndexOutOfBoundsException
     * will be thrown, because in that case no valid index can be specified.
     * </p>
     * <pre>
     * ArrayUtils.removeAll([1], 0)             = []
     * ArrayUtils.removeAll([2, 6], 0)          = [6]
     * ArrayUtils.removeAll([2, 6], 0, 1)       = []
     * ArrayUtils.removeAll([2, 6, 3], 1, 2)    = [2]
     * ArrayUtils.removeAll([2, 6, 3], 0, 2)    = [6]
     * ArrayUtils.removeAll([2, 6, 3], 0, 1, 2) = []
     * </pre>
     *
     * @param array   the array to remove the element from, may not be {@code null}
     * @param indices the positions of the elements to be removed
     * @return A new array containing the existing elements except those
     *         at the specified positions.
     * @throws IndexOutOfBoundsException if any index is out of range
     * (index &lt; 0 || index &gt;= array.length), or if the array is {@code null}.
     * @since 3.0.1
     */
    public static double[] removeAll(final double[] array, final int... indices) {
        return (double[]) removeAll((Object) array, indices);
    }

    /**
     * Removes the elements at the specified positions from the specified array.
     * All remaining elements are shifted to the left.
     * <p>
     * This method returns a new array with the same elements of the input
     * array except those at the specified positions. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <p>
     * If the input array is {@code null}, an IndexOutOfBoundsException
     * will be thrown, because in that case no valid index can be specified.
     * </p>
     * <pre>
     * ArrayUtils.removeAll([1], 0)             = []
     * ArrayUtils.removeAll([2, 6], 0)          = [6]
     * ArrayUtils.removeAll([2, 6], 0, 1)       = []
     * ArrayUtils.removeAll([2, 6, 3], 1, 2)    = [2]
     * ArrayUtils.removeAll([2, 6, 3], 0, 2)    = [6]
     * ArrayUtils.removeAll([2, 6, 3], 0, 1, 2) = []
     * </pre>
     *
     * @param array   the array to remove the element from, may not be {@code null}
     * @param indices the positions of the elements to be removed
     * @return A new array containing the existing elements except those
     *         at the specified positions.
     * @throws IndexOutOfBoundsException if any index is out of range
     * (index &lt; 0 || index &gt;= array.length), or if the array is {@code null}.
     * @since 3.0.1
     */
    public static float[] removeAll(final float[] array, final int... indices) {
        return (float[]) removeAll((Object) array, indices);
    }

    /**
     * Removes the elements at the specified positions from the specified array.
     * All remaining elements are shifted to the left.
     * <p>
     * This method returns a new array with the same elements of the input
     * array except those at the specified positions. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <p>
     * If the input array is {@code null}, an IndexOutOfBoundsException
     * will be thrown, because in that case no valid index can be specified.
     * </p>
     * <pre>
     * ArrayUtils.removeAll([1], 0)             = []
     * ArrayUtils.removeAll([2, 6], 0)          = [6]
     * ArrayUtils.removeAll([2, 6], 0, 1)       = []
     * ArrayUtils.removeAll([2, 6, 3], 1, 2)    = [2]
     * ArrayUtils.removeAll([2, 6, 3], 0, 2)    = [6]
     * ArrayUtils.removeAll([2, 6, 3], 0, 1, 2) = []
     * </pre>
     *
     * @param array   the array to remove the element from, may not be {@code null}
     * @param indices the positions of the elements to be removed
     * @return A new array containing the existing elements except those
     *         at the specified positions.
     * @throws IndexOutOfBoundsException if any index is out of range
     * (index &lt; 0 || index &gt;= array.length), or if the array is {@code null}.
     * @since 3.0.1
     */
    public static int[] removeAll(final int[] array, final int... indices) {
        return (int[]) removeAll((Object) array, indices);
    }

    /**
     * Removes the elements at the specified positions from the specified array.
     * All remaining elements are shifted to the left.
     * <p>
     * This method returns a new array with the same elements of the input
     * array except those at the specified positions. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <p>
     * If the input array is {@code null}, an IndexOutOfBoundsException
     * will be thrown, because in that case no valid index can be specified.
     * </p>
     * <pre>
     * ArrayUtils.removeAll([1], 0)             = []
     * ArrayUtils.removeAll([2, 6], 0)          = [6]
     * ArrayUtils.removeAll([2, 6], 0, 1)       = []
     * ArrayUtils.removeAll([2, 6, 3], 1, 2)    = [2]
     * ArrayUtils.removeAll([2, 6, 3], 0, 2)    = [6]
     * ArrayUtils.removeAll([2, 6, 3], 0, 1, 2) = []
     * </pre>
     *
     * @param array   the array to remove the element from, may not be {@code null}
     * @param indices the positions of the elements to be removed
     * @return A new array containing the existing elements except those
     *         at the specified positions.
     * @throws IndexOutOfBoundsException if any index is out of range
     * (index &lt; 0 || index &gt;= array.length), or if the array is {@code null}.
     * @since 3.0.1
     */
    public static long[] removeAll(final long[] array, final int... indices) {
        return (long[]) removeAll((Object) array, indices);
    }

    /**
     * Removes multiple array elements specified by index.
     *
     * @param array source
     * @param indices to remove
     * @return new array of same type minus elements specified by unique values of {@code indices}
     */
    // package protected for access by unit tests
    static Object removeAll(final Object array, final int... indices) {
        if (array == null) {
            return null;
        }
        final int length = getLength(array);
        int diff = 0; // number of distinct indexes, i.e. number of entries that will be removed
        final int[] clonedIndices = ArraySorter.sort(clone(indices));
        // identify length of result array
        if (isNotEmpty(clonedIndices)) {
            int i = clonedIndices.length;
            int prevIndex = length;
            while (--i >= 0) {
                final int index = clonedIndices[i];
                if (index < 0 || index >= length) {
                    throw new IndexOutOfBoundsException("Index: " + index + ", Length: " + length);
                }
                if (index >= prevIndex) {
                    continue;
                }
                diff++;
                prevIndex = index;
            }
        }
        // create result array
        final Object result = Array.newInstance(array.getClass().getComponentType(), length - diff);
        if (diff < length && clonedIndices != null) {
            int end = length; // index just after last copy
            int dest = length - diff; // number of entries so far not copied
            for (int i = clonedIndices.length - 1; i >= 0; i--) {
                final int index = clonedIndices[i];
                if (end - index > 1) { // same as (cp > 0)
                    final int cp = end - index - 1;
                    dest -= cp;
                    System.arraycopy(array, index + 1, result, dest, cp);
                    // After this copy, we still have room for dest items.
                }
                end = index;
            }
            if (end > 0) {
                System.arraycopy(array, 0, result, 0, end);
            }
        }
        return result;
    }

    /**
     * Removes the elements at the specified positions from the specified array.
     * All remaining elements are shifted to the left.
     * <p>
     * This method returns a new array with the same elements of the input
     * array except those at the specified positions. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <p>
     * If the input array is {@code null}, an IndexOutOfBoundsException
     * will be thrown, because in that case no valid index can be specified.
     * </p>
     * <pre>
     * ArrayUtils.removeAll([1], 0)             = []
     * ArrayUtils.removeAll([2, 6], 0)          = [6]
     * ArrayUtils.removeAll([2, 6], 0, 1)       = []
     * ArrayUtils.removeAll([2, 6, 3], 1, 2)    = [2]
     * ArrayUtils.removeAll([2, 6, 3], 0, 2)    = [6]
     * ArrayUtils.removeAll([2, 6, 3], 0, 1, 2) = []
     * </pre>
     *
     * @param array   the array to remove the element from, may not be {@code null}
     * @param indices the positions of the elements to be removed
     * @return A new array containing the existing elements except those
     *         at the specified positions.
     * @throws IndexOutOfBoundsException if any index is out of range
     * (index &lt; 0 || index &gt;= array.length), or if the array is {@code null}.
     * @since 3.0.1
     */
    public static short[] removeAll(final short[] array, final int... indices) {
        return (short[]) removeAll((Object) array, indices);
    }

    /**
     * Removes the elements at the specified positions from the specified array.
     * All remaining elements are shifted to the left.
     * <p>
     * This method returns a new array with the same elements of the input
     * array except those at the specified positions. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <p>
     * If the input array is {@code null}, an IndexOutOfBoundsException
     * will be thrown, because in that case no valid index can be specified.
     * </p>
     * <pre>
     * ArrayUtils.removeAll(["a", "b", "c"], 0, 2) = ["b"]
     * ArrayUtils.removeAll(["a", "b", "c"], 1, 2) = ["a"]
     * </pre>
     *
     * @param <T> the component type of the array
     * @param array   the array to remove the element from, may not be {@code null}
     * @param indices the positions of the elements to be removed
     * @return A new array containing the existing elements except those
     *         at the specified positions.
     * @throws IndexOutOfBoundsException if any index is out of range
     * (index &lt; 0 || index &gt;= array.length), or if the array is {@code null}.
     * @since 3.0.1
     */
    @SuppressWarnings("unchecked") // removeAll() always creates an array of the same type as its input
    public static <T> T[] removeAll(final T[] array, final int... indices) {
        return (T[]) removeAll((Object) array, indices);
    }

    /**
     * Removes the occurrences of the specified element from the specified boolean array.
     * <p>
     * All subsequent elements are shifted to the left (subtracts one from their indices).
     * If the array doesn't contain such an element, no elements are removed from the array.
     * {@code null} will be returned if the input array is {@code null}.
     * </p>
     *
     * @param array the input array, will not be modified, and may be {@code null}.
     * @param element the element to remove.
     * @return A new array containing the existing elements except the occurrences of the specified element.
     * @since 3.5
     * @deprecated Use {@link #removeAllOccurrences(boolean[], boolean)}
     */
    @Deprecated
    public static boolean[] removeAllOccurences(final boolean[] array, final boolean element) {
        return (boolean[]) removeAt(array, indexesOf(array, element));
    }

    /**
     * Removes the occurrences of the specified element from the specified byte array.
     * <p>
     * All subsequent elements are shifted to the left (subtracts one from their indices).
     * If the array doesn't contain such an element, no elements are removed from the array.
     * {@code null} will be returned if the input array is {@code null}.
     * </p>
     *
     * @param array the input array, will not be modified, and may be {@code null}.
     * @param element the element to remove.
     * @return A new array containing the existing elements except the occurrences of the specified element.
     * @since 3.5
     * @deprecated Use {@link #removeAllOccurrences(byte[], byte)}
     */
    @Deprecated
    public static byte[] removeAllOccurences(final byte[] array, final byte element) {
        return (byte[]) removeAt(array, indexesOf(array, element));
    }

    /**
     * Removes the occurrences of the specified element from the specified char array.
     * <p>
     * All subsequent elements are shifted to the left (subtracts one from their indices).
     * If the array doesn't contain such an element, no elements are removed from the array.
     * {@code null} will be returned if the input array is {@code null}.
     * </p>
     *
     * @param array the input array, will not be modified, and may be {@code null}.
     * @param element the element to remove.
     * @return A new array containing the existing elements except the occurrences of the specified element.
     * @since 3.5
     * @deprecated Use {@link #removeAllOccurrences(char[], char)}
     */
    @Deprecated
    public static char[] removeAllOccurences(final char[] array, final char element) {
        return (char[]) removeAt(array, indexesOf(array, element));
    }

    /**
     * Removes the occurrences of the specified element from the specified double array.
     * <p>
     * All subsequent elements are shifted to the left (subtracts one from their indices).
     * If the array doesn't contain such an element, no elements are removed from the array.
     * {@code null} will be returned if the input array is {@code null}.
     * </p>
     *
     * @param array the input array, will not be modified, and may be {@code null}.
     * @param element the element to remove.
     * @return A new array containing the existing elements except the occurrences of the specified element.
     * @since 3.5
     * @deprecated Use {@link #removeAllOccurrences(double[], double)}
     */
    @Deprecated
    public static double[] removeAllOccurences(final double[] array, final double element) {
        return (double[]) removeAt(array, indexesOf(array, element));
    }

    /**
     * Removes the occurrences of the specified element from the specified float array.
     * <p>
     * All subsequent elements are shifted to the left (subtracts one from their indices).
     * If the array doesn't contain such an element, no elements are removed from the array.
     * {@code null} will be returned if the input array is {@code null}.
     * </p>
     *
     * @param array the input array, will not be modified, and may be {@code null}.
     * @param element the element to remove.
     * @return A new array containing the existing elements except the occurrences of the specified element.
     * @since 3.5
     * @deprecated Use {@link #removeAllOccurrences(float[], float)}
     */
    @Deprecated
    public static float[] removeAllOccurences(final float[] array, final float element) {
        return (float[]) removeAt(array, indexesOf(array, element));
    }

    /**
     * Removes the occurrences of the specified element from the specified int array.
     * <p>
     * All subsequent elements are shifted to the left (subtracts one from their indices).
     * If the array doesn't contain such an element, no elements are removed from the array.
     * {@code null} will be returned if the input array is {@code null}.
     * </p>
     *
     * @param array the input array, will not be modified, and may be {@code null}.
     * @param element the element to remove.
     * @return A new array containing the existing elements except the occurrences of the specified element.
     * @since 3.5
     * @deprecated Use {@link #removeAllOccurrences(int[], int)}
     */
    @Deprecated
    public static int[] removeAllOccurences(final int[] array, final int element) {
        return (int[]) removeAt(array, indexesOf(array, element));
    }

    /**
     * Removes the occurrences of the specified element from the specified long array.
     * <p>
     * All subsequent elements are shifted to the left (subtracts one from their indices).
     * If the array doesn't contain such an element, no elements are removed from the array.
     * {@code null} will be returned if the input array is {@code null}.
     * </p>
     *
     * @param array the input array, will not be modified, and may be {@code null}.
     * @param element the element to remove.
     * @return A new array containing the existing elements except the occurrences of the specified element.
     * @since 3.5
     * @deprecated Use {@link #removeAllOccurrences(long[], long)}
     */
    @Deprecated
    public static long[] removeAllOccurences(final long[] array, final long element) {
        return (long[]) removeAt(array, indexesOf(array, element));
    }

    /**
     * Removes the occurrences of the specified element from the specified short array.
     * <p>
     * All subsequent elements are shifted to the left (subtracts one from their indices).
     * If the array doesn't contain such an element, no elements are removed from the array.
     * {@code null} will be returned if the input array is {@code null}.
     * </p>
     *
     * @param array the input array, will not be modified, and may be {@code null}.
     * @param element the element to remove.
     * @return A new array containing the existing elements except the occurrences of the specified element.
     * @since 3.5
     * @deprecated Use {@link #removeAllOccurrences(short[], short)}
     */
    @Deprecated
    public static short[] removeAllOccurences(final short[] array, final short element) {
        return (short[]) removeAt(array, indexesOf(array, element));
    }

    /**
     * Removes the occurrences of the specified element from the specified array.
     * <p>
     * All subsequent elements are shifted to the left (subtracts one from their indices).
     * If the array doesn't contain such an element, no elements are removed from the array.
     * {@code null} will be returned if the input array is {@code null}.
     * </p>
     *
     * @param <T> the type of object in the array, may be {@code null}.
     * @param array the input array, will not be modified, and may be {@code null}.
     * @param element the element to remove, may be {@code null}.
     * @return A new array containing the existing elements except the occurrences of the specified element.
     * @since 3.5
     * @deprecated Use {@link #removeAllOccurrences(Object[], Object)}
     */
    @Deprecated
    public static <T> T[] removeAllOccurences(final T[] array, final T element) {
        return (T[]) removeAt(array, indexesOf(array, element));
    }

    /**
     * Removes the occurrences of the specified element from the specified boolean array.
     * <p>
     * All subsequent elements are shifted to the left (subtracts one from their indices).
     * If the array doesn't contain such an element, no elements are removed from the array.
     * {@code null} will be returned if the input array is {@code null}.
     * </p>
     *
     * @param array the input array, will not be modified, and may be {@code null}.
     * @param element the element to remove.
     * @return A new array containing the existing elements except the occurrences of the specified element.
     * @since 3.10
     */
    public static boolean[] removeAllOccurrences(final boolean[] array, final boolean element) {
        return (boolean[]) removeAt(array, indexesOf(array, element));
    }

    /**
     * Removes the occurrences of the specified element from the specified byte array.
     * <p>
     * All subsequent elements are shifted to the left (subtracts one from their indices).
     * If the array doesn't contain such an element, no elements are removed from the array.
     * {@code null} will be returned if the input array is {@code null}.
     * </p>
     *
     * @param array the input array, will not be modified, and may be {@code null}.
     * @param element the element to remove.
     * @return A new array containing the existing elements except the occurrences of the specified element.
     * @since 3.10
     */
    public static byte[] removeAllOccurrences(final byte[] array, final byte element) {
        return (byte[]) removeAt(array, indexesOf(array, element));
    }

    /**
     * Removes the occurrences of the specified element from the specified char array.
     * <p>
     * All subsequent elements are shifted to the left (subtracts one from their indices).
     * If the array doesn't contain such an element, no elements are removed from the array.
     * {@code null} will be returned if the input array is {@code null}.
     * </p>
     *
     * @param array the input array, will not be modified, and may be {@code null}.
     * @param element the element to remove.
     * @return A new array containing the existing elements except the occurrences of the specified element.
     * @since 3.10
     */
    public static char[] removeAllOccurrences(final char[] array, final char element) {
        return (char[]) removeAt(array, indexesOf(array, element));
    }

    /**
     * Removes the occurrences of the specified element from the specified double array.
     * <p>
     * All subsequent elements are shifted to the left (subtracts one from their indices).
     * If the array doesn't contain such an element, no elements are removed from the array.
     * {@code null} will be returned if the input array is {@code null}.
     * </p>
     *
     * @param array the input array, will not be modified, and may be {@code null}.
     * @param element the element to remove.
     * @return A new array containing the existing elements except the occurrences of the specified element.
     * @since 3.10
     */
    public static double[] removeAllOccurrences(final double[] array, final double element) {
        return (double[]) removeAt(array, indexesOf(array, element));
    }

    /**
     * Removes the occurrences of the specified element from the specified float array.
     * <p>
     * All subsequent elements are shifted to the left (subtracts one from their indices).
     * If the array doesn't contain such an element, no elements are removed from the array.
     * {@code null} will be returned if the input array is {@code null}.
     * </p>
     *
     * @param array the input array, will not be modified, and may be {@code null}.
     * @param element the element to remove.
     * @return A new array containing the existing elements except the occurrences of the specified element.
     * @since 3.10
     */
    public static float[] removeAllOccurrences(final float[] array, final float element) {
        return (float[]) removeAt(array, indexesOf(array, element));
    }

    /**
     * Removes the occurrences of the specified element from the specified int array.
     * <p>
     * All subsequent elements are shifted to the left (subtracts one from their indices).
     * If the array doesn't contain such an element, no elements are removed from the array.
     * {@code null} will be returned if the input array is {@code null}.
     * </p>
     *
     * @param array the input array, will not be modified, and may be {@code null}.
     * @param element the element to remove.
     * @return A new array containing the existing elements except the occurrences of the specified element.
     * @since 3.10
     */
    public static int[] removeAllOccurrences(final int[] array, final int element) {
        return (int[]) removeAt(array, indexesOf(array, element));
    }

    /**
     * Removes the occurrences of the specified element from the specified long array.
     * <p>
     * All subsequent elements are shifted to the left (subtracts one from their indices).
     * If the array doesn't contain such an element, no elements are removed from the array.
     * {@code null} will be returned if the input array is {@code null}.
     * </p>
     *
     * @param array the input array, will not be modified, and may be {@code null}.
     * @param element the element to remove.
     * @return A new array containing the existing elements except the occurrences of the specified element.
     * @since 3.10
     */
    public static long[] removeAllOccurrences(final long[] array, final long element) {
        return (long[]) removeAt(array, indexesOf(array, element));
    }

    /**
     * Removes the occurrences of the specified element from the specified short array.
     * <p>
     * All subsequent elements are shifted to the left (subtracts one from their indices).
     * If the array doesn't contain such an element, no elements are removed from the array.
     * {@code null} will be returned if the input array is {@code null}.
     * </p>
     *
     * @param array the input array, will not be modified, and may be {@code null}.
     * @param element the element to remove.
     * @return A new array containing the existing elements except the occurrences of the specified element.
     * @since 3.10
     */
    public static short[] removeAllOccurrences(final short[] array, final short element) {
        return (short[]) removeAt(array, indexesOf(array, element));
    }

    /**
     * Removes the occurrences of the specified element from the specified array.
     * <p>
     * All subsequent elements are shifted to the left (subtracts one from their indices).
     * If the array doesn't contain such an element, no elements are removed from the array.
     * {@code null} will be returned if the input array is {@code null}.
     * </p>
     *
     * @param <T> the type of object in the array, may be {@code null}.
     * @param array the input array, will not be modified, and may be {@code null}.
     * @param element the element to remove, may be {@code null}.
     * @return A new array containing the existing elements except the occurrences of the specified element.
     * @since 3.10
     */
    public static <T> T[] removeAllOccurrences(final T[] array, final T element) {
        return (T[]) removeAt(array, indexesOf(array, element));
    }

    /**
     * Removes multiple array elements specified by indices.
     *
     * @param array the input array, will not be modified, and may be {@code null}.
     * @param indices to remove.
     * @return new array of same type minus elements specified by the set bits in {@code indices}.
     */
    // package protected for access by unit tests
    static Object removeAt(final Object array, final BitSet indices) {
        if (array == null) {
            return null;
        }
        final int srcLength = getLength(array);
        // No need to check maxIndex here, because method only currently called from removeElements()
        // which guarantee to generate only valid bit entries.
//        final int maxIndex = indices.length();
//        if (maxIndex > srcLength) {
//            throw new IndexOutOfBoundsException("Index: " + (maxIndex-1) + ", Length: " + srcLength);
//        }
        final int removals = indices.cardinality(); // true bits are items to remove
        final Object result = Array.newInstance(array.getClass().getComponentType(), srcLength - removals);
        int srcIndex = 0;
        int destIndex = 0;
        int count;
        int set;
        while ((set = indices.nextSetBit(srcIndex)) != -1) {
            count = set - srcIndex;
            if (count > 0) {
                System.arraycopy(array, srcIndex, result, destIndex, count);
                destIndex += count;
            }
            srcIndex = indices.nextClearBit(set);
        }
        count = srcLength - srcIndex;
        if (count > 0) {
            System.arraycopy(array, srcIndex, result, destIndex, count);
        }
        return result;
    }

    /**
     * Removes the first occurrence of the specified element from the
     * specified array. All subsequent elements are shifted to the left
     * (subtracts one from their indices). If the array doesn't contain
     * such an element, no elements are removed from the array.
     * <p>
     * This method returns a new array with the same elements of the input
     * array except the first occurrence of the specified element. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <pre>
     * ArrayUtils.removeElement(null, true)                = null
     * ArrayUtils.removeElement([], true)                  = []
     * ArrayUtils.removeElement([true], false)             = [true]
     * ArrayUtils.removeElement([true, false], false)      = [true]
     * ArrayUtils.removeElement([true, false, true], true) = [false, true]
     * </pre>
     *
     * @param array the input array, may be {@code null}.
     * @param element  the element to be removed.
     * @return A new array containing the existing elements except the first
     *         occurrence of the specified element.
     * @since 2.1
     */
    public static boolean[] removeElement(final boolean[] array, final boolean element) {
        final int index = indexOf(array, element);
        return index == INDEX_NOT_FOUND ? clone(array) : remove(array, index);
    }

    /**
     * Removes the first occurrence of the specified element from the
     * specified array. All subsequent elements are shifted to the left
     * (subtracts one from their indices). If the array doesn't contain
     * such an element, no elements are removed from the array.
     * <p>
     * This method returns a new array with the same elements of the input
     * array except the first occurrence of the specified element. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <pre>
     * ArrayUtils.removeElement(null, 1)        = null
     * ArrayUtils.removeElement([], 1)          = []
     * ArrayUtils.removeElement([1], 0)         = [1]
     * ArrayUtils.removeElement([1, 0], 0)      = [1]
     * ArrayUtils.removeElement([1, 0, 1], 1)   = [0, 1]
     * </pre>
     *
     * @param array the input array, may be {@code null}.
     * @param element  the element to be removed.
     * @return A new array containing the existing elements except the first
     *         occurrence of the specified element.
     * @since 2.1
     */
    public static byte[] removeElement(final byte[] array, final byte element) {
        final int index = indexOf(array, element);
        return index == INDEX_NOT_FOUND ? clone(array) : remove(array, index);
    }

    /**
     * Removes the first occurrence of the specified element from the
     * specified array. All subsequent elements are shifted to the left
     * (subtracts one from their indices). If the array doesn't contain
     * such an element, no elements are removed from the array.
     * <p>
     * This method returns a new array with the same elements of the input
     * array except the first occurrence of the specified element. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <pre>
     * ArrayUtils.removeElement(null, 'a')            = null
     * ArrayUtils.removeElement([], 'a')              = []
     * ArrayUtils.removeElement(['a'], 'b')           = ['a']
     * ArrayUtils.removeElement(['a', 'b'], 'a')      = ['b']
     * ArrayUtils.removeElement(['a', 'b', 'a'], 'a') = ['b', 'a']
     * </pre>
     *
     * @param array the input array, may be {@code null}.
     * @param element  the element to be removed.
     * @return A new array containing the existing elements except the first
     *         occurrence of the specified element.
     * @since 2.1
     */
    public static char[] removeElement(final char[] array, final char element) {
        final int index = indexOf(array, element);
        return index == INDEX_NOT_FOUND ? clone(array) : remove(array, index);
    }

    /**
     * Removes the first occurrence of the specified element from the
     * specified array. All subsequent elements are shifted to the left
     * (subtracts one from their indices). If the array doesn't contain
     * such an element, no elements are removed from the array.
     * <p>
     * This method returns a new array with the same elements of the input
     * array except the first occurrence of the specified element. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <pre>
     * ArrayUtils.removeElement(null, 1.1)            = null
     * ArrayUtils.removeElement([], 1.1)              = []
     * ArrayUtils.removeElement([1.1], 1.2)           = [1.1]
     * ArrayUtils.removeElement([1.1, 2.3], 1.1)      = [2.3]
     * ArrayUtils.removeElement([1.1, 2.3, 1.1], 1.1) = [2.3, 1.1]
     * </pre>
     *
     * @param array the input array, may be {@code null}.
     * @param element  the element to be removed.
     * @return A new array containing the existing elements except the first
     *         occurrence of the specified element.
     * @since 2.1
     */
    public static double[] removeElement(final double[] array, final double element) {
        final int index = indexOf(array, element);
        return index == INDEX_NOT_FOUND ? clone(array) : remove(array, index);
    }

    /**
     * Removes the first occurrence of the specified element from the
     * specified array. All subsequent elements are shifted to the left
     * (subtracts one from their indices). If the array doesn't contain
     * such an element, no elements are removed from the array.
     * <p>
     * This method returns a new array with the same elements of the input
     * array except the first occurrence of the specified element. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <pre>
     * ArrayUtils.removeElement(null, 1.1)            = null
     * ArrayUtils.removeElement([], 1.1)              = []
     * ArrayUtils.removeElement([1.1], 1.2)           = [1.1]
     * ArrayUtils.removeElement([1.1, 2.3], 1.1)      = [2.3]
     * ArrayUtils.removeElement([1.1, 2.3, 1.1], 1.1) = [2.3, 1.1]
     * </pre>
     *
     * @param array the input array, may be {@code null}.
     * @param element  the element to be removed.
     * @return A new array containing the existing elements except the first
     *         occurrence of the specified element.
     * @since 2.1
     */
    public static float[] removeElement(final float[] array, final float element) {
        final int index = indexOf(array, element);
        return index == INDEX_NOT_FOUND ? clone(array) : remove(array, index);
    }

    /**
     * Removes the first occurrence of the specified element from the
     * specified array. All subsequent elements are shifted to the left
     * (subtracts one from their indices). If the array doesn't contain
     * such an element, no elements are removed from the array.
     * <p>
     * This method returns a new array with the same elements of the input
     * array except the first occurrence of the specified element. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <pre>
     * ArrayUtils.removeElement(null, 1)      = null
     * ArrayUtils.removeElement([], 1)        = []
     * ArrayUtils.removeElement([1], 2)       = [1]
     * ArrayUtils.removeElement([1, 3], 1)    = [3]
     * ArrayUtils.removeElement([1, 3, 1], 1) = [3, 1]
     * </pre>
     *
     * @param array the input array, may be {@code null}.
     * @param element  the element to be removed.
     * @return A new array containing the existing elements except the first
     *         occurrence of the specified element.
     * @since 2.1
     */
    public static int[] removeElement(final int[] array, final int element) {
        final int index = indexOf(array, element);
        return index == INDEX_NOT_FOUND ? clone(array) : remove(array, index);
    }

    /**
     * Removes the first occurrence of the specified element from the
     * specified array. All subsequent elements are shifted to the left
     * (subtracts one from their indices). If the array doesn't contain
     * such an element, no elements are removed from the array.
     * <p>
     * This method returns a new array with the same elements of the input
     * array except the first occurrence of the specified element. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <pre>
     * ArrayUtils.removeElement(null, 1)      = null
     * ArrayUtils.removeElement([], 1)        = []
     * ArrayUtils.removeElement([1], 2)       = [1]
     * ArrayUtils.removeElement([1, 3], 1)    = [3]
     * ArrayUtils.removeElement([1, 3, 1], 1) = [3, 1]
     * </pre>
     *
     * @param array the input array, may be {@code null}.
     * @param element  the element to be removed.
     * @return A new array containing the existing elements except the first
     *         occurrence of the specified element.
     * @since 2.1
     */
    public static long[] removeElement(final long[] array, final long element) {
        final int index = indexOf(array, element);
        return index == INDEX_NOT_FOUND ? clone(array) : remove(array, index);
    }

    /**
     * Removes the first occurrence of the specified element from the
     * specified array. All subsequent elements are shifted to the left
     * (subtracts one from their indices). If the array doesn't contain
     * such an element, no elements are removed from the array.
     * <p>
     * This method returns a new array with the same elements of the input
     * array except the first occurrence of the specified element. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <pre>
     * ArrayUtils.removeElement(null, 1)      = null
     * ArrayUtils.removeElement([], 1)        = []
     * ArrayUtils.removeElement([1], 2)       = [1]
     * ArrayUtils.removeElement([1, 3], 1)    = [3]
     * ArrayUtils.removeElement([1, 3, 1], 1) = [3, 1]
     * </pre>
     *
     * @param array the input array, may be {@code null}.
     * @param element  the element to be removed.
     * @return A new array containing the existing elements except the first
     *         occurrence of the specified element.
     * @since 2.1
     */
    public static short[] removeElement(final short[] array, final short element) {
        final int index = indexOf(array, element);
        return index == INDEX_NOT_FOUND ? clone(array) : remove(array, index);
    }

    /**
     * Removes the first occurrence of the specified element from the
     * specified array. All subsequent elements are shifted to the left
     * (subtracts one from their indices). If the array doesn't contain
     * such an element, no elements are removed from the array.
     * <p>
     * This method returns a new array with the same elements of the input
     * array except the first occurrence of the specified element. The component
     * type of the returned array is always the same as that of the input
     * array.
     * </p>
     * <pre>
     * ArrayUtils.removeElement(null, "a")            = null
     * ArrayUtils.removeElement([], "a")              = []
     * ArrayUtils.removeElement(["a"], "b")           = ["a"]
     * ArrayUtils.removeElement(["a", "b"], "a")      = ["b"]
     * ArrayUtils.removeElement(["a", "b", "a"], "a") = ["b", "a"]
     * </pre>
     *
     * @param <T> the component type of the array
     * @param array the input array, may be {@code null}.
     * @param element  the element to be removed, may be {@code null}.
     * @return A new array containing the existing elements except the first
     *         occurrence of the specified element.
     * @since 2.1
     */
    public static <T> T[] removeElement(final T[] array, final Object element) {
        final int index = indexOf(array, element);
        return index == INDEX_NOT_FOUND ? clone(array) : remove(array, index);
    }

    /**
     * Removes occurrences of specified elements, in specified quantities,
     * from the specified array. All subsequent elements are shifted left.
     * For any element-to-be-removed specified in greater quantities than
     * contained in the original array, no change occurs beyond the
     * removal of the existing matching items.
     * <p>
     * This method returns a new array with the same elements of the input
     * array except for the earliest-encountered occurrences of the specified
     * elements. The component type of the returned array is always the same
     * as that of the input array.
     * </p>
     * <pre>
     * ArrayUtils.removeElements(null, true, false)               = null
     * ArrayUtils.removeElements([], true, false)                 = []
     * ArrayUtils.removeElements([true], false, false)            = [true]
     * ArrayUtils.removeElements([true, false], true, true)       = [false]
     * ArrayUtils.removeElements([true, false, true], true)       = [false, true]
     * ArrayUtils.removeElements([true, false, true], true, true) = [false]
     * </pre>
     *
     * @param array the input array, will not be modified, and may be {@code null}.
     * @param values  the values to be removed.
     * @return A new array containing the existing elements except the
     *         earliest-encountered occurrences of the specified elements.
     * @since 3.0.1
     */
    public static boolean[] removeElements(final boolean[] array, final boolean... values) {
        if (isEmpty(array) || isEmpty(values)) {
            return clone(array);
        }
        final HashMap<Boolean, MutableInt> occurrences = new HashMap<>(2); // only two possible values here
        for (final boolean v : values) {
            increment(occurrences, Boolean.valueOf(v));
        }
        final BitSet toRemove = new BitSet();
        for (int i = 0; i < array.length; i++) {
            final boolean key = array[i];
            final MutableInt count = occurrences.get(key);
            if (count != null) {
                if (count.decrementAndGet() == 0) {
                    occurrences.remove(key);
                }
                toRemove.set(i);
            }
        }
        return (boolean[]) removeAt(array, toRemove);
    }

    /**
     * Removes occurrences of specified elements, in specified quantities,
     * from the specified array. All subsequent elements are shifted left.
     * For any element-to-be-removed specified in greater quantities than
     * contained in the original array, no change occurs beyond the
     * removal of the existing matching items.
     * <p>
     * This method returns a new array with the same elements of the input
     * array except for the earliest-encountered occurrences of the specified
     * elements. The component type of the returned array is always the same
     * as that of the input array.
     * </p>
     * <pre>
     * ArrayUtils.removeElements(null, 1, 2)      = null
     * ArrayUtils.removeElements([], 1, 2)        = []
     * ArrayUtils.removeElements([1], 2, 3)       = [1]
     * ArrayUtils.removeElements([1, 3], 1, 2)    = [3]
     * ArrayUtils.removeElements([1, 3, 1], 1)    = [3, 1]
     * ArrayUtils.removeElements([1, 3, 1], 1, 1) = [3]
     * </pre>
     *
     * @param array the input array, will not be modified, and may be {@code null}.
     * @param values  the values to be removed.
     * @return A new array containing the existing elements except the
     *         earliest-encountered occurrences of the specified elements.
     * @since 3.0.1
     */
    public static byte[] removeElements(final byte[] array, final byte... values) {
        if (isEmpty(array) || isEmpty(values)) {
            return clone(array);
        }
        final HashMap<Byte, MutableInt> occurrences = new HashMap<>(values.length);
        for (final byte v : values) {
            increment(occurrences, Byte.valueOf(v));
        }
        final BitSet toRemove = new BitSet();
        for (int i = 0; i < array.length; i++) {
            final byte key = array[i];
            final MutableInt count = occurrences.get(key);
            if (count != null) {
                if (count.decrementAndGet() == 0) {
                    occurrences.remove(key);
                }
                toRemove.set(i);
            }
        }
        return (byte[]) removeAt(array, toRemove);
    }

    /**
     * Removes occurrences of specified elements, in specified quantities,
     * from the specified array. All subsequent elements are shifted left.
     * For any element-to-be-removed specified in greater quantities than
     * contained in the original array, no change occurs beyond the
     * removal of the existing matching items.
     * <p>
     * This method returns a new array with the same elements of the input
     * array except for the earliest-encountered occurrences of the specified
     * elements. The component type of the returned array is always the same
     * as that of the input array.
     * </p>
     * <pre>
     * ArrayUtils.removeElements(null, 1, 2)      = null
     * ArrayUtils.removeElements([], 1, 2)        = []
     * ArrayUtils.removeElements([1], 2, 3)       = [1]
     * ArrayUtils.removeElements([1, 3], 1, 2)    = [3]
     * ArrayUtils.removeElements([1, 3, 1], 1)    = [3, 1]
     * ArrayUtils.removeElements([1, 3, 1], 1, 1) = [3]
     * </pre>
     *
     * @param array the input array, will not be modified, and may be {@code null}.
     * @param values  the values to be removed.
     * @return A new array containing the existing elements except the
     *         earliest-encountered occurrences of the specified elements.
     * @since 3.0.1
     */
    public static char[] removeElements(final char[] array, final char... values) {
        if (isEmpty(array) || isEmpty(values)) {
            return clone(array);
        }
        final HashMap<Character, MutableInt> occurrences = new HashMap<>(values.length);
        for (final char v : values) {
            increment(occurrences, Character.valueOf(v));
        }
        final BitSet toRemove = new BitSet();
        for (int i = 0; i < array.length; i++) {
            final char key = array[i];
            final MutableInt count = occurrences.get(key);
            if (count != null) {
                if (count.decrementAndGet() == 0) {
                    occurrences.remove(key);
                }
                toRemove.set(i);
            }
        }
        return (char[]) removeAt(array, toRemove);
    }

    /**
     * Removes occurrences of specified elements, in specified quantities,
     * from the specified array. All subsequent elements are shifted left.
     * For any element-to-be-removed specified in greater quantities than
     * contained in the original array, no change occurs beyond the
     * removal of the existing matching items.
     * <p>
     * This method returns a new array with the same elements of the input
     * array except for the earliest-encountered occurrences of the specified
     * elements. The component type of the returned array is always the same
     * as that of the input array.
     * </p>
     * <pre>
     * ArrayUtils.removeElements(null, 1, 2)      = null
     * ArrayUtils.removeElements([], 1, 2)        = []
     * ArrayUtils.removeElements([1], 2, 3)       = [1]
     * ArrayUtils.removeElements([1, 3], 1, 2)    = [3]
     * ArrayUtils.removeElements([1, 3, 1], 1)    = [3, 1]
     * ArrayUtils.removeElements([1, 3, 1], 1, 1) = [3]
     * </pre>
     *
     * @param array the input array, will not be modified, and may be {@code null}.
     * @param values  the values to be removed.
     * @return A new array containing the existing elements except the
     *         earliest-encountered occurrences of the specified elements.
     * @since 3.0.1
     */
    public static double[] removeElements(final double[] array, final double... values) {
        if (isEmpty(array) || isEmpty(values)) {
            return clone(array);
        }
        final HashMap<Double, MutableInt> occurrences = new HashMap<>(values.length);
        for (final double v : values) {
            increment(occurrences, Double.valueOf(v));
        }
        final BitSet toRemove = new BitSet();
        for (int i = 0; i < array.length; i++) {
            final double key = array[i];
            final MutableInt count = occurrences.get(key);
            if (count != null) {
                if (count.decrementAndGet() == 0) {
                    occurrences.remove(key);
                }
                toRemove.set(i);
            }
        }
        return (double[]) removeAt(array, toRemove);
    }

    /**
     * Removes occurrences of specified elements, in specified quantities,
     * from the specified array. All subsequent elements are shifted left.
     * For any element-to-be-removed specified in greater quantities than
     * contained in the original array, no change occurs beyond the
     * removal of the existing matching items.
     * <p>
     * This method returns a new array with the same elements of the input
     * array except for the earliest-encountered occurrences of the specified
     * elements. The component type of the returned array is always the same
     * as that of the input array.
     * </p>
     * <pre>
     * ArrayUtils.removeElements(null, 1, 2)      = null
     * ArrayUtils.removeElements([], 1, 2)        = []
     * ArrayUtils.removeElements([1], 2, 3)       = [1]
     * ArrayUtils.removeElements([1, 3], 1, 2)    = [3]
     * ArrayUtils.removeElements([1, 3, 1], 1)    = [3, 1]
     * ArrayUtils.removeElements([1, 3, 1], 1, 1) = [3]
     * </pre>
     *
     * @param array the input array, will not be modified, and may be {@code null}.
     * @param values  the values to be removed.
     * @return A new array containing the existing elements except the
     *         earliest-encountered occurrences of the specified elements.
     * @since 3.0.1
     */
    public static float[] removeElements(final float[] array, final float... values) {
        if (isEmpty(array) || isEmpty(values)) {
            return clone(array);
        }
        final HashMap<Float, MutableInt> occurrences = new HashMap<>(values.length);
        for (final float v : values) {
            increment(occurrences, Float.valueOf(v));
        }
        final BitSet toRemove = new BitSet();
        for (int i = 0; i < array.length; i++) {
            final float key = array[i];
            final MutableInt count = occurrences.get(key);
            if (count != null) {
                if (count.decrementAndGet() == 0) {
                    occurrences.remove(key);
                }
                toRemove.set(i);
            }
        }
        return (float[]) removeAt(array, toRemove);
    }

    /**
     * Removes occurrences of specified elements, in specified quantities,
     * from the specified array. All subsequent elements are shifted left.
     * For any element-to-be-removed specified in greater quantities than
     * contained in the original array, no change occurs beyond the
     * removal of the existing matching items.
     * <p>
     * This method returns a new array with the same elements of the input
     * array except for the earliest-encountered occurrences of the specified
     * elements. The component type of the returned array is always the same
     * as that of the input array.
     * </p>
     * <pre>
     * ArrayUtils.removeElements(null, 1, 2)      = null
     * ArrayUtils.removeElements([], 1, 2)        = []
     * ArrayUtils.removeElements([1], 2, 3)       = [1]
     * ArrayUtils.removeElements([1, 3], 1, 2)    = [3]
     * ArrayUtils.removeElements([1, 3, 1], 1)    = [3, 1]
     * ArrayUtils.removeElements([1, 3, 1], 1, 1) = [3]
     * </pre>
     *
     * @param array the input array, will not be modified, and may be {@code null}.
     * @param values  the values to be removed.
     * @return A new array containing the existing elements except the
     *         earliest-encountered occurrences of the specified elements.
     * @since 3.0.1
     */
    public static int[] removeElements(final int[] array, final int... values) {
        if (isEmpty(array) || isEmpty(values)) {
            return clone(array);
        }
        final HashMap<Integer, MutableInt> occurrences = new HashMap<>(values.length);
        for (final int v : values) {
            increment(occurrences, Integer.valueOf(v));
        }
        final BitSet toRemove = new BitSet();
        for (int i = 0; i < array.length; i++) {
            final int key = array[i];
            final MutableInt count = occurrences.get(key);
            if (count != null) {
                if (count.decrementAndGet() == 0) {
                    occurrences.remove(key);
                }
                toRemove.set(i);
            }
        }
        return (int[]) removeAt(array, toRemove);
    }

    /**
     * Removes occurrences of specified elements, in specified quantities,
     * from the specified array. All subsequent elements are shifted left.
     * For any element-to-be-removed specified in greater quantities than
     * contained in the original array, no change occurs beyond the
     * removal of the existing matching items.
     * <p>
     * This method returns a new array with the same elements of the input
     * array except for the earliest-encountered occurrences of the specified
     * elements. The component type of the returned array is always the same
     * as that of the input array.
     * </p>
     * <pre>
     * ArrayUtils.removeElements(null, 1, 2)      = null
     * ArrayUtils.removeElements([], 1, 2)        = []
     * ArrayUtils.removeElements([1], 2, 3)       = [1]
     * ArrayUtils.removeElements([1, 3], 1, 2)    = [3]
     * ArrayUtils.removeElements([1, 3, 1], 1)    = [3, 1]
     * ArrayUtils.removeElements([1, 3, 1], 1, 1) = [3]
     * </pre>
     *
     * @param array the input array, will not be modified, and may be {@code null}.
     * @param values  the values to be removed.
     * @return A new array containing the existing elements except the
     *         earliest-encountered occurrences of the specified elements.
     * @since 3.0.1
     */
    public static long[] removeElements(final long[] array, final long... values) {
        if (isEmpty(array) || isEmpty(values)) {
            return clone(array);
        }
        final HashMap<Long, MutableInt> occurrences = new HashMap<>(values.length);
        for (final long v : values) {
            increment(occurrences, Long.valueOf(v));
        }
        final BitSet toRemove = new BitSet();
        for (int i = 0; i < array.length; i++) {
            final long key = array[i];
            final MutableInt count = occurrences.get(key);
            if (count != null) {
                if (count.decrementAndGet() == 0) {
                    occurrences.remove(key);
                }
                toRemove.set(i);
            }
        }
        return (long[]) removeAt(array, toRemove);
    }

    /**
     * Removes occurrences of specified elements, in specified quantities,
     * from the specified array. All subsequent elements are shifted left.
     * For any element-to-be-removed specified in greater quantities than
     * contained in the original array, no change occurs beyond the
     * removal of the existing matching items.
     * <p>
     * This method returns a new array with the same elements of the input
     * array except for the earliest-encountered occurrences of the specified
     * elements. The component type of the returned array is always the same
     * as that of the input array.
     * </p>
     * <pre>
     * ArrayUtils.removeElements(null, 1, 2)      = null
     * ArrayUtils.removeElements([], 1, 2)        = []
     * ArrayUtils.removeElements([1], 2, 3)       = [1]
     * ArrayUtils.removeElements([1, 3], 1, 2)    = [3]
     * ArrayUtils.removeElements([1, 3, 1], 1)    = [3, 1]
     * ArrayUtils.removeElements([1, 3, 1], 1, 1) = [3]
     * </pre>
     *
     * @param array the input array, will not be modified, and may be {@code null}.
     * @param values  the values to be removed.
     * @return A new array containing the existing elements except the
     *         earliest-encountered occurrences of the specified elements.
     * @since 3.0.1
     */
    public static short[] removeElements(final short[] array, final short... values) {
        if (isEmpty(array) || isEmpty(values)) {
            return clone(array);
        }
        final HashMap<Short, MutableInt> occurrences = new HashMap<>(values.length);
        for (final short v : values) {
            increment(occurrences, Short.valueOf(v));
        }
        final BitSet toRemove = new BitSet();
        for (int i = 0; i < array.length; i++) {
            final short key = array[i];
            final MutableInt count = occurrences.get(key);
            if (count != null) {
                if (count.decrementAndGet() == 0) {
                    occurrences.remove(key);
                }
                toRemove.set(i);
            }
        }
        return (short[]) removeAt(array, toRemove);
    }

    /**
     * Removes occurrences of specified elements, in specified quantities,
     * from the specified array. All subsequent elements are shifted left.
     * For any element-to-be-removed specified in greater quantities than
     * contained in the original array, no change occurs beyond the
     * removal of the existing matching items.
     * <p>
     * This method returns a new array with the same elements of the input
     * array except for the earliest-encountered occurrences of the specified
     * elements. The component type of the returned array is always the same
     * as that of the input array.
     * </p>
     * <pre>
     * ArrayUtils.removeElements(null, "a", "b")            = null
     * ArrayUtils.removeElements([], "a", "b")              = []
     * ArrayUtils.removeElements(["a"], "b", "c")           = ["a"]
     * ArrayUtils.removeElements(["a", "b"], "a", "c")      = ["b"]
     * ArrayUtils.removeElements(["a", "b", "a"], "a")      = ["b", "a"]
     * ArrayUtils.removeElements(["a", "b", "a"], "a", "a") = ["b"]
     * </pre>
     *
     * @param <T> the component type of the array
     * @param array the input array, will not be modified, and may be {@code null}.
     * @param values  the values to be removed.
     * @return A new array containing the existing elements except the
     *         earliest-encountered occurrences of the specified elements.
     * @since 3.0.1
     */
    @SafeVarargs
    public static <T> T[] removeElements(final T[] array, final T... values) {
        if (isEmpty(array) || isEmpty(values)) {
            return clone(array);
        }
        final HashMap<T, MutableInt> occurrences = new HashMap<>(values.length);
        for (final T v : values) {
            increment(occurrences, v);
        }
        final BitSet toRemove = new BitSet();
        for (int i = 0; i < array.length; i++) {
            final T key = array[i];
            final MutableInt count = occurrences.get(key);
            if (count != null) {
                if (count.decrementAndGet() == 0) {
                    occurrences.remove(key);
                }
                toRemove.set(i);
            }
        }
        @SuppressWarnings("unchecked") // removeAll() always creates an array of the same type as its input
        final T[] result = (T[]) removeAt(array, toRemove);
        return result;
    }

    /**
     * Reverses the order of the given array.
     * <p>
     * This method does nothing for a {@code null} input array.
     * </p>
     *
     * @param array  the array to reverse, may be {@code null}.
     */
    public static void reverse(final boolean[] array) {
        if (array == null) {
            return;
        }
        reverse(array, 0, array.length);
    }

    /**
     * Reverses the order of the given array in the given range.
     * <p>
     * This method does nothing for a {@code null} input array.
     * </p>
     *
     * @param array
     *            the array to reverse, may be {@code null}.
     * @param startIndexInclusive
     *            the starting index. Undervalue (&lt;0) is promoted to 0, overvalue (&gt;array.length) results in no
     *            change.
     * @param endIndexExclusive
     *            elements up to endIndex-1 are reversed in the array. Undervalue (&lt; start index) results in no
     *            change. Overvalue (&gt;array.length) is demoted to array length.
     * @since 3.2
     */
    public static void reverse(final boolean[] array, final int startIndexInclusive, final int endIndexExclusive) {
        if (array == null) {
            return;
        }
        int i = Math.max(startIndexInclusive, 0);
        int j = Math.min(array.length, endIndexExclusive) - 1;
        boolean tmp;
        while (j > i) {
            tmp = array[j];
            array[j] = array[i];
            array[i] = tmp;
            j--;
            i++;
        }
    }

    /**
     * Reverses the order of the given array.
     * <p>
     * This method does nothing for a {@code null} input array.
     * </p>
     *
     * @param array  the array to reverse, may be {@code null}
     */
    public static void reverse(final byte[] array) {
        if (array != null) {
            reverse(array, 0, array.length);
        }
    }

    /**
     * Reverses the order of the given array in the given range.
     * <p>
     * This method does nothing for a {@code null} input array.
     * </p>
     *
     * @param array
     *            the array to reverse, may be {@code null}
     * @param startIndexInclusive
     *            the starting index. Undervalue (&lt;0) is promoted to 0, overvalue (&gt;array.length) results in no
     *            change.
     * @param endIndexExclusive
     *            elements up to endIndex-1 are reversed in the array. Undervalue (&lt; start index) results in no
     *            change. Overvalue (&gt;array.length) is demoted to array length.
     * @since 3.2
     */
    public static void reverse(final byte[] array, final int startIndexInclusive, final int endIndexExclusive) {
        if (array == null) {
            return;
        }
        int i = Math.max(startIndexInclusive, 0);
        int j = Math.min(array.length, endIndexExclusive) - 1;
        byte tmp;
        while (j > i) {
            tmp = array[j];
            array[j] = array[i];
            array[i] = tmp;
            j--;
            i++;
        }
    }

    /**
     * Reverses the order of the given array.
     * <p>
     * This method does nothing for a {@code null} input array.
     * </p>
     *
     * @param array  the array to reverse, may be {@code null}
     */
    public static void reverse(final char[] array) {
        if (array != null) {
            reverse(array, 0, array.length);
        }
    }

    /**
     * Reverses the order of the given array in the given range.
     * <p>
     * This method does nothing for a {@code null} input array.
     * </p>
     *
     * @param array
     *            the array to reverse, may be {@code null}
     * @param startIndexInclusive
     *            the starting index. Undervalue (&lt;0) is promoted to 0, overvalue (&gt;array.length) results in no
     *            change.
     * @param endIndexExclusive
     *            elements up to endIndex-1 are reversed in the array. Undervalue (&lt; start index) results in no
     *            change. Overvalue (&gt;array.length) is demoted to array length.
     * @since 3.2
     */
    public static void reverse(final char[] array, final int startIndexInclusive, final int endIndexExclusive) {
        if (array == null) {
            return;
        }
        int i = Math.max(startIndexInclusive, 0);
        int j = Math.min(array.length, endIndexExclusive) - 1;
        char tmp;
        while (j > i) {
            tmp = array[j];
            array[j] = array[i];
            array[i] = tmp;
            j--;
            i++;
        }
    }

    /**
     * Reverses the order of the given array.
     * <p>
     * This method does nothing for a {@code null} input array.
     * </p>
     *
     * @param array  the array to reverse, may be {@code null}
     */
    public static void reverse(final double[] array) {
        if (array != null) {
            reverse(array, 0, array.length);
        }
    }

    /**
     * Reverses the order of the given array in the given range.
     * <p>
     * This method does nothing for a {@code null} input array.
     * </p>
     *
     * @param array
     *            the array to reverse, may be {@code null}
     * @param startIndexInclusive
     *            the starting index. Undervalue (&lt;0) is promoted to 0, overvalue (&gt;array.length) results in no
     *            change.
     * @param endIndexExclusive
     *            elements up to endIndex-1 are reversed in the array. Undervalue (&lt; start index) results in no
     *            change. Overvalue (&gt;array.length) is demoted to array length.
     * @since 3.2
     */
    public static void reverse(final double[] array, final int startIndexInclusive, final int endIndexExclusive) {
        if (array == null) {
            return;
        }
        int i = Math.max(startIndexInclusive, 0);
        int j = Math.min(array.length, endIndexExclusive) - 1;
        double tmp;
        while (j > i) {
            tmp = array[j];
            array[j] = array[i];
            array[i] = tmp;
            j--;
            i++;
        }
    }

    /**
     * Reverses the order of the given array.
     * <p>
     * This method does nothing for a {@code null} input array.
     * </p>
     *
     * @param array  the array to reverse, may be {@code null}.
     */
    public static void reverse(final float[] array) {
        if (array != null) {
            reverse(array, 0, array.length);
        }
    }

    /**
     * Reverses the order of the given array in the given range.
     * <p>
     * This method does nothing for a {@code null} input array.
     * </p>
     *
     * @param array
     *            the array to reverse, may be {@code null}.
     * @param startIndexInclusive
     *            the starting index. Undervalue (&lt;0) is promoted to 0, overvalue (&gt;array.length) results in no
     *            change.
     * @param endIndexExclusive
     *            elements up to endIndex-1 are reversed in the array. Undervalue (&lt; start index) results in no
     *            change. Overvalue (&gt;array.length) is demoted to array length.
     * @since 3.2
     */
    public static void reverse(final float[] array, final int startIndexInclusive, final int endIndexExclusive) {
        if (array == null) {
            return;
        }
        int i = Math.max(startIndexInclusive, 0);
        int j = Math.min(array.length, endIndexExclusive) - 1;
        float tmp;
        while (j > i) {
            tmp = array[j];
            array[j] = array[i];
            array[i] = tmp;
            j--;
            i++;
        }
    }

    /**
     * Reverses the order of the given array.
     * <p>
     * This method does nothing for a {@code null} input array.
     * </p>
     *
     * @param array  the array to reverse, may be {@code null}.
     */
    public static void reverse(final int[] array) {
        if (array != null) {
            reverse(array, 0, array.length);
        }
    }

    /**
     * Reverses the order of the given array in the given range.
     * <p>
     * This method does nothing for a {@code null} input array.
     * </p>
     *
     * @param array
     *            the array to reverse, may be {@code null}.
     * @param startIndexInclusive
     *            the starting index. Undervalue (&lt;0) is promoted to 0, overvalue (&gt;array.length) results in no
     *            change.
     * @param endIndexExclusive
     *            elements up to endIndex-1 are reversed in the array. Undervalue (&lt; start index) results in no
     *            change. Overvalue (&gt;array.length) is demoted to array length.
     * @since 3.2
     */
    public static void reverse(final int[] array, final int startIndexInclusive, final int endIndexExclusive) {
        if (array == null) {
            return;
        }
        int i = Math.max(startIndexInclusive, 0);
        int j = Math.min(array.length, endIndexExclusive) - 1;
        int tmp;
        while (j > i) {
            tmp = array[j];
            array[j] = array[i];
            array[i] = tmp;
            j--;
            i++;
        }
    }

    /**
     * Reverses the order of the given array.
     * <p>
     * This method does nothing for a {@code null} input array.
     * </p>
     *
     * @param array  the array to reverse, may be {@code null}.
     */
    public static void reverse(final long[] array) {
        if (array != null) {
            reverse(array, 0, array.length);
        }
    }

    /**
     * Reverses the order of the given array in the given range.
     * <p>
     * This method does nothing for a {@code null} input array.
     * </p>
     *
     * @param array
     *            the array to reverse, may be {@code null}.
     * @param startIndexInclusive
     *            the starting index. Undervalue (&lt;0) is promoted to 0, overvalue (&gt;array.length) results in no
     *            change.
     * @param endIndexExclusive
     *            elements up to endIndex-1 are reversed in the array. Undervalue (&lt; start index) results in no
     *            change. Overvalue (&gt;array.length) is demoted to array length.
     * @since 3.2
     */
    public static void reverse(final long[] array, final int startIndexInclusive, final int endIndexExclusive) {
        if (array == null) {
            return;
        }
        int i = Math.max(startIndexInclusive, 0);
        int j = Math.min(array.length, endIndexExclusive) - 1;
        long tmp;
        while (j > i) {
            tmp = array[j];
            array[j] = array[i];
            array[i] = tmp;
            j--;
            i++;
        }
    }

    /**
     * Reverses the order of the given array.
     * <p>
     * There is no special handling for multi-dimensional arrays.
     * </p>
     * <p>
     * This method does nothing for a {@code null} input array.
     * </p>
     *
     * @param array  the array to reverse, may be {@code null}.
     */
    public static void reverse(final Object[] array) {
        if (array != null) {
            reverse(array, 0, array.length);
        }
    }

    /**
     * Reverses the order of the given array in the given range.
     * <p>
     * This method does nothing for a {@code null} input array.
     * </p>
     *
     * @param array
     *            the array to reverse, may be {@code null}.
     * @param startIndexInclusive
     *            the starting index. Under value (&lt;0) is promoted to 0, over value (&gt;array.length) results in no
     *            change.
     * @param endIndexExclusive
     *            elements up to endIndex-1 are reversed in the array. Under value (&lt; start index) results in no
     *            change. Over value (&gt;array.length) is demoted to array length.
     * @since 3.2
     */
    public static void reverse(final Object[] array, final int startIndexInclusive, final int endIndexExclusive) {
        if (array == null) {
            return;
        }
        int i = Math.max(startIndexInclusive, 0);
        int j = Math.min(array.length, endIndexExclusive) - 1;
        Object tmp;
        while (j > i) {
            tmp = array[j];
            array[j] = array[i];
            array[i] = tmp;
            j--;
            i++;
        }
    }

    /**
     * Reverses the order of the given array.
     * <p>
     * This method does nothing for a {@code null} input array.
     * </p>
     *
     * @param array  the array to reverse, may be {@code null}.
     */
    public static void reverse(final short[] array) {
        if (array != null) {
            reverse(array, 0, array.length);
        }
    }

    /**
     * Reverses the order of the given array in the given range.
     * <p>
     * This method does nothing for a {@code null} input array.
     * </p>
     *
     * @param array
     *            the array to reverse, may be {@code null}.
     * @param startIndexInclusive
     *            the starting index. Undervalue (&lt;0) is promoted to 0, overvalue (&gt;array.length) results in no
     *            change.
     * @param endIndexExclusive
     *            elements up to endIndex-1 are reversed in the array. Undervalue (&lt; start index) results in no
     *            change. Overvalue (&gt;array.length) is demoted to array length.
     * @since 3.2
     */
    public static void reverse(final short[] array, final int startIndexInclusive, final int endIndexExclusive) {
        if (array == null) {
            return;
        }
        int i = Math.max(startIndexInclusive, 0);
        int j = Math.min(array.length, endIndexExclusive) - 1;
        short tmp;
        while (j > i) {
            tmp = array[j];
            array[j] = array[i];
            array[i] = tmp;
            j--;
            i++;
        }
    }

    /**
     * Sets all elements of the specified array, using the provided generator supplier to compute each element.
     * <p>
     * If the generator supplier throws an exception, it is relayed to the caller and the array is left in an indeterminate
     * state.
     * </p>
     *
     * @param <T> type of elements of the array, may be {@code null}.
     * @param array array to be initialized, may be {@code null}.
     * @param generator a function accepting an index and producing the desired value for that position.
     * @return the input array
     * @since 3.13.0
     */
    public static <T> T[] setAll(final T[] array, final IntFunction<? extends T> generator) {
        if (array != null && generator != null) {
            Arrays.setAll(array, generator);
        }
        return array;
    }

    /**
     * Sets all elements of the specified array, using the provided generator supplier to compute each element.
     * <p>
     * If the generator supplier throws an exception, it is relayed to the caller and the array is left in an indeterminate
     * state.
     * </p>
     *
     * @param <T> type of elements of the array, may be {@code null}.
     * @param array array to be initialized, may be {@code null}.
     * @param generator a function accepting an index and producing the desired value for that position.
     * @return the input array
     * @since 3.13.0
     */
    public static <T> T[] setAll(final T[] array, final Supplier<? extends T> generator) {
        if (array != null && generator != null) {
            for (int i = 0; i < array.length; i++) {
                array[i] = generator.get();
            }
        }
        return array;
    }

    /**
     * Shifts the order of the given boolean array.
     *
     * <p>There is no special handling for multi-dimensional arrays. This method
     * does nothing for {@code null} or empty input arrays.</p>
     *
     * @param array  the array to shift, may be {@code null}.
     * @param offset
     *          The number of positions to rotate the elements.  If the offset is larger than the number of elements to
     *          rotate, than the effective offset is modulo the number of elements to rotate.
     * @since 3.5
     */
    public static void shift(final boolean[] array, final int offset) {
        if (array != null) {
            shift(array, 0, array.length, offset);
        }
    }

    /**
     * Shifts the order of a series of elements in the given boolean array.
     *
     * <p>There is no special handling for multi-dimensional arrays. This method
     * does nothing for {@code null} or empty input arrays.</p>
     *
     * @param array
     *            the array to shift, may be {@code null}.
     * @param startIndexInclusive
     *            the starting index. Undervalue (&lt;0) is promoted to 0, overvalue (&gt;array.length) results in no
     *            change.
     * @param endIndexExclusive
     *            elements up to endIndex-1 are shifted in the array. Undervalue (&lt; start index) results in no
     *            change. Overvalue (&gt;array.length) is demoted to array length.
     * @param offset
     *          The number of positions to rotate the elements.  If the offset is larger than the number of elements to
     *          rotate, than the effective offset is modulo the number of elements to rotate.
     * @since 3.5
     */
    public static void shift(final boolean[] array, int startIndexInclusive, int endIndexExclusive, int offset) {
        if (array == null || startIndexInclusive >= array.length - 1 || endIndexExclusive <= 0) {
            return;
        }
        startIndexInclusive = max0(startIndexInclusive);
        endIndexExclusive = Math.min(endIndexExclusive, array.length);
        int n = endIndexExclusive - startIndexInclusive;
        if (n <= 1) {
            return;
        }
        offset %= n;
        if (offset < 0) {
            offset += n;
        }
        // For algorithm explanations and proof of O(n) time complexity and O(1) space complexity
        // see https://beradrian.wordpress.com/2015/04/07/shift-an-array-in-on-in-place/
        while (n > 1 && offset > 0) {
            final int nOffset = n - offset;
            if (offset > nOffset) {
                swap(array, startIndexInclusive, startIndexInclusive + n - nOffset,  nOffset);
                n = offset;
                offset -= nOffset;
            } else if (offset < nOffset) {
                swap(array, startIndexInclusive, startIndexInclusive + nOffset,  offset);
                startIndexInclusive += offset;
                n = nOffset;
            } else {
                swap(array, startIndexInclusive, startIndexInclusive + nOffset, offset);
                break;
            }
        }
    }

    /**
     * Shifts the order of the given byte array.
     *
     * <p>There is no special handling for multi-dimensional arrays. This method
     * does nothing for {@code null} or empty input arrays.</p>
     *
     * @param array  the array to shift, may be {@code null}.
     * @param offset
     *          The number of positions to rotate the elements.  If the offset is larger than the number of elements to
     *          rotate, than the effective offset is modulo the number of elements to rotate.
     * @since 3.5
     */
    public static void shift(final byte[] array, final int offset) {
        if (array != null) {
            shift(array, 0, array.length, offset);
        }
    }

    /**
     * Shifts the order of a series of elements in the given byte array.
     *
     * <p>There is no special handling for multi-dimensional arrays. This method
     * does nothing for {@code null} or empty input arrays.</p>
     *
     * @param array
     *            the array to shift, may be {@code null}.
     * @param startIndexInclusive
     *            the starting index. Undervalue (&lt;0) is promoted to 0, overvalue (&gt;array.length) results in no
     *            change.
     * @param endIndexExclusive
     *            elements up to endIndex-1 are shifted in the array. Undervalue (&lt; start index) results in no
     *            change. Overvalue (&gt;array.length) is demoted to array length.
     * @param offset
     *          The number of positions to rotate the elements.  If the offset is larger than the number of elements to
     *          rotate, than the effective offset is modulo the number of elements to rotate.
     * @since 3.5
     */
    public static void shift(final byte[] array, int startIndexInclusive, int endIndexExclusive, int offset) {
        if (array == null || startIndexInclusive >= array.length - 1 || endIndexExclusive <= 0) {
            return;
        }
        startIndexInclusive = max0(startIndexInclusive);
        endIndexExclusive = Math.min(endIndexExclusive, array.length);
        int n = endIndexExclusive - startIndexInclusive;
        if (n <= 1) {
            return;
        }
        offset %= n;
        if (offset < 0) {
            offset += n;
        }
        // For algorithm explanations and proof of O(n) time complexity and O(1) space complexity
        // see https://beradrian.wordpress.com/2015/04/07/shift-an-array-in-on-in-place/
        while (n > 1 && offset > 0) {
            final int nOffset = n - offset;
            if (offset > nOffset) {
                swap(array, startIndexInclusive, startIndexInclusive + n - nOffset,  nOffset);
                n = offset;
                offset -= nOffset;
            } else if (offset < nOffset) {
                swap(array, startIndexInclusive, startIndexInclusive + nOffset,  offset);
                startIndexInclusive += offset;
                n = nOffset;
            } else {
                swap(array, startIndexInclusive, startIndexInclusive + nOffset, offset);
                break;
            }
        }
    }

    /**
     * Shifts the order of the given char array.
     *
     * <p>There is no special handling for multi-dimensional arrays. This method
     * does nothing for {@code null} or empty input arrays.</p>
     *
     * @param array  the array to shift, may be {@code null}.
     * @param offset
     *          The number of positions to rotate the elements.  If the offset is larger than the number of elements to
     *          rotate, than the effective offset is modulo the number of elements to rotate.
     * @since 3.5
     */
    public static void shift(final char[] array, final int offset) {
        if (array != null) {
            shift(array, 0, array.length, offset);
        }
    }

    /**
     * Shifts the order of a series of elements in the given char array.
     *
     * <p>There is no special handling for multi-dimensional arrays. This method
     * does nothing for {@code null} or empty input arrays.</p>
     *
     * @param array
     *            the array to shift, may be {@code null}.
     * @param startIndexInclusive
     *            the starting index. Undervalue (&lt;0) is promoted to 0, overvalue (&gt;array.length) results in no
     *            change.
     * @param endIndexExclusive
     *            elements up to endIndex-1 are shifted in the array. Undervalue (&lt; start index) results in no
     *            change. Overvalue (&gt;array.length) is demoted to array length.
     * @param offset
     *          The number of positions to rotate the elements.  If the offset is larger than the number of elements to
     *          rotate, than the effective offset is modulo the number of elements to rotate.
     * @since 3.5
     */
    public static void shift(final char[] array, int startIndexInclusive, int endIndexExclusive, int offset) {
        if (array == null || startIndexInclusive >= array.length - 1 || endIndexExclusive <= 0) {
            return;
        }
        startIndexInclusive = max0(startIndexInclusive);
        endIndexExclusive = Math.min(endIndexExclusive, array.length);
        int n = endIndexExclusive - startIndexInclusive;
        if (n <= 1) {
            return;
        }
        offset %= n;
        if (offset < 0) {
            offset += n;
        }
        // For algorithm explanations and proof of O(n) time complexity and O(1) space complexity
        // see https://beradrian.wordpress.com/2015/04/07/shift-an-array-in-on-in-place/
        while (n > 1 && offset > 0) {
            final int nOffset = n - offset;
            if (offset > nOffset) {
                swap(array, startIndexInclusive, startIndexInclusive + n - nOffset,  nOffset);
                n = offset;
                offset -= nOffset;
            } else if (offset < nOffset) {
                swap(array, startIndexInclusive, startIndexInclusive + nOffset,  offset);
                startIndexInclusive += offset;
                n = nOffset;
            } else {
                swap(array, startIndexInclusive, startIndexInclusive + nOffset, offset);
                break;
            }
        }
    }

    /**
     * Shifts the order of the given double array.
     *
     * <p>There is no special handling for multi-dimensional arrays. This method
     * does nothing for {@code null} or empty input arrays.</p>
     *
     * @param array  the array to shift, may be {@code null}.
     * @param offset
     *          The number of positions to rotate the elements.  If the offset is larger than the number of elements to
     *          rotate, than the effective offset is modulo the number of elements to rotate.
     * @since 3.5
     */
    public static void shift(final double[] array, final int offset) {
        if (array != null) {
            shift(array, 0, array.length, offset);
        }
    }

    /**
     * Shifts the order of a series of elements in the given double array.
     *
     * <p>There is no special handling for multi-dimensional arrays. This method
     * does nothing for {@code null} or empty input arrays.</p>
     *
     * @param array
     *            the array to shift, may be {@code null}.
     * @param startIndexInclusive
     *            the starting index. Undervalue (&lt;0) is promoted to 0, overvalue (&gt;array.length) results in no
     *            change.
     * @param endIndexExclusive
     *            elements up to endIndex-1 are shifted in the array. Undervalue (&lt; start index) results in no
     *            change. Overvalue (&gt;array.length) is demoted to array length.
     * @param offset
     *          The number of positions to rotate the elements.  If the offset is larger than the number of elements to
     *          rotate, than the effective offset is modulo the number of elements to rotate.
     * @since 3.5
     */
    public static void shift(final double[] array, int startIndexInclusive, int endIndexExclusive, int offset) {
        if (array == null || startIndexInclusive >= array.length - 1 || endIndexExclusive <= 0) {
            return;
        }
        startIndexInclusive = max0(startIndexInclusive);
        endIndexExclusive = Math.min(endIndexExclusive, array.length);
        int n = endIndexExclusive - startIndexInclusive;
        if (n <= 1) {
            return;
        }
        offset %= n;
        if (offset < 0) {
            offset += n;
        }
        // For algorithm explanations and proof of O(n) time complexity and O(1) space complexity
        // see https://beradrian.wordpress.com/2015/04/07/shift-an-array-in-on-in-place/
        while (n > 1 && offset > 0) {
            final int nOffset = n - offset;
            if (offset > nOffset) {
                swap(array, startIndexInclusive, startIndexInclusive + n - nOffset,  nOffset);
                n = offset;
                offset -= nOffset;
            } else if (offset < nOffset) {
                swap(array, startIndexInclusive, startIndexInclusive + nOffset,  offset);
                startIndexInclusive += offset;
                n = nOffset;
            } else {
                swap(array, startIndexInclusive, startIndexInclusive + nOffset, offset);
                break;
            }
        }
    }

    /**
     * Shifts the order of the given float array.
     *
     * <p>There is no special handling for multi-dimensional arrays. This method
     * does nothing for {@code null} or empty input arrays.</p>
     *
     * @param array  the array to shift, may be {@code null}.
     * @param offset
     *          The number of positions to rotate the elements.  If the offset is larger than the number of elements to
     *          rotate, than the effective offset is modulo the number of elements to rotate.
     * @since 3.5
     */
    public static void shift(final float[] array, final int offset) {
        if (array != null) {
            shift(array, 0, array.length, offset);
        }
    }

    /**
     * Shifts the order of a series of elements in the given float array.
     *
     * <p>There is no special handling for multi-dimensional arrays. This method
     * does nothing for {@code null} or empty input arrays.</p>
     *
     * @param array
     *            the array to shift, may be {@code null}.
     * @param startIndexInclusive
     *            the starting index. Undervalue (&lt;0) is promoted to 0, overvalue (&gt;array.length) results in no
     *            change.
     * @param endIndexExclusive
     *            elements up to endIndex-1 are shifted in the array. Undervalue (&lt; start index) results in no
     *            change. Overvalue (&gt;array.length) is demoted to array length.
     * @param offset
     *          The number of positions to rotate the elements.  If the offset is larger than the number of elements to
     *          rotate, than the effective offset is modulo the number of elements to rotate.
     * @since 3.5
     */
    public static void shift(final float[] array, int startIndexInclusive, int endIndexExclusive, int offset) {
        if (array == null || startIndexInclusive >= array.length - 1 || endIndexExclusive <= 0) {
            return;
        }
        startIndexInclusive = max0(startIndexInclusive);
        endIndexExclusive = Math.min(endIndexExclusive, array.length);
        int n = endIndexExclusive - startIndexInclusive;
        if (n <= 1) {
            return;
        }
        offset %= n;
        if (offset < 0) {
            offset += n;
        }
        // For algorithm explanations and proof of O(n) time complexity and O(1) space complexity
        // see https://beradrian.wordpress.com/2015/04/07/shift-an-array-in-on-in-place/
        while (n > 1 && offset > 0) {
            final int nOffset = n - offset;
            if (offset > nOffset) {
                swap(array, startIndexInclusive, startIndexInclusive + n - nOffset,  nOffset);
                n = offset;
                offset -= nOffset;
            } else if (offset < nOffset) {
                swap(array, startIndexInclusive, startIndexInclusive + nOffset,  offset);
                startIndexInclusive += offset;
                n = nOffset;
            } else {
                swap(array, startIndexInclusive, startIndexInclusive + nOffset, offset);
                break;
            }
        }
    }

    /**
     * Shifts the order of the given int array.
     *
     * <p>There is no special handling for multi-dimensional arrays. This method
     * does nothing for {@code null} or empty input arrays.</p>
     *
     * @param array  the array to shift, may be {@code null}.
     * @param offset
     *          The number of positions to rotate the elements.  If the offset is larger than the number of elements to
     *          rotate, than the effective offset is modulo the number of elements to rotate.
     * @since 3.5
     */
    public static void shift(final int[] array, final int offset) {
        if (array != null) {
            shift(array, 0, array.length, offset);
        }
    }

    /**
     * Shifts the order of a series of elements in the given int array.
     *
     * <p>There is no special handling for multi-dimensional arrays. This method
     * does nothing for {@code null} or empty input arrays.</p>
     *
     * @param array
     *            the array to shift, may be {@code null}.
     * @param startIndexInclusive
     *            the starting index. Undervalue (&lt;0) is promoted to 0, overvalue (&gt;array.length) results in no
     *            change.
     * @param endIndexExclusive
     *            elements up to endIndex-1 are shifted in the array. Undervalue (&lt; start index) results in no
     *            change. Overvalue (&gt;array.length) is demoted to array length.
     * @param offset
     *          The number of positions to rotate the elements.  If the offset is larger than the number of elements to
     *          rotate, than the effective offset is modulo the number of elements to rotate.
     * @since 3.5
     */
    public static void shift(final int[] array, int startIndexInclusive, int endIndexExclusive, int offset) {
        if (array == null || startIndexInclusive >= array.length - 1 || endIndexExclusive <= 0) {
            return;
        }
        startIndexInclusive = max0(startIndexInclusive);
        endIndexExclusive = Math.min(endIndexExclusive, array.length);
        int n = endIndexExclusive - startIndexInclusive;
        if (n <= 1) {
            return;
        }
        offset %= n;
        if (offset < 0) {
            offset += n;
        }
        // For algorithm explanations and proof of O(n) time complexity and O(1) space complexity
        // see https://beradrian.wordpress.com/2015/04/07/shift-an-array-in-on-in-place/
        while (n > 1 && offset > 0) {
            final int nOffset = n - offset;
            if (offset > nOffset) {
                swap(array, startIndexInclusive, startIndexInclusive + n - nOffset,  nOffset);
                n = offset;
                offset -= nOffset;
            } else if (offset < nOffset) {
                swap(array, startIndexInclusive, startIndexInclusive + nOffset,  offset);
                startIndexInclusive += offset;
                n = nOffset;
            } else {
                swap(array, startIndexInclusive, startIndexInclusive + nOffset, offset);
                break;
            }
        }
    }

    /**
     * Shifts the order of the given long array.
     *
     * <p>There is no special handling for multi-dimensional arrays. This method
     * does nothing for {@code null} or empty input arrays.</p>
     *
     * @param array  the array to shift, may be {@code null}.
     * @param offset
     *          The number of positions to rotate the elements.  If the offset is larger than the number of elements to
     *          rotate, than the effective offset is modulo the number of elements to rotate.
     * @since 3.5
     */
    public static void shift(final long[] array, final int offset) {
        if (array != null) {
            shift(array, 0, array.length, offset);
        }
    }

    /**
     * Shifts the order of a series of elements in the given long array.
     *
     * <p>There is no special handling for multi-dimensional arrays. This method
     * does nothing for {@code null} or empty input arrays.</p>
     *
     * @param array
     *            the array to shift, may be {@code null}.
     * @param startIndexInclusive
     *            the starting index. Undervalue (&lt;0) is promoted to 0, overvalue (&gt;array.length) results in no
     *            change.
     * @param endIndexExclusive
     *            elements up to endIndex-1 are shifted in the array. Undervalue (&lt; start index) results in no
     *            change. Overvalue (&gt;array.length) is demoted to array length.
     * @param offset
     *          The number of positions to rotate the elements.  If the offset is larger than the number of elements to
     *          rotate, than the effective offset is modulo the number of elements to rotate.
     * @since 3.5
     */
    public static void shift(final long[] array, int startIndexInclusive, int endIndexExclusive, int offset) {
        if (array == null || startIndexInclusive >= array.length - 1 || endIndexExclusive <= 0) {
            return;
        }
        startIndexInclusive = max0(startIndexInclusive);
        endIndexExclusive = Math.min(endIndexExclusive, array.length);
        int n = endIndexExclusive - startIndexInclusive;
        if (n <= 1) {
            return;
        }
        offset %= n;
        if (offset < 0) {
            offset += n;
        }
        // For algorithm explanations and proof of O(n) time complexity and O(1) space complexity
        // see https://beradrian.wordpress.com/2015/04/07/shift-an-array-in-on-in-place/
        while (n > 1 && offset > 0) {
            final int nOffset = n - offset;
            if (offset > nOffset) {
                swap(array, startIndexInclusive, startIndexInclusive + n - nOffset,  nOffset);
                n = offset;
                offset -= nOffset;
            } else if (offset < nOffset) {
                swap(array, startIndexInclusive, startIndexInclusive + nOffset,  offset);
                startIndexInclusive += offset;
                n = nOffset;
            } else {
                swap(array, startIndexInclusive, startIndexInclusive + nOffset, offset);
                break;
            }
        }
    }

    /**
     * Shifts the order of the given array.
     *
     * <p>There is no special handling for multi-dimensional arrays. This method
     * does nothing for {@code null} or empty input arrays.</p>
     *
     * @param array  the array to shift, may be {@code null}.
     * @param offset
     *          The number of positions to rotate the elements.  If the offset is larger than the number of elements to
     *          rotate, than the effective offset is modulo the number of elements to rotate.
     * @since 3.5
     */
    public static void shift(final Object[] array, final int offset) {
        if (array != null) {
            shift(array, 0, array.length, offset);
        }
    }

    /**
     * Shifts the order of a series of elements in the given array.
     *
     * <p>There is no special handling for multi-dimensional arrays. This method
     * does nothing for {@code null} or empty input arrays.</p>
     *
     * @param array
     *            the array to shift, may be {@code null}.
     * @param startIndexInclusive
     *            the starting index. Undervalue (&lt;0) is promoted to 0, overvalue (&gt;array.length) results in no
     *            change.
     * @param endIndexExclusive
     *            elements up to endIndex-1 are shifted in the array. Undervalue (&lt; start index) results in no
     *            change. Overvalue (&gt;array.length) is demoted to array length.
     * @param offset
     *          The number of positions to rotate the elements.  If the offset is larger than the number of elements to
     *          rotate, than the effective offset is modulo the number of elements to rotate.
     * @since 3.5
     */
    public static void shift(final Object[] array, int startIndexInclusive, int endIndexExclusive, int offset) {
        if (array == null || startIndexInclusive >= array.length - 1 || endIndexExclusive <= 0) {
            return;
        }
        startIndexInclusive = max0(startIndexInclusive);
        endIndexExclusive = Math.min(endIndexExclusive, array.length);
        int n = endIndexExclusive - startIndexInclusive;
        if (n <= 1) {
            return;
        }
        offset %= n;
        if (offset < 0) {
            offset += n;
        }
        // For algorithm explanations and proof of O(n) time complexity and O(1) space complexity
        // see https://beradrian.wordpress.com/2015/04/07/shift-an-array-in-on-in-place/
        while (n > 1 && offset > 0) {
            final int nOffset = n - offset;
            if (offset > nOffset) {
                swap(array, startIndexInclusive, startIndexInclusive + n - nOffset,  nOffset);
                n = offset;
                offset -= nOffset;
            } else if (offset < nOffset) {
                swap(array, startIndexInclusive, startIndexInclusive + nOffset,  offset);
                startIndexInclusive += offset;
                n = nOffset;
            } else {
                swap(array, startIndexInclusive, startIndexInclusive + nOffset, offset);
                break;
            }
        }
    }

    /**
     * Shifts the order of the given short array.
     *
     * <p>There is no special handling for multi-dimensional arrays. This method
     * does nothing for {@code null} or empty input arrays.</p>
     *
     * @param array  the array to shift, may be {@code null}.
     * @param offset
     *          The number of positions to rotate the elements.  If the offset is larger than the number of elements to
     *          rotate, than the effective offset is modulo the number of elements to rotate.
     * @since 3.5
     */
    public static void shift(final short[] array, final int offset) {
        if (array != null) {
            shift(array, 0, array.length, offset);
        }
    }

    /**
     * Shifts the order of a series of elements in the given short array.
     *
     * <p>There is no special handling for multi-dimensional arrays. This method
     * does nothing for {@code null} or empty input arrays.</p>
     *
     * @param array
     *            the array to shift, may be {@code null}.
     * @param startIndexInclusive
     *            the starting index. Undervalue (&lt;0) is promoted to 0, overvalue (&gt;array.length) results in no
     *            change.
     * @param endIndexExclusive
     *            elements up to endIndex-1 are shifted in the array. Undervalue (&lt; start index) results in no
     *            change. Overvalue (&gt;array.length) is demoted to array length.
     * @param offset
     *          The number of positions to rotate the elements.  If the offset is larger than the number of elements to
     *          rotate, than the effective offset is modulo the number of elements to rotate.
     * @since 3.5
     */
    public static void shift(final short[] array, int startIndexInclusive, int endIndexExclusive, int offset) {
        if (array == null || startIndexInclusive >= array.length - 1 || endIndexExclusive <= 0) {
            return;
        }
        startIndexInclusive = max0(startIndexInclusive);
        endIndexExclusive = Math.min(endIndexExclusive, array.length);
        int n = endIndexExclusive - startIndexInclusive;
        if (n <= 1) {
            return;
        }
        offset %= n;
        if (offset < 0) {
            offset += n;
        }
        // For algorithm explanations and proof of O(n) time complexity and O(1) space complexity
        // see https://beradrian.wordpress.com/2015/04/07/shift-an-array-in-on-in-place/
        while (n > 1 && offset > 0) {
            final int nOffset = n - offset;
            if (offset > nOffset) {
                swap(array, startIndexInclusive, startIndexInclusive + n - nOffset,  nOffset);
                n = offset;
                offset -= nOffset;
            } else if (offset < nOffset) {
                swap(array, startIndexInclusive, startIndexInclusive + nOffset,  offset);
                startIndexInclusive += offset;
                n = nOffset;
            } else {
                swap(array, startIndexInclusive, startIndexInclusive + nOffset, offset);
                break;
            }
        }
    }

    /**
     * Shuffles randomly the elements of the specified array using the <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle
     * algorithm</a>.
     * <p>
     * This method uses the current {@link ThreadLocalRandom} as its random number generator.
     * </p>
     * <p>
     * Instances of {@link ThreadLocalRandom} are not cryptographically secure. For security-sensitive applications, consider using a {@code shuffle} method
     * with a {@link SecureRandom} argument.
     * </p>
     *
     * @param array the array to shuffle.
     * @see <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle algorithm</a>
     * @since 3.6
     */
    public static void shuffle(final boolean[] array) {
        shuffle(array, random());
    }

    /**
     * Shuffles randomly the elements of the specified array using the <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle
     * algorithm</a>.
     *
     * @param array  the array to shuffle
     * @param random the source of randomness used to permute the elements
     * @see <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle algorithm</a>
     * @since 3.6
     */
    public static void shuffle(final boolean[] array, final Random random) {
        for (int i = array.length; i > 1; i--) {
            swap(array, i - 1, random.nextInt(i), 1);
        }
    }

    /**
     * Shuffles randomly the elements of the specified array using the <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle
     * algorithm</a>.
     * <p>
     * This method uses the current {@link ThreadLocalRandom} as its random number generator.
     * </p>
     * <p>
     * Instances of {@link ThreadLocalRandom} are not cryptographically secure. For security-sensitive applications, consider using a {@code shuffle} method
     * with a {@link SecureRandom} argument.
     * </p>
     *
     * @param array the array to shuffle.
     * @see <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle algorithm</a>
     * @since 3.6
     */
    public static void shuffle(final byte[] array) {
        shuffle(array, random());
    }

    /**
     * Shuffles randomly the elements of the specified array using the <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle
     * algorithm</a>.
     *
     * @param array  the array to shuffle
     * @param random the source of randomness used to permute the elements
     * @see <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle algorithm</a>
     * @since 3.6
     */
    public static void shuffle(final byte[] array, final Random random) {
        for (int i = array.length; i > 1; i--) {
            swap(array, i - 1, random.nextInt(i), 1);
        }
    }

    /**
     * Shuffles randomly the elements of the specified array using the <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle
     * algorithm</a>.
     * <p>
     * This method uses the current {@link ThreadLocalRandom} as its random number generator.
     * </p>
     * <p>
     * Instances of {@link ThreadLocalRandom} are not cryptographically secure. For security-sensitive applications, consider using a {@code shuffle} method
     * with a {@link SecureRandom} argument.
     * </p>
     *
     * @param array the array to shuffle.
     * @see <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle algorithm</a>
     * @since 3.6
     */
    public static void shuffle(final char[] array) {
        shuffle(array, random());
    }

    /**
     * Shuffles randomly the elements of the specified array using the <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle
     * algorithm</a>.
     *
     * @param array  the array to shuffle
     * @param random the source of randomness used to permute the elements
     * @see <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle algorithm</a>
     * @since 3.6
     */
    public static void shuffle(final char[] array, final Random random) {
        for (int i = array.length; i > 1; i--) {
            swap(array, i - 1, random.nextInt(i), 1);
        }
    }

    /**
     * Shuffles randomly the elements of the specified array using the <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle
     * algorithm</a>.
     * <p>
     * This method uses the current {@link ThreadLocalRandom} as its random number generator.
     * </p>
     * <p>
     * Instances of {@link ThreadLocalRandom} are not cryptographically secure. For security-sensitive applications, consider using a {@code shuffle} method
     * with a {@link SecureRandom} argument.
     * </p>
     *
     * @param array the array to shuffle.
     * @see <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle algorithm</a>
     * @since 3.6
     */
    public static void shuffle(final double[] array) {
        shuffle(array, random());
    }

    /**
     * Shuffles randomly the elements of the specified array using the <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle
     * algorithm</a>.
     *
     * @param array  the array to shuffle
     * @param random the source of randomness used to permute the elements
     * @see <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle algorithm</a>
     * @since 3.6
     */
    public static void shuffle(final double[] array, final Random random) {
        for (int i = array.length; i > 1; i--) {
            swap(array, i - 1, random.nextInt(i), 1);
        }
    }

    /**
     * Shuffles randomly the elements of the specified array using the <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle
     * algorithm</a>.
     * <p>
     * This method uses the current {@link ThreadLocalRandom} as its random number generator.
     * </p>
     * <p>
     * Instances of {@link ThreadLocalRandom} are not cryptographically secure. For security-sensitive applications, consider using a {@code shuffle} method
     * with a {@link SecureRandom} argument.
     * </p>
     *
     * @param array the array to shuffle.
     * @see <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle algorithm</a>
     * @since 3.6
     */
    public static void shuffle(final float[] array) {
        shuffle(array, random());
    }

    /**
     * Shuffles randomly the elements of the specified array using the <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle
     * algorithm</a>.
     *
     * @param array  the array to shuffle
     * @param random the source of randomness used to permute the elements
     * @see <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle algorithm</a>
     * @since 3.6
     */
    public static void shuffle(final float[] array, final Random random) {
        for (int i = array.length; i > 1; i--) {
            swap(array, i - 1, random.nextInt(i), 1);
        }
    }

    /**
     * Shuffles randomly the elements of the specified array using the <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle
     * algorithm</a>.
     * <p>
     * This method uses the current {@link ThreadLocalRandom} as its random number generator.
     * </p>
     * <p>
     * Instances of {@link ThreadLocalRandom} are not cryptographically secure. For security-sensitive applications, consider using a {@code shuffle} method
     * with a {@link SecureRandom} argument.
     * </p>
     *
     * @param array the array to shuffle.
     * @see <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle algorithm</a>
     * @since 3.6
     */
    public static void shuffle(final int[] array) {
        shuffle(array, random());
    }

    /**
     * Shuffles randomly the elements of the specified array using the <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle
     * algorithm</a>.
     *
     * @param array  the array to shuffle
     * @param random the source of randomness used to permute the elements
     * @see <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle algorithm</a>
     * @since 3.6
     */
    public static void shuffle(final int[] array, final Random random) {
        for (int i = array.length; i > 1; i--) {
            swap(array, i - 1, random.nextInt(i), 1);
        }
    }

    /**
     * Shuffles randomly the elements of the specified array using the <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle
     * algorithm</a>.
     * <p>
     * This method uses the current {@link ThreadLocalRandom} as its random number generator.
     * </p>
     * <p>
     * Instances of {@link ThreadLocalRandom} are not cryptographically secure. For security-sensitive applications, consider using a {@code shuffle} method
     * with a {@link SecureRandom} argument.
     * </p>
     *
     * @param array the array to shuffle.
     * @see <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle algorithm</a>
     * @since 3.6
     */
    public static void shuffle(final long[] array) {
        shuffle(array, random());
    }

    /**
     * Shuffles randomly the elements of the specified array using the <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle
     * algorithm</a>.
     *
     * @param array  the array to shuffle
     * @param random the source of randomness used to permute the elements
     * @see <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle algorithm</a>
     * @since 3.6
     */
    public static void shuffle(final long[] array, final Random random) {
        for (int i = array.length; i > 1; i--) {
            swap(array, i - 1, random.nextInt(i), 1);
        }
    }

    /**
     * Shuffles randomly the elements of the specified array using the <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle
     * algorithm</a>.
     * <p>
     * This method uses the current {@link ThreadLocalRandom} as its random number generator.
     * </p>
     * <p>
     * Instances of {@link ThreadLocalRandom} are not cryptographically secure. For security-sensitive applications, consider using a {@code shuffle} method
     * with a {@link SecureRandom} argument.
     * </p>
     *
     * @param array the array to shuffle.
     * @see <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle algorithm</a>
     * @since 3.6
     */
    public static void shuffle(final Object[] array) {
        shuffle(array, random());
    }

    /**
     * Shuffles randomly the elements of the specified array using the <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle
     * algorithm</a>.
     *
     * @param array  the array to shuffle
     * @param random the source of randomness used to permute the elements
     * @see <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle algorithm</a>
     * @since 3.6
     */
    public static void shuffle(final Object[] array, final Random random) {
        for (int i = array.length; i > 1; i--) {
            swap(array, i - 1, random.nextInt(i), 1);
        }
    }

    /**
     * Shuffles randomly the elements of the specified array using the <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle
     * algorithm</a>.
     * <p>
     * This method uses the current {@link ThreadLocalRandom} as its random number generator.
     * </p>
     * <p>
     * Instances of {@link ThreadLocalRandom} are not cryptographically secure. For security-sensitive applications, consider using a {@code shuffle} method
     * with a {@link SecureRandom} argument.
     * </p>
     *
     * @param array the array to shuffle.
     * @see <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle algorithm</a>
     * @since 3.6
     */
    public static void shuffle(final short[] array) {
        shuffle(array, random());
    }

    /**
     * Shuffles randomly the elements of the specified array using the <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle
     * algorithm</a>.
     *
     * @param array  the array to shuffle
     * @param random the source of randomness used to permute the elements
     * @see <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle algorithm</a>
     * @since 3.6
     */
    public static void shuffle(final short[] array, final Random random) {
        for (int i = array.length; i > 1; i--) {
            swap(array, i - 1, random.nextInt(i), 1);
        }
    }

    /**
     * Tests whether the given data array starts with an expected array, for example, signature bytes.
     * <p>
     * If both arrays are null, the method returns true. The method return false when one array is null and the other not.
     * </p>
     *
     * @param data     The data to search, maybe larger than the expected data.
     * @param expected The expected data to find.
     * @return whether a match was found.
     * @since 3.18.0
     */
    public static boolean startsWith(final byte[] data, final byte[] expected) {
        if (data == expected) {
            return true;
        }
        if (data == null || expected == null) {
            return false;
        }
        final int dataLen = data.length;
        if (expected.length > dataLen) {
            return false;
        }
        if (expected.length == dataLen) {
            // delegate to Arrays.equals() which has optimizations on Java > 8
            return Arrays.equals(data, expected);
        }
        // Once we are on Java 9+ we can delegate to Arrays here as well (or not).
        for (int i = 0; i < expected.length; i++) {
            if (data[i] != expected[i]) {
                return false;
            }
        }
        return true;
    }

    /**
     * Produces a new {@code boolean} array containing the elements
     * between the start and end indices.
     * <p>
     * The start index is inclusive, the end index exclusive.
     * Null array input produces null output.
     * </p>
     *
     * @param array  the array
     * @param startIndexInclusive  the starting index. Undervalue (&lt;0)
     *      is promoted to 0, overvalue (&gt;array.length) results
     *      in an empty array.
     * @param endIndexExclusive  elements up to endIndex-1 are present in the
     *      returned subarray. Undervalue (&lt; startIndex) produces
     *      empty array, overvalue (&gt;array.length) is demoted to
     *      array length.
     * @return a new array containing the elements between
     *      the start and end indices.
     * @since 2.1
     * @see Arrays#copyOfRange(boolean[], int, int)
     */
    public static boolean[] subarray(final boolean[] array, int startIndexInclusive, int endIndexExclusive) {
        if (array == null) {
            return null;
        }
        startIndexInclusive = max0(startIndexInclusive);
        endIndexExclusive = Math.min(endIndexExclusive, array.length);
        final int newSize = endIndexExclusive - startIndexInclusive;
        if (newSize <= 0) {
            return EMPTY_BOOLEAN_ARRAY;
        }
        return arraycopy(array, startIndexInclusive, 0, newSize, boolean[]::new);
    }

    /**
     * Produces a new {@code byte} array containing the elements
     * between the start and end indices.
     * <p>
     * The start index is inclusive, the end index exclusive.
     * Null array input produces null output.
     * </p>
     *
     * @param array  the array
     * @param startIndexInclusive  the starting index. Undervalue (&lt;0)
     *      is promoted to 0, overvalue (&gt;array.length) results
     *      in an empty array.
     * @param endIndexExclusive  elements up to endIndex-1 are present in the
     *      returned subarray. Undervalue (&lt; startIndex) produces
     *      empty array, overvalue (&gt;array.length) is demoted to
     *      array length.
     * @return a new array containing the elements between
     *      the start and end indices.
     * @since 2.1
     * @see Arrays#copyOfRange(byte[], int, int)
     */
    public static byte[] subarray(final byte[] array, int startIndexInclusive, int endIndexExclusive) {
        if (array == null) {
            return null;
        }
        startIndexInclusive = max0(startIndexInclusive);
        endIndexExclusive = Math.min(endIndexExclusive, array.length);
        final int newSize = endIndexExclusive - startIndexInclusive;
        if (newSize <= 0) {
            return EMPTY_BYTE_ARRAY;
        }
        return arraycopy(array, startIndexInclusive, 0, newSize, byte[]::new);
    }

    /**
     * Produces a new {@code char} array containing the elements
     * between the start and end indices.
     * <p>
     * The start index is inclusive, the end index exclusive.
     * Null array input produces null output.
     * </p>
     *
     * @param array  the array
     * @param startIndexInclusive  the starting index. Undervalue (&lt;0)
     *      is promoted to 0, overvalue (&gt;array.length) results
     *      in an empty array.
     * @param endIndexExclusive  elements up to endIndex-1 are present in the
     *      returned subarray. Undervalue (&lt; startIndex) produces
     *      empty array, overvalue (&gt;array.length) is demoted to
     *      array length.
     * @return a new array containing the elements between
     *      the start and end indices.
     * @since 2.1
     * @see Arrays#copyOfRange(char[], int, int)
     */
    public static char[] subarray(final char[] array, int startIndexInclusive, int endIndexExclusive) {
        if (array == null) {
            return null;
        }
        startIndexInclusive = max0(startIndexInclusive);
        endIndexExclusive = Math.min(endIndexExclusive, array.length);
        final int newSize = endIndexExclusive - startIndexInclusive;
        if (newSize <= 0) {
            return EMPTY_CHAR_ARRAY;
        }
        return arraycopy(array, startIndexInclusive, 0, newSize, char[]::new);
    }

    /**
     * Produces a new {@code double} array containing the elements
     * between the start and end indices.
     * <p>
     * The start index is inclusive, the end index exclusive.
     * Null array input produces null output.
     * </p>
     *
     * @param array  the array
     * @param startIndexInclusive  the starting index. Undervalue (&lt;0)
     *      is promoted to 0, overvalue (&gt;array.length) results
     *      in an empty array.
     * @param endIndexExclusive  elements up to endIndex-1 are present in the
     *      returned subarray. Undervalue (&lt; startIndex) produces
     *      empty array, overvalue (&gt;array.length) is demoted to
     *      array length.
     * @return a new array containing the elements between
     *      the start and end indices.
     * @since 2.1
     * @see Arrays#copyOfRange(double[], int, int)
     */
    public static double[] subarray(final double[] array, int startIndexInclusive, int endIndexExclusive) {
        if (array == null) {
            return null;
        }
        startIndexInclusive = max0(startIndexInclusive);
        endIndexExclusive = Math.min(endIndexExclusive, array.length);
        final int newSize = endIndexExclusive - startIndexInclusive;
        if (newSize <= 0) {
            return EMPTY_DOUBLE_ARRAY;
        }
        return arraycopy(array, startIndexInclusive, 0, newSize, double[]::new);
    }

    /**
     * Produces a new {@code float} array containing the elements
     * between the start and end indices.
     * <p>
     * The start index is inclusive, the end index exclusive.
     * Null array input produces null output.
     * </p>
     *
     * @param array  the array
     * @param startIndexInclusive  the starting index. Undervalue (&lt;0)
     *      is promoted to 0, overvalue (&gt;array.length) results
     *      in an empty array.
     * @param endIndexExclusive  elements up to endIndex-1 are present in the
     *      returned subarray. Undervalue (&lt; startIndex) produces
     *      empty array, overvalue (&gt;array.length) is demoted to
     *      array length.
     * @return a new array containing the elements between
     *      the start and end indices.
     * @since 2.1
     * @see Arrays#copyOfRange(float[], int, int)
     */
    public static float[] subarray(final float[] array, int startIndexInclusive, int endIndexExclusive) {
        if (array == null) {
            return null;
        }
        startIndexInclusive = max0(startIndexInclusive);
        endIndexExclusive = Math.min(endIndexExclusive, array.length);
        final int newSize = endIndexExclusive - startIndexInclusive;
        if (newSize <= 0) {
            return EMPTY_FLOAT_ARRAY;
        }
        return arraycopy(array, startIndexInclusive, 0, newSize, float[]::new);
    }

    /**
     * Produces a new {@code int} array containing the elements
     * between the start and end indices.
     * <p>
     * The start index is inclusive, the end index exclusive.
     * Null array input produces null output.
     * </p>
     *
     * @param array  the array
     * @param startIndexInclusive  the starting index. Undervalue (&lt;0)
     *      is promoted to 0, overvalue (&gt;array.length) results
     *      in an empty array.
     * @param endIndexExclusive  elements up to endIndex-1 are present in the
     *      returned subarray. Undervalue (&lt; startIndex) produces
     *      empty array, overvalue (&gt;array.length) is demoted to
     *      array length.
     * @return a new array containing the elements between
     *      the start and end indices.
     * @since 2.1
     * @see Arrays#copyOfRange(int[], int, int)
     */
    public static int[] subarray(final int[] array, int startIndexInclusive, int endIndexExclusive) {
        if (array == null) {
            return null;
        }
        startIndexInclusive = max0(startIndexInclusive);
        endIndexExclusive = Math.min(endIndexExclusive, array.length);
        final int newSize = endIndexExclusive - startIndexInclusive;
        if (newSize <= 0) {
            return EMPTY_INT_ARRAY;
        }
        return arraycopy(array, startIndexInclusive, 0, newSize, int[]::new);
    }

    /**
     * Produces a new {@code long} array containing the elements
     * between the start and end indices.
     * <p>
     * The start index is inclusive, the end index exclusive.
     * Null array input produces null output.
     * </p>
     *
     * @param array  the array
     * @param startIndexInclusive  the starting index. Undervalue (&lt;0)
     *      is promoted to 0, overvalue (&gt;array.length) results
     *      in an empty array.
     * @param endIndexExclusive  elements up to endIndex-1 are present in the
     *      returned subarray. Undervalue (&lt; startIndex) produces
     *      empty array, overvalue (&gt;array.length) is demoted to
     *      array length.
     * @return a new array containing the elements between
     *      the start and end indices.
     * @since 2.1
     * @see Arrays#copyOfRange(long[], int, int)
     */
    public static long[] subarray(final long[] array, int startIndexInclusive, int endIndexExclusive) {
        if (array == null) {
            return null;
        }
        startIndexInclusive = max0(startIndexInclusive);
        endIndexExclusive = Math.min(endIndexExclusive, array.length);
        final int newSize = endIndexExclusive - startIndexInclusive;
        if (newSize <= 0) {
            return EMPTY_LONG_ARRAY;
        }
        return arraycopy(array, startIndexInclusive, 0, newSize, long[]::new);
    }

    /**
     * Produces a new {@code short} array containing the elements
     * between the start and end indices.
     * <p>
     * The start index is inclusive, the end index exclusive.
     * Null array input produces null output.
     * </p>
     *
     * @param array  the array
     * @param startIndexInclusive  the starting index. Undervalue (&lt;0)
     *      is promoted to 0, overvalue (&gt;array.length) results
     *      in an empty array.
     * @param endIndexExclusive  elements up to endIndex-1 are present in the
     *      returned subarray. Undervalue (&lt; startIndex) produces
     *      empty array, overvalue (&gt;array.length) is demoted to
     *      array length.
     * @return a new array containing the elements between
     *      the start and end indices.
     * @since 2.1
     * @see Arrays#copyOfRange(short[], int, int)
     */
    public static short[] subarray(final short[] array, int startIndexInclusive, int endIndexExclusive) {
        if (array == null) {
            return null;
        }
        startIndexInclusive = max0(startIndexInclusive);
        endIndexExclusive = Math.min(endIndexExclusive, array.length);
        final int newSize = endIndexExclusive - startIndexInclusive;
        if (newSize <= 0) {
            return EMPTY_SHORT_ARRAY;
        }
        return arraycopy(array, startIndexInclusive, 0, newSize, short[]::new);
    }

    /**
     * Produces a new array containing the elements between
     * the start and end indices.
     * <p>
     * The start index is inclusive, the end index exclusive.
     * Null array input produces null output.
     * </p>
     * <p>
     * The component type of the subarray is always the same as
     * that of the input array. Thus, if the input is an array of type
     * {@link Date}, the following usage is envisaged:
     * </p>
     * <pre>
     * Date[] someDates = (Date[]) ArrayUtils.subarray(allDates, 2, 5);
     * </pre>
     *
     * @param <T> the component type of the array
     * @param array  the array
     * @param startIndexInclusive  the starting index. Undervalue (&lt;0)
     *      is promoted to 0, overvalue (&gt;array.length) results
     *      in an empty array.
     * @param endIndexExclusive  elements up to endIndex-1 are present in the
     *      returned subarray. Undervalue (&lt; startIndex) produces
     *      empty array, overvalue (&gt;array.length) is demoted to
     *      array length.
     * @return a new array containing the elements between
     *      the start and end indices.
     * @since 2.1
     * @see Arrays#copyOfRange(Object[], int, int)
     */
    public static <T> T[] subarray(final T[] array, int startIndexInclusive, int endIndexExclusive) {
        if (array == null) {
            return null;
        }
        startIndexInclusive = max0(startIndexInclusive);
        endIndexExclusive = Math.min(endIndexExclusive, array.length);
        final int newSize = endIndexExclusive - startIndexInclusive;
        final Class<T> type = getComponentType(array);
        if (newSize <= 0) {
            return newInstance(type, 0);
        }
        return arraycopy(array, startIndexInclusive, 0, newSize, () -> newInstance(type, newSize));
    }

    /**
     * Swaps two elements in the given boolean array.
     *
     * <p>There is no special handling for multi-dimensional arrays. This method
     * does nothing for a {@code null} or empty input array or for overflow indices.
     * Negative indices are promoted to 0(zero).</p>
     *
     * Examples:
     * <ul>
     *     <li>ArrayUtils.swap([1, 2, 3], 0, 2) -&gt; [3, 2, 1]</li>
     *     <li>ArrayUtils.swap([1, 2, 3], 0, 0) -&gt; [1, 2, 3]</li>
     *     <li>ArrayUtils.swap([1, 2, 3], 1, 0) -&gt; [2, 1, 3]</li>
     *     <li>ArrayUtils.swap([1, 2, 3], 0, 5) -&gt; [1, 2, 3]</li>
     *     <li>ArrayUtils.swap([1, 2, 3], -1, 1) -&gt; [2, 1, 3]</li>
     * </ul>
     *
     * @param array  the array to swap, may be {@code null}.
     * @param offset1 the index of the first element to swap
     * @param offset2 the index of the second element to swap
     * @since 3.5
     */
    public static void swap(final boolean[] array, final int offset1, final int offset2) {
        swap(array, offset1, offset2, 1);
    }

    /**
     * Swaps a series of elements in the given boolean array.
     *
     * <p>This method does nothing for a {@code null} or empty input array or
     * for overflow indices. Negative indices are promoted to 0(zero). If any
     * of the sub-arrays to swap falls outside of the given array, then the
     * swap is stopped at the end of the array and as many as possible elements
     * are swapped.</p>
     *
     * Examples:
     * <ul>
     *     <li>ArrayUtils.swap([true, false, true, false], 0, 2, 1) -&gt; [true, false, true, false]</li>
     *     <li>ArrayUtils.swap([true, false, true, false], 0, 0, 1) -&gt; [true, false, true, false]</li>
     *     <li>ArrayUtils.swap([true, false, true, false], 0, 2, 2) -&gt; [true, false, true, false]</li>
     *     <li>ArrayUtils.swap([true, false, true, false], -3, 2, 2) -&gt; [true, false, true, false]</li>
     *     <li>ArrayUtils.swap([true, false, true, false], 0, 3, 3) -&gt; [false, false, true, true]</li>
     * </ul>
     *
     * @param array the array to swap, may be {@code null}.
     * @param offset1 the index of the first element in the series to swap
     * @param offset2 the index of the second element in the series to swap
     * @param len the number of elements to swap starting with the given indices
     * @since 3.5
     */
    public static void swap(final boolean[] array, int offset1, int offset2, int len) {
        if (isEmpty(array) || offset1 >= array.length || offset2 >= array.length) {
            return;
        }
        offset1 = max0(offset1);
        offset2 = max0(offset2);
        len = Math.min(Math.min(len, array.length - offset1), array.length - offset2);
        for (int i = 0; i < len; i++, offset1++, offset2++) {
            final boolean aux = array[offset1];
            array[offset1] = array[offset2];
            array[offset2] = aux;
        }
    }

    /**
     * Swaps two elements in the given byte array.
     *
     * <p>There is no special handling for multi-dimensional arrays. This method
     * does nothing for a {@code null} or empty input array or for overflow indices.
     * Negative indices are promoted to 0(zero).</p>
     *
     * Examples:
     * <ul>
     *     <li>ArrayUtils.swap([1, 2, 3], 0, 2) -&gt; [3, 2, 1]</li>
     *     <li>ArrayUtils.swap([1, 2, 3], 0, 0) -&gt; [1, 2, 3]</li>
     *     <li>ArrayUtils.swap([1, 2, 3], 1, 0) -&gt; [2, 1, 3]</li>
     *     <li>ArrayUtils.swap([1, 2, 3], 0, 5) -&gt; [1, 2, 3]</li>
     *     <li>ArrayUtils.swap([1, 2, 3], -1, 1) -&gt; [2, 1, 3]</li>
     * </ul>
     *
     * @param array  the array to swap, may be {@code null}.
     * @param offset1 the index of the first element to swap
     * @param offset2 the index of the second element to swap
     * @since 3.5
     */
    public static void swap(final byte[] array, final int offset1, final int offset2) {
        swap(array, offset1, offset2, 1);
    }

    /**
     * Swaps a series of elements in the given byte array.
     *
     * <p>This method does nothing for a {@code null} or empty input array or
     * for overflow indices. Negative indices are promoted to 0(zero). If any
     * of the sub-arrays to swap falls outside of the given array, then the
     * swap is stopped at the end of the array and as many as possible elements
     * are swapped.</p>
     *
     * Examples:
     * <ul>
     *     <li>ArrayUtils.swap([1, 2, 3, 4], 0, 2, 1) -&gt; [3, 2, 1, 4]</li>
     *     <li>ArrayUtils.swap([1, 2, 3, 4], 0, 0, 1) -&gt; [1, 2, 3, 4]</li>
     *     <li>ArrayUtils.swap([1, 2, 3, 4], 2, 0, 2) -&gt; [3, 4, 1, 2]</li>
     *     <li>ArrayUtils.swap([1, 2, 3, 4], -3, 2, 2) -&gt; [3, 4, 1, 2]</li>
     *     <li>ArrayUtils.swap([1, 2, 3, 4], 0, 3, 3) -&gt; [4, 2, 3, 1]</li>
     * </ul>
     *
     * @param array the array to swap, may be {@code null}.
     * @param offset1 the index of the first element in the series to swap
     * @param offset2 the index of the second element in the series to swap
     * @param len the number of elements to swap starting with the given indices
     * @since 3.5
     */
    public static void swap(final byte[] array, int offset1, int offset2, int len) {
        if (isEmpty(array) || offset1 >= array.length || offset2 >= array.length) {
            return;
        }
        offset1 = max0(offset1);
        offset2 = max0(offset2);
        len = Math.min(Math.min(len, array.length - offset1), array.length - offset2);
        for (int i = 0; i < len; i++, offset1++, offset2++) {
            final byte aux = array[offset1];
            array[offset1] = array[offset2];
            array[offset2] = aux;
        }
    }

    /**
     * Swaps two elements in the given char array.
     *
     * <p>There is no special handling for multi-dimensional arrays. This method
     * does nothing for a {@code null} or empty input array or for overflow indices.
     * Negative indices are promoted to 0(zero).</p>
     *
     * Examples:
     * <ul>
     *     <li>ArrayUtils.swap([1, 2, 3], 0, 2) -&gt; [3, 2, 1]</li>
     *     <li>ArrayUtils.swap([1, 2, 3], 0, 0) -&gt; [1, 2, 3]</li>
     *     <li>ArrayUtils.swap([1, 2, 3], 1, 0) -&gt; [2, 1, 3]</li>
     *     <li>ArrayUtils.swap([1, 2, 3], 0, 5) -&gt; [1, 2, 3]</li>
     *     <li>ArrayUtils.swap([1, 2, 3], -1, 1) -&gt; [2, 1, 3]</li>
     * </ul>
     *
     * @param array  the array to swap, may be {@code null}.
     * @param offset1 the index of the first element to swap
     * @param offset2 the index of the second element to swap
     * @since 3.5
     */
    public static void swap(final char[] array, final int offset1, final int offset2) {
        swap(array, offset1, offset2, 1);
    }

    /**
     * Swaps a series of elements in the given char array.
     *
     * <p>This method does nothing for a {@code null} or empty input array or
     * for overflow indices. Negative indices are promoted to 0(zero). If any
     * of the sub-arrays to swap falls outside of the given array, then the
     * swap is stopped at the end of the array and as many as possible elements
     * are swapped.</p>
     *
     * Examples:
     * <ul>
     *     <li>ArrayUtils.swap([1, 2, 3, 4], 0, 2, 1) -&gt; [3, 2, 1, 4]</li>
     *     <li>ArrayUtils.swap([1, 2, 3, 4], 0, 0, 1) -&gt; [1, 2, 3, 4]</li>
     *     <li>ArrayUtils.swap([1, 2, 3, 4], 2, 0, 2) -&gt; [3, 4, 1, 2]</li>
     *     <li>ArrayUtils.swap([1, 2, 3, 4], -3, 2, 2) -&gt; [3, 4, 1, 2]</li>
     *     <li>ArrayUtils.swap([1, 2, 3, 4], 0, 3, 3) -&gt; [4, 2, 3, 1]</li>
     * </ul>
     *
     * @param array the array to swap, may be {@code null}.
     * @param offset1 the index of the first element in the series to swap
     * @param offset2 the index of the second element in the series to swap
     * @param len the number of elements to swap starting with the given indices
     * @since 3.5
     */
    public static void swap(final char[] array, int offset1, int offset2, int len) {
        if (isEmpty(array) || offset1 >= array.length || offset2 >= array.length) {
            return;
        }
        offset1 = max0(offset1);
        offset2 = max0(offset2);
        len = Math.min(Math.min(len, array.length - offset1), array.length - offset2);
        for (int i = 0; i < len; i++, offset1++, offset2++) {
            final char aux = array[offset1];
            array[offset1] = array[offset2];
            array[offset2] = aux;
        }
    }

    /**
     * Swaps two elements in the given double array.
     *
     * <p>There is no special handling for multi-dimensional arrays. This method
     * does nothing for a {@code null} or empty input array or for overflow indices.
     * Negative indices are promoted to 0(zero).</p>
     *
     * Examples:
     * <ul>
     *     <li>ArrayUtils.swap([1, 2, 3], 0, 2) -&gt; [3, 2, 1]</li>
     *     <li>ArrayUtils.swap([1, 2, 3], 0, 0) -&gt; [1, 2, 3]</li>
     *     <li>ArrayUtils.swap([1, 2, 3], 1, 0) -&gt; [2, 1, 3]</li>
     *     <li>ArrayUtils.swap([1, 2, 3], 0, 5) -&gt; [1, 2, 3]</li>
     *     <li>ArrayUtils.swap([1, 2, 3], -1, 1) -&gt; [2, 1, 3]</li>
     * </ul>
     *
     * @param array  the array to swap, may be {@code null}.
     * @param offset1 the index of the first element to swap
     * @param offset2 the index of the second element to swap
     * @since 3.5
     */
    public static void swap(final double[] array, final int offset1, final int offset2) {
        swap(array, offset1, offset2, 1);
    }

    /**
     * Swaps a series of elements in the given double array.
     *
     * <p>This method does nothing for a {@code null} or empty input array or
     * for overflow indices. Negative indices are promoted to 0(zero). If any
     * of the sub-arrays to swap falls outside of the given array, then the
     * swap is stopped at the end of the array and as many as possible elements
     * are swapped.</p>
     *
     * Examples:
     * <ul>
     *     <li>ArrayUtils.swap([1, 2, 3, 4], 0, 2, 1) -&gt; [3, 2, 1, 4]</li>
     *     <li>ArrayUtils.swap([1, 2, 3, 4], 0, 0, 1) -&gt; [1, 2, 3, 4]</li>
     *     <li>ArrayUtils.swap([1, 2, 3, 4], 2, 0, 2) -&gt; [3, 4, 1, 2]</li>
     *     <li>ArrayUtils.swap([1, 2, 3, 4], -3, 2, 2) -&gt; [3, 4, 1, 2]</li>
     *     <li>ArrayUtils.swap([1, 2, 3, 4], 0, 3, 3) -&gt; [4, 2, 3, 1]</li>
     * </ul>
     *
     * @param array the array to swap, may be {@code null}.
     * @param offset1 the index of the first element in the series to swap
     * @param offset2 the index of the second element in the series to swap
     * @param len the number of elements to swap starting with the given indices
     * @since 3.5
     */
    public static void swap(final double[] array,  int offset1, int offset2, int len) {
        if (isEmpty(array) || offset1 >= array.length || offset2 >= array.length) {
            return;
        }
        offset1 = max0(offset1);
        offset2 = max0(offset2);
        len = Math.min(Math.min(len, array.length - offset1), array.length - offset2);
        for (int i = 0; i < len; i++, offset1++, offset2++) {
            final double aux = array[offset1];
            array[offset1] = array[offset2];
            array[offset2] = aux;
        }
    }

    /**
     * Swaps two elements in the given float array.
     *
     * <p>There is no special handling for multi-dimensional arrays. This method
     * does nothing for a {@code null} or empty input array or for overflow indices.
     * Negative indices are promoted to 0(zero).</p>
     *
     * Examples:
     * <ul>
     *     <li>ArrayUtils.swap([1, 2, 3], 0, 2) -&gt; [3, 2, 1]</li>
     *     <li>ArrayUtils.swap([1, 2, 3], 0, 0) -&gt; [1, 2, 3]</li>
     *     <li>ArrayUtils.swap([1, 2, 3], 1, 0) -&gt; [2, 1, 3]</li>
     *     <li>ArrayUtils.swap([1, 2, 3], 0, 5) -&gt; [1, 2, 3]</li>
     *     <li>ArrayUtils.swap([1, 2, 3], -1, 1) -&gt; [2, 1, 3]</li>
     * </ul>
     *
     * @param array  the array to swap, may be {@code null}.
     * @param offset1 the index of the first element to swap
     * @param offset2 the index of the second element to swap
     * @since 3.5
     */
    public static void swap(final float[] array, final int offset1, final int offset2) {
        swap(array, offset1, offset2, 1);
    }

    /**
     * Swaps a series of elements in the given float array.
     *
     * <p>This method does nothing for a {@code null} or empty input array or
     * for overflow indices. Negative indices are promoted to 0(zero). If any
     * of the sub-arrays to swap falls outside of the given array, then the
     * swap is stopped at the end of the array and as many as possible elements
     * are swapped.</p>
     *
     * Examples:
     * <ul>
     *     <li>ArrayUtils.swap([1, 2, 3, 4], 0, 2, 1) -&gt; [3, 2, 1, 4]</li>
     *     <li>ArrayUtils.swap([1, 2, 3, 4], 0, 0, 1) -&gt; [1, 2, 3, 4]</li>
     *     <li>ArrayUtils.swap([1, 2, 3, 4], 2, 0, 2) -&gt; [3, 4, 1, 2]</li>
     *     <li>ArrayUtils.swap([1, 2, 3, 4], -3, 2, 2) -&gt; [3, 4, 1, 2]</li>
     *     <li>ArrayUtils.swap([1, 2, 3, 4], 0, 3, 3) -&gt; [4, 2, 3, 1]</li>
     * </ul>
     *
     * @param array the array to swap, may be {@code null}.
     * @param offset1 the index of the first element in the series to swap
     * @param offset2 the index of the second element in the series to swap
     * @param len the number of elements to swap starting with the given indices
     * @since 3.5
     */
    public static void swap(final float[] array, int offset1, int offset2, int len) {
        if (isEmpty(array) || offset1 >= array.length || offset2 >= array.length) {
            return;
        }
        offset1 = max0(offset1);
        offset2 = max0(offset2);
        len = Math.min(Math.min(len, array.length - offset1), array.length - offset2);
        for (int i = 0; i < len; i++, offset1++, offset2++) {
            final float aux = array[offset1];
            array[offset1] = array[offset2];
            array[offset2] = aux;
        }

    }

    /**
     * Swaps two elements in the given int array.
     *
     * <p>There is no special handling for multi-dimensional arrays. This method
     * does nothing for a {@code null} or empty input array or for overflow indices.
     * Negative indices are promoted to 0(zero).</p>
     *
     * Examples:
     * <ul>
     *     <li>ArrayUtils.swap([1, 2, 3], 0, 2) -&gt; [3, 2, 1]</li>
     *     <li>ArrayUtils.swap([1, 2, 3], 0, 0) -&gt; [1, 2, 3]</li>
     *     <li>ArrayUtils.swap([1, 2, 3], 1, 0) -&gt; [2, 1, 3]</li>
     *     <li>ArrayUtils.swap([1, 2, 3], 0, 5) -&gt; [1, 2, 3]</li>
     *     <li>ArrayUtils.swap([1, 2, 3], -1, 1) -&gt; [2, 1, 3]</li>
     * </ul>
     *
     * @param array  the array to swap, may be {@code null}
     * @param offset1 the index of the first element to swap
     * @param offset2 the index of the second element to swap
     * @since 3.5
     */
    public static void swap(final int[] array, final int offset1, final int offset2) {
        swap(array, offset1, offset2, 1);
    }

    /**
     * Swaps a series of elements in the given int array.
     *
     * <p>This method does nothing for a {@code null} or empty input array or
     * for overflow indices. Negative indices are promoted to 0(zero). If any
     * of the sub-arrays to swap falls outside of the given array, then the
     * swap is stopped at the end of the array and as many as possible elements
     * are swapped.</p>
     *
     * Examples:
     * <ul>
     *     <li>ArrayUtils.swap([1, 2, 3, 4], 0, 2, 1) -&gt; [3, 2, 1, 4]</li>
     *     <li>ArrayUtils.swap([1, 2, 3, 4], 0, 0, 1) -&gt; [1, 2, 3, 4]</li>
     *     <li>ArrayUtils.swap([1, 2, 3, 4], 2, 0, 2) -&gt; [3, 4, 1, 2]</li>
     *     <li>ArrayUtils.swap([1, 2, 3, 4], -3, 2, 2) -&gt; [3, 4, 1, 2]</li>
     *     <li>ArrayUtils.swap([1, 2, 3, 4], 0, 3, 3) -&gt; [4, 2, 3, 1]</li>
     * </ul>
     *
     * @param array the array to swap, may be {@code null}
     * @param offset1 the index of the first element in the series to swap
     * @param offset2 the index of the second element in the series to swap
     * @param len the number of elements to swap starting with the given indices
     * @since 3.5
     */
    public static void swap(final int[] array,  int offset1, int offset2, int len) {
        if (isEmpty(array) || offset1 >= array.length || offset2 >= array.length) {
            return;
        }
        offset1 = max0(offset1);
        offset2 = max0(offset2);
        len = Math.min(Math.min(len, array.length - offset1), array.length - offset2);
        for (int i = 0; i < len; i++, offset1++, offset2++) {
            final int aux = array[offset1];
            array[offset1] = array[offset2];
            array[offset2] = aux;
        }
    }

    /**
     * Swaps two elements in the given long array.
     *
     * <p>There is no special handling for multi-dimensional arrays. This method
     * does nothing for a {@code null} or empty input array or for overflow indices.
     * Negative indices are promoted to 0(zero).</p>
     *
     * Examples:
     * <ul>
     *     <li>ArrayUtils.swap([true, false, true], 0, 2) -&gt; [true, false, true]</li>
     *     <li>ArrayUtils.swap([true, false, true], 0, 0) -&gt; [true, false, true]</li>
     *     <li>ArrayUtils.swap([true, false, true], 1, 0) -&gt; [false, true, true]</li>
     *     <li>ArrayUtils.swap([true, false, true], 0, 5) -&gt; [true, false, true]</li>
     *     <li>ArrayUtils.swap([true, false, true], -1, 1) -&gt; [false, true, true]</li>
     * </ul>
     *
     * @param array  the array to swap, may be {@code null}
     * @param offset1 the index of the first element to swap
     * @param offset2 the index of the second element to swap
     * @since 3.5
     */
    public static void swap(final long[] array, final int offset1, final int offset2) {
        swap(array, offset1, offset2, 1);
    }

    /**
     * Swaps a series of elements in the given long array.
     *
     * <p>This method does nothing for a {@code null} or empty input array or
     * for overflow indices. Negative indices are promoted to 0(zero). If any
     * of the sub-arrays to swap falls outside of the given array, then the
     * swap is stopped at the end of the array and as many as possible elements
     * are swapped.</p>
     *
     * Examples:
     * <ul>
     *     <li>ArrayUtils.swap([1, 2, 3, 4], 0, 2, 1) -&gt; [3, 2, 1, 4]</li>
     *     <li>ArrayUtils.swap([1, 2, 3, 4], 0, 0, 1) -&gt; [1, 2, 3, 4]</li>
     *     <li>ArrayUtils.swap([1, 2, 3, 4], 2, 0, 2) -&gt; [3, 4, 1, 2]</li>
     *     <li>ArrayUtils.swap([1, 2, 3, 4], -3, 2, 2) -&gt; [3, 4, 1, 2]</li>
     *     <li>ArrayUtils.swap([1, 2, 3, 4], 0, 3, 3) -&gt; [4, 2, 3, 1]</li>
     * </ul>
     *
     * @param array the array to swap, may be {@code null}
     * @param offset1 the index of the first element in the series to swap
     * @param offset2 the index of the second element in the series to swap
     * @param len the number of elements to swap starting with the given indices
     * @since 3.5
     */
    public static void swap(final long[] array,  int offset1, int offset2, int len) {
        if (isEmpty(array) || offset1 >= array.length || offset2 >= array.length) {
            return;
        }
        offset1 = max0(offset1);
        offset2 = max0(offset2);
        len = Math.min(Math.min(len, array.length - offset1), array.length - offset2);
        for (int i = 0; i < len; i++, offset1++, offset2++) {
            final long aux = array[offset1];
            array[offset1] = array[offset2];
            array[offset2] = aux;
        }
    }

    /**
     * Swaps two elements in the given array.
     *
     * <p>There is no special handling for multi-dimensional arrays. This method
     * does nothing for a {@code null} or empty input array or for overflow indices.
     * Negative indices are promoted to 0(zero).</p>
     *
     * Examples:
     * <ul>
     *     <li>ArrayUtils.swap(["1", "2", "3"], 0, 2) -&gt; ["3", "2", "1"]</li>
     *     <li>ArrayUtils.swap(["1", "2", "3"], 0, 0) -&gt; ["1", "2", "3"]</li>
     *     <li>ArrayUtils.swap(["1", "2", "3"], 1, 0) -&gt; ["2", "1", "3"]</li>
     *     <li>ArrayUtils.swap(["1", "2", "3"], 0, 5) -&gt; ["1", "2", "3"]</li>
     *     <li>ArrayUtils.swap(["1", "2", "3"], -1, 1) -&gt; ["2", "1", "3"]</li>
     * </ul>
     *
     * @param array the array to swap, may be {@code null}
     * @param offset1 the index of the first element to swap
     * @param offset2 the index of the second element to swap
     * @since 3.5
     */
    public static void swap(final Object[] array, final int offset1, final int offset2) {
        swap(array, offset1, offset2, 1);
    }

    /**
     * Swaps a series of elements in the given array.
     *
     * <p>This method does nothing for a {@code null} or empty input array or
     * for overflow indices. Negative indices are promoted to 0(zero). If any
     * of the sub-arrays to swap falls outside of the given array, then the
     * swap is stopped at the end of the array and as many as possible elements
     * are swapped.</p>
     *
     * Examples:
     * <ul>
     *     <li>ArrayUtils.swap(["1", "2", "3", "4"], 0, 2, 1) -&gt; ["3", "2", "1", "4"]</li>
     *     <li>ArrayUtils.swap(["1", "2", "3", "4"], 0, 0, 1) -&gt; ["1", "2", "3", "4"]</li>
     *     <li>ArrayUtils.swap(["1", "2", "3", "4"], 2, 0, 2) -&gt; ["3", "4", "1", "2"]</li>
     *     <li>ArrayUtils.swap(["1", "2", "3", "4"], -3, 2, 2) -&gt; ["3", "4", "1", "2"]</li>
     *     <li>ArrayUtils.swap(["1", "2", "3", "4"], 0, 3, 3) -&gt; ["4", "2", "3", "1"]</li>
     * </ul>
     *
     * @param array the array to swap, may be {@code null}
     * @param offset1 the index of the first element in the series to swap
     * @param offset2 the index of the second element in the series to swap
     * @param len the number of elements to swap starting with the given indices
     * @since 3.5
     */
    public static void swap(final Object[] array,  int offset1, int offset2, int len) {
        if (isEmpty(array) || offset1 >= array.length || offset2 >= array.length) {
            return;
        }
        offset1 = max0(offset1);
        offset2 = max0(offset2);
        len = Math.min(Math.min(len, array.length - offset1), array.length - offset2);
        for (int i = 0; i < len; i++, offset1++, offset2++) {
            final Object aux = array[offset1];
            array[offset1] = array[offset2];
            array[offset2] = aux;
        }
    }

    /**
     * Swaps two elements in the given short array.
     *
     * <p>There is no special handling for multi-dimensional arrays. This method
     * does nothing for a {@code null} or empty input array or for overflow indices.
     * Negative indices are promoted to 0(zero).</p>
     *
     * Examples:
     * <ul>
     *     <li>ArrayUtils.swap([1, 2, 3], 0, 2) -&gt; [3, 2, 1]</li>
     *     <li>ArrayUtils.swap([1, 2, 3], 0, 0) -&gt; [1, 2, 3]</li>
     *     <li>ArrayUtils.swap([1, 2, 3], 1, 0) -&gt; [2, 1, 3]</li>
     *     <li>ArrayUtils.swap([1, 2, 3], 0, 5) -&gt; [1, 2, 3]</li>
     *     <li>ArrayUtils.swap([1, 2, 3], -1, 1) -&gt; [2, 1, 3]</li>
     * </ul>
     *
     * @param array  the array to swap, may be {@code null}
     * @param offset1 the index of the first element to swap
     * @param offset2 the index of the second element to swap
     * @since 3.5
     */
    public static void swap(final short[] array, final int offset1, final int offset2) {
        swap(array, offset1, offset2, 1);
    }

    /**
     * Swaps a series of elements in the given short array.
     *
     * <p>This method does nothing for a {@code null} or empty input array or
     * for overflow indices. Negative indices are promoted to 0(zero). If any
     * of the sub-arrays to swap falls outside of the given array, then the
     * swap is stopped at the end of the array and as many as possible elements
     * are swapped.</p>
     *
     * Examples:
     * <ul>
     *     <li>ArrayUtils.swap([1, 2, 3, 4], 0, 2, 1) -&gt; [3, 2, 1, 4]</li>
     *     <li>ArrayUtils.swap([1, 2, 3, 4], 0, 0, 1) -&gt; [1, 2, 3, 4]</li>
     *     <li>ArrayUtils.swap([1, 2, 3, 4], 2, 0, 2) -&gt; [3, 4, 1, 2]</li>
     *     <li>ArrayUtils.swap([1, 2, 3, 4], -3, 2, 2) -&gt; [3, 4, 1, 2]</li>
     *     <li>ArrayUtils.swap([1, 2, 3, 4], 0, 3, 3) -&gt; [4, 2, 3, 1]</li>
     * </ul>
     *
     * @param array the array to swap, may be {@code null}
     * @param offset1 the index of the first element in the series to swap
     * @param offset2 the index of the second element in the series to swap
     * @param len the number of elements to swap starting with the given indices
     * @since 3.5
     */
    public static void swap(final short[] array, int offset1, int offset2, int len) {
        if (isEmpty(array) || offset1 >= array.length || offset2 >= array.length) {
            return;
        }
        offset1 = max0(offset1);
        offset2 = max0(offset2);
        if (offset1 == offset2) {
            return;
        }
        len = Math.min(Math.min(len, array.length - offset1), array.length - offset2);
        for (int i = 0; i < len; i++, offset1++, offset2++) {
            final short aux = array[offset1];
            array[offset1] = array[offset2];
            array[offset2] = aux;
        }
    }

    /**
     * Create a type-safe generic array.
     * <p>
     * The Java language does not allow an array to be created from a generic type:
     * </p>
     * <pre>
    public static &lt;T&gt; T[] createAnArray(int size) {
        return new T[size]; // compiler error here
    }
    public static &lt;T&gt; T[] createAnArray(int size) {
        return (T[]) new Object[size]; // ClassCastException at runtime
    }
     * </pre>
     * <p>
     * Therefore new arrays of generic types can be created with this method.
     * For example, an array of Strings can be created:
     * </p>
     * <pre>{@code
     * String[] array = ArrayUtils.toArray("1", "2");
     * String[] emptyArray = ArrayUtils.<String>toArray();
     * }</pre>
     * <p>
     * The method is typically used in scenarios, where the caller itself uses generic types
     * that have to be combined into an array.
     * </p>
     * <p>
     * Note, this method makes only sense to provide arguments of the same type so that the
     * compiler can deduce the type of the array itself. While it is possible to select the
     * type explicitly like in
     * {@code Number[] array = ArrayUtils.<Number>toArray(Integer.valueOf(42), Double.valueOf(Math.PI))},
     * there is no real advantage when compared to
     * {@code new Number[] {Integer.valueOf(42), Double.valueOf(Math.PI)}}.
     * </p>
     *
     * @param  <T>   the array's element type
     * @param  items  the varargs array items, null allowed
     * @return the array, not null unless a null array is passed in
     * @since 3.0
     */
    public static <T> T[] toArray(@SuppressWarnings("unchecked") final T... items) {
        return items;
    }

    /**
     * Converts the given array into a {@link java.util.Map}. Each element of the array
     * must be either a {@link java.util.Map.Entry} or an Array, containing at least two
     * elements, where the first element is used as key and the second as
     * value.
     * <p>
     * This method can be used to initialize:
     * </p>
     * <pre>
     * // Create a Map mapping colors.
     * Map colorMap = ArrayUtils.toMap(new String[][] {
     *     {"RED", "#FF0000"},
     *     {"GREEN", "#00FF00"},
     *     {"BLUE", "#0000FF"}});
     * </pre>
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param array  an array whose elements are either a {@link java.util.Map.Entry} or
     *  an Array containing at least two elements, may be {@code null}
     * @return a {@link Map} that was created from the array
     * @throws IllegalArgumentException  if one element of this Array is
     *  itself an Array containing less than two elements
     * @throws IllegalArgumentException  if the array contains elements other
     *  than {@link java.util.Map.Entry} and an Array
     */
    public static Map<Object, Object> toMap(final Object[] array) {
        if (array == null) {
            return null;
        }
        final Map<Object, Object> map = new HashMap<>((int) (array.length * 1.5));
        for (int i = 0; i < array.length; i++) {
            final Object object = array[i];
            if (object instanceof Map.Entry<?, ?>) {
                final Map.Entry<?, ?> entry = (Map.Entry<?, ?>) object;
                map.put(entry.getKey(), entry.getValue());
            } else if (object instanceof Object[]) {
                final Object[] entry = (Object[]) object;
                if (entry.length < 2) {
                    throw new IllegalArgumentException("Array element " + i + ", '"
                        + object
                        + "', has a length less than 2");
                }
                map.put(entry[0], entry[1]);
            } else {
                throw new IllegalArgumentException("Array element " + i + ", '"
                        + object
                        + "', is neither of type Map.Entry nor an Array");
            }
        }
        return map;
    }

    /**
     * Converts an array of primitive booleans to objects.
     *
     * <p>This method returns {@code null} for a {@code null} input array.</p>
     *
     * @param array  a {@code boolean} array
     * @return a {@link Boolean} array, {@code null} if null array input
     */
    public static Boolean[] toObject(final boolean[] array) {
        if (array == null) {
            return null;
        }
        if (array.length == 0) {
            return EMPTY_BOOLEAN_OBJECT_ARRAY;
        }
        return setAll(new Boolean[array.length], i -> array[i] ? Boolean.TRUE : Boolean.FALSE);
    }

    /**
     * Converts an array of primitive bytes to objects.
     *
     * <p>This method returns {@code null} for a {@code null} input array.</p>
     *
     * @param array  a {@code byte} array
     * @return a {@link Byte} array, {@code null} if null array input
     */
    public static Byte[] toObject(final byte[] array) {
        if (array == null) {
            return null;
        }
        if (array.length == 0) {
            return EMPTY_BYTE_OBJECT_ARRAY;
        }
        return setAll(new Byte[array.length], i -> Byte.valueOf(array[i]));
    }

    /**
     * Converts an array of primitive chars to objects.
     *
     * <p>This method returns {@code null} for a {@code null} input array.</p>
     *
     * @param array a {@code char} array
     * @return a {@link Character} array, {@code null} if null array input
     */
    public static Character[] toObject(final char[] array) {
        if (array == null) {
            return null;
        }
        if (array.length == 0) {
            return EMPTY_CHARACTER_OBJECT_ARRAY;
        }
        return setAll(new Character[array.length], i -> Character.valueOf(array[i]));
     }

    /**
     * Converts an array of primitive doubles to objects.
     *
     * <p>This method returns {@code null} for a {@code null} input array.</p>
     *
     * @param array  a {@code double} array
     * @return a {@link Double} array, {@code null} if null array input
     */
    public static Double[] toObject(final double[] array) {
        if (array == null) {
            return null;
        }
        if (array.length == 0) {
            return EMPTY_DOUBLE_OBJECT_ARRAY;
        }
        return setAll(new Double[array.length], i -> Double.valueOf(array[i]));
    }

    /**
     * Converts an array of primitive floats to objects.
     *
     * <p>This method returns {@code null} for a {@code null} input array.</p>
     *
     * @param array  a {@code float} array
     * @return a {@link Float} array, {@code null} if null array input
     */
    public static Float[] toObject(final float[] array) {
        if (array == null) {
            return null;
        }
        if (array.length == 0) {
            return EMPTY_FLOAT_OBJECT_ARRAY;
        }
        return setAll(new Float[array.length], i -> Float.valueOf(array[i]));
    }

    /**
     * Converts an array of primitive ints to objects.
     *
     * <p>This method returns {@code null} for a {@code null} input array.</p>
     *
     * @param array  an {@code int} array
     * @return an {@link Integer} array, {@code null} if null array input
     */
    public static Integer[] toObject(final int[] array) {
        if (array == null) {
            return null;
        }
        if (array.length == 0) {
            return EMPTY_INTEGER_OBJECT_ARRAY;
        }
        return setAll(new Integer[array.length], i -> Integer.valueOf(array[i]));
    }

    /**
     * Converts an array of primitive longs to objects.
     *
     * <p>This method returns {@code null} for a {@code null} input array.</p>
     *
     * @param array  a {@code long} array
     * @return a {@link Long} array, {@code null} if null array input
     */
    public static Long[] toObject(final long[] array) {
        if (array == null) {
            return null;
        }
        if (array.length == 0) {
            return EMPTY_LONG_OBJECT_ARRAY;
        }
        return setAll(new Long[array.length], i -> Long.valueOf(array[i]));
    }

    /**
     * Converts an array of primitive shorts to objects.
     *
     * <p>This method returns {@code null} for a {@code null} input array.</p>
     *
     * @param array  a {@code short} array
     * @return a {@link Short} array, {@code null} if null array input
     */
    public static Short[] toObject(final short[] array) {
        if (array == null) {
            return null;
        }
        if (array.length == 0) {
            return EMPTY_SHORT_OBJECT_ARRAY;
        }
        return setAll(new Short[array.length], i -> Short.valueOf(array[i]));
    }

    /**
     * Converts an array of object Booleans to primitives.
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     * <p>
     * Null array elements map to false, like {@code Boolean.parseBoolean(null)} and its callers return false.
     * </p>
     *
     * @param array a {@link Boolean} array, may be {@code null}
     * @return a {@code boolean} array, {@code null} if null array input
     */
    public static boolean[] toPrimitive(final Boolean[] array) {
        return toPrimitive(array, false);
    }

    /**
     * Converts an array of object Booleans to primitives handling {@code null}.
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param array  a {@link Boolean} array, may be {@code null}
     * @param valueForNull  the value to insert if {@code null} found
     * @return a {@code boolean} array, {@code null} if null array input
     */
    public static boolean[] toPrimitive(final Boolean[] array, final boolean valueForNull) {
        if (array == null) {
            return null;
        }
        if (array.length == 0) {
            return EMPTY_BOOLEAN_ARRAY;
        }
        final boolean[] result = new boolean[array.length];
        for (int i = 0; i < array.length; i++) {
            final Boolean b = array[i];
            result[i] = b == null ? valueForNull : b.booleanValue();
        }
        return result;
    }

    /**
     * Converts an array of object Bytes to primitives.
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param array  a {@link Byte} array, may be {@code null}
     * @return a {@code byte} array, {@code null} if null array input
     * @throws NullPointerException if an array element is {@code null}
     */
    public static byte[] toPrimitive(final Byte[] array) {
        if (array == null) {
            return null;
        }
        if (array.length == 0) {
            return EMPTY_BYTE_ARRAY;
        }
        final byte[] result = new byte[array.length];
        for (int i = 0; i < array.length; i++) {
            result[i] = array[i].byteValue();
        }
        return result;
    }

    /**
     * Converts an array of object Bytes to primitives handling {@code null}.
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param array  a {@link Byte} array, may be {@code null}
     * @param valueForNull  the value to insert if {@code null} found
     * @return a {@code byte} array, {@code null} if null array input
     */
    public static byte[] toPrimitive(final Byte[] array, final byte valueForNull) {
        if (array == null) {
            return null;
        }
        if (array.length == 0) {
            return EMPTY_BYTE_ARRAY;
        }
        final byte[] result = new byte[array.length];
        for (int i = 0; i < array.length; i++) {
            final Byte b = array[i];
            result[i] = b == null ? valueForNull : b.byteValue();
        }
        return result;
    }

    /**
     * Converts an array of object Characters to primitives.
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param array  a {@link Character} array, may be {@code null}
     * @return a {@code char} array, {@code null} if null array input
     * @throws NullPointerException if an array element is {@code null}
     */
    public static char[] toPrimitive(final Character[] array) {
        if (array == null) {
            return null;
        }
        if (array.length == 0) {
            return EMPTY_CHAR_ARRAY;
        }
        final char[] result = new char[array.length];
        for (int i = 0; i < array.length; i++) {
            result[i] = array[i].charValue();
        }
        return result;
    }

    /**
     * Converts an array of object Character to primitives handling {@code null}.
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param array  a {@link Character} array, may be {@code null}
     * @param valueForNull  the value to insert if {@code null} found
     * @return a {@code char} array, {@code null} if null array input
     */
    public static char[] toPrimitive(final Character[] array, final char valueForNull) {
        if (array == null) {
            return null;
        }
        if (array.length == 0) {
            return EMPTY_CHAR_ARRAY;
        }
        final char[] result = new char[array.length];
        for (int i = 0; i < array.length; i++) {
            final Character b = array[i];
            result[i] = b == null ? valueForNull : b.charValue();
        }
        return result;
    }

    /**
     * Converts an array of object Doubles to primitives.
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param array  a {@link Double} array, may be {@code null}
     * @return a {@code double} array, {@code null} if null array input
     * @throws NullPointerException if an array element is {@code null}
     */
    public static double[] toPrimitive(final Double[] array) {
        if (array == null) {
            return null;
        }
        if (array.length == 0) {
            return EMPTY_DOUBLE_ARRAY;
        }
        final double[] result = new double[array.length];
        for (int i = 0; i < array.length; i++) {
            result[i] = array[i].doubleValue();
        }
        return result;
    }

    /**
     * Converts an array of object Doubles to primitives handling {@code null}.
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param array  a {@link Double} array, may be {@code null}
     * @param valueForNull  the value to insert if {@code null} found
     * @return a {@code double} array, {@code null} if null array input
     */
    public static double[] toPrimitive(final Double[] array, final double valueForNull) {
        if (array == null) {
            return null;
        }
        if (array.length == 0) {
            return EMPTY_DOUBLE_ARRAY;
        }
        final double[] result = new double[array.length];
        for (int i = 0; i < array.length; i++) {
            final Double b = array[i];
            result[i] = b == null ? valueForNull : b.doubleValue();
        }
        return result;
    }

    /**
     * Converts an array of object Floats to primitives.
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param array  a {@link Float} array, may be {@code null}
     * @return a {@code float} array, {@code null} if null array input
     * @throws NullPointerException if an array element is {@code null}
     */
    public static float[] toPrimitive(final Float[] array) {
        if (array == null) {
            return null;
        }
        if (array.length == 0) {
            return EMPTY_FLOAT_ARRAY;
        }
        final float[] result = new float[array.length];
        for (int i = 0; i < array.length; i++) {
            result[i] = array[i].floatValue();
        }
        return result;
    }

    /**
     * Converts an array of object Floats to primitives handling {@code null}.
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param array  a {@link Float} array, may be {@code null}
     * @param valueForNull  the value to insert if {@code null} found
     * @return a {@code float} array, {@code null} if null array input
     */
    public static float[] toPrimitive(final Float[] array, final float valueForNull) {
        if (array == null) {
            return null;
        }
        if (array.length == 0) {
            return EMPTY_FLOAT_ARRAY;
        }
        final float[] result = new float[array.length];
        for (int i = 0; i < array.length; i++) {
            final Float b = array[i];
            result[i] = b == null ? valueForNull : b.floatValue();
        }
        return result;
    }

    /**
     * Converts an array of object Integers to primitives.
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param array  a {@link Integer} array, may be {@code null}
     * @return an {@code int} array, {@code null} if null array input
     * @throws NullPointerException if an array element is {@code null}
     */
    public static int[] toPrimitive(final Integer[] array) {
        if (array == null) {
            return null;
        }
        if (array.length == 0) {
            return EMPTY_INT_ARRAY;
        }
        final int[] result = new int[array.length];
        for (int i = 0; i < array.length; i++) {
            result[i] = array[i].intValue();
        }
        return result;
    }

    /**
     * Converts an array of object Integer to primitives handling {@code null}.
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param array  a {@link Integer} array, may be {@code null}
     * @param valueForNull  the value to insert if {@code null} found
     * @return an {@code int} array, {@code null} if null array input
     */
    public static int[] toPrimitive(final Integer[] array, final int valueForNull) {
        if (array == null) {
            return null;
        }
        if (array.length == 0) {
            return EMPTY_INT_ARRAY;
        }
        final int[] result = new int[array.length];
        for (int i = 0; i < array.length; i++) {
            final Integer b = array[i];
            result[i] = b == null ? valueForNull : b.intValue();
        }
        return result;
    }

    /**
     * Converts an array of object Longs to primitives.
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param array  a {@link Long} array, may be {@code null}
     * @return a {@code long} array, {@code null} if null array input
     * @throws NullPointerException if an array element is {@code null}
     */
    public static long[] toPrimitive(final Long[] array) {
        if (array == null) {
            return null;
        }
        if (array.length == 0) {
            return EMPTY_LONG_ARRAY;
        }
        final long[] result = new long[array.length];
        for (int i = 0; i < array.length; i++) {
            result[i] = array[i].longValue();
        }
        return result;
    }

    /**
     * Converts an array of object Long to primitives handling {@code null}.
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param array  a {@link Long} array, may be {@code null}
     * @param valueForNull  the value to insert if {@code null} found
     * @return a {@code long} array, {@code null} if null array input
     */
    public static long[] toPrimitive(final Long[] array, final long valueForNull) {
        if (array == null) {
            return null;
        }
        if (array.length == 0) {
            return EMPTY_LONG_ARRAY;
        }
        final long[] result = new long[array.length];
        for (int i = 0; i < array.length; i++) {
            final Long b = array[i];
            result[i] = b == null ? valueForNull : b.longValue();
        }
        return result;
    }

    /**
     * Create an array of primitive type from an array of wrapper types.
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param array  an array of wrapper object
     * @return an array of the corresponding primitive type, or the original array
     * @since 3.5
     */
    public static Object toPrimitive(final Object array) {
        if (array == null) {
            return null;
        }
        final Class<?> ct = array.getClass().getComponentType();
        final Class<?> pt = ClassUtils.wrapperToPrimitive(ct);
        if (Boolean.TYPE.equals(pt)) {
            return toPrimitive((Boolean[]) array);
        }
        if (Character.TYPE.equals(pt)) {
            return toPrimitive((Character[]) array);
        }
        if (Byte.TYPE.equals(pt)) {
            return toPrimitive((Byte[]) array);
        }
        if (Integer.TYPE.equals(pt)) {
            return toPrimitive((Integer[]) array);
        }
        if (Long.TYPE.equals(pt)) {
            return toPrimitive((Long[]) array);
        }
        if (Short.TYPE.equals(pt)) {
            return toPrimitive((Short[]) array);
        }
        if (Double.TYPE.equals(pt)) {
            return toPrimitive((Double[]) array);
        }
        if (Float.TYPE.equals(pt)) {
            return toPrimitive((Float[]) array);
        }
        return array;
    }

    /**
     * Converts an array of object Shorts to primitives.
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param array  a {@link Short} array, may be {@code null}
     * @return a {@code byte} array, {@code null} if null array input
     * @throws NullPointerException if an array element is {@code null}
     */
    public static short[] toPrimitive(final Short[] array) {
        if (array == null) {
            return null;
        }
        if (array.length == 0) {
            return EMPTY_SHORT_ARRAY;
        }
        final short[] result = new short[array.length];
        for (int i = 0; i < array.length; i++) {
            result[i] = array[i].shortValue();
        }
        return result;
    }

    /**
     * Converts an array of object Short to primitives handling {@code null}.
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param array  a {@link Short} array, may be {@code null}
     * @param valueForNull  the value to insert if {@code null} found
     * @return a {@code byte} array, {@code null} if null array input
     */
    public static short[] toPrimitive(final Short[] array, final short valueForNull) {
        if (array == null) {
            return null;
        }
        if (array.length == 0) {
            return EMPTY_SHORT_ARRAY;
        }
        final short[] result = new short[array.length];
        for (int i = 0; i < array.length; i++) {
            final Short b = array[i];
            result[i] = b == null ? valueForNull : b.shortValue();
        }
        return result;
    }

    /**
     * Outputs an array as a String, treating {@code null} as an empty array.
     * <p>
     * Multi-dimensional arrays are handled correctly, including
     * multi-dimensional primitive arrays.
     * </p>
     * <p>
     * The format is that of Java source code, for example {@code {a,b}}.
     * </p>
     *
     * @param array  the array to get a toString for, may be {@code null}
     * @return a String representation of the array, '{}' if null array input
     */
    public static String toString(final Object array) {
        return toString(array, "{}");
    }

    /**
     * Outputs an array as a String handling {@code null}s.
     * <p>
     * Multi-dimensional arrays are handled correctly, including
     * multi-dimensional primitive arrays.
     * </p>
     * <p>
     * The format is that of Java source code, for example {@code {a,b}}.
     * </p>
     *
     * @param array  the array to get a toString for, may be {@code null}
     * @param stringIfNull  the String to return if the array is {@code null}
     * @return a String representation of the array
     */
    public static String toString(final Object array, final String stringIfNull) {
        if (array == null) {
            return stringIfNull;
        }
        return new ToStringBuilder(array, ToStringStyle.SIMPLE_STYLE).append(array).toString();
    }

    /**
     * Returns an array containing the string representation of each element in the argument array.
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param array the {@code Object[]} to be processed, may be {@code null}.
     * @return {@code String[]} of the same size as the source with its element's string representation,
     * {@code null} if null array input
     * @since 3.6
     */
    public static String[] toStringArray(final Object[] array) {
        return toStringArray(array, "null");
    }

    /**
     * Returns an array containing the string representation of each element in the argument
     * array handling {@code null} elements.
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param array the Object[] to be processed, may be {@code null}.
     * @param valueForNullElements the value to insert if {@code null} is found
     * @return a {@link String} array, {@code null} if null array input
     * @since 3.6
     */
    public static String[] toStringArray(final Object[] array, final String valueForNullElements) {
        if (null == array) {
            return null;
        }
        if (array.length == 0) {
            return EMPTY_STRING_ARRAY;
        }
        return map(array, String.class, e -> Objects.toString(e, valueForNullElements));
    }

    /**
     * ArrayUtils instances should NOT be constructed in standard programming.
     * Instead, the class should be used as {@code ArrayUtils.clone(new int[] {2})}.
     * <p>
     * This constructor is public to permit tools that require a JavaBean instance
     * to operate.
     * </p>
     *
     * @deprecated TODO Make private in 4.0.
     */
    @Deprecated
    public ArrayUtils() {
        // empty
    }
}
