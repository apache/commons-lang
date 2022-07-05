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

import static org.junit.jupiter.api.Assertions.assertArrayEquals;

import java.util.Arrays;

import org.junit.jupiter.api.Test;

public class ArraySorterTest extends AbstractLangTest {

    @Test
    public void testSortByteArray() {
        final byte[] array1 = {2, 1};
        final byte[] array2 = array1.clone();
        Arrays.sort(array1);
        assertArrayEquals(array1, ArraySorter.sort(array2));
    }

    @Test
    public void testSortCharArray() {
        final char[] array1 = {2, 1};
        final char[] array2 = array1.clone();
        Arrays.sort(array1);
        assertArrayEquals(array1, ArraySorter.sort(array2));
    }

    @Test
    public void testSortComparable() {
        final String[] array1 = ArrayUtils.toArray("foo", "bar");
        final String[] array2 = array1.clone();
        Arrays.sort(array1);
        assertArrayEquals(array1, ArraySorter.sort(array2, String::compareTo));
    }

    @Test
    public void testSortDoubleArray() {
        final double[] array1 = {2, 1};
        final double[] array2 = array1.clone();
        Arrays.sort(array1);
        assertArrayEquals(array1, ArraySorter.sort(array2));
    }

    @Test
    public void testSortFloatArray() {
        final float[] array1 = {2, 1};
        final float[] array2 = array1.clone();
        Arrays.sort(array1);
        assertArrayEquals(array1, ArraySorter.sort(array2));
    }

    @Test
    public void testSortIntArray() {
        final int[] array1 = {2, 1};
        final int[] array2 = array1.clone();
        Arrays.sort(array1);
        assertArrayEquals(array1, ArraySorter.sort(array2));
    }

    @Test
    public void testSortLongArray() {
        final long[] array1 = {2, 1};
        final long[] array2 = array1.clone();
        Arrays.sort(array1);
        assertArrayEquals(array1, ArraySorter.sort(array2));
    }

    @Test
    public void testSortObjects() {
        final String[] array1 = ArrayUtils.toArray("foo", "bar");
        final String[] array2 = array1.clone();
        Arrays.sort(array1);
        assertArrayEquals(array1, ArraySorter.sort(array2));
    }

    @Test
    public void testSortShortArray() {
        final short[] array1 = {2, 1};
        final short[] array2 = array1.clone();
        Arrays.sort(array1);
        assertArrayEquals(array1, ArraySorter.sort(array2));
    }

}
