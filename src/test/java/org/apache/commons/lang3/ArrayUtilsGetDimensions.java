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

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class ArrayUtilsGetDimensions {

    // Tests cyclic references
    static class Node {

        String name;

        Node next;

        Node(final String name) {
            this.name = name;
        }
    }

    private static Stream<Arguments> provideArraysWithExpectedDimensions() {
        return Stream.of(Arguments.of(new int[] { 1, 2, 3 }, 1), Arguments.of(new int[][] { { 1, 2 }, { 3, 4 } }, 2),
                Arguments.of(new int[][][] { { { 1 } } }, 3), Arguments.of(new String[] { "a", "b" }, 1), Arguments.of(new String[][] { { "a" }, { "b" } }, 2),
                Arguments.of(new Object[5], 1), Arguments.of(new boolean[2][3], 2), Arguments.of(new char[1][2][3][4], 4));
    }

    @Test
    void testArraysOfArrays() {
        final Object[] arrayOfArrays = { new int[] { 1, 2 }, new String[] { "a", "b" } };
        assertEquals(1, ArrayUtils.getDimensions(arrayOfArrays));
    }

    @Test
    void testArraysWithCyclicObjects() {
        final Node node1 = new Node("A");
        final Node node2 = new Node("B");
        node1.next = node2;
        node2.next = node1; // Create cycle
        final Node[] arr = { node1, node2 };
        assertEquals(1, ArrayUtils.getDimensions(arr));
        final Node[][] arr2D = { { node1, node2 }, { node2, node1 } };
        assertEquals(2, ArrayUtils.getDimensions(arr2D));
    }

    @Test
    void testArraysWithNullElements() {
        final String[] arrWithNull = { null, "test", null };
        assertEquals(1, ArrayUtils.getDimensions(arrWithNull));
        final String[][] arr2DWithNull = { { null, "a" }, null, { "b" } };
        assertEquals(2, ArrayUtils.getDimensions(arr2DWithNull));
    }

    @Test
    void testEmptyOneDimensionalArrays() {
        assertEquals(1, ArrayUtils.getDimensions(new int[0]));
        assertEquals(1, ArrayUtils.getDimensions(new String[0]));
    }

    @Test
    void testHighDimensionalArrays() {
        assertEquals(4, ArrayUtils.getDimensions(new int[1][2][3][4]));
        assertEquals(5, ArrayUtils.getDimensions(new byte[1][1][1][1][1]));
        assertEquals(6, ArrayUtils.getDimensions(new String[2][2][2][2][2][2]));
    }

    @Test
    void testInitializedArrays() {
        final int[] arr1D = { 1, 2, 3, 4, 5 };
        assertEquals(1, ArrayUtils.getDimensions(arr1D));
        final int[][] arr2D = { { 1, 2, 3 }, { 4, 5, 6 } };
        assertEquals(2, ArrayUtils.getDimensions(arr2D));
        final int[][][] arr3D = { { { 1, 2 }, { 3, 4 } }, { { 5, 6 }, { 7, 8 } } };
        assertEquals(3, ArrayUtils.getDimensions(arr3D));
    }

    @Test
    void testJaggedArrays() {
        final int[][] jagged = { { 1, 2 }, { 3, 4, 5 }, { 6 } };
        assertEquals(2, ArrayUtils.getDimensions(jagged));
        final String[][] jaggedStrings = { { "a" }, { "b", "c", "d" } };
        assertEquals(2, ArrayUtils.getDimensions(jaggedStrings));
    }

    @Test
    void testNonArrayObject() {
        assertEquals(0, ArrayUtils.getDimensions("Not an array"));
        assertEquals(0, ArrayUtils.getDimensions(42));
        assertEquals(0, ArrayUtils.getDimensions(new Object()));
        assertEquals(0, ArrayUtils.getDimensions(3.14));
    }

    @Test
    void testNullArray() {
        assertEquals(0, ArrayUtils.getDimensions(null));
    }

    @Test
    void testOneDimensionalObjectArrays() {
        assertEquals(1, ArrayUtils.getDimensions(new String[5]));
        assertEquals(1, ArrayUtils.getDimensions(new Object[10]));
        assertEquals(1, ArrayUtils.getDimensions(new Integer[3]));
    }

    @Test
    void testOneDimensionalPrimitiveArrays() {
        assertEquals(1, ArrayUtils.getDimensions(new int[5]));
        assertEquals(1, ArrayUtils.getDimensions(new byte[10]));
        assertEquals(1, ArrayUtils.getDimensions(new short[3]));
        assertEquals(1, ArrayUtils.getDimensions(new long[7]));
        assertEquals(1, ArrayUtils.getDimensions(new float[4]));
        assertEquals(1, ArrayUtils.getDimensions(new double[6]));
        assertEquals(1, ArrayUtils.getDimensions(new boolean[2]));
        assertEquals(1, ArrayUtils.getDimensions(new char[8]));
    }

    @Test
    void testThreeDimensionalArrays() {
        assertEquals(3, ArrayUtils.getDimensions(new int[2][3][4]));
        assertEquals(3, ArrayUtils.getDimensions(new String[1][2][3]));
        assertEquals(3, ArrayUtils.getDimensions(new Object[5][5][5]));
    }

    @Test
    void testTwoDimensionalArrays() {
        assertEquals(2, ArrayUtils.getDimensions(new int[3][4]));
        assertEquals(2, ArrayUtils.getDimensions(new String[2][5]));
        assertEquals(2, ArrayUtils.getDimensions(new double[1][1]));
    }

    @ParameterizedTest
    @MethodSource("provideArraysWithExpectedDimensions")
    void testVariousArrayTypes(final Object array, final int expectedDimensions) {
        assertEquals(expectedDimensions, ArrayUtils.getDimensions(array));
    }
}
