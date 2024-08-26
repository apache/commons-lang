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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrowsExactly;

import java.util.stream.IntStream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;

/**
 * Unit tests {@link ArrayUtils} binarySearch functions.
 */
public class ArrayUtilsBinarySearchTest extends AbstractLangTest {

    @Test
    public void binarySearch_whenLowHigherThanEnd_throw() {
        final Data[] list = createList(0, 1);
        assertThrowsExactly(IllegalArgumentException.class, () -> ArrayUtils.binarySearch(list, 1, 0, 0, Data::getValue, Integer::compare));
    }

    @Test
    public void binarySearch_whenLowNegative_throw() {
        final Data[] list = createList(0, 1);
        assertThrowsExactly(ArrayIndexOutOfBoundsException.class, () -> ArrayUtils.binarySearch(list, -1, 0, 0, Data::getValue, Integer::compare));
    }

    @Test
    public void binarySearch_whenEndBeyondLength_throw() {
        final Data[] list = createList(0, 1);
        assertThrowsExactly(ArrayIndexOutOfBoundsException.class, () -> ArrayUtils.binarySearch(list, 0, 3, 0, Data::getValue, Integer::compare));
    }

    @Test
    public void binarySearch_whenEmpty_returnM1() {
        final Data[] list = createList();
        final int found = ArrayUtils.binarySearch(list, 0, Data::getValue, Integer::compare);
        assertEquals(-1, found);
    }

    @Test
    public void binarySearch_whenExists_returnIndex() {
        final Data[] list = createList(0, 1, 2, 4, 7, 9, 12, 15, 17, 19, 25);
        final int found = ArrayUtils.binarySearch(list, 9, Data::getValue, Integer::compare);
        assertEquals(5, found);
    }

    @Test
    public void binarySearch_whenNotExistsMiddle_returnMinusInsertion() {
        final Data[] list = createList(0, 1, 2, 4, 7, 9, 12, 15, 17, 19, 25);
        final int found = ArrayUtils.binarySearch(list, 8, Data::getValue, Integer::compare);
        assertEquals(-6, found);
    }

    @Test
    public void binarySearch_whenNotExistsBeginning_returnMinus1() {
        final Data[] list = createList(0, 1, 2, 4, 7, 9, 12, 15, 17, 19, 25);
        final int found = ArrayUtils.binarySearch(list, -3, Data::getValue, Integer::compare);
        assertEquals(-1, found);
    }

    @Test
    public void binarySearch_whenNotExistsEnd_returnMinusLength() {
        final Data[] list = createList(0, 1, 2, 4, 7, 9, 12, 15, 17, 19, 25);
        final int found = ArrayUtils.binarySearch(list, 29, Data::getValue, Integer::compare);
        assertEquals(-(list.length + 1), found);
    }

    @Test
    @Timeout(10)
    public void binarySearch_whenUnsorted_dontInfiniteLoop() {
        final Data[] list = createList(7, 1, 4, 9, 11, 8);
        final int found = ArrayUtils.binarySearch(list, 10, Data::getValue, Integer::compare);
    }

    private Data[] createList(int... values) {
        return IntStream.of(values).mapToObj(Data::new)
                .toArray(Data[]::new);
    }

    static class Data {

        private final int value;

        Data(int value) {
            this.value = value;
        }

        public int getValue() {
            return value;
        }
    }
}
