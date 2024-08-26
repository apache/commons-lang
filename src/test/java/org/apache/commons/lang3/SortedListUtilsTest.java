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

import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests {@link SortedListUtils}.
 */
public class SortedListUtilsTest extends AbstractLangTest {

    @Test
    public void binarySearch_whenEmpty_returnM1() {
        List<Data> list = createList();
        int found = SortedListUtils.binarySearch(list, 0, Data::getValue, Integer::compare);
        assertEquals(-1, found);
    }

    @Test
    public void binarySearch_whenExists_returnIndex() {
        List<Data> list = createList(0, 1, 2, 4, 7, 9, 12, 15, 17, 19, 25);
        int found = SortedListUtils.binarySearch(list, 9, Data::getValue, Integer::compare);
        assertEquals(5, found);
    }

    @Test
    public void binarySearch_whenNotExists_returnMinusInsertion() {
        List<Data> list = createList(0, 1, 2, 4, 7, 9, 12, 15, 17, 19, 25);
        int found = SortedListUtils.binarySearch(list, 8, Data::getValue, Integer::compare);
        assertEquals(-6, found);
    }

    @Test
    public void binarySearch_whenNotExistsBeginning_returnMinus1() {
        List<Data> list = createList(0, 1, 2, 4, 7, 9, 12, 15, 17, 19, 25);
        int found = SortedListUtils.binarySearch(list, -3, Data::getValue, Integer::compare);
        assertEquals(-1, found);
    }

    @Test
    public void binarySearch_whenNotExistsEnd_returnMinusLength() {
        List<Data> list = createList(0, 1, 2, 4, 7, 9, 12, 15, 17, 19, 25);
        int found = SortedListUtils.binarySearch(list, 29, Data::getValue, Integer::compare);
        assertEquals(-(list.size() + 1), found);
    }

    private List<Data> createList(int... values) {
        return IntStream.of(values).mapToObj(Data::new)
                .collect(Collectors.toList());
    }

    public class Data
    {
        private final int value;

        public Data(int value) {
            this.value = value;
        }

        public int getValue() {
            return value;
        }
    }
}
