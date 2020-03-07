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
package org.apache.commons.lang3.stream;

import org.apache.commons.lang3.Functions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.OptionalDouble;
import java.util.OptionalInt;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.apache.commons.lang3.stream.TestStringConstants.EXPECTED_EXCEPTION;
import static org.apache.commons.lang3.stream.TestStringConstants.EXPECTED_NFE_MESSAGE_INT;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

public class FailableIntStreamTest {
    private static final Functions.FailableFunction<List<String>, IntStream, ?> LIST_TO_INT_STREAM =
            list -> list.stream().mapToInt(Integer::valueOf);

    private final List<String> input = Arrays.asList("1", "2", "3", "4", "5", "6");
    private final List<String> failingInput = Arrays.asList("1", "2", "3", "4 ", "5", "6");
    private final List<List<String>> flatMapInput = Arrays.asList(input, Arrays.asList("7", "8"));
    private final List<List<String>> failingFlatMapInput = Arrays.asList(failingInput,
            Arrays.asList("7", "8"));

    private FailableIntStream inputStream;
    private FailableIntStream failingInputStream;
    private FailableIntStream flatMapInputStream;
    private FailableIntStream failingFlatMapInputStream;

    @BeforeEach
    void beforeEach() {
        inputStream = Functions.stream(input).mapToInt(Integer::valueOf);
        failingInputStream = Functions.stream(failingInput).mapToInt(Integer::valueOf);
        flatMapInputStream = Functions.stream(flatMapInput).flatMapToInt(LIST_TO_INT_STREAM);
        failingFlatMapInputStream = Functions.stream(failingFlatMapInput).flatMapToInt(LIST_TO_INT_STREAM);
    }

    @Test
    void testIntStreamFromMapCount() {
        assertEquals(6L, inputStream.count());
    }

    @Test
    void testIntStreamFromMapCountFailing() {
        assertEquals(6L, failingInputStream.count());
    }

    @Test
    void testIntStreamFromFlatMapCount() {
        assertEquals(8L, flatMapInputStream.count());
    }

    @Test
    void testIntStreamFromFlatMapCountFailing() {
        try {
            failingFlatMapInputStream.count();
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_INT, nfe.getMessage());
        }
    }

    @Test
    void testIntStreamFromMapSum() {
        assertEquals(21, inputStream.sum());
    }

    @Test
    void testIntStreamFromMapSumFailing() {
        try {
            failingInputStream.sum();
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_INT, nfe.getMessage());
        }
    }

    @Test
    void testIntStreamFromFlatMapSum() {
        assertEquals(36, flatMapInputStream.sum());
    }

    @Test
    void testIntStreamFromFlatMapSumFailing() {
        try {
            failingFlatMapInputStream.sum();
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_INT, nfe.getMessage());
        }
    }

    @Test
    void testIntStreamFromMapAverage() {
        final OptionalDouble average = inputStream.average();

        assertTrue(average.isPresent());
        assertEquals(3.5, average.getAsDouble());
    }

    @Test
    void testIntStreamFromMapAverageFailing() {
        try {
            failingInputStream.average();
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_INT, nfe.getMessage());
        }
    }

    @Test
    void testIntStreamFromFlatMapAverage() {
        final OptionalDouble average = flatMapInputStream.average();

        assertTrue(average.isPresent());
        assertEquals(4.5, average.getAsDouble());
    }

    @Test
    void testIntStreamFromFlatMapAverageFailing() {
        try {
            failingFlatMapInputStream.average();
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_INT, nfe.getMessage());
        }
    }

    @Test
    void testIntStreamFromMapBoxed() {
        assertEquals(input, inputStream.boxed().map(Object::toString).collect(Collectors.toList()));
    }

    @Test
    void testIntStreamFromMapBoxedFailing() {
        try {
            failingInputStream.boxed().collect(Collectors.toList());
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_INT, nfe.getMessage());
        }
    }

    @Test
    void testIntStreamFromFlatMapBoxed() {
        final List<String> flatMap = flatMapInput.stream().flatMap(Collection::stream)
                .collect(Collectors.toList());

        assertEquals(flatMap, flatMapInputStream.boxed().map(Object::toString)
                .collect(Collectors.toList()));
    }

    @Test
    void testIntStreamFromFlatMapBoxedFailing() {
        try {
            failingFlatMapInputStream.boxed().collect(Collectors.toList());
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_INT, nfe.getMessage());
        }
    }

    @Test
    void testIntStreamFromMap_MapToObj() {
        assertEquals(input, inputStream.mapToObj(Objects::toString).collect(Collectors.toList()));
    }

    @Test
    void testIntStreamFromMap_MapToObjFailing() {
        try {
            failingInputStream.mapToObj(Objects::toString).collect(Collectors.toList());
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_INT, nfe.getMessage());
        }
    }

    @Test
    void testIntStreamFromFlatMap_MapToObj() {
        final List<String> flatMap = flatMapInput.stream().flatMap(Collection::stream)
                .collect(Collectors.toList());

        assertEquals(flatMap, flatMapInputStream.mapToObj(Objects::toString)
                .collect(Collectors.toList()));
    }

    @Test
    void testIntStreamFromFlatMap_MapToObjFailing() {
        try {
            failingFlatMapInputStream.mapToObj(Objects::toString).collect(Collectors.toList());
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_INT, nfe.getMessage());
        }
    }

    @Test
    void testIntStreamFromMapMax() {
        final OptionalInt max = inputStream.max();

        assertTrue(max.isPresent());
        assertEquals(6, max.getAsInt());
    }

    @Test
    void testIntStreamFromMapMaxFailing() {
        try {
            failingInputStream.max();
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_INT, nfe.getMessage());
        }
    }

    @Test
    void testIntStreamFromFlatMapMax() {
        final OptionalInt max = flatMapInputStream.max();

        assertTrue(max.isPresent());
        assertEquals(8, max.getAsInt());
    }

    @Test
    void testIntStreamFromFlatMapMaxFailing() {
        try {
            failingFlatMapInputStream.max();
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_INT, nfe.getMessage());
        }
    }

    @Test
    void testIntStreamFromMapMin() {
        final OptionalInt min = inputStream.min();

        assertTrue(min.isPresent());
        assertEquals(1, min.getAsInt());
    }

    @Test
    void testIntStreamFromMapMinFailing() {
        try {
            failingInputStream.min();
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_INT, nfe.getMessage());
        }
    }

    @Test
    void testIntStreamFromFlatMapMin() {
        final OptionalInt min = flatMapInputStream.min();

        assertTrue(min.isPresent());
        assertEquals(1, min.getAsInt());
    }

    @Test
    void testIntStreamFromFlatMapMinFailing() {
        try {
            failingFlatMapInputStream.min();
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_INT, nfe.getMessage());
        }
    }

    @Test
    void testSequentialIntStreamFromMapFindFirst() {
        final OptionalInt first = inputStream.sequential().findFirst();

        assertTrue(first.isPresent());
        assertEquals(1, first.getAsInt());
    }

    @Test
    void testSequentialIntStreamFromFlatMapFindFirst() {
        final OptionalInt first = flatMapInputStream.sequential().findFirst();

        assertTrue(first.isPresent());
        assertEquals(1, first.getAsInt());
    }

    @Test
    void testParallelIntStreamFromMapFindFirst() {
        final OptionalInt first = inputStream.parallel().findFirst();

        assertTrue(first.isPresent());
        assertEquals(1, first.getAsInt());
    }

    @Test
    void testParallelIntStreamFromFlatMapFindFirst() {
        final OptionalInt first = flatMapInputStream.parallel().findFirst();

        assertTrue(first.isPresent());
        assertEquals(1, first.getAsInt());
    }

    @Test
    void testSequentialIntStreamFromMapFindAny() {
        final OptionalInt any = inputStream.sequential().findAny();

        assertTrue(any.isPresent());
        assertEquals(1, any.getAsInt());
    }

    @Test
    void testSequentialIntStreamFromFlatMapFindAny() {
        final OptionalInt any = flatMapInputStream.sequential().findAny();

        assertTrue(any.isPresent());
        assertEquals(1, any.getAsInt());
    }

    @Test
    void testParallelIntStreamFromMapFindAny() {
        final OptionalInt first = inputStream.parallel().findAny();

        assertTrue(first.isPresent());
        assertTrue(input.contains(Integer.toString(first.getAsInt())));
    }

    @Test
    void testParallelIntStreamFromFlatMapFindAny() {
        final OptionalInt any = flatMapInputStream.parallel().findAny();
        final List<String> flatMap = flatMapInput.stream().flatMap(Collection::stream)
                .collect(Collectors.toList());

        assertTrue(any.isPresent());
        assertTrue(flatMap.contains(Integer.toString(any.getAsInt())));
    }

    @Test
    void testIntStreamFromMapForEach() {
        final List<String> mapList = new ArrayList<>();
        inputStream.forEach(value -> mapList.add(Integer.toString(value)));

        assertEquals(input, mapList);
    }

    @Test
    void testIntStreamFromMapForEachFailing() {
        try {
            failingInputStream.forEach(value -> {});
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_INT, nfe.getMessage());
        }
    }

    @Test
    void testIntStreamFromFlatMapForEach() {
        final List<String> flatMapList = new ArrayList<>();
        final List<String> flatMap = flatMapInput.stream().flatMap(Collection::stream)
                .collect(Collectors.toList());

        flatMapInputStream.forEach(value -> flatMapList.add(Integer.toString(value)));

        assertEquals(flatMap, flatMapList);
    }

    @Test
    void testIntStreamFromFlatMapForEachFailing() {
        try {
            failingFlatMapInputStream.forEach(value -> {});
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_INT, nfe.getMessage());
        }
    }
}
