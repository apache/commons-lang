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
import java.util.OptionalLong;
import java.util.stream.Collectors;
import java.util.stream.LongStream;

import static org.apache.commons.lang3.stream.TestConstants.EXPECTED_EXCEPTION;
import static org.apache.commons.lang3.stream.TestConstants.EXPECTED_NFE_MESSAGE_LONG;
import static org.apache.commons.lang3.stream.TestConstants.FLAT_MAP_INPUT_DOUBLE;
import static org.apache.commons.lang3.stream.TestConstants.FLAT_MAP_INPUT_INT;
import static org.apache.commons.lang3.stream.TestConstants.FLAT_MAP_INPUT_LONG;
import static org.apache.commons.lang3.stream.TestConstants.INPUT_DOUBLE;
import static org.apache.commons.lang3.stream.TestConstants.INPUT_INT;
import static org.apache.commons.lang3.stream.TestConstants.INPUT_LONG;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

public class FailableLongStreamTest {
    private static final Functions.FailableFunction<List<String>, LongStream, ?> LIST_TO_LONG_STREAM =
            list -> list.stream().mapToLong(Long::valueOf);

    private final List<String> failingInput = Arrays.asList("1000000000", "2000000000", "3000000000",
            "4000000000 ", "5000000000", "6000000000");
    private final List<List<String>> failingFlatMapInput = Arrays.asList(failingInput,
            Arrays.asList("7000000000", "8000000000"));

    private FailableLongStream inputStream;
    private FailableLongStream failingInputStream;
    private FailableLongStream flatMapInputStream;
    private FailableLongStream failingFlatMapInputStream;

    @BeforeEach
    void beforeEach() {
        inputStream = Functions.failableLongStream(INPUT_LONG, Long::valueOf);
        failingInputStream = Functions.failableLongStream(failingInput, Long::valueOf);
        flatMapInputStream = Functions.failableStream(FLAT_MAP_INPUT_LONG)
                .flatMapToLong(LIST_TO_LONG_STREAM);
        failingFlatMapInputStream = Functions.failableStream(failingFlatMapInput)
                .flatMapToLong(LIST_TO_LONG_STREAM);
    }

    @Test
    void testLongStreamFromMapCount() {
        assertEquals(6L, inputStream.count());
    }

    @Test
    void testLongStreamFromFlatMapCount() {
        assertEquals(8L, flatMapInputStream.count());
    }

    @Test
    void testLongStreamFromMapSum() {
        assertEquals(2.1E10, inputStream.sum());
    }

    @Test
    void testLongStreamFromMapSumFailing() {
        try {
            failingInputStream.sum();
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_LONG, nfe.getMessage());
        }
    }

    @Test
    void testLongStreamFromFlatMapSum() {
        assertEquals(3.6E10, flatMapInputStream.sum());
    }

    @Test
    void testLongStreamFromFlatMapSumFailing() {
        try {
            failingFlatMapInputStream.sum();
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_LONG, nfe.getMessage());
        }
    }

    @Test
    void testLongStreamFromMapAverage() {
        final OptionalDouble average = inputStream.average();

        assertTrue(average.isPresent());
        assertEquals(3.5E9, average.getAsDouble());
    }

    @Test
    void testLongStreamFromMapAverageFailing() {
        try {
            failingInputStream.average();
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_LONG, nfe.getMessage());
        }
    }

    @Test
    void testLongStreamFromFlatMapAverage() {
        final OptionalDouble average = flatMapInputStream.average();

        assertTrue(average.isPresent());
        assertEquals(4.5E9, average.getAsDouble());
    }

    @Test
    void testLongStreamFromFlatMapAverageFailing() {
        try {
            failingFlatMapInputStream.average();
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_LONG, nfe.getMessage());
        }
    }

    @Test
    void testLongStreamFromMapBoxed() {
        assertEquals(INPUT_LONG, inputStream.boxed().map(Object::toString)
                .collect(Collectors.toList()));
    }

    @Test
    void testLongStreamFromMapBoxedFailing() {
        try {
            failingInputStream.boxed().collect(Collectors.toList());
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_LONG, nfe.getMessage());
        }
    }

    @Test
    void testLongStreamFromFlatMapBoxed() {
        final List<String> flatMap = FLAT_MAP_INPUT_LONG.stream()
                .flatMap(Collection::stream).collect(Collectors.toList());

        assertEquals(flatMap, flatMapInputStream.boxed().map(Object::toString)
                .collect(Collectors.toList()));
    }

    @Test
    void testLongStreamFromFlatMapBoxedFailing() {
        try {
            failingFlatMapInputStream.boxed().collect(Collectors.toList());
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_LONG, nfe.getMessage());
        }
    }

    @Test
    void testLongStreamFromMapMax() {
        final OptionalLong max = inputStream.max();

        assertTrue(max.isPresent());
        assertEquals(6E9, max.getAsLong());
    }

    @Test
    void testLongStreamFromMapMaxFailing() {
        try {
            failingInputStream.max();
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_LONG, nfe.getMessage());
        }
    }

    @Test
    void testLongStreamFromFlatMapMax() {
        final OptionalLong max = flatMapInputStream.max();

        assertTrue(max.isPresent());
        assertEquals(8E9, max.getAsLong());
    }

    @Test
    void testLongStreamFromFlatMapMaxFailing() {
        try {
            failingFlatMapInputStream.max();
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_LONG, nfe.getMessage());
        }
    }

    @Test
    void testLongStreamFromMapMin() {
        final OptionalLong min = inputStream.min();

        assertTrue(min.isPresent());
        assertEquals(1E9, min.getAsLong());
    }

    @Test
    void testLongStreamFromMapMinFailing() {
        try {
            failingInputStream.min();
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_LONG, nfe.getMessage());
        }
    }

    @Test
    void testLongStreamFromFlatMapMin() {
        final OptionalLong min = flatMapInputStream.min();

        assertTrue(min.isPresent());
        assertEquals(1E9, min.getAsLong());
    }

    @Test
    void testLongStreamFromFlatMapMinFailing() {
        try {
            failingFlatMapInputStream.min();
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_LONG, nfe.getMessage());
        }
    }

    @Test
    void testSequentialLongStreamFromMapFindFirst() {
        final OptionalLong first = inputStream.sequential().findFirst();

        assertTrue(first.isPresent());
        assertEquals(1E9, first.getAsLong());
    }

    @Test
    void testSequentialLongStreamFromFlatMapFindFirst() {
        final OptionalLong first = flatMapInputStream.sequential().findFirst();

        assertTrue(first.isPresent());
        assertEquals(1E9, first.getAsLong());
    }

    @Test
    void testParallelLongStreamFromMapFindFirst() {
        final OptionalLong first = inputStream.parallel().findFirst();

        assertTrue(first.isPresent());
        assertEquals(1E9, first.getAsLong());
    }

    @Test
    void testParallelLongStreamFromFlatMapFindFirst() {
        final OptionalLong first = flatMapInputStream.parallel().findFirst();

        assertTrue(first.isPresent());
        assertEquals(1E9, first.getAsLong());
    }

    @Test
    void testSequentialLongStreamFromMapFindAny() {
        final OptionalLong any = inputStream.sequential().findAny();

        assertTrue(any.isPresent());
        assertEquals(1E9, any.getAsLong());
    }

    @Test
    void testSequentialLongStreamFromFlatMapFindAny() {
        final OptionalLong any = flatMapInputStream.sequential().findAny();

        assertTrue(any.isPresent());
        assertEquals(1E9, any.getAsLong());
    }

    @Test
    void testParallelLongStreamFromMapFindAny() {
        final OptionalLong first = inputStream.parallel().findAny();

        assertTrue(first.isPresent());
        assertTrue(INPUT_LONG.contains(Long.toString(first.getAsLong())));
    }

    @Test
    void testParallelLongStreamFromFlatMapFindAny() {
        final OptionalLong any = flatMapInputStream.parallel().findAny();
        final List<String> flatMap = FLAT_MAP_INPUT_LONG.stream()
                .flatMap(Collection::stream).collect(Collectors.toList());

        assertTrue(any.isPresent());
        assertTrue(flatMap.contains(Long.toString(any.getAsLong())));
    }

    @Test
    void testLongStreamFromMapForEach() {
        final List<String> mapList = new ArrayList<>();
        inputStream.forEach(value -> mapList.add(Long.toString(value)));

        assertEquals(INPUT_LONG, mapList);
    }

    @Test
    void testLongStreamFromMapForEachFailing() {
        try {
            failingInputStream.forEach(value -> {});
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_LONG, nfe.getMessage());
        }
    }

    @Test
    void testLongStreamFromFlatMapForEach() {
        final List<String> flatMapList = new ArrayList<>();
        final List<String> flatMap = FLAT_MAP_INPUT_LONG.stream()
                .flatMap(Collection::stream).collect(Collectors.toList());

        flatMapInputStream.forEach(value -> flatMapList.add(Long.toString(value)));

        assertEquals(flatMap, flatMapList);
    }

    @Test
    void testLongStreamFromFlatMapForEachFailing() {
        try {
            failingFlatMapInputStream.forEach(value -> {});
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_LONG, nfe.getMessage());
        }
    }

    @Test
    void testLongStreamFromMap_Map() {
        final List<String> expectedResults = Arrays.asList("1000000001", "2000000001", "3000000001",
                "4000000001", "5000000001", "6000000001");
        final List<String> actualResults = inputStream.map(input -> input + 1)
                .mapToObj(Objects::toString).collect(Collectors.toList());

        assertEquals(expectedResults, actualResults);
    }

    @Test
    void testLongStreamFromMap_MapFailing() {
        try {
            failingInputStream.map(input -> input + 1).mapToObj(Objects::toString)
                    .collect(Collectors.toList());
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_LONG, nfe.getMessage());
        }
    }

    @Test
    void testLongStreamFromFlatMap_Map() {
        final List<String> expectedResults = Arrays.asList("1000000001", "2000000001", "3000000001",
                "4000000001", "5000000001", "6000000001", "7000000001", "8000000001");
        final List<String> actualResults = flatMapInputStream.map(input -> input + 1)
                .mapToObj(Objects::toString).collect(Collectors.toList());

        assertEquals(expectedResults, actualResults);
    }

    @Test
    void testLongStreamFromFlatMap_MapFailing() {
        try {
            failingFlatMapInputStream.map(input -> input + 1).mapToObj(Objects::toString)
                    .collect(Collectors.toList());
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_LONG, nfe.getMessage());
        }
    }

    @Test
    void testLongStreamFromMap_MapToObj() {
        assertEquals(INPUT_LONG, inputStream.mapToObj(Objects::toString)
                .collect(Collectors.toList()));
    }

    @Test
    void testLongStreamFromMap_MapToObjFailing() {
        try {
            failingInputStream.mapToObj(Objects::toString).collect(Collectors.toList());
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_LONG, nfe.getMessage());
        }
    }

    @Test
    void testLongStreamFromFlatMap_MapToObj() {
        final List<String> flatMap = FLAT_MAP_INPUT_LONG.stream()
                .flatMap(Collection::stream).collect(Collectors.toList());

        assertEquals(flatMap, flatMapInputStream.mapToObj(Objects::toString)
                .collect(Collectors.toList()));
    }

    @Test
    void testLongStreamFromFlatMap_MapToObjFailing() {
        try {
            failingFlatMapInputStream.mapToObj(Objects::toString).collect(Collectors.toList());
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_LONG, nfe.getMessage());
        }
    }

    @Test
    void testLongStreamFromMap_MapToDouble() {
        List<String> convertedDoubles = inputStream.mapToDouble(value -> (value / 1E9) + 0.5)
                .boxed().map(Object::toString).collect(Collectors.toList());

        assertEquals(INPUT_DOUBLE, convertedDoubles);
    }

    @Test
    void testLongStreamFromMap_MapToDoubleFailing() {
        try {
            failingInputStream.mapToDouble(value -> value).forEach(value -> {});
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_LONG, nfe.getMessage());
        }
    }

    @Test
    void testLongStreamFromFlatMap_MapToDouble() {
        List<String> convertedDoubles = flatMapInputStream.mapToDouble(value -> (value / 1E9) + 0.5)
                .boxed().map(Object::toString).collect(Collectors.toList());
        List<String> longFlatMap = FLAT_MAP_INPUT_DOUBLE.stream().flatMap(Collection::stream)
                .collect(Collectors.toList());

        assertEquals(longFlatMap, convertedDoubles);
    }

    @Test
    void testLongStreamFromFlatMap_MapToDoubleFailing() {
        try {
            failingFlatMapInputStream.mapToDouble(value -> value).forEach(value -> {});
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_LONG, nfe.getMessage());
        }
    }

    @Test
    void testLongStreamFromMap_MapToInt() {
        List<String> convertedDoubles = inputStream.mapToInt(value -> (int) (value / 1E9))
                .boxed().map(Object::toString).collect(Collectors.toList());

        assertEquals(INPUT_INT, convertedDoubles);
    }

    @Test
    void testLongStreamFromMap_MapToIntFailing() {
        try {
            failingInputStream.mapToInt(value -> (int) (value / 1E9)).forEach(value -> {});
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_LONG, nfe.getMessage());
        }
    }

    @Test
    void testLongStreamFromFlatMap_MapToInt() {
        List<String> convertedDoubles = flatMapInputStream.mapToInt(value -> (int) (value / 1E9))
                .boxed().map(Object::toString).collect(Collectors.toList());
        List<String> intFlatMap = FLAT_MAP_INPUT_INT.stream().flatMap(Collection::stream)
                .collect(Collectors.toList());

        assertEquals(intFlatMap, convertedDoubles);
    }

    @Test
    void testLongStreamFromFlatMap_MapToIntFailing() {
        try {
            failingFlatMapInputStream.mapToInt(value -> (int) (value / 1E9))
                    .forEach(value -> {});
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_LONG, nfe.getMessage());
        }
    }
}
