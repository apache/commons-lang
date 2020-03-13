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
import org.junit.jupiter.api.function.Executable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.OptionalDouble;
import java.util.OptionalInt;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.apache.commons.lang3.stream.TestConstants.EXPECTED_NFE_MESSAGE_INT;
import static org.apache.commons.lang3.stream.TestConstants.FAILING_FLAT_MAP_INPUT_INT;
import static org.apache.commons.lang3.stream.TestConstants.FAILING_INPUT_INT;
import static org.apache.commons.lang3.stream.TestConstants.FLAT_MAP_INPUT_DOUBLE;
import static org.apache.commons.lang3.stream.TestConstants.FLAT_MAP_INPUT_INT;
import static org.apache.commons.lang3.stream.TestConstants.FLAT_MAP_INPUT_LONG;
import static org.apache.commons.lang3.stream.TestConstants.INPUT_DOUBLE;
import static org.apache.commons.lang3.stream.TestConstants.INPUT_INT;
import static org.apache.commons.lang3.stream.TestConstants.INPUT_LONG;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class FailableIntStreamTest {
    private static final Functions.FailableFunction<List<String>, IntStream, ?> LIST_TO_INT_STREAM =
            list -> list.stream().mapToInt(Integer::valueOf);

    private FailableIntStream inputStream;
    private FailableIntStream failingInputStream;
    private FailableIntStream flatMapInputStream;
    private FailableIntStream failingFlatMapInputStream;

    @BeforeEach
    void beforeEach() {
        inputStream = Functions.failableIntStream(INPUT_INT, Integer::valueOf);
        failingInputStream = Functions.failableIntStream(FAILING_INPUT_INT, Integer::valueOf);
        flatMapInputStream = Functions.failableStream(FLAT_MAP_INPUT_INT).flatMapToInt(LIST_TO_INT_STREAM);
        failingFlatMapInputStream = Functions.failableStream(FAILING_FLAT_MAP_INPUT_INT)
                .flatMapToInt(LIST_TO_INT_STREAM);
    }

    @Test
    void testIntStreamFromMapCount() {
        assertEquals(6L, inputStream.count());
    }

    @Test
    void testIntStreamFromFlatMapCount() {
        assertEquals(8L, flatMapInputStream.count());
    }

    @Test
    void testIntStreamFromMapDistinct() {
        final List<Integer> input = inputStream.boxed().collect(Collectors.toList());
        input.add(1);

        final List<String> actualResult = Functions.failableIntStream(input, Integer::valueOf)
                .distinct().boxed().map(Objects::toString).collect(Collectors.toList());

        assertEquals(INPUT_INT, actualResult);
    }

    @Test
    void testIntStreamFromMapDistinctFailing() {
        final List<String> input = new ArrayList<>(FAILING_INPUT_INT);
        input.add("1");

        final Executable executable = () -> Functions.failableIntStream(input, Integer::valueOf)
                .distinct().boxed().map(Objects::toString).collect(Collectors.toList());

        assertThrows(NumberFormatException.class, executable, EXPECTED_NFE_MESSAGE_INT);
    }

    @Test
    void testIntStreamFromFlatMapDistinct() {
        final List<Integer> input = flatMapInputStream.boxed().collect(Collectors.toList());
        input.add(1);

        final List<String> expectedResult = FLAT_MAP_INPUT_INT.stream()
                .flatMap(Collection::stream).collect(Collectors.toList());
        final List<String> actualResult = Functions.failableIntStream(input, Integer::valueOf)
                .distinct().boxed().map(Objects::toString).collect(Collectors.toList());

        assertEquals(expectedResult, actualResult);
    }

    @Test
    void testIntStreamFromFlatMapDistinctFailing() {
        final List<String> input = FAILING_FLAT_MAP_INPUT_INT.stream().flatMap(Collection::stream)
                .collect(Collectors.toList());
        input.add("1");

        final Executable executable = () -> Functions.failableIntStream(input, Integer::valueOf)
                .distinct().boxed().map(Objects::toString).collect(Collectors.toList());

        assertThrows(NumberFormatException.class, executable, EXPECTED_NFE_MESSAGE_INT);
    }

    @Test
    void testIntStreamFromMapLimit() {
        assertEquals(4L, inputStream.limit(4).count());
    }

    @Test
    void testIntStreamFromMapLimit_IncludeInvalidElement() {
        assertThrows(NumberFormatException.class, () -> failingInputStream.limit(4).count(),
                EXPECTED_NFE_MESSAGE_INT);
    }

    @Test
    void testIntStreamFromMapLimit_ExcludeInvalidElement() {
        assertEquals(3L, failingInputStream.limit(3).count());
    }

    @Test
    void testIntStreamFromFlatMapLimit() {
        assertEquals(4L, flatMapInputStream.limit(4).count());
    }

    @Test
    void testIntStreamFromFlatMapLimitFailing() {
        assertThrows(NumberFormatException.class, () -> failingFlatMapInputStream.limit(4).count(),
                EXPECTED_NFE_MESSAGE_INT);
    }

    @Test
    void testIntStreamFromMapSkip() {
        assertEquals(4L, inputStream.skip(2).count());
    }

    @Test
    void testIntStreamFromMapSkipFailing() {
        assertThrows(NumberFormatException.class, () -> failingInputStream.skip(2).count(),
                EXPECTED_NFE_MESSAGE_INT);
    }

    @Test
    void testIntStreamFromFlatMapSkip() {
        assertEquals(6L, flatMapInputStream.skip(2).count());
    }

    @Test
    void testIntStreamFromFlatMapSkipFailing() {
        assertThrows(NumberFormatException.class, () -> failingFlatMapInputStream.skip(2).count(),
                EXPECTED_NFE_MESSAGE_INT);
    }

    @Test
    void testIntStreamFromMapSum() {
        assertEquals(21, inputStream.sum());
    }

    @Test
    void testIntStreamFromMapSumFailing() {
        assertThrows(NumberFormatException.class, () -> failingInputStream.sum(),
                EXPECTED_NFE_MESSAGE_INT);
    }

    @Test
    void testIntStreamFromFlatMapSum() {
        assertEquals(36, flatMapInputStream.sum());
    }

    @Test
    void testIntStreamFromFlatMapSumFailing() {
        assertThrows(NumberFormatException.class, () -> failingFlatMapInputStream.sum(),
                EXPECTED_NFE_MESSAGE_INT);
    }

    @Test
    void testIntStreamFromMapAverage() {
        final OptionalDouble average = inputStream.average();

        assertTrue(average.isPresent());
        assertEquals(3.5, average.getAsDouble());
    }

    @Test
    void testIntStreamFromMapAverageFailing() {
        assertThrows(NumberFormatException.class, () -> failingInputStream.average(),
                EXPECTED_NFE_MESSAGE_INT);
    }

    @Test
    void testIntStreamFromFlatMapAverage() {
        final OptionalDouble average = flatMapInputStream.average();

        assertTrue(average.isPresent());
        assertEquals(4.5, average.getAsDouble());
    }

    @Test
    void testIntStreamFromFlatMapAverageFailing() {
        assertThrows(NumberFormatException.class, () -> failingFlatMapInputStream.average(),
                EXPECTED_NFE_MESSAGE_INT);
    }

    @Test
    void testIntStreamFromMapBoxed() {
        assertEquals(INPUT_INT, inputStream.boxed().map(Object::toString)
                .collect(Collectors.toList()));
    }

    @Test
    void testIntStreamFromMapBoxedFailing() {
        assertThrows(NumberFormatException.class,
                () -> failingInputStream.boxed().collect(Collectors.toList()),
                EXPECTED_NFE_MESSAGE_INT);
    }

    @Test
    void testIntStreamFromFlatMapBoxed() {
        final List<String> flatMap = FLAT_MAP_INPUT_INT.stream().flatMap(Collection::stream)
                .collect(Collectors.toList());

        assertEquals(flatMap, flatMapInputStream.boxed().map(Object::toString)
                .collect(Collectors.toList()));
    }

    @Test
    void testIntStreamFromFlatMapBoxedFailing() {
        assertThrows(NumberFormatException.class,
                () -> failingFlatMapInputStream.boxed().collect(Collectors.toList()),
                EXPECTED_NFE_MESSAGE_INT);
    }

    @Test
    void testIntStreamFromMapMax() {
        final OptionalInt max = inputStream.max();

        assertTrue(max.isPresent());
        assertEquals(6, max.getAsInt());
    }

    @Test
    void testIntStreamFromMapMaxFailing() {
        assertThrows(NumberFormatException.class, () -> failingInputStream.max(),
                EXPECTED_NFE_MESSAGE_INT);
    }

    @Test
    void testIntStreamFromFlatMapMax() {
        final OptionalInt max = flatMapInputStream.max();

        assertTrue(max.isPresent());
        assertEquals(8, max.getAsInt());
    }

    @Test
    void testIntStreamFromFlatMapMaxFailing() {
        assertThrows(NumberFormatException.class, () -> failingFlatMapInputStream.max(),
                EXPECTED_NFE_MESSAGE_INT);
    }

    @Test
    void testIntStreamFromMapMin() {
        final OptionalInt min = inputStream.min();

        assertTrue(min.isPresent());
        assertEquals(1, min.getAsInt());
    }

    @Test
    void testIntStreamFromMapMinFailing() {
        assertThrows(NumberFormatException.class, () -> failingInputStream.min(),
                EXPECTED_NFE_MESSAGE_INT);
    }

    @Test
    void testIntStreamFromFlatMapMin() {
        final OptionalInt min = flatMapInputStream.min();

        assertTrue(min.isPresent());
        assertEquals(1, min.getAsInt());
    }

    @Test
    void testIntStreamFromFlatMapMinFailing() {
        assertThrows(NumberFormatException.class, () -> failingFlatMapInputStream.min(),
                EXPECTED_NFE_MESSAGE_INT);
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
        assertTrue(INPUT_INT.contains(Integer.toString(first.getAsInt())));
    }

    @Test
    void testParallelIntStreamFromFlatMapFindAny() {
        final OptionalInt any = flatMapInputStream.parallel().findAny();
        final List<String> flatMap = FLAT_MAP_INPUT_INT.stream().flatMap(Collection::stream)
                .collect(Collectors.toList());

        assertTrue(any.isPresent());
        assertTrue(flatMap.contains(Integer.toString(any.getAsInt())));
    }

    @Test
    void testIntStreamFromMapForEach() {
        final List<String> mapList = new ArrayList<>();
        inputStream.forEach(value -> mapList.add(Integer.toString(value)));

        assertEquals(INPUT_INT, mapList);
    }

    @Test
    void testIntStreamFromMapForEachFailing() {
        assertThrows(NumberFormatException.class,
                () -> failingInputStream.forEach(value -> {}),
                EXPECTED_NFE_MESSAGE_INT);
    }

    @Test
    void testIntStreamFromFlatMapForEach() {
        final List<String> flatMapList = new ArrayList<>();
        final List<String> flatMap = FLAT_MAP_INPUT_INT.stream().flatMap(Collection::stream)
                .collect(Collectors.toList());

        flatMapInputStream.forEach(value -> flatMapList.add(Integer.toString(value)));

        assertEquals(flatMap, flatMapList);
    }

    @Test
    void testIntStreamFromFlatMapForEachFailing() {
        assertThrows(NumberFormatException.class,
                () -> failingFlatMapInputStream.forEach(value -> {}),
                EXPECTED_NFE_MESSAGE_INT);
    }

    @Test
    void testIntStreamFromMap_Map() {
        final List<String> expectedResults = Arrays.asList("2", "3", "4", "5", "6", "7");
        final List<String> actualResults = inputStream.map(input -> input + 1)
                .mapToObj(Objects::toString).collect(Collectors.toList());

        assertEquals(expectedResults, actualResults);
    }

    @Test
    void testIntStreamFromMap_MapFailing() {
        final Executable executable = () -> failingInputStream.map(input -> input + 1)
                .mapToObj(Objects::toString).collect(Collectors.toList());

        assertThrows(NumberFormatException.class, executable, EXPECTED_NFE_MESSAGE_INT);
    }

    @Test
    void testIntStreamFromFlatMap_Map() {
        final List<String> expectedResults = Arrays.asList("2", "3", "4", "5", "6", "7", "8", "9");
        final List<String> actualResults = flatMapInputStream.map(input -> input + 1)
                .mapToObj(Objects::toString).collect(Collectors.toList());

        assertEquals(expectedResults, actualResults);
    }

    @Test
    void testIntStreamFromFlatMap_MapFailing() {
        final Executable executable = () -> failingFlatMapInputStream.map(input -> input + 1)
                .mapToObj(Objects::toString).collect(Collectors.toList());

        assertThrows(NumberFormatException.class, executable, EXPECTED_NFE_MESSAGE_INT);
    }

    @Test
    void testIntStreamFromMap_MapToObj() {
        assertEquals(INPUT_INT, inputStream.mapToObj(Objects::toString)
                .collect(Collectors.toList()));
    }

    @Test
    void testIntStreamFromMap_MapToObjFailing() {
        final Executable executable = () -> failingInputStream.mapToObj(Objects::toString)
                .collect(Collectors.toList());

        assertThrows(NumberFormatException.class, executable, EXPECTED_NFE_MESSAGE_INT);
    }

    @Test
    void testIntStreamFromFlatMap_MapToObj() {
        final List<String> flatMap = FLAT_MAP_INPUT_INT.stream().flatMap(Collection::stream)
                .collect(Collectors.toList());

        assertEquals(flatMap, flatMapInputStream.mapToObj(Objects::toString)
                .collect(Collectors.toList()));
    }

    @Test
    void testIntStreamFromFlatMap_MapToObjFailing() {
        final Executable executable = () -> failingFlatMapInputStream
                .mapToObj(Objects::toString).collect(Collectors.toList());

        assertThrows(NumberFormatException.class, executable, EXPECTED_NFE_MESSAGE_INT);
    }

    @Test
    void testIntStreamFromMap_MapToDouble() {
        List<String> convertedDoubles = inputStream.mapToDouble(value -> value + 0.5)
                .boxed().map(Object::toString).collect(Collectors.toList());

        assertEquals(INPUT_DOUBLE, convertedDoubles);
    }

    @Test
    void testIntStreamFromMap_MapToDoubleFailing() {
        final Executable executable = () -> failingInputStream.mapToDouble(value -> value)
                .forEach(value -> {});

        assertThrows(NumberFormatException.class, executable, EXPECTED_NFE_MESSAGE_INT);
    }

    @Test
    void testIntStreamFromFlatMap_MapToDouble() {
        List<String> convertedDoubles = flatMapInputStream.mapToDouble(value -> value + 0.5)
                .boxed().map(Object::toString).collect(Collectors.toList());
        List<String> longFlatMap = FLAT_MAP_INPUT_DOUBLE.stream().flatMap(Collection::stream)
                .collect(Collectors.toList());

        assertEquals(longFlatMap, convertedDoubles);
    }

    @Test
    void testIntStreamFromFlatMap_MapToDoubleFailing() {
        final Executable executable = () -> failingFlatMapInputStream
                .mapToDouble(value -> value).forEach(value -> {});

        assertThrows(NumberFormatException.class, executable, EXPECTED_NFE_MESSAGE_INT);
    }

    @Test
    void testIntStreamFromMap_MapToLong() {
        List<String> convertedDoubles = inputStream.mapToLong(value -> (long) (value * 1E9))
                .boxed().map(Object::toString).collect(Collectors.toList());

        assertEquals(INPUT_LONG, convertedDoubles);
    }

    @Test
    void testIntStreamFromMap_MapToLongFailing() {
        final Executable executable = () -> failingInputStream.mapToLong(value -> value)
                .forEach(value -> {});

        assertThrows(NumberFormatException.class, executable, EXPECTED_NFE_MESSAGE_INT);
    }

    @Test
    void testIntStreamFromFlatMap_MapToLong() {
        List<String> convertedDoubles = flatMapInputStream.mapToLong(value -> (long) (value * 1E9))
                .boxed().map(Object::toString).collect(Collectors.toList());
        List<String> longFlatMap = FLAT_MAP_INPUT_LONG.stream().flatMap(Collection::stream)
                .collect(Collectors.toList());

        assertEquals(longFlatMap, convertedDoubles);
    }

    @Test
    void testIntStreamFromFlatMap_MapToLongFailing() {
        final Executable executable = () -> failingFlatMapInputStream
                .mapToLong(value -> value).forEach(value -> {});

        assertThrows(NumberFormatException.class, executable, EXPECTED_NFE_MESSAGE_INT);
    }

    @Test
    void testIntStreamFromMapAllMatch() {
        assertTrue(inputStream.allMatch(Objects::nonNull));
    }

    @Test
    void testIntStreamFromMapAllMatchFailing() {
        assertThrows(NumberFormatException.class,
                () -> failingInputStream.allMatch(Objects::nonNull),
                EXPECTED_NFE_MESSAGE_INT);
    }

    @Test
    void testIntStreamFromMapAnyMatch() {
        assertTrue(inputStream.anyMatch(i -> i % 2 == 0));
    }

    @Test
    void testIntStreamFromMapAnyMatchFailing() {
        assertThrows(NumberFormatException.class,
                () -> failingInputStream.anyMatch(i -> i == 4),
                EXPECTED_NFE_MESSAGE_INT);
    }

    @Test
    void testIntStreamFromMapNoneMatch() {
        assertTrue(inputStream.noneMatch(Objects::isNull));
    }

    @Test
    void testIntStreamFromMapNoneMatchFailing() {
        assertThrows(NumberFormatException.class,
                () -> failingInputStream.noneMatch(Objects::isNull),
                EXPECTED_NFE_MESSAGE_INT);
    }
}
