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
import java.util.OptionalLong;
import java.util.stream.Collectors;
import java.util.stream.LongStream;

import static org.apache.commons.lang3.stream.TestConstants.EXPECTED_NFE_MESSAGE_LONG;
import static org.apache.commons.lang3.stream.TestConstants.FLAT_MAP_INPUT_DOUBLE;
import static org.apache.commons.lang3.stream.TestConstants.FLAT_MAP_INPUT_INT;
import static org.apache.commons.lang3.stream.TestConstants.FLAT_MAP_INPUT_LONG;
import static org.apache.commons.lang3.stream.TestConstants.INPUT_DOUBLE;
import static org.apache.commons.lang3.stream.TestConstants.INPUT_INT;
import static org.apache.commons.lang3.stream.TestConstants.INPUT_LONG;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

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
    void testLongStreamFromMapDistinct() {
        final List<Long> input = inputStream.boxed().collect(Collectors.toList());
        input.add((long) 1E9);

        final List<String> actualResult = Functions.failableLongStream(input, Long::valueOf)
                .distinct().boxed().map(Objects::toString).collect(Collectors.toList());

        assertEquals(INPUT_LONG, actualResult);
    }

    @Test
    void testLongStreamFromMapDistinctFailing() {
        final List<String> input = new ArrayList<>(failingInput);
        input.add("1000000000");

        final Executable executable = () -> Functions.failableLongStream(input, Long::valueOf)
                .distinct().boxed().map(Objects::toString).collect(Collectors.toList());

        assertThrows(NumberFormatException.class, executable, EXPECTED_NFE_MESSAGE_LONG);
    }

    @Test
    void testLongStreamFromFlatMapDistinct() {
        final List<Long> input = flatMapInputStream.boxed().collect(Collectors.toList());
        input.add((long) 1E9);

        final List<String> expectedResult = FLAT_MAP_INPUT_LONG.stream()
                .flatMap(Collection::stream).collect(Collectors.toList());
        final List<String> actualResult = Functions.failableLongStream(input, Long::valueOf)
                .distinct().boxed().map(Objects::toString).collect(Collectors.toList());

        assertEquals(expectedResult, actualResult);
    }

    @Test
    void testLongStreamFromFlatMapDistinctFailing() {
        final List<String> input = failingFlatMapInput.stream().flatMap(Collection::stream)
                .collect(Collectors.toList());
        input.add("1000000000");

        final Executable executable = () -> Functions.failableLongStream(input, Long::valueOf)
                .distinct().boxed().map(Objects::toString).collect(Collectors.toList());

        assertThrows(NumberFormatException.class, executable, EXPECTED_NFE_MESSAGE_LONG);
    }

    @Test
    void testLongStreamFromMapLimit() {
        assertEquals(4L, inputStream.limit(4).count());
    }

    @Test
    void testLongStreamFromMapLimitFailing() {
        assertThrows(NumberFormatException.class,
                () -> failingInputStream.limit(4).count(), EXPECTED_NFE_MESSAGE_LONG);
    }

    @Test
    void testLongStreamFromMapLimit_ExcludeInvalidElement() {
        assertEquals(3L, failingInputStream.limit(3).count());
    }

    @Test
    void testLongStreamFromFlatMapLimit() {
        assertEquals(4L, flatMapInputStream.limit(4).count());
    }

    @Test
    void testLongStreamFromFlatMapLimitFailing() {
        assertThrows(NumberFormatException.class,
                () -> failingFlatMapInputStream.limit(4).count(), EXPECTED_NFE_MESSAGE_LONG);
    }

    @Test
    void testLongStreamFromMapSkip() {
        assertEquals(4L, inputStream.skip(2).count());
    }

    @Test
    void testLongStreamFromMapSkipFailing() {
        assertThrows(NumberFormatException.class,
                () -> failingInputStream.skip(2).count(), EXPECTED_NFE_MESSAGE_LONG);
    }

    @Test
    void testLongStreamFromFlatMapSkip() {
        assertEquals(6L, flatMapInputStream.skip(2).count());
    }

    @Test
    void testLongStreamFromFlatMapSkipFailing() {
        assertThrows(NumberFormatException.class,
                () -> failingFlatMapInputStream.skip(2).count(), EXPECTED_NFE_MESSAGE_LONG);
    }

    @Test
    void testLongStreamFromMapSum() {
        assertEquals(2.1E10, inputStream.sum());
    }

    @Test
    void testLongStreamFromMapSumFailing() {
        assertThrows(NumberFormatException.class,
                () -> failingInputStream.sum(), EXPECTED_NFE_MESSAGE_LONG);
    }

    @Test
    void testLongStreamFromFlatMapSum() {
        assertEquals(3.6E10, flatMapInputStream.sum());
    }

    @Test
    void testLongStreamFromFlatMapSumFailing() {
        assertThrows(NumberFormatException.class,
                () -> failingFlatMapInputStream.sum(), EXPECTED_NFE_MESSAGE_LONG);
    }

    @Test
    void testLongStreamFromMapAverage() {
        final OptionalDouble average = inputStream.average();

        assertTrue(average.isPresent());
        assertEquals(3.5E9, average.getAsDouble());
    }

    @Test
    void testLongStreamFromMapAverageFailing() {
        assertThrows(NumberFormatException.class,
                () -> failingInputStream.average(), EXPECTED_NFE_MESSAGE_LONG);
    }

    @Test
    void testLongStreamFromFlatMapAverage() {
        final OptionalDouble average = flatMapInputStream.average();

        assertTrue(average.isPresent());
        assertEquals(4.5E9, average.getAsDouble());
    }

    @Test
    void testLongStreamFromFlatMapAverageFailing() {
        assertThrows(NumberFormatException.class,
                () -> failingFlatMapInputStream.average(), EXPECTED_NFE_MESSAGE_LONG);
    }

    @Test
    void testLongStreamFromMapBoxed() {
        assertEquals(INPUT_LONG, inputStream.boxed().map(Object::toString)
                .collect(Collectors.toList()));
    }

    @Test
    void testLongStreamFromMapBoxedFailing() {
        assertThrows(NumberFormatException.class,
                () -> failingInputStream.boxed().collect(Collectors.toList()),
                EXPECTED_NFE_MESSAGE_LONG);
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
        assertThrows(NumberFormatException.class,
                () -> failingFlatMapInputStream.boxed().collect(Collectors.toList()),
                EXPECTED_NFE_MESSAGE_LONG);
    }

    @Test
    void testLongStreamFromMapMax() {
        final OptionalLong max = inputStream.max();

        assertTrue(max.isPresent());
        assertEquals(6E9, max.getAsLong());
    }

    @Test
    void testLongStreamFromMapMaxFailing() {
        assertThrows(NumberFormatException.class, () -> failingInputStream.max(),
                EXPECTED_NFE_MESSAGE_LONG);
    }

    @Test
    void testLongStreamFromFlatMapMax() {
        final OptionalLong max = flatMapInputStream.max();

        assertTrue(max.isPresent());
        assertEquals(8E9, max.getAsLong());
    }

    @Test
    void testLongStreamFromFlatMapMaxFailing() {
        assertThrows(NumberFormatException.class, () -> failingFlatMapInputStream.max(),
                EXPECTED_NFE_MESSAGE_LONG);
    }

    @Test
    void testLongStreamFromMapMin() {
        final OptionalLong min = inputStream.min();

        assertTrue(min.isPresent());
        assertEquals(1E9, min.getAsLong());
    }

    @Test
    void testLongStreamFromMapMinFailing() {
        assertThrows(NumberFormatException.class, () -> failingInputStream.min(),
                EXPECTED_NFE_MESSAGE_LONG);
    }

    @Test
    void testLongStreamFromFlatMapMin() {
        final OptionalLong min = flatMapInputStream.min();

        assertTrue(min.isPresent());
        assertEquals(1E9, min.getAsLong());
    }

    @Test
    void testLongStreamFromFlatMapMinFailing() {
        assertThrows(NumberFormatException.class,
                () -> failingFlatMapInputStream.min(), EXPECTED_NFE_MESSAGE_LONG);
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
        assertThrows(NumberFormatException.class,
                () -> failingInputStream.forEach(value -> {}), EXPECTED_NFE_MESSAGE_LONG);
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
        assertThrows(NumberFormatException.class,
                () -> failingFlatMapInputStream.forEach(value -> {}), EXPECTED_NFE_MESSAGE_LONG);
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
        final Executable executable = () -> failingInputStream.map(input -> input + 1)
                .mapToObj(Objects::toString).collect(Collectors.toList());

        assertThrows(NumberFormatException.class, executable, EXPECTED_NFE_MESSAGE_LONG);
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
        final Executable executable = () -> failingFlatMapInputStream.map(input -> input + 1)
                .mapToObj(Objects::toString).collect(Collectors.toList());

        assertThrows(NumberFormatException.class, executable, EXPECTED_NFE_MESSAGE_LONG);
    }

    @Test
    void testLongStreamFromMap_MapToObj() {
        assertEquals(INPUT_LONG, inputStream.mapToObj(Objects::toString)
                .collect(Collectors.toList()));
    }

    @Test
    void testLongStreamFromMap_MapToObjFailing() {
        final Executable executable = () -> failingInputStream.mapToObj(Objects::toString)
                .collect(Collectors.toList());

        assertThrows(NumberFormatException.class, executable, EXPECTED_NFE_MESSAGE_LONG);
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
        final Executable executable = () -> failingFlatMapInputStream
                .mapToObj(Objects::toString).collect(Collectors.toList());

        assertThrows(NumberFormatException.class, executable, EXPECTED_NFE_MESSAGE_LONG);
    }

    @Test
    void testLongStreamFromMap_MapToDouble() {
        List<String> convertedDoubles = inputStream.mapToDouble(value -> (value / 1E9) + 0.5)
                .boxed().map(Object::toString).collect(Collectors.toList());

        assertEquals(INPUT_DOUBLE, convertedDoubles);
    }

    @Test
    void testLongStreamFromMap_MapToDoubleFailing() {
        final Executable executable = () -> failingInputStream.mapToDouble(value -> value)
                .forEach(value -> {});

        assertThrows(NumberFormatException.class, executable, EXPECTED_NFE_MESSAGE_LONG);
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
        final Executable executable = () -> failingFlatMapInputStream
                .mapToDouble(value -> value).forEach(value -> {});

        assertThrows(NumberFormatException.class, executable, EXPECTED_NFE_MESSAGE_LONG);
    }

    @Test
    void testLongStreamFromMap_MapToInt() {
        List<String> convertedDoubles = inputStream.mapToInt(value -> (int) (value / 1E9))
                .boxed().map(Object::toString).collect(Collectors.toList());

        assertEquals(INPUT_INT, convertedDoubles);
    }

    @Test
    void testLongStreamFromMap_MapToIntFailing() {
        final Executable executable = () -> failingInputStream
                .mapToInt(value -> (int) (value / 1E9)).forEach(value -> {});

        assertThrows(NumberFormatException.class, executable, EXPECTED_NFE_MESSAGE_LONG);
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
        final Executable executable = () -> failingFlatMapInputStream
                .mapToInt(value -> (int) (value / 1E9)).forEach(value -> {});

        assertThrows(NumberFormatException.class, executable, EXPECTED_NFE_MESSAGE_LONG);
    }

    @Test
    void testLongStreamFromMapAllMatch() {
        assertTrue(inputStream.allMatch(Objects::nonNull));
    }

    @Test
    void testLongStreamFromMapAllMatchFailing() {
        assertThrows(NumberFormatException.class,
                () -> failingInputStream.allMatch(Objects::nonNull), EXPECTED_NFE_MESSAGE_LONG);
    }

    @Test
    void testLongStreamFromMapAnyMatch() {
        assertTrue(inputStream.anyMatch(i -> i % 2 == 0));
    }

    @Test
    void testLongStreamFromMapAnyMatchFailing() {
        assertThrows(NumberFormatException.class,
                () -> failingInputStream.anyMatch(i -> i == 4000000000L),
                EXPECTED_NFE_MESSAGE_LONG);
    }

    @Test
    void testLongStreamFromMapNoneMatch() {
        assertTrue(inputStream.noneMatch(Objects::isNull));
    }

    @Test
    void testLongStreamFromMapNoneMatchFailing() {
        assertThrows(NumberFormatException.class,
                () -> failingInputStream.noneMatch(Objects::isNull), EXPECTED_NFE_MESSAGE_LONG);
    }
}
