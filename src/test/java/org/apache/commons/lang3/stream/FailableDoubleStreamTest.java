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
import java.util.stream.Collectors;
import java.util.stream.DoubleStream;

import static org.apache.commons.lang3.stream.TestConstants.EXPECTED_NFE_MESSAGE_DOUBLE;
import static org.apache.commons.lang3.stream.TestConstants.FLAT_MAP_INPUT_DOUBLE;
import static org.apache.commons.lang3.stream.TestConstants.FLAT_MAP_INPUT_INT;
import static org.apache.commons.lang3.stream.TestConstants.FLAT_MAP_INPUT_LONG;
import static org.apache.commons.lang3.stream.TestConstants.INPUT_DOUBLE;
import static org.apache.commons.lang3.stream.TestConstants.INPUT_INT;
import static org.apache.commons.lang3.stream.TestConstants.INPUT_LONG;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class FailableDoubleStreamTest {
    private static final Functions.FailableFunction<List<String>, DoubleStream, ?> LIST_TO_DOUBLE_STREAM =
            list -> list.stream().mapToDouble(Double::valueOf);

    private final List<String> failingInput = Arrays.asList("1.5", "2.5", "3.5", "4.5x", "5.5", "6.5");
    private final List<List<String>> failingFlatMapInput = Arrays.asList(failingInput,
            Arrays.asList("7.5", "8.5"));

    private FailableDoubleStream inputStream;
    private FailableDoubleStream failingInputStream;
    private FailableDoubleStream flatMapInputStream;
    private FailableDoubleStream failingFlatMapInputStream;

    @BeforeEach
    void beforeEach() {
        inputStream = Functions.failableDoubleStream(INPUT_DOUBLE, Double::valueOf);
        failingInputStream = Functions.failableDoubleStream(failingInput, Double::valueOf);
        flatMapInputStream = Functions.failableStream(FLAT_MAP_INPUT_DOUBLE)
                .flatMapToDouble(LIST_TO_DOUBLE_STREAM);
        failingFlatMapInputStream = Functions.failableStream(failingFlatMapInput)
                .flatMapToDouble(LIST_TO_DOUBLE_STREAM);
    }

    @Test
    void testDoubleStreamFromMapCount() {
        assertEquals(6L, inputStream.count());
    }

    @Test
    void testDoubleStreamFromFlatMapCount() {
        assertEquals(8L, flatMapInputStream.count());
    }

    @Test
    void testDoubleStreamFromMapDistinct() {
        final List<Double> input = inputStream.boxed().collect(Collectors.toList());
        input.add(1.5);

        final List<String> actualResult = Functions.failableDoubleStream(input, Double::valueOf)
                .distinct().boxed().map(Objects::toString).collect(Collectors.toList());

        assertEquals(INPUT_DOUBLE, actualResult);
    }

    @Test
    void testDoubleStreamFromMapDistinctFailing() {
        final List<String> input = new ArrayList<>(failingInput);
        input.add("1.5");

        final Executable executable = () -> Functions.failableDoubleStream(input, Double::valueOf)
                .distinct().boxed().map(Objects::toString).collect(Collectors.toList());

        assertThrows(NumberFormatException.class, executable, EXPECTED_NFE_MESSAGE_DOUBLE);
    }

    @Test
    void testDoubleStreamFromFlatMapDistinct() {
        final List<Double> input = flatMapInputStream.boxed().collect(Collectors.toList());
        input.add(1.5);

        final List<String> expectedResult = FLAT_MAP_INPUT_DOUBLE.stream()
                .flatMap(Collection::stream).collect(Collectors.toList());
        final List<String> actualResult = Functions.failableDoubleStream(input, Double::valueOf)
                .distinct().boxed().map(Objects::toString).collect(Collectors.toList());

        assertEquals(expectedResult, actualResult);
    }

    @Test
    void testDoubleStreamFromFlatMapDistinctFailing() {
        final List<String> input = failingFlatMapInput.stream().flatMap(Collection::stream)
                .collect(Collectors.toList());
        input.add("1.5");

        final Executable executable = () -> Functions.failableDoubleStream(input, Double::valueOf)
                .distinct().boxed().map(Objects::toString).collect(Collectors.toList());

        assertThrows(NumberFormatException.class, executable, EXPECTED_NFE_MESSAGE_DOUBLE);
    }

    @Test
    void testDoubleStreamFromMapLimit() {
        assertEquals(4L, inputStream.limit(4).count());
    }

    @Test
    void testDoubleStreamFromMapLimit_IncludeInvalidElement() {
        assertThrows(NumberFormatException.class, () -> failingInputStream.limit(4).count(),
                EXPECTED_NFE_MESSAGE_DOUBLE);
    }

    @Test
    void testDoubleStreamFromMapLimit_ExcludeInvalidElement() {
        assertEquals(3L, failingInputStream.limit(3).count());
    }

    @Test
    void testDoubleStreamFromFlatMapLimit() {
        assertEquals(4L, flatMapInputStream.limit(4).count());
    }

    @Test
    void testDoubleStreamFromFlatMapLimitFailing() {
        assertThrows(NumberFormatException.class,
                () -> failingFlatMapInputStream.limit(4).count(), EXPECTED_NFE_MESSAGE_DOUBLE);
    }

    @Test
    void testDoubleStreamFromMapSkip() {
        assertEquals(4L, inputStream.skip(2).count());
    }

    @Test
    void testDoubleStreamFromMapSkipFailing() {
        assertThrows(NumberFormatException.class, () -> failingInputStream.skip(2).count(),
                EXPECTED_NFE_MESSAGE_DOUBLE);
    }

    @Test
    void testDoubleStreamFromFlatMapSkip() {
        assertEquals(6L, flatMapInputStream.skip(2).count());
    }

    @Test
    void testDoubleStreamFromFlatMapSkipFailing() {
        assertThrows(NumberFormatException.class,
                () -> failingFlatMapInputStream.skip(2).count(), EXPECTED_NFE_MESSAGE_DOUBLE);
    }

    @Test
    void testDoubleStreamFromMapSum() {
        assertEquals(24.0, inputStream.sum());
    }

    @Test
    void testDoubleStreamFromMapSumFailing() {
        assertThrows(NumberFormatException.class, () -> failingInputStream.sum(),
                EXPECTED_NFE_MESSAGE_DOUBLE);
    }

    @Test
    void testDoubleStreamFromFlatMapSum() {
        assertEquals(40, flatMapInputStream.sum());
    }

    @Test
    void testDoubleStreamFromFlatMapSumFailing() {
        assertThrows(NumberFormatException.class, () -> failingFlatMapInputStream.sum(),
                EXPECTED_NFE_MESSAGE_DOUBLE);
    }

    @Test
    void testDoubleStreamFromMapAverage() {
        final OptionalDouble average = inputStream.average();

        assertTrue(average.isPresent());
        assertEquals(4.0, average.getAsDouble());
    }

    @Test
    void testDoubleStreamFromMapAverageFailing() {
        assertThrows(NumberFormatException.class, () -> failingInputStream.average(),
                EXPECTED_NFE_MESSAGE_DOUBLE);
    }

    @Test
    void testDoubleStreamFromFlatMapAverage() {
        final OptionalDouble average = flatMapInputStream.average();

        assertTrue(average.isPresent());
        assertEquals(5.0, average.getAsDouble());
    }

    @Test
    void testDoubleStreamFromFlatMapAverageFailing() {
        assertThrows(NumberFormatException.class, () -> failingFlatMapInputStream.average(),
                EXPECTED_NFE_MESSAGE_DOUBLE);
    }

    @Test
    void testDoubleStreamFromMapBoxed() {
        assertEquals(INPUT_DOUBLE, inputStream.boxed().map(Object::toString)
                .collect(Collectors.toList()));
    }

    @Test
    void testDoubleStreamFromMapBoxedFailing() {
        assertThrows(NumberFormatException.class,
                () -> failingInputStream.boxed().collect(Collectors.toList()),
                EXPECTED_NFE_MESSAGE_DOUBLE);
    }

    @Test
    void testDoubleStreamFromFlatMapBoxed() {
        final List<String> flatMap = FLAT_MAP_INPUT_DOUBLE.stream()
                .flatMap(Collection::stream).collect(Collectors.toList());

        assertEquals(flatMap, flatMapInputStream.boxed().map(Object::toString)
                .collect(Collectors.toList()));
    }

    @Test
    void testDoubleStreamFromFlatMapBoxedFailing() {
        assertThrows(NumberFormatException.class,
                () -> failingFlatMapInputStream.boxed().collect(Collectors.toList()),
                EXPECTED_NFE_MESSAGE_DOUBLE);
    }

    @Test
    void testDoubleStreamFromMapMax() {
        final OptionalDouble max = inputStream.max();

        assertTrue(max.isPresent());
        assertEquals(6.5, max.getAsDouble());
    }

    @Test
    void testDoubleStreamFromMapMaxFailing() {
        assertThrows(NumberFormatException.class, () -> failingInputStream.max(),
                EXPECTED_NFE_MESSAGE_DOUBLE);
    }

    @Test
    void testDoubleStreamFromFlatMapMax() {
        final OptionalDouble max = flatMapInputStream.max();

        assertTrue(max.isPresent());
        assertEquals(8.5, max.getAsDouble());
    }

    @Test
    void testDoubleStreamFromFlatMapMaxFailing() {
        assertThrows(NumberFormatException.class, () -> failingFlatMapInputStream.max(),
                EXPECTED_NFE_MESSAGE_DOUBLE);
    }

    @Test
    void testDoubleStreamFromMapMin() {
        final OptionalDouble min = inputStream.min();

        assertTrue(min.isPresent());
        assertEquals(1.5, min.getAsDouble());
    }

    @Test
    void testDoubleStreamFromMapMinFailing() {
        assertThrows(NumberFormatException.class, () -> failingInputStream.min(),
                EXPECTED_NFE_MESSAGE_DOUBLE);
    }

    @Test
    void testDoubleStreamFromFlatMapMin() {
        final OptionalDouble min = flatMapInputStream.min();

        assertTrue(min.isPresent());
        assertEquals(1.5, min.getAsDouble());
    }

    @Test
    void testDoubleStreamFromFlatMapMinFailing() {
        assertThrows(NumberFormatException.class, () -> failingFlatMapInputStream.min(),
                EXPECTED_NFE_MESSAGE_DOUBLE);
    }

    @Test
    void testSequentialDoubleStreamFromMapFindFirst() {
        final OptionalDouble first = inputStream.sequential().findFirst();

        assertTrue(first.isPresent());
        assertEquals(1.5, first.getAsDouble());
    }

    @Test
    void testSequentialDoubleStreamFromFlatMapFindFirst() {
        final OptionalDouble first = flatMapInputStream.sequential().findFirst();

        assertTrue(first.isPresent());
        assertEquals(1.5, first.getAsDouble());
    }

    @Test
    void testParallelDoubleStreamFromMapFindFirst() {
        final OptionalDouble first = inputStream.parallel().findFirst();

        assertTrue(first.isPresent());
        assertEquals(1.5, first.getAsDouble());
    }

    @Test
    void testParallelDoubleStreamFromFlatMapFindFirst() {
        final OptionalDouble first = flatMapInputStream.parallel().findFirst();

        assertTrue(first.isPresent());
        assertEquals(1.5, first.getAsDouble());
    }

    @Test
    void testSequentialDoubleStreamFromMapFindAny() {
        final OptionalDouble any = inputStream.sequential().findAny();

        assertTrue(any.isPresent());
        assertEquals(1.5, any.getAsDouble());
    }

    @Test
    void testSequentialDoubleStreamFromFlatMapFindAny() {
        final OptionalDouble any = flatMapInputStream.sequential().findAny();

        assertTrue(any.isPresent());
        assertEquals(1.5, any.getAsDouble());
    }

    @Test
    void testParallelDoubleStreamFromMapFindAny() {
        final OptionalDouble first = inputStream.parallel().findAny();

        assertTrue(first.isPresent());
        assertTrue(INPUT_DOUBLE.contains(Double.toString(first.getAsDouble())));
    }

    @Test
    void testParallelDoubleStreamFromFlatMapFindAny() {
        final OptionalDouble any = flatMapInputStream.parallel().findAny();
        final List<String> flatMap = FLAT_MAP_INPUT_DOUBLE.stream().flatMap(Collection::stream)
                .collect(Collectors.toList());

        assertTrue(any.isPresent());
        assertTrue(flatMap.contains(Double.toString(any.getAsDouble())));
    }

    @Test
    void testDoubleStreamFromMapForEach() {
        final List<String> mapList = new ArrayList<>();
        inputStream.forEach(value -> mapList.add(Double.toString(value)));

        assertEquals(INPUT_DOUBLE, mapList);
    }

    @Test
    void testDoubleStreamFromMapForEachFailing() {
        assertThrows(NumberFormatException.class, () -> failingInputStream.forEach(value -> {}),
                EXPECTED_NFE_MESSAGE_DOUBLE);
    }

    @Test
    void testDoubleStreamFromFlatMapForEach() {
        final List<String> flatMapList = new ArrayList<>();
        final List<String> flatMap = FLAT_MAP_INPUT_DOUBLE.stream().flatMap(Collection::stream)
                .collect(Collectors.toList());

        flatMapInputStream.forEach(value -> flatMapList.add(Double.toString(value)));

        assertEquals(flatMap, flatMapList);
    }

    @Test
    void testDoubleStreamFromFlatMapForEachFailing() {
        assertThrows(NumberFormatException.class,
                () -> failingFlatMapInputStream.forEach(value -> {}),
                EXPECTED_NFE_MESSAGE_DOUBLE);
    }

    @Test
    void testDoubleStreamFromMap_Map() {
        final List<String> expectedResults = Arrays.asList("2.5", "3.5", "4.5", "5.5", "6.5", "7.5");
        final List<String> actualResults = inputStream.map(input -> input + 1)
                .mapToObj(Objects::toString).collect(Collectors.toList());

        assertEquals(expectedResults, actualResults);
    }

    @Test
    void testDoubleStreamFromMap_MapFailing() {
        final Executable executable = () -> failingInputStream.map(input -> input + 1)
                .mapToObj(Objects::toString).collect(Collectors.toList());

        assertThrows(NumberFormatException.class, executable, EXPECTED_NFE_MESSAGE_DOUBLE);
    }

    @Test
    void testDoubleStreamFromFlatMap_Map() {
        final List<String> expectedResults = Arrays.asList("2.5", "3.5", "4.5", "5.5", "6.5",
                "7.5", "8.5", "9.5");
        final List<String> actualResults = flatMapInputStream.map(input -> input + 1)
                .mapToObj(Objects::toString).collect(Collectors.toList());

        assertEquals(expectedResults, actualResults);
    }

    @Test
    void testDoubleStreamFromFlatMap_MapFailing() {
        final Executable executable = () -> failingFlatMapInputStream
                .map(input -> input + 1).mapToObj(Objects::toString)
                .collect(Collectors.toList());

        assertThrows(NumberFormatException.class, executable, EXPECTED_NFE_MESSAGE_DOUBLE);
    }

    @Test
    void testDoubleStreamFromMap_MapToObj() {
        assertEquals(INPUT_DOUBLE, inputStream.mapToObj(Objects::toString)
                .collect(Collectors.toList()));
    }

    @Test
    void testDoubleStreamFromMap_MapToObjFailing() {
        final Executable executable = () -> failingInputStream.mapToObj(Objects::toString)
                .collect(Collectors.toList());

        assertThrows(NumberFormatException.class, executable, EXPECTED_NFE_MESSAGE_DOUBLE);
    }

    @Test
    void testDoubleStreamFromFlatMap_MapToObj() {
        final List<String> flatMap = FLAT_MAP_INPUT_DOUBLE.stream()
                .flatMap(Collection::stream).collect(Collectors.toList());

        assertEquals(flatMap, flatMapInputStream.mapToObj(Objects::toString)
                .collect(Collectors.toList()));
    }

    @Test
    void testDoubleStreamFromFlatMap_MapToObjFailing() {
        final Executable executable = () -> failingFlatMapInputStream.mapToObj(Objects::toString)
                .collect(Collectors.toList());

        assertThrows(NumberFormatException.class, executable, EXPECTED_NFE_MESSAGE_DOUBLE);
    }

    @Test
    void testDoubleStreamFromMap_MapToInt() {
        List<String> convertedDoubles = inputStream.mapToInt(value -> (int) value)
                .boxed().map(Object::toString).collect(Collectors.toList());

        assertEquals(INPUT_INT, convertedDoubles);
    }

    @Test
    void testDoubleStreamFromMap_MapToIntFailing() {
        final Executable executable = () -> failingInputStream.mapToInt(value -> (int) value)
                .forEach(value -> {});

        assertThrows(NumberFormatException.class, executable, EXPECTED_NFE_MESSAGE_DOUBLE);
    }

    @Test
    void testDoubleStreamFromFlatMap_MapToInt() {
        List<String> convertedDoubles = flatMapInputStream.mapToInt(value -> (int) value)
                .boxed().map(Object::toString).collect(Collectors.toList());
        List<String> intFlatMap = FLAT_MAP_INPUT_INT.stream().flatMap(Collection::stream)
                .collect(Collectors.toList());

        assertEquals(intFlatMap, convertedDoubles);
    }

    @Test
    void testDoubleStreamFromFlatMap_MapToIntFailing() {
        final Executable executable = () -> failingFlatMapInputStream
                .mapToInt(value -> (int) value).forEach(value -> {});

        assertThrows(NumberFormatException.class, executable, EXPECTED_NFE_MESSAGE_DOUBLE);
    }

    @Test
    void testDoubleStreamFromMap_MapToLong() {
        List<String> convertedDoubles = inputStream.mapToLong(value -> (long) ((long) value * 1E9))
                .boxed().map(Object::toString).collect(Collectors.toList());

        assertEquals(INPUT_LONG, convertedDoubles);
    }

    @Test
    void testDoubleStreamFromMap_MapToLongFailing() {
        final Executable executable = () -> failingInputStream
                .mapToLong(value -> (long) value).forEach(value -> {});

        assertThrows(NumberFormatException.class, executable, EXPECTED_NFE_MESSAGE_DOUBLE);
    }

    @Test
    void testDoubleStreamFromFlatMap_MapToLong() {
        List<String> convertedDoubles = flatMapInputStream.mapToLong(value -> (long) ((long) value * 1E9))
                .boxed().map(Object::toString).collect(Collectors.toList());
        List<String> longFlatMap = FLAT_MAP_INPUT_LONG.stream().flatMap(Collection::stream)
                .collect(Collectors.toList());

        assertEquals(longFlatMap, convertedDoubles);
    }

    @Test
    void testDoubleStreamFromFlatMap_MapToLongFailing() {
        final Executable executable = () -> failingFlatMapInputStream
                .mapToLong(value -> (long) value).forEach(value -> {});

        assertThrows(NumberFormatException.class, executable, EXPECTED_NFE_MESSAGE_DOUBLE);
    }

    @Test
    void testDoubleStreamFromMapAllMatch() {
        assertTrue(inputStream.allMatch(Objects::nonNull));
    }

    @Test
    void testDoubleStreamFromMapAllMatchFailing() {
        assertThrows(NumberFormatException.class,
                () -> failingInputStream.allMatch(Objects::nonNull),
                EXPECTED_NFE_MESSAGE_DOUBLE);
    }

    @Test
    void testDoubleStreamFromMapAnyMatch() {
        assertTrue(inputStream.anyMatch(i -> i % 2 != 0));
    }

    @Test
    void testDoubleStreamFromMapAnyMatchFailing() {
        assertThrows(NumberFormatException.class,
                () -> failingInputStream.anyMatch(i -> i == 4.5),
                EXPECTED_NFE_MESSAGE_DOUBLE);
    }

    @Test
    void testDoubleStreamFromMapNoneMatch() {
        assertTrue(inputStream.noneMatch(Objects::isNull));
    }

    @Test
    void testDoubleStreamFromMapNoneMatchFailing() {
        assertThrows(NumberFormatException.class,
                () -> failingInputStream.noneMatch(Objects::isNull),
                EXPECTED_NFE_MESSAGE_DOUBLE);
    }
}
