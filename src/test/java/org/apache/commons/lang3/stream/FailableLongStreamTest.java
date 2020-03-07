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

import static org.apache.commons.lang3.stream.TestStringConstants.EXPECTED_EXCEPTION;
import static org.apache.commons.lang3.stream.TestStringConstants.EXPECTED_NFE_MESSAGE_LONG;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

public class FailableLongStreamTest {
    private static final Functions.FailableFunction<List<String>, LongStream, ?> LIST_TO_LONG_STREAM =
            list -> list.stream().mapToLong(Long::valueOf);

    private final List<String> input = Arrays.asList("1000000000", "2000000000", "3000000000",
            "4000000000", "5000000000", "6000000000");
    private final List<String> failingInput = Arrays.asList("1000000000", "2000000000", "3000000000",
            "4000000000 ", "5000000000", "6000000000");
    private final List<List<String>> flatMapInput = Arrays.asList(input,
            Arrays.asList("7000000000", "8000000000"));
    private final List<List<String>> failingFlatMapInput = Arrays.asList(failingInput,
            Arrays.asList("7000000000", "8000000000"));

    private FailableLongStream inputStream;
    private FailableLongStream failingInputStream;
    private FailableLongStream flatMapInputStream;
    private FailableLongStream failingFlatMapInputStream;

    @BeforeEach
    void beforeEach() {
        inputStream = Functions.stream(input).mapToLong(Long::valueOf);
        failingInputStream = Functions.stream(failingInput).mapToLong(Long::valueOf);
        flatMapInputStream = Functions.stream(flatMapInput).flatMapToLong(LIST_TO_LONG_STREAM);
        failingFlatMapInputStream = Functions.stream(failingFlatMapInput).flatMapToLong(LIST_TO_LONG_STREAM);
    }

    @Test
    void testLongStreamFromMapCount() {
        assertEquals(6L, inputStream.count());
    }

    @Test
    void testLongStreamFromMapCountFailing() {
        assertEquals(6L, failingInputStream.count());
    }

    @Test
    void testLongStreamFromFlatMapCount() {
        assertEquals(8L, flatMapInputStream.count());
    }

    @Test
    void testLongStreamFromFlatMapCountFailing() {
        try {
            failingFlatMapInputStream.count();
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_LONG, nfe.getMessage());
        }
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
        assertEquals(input, inputStream.boxed().map(Object::toString).collect(Collectors.toList()));
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
        final List<String> flatMap = flatMapInput.stream().flatMap(Collection::stream)
                .collect(Collectors.toList());

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
    void testLongStreamFromMap_MapToObj() {
        assertEquals(input, inputStream.mapToObj(Objects::toString).collect(Collectors.toList()));
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
        final List<String> flatMap = flatMapInput.stream().flatMap(Collection::stream)
                .collect(Collectors.toList());

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
        assertTrue(input.contains(Long.toString(first.getAsLong())));
    }

    @Test
    void testParallelLongStreamFromFlatMapFindAny() {
        final OptionalLong any = flatMapInputStream.parallel().findAny();
        final List<String> flatMap = flatMapInput.stream().flatMap(Collection::stream)
                .collect(Collectors.toList());

        assertTrue(any.isPresent());
        assertTrue(flatMap.contains(Long.toString(any.getAsLong())));
    }

    @Test
    void testLongStreamFromMapForEach() {
        final List<String> mapList = new ArrayList<>();
        inputStream.forEach(value -> mapList.add(Long.toString(value)));

        assertEquals(input, mapList);
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
        final List<String> flatMap = flatMapInput.stream().flatMap(Collection::stream)
                .collect(Collectors.toList());

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
}
