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

import java.util.Arrays;
import java.util.List;
import java.util.stream.LongStream;

import static org.apache.commons.lang3.stream.TestStringConstants.EXPECTED_EXCEPTION;
import static org.junit.jupiter.api.Assertions.assertEquals;
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
    void testLongStreamFromMapSum() {
        assertEquals(21000000000L, inputStream.sum());
    }

    @Test
    void testLongStreamFromMapSumFailing() {
        try {
            failingInputStream.sum();
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals("For input string: \"4000000000 \"", nfe.getMessage());
        }
    }

    @Test
    void testLongStreamFromFlatMapSum() {
        assertEquals(36000000000L, flatMapInputStream.sum());
    }

    @Test
    void testLongStreamFromFlatMapSumFailing() {
        try {
            failingFlatMapInputStream.sum();
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals("For input string: \"4000000000 \"", nfe.getMessage());
        }
    }
}
