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
import java.util.stream.IntStream;

import static org.apache.commons.lang3.stream.TestStringConstants.EXPECTED_EXCEPTION;
import static org.apache.commons.lang3.stream.TestStringConstants.EXPECTED_NFE_MESSAGE_INT;
import static org.junit.jupiter.api.Assertions.assertEquals;
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
}
