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
import java.util.stream.DoubleStream;

import static org.apache.commons.lang3.stream.TestStringConstants.EXPECTED_EXCEPTION;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

public class FailableDoubleStreamTest {
    private static final Functions.FailableFunction<List<String>, DoubleStream, ?> LIST_TO_DOUBLE_STREAM =
            list -> list.stream().mapToDouble(Double::valueOf);

    private final List<String> input = Arrays.asList("1.5", "2.5", "3.5", "4.5", "5.5", "6.5");
    private final List<String> failingInput = Arrays.asList("1.5", "2.5", "3.5", "4.5x", "5.5", "6.5");
    private final List<List<String>> flatMapInput = Arrays.asList(input, Arrays.asList("7.5", "8.5"));
    private final List<List<String>> failingFlatMapInput = Arrays.asList(failingInput, Arrays.asList("7.5", "8.5"));

    private FailableDoubleStream inputStream;
    private FailableDoubleStream failingInputStream;
    private FailableDoubleStream flatMapInputStream;
    private FailableDoubleStream failingFlatMapInputStream;

    @BeforeEach
    void beforeEach() {
        inputStream = Functions.stream(input).mapToDouble(Double::valueOf);
        failingInputStream = Functions.stream(failingInput).mapToDouble(Double::valueOf);
        flatMapInputStream = Functions.stream(flatMapInput).flatMapToDouble(LIST_TO_DOUBLE_STREAM);
        failingFlatMapInputStream = Functions.stream(failingFlatMapInput).flatMapToDouble(LIST_TO_DOUBLE_STREAM);
    }

    @Test
    void testDoubleStreamFromMapSum() {
        assertEquals(24, inputStream.sum());
    }

    @Test
    void testDoubleStreamFromMapSumFailing() {
        try {
            failingInputStream.sum();
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals("For input string: \"4.5x\"", nfe.getMessage());
        }
    }

    @Test
    void testDoubleStreamFromFlatMapSum() {
        assertEquals(40, flatMapInputStream.sum());
    }

    @Test
    void testDoubleStreamFromFlatMapSumFailing() {
        try {
            failingFlatMapInputStream.sum();
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals("For input string: \"4.5x\"", nfe.getMessage());
        }
    }
}
