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

import static org.apache.commons.lang3.stream.TestStringConstants.EXPECTED_EXCEPTION;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

public class FailableLongStreamTest {
    private List<String> input;
    private List<String> failingInput;
    private List<List<String>> flatMapInput;
    private List<List<String>> failingFlatMapInput;

    @BeforeEach
    void beforeEach() {
        input = Arrays.asList("1000000000", "2000000000", "3000000000", "4000000000", "5000000000", "6000000000");
        failingInput = Arrays.asList("1000000000", "2000000000", "3000000000", "4000000000 ", "5000000000", "6000000000");
        flatMapInput = Arrays.asList(input, Arrays.asList("7000000000", "8000000000"));
        failingFlatMapInput = Arrays.asList(failingInput, Arrays.asList("7000000000", "8000000000"));
    }

    @Test
    void testLongStreamFromMapSum() {
        assertEquals(21000000000L, Functions.stream(input).mapToLong(Long::valueOf).sum());
    }

    @Test
    void testLongStreamFromMapSumFailing() {
        try {
            Functions.stream(failingInput).mapToLong(Long::valueOf).sum();
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals("For input string: \"4000000000 \"", nfe.getMessage());
        }
    }

    @Test
    void testLongStreamFromFlatMapSum() {
        assertEquals(36000000000L, Functions.stream(flatMapInput).flatMapToLong(list -> list.stream().mapToLong(Long::valueOf)).sum());
    }

    @Test
    void testLongStreamFromFlatMapSumFailing() {
        try {
            Functions.stream(failingFlatMapInput).flatMapToLong(list -> list.stream().mapToLong(Long::valueOf)).sum();
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals("For input string: \"4000000000 \"", nfe.getMessage());
        }
    }
}
