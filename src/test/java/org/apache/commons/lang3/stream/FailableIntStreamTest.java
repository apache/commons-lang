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

public class FailableIntStreamTest {
    private List<String> input;
    private List<String> failingInput;
    private List<List<String>> flatMapInput;
    private List<List<String>> failingFlatMapInput;

    @BeforeEach
    void beforeEach() {
        input = Arrays.asList("1", "2", "3", "4", "5", "6");
        failingInput = Arrays.asList("1", "2", "3", "4 ", "5", "6");
        flatMapInput = Arrays.asList(input, Arrays.asList("7", "8"));
        failingFlatMapInput = Arrays.asList(failingInput, Arrays.asList("7", "8"));
    }

    @Test
    void testIntStreamFromMapSum() {
        assertEquals(21, Functions.stream(input).mapToInt(Integer::valueOf).sum());
    }

    @Test
    void testIntStreamFromMapSumFailing() {
        try {
            Functions.stream(failingInput).mapToInt(Integer::valueOf).sum();
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals("For input string: \"4 \"", nfe.getMessage());
        }
    }

    @Test
    void testIntStreamFromFlatMapSum() {
        assertEquals(36, Functions.stream(flatMapInput).flatMapToInt(list -> list.stream().mapToInt(Integer::valueOf)).sum());
    }

    @Test
    void testIntStreamFromFlatMapSumFailing() {
        try {
            Functions.stream(failingFlatMapInput).flatMapToInt(list -> list.stream().mapToInt(Integer::valueOf)).sum();
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals("For input string: \"4 \"", nfe.getMessage());
        }
    }
}
