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
import org.apache.commons.lang3.Streams;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.xml.sax.SAXException;

import java.lang.reflect.UndeclaredThrowableException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.apache.commons.lang3.stream.TestStringConstants.EXPECTED_EXCEPTION;
import static org.junit.jupiter.api.Assertions.*;

public class FailableStreamTest {
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
    void testSimpleStreamMap() {
        final List<Integer> output = Functions.stream(input).map(Integer::valueOf).collect(Collectors.toList());
        assertEquals(6, output.size());
        for (int i = 0;  i < 6;  i++) {
            assertEquals(i+1, output.get(i).intValue());
        }
    }

    @Test
    void testSimpleStreamMapFailing() {
        try {
            Functions.stream(failingInput).map(Integer::valueOf).collect(Collectors.toList());
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals("For input string: \"4 \"", nfe.getMessage());
        }
    }

    @Test
    void testSimpleStreamFlatMap() {
        final List<Integer> output = Functions.stream(flatMapInput).flatMap(Collection::stream).map(Integer::valueOf)
                .collect(Collectors.toList());
        assertEquals(8, output.size());
        for (int i = 0;  i < 8;  i++) {
            assertEquals(i+1, output.get(i).intValue());
        }
    }

    @Test
    void testSimpleStreamFlatMapFailing() {
        try {
            Functions.stream(failingFlatMapInput).flatMap(Collection::stream).map(Integer::valueOf).collect(Collectors.toList());
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals("For input string: \"4 \"", nfe.getMessage());
        }
    }

    @Test
    void testSimpleStreamForEach() {
        final List<Integer> output = new ArrayList<>();
        Functions.stream(input).forEach(s -> output.add(Integer.valueOf(s)));
        assertEquals(6, output.size());
        for (int i = 0;  i < 6;  i++) {
            assertEquals(i+1, output.get(i).intValue());
        }
    }

    @Test
    void testToArray() {
        final String[] array = Stream.of("2", "3", "1").collect(Streams.toArray(String.class));
        assertNotNull(array);
        assertEquals(3, array.length);
        assertEquals("2", array[0]);
        assertEquals("3", array[1]);
        assertEquals("1", array[2]);
    }

    protected <T extends Throwable> Functions.FailableConsumer<String, T> asIntConsumer(final T pThrowable) {
        return s -> {
            final int i = Integer.parseInt(s);
            if (i == 4) {
                throw pThrowable;
            }
        };
    }

    @SuppressWarnings("java:S1181")
    @Test
    void testSimpleStreamForEachFailing() {
        final IllegalArgumentException ise = new IllegalArgumentException("Invalid argument: 4");
        try {
            Functions.stream(input).forEach(asIntConsumer(ise));
            fail(EXPECTED_EXCEPTION);
        } catch (final IllegalArgumentException e) {
            assertSame(ise, e);
        }
        final OutOfMemoryError oome = new OutOfMemoryError();
        try {
            Functions.stream(input).forEach(asIntConsumer(oome));
            fail(EXPECTED_EXCEPTION);
        } catch (final Throwable t) {
            assertSame(oome, t);
        }
        final SAXException se = new SAXException();
        try {
            Functions.stream(input).forEach(asIntConsumer(se));
            fail(EXPECTED_EXCEPTION);
        } catch (final UndeclaredThrowableException ute) {
            assertSame(se, ute.getCause());
        }
    }

    @Test
    void testSimpleStreamFilter() {
        final List<Integer> output = Functions.stream(input)
                .map(Integer::valueOf)
                .filter(i -> i % 2 == 0)
                .collect(Collectors.toList());
        assertEvenNumbers(output);
    }

    private void assertEvenNumbers(final List<Integer> output) {
        assertEquals(3, output.size());
        for (int i = 0;  i < 3;  i++) {
            assertEquals((i+1)*2, output.get(i).intValue());
        }
    }

    protected <T extends Throwable> Functions.FailablePredicate<Integer, T> asIntPredicate(final T pThrowable) {
        return i -> {
            if (i == 5 && pThrowable != null) {
                throw pThrowable;
            }
            return i%2==0;
        };
    }

    @SuppressWarnings("java:S1181")
    @Test
    void testSimpleStreamFilterFailing() {
        final List<Integer> output = Functions.stream(input)
                .map(Integer::valueOf)
                .filter(asIntPredicate(null))
                .collect(Collectors.toList());
        assertEvenNumbers(output);

        output.clear();
        final IllegalArgumentException iae = new IllegalArgumentException("Invalid argument: " + 5);
        try {
            Functions.stream(input)
                    .map(Integer::valueOf)
                    .filter(asIntPredicate(iae))
                    .collect(Collectors.toList());
            fail(EXPECTED_EXCEPTION);
        } catch (final IllegalArgumentException e) {
            assertSame(iae, e);
        }

        output.clear();
        final OutOfMemoryError oome = new OutOfMemoryError();
        try {
            Functions.stream(input)
                    .map(Integer::valueOf)
                    .filter(asIntPredicate(oome))
                    .collect(Collectors.toList());
            fail(EXPECTED_EXCEPTION);
        } catch (final Throwable t) {
            assertSame(oome, t);
        }

        output.clear();
        final SAXException se = new SAXException();
        try {
            Functions.stream(input)
                    .map(Integer::valueOf)
                    .filter(asIntPredicate(se))
                    .collect(Collectors.toList());
            fail(EXPECTED_EXCEPTION);
        } catch (final UndeclaredThrowableException t) {
            assertSame(se, t.getCause());
        }
    }
}
