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
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.apache.commons.lang3.stream.TestConstants.EXPECTED_EXCEPTION;
import static org.apache.commons.lang3.stream.TestConstants.EXPECTED_NFE_MESSAGE_INT;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

public class FailableStreamTest {
    private static final Functions.FailableFunction<List<String>, Stream<Integer>, ?> LIST_TO_STREAM =
            list -> list.stream().map(Integer::valueOf);

    private FailableStream<Integer> inputStream;
    private FailableStream<Integer> failingInputStream;
    private FailableStream<Integer> flatMapInputStream;
    private FailableStream<Integer> failingFlatMapInputStream;

    @BeforeEach
    void beforeEach() {
        inputStream = Functions.failableStream(TestConstants.INPUT_INT).map(Integer::valueOf);
        failingInputStream = Functions.failableStream(TestConstants.FAILING_INPUT_INT).map(Integer::valueOf);
        flatMapInputStream = Functions.failableStream(TestConstants.FLAT_MAP_INPUT_INT)
                .flatMap(LIST_TO_STREAM);
        failingFlatMapInputStream = Functions.failableStream(TestConstants.FAILING_FLAT_MAP_INPUT_INT)
                .flatMap(LIST_TO_STREAM);
    }

    @Test
    void testSimpleStreamFromMapCount() {
        assertEquals(6L, inputStream.count());
    }

    @Test
    void testSimpleStreamFromFlatMapCount() {
        assertEquals(8L, flatMapInputStream.count());
    }

    @Test
    void testSimpleStreamFromMapLimit() {
        assertEquals(3L, inputStream.limit(3).count());
    }

    @Test
    void testSimpleStreamFromFlatMapLimit() {
        assertEquals(3L, flatMapInputStream.limit(3).count());
    }

    @Test
    void testSimpleStreamMap() {
        final List<Integer> output = inputStream.collect(Collectors.toList());
        assertEquals(6, output.size());
        for (int i = 0;  i < 6;  i++) {
            assertEquals(i+1, output.get(i).intValue());
        }
    }

    @Test
    void testSimpleStreamMapFailing() {
        try {
            failingInputStream.collect(Collectors.toList());
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_INT, nfe.getMessage());
        }
    }

    @Test
    void testSimpleStreamFlatMap() {
        final List<Integer> output = flatMapInputStream.collect(Collectors.toList());
        assertEquals(8, output.size());
        for (int i = 0;  i < 8;  i++) {
            assertEquals(i+1, output.get(i).intValue());
        }
    }

    @Test
    void testSimpleStreamFlatMapFailing() {
        try {
            failingFlatMapInputStream.collect(Collectors.toList());
            fail(EXPECTED_EXCEPTION);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_INT, nfe.getMessage());
        }
    }

    @Test
    void testSimpleStreamForEach() {
        final List<Integer> output = new ArrayList<>();
        Functions.failableStream(TestConstants.INPUT_INT).forEach(s -> output.add(Integer.valueOf(s)));
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
            Functions.failableStream(TestConstants.INPUT_INT).forEach(asIntConsumer(ise));
            fail(EXPECTED_EXCEPTION);
        } catch (final IllegalArgumentException e) {
            assertSame(ise, e);
        }
        final OutOfMemoryError oome = new OutOfMemoryError();
        try {
            Functions.failableStream(TestConstants.INPUT_INT).forEach(asIntConsumer(oome));
            fail(EXPECTED_EXCEPTION);
        } catch (final Throwable t) {
            assertSame(oome, t);
        }
        final SAXException se = new SAXException();
        try {
            Functions.failableStream(TestConstants.INPUT_INT).forEach(asIntConsumer(se));
            fail(EXPECTED_EXCEPTION);
        } catch (final UndeclaredThrowableException ute) {
            assertSame(se, ute.getCause());
        }
    }

    @Test
    void testSimpleStreamFilter() {
        final List<Integer> output = inputStream
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
            return i % 2==0;
        };
    }

    @SuppressWarnings("java:S1181")
    @Test
    void testSimpleStreamFilterFailing() {
        final List<Integer> output = inputStream
                .filter(asIntPredicate(null))
                .collect(Collectors.toList());
        assertEvenNumbers(output);

        output.clear();

        inputStream = Functions.failableStream(TestConstants.INPUT_INT).map(Integer::valueOf);
        final IllegalArgumentException iae = new IllegalArgumentException("Invalid argument: " + 5);
        try {
            inputStream.filter(asIntPredicate(iae)).collect(Collectors.toList());
            fail(EXPECTED_EXCEPTION);
        } catch (final IllegalArgumentException e) {
            assertSame(iae, e);
        }

        output.clear();

        inputStream = Functions.failableStream(TestConstants.INPUT_INT).map(Integer::valueOf);
        final OutOfMemoryError oome = new OutOfMemoryError();
        try {
            inputStream.filter(asIntPredicate(oome)).collect(Collectors.toList());
            fail(EXPECTED_EXCEPTION);
        } catch (final Throwable t) {
            assertSame(oome, t);
        }

        output.clear();

        inputStream = Functions.failableStream(TestConstants.INPUT_INT).map(Integer::valueOf);
        final SAXException se = new SAXException();
        try {
            inputStream.filter(asIntPredicate(se)).collect(Collectors.toList());
            fail(EXPECTED_EXCEPTION);
        } catch (final UndeclaredThrowableException t) {
            assertSame(se, t.getCause());
        }
    }

    @Test
    void testSimpleStreamAllMatch() {
        assertTrue(inputStream.allMatch(Objects::nonNull));
    }

    @Test
    void testSimpleStreamAllMatchFailing() {
        try {
            failingInputStream.allMatch(Objects::nonNull);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_INT, nfe.getMessage());
        }
    }

    @Test
    void testSimpleStreamAnyMatch() {
        assertTrue(Functions.failableStream(TestConstants.INPUT_INT).map(Integer::valueOf).anyMatch(i -> i % 2 == 0));
    }

    @Test
    void testSimpleStreamAnyMatchFailing() {
        try {
            failingInputStream.anyMatch(i -> i % 2 == 0);
        } catch (final NumberFormatException nfe) {
            assertEquals(EXPECTED_NFE_MESSAGE_INT, nfe.getMessage());
        }
    }
}
