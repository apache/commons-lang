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
package org.apache.commons.lang3;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.fail;

import java.lang.reflect.UndeclaredThrowableException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.lang3.Functions.FailableConsumer;
import org.apache.commons.lang3.Functions.FailablePredicate;
import org.junit.jupiter.api.Test;
import org.xml.sax.SAXException;

class StreamsTest {
    @Test
    void testSimpleStreamMap() {
        final List<String> input = Arrays.asList("1", "2", "3", "4", "5", "6");
        final List<Integer> output = Functions.stream(input).map((s) -> Integer.valueOf(s)).collect(Collectors.toList());
        assertEquals(6, output.size());
        for (int i = 0;  i < 6;  i++) {
            assertEquals(i+1, output.get(i).intValue());
        }
    }

    @Test
    void testSimpleStreamMapFailing() {
        final List<String> input = Arrays.asList("1", "2", "3", "4 ", "5", "6");
        try {
            Functions.stream(input).map((s) -> Integer.valueOf(s)).collect(Collectors.toList());
            fail("Expected Exception");
        } catch (final NumberFormatException nfe) {
            assertEquals("For input string: \"4 \"", nfe.getMessage());
        }
    }

    @Test
    void testSimpleStreamForEach() {
        final List<String> input = Arrays.asList("1", "2", "3", "4", "5", "6");
        final List<Integer> output = new ArrayList<>();
        Functions.stream(input).forEach((s) -> output.add(Integer.valueOf(s)));
        assertEquals(6, output.size());
        for (int i = 0;  i < 6;  i++) {
            assertEquals(i+1, output.get(i).intValue());
        }
    }

    @Test
    void testToArray() {
        final String[] array = Arrays.asList("2", "3", "1").stream().collect(Streams.toArray(String.class));
        assertNotNull(array);
        assertEquals(3, array.length);
        assertEquals("2", array[0]);
        assertEquals("3", array[1]);
        assertEquals("1", array[2]);
    }

    protected <T extends Throwable> FailableConsumer<String, T> asIntConsumer(final T pThrowable) {
        return (s) -> {
            final Integer i = Integer.valueOf(s);
            if (i.intValue() == 4) {
                throw pThrowable;
            }
        };
    }

    @Test
    void testSimpleStreamForEachFailing() {
        final List<String> input = Arrays.asList("1", "2", "3", "4", "5", "6");
        final List<Integer> output = new ArrayList<>();
        final IllegalArgumentException ise = new IllegalArgumentException("Invalid argument: 4");
        try {
            Functions.stream(input).forEach(asIntConsumer(ise));
            fail("Expected Exception");
        } catch (final IllegalArgumentException e) {
            assertSame(ise, e);
        }
        output.clear();
        final OutOfMemoryError oome = new OutOfMemoryError();
        try {
            Functions.stream(input).forEach(asIntConsumer(oome));
            fail("Expected Exception");
        } catch (final Throwable t) {
            assertSame(oome, t);
        }
        output.clear();
        final SAXException se = new SAXException();
        try {
            Functions.stream(input).forEach(asIntConsumer(se));
            fail("Expected Exception");
        } catch (final UndeclaredThrowableException ute) {
            assertSame(se, ute.getCause());
        }
    }

    @Test
    void testSimpleStreamFilter() {
        final List<String> input = Arrays.asList("1", "2", "3", "4", "5", "6");
        final List<Integer> output = Functions.stream(input)
                .map((s) -> Integer.valueOf(s))
                .filter((i) -> {
                    return i.intValue() %2 == 0;
                })
                .collect(Collectors.toList());
        assertEvenNumbers(output);
    }

    private void assertEvenNumbers(final List<Integer> output) {
        assertEquals(3, output.size());
        for (int i = 0;  i < 3;  i++) {
            assertEquals((i+1)*2, output.get(i).intValue());
        }
    }

    protected <T extends Throwable> FailablePredicate<Integer, T> asIntPredicate(final T pThrowable) {
        return (i) -> {
            if (i.intValue() == 5) {
                if (pThrowable != null) {
                    throw pThrowable;
                }
            }
            return i%2==0;
        };
    }

    @Test
    void testSimpleStreamFilterFailing() {
        final List<String> input = Arrays.asList("1", "2", "3", "4", "5", "6");
        final List<Integer> output = Functions.stream(input)
                .map((s) -> Integer.valueOf(s))
                .filter(asIntPredicate(null))
                .collect(Collectors.toList());
        assertEvenNumbers(output);

        output.clear();
        final IllegalArgumentException iae = new IllegalArgumentException("Invalid argument: " + 5);
        try {
            Functions.stream(input)
                    .map((s) -> Integer.valueOf(s))
                    .filter(asIntPredicate(iae))
                    .collect(Collectors.toList());
            fail("Expected Exception");
        } catch (final IllegalArgumentException e) {
            assertSame(iae, e);
        }

        output.clear();
        final OutOfMemoryError oome = new OutOfMemoryError();
        try {
            Functions.stream(input)
                    .map((s) -> Integer.valueOf(s))
                    .filter(asIntPredicate(oome))
                    .collect(Collectors.toList());
            fail("Expected Exception");
        } catch (final Throwable t) {
            assertSame(oome, t);
        }

        output.clear();
        final SAXException se = new SAXException();
        try {
            Functions.stream(input)
                    .map((s) -> Integer.valueOf(s))
                    .filter(asIntPredicate(se))
                    .collect(Collectors.toList());
            fail("Expected Exception");
        } catch (final UndeclaredThrowableException t) {
            assertSame(se, t.getCause());
        }
    }
}
