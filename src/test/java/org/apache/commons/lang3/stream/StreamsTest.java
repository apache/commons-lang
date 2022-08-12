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

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsEqual.equalTo;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.DynamicTest.dynamicTest;

import java.lang.reflect.UndeclaredThrowableException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.function.Failable;
import org.apache.commons.lang3.function.FailableConsumer;
import org.apache.commons.lang3.function.FailablePredicate;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestFactory;
import org.junit.jupiter.api.function.Executable;
import org.xml.sax.SAXException;

/**
 * Tests {@link Streams}.
 */
public class StreamsTest extends AbstractLangTest {

    protected <T extends Throwable> FailableConsumer<String, T> asIntConsumer(final T pThrowable) {
        return s -> {
            final int i = Integer.parseInt(s);
            if (i == 4) {
                throw pThrowable;
            }
        };
    }

    protected <T extends Throwable> FailablePredicate<Integer, T> asIntPredicate(final T pThrowable) {
        return i -> {
            if (i.intValue() == 5 && pThrowable != null) {
                throw pThrowable;
            }
            return i % 2 == 0;
        };
    }

    private void assertEvenNumbers(final List<Integer> output) {
        assertEquals(3, output.size());
        for (int i = 0; i < 3; i++) {
            assertEquals((i + 1) * 2, output.get(i).intValue());
        }
    }

    @TestFactory
    public Stream<DynamicTest> simpleStreamFilterFailing() {
        final List<String> input = Arrays.asList("1", "2", "3", "4", "5", "6");
        final List<Integer> output = Failable.stream(input).map(Integer::valueOf).filter(asIntPredicate(null)).collect(Collectors.toList());
        assertEvenNumbers(output);

        return Stream.of(

            dynamicTest("IllegalArgumentException", () -> {
                final IllegalArgumentException iae = new IllegalArgumentException("Invalid argument: " + 5);
                final Executable testMethod = () -> Failable.stream(input).map(Integer::valueOf).filter(asIntPredicate(iae)).collect(Collectors.toList());
                final IllegalArgumentException thrown = assertThrows(IllegalArgumentException.class, testMethod);
                assertThat(thrown.getMessage(), is(equalTo("Invalid argument: " + 5)));
            }),

            dynamicTest("OutOfMemoryError", () -> {
                final OutOfMemoryError oome = new OutOfMemoryError();
                final Executable testMethod = () -> Failable.stream(input).map(Integer::valueOf).filter(asIntPredicate(oome)).collect(Collectors.toList());
                final OutOfMemoryError thrown = assertThrows(OutOfMemoryError.class, testMethod);
                assertThat(thrown.getMessage(), is(nullValue()));
            }),

            dynamicTest("SAXException", () -> {
                final SAXException se = new SAXException();
                final Executable testMethod = () -> Failable.stream(input).map(Integer::valueOf).filter(asIntPredicate(se)).collect(Collectors.toList());
                final UndeclaredThrowableException thrown = assertThrows(UndeclaredThrowableException.class, testMethod);
                assertAll(() -> assertThat(thrown.getMessage(), is(nullValue())), () -> assertThat(thrown.getCause(), is(equalTo(se))));
            }));
    }

    @TestFactory
    public Stream<DynamicTest> simpleStreamForEachFailing() {
        final List<String> input = Arrays.asList("1", "2", "3", "4", "5", "6");

        return Stream.of(

            dynamicTest("IllegalArgumentException", () -> {
                final IllegalArgumentException ise = new IllegalArgumentException();
                final Executable testMethod = () -> Failable.stream(input).forEach(asIntConsumer(ise));
                final IllegalArgumentException thrown = assertThrows(IllegalArgumentException.class, testMethod);
                assertThat(thrown.getMessage(), is(nullValue()));
            }),

            dynamicTest("OutOfMemoryError", () -> {
                final OutOfMemoryError oome = new OutOfMemoryError();
                final Executable oomeTestMethod = () -> Failable.stream(input).forEach(asIntConsumer(oome));
                final OutOfMemoryError oomeThrown = assertThrows(OutOfMemoryError.class, oomeTestMethod);
                assertThat(oomeThrown.getMessage(), is(nullValue()));
            }),

            dynamicTest("SAXException", () -> {
                final SAXException se = new SAXException();
                final Executable seTestMethod = () -> Failable.stream(input).forEach(asIntConsumer(se));
                final UndeclaredThrowableException seThrown = assertThrows(UndeclaredThrowableException.class, seTestMethod);
                assertAll(() -> assertThat(seThrown.getMessage(), is(nullValue())), () -> assertThat(seThrown.getCause(), is(equalTo(se))));
            }));
    }

    @Test
    public void testInstanceOfStream() {
        assertEquals(2, Streams.instancesOf(String.class, Arrays.asList("A", "B")).collect(Collectors.toList()).size());
        assertEquals(2, Streams.instancesOf(String.class, Arrays.asList(null, "A", null, "B", null)).collect(Collectors.toList()).size());
        assertEquals(0, Streams.instancesOf(String.class, Arrays.asList(null, null)).collect(Collectors.toList()).size());
        //
        final List<Object> objects = Arrays.asList("A", "B");
        assertEquals(2, Streams.instancesOf(String.class, objects).collect(Collectors.toList()).size());
    }

    @Test
    public void testNullSafeStreamNotNull() {
        assertEquals(2, Streams.nonNull(Arrays.asList("A", "B")).collect(Collectors.toList()).size());
        assertEquals(2, Streams.nonNull(Arrays.asList(null, "A", null, "B", null)).collect(Collectors.toList()).size());
        assertEquals(0, Streams.nonNull(Arrays.asList(null, null)).collect(Collectors.toList()).size());
    }

    @Test
    public void testNullSafeStreamNull() {
        final List<String> input = null;
        assertEquals(0, Streams.nonNull(input).collect(Collectors.toList()).size());
    }

    @Test
    public void testOfArray() {
        assertEquals(0, Streams.of((Object[]) null).count());
        assertEquals(1, Streams.of("foo").count());
        assertEquals(2, Streams.of("foo", "bar").count());
    }

    @Test
    public void testOfCollectionNotNull() {
        assertEquals(2, Streams.of(Arrays.asList("A", "B")).collect(Collectors.toList()).size());
    }

    @Test
    public void testOfCollectionNull() {
        final List<String> input = null;
        assertEquals(0, Streams.of(input).collect(Collectors.toList()).size());
    }

    @Test
    public void testOfEnumeration() {
        final Hashtable<String, Integer> table = new Hashtable<>();
        assertEquals(0, Streams.of(table.elements()).count());
        table.put("One", 1);
        assertEquals(1, Streams.of(table.elements()).count());
        table.put("Two", 2);
        assertEquals(2, Streams.of(table.elements()).count());
        final List<String> collect = Streams.of(table.keys()).collect(Collectors.toList());
        assertTrue(collect.contains("One"));
        assertTrue(collect.contains("Two"));
        assertEquals(2, collect.size());
    }

    @Test
    public void testOfIterableNotNull() {
        assertEquals(2, Streams.of((Iterable<String>) Arrays.asList("A", "B")).collect(Collectors.toList()).size());
    }

    @Test
    public void testOfIterableNull() {
        final Iterable<String> input = null;
        assertEquals(0, Streams.of(input).collect(Collectors.toList()).size());
    }

    @Test
    public void testOfIteratorNotNull() {
        assertEquals(2, Streams.of(Arrays.asList("A", "B").iterator()).collect(Collectors.toList()).size());
    }

    @Test
    public void testOfIteratorNull() {
        final Iterator<String> input = null;
        assertEquals(0, Streams.of(input).collect(Collectors.toList()).size());
    }

    @Test
    public void testSimpleStreamFilter() {
        final List<String> input = Arrays.asList("1", "2", "3", "4", "5", "6");
        final List<Integer> output = Failable.stream(input).map(Integer::valueOf).filter(i -> (i.intValue() % 2 == 0)).collect(Collectors.toList());
        assertEvenNumbers(output);
    }

    @Test
    public void testSimpleStreamForEach() {
        final List<String> input = Arrays.asList("1", "2", "3", "4", "5", "6");
        final List<Integer> output = new ArrayList<>();
        Failable.stream(input).forEach(s -> output.add(Integer.valueOf(s)));
        assertEquals(6, output.size());
        for (int i = 0; i < 6; i++) {
            assertEquals(i + 1, output.get(i).intValue());
        }
    }

    @Test
    public void testSimpleStreamMap() {
        final List<String> input = Arrays.asList("1", "2", "3", "4", "5", "6");
        final List<Integer> output = Failable.stream(input).map(Integer::valueOf).collect(Collectors.toList());
        assertEquals(6, output.size());
        for (int i = 0; i < 6; i++) {
            assertEquals(i + 1, output.get(i).intValue());
        }
    }

    @Test
    public void testSimpleStreamMapFailing() {
        final List<String> input = Arrays.asList("1", "2", "3", "4 ", "5", "6");
        final Executable testMethod = () -> Failable.stream(input).map(Integer::valueOf).collect(Collectors.toList());
        final NumberFormatException thrown = assertThrows(NumberFormatException.class, testMethod);
        assertEquals("For input string: \"4 \"", thrown.getMessage());
    }

    @Test
    public void testStreamCollection() {
        final List<String> input = Arrays.asList("1", "2", "3", "4", "5", "6");
        assertEquals(6, Streams.stream(input).collect(Collectors.toList()).size());
    }

    @Test
    public void testStreamCollectionNull() {
        final List<String> input = null;
        assertEquals(0, Streams.stream(input).collect(Collectors.toList()).size());
    }

    @Test
    public void testToArray() {
        final String[] array = Arrays.asList("2", "3", "1").stream().collect(Streams.toArray(String.class));
        assertNotNull(array);
        assertEquals(3, array.length);
        assertEquals("2", array[0]);
        assertEquals("3", array[1]);
        assertEquals("1", array[2]);
    }

}
