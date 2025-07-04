/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang3;

import static org.apache.commons.lang3.LangAssertions.assertIllegalArgumentException;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.DynamicTest.dynamicTest;

import java.lang.reflect.UndeclaredThrowableException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang3.Functions.FailableConsumer;
import org.apache.commons.lang3.Functions.FailablePredicate;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestFactory;
import org.junit.jupiter.api.function.Executable;
import org.xml.sax.SAXException;

/**
 * Tests Streams.
 *
 * @deprecated this test can be removed once the deprecated source class {@link org.apache.commons.lang3.Streams} is removed.
 */
@Deprecated
class StreamsTest extends AbstractLangTest {

    protected <T extends Throwable> FailableConsumer<String, T> asIntConsumer(final T throwable) {
        return s -> {
            final int i = Integer.parseInt(s);
            if (i == 4) {
                throw throwable;
            }
        };
    }

    protected <T extends Throwable> FailablePredicate<Integer, T> asIntPredicate(final T phrowable) {
        return i -> {
            if (i.intValue() == 5 && phrowable != null) {
                throw phrowable;
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
        final List<Integer> output = Functions.stream(input)
                .map(Integer::valueOf)
                .filter(asIntPredicate(null))
                .collect(Collectors.toList());
        assertEvenNumbers(output);
        return Stream.of(
                dynamicTest("IllegalArgumentException", () -> {
                    final IllegalArgumentException iae = new IllegalArgumentException("Invalid argument: " + 5);
                    final Executable testMethod = () -> Functions.stream(input)
                            .map(Integer::valueOf)
                            .filter(asIntPredicate(iae))
                            .collect(Collectors.toList());
                    final IllegalArgumentException thrown = assertIllegalArgumentException(testMethod);
                    assertEquals("Invalid argument: " + 5, thrown.getMessage());
                }),
                dynamicTest("OutOfMemoryError", () -> {
                    final OutOfMemoryError oome = new OutOfMemoryError();
                    final Executable testMethod = () -> Functions.stream(input)
                            .map(Integer::valueOf)
                            .filter(asIntPredicate(oome))
                            .collect(Collectors.toList());
                    final OutOfMemoryError thrown = assertThrows(OutOfMemoryError.class, testMethod);
                    assertNull(thrown.getMessage());
                }),
                dynamicTest("SAXException", () -> {
                    final SAXException se = new SAXException();
                    final Executable testMethod = () -> Functions.stream(input)
                            .map(Integer::valueOf)
                            .filter(asIntPredicate(se))
                            .collect(Collectors.toList());
                    final UndeclaredThrowableException thrown = assertThrows(UndeclaredThrowableException.class, testMethod);
                    assertNull(thrown.getMessage());
                    assertEquals(se, thrown.getCause());
                })
        );
    }

    @TestFactory
    public Stream<DynamicTest> simpleStreamForEachFailing() {
        final List<String> input = Arrays.asList("1", "2", "3", "4", "5", "6");
        return Stream.of(
                dynamicTest("IllegalArgumentException", () -> {
                    final IllegalArgumentException ise = new IllegalArgumentException();
                    final Executable testMethod = () -> Functions.stream(input).forEach(asIntConsumer(ise));
                    final IllegalArgumentException thrown = assertIllegalArgumentException(testMethod);
                    assertNull(thrown.getMessage());
                }),
                dynamicTest("OutOfMemoryError", () -> {
                    final OutOfMemoryError oome = new OutOfMemoryError();
                    final Executable oomeTestMethod = () -> Functions.stream(input).forEach(asIntConsumer(oome));
                    final OutOfMemoryError oomeThrown = assertThrows(OutOfMemoryError.class, oomeTestMethod);
                    assertNull(oomeThrown.getMessage());
                }),
                dynamicTest("SAXException", () -> {
                    final SAXException se = new SAXException();
                    final Executable seTestMethod = () -> Functions.stream(input).forEach(asIntConsumer(se));
                    final UndeclaredThrowableException seThrown = assertThrows(UndeclaredThrowableException.class, seTestMethod);
                    assertNull(seThrown.getMessage());
                    assertEquals(se, seThrown.getCause());
                })
        );
    }

    @Test
    void testSimpleStreamFilter() {
        final List<String> input = Arrays.asList("1", "2", "3", "4", "5", "6");
        final List<Integer> output = Functions.stream(input)
                .map(Integer::valueOf)
                .filter(i -> i.intValue() % 2 == 0)
                .collect(Collectors.toList());
        assertEvenNumbers(output);
    }

    @Test
    void testSimpleStreamForEach() {
        final List<String> input = Arrays.asList("1", "2", "3", "4", "5", "6");
        final List<Integer> output = new ArrayList<>();
        Functions.stream(input).forEach(s -> output.add(Integer.valueOf(s)));
        assertEquals(6, output.size());
        for (int i = 0; i < 6; i++) {
            assertEquals(i + 1, output.get(i).intValue());
        }
    }

    @Test
    void testSimpleStreamMap() {
        final List<String> input = Arrays.asList("1", "2", "3", "4", "5", "6");
        final List<Integer> output = Functions.stream(input).map(Integer::valueOf).collect(Collectors.toList());
        assertEquals(6, output.size());
        for (int i = 0; i < 6; i++) {
            assertEquals(i + 1, output.get(i).intValue());
        }
    }

    @Test
    void testSimpleStreamMapFailing() {
        final List<String> input = Arrays.asList("1", "2", "3", "4 ", "5", "6");
        final Executable testMethod = () -> Functions.stream(input).map(Integer::valueOf).collect(Collectors.toList());
        final NumberFormatException thrown = assertThrows(NumberFormatException.class, testMethod);
        assertEquals("For input string: \"4 \"", thrown.getMessage());
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

}
