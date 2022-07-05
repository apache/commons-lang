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

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.params.provider.Arguments.arguments;

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.util.Random;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Tests CharSequenceUtils
 */
public class CharSequenceUtilsTest extends AbstractLangTest {

    @Test
    public void testConstructor() {
        assertNotNull(new CharSequenceUtils());
        final Constructor<?>[] cons = CharSequenceUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertTrue(Modifier.isPublic(cons[0].getModifiers()));
        assertTrue(Modifier.isPublic(CharSequenceUtils.class.getModifiers()));
        assertFalse(Modifier.isFinal(CharSequenceUtils.class.getModifiers()));
    }

    @Test
    public void testSubSequence() {
        //
        // null input
        //
        assertNull(CharSequenceUtils.subSequence(null, -1));
        assertNull(CharSequenceUtils.subSequence(null, 0));
        assertNull(CharSequenceUtils.subSequence(null, 1));
        //
        // non-null input
        //
        assertEquals(StringUtils.EMPTY, CharSequenceUtils.subSequence(StringUtils.EMPTY, 0));
        assertEquals("012", CharSequenceUtils.subSequence("012", 0));
        assertEquals("12", CharSequenceUtils.subSequence("012", 1));
        assertEquals("2", CharSequenceUtils.subSequence("012", 2));
        assertEquals(StringUtils.EMPTY, CharSequenceUtils.subSequence("012", 3));
    }

    @Test
    public void testSubSequenceNegativeStart() {
        assertThrows(IndexOutOfBoundsException.class, () -> CharSequenceUtils.subSequence(StringUtils.EMPTY, -1));
    }

    @Test
    public void testSubSequenceTooLong() {
        assertThrows(IndexOutOfBoundsException.class, () -> CharSequenceUtils.subSequence(StringUtils.EMPTY, 1));
    }

    static class TestData{
        final String source;
        final boolean ignoreCase;
        final int toffset;
        final String other;
        final int ooffset;
        final int len;
        final boolean expected;
        final Class<? extends Throwable> throwable;
        TestData(final String source, final boolean ignoreCase, final int toffset,
                final String other, final int ooffset, final int len, final boolean expected) {
            this.source = source;
            this.ignoreCase = ignoreCase;
            this.toffset = toffset;
            this.other = other;
            this.ooffset = ooffset;
            this.len = len;
            this.expected = expected;
            this.throwable = null;
        }
        TestData(final String source, final boolean ignoreCase, final int toffset,
                final String other, final int ooffset, final int len, final Class<? extends Throwable> throwable) {
            this.source = source;
            this.ignoreCase = ignoreCase;
            this.toffset = toffset;
            this.other = other;
            this.ooffset = ooffset;
            this.len = len;
            this.expected = false;
            this.throwable = throwable;
        }
        @Override
        public String toString() {
            final StringBuilder sb = new StringBuilder();
            sb.append(source).append("[").append(toffset).append("]");
            sb.append(ignoreCase? " caseblind ":" samecase ");
            sb.append(other).append("[").append(ooffset).append("]");
            sb.append(" ").append(len).append(" => ");
            if (throwable != null) {
                sb.append(throwable);
            } else {
                sb.append(expected);
            }
            return sb.toString();
        }
    }

    private static final TestData[] TEST_DATA = {
            //          Source  IgnoreCase Offset Other  Offset Length Result
            new TestData("",    true,      -1,    "",    -1,    -1,    false),
            new TestData("",    true,      0,     "",    0,     1,     false),
            new TestData("a",   true,      0,     "abc", 0,     0,     true),
            new TestData("a",   true,      0,     "abc", 0,     1,     true),
            new TestData("a",   true,      0,     null,  0,     0,     NullPointerException.class),
            new TestData(null,  true,      0,     null,  0,     0,     NullPointerException.class),
            new TestData(null,  true,      0,     "",    0,     0,     NullPointerException.class),
            new TestData("Abc", true,      0,     "abc", 0,     3,     true),
            new TestData("Abc", false,     0,     "abc", 0,     3,     false),
            new TestData("Abc", true,      1,     "abc", 1,     2,     true),
            new TestData("Abc", false,     1,     "abc", 1,     2,     true),
            new TestData("Abcd", true,      1,     "abcD", 1,     2,     true),
            new TestData("Abcd", false,     1,     "abcD", 1,     2,     true),
    };

    private abstract static class RunTest {

        abstract boolean invoke();

        void run(final TestData data, final String id) {
            if (data.throwable != null) {
                assertThrows(data.throwable, this::invoke, id + " Expected " + data.throwable);
            } else {
                final boolean stringCheck = invoke();
                assertEquals(data.expected, stringCheck, id + " Failed test " + data);
            }
        }

    }

    @Test
    public void testRegionMatches() {
        for (final TestData data : TEST_DATA) {
            new RunTest() {
                @Override
                boolean invoke() {
                    return data.source.regionMatches(data.ignoreCase, data.toffset, data.other, data.ooffset, data.len);
                }
            }.run(data, "String");
            new RunTest() {
                @Override
                boolean invoke() {
                    return CharSequenceUtils.regionMatches(data.source, data.ignoreCase, data.toffset, data.other, data.ooffset, data.len);
                }
            }.run(data, "CSString");
            new RunTest() {
                @Override
                boolean invoke() {
                    return CharSequenceUtils.regionMatches(new StringBuilder(data.source), data.ignoreCase, data.toffset, data.other, data.ooffset, data.len);
                }
            }.run(data, "CSNonString");
        }
    }


    @Test
    public void testToCharArray() {
        final StringBuilder builder = new StringBuilder("abcdefg");
        final char[] expected = builder.toString().toCharArray();
        assertArrayEquals(expected, CharSequenceUtils.toCharArray(builder));
        assertArrayEquals(expected, CharSequenceUtils.toCharArray(builder.toString()));
        assertArrayEquals(ArrayUtils.EMPTY_CHAR_ARRAY, CharSequenceUtils.toCharArray(null));
    }

    static class WrapperString implements CharSequence {
        private final CharSequence inner;

        WrapperString(final CharSequence inner) {
            this.inner = inner;
        }

        @Override
        public int length() {
            return inner.length();
        }

        @Override
        public char charAt(final int index) {
            return inner.charAt(index);
        }

        @Override
        public CharSequence subSequence(final int start, final int end) {
            return inner.subSequence(start, end);
        }

        @Override
        public String toString() {
            return inner.toString();
        }

        @Override
        public IntStream chars() {
            return inner.chars();
        }

        @Override
        public IntStream codePoints() {
            return inner.codePoints();
        }
    }

    @Test
    public void testNewLastIndexOf() {
        testNewLastIndexOfSingle("808087847-1321060740-635567660180086727-925755305", "-1321060740-635567660", 21);
        testNewLastIndexOfSingle("", "");
        testNewLastIndexOfSingle("1", "");
        testNewLastIndexOfSingle("", "1");
        testNewLastIndexOfSingle("1", "1");
        testNewLastIndexOfSingle("11", "1");
        testNewLastIndexOfSingle("1", "11");

        testNewLastIndexOfSingle("apache", "a");
        testNewLastIndexOfSingle("apache", "p");
        testNewLastIndexOfSingle("apache", "e");
        testNewLastIndexOfSingle("apache", "x");
        testNewLastIndexOfSingle("oraoraoraora", "r");
        testNewLastIndexOfSingle("mudamudamudamuda", "d");
        // There is a route through checkLaterThan1#checkLaterThan1
        // which only gets touched if there is a two letter (or more) partial match
        // (in this case "st") earlier in the searched string.
        testNewLastIndexOfSingle("junk-ststarting", "starting");

        final Random random = new Random();
        final StringBuilder seg = new StringBuilder();
        while (seg.length() <= CharSequenceUtils.TO_STRING_LIMIT) {
            seg.append(random.nextInt());
        }
        StringBuilder original = new StringBuilder(seg);
        testNewLastIndexOfSingle(original, seg);
        for (int i = 0; i < 100; i++) {
            if (random.nextDouble() < 0.5) {
                original.append(random.nextInt() % 10);
            } else {
                original = new StringBuilder().append(String.valueOf(random.nextInt() % 100)).append(original);
            }
            testNewLastIndexOfSingle(original, seg);
        }
    }

    @ParameterizedTest
    @MethodSource("lastIndexWithStandardCharSequence")
    public void testLastIndexOfWithDifferentCharSequences(final CharSequence cs, final CharSequence search, final int start,
                                                          final int expected) {
        assertEquals(expected, CharSequenceUtils.lastIndexOf(cs, search, start));
    }

    static Stream<Arguments> lastIndexWithStandardCharSequence() {
        return Stream.of(
            arguments("abc", "b", 2, 1),
            arguments(new StringBuilder("abc"), "b", 2, 1),
            arguments(new StringBuffer("abc"), "b", 2, 1),
            arguments("abc", new StringBuilder("b"), 2, 1),
            arguments(new StringBuilder("abc"), new StringBuilder("b"), 2, 1),
            arguments(new StringBuffer("abc"), new StringBuffer("b"), 2, 1),
            arguments(new StringBuilder("abc"), new StringBuffer("b"), 2, 1)
        );
    }

    private void testNewLastIndexOfSingle(final CharSequence a, final CharSequence b) {
        final int maxa = Math.max(a.length(), b.length());
        for (int i = -maxa - 10; i <= maxa + 10; i++) {
            testNewLastIndexOfSingle(a, b, i);
        }
        testNewLastIndexOfSingle(a, b, Integer.MIN_VALUE);
        testNewLastIndexOfSingle(a, b, Integer.MAX_VALUE);
    }

    private void testNewLastIndexOfSingle(final CharSequence a, final CharSequence b, final int start) {
        testNewLastIndexOfSingleSingle(a, b, start);
        testNewLastIndexOfSingleSingle(b, a, start);
    }

    private void testNewLastIndexOfSingleSingle(final CharSequence a, final CharSequence b, final int start) {
        assertEquals(
                a.toString().lastIndexOf(b.toString(), start),
                CharSequenceUtils.lastIndexOf(new WrapperString(a.toString()), new WrapperString(b.toString()), start),
                "testNewLastIndexOf fails! original : " + a + " seg : " + b + " start : " + start
        );
    }
}
