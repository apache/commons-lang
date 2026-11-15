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

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.IntStream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Index-boundary regressions using virtual inputs, without large allocations or long scans.
 */
class StringsOverflowTest extends AbstractLangTest {

    private static final class RepeatedCharSequence implements CharSequence {

        private final int length;

        private RepeatedCharSequence(final int length) {
            this.length = length;
        }

        @Override
        public char charAt(final int index) {
            if (index < 0 || index >= length) {
                throw new IndexOutOfBoundsException("Index: " + index + ", length: " + length);
            }
            return 'a';
        }

        @Override
        public int length() {
            return length;
        }

        @Override
        public CharSequence subSequence(final int start, final int end) {
            if (start < 0 || end < start || end > length) {
                throw new IndexOutOfBoundsException();
            }
            return new RepeatedCharSequence(end - start);
        }

        @Override
        public String toString() {
            throw new AssertionError("The search must not materialize the virtual input");
        }
    }

    @ParameterizedTest
    @ValueSource(ints = { Integer.MAX_VALUE - 1, Integer.MAX_VALUE })
    void testIndexOfEmptySearchAtMaximumLength(final int length) {
        final CharSequence input = new RepeatedCharSequence(length);
        assertEquals(0, Strings.CI.indexOf(input, ""));
        for (final int start : new int[] { Integer.MIN_VALUE, -1, 1, length - 1, length }) {
            assertEquals(Math.max(start, 0), Strings.CI.indexOf(input, "", start), "start=" + start);
        }
        if (length < Integer.MAX_VALUE) {
            assertEquals(-1, Strings.CI.indexOf(input, "", length + 1));
        }
    }

    @ParameterizedTest
    @CsvSource({
        "2147483646, 1",
        "2147483646, 2",
        "2147483647, 1",
        "2147483647, 2"
    })
    void testIndexOfNonEmptySearchAtMaximumLength(final int length, final int searchLength) {
        final CharSequence input = new RepeatedCharSequence(length);
        final String search = StringUtils.repeat('A', searchLength);
        final int lastStart = length - searchLength;
        assertEquals(lastStart - 1, Strings.CI.indexOf(input, search, lastStart - 1));
        assertEquals(lastStart, Strings.CI.indexOf(input, search, lastStart));
        assertEquals(-1, Strings.CI.indexOf(input, search, lastStart + 1));
        // A miss at the last candidate must terminate without wrapping the loop counter.
        assertEquals(-1, Strings.CI.indexOf(input, StringUtils.repeat('B', searchLength), lastStart));
    }

    @ParameterizedTest
    @CsvSource({
        "2147483630, 17",
        "2147483630, 18",
        "2147483631, 16",
        "2147483631, 17",
        "2147483646, 1",
        "2147483646, 2",
        "2147483647, 1",
        "2147483647, 2"
    })
    void testLastIndexOfAtMaximumLength(final int length, final int searchLength) {
        final CharSequence input = new RepeatedCharSequence(length);
        final String search = StringUtils.repeat('a', searchLength);
        final int lastStart = length - searchLength;
        assertEquals(lastStart, Strings.CS.lastIndexOf(input, search));
        // Cross both the last valid match and the former start+searchLength overflow boundary.
        final int[] starts = IntStream.of(lastStart - 1, lastStart, lastStart + 1, length - 1, Integer.MAX_VALUE).distinct().toArray();
        for (final int start : starts) {
            assertEquals(Math.min(start, lastStart), Strings.CS.lastIndexOf(input, search, start), "start=" + start);
        }
    }

    @Test
    void testSearchLongerThanMaximumLengthInput() {
        final CharSequence input = new RepeatedCharSequence(Integer.MAX_VALUE - 1);
        final CharSequence search = new RepeatedCharSequence(Integer.MAX_VALUE);
        assertEquals(-1, Strings.CI.indexOf(input, search));
        assertEquals(-1, Strings.CS.lastIndexOf(input, search));
    }
}
