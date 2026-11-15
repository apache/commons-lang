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

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledIfSystemProperty;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Boundary and exhaustive small-input tests for the legacy Levenshtein implementations.
 *
 * <p>
 * Enable the maximum-length scans with {@code -Dtest.stringutils.large.levenshtein=true}.
 * These tests use constant-size virtual inputs but perform billions of iterations; no large heap is needed.
 * </p>
 */
class StringUtilsLevenshteinDistanceTest extends AbstractLangTest {

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
            return StringUtils.repeat('a', length);
        }
    }

    // Invoke the legacy methods reflectively without introducing deprecated API usages.
    private static int distance(final CharSequence left, final CharSequence right) throws ReflectiveOperationException {
        final Method method = StringUtils.class.getMethod("getLevenshteinDistance", CharSequence.class, CharSequence.class);
        return (Integer) method.invoke(null, left, right);
    }

    private static int distance(final CharSequence left, final CharSequence right, final int threshold) throws ReflectiveOperationException {
        final Method method = StringUtils.class.getMethod("getLevenshteinDistance", CharSequence.class, CharSequence.class, int.class);
        return (Integer) method.invoke(null, left, right, threshold);
    }

    /**
     * Uses a full matrix as an independent oracle for the optimized row and stripe implementations.
     */
    private static int referenceDistance(final String left, final String right) {
        final int[][] costs = new int[left.length() + 1][right.length() + 1];
        for (int i = 0; i <= left.length(); i++) {
            costs[i][0] = i;
        }
        for (int j = 0; j <= right.length(); j++) {
            costs[0][j] = j;
        }
        for (int i = 1; i <= left.length(); i++) {
            for (int j = 1; j <= right.length(); j++) {
                final int substitution = costs[i - 1][j - 1] + (left.charAt(i - 1) == right.charAt(j - 1) ? 0 : 1);
                costs[i][j] = Math.min(substitution, Math.min(costs[i - 1][j] + 1, costs[i][j - 1] + 1));
            }
        }
        return costs[left.length()][right.length()];
    }

    @Test
    void testAllShortBinaryInputs() throws ReflectiveOperationException {
        final List<String> inputs = new ArrayList<>();
        for (int length = 1; length <= 4; length++) {
            for (int bits = 0; bits < (1 << length); bits++) {
                final StringBuilder text = new StringBuilder(length);
                for (int bit = 0; bit < length; bit++) {
                    text.append((bits & (1 << bit)) == 0 ? 'x' : 'y');
                }
                inputs.add(text.toString());
            }
        }
        for (final String left : inputs) {
            for (final String right : inputs) {
                final int expected = referenceDistance(left, right);
                final String context = left + " -> " + right;
                assertEquals(expected, distance(left, right), context);
                for (final int threshold : new int[] { 0, 1, 2, 3, 4, 5, Integer.MAX_VALUE - 1, Integer.MAX_VALUE }) {
                    assertEquals(expected <= threshold ? expected : -1, distance(left, right, threshold), context + ", threshold=" + threshold);
                }
            }
        }
    }

    @ParameterizedTest
    @ValueSource(ints = { Integer.MAX_VALUE - 1, Integer.MAX_VALUE })
    void testMaximumLengthEmptyAndRejectedInputs(final int length) throws ReflectiveOperationException {
        final CharSequence input = new RepeatedCharSequence(length);
        assertEquals(length, distance("", input));
        assertEquals(length, distance(input, ""));
        assertEquals(length, distance("", input, length));
        assertEquals(length, distance(input, "", length));
        assertEquals(-1, distance("", input, length - 1));
        assertEquals(-1, distance(input, "", length - 1));
        assertEquals(-1, distance("a", input, length - 2));
        assertEquals(-1, distance(input, "a", length - 2));
    }

    @ParameterizedTest
    @CsvSource({
        "2147483646, a, false, 2147483645",
        "2147483646, b, true, 2147483646",
        "2147483647, a, false, 2147483646",
        "2147483647, a, true, 2147483646",
        "2147483647, b, false, 2147483647",
        "2147483647, b, true, 2147483647",
        "2147483647, aa, true, 2147483645"
    })
    @EnabledIfSystemProperty(named = "test.stringutils.large.levenshtein", matches = "true")
    void testMaximumLengthUnbounded(final int length, final String text, final boolean reverse, final int expected)
            throws ReflectiveOperationException {
        final CharSequence input = new RepeatedCharSequence(length);
        assertEquals(expected, reverse ? distance(input, text) : distance(text, input));
    }

    @ParameterizedTest
    @CsvSource({
        "2147483646, a, 2147483645, false, 2147483645",
        "2147483646, b, 2147483645, true, -1",
        "2147483647, a, 2147483646, false, 2147483646",
        "2147483647, a, 2147483647, true, 2147483646",
        "2147483647, b, 2147483646, false, -1",
        "2147483647, b, 2147483647, true, 2147483647",
        "2147483647, aa, 2147483645, false, 2147483645",
        "2147483647, bb, 2147483645, true, -1"
    })
    @EnabledIfSystemProperty(named = "test.stringutils.large.levenshtein", matches = "true")
    void testMaximumLengthWithThreshold(final int length, final String text, final int threshold, final boolean reverse, final int expected)
            throws ReflectiveOperationException {
        final CharSequence input = new RepeatedCharSequence(length);
        assertEquals(expected, reverse ? distance(input, text, threshold) : distance(text, input, threshold));
    }
}
