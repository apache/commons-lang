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
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledIfSystemProperty;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Score-boundary and exhaustive small-input tests for the legacy fuzzy distance implementation.
 *
 * <p>
 * Enable the large-input tests with {@code -Dtest.stringutils.large.heap=true -DheapSize=-Xmx8g}.
 * Use {@code -DheapSize=-Xmx12g} on Java 8.
 * </p>
 */
class StringUtilsFuzzyDistanceTest extends AbstractLangTest {

    // Invoke the legacy method reflectively without introducing deprecated API usages.
    private static int distance(final CharSequence term, final CharSequence query) throws ReflectiveOperationException {
        final Method method = StringUtils.class.getMethod("getFuzzyDistance", CharSequence.class, CharSequence.class, Locale.class);
        return (Integer) method.invoke(null, term, query, Locale.ROOT);
    }

    /**
     * Counts greedy matches and their contiguous runs instead of incrementally accumulating the score.
     * Each run contributes three points per character, minus the first character's missing two-point bonus.
     */
    private static int referenceDistance(final String term, final String query) {
        int matches = 0;
        int runs = 0;
        int nextIndex = 0;
        for (int i = 0; i < query.length(); i++) {
            final int index = term.indexOf(query.charAt(i), nextIndex);
            if (index < 0) {
                break;
            }
            if (matches == 0 || index != nextIndex) {
                runs++;
            }
            matches++;
            nextIndex = index + 1;
        }
        return 3 * matches - 2 * runs;
    }

    private static String termWithGaps(final String query, final int initialGaps, final boolean lastGap) {
        if (initialGaps == 0 && !lastGap) {
            return query;
        }
        final char[] chars = new char[query.length() + initialGaps + (lastGap ? 1 : 0)];
        Arrays.fill(chars, 'a');
        for (int i = 0; i < initialGaps; i++) {
            chars[2 * i + 1] = 'b';
        }
        if (lastGap) {
            chars[chars.length - 2] = 'b';
        }
        return new String(chars);
    }

    @Test
    void testAllShortBinaryInputs() throws ReflectiveOperationException {
        final List<String> inputs = new ArrayList<>();
        for (int length = 1; length <= 5; length++) {
            for (int bits = 0; bits < (1 << length); bits++) {
                final StringBuilder text = new StringBuilder(length);
                for (int bit = 0; bit < length; bit++) {
                    text.append((bits & (1 << bit)) == 0 ? 'x' : 'y');
                }
                inputs.add(text.toString());
            }
        }
        for (final String term : inputs) {
            for (final String query : inputs) {
                assertEquals(referenceDistance(term, query), distance(term, query), term + " -> " + query);
            }
        }
    }

    @ParameterizedTest
    @CsvSource({
        "715827882, 0, false, 2147483644",
        "715827883, 1, false, 2147483645",
        "715827884, 2, false, 2147483646",
        "715827883, 0, false, 2147483647",
        "715827884, 1, true,  2147483646",
        "715827885, 2, true,  2147483647"
    })
    @EnabledIfSystemProperty(named = "test.stringutils.large.heap", matches = "true")
    void testRepresentableScoreBoundaries(final int matches, final int initialGaps, final boolean lastGap, final int expected)
            throws ReflectiveOperationException {
        // A run of n matches scores 3*n-2; each gap removes two bonus points.
        // Include both consecutive and non-consecutive final matches at the integer limit.
        final String query = StringUtils.repeat('a', matches);
        assertEquals(expected, distance(termWithGaps(query, initialGaps, lastGap), query));
    }

    @Test
    @EnabledIfSystemProperty(named = "test.stringutils.large.heap", matches = "true")
    void testScoreAtMaximumWithUnmatchedSuffix() throws ReflectiveOperationException {
        final String term = StringUtils.repeat('a', 715_827_883);
        final String query = term + "b";
        assertEquals(Integer.MAX_VALUE, distance(term, query));
        assertEquals(Integer.MAX_VALUE, distance(query, term));
    }

    @ParameterizedTest
    @CsvSource({
        "715827884,  1, false",
        "715827885,  2, false",
        "715827884,  0, false",
        "715827884,  0, true",
        "1431655766, 0, false",
        "1431655767, 0, false"
    })
    @EnabledIfSystemProperty(named = "test.stringutils.large.heap", matches = "true")
    void testUnrepresentableScores(final int matches, final int initialGaps, final boolean lastGap) {
        // Exercise scores MAX_VALUE+1, +2 and +3, an overflowing non-consecutive match,
        // and scores that would wrap to zero or a positive int.
        final String query = StringUtils.repeat('a', matches);
        final String term = termWithGaps(query, initialGaps, lastGap);
        final InvocationTargetException exception = assertThrows(InvocationTargetException.class, () -> distance(term, query));
        assertInstanceOf(ArithmeticException.class, exception.getCause());
    }
}
