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
package org.apache.commons.lang3.builder;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Tests {@link DiffBuilder#append(String, Object, Object)} independently of reflection accessibility.
 */
class DiffBuilderObjectTest extends AbstractBuilderTest {

    private static final ToStringStyle SHORT_STYLE = ToStringStyle.SHORT_PREFIX_STYLE;

    private static DiffResult<String> diffObjects(final Object left, final Object right) {
        return DiffBuilder.<String>builder().setLeft("left").setRight("right").setStyle(SHORT_STYLE).build()
                .append("value", left, right).build();
    }

    static Stream<Arguments> testObjectIncompatibleArrays() {
        return Stream.of(
                Arguments.of(new boolean[] { true }, "value"),
                Arguments.of(new byte[] { 1 }, "value"),
                Arguments.of(new char[] { 'a' }, "value"),
                Arguments.of(new double[] { 1 }, "value"),
                Arguments.of(new float[] { 1 }, "value"),
                Arguments.of(new int[] { 1 }, "value"),
                Arguments.of(new long[] { 1 }, "value"),
                Arguments.of(new short[] { 1 }, "value"),
                Arguments.of(new Object[] { "value" }, "value"),
                Arguments.of(new int[] { 1 }, new long[] { 1 }),
                Arguments.of(new int[] { 1 }, new Integer[] { 1 }),
                Arguments.of(new int[][] { { 1 } }, new int[] { 1 }));
    }

    static Stream<Arguments> testObjectPrimitiveArrays() {
        return Stream.of(
                Arguments.of(new boolean[] { true }, new boolean[] { true }, new boolean[] { false },
                        new Boolean[] { true }, new Boolean[] { false }),
                Arguments.of(new byte[] { 1 }, new byte[] { 1 }, new byte[] { 2 },
                        new Byte[] { 1 }, new Byte[] { 2 }),
                Arguments.of(new char[] { 'a' }, new char[] { 'a' }, new char[] { 'b' },
                        new Character[] { 'a' }, new Character[] { 'b' }),
                Arguments.of(new double[] { 1 }, new double[] { 1 }, new double[] { 2 },
                        new Double[] { 1.0 }, new Double[] { 2.0 }),
                Arguments.of(new float[] { 1 }, new float[] { 1 }, new float[] { 2 },
                        new Float[] { 1.0f }, new Float[] { 2.0f }),
                Arguments.of(new int[] { 1 }, new int[] { 1 }, new int[] { 2 },
                        new Integer[] { 1 }, new Integer[] { 2 }),
                Arguments.of(new long[] { 1 }, new long[] { 1 }, new long[] { 2 },
                        new Long[] { 1L }, new Long[] { 2L }),
                Arguments.of(new short[] { 1 }, new short[] { 1 }, new short[] { 2 },
                        new Short[] { 1 }, new Short[] { 2 }));
    }

    @Test
    void testObjectArraysAndNull() {
        final Object[][] cases = { { new int[] { 1 }, new Integer[] { 1 } }, { new String[] { "value" }, new String[] { "value" } } };
        for (final Object[] pair : cases) {
            final Object array = pair[0];
            final Object[] expected = (Object[]) pair[1];
            final DiffResult<String> left = diffObjects(array, null);
            final DiffResult<String> right = diffObjects(null, array);
            assertEquals(1, left.getNumberOfDiffs());
            assertEquals(1, right.getNumberOfDiffs());
            assertNull(left.getDiffs().get(0).getRight());
            assertNull(right.getDiffs().get(0).getLeft());
            assertArrayEquals(expected, (Object[]) left.getDiffs().get(0).getLeft());
            assertArrayEquals(expected, (Object[]) right.getDiffs().get(0).getRight());
        }
    }

    @Test
    void testObjectArraysWithDifferentComponentTypes() {
        final Object[] left = { "value" };
        final String[] right = { "value" };
        assertEquals(0, diffObjects(left, right).getNumberOfDiffs());
        assertEquals(0, diffObjects(right, left).getNumberOfDiffs());
        right[0] = "other";
        assertEquals(1, diffObjects(left, right).getNumberOfDiffs());
        assertEquals(1, diffObjects(right, left).getNumberOfDiffs());
    }

    @ParameterizedTest
    @MethodSource
    void testObjectIncompatibleArrays(final Object left, final Object right) {
        for (final Object[] pair : new Object[][] { { left, right }, { right, left } }) {
            final DiffResult<String> result = diffObjects(pair[0], pair[1]);
            assertEquals(1, result.getNumberOfDiffs());
            final Diff<?> diff = result.getDiffs().get(0);
            assertEquals("value", diff.getFieldName());
            assertSame(pair[0], diff.getLeft());
            assertSame(pair[1], diff.getRight());
        }
    }

    @ParameterizedTest
    @MethodSource
    void testObjectPrimitiveArrays(final Object left, final Object equal, final Object right, final Object[] expectedLeft, final Object[] expectedRight) {
        assertEquals(0, diffObjects(left, equal).getNumberOfDiffs());
        final DiffResult<String> result = diffObjects(left, right);
        assertEquals(1, result.getNumberOfDiffs());
        final Diff<?> diff = result.getDiffs().get(0);
        assertEquals("value", diff.getFieldName());
        assertEquals(expectedLeft.getClass(), diff.getType());
        assertArrayEquals(expectedLeft, (Object[]) diff.getLeft());
        assertArrayEquals(expectedRight, (Object[]) diff.getRight());
    }
}
