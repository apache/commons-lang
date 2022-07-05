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
package org.apache.commons.lang3.builder;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Iterator;
import java.util.List;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Unit tests {@link DiffResult}.
 */
public class DiffResultTest extends AbstractLangTest {

    private static final SimpleClass SIMPLE_FALSE = new SimpleClass(false);
    private static final SimpleClass SIMPLE_TRUE = new SimpleClass(true);
    private static final ToStringStyle SHORT_STYLE = ToStringStyle.SHORT_PREFIX_STYLE;

    private static class SimpleClass implements Diffable<SimpleClass> {
        private final boolean booleanField;

        SimpleClass(final boolean booleanField) {
            this.booleanField = booleanField;
        }

        static String getFieldName() {
            return "booleanField";
        }

        @Override
        public DiffResult<SimpleClass> diff(final SimpleClass obj) {
            return new DiffBuilder<>(this, obj, ToStringStyle.SHORT_PREFIX_STYLE)
                    .append(getFieldName(), booleanField, obj.booleanField)
                    .build();
        }
    }

    private static class EmptyClass {
        // empty
    }

    @Test
    public void testListIsNonModifiable() {
        final SimpleClass lhs = new SimpleClass(true);
        final SimpleClass rhs = new SimpleClass(false);

        final List<Diff<?>> diffs = lhs.diff(rhs).getDiffs();

        final DiffResult<SimpleClass> list = new DiffResult<>(lhs, rhs, diffs, SHORT_STYLE);
        assertEquals(diffs, list.getDiffs());
        assertEquals(1, list.getNumberOfDiffs());
        assertThrows(UnsupportedOperationException.class, () -> list.getDiffs().remove(0));
    }

    @Test
    public void testIterator() {
        final SimpleClass lhs = new SimpleClass(true);
        final SimpleClass rhs = new SimpleClass(false);

        final List<Diff<?>> diffs = lhs.diff(rhs).getDiffs();
        final Iterator<Diff<?>> expectedIterator = diffs.iterator();

        final DiffResult<SimpleClass> list = new DiffResult<>(lhs, rhs, diffs, SHORT_STYLE);
        final Iterator<Diff<?>> iterator = list.iterator();

        while (iterator.hasNext()) {
            assertTrue(expectedIterator.hasNext());
            assertEquals(expectedIterator.next(), iterator.next());
        }
    }

    @Test
    public void testToStringOutput() {
        final DiffResult<EmptyClass> list = new DiffBuilder<>(new EmptyClass(), new EmptyClass(),
                ToStringStyle.SHORT_PREFIX_STYLE).append("test", false, true)
                .build();
        assertEquals(
                "DiffResultTest.EmptyClass[test=false] differs from DiffResultTest.EmptyClass[test=true]",
                list.toString());
    }

    @Test
    public void testToStringSpecifyStyleOutput() {
        final DiffResult<SimpleClass> list = SIMPLE_FALSE.diff(SIMPLE_TRUE);
        assertEquals(list.getToStringStyle(), SHORT_STYLE);

        final String lhsString = new ToStringBuilder(SIMPLE_FALSE,
                ToStringStyle.MULTI_LINE_STYLE).append(
                SimpleClass.getFieldName(), SIMPLE_FALSE.booleanField).build();

        final String rhsString = new ToStringBuilder(SIMPLE_TRUE,
                ToStringStyle.MULTI_LINE_STYLE).append(
                SimpleClass.getFieldName(), SIMPLE_TRUE.booleanField).build();

        final String expectedOutput = String.format("%s differs from %s", lhsString,
                rhsString);
        assertEquals(expectedOutput,
                list.toString(ToStringStyle.MULTI_LINE_STYLE));
    }

    @Test
    public void testNullLhs() {
        assertThrows(NullPointerException.class,
            () -> new DiffResult<>(null, SIMPLE_FALSE, SIMPLE_TRUE.diff(SIMPLE_FALSE).getDiffs(), SHORT_STYLE));
    }

    @Test
    public void testNullRhs() {
        assertThrows(NullPointerException.class,
            () -> new DiffResult<>(SIMPLE_TRUE, null, SIMPLE_TRUE.diff(SIMPLE_FALSE).getDiffs(), SHORT_STYLE));
    }

    @Test
    public void testNullList() {
        assertThrows(NullPointerException.class,
            () -> new DiffResult<>(SIMPLE_TRUE, SIMPLE_FALSE, null, SHORT_STYLE));
    }

    @Test
    public void testNullStyle() {
        final DiffResult<SimpleClass> diffResult = new DiffResult<>(SIMPLE_TRUE, SIMPLE_FALSE, SIMPLE_TRUE
                .diff(SIMPLE_FALSE).getDiffs(), null);
        assertEquals(ToStringStyle.DEFAULT_STYLE, diffResult.getToStringStyle());
    }

    @Test
    public void testNoDifferencesString() {
        final DiffResult<SimpleClass> diffResult = new DiffBuilder<>(SIMPLE_TRUE, SIMPLE_TRUE,
                SHORT_STYLE).build();
        assertEquals(DiffResult.OBJECTS_SAME_STRING, diffResult.toString());
    }

    @Test
    public void testLeftAndRightGetters() {
        final SimpleClass left = new SimpleClass(true);
        final SimpleClass right = new SimpleClass(false);

        final List<Diff<?>> diffs = left.diff(right).getDiffs();
        final DiffResult diffResult = new DiffResult(left, right, diffs, SHORT_STYLE);

        assertEquals(left, diffResult.getLeft());
        assertEquals(right, diffResult.getRight());
    }
}
