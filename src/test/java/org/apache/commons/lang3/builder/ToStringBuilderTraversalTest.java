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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.ref.Reference;
import java.util.Map;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.stream.Stream;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Tests visited-object ownership across builder traversals.
 */
class ToStringBuilderTraversalTest extends AbstractLangTest {

    private static final class FailingStyle extends RecursiveToStringStyle {

        private static final long serialVersionUID = 1L;
        private final Object failure;

        FailingStyle(final Object failure) {
            this.failure = failure;
        }

        @Override
        public void appendDetail(final StringBuffer buffer, final String fieldName, final Object value) {
            if (value == failure) {
                throw new IllegalStateException("Cannot render field");
            }
            super.appendDetail(buffer, fieldName, value);
        }
    }

    private static final class IdentityOnly {

        @Override
        public boolean equals(final Object other) {
            throw new AssertionError("Traversal must not call equals()");
        }

        @Override
        public int hashCode() {
            throw new AssertionError("Traversal must not call hashCode()");
        }
    }

    static Stream<BiConsumer<ToStringBuilder, Object[]>> appenders() {
        // @formatter:off
        return Stream.of(
                (builder, array) -> builder.append((Object) array),
                (builder, array) -> builder.append(array),
                (builder, array) -> builder.append("value", (Object) array),
                (builder, array) -> builder.append("value", array),
                (builder, array) -> builder.append("value", (Object) array, true),
                (builder, array) -> builder.append("value", array, true));
        // @formatter:on
    }

    private static void assertNoVisitedObjects() {
        assertNull(ToStringStyle.getActiveTraversal(), "The traversal must have ended");
        final Map<?, ?> direct = ToStringStyle.getDirectTraversals().get();
        assertTrue(direct == null || direct.isEmpty(), "Completed direct style output must release its traversal");
    }

    private static Set<?> builderVisited(final ToStringBuilder builder) {
        return builder.getTraversal().getVisited();
    }

    private static String identityDetailPrefix(final Object object) {
        return ObjectUtils.identityToString(object) + "[";
    }

    static Stream<Consumer<ToStringBuilder>> primitiveAppenders() {
        // @formatter:off
        return Stream.of(
                builder -> builder.append(true),
                builder -> builder.append("value", true),
                builder -> builder.append(new boolean[] {true}),
                builder -> builder.append("value", new boolean[] {true}),
                builder -> builder.append("value", new boolean[] {true}, true),
                builder -> builder.append((byte) 1),
                builder -> builder.append("value", (byte) 1),
                builder -> builder.append(new byte[] {1}),
                builder -> builder.append("value", new byte[] {1}),
                builder -> builder.append("value", new byte[] {1}, true),
                builder -> builder.append('a'),
                builder -> builder.append("value", 'a'),
                builder -> builder.append(new char[] {'a'}),
                builder -> builder.append("value", new char[] {'a'}),
                builder -> builder.append("value", new char[] {'a'}, true),
                builder -> builder.append(1.0),
                builder -> builder.append("value", 1.0),
                builder -> builder.append(new double[] {1}),
                builder -> builder.append("value", new double[] {1}),
                builder -> builder.append("value", new double[] {1}, true),
                builder -> builder.append(1.0f),
                builder -> builder.append("value", 1.0f),
                builder -> builder.append(new float[] {1}),
                builder -> builder.append("value", new float[] {1}),
                builder -> builder.append("value", new float[] {1}, true),
                builder -> builder.append(1),
                builder -> builder.append("value", 1),
                builder -> builder.append(new int[] {1}),
                builder -> builder.append("value", new int[] {1}),
                builder -> builder.append("value", new int[] {1}, true),
                builder -> builder.append(1L),
                builder -> builder.append("value", 1L),
                builder -> builder.append(new long[] {1}),
                builder -> builder.append("value", new long[] {1}),
                builder -> builder.append("value", new long[] {1}, true),
                builder -> builder.append((short) 1),
                builder -> builder.append("value", (short) 1),
                builder -> builder.append(new short[] {1}),
                builder -> builder.append("value", new short[] {1}),
                builder -> builder.append("value", new short[] {1}, true));
        // @formatter:on
    }

    static Stream<ToStringStyle> styles() {
        return Stream.of(new RecursiveToStringStyle(), new MultilineRecursiveToStringStyle());
    }

    private final Object orphan = new Object();

    @BeforeEach
    void setUp() {
        new ReflectionToStringBuilder(orphan, new RecursiveToStringStyle());
    }

    @AfterEach
    void tearDown() {
        ToStringStyle.unregister(orphan);
        try {
            assertNoVisitedObjects();
        } finally {
            ToStringStyle.endStyleTraversal(true);
            ToStringStyle.getDirectTraversals().remove();
        }
    }

    @Test
    void testAppendFailureReleasesTraversal() {
        final Object failure = new Object();
        final ToStringBuilder builder = new ToStringBuilder(new Object(), new FailingStyle(failure));
        try {
            assertThrows(IllegalStateException.class, () -> builder.append(new Object[] { new Object(), failure }));
            assertNoVisitedObjects();
            assertNull(builderVisited(builder), "A failed append must release the builder's visited objects");
            builder.toString();
            assertNull(builderVisited(builder));
        } finally {
            ToStringStyle.unregister(builder.getObject());
        }
        new ReflectionToStringBuilder(new Object[] { new Object() }, new RecursiveToStringStyle()).toString();
        assertNoVisitedObjects();
    }

    @ParameterizedTest
    @MethodSource("styles")
    void testAppendsBeforeReflectionToString(final ToStringStyle style) {
        final Object shared = new Object();
        for (int i = 0; i < 10; i++) {
            final ReflectionToStringBuilder builder = new ReflectionToStringBuilder(new Object[] { shared, new Object() }, style);
            builder.append("extra", shared);
            assertNoVisitedObjects();
            assertEquals(1, StringUtils.countMatches(builder.toString(), identityDetailPrefix(shared)));
            assertNoVisitedObjects();
            assertNull(builderVisited(builder));
        }
        assertEquals(1, ToStringStyle.getRegistry().size());
    }

    @ParameterizedTest
    @MethodSource("styles")
    void testBuilderAndDirectStyleAppendsShareVisitedObjects(final ToStringStyle style) {
        final Object shared = new Object();
        final ToStringBuilder builder = new ToStringBuilder(new Object(), style).append(shared);
        style.append(builder.getStringBuffer(), "direct", shared, Boolean.TRUE);
        final String result = builder.append(shared).toString();
        assertEquals(1, StringUtils.countMatches(result, identityDetailPrefix(shared)));
        assertNull(builderVisited(builder));
        assertNoVisitedObjects();
    }

    @Test
    void testCollectedVisitedReferencesAreRemoved() {
        final ToStringBuilder builder = new ToStringBuilder(new Object(), new RecursiveToStringStyle());
        builder.append(new Object());
        final Set<?> visited = builderVisited(builder);
        final Reference<?> reference = (Reference<?>) visited.iterator().next();
        // Exercise queue cleanup deterministically without depending on the garbage collector's scheduling.
        reference.clear();
        reference.enqueue();
        final Object next = new Object();
        builder.append(next);
        assertEquals(1, visited.size());
        assertEquals(1, StringUtils.countMatches(builder.toString(), identityDetailPrefix(next)));
        assertNoVisitedObjects();
    }

    @Test
    void testCompletionFailureReleasesTraversal() {
        final Object root = new Object();
        final ToStringStyle style = new RecursiveToStringStyle() {

            private static final long serialVersionUID = 1L;

            @Override
            public void appendEnd(final StringBuffer buffer, final Object object) {
                if (object == root) {
                    throw new IllegalStateException("Cannot complete output");
                }
                super.appendEnd(buffer, object);
            }
        };
        final ToStringBuilder builder = new ToStringBuilder(root, style);
        try {
            builder.append(new Object[] { new Object() });
            assertThrows(IllegalStateException.class, builder::toString);
            assertNoVisitedObjects();
            assertNull(builderVisited(builder));
        } finally {
            ToStringStyle.unregister(builder.getObject());
        }
    }

    @Test
    void testConstructorCallbackSharesVisitedObjects() {
        final Object root = new Object();
        final Object shared = new Object();
        final ToStringStyle style = new RecursiveToStringStyle() {

            private static final long serialVersionUID = 1L;

            @Override
            public void appendStart(final StringBuffer buffer, final Object object) {
                super.appendStart(buffer, object);
                if (object == root) {
                    append(buffer, "prefix", shared, Boolean.TRUE);
                }
            }
        };
        final ToStringBuilder builder = new ToStringBuilder(root, style);
        assertNoVisitedObjects();
        assertEquals(1, StringUtils.countMatches(builder.append(shared).toString(), identityDetailPrefix(shared)));
        assertNoVisitedObjects();
    }

    @Test
    void testConstructorFailureReleasesTraversal() {
        final Object root = new Object();
        final ToStringStyle style = new RecursiveToStringStyle() {

            private static final long serialVersionUID = 1L;

            @Override
            public void appendStart(final StringBuffer buffer, final Object object) {
                super.appendStart(buffer, object);
                if (object == root) {
                    append(buffer, "prefix", new Object(), Boolean.TRUE);
                    throw new IllegalStateException("Cannot start output");
                }
            }
        };
        try {
            assertThrows(IllegalStateException.class, () -> new ToStringBuilder(root, style));
            assertNoVisitedObjects();
        } finally {
            ToStringStyle.unregister(root);
        }
    }

    @ParameterizedTest
    @MethodSource("primitiveAppenders")
    void testCustomPrimitiveFormattingSharesVisitedObjects(final Consumer<ToStringBuilder> appender) {
        final Object shared = new Object();
        final ToStringStyle style = new RecursiveToStringStyle() {

            private static final long serialVersionUID = 1L;

            @Override
            protected void appendFieldStart(final StringBuffer buffer, final String fieldName) {
                super.appendFieldStart(buffer, fieldName);
                appendInternal(buffer, fieldName, shared, true);
            }
        };
        final ToStringBuilder builder = new ToStringBuilder(new Object(), style).append(shared);
        appender.accept(builder);
        assertNoVisitedObjects();
        assertEquals(1, StringUtils.countMatches(builder.toString(), identityDetailPrefix(shared)));
    }

    @Test
    void testCustomSuperAndToStringFormattingShareVisitedObjects() {
        final Object shared = new Object();
        final ToStringStyle style = new RecursiveToStringStyle() {

            private static final long serialVersionUID = 1L;

            @Override
            public void appendSuper(final StringBuffer buffer, final String superToString) {
                appendInternal(buffer, null, shared, true);
            }

            @Override
            public void appendToString(final StringBuffer buffer, final String toString) {
                appendInternal(buffer, null, shared, true);
            }
        };
        final ToStringBuilder builder = new ToStringBuilder(new Object(), style).append(shared);
        builder.appendSuper("super").appendToString("delegate");
        assertNoVisitedObjects();
        assertEquals(1, StringUtils.countMatches(builder.toString(), identityDetailPrefix(shared)));
    }

    @Test
    void testDirectArrayAppendKeepsTraversalActiveBetweenItems() {
        final RecursiveToStringStyle style = new RecursiveToStringStyle() {

            private static final long serialVersionUID = 1L;

            @Override
            protected void appendDetail(final StringBuffer buffer, final String fieldName, final int index, final Object value) {
                assertNotNull(ToStringStyle.getActiveTraversal(), "The array traversal must survive between items, even without appendStart()");
                super.appendDetail(buffer, fieldName, index, value);
            }
        };
        final Object shared = new Object();
        final StringBuffer buffer = new StringBuffer();
        style.append(buffer, "array", new Object[] { shared, shared }, Boolean.TRUE);
        assertEquals(1, StringUtils.countMatches(buffer, identityDetailPrefix(shared)));
        assertNoVisitedObjects();
    }

    @ParameterizedTest
    @MethodSource("styles")
    void testDirectStyleAppendThenBuilderCompletion(final ToStringStyle style) {
        for (int i = 0; i < 100; i++) {
            final Object shared = new Object();
            final ToStringBuilder builder = new ToStringBuilder(new Object(), style);
            style.append(builder.getStringBuffer(), "direct", shared, Boolean.TRUE);
            final String result = builder.append(shared).toString();
            assertEquals(1, StringUtils.countMatches(result, identityDetailPrefix(shared)));
            assertNull(builderVisited(builder));
            assertNoVisitedObjects();
        }
        assertEquals(1, ToStringStyle.getRegistry().size());
    }

    @Test
    void testDirectStyleAppendThenBuilderCompletionWithoutOrphan() {
        ToStringStyle.unregister(orphan);
        for (int i = 0; i < 100; i++) {
            final ToStringBuilder builder = new ToStringBuilder(new Object(), new RecursiveToStringStyle());
            builder.getStyle().append(builder.getStringBuffer(), "direct", new Object(), Boolean.TRUE);
            builder.toString();
            assertNull(builderVisited(builder));
            assertNoVisitedObjects();
        }
        assertTrue(ToStringStyle.getRegistry().isEmpty());
    }

    @ParameterizedTest
    @MethodSource("styles")
    void testDirectStyleCallsKeepSharedReferences(final ToStringStyle style) {
        final Object shared = new Object();
        Object graph = shared;
        for (int i = 0; i < 20; i++) {
            graph = new Object[] { graph, graph };
        }
        final StringBuffer buffer = new StringBuffer();
        final Object root = new Object();
        try {
            style.appendStart(buffer, root);
            style.append(buffer, "graph", new Object[] { graph, graph }, Boolean.TRUE);
            style.appendEnd(buffer, root);
            assertEquals(1, StringUtils.countMatches(buffer, identityDetailPrefix(shared)));
            assertTrue(buffer.length() < 10_000, "Shared references must not be expanded repeatedly");
            assertNoVisitedObjects();
        } finally {
            ToStringStyle.unregister(root);
        }
    }

    @Test
    void testDirectStyleCompletionReleasesBuilderState() {
        final ToStringBuilder builder = new ToStringBuilder(new Object(), new RecursiveToStringStyle()).append(new Object());
        builder.getStyle().appendEnd(builder.getStringBuffer(), builder.getObject());
        assertNull(builderVisited(builder));
        assertNoVisitedObjects();
    }

    @Test
    void testDirectStyleStateSurvivesFailedBuilder() {
        final Object shared = new Object();
        final Object root = new Object();
        final Object failure = new Object();
        final Object[] failedRoot = { new Object(), failure };
        final RecursiveToStringStyle style = new RecursiveToStringStyle();
        final StringBuffer buffer = new StringBuffer();
        try {
            style.appendStart(buffer, root);
            style.append(buffer, "first", shared, Boolean.TRUE);
            assertThrows(IllegalStateException.class, () -> new ReflectionToStringBuilder(failedRoot, new FailingStyle(failure)).toString());
            style.append(buffer, "second", shared, Boolean.TRUE);
            style.appendEnd(buffer, root);
            assertEquals(1, StringUtils.countMatches(buffer, identityDetailPrefix(shared)));
            assertNoVisitedObjects();
        } finally {
            ToStringStyle.unregister(root);
            ToStringStyle.unregister(failedRoot);
        }
    }

    @ParameterizedTest
    @MethodSource("styles")
    void testDirectStyleStateSurvivesIndependentBuilder(final ToStringStyle style) {
        final Object shared = new Object();
        final Object root = new Object();
        final StringBuffer buffer = new StringBuffer();
        try {
            style.appendStart(buffer, root);
            style.append(buffer, "first", shared, Boolean.TRUE);
            new ReflectionToStringBuilder(new Object[] { new Object() }, new RecursiveToStringStyle()).toString();
            style.append(buffer, "second", shared, Boolean.TRUE);
            style.appendEnd(buffer, root);
            assertEquals(1, StringUtils.countMatches(buffer, identityDetailPrefix(shared)));
            assertNoVisitedObjects();
        } finally {
            ToStringStyle.unregister(root);
        }
    }

    @ParameterizedTest
    @MethodSource("styles")
    void testInterleavedBuildersKeepIndependentVisitedObjects(final ToStringStyle style) {
        final Object shared = new Object();
        final ToStringBuilder first = new ToStringBuilder(new Object(), style).append(shared);
        final ToStringBuilder second = new ToStringBuilder(new Object(), style).append(shared);
        assertEquals(1, StringUtils.countMatches(first.append(shared).toString(), identityDetailPrefix(shared)));
        assertEquals(1, StringUtils.countMatches(second.append(shared).toString(), identityDetailPrefix(shared)));
        assertNoVisitedObjects();
    }

    @ParameterizedTest
    @MethodSource("styles")
    void testInterleavedDirectStyleOutputs(final ToStringStyle style) {
        final Object shared = new Object();
        final Object firstRoot = new Object();
        final Object secondRoot = new Object();
        final StringBuffer first = new StringBuffer();
        final StringBuffer second = new StringBuffer();
        try {
            style.appendStart(first, firstRoot);
            style.append(first, "first", shared, Boolean.TRUE);
            style.appendStart(second, secondRoot);
            style.append(second, "first", shared, Boolean.TRUE);
            style.append(first, "second", shared, Boolean.TRUE);
            style.appendEnd(first, firstRoot);
            style.append(second, "second", shared, Boolean.TRUE);
            style.appendEnd(second, secondRoot);
            assertEquals(1, StringUtils.countMatches(first, identityDetailPrefix(shared)));
            assertEquals(1, StringUtils.countMatches(second, identityDetailPrefix(shared)));
            assertNoVisitedObjects();
        } finally {
            ToStringStyle.unregister(firstRoot);
            ToStringStyle.unregister(secondRoot);
        }
    }

    @ParameterizedTest
    @MethodSource("appenders")
    void testManualAppendsReleaseTraversal(final BiConsumer<ToStringBuilder, Object[]> appender) {
        for (int i = 0; i < 10; i++) {
            final Object shared = new Object();
            final ToStringBuilder builder = new ToStringBuilder(new Object(), new RecursiveToStringStyle());
            appender.accept(builder, new Object[] { shared, shared });
            assertNoVisitedObjects();
            appender.accept(builder, new Object[] { shared });
            assertEquals(1, StringUtils.countMatches(builder.toString(), identityDetailPrefix(shared)));
            assertNoVisitedObjects();
            assertNull(builderVisited(builder));
        }
        assertEquals(1, ToStringStyle.getRegistry().size());
    }

    @ParameterizedTest
    @MethodSource("styles")
    void testNestedBuildersKeepSharedReferences(final ToStringStyle style) {
        final Object shared = new Object();
        final Object[] root = { new Object(), shared, new Object[] { shared } };
        final String result = new ReflectionToStringBuilder(root, style).toString();
        assertEquals(1, StringUtils.countMatches(result, identityDetailPrefix(shared)));
        assertNoVisitedObjects();
    }

    @Test
    void testNestedBuilderWithEarlierAppends() {
        final Object shared = new Object();
        final ReflectionToStringBuilder nested = new ReflectionToStringBuilder(new Object[] { shared }, new RecursiveToStringStyle());
        nested.append(shared);
        final ToStringBuilder outer = new ToStringBuilder(new Object());
        final String result = outer.append(nested).toString();
        assertEquals(1, StringUtils.countMatches(result, identityDetailPrefix(shared)));
        assertNoVisitedObjects();
        assertNull(builderVisited(nested));
    }

    @Test
    void testNestedBuilderWithEarlierAppendsJoinsVisitedObjects() {
        final Object shared = new Object();
        final ReflectionToStringBuilder nested = new ReflectionToStringBuilder(new Object[] { shared }, new RecursiveToStringStyle());
        nested.append(shared);
        final RecursiveToStringStyle style = new RecursiveToStringStyle() {

            private static final long serialVersionUID = 1L;

            @Override
            protected boolean accept(final Class<?> clazz) {
                return clazz != ReflectionToStringBuilder.class && super.accept(clazz);
            }
        };
        final ToStringBuilder outer = new ToStringBuilder(new Object(), style).append(new Object());
        final String result = outer.append(nested).append(shared).toString();
        assertEquals(1, StringUtils.countMatches(result, identityDetailPrefix(shared)));
        assertNoVisitedObjects();
    }

    @Test
    void testNullReflectionCompletionReleasesEarlierAppends() {
        final ReflectionToStringBuilder builder = new ReflectionToStringBuilder(null, new RecursiveToStringStyle());
        builder.append(new Object[] { new Object() });
        assertEquals("<null>", builder.toString());
        assertNoVisitedObjects();
        assertNull(builderVisited(builder));
    }

    @Test
    void testOrphanWithEarlierAppendDoesNotPolluteLaterBuilders() {
        new ReflectionToStringBuilder(orphan, new RecursiveToStringStyle()).append(new Object[] { new Object() });
        for (int i = 0; i < 10; i++) {
            new ReflectionToStringBuilder(new Object[] { new Object() }, new RecursiveToStringStyle()).toString();
            assertNoVisitedObjects();
        }
    }

    @ParameterizedTest
    @MethodSource("styles")
    void testReflectionArrayAppendSharesVisitedObjects(final ToStringStyle style) {
        final Object shared = new Object();
        final ReflectionToStringBuilder builder = new ReflectionToStringBuilder(new Object[] { shared }, style);
        builder.reflectionAppendArray(new Object[] { shared });
        assertNoVisitedObjects();
        assertEquals(1, StringUtils.countMatches(builder.toString(), identityDetailPrefix(shared)));
        assertNoVisitedObjects();
    }

    @Test
    void testReflectionFailureReleasesTraversal() {
        final Object failure = new Object();
        final Object[] root = { new Object(), failure };
        final ReflectionToStringBuilder builder = new ReflectionToStringBuilder(root, new FailingStyle(failure));
        try {
            assertThrows(IllegalStateException.class, builder::toString);
            assertNoVisitedObjects();
            assertNull(builderVisited(builder));
        } finally {
            ToStringStyle.unregister(root);
        }
    }

    @Test
    void testShallowBuilderDoesNotAllocateVisitedMap() {
        final ToStringBuilder builder = new ToStringBuilder(new Object());
        builder.append(new Object());
        assertNull(builderVisited(builder));
        builder.toString();
        assertNoVisitedObjects();
    }

    @Test
    void testValidationFailureReleasesEarlierAppends() {
        final ReflectionToStringBuilder builder = new ReflectionToStringBuilder(new Object(), new RecursiveToStringStyle());
        builder.append(new Object());
        builder.setIncludeFieldNames("value").setExcludeFieldNames("value");
        assertThrows(IllegalStateException.class, builder::toString);
        assertNoVisitedObjects();
        assertNull(builderVisited(builder));
    }

    @ParameterizedTest
    @MethodSource("styles")
    void testVisitedObjectsUseIdentityWithoutCallingObjectMethods(final ToStringStyle style) {
        final Object first = new IdentityOnly();
        final Object second = new IdentityOnly();
        final String result = new ToStringBuilder(new Object(), style).append(new Object[] { first, second, first }).toString();
        assertEquals(1, StringUtils.countMatches(result, identityDetailPrefix(first)));
        assertEquals(1, StringUtils.countMatches(result, identityDetailPrefix(second)));
        assertNoVisitedObjects();
    }
}
