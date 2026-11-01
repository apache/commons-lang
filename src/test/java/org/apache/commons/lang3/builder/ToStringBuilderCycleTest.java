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
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.stream.Stream;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Tests cycle detection and repeated-reference abbreviation across complete outputs.
 *
 * <p>
 * Detecting a reference to an object currently being rendered prevents infinite recursion, but does not prevent expanding a shared object again after its first
 * rendering finishes. Recursive styles must retain visited state across nested builders and appends so each object is detailed at most once per output. The
 * tests combine cycles with shared branches to exercise both requirements through reflection builders, manual builders, and direct style calls.
 * </p>
 *
 * <p>
 * Each graph is rendered twice with the same style to detect visited state leaking into a later output. Some cases leave an unrelated builder's root
 * registered: completing the current output must release its state without waiting for the entire root registry to become empty.
 * </p>
 */
class ToStringBuilderCycleTest extends AbstractLangTest {

    /**
     * Supplies fields that reflection can render to distinguish a cycle edge from a shared branch.
     *
     * <p>
     * The unique {@code id} lets tests count detailed renderings independently of identity hash codes and whitespace. The {@code next} and {@code shared}
     * fields are read reflectively, so they do not need accessors or a custom {@code toString()} implementation.
     * </p>
     */
    private static final class Node {

        @SuppressWarnings("unused")
        private final int id;
        @SuppressWarnings("unused")
        private Node next;
        @SuppressWarnings("unused")
        private Node shared;

        /**
         * Creates a node whose edges will be connected by the test.
         *
         * @param id the identifier used to count this node's detailed renderings.
         */
        private Node(final int id) {
            this.id = id;
        }
    }

    /**
     * Selects the API that owns the output and must release its traversal state.
     *
     * <p>
     * Reflection uses the graph itself as the root. Manual builders and direct calls use a separate root and append the graph as a named field. This difference
     * accounts for the additional graph identity in a reflection output's prefix.
     * </p>
     */
    private enum RenderMode {

        REFLECTION((value, style) -> complete(new ReflectionToStringBuilder(value, style))),
        MANUAL((value, style) -> complete(new ToStringBuilder(new Object(), style).append("graph", value))), DIRECT(ToStringBuilderCycleTest::renderDirect);

        private final BiFunction<Object, ToStringStyle, String> renderer;

        /**
         * Associates an entry point with its rendering and completion checks.
         *
         * @param renderer the operation that completes one output for a graph and style.
         */
        RenderMode(final BiFunction<Object, ToStringStyle, String> renderer) {
            this.renderer = renderer;
        }
    }

    /**
     * Supplies the recursive styles under test and the default style as a control for container cycle detection.
     */
    private enum Style {

        DEFAULT(() -> ToStringStyle.DEFAULT_STYLE), RECURSIVE(RecursiveToStringStyle::new), MULTILINE(MultilineRecursiveToStringStyle::new);

        private final Supplier<ToStringStyle> factory;

        /**
         * Associates a style with a supplier so mutable recursive styles are not shared between test cases.
         *
         * @param factory the style supplier; the default style can use its shared instance.
         */
        Style(final Supplier<ToStringStyle> factory) {
            this.factory = factory;
        }
    }

    /**
     * Renders a graph twice and checks that each completed output releases its traversal state.
     *
     * <p>
     * An optional abandoned builder leaves an unrelated root registered throughout both renderings. This exposes cleanup that incorrectly depends on an empty
     * root registry. Repeating the same graph also exposes stale visited marks that would abbreviate its first occurrence in the next output.
     * </p>
     *
     * @param graph      the graph to render on both passes.
     * @param kind       the style to create and reuse.
     * @param mode       the rendering entry point.
     * @param abandoned  whether to keep an unrelated abandoned builder's root registered.
     * @param assertions the output assertions to apply on each pass.
     */
    private static void assertRepeatedOutput(final Object graph, final Style kind, final RenderMode mode, final boolean abandoned,
            final Consumer<String> assertions) {
        final Object orphan = new Object();
        if (abandoned) {
            new ReflectionToStringBuilder(orphan, new RecursiveToStringStyle());
        }
        try {
            final ToStringStyle style = kind.factory.get();
            for (int repeat = 0; repeat < 2; repeat++) {
                assertions.accept(mode.renderer.apply(graph, style));
                // A completed output must release its state even when an unrelated root remains registered.
                assertState(orphan, abandoned);
            }
        } finally {
            ToStringStyle.unregister(orphan);
        }
    }

    /**
     * Checks that completed outputs leave no active or direct traversal and only the deliberately abandoned root, if any.
     *
     * @param orphan    the unrelated registered root, ignored when {@code abandoned} is {@code false} and may then be {@code null}.
     * @param abandoned whether the unrelated root should remain registered.
     */
    private static void assertState(final Object orphan, final boolean abandoned) {
        assertNull(ToStringStyle.getActiveTraversal());
        final Map<?, ?> direct = ToStringStyle.getDirectTraversals().get();
        assertTrue(direct == null || direct.isEmpty(), "Completed direct output must release its traversal");
        assertEquals(abandoned ? 1 : 0, ToStringStyle.getRegistry().size());
        if (abandoned) {
            assertTrue(ToStringStyle.getRegistry().containsKey(orphan));
        }
    }

    /**
     * Completes a builder and checks that both its visited state and root registration have been released.
     *
     * <p>
     * The final unregister operation prevents a failed assertion from leaving the test root behind; the assertions run before that fallback cleanup.
     * </p>
     *
     * @param builder the builder whose output is ready to complete.
     * @return the completed output.
     */
    private static String complete(final ToStringBuilder builder) {
        try {
            final String output = builder.toString();
            assertNull(builder.getTraversal().getVisited());
            assertFalse(ToStringStyle.getRegistry().containsKey(builder.getObject()));
            return output;
        } finally {
            ToStringStyle.unregister(builder.getObject());
        }
    }

    /**
     * Supplies long cycles that close at the root or at an interior node through each rendering entry point.
     *
     * @return combinations of both recursive styles, all entry points, depths of 32 and 64, cycle targets, and unrelated root registration.
     */
    static Stream<Arguments> longCycles() {
        final List<Arguments> arguments = new ArrayList<>();
        for (final Style style : new Style[] { Style.RECURSIVE, Style.MULTILINE }) {
            for (final RenderMode mode : RenderMode.values()) {
                for (final int depth : new int[] { 32, 64 }) {
                    for (final int backEdge : new int[] { 0, depth / 2 }) {
                        for (final boolean abandoned : new boolean[] { false, true }) {
                            arguments.add(Arguments.of(style, mode, depth, backEdge, abandoned));
                        }
                    }
                }
            }
        }
        return arguments.stream();
    }

    /**
     * Supplies mixed container cycles for both recursive styles and the default style.
     *
     * @return combinations of styles, rendering entry points, and unrelated root registration.
     */
    static Stream<Arguments> mixedCycles() {
        final List<Arguments> arguments = new ArrayList<>();
        for (final Style style : Style.values()) {
            for (final RenderMode mode : RenderMode.values()) {
                for (final boolean abandoned : new boolean[] { false, true }) {
                    arguments.add(Arguments.of(style, mode, abandoned));
                }
            }
        }
        return arguments.stream();
    }

    /**
     * Renders a graph using the direct style lifecycle and checks that {@code appendEnd()} unregisters its root.
     *
     * <p>
     * These calls have no builder to own the traversal. They exercise the state associated with the output buffer between {@code appendStart()},
     * {@code append()}, and {@code appendEnd()}.
     * </p>
     *
     * @param graph the graph to append as a field of a separate root.
     * @param style the style that manages the direct traversal.
     * @return the completed output.
     */
    private static String renderDirect(final Object graph, final ToStringStyle style) {
        final Object root = new Object();
        final StringBuffer buffer = new StringBuffer();
        try {
            style.appendStart(buffer, root);
            style.append(buffer, "graph", graph, Boolean.TRUE);
            style.appendEnd(buffer, root);
            assertFalse(ToStringStyle.getRegistry().containsKey(root));
            return buffer.toString();
        } finally {
            ToStringStyle.unregister(root);
        }
    }

    /**
     * Checks for leaked state before clearing root registrations and active and direct traversal state for subsequent tests.
     */
    @AfterEach
    void tearDown() {
        try {
            assertState(null, false);
        } finally {
            // Keep a failed cycle assertion from contaminating subsequent tests on this thread.
            new ArrayList<>(ToStringStyle.getRegistry().keySet()).forEach(ToStringStyle::unregister);
            ToStringStyle.endStyleTraversal(true);
            ToStringStyle.getDirectTraversals().remove();
        }
    }

    /**
     * Tests a long chain whose final edge closes a cycle and whose nodes all share a reference to the middle node.
     *
     * <p>
     * Each identifier must occur in detail exactly once. Cycle-closing edges and subsequent shared references must be abbreviated; merely terminating without a
     * stack overflow would not detect redundant expansion of the shared branch. Both renderings must satisfy the same assertions and release their traversal
     * state, including when an unrelated root remains registered.
     * </p>
     *
     * @param style     the recursive style to exercise.
     * @param mode      the rendering entry point.
     * @param depth     the number of distinct nodes in the chain.
     * @param backEdge  the index targeted by the final node's {@code next} reference.
     * @param abandoned whether an unrelated abandoned builder's root remains registered.
     */
    @ParameterizedTest(name = "{0}, {1}, depth={2}, backEdge={3}, abandoned={4}")
    @MethodSource("longCycles")
    void testLongCycleWithSharedBranches(final Style style, final RenderMode mode, final int depth, final int backEdge, final boolean abandoned) {
        final Node[] nodes = new Node[depth];
        for (int i = 0; i < depth; i++) {
            nodes[i] = new Node(i);
        }
        for (int i = 0; i < depth - 1; i++) {
            nodes[i].next = nodes[i + 1];
        }
        nodes[depth - 1].next = nodes[backEdge];
        for (final Node node : nodes) {
            node.shared = nodes[depth / 2];
        }
        assertRepeatedOutput(nodes[0], style, mode, abandoned, output -> {
            // Every node is detailed once. Closing the cycle and following shared branches must use abbreviated references.
            for (int i = 0; i < depth; i++) {
                assertEquals(1, StringUtils.countMatches(output, "id=" + i + ","), "Node " + i + " must be detailed exactly once");
            }
            assertTrue(output.contains("next=" + ObjectUtils.identityToString(nodes[backEdge])));
            assertTrue(output.length() < 1_000_000, "Cycle output must remain bounded");
        });
    }

    /**
     * Tests an {@code array -> list -> map -> array} cycle with a child shared by the list and map.
     *
     * <p>
     * The cycle must close with one reference to the array's identity; reflection adds a second occurrence in the root prefix. The shared child's identity must
     * occur twice. Recursive styles must detail that child only once, while the default style uses its inherited {@code Object.toString()} on both occurrences.
     * These assertions check that cycle detection and visited state survive transitions between container types.
     * </p>
     *
     * @param style     the style to exercise, including the default style as a control.
     * @param mode      the rendering entry point.
     * @param abandoned whether an unrelated abandoned builder's root remains registered.
     */
    @ParameterizedTest(name = "{0}, {1}, abandoned={2}")
    @MethodSource("mixedCycles")
    void testMixedArrayCollectionMapCycle(final Style style, final RenderMode mode, final boolean abandoned) {
        final Object[] graph = new Object[1];
        final List<Object> list = new ArrayList<>();
        final Map<String, Object> map = new LinkedHashMap<>();
        final Node shared = new Node(999);
        graph[0] = list;
        list.add(map);
        list.add(shared);
        map.put("back", graph);
        map.put("shared", shared);
        assertRepeatedOutput(graph, style, mode, abandoned, output -> {
            // The array -> list -> map -> array cycle closes with an identity reference, not another expansion.
            final String graphIdentity = ObjectUtils.identityToString(graph);
            assertEquals(mode == RenderMode.REFLECTION ? 2 : 1, StringUtils.countMatches(output, graphIdentity));
            final String childIdentity = ObjectUtils.identityToString(shared);
            assertEquals(2, StringUtils.countMatches(output, childIdentity));
            // Recursive styles detail the shared child once; the default style uses its own Object.toString() both times.
            assertEquals(style == Style.DEFAULT ? 0 : 1, StringUtils.countMatches(output, childIdentity + "["));
            assertTrue(output.length() < 10_000, "Mixed cycle output must remain bounded");
        });
    }
}
