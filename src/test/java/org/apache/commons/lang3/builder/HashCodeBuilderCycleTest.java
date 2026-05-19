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

import static org.junit.jupiter.api.Assertions.assertNotEquals;

import org.junit.jupiter.api.Test;

/**
 * Tests cycles in {@link HashCodeBuilder}.
 * <p>
 * {@link HashCodeBuilder#append(Object)} calls object.hashCode() directly without the ThreadLocal cycle-guard registry that reflectionHashCode() uses.
 * </p>
 *
 * <p>
 * When two objects reference each other and each implements hashCode() via {@code new HashCodeBuilder().append(peer)}, calling hashCode() recurses infinitely:
 * a.hashCode() → append(b) → b.hashCode() → append(a) → ...
 * </p>
 *
 * <p>
 * Pre-patch: StackOverflowError is thrown. Post-patch: cycle detected; completes without error.
 * </p>
 */
class HashCodeBuilderCycleTest {

    static class CyclicNode {

        final String label;
        CyclicNode peer;

        CyclicNode(final String label) {
            this.label = label;
        }

        @Override
        public int hashCode() {
            return new HashCodeBuilder(17, 37).append(label).append(peer).toHashCode();
        }

        @Override
        public boolean equals(final Object o) {
            return o instanceof CyclicNode cn && label.equals(cn.label);
        }
    }

    @Test
    void cyclicPeerDoesNotOverflowStack() {
        final CyclicNode a = new CyclicNode("a");
        final CyclicNode b = new CyclicNode("b");
        a.peer = b;
        b.peer = a;
        assertNotEquals(0, a.hashCode());
    }

    @Test
    void selfReferentialDoesNotOverflowStack() {
        final CyclicNode self = new CyclicNode("self");
        self.peer = self;
        assertNotEquals(0, self.hashCode());
    }

    @Test
    void acyclicChainProducesValue() {
        final CyclicNode a = new CyclicNode("a");
        final CyclicNode b = new CyclicNode("b");
        a.peer = b; // b.peer is null, no cycle
        assertNotEquals(0, a.hashCode());
    }
}
