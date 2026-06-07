/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
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

import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

/**
 * Tests {@link EqualsBuilder#reflectionEquals(Object, Object, String...)}.
 * <p>
 * reflectionEquals array fix enables cyclic-array.
 * </p>
 * <p>
 * Pre-patch: Object[] elements containing themselves cause StackOverflowError when compared via EqualsBuilder.reflectionEquals because the array branch in
 * reflectionAppend calls append(lhs, rhs) which recurses without cycle check.
 * </p>
 * <p>
 * Post-patch: the arrays are registered before recursing so cycles are detected and the comparison terminates (returning false).
 * </p>
 */
class EqualsBuilderReflectionEqualsCycleTest {

    @Test
    void testCrossReferentialObjectArrays() {
        final Object[] a = new Object[1];
        final Object[] b = new Object[1];
        // a[0] -> b, b[0] -> a: mutual cycle
        a[0] = b;
        b[0] = a;
        assertTrue(EqualsBuilder.reflectionEquals(a, b));
    }

    @Test
    void testSelfReferentialObjectArrays() {
        final Object[] a = new Object[1];
        final Object[] b = new Object[1];
        a[0] = a;
        b[0] = b;
        // Pre-patch: StackOverflowError; Post-patch: terminates without error.
        // With cycle detection, comparing a[0]=a vs b[0]=b sees (a,b) already registered
        // and treats the cycle as equal, so the overall result is true (structurally isomorphic).
        // The key assertion is that NO StackOverflowError is thrown.
        assertTrue(EqualsBuilder.reflectionEquals(a, b));
    }
}
