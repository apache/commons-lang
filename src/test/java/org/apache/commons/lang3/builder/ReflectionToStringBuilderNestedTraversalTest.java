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

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Test;

/**
 * Tests that nested reflection calls preserve visited objects until the enclosing output is complete.
 */
class ReflectionToStringBuilderNestedTraversalTest extends AbstractLangTest {

    private static final class Holder {

        private final Object child;

        private Holder(final Object child) {
            this.child = child;
        }

        @Override
        public String toString() {
            return ReflectionToStringBuilder.toString(this, new RecursiveToStringStyle());
        }
    }

    private final Object shared = new Object();

    private void assertSharedChildDetailedOnce(final String output) {
        // Expect "child=java.lang.Object@<id>[]" in the first holder and "child=java.lang.Object@<id>" in the second.
        // The empty brackets are the detailed rendering of an Object with no fields. The second occurrence
        // must use only the identity because the same child has already been visited in this outer output.
        final String childField = "child=" + ObjectUtils.identityToString(shared);
        assertEquals(2, StringUtils.countMatches(output, childField), output);
        assertEquals(1, StringUtils.countMatches(output, childField + "[]"), output);
    }

    @Test
    void testNestedReflectionInManualBuilder() {
        // The outer style does not mark the holders visited, so the first nested call starts with an empty visited set.
        // Both append calls belong to one outer output, so the first holder's nested reflection call
        // must preserve the shared child's visited mark for the second holder.
        // @formatter:off
        final String output = new ToStringBuilder(new Object(), ToStringStyle.DEFAULT_STYLE)
                .append("first", new Holder(shared))
                .append("second", new Holder(shared))
                .toString();
        // @formatter:on
        assertSharedChildDetailedOnce(output);
    }

    @Test
    void testNestedReflectionInReflectionBuilder() {
        // Both array elements belong to one outer reflection call. Completing the first holder's nested call
        // must preserve the shared child's visited mark so only the first element renders that child in detail.
        final Object[] holders = { new Holder(shared), new Holder(shared) };
        final String output = ReflectionToStringBuilder.toString(holders, ToStringStyle.DEFAULT_STYLE);
        assertSharedChildDetailedOnce(output);
    }
}
