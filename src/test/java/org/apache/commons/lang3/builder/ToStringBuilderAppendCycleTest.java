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

import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;

/**
 * Tests {@code ToStringStyle} collection toString can overflow the stack.
 *
 * Pre-patch: appendDetail(StringBuffer, String, Collection) calls buffer.append(coll) which invokes the collection's own toString(), bypassing the cycle
 * registry. Two mutually-referencing ArrayLists cause StackOverflowError. Post-patch: the cycle is detected and a safe representation is produced.
 */
class ToStringBuilderAppendCycleTest {

    @Test
    void testMutuallyCyclicCollection() {
        final List<Object> a = new ArrayList<>();
        final List<Object> b = new ArrayList<>();
        a.add(b);
        b.add(a);
        assertNotNull(new ToStringBuilder(new Object(), ToStringStyle.DEFAULT_STYLE).append("field", a).build());
    }

    @Test
    void testMutuallyCyclicMap() {
        final Map<Object, Object> a = new HashMap<>();
        final Map<Object, Object> b = new HashMap<>();
        a.put(b, b);
        b.put(a, a);
        assertNotNull(new ToStringBuilder(new Object(), ToStringStyle.DEFAULT_STYLE).append("field", a).build());
    }

    @Test
    void testSelfReferentialCollection() {
        final List<Object> a = new ArrayList<>();
        a.add(a);
        assertNotNull(new ToStringBuilder(new Object(), ToStringStyle.DEFAULT_STYLE).append("field", a).build());
    }

    @Test
    void testSelfReferentialObjectArray() {
        final Object[] a = { null };
        a[0] = a;
        assertNotNull(new ToStringBuilder(new Object(), ToStringStyle.DEFAULT_STYLE).append("field", a).build());
    }
}
