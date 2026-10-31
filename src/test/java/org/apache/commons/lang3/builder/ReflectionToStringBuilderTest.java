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

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link ReflectionToStringBuilder}.
 */
class ReflectionToStringBuilderTest extends AbstractLangTest {

    private static final String REGISTRY = "REGISTRY";

    private static final String VISITED = "VISITED";

    private static final class Graph {

        final List<Object> items = new ArrayList<>();

        Graph() {
            for (int i = 0; i < 999; i++) {
                items.add(new Object());
            }
        }
    }

    @Test
    void testConstructorWithNullObject() {
        assertEquals("<null>", new ReflectionToStringBuilder(null, ToStringStyle.DEFAULT_STYLE, new StringBuffer()).toString());
    }

    @Test
    void testVisitedIsClearedAfterToStringWhenAnotherBuilderWasNeverStringified() throws Exception {
        final Object orphanRoot = new Graph();
        // Constructing registers the root, but toString() is never called, so the root is never unregistered.
        new ReflectionToStringBuilder(orphanRoot, new RecursiveToStringStyle());
        try {
            for (int i = 0; i < 10; i++) {
                new ReflectionToStringBuilder(new Graph(), new RecursiveToStringStyle()).toString();
            }
            assertEquals(0, threadLocalMapSize(VISITED), "VISITED must be empty once a top-level toString() completes");
            assertEquals(1, threadLocalMapSize(REGISTRY), "REGISTRY must not be empty");
        } finally {
            ToStringStyle.unregister(orphanRoot);
        }
    }

    @Test
    void testVisitedIsEmptyAfterPlainToString() throws Exception {
        new ReflectionToStringBuilder(new Graph(), new RecursiveToStringStyle()).toString();
        assertEquals(0, threadLocalMapSize(REGISTRY));
        assertEquals(0, threadLocalMapSize(VISITED));
    }

    private static int threadLocalMapSize(final String fieldName) throws ReflectiveOperationException {
        final Field field = ToStringStyle.class.getDeclaredField(fieldName);
        field.setAccessible(true);
        return ((Map<?, ?>) ((ThreadLocal<?>) field.get(null)).get()).size();
    }

}
