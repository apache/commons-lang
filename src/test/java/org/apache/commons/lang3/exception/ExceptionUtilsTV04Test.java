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

package org.apache.commons.lang3.exception;

import static org.junit.jupiter.api.Assertions.*;
import java.util.List;
import java.util.Arrays;
import java.util.stream.Collectors;
import org.junit.jupiter.api.Test;

class ExceptionUtilsTV04Test {
    @Test void extractsRootCauseFromNestedChain() {
        IllegalArgumentException root = new IllegalArgumentException("root");
        RuntimeException middle = new RuntimeException("middle", root);
        Exception outer = new Exception("outer", middle);
        assertSame(root, ExceptionUtils.getRootCause(outer));
        assertEquals("root", ExceptionUtils.getRootCauseMessage(outer));
        assertEquals(3, ExceptionUtils.getThrowableCount(outer));
    }
    @Test void handlesNullThrowable() {
        assertNull(ExceptionUtils.getRootCause(null));
        assertEquals(0, ExceptionUtils.getThrowableCount(null));
        assertTrue(ExceptionUtils.getThrowableList(null).isEmpty());
        assertNull(ExceptionUtils.getMessage(null));
    }
    @Test void searchesThrowableTypesFromRequestedIndex() {
        RuntimeException root = new RuntimeException("root");
        IllegalStateException middle = new IllegalStateException("middle", root);
        Exception outer = new Exception("outer", middle);
        assertEquals(1, ExceptionUtils.indexOfThrowable(outer, IllegalStateException.class));
        assertEquals(2, ExceptionUtils.indexOfThrowable(outer, RuntimeException.class, 2));
        assertEquals(-1, ExceptionUtils.indexOfThrowable(outer, RuntimeException.class, 3));
        assertTrue(ExceptionUtils.hasCause(outer, RuntimeException.class));
    }
    @Test void streamsThrowableChainFromOuterToInner() {
        RuntimeException root = new RuntimeException("root");
        Exception outer = new Exception("outer", root);
        List<Throwable> chain = ExceptionUtils.stream(outer).collect(Collectors.toList());
        assertEquals(Arrays.asList(outer, root), chain);
    }
}
