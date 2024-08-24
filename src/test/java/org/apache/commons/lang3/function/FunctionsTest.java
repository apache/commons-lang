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

package org.apache.commons.lang3.function;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Function;

import org.junit.jupiter.api.Test;

/**
 * Tests {@link Functions}.
 */
public class FunctionsTest {

    /**
     * Tests {@link Functions#apply(Function, Object)}.
     */
    @Test
    public void testApply() {
        final AtomicBoolean bool = new AtomicBoolean();
        assertFalse(Functions.apply(bool::getAndSet, true));
        assertTrue(bool.get());
        assertNull(Functions.apply(null, "foo"));
        assertNull(Functions.apply(null, null));
    }

    /**
     * Tests {@link Functions#function(Function)}.
     */
    @Test
    public void testFunction() {
        assertEquals("foo", Functions.function(String::valueOf).andThen(String::toString).apply("foo"));
    }
}
