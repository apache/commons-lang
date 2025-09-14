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

package org.apache.commons.lang3.function;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Function;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link Functions}.
 */
class FunctionsTest {

    /**
     * Tests {@link Functions#apply(Function, Object)}.
     */
    @Test
    void testApply() {
        final AtomicBoolean bool = new AtomicBoolean();
        assertFalse(Functions.apply(bool::getAndSet, true));
        assertTrue(bool.get());
        assertNull(Functions.apply(null, "foo"));
        assertNull(Functions.apply(null, null));
    }

    @Test
    void testApplyNonNull() {
        assertEquals("A", Functions.applyNonNull("a", String::toUpperCase));
        assertNull(Functions.applyNonNull((String) null, String::toUpperCase));
        assertNull(Functions.applyNonNull("a", s -> null));
        assertThrows(NullPointerException.class, () -> Functions.applyNonNull("a", null));
    }

    @Test
    void testApplyNonNull2() {
        assertEquals("A", Functions.applyNonNull(" a ", String::toUpperCase, String::trim));
        assertNull(Functions.applyNonNull((String) null, String::toUpperCase, String::trim));
        assertNull(Functions.applyNonNull(" a ", s -> null, String::trim));
        assertNull(Functions.applyNonNull(" a ", String::toUpperCase, s -> null));
        assertThrows(NullPointerException.class, () -> Functions.applyNonNull(" a ", null, String::trim));
        assertThrows(NullPointerException.class, () -> Functions.applyNonNull(" a ", String::toUpperCase, null));
    }

    @Test
    void testApplyNonNull3() {
        assertEquals("CBA", Functions.applyNonNull(" abc ", String::toUpperCase, String::trim, StringUtils::reverse));
        assertNull(Functions.applyNonNull((String) null, String::toUpperCase, String::trim, StringUtils::reverse));
        assertNull(Functions.applyNonNull(" abc ", s -> null, String::trim, StringUtils::reverse));
        assertNull(Functions.applyNonNull(" abc ", String::toUpperCase, s -> null, StringUtils::reverse));
        assertNull(Functions.applyNonNull(" abc ", String::toUpperCase, String::trim, s -> null));
        assertThrows(NullPointerException.class, () -> Functions.applyNonNull(" abc ", null, String::trim, StringUtils::reverse));
        assertThrows(NullPointerException.class, () -> Functions.applyNonNull(" abc ", String::toUpperCase, null, StringUtils::reverse));
        assertThrows(NullPointerException.class, () -> Functions.applyNonNull(" abc ", String::toUpperCase, String::trim, null));
    }

    /**
     * Tests {@link Functions#function(Function)}.
     */
    @Test
    void testFunction() {
        assertEquals("foo", Functions.function(String::valueOf).andThen(String::toString).apply("foo"));
    }
}
