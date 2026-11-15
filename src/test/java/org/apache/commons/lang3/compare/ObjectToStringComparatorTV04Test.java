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

package org.apache.commons.lang3.compare;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;

class ObjectToStringComparatorTV04Test {
    private final ObjectToStringComparator comparator = ObjectToStringComparator.INSTANCE;
    @Test void comparesIdentityAndNulls() {
        Object value = new Object();
        assertEquals(0, comparator.compare(value, value));
        assertEquals(1, comparator.compare(null, "a"));
        assertEquals(-1, comparator.compare("a", null));
        assertEquals(0, comparator.compare(null, null));
    }
    @Test void comparesStringRepresentationsLexicographically() {
        assertTrue(comparator.compare(12, 2) < 0); // "12" precedes "2"
        assertEquals(0, comparator.compare(42, "42"));
        assertTrue(comparator.compare("b", "a") > 0);
    }
    @Test void handlesObjectsWhoseToStringReturnsNull() {
        Object nullString = new Object() { @Override public String toString() { return null; } };
        Object normal = new Object() { @Override public String toString() { return "x"; } };
        assertEquals(0, comparator.compare(nullString, nullString));
        assertEquals(1, comparator.compare(nullString, normal));
        assertEquals(-1, comparator.compare(normal, nullString));
    }
}
