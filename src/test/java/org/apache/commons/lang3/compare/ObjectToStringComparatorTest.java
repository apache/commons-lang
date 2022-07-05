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

package org.apache.commons.lang3.compare;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.Arrays;
import java.util.List;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link ObjectToStringComparator}.
 */
public class ObjectToStringComparatorTest extends AbstractLangTest {

    private static class Thing {

        final String string;

        Thing(final String string) {
            this.string = string;
        }

        @Override
        public String toString() {
            return string;
        }
    }

    @Test
    public void testNull() {
        final List<Thing> things = Arrays.asList(null, new Thing("y"), null);
        things.sort(ObjectToStringComparator.INSTANCE);
        assertEquals("y", things.get(0).string);
        assertNull(things.get(1));
        assertNull(things.get(2));
    }

    @Test
    public void testNullToString() {
        final List<Thing> things = Arrays.asList(new Thing(null), new Thing("y"), new Thing(null));
        things.sort(ObjectToStringComparator.INSTANCE);
        assertEquals("y", things.get(0).string);
        assertNull(things.get(1).string);
        assertNull(things.get(2).string);
    }

    @Test
    public void testSortCollection() {
        final List<Thing> things = Arrays.asList(new Thing("z"), new Thing("y"), new Thing("x"));
        things.sort(ObjectToStringComparator.INSTANCE);
        assertEquals("x", things.get(0).string);
        assertEquals("y", things.get(1).string);
        assertEquals("z", things.get(2).string);
    }
}
