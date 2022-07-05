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

package org.apache.commons.lang3;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import java.util.function.IntFunction;
import java.util.function.Supplier;

import org.junit.jupiter.api.Test;

public class ArrayUtilsSetTest extends AbstractLangTest {

    @Test
    public void testSetAll_IntFunction() {
        final IntFunction<?> nullIntFunction = null;
        assertNull(ArrayUtils.setAll(null, nullIntFunction));
        assertArrayEquals(null, ArrayUtils.setAll(null, nullIntFunction));
        assertArrayEquals(ArrayUtils.EMPTY_BOOLEAN_OBJECT_ARRAY, ArrayUtils.setAll(ArrayUtils.EMPTY_BOOLEAN_OBJECT_ARRAY, nullIntFunction));
        assertArrayEquals(ArrayUtils.EMPTY_OBJECT_ARRAY, ArrayUtils.setAll(ArrayUtils.EMPTY_OBJECT_ARRAY, nullIntFunction));
        final Integer[] array = new Integer[10];
        final Integer[] array2 = ArrayUtils.setAll(array, Integer::valueOf);
        assertSame(array, array2);
        for (int i = 0; i < array.length; i++) {
            assertEquals(i, array[i].intValue());
        }
    }

    @Test
    public void testSetAll_Suppiler() {
        final Supplier<?> nullSupplier = null;
        assertNull(ArrayUtils.setAll(null, nullSupplier));
        assertArrayEquals(null, ArrayUtils.setAll(null, nullSupplier));
        assertArrayEquals(ArrayUtils.EMPTY_BOOLEAN_OBJECT_ARRAY, ArrayUtils.setAll(ArrayUtils.EMPTY_BOOLEAN_OBJECT_ARRAY, nullSupplier));
        assertArrayEquals(ArrayUtils.EMPTY_OBJECT_ARRAY, ArrayUtils.setAll(ArrayUtils.EMPTY_OBJECT_ARRAY, nullSupplier));
        final String[] array = new String[10];
        final String[] array2 = ArrayUtils.setAll(array, () -> StringUtils.EMPTY);
        assertSame(array, array2);
        for (final String s : array) {
            assertEquals(StringUtils.EMPTY, s);
        }
    }
}
