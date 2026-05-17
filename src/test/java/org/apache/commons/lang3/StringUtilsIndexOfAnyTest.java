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

package org.apache.commons.lang3;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

/**
 * Tests {@code StringUtils#indexOfAny(...)}} methods.
 * <p>
 * Negative start index can crash new indexOfAny overload.
 * </p>
 * <p>
 * indexOfAny(CharSequence, int, char...) does not clamp negative csStart. When csStart < 0, the loop starts at a negative index causing
 * StringIndexOutOfBoundsException.
 * </p>
 * <p>
 * Pre-patch: indexOfAny("abc", -1, 'a') throws StringIndexOutOfBoundsException.
 * </p>
 * <p>
 * Post-patch: returns 0 (clamped to 0 per StringUtils convention).
 * </p>
 */
public class StringUtilsIndexOfAnyTest {

    @Test
    public void testLargeNegativeStartIndexClampsToZero() {
        assertEquals(0, StringUtils.indexOfAny("abc", Integer.MIN_VALUE, 'a'));
    }

    @Test
    public void testNegativeStartIndexClampsToZero() {
        // With csStart clamped to 0, the search finds 'a' at index 0
        assertEquals(0, StringUtils.indexOfAny("abc", -1, 'a'));
    }

    @Test
    public void testNegativeStartIndexDoesNotThrow() {
        assertEquals(0, StringUtils.indexOfAny("abc", -1, 'a'));
    }

    @Test
    public void testPositiveStartIndexWorksNormally() {
        // Start after 'a', should find 'b' at index 1
        assertEquals(1, StringUtils.indexOfAny("abc", 1, 'b'));
    }

    @Test
    public void testZeroStartIndexWorksNormally() {
        assertEquals(1, StringUtils.indexOfAny("abc", 0, 'b'));
    }
}
