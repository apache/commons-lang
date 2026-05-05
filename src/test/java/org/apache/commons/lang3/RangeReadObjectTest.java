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
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.InvalidObjectException;

import org.apache.commons.lang3.reflect.FieldUtils;
import org.junit.jupiter.api.Test;

/**
 * Tests that a serialized {@link Range} can't store a bad cached hashCode.
 */
class RangeReadObjectTest {

    @Test
    void testBadHashCodeRejected() throws Exception {
        final Range<Integer> range = Range.of(1, 100);
        final byte[] bytes = SerializationUtils.serialize(range);
        // Locate the legitimate hashCode int in the serialized stream and overwrite it.
        final int hashCode = (Integer) FieldUtils.readDeclaredField(range, "hashCode", true);
        final byte[] edited = SerializationUtilsTest.replaceLastInt(bytes, hashCode, 0xDEADBEEF);
        final SerializationException ex = assertThrows(SerializationException.class, () -> SerializationUtils.deserialize(edited),
                "Bad hashCode in stream must be rejected with InvalidObjectException");
        assertInstanceOf(InvalidObjectException.class, ex.getCause());
        assertEquals("java.io.InvalidObjectException: Range hashCode does not match minimum/maximum.", ex.getMessage());
    }

    @Test
    void testRoundTripPreservesCorrectHashCode() throws Exception {
        final Range<String> range = Range.of("apple", "mango");
        final Range<String> roundtrip = SerializationUtils.roundtrip(range);
        assertEquals(range.hashCode(), roundtrip.hashCode(), "Round-trip serialization must preserve the correct hashCode");
        assertEquals(range, roundtrip);
    }
}
