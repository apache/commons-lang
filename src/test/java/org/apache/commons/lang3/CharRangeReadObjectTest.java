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
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InvalidObjectException;
import java.io.ObjectInputStream;

import org.apache.commons.lang3.reflect.FieldUtils;
import org.junit.jupiter.api.Test;

/**
 * Tests that a serialized {@link CharRange} can't carry an inverted ({@code start > end}) range.
 */
class CharRangeReadObjectTest {

    private static Object deserialize(final byte[] bytes) throws IOException, ClassNotFoundException {
        try (ObjectInputStream ois = new ObjectInputStream(new ByteArrayInputStream(bytes))) {
            return ois.readObject();
        }
    }

    @Test
    void testForgedInvertedRangeIsRejected() throws Exception {
        // Negative control: the factory reverses reversed endpoints, so no inverted range can be built directly.
        assertEquals(CharRange.isIn('a', 'e'), CharRange.isIn('e', 'a'));
        final CharRange seed = CharRange.isIn('a', 'e');
        FieldUtils.writeDeclaredField(seed, "start", 'z', true);
        FieldUtils.writeDeclaredField(seed, "end", 'a', true);
        assertThrows(InvalidObjectException.class, () -> deserialize(SerializationUtils.serialize(seed)));
        assertThrows(SerializationException.class, () -> SerializationUtils.roundtrip(seed));
    }

    @Test
    void testRoundTripPreservesRange() throws Exception {
        final CharRange range = CharRange.isIn('a', 'e');
        final CharRange roundtrip = SerializationUtils.roundtrip(range);
        assertEquals(range, roundtrip);
        assertTrue(roundtrip.contains('c'));
        assertFalse(roundtrip.contains('z'));
    }
}
