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

package org.apache.commons.lang3.math;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.InvalidObjectException;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang3.SerializationException;
import org.apache.commons.lang3.SerializationUtils;
import org.apache.commons.lang3.SerializationUtilsTest;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.junit.jupiter.api.Test;

/**
 * Tests that a serialized {@link Fraction} can't store a bad cached hashCode.
 */
public class FractionReadObjectTest {

    @Test
    public void testBadHashCodeStreamIsRejected() throws Exception {
        final Fraction fraction = Fraction.getFraction(3, 7);
        final byte[] bytes = SerializationUtils.serialize(fraction);
        final int hashCode = (Integer) FieldUtils.readDeclaredField(fraction, "hashCode", true);
        final byte[] edited = SerializationUtilsTest.replaceLastInt(bytes, hashCode, 0xCAFEBABE);
        final SerializationException ex = assertThrows(SerializationException.class, () -> SerializationUtils.deserialize(edited),
                "Bad hashCode in stream must be rejected with InvalidObjectException");
        assertInstanceOf(InvalidObjectException.class, ex.getCause());
        assertEquals("java.io.InvalidObjectException: Fraction hashCode does not match numerator/denominator.", ex.getMessage());

    }

    @Test
    public void testHashMapLookupAfterRoundTrip() throws Exception {
        final Fraction fraction = Fraction.getFraction(1, 4);
        final byte[] bytes = SerializationUtils.serialize(fraction);
        final Fraction deserialized = SerializationUtils.deserialize(bytes);
        final Map<Fraction, String> map = new HashMap<>();
        map.put(fraction, "quarter");
        assertEquals("quarter", map.get(deserialized), "HashMap lookup must work after deserialization");
    }

    @Test
    public void testRoundTripPreservesHashCode() throws Exception {
        final Fraction fraction = Fraction.getFraction(1, 4);
        final Fraction roundtrip = SerializationUtils.roundtrip(fraction);
        assertEquals(fraction.hashCode(), roundtrip.hashCode(), "Round-trip serialization must preserve the correct hashCode");
        assertEquals(fraction, roundtrip);

    }
}
