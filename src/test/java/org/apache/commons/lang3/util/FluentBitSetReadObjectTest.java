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

package org.apache.commons.lang3.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.ObjectStreamClass;
import java.io.Serializable;
import java.util.BitSet;

import org.apache.commons.lang3.SerializationException;
import org.apache.commons.lang3.SerializationUtils;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link FluentBitSet#readObject(ObjectInputStream)}.
 * <p>
 * Covers normal round-trip serialization as well as rejection of a forged stream in which the {@code bitSet} field is {@code null}.
 * </p>
 */
class FluentBitSetReadObjectTest {

    /**
     * Forge class: mirrors the field layout of {@link FluentBitSet} (same field name, type, and {@code serialVersionUID}) but declares {@code bitSet} as
     * non-final so it can be set to {@code null}, which the real constructor rejects.
     */
    private static final class FluentBitSetForge implements Serializable {

        private static final long serialVersionUID = 1L; // must match FluentBitSet.serialVersionUID

        // Non-final so we can construct an instance with bitSet == null.
        @SuppressWarnings("unused")
        private final BitSet bitSet;

        FluentBitSetForge(final BitSet bitSet) {
            this.bitSet = bitSet;
        }
    }

    /**
     * Deserializes an object from the given byte array using a plain {@link ObjectInputStream}.
     */
    private static Object deserialize(final byte[] bytes) throws IOException, ClassNotFoundException {
        try (ObjectInputStream ois = new ObjectInputStream(new ByteArrayInputStream(bytes))) {
            return ois.readObject();
        }
    }

    /**
     * Builds a serialized stream that looks like a {@link FluentBitSet} (matching class name and {@code serialVersionUID}) but carries {@code bitSet == null}.
     */
    private static byte[] forgeNullBitSetStream() throws IOException {
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try (ObjectOutputStream oos = new ObjectOutputStream(baos) {

            @Override
            protected void writeClassDescriptor(final ObjectStreamClass desc) throws IOException {
                if (desc.getName().equals(FluentBitSetForge.class.getName())) {
                    // Emit the descriptor for FluentBitSet so the stream deserializes as that class.
                    super.writeClassDescriptor(ObjectStreamClass.lookup(FluentBitSet.class));
                } else {
                    super.writeClassDescriptor(desc);
                }
            }
        }) {
            oos.writeObject(new FluentBitSetForge(null));
        }
        return baos.toByteArray();
    }

    /**
     * Tests that a deserialized {@link FluentBitSet} remains fully functional (bits can be read and modified) after round-trip serialization.
     */
    @Test
    void testDeserializedInstanceIsMutable() {
        final FluentBitSet original = new FluentBitSet().set(5);
        final FluentBitSet roundtrip = SerializationUtils.roundtrip(original);
        assertTrue(roundtrip.get(5));
        roundtrip.set(6);
        assertTrue(roundtrip.get(6), "Deserialized instance must remain mutable");
        // The original must not be affected.
        assertInstanceOf(FluentBitSet.class, roundtrip);
        assertTrue(original.get(5));
    }

    /**
     * Tests that a forged stream with {@code bitSet == null} is rejected with {@link InvalidObjectException} when deserialized directly via
     * {@link ObjectInputStream}.
     */
    @Test
    void testNullBitSetRejectedByObjectInputStream() throws Exception {
        final byte[] forged = forgeNullBitSetStream();
        final Exception ex = assertThrows(InvalidObjectException.class, () -> deserialize(forged));
        assertTrue(ex.getMessage().contains("bitSet null"));
    }

    /**
     * Tests that a forged stream with {@code bitSet == null} is rejected with {@link SerializationException} wrapping an {@link InvalidObjectException} when
     * deserialized via {@link SerializationUtils#deserialize(byte[])}.
     */
    @Test
    void testNullBitSetRejectedBySerializationUtils() throws Exception {
        final byte[] forged = forgeNullBitSetStream();
        final SerializationException ex = assertThrows(SerializationException.class, () -> SerializationUtils.deserialize(forged));
        assertInstanceOf(InvalidObjectException.class, ex.getCause());
        assertTrue(ex.getCause().getMessage().contains("bitSet null"));
    }

    /**
     * Tests that round-trip serialization of an empty {@link FluentBitSet} produces an equal, empty instance.
     */
    @Test
    void testRoundTripEmptyBitSet() {
        final FluentBitSet original = new FluentBitSet();
        final FluentBitSet roundtrip = SerializationUtils.roundtrip(original);
        assertEquals(original, roundtrip);
        assertTrue(roundtrip.isEmpty());
    }

    /**
     * Tests that round-trip serialization of a {@link FluentBitSet} with scattered bits set preserves all bit values.
     */
    @Test
    void testRoundTripPreservesBits() {
        final FluentBitSet original = new FluentBitSet().set(1, 3, 5, 7, 100);
        final FluentBitSet roundtrip = SerializationUtils.roundtrip(original);
        assertEquals(original, roundtrip);
        assertEquals(original.bitSet(), roundtrip.bitSet());
    }

    /**
     * Tests that round-trip serialization preserves the hash code of a {@link FluentBitSet}.
     */
    @Test
    void testRoundTripPreservesHashCode() {
        final FluentBitSet original = new FluentBitSet().set(2, 4, 8, 16);
        assertEquals(original.hashCode(), SerializationUtils.roundtrip(original).hashCode());
    }

    /**
     * Tests that a {@link FluentBitSet} wrapping a {@link BitSet} that was constructed via {@link BitSet#valueOf(long[])} survives a round-trip serialization.
     */
    @Test
    void testRoundTripWithBitSetValueOf() {
        final BitSet bs = BitSet.valueOf(new long[] { 0b1010_1010L });
        final FluentBitSet original = new FluentBitSet(bs);
        assertEquals(original, SerializationUtils.roundtrip(original));
    }

    /**
     * Tests that round-trip serialization of a {@link FluentBitSet} with a large bit index preserves the full contents.
     */
    @Test
    void testRoundTripWithHighBitIndex() {
        final FluentBitSet original = new FluentBitSet(256).set(0, 127, 255);
        final FluentBitSet roundtrip = SerializationUtils.roundtrip(original);
        assertEquals(original, roundtrip);
        assertTrue(roundtrip.get(0));
        assertTrue(roundtrip.get(127));
        assertTrue(roundtrip.get(255));
    }

    /**
     * Tests that two independently deserialized instances of the same {@link FluentBitSet} are equal, ensuring readObject leaves the object in a consistent
     * state.
     */
    @Test
    void testTwoDeserializedInstancesAreEqual() {
        final FluentBitSet original = new FluentBitSet().set(10, 20, 30);
        final byte[] bytes = SerializationUtils.serialize(original);
        final FluentBitSet first = SerializationUtils.deserialize(bytes);
        final FluentBitSet second = SerializationUtils.deserialize(bytes);
        assertEquals(first, second);
        assertEquals(first.bitSet(), second.bitSet());
    }
}
