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
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.Collections;
import java.util.Comparator;
import java.util.Objects;

import org.apache.commons.lang3.reflect.FieldUtils;
import org.junit.jupiter.api.Test;

/**
 * Tests that a serialized {@link Range} can't store a bad cached hashCode.
 */
class RangeReadObjectTest {

    /**
     * Standin class used only to drive {@link ObjectOutputStream#writeObject(Object)} into emitting a stream that matches the wire format of {@link Range} but
     * with caller-controlled field values. The class name and serialVersionUID are spoofed in the stream below via a custom {@link ObjectOutputStream} subclass
     * so the stream reads back as a {@code Range}.
     */
    private static final class RangeForge implements Serializable {

        private static final long serialVersionUID = 2L; // matches Range.serialVersionUID
        private final Object comparator;
        private final int hashCode;
        private final Object maximum;
        private final Object minimum;

        RangeForge(final Object comparator, final Object minimum, final Object maximum, final int hashCode) {
            this.comparator = comparator;
            this.minimum = minimum;
            this.maximum = maximum;
            this.hashCode = hashCode;
        }
    }

    private static Object deserialize(final byte[] bytes) throws IOException, ClassNotFoundException {
        try (ObjectInputStream ois = new ObjectInputStream(new ByteArrayInputStream(bytes))) {
            return ois.readObject();
        }
    }

    /**
     * Serializes a {@link RangeForge} but rewrites the class descriptor name to "org.apache.commons.lang3.Range" so the resulting bytes deserialize as a
     * {@link Range}. Because the field set, types, order, and serialVersionUID all match, default deserialization assigns each forged value to the
     * corresponding Range field via reflection (bypassing the constructor).
     */
    private static byte[] forgeRangeStream(final Object comparator, final Object minimum, final Object maximum, final int hashCode) throws IOException {
        // Build the legitimate-shape bytes via RangeForge, then rewrite the embedded class name.
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try (ObjectOutputStream oos = new ObjectOutputStream(baos) {

            @Override
            protected void writeClassDescriptor(final java.io.ObjectStreamClass desc) throws IOException {
                if (desc.getName().equals(RangeForge.class.getName())) {
                    // Emit a descriptor whose name is Range but whose field layout still matches RangeForge.
                    final java.io.ObjectStreamClass spoofed = java.io.ObjectStreamClass.lookup(Range.class);
                    super.writeClassDescriptor(spoofed);
                } else {
                    super.writeClassDescriptor(desc);
                }
            }
        }) {
            oos.writeObject(new RangeForge(comparator, minimum, maximum, hashCode));
        }
        return baos.toByteArray();
    }

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

    /**
     * Forged stream with {@code comparator == null}; F-004 hashCode check passes because we set hashCode canonically; {@code contains()} then NPEs on
     * {@code comparator.compare(...)}.
     */
    @Test
    void testComparatorNullViaForgedStream() throws Exception {
        final Integer min = Integer.valueOf(1);
        final Integer max = Integer.valueOf(10);
        final int canonicalHash = Objects.hash(min, max);
        final byte[] forged = forgeRangeStream(null, min, max, canonicalHash);
        assertThrows(InvalidObjectException.class, () -> deserialize(forged));
    }

    /**
     * Forged stream: minimum=1, maximum=10, hashCode=hash(1,10) (all legitimate), but comparator replaced with a reversed ordering. The hashCode gate passes
     * (comparator excluded from the hash) and the null gates pass (comparator is non-null); the ordering invariant is the only one violated. The deserialized
     * Range still reports endpoints [1,10] but {@code contains(5)} returns false because it trusts the reversed comparator.
     */
    @Test
    void testForgedReversedComparatorBreaksContains() throws Exception {
        final Range<Integer> reference = Range.of(Integer.valueOf(1), Integer.valueOf(10));
        final Range<Integer> forged = Range.of(Integer.valueOf(1), Integer.valueOf(10));
        final Comparator<Integer> reversed = Collections.reverseOrder();
        FieldUtils.writeDeclaredField(forged, "comparator", reversed, true);
        assertThrows(InvalidObjectException.class, () -> deserialize(SerializationUtils.serialize(forged)));
        assertThrows(SerializationException.class, () -> SerializationUtils.deserialize(SerializationUtils.serialize(forged)));
        assertThrows(SerializationException.class, () -> SerializationUtils.roundtrip(forged));
        assertTrue(reference.contains(Integer.valueOf(5)));
    }

    /**
     * Forged stream with {@code maximum == null}; symmetric to F-061b.
     */
    @Test
    void testMaximumNullViaForgedStream() throws Exception {
        final Integer min = Integer.valueOf(1);
        final int canonicalHash = Objects.hash(min, (Object) null);
        final Object comparator = Range.of(Integer.valueOf(1), Integer.valueOf(2)).getComparator();
        final byte[] forged = forgeRangeStream(comparator, min, null, canonicalHash);
        assertThrows(InvalidObjectException.class, () -> deserialize(forged));
    }

    /**
     * Forged stream with {@code minimum == null}; {@code Objects.hash(null, max)} is a valid int, so the F-004 check passes. {@code contains()} NPEs
     * because {@code comparator.compare(element, null)} unboxes null (or, for ComparableComparator, calls {@code element.compareTo(null)} which is an
     * NPE-by-contract).
     */
    @Test
    void testMinimumNullViaForgedStream() throws Exception {
        final Integer max = Integer.valueOf(10);
        final int canonicalHash = Objects.hash((Object) null, max);
        // comparator must be non-null here so we isolate the minimum-null gap.
        // We use ComparableComparator.INSTANCE via deserialization round-trip of a real Range.
        final Object comparator = Range.of(Integer.valueOf(1), Integer.valueOf(2)).getComparator();
        final byte[] forged = forgeRangeStream(comparator, null, max, canonicalHash);
        assertThrows(InvalidObjectException.class, () -> deserialize(forged));
    }

    @Test
    void testRoundTripPreservesCorrectHashCode() throws Exception {
        final Range<String> range = Range.of("apple", "mango");
        final Range<String> roundtrip = SerializationUtils.roundtrip(range);
        assertEquals(range.hashCode(), roundtrip.hashCode(), "Round-trip serialization must preserve the correct hashCode");
        assertEquals(range, roundtrip);
    }

}
