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
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.Base64;
import java.util.Collections;
import java.util.Comparator;
import java.util.Objects;

import org.apache.commons.lang3.reflect.FieldUtils;
import org.junit.jupiter.api.Test;

/**
 * Tests range invariants and hash code reconstruction during deserialization.
 */
class RangeReadObjectTest {

    private static final class ChangingHashEndpoint extends IdentityEndpoint {
        private static final long serialVersionUID = 1L;
        private transient int hash = 123;

        ChangingHashEndpoint(final int value) {
            super(value);
        }

        @Override
        public boolean equals(final Object other) {
            return this == other;
        }

        @Override
        public int hashCode() {
            return hash;
        }
    }

    private static class IdentityEndpoint implements Serializable, Comparable<IdentityEndpoint> {
        private static final long serialVersionUID = 1L;
        private final int value;

        IdentityEndpoint(final int value) {
            this.value = value;
        }

        @Override
        public int compareTo(final IdentityEndpoint other) {
            return Integer.compare(value, other.value);
        }
    }

    /**
     * Standin class used only to drive {@link ObjectOutputStream#writeObject(Object)} into emitting a stream that matches the wire format of {@link Range} but
     * with caller-controlled field values. The class name and serialVersionUID are spoofed in the stream below via a custom {@link ObjectOutputStream} subclass
     * so the stream reads back as a {@code Range}.
     */
    private static final class RangeForge implements Serializable {

        private static final long serialVersionUID = 1L; // matches Range.serialVersionUID
        private final Object comparator;
        private final Object maximum;
        private final Object minimum;

        RangeForge(final Object comparator, final Object minimum, final Object maximum) {
            this.comparator = comparator;
            this.minimum = minimum;
            this.maximum = maximum;
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
    private static byte[] forgeRangeStream(final Object comparator, final Object minimum, final Object maximum) throws IOException {
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
            oos.writeObject(new RangeForge(comparator, minimum, maximum));
        }
        return baos.toByteArray();
    }

    @Test
    void testCachedHashCodeRecomputed() throws Exception {
        final Range<Integer> range = Range.of(1, 100);
        FieldUtils.writeDeclaredField(range, "hashCode", 0xDEADBEEF, true);
        final Range<Integer> copy = SerializationUtils.roundtrip(range);
        assertEquals(Range.of(1, 100).hashCode(), copy.hashCode());
        assertEquals(range, copy);
    }

    @Test
    void testCloneIdentityHashEndpoints() {
        final Range<IdentityEndpoint> original = Range.of(new IdentityEndpoint(1), new IdentityEndpoint(2));
        final Range<IdentityEndpoint> copy = SerializationUtils.clone(original);
        assertNotSame(original.getMinimum(), copy.getMinimum());
        assertNotSame(original.getMaximum(), copy.getMaximum());
        assertEquals(0, original.getMinimum().compareTo(copy.getMinimum()));
        assertEquals(0, original.getMaximum().compareTo(copy.getMaximum()));
        assertEquals(Objects.hash(copy.getMinimum(), copy.getMaximum()), copy.hashCode());
    }

    @Test
    void testComparatorNullViaForgedStream() throws Exception {
        final Integer min = Integer.valueOf(1);
        final Integer max = Integer.valueOf(10);
        final byte[] forged = forgeRangeStream(null, min, max);
        assertThrows(InvalidObjectException.class, () -> deserialize(forged));
    }

    @Test
    void testDeserializeVersion320() {
        // Streams generated using Commons Lang 3.20.0, with endpoints 1 and 2.
        final Range<?>[] expected = {Range.of(1, 2), IntegerRange.of(1, 2), LongRange.of(1, 2), DoubleRange.of(1, 2)};
        final String[] streams = {
            "rO0ABXNyAB5vcmcuYXBhY2hlLmNvbW1vbnMubGFuZzMuUmFuZ2UAAAAAAAAAAQIAA0wACmNvbXBhcmF0b3J0ABZMamF2YS91dGls" +
                "L0NvbXBhcmF0b3I7TAAHbWF4aW11bXQAEkxqYXZhL2xhbmcvT2JqZWN0O0wAB21pbmltdW1xAH4AAnhwfnIAM29yZy5hcGFjaGUu" +
                "Y29tbW9ucy5sYW5nMy5SYW5nZSRDb21wYXJhYmxlQ29tcGFyYXRvcgAAAAAAAAAAEgAAeHIADmphdmEubGFuZy5FbnVtAAAAAAAA" +
                "AAASAAB4cHQACElOU1RBTkNFc3IAEWphdmEubGFuZy5JbnRlZ2VyEuKgpPeBhzgCAAFJAAV2YWx1ZXhyABBqYXZhLmxhbmcuTnVt" +
                "YmVyhqyVHQuU4IsCAAB4cAAAAAJzcQB+AAgAAAAB",
            "rO0ABXNyACVvcmcuYXBhY2hlLmNvbW1vbnMubGFuZzMuSW50ZWdlclJhbmdlAAAAAAAAAAECAAB4cgAkb3JnLmFwYWNoZS5jb21t" +
                "b25zLmxhbmczLk51bWJlclJhbmdlAAAAAAAAAAECAAB4cgAeb3JnLmFwYWNoZS5jb21tb25zLmxhbmczLlJhbmdlAAAAAAAAAAEC" +
                "AANMAApjb21wYXJhdG9ydAAWTGphdmEvdXRpbC9Db21wYXJhdG9yO0wAB21heGltdW10ABJMamF2YS9sYW5nL09iamVjdDtMAAdt" +
                "aW5pbXVtcQB+AAR4cH5yADNvcmcuYXBhY2hlLmNvbW1vbnMubGFuZzMuUmFuZ2UkQ29tcGFyYWJsZUNvbXBhcmF0b3IAAAAAAAAA" +
                "ABIAAHhyAA5qYXZhLmxhbmcuRW51bQAAAAAAAAAAEgAAeHB0AAhJTlNUQU5DRXNyABFqYXZhLmxhbmcuSW50ZWdlchLioKT3gYc4" +
                "AgABSQAFdmFsdWV4cgAQamF2YS5sYW5nLk51bWJlcoaslR0LlOCLAgAAeHAAAAACc3EAfgAKAAAAAQ==",
            "rO0ABXNyACJvcmcuYXBhY2hlLmNvbW1vbnMubGFuZzMuTG9uZ1JhbmdlAAAAAAAAAAECAAB4cgAkb3JnLmFwYWNoZS5jb21tb25z" +
                "LmxhbmczLk51bWJlclJhbmdlAAAAAAAAAAECAAB4cgAeb3JnLmFwYWNoZS5jb21tb25zLmxhbmczLlJhbmdlAAAAAAAAAAECAANM" +
                "AApjb21wYXJhdG9ydAAWTGphdmEvdXRpbC9Db21wYXJhdG9yO0wAB21heGltdW10ABJMamF2YS9sYW5nL09iamVjdDtMAAdtaW5p" +
                "bXVtcQB+AAR4cH5yADNvcmcuYXBhY2hlLmNvbW1vbnMubGFuZzMuUmFuZ2UkQ29tcGFyYWJsZUNvbXBhcmF0b3IAAAAAAAAAABIA" +
                "AHhyAA5qYXZhLmxhbmcuRW51bQAAAAAAAAAAEgAAeHB0AAhJTlNUQU5DRXNyAA5qYXZhLmxhbmcuTG9uZzuL5JDMjyPfAgABSgAF" +
                "dmFsdWV4cgAQamF2YS5sYW5nLk51bWJlcoaslR0LlOCLAgAAeHAAAAAAAAAAAnNxAH4ACgAAAAAAAAAB",
            "rO0ABXNyACRvcmcuYXBhY2hlLmNvbW1vbnMubGFuZzMuRG91YmxlUmFuZ2UAAAAAAAAAAQIAAHhyACRvcmcuYXBhY2hlLmNvbW1v" +
                "bnMubGFuZzMuTnVtYmVyUmFuZ2UAAAAAAAAAAQIAAHhyAB5vcmcuYXBhY2hlLmNvbW1vbnMubGFuZzMuUmFuZ2UAAAAAAAAAAQIA" +
                "A0wACmNvbXBhcmF0b3J0ABZMamF2YS91dGlsL0NvbXBhcmF0b3I7TAAHbWF4aW11bXQAEkxqYXZhL2xhbmcvT2JqZWN0O0wAB21p" +
                "bmltdW1xAH4ABHhwfnIAM29yZy5hcGFjaGUuY29tbW9ucy5sYW5nMy5SYW5nZSRDb21wYXJhYmxlQ29tcGFyYXRvcgAAAAAAAAAA" +
                "EgAAeHIADmphdmEubGFuZy5FbnVtAAAAAAAAAAASAAB4cHQACElOU1RBTkNFc3IAEGphdmEubGFuZy5Eb3VibGWAs8JKKWv7BAIA" +
                "AUQABXZhbHVleHIAEGphdmEubGFuZy5OdW1iZXKGrJUdC5TgiwIAAHhwQAAAAAAAAABzcQB+AAo/8AAAAAAAAA=="
        };
        for (int i = 0; i < streams.length; i++) {
            final Range<?> actual = SerializationUtils.deserialize(Base64.getDecoder().decode(streams[i]));
            assertEquals(expected[i].getClass(), actual.getClass());
            assertEquals(expected[i], actual);
            assertEquals(expected[i].hashCode(), actual.hashCode());
        }
    }

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

    @Test
    void testMaximumNullViaForgedStream() throws Exception {
        final Integer min = Integer.valueOf(1);
        final Object comparator = Range.of(Integer.valueOf(1), Integer.valueOf(2)).getComparator();
        final byte[] forged = forgeRangeStream(comparator, min, null);
        assertThrows(InvalidObjectException.class, () -> deserialize(forged));
    }

    @Test
    void testMinimumNullViaForgedStream() throws Exception {
        final Integer max = Integer.valueOf(10);
        // comparator must be non-null here so we isolate the minimum-null gap.
        // We use ComparableComparator.INSTANCE via deserialization round-trip of a real Range.
        final Object comparator = Range.of(Integer.valueOf(1), Integer.valueOf(2)).getComparator();
        final byte[] forged = forgeRangeStream(comparator, null, max);
        assertThrows(InvalidObjectException.class, () -> deserialize(forged));
    }


    @Test
    void testNaNEndpointViaForgedStream() throws Exception {
        final Double min = Double.valueOf(5.0);
        final Double max = Double.valueOf(Double.NaN);
        final Object comparator = Range.of(Integer.valueOf(1), Integer.valueOf(2)).getComparator();
        final byte[] forged = forgeRangeStream(comparator, min, max);
        assertThrows(InvalidObjectException.class, () -> deserialize(forged));
    }

    @Test
    void testRoundTripChangingHashEndpoints() {
        final Range<ChangingHashEndpoint> original = Range.of(new ChangingHashEndpoint(1), new ChangingHashEndpoint(2));
        final Range<ChangingHashEndpoint> copy = SerializationUtils.roundtrip(original);
        assertEquals(123, original.getMinimum().hashCode());
        assertEquals(0, copy.getMinimum().hashCode());
        assertEquals(Objects.hash(copy.getMinimum(), copy.getMaximum()), copy.hashCode());
    }

    @Test
    void testRoundTripPreservesCorrectHashCode() throws Exception {
        final Range<String> range = Range.of("apple", "mango");
        final Range<String> roundtrip = SerializationUtils.roundtrip(range);
        assertEquals(range.hashCode(), roundtrip.hashCode(), "Round-trip serialization must preserve the correct hashCode");
        assertEquals(range, roundtrip);
    }

}
