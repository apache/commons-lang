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

package org.apache.commons.lang3.time;

import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.ObjectStreamClass;
import java.io.Serializable;
import java.util.Locale;
import java.util.TimeZone;

import org.junit.jupiter.api.Test;

/**
 * Tests that a deserialized {@link FastDateParser} rejects null {@code pattern} and null {@code timeZone} fields.
 *
 * <p>
 * The two null-checks were introduced in {@link FastDateParser#readObject(ObjectInputStream)}:
 * </p>
 * <ul>
 * <li>{@code if (pattern == null) throw new InvalidObjectException("pattern null");}</li>
 * <li>{@code if (timeZone == null) throw new InvalidObjectException("timeZone null");}</li>
 * </ul>
 *
 * <p>
 * Because neither null value can reach {@code readObject} through the normal public API, the tests forge a malicious serialization stream. A
 * {@link FastDateParserForge} helper carries the same non-transient field set as {@link FastDateParser} (same names, same types, same {@code serialVersionUID})
 * but allows null values. A custom {@link ObjectOutputStream} sub-class rewrites the class-descriptor name to {@code FastDateParser} so the stream is accepted
 * by {@link ObjectInputStream} as a {@link FastDateParser} payload; {@code defaultReadObject} then assigns the forged values to the actual
 * {@link FastDateParser} fields, triggering the null checks.
 * </p>
 */
class FastDateParserReadObjectTest {

    /**
     * Forge carrier: same non-transient fields as {@link FastDateParser}, in the same alphabetical order used by Java default serialization ({@code century},
     * {@code locale}, {@code pattern}, {@code startYear}, {@code timeZone}), with the same {@code serialVersionUID}. Allows null for {@code pattern} and
     * {@code timeZone}.
     */
    private static final class FastDateParserForge implements Serializable {

        /** Must match {@link FastDateParser#serialVersionUID}. */
        private static final long serialVersionUID = 3L;
        // Fields must match FastDateParser's non-transient fields by name and type.
        private final int century;
        private final Locale locale;
        private final String pattern;
        private final int startYear;
        private final TimeZone timeZone;

        FastDateParserForge(final String pattern, final TimeZone timeZone, final Locale locale, final int century, final int startYear) {
            this.pattern = pattern;
            this.timeZone = timeZone;
            this.locale = locale;
            this.century = century;
            this.startYear = startYear;
        }
    }

    /**
     * Deserializes {@code bytes} and returns the resulting object.
     *
     * @param bytes serialized form
     * @return the deserialized object
     * @throws IOException            if an I/O error occurs
     * @throws ClassNotFoundException if the class of the serialized object cannot be found
     */
    private static Object deserialize(final byte[] bytes) throws IOException, ClassNotFoundException {
        try (ObjectInputStream ois = new ObjectInputStream(new ByteArrayInputStream(bytes))) {
            return ois.readObject();
        }
    }

    /**
     * Serializes a {@link FastDateParserForge} but rewrites the class descriptor so that the resulting stream is treated as a {@link FastDateParser} during
     * deserialization.
     *
     * @param forge the forge instance to serialize
     * @return a byte array whose class descriptor names {@link FastDateParser}
     * @throws IOException if an I/O error occurs
     */
    private static byte[] forgeStream(final FastDateParserForge forge) throws IOException {
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try (ObjectOutputStream oos = new ObjectOutputStream(baos) {

            @Override
            protected void writeClassDescriptor(final ObjectStreamClass desc) throws IOException {
                if (desc.getName().equals(FastDateParserForge.class.getName())) {
                    // Spoof the class descriptor so the stream deserializes as FastDateParser.
                    super.writeClassDescriptor(ObjectStreamClass.lookup(FastDateParser.class));
                } else {
                    super.writeClassDescriptor(desc);
                }
            }
        }) {
            oos.writeObject(forge);
        }
        return baos.toByteArray();
    }

    /**
     * Tests that a forged stream whose {@code pattern} field is {@code null} is rejected with {@link InvalidObjectException}.
     */
    @Test
    void testNullPatternRejected() throws IOException {
        final FastDateParserForge forge = new FastDateParserForge(null, // pattern = null (the evil value under test)
                TimeZone.getTimeZone("GMT"), Locale.US, 1900, 0);
        final byte[] forgedBytes = forgeStream(forge);
        assertThrows(InvalidObjectException.class, () -> deserialize(forgedBytes), "A null pattern must be rejected with InvalidObjectException");
    }

    /**
     * Tests that a forged stream whose {@code timeZone} field is {@code null} is rejected with {@link InvalidObjectException}.
     */
    @Test
    void testNullTimeZoneRejected() throws IOException {
        final FastDateParserForge forge = new FastDateParserForge("yyyy-MM-dd", null, // timeZone = null (the evil value under test)
                Locale.US, 1900, 0);
        final byte[] forgedBytes = forgeStream(forge);
        assertThrows(InvalidObjectException.class, () -> deserialize(forgedBytes), "A null timeZone must be rejected with InvalidObjectException");
    }
}
