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
package org.apache.commons.lang3;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.Objects;
import java.util.Random;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Tests {@link CachedRandomBits}.
 */
class CachedRandomBitsTest {

    /** MockRandom mocks a Random class nextBytes to use a specific list of outputs */
    private static class MockRandom extends Random {

        private static final long serialVersionUID = 1L;
        private final byte[] outputs;
        private int index;

        MockRandom(final byte[] outputs) {
            this.outputs = outputs.clone();
            this.index = 0;
        }

        @Override
        public void nextBytes(final byte[] bytes) {
            Objects.requireNonNull(bytes, "bytes");
            if (index + bytes.length > outputs.length) {
                throw new IllegalStateException("Not enough outputs given in MockRandom");
            }
            System.arraycopy(outputs, index, bytes, 0, bytes.length);
            index += bytes.length;
        }
    }

    @ParameterizedTest
    @ValueSource(ints = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 32})
    void testNext(final int cacheSize) {
        final MockRandom random = new MockRandom(new byte[]{
                0x11, 0x12, 0x13, 0x25,
                (byte) 0xab, (byte) 0xcd, (byte) 0xef, (byte) 0xff,
                0x55, 0x44, 0x12, 0x34,
                0x56, 0x78, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00,
        });

        final CachedRandomBits arb = new CachedRandomBits(cacheSize, random);

        assertThrows(IllegalArgumentException.class, () -> arb.nextBits(0));
        assertThrows(IllegalArgumentException.class, () -> arb.nextBits(33));

        assertEquals(0x11, arb.nextBits(8));
        assertEquals(0x12, arb.nextBits(8));
        assertEquals(0x1325, arb.nextBits(16));

        assertEquals(0xabcdefff, arb.nextBits(32));

        assertEquals(0x5, arb.nextBits(4));
        assertEquals(0x1, arb.nextBits(1));
        assertEquals(0x0, arb.nextBits(1));
        assertEquals(0x1, arb.nextBits(2));

        assertEquals(0x4, arb.nextBits(6));

        assertEquals(0x40000000 | 0x12345600 >> 2 | 0x38, arb.nextBits(32));

        assertEquals(1, arb.nextBits(1));
        assertEquals(0, arb.nextBits(1));
        assertEquals(0, arb.nextBits(9));
        assertEquals(0, arb.nextBits(31));
    }
}
