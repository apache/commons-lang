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

import java.util.Objects;
import java.util.Random;

/**
 * Generates random integers of specific bit length.
 *
 * <p>
 * It is more efficient than calling Random.nextInt(1 << nbBits). It uses a cache of cacheSize random bytes that it replenishes when it gets empty. This is
 * especially beneficial for SecureRandom Drbg implementations, which incur a constant cost at each randomness generation.
 * </p>
 *
 * <p>
 * Used internally by RandomStringUtils.
 * </p>
 *
 * <p>
 * #NotThreadSafe#
 * </p>
 */
final class CachedRandomBits {

    /**
     * The maximum size of the cache.
     *
     * <p>
     * This is to prevent the possibility of overflow in the {@code if (bitIndex >> 3 >= cache.length)} in the {@link #nextBits(int)} method.
     * </p>
     */
    private static final int MAX_CACHE_SIZE = Integer.MAX_VALUE >> 3;

    /** Maximum number of bits that can be generated (size of an int) */
    private static final int MAX_BITS = 32;

    /** Mask to extract the bit offset within a byte (0-7) */
    private static final int BIT_INDEX_MASK = 0x7;

    /** Number of bits in a byte */
    private static final int BITS_PER_BYTE = 8;
    private final Random random;
    private final byte[] cache;
    /**
     * Index of the next bit in the cache to be used.
     *
     * <ul>
     * <li>bitIndex=0 means the cache is fully random and none of the bits have been used yet.</li>
     * <li>bitIndex=1 means that only the LSB of cache[0] has been used and all other bits can be used.</li>
     * <li>bitIndex=8 means that only the 8 bits of cache[0] has been used.</li>
     * </ul>
     */
    private int bitIndex;
    /**
     * Creates a new instance.
     *
     * @param cacheSize number of bytes cached (only affects performance)
     * @param random random source
     */
    CachedRandomBits(final int cacheSize, final Random random) {
        if (cacheSize <= 0) {
            throw new IllegalArgumentException("cacheSize must be positive");
        }
        this.cache = cacheSize <= MAX_CACHE_SIZE ? new byte[cacheSize] : new byte[MAX_CACHE_SIZE];
        this.random = Objects.requireNonNull(random, "random");
        this.random.nextBytes(this.cache);
        this.bitIndex = 0;
    }

    /**
     * Generates a random integer with the specified number of bits.
     *
     * <p>This method efficiently generates random bits by using a byte cache and bit manipulation:
     * <ul>
     *   <li>Uses a byte array cache to avoid frequent calls to the underlying random number generator</li>
     *   <li>Extracts bits from each byte using bit shifting and masking</li>
     *   <li>Handles partial bytes to avoid wasting random bits</li>
     *   <li>Accumulates bits until the requested number is reached</li>
     * </ul>
     * </p>
     *
     * @param bits number of bits to generate, MUST be between 1 and 32 (inclusive)
     * @return random integer containing exactly the requested number of random bits
     * @throws IllegalArgumentException if bits is not between 1 and 32
     */
    public int nextBits(final int bits) {
        if (bits > MAX_BITS || bits <= 0) {
            throw new IllegalArgumentException("number of bits must be between 1 and " + MAX_BITS);
        }
        int result = 0;
        int generatedBits = 0; // number of generated bits up to now
        while (generatedBits < bits) {
            // Check if we need to refill the cache
            // Convert bitIndex to byte index by dividing by 8 (right shift by 3)
            if (bitIndex >> 3 >= cache.length) {
                // We exhausted the number of bits in the cache
                // This should only happen if the bitIndex is exactly matching the cache length
                assert bitIndex == cache.length * BITS_PER_BYTE;
                random.nextBytes(cache);
                bitIndex = 0;
            }
            // Calculate how many bits we can extract from the current byte
            // 1. Get current position within byte (0-7) using bitIndex & 0x7
            // 2. Calculate remaining bits in byte: 8 - (position within byte)
            // 3. Take minimum of remaining bits in byte and bits still needed
            final int generatedBitsInIteration = Math.min(
                BITS_PER_BYTE - (bitIndex & BIT_INDEX_MASK),
                bits - generatedBits);
            // Shift existing result left to make room for new bits
            result = result << generatedBitsInIteration;
            // Extract and append new bits:
            // 1. Get byte from cache (bitIndex >> 3 converts bit index to byte index)
            // 2. Shift right by bit position within byte (bitIndex & 0x7)
            // 3. Mask to keep only the bits we want ((1 << generatedBitsInIteration) - 1)
            result |= cache[bitIndex >> 3] >> (bitIndex & BIT_INDEX_MASK) & ((1 << generatedBitsInIteration) - 1);
            // Update counters
            generatedBits += generatedBitsInIteration;
            bitIndex += generatedBitsInIteration;
        }
        return result;
    }
}
