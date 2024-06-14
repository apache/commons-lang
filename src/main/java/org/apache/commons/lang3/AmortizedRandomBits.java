/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang3;

import java.util.Random;

/**
 * AmortizedRandomBits enable to generate random integers of specific bit length.
 *
 * <p>It is more efficient way than calling Random.nextInt(1 << nbBits). It uses a cache of
 * cacheSize random bytes that it replenishes when it gets empty. This is especially beneficial for
 * SecureRandom Drbg implementations that incur a constant cost at each randomness generation. It is
 * not thread safe.
 *
 * <p>Used internally by RandomStringUtils.
 */
class AmortizedRandomBits {
    private final Random random;

    private final byte[] cache;

    // bitIndex is the index of the next bit in the cache to be used
    // bitIndex=0 means the cache is fully random and none of the bits have been used yet
    // bitIndex=1 means that only the LSB of cache[0] has been used and all other bits can be used
    // bitIndex=8 means that only the 8 bits of cache[0] has been used
    private int bitIndex;

    /**
     * @param cacheSize number of bytes cached (only affects performance)
     * @param random random source
     */
    AmortizedRandomBits(final int cacheSize, final Random random) {
        if (cacheSize <= 0) {
            throw new IllegalArgumentException("cacheSize must be positive");
        }
        this.cache = new byte[cacheSize];
        this.random = random;
        this.random.nextBytes(this.cache);
        this.bitIndex = 0;
    }

    /**
     * nextBits returns a random integer with the number of bits specified
     *
     * @param bits number of bits to generate, MUST be between 1 and 32
     * @return random integer with {@code bits} bits
     */
    public int nextBits(final int bits) {
        if (bits > 32 || bits <= 0) {
            throw new IllegalArgumentException("number of bits must be between 1 and 32");
        }

        int result = 0;
        int generatedBits = 0; // number of generated bits up to now

        while (generatedBits < bits) {
            if (bitIndex / 8 >= cache.length) {
                // we exhausted the number of bits in the cache
                // this should only happen if the bitIndex is exactly matching the cache length
                assert bitIndex == cache.length * 8;
                random.nextBytes(cache);
                bitIndex = 0;
            }

            // generatedBitsInIteration is the number of bits that we will generate
            // in this iteration of the while loop
            int generatedBitsInIteration = Math.min(8 - (bitIndex % 8), bits - generatedBits);

            result = result << generatedBitsInIteration;
            result |= (cache[bitIndex / 8] >> (bitIndex % 8)) & ((1 << generatedBitsInIteration) - 1);

            generatedBits += generatedBitsInIteration;
            bitIndex += generatedBitsInIteration;
        }

        return result;
    }
}
