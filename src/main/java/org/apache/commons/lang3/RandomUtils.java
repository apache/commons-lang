/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
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
 * Utility library that supplements the standard {@link Random} class.
 * 
 * @since 3.3
 *
 * @version $Id$
 */
public class RandomUtils {

    /**
     * Random object used by random method. This has to be not local to the
     * random method so as to not return the same value in the same millisecond.
     */
    private static final Random RANDOM = new Random();

    /**
     * <p>
     * {@code RandomUtils} instances should NOT be constructed in standard
     * programming. Instead, the class should be used as
     * {@code RandomUtils.nextBytes(5);}.
     * </p>
     * 
     * <p>
     * This constructor is public to permit tools that require a JavaBean
     * instance to operate.
     * </p>
     */
    public RandomUtils() {
        super();
    }

    /**
     * Creates a array of the specified length filled with random bytes.
     * 
     * @param count
     *            the size of the returned array
     * @return the random byte array
     */
    public static byte[] nextBytes(int count) {
        Validate.isTrue(count >= 0, "Count cannot be negative.");

        byte[] result = new byte[count];
        RANDOM.nextBytes(result);
        return result;
    }

    /**
     * Returns a random integer within the specified range.
     * 
     * @param startInclusive
     *            the smallest value that can be returned, must be non-negative
     * @param endExclusive
     *            the upper bound (not included), must be non-negative
     * @throws IllegalArgumentException
     *             if {@code startInclusive > endInclusive}
     * @return the random integer
     */
    public static int nextInt(int startInclusive, int endExclusive) {
        Validate.isTrue(endExclusive >= startInclusive,
                "Start value must be smaller or equal to end value.");
        Validate.isTrue(startInclusive >= 0, "Both range values must be non-negative.");

        if (startInclusive == endExclusive) {
            return startInclusive;
        }
        
        return startInclusive + RANDOM.nextInt(endExclusive - startInclusive);
    }
    
    /**
     * Returns a random long within the specified range.
     * 
     * @param startInclusive
     *            the smallest value that can be returned, must be non-negative
     * @param endExclusive
     *            the upper bound (not included), must be non-negative
     * @throws IllegalArgumentException
     *             if {@code startInclusive > endInclusive}
     * @return the random long
     */
    public static long nextLong(long startInclusive, long endExclusive) {
        Validate.isTrue(endExclusive >= startInclusive,
                "Start value must be smaller or equal to end value.");
        Validate.isTrue(startInclusive >= 0, "Both range values must be non-negative.");

        if (startInclusive == endExclusive) {
            return startInclusive;
        }

        return (long) nextDouble(startInclusive, endExclusive);
    }    
    
    
    /**
     * Returns a random double within the specified range.
     * 
     * @param startInclusive
     *            the smallest value that can be returned, must be non-negative
     * @param endInclusive
     *            the upper bound (included), must be non-negative
     * @throws IllegalArgumentException
     *             if {@code startInclusive > endInclusive}
     * @return the random double
     */
    public static double nextDouble(double startInclusive, double endInclusive) {
        Validate.isTrue(endInclusive >= startInclusive,
                "Start value must be smaller or equal to end value.");
        Validate.isTrue(startInclusive >= 0, "Both range values must be non-negative.");
        
        if (startInclusive == endInclusive) {
            return startInclusive;
        }
        
        return startInclusive + ((endInclusive - startInclusive) * RANDOM.nextDouble());
    }
    
    /**
     * Returns a random float within the specified range.
     * 
     * @param startInclusive
     *            the smallest value that can be returned, must be non-negative
     * @param endInclusive
     *            the upper bound (included), must be non-negative
     * @throws IllegalArgumentException
     *             if {@code startInclusive > endInclusive}
     * @return the random float
     */
    public static float nextFloat(float startInclusive, float endInclusive) {
        Validate.isTrue(endInclusive >= startInclusive,
                "Start value must be smaller or equal to end value.");
        Validate.isTrue(startInclusive >= 0, "Both range values must be non-negative.");
        
        if (startInclusive == endInclusive) {
            return startInclusive;
        }
        
        return startInclusive + ((endInclusive - startInclusive) * RANDOM.nextFloat());
    }    
}
