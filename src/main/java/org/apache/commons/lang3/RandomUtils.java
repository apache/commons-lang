/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang3;

import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.security.Security;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;
import java.util.function.Supplier;

import org.apache.commons.lang3.exception.UncheckedException;

/**
 * Supplements the standard {@link Random} class.
 * <p>
 * Use {@link #secure()} to get the singleton instance based on {@link SecureRandom#SecureRandom()} which uses a secure random number generator implementing the
 * default random number algorithm.
 * </p>
 * <p>
 * Use {@link #secureStrong()} to get the singleton instance based on {@link SecureRandom#getInstanceStrong()} which uses an instance that was selected by using
 * the algorithms/providers specified in the {@code securerandom.strongAlgorithms} {@link Security} property.
 * </p>
 * <p>
 * Use {@link #insecure()} to get the singleton instance based on {@link ThreadLocalRandom#current()} <strong>which is not cryptographically secure</strong>. In addition,
 * instances do not use a cryptographically random seed unless the {@linkplain System#getProperty system property} {@code java.util.secureRandomSeed} is set to
 * {@code true}.
 * </p>
 * <p>
 * Starting in version 3.17.0, the method {@link #secure()} uses {@link SecureRandom#SecureRandom()} instead of {@link SecureRandom#getInstanceStrong()}, and
 * adds {@link #secureStrong()}.
 * </p>
 * <p>
 * Starting in version 3.16.0, this class uses {@link #secure()} for static methods and adds {@link #insecure()}.
 * </p>
 * <p>
 * Starting in version 3.15.0, this class uses {@link SecureRandom#getInstanceStrong()} for static methods.
 * </p>
 * <p>
 * Before version 3.15.0, this class used {@link ThreadLocalRandom#current()} for static methods, which is not cryptographically secure.
 * </p>
 * <p>
 * Please note that the Apache Commons project provides a component dedicated to pseudo-random number generation, namely
 * <a href="https://commons.apache.org/proper/commons-rng/">Commons RNG</a>, that may be a better choice for applications with more stringent requirements
 * (performance and/or correctness).
 * </p>
 *
 * @see #secure()
 * @see #secureStrong()
 * @see #insecure()
 * @see SecureRandom#SecureRandom()
 * @see SecureRandom#getInstanceStrong()
 * @see ThreadLocalRandom#current()
 * @see RandomStringUtils
 * @since 3.3
 */
public class RandomUtils {

    private static final RandomUtils INSECURE = new RandomUtils(ThreadLocalRandom::current);

    private static final RandomUtils SECURE = new RandomUtils(SecureRandom::new);

    private static final Supplier<Random> SECURE_STRONG_SUPPLIER = () -> RandomUtils.SECURE_RANDOM_STRONG.get();

    private static final RandomUtils SECURE_STRONG = new RandomUtils(SECURE_STRONG_SUPPLIER);

    private static final ThreadLocal<SecureRandom> SECURE_RANDOM_STRONG = ThreadLocal.withInitial(() -> {
        try {
            return SecureRandom.getInstanceStrong();
        } catch (final NoSuchAlgorithmException e) {
            throw new UncheckedException(e);
        }
    });

    /**
     * Gets the singleton instance based on {@link ThreadLocalRandom#current()}; <b>which is not cryptographically
     * secure</b>; for more secure processing use {@link #secure()} or {@link #secureStrong()}.
     * <p>
     * The method {@link ThreadLocalRandom#current()} is called on-demand.
     * </p>
     *
     * @return the singleton instance based on {@link ThreadLocalRandom#current()}.
     * @see ThreadLocalRandom#current()
     * @see #secure()
     * @see #secureStrong()
     * @since 3.17.0
     */
    public static RandomUtils insecure() {
        return INSECURE;
    }

    /**
     * Generates a random boolean value.
     *
     * @return the random boolean.
     * @since 3.5
     * @deprecated Use {@link #secure()}, {@link #secureStrong()}, or {@link #insecure()}.
     */
    @Deprecated
    public static boolean nextBoolean() {
        return secure().randomBoolean();
    }

    /**
     * Generates an array of random bytes.
     *
     * @param count the size of the returned array.
     * @return the random byte array.
     * @throws IllegalArgumentException if {@code count} is negative.
     * @deprecated Use {@link #secure()}, {@link #secureStrong()}, or {@link #insecure()}.
     */
    @Deprecated
    public static byte[] nextBytes(final int count) {
        return secure().randomBytes(count);
    }

    /**
     * Generates a random double between 0 (inclusive) and Double.MAX_VALUE (exclusive).
     *
     * @return the random double.
     * @see #nextDouble(double, double)
     * @since 3.5
     * @deprecated Use {@link #secure()}, {@link #secureStrong()}, or {@link #insecure()}.
     */
    @Deprecated
    public static double nextDouble() {
        return secure().randomDouble();
    }

    /**
     * Generates a random double within the specified range.
     *
     * @param startInclusive the smallest value that can be returned, must be non-negative.
     * @param endExclusive   the upper bound (not included).
     * @throws IllegalArgumentException if {@code startInclusive > endExclusive} or if {@code startInclusive} is negative.
     * @return the random double.
     * @deprecated Use {@link #secure()}, {@link #secureStrong()}, or {@link #insecure()}.
     */
    @Deprecated
    public static double nextDouble(final double startInclusive, final double endExclusive) {
        return secure().randomDouble(startInclusive, endExclusive);
    }

    /**
     * Generates a random float between 0 (inclusive) and Float.MAX_VALUE (exclusive).
     *
     * @return the random float.
     * @see #nextFloat(float, float)
     * @since 3.5
     * @deprecated Use {@link #secure()}, {@link #secureStrong()}, or {@link #insecure()}.
     */
    @Deprecated
    public static float nextFloat() {
        return secure().randomFloat();
    }

    /**
     * Generates a random float within the specified range.
     *
     * @param startInclusive the smallest value that can be returned, must be non-negative.
     * @param endExclusive   the upper bound (not included).
     * @throws IllegalArgumentException if {@code startInclusive > endExclusive} or if {@code startInclusive} is negative.
     * @return the random float.
     * @deprecated Use {@link #secure()}, {@link #secureStrong()}, or {@link #insecure()}.
     */
    @Deprecated
    public static float nextFloat(final float startInclusive, final float endExclusive) {
        return secure().randomFloat(startInclusive, endExclusive);
    }

    /**
     * Generates a random int between 0 (inclusive) and Integer.MAX_VALUE (exclusive).
     *
     * @return the random integer.
     * @see #nextInt(int, int)
     * @since 3.5
     * @deprecated Use {@link #secure()}, {@link #secureStrong()}, or {@link #insecure()}.
     */
    @Deprecated
    public static int nextInt() {
        return secure().randomInt();
    }

    /**
     * Generates a random integer within the specified range.
     *
     * @param startInclusive the smallest value that can be returned, must be non-negative.
     * @param endExclusive   the upper bound (not included).
     * @throws IllegalArgumentException if {@code startInclusive > endExclusive} or if {@code startInclusive} is negative.
     * @return the random integer.
     * @deprecated Use {@link #secure()}, {@link #secureStrong()}, or {@link #insecure()}.
     */
    @Deprecated
    public static int nextInt(final int startInclusive, final int endExclusive) {
        return secure().randomInt(startInclusive, endExclusive);
    }

    /**
     * Generates a random long between 0 (inclusive) and Long.MAX_VALUE (exclusive).
     *
     * @return the random long.
     * @see #nextLong(long, long)
     * @since 3.5
     * @deprecated Use {@link #secure()}, {@link #secureStrong()}, or {@link #insecure()}.
     */
    @Deprecated
    public static long nextLong() {
        return secure().randomLong();
    }

    /**
     * Generates a random long within the specified range.
     *
     * @param startInclusive the smallest value that can be returned, must be non-negative.
     * @param endExclusive   the upper bound (not included).
     * @throws IllegalArgumentException if {@code startInclusive > endExclusive} or if {@code startInclusive} is negative.
     * @return the random long.
     * @deprecated Use {@link #secure()}, {@link #secureStrong()}, or {@link #insecure()}.
     */
    @Deprecated
    public static long nextLong(final long startInclusive, final long endExclusive) {
        return secure().randomLong(startInclusive, endExclusive);
    }

    /**
     * Gets the singleton instance based on {@link SecureRandom#SecureRandom()} which uses the default algorithm
     * and provider of {@link SecureRandom}.
     * <p>
     * The method {@link SecureRandom#SecureRandom()} is called on-demand.
     * </p>
     *
     * @return the singleton instance based on {@link SecureRandom#SecureRandom()}.
     * @see SecureRandom#SecureRandom()
     * @since 3.16.0
     */
    public static RandomUtils secure() {
        return SECURE;
    }

    static SecureRandom secureRandom() {
        return SECURE_RANDOM_STRONG.get();
    }

    /**
     * Gets the singleton instance based on {@link SecureRandom#getInstanceStrong()} which uses an algorithms/providers
     * specified in the {@code securerandom.strongAlgorithms} {@link Security} property.
     * <p>
     * The method {@link SecureRandom#getInstanceStrong()} is called on-demand.
     * </p>
     *
     * @return the singleton instance based on {@link SecureRandom#getInstanceStrong()}.
     * @see SecureRandom#getInstanceStrong()
     * @since 3.17.0
     */
    public static RandomUtils secureStrong() {
        return SECURE_STRONG;
    }

    private final Supplier<Random> random;

    /**
     * {@link RandomUtils} instances should NOT be constructed in standard programming. Instead, the class should be
     * used as {@code RandomUtils.nextBytes(5);}.
     * <p>
     * This constructor is public to permit tools that require a JavaBean instance to operate.
     * </p>
     *
     * @deprecated TODO Make private in 4.0.
     */
    @Deprecated
    public RandomUtils() {
        this(SECURE_STRONG_SUPPLIER);
    }

    private RandomUtils(final Supplier<Random> random) {
        this.random = random;
    }

    Random random() {
        return random.get();
    }

    /**
     * Generates a random boolean value.
     *
     * @return the random boolean.
     * @since 3.16.0
     */
    public boolean randomBoolean() {
        return random().nextBoolean();
    }

    /**
     * Generates an array of random bytes.
     *
     * @param count the size of the returned array.
     * @return the random byte array.
     * @throws IllegalArgumentException if {@code count} is negative.
     * @since 3.16.0
     */
    public byte[] randomBytes(final int count) {
        Validate.isTrue(count >= 0, "Count cannot be negative.");
        final byte[] result = new byte[count];
        random().nextBytes(result);
        return result;
    }

    /**
     * Generates a random double between 0 (inclusive) and Double.MAX_VALUE (exclusive).
     *
     * @return the random double.
     * @see #randomDouble(double, double)
     * @since 3.16.0
     */
    public double randomDouble() {
        return randomDouble(0, Double.MAX_VALUE);
    }

    /**
     * Generates a random double within the specified range.
     *
     * @param startInclusive the smallest value that can be returned, must be non-negative.
     * @param endExclusive   the upper bound (not included).
     * @throws IllegalArgumentException if {@code startInclusive > endExclusive} or if {@code startInclusive} is negative.
     * @return the random double.
     * @since 3.16.0
     */
    public double randomDouble(final double startInclusive, final double endExclusive) {
        Validate.isTrue(endExclusive >= startInclusive, "Start value must be smaller or equal to end value.");
        Validate.isTrue(startInclusive >= 0, "Both range values must be non-negative.");
        if (startInclusive == endExclusive) {
            return startInclusive;
        }
        return startInclusive + (endExclusive - startInclusive) * random().nextDouble();
    }

    /**
     * Generates a random float between 0 (inclusive) and Float.MAX_VALUE (exclusive).
     *
     * @return the random float.
     * @see #randomFloat(float, float)
     * @since 3.16.0
     */
    public float randomFloat() {
        return randomFloat(0, Float.MAX_VALUE);
    }

    /**
     * Generates a random float within the specified range.
     *
     * @param startInclusive the smallest value that can be returned, must be non-negative.
     * @param endExclusive   the upper bound (not included).
     * @throws IllegalArgumentException if {@code startInclusive > endExclusive} or if {@code startInclusive} is negative.
     * @return the random float.
     * @since 3.16.0
     */
    public float randomFloat(final float startInclusive, final float endExclusive) {
        Validate.isTrue(endExclusive >= startInclusive, "Start value must be smaller or equal to end value.");
        Validate.isTrue(startInclusive >= 0, "Both range values must be non-negative.");
        if (startInclusive == endExclusive) {
            return startInclusive;
        }
        return startInclusive + (endExclusive - startInclusive) * random().nextFloat();
    }

    /**
     * Generates a random int between 0 (inclusive) and Integer.MAX_VALUE (exclusive).
     *
     * @return the random integer.
     * @see #randomInt(int, int)
     * @since 3.16.0
     */
    public int randomInt() {
        return randomInt(0, Integer.MAX_VALUE);
    }

    /**
     * Generates a random integer within the specified range.
     *
     * @param startInclusive the smallest value that can be returned, must be non-negative.
     * @param endExclusive   the upper bound (not included).
     * @throws IllegalArgumentException if {@code startInclusive > endExclusive} or if {@code startInclusive} is negative.
     * @return the random integer.
     * @since 3.16.0
     */
    public int randomInt(final int startInclusive, final int endExclusive) {
        Validate.isTrue(endExclusive >= startInclusive, "Start value must be smaller or equal to end value.");
        Validate.isTrue(startInclusive >= 0, "Both range values must be non-negative.");
        if (startInclusive == endExclusive) {
            return startInclusive;
        }
        return startInclusive + random().nextInt(endExclusive - startInclusive);
    }

    /**
     * Generates a random long between 0 (inclusive) and Long.MAX_VALUE (exclusive).
     *
     * @return the random long.
     * @see #randomLong(long, long)
     * @since 3.16.0
     */
    public long randomLong() {
        return randomLong(Long.MAX_VALUE);
    }

    /**
     * Generates a {@code long} value between 0 (inclusive) and the specified value (exclusive).
     *
     * @param n Bound on the random number to be returned. Must be positive.
     * @return a random {@code long} value between 0 (inclusive) and {@code n} (exclusive).
     */
    private long randomLong(final long n) {
        // Extracted from o.a.c.rng.core.BaseProvider.nextLong(long)
        long bits;
        long val;
        do {
            bits = random().nextLong() >>> 1;
            val = bits % n;
        } while (bits - val + n - 1 < 0);
        return val;
    }

    /**
     * Generates a random long within the specified range.
     *
     * @param startInclusive the smallest value that can be returned, must be non-negative.
     * @param endExclusive   the upper bound (not included).
     * @throws IllegalArgumentException if {@code startInclusive > endExclusive} or if {@code startInclusive} is negative.
     * @return the random long.
     * @since 3.16.0
     */
    public long randomLong(final long startInclusive, final long endExclusive) {
        Validate.isTrue(endExclusive >= startInclusive, "Start value must be smaller or equal to end value.");
        Validate.isTrue(startInclusive >= 0, "Both range values must be non-negative.");
        if (startInclusive == endExclusive) {
            return startInclusive;
        }
        return startInclusive + randomLong(endExclusive - startInclusive);
    }

    @Override
    public String toString() {
        return "RandomUtils [random=" + random() + "]";
    }

}
