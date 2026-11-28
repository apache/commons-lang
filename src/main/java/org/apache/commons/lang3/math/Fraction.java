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

import java.io.IOException;
import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.math.BigInteger;
import java.util.Objects;

/**
 * {@link Fraction} is a {@link Number} implementation that stores fractions accurately.
 * <p>
 * This class is immutable, and interoperable with most methods that accept a {@link Number}.
 * </p>
 * <p>
 * Note that this class is intended for common use cases, it is <em>int</em> based and thus suffers from various overflow issues. For a BigInteger based
 * equivalent, please see the Commons Math BigFraction class.
 * </p>
 *
 * @since 2.0
 */
public final class Fraction extends Number implements Comparable<Fraction> {

    /**
     * Required for serialization support. Lang version 2.0.
     *
     * @see Serializable
     */
    private static final long serialVersionUID = 65382027393090L;

    /**
     * {@link Fraction} representation of 0.
     */
    public static final Fraction ZERO = new Fraction(0, 1);

    /**
     * {@link Fraction} representation of 1.
     */
    public static final Fraction ONE = new Fraction(1, 1);

    /**
     * {@link Fraction} representation of 1/2.
     */
    public static final Fraction ONE_HALF = new Fraction(1, 2);

    /**
     * {@link Fraction} representation of 1/3.
     */
    public static final Fraction ONE_THIRD = new Fraction(1, 3);

    /**
     * {@link Fraction} representation of 2/3.
     */
    public static final Fraction TWO_THIRDS = new Fraction(2, 3);

    /**
     * {@link Fraction} representation of 1/4.
     */
    public static final Fraction ONE_QUARTER = new Fraction(1, 4);

    /**
     * {@link Fraction} representation of 2/4.
     */
    public static final Fraction TWO_QUARTERS = new Fraction(2, 4);

    /**
     * {@link Fraction} representation of 3/4.
     */
    public static final Fraction THREE_QUARTERS = new Fraction(3, 4);

    /**
     * {@link Fraction} representation of 1/5.
     */
    public static final Fraction ONE_FIFTH = new Fraction(1, 5);

    /**
     * {@link Fraction} representation of 2/5.
     */
    public static final Fraction TWO_FIFTHS = new Fraction(2, 5);

    /**
     * {@link Fraction} representation of 3/5.
     */
    public static final Fraction THREE_FIFTHS = new Fraction(3, 5);

    /**
     * {@link Fraction} representation of 4/5.
     */
    public static final Fraction FOUR_FIFTHS = new Fraction(4, 5);

    /**
     * Checks that a denominator is not zero.
     *
     * @param denominator The denominator to check
     * @throws ArithmeticException Thrown if the denominator is zero.
     */
    private static void checkDenominator(final int denominator) {
        if (denominator == 0) {
            throw new ArithmeticException("The denominator must not be zero");
        }
    }

    /**
     * Gets a {@link Fraction} instance from a {@code double} value.
     *
     * <p>
     * This method uses the <a href="https://web.archive.org/web/20210516065058/http%3A//archives.math.utk.edu/articles/atuyl/confrac/"> continued fraction
     * algorithm</a>, bounding the denominator by 10,000 and stopping when the approximation no longer improves.
     * </p>
     *
     * @param value The double value to convert
     * @return A fraction instance that is close to the value
     * @throws ArithmeticException Thrown if {@code |value| &gt; Integer.MAX_VALUE} or {@code value = NaN}.
     * @throws ArithmeticException Thrown if the calculated numerator cannot be represented as an {@code int}.
     */
    public static Fraction getFraction(double value) {
        final int sign = value < 0 ? -1 : 1;
        value = Math.abs(value);
        if (value > Integer.MAX_VALUE || Double.isNaN(value)) {
            throw new ArithmeticException("The value must not be NaN and its absolute value must not exceed Integer.MAX_VALUE");
        }
        final int wholeNumber = (int) value;
        value -= wholeNumber;
        int numer0 = 0; // the pre-previous
        int denom0 = 1; // the pre-previous
        int numer1 = 1; // the previous
        int denom1 = 0; // the previous
        int numer2; // the current, setup in calculation
        int denom2; // the current, setup in calculation
        int a1 = (int) value;
        int a2;
        double x1 = 1;
        double x2;
        double y1 = value - a1;
        double y2;
        double delta1;
        double delta2 = Double.MAX_VALUE;
        double fraction;
        // Convergent denominators grow at least as fast as Fibonacci numbers before integer overflow.
        // Check improvement as well as the denominator: overflow can wrap a denominator to a small positive value.
        do {
            delta1 = delta2;
            a2 = (int) (x1 / y1);
            x2 = y1;
            y2 = x1 - a2 * y1;
            numer2 = a1 * numer1 + numer0;
            denom2 = a1 * denom1 + denom0;
            fraction = (double) numer2 / (double) denom2;
            delta2 = Math.abs(value - fraction);
            a1 = a2;
            x1 = x2;
            y1 = y2;
            numer0 = numer1;
            denom0 = denom1;
            numer1 = numer2;
            denom1 = denom2;
        } while (denom2 > 0 && denom2 <= 10000 && delta1 > delta2);
        // Use long arithmetic and apply the sign before narrowing so that Integer.MIN_VALUE remains representable.
        final long numerator = (numer0 + (long) wholeNumber * denom0) * sign;
        return getReducedFraction(Math.toIntExact(numerator), denom0);
    }

    /**
     * Gets a {@link Fraction} instance with the 2 parts of a fraction Y/Z.
     * <p>
     * Any negative signs are resolved to be on the numerator.
     * </p>
     *
     * @param numerator   The numerator, for example the three in 'three sevenths'
     * @param denominator The denominator, for example the seven in 'three sevenths'
     * @return A new fraction instance
     * @throws ArithmeticException Thrown if the denominator is zero, or if the denominator is negative and either argument is
     *         {@link Integer#MIN_VALUE}, preventing sign normalization without overflow.
     */
    public static Fraction getFraction(int numerator, int denominator) {
        checkDenominator(denominator);
        if (denominator < 0) {
            if (numerator == Integer.MIN_VALUE || denominator == Integer.MIN_VALUE) {
                throw new ArithmeticException("overflow: can't negate");
            }
            numerator = -numerator;
            denominator = -denominator;
        }
        return new Fraction(numerator, denominator);
    }

    /**
     * Gets a {@link Fraction} instance with the 3 parts of a fraction X Y/Z.
     * <p>
     * The negative sign must be passed in on the whole number part.
     * </p>
     *
     * @param whole       The whole number, for example the one in 'one and three sevenths'
     * @param numerator   The numerator, for example the three in 'one and three sevenths'
     * @param denominator The denominator, for example the seven in 'one and three sevenths'
     * @return A new fraction instance
     * @throws ArithmeticException Thrown if the denominator is {@code zero}.
     * @throws ArithmeticException Thrown if the denominator is negative.
     * @throws ArithmeticException Thrown if the numerator is negative.
     * @throws ArithmeticException Thrown if the resulting numerator cannot be represented as an {@code int}.
     */
    public static Fraction getFraction(final int whole, final int numerator, final int denominator) {
        checkDenominator(denominator);
        if (denominator < 0) {
            throw new ArithmeticException("The denominator must not be negative");
        }
        if (numerator < 0) {
            throw new ArithmeticException("The numerator must not be negative");
        }
        final long numeratorValue;
        if (whole < 0) {
            numeratorValue = whole * (long) denominator - numerator;
        } else {
            numeratorValue = whole * (long) denominator + numerator;
        }
        if (numeratorValue < Integer.MIN_VALUE || numeratorValue > Integer.MAX_VALUE) {
            throw new ArithmeticException("The resulting numerator cannot be represented as an int");
        }
        return new Fraction((int) numeratorValue, denominator);
    }

    /**
     * Gets a Fraction from a {@link String}.
     * <p>
     * The formats accepted are:
     * </p>
     * <ol>
     * <li>A string containing a decimal point, parsed by {@link Double#parseDouble(String)}</li>
     * <li>{@code "X Y/Z"}</li>
     * <li>{@code "Y/Z"}</li>
     * <li>{@code "X"} (a simple whole number)</li>
     * </ol>
     *
     * @param str The string to parse, must not be {@code null}
     * @return The resulting {@link Fraction} instance
     * @throws NullPointerException  Thrown if the string is {@code null}
     * @throws NumberFormatException Thrown if the format is invalid, an integer component is outside the range of an {@code int}, or the parsed values
     *                               cannot form a valid {@code Fraction}. If fraction construction throws an {@link ArithmeticException}, for example for
     *                               a zero denominator or arithmetic overflow, that exception is preserved as the {@link Throwable#getCause() cause}.
     *                               Failures while parsing the numeric components do not have an {@code ArithmeticException} cause.
     */
    public static Fraction getFraction(final String str) {
        Objects.requireNonNull(str, "str");
        // parse double format
        int pos = str.indexOf('.');
        if (pos >= 0) {
            final double value = Double.parseDouble(str);
            try {
                return getFraction(value);
            } catch (final ArithmeticException e) {
                throw toNumberFormatException(str, e);
            }
        }

        // parse X Y/Z format
        pos = str.indexOf(' ');
        if (pos > 0) {
            final int whole = Integer.parseInt(str.substring(0, pos));
            final String remainder = str.substring(pos + 1);
            pos = remainder.indexOf('/');
            if (pos < 0) {
                throw new NumberFormatException("The fraction could not be parsed as the format X Y/Z");
            }
            final int numer = Integer.parseInt(remainder.substring(0, pos));
            final int denom = Integer.parseInt(remainder.substring(pos + 1));
            try {
                return getFraction(whole, numer, denom);
            } catch (final ArithmeticException e) {
                throw toNumberFormatException(str, e);
            }
        }

        // parse Y/Z format
        pos = str.indexOf('/');
        if (pos < 0) {
            // simple whole number
            return getFraction(Integer.parseInt(str), 1);
        }
        final int numer = Integer.parseInt(str.substring(0, pos));
        final int denom = Integer.parseInt(str.substring(pos + 1));
        try {
            return getFraction(numer, denom);
        } catch (final ArithmeticException e) {
            throw toNumberFormatException(str, e);
        }
    }

    /**
     * Gets a reduced {@link Fraction} instance with the 2 parts of a fraction Y/Z.
     * <p>
     * For example, if the input parameters represent 2/4, then the created fraction will be 1/2.
     * </p>
     *
     * <p>
     * Any negative signs are resolved to be on the numerator.
     * </p>
     *
     * @param numerator   The numerator, for example the three in 'three sevenths'
     * @param denominator The denominator, for example the seven in 'three sevenths'
     * @return A fraction instance, with the numerator and denominator reduced
     * @throws ArithmeticException Thrown if the denominator is {@code zero}, or the reduced numerator or positive denominator cannot be represented as an
     *                             {@code int}.
     */
    public static Fraction getReducedFraction(int numerator, int denominator) {
        checkDenominator(denominator);
        if (numerator == 0) {
            return ZERO; // normalize zero.
        }
        // Reduce common powers of two before sign normalization to avoid negating Integer.MIN_VALUE.
        while ((numerator & 1) == 0 && (denominator & 1) == 0) {
            numerator /= 2;
            denominator /= 2;
        }
        if (denominator < 0) {
            if (numerator == Integer.MIN_VALUE || denominator == Integer.MIN_VALUE) {
                throw new ArithmeticException("overflow: can't negate");
            }
            numerator = -numerator;
            denominator = -denominator;
        }
        // simplify fraction.
        final int gcd = greatestCommonDivisor(numerator, denominator);
        numerator /= gcd;
        denominator /= gcd;
        return new Fraction(numerator, denominator);
    }

    /**
     * Gets the greatest common divisor of the absolute value of
     * two numbers, using the "binary gcd" method based on subtraction
     * and halving. See Knuth 4.5.2 algorithm B.
     * This algorithm is due to Josef Stein (1961).
     *
     * @param u A number
     * @param v A number
     * @return The non-negative greatest common divisor, or zero if both arguments are zero.
     * @throws ArithmeticException Thrown if the greatest common divisor cannot be represented as an {@code int}.
     */
    private static int greatestCommonDivisor(int u, int v) {
        // From Commons Math:
        if (u == 0 || v == 0) {
            if (u == Integer.MIN_VALUE || v == Integer.MIN_VALUE) {
                throw new ArithmeticException("overflow: gcd is 2^31");
            }
            return Math.abs(u) + Math.abs(v);
        }
        // if either operand is abs 1, return 1:
        if (Math.abs(u) == 1 || Math.abs(v) == 1) {
            return 1;
        }
        // keep u and v negative, as negative integers range down to
        // -2^31, while positive numbers can only be as large as 2^31-1
        // (that is, we can't necessarily negate a negative number without
        // overflow)
        if (u > 0) {
            u = -u;
        } // make u negative
        if (v > 0) {
            v = -v;
        } // make v negative
        // B1. [Find power of 2]
        int k = 0;
        // Non-zero ints become odd after at most 31 halvings, so no separate iteration bound is needed.
        while ((u & 1) == 0 && (v & 1) == 0) { // while u and v are both even...
            u /= 2;
            v /= 2;
            k++; // cast out twos.
        }
        if (k == 31) {
            throw new ArithmeticException("overflow: gcd is 2^31");
        }
        // B2. Initialize: u and v have been divided by 2^k and at least
        // one is odd.
        int t = (u & 1) == 1 ? v : -(u / 2)/* B3 */;
        // t negative: u was odd, v may be even (t replaces v)
        // t positive: u was even, v is odd (t replaces u)
        do {
            /* assert u<0 && v<0; */
            // B4/B3: cast out twos from t.
            while ((t & 1) == 0) { // while t is even.
                t /= 2; // cast out twos
            }
            // B5 [reset max(u,v)]
            if (t > 0) {
                u = -t;
            } else {
                v = t;
            }
            // B6/B3. at this point both u and v should be odd.
            t = (v - u) / 2;
            // |u| larger: t positive (replace u)
            // |v| larger: t negative (replace v)
        } while (t != 0);
        return -u * (1 << k); // gcd is -u*2^k
    }

    private static int hash(final int value1, final int value2) {
        return Objects.hash(value1, value2);
    }

    /**
     * Multiplies two integers, checking for overflow.
     *
     * @param x A factor
     * @param y A factor
     * @return The product {@code x*y}
     * @throws ArithmeticException Thrown if the result cannot be represented as an int.
     */
    private static int mulAndCheck(final int x, final int y) {
        final long m = (long) x * (long) y;
        if (m < Integer.MIN_VALUE || m > Integer.MAX_VALUE) {
            throw new ArithmeticException("overflow: mul");
        }
        return (int) m;
    }

    /**
     *  Multiplies two non-negative integers, checking for overflow.
     *
     * @param x A non-negative factor
     * @param y A non-negative factor
     * @return The product {@code x*y}
     * @throws ArithmeticException Thrown if the result cannot be represented as an int.
     */
    private static int mulPosAndCheck(final int x, final int y) {
        /* assert x>=0 && y>=0; */
        final long m = (long) x * (long) y;
        if (m > Integer.MAX_VALUE) {
            throw new ArithmeticException("overflow: mulPos");
        }
        return (int) m;
    }

    /**
     * Converts an {@link ArithmeticException} raised while parsing a string into the {@link NumberFormatException} that
     * {@link #getFraction(String)} documents, preserving the original exception as the cause.
     *
     * @param str The string being parsed.
     * @param cause The arithmetic failure while constructing the fraction.
     * @return The exception for the caller to throw, never {@code null}.
     */
    private static NumberFormatException toNumberFormatException(final String str, final ArithmeticException cause) {
        final NumberFormatException nfe = new NumberFormatException("The fraction could not be parsed from '" + str + "': " + cause.getMessage());
        nfe.initCause(cause);
        return nfe;
    }

    /**
     * The numerator number part of the fraction (the three in three sevenths).
     */
    private final int numerator;

    /**
     * The denominator number part of the fraction (the seven in three sevenths).
     */
    private final int denominator;

    /**
     * Cached output hashCode (class is immutable).
     */
    private final int hashCode;

    /**
     * Cached output toString (class is immutable).
     */
    private transient String toString;

    /**
     * Cached output toProperString (class is immutable).
     */
    private transient String toProperString;

    /**
     * Constructs a {@link Fraction} instance with the 2 parts
     * of a fraction Y/Z.
     *
     * @param numerator  The numerator, for example the three in 'three sevenths'
     * @param denominator  The denominator, for example the seven in 'three sevenths'
     */
    private Fraction(final int numerator, final int denominator) {
        this.numerator = numerator;
        this.denominator = denominator;
        this.hashCode = hash(denominator, numerator);
    }

    /**
     * Gets a fraction that is the non-negative equivalent of this one.
     * <p>
     * More precisely: {@code (fraction &gt;= 0 ? this : -fraction)}
     * </p>
     * <p>
     * The returned fraction is not reduced.
     * </p>
     *
     * @return {@code this} if it is non-negative, or a new positive fraction instance with the opposite signed numerator
     * @throws ArithmeticException Thrown if the numerator is {@link Integer#MIN_VALUE} and cannot be negated.
     */
    public Fraction abs() {
        if (numerator >= 0) {
            return this;
        }
        return negate();
    }

    /**
     * Adds the value of this fraction to another, returning the result in reduced form.
     * The algorithm follows Knuth, 4.5.1.
     *
     * @param fraction  The fraction to add, must not be {@code null}
     * @return A {@link Fraction} instance with the resulting values
     * @throws NullPointerException Thrown if the fraction is {@code null}.
     * @throws ArithmeticException Thrown if the reduced numerator or positive denominator cannot be represented as an {@code int}.
     */
    public Fraction add(final Fraction fraction) {
        return addSub(fraction, true /* add */);
    }

    /**
     * Implements add and subtract using the algorithm described in <a href="https://www-cs-faculty.stanford.edu/~knuth/taocp.html">
     * The Art of Computer Programming (TAOCP)</a> 4.5.1 by Donald Knuth.
     *
     * @param fraction The fraction to add or subtract, must not be {@code null}
     * @param isAdd true to add, false to subtract
     * @return A {@link Fraction} instance with the resulting values
     * @throws NullPointerException Thrown if the fraction is {@code null}.
     * @throws ArithmeticException Thrown if the resulting numerator or denominator
     *   cannot be represented in an {@code int}.
     */
    private Fraction addSub(final Fraction fraction, final boolean isAdd) {
        Objects.requireNonNull(fraction, "fraction");
        // zero is identity for addition.
        if (numerator == 0) {
            return isAdd ? fraction.reduce() : fraction.reduce().negate();
        }
        if (fraction.numerator == 0) {
            return reduce();
        }
        // Knuth 4.5.1 assumes operands in lowest terms and this class does not reduce on
        // construction, so reduce both first, as multiplyBy does.
        final int thisGcd = greatestCommonDivisor(numerator, denominator);
        final int thatGcd = greatestCommonDivisor(fraction.numerator, fraction.denominator);
        final int thisNumerator = numerator / thisGcd;
        final int thisDenominator = denominator / thisGcd;
        final int thatNumerator = fraction.numerator / thatGcd;
        final int thatDenominator = fraction.denominator / thatGcd;
        // if denominators are randomly distributed, d1 will be 1 about 61%
        // of the time.
        final int d1 = greatestCommonDivisor(thisDenominator, thatDenominator);
        if (d1 == 1) {
            // result is ((u*v' +/- u'v) / u'v')
            // the int cross products u*v' and u'*v can overflow even when the reduced result
            // fits an int, so widen to long and let Math narrow the final numerator back.
            final long uvp = (long) thisNumerator * thatDenominator;
            final long upv = (long) thatNumerator * thisDenominator;
            final long t = isAdd ? Math.addExact(uvp, upv) : Math.subtractExact(uvp, upv);
            return new Fraction(Math.toIntExact(t), mulPosAndCheck(thisDenominator, thatDenominator));
        }
        // Compute t exactly before canceling any remaining common factors.
        // t = u(v'/d1) +/- v(u'/d1)
        final BigInteger uvp = BigInteger.valueOf(thisNumerator).multiply(BigInteger.valueOf(thatDenominator / d1));
        final BigInteger upv = BigInteger.valueOf(thatNumerator).multiply(BigInteger.valueOf(thisDenominator / d1));
        final BigInteger t = isAdd ? uvp.add(upv) : uvp.subtract(upv);
        // but d2 doesn't need extra precision because
        // d2 = gcd(t,d1) = gcd(t mod d1, d1)
        final int tmodd1 = t.mod(BigInteger.valueOf(d1)).intValue();
        final int d2 = tmodd1 == 0 ? d1 : greatestCommonDivisor(tmodd1, d1);

        // result is (t/d2) / (u'/d1)(v'/d2)
        final BigInteger w = t.divide(BigInteger.valueOf(d2));
        if (w.bitLength() > 31) {
            throw new ArithmeticException("overflow: reduced numerator cannot be represented as an int");
        }
        return new Fraction(w.intValue(), mulPosAndCheck(thisDenominator / d1, thatDenominator / d2));
    }

    /**
     * Compares this object to another based on size.
     * <p>
     * Note: this class has a natural ordering that is inconsistent with equals, because, for example, equals treats 1/2 and 2/4 as different, whereas compareTo
     * treats them as equal.
     * </p>
     *
     * @param other The object to compare to
     * @return -1 if this is less, 0 if equal, +1 if greater
     * @throws ClassCastException Thrown if the object is not a {@link Fraction}.
     * @throws NullPointerException Thrown if the object is {@code null}.
     */
    @Override
    public int compareTo(final Fraction other) {
        if (this == other || numerator == other.numerator && denominator == other.denominator) {
            return 0;
        }

        // otherwise see which is less
        final long first = (long) numerator * (long) other.denominator;
        final long second = (long) other.numerator * (long) denominator;
        return Long.compare(first, second);
    }

    /**
     * Divides the value of this fraction by another, returning the result in reduced form.
     *
     * @param fraction  The fraction to divide by, must not be {@code null}
     * @return A {@link Fraction} instance with the resulting values
     * @throws NullPointerException Thrown if the fraction is {@code null}.
     * @throws ArithmeticException Thrown if the fraction to divide by is zero.
     * @throws ArithmeticException Thrown if the reduced numerator is outside the range {@link Integer#MIN_VALUE} to {@link Integer#MAX_VALUE},
     *         or the reduced denominator exceeds {@link Integer#MAX_VALUE}.
     */
    public Fraction divideBy(final Fraction fraction) {
        Objects.requireNonNull(fraction, "fraction");
        if (fraction.numerator == 0) {
            throw new ArithmeticException("The fraction to divide by must not be zero");
        }
        if (numerator == 0) {
            return ZERO;
        }
        // Products of int values fit in a long. Reduce before narrowing to avoid overflow in an intermediate reciprocal.
        long resultNumerator = (long) numerator * fraction.denominator;
        long resultDenominator = (long) denominator * fraction.numerator;
        if (resultDenominator < 0) {
            resultNumerator = -resultNumerator;
            resultDenominator = -resultDenominator;
        }
        long gcd = Math.abs(resultNumerator);
        long remainder = resultDenominator;
        while (remainder != 0) {
            final long next = gcd % remainder;
            gcd = remainder;
            remainder = next;
        }
        return new Fraction(Math.toIntExact(resultNumerator / gcd), Math.toIntExact(resultDenominator / gcd));
    }

    /**
     * Gets the fraction as a {@code double}. This calculates the fraction
     * as the numerator divided by denominator.
     *
     * @return The fraction as a {@code double}
     */
    @Override
    public double doubleValue() {
        return (double) numerator / (double) denominator;
    }

    /**
     * Compares this fraction to another object to test if they are equal.
     * <p>
     * To be equal, both values must be equal. Thus 2/4 is not equal to 1/2.
     * </p>
     *
     * @param obj The reference object with which to compare
     * @return {@code true} if this object is equal
     */
    @Override
    public boolean equals(final Object obj) {
        if (obj == this) {
            return true;
        }
        if (!(obj instanceof Fraction)) {
            return false;
        }
        final Fraction other = (Fraction) obj;
        return getNumerator() == other.getNumerator() && getDenominator() == other.getDenominator();
    }

    /**
     * Gets the fraction as a {@code float}. This calculates the fraction
     * as the numerator divided by denominator.
     *
     * @return The fraction as a {@code float}
     */
    @Override
    public float floatValue() {
        return (float) numerator / (float) denominator;
    }

    /**
     * Gets the denominator part of the fraction.
     *
     * @return The denominator fraction part
     */
    public int getDenominator() {
        return denominator;
    }

    /**
     * Gets the numerator part of the fraction.
     * <p>
     * This method may return a value greater than the denominator, an improper fraction, such as the seven in 7/4.
     * </p>
     *
     * @return The numerator fraction part
     */
    public int getNumerator() {
        return numerator;
    }

    /**
     * Gets the proper numerator, always non-negative.
     * <p>
     * An improper fraction 7/4 can be resolved into a proper one, 1 3/4. This method returns the 3 from the proper fraction.
     * </p>
     *
     * <p>
     * If the fraction is negative such as -7/4, it can be resolved into -1 3/4, so this method returns the positive proper numerator, 3.
     * </p>
     *
     * @return The numerator fraction part of a proper fraction, always non-negative
     */
    public int getProperNumerator() {
        return Math.abs(numerator % denominator);
    }

    /**
     * Gets the proper whole part of the fraction.
     * <p>
     * An improper fraction 7/4 can be resolved into a proper one, 1 3/4. This method returns the 1 from the proper fraction.
     * </p>
     *
     * <p>
     * If the fraction is negative such as -7/4, it can be resolved into -1 3/4, so this method returns the signed whole part -1.
     * </p>
     *
     * @return The whole fraction part of a proper fraction, that includes the sign
     */
    public int getProperWhole() {
        return numerator / denominator;
    }

    /**
     * Gets a hashCode for the fraction.
     *
     * @return A hash code value for this object
     */
    @Override
    public int hashCode() {
        return hashCode;
    }

    /**
     * Gets the fraction as an {@code int}. This returns the whole number
     * part of the fraction.
     *
     * @return The whole number fraction part
     */
    @Override
    public int intValue() {
        return numerator / denominator;
    }

    /**
     * Gets a fraction that is the inverse (1/fraction) of this one.
     * <p>
     * The returned fraction is not reduced.
     * </p>
     *
     * @return A new fraction instance with the numerator and denominator inverted.
     * @throws ArithmeticException Thrown if the fraction represents zero.
     * @throws ArithmeticException Thrown if the numerator is {@link Integer#MIN_VALUE}, because the inverted denominator cannot be represented
     *         as a positive {@code int}.
     */
    public Fraction invert() {
        if (numerator == 0) {
            throw new ArithmeticException("Unable to invert zero.");
        }
        if (numerator == Integer.MIN_VALUE) {
            throw new ArithmeticException("overflow: can't negate numerator");
        }
        if (numerator < 0) {
            return new Fraction(-denominator, -numerator);
        }
        return new Fraction(denominator, numerator);
    }

    /**
     * Gets the fraction as a {@code long}. This returns the whole number
     * part of the fraction.
     *
     * @return The whole number fraction part
     */
    @Override
    public long longValue() {
        return (long) numerator / denominator;
    }

    /**
     * Multiplies the value of this fraction by another, returning the
     * result in reduced form.
     *
     * @param fraction  The fraction to multiply by, must not be {@code null}
     * @return A {@link Fraction} instance with the resulting values
     * @throws NullPointerException Thrown if the fraction is {@code null}.
     * @throws ArithmeticException Thrown if the reduced numerator or positive denominator cannot be represented as an {@code int}.
     */
    public Fraction multiplyBy(final Fraction fraction) {
        Objects.requireNonNull(fraction, "fraction");
        if (numerator == 0 || fraction.numerator == 0) {
            return ZERO;
        }
        // knuth 4.5.1
        // make sure we don't overflow unless the result *must* overflow.
        // Reduce both operands first: the cross-gcd below cancels the cross terms only, so a
        // factor shared inside an unreduced operand survives into the product and can overflow
        // an int even when the reduced result fits.
        final int thisGcd = greatestCommonDivisor(numerator, denominator);
        final int thatGcd = greatestCommonDivisor(fraction.numerator, fraction.denominator);
        final int thisNumerator = numerator / thisGcd;
        final int thisDenominator = denominator / thisGcd;
        final int thatNumerator = fraction.numerator / thatGcd;
        final int thatDenominator = fraction.denominator / thatGcd;
        final int d1 = greatestCommonDivisor(thisNumerator, thatDenominator);
        final int d2 = greatestCommonDivisor(thatNumerator, thisDenominator);
        return getReducedFraction(mulAndCheck(thisNumerator / d1, thatNumerator / d2), mulPosAndCheck(thisDenominator / d2, thatDenominator / d1));
    }

    /**
     * Gets a fraction that is the negative (-fraction) of this one.
     * <p>
     * The returned fraction is not reduced.
     * </p>
     *
     * @return A new fraction instance with the opposite signed numerator
     * @throws ArithmeticException Thrown if the numerator is {@link Integer#MIN_VALUE} and cannot be negated.
     */
    public Fraction negate() {
        // the positive range is one smaller than the negative range of an int.
        if (numerator == Integer.MIN_VALUE) {
            throw new ArithmeticException("overflow: too large to negate");
        }
        return new Fraction(-numerator, denominator);
    }

    /**
     * Gets a fraction that is raised to the passed in power.
     *
     * <p>
     * For powers other than {@code 1} and {@code -1}, the returned fraction is in reduced form. A power of {@code 1} returns this instance unchanged.
     * A power of {@code -1} returns the unreduced reciprocal unless reduction is necessary to avoid overflow.
     * </p>
     *
     * @param power The power to raise the fraction to
     * @return {@code this} if the power is one, {@link #ONE} if the power is zero (including zero raised to zero), or a fraction instance raised to the
     *         appropriate power
     * @throws ArithmeticException Thrown if this fraction is zero and the power is negative.
     * @throws ArithmeticException Thrown if the resulting numerator or positive denominator cannot be represented as an {@code int}.
     */
    public Fraction pow(final int power) {
        if (power == 1) {
            return this;
        }
        if (power == 0) {
            return ONE;
        }
        if (power < 0) {
            // Reduce a minimum-value numerator before inversion, while preserving other unreduced reciprocals.
            final Fraction inverse = (numerator == Integer.MIN_VALUE ? reduce() : this).invert();
            if (power == Integer.MIN_VALUE) { // MIN_VALUE can't be negated.
                return inverse.pow(2).pow(-(power / 2));
            }
            return inverse.pow(-power);
        }
        final Fraction f = multiplyBy(this);
        if (power % 2 == 0) { // if even...
            return f.pow(power / 2);
        }
        return f.pow(power / 2).multiplyBy(this);
    }

    /**
     * Validates the denominator and cached hash code after deserialization.
     *
     * @param in The stream to read.
     * @throws IOException Thrown if reading the serialized fields fails.
     * @throws ClassNotFoundException Thrown if a serialized class cannot be found.
     * @throws InvalidObjectException Thrown if the cached hash code does not match the denominator and numerator.
     * @throws ArithmeticException Thrown if the deserialized denominator is zero.
     */
    private void readObject(final ObjectInputStream in) throws IOException, ClassNotFoundException {
        in.defaultReadObject();
        checkDenominator(denominator);
        if (hashCode != hash(denominator, numerator)) {
            throw new InvalidObjectException("Fraction hashCode does not match numerator/denominator.");
        }
    }

    /**
     * Reduces the fraction to the smallest values for the numerator and denominator, returning the result.
     * <p>
     * For example, if this fraction represents 2/4, then the result will be 1/2.
     * </p>
     *
     * @return A reduced fraction instance, or {@code this} if no simplification is possible
     */
    public Fraction reduce() {
        if (numerator == 0) {
            return equals(ZERO) ? this : ZERO;
        }
        final int gcd = greatestCommonDivisor(Math.abs(numerator), denominator);
        if (gcd == 1) {
            return this;
        }
        return getFraction(numerator / gcd, denominator / gcd);
    }

    /**
     * Subtracts the value of another fraction from the value of this one,
     * returning the result in reduced form.
     *
     * @param fraction  The fraction to subtract, must not be {@code null}
     * @return A {@link Fraction} instance with the resulting values
     * @throws NullPointerException Thrown if the fraction is {@code null}.
     * @throws ArithmeticException Thrown if the resulting numerator or denominator
     *   cannot be represented in an {@code int}.
     */
    public Fraction subtract(final Fraction fraction) {
        return addSub(fraction, false /* subtract */);
    }

    /**
     * Gets the fraction as a proper {@link String} in the format X Y/Z.
     * <p>
     * The format used in '<em>wholeNumber</em> <em>numerator</em>/<em>denominator</em>'. If the whole number is zero it will be omitted. If the numerator is
     * zero, only the whole number is returned.
     * </p>
     *
     * @return A {@link String} form of the fraction
     */
    public String toProperString() {
        // Read the lazily initialized field only once: a second racy read may see null.
        String result = toProperString;
        if (result == null) {
            if (numerator == 0) {
                result = "0";
            } else if (numerator == denominator) {
                result = "1";
            } else if (numerator == -1 * denominator) {
                result = "-1";
            } else if ((numerator > 0 ? -numerator : numerator) < -denominator) {
                // note that we do the magnitude comparison test above with
                // NEGATIVE (not positive) numbers, since negative numbers
                // have a larger range. otherwise numerator == Integer.MIN_VALUE
                // is handled incorrectly.
                final int properNumerator = getProperNumerator();
                if (properNumerator == 0) {
                    result = Integer.toString(getProperWhole());
                } else {
                    result = getProperWhole() + " " + properNumerator + "/" + getDenominator();
                }
            } else {
                result = getNumerator() + "/" + getDenominator();
            }
            toProperString = result;
        }
        return result;
    }

    /**
     * Gets the fraction as a {@link String}.
     * <p>
     * The format used is '<em>numerator</em>/<em>denominator</em>' always.
     * </p>
     *
     * @return A {@link String} form of the fraction
     */
    @Override
    public String toString() {
        // Read the lazily initialized field only once: a second racy read may see null.
        String result = toString;
        if (result == null) {
            result = getNumerator() + "/" + getDenominator();
            toString = result;
        }
        return result;
    }
}
