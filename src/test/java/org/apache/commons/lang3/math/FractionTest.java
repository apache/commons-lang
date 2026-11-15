/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.commons.lang3.math;

import static org.apache.commons.lang3.LangAssertions.assertNullPointerException;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;
import java.util.stream.Stream;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Tests {@link Fraction}.
 */
class FractionTest extends AbstractLangTest {

    private static final int SKIP = 500; // 53

    private static Stream<Arguments> arithmeticBoundaryValues() {
        final int[] numerators = {
            Integer.MIN_VALUE, Integer.MIN_VALUE + 1, -1073741824, -46341, -2, -1, 0, 1, 2, 46341, 1073741824,
            Integer.MAX_VALUE - 1, Integer.MAX_VALUE
        };
        final int[] denominators = {1, 2, 3, 4, 46341, 1073741824, Integer.MAX_VALUE - 1, Integer.MAX_VALUE};
        final List<Fraction> fractions = new ArrayList<>();
        for (final int numerator : numerators) {
            for (final int denominator : denominators) {
                // Preserve unreduced operands so cancellation within each operand is exercised as well.
                fractions.add(Fraction.getFraction(numerator, denominator));
            }
        }
        return fractions.stream()
                .flatMap(left -> fractions.stream().map(right -> Arguments.of(left, right)));
    }

    private static void assertReducedResult(final BigInteger numerator, final BigInteger denominator, final Supplier<Fraction> operation) {
        // Use arbitrary-precision arithmetic as an independent oracle for the reduced result and its range.
        final BigInteger gcd = numerator.gcd(denominator);
        final BigInteger expectedNumerator = numerator.divide(gcd);
        final BigInteger expectedDenominator = denominator.divide(gcd);
        final BigInteger minValue = BigInteger.valueOf(Integer.MIN_VALUE);
        final BigInteger maxValue = BigInteger.valueOf(Integer.MAX_VALUE);
        if (expectedNumerator.compareTo(minValue) < 0 || expectedNumerator.compareTo(maxValue) > 0
                || expectedDenominator.compareTo(maxValue) > 0) {
            assertThrows(ArithmeticException.class, operation::get);
        } else {
            final Fraction result = operation.get();
            assertEquals(expectedNumerator.intValueExact(), result.getNumerator());
            assertEquals(expectedDenominator.intValueExact(), result.getDenominator());
        }
    }

    private static Stream<Arguments> factoryDoubleValues() {
        return Stream.of(
            Arguments.of(0.0d, 0, 1),
            Arguments.of(1.0d, 1, 1),
            Arguments.of(0.5d, 1, 2),
            Arguments.of(-0.875d, -7, 8),
            Arguments.of(1.25d, 5, 4),
            Arguments.of(0.66666d, 2, 3),
            Arguments.of(1.0d / 10001d, 0, 1));
    }

    @ParameterizedTest(name = "[{index}] abs({0}/{1})")
    @CsvSource({
        "50, 75, 50, 75",
        "-50, 75, 50, 75",
        "2147483647, 1, 2147483647, 1",
        "2147483647, -1, 2147483647, 1"
    })
    void testAbs(final int numerator, final int denominator, final int expectedNumerator, final int expectedDenominator) {
        final Fraction result = Fraction.getFraction(numerator, denominator).abs();
        assertEquals(expectedNumerator, result.getNumerator());
        assertEquals(expectedDenominator, result.getDenominator());
    }

    @Test
    void testAbsOverflow() {
        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(Integer.MIN_VALUE, 1).abs());
    }

    @Test
    void testAdd() {
        Fraction f;
        Fraction f1;
        Fraction f2;

        f1 = Fraction.getFraction(3, 5);
        f2 = Fraction.getFraction(1, 5);
        f = f1.add(f2);
        assertEquals(4, f.getNumerator());
        assertEquals(5, f.getDenominator());

        f1 = Fraction.getFraction(3, 5);
        f2 = Fraction.getFraction(2, 5);
        f = f1.add(f2);
        assertEquals(1, f.getNumerator());
        assertEquals(1, f.getDenominator());

        f1 = Fraction.getFraction(3, 5);
        f2 = Fraction.getFraction(3, 5);
        f = f1.add(f2);
        assertEquals(6, f.getNumerator());
        assertEquals(5, f.getDenominator());

        f1 = Fraction.getFraction(3, 5);
        f2 = Fraction.getFraction(-4, 5);
        f = f1.add(f2);
        assertEquals(-1, f.getNumerator());
        assertEquals(5, f.getDenominator());

        f1 = Fraction.getFraction(Integer.MAX_VALUE - 1, 1);
        f2 = Fraction.ONE;
        f = f1.add(f2);
        assertEquals(Integer.MAX_VALUE, f.getNumerator());
        assertEquals(1, f.getDenominator());

        f1 = Fraction.getFraction(3, 5);
        f2 = Fraction.getFraction(1, 2);
        f = f1.add(f2);
        assertEquals(11, f.getNumerator());
        assertEquals(10, f.getDenominator());

        f1 = Fraction.getFraction(3, 8);
        f2 = Fraction.getFraction(1, 6);
        f = f1.add(f2);
        assertEquals(13, f.getNumerator());
        assertEquals(24, f.getDenominator());

        f1 = Fraction.getFraction(0, 5);
        f2 = Fraction.getFraction(1, 5);
        f = f1.add(f2);
        assertSame(f2, f);
        f = f2.add(f1);
        assertSame(f2, f);

        f1 = Fraction.getFraction(-1, 13 * 13 * 2 * 2);
        f2 = Fraction.getFraction(-2, 13 * 17 * 2);
        final Fraction fr = f1.add(f2);
        assertEquals(13 * 13 * 17 * 2 * 2, fr.getDenominator());
        assertEquals(-17 - 2 * 13 * 2, fr.getNumerator());

        assertNullPointerException(() -> fr.add(null));

        // if this fraction is added naively, it will overflow.
        // check that it doesn't.
        f1 = Fraction.getFraction(1, 32768 * 3);
        f2 = Fraction.getFraction(1, 59049);
        f = f1.add(f2);
        assertEquals(52451, f.getNumerator());
        assertEquals(1934917632, f.getDenominator());

        f1 = Fraction.getFraction(Integer.MIN_VALUE, 3);
        f2 = Fraction.ONE_THIRD;
        f = f1.add(f2);
        assertEquals(Integer.MIN_VALUE + 1, f.getNumerator());
        assertEquals(3, f.getDenominator());

        f1 = Fraction.getFraction(Integer.MAX_VALUE - 1, 1);
        f2 = Fraction.ONE;
        f = f1.add(f2);
        assertEquals(Integer.MAX_VALUE, f.getNumerator());
        assertEquals(1, f.getDenominator());

        final Fraction overflower = f;
        assertThrows(ArithmeticException.class, () -> overflower.add(Fraction.ONE)); // should overflow

        // denominator should not be a multiple of 2 or 3 to trigger overflow
        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(Integer.MIN_VALUE, 5).add(Fraction.getFraction(-1, 5)));

        final Fraction maxValue = Fraction.getFraction(Integer.MAX_VALUE, 1);
        assertThrows(ArithmeticException.class, () -> maxValue.add(maxValue));

        final Fraction negativeMaxValue = Fraction.getFraction(-Integer.MAX_VALUE, 1);
        assertThrows(ArithmeticException.class, () -> negativeMaxValue.add(negativeMaxValue));

        final Fraction f3 = Fraction.getFraction(3, 327680);
        final Fraction f4 = Fraction.getFraction(2, 59049);
        assertThrows(ArithmeticException.class, () -> f3.add(f4)); // should overflow

        // the cross products u*v' and u'*v overflow an int, but the reduced result fits.
        f1 = Fraction.getFraction(Integer.MAX_VALUE, 2);
        f2 = Fraction.getFraction(-Integer.MAX_VALUE, 1);
        f = f1.add(f2);
        assertEquals(-Integer.MAX_VALUE, f.getNumerator());
        assertEquals(2, f.getDenominator());

        f1 = Fraction.getFraction(2, 1);
        f2 = Fraction.getFraction(-Integer.MAX_VALUE, 2114962910);
        f = f1.add(f2);
        assertEquals(2082442173, f.getNumerator());
        assertEquals(2114962910, f.getDenominator());
    }

    @ParameterizedTest(name = "[{index}] {0} + {1}")
    @MethodSource("arithmeticBoundaryValues")
    void testAddBoundaryValues(final Fraction left, final Fraction right) {
        final BigInteger numerator = BigInteger.valueOf(left.getNumerator()).multiply(BigInteger.valueOf(right.getDenominator()))
                .add(BigInteger.valueOf(right.getNumerator()).multiply(BigInteger.valueOf(left.getDenominator())));
        final BigInteger denominator = BigInteger.valueOf(left.getDenominator()).multiply(BigInteger.valueOf(right.getDenominator()));
        assertReducedResult(numerator, denominator, () -> left.add(right));
    }

    @Test
    void testAddSubtractUnreducedOperands() {
        // 1073741823/2147483646 is 1/2, and 1/2 + 3/5 is 11/10.
        Fraction f = Fraction.getFraction(1073741823, 2147483646).add(Fraction.getFraction(3, 5));
        assertEquals(11, f.getNumerator());
        assertEquals(10, f.getDenominator());

        f = Fraction.getFraction(1073741823, 2147483646).subtract(Fraction.getFraction(3, 5));
        assertEquals(-1, f.getNumerator());
        assertEquals(10, f.getDenominator());

        // 2147483646/2147483646 is 1, and 1 + -11 is -10.
        f = Fraction.getFraction(2147483646, 2147483646).add(Fraction.getFraction(-11, 1));
        assertEquals(-10, f.getNumerator());
        assertEquals(1, f.getDenominator());

        // add() returns the result in reduced form.
        f = Fraction.getFraction(50, 100).add(Fraction.getFraction(1, 3));
        assertEquals(5, f.getNumerator());
        assertEquals(6, f.getDenominator());

        f = Fraction.getFraction(2, 4).add(Fraction.getFraction(1, 2));
        assertEquals(1, f.getNumerator());
        assertEquals(1, f.getDenominator());

        // Reducing the operands by hand must not change the answer.
        assertEquals(Fraction.getFraction(7, 13).reduce().add(Fraction.getFraction(46341, 1073741823).reduce()),
                Fraction.getFraction(7, 13).add(Fraction.getFraction(46341, 1073741823)));

        // Both operands unreduced: 2/4 + 2/6 is 1/2 + 1/3.
        f = Fraction.getFraction(2, 4).add(Fraction.getFraction(2, 6));
        assertEquals(5, f.getNumerator());
        assertEquals(6, f.getDenominator());

        // Reduced denominators share a factor: 2/4 - 2/12 is 1/2 - 1/6.
        f = Fraction.getFraction(2, 4).subtract(Fraction.getFraction(2, 12));
        assertEquals(1, f.getNumerator());
        assertEquals(3, f.getDenominator());

        // Equal values cancel to 0/1.
        f = Fraction.getFraction(2, 4).subtract(Fraction.getFraction(3, 6));
        assertEquals(0, f.getNumerator());
        assertEquals(1, f.getDenominator());

        // Integer.MIN_VALUE/2 reduces to -1073741824/1 without overflowing.
        f = Fraction.getFraction(Integer.MIN_VALUE, 2).add(Fraction.getFraction(2, 4));
        assertEquals(-Integer.MAX_VALUE, f.getNumerator());
        assertEquals(2, f.getDenominator());

        // A result that genuinely does not fit an int still overflows.
        final Fraction maxValue = Fraction.getFraction(-Integer.MAX_VALUE, 1);
        assertThrows(ArithmeticException.class, () -> maxValue.add(maxValue));
    }

    @Test
    void testAddSubtractZeroOperand() {
        // A zero operand returns the other operand in reduced form.
        Fraction f = Fraction.ZERO.add(Fraction.getFraction(2, 4));
        assertEquals(1, f.getNumerator());
        assertEquals(2, f.getDenominator());

        f = Fraction.getFraction(2, 4).add(Fraction.ZERO);
        assertEquals(1, f.getNumerator());
        assertEquals(2, f.getDenominator());

        f = Fraction.ZERO.subtract(Fraction.getFraction(2, 4));
        assertEquals(-1, f.getNumerator());
        assertEquals(2, f.getDenominator());

        f = Fraction.getFraction(2, 4).subtract(Fraction.ZERO);
        assertEquals(1, f.getNumerator());
        assertEquals(2, f.getDenominator());

        // Integer.MIN_VALUE/2 reduces to -1073741824/1, whose negation fits an int.
        f = Fraction.ZERO.subtract(Fraction.getFraction(Integer.MIN_VALUE, 2));
        assertEquals(1073741824, f.getNumerator());
        assertEquals(1, f.getDenominator());

        // Integer.MIN_VALUE/1 is in lowest terms and still cannot be negated.
        assertThrows(ArithmeticException.class, () -> Fraction.ZERO.subtract(Fraction.getFraction(Integer.MIN_VALUE, 1)));

        // both operands being unreduced zeros
        assertEquals(Fraction.ZERO, Fraction.getFraction(0, 2).add(Fraction.getFraction(0, 3)));
        assertEquals(Fraction.ZERO, Fraction.getFraction(0, 2).subtract(Fraction.getFraction(0, 3)));
    }

    @Test
    void testCompareTo() {
        final Fraction f1;
        Fraction f2;

        f1 = Fraction.getFraction(3, 5);
        assertEquals(0, f1.compareTo(f1));

        final Fraction fr = f1;
        assertNullPointerException(() -> fr.compareTo(null));

        f2 = Fraction.getFraction(2, 5);
        assertTrue(f1.compareTo(f2) > 0);
        assertEquals(0, f2.compareTo(f2));

        f2 = Fraction.getFraction(4, 5);
        assertTrue(f1.compareTo(f2) < 0);
        assertEquals(0, f2.compareTo(f2));

        f2 = Fraction.getFraction(3, 5);
        assertEquals(0, f1.compareTo(f2));
        assertEquals(0, f2.compareTo(f2));

        f2 = Fraction.getFraction(6, 10);
        assertEquals(0, f1.compareTo(f2));
        assertEquals(0, f2.compareTo(f2));

        f2 = Fraction.getFraction(-1, 1, Integer.MAX_VALUE);
        assertTrue(f1.compareTo(f2) > 0);
        assertEquals(0, f2.compareTo(f2));

    }

    @ParameterizedTest(name = "[{index}] compare {0} to {1}")
    @MethodSource("arithmeticBoundaryValues")
    void testCompareToBoundaryValues(final Fraction left, final Fraction right) {
        // Compare exact cross-products to check ordering without fixed-width overflow.
        final BigInteger leftProduct = BigInteger.valueOf(left.getNumerator()).multiply(BigInteger.valueOf(right.getDenominator()));
        final BigInteger rightProduct = BigInteger.valueOf(right.getNumerator()).multiply(BigInteger.valueOf(left.getDenominator()));
        final int expected = leftProduct.compareTo(rightProduct);
        final int actual = Integer.signum(left.compareTo(right));
        assertEquals(expected, actual);
        assertEquals(-actual, Integer.signum(right.compareTo(left)));
    }

    @Test
    void testConstants() {
        assertEquals(0, Fraction.ZERO.getNumerator());
        assertEquals(1, Fraction.ZERO.getDenominator());

        assertEquals(1, Fraction.ONE.getNumerator());
        assertEquals(1, Fraction.ONE.getDenominator());

        assertEquals(1, Fraction.ONE_HALF.getNumerator());
        assertEquals(2, Fraction.ONE_HALF.getDenominator());

        assertEquals(1, Fraction.ONE_THIRD.getNumerator());
        assertEquals(3, Fraction.ONE_THIRD.getDenominator());

        assertEquals(2, Fraction.TWO_THIRDS.getNumerator());
        assertEquals(3, Fraction.TWO_THIRDS.getDenominator());

        assertEquals(1, Fraction.ONE_QUARTER.getNumerator());
        assertEquals(4, Fraction.ONE_QUARTER.getDenominator());

        assertEquals(2, Fraction.TWO_QUARTERS.getNumerator());
        assertEquals(4, Fraction.TWO_QUARTERS.getDenominator());

        assertEquals(3, Fraction.THREE_QUARTERS.getNumerator());
        assertEquals(4, Fraction.THREE_QUARTERS.getDenominator());

        assertEquals(1, Fraction.ONE_FIFTH.getNumerator());
        assertEquals(5, Fraction.ONE_FIFTH.getDenominator());

        assertEquals(2, Fraction.TWO_FIFTHS.getNumerator());
        assertEquals(5, Fraction.TWO_FIFTHS.getDenominator());

        assertEquals(3, Fraction.THREE_FIFTHS.getNumerator());
        assertEquals(5, Fraction.THREE_FIFTHS.getDenominator());

        assertEquals(4, Fraction.FOUR_FIFTHS.getNumerator());
        assertEquals(5, Fraction.FOUR_FIFTHS.getDenominator());
    }

    @Test
    void testConversions() {
        final Fraction f;

        f = Fraction.getFraction(3, 7, 8);
        assertEquals(3, f.intValue());
        assertEquals(3L, f.longValue());
        assertEquals(3.875f, f.floatValue(), 0.00001f);
        assertEquals(3.875d, f.doubleValue(), 0.00001d);
    }

    @Test
    void testDivide() {
        Fraction f;
        Fraction f1;
        Fraction f2;

        f1 = Fraction.getFraction(3, 5);
        f2 = Fraction.getFraction(2, 5);
        f = f1.divideBy(f2);
        assertEquals(3, f.getNumerator());
        assertEquals(2, f.getDenominator());

        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(3, 5).divideBy(Fraction.ZERO));

        f1 = Fraction.getFraction(0, 5);
        f2 = Fraction.getFraction(2, 7);
        f = f1.divideBy(f2);
        assertSame(Fraction.ZERO, f);

        f1 = Fraction.getFraction(2, 7);
        f2 = Fraction.ONE;
        f = f1.divideBy(f2);
        assertEquals(2, f.getNumerator());
        assertEquals(7, f.getDenominator());

        f1 = Fraction.getFraction(1, Integer.MAX_VALUE);
        f = f1.divideBy(f1);
        assertEquals(1, f.getNumerator());
        assertEquals(1, f.getDenominator());

        f1 = Fraction.getFraction(Integer.MIN_VALUE, Integer.MAX_VALUE);
        f2 = Fraction.getFraction(1, Integer.MAX_VALUE);
        final Fraction fr = f1.divideBy(f2);
        assertEquals(Integer.MIN_VALUE, fr.getNumerator());
        assertEquals(1, fr.getDenominator());

        assertNullPointerException(() -> fr.divideBy(null));

        final Fraction smallest = Fraction.getFraction(1, Integer.MAX_VALUE);
        assertThrows(ArithmeticException.class, () -> smallest.divideBy(smallest.invert())); // Should overflow

        final Fraction negative = Fraction.getFraction(1, -Integer.MAX_VALUE);
        assertThrows(ArithmeticException.class, () -> negative.divideBy(negative.invert())); // Should overflow

        // An unreduced divisor must not trigger a spurious overflow when the reduced quotient fits an int.
        f = Fraction.getFraction(-1, 46341).divideBy(Fraction.getFraction(1000000, 100));
        assertEquals(-1, f.getNumerator());
        assertEquals(463410000, f.getDenominator());
    }

    @ParameterizedTest(name = "[{index}] {0} / {1}")
    @MethodSource("arithmeticBoundaryValues")
    void testDivideBoundaryValues(final Fraction dividend, final Fraction divisor) {
        if (divisor.getNumerator() == 0) {
            assertThrows(ArithmeticException.class, () -> dividend.divideBy(divisor));
            return;
        }

        // Use arbitrary-precision arithmetic as an independent oracle for the reduced quotient and its range.
        final BigInteger numerator = BigInteger.valueOf(dividend.getNumerator()).multiply(BigInteger.valueOf(divisor.getDenominator()));
        final BigInteger denominator = BigInteger.valueOf(dividend.getDenominator()).multiply(BigInteger.valueOf(divisor.getNumerator()));
        final BigInteger signedGcd = numerator.gcd(denominator).multiply(BigInteger.valueOf(denominator.signum()));
        final BigInteger expectedNumerator = numerator.divide(signedGcd);
        final BigInteger expectedDenominator = denominator.divide(signedGcd);
        final BigInteger minValue = BigInteger.valueOf(Integer.MIN_VALUE);
        final BigInteger maxValue = BigInteger.valueOf(Integer.MAX_VALUE);

        if (expectedNumerator.compareTo(minValue) < 0 || expectedNumerator.compareTo(maxValue) > 0
                || expectedDenominator.compareTo(maxValue) > 0) {
            assertThrows(ArithmeticException.class, () -> dividend.divideBy(divisor));
        } else {
            final Fraction quotient = dividend.divideBy(divisor);
            assertEquals(expectedNumerator.intValueExact(), quotient.getNumerator());
            assertEquals(expectedDenominator.intValueExact(), quotient.getDenominator());
            if (expectedNumerator.signum() == 0) {
                assertSame(Fraction.ZERO, quotient);
            }
        }
    }

    @ParameterizedTest
    @CsvSource({
        "-2147483648, 1, -2147483648, 1, 1, 1",
        "-2147483648, 1, -2147483648, 2147483647, 2147483647, 1",
        "-2147483648, 2147483647, -2147483648, 2147483647, 1, 1",
        "-2147483648, 2, -2147483648, 4, 2, 1",
        "-2147483648, 3, -2147483648, 5, 5, 3",
        "-2147483648, 6, -2147483648, 10, 5, 3",
        "-1, 1, -2147483648, 2, 1, 1073741824",
        "1, 1, -2147483648, 2, -1, 1073741824",
        "2, 1, -2147483648, 1, -1, 1073741824",
        "-2147483648, 2, -1, 1, 1073741824, 1",
        "-2147483648, 1, 1, 1, -2147483648, 1"
    })
    void testDivideIntegerMinValue(final int numerator, final int denominator, final int divisorNumerator, final int divisorDenominator,
            final int expectedNumerator, final int expectedDenominator) {
        final Fraction quotient = Fraction.getFraction(numerator, denominator).divideBy(Fraction.getFraction(divisorNumerator, divisorDenominator));
        assertEquals(expectedNumerator, quotient.getNumerator());
        assertEquals(expectedDenominator, quotient.getDenominator());
    }

    @ParameterizedTest
    @CsvSource({
        "1, 1, -2147483648, 1",
        "-1, 1, -2147483648, 1",
        "-2147483648, 1, -1, 1",
        "-2147483648, 1, 1, 2",
        "1, 2147483647, 2147483647, 1",
        "2147483647, 1, 1, 2147483647",
        "2, 3, -2147483648, 1"
    })
    void testDivideOverflow(final int numerator, final int denominator, final int divisorNumerator, final int divisorDenominator) {
        final Fraction dividend = Fraction.getFraction(numerator, denominator);
        final Fraction divisor = Fraction.getFraction(divisorNumerator, divisorDenominator);
        assertThrows(ArithmeticException.class, () -> dividend.divideBy(divisor));
    }

    @Test
    void testDivideZeroByIntegerMinValue() {
        final Fraction divisor = Fraction.getFraction(Integer.MIN_VALUE, 1);
        assertSame(Fraction.ZERO, Fraction.ZERO.divideBy(divisor));
        assertSame(Fraction.ZERO, Fraction.getFraction(0, 5).divideBy(divisor));
        assertNullPointerException(() -> Fraction.ZERO.divideBy(null));
        assertThrows(ArithmeticException.class, () -> Fraction.ZERO.divideBy(Fraction.ZERO));
    }

    @Test
    void testEquals() {
        Fraction f1;
        Fraction f2;

        f1 = Fraction.getFraction(3, 5);
        assertNotEquals(null, f1);
        assertNotEquals(f1, new Object());
        assertNotEquals(f1, Integer.valueOf(6));

        f1 = Fraction.getFraction(3, 5);
        f2 = Fraction.getFraction(2, 5);
        assertNotEquals(f1, f2);
        assertEquals(f1, f1);
        assertEquals(f2, f2);

        f2 = Fraction.getFraction(3, 5);
        assertEquals(f1, f2);

        f2 = Fraction.getFraction(6, 10);
        assertNotEquals(f1, f2);
    }

    @ParameterizedTest(name = "[{index}] getFraction({0})")
    @MethodSource("factoryDoubleValues")
    void testFactory_double(final double value, final int expectedNumerator, final int expectedDenominator) {
        final Fraction result = Fraction.getFraction(value);
        assertEquals(expectedNumerator, result.getNumerator());
        assertEquals(expectedDenominator, result.getDenominator());
    }

    @ParameterizedTest(name = "[{index}] getFraction({0}) throws")
    @ValueSource(doubles = {Double.NaN, Double.POSITIVE_INFINITY, Double.NEGATIVE_INFINITY, (double) Integer.MAX_VALUE + 1, 2147483646.5d, 1073741824.5d})
    void testFactory_doubleInvalid(final double value) {
        // The fractional boundary values must throw instead of overflowing the numerator to a negative value.
        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(value));
    }

    @Test
    void testFactory_doubleRoundTrip() {
        Fraction f;
        Fraction f2;
        for (int i = 1; i <= 100; i++) { // denominator
            for (int j = 1; j <= i; j++) { // numerator
                final double value = (double) j / (double) i;
                final String message = "getFraction(" + j + "/" + i + " = " + value + ")";
                f = assertDoesNotThrow(() -> Fraction.getFraction(value), message);

                f2 = Fraction.getReducedFraction(j, i);
                assertEquals(f2.getNumerator(), f.getNumerator(), message);
                assertEquals(f2.getDenominator(), f.getDenominator(), message);
            }
        }
        // save time by skipping some tests! (
        for (int i = 1001; i <= 10000; i += SKIP) { // denominator
            for (int j = 1; j <= i; j++) { // numerator
                final double value = (double) j / (double) i;
                final String message = "getFraction(" + j + "/" + i + " = " + value + ")";
                f = assertDoesNotThrow(() -> Fraction.getFraction(value), message);
                f2 = Fraction.getReducedFraction(j, i);
                assertEquals(f2.getNumerator(), f.getNumerator(), message);
                assertEquals(f2.getDenominator(), f.getDenominator(), message);
            }
        }
    }

    @ParameterizedTest(name = "[{index}] getFraction({0}, {1})")
    @CsvSource({
        "0, 1, 0, 1",
        "0, 2, 0, 2",
        "1, 1, 1, 1",
        "2, 1, 2, 1",
        "23, 345, 23, 345",
        "22, 7, 22, 7",
        "-6, 10, -6, 10",
        "6, -10, -6, 10",
        "-6, -10, 6, 10"
    })
    void testFactory_int_int(final int numerator, final int denominator, final int expectedNumerator, final int expectedDenominator) {
        final Fraction result = Fraction.getFraction(numerator, denominator);
        assertEquals(expectedNumerator, result.getNumerator());
        assertEquals(expectedDenominator, result.getDenominator());
    }

    @ParameterizedTest(name = "[{index}] getFraction({0}, {1}) throws")
    @CsvSource({
        "1, 0",
        "2, 0",
        "-3, 0",
        "4, -2147483648",
        "1, -2147483648"
    })
    void testFactory_int_intInvalid(final int numerator, final int denominator) {
        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(numerator, denominator));
    }

    @ParameterizedTest(name = "[{index}] getFraction({0}, {1}, {2})")
    @CsvSource({
        "0, 0, 2, 0, 2",
        "2, 0, 2, 4, 2",
        "0, 1, 2, 1, 2",
        "1, 1, 2, 3, 2",
        "-1, 6, 10, -16, 10",
        "-1, 0, 2147483647, -2147483647, 2147483647"
    })
    void testFactory_int_int_int(final int whole, final int numerator, final int denominator, final int expectedNumerator, final int expectedDenominator) {
        final Fraction result = Fraction.getFraction(whole, numerator, denominator);
        assertEquals(expectedNumerator, result.getNumerator());
        assertEquals(expectedDenominator, result.getDenominator());
    }

    @ParameterizedTest(name = "[{index}] getFraction({0}, {1}, {2}) throws")
    @CsvSource({
        "1, -6, -10",
        "-1, -6, 10",
        "-1, 6, -10",
        "-1, -6, -10",
        "0, 1, 0",
        "1, 2, 0",
        "-1, -3, 0",
        "2147483647, 1, 2",
        "-2147483647, 1, 2",
        "0, 4, -2147483648",
        "1, 1, 2147483647",
        "-1, 2, 2147483647"
    })
    void testFactory_int_int_intInvalid(final int whole, final int numerator, final int denominator) {
        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(whole, numerator, denominator));
    }

    @Test
    void testFactory_String() {
        assertNullPointerException(() -> Fraction.getFraction(null));
    }

    @ParameterizedTest(name = "[{index}] getFraction({0})")
    @CsvSource({
        "'0.0', 0, 1",
        "'0.2', 1, 5",
        "'0.5', 1, 2",
        "'0.66666', 2, 3"
    })
    void testFactory_String_double(final String value, final int expectedNumerator, final int expectedDenominator) {
        final Fraction result = Fraction.getFraction(value);
        assertEquals(expectedNumerator, result.getNumerator());
        assertEquals(expectedDenominator, result.getDenominator());
    }

    @ParameterizedTest(name = "[{index}] getFraction({0}) throws")
    @ValueSource(strings = {"2.3R", "2147483648", "."})
    void testFactory_String_doubleInvalid(final String value) {
        assertThrows(NumberFormatException.class, () -> Fraction.getFraction(value));
    }

    @ParameterizedTest(name = "[{index}] getFraction({0})")
    @CsvSource({
        "'0/1', 0, 1",
        "'1/5', 1, 5",
        "'1/2', 1, 2",
        "'2/3', 2, 3",
        "'7/3', 7, 3",
        "'2/4', 2, 4"
    })
    void testFactory_String_improper(final String value, final int expectedNumerator, final int expectedDenominator) {
        final Fraction result = Fraction.getFraction(value);
        assertEquals(expectedNumerator, result.getNumerator());
        assertEquals(expectedDenominator, result.getDenominator());
    }

    @ParameterizedTest(name = "[{index}] getFraction({0}) throws")
    @ValueSource(strings = {"2/d", "2e/3", "2/", "/"})
    void testFactory_String_improperInvalid(final String value) {
        assertThrows(NumberFormatException.class, () -> Fraction.getFraction(value));
    }

    @ParameterizedTest(name = "[{index}] getFraction({0})")
    @CsvSource({
        "'0 0/1', 0, 1",
        "'1 1/5', 6, 5",
        "'7 1/2', 15, 2",
        "'1 2/4', 6, 4",
        "'-7 1/2', -15, 2",
        "'-1 2/4', -6, 4"
    })
    void testFactory_String_proper(final String value, final int expectedNumerator, final int expectedDenominator) {
        final Fraction result = Fraction.getFraction(value);
        assertEquals(expectedNumerator, result.getNumerator());
        assertEquals(expectedDenominator, result.getDenominator());
    }

    @ParameterizedTest(name = "[{index}] getFraction({0}) throws")
    @ValueSource(strings = {"2 3", "a 3", "2 b/4", "2 ", " 3", " "})
    void testFactory_String_properInvalid(final String value) {
        assertThrows(NumberFormatException.class, () -> Fraction.getFraction(value));
    }

    /**
     * Tests that string contents that are well-formed but unrepresentable throw the documented NumberFormatException
     * (with the ArithmeticException preserved as the cause) instead of an undeclared ArithmeticException.
     *
     * @param value A well-formed string whose value cannot be represented.
     */
    @ParameterizedTest(name = "[{index}] getFraction({0}) preserves the arithmetic cause")
    @ValueSource(strings = {
        // Decimal values outside the supported range.
        "9999999999.5", "2147483648.5",
        // Y/Z: zero denominator and negation overflow.
        "1/0", "1/-2147483648",
        // X Y/Z: zero denominator and combined-numerator overflow.
        "1 2/0", "2147483647 1/2"
    })
    void testFactory_String_unrepresentableThrowsNumberFormatException(final String value) {
        final NumberFormatException exception = assertThrows(NumberFormatException.class, () -> Fraction.getFraction(value));
        assertInstanceOf(ArithmeticException.class, exception.getCause());
    }

    @Test
    void testGets() {
        Fraction f;

        f = Fraction.getFraction(3, 5, 6);
        assertEquals(23, f.getNumerator());
        assertEquals(3, f.getProperWhole());
        assertEquals(5, f.getProperNumerator());
        assertEquals(6, f.getDenominator());

        f = Fraction.getFraction(-3, 5, 6);
        assertEquals(-23, f.getNumerator());
        assertEquals(-3, f.getProperWhole());
        assertEquals(5, f.getProperNumerator());
        assertEquals(6, f.getDenominator());

        f = Fraction.getFraction(Integer.MIN_VALUE, 0, 1);
        assertEquals(Integer.MIN_VALUE, f.getNumerator());
        assertEquals(Integer.MIN_VALUE, f.getProperWhole());
        assertEquals(0, f.getProperNumerator());
        assertEquals(1, f.getDenominator());
    }

    @Test
    void testHashCode() {
        final Fraction f1 = Fraction.getFraction(3, 5);
        Fraction f2 = Fraction.getFraction(3, 5);
        assertEquals(f1.hashCode(), f2.hashCode());
        f2 = Fraction.getFraction(2, 5);
        assertTrue(f1.hashCode() != f2.hashCode());
        f2 = Fraction.getFraction(6, 10);
        assertTrue(f1.hashCode() != f2.hashCode());
    }

    /**
     * Tests https://issues.apache.org/jira/browse/LANG-1764
     */
    @ParameterizedTest
    // @formatter:off
    @CsvSource({
        "0,          37,         -464320789,  46",
        "0,          37,         -464320788,  9",
        "0,          37,         1857283155,  38",
        "0,          25185704,   1161454280,  1050304",
        "0,          38817068,   1509581512,  18875972",
        "0,          38817068,   -2146369536, 2145078572",
        "1400217380, 128,        2092630052,  150535040",
        "1400217380, 128,        -580400986,  268435638",
        "1400217380, 2147483592, -2147483648, 268435452",
        "1756395909, 4194598,    1174949894,  42860673"
    })
    // @formatter:on
    void testHashCodeNotEquals(final int f1n, final int f1d, final int f2n, final int f2d) {
        assertNotEquals(Fraction.getFraction(f1n, f1d), Fraction.getFraction(f2n, f2d));
        assertNotEquals(Fraction.getFraction(f1n, f1d).hashCode(), Fraction.getFraction(f2n, f2d).hashCode());
    }

    @ParameterizedTest(name = "[{index}] invert({0}/{1})")
    @CsvSource({
        "50, 75, 75, 50",
        "4, 3, 3, 4",
        "-15, 47, -47, 15",
        "2147483647, 1, 1, 2147483647"
    })
    void testInvert(final int numerator, final int denominator, final int expectedNumerator, final int expectedDenominator) {
        final Fraction result = Fraction.getFraction(numerator, denominator).invert();
        assertEquals(expectedNumerator, result.getNumerator());
        assertEquals(expectedDenominator, result.getDenominator());
    }

    @ParameterizedTest(name = "[{index}] invert({0}/{1}) throws")
    @CsvSource({
        "0, 3",
        "-2147483648, 1",
        "-2147483648, 2"
    })
    void testInvertInvalid(final int numerator, final int denominator) {
        // Inversion does not reduce, even when the reduced reciprocal would be representable.
        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(numerator, denominator).invert());
    }

    @Test
    void testMultiply() {
        Fraction f;
        Fraction f1;
        Fraction f2;

        f1 = Fraction.getFraction(3, 5);
        f2 = Fraction.getFraction(2, 5);
        f = f1.multiplyBy(f2);
        assertEquals(6, f.getNumerator());
        assertEquals(25, f.getDenominator());

        f1 = Fraction.getFraction(6, 10);
        f2 = Fraction.getFraction(6, 10);
        f = f1.multiplyBy(f2);
        assertEquals(9, f.getNumerator());
        assertEquals(25, f.getDenominator());
        f = f.multiplyBy(f2);
        assertEquals(27, f.getNumerator());
        assertEquals(125, f.getDenominator());

        f1 = Fraction.getFraction(3, 5);
        f2 = Fraction.getFraction(-2, 5);
        f = f1.multiplyBy(f2);
        assertEquals(-6, f.getNumerator());
        assertEquals(25, f.getDenominator());

        f1 = Fraction.getFraction(-3, 5);
        f2 = Fraction.getFraction(-2, 5);
        f = f1.multiplyBy(f2);
        assertEquals(6, f.getNumerator());
        assertEquals(25, f.getDenominator());

        f1 = Fraction.getFraction(0, 5);
        f2 = Fraction.getFraction(2, 7);
        f = f1.multiplyBy(f2);
        assertSame(Fraction.ZERO, f);

        f1 = Fraction.getFraction(2, 7);
        f2 = Fraction.ONE;
        f = f1.multiplyBy(f2);
        assertEquals(2, f.getNumerator());
        assertEquals(7, f.getDenominator());

        f1 = Fraction.getFraction(Integer.MAX_VALUE, 1);
        f2 = Fraction.getFraction(Integer.MIN_VALUE, Integer.MAX_VALUE);
        f = f1.multiplyBy(f2);
        assertEquals(Integer.MIN_VALUE, f.getNumerator());
        assertEquals(1, f.getDenominator());

        final Fraction fr = f;
        assertNullPointerException(() -> fr.multiplyBy(null));

        final Fraction fr1 = Fraction.getFraction(1, Integer.MAX_VALUE);
        assertThrows(ArithmeticException.class, () -> fr1.multiplyBy(fr1));

        final Fraction fr2 = Fraction.getFraction(1, -Integer.MAX_VALUE);
        assertThrows(ArithmeticException.class, () -> fr2.multiplyBy(fr2));

        // An unreduced operand must not trigger a spurious overflow when the reduced product fits an int.
        f = Fraction.getFraction(-1, 46341).multiplyBy(Fraction.getFraction(100, 1000000));
        assertEquals(-1, f.getNumerator());
        assertEquals(463410000, f.getDenominator());

        f = Fraction.getFraction(1, 10000).multiplyBy(Fraction.getFraction(100, 1000000));
        assertEquals(1, f.getNumerator());
        assertEquals(100000000, f.getDenominator());
    }

    @ParameterizedTest(name = "[{index}] {0} * {1}")
    @MethodSource("arithmeticBoundaryValues")
    void testMultiplyBoundaryValues(final Fraction left, final Fraction right) {
        if (left.getNumerator() == 0 || right.getNumerator() == 0) {
            assertSame(Fraction.ZERO, left.multiplyBy(right));
            return;
        }
        final BigInteger numerator = BigInteger.valueOf(left.getNumerator()).multiply(BigInteger.valueOf(right.getNumerator()));
        final BigInteger denominator = BigInteger.valueOf(left.getDenominator()).multiply(BigInteger.valueOf(right.getDenominator()));
        assertReducedResult(numerator, denominator, () -> left.multiplyBy(right));
    }

    @ParameterizedTest(name = "[{index}] negate({0}/{1})")
    @CsvSource({
        "50, 75, -50, 75",
        "-50, 75, 50, 75",
        "2147483646, 2147483647, -2147483646, 2147483647"
    })
    void testNegate(final int numerator, final int denominator, final int expectedNumerator, final int expectedDenominator) {
        final Fraction result = Fraction.getFraction(numerator, denominator).negate();
        assertEquals(expectedNumerator, result.getNumerator());
        assertEquals(expectedDenominator, result.getDenominator());
    }

    @Test
    void testNegateOverflow() {
        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(Integer.MIN_VALUE, 1).negate());
    }

    @Test
    void testPow() {
        Fraction f;

        f = Fraction.getFraction(3, 5);
        assertEquals(Fraction.ONE, f.pow(0));

        f = Fraction.getFraction(3, 5);
        assertSame(f, f.pow(1));
        assertEquals(f, f.pow(1));

        f = Fraction.getFraction(3, 5);
        f = f.pow(2);
        assertEquals(9, f.getNumerator());
        assertEquals(25, f.getDenominator());

        f = Fraction.getFraction(3, 5);
        f = f.pow(3);
        assertEquals(27, f.getNumerator());
        assertEquals(125, f.getDenominator());

        f = Fraction.getFraction(3, 5);
        f = f.pow(-1);
        assertEquals(5, f.getNumerator());
        assertEquals(3, f.getDenominator());

        f = Fraction.getFraction(3, 5);
        f = f.pow(-2);
        assertEquals(25, f.getNumerator());
        assertEquals(9, f.getDenominator());

        // check unreduced fractions stay that way.
        f = Fraction.getFraction(6, 10);
        assertEquals(Fraction.ONE, f.pow(0));

        f = Fraction.getFraction(6, 10);
        assertEquals(f, f.pow(1));
        assertNotEquals(f.pow(1), Fraction.getFraction(3, 5));

        f = Fraction.getFraction(6, 10);
        f = f.pow(2);
        assertEquals(9, f.getNumerator());
        assertEquals(25, f.getDenominator());

        f = Fraction.getFraction(6, 10);
        f = f.pow(3);
        assertEquals(27, f.getNumerator());
        assertEquals(125, f.getDenominator());

        f = Fraction.getFraction(6, 10);
        f = f.pow(-1);
        assertEquals(10, f.getNumerator());
        assertEquals(6, f.getDenominator());

        f = Fraction.getFraction(6, 10);
        f = f.pow(-2);
        assertEquals(25, f.getNumerator());
        assertEquals(9, f.getDenominator());

        // zero to any positive power is still zero.
        f = Fraction.getFraction(0, 1231);
        f = f.pow(1);
        assertEquals(0, f.compareTo(Fraction.ZERO));
        assertEquals(0, f.getNumerator());
        assertEquals(1231, f.getDenominator());
        f = f.pow(2);
        assertEquals(0, f.compareTo(Fraction.ZERO));
        assertEquals(0, f.getNumerator());
        assertEquals(1, f.getDenominator());

        // zero to negative powers should throw an exception
        final Fraction fr = f;
        assertThrows(ArithmeticException.class, () -> fr.pow(-1));
        assertThrows(ArithmeticException.class, () -> fr.pow(Integer.MIN_VALUE));

        // one to any power is still one.
        f = Fraction.getFraction(1, 1);
        f = f.pow(0);
        assertEquals(f, Fraction.ONE);
        f = f.pow(1);
        assertEquals(f, Fraction.ONE);
        f = f.pow(-1);
        assertEquals(f, Fraction.ONE);
        f = f.pow(Integer.MAX_VALUE);
        assertEquals(f, Fraction.ONE);
        f = f.pow(Integer.MIN_VALUE);
        assertEquals(f, Fraction.ONE);

        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(Integer.MAX_VALUE, 1).pow(2));

        // Numerator growing too negative during the pow operation.
        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(Integer.MIN_VALUE, 1).pow(3));

        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(65536, 1).pow(2));
    }

    @ParameterizedTest(name = "[{index}] ({0}/{1})^{2}")
    @CsvSource({
        "-2, 3, 2, 4, 9",
        "-2, 3, 3, -8, 27",
        "-2, 3, -2, 9, 4",
        "-2, 3, -3, -27, 8",
        "-4, 6, 2, 4, 9",
        "-4, 6, 3, -8, 27",
        "-4, 6, -2, 9, 4",
        "-4, 6, -3, -27, 8"
    })
    void testPowNegativeBase(final int numerator, final int denominator, final int power, final int expectedNumerator, final int expectedDenominator) {
        final Fraction result = Fraction.getFraction(numerator, denominator).pow(power);
        assertEquals(expectedNumerator, result.getNumerator());
        assertEquals(expectedDenominator, result.getDenominator());
    }

    @ParameterizedTest(name = "[{index}] (-1)^{0}")
    @CsvSource({
        "-2147483648, 1",
        "2147483647, -1"
    })
    void testPowNegativeOne(final int power, final int expectedNumerator) {
        final Fraction result = Fraction.getFraction(-1, 1).pow(power);
        assertEquals(expectedNumerator, result.getNumerator());
        assertEquals(1, result.getDenominator());
    }

    @Test
    void testPowZeroToZero() {
        assertSame(Fraction.ONE, Fraction.ZERO.pow(0));
        assertSame(Fraction.ONE, Fraction.getFraction(0, 5).pow(0));
    }

    @ParameterizedTest(name = "[{index}] reduce({0}/{1})")
    @CsvSource({
        "50, 75, 2, 3",
        "-2, -3, 2, 3",
        "2, -3, -2, 3",
        "-2, 3, -2, 3",
        "2, 3, 2, 3",
        "0, 1, 0, 1",
        "0, 100, 0, 1",
        "-2147483648, 2, -1073741824, 1"
    })
    void testReduce(final int numerator, final int denominator, final int expectedNumerator, final int expectedDenominator) {
        final Fraction result = Fraction.getFraction(numerator, denominator).reduce();
        assertEquals(expectedNumerator, result.getNumerator());
        assertEquals(expectedDenominator, result.getDenominator());
    }

    @ParameterizedTest(name = "[{index}] reduce({0}/{1}) preserves identity")
    @CsvSource({
        "-2, 3",
        "2, 3",
        "0, 1"
    })
    void testReduceIdentity(final int numerator, final int denominator) {
        final Fraction fraction = Fraction.getFraction(numerator, denominator);
        assertSame(fraction, fraction.reduce());
    }

    @Test
    void testReduceZero() {
        assertSame(Fraction.ZERO, Fraction.getFraction(0, 100).reduce());
    }

    @ParameterizedTest(name = "[{index}] getReducedFraction({0}, {1})")
    @CsvSource({
        "0, 1, 0, 1",
        "1, 1, 1, 1",
        "2, 1, 2, 1",
        "22, 7, 22, 7",
        "-6, 10, -3, 5",
        "6, -10, -3, 5",
        "-6, -10, 3, 5",
        "0, 2, 0, 1",
        "2, 2, 1, 1",
        "2, 4, 1, 2",
        "15, 10, 3, 2",
        "121, 22, 11, 2",
        "-2, -2147483648, 1, 1073741824",
        "-2147483648, 2, -1073741824, 1"
    })
    void testReducedFactory_int_int(final int numerator, final int denominator, final int expectedNumerator, final int expectedDenominator) {
        final Fraction result = Fraction.getReducedFraction(numerator, denominator);
        assertEquals(expectedNumerator, result.getNumerator());
        assertEquals(expectedDenominator, result.getDenominator());
    }

    @ParameterizedTest(name = "[{index}] getReducedFraction({0}, {1}) throws")
    @CsvSource({
        "1, 0",
        "2, 0",
        "-3, 0",
        "-7, -2147483648"
    })
    void testReducedFactory_int_intInvalid(final int numerator, final int denominator) {
        assertThrows(ArithmeticException.class, () -> Fraction.getReducedFraction(numerator, denominator));
    }

    @ParameterizedTest(name = "[{index}] getReducedFraction({0}, {1})")
    @CsvSource({
        "-2147483648, -2, 1073741824, 1",
        "-2147483648, -6, 1073741824, 3",
        "-2147483648, -2147483648, 1, 1",
        "-2, -2147483648, 1, 1073741824"
    })
    void testReducedFactoryIntegerMinValue(final int numerator, final int denominator, final int expectedNumerator, final int expectedDenominator) {
        final Fraction result = Fraction.getReducedFraction(numerator, denominator);
        assertEquals(expectedNumerator, result.getNumerator());
        assertEquals(expectedDenominator, result.getDenominator());
    }

    @ParameterizedTest(name = "[{index}] getReducedFraction({0}, {1}) throws")
    @CsvSource({
        "-2147483648, -1",
        "-2147483648, -3",
        "-7, -2147483648"
    })
    void testReducedFactoryIntegerMinValueInvalid(final int numerator, final int denominator) {
        assertThrows(ArithmeticException.class, () -> Fraction.getReducedFraction(numerator, denominator));
    }

    @Test
    void testSubtract() {
        Fraction f;
        Fraction f1;
        Fraction f2;

        f1 = Fraction.getFraction(3, 5);
        f2 = Fraction.getFraction(1, 5);
        f = f1.subtract(f2);
        assertEquals(2, f.getNumerator());
        assertEquals(5, f.getDenominator());

        f1 = Fraction.getFraction(7, 5);
        f2 = Fraction.getFraction(2, 5);
        f = f1.subtract(f2);
        assertEquals(1, f.getNumerator());
        assertEquals(1, f.getDenominator());

        f1 = Fraction.getFraction(3, 5);
        f2 = Fraction.getFraction(3, 5);
        f = f1.subtract(f2);
        assertEquals(0, f.getNumerator());
        assertEquals(1, f.getDenominator());

        f1 = Fraction.getFraction(3, 5);
        f2 = Fraction.getFraction(-4, 5);
        f = f1.subtract(f2);
        assertEquals(7, f.getNumerator());
        assertEquals(5, f.getDenominator());

        f1 = Fraction.getFraction(0, 5);
        f2 = Fraction.getFraction(4, 5);
        f = f1.subtract(f2);
        assertEquals(-4, f.getNumerator());
        assertEquals(5, f.getDenominator());

        f1 = Fraction.getFraction(0, 5);
        f2 = Fraction.getFraction(-4, 5);
        f = f1.subtract(f2);
        assertEquals(4, f.getNumerator());
        assertEquals(5, f.getDenominator());

        f1 = Fraction.getFraction(3, 5);
        f2 = Fraction.getFraction(1, 2);
        f = f1.subtract(f2);
        assertEquals(1, f.getNumerator());
        assertEquals(10, f.getDenominator());

        f1 = Fraction.getFraction(0, 5);
        f2 = Fraction.getFraction(1, 5);
        f = f2.subtract(f1);
        assertSame(f2, f);

        final Fraction fr = f;
        assertNullPointerException(() -> fr.subtract(null));

        // if this fraction is subtracted naively, it will overflow.
        // check that it doesn't.
        f1 = Fraction.getFraction(1, 32768 * 3);
        f2 = Fraction.getFraction(1, 59049);
        f = f1.subtract(f2);
        assertEquals(-13085, f.getNumerator());
        assertEquals(1934917632, f.getDenominator());

        f1 = Fraction.getFraction(Integer.MIN_VALUE, 3);
        f2 = Fraction.ONE_THIRD.negate();
        f = f1.subtract(f2);
        assertEquals(Integer.MIN_VALUE + 1, f.getNumerator());
        assertEquals(3, f.getDenominator());

        f1 = Fraction.getFraction(Integer.MAX_VALUE, 1);
        f2 = Fraction.ONE;
        f = f1.subtract(f2);
        assertEquals(Integer.MAX_VALUE - 1, f.getNumerator());
        assertEquals(1, f.getDenominator());

        // Should overflow
        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(1, Integer.MAX_VALUE).subtract(Fraction.getFraction(1, Integer.MAX_VALUE - 1)));

        // denominator should not be a multiple of 2 or 3 to trigger overflow
        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(Integer.MIN_VALUE, 5).subtract(Fraction.getFraction(1, 5)));

        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(Integer.MIN_VALUE, 1).subtract(Fraction.ONE));

        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(Integer.MAX_VALUE, 1).subtract(Fraction.ONE.negate()));

        // Should overflow
        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(3, 327680).subtract(Fraction.getFraction(2, 59049)));

        // the cross products u*v' and u'*v overflow an int, but the reduced result fits.
        f1 = Fraction.getFraction(Integer.MAX_VALUE, 2);
        f2 = Fraction.getFraction(Integer.MAX_VALUE, 1);
        f = f1.subtract(f2);
        assertEquals(-Integer.MAX_VALUE, f.getNumerator());
        assertEquals(2, f.getDenominator());
    }

    @ParameterizedTest(name = "[{index}] {0} - {1}")
    @MethodSource("arithmeticBoundaryValues")
    void testSubtractBoundaryValues(final Fraction left, final Fraction right) {
        final BigInteger numerator = BigInteger.valueOf(left.getNumerator()).multiply(BigInteger.valueOf(right.getDenominator()))
                .subtract(BigInteger.valueOf(right.getNumerator()).multiply(BigInteger.valueOf(left.getDenominator())));
        final BigInteger denominator = BigInteger.valueOf(left.getDenominator()).multiply(BigInteger.valueOf(right.getDenominator()));
        assertReducedResult(numerator, denominator, () -> left.subtract(right));
    }

    @Test
    void testToProperString() {
        Fraction f;

        f = Fraction.getFraction(3, 5);
        String str = f.toProperString();
        assertEquals("3/5", str);
        // Subsequent calls reuse the string cached by the first call.
        assertSame(str, f.toProperString());

        f = Fraction.getFraction(7, 5);
        str = f.toProperString();
        assertEquals("1 2/5", str);
        assertSame(str, f.toProperString());

        f = Fraction.getFraction(14, 10);
        str = f.toProperString();
        assertEquals("1 4/10", str);
        assertSame(str, f.toProperString());

        f = Fraction.getFraction(4, 2);
        str = f.toProperString();
        assertEquals("2", str);
        assertSame(str, f.toProperString());

        f = Fraction.getFraction(0, 2);
        str = f.toProperString();
        assertEquals("0", str);
        assertSame(str, f.toProperString());

        f = Fraction.getFraction(2, 2);
        str = f.toProperString();
        assertEquals("1", str);
        assertSame(str, f.toProperString());

        f = Fraction.getFraction(-7, 5);
        str = f.toProperString();
        assertEquals("-1 2/5", str);
        assertSame(str, f.toProperString());

        f = Fraction.getFraction(Integer.MIN_VALUE, 0, 1);
        str = f.toProperString();
        assertEquals("-2147483648", str);
        assertSame(str, f.toProperString());

        f = Fraction.getFraction(-1, 1, Integer.MAX_VALUE);
        str = f.toProperString();
        assertEquals("-1 1/2147483647", str);
        assertSame(str, f.toProperString());

        f = Fraction.getFraction(-1);
        str = f.toProperString();
        assertEquals("-1", str);
        assertSame(str, f.toProperString());
    }

    @Test
    void testToString() {
        Fraction f;

        f = Fraction.getFraction(3, 5);
        final String str = f.toString();
        assertEquals("3/5", str);
        assertSame(str, f.toString());

        f = Fraction.getFraction(7, 5);
        assertEquals("7/5", f.toString());

        f = Fraction.getFraction(4, 2);
        assertEquals("4/2", f.toString());

        f = Fraction.getFraction(0, 2);
        assertEquals("0/2", f.toString());

        f = Fraction.getFraction(2, 2);
        assertEquals("2/2", f.toString());

        f = Fraction.getFraction(Integer.MIN_VALUE, 0, 1);
        assertEquals("-2147483648/1", f.toString());

        f = Fraction.getFraction(-1, 1, Integer.MAX_VALUE);
        assertEquals("-2147483648/2147483647", f.toString());
    }
}
