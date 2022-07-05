/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.commons.lang3.math;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Test cases for the {@link Fraction} class
 */
public class FractionTest extends AbstractLangTest {

    private static final int SKIP = 500;  //53

    @Test
    public void testAbs() {
        Fraction f;

        f = Fraction.getFraction(50, 75);
        f = f.abs();
        assertEquals(50, f.getNumerator());
        assertEquals(75, f.getDenominator());

        f = Fraction.getFraction(-50, 75);
        f = f.abs();
        assertEquals(50, f.getNumerator());
        assertEquals(75, f.getDenominator());

        f = Fraction.getFraction(Integer.MAX_VALUE, 1);
        f = f.abs();
        assertEquals(Integer.MAX_VALUE, f.getNumerator());
        assertEquals(1, f.getDenominator());

        f = Fraction.getFraction(Integer.MAX_VALUE, -1);
        f = f.abs();
        assertEquals(Integer.MAX_VALUE, f.getNumerator());
        assertEquals(1, f.getDenominator());

        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(Integer.MIN_VALUE, 1).abs());
    }

    @Test
    public void testAdd() {
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

        f1 = Fraction.getFraction(-1, 13*13*2*2);
        f2 = Fraction.getFraction(-2, 13*17*2);
        final Fraction fr = f1.add(f2);
        assertEquals(13*13*17*2*2, fr.getDenominator());
        assertEquals(-17 - 2*13*2, fr.getNumerator());

        assertThrows(NullPointerException.class, () -> fr.add(null));

        // if this fraction is added naively, it will overflow.
        // check that it doesn't.
        f1 = Fraction.getFraction(1, 32768*3);
        f2 = Fraction.getFraction(1, 59049);
        f = f1.add(f2);
        assertEquals(52451, f.getNumerator());
        assertEquals(1934917632, f.getDenominator());

        f1 = Fraction.getFraction(Integer.MIN_VALUE, 3);
        f2 = Fraction.ONE_THIRD;
        f = f1.add(f2);
        assertEquals(Integer.MIN_VALUE+1, f.getNumerator());
        assertEquals(3, f.getDenominator());

        f1 = Fraction.getFraction(Integer.MAX_VALUE - 1, 1);
        f2 = Fraction.ONE;
        f = f1.add(f2);
        assertEquals(Integer.MAX_VALUE, f.getNumerator());
        assertEquals(1, f.getDenominator());

        final Fraction overflower = f;
        assertThrows(ArithmeticException.class, () -> overflower.add(Fraction.ONE)); // should overflow

        // denominator should not be a multiple of 2 or 3 to trigger overflow
        assertThrows(
                ArithmeticException.class,
                () -> Fraction.getFraction(Integer.MIN_VALUE, 5).add(Fraction.getFraction(-1, 5)));

        final Fraction maxValue = Fraction.getFraction(-Integer.MAX_VALUE, 1);
        assertThrows(ArithmeticException.class, () -> maxValue.add(maxValue));

        final Fraction negativeMaxValue = Fraction.getFraction(-Integer.MAX_VALUE, 1);
        assertThrows(ArithmeticException.class, () -> negativeMaxValue.add(negativeMaxValue));

        final Fraction f3 = Fraction.getFraction(3, 327680);
        final Fraction f4 = Fraction.getFraction(2, 59049);
        assertThrows(ArithmeticException.class, () -> f3.add(f4)); // should overflow
    }

    @Test
    public void testCompareTo() {
        final Fraction f1;
        Fraction f2;

        f1 = Fraction.getFraction(3, 5);
        assertEquals(0, f1.compareTo(f1));

        final Fraction fr = f1;
        assertThrows(NullPointerException.class, () -> fr.compareTo(null));

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

    @Test
    public void testConstants() {
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
    public void testConversions() {
        final Fraction f;

        f = Fraction.getFraction(3, 7, 8);
        assertEquals(3, f.intValue());
        assertEquals(3L, f.longValue());
        assertEquals(3.875f, f.floatValue(), 0.00001f);
        assertEquals(3.875d, f.doubleValue(), 0.00001d);
    }

    @Test
    public void testDivide() {
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

        assertThrows(NullPointerException.class, () -> fr.divideBy(null));

        final Fraction smallest = Fraction.getFraction(1, Integer.MAX_VALUE);
        assertThrows(ArithmeticException.class, () -> smallest.divideBy(smallest.invert())); // Should overflow

        final Fraction negative = Fraction.getFraction(1, -Integer.MAX_VALUE);
        assertThrows(ArithmeticException.class, () -> negative.divideBy(negative.invert())); // Should overflow
    }


    @Test
    public void testEquals() {
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

    @Test
    public void testFactory_double() {
        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(Double.NaN));
        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(Double.POSITIVE_INFINITY));
        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(Double.NEGATIVE_INFINITY));
        assertThrows(ArithmeticException.class, () -> Fraction.getFraction((double) Integer.MAX_VALUE + 1));

        // zero
        Fraction f = Fraction.getFraction(0.0d);
        assertEquals(0, f.getNumerator());
        assertEquals(1, f.getDenominator());

        // one
        f = Fraction.getFraction(1.0d);
        assertEquals(1, f.getNumerator());
        assertEquals(1, f.getDenominator());

        // one half
        f = Fraction.getFraction(0.5d);
        assertEquals(1, f.getNumerator());
        assertEquals(2, f.getDenominator());

        // negative
        f = Fraction.getFraction(-0.875d);
        assertEquals(-7, f.getNumerator());
        assertEquals(8, f.getDenominator());

        // over 1
        f = Fraction.getFraction(1.25d);
        assertEquals(5, f.getNumerator());
        assertEquals(4, f.getDenominator());

        // two thirds
        f = Fraction.getFraction(0.66666d);
        assertEquals(2, f.getNumerator());
        assertEquals(3, f.getDenominator());

        // small
        f = Fraction.getFraction(1.0d/10001d);
        assertEquals(0, f.getNumerator());
        assertEquals(1, f.getDenominator());

        // normal
        Fraction f2 = null;
        for (int i = 1; i <= 100; i++) {  // denominator
            for (int j = 1; j <= i; j++) {  // numerator
                f = Fraction.getFraction((double) j / (double) i);

                f2 = Fraction.getReducedFraction(j, i);
                assertEquals(f2.getNumerator(), f.getNumerator());
                assertEquals(f2.getDenominator(), f.getDenominator());
            }
        }
        // save time by skipping some tests!  (
        for (int i = 1001; i <= 10000; i+=SKIP) {  // denominator
            for (int j = 1; j <= i; j++) {  // numerator
                f = Fraction.getFraction((double) j / (double) i);
                f2 = Fraction.getReducedFraction(j, i);
                assertEquals(f2.getNumerator(), f.getNumerator());
                assertEquals(f2.getDenominator(), f.getDenominator());
            }
        }
    }

    @Test
    public void testFactory_int_int() {
        Fraction f;

        // zero
        f = Fraction.getFraction(0, 1);
        assertEquals(0, f.getNumerator());
        assertEquals(1, f.getDenominator());

        f = Fraction.getFraction(0, 2);
        assertEquals(0, f.getNumerator());
        assertEquals(2, f.getDenominator());

        // normal
        f = Fraction.getFraction(1, 1);
        assertEquals(1, f.getNumerator());
        assertEquals(1, f.getDenominator());

        f = Fraction.getFraction(2, 1);
        assertEquals(2, f.getNumerator());
        assertEquals(1, f.getDenominator());

        f = Fraction.getFraction(23, 345);
        assertEquals(23, f.getNumerator());
        assertEquals(345, f.getDenominator());

        // improper
        f = Fraction.getFraction(22, 7);
        assertEquals(22, f.getNumerator());
        assertEquals(7, f.getDenominator());

        // negatives
        f = Fraction.getFraction(-6, 10);
        assertEquals(-6, f.getNumerator());
        assertEquals(10, f.getDenominator());

        f = Fraction.getFraction(6, -10);
        assertEquals(-6, f.getNumerator());
        assertEquals(10, f.getDenominator());

        f = Fraction.getFraction(-6, -10);
        assertEquals(6, f.getNumerator());
        assertEquals(10, f.getDenominator());

        // zero denominator
        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(1, 0));
        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(2, 0));
        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(-3, 0));

        // very large: can't represent as unsimplified fraction, although
        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(4, Integer.MIN_VALUE));
        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(1, Integer.MIN_VALUE));
    }

    @Test
    public void testFactory_int_int_int() {
        Fraction f;

        // zero
        f = Fraction.getFraction(0, 0, 2);
        assertEquals(0, f.getNumerator());
        assertEquals(2, f.getDenominator());

        f = Fraction.getFraction(2, 0, 2);
        assertEquals(4, f.getNumerator());
        assertEquals(2, f.getDenominator());

        f = Fraction.getFraction(0, 1, 2);
        assertEquals(1, f.getNumerator());
        assertEquals(2, f.getDenominator());

        // normal
        f = Fraction.getFraction(1, 1, 2);
        assertEquals(3, f.getNumerator());
        assertEquals(2, f.getDenominator());

        // negatives
        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(1, -6, -10));
        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(1, -6, -10));
        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(1, -6, -10));

        // negative whole
        f = Fraction.getFraction(-1, 6, 10);
        assertEquals(-16, f.getNumerator());
        assertEquals(10, f.getDenominator());

        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(-1, -6, 10));
        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(-1, 6, -10));
        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(-1, -6, -10));

        // zero denominator
        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(0, 1, 0));
        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(1, 2, 0));
        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(-1, -3, 0));
        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(Integer.MAX_VALUE, 1, 2));
        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(-Integer.MAX_VALUE, 1, 2));

        // very large
        f = Fraction.getFraction(-1, 0, Integer.MAX_VALUE);
        assertEquals(-Integer.MAX_VALUE, f.getNumerator());
        assertEquals(Integer.MAX_VALUE, f.getDenominator());

        // negative denominators not allowed in this constructor.
        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(0, 4, Integer.MIN_VALUE));
        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(1, 1, Integer.MAX_VALUE));
        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(-1, 2, Integer.MAX_VALUE));
    }

    @Test
    public void testFactory_String() {
        assertThrows(NullPointerException.class, () -> Fraction.getFraction(null));
    }

    @Test
    public void testFactory_String_double() {
        Fraction f;

        f = Fraction.getFraction("0.0");
        assertEquals(0, f.getNumerator());
        assertEquals(1, f.getDenominator());

        f = Fraction.getFraction("0.2");
        assertEquals(1, f.getNumerator());
        assertEquals(5, f.getDenominator());

        f = Fraction.getFraction("0.5");
        assertEquals(1, f.getNumerator());
        assertEquals(2, f.getDenominator());

        f = Fraction.getFraction("0.66666");
        assertEquals(2, f.getNumerator());
        assertEquals(3, f.getDenominator());

        assertThrows(NumberFormatException.class, () -> Fraction.getFraction("2.3R"));
        assertThrows(NumberFormatException.class, () -> Fraction.getFraction("2147483648")); // too big
        assertThrows(NumberFormatException.class, () -> Fraction.getFraction("."));
    }

    @Test
    public void testFactory_String_improper() {
        Fraction f;

        f = Fraction.getFraction("0/1");
        assertEquals(0, f.getNumerator());
        assertEquals(1, f.getDenominator());

        f = Fraction.getFraction("1/5");
        assertEquals(1, f.getNumerator());
        assertEquals(5, f.getDenominator());

        f = Fraction.getFraction("1/2");
        assertEquals(1, f.getNumerator());
        assertEquals(2, f.getDenominator());

        f = Fraction.getFraction("2/3");
        assertEquals(2, f.getNumerator());
        assertEquals(3, f.getDenominator());

        f = Fraction.getFraction("7/3");
        assertEquals(7, f.getNumerator());
        assertEquals(3, f.getDenominator());

        f = Fraction.getFraction("2/4");
        assertEquals(2, f.getNumerator());
        assertEquals(4, f.getDenominator());

        assertThrows(NumberFormatException.class, () -> Fraction.getFraction("2/d"));
        assertThrows(NumberFormatException.class, () -> Fraction.getFraction("2e/3"));
        assertThrows(NumberFormatException.class, () -> Fraction.getFraction("2/"));
        assertThrows(NumberFormatException.class, () -> Fraction.getFraction("/"));
    }

    @Test
    public void testFactory_String_proper() {
        Fraction f;

        f = Fraction.getFraction("0 0/1");
        assertEquals(0, f.getNumerator());
        assertEquals(1, f.getDenominator());

        f = Fraction.getFraction("1 1/5");
        assertEquals(6, f.getNumerator());
        assertEquals(5, f.getDenominator());

        f = Fraction.getFraction("7 1/2");
        assertEquals(15, f.getNumerator());
        assertEquals(2, f.getDenominator());

        f = Fraction.getFraction("1 2/4");
        assertEquals(6, f.getNumerator());
        assertEquals(4, f.getDenominator());

        f = Fraction.getFraction("-7 1/2");
        assertEquals(-15, f.getNumerator());
        assertEquals(2, f.getDenominator());

        f = Fraction.getFraction("-1 2/4");
        assertEquals(-6, f.getNumerator());
        assertEquals(4, f.getDenominator());

        assertThrows(NumberFormatException.class, () -> Fraction.getFraction("2 3"));
        assertThrows(NumberFormatException.class, () -> Fraction.getFraction("a 3"));
        assertThrows(NumberFormatException.class, () -> Fraction.getFraction("2 b/4"));
        assertThrows(NumberFormatException.class, () -> Fraction.getFraction("2 "));
        assertThrows(NumberFormatException.class, () -> Fraction.getFraction(" 3"));
        assertThrows(NumberFormatException.class, () -> Fraction.getFraction(" "));
    }

    @Test
    public void testGets() {
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
    public void testHashCode() {
        final Fraction f1 = Fraction.getFraction(3, 5);
        Fraction f2 = Fraction.getFraction(3, 5);

        assertEquals(f1.hashCode(), f2.hashCode());

        f2 = Fraction.getFraction(2, 5);
        assertTrue(f1.hashCode() != f2.hashCode());

        f2 = Fraction.getFraction(6, 10);
        assertTrue(f1.hashCode() != f2.hashCode());
    }

    @Test
    public void testInvert() {
        Fraction f;

        f = Fraction.getFraction(50, 75);
        f = f.invert();
        assertEquals(75, f.getNumerator());
        assertEquals(50, f.getDenominator());

        f = Fraction.getFraction(4, 3);
        f = f.invert();
        assertEquals(3, f.getNumerator());
        assertEquals(4, f.getDenominator());

        f = Fraction.getFraction(-15, 47);
        f = f.invert();
        assertEquals(-47, f.getNumerator());
        assertEquals(15, f.getDenominator());

        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(0, 3).invert());
        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(Integer.MIN_VALUE, 1).invert());

        f = Fraction.getFraction(Integer.MAX_VALUE, 1);
        f = f.invert();
        assertEquals(1, f.getNumerator());
        assertEquals(Integer.MAX_VALUE, f.getDenominator());
    }

    @Test
    public void testMultiply() {
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
        assertThrows(NullPointerException.class, () -> fr.multiplyBy(null));

        final Fraction fr1 = Fraction.getFraction(1, Integer.MAX_VALUE);
        assertThrows(ArithmeticException.class, () -> fr1.multiplyBy(fr1));

        final Fraction fr2 = Fraction.getFraction(1, -Integer.MAX_VALUE);
        assertThrows(ArithmeticException.class, () -> fr2.multiplyBy(fr2));
    }

    @Test
    public void testNegate() {
        Fraction f;

        f = Fraction.getFraction(50, 75);
        f = f.negate();
        assertEquals(-50, f.getNumerator());
        assertEquals(75, f.getDenominator());

        f = Fraction.getFraction(-50, 75);
        f = f.negate();
        assertEquals(50, f.getNumerator());
        assertEquals(75, f.getDenominator());

        // large values
        f = Fraction.getFraction(Integer.MAX_VALUE-1, Integer.MAX_VALUE);
        f = f.negate();
        assertEquals(Integer.MIN_VALUE+2, f.getNumerator());
        assertEquals(Integer.MAX_VALUE, f.getDenominator());

        assertThrows(ArithmeticException.class, () -> Fraction.getFraction(Integer.MIN_VALUE, 1).negate());
    }

    @Test
    public void testPow() {
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

    @Test
    public void testReduce() {
        Fraction f;

        f = Fraction.getFraction(50, 75);
        Fraction result = f.reduce();
        assertEquals(2, result.getNumerator());
        assertEquals(3, result.getDenominator());

        f = Fraction.getFraction(-2, -3);
        result = f.reduce();
        assertEquals(2, result.getNumerator());
        assertEquals(3, result.getDenominator());

        f = Fraction.getFraction(2, -3);
        result = f.reduce();
        assertEquals(-2, result.getNumerator());
        assertEquals(3, result.getDenominator());

        f = Fraction.getFraction(-2, 3);
        result = f.reduce();
        assertEquals(-2, result.getNumerator());
        assertEquals(3, result.getDenominator());
        assertSame(f, result);

        f = Fraction.getFraction(2, 3);
        result = f.reduce();
        assertEquals(2, result.getNumerator());
        assertEquals(3, result.getDenominator());
        assertSame(f, result);

        f = Fraction.getFraction(0, 1);
        result = f.reduce();
        assertEquals(0, result.getNumerator());
        assertEquals(1, result.getDenominator());
        assertSame(f, result);

        f = Fraction.getFraction(0, 100);
        result = f.reduce();
        assertEquals(0, result.getNumerator());
        assertEquals(1, result.getDenominator());
        assertSame(result, Fraction.ZERO);

        f = Fraction.getFraction(Integer.MIN_VALUE, 2);
        result = f.reduce();
        assertEquals(Integer.MIN_VALUE / 2, result.getNumerator());
        assertEquals(1, result.getDenominator());
    }

    @Test
    public void testReducedFactory_int_int() {
        Fraction f;

        // zero
        f = Fraction.getReducedFraction(0, 1);
        assertEquals(0, f.getNumerator());
        assertEquals(1, f.getDenominator());

        // normal
        f = Fraction.getReducedFraction(1, 1);
        assertEquals(1, f.getNumerator());
        assertEquals(1, f.getDenominator());

        f = Fraction.getReducedFraction(2, 1);
        assertEquals(2, f.getNumerator());
        assertEquals(1, f.getDenominator());

        // improper
        f = Fraction.getReducedFraction(22, 7);
        assertEquals(22, f.getNumerator());
        assertEquals(7, f.getDenominator());

        // negatives
        f = Fraction.getReducedFraction(-6, 10);
        assertEquals(-3, f.getNumerator());
        assertEquals(5, f.getDenominator());

        f = Fraction.getReducedFraction(6, -10);
        assertEquals(-3, f.getNumerator());
        assertEquals(5, f.getDenominator());

        f = Fraction.getReducedFraction(-6, -10);
        assertEquals(3, f.getNumerator());
        assertEquals(5, f.getDenominator());

        // zero denominator
        assertThrows(ArithmeticException.class, () -> Fraction.getReducedFraction(1, 0));
        assertThrows(ArithmeticException.class, () -> Fraction.getReducedFraction(2, 0));
        assertThrows(ArithmeticException.class, () -> Fraction.getReducedFraction(-3, 0));

        // reduced
        f = Fraction.getReducedFraction(0, 2);
        assertEquals(0, f.getNumerator());
        assertEquals(1, f.getDenominator());

        f = Fraction.getReducedFraction(2, 2);
        assertEquals(1, f.getNumerator());
        assertEquals(1, f.getDenominator());

        f = Fraction.getReducedFraction(2, 4);
        assertEquals(1, f.getNumerator());
        assertEquals(2, f.getDenominator());

        f = Fraction.getReducedFraction(15, 10);
        assertEquals(3, f.getNumerator());
        assertEquals(2, f.getDenominator());

        f = Fraction.getReducedFraction(121, 22);
        assertEquals(11, f.getNumerator());
        assertEquals(2, f.getDenominator());

        // Extreme values
        // OK, can reduce before negating
        f = Fraction.getReducedFraction(-2, Integer.MIN_VALUE);
        assertEquals(1, f.getNumerator());
        assertEquals(-(Integer.MIN_VALUE / 2), f.getDenominator());

        // Can't reduce, negation will throw
        assertThrows(ArithmeticException.class, () -> Fraction.getReducedFraction(-7, Integer.MIN_VALUE));

        // LANG-662
        f = Fraction.getReducedFraction(Integer.MIN_VALUE, 2);
        assertEquals(Integer.MIN_VALUE / 2, f.getNumerator());
        assertEquals(1, f.getDenominator());
    }

    @Test
    public void testSubtract() {
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
        assertThrows(NullPointerException.class, () -> fr.subtract(null));

        // if this fraction is subtracted naively, it will overflow.
        // check that it doesn't.
        f1 = Fraction.getFraction(1, 32768*3);
        f2 = Fraction.getFraction(1, 59049);
        f = f1.subtract(f2);
        assertEquals(-13085, f.getNumerator());
        assertEquals(1934917632, f.getDenominator());

        f1 = Fraction.getFraction(Integer.MIN_VALUE, 3);
        f2 = Fraction.ONE_THIRD.negate();
        f = f1.subtract(f2);
        assertEquals(Integer.MIN_VALUE+1, f.getNumerator());
        assertEquals(3, f.getDenominator());

        f1 = Fraction.getFraction(Integer.MAX_VALUE, 1);
        f2 = Fraction.ONE;
        f = f1.subtract(f2);
        assertEquals(Integer.MAX_VALUE-1, f.getNumerator());
        assertEquals(1, f.getDenominator());

        // Should overflow
        assertThrows(
                ArithmeticException.class,
                () -> Fraction.getFraction(1, Integer.MAX_VALUE).subtract(Fraction.getFraction(1, Integer.MAX_VALUE - 1)));
            f = f1.subtract(f2);

        // denominator should not be a multiple of 2 or 3 to trigger overflow
        assertThrows(
                ArithmeticException.class,
                () -> Fraction.getFraction(Integer.MIN_VALUE, 5).subtract(Fraction.getFraction(1, 5)));

        assertThrows(
                ArithmeticException.class, () -> Fraction.getFraction(Integer.MIN_VALUE, 1).subtract(Fraction.ONE));

        assertThrows(
                ArithmeticException.class,
                () -> Fraction.getFraction(Integer.MAX_VALUE, 1).subtract(Fraction.ONE.negate()));

        // Should overflow
        assertThrows(
                ArithmeticException.class,
                () -> Fraction.getFraction(3, 327680).subtract(Fraction.getFraction(2, 59049)));
    }

    @Test
    public void testToProperString() {
        Fraction f;

        f = Fraction.getFraction(3, 5);
        final String str = f.toProperString();
        assertEquals("3/5", str);
        assertSame(str, f.toProperString());

        f = Fraction.getFraction(7, 5);
        assertEquals("1 2/5", f.toProperString());

        f = Fraction.getFraction(14, 10);
        assertEquals("1 4/10", f.toProperString());

        f = Fraction.getFraction(4, 2);
        assertEquals("2", f.toProperString());

        f = Fraction.getFraction(0, 2);
        assertEquals("0", f.toProperString());

        f = Fraction.getFraction(2, 2);
        assertEquals("1", f.toProperString());

        f = Fraction.getFraction(-7, 5);
        assertEquals("-1 2/5", f.toProperString());

        f = Fraction.getFraction(Integer.MIN_VALUE, 0, 1);
        assertEquals("-2147483648", f.toProperString());

        f = Fraction.getFraction(-1, 1, Integer.MAX_VALUE);
        assertEquals("-1 1/2147483647", f.toProperString());

        assertEquals("-1", Fraction.getFraction(-1).toProperString());
    }

    @Test
    public void testToString() {
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
