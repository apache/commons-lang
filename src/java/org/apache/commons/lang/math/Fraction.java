/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002-2003 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution, if
 *    any, must include the following acknowlegement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowlegement may appear in the software itself,
 *    if and wherever such third-party acknowlegements normally appear.
 *
 * 4. The names "The Jakarta Project", "Commons", and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 */
package org.apache.commons.lang.math;

import java.io.Serializable;

/**
 * <p><code>Fraction</code> is a <code>Number</code> implementation that
 * stores fractions accurately.</p>
 *
 * <p>This class is immutable, and interoperable with most methods that accept
 * a <code>Number</code>.</p>
 *
 * @author Travis Reeder
 * @author Stephen Colebourne
 * @author Tim O'Brien
 * @since 2.0
 * @version $Id: Fraction.java,v 1.7 2003/08/04 01:20:47 scolebourne Exp $
 */
public final class Fraction extends Number implements Serializable, Comparable {

    /** Serialization lock, Lang version 2.0 */
    private static final long serialVersionUID = 65382027393090L;

    public static final Fraction ZERO = new Fraction(0, 1);
    public static final Fraction ONE = new Fraction(1, 1);

    public static final Fraction ONE_HALF = new Fraction(1, 2);

    public static final Fraction ONE_THIRD = new Fraction(1, 3);
    public static final Fraction TWO_THIRDS = new Fraction(2, 3);

    public static final Fraction ONE_QUARTER = new Fraction(1, 4);
    public static final Fraction TWO_QUARTERS = new Fraction(2, 4);
    public static final Fraction THREE_QUARTERS = new Fraction(3, 4);

    public static final Fraction ONE_FIFTH = new Fraction(1, 5);
    public static final Fraction TWO_FIFTHS = new Fraction(2, 5);
    public static final Fraction THREE_FIFTHS = new Fraction(3, 5);
    public static final Fraction FOUR_FIFTHS = new Fraction(4, 5);


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
    private transient int hashCode = 0;
    /**
     * Cached output toString (class is immutable).
     */
    private transient String toString = null;
    /**
     * Cached output toProperString (class is immutable).
     */
    private transient String toProperString = null;

    /**
     * <p>Constructs a <code>Fraction</code> instance with the 2 parts
     * of a fraction Y/Z.</p>
     *
     * @param numerator  the numerator, for example the three in 'three sevenths'
     * @param denominator  the denominator, for example the seven in 'three sevenths'
     */
    private Fraction(int numerator, int denominator) {
        super();
        this.numerator = numerator;
        this.denominator = denominator;
    }

    /**
     * <p>Creates a <code>Fraction</code> instance with the 2 parts
     * of a fraction Y/Z.</p>
     *
     * <p>Any negative signs are resolved to be on the numerator.</p>
     *
     * @param numerator  the numerator, for example the three in 'three sevenths'
     * @param denominator  the denominator, for example the seven in 'three sevenths'
     * @return a new fraction instance
     * @throws ArithmeticException if the denomiator is <code>zero</code>
     */
    public static Fraction getFraction(int numerator, int denominator) {
        if (denominator == 0) {
            throw new ArithmeticException("The denominator must not be zero");
        }
        if (denominator < 0) {
            numerator = -numerator;
            denominator = -denominator;
        }
        return new Fraction(numerator, denominator);
    }

    /**
     * <p>Creates a <code>Fraction</code> instance with the 3 parts
     * of a fraction X Y/Z.</p>
     *
     * <p>The negative sign must be passed in on the whole number part.</p>
     *
     * @param whole  the whole number, for example the one in 'one and three sevenths'
     * @param numerator  the numerator, for example the three in 'one and three sevenths'
     * @param denominator  the denominator, for example the seven in 'one and three sevenths'
     * @return a new fraction instance
     * @throws ArithmeticException if the denomiator is <code>zero</code>
     * @throws ArithmeticException if the denomiator is negative
     * @throws ArithmeticException if the numerator is negative
     */
    public static Fraction getFraction(int whole, int numerator, int denominator) {
        if (denominator == 0) {
            throw new ArithmeticException("The denominator must not be zero");
        }
        if (denominator < 0) {
            throw new ArithmeticException("The denominator must not be negative");
        }
        if (numerator < 0) {
            throw new ArithmeticException("The numerator must not be negative");
        }
        if (whole < 0) {
            numerator = whole * denominator - numerator;
        } else {
            numerator = whole * denominator + numerator;
        }
        return new Fraction(numerator, denominator);
    }

    /**
     * <p>Creates a <code>Fraction</code> instance with the 2 parts
     * of a fraction Y/Z.</p>
     *
     * <p>Any negative signs are resolved to be on the numerator.</p>
     *
     * @param numerator  the numerator, for example the three in 'three sevenths'
     * @param denominator  the denominator, for example the seven in 'three sevenths'
     * @return a new fraction instance, with the numerator and denominator reduced
     * @throws ArithmeticException if the denomiator is <code>zero</code>
     */
    public static Fraction getReducedFraction(int numerator, int denominator) {
        if (denominator == 0) {
            throw new ArithmeticException("The denominator must not be zero");
        }
        if (denominator < 0) {
            numerator = -numerator;
            denominator = -denominator;
        }
        int gcd = greatestCommonDenominator(Math.abs(numerator), denominator);
        if (gcd == 0) {
            return new Fraction(numerator, denominator);
        }
        return new Fraction(numerator / gcd, denominator / gcd);
    }

    /**
     * <p>Creates a <code>Fraction</code> instance from a <code>double</code> value.</p>
     *
     * <p>This method uses the continued fraction algorithm.</p>
     *
     * @param value  the double value to convert
     * @return a new fraction instance that is close to the value
     * @throws ArithmeticException if the value is infinite or <code>NaN</code>
     * @throws ArithmeticException if the calculated denomiator is <code>zero</code>
     */
    public static Fraction getFraction(double value) {
        if (Double.isInfinite(value) || Double.isNaN(value)) {
            throw new ArithmeticException("The value must not be infinite or NaN");
        }
        int sign = (value < 0 ? -1 : 1);
        value = Math.abs(value);
        int wholeNumber = (int) value;
        value -= wholeNumber;

        // http://archives.math.utk.edu/articles/atuyl/confrac/
        int numer0 = 0;  // the pre-previous
        int denom0 = 1;  // the pre-previous
        int numer1 = 1;  // the previous
        int denom1 = 0;  // the previous
        int numer2 = 0;  // the current, setup in calculation
        int denom2 = 0;  // the current, setup in calculation
        int a1 = (int) value;
        int a2 = 0;
        double x1 = 1;
        double x2 = 0;
        double y1 = value - a1;
        double y2 = 0;
        double delta1, delta2 = Double.MAX_VALUE;
        double fraction;
        int i = 1;
//        System.out.println("---");
        do {
            delta1 = delta2;
            a2 = (int) (x1 / y1);
            x2 = y1;
            y2 = x1 - a2 * y1;
            numer2 = a1 * numer1 + numer0;
            denom2 = a1 * denom1 + denom0;
            fraction = (double) numer2 / (double) denom2;
            delta2 = Math.abs(value - fraction);
//            System.out.println(numer2 + " " + denom2 + " " + fraction + " " + delta2 + " " + y1);
            a1 = a2;
            x1 = x2;
            y1 = y2;
            numer0 = numer1;
            denom0 = denom1;
            numer1 = numer2;
            denom1 = denom2;
            i++;
//            System.out.println(">>" + delta1 +" "+ delta2+" "+(delta1 > delta2)+" "+i+" "+denom2);
        } while ((delta1 > delta2) && (denom2 <= 10000) && (denom2 > 0) && (i < 25));
        if (i == 25) {
            throw new ArithmeticException("Unable to convert double to fraction");
        }
        return getReducedFraction((numer0 + wholeNumber * denom0) * sign, denom0);
    }

    /**
     * <p>Creates a Fraction from a <code>String</code>.</p>
     *
     * <p>The formats accepted are:</p>
     *
     * <p>
     * <ol>
     *  <li><code>double</code> String containing a dot</li>
     *  <li>'X Y/Z'</li>
     *  <li>'Y/Z'</li>
     * </ol>
     * and a .</p>
     *
     * @param str  the string to parse, must not be <code>null</code>
     * @return the new <code>Fraction</code> instance
     * @throws IllegalArgumentException if the string is <code>null</code>
     * @throws NumberFormatException if the number format is invalid
     */
    public static Fraction getFraction(String str) {
        if (str == null) {
            throw new IllegalArgumentException("The string must not be null");
        }
        // parse double format
        int pos = str.indexOf('.');
        if (pos >= 0) {
            return getFraction(Double.parseDouble(str));
        }

        // parse X Y/Z format
        pos = str.indexOf(' ');
        if (pos > 0) {
            int whole = Integer.parseInt(str.substring(0, pos));
            str = str.substring(pos + 1);
            pos = str.indexOf('/');
            if (pos < 0) {
                throw new NumberFormatException("The fraction could not be parsed as the format X Y/Z");
            } else {
                int denom = Integer.parseInt(str.substring(pos + 1));
                return getFraction(
                    Integer.parseInt(str.substring(0, pos)) + whole * denom,
                    denom
                );
            }
        }

        // parse Y/Z format
        pos = str.indexOf('/');
        if (pos < 0) {
            // simple whole number
            return getFraction(Integer.parseInt(str), 1);
        } else {
            return getFraction(
                Integer.parseInt(str.substring(0, pos)),
                Integer.parseInt(str.substring(pos + 1))
            );
        }
    }

    // Accessors
    //-------------------------------------------------------------------

    /**
     * <p>Gets the numerator part of the fraction.</p>
     *
     * <p>This method may return a value greater than the denominator, an
     * improper fraction, such as the seven in 7/4.</p>
     *
     * @return the numerator fraction part
     */
    public int getNumerator() {
        return numerator;
    }

    /**
     * <p>Gets the denominator part of the fraction.</p>
     *
     * @return the denominator fraction part
     */
    public int getDenominator() {
        return denominator;
    }

    /**
     * <p>Gets the proper numerator, always positive.</p>
     *
     * <p>An improper fraction 7/4 can be resolved into a proper one, 1 3/4.
     * This method returns the 3 from the proper fraction.</p>
     *
     * <p>If the fraction is negative such as -7/4, it can be resolved into
     * -1 3/4, so this method returns the positive proper numerator, 3.</p>
     *
     * @return the numerator fraction part of a proper fraction, always positive
     */
    public int getProperNumerator() {
        return Math.abs(numerator % denominator);
    }

    /**
     * <p>Gets the proper whole part of the fraction.</p>
     *
     * <p>An improper fraction 7/4 can be resolved into a proper one, 1 3/4.
     * This method returns the 1 from the proper fraction.</p>
     *
     * <p>If the fraction is negative such as -7/4, it can be resolved into
     * -1 3/4, so this method returns the positive whole part -1.</p>
     *
     * @return the whole fraction part of a proper fraction, that includes the sign
     */
    public int getProperWhole() {
        return numerator / denominator;
    }

    // Number methods
    //-------------------------------------------------------------------

    /**
     * <p>Gets the fraction as an <code>int</code>. This returns the whole number
     * part of the fraction.</p>
     *
     * @return the whole number fraction part
     */
    public int intValue() {
        return numerator / denominator;
    }

    /**
     * <p>Gets the fraction as a <code>long</code>. This returns the whole number
     * part of the fraction.</p>
     *
     * @return the whole number fraction part
     */
    public long longValue() {
        return (long) numerator / denominator;
    }

    /**
     * <p>Gets the fraction as a <code>float</code>. This calculates the fraction
     * as the numerator divided by denominator.</p>
     *
     * @return the fraction as a <code>float</code>
     */
    public float floatValue() {
        return ((float) numerator) / ((float) denominator);
    }

    /**
     * <p>Gets the fraction as a <code>double</code>. This calculates the fraction
     * as the numerator divided by denominator.</p>
     *
     * @return the fraction as a <code>double</code>
     */
    public double doubleValue() {
        return ((double) numerator) / ((double) denominator);
    }

    // Calculations
    //-------------------------------------------------------------------

    /**
     * <p>Reduce the fraction to the smallest values for the numerator and
     * denominator, returning the result..</p>
     *
     * @return a new reduce fraction instance, or this if no simplification possible
     */
    public Fraction reduce() {
        int gcd = greatestCommonDenominator(Math.abs(numerator), denominator);
        if (gcd == 0) {
            return this;
        }
        return Fraction.getFraction(numerator / gcd, denominator / gcd);
    }

    /**
     * <p>Gets a fraction that is the invert (1/fraction) of this one.</p>
     *
     * <p>The returned fraction is not reduced.</p>
     *
     * @return a new fraction instance with the numerator and denominator inverted
     * @throws ArithmeticException if the numerator is <code>zero</code>
     */
    public Fraction invert() {
        if (numerator == 0) {
            throw new ArithmeticException("Unable to invert a fraction with a zero numerator");
        }
        return getFraction(denominator, numerator);
    }

    /**
     * <p>Gets a fraction that is the negative (-fraction) of this one.</p>
     *
     * <p>The returned fraction is not reduced.</p>
     *
     * @return a new fraction instance with the opposite signed numerator
     */
    public Fraction negate() {
        return getFraction(-numerator, denominator);
    }

    /**
     * <p>Gets a fraction that is the positive equivalent of this one.</p>
     * <p>More precisely: <pre>(fraction >= 0 ? this : -fraction)</pre></p>
     *
     * <p>The returned fraction is not reduced.</p>
     *
     * @return <code>this</code> if it is positive, or a new positive fraction
     *  instance with the opposite signed numerator
     */
    public Fraction abs() {
        if (numerator >= 0) {
            return this;
        }
        return getFraction(-numerator, denominator);
    }

    /**
     * <p>Gets a fraction that is raised to the passed in power.</p>
     *
     * <p>The returned fraction is not reduced.</p>
     *
     * @param power  the power to raise the fraction to
     * @return <code>this</code> if the power is one, <code>ONE</code> if the power
     * is zero or a new fraction instance raised to the appropriate power
     */
    public Fraction pow(int power) {
        if (power == 1) {
            return this;
        } else if (power == 0) {
            return ONE;
        } else if (power < 0) {
            return getFraction((int) Math.pow(denominator, -power), (int) Math.pow(numerator, -power));
        }
        return getFraction((int) Math.pow(numerator, power), (int) Math.pow(denominator, power));
    }

    /**
     * <p>Gets the greatest common denominator of two numbers.</p>
     *
     * @param number1  a positive number
     * @param number2  a positive number
     * @return the greatest common denominator
     */
    private static int greatestCommonDenominator(int number1, int number2) {
        int remainder = number1 % number2;
        while (remainder != 0) {
            number1 = number2;
            number2 = remainder;
            remainder = number1 % number2;
        }
        return number2;
    }

    // Arithmetic
    //-------------------------------------------------------------------

    /**
     * <p>Adds the value of this fraction to another, returning the result.</p>
     *
     * <p>The implementation spots common cases of zero numerators and equal
     * denominators. Otherwise, it uses <code>(a/b) + (c/d) = (a*d + b*c) / (b*d)</code>
     * and then reduces the result.</p>
     *
     * @param fraction  the fraction to add, must not be <code>null</code>
     * @return a <code>Fraction</code> instance with the resulting values
     * @throws IllegalArgumentException if the fraction is <code>null</code>
     */
    public Fraction add(Fraction fraction) {
        if (fraction == null) {
            throw new IllegalArgumentException("The fraction must not be null");
        }
        if (numerator == 0) {
            return fraction;
        }
        if (fraction.numerator == 0) {
            return this;
        }
        if (denominator == fraction.denominator) {
            return getReducedFraction(numerator + fraction.numerator, denominator);
        }
        return getReducedFraction(
            numerator * fraction.denominator + denominator * fraction.numerator,
            denominator * fraction.denominator
        );
    }

    /**
     * <p>Subtracts the value of another fraction from the value of this one,
     * returning the result.</p>
     *
     * <p>The implementation spots common cases of zero numerators and equal
     * denominators. Otherwise, it uses <code>(a/b) - (c/d) = (a*d - b*c) / (b*d)</code>
     * and then reduces the result.</p>
     *
     * @param fraction  the fraction to subtract, must not be <code>null</code>
     * @return a <code>Fraction</code> instance with the resulting values
     * @throws IllegalArgumentException if the fraction is <code>null</code>
     */
    public Fraction subtract(Fraction fraction) {
        if (fraction == null) {
            throw new IllegalArgumentException("The fraction must not be null");
        }
        if (numerator == 0) {
            return fraction.negate();
        }
        if (fraction.numerator == 0) {
            return this;
        }
        if (denominator == fraction.denominator) {
            return getReducedFraction(numerator - fraction.numerator, denominator);
        }
        return getReducedFraction(
            numerator * fraction.denominator - denominator * fraction.numerator,
            denominator * fraction.denominator
        );
    }

    /**
     * <p>Multiplies the value of this fraction by another, returning the result.</p>
     *
     * <p>The implementation uses <code>(a/b)*(c/d) = (a*c)/(b*d)</code>
     * and then reduces the result.</p>
     *
     * @param fraction  the fraction to multipy by, must not be <code>null</code>
     * @return a <code>Fraction</code> instance with the resulting values
     * @throws IllegalArgumentException if the fraction is <code>null</code>
     */
    public Fraction multiplyBy(Fraction fraction) {
        if (fraction == null) {
            throw new IllegalArgumentException("The fraction must not be null");
        }
        if (numerator == 0 || fraction.numerator == 0) {
            return ZERO;
        }
        return getReducedFraction(
            numerator * fraction.numerator,
            denominator * fraction.denominator
        );
    }

    /**
     * <p>Divide the value of this fraction by another, returning the result.</p>
     *
     * <p>The implementation uses <code>(a/b)/(c/d) = a/b * d/c = (a*d)/(b*c)</code>
     * and then reduces the result.</p>
     *
     * @param fraction  the fraction to divide by, must not be <code>null</code>
     * @return a <code>Fraction</code> instance with the resulting values
     * @throws IllegalArgumentException if the fraction is <code>null</code>
     * @throws ArithmeticException if the fraction to divide by is zero
     */
    public Fraction divideBy(Fraction fraction) {
        if (fraction == null) {
            throw new IllegalArgumentException("The fraction must not be null");
        }
        if (fraction.numerator == 0) {
            throw new ArithmeticException("The fraction to divide by must not be zero");
        }
        if (numerator == 0) {
            return ZERO;
        }
        return getReducedFraction(
            numerator * fraction.denominator,
            denominator * fraction.numerator
        );
    }

    // Basics
    //-------------------------------------------------------------------

    /**
     * <p>Compares this fraction to another object to test if they are equal.</p>.
     *
     * <p>To be equal, both values must be equal. Thus 2/4 is not equal to 1/2.</p>
     *
     * @param obj the reference object with which to compare
     * @return <code>true</code> if this object is equal
     */
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        if (obj instanceof Fraction == false) {
            return false;
        }
        Fraction other = (Fraction) obj;
        return (numerator == other.numerator &&
                denominator == other.denominator);
    }

    /**
     * <p>Gets a hashCode for the fraction.</p>
     *
     * @return a hash code value for this object
     */
    public int hashCode() {
        if (hashCode == 0) {
            hashCode = 17;
            hashCode = 37 * hashCode + numerator;
            hashCode = 37 * hashCode + denominator;
        }
        return hashCode;
    }

    /**
     * <p>Compares this object to another based on size.</p>
     *
     * @param object  the object to compare to
     * @return -ve if this is less, 0 if equal, +ve if greater
     * @throws ClassCastException if the object is not a <code>Fraction</code>
     * @throws NullPointerException if the object is <code>null</code>
     */
    public int compareTo(Object object) {
        Fraction other = (Fraction) object;
        if (numerator == other.numerator && denominator == other.denominator) {
            return 0;
        }

        // otherwise see which is less
        long first = (long) numerator * (long) other.denominator;
        long second = (long) other.numerator * (long) denominator;
        if (first == second) {
            return 0;
        } else if (first < second) {
            return -1;
        } else {
            return 1;
        }
    }

    /**
     * <p>Gets the fraction as a <code>String</code>.</p>
     *
     * <p>The format used is '<i>numerator</i>/<i>denominator</i>' always.
     *
     * @return a <code>String</code> form of the fraction
     */
    public String toString() {
        if (toString == null) {
            toString = new StringBuffer(32)
                .append(numerator)
                .append('/')
                .append(denominator).toString();
        }
        return toString;
    }

    /**
     * <p>Gets the fraction as a proper <code>String</code> in the format X Y/Z.</p>
     *
     * <p>The format used in '<i>wholeNumber</i> <i>numerator</i>/<i>denominator</i>'.
     * If the whole number is zero it will be ommitted. If the numerator is zero,
     * only the whole number is returned.</p>
     *
     * @return a <code>String</code> form of the fraction
     */
    public String toProperString() {
        if (toProperString == null) {
            if (numerator == 0) {
                toProperString = "0";
            } else if (numerator == denominator) {
                toProperString = "1";
            } else if (Math.abs(numerator) > denominator) {
                int properNumerator = getProperNumerator();
                if (properNumerator == 0) {
                    toProperString = Integer.toString(getProperWhole());
                } else {
                    toProperString = new StringBuffer(32)
                        .append(getProperWhole()).append(' ')
                        .append(properNumerator).append('/')
                        .append(denominator).toString();
                }
            } else {
                toProperString = new StringBuffer(32)
                    .append(numerator).append('/')
                    .append(denominator).toString();
            }
        }
        return toProperString;
    }

}
