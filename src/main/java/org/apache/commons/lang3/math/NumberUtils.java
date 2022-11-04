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
package org.apache.commons.lang3.math;

import java.lang.reflect.Array;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.util.Objects;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.Validate;

/**
 * Provides extra functionality for Java Number classes.
 *
 * @since 2.0
 */
public class NumberUtils {

    /** Reusable Long constant for zero. */
    public static final Long LONG_ZERO = Long.valueOf(0L);
    /** Reusable Long constant for one. */
    public static final Long LONG_ONE = Long.valueOf(1L);
    /** Reusable Long constant for minus one. */
    public static final Long LONG_MINUS_ONE = Long.valueOf(-1L);
    /** Reusable Integer constant for zero. */
    public static final Integer INTEGER_ZERO = Integer.valueOf(0);
    /** Reusable Integer constant for one. */
    public static final Integer INTEGER_ONE = Integer.valueOf(1);
    /** Reusable Integer constant for two */
    public static final Integer INTEGER_TWO = Integer.valueOf(2);
    /** Reusable Integer constant for minus one. */
    public static final Integer INTEGER_MINUS_ONE = Integer.valueOf(-1);
    /** Reusable Short constant for zero. */
    public static final Short SHORT_ZERO = Short.valueOf((short) 0);
    /** Reusable Short constant for one. */
    public static final Short SHORT_ONE = Short.valueOf((short) 1);
    /** Reusable Short constant for minus one. */
    public static final Short SHORT_MINUS_ONE = Short.valueOf((short) -1);
    /** Reusable Byte constant for zero. */
    public static final Byte BYTE_ZERO = Byte.valueOf((byte) 0);
    /** Reusable Byte constant for one. */
    public static final Byte BYTE_ONE = Byte.valueOf((byte) 1);
    /** Reusable Byte constant for minus one. */
    public static final Byte BYTE_MINUS_ONE = Byte.valueOf((byte) -1);
    /** Reusable Double constant for zero. */
    public static final Double DOUBLE_ZERO = Double.valueOf(0.0d);
    /** Reusable Double constant for one. */
    public static final Double DOUBLE_ONE = Double.valueOf(1.0d);
    /** Reusable Double constant for minus one. */
    public static final Double DOUBLE_MINUS_ONE = Double.valueOf(-1.0d);
    /** Reusable Float constant for zero. */
    public static final Float FLOAT_ZERO = Float.valueOf(0.0f);
    /** Reusable Float constant for one. */
    public static final Float FLOAT_ONE = Float.valueOf(1.0f);
    /** Reusable Float constant for minus one. */
    public static final Float FLOAT_MINUS_ONE = Float.valueOf(-1.0f);

    /**
     * {@link Integer#MAX_VALUE} as a {@link Long}.
     *
     * @since 3.12.0
     */
    public static final Long LONG_INT_MAX_VALUE = Long.valueOf(Integer.MAX_VALUE);

    /**
     * {@link Integer#MIN_VALUE} as a {@link Long}.
     *
     * @since 3.12.0
     */
    public static final Long LONG_INT_MIN_VALUE = Long.valueOf(Integer.MIN_VALUE);


    /**
     * {@link NumberUtils} instances should NOT be constructed in standard programming.
     * Instead, the class should be used as {@code NumberUtils.toInt("6");}.
     *
     * <p>This constructor is public to permit tools that require a JavaBean instance
     * to operate.</p>
     */
    public NumberUtils() {
    }

    /**
     * Convert a {@link String} to an {@code int}, returning
     * {@code zero} if the conversion fails.
     *
     * <p>If the string is {@code null}, {@code zero} is returned.</p>
     *
     * <pre>
     *   NumberUtils.toInt(null) = 0
     *   NumberUtils.toInt("")   = 0
     *   NumberUtils.toInt("1")  = 1
     * </pre>
     *
     * @param str  the string to convert, may be null
     * @return the int represented by the string, or {@code zero} if
     *  conversion fails
     * @since 2.1
     */
    public static int toInt(final String str) {
        return toInt(str, 0);
    }

    /**
     * Convert a {@link String} to an {@code int}, returning a
     * default value if the conversion fails.
     *
     * <p>If the string is {@code null}, the default value is returned.</p>
     *
     * <pre>
     *   NumberUtils.toInt(null, 1) = 1
     *   NumberUtils.toInt("", 1)   = 1
     *   NumberUtils.toInt("1", 0)  = 1
     * </pre>
     *
     * @param str  the string to convert, may be null
     * @param defaultValue  the default value
     * @return the int represented by the string, or the default if conversion fails
     * @since 2.1
     */
    public static int toInt(final String str, final int defaultValue) {
        if (str == null) {
            return defaultValue;
        }
        try {
            return Integer.parseInt(str);
        } catch (final NumberFormatException nfe) {
            return defaultValue;
        }
    }

    /**
     * Convert a {@link String} to a {@code long}, returning
     * {@code zero} if the conversion fails.
     *
     * <p>If the string is {@code null}, {@code zero} is returned.</p>
     *
     * <pre>
     *   NumberUtils.toLong(null) = 0L
     *   NumberUtils.toLong("")   = 0L
     *   NumberUtils.toLong("1")  = 1L
     * </pre>
     *
     * @param str  the string to convert, may be null
     * @return the long represented by the string, or {@code 0} if
     *  conversion fails
     * @since 2.1
     */
    public static long toLong(final String str) {
        return toLong(str, 0L);
    }

    /**
     * Convert a {@link String} to a {@code long}, returning a
     * default value if the conversion fails.
     *
     * <p>If the string is {@code null}, the default value is returned.</p>
     *
     * <pre>
     *   NumberUtils.toLong(null, 1L) = 1L
     *   NumberUtils.toLong("", 1L)   = 1L
     *   NumberUtils.toLong("1", 0L)  = 1L
     * </pre>
     *
     * @param str  the string to convert, may be null
     * @param defaultValue  the default value
     * @return the long represented by the string, or the default if conversion fails
     * @since 2.1
     */
    public static long toLong(final String str, final long defaultValue) {
        if (str == null) {
            return defaultValue;
        }
        try {
            return Long.parseLong(str);
        } catch (final NumberFormatException nfe) {
            return defaultValue;
        }
    }

    /**
     * Convert a {@link String} to a {@code float}, returning
     * {@code 0.0f} if the conversion fails.
     *
     * <p>If the string {@code str} is {@code null},
     * {@code 0.0f} is returned.</p>
     *
     * <pre>
     *   NumberUtils.toFloat(null)   = 0.0f
     *   NumberUtils.toFloat("")     = 0.0f
     *   NumberUtils.toFloat("1.5")  = 1.5f
     * </pre>
     *
     * @param str the string to convert, may be {@code null}
     * @return the float represented by the string, or {@code 0.0f}
     *  if conversion fails
     * @since 2.1
     */
    public static float toFloat(final String str) {
        return toFloat(str, 0.0f);
    }

    /**
     * Convert a {@link String} to a {@code float}, returning a
     * default value if the conversion fails.
     *
     * <p>If the string {@code str} is {@code null}, the default
     * value is returned.</p>
     *
     * <pre>
     *   NumberUtils.toFloat(null, 1.1f)   = 1.0f
     *   NumberUtils.toFloat("", 1.1f)     = 1.1f
     *   NumberUtils.toFloat("1.5", 0.0f)  = 1.5f
     * </pre>
     *
     * @param str the string to convert, may be {@code null}
     * @param defaultValue the default value
     * @return the float represented by the string, or defaultValue
     *  if conversion fails
     * @since 2.1
     */
    public static float toFloat(final String str, final float defaultValue) {
      if (str == null) {
          return defaultValue;
      }
      try {
          return Float.parseFloat(str);
      } catch (final NumberFormatException nfe) {
          return defaultValue;
      }
    }

    /**
     * Convert a {@link String} to a {@code double}, returning
     * {@code 0.0d} if the conversion fails.
     *
     * <p>If the string {@code str} is {@code null},
     * {@code 0.0d} is returned.</p>
     *
     * <pre>
     *   NumberUtils.toDouble(null)   = 0.0d
     *   NumberUtils.toDouble("")     = 0.0d
     *   NumberUtils.toDouble("1.5")  = 1.5d
     * </pre>
     *
     * @param str the string to convert, may be {@code null}
     * @return the double represented by the string, or {@code 0.0d}
     *  if conversion fails
     * @since 2.1
     */
    public static double toDouble(final String str) {
        return toDouble(str, 0.0d);
    }

    /**
     * Convert a {@link String} to a {@code double}, returning a
     * default value if the conversion fails.
     *
     * <p>If the string {@code str} is {@code null}, the default
     * value is returned.</p>
     *
     * <pre>
     *   NumberUtils.toDouble(null, 1.1d)   = 1.1d
     *   NumberUtils.toDouble("", 1.1d)     = 1.1d
     *   NumberUtils.toDouble("1.5", 0.0d)  = 1.5d
     * </pre>
     *
     * @param str the string to convert, may be {@code null}
     * @param defaultValue the default value
     * @return the double represented by the string, or defaultValue
     *  if conversion fails
     * @since 2.1
     */
    public static double toDouble(final String str, final double defaultValue) {
      if (str == null) {
          return defaultValue;
      }
      try {
          return Double.parseDouble(str);
      } catch (final NumberFormatException nfe) {
          return defaultValue;
      }
    }

    /**
     * Convert a {@link BigDecimal} to a {@code double}.
     *
     * <p>If the {@link BigDecimal} {@code value} is
     * {@code null}, then the specified default value is returned.</p>
     *
     * <pre>
     *   NumberUtils.toDouble(null)                     = 0.0d
     *   NumberUtils.toDouble(BigDecimal.valueOf(8.5d)) = 8.5d
     * </pre>
     *
     * @param value the {@link BigDecimal} to convert, may be {@code null}.
     * @return the double represented by the {@link BigDecimal} or
     *  {@code 0.0d} if the {@link BigDecimal} is {@code null}.
     * @since 3.8
     */
    public static double toDouble(final BigDecimal value) {
        return toDouble(value, 0.0d);
    }

    /**
     * Convert a {@link BigDecimal} to a {@code double}.
     *
     * <p>If the {@link BigDecimal} {@code value} is
     * {@code null}, then the specified default value is returned.</p>
     *
     * <pre>
     *   NumberUtils.toDouble(null, 1.1d)                     = 1.1d
     *   NumberUtils.toDouble(BigDecimal.valueOf(8.5d), 1.1d) = 8.5d
     * </pre>
     *
     * @param value the {@link BigDecimal} to convert, may be {@code null}.
     * @param defaultValue the default value
     * @return the double represented by the {@link BigDecimal} or the
     *  defaultValue if the {@link BigDecimal} is {@code null}.
     * @since 3.8
     */
    public static double toDouble(final BigDecimal value, final double defaultValue) {
        return value == null ? defaultValue : value.doubleValue();
    }

     /**
     * Convert a {@link String} to a {@code byte}, returning
     * {@code zero} if the conversion fails.
     *
     * <p>If the string is {@code null}, {@code zero} is returned.</p>
     *
     * <pre>
     *   NumberUtils.toByte(null) = 0
     *   NumberUtils.toByte("")   = 0
     *   NumberUtils.toByte("1")  = 1
     * </pre>
     *
     * @param str  the string to convert, may be null
     * @return the byte represented by the string, or {@code zero} if
     *  conversion fails
     * @since 2.5
     */
    public static byte toByte(final String str) {
        return toByte(str, (byte) 0);
    }

    /**
     * Convert a {@link String} to a {@code byte}, returning a
     * default value if the conversion fails.
     *
     * <p>If the string is {@code null}, the default value is returned.</p>
     *
     * <pre>
     *   NumberUtils.toByte(null, 1) = 1
     *   NumberUtils.toByte("", 1)   = 1
     *   NumberUtils.toByte("1", 0)  = 1
     * </pre>
     *
     * @param str  the string to convert, may be null
     * @param defaultValue  the default value
     * @return the byte represented by the string, or the default if conversion fails
     * @since 2.5
     */
    public static byte toByte(final String str, final byte defaultValue) {
        if (str == null) {
            return defaultValue;
        }
        try {
            return Byte.parseByte(str);
        } catch (final NumberFormatException nfe) {
            return defaultValue;
        }
    }

    /**
     * Convert a {@link String} to a {@code short}, returning
     * {@code zero} if the conversion fails.
     *
     * <p>If the string is {@code null}, {@code zero} is returned.</p>
     *
     * <pre>
     *   NumberUtils.toShort(null) = 0
     *   NumberUtils.toShort("")   = 0
     *   NumberUtils.toShort("1")  = 1
     * </pre>
     *
     * @param str  the string to convert, may be null
     * @return the short represented by the string, or {@code zero} if
     *  conversion fails
     * @since 2.5
     */
    public static short toShort(final String str) {
        return toShort(str, (short) 0);
    }

    /**
     * Convert a {@link String} to an {@code short}, returning a
     * default value if the conversion fails.
     *
     * <p>If the string is {@code null}, the default value is returned.</p>
     *
     * <pre>
     *   NumberUtils.toShort(null, 1) = 1
     *   NumberUtils.toShort("", 1)   = 1
     *   NumberUtils.toShort("1", 0)  = 1
     * </pre>
     *
     * @param str  the string to convert, may be null
     * @param defaultValue  the default value
     * @return the short represented by the string, or the default if conversion fails
     * @since 2.5
     */
    public static short toShort(final String str, final short defaultValue) {
        if (str == null) {
            return defaultValue;
        }
        try {
            return Short.parseShort(str);
        } catch (final NumberFormatException nfe) {
            return defaultValue;
        }
    }

    /**
     * Convert a {@link BigDecimal} to a {@link BigDecimal} with a scale of
     * two that has been rounded using {@code RoundingMode.HALF_EVEN}. If the supplied
     * {@code value} is null, then {@code BigDecimal.ZERO} is returned.
     *
     * <p>Note, the scale of a {@link BigDecimal} is the number of digits to the right of the
     * decimal point.</p>
     *
     * @param value the {@link BigDecimal} to convert, may be null.
     * @return the scaled, with appropriate rounding, {@link BigDecimal}.
     * @since 3.8
     */
    public static BigDecimal toScaledBigDecimal(final BigDecimal value) {
        return toScaledBigDecimal(value, INTEGER_TWO, RoundingMode.HALF_EVEN);
    }

    /**
     * Convert a {@link BigDecimal} to a {@link BigDecimal} whose scale is the
     * specified value with a {@link RoundingMode} applied. If the input {@code value}
     * is {@code null}, we simply return {@code BigDecimal.ZERO}.
     *
     * @param value the {@link BigDecimal} to convert, may be null.
     * @param scale the number of digits to the right of the decimal point.
     * @param roundingMode a rounding behavior for numerical operations capable of
     *  discarding precision.
     * @return the scaled, with appropriate rounding, {@link BigDecimal}.
     * @since 3.8
     */
    public static BigDecimal toScaledBigDecimal(final BigDecimal value, final int scale, final RoundingMode roundingMode) {
        if (value == null) {
            return BigDecimal.ZERO;
        }
        return value.setScale(
            scale,
            roundingMode == null ? RoundingMode.HALF_EVEN : roundingMode
        );
    }

    /**
     * Convert a {@link Float} to a {@link BigDecimal} with a scale of
     * two that has been rounded using {@code RoundingMode.HALF_EVEN}. If the supplied
     * {@code value} is null, then {@code BigDecimal.ZERO} is returned.
     *
     * <p>Note, the scale of a {@link BigDecimal} is the number of digits to the right of the
     * decimal point.</p>
     *
     * @param value the {@link Float} to convert, may be null.
     * @return the scaled, with appropriate rounding, {@link BigDecimal}.
     * @since 3.8
     */
    public static BigDecimal toScaledBigDecimal(final Float value) {
        return toScaledBigDecimal(value, INTEGER_TWO, RoundingMode.HALF_EVEN);
    }

    /**
     * Convert a {@link Float} to a {@link BigDecimal} whose scale is the
     * specified value with a {@link RoundingMode} applied. If the input {@code value}
     * is {@code null}, we simply return {@code BigDecimal.ZERO}.
     *
     * @param value the {@link Float} to convert, may be null.
     * @param scale the number of digits to the right of the decimal point.
     * @param roundingMode a rounding behavior for numerical operations capable of
     *  discarding precision.
     * @return the scaled, with appropriate rounding, {@link BigDecimal}.
     * @since 3.8
     */
    public static BigDecimal toScaledBigDecimal(final Float value, final int scale, final RoundingMode roundingMode) {
        if (value == null) {
            return BigDecimal.ZERO;
        }
        return toScaledBigDecimal(
            BigDecimal.valueOf(value),
            scale,
            roundingMode
        );
    }

    /**
     * Convert a {@link Double} to a {@link BigDecimal} with a scale of
     * two that has been rounded using {@code RoundingMode.HALF_EVEN}. If the supplied
     * {@code value} is null, then {@code BigDecimal.ZERO} is returned.
     *
     * <p>Note, the scale of a {@link BigDecimal} is the number of digits to the right of the
     * decimal point.</p>
     *
     * @param value the {@link Double} to convert, may be null.
     * @return the scaled, with appropriate rounding, {@link BigDecimal}.
     * @since 3.8
     */
    public static BigDecimal toScaledBigDecimal(final Double value) {
        return toScaledBigDecimal(value, INTEGER_TWO, RoundingMode.HALF_EVEN);
    }

    /**
     * Convert a {@link Double} to a {@link BigDecimal} whose scale is the
     * specified value with a {@link RoundingMode} applied. If the input {@code value}
     * is {@code null}, we simply return {@code BigDecimal.ZERO}.
     *
     * @param value the {@link Double} to convert, may be null.
     * @param scale the number of digits to the right of the decimal point.
     * @param roundingMode a rounding behavior for numerical operations capable of
     *  discarding precision.
     * @return the scaled, with appropriate rounding, {@link BigDecimal}.
     * @since 3.8
     */
    public static BigDecimal toScaledBigDecimal(final Double value, final int scale, final RoundingMode roundingMode) {
        if (value == null) {
            return BigDecimal.ZERO;
        }
        return toScaledBigDecimal(
            BigDecimal.valueOf(value),
            scale,
            roundingMode
        );
    }

    /**
     * Convert a {@link String} to a {@link BigDecimal} with a scale of
     * two that has been rounded using {@code RoundingMode.HALF_EVEN}. If the supplied
     * {@code value} is null, then {@code BigDecimal.ZERO} is returned.
     *
     * <p>Note, the scale of a {@link BigDecimal} is the number of digits to the right of the
     * decimal point.</p>
     *
     * @param value the {@link String} to convert, may be null.
     * @return the scaled, with appropriate rounding, {@link BigDecimal}.
     * @since 3.8
     */
    public static BigDecimal toScaledBigDecimal(final String value) {
        return toScaledBigDecimal(value, INTEGER_TWO, RoundingMode.HALF_EVEN);
    }

    /**
     * Convert a {@link String} to a {@link BigDecimal} whose scale is the
     * specified value with a {@link RoundingMode} applied. If the input {@code value}
     * is {@code null}, we simply return {@code BigDecimal.ZERO}.
     *
     * @param value the {@link String} to convert, may be null.
     * @param scale the number of digits to the right of the decimal point.
     * @param roundingMode a rounding behavior for numerical operations capable of
     *  discarding precision.
     * @return the scaled, with appropriate rounding, {@link BigDecimal}.
     * @since 3.8
     */
    public static BigDecimal toScaledBigDecimal(final String value, final int scale, final RoundingMode roundingMode) {
        if (value == null) {
            return BigDecimal.ZERO;
        }
        return toScaledBigDecimal(
            createBigDecimal(value),
            scale,
            roundingMode
        );
    }

    // must handle Long, Float, Integer, Float, Short,
    //                  BigDecimal, BigInteger and Byte
    // useful methods:
    // Byte.decode(String)
    // Byte.valueOf(String, int radix)
    // Byte.valueOf(String)
    // Double.valueOf(String)
    // Float.valueOf(String)
    // Float.valueOf(String)
    // Integer.valueOf(String, int radix)
    // Integer.valueOf(String)
    // Integer.decode(String)
    // Integer.getInteger(String)
    // Integer.getInteger(String, int val)
    // Integer.getInteger(String, Integer val)
    // Integer.valueOf(String)
    // Double.valueOf(String)
    // new Byte(String)
    // Long.valueOf(String)
    // Long.getLong(String)
    // Long.getLong(String, int)
    // Long.getLong(String, Integer)
    // Long.valueOf(String, int)
    // Long.valueOf(String)
    // Short.valueOf(String)
    // Short.decode(String)
    // Short.valueOf(String, int)
    // Short.valueOf(String)
    // new BigDecimal(String)
    // new BigInteger(String)
    // new BigInteger(String, int radix)
    // Possible inputs:
    // 45 45.5 45E7 4.5E7 Hex Oct Binary xxxF xxxD xxxf xxxd
    // plus minus everything. Prolly more. A lot are not separable.

    /**
     * Turns a string value into a java.lang.Number.
     *
     * <p>If the string starts with {@code 0x} or {@code -0x} (lower or upper case) or {@code #} or {@code -#}, it
     * will be interpreted as a hexadecimal Integer - or Long, if the number of digits after the
     * prefix is more than 8 - or BigInteger if there are more than 16 digits.
     * </p>
     * <p>Then, the value is examined for a type qualifier on the end, i.e. one of
     * {@code 'f', 'F', 'd', 'D', 'l', 'L'}.  If it is found, it starts
     * trying to create successively larger types from the type specified
     * until one is found that can represent the value.</p>
     *
     * <p>If a type specifier is not found, it will check for a decimal point
     * and then try successively larger types from {@link Integer} to
     * {@link BigInteger} and from {@link Float} to
     * {@link BigDecimal}.</p>
     *
     * <p>
     * Integral values with a leading {@code 0} will be interpreted as octal; the returned number will
     * be Integer, Long or BigDecimal as appropriate.
     * </p>
     *
     * <p>Returns {@code null} if the string is {@code null}.</p>
     *
     * <p>This method does not trim the input string, i.e., strings with leading
     * or trailing spaces will generate NumberFormatExceptions.</p>
     *
     * @param str  String containing a number, may be null
     * @return Number created from the string (or null if the input is null)
     * @throws NumberFormatException if the value cannot be converted
     */
    public static Number createNumber(final String str) {
        if (str == null) {
            return null;
        }
        if (StringUtils.isBlank(str)) {
            throw new NumberFormatException("A blank string is not a valid number");
        }
        // Need to deal with all possible hex prefixes here
        final String[] hex_prefixes = {"0x", "0X", "#"};
        final int length = str.length();
        final int offset = str.charAt(0) == '+' || str.charAt(0) == '-' ? 1 : 0;
        int pfxLen = 0;
        for (final String pfx : hex_prefixes) {
            if (str.startsWith(pfx, offset)) {
                pfxLen += pfx.length() + offset;
                break;
            }
        }
        if (pfxLen > 0) { // we have a hex number
            char firstSigDigit = 0; // strip leading zeroes
            for (int i = pfxLen; i < length; i++) {
                firstSigDigit = str.charAt(i);
                if (firstSigDigit != '0') {
                    break;
                }
                pfxLen++;
            }
            final int hexDigits = length - pfxLen;
            if (hexDigits > 16 || hexDigits == 16 && firstSigDigit > '7') { // too many for Long
                return createBigInteger(str);
            }
            if (hexDigits > 8 || hexDigits == 8 && firstSigDigit > '7') { // too many for an int
                return createLong(str);
            }
            return createInteger(str);
        }
        final char lastChar = str.charAt(length - 1);
        final String mant;
        final String dec;
        final String exp;
        final int decPos = str.indexOf('.');
        final int expPos = str.indexOf('e') + str.indexOf('E') + 1; // assumes both not present
        // if both e and E are present, this is caught by the checks on expPos (which prevent IOOBE)
        // and the parsing which will detect if e or E appear in a number due to using the wrong offset

        // Detect if the return type has been requested
        final boolean requestType = !Character.isDigit(lastChar) && lastChar != '.';
        if (decPos > -1) { // there is a decimal point
            if (expPos > -1) { // there is an exponent
                if (expPos < decPos || expPos > length) { // prevents double exponent causing IOOBE
                    throw new NumberFormatException(str + " is not a valid number.");
                }
                dec = str.substring(decPos + 1, expPos);
            } else {
                // No exponent, but there may be a type character to remove
                dec = str.substring(decPos + 1, requestType ? length - 1 : length);
            }
            mant = getMantissa(str, decPos);
        } else {
            if (expPos > -1) {
                if (expPos > length) { // prevents double exponent causing IOOBE
                    throw new NumberFormatException(str + " is not a valid number.");
                }
                mant = getMantissa(str, expPos);
            } else {
                // No decimal, no exponent, but there may be a type character to remove
                mant = getMantissa(str, requestType ? length - 1 : length);
            }
            dec = null;
        }
        if (requestType) {
            if (expPos > -1 && expPos < length - 1) {
                exp = str.substring(expPos + 1, length - 1);
            } else {
                exp = null;
            }
            //Requesting a specific type.
            final String numeric = str.substring(0, length - 1);
            switch (lastChar) {
                case 'l' :
                case 'L' :
                    if (dec == null
                        && exp == null
                        && (!numeric.isEmpty() && numeric.charAt(0) == '-' && isDigits(numeric.substring(1)) || isDigits(numeric))) {
                        try {
                            return createLong(numeric);
                        } catch (final NumberFormatException ignored) {
                            // Too big for a long
                        }
                        return createBigInteger(numeric);

                    }
                    throw new NumberFormatException(str + " is not a valid number.");
                case 'f' :
                case 'F' :
                    try {
                        final Float f = createFloat(str);
                        if (!(f.isInfinite() || f.floatValue() == 0.0F && !isZero(mant, dec))) {
                            //If it's too big for a float or the float value = 0 and the string
                            //has non-zeros in it, then float does not have the precision we want
                            return f;
                        }

                    } catch (final NumberFormatException ignored) {
                        // ignore the bad number
                    }
                    //$FALL-THROUGH$
                case 'd' :
                case 'D' :
                    try {
                        final Double d = createDouble(str);
                        if (!(d.isInfinite() || d.doubleValue() == 0.0D && !isZero(mant, dec))) {
                            return d;
                        }
                    } catch (final NumberFormatException ignored) {
                        // ignore the bad number
                    }
                    try {
                        return createBigDecimal(numeric);
                    } catch (final NumberFormatException ignored) {
                        // ignore the bad number
                    }
                    //$FALL-THROUGH$
                default :
                    throw new NumberFormatException(str + " is not a valid number.");

            }
        }
        //User doesn't have a preference on the return type, so let's start
        //small and go from there...
        if (expPos > -1 && expPos < length - 1) {
            exp = str.substring(expPos + 1);
        } else {
            exp = null;
        }
        if (dec == null && exp == null) { // no decimal point and no exponent
            //Must be an Integer, Long, Biginteger
            try {
                return createInteger(str);
            } catch (final NumberFormatException ignored) {
                // ignore the bad number
            }
            try {
                return createLong(str);
            } catch (final NumberFormatException ignored) {
                // ignore the bad number
            }
            return createBigInteger(str);
        }

        //Must be a Float, Double, BigDecimal
        try {
            final Float f = createFloat(str);
            final Double d = createDouble(str);
            if (!f.isInfinite()
                    && !(f.floatValue() == 0.0F && !isZero(mant, dec))
                    && f.toString().equals(d.toString())) {
                return f;
            }
            if (!d.isInfinite() && !(d.doubleValue() == 0.0D && !isZero(mant, dec))) {
                final BigDecimal b = createBigDecimal(str);
                if (b.compareTo(BigDecimal.valueOf(d.doubleValue())) == 0) {
                    return d;
                }
                return b;
            }
        } catch (final NumberFormatException ignored) {
            // ignore the bad number
        }
        return createBigDecimal(str);
    }

    /**
     * Utility method for {@link #createNumber(java.lang.String)}.
     *
     * <p>Returns mantissa of the given number.</p>
     *
     * @param str the string representation of the number
     * @param stopPos the position of the exponent or decimal point
     * @return mantissa of the given number
     */
    private static String getMantissa(final String str, final int stopPos) {
        final char firstChar = str.charAt(0);
        final boolean hasSign = firstChar == '-' || firstChar == '+';

        return hasSign ? str.substring(1, stopPos) : str.substring(0, stopPos);
    }

    /**
     * Utility method for {@link #createNumber(java.lang.String)}.
     *
     * <p>This will check if the magnitude of the number is zero by checking if there
     * are only zeros before and after the decimal place.</p>
     *
     * <p>Note: It is <strong>assumed</strong> that the input string has been converted
     * to either a Float or Double with a value of zero when this method is called.
     * This eliminates invalid input for example {@code ".", ".D", ".e0"}.</p>
     *
     * <p>Thus the method only requires checking if both arguments are null, empty or
     * contain only zeros.</p>
     *
     * <p>Given {@code s = mant + "." + dec}:</p>
     * <ul>
     * <li>{@code true} if s is {@code "0.0"}
     * <li>{@code true} if s is {@code "0."}
     * <li>{@code true} if s is {@code ".0"}
     * <li>{@code false} otherwise (this assumes {@code "."} is not possible)
     * </ul>
     *
     * @param mant the mantissa decimal digits before the decimal point (sign must be removed; never null)
     * @param dec the decimal digits after the decimal point (exponent and type specifier removed;
     *            can be null)
     * @return true if the magnitude is zero
     */
    private static boolean isZero(final String mant, final String dec) {
        return isAllZeros(mant) && isAllZeros(dec);
    }

    /**
     * Utility method for {@link #createNumber(java.lang.String)}.
     *
     * <p>Returns {@code true} if s is {@code null} or empty.</p>
     *
     * @param str the String to check
     * @return if it is all zeros or {@code null}
     */
    private static boolean isAllZeros(final String str) {
        if (str == null) {
            return true;
        }
        for (int i = str.length() - 1; i >= 0; i--) {
            if (str.charAt(i) != '0') {
                return false;
            }
        }
        return true;
    }

    /**
     * Convert a {@link String} to a {@link Float}.
     *
     * <p>Returns {@code null} if the string is {@code null}.</p>
     *
     * @param str  a {@link String} to convert, may be null
     * @return converted {@link Float} (or null if the input is null)
     * @throws NumberFormatException if the value cannot be converted
     */
    public static Float createFloat(final String str) {
        if (str == null) {
            return null;
        }
        return Float.valueOf(str);
    }

    /**
     * Convert a {@link String} to a {@link Double}.
     *
     * <p>Returns {@code null} if the string is {@code null}.</p>
     *
     * @param str  a {@link String} to convert, may be null
     * @return converted {@link Double} (or null if the input is null)
     * @throws NumberFormatException if the value cannot be converted
     */
    public static Double createDouble(final String str) {
        if (str == null) {
            return null;
        }
        return Double.valueOf(str);
    }

    /**
     * Convert a {@link String} to a {@link Integer}, handling
     * hex (0xhhhh) and octal (0dddd) notations.
     * N.B. a leading zero means octal; spaces are not trimmed.
     *
     * <p>Returns {@code null} if the string is {@code null}.</p>
     *
     * @param str  a {@link String} to convert, may be null
     * @return converted {@link Integer} (or null if the input is null)
     * @throws NumberFormatException if the value cannot be converted
     */
    public static Integer createInteger(final String str) {
        if (str == null) {
            return null;
        }
        // decode() handles 0xAABD and 0777 (hex and octal) as well.
        return Integer.decode(str);
    }

    /**
     * Convert a {@link String} to a {@link Long};
     * since 3.1 it handles hex (0Xhhhh) and octal (0ddd) notations.
     * N.B. a leading zero means octal; spaces are not trimmed.
     *
     * <p>Returns {@code null} if the string is {@code null}.</p>
     *
     * @param str  a {@link String} to convert, may be null
     * @return converted {@link Long} (or null if the input is null)
     * @throws NumberFormatException if the value cannot be converted
     */
    public static Long createLong(final String str) {
        if (str == null) {
            return null;
        }
        return Long.decode(str);
    }

    /**
     * Convert a {@link String} to a {@link BigInteger};
     * since 3.2 it handles hex (0x or #) and octal (0) notations.
     *
     * <p>Returns {@code null} if the string is {@code null}.</p>
     *
     * @param str  a {@link String} to convert, may be null
     * @return converted {@link BigInteger} (or null if the input is null)
     * @throws NumberFormatException if the value cannot be converted
     */
    public static BigInteger createBigInteger(final String str) {
        if (str == null) {
            return null;
        }
        if (str.isEmpty()) {
            throw new NumberFormatException("An empty string is not a valid number");
        }
        int pos = 0; // offset within string
        int radix = 10;
        boolean negate = false; // need to negate later?
        final char char0 = str.charAt(0);
        if (char0 == '-') {
            negate = true;
            pos = 1;
        } else if (char0 == '+') {
            pos = 1;
        }
        if (str.startsWith("0x", pos) || str.startsWith("0X", pos)) { // hex
            radix = 16;
            pos += 2;
        } else if (str.startsWith("#", pos)) { // alternative hex (allowed by Long/Integer)
            radix = 16;
            pos++;
        } else if (str.startsWith("0", pos) && str.length() > pos + 1) { // octal; so long as there are additional digits
            radix = 8;
            pos++;
        } // default is to treat as decimal

        final BigInteger value = new BigInteger(str.substring(pos), radix);
        return negate ? value.negate() : value;
    }

    /**
     * Convert a {@link String} to a {@link BigDecimal}.
     *
     * <p>Returns {@code null} if the string is {@code null}.</p>
     *
     * @param str  a {@link String} to convert, may be null
     * @return converted {@link BigDecimal} (or null if the input is null)
     * @throws NumberFormatException if the value cannot be converted
     */
    public static BigDecimal createBigDecimal(final String str) {
        if (str == null) {
            return null;
        }
        // handle JDK1.3.1 bug where "" throws IndexOutOfBoundsException
        if (StringUtils.isBlank(str)) {
            throw new NumberFormatException("A blank string is not a valid number");
        }
        return new BigDecimal(str);
    }

    /**
     * Returns the minimum value in an array.
     *
     * @param array  an array, must not be null or empty
     * @return the minimum value in the array
     * @throws NullPointerException if {@code array} is {@code null}
     * @throws IllegalArgumentException if {@code array} is empty
     * @since 3.4 Changed signature from min(long[]) to min(long...)
     */
    public static long min(final long... array) {
        // Validates input
        validateArray(array);

        // Finds and returns min
        long min = array[0];
        for (int i = 1; i < array.length; i++) {
            if (array[i] < min) {
                min = array[i];
            }
        }

        return min;
    }

    /**
     * Returns the minimum value in an array.
     *
     * @param array  an array, must not be null or empty
     * @return the minimum value in the array
     * @throws NullPointerException if {@code array} is {@code null}
     * @throws IllegalArgumentException if {@code array} is empty
     * @since 3.4 Changed signature from min(int[]) to min(int...)
     */
    public static int min(final int... array) {
        // Validates input
        validateArray(array);

        // Finds and returns min
        int min = array[0];
        for (int j = 1; j < array.length; j++) {
            if (array[j] < min) {
                min = array[j];
            }
        }

        return min;
    }

    /**
     * Returns the minimum value in an array.
     *
     * @param array  an array, must not be null or empty
     * @return the minimum value in the array
     * @throws NullPointerException if {@code array} is {@code null}
     * @throws IllegalArgumentException if {@code array} is empty
     * @since 3.4 Changed signature from min(short[]) to min(short...)
     */
    public static short min(final short... array) {
        // Validates input
        validateArray(array);

        // Finds and returns min
        short min = array[0];
        for (int i = 1; i < array.length; i++) {
            if (array[i] < min) {
                min = array[i];
            }
        }

        return min;
    }

    /**
     * Returns the minimum value in an array.
     *
     * @param array  an array, must not be null or empty
     * @return the minimum value in the array
     * @throws NullPointerException if {@code array} is {@code null}
     * @throws IllegalArgumentException if {@code array} is empty
     * @since 3.4 Changed signature from min(byte[]) to min(byte...)
     */
    public static byte min(final byte... array) {
        // Validates input
        validateArray(array);

        // Finds and returns min
        byte min = array[0];
        for (int i = 1; i < array.length; i++) {
            if (array[i] < min) {
                min = array[i];
            }
        }

        return min;
    }

     /**
     * Returns the minimum value in an array.
     *
     * @param array  an array, must not be null or empty
     * @return the minimum value in the array
     * @throws NullPointerException if {@code array} is {@code null}
     * @throws IllegalArgumentException if {@code array} is empty
     * @see IEEE754rUtils#min(double[]) IEEE754rUtils for a version of this method that handles NaN differently
     * @since 3.4 Changed signature from min(double[]) to min(double...)
     */
    public static double min(final double... array) {
        // Validates input
        validateArray(array);

        // Finds and returns min
        double min = array[0];
        for (int i = 1; i < array.length; i++) {
            if (Double.isNaN(array[i])) {
                return Double.NaN;
            }
            if (array[i] < min) {
                min = array[i];
            }
        }

        return min;
    }

    /**
     * Returns the minimum value in an array.
     *
     * @param array  an array, must not be null or empty
     * @return the minimum value in the array
     * @throws NullPointerException if {@code array} is {@code null}
     * @throws IllegalArgumentException if {@code array} is empty
     * @see IEEE754rUtils#min(float[]) IEEE754rUtils for a version of this method that handles NaN differently
     * @since 3.4 Changed signature from min(float[]) to min(float...)
     */
    public static float min(final float... array) {
        // Validates input
        validateArray(array);

        // Finds and returns min
        float min = array[0];
        for (int i = 1; i < array.length; i++) {
            if (Float.isNaN(array[i])) {
                return Float.NaN;
            }
            if (array[i] < min) {
                min = array[i];
            }
        }

        return min;
    }

    /**
     * Returns the maximum value in an array.
     *
     * @param array  an array, must not be null or empty
     * @return the maximum value in the array
     * @throws NullPointerException if {@code array} is {@code null}
     * @throws IllegalArgumentException if {@code array} is empty
     * @since 3.4 Changed signature from max(long[]) to max(long...)
     */
    public static long max(final long... array) {
        // Validates input
        validateArray(array);

        // Finds and returns max
        long max = array[0];
        for (int j = 1; j < array.length; j++) {
            if (array[j] > max) {
                max = array[j];
            }
        }

        return max;
    }

    /**
     * Returns the maximum value in an array.
     *
     * @param array  an array, must not be null or empty
     * @return the maximum value in the array
     * @throws NullPointerException if {@code array} is {@code null}
     * @throws IllegalArgumentException if {@code array} is empty
     * @since 3.4 Changed signature from max(int[]) to max(int...)
     */
    public static int max(final int... array) {
        // Validates input
        validateArray(array);

        // Finds and returns max
        int max = array[0];
        for (int j = 1; j < array.length; j++) {
            if (array[j] > max) {
                max = array[j];
            }
        }

        return max;
    }

    /**
     * Returns the maximum value in an array.
     *
     * @param array  an array, must not be null or empty
     * @return the maximum value in the array
     * @throws NullPointerException if {@code array} is {@code null}
     * @throws IllegalArgumentException if {@code array} is empty
     * @since 3.4 Changed signature from max(short[]) to max(short...)
     */
    public static short max(final short... array) {
        // Validates input
        validateArray(array);

        // Finds and returns max
        short max = array[0];
        for (int i = 1; i < array.length; i++) {
            if (array[i] > max) {
                max = array[i];
            }
        }

        return max;
    }

    /**
     * Returns the maximum value in an array.
     *
     * @param array  an array, must not be null or empty
     * @return the maximum value in the array
     * @throws NullPointerException if {@code array} is {@code null}
     * @throws IllegalArgumentException if {@code array} is empty
     * @since 3.4 Changed signature from max(byte[]) to max(byte...)
     */
    public static byte max(final byte... array) {
        // Validates input
        validateArray(array);

        // Finds and returns max
        byte max = array[0];
        for (int i = 1; i < array.length; i++) {
            if (array[i] > max) {
                max = array[i];
            }
        }

        return max;
    }

    /**
     * Returns the maximum value in an array.
     *
     * @param array  an array, must not be null or empty
     * @return the maximum value in the array
     * @throws NullPointerException if {@code array} is {@code null}
     * @throws IllegalArgumentException if {@code array} is empty
     * @see IEEE754rUtils#max(double[]) IEEE754rUtils for a version of this method that handles NaN differently
     * @since 3.4 Changed signature from max(double[]) to max(double...)
     */
    public static double max(final double... array) {
        // Validates input
        validateArray(array);

        // Finds and returns max
        double max = array[0];
        for (int j = 1; j < array.length; j++) {
            if (Double.isNaN(array[j])) {
                return Double.NaN;
            }
            if (array[j] > max) {
                max = array[j];
            }
        }

        return max;
    }

    /**
     * Returns the maximum value in an array.
     *
     * @param array  an array, must not be null or empty
     * @return the maximum value in the array
     * @throws NullPointerException if {@code array} is {@code null}
     * @throws IllegalArgumentException if {@code array} is empty
     * @see IEEE754rUtils#max(float[]) IEEE754rUtils for a version of this method that handles NaN differently
     * @since 3.4 Changed signature from max(float[]) to max(float...)
     */
    public static float max(final float... array) {
        // Validates input
        validateArray(array);

        // Finds and returns max
        float max = array[0];
        for (int j = 1; j < array.length; j++) {
            if (Float.isNaN(array[j])) {
                return Float.NaN;
            }
            if (array[j] > max) {
                max = array[j];
            }
        }

        return max;
    }

    /**
     * Checks if the specified array is neither null nor empty.
     *
     * @param array  the array to check
     * @throws IllegalArgumentException if {@code array} is empty
     * @throws NullPointerException if {@code array} is {@code null}
     */
    private static void validateArray(final Object array) {
        Objects.requireNonNull(array, "array");
        Validate.isTrue(Array.getLength(array) != 0, "Array cannot be empty.");
    }

    // 3 param min
    /**
     * Gets the minimum of three {@code long} values.
     *
     * @param a  value 1
     * @param b  value 2
     * @param c  value 3
     * @return  the smallest of the values
     */
    public static long min(long a, final long b, final long c) {
        if (b < a) {
            a = b;
        }
        if (c < a) {
            a = c;
        }
        return a;
    }

    /**
     * Gets the minimum of three {@code int} values.
     *
     * @param a  value 1
     * @param b  value 2
     * @param c  value 3
     * @return  the smallest of the values
     */
    public static int min(int a, final int b, final int c) {
        if (b < a) {
            a = b;
        }
        if (c < a) {
            a = c;
        }
        return a;
    }

    /**
     * Gets the minimum of three {@code short} values.
     *
     * @param a  value 1
     * @param b  value 2
     * @param c  value 3
     * @return  the smallest of the values
     */
    public static short min(short a, final short b, final short c) {
        if (b < a) {
            a = b;
        }
        if (c < a) {
            a = c;
        }
        return a;
    }

    /**
     * Gets the minimum of three {@code byte} values.
     *
     * @param a  value 1
     * @param b  value 2
     * @param c  value 3
     * @return  the smallest of the values
     */
    public static byte min(byte a, final byte b, final byte c) {
        if (b < a) {
            a = b;
        }
        if (c < a) {
            a = c;
        }
        return a;
    }

    /**
     * Gets the minimum of three {@code double} values.
     *
     * <p>If any value is {@code NaN}, {@code NaN} is
     * returned. Infinity is handled.</p>
     *
     * @param a  value 1
     * @param b  value 2
     * @param c  value 3
     * @return  the smallest of the values
     * @see IEEE754rUtils#min(double, double, double) for a version of this method that handles NaN differently
     */
    public static double min(final double a, final double b, final double c) {
        return Math.min(Math.min(a, b), c);
    }

    /**
     * Gets the minimum of three {@code float} values.
     *
     * <p>If any value is {@code NaN}, {@code NaN} is
     * returned. Infinity is handled.</p>
     *
     * @param a  value 1
     * @param b  value 2
     * @param c  value 3
     * @return  the smallest of the values
     * @see IEEE754rUtils#min(float, float, float) for a version of this method that handles NaN differently
     */
    public static float min(final float a, final float b, final float c) {
        return Math.min(Math.min(a, b), c);
    }

    // 3 param max
    /**
     * Gets the maximum of three {@code long} values.
     *
     * @param a  value 1
     * @param b  value 2
     * @param c  value 3
     * @return  the largest of the values
     */
    public static long max(long a, final long b, final long c) {
        if (b > a) {
            a = b;
        }
        if (c > a) {
            a = c;
        }
        return a;
    }

    /**
     * Gets the maximum of three {@code int} values.
     *
     * @param a  value 1
     * @param b  value 2
     * @param c  value 3
     * @return  the largest of the values
     */
    public static int max(int a, final int b, final int c) {
        if (b > a) {
            a = b;
        }
        if (c > a) {
            a = c;
        }
        return a;
    }

    /**
     * Gets the maximum of three {@code short} values.
     *
     * @param a  value 1
     * @param b  value 2
     * @param c  value 3
     * @return  the largest of the values
     */
    public static short max(short a, final short b, final short c) {
        if (b > a) {
            a = b;
        }
        if (c > a) {
            a = c;
        }
        return a;
    }

    /**
     * Gets the maximum of three {@code byte} values.
     *
     * @param a  value 1
     * @param b  value 2
     * @param c  value 3
     * @return  the largest of the values
     */
    public static byte max(byte a, final byte b, final byte c) {
        if (b > a) {
            a = b;
        }
        if (c > a) {
            a = c;
        }
        return a;
    }

    /**
     * Gets the maximum of three {@code double} values.
     *
     * <p>If any value is {@code NaN}, {@code NaN} is
     * returned. Infinity is handled.</p>
     *
     * @param a  value 1
     * @param b  value 2
     * @param c  value 3
     * @return  the largest of the values
     * @see IEEE754rUtils#max(double, double, double) for a version of this method that handles NaN differently
     */
    public static double max(final double a, final double b, final double c) {
        return Math.max(Math.max(a, b), c);
    }

    /**
     * Gets the maximum of three {@code float} values.
     *
     * <p>If any value is {@code NaN}, {@code NaN} is
     * returned. Infinity is handled.</p>
     *
     * @param a  value 1
     * @param b  value 2
     * @param c  value 3
     * @return  the largest of the values
     * @see IEEE754rUtils#max(float, float, float) for a version of this method that handles NaN differently
     */
    public static float max(final float a, final float b, final float c) {
        return Math.max(Math.max(a, b), c);
    }

    /**
     * Checks whether the {@link String} contains only
     * digit characters.
     *
     * <p>{@code null} and empty String will return
     * {@code false}.</p>
     *
     * @param str  the {@link String} to check
     * @return {@code true} if str contains only Unicode numeric
     */
    public static boolean isDigits(final String str) {
        return StringUtils.isNumeric(str);
    }

    /**
     * Checks whether the String a valid Java number.
     *
     * <p>Valid numbers include hexadecimal marked with the {@code 0x} or
     * {@code 0X} qualifier, octal numbers, scientific notation and
     * numbers marked with a type qualifier (e.g. 123L).</p>
     *
     * <p>Non-hexadecimal strings beginning with a leading zero are
     * treated as octal values. Thus the string {@code 09} will return
     * {@code false}, since {@code 9} is not a valid octal value.
     * However, numbers beginning with {@code 0.} are treated as decimal.</p>
     *
     * <p>{@code null} and empty/blank {@link String} will return
     * {@code false}.</p>
     *
     * <p>Note, {@link #createNumber(String)} should return a number for every
     * input resulting in {@code true}.</p>
     *
     * @param str  the {@link String} to check
     * @return {@code true} if the string is a correctly formatted number
     * @since 3.3 the code supports hex {@code 0Xhhh} an
     *        octal {@code 0ddd} validation
     * @deprecated This feature will be removed in Lang 4.0,
     *             use {@link NumberUtils#isCreatable(String)} instead
     */
    @Deprecated
    public static boolean isNumber(final String str) {
        return isCreatable(str);
    }

    /**
     * Checks whether the String a valid Java number.
     *
     * <p>Valid numbers include hexadecimal marked with the {@code 0x} or
     * {@code 0X} qualifier, octal numbers, scientific notation and
     * numbers marked with a type qualifier (e.g. 123L).</p>
     *
     * <p>Non-hexadecimal strings beginning with a leading zero are
     * treated as octal values. Thus the string {@code 09} will return
     * {@code false}, since {@code 9} is not a valid octal value.
     * However, numbers beginning with {@code 0.} are treated as decimal.</p>
     *
     * <p>{@code null} and empty/blank {@link String} will return
     * {@code false}.</p>
     *
     * <p>Note, {@link #createNumber(String)} should return a number for every
     * input resulting in {@code true}.</p>
     *
     * @param str  the {@link String} to check
     * @return {@code true} if the string is a correctly formatted number
     * @since 3.5
     */
    public static boolean isCreatable(final String str) {
        if (StringUtils.isEmpty(str)) {
            return false;
        }
        final char[] chars = str.toCharArray();
        int sz = chars.length;
        boolean hasExp = false;
        boolean hasDecPoint = false;
        boolean allowSigns = false;
        boolean foundDigit = false;
        // deal with any possible sign up front
        final int start = chars[0] == '-' || chars[0] == '+' ? 1 : 0;
        if (sz > start + 1 && chars[start] == '0' && !StringUtils.contains(str, '.')) { // leading 0, skip if is a decimal number
            if (chars[start + 1] == 'x' || chars[start + 1] == 'X') { // leading 0x/0X
                int i = start + 2;
                if (i == sz) {
                    return false; // str == "0x"
                }
                // checking hex (it can't be anything else)
                for (; i < chars.length; i++) {
                    if ((chars[i] < '0' || chars[i] > '9')
                        && (chars[i] < 'a' || chars[i] > 'f')
                        && (chars[i] < 'A' || chars[i] > 'F')) {
                        return false;
                    }
                }
                return true;
           }
            if (Character.isDigit(chars[start + 1])) {
                   // leading 0, but not hex, must be octal
                   int i = start + 1;
                   for (; i < chars.length; i++) {
                       if (chars[i] < '0' || chars[i] > '7') {
                           return false;
                       }
                   }
                   return true;
               }
        }
        sz--; // don't want to loop to the last char, check it afterwords
              // for type qualifiers
        int i = start;
        // loop to the next to last char or to the last char if we need another digit to
        // make a valid number (e.g. chars[0..5] = "1234E")
        while (i < sz || i < sz + 1 && allowSigns && !foundDigit) {
            if (chars[i] >= '0' && chars[i] <= '9') {
                foundDigit = true;
                allowSigns = false;

            } else if (chars[i] == '.') {
                if (hasDecPoint || hasExp) {
                    // two decimal points or dec in exponent
                    return false;
                }
                hasDecPoint = true;
            } else if (chars[i] == 'e' || chars[i] == 'E') {
                // we've already taken care of hex.
                if (hasExp) {
                    // two E's
                    return false;
                }
                if (!foundDigit) {
                    return false;
                }
                hasExp = true;
                allowSigns = true;
            } else if (chars[i] == '+' || chars[i] == '-') {
                if (!allowSigns) {
                    return false;
                }
                allowSigns = false;
                foundDigit = false; // we need a digit after the E
            } else {
                return false;
            }
            i++;
        }
        if (i < chars.length) {
            if (chars[i] >= '0' && chars[i] <= '9') {
                // no type qualifier, OK
                return true;
            }
            if (chars[i] == 'e' || chars[i] == 'E') {
                // can't have an E at the last byte
                return false;
            }
            if (chars[i] == '.') {
                if (hasDecPoint || hasExp) {
                    // two decimal points or dec in exponent
                    return false;
                }
                // single trailing decimal point after non-exponent is ok
                return foundDigit;
            }
            if (!allowSigns
                && (chars[i] == 'd'
                    || chars[i] == 'D'
                    || chars[i] == 'f'
                    || chars[i] == 'F')) {
                return foundDigit;
            }
            if (chars[i] == 'l'
                || chars[i] == 'L') {
                // not allowing L with an exponent or decimal point
                return foundDigit && !hasExp && !hasDecPoint;
            }
            // last character is illegal
            return false;
        }
        // allowSigns is true iff the val ends in 'E'
        // found digit it to make sure weird stuff like '.' and '1E-' doesn't pass
        return !allowSigns && foundDigit;
    }

    /**
     * Checks whether the given String is a parsable number.
     *
     * <p>Parsable numbers include those Strings understood by {@link Integer#parseInt(String)},
     * {@link Long#parseLong(String)}, {@link Float#parseFloat(String)} or
     * {@link Double#parseDouble(String)}. This method can be used instead of catching {@link java.text.ParseException}
     * when calling one of those methods.</p>
     *
     * <p>Hexadecimal and scientific notations are <strong>not</strong> considered parsable.
     * See {@link #isCreatable(String)} on those cases.</p>
     *
     * <p>{@code null} and empty String will return {@code false}.</p>
     *
     * @param str the String to check.
     * @return {@code true} if the string is a parsable number.
     * @since 3.4
     */
    public static boolean isParsable(final String str) {
        if (StringUtils.isEmpty(str)) {
            return false;
        }
        if (str.charAt(str.length() - 1) == '.') {
            return false;
        }
        if (str.charAt(0) == '-') {
            if (str.length() == 1) {
                return false;
            }
            return withDecimalsParsing(str, 1);
        }
        return withDecimalsParsing(str, 0);
    }

    private static boolean withDecimalsParsing(final String str, final int beginIdx) {
        int decimalPoints = 0;
        for (int i = beginIdx; i < str.length(); i++) {
            final boolean isDecimalPoint = str.charAt(i) == '.';
            if (isDecimalPoint) {
                decimalPoints++;
            }
            if (decimalPoints > 1) {
                return false;
            }
            if (!isDecimalPoint && !Character.isDigit(str.charAt(i))) {
                return false;
            }
        }
        return true;
    }

    /**
     * Compares two {@code int} values numerically. This is the same functionality as provided in Java 7.
     *
     * @param x the first {@code int} to compare
     * @param y the second {@code int} to compare
     * @return the value {@code 0} if {@code x == y};
     *         a value less than {@code 0} if {@code x < y}; and
     *         a value greater than {@code 0} if {@code x > y}
     * @since 3.4
     */
    public static int compare(final int x, final int y) {
        if (x == y) {
            return 0;
        }
        return x < y ? -1 : 1;
    }

    /**
     * Compares to {@code long} values numerically. This is the same functionality as provided in Java 7.
     *
     * @param x the first {@code long} to compare
     * @param y the second {@code long} to compare
     * @return the value {@code 0} if {@code x == y};
     *         a value less than {@code 0} if {@code x < y}; and
     *         a value greater than {@code 0} if {@code x > y}
     * @since 3.4
     */
    public static int compare(final long x, final long y) {
        if (x == y) {
            return 0;
        }
        return x < y ? -1 : 1;
    }

    /**
     * Compares to {@code short} values numerically. This is the same functionality as provided in Java 7.
     *
     * @param x the first {@code short} to compare
     * @param y the second {@code short} to compare
     * @return the value {@code 0} if {@code x == y};
     *         a value less than {@code 0} if {@code x < y}; and
     *         a value greater than {@code 0} if {@code x > y}
     * @since 3.4
     */
    public static int compare(final short x, final short y) {
        if (x == y) {
            return 0;
        }
        return x < y ? -1 : 1;
    }

    /**
     * Compares two {@code byte} values numerically. This is the same functionality as provided in Java 7.
     *
     * @param x the first {@code byte} to compare
     * @param y the second {@code byte} to compare
     * @return the value {@code 0} if {@code x == y};
     *         a value less than {@code 0} if {@code x < y}; and
     *         a value greater than {@code 0} if {@code x > y}
     * @since 3.4
     */
    public static int compare(final byte x, final byte y) {
        return x - y;
    }
}
