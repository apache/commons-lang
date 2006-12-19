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
package org.apache.commons.lang;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * <p>Provides extra functionality for Java Number classes.</p>
 *
 * @author <a href="mailto:rand_mcneely@yahoo.com">Rand McNeely</a>
 * @author Stephen Colebourne
 * @author <a href="mailto:steve.downey@netfolio.com">Steve Downey</a>
 * @author Eric Pugh
 * @author Phil Steitz
 * @since 1.0
 * @version $Id$
 * 
 * @deprecated Moved to org.apache.commons.lang.math.
 *             Class will be removed in Commons Lang 3.0.
 */
public final class NumberUtils {
    // DEPRECATED CLASS !!!
    
    /**
     * <p><code>NumberUtils</code> instances should NOT be constructed in standard programming.
     * Instead, the class should be used as <code>NumberUtils.stringToInt("6");</code>.</p>
     *
     * <p>This constructor is public to permit tools that require a JavaBean instance
     * to operate.</p>
     */
    public NumberUtils() {
      super();
    }

    //--------------------------------------------------------------------
    
    /**
     * <p>Convert a <code>String</code> to an <code>int</code>, returning
     * <code>zero</code> if the conversion fails.</p>
     * 
     * @param str  the string to convert
     * @return the int represented by the string, or <code>zero</code> if
     *  conversion fails
     */
    public static int stringToInt(String str) {
        return stringToInt(str, 0);
    }

    /**
     * <p>Convert a <code>String</code> to an <code>int</code>, returning a
     * default value if the conversion fails.</p>
     * 
     * @param str  the string to convert
     * @param defaultValue  the default value
     * @return the int represented by the string, or the default if conversion fails
     */
    public static int stringToInt(String str, int defaultValue) {
        try {
            return Integer.parseInt(str);
        } catch (NumberFormatException nfe) {
            return defaultValue;
        }
    }

    //--------------------------------------------------------------------
    
    // must handle Long, Float, Integer, Float, Short,
    //                  BigDecimal, BigInteger and Byte
    // useful methods:
    // Byte.decode(String)
    // Byte.valueOf(String,int radix)
    // Byte.valueOf(String)
    // Double.valueOf(String)
    // Float.valueOf(String)
    // new Float(String)
    // Integer.valueOf(String,int radix)
    // Integer.valueOf(String)
    // Integer.decode(String)
    // Integer.getInteger(String)
    // Integer.getInteger(String,int val)
    // Integer.getInteger(String,Integer val)
    // new Integer(String)
    // new Double(String)
    // new Byte(String)
    // new Long(String)
    // Long.getLong(String)
    // Long.getLong(String,int)
    // Long.getLong(String,Integer)
    // Long.valueOf(String,int)
    // Long.valueOf(String)
    // new Short(String)
    // Short.decode(String)
    // Short.valueOf(String,int)
    // Short.valueOf(String)
    // new BigDecimal(String)
    // new BigInteger(String)
    // new BigInteger(String,int radix)
    // Possible inputs:
    // 45 45.5 45E7 4.5E7 Hex Oct Binary xxxF xxxD xxxf xxxd
    // plus minus everything. Prolly more. A lot are not separable.

    /**
     * <p>Turns a string value into a java.lang.Number.</p>
     *
     * <p>First, the value is examined for a type qualifier on the end
     * (<code>'f','F','d','D','l','L'</code>).  If it is found, it starts 
     * trying to create successively larger types from the type specified
     * until one is found that can hold the value.</p>
     *
     * <p>If a type specifier is not found, it will check for a decimal point
     * and then try successively larger types from <code>Integer</code> to
     * <code>BigInteger</code> and from <code>Float</code> to
     * <code>BigDecimal</code>.</p>
     *
     * <p>If the string starts with <code>0x</code> or <code>-0x</code>, it
     * will be interpreted as a hexadecimal integer.  Values with leading
     * <code>0</code>'s will not be interpreted as octal.</p>
     *
     * @param val String containing a number
     * @return Number created from the string
     * @throws NumberFormatException if the value cannot be converted
     */
    public static Number createNumber(String val) throws NumberFormatException {
        if (val == null) {
            return null;
        }
        if (val.length() == 0) {
            throw new NumberFormatException("\"\" is not a valid number.");
        }
        if (val.startsWith("--")) {
            // this is protection for poorness in java.lang.BigDecimal.
            // it accepts this as a legal value, but it does not appear 
            // to be in specification of class. OS X Java parses it to 
            // a wrong value.
            return null;
        }
        if (val.startsWith("0x") || val.startsWith("-0x")) {
            return createInteger(val);
        }   
        char lastChar = val.charAt(val.length() - 1);
        String mant;
        String dec;
        String exp;
        int decPos = val.indexOf('.');
        int expPos = val.indexOf('e') + val.indexOf('E') + 1;

        if (decPos > -1) {

            if (expPos > -1) {
                if (expPos < decPos) {
                    throw new NumberFormatException(val + " is not a valid number.");
                }
                dec = val.substring(decPos + 1, expPos);
            } else {
                dec = val.substring(decPos + 1);
            }
            mant = val.substring(0, decPos);
        } else {
            if (expPos > -1) {
                mant = val.substring(0, expPos);
            } else {
                mant = val;
            }
            dec = null;
        }
        if (!Character.isDigit(lastChar)) {
            if (expPos > -1 && expPos < val.length() - 1) {
                exp = val.substring(expPos + 1, val.length() - 1);
            } else {
                exp = null;
            }
            //Requesting a specific type..
            String numeric = val.substring(0, val.length() - 1);
            boolean allZeros = isAllZeros(mant) && isAllZeros(exp);
            switch (lastChar) {
                case 'l' :
                case 'L' :
                    if (dec == null
                        && exp == null
                        && (numeric.charAt(0) == '-' && isDigits(numeric.substring(1)) || isDigits(numeric))) {
                        try {
                            return createLong(numeric);
                        } catch (NumberFormatException nfe) {
                            //Too big for a long
                        }
                        return createBigInteger(numeric);

                    }
                    throw new NumberFormatException(val + " is not a valid number.");
                case 'f' :
                case 'F' :
                    try {
                        Float f = NumberUtils.createFloat(numeric);
                        if (!(f.isInfinite() || (f.floatValue() == 0.0F && !allZeros))) {
                            //If it's too big for a float or the float value = 0 and the string
                            //has non-zeros in it, then float does not have the precision we want
                            return f;
                        }

                    } catch (NumberFormatException e) {
                        // ignore the bad number
                    }
                    //Fall through
                case 'd' :
                case 'D' :
                    try {
                        Double d = NumberUtils.createDouble(numeric);
                        if (!(d.isInfinite() || (d.floatValue() == 0.0D && !allZeros))) {
                            return d;
                        }
                    } catch (NumberFormatException nfe) {
                        // empty catch
                    }
                    try {
                        return createBigDecimal(numeric);
                    } catch (NumberFormatException e) {
                        // empty catch
                    }
                    //Fall through
                default :
                    throw new NumberFormatException(val + " is not a valid number.");

            }
        } else {
            //User doesn't have a preference on the return type, so let's start
            //small and go from there...
            if (expPos > -1 && expPos < val.length() - 1) {
                exp = val.substring(expPos + 1, val.length());
            } else {
                exp = null;
            }
            if (dec == null && exp == null) {
                //Must be an int,long,bigint
                try {
                    return createInteger(val);
                } catch (NumberFormatException nfe) {
                    // empty catch
                }
                try {
                    return createLong(val);
                } catch (NumberFormatException nfe) {
                    // empty catch
                }
                return createBigInteger(val);

            } else {
                //Must be a float,double,BigDec
                boolean allZeros = isAllZeros(mant) && isAllZeros(exp);
                try {
                    Float f = createFloat(val);
                    if (!(f.isInfinite() || (f.floatValue() == 0.0F && !allZeros))) {
                        return f;
                    }
                } catch (NumberFormatException nfe) {
                    // empty catch
                }
                try {
                    Double d = createDouble(val);
                    if (!(d.isInfinite() || (d.doubleValue() == 0.0D && !allZeros))) {
                        return d;
                    }
                } catch (NumberFormatException nfe) {
                    // empty catch
                }

                return createBigDecimal(val);

            }

        }
    }

    /**
     * <p>Utility method for {@link #createNumber(java.lang.String)}.</p>
     *
     * <p>Returns <code>true</code> if s is <code>null</code>.</p>
     * 
     * @param s the String to check
     * @return if it is all zeros or <code>null</code>
     */
    private static boolean isAllZeros(String s) {
        if (s == null) {
            return true;
        }
        for (int i = s.length() - 1; i >= 0; i--) {
            if (s.charAt(i) != '0') {
                return false;
            }
        }
        return s.length() > 0;
    }

    //--------------------------------------------------------------------
    
    /**
     * <p>Convert a <code>String</code> to a <code>Float</code>.</p>
     * 
     * @param val  a <code>String</code> to convert
     * @return converted <code>Float</code>
     * @throws NumberFormatException if the value cannot be converted
     */
    public static Float createFloat(String val) {
        return Float.valueOf(val);
    }

    /**
     * <p>Convert a <code>String</code> to a <code>Double</code>.</p>
     * 
     * @param val  a <code>String</code> to convert
     * @return converted <code>Double</code>
     * @throws NumberFormatException if the value cannot be converted
     */
    public static Double createDouble(String val) {
        return Double.valueOf(val);
    }

    /**
     * <p>Convert a <code>String</code> to a <code>Integer</code>, handling
     * hex and octal notations.</p>
     * 
     * @param val  a <code>String</code> to convert
     * @return converted <code>Integer</code>
     * @throws NumberFormatException if the value cannot be converted
     */
    public static Integer createInteger(String val) {
        // decode() handles 0xAABD and 0777 (hex and octal) as well.
        return Integer.decode(val);
    }

    /**
     * <p>Convert a <code>String</code> to a <code>Long</code>.</p>
     * 
     * @param val  a <code>String</code> to convert
     * @return converted <code>Long</code>
     * @throws NumberFormatException if the value cannot be converted
     */
    public static Long createLong(String val) {
        return Long.valueOf(val);
    }

    /**
     * <p>Convert a <code>String</code> to a <code>BigInteger</code>.</p>
     * 
     * @param val  a <code>String</code> to convert
     * @return converted <code>BigInteger</code>
     * @throws NumberFormatException if the value cannot be converted
     */
    public static BigInteger createBigInteger(String val) {
        BigInteger bi = new BigInteger(val);
        return bi;
    }

    /**
     * <p>Convert a <code>String</code> to a <code>BigDecimal</code>.</p>
     * 
     * @param val  a <code>String</code> to convert
     * @return converted <code>BigDecimal</code>
     * @throws NumberFormatException if the value cannot be converted
     */
    public static BigDecimal createBigDecimal(String val) {
        BigDecimal bd = new BigDecimal(val);
        return bd;
    }

    //--------------------------------------------------------------------
    
    /**
     * <p>Gets the minimum of three <code>long</code> values.</p>
     * 
     * @param a  value 1
     * @param b  value 2
     * @param c  value 3
     * @return  the smallest of the values
     */
    public static long minimum(long a, long b, long c) {
        if (b < a) {
            a = b;
        }
        if (c < a) {
            a = c;
        }
        return a;
    }

    /**
     * <p>Gets the minimum of three <code>int</code> values.</p>
     * 
     * @param a  value 1
     * @param b  value 2
     * @param c  value 3
     * @return  the smallest of the values
     */
    public static int minimum(int a, int b, int c) {
        if (b < a) {
            a = b;
        }
        if (c < a) {
            a = c;
        }
        return a;
    }

    /**
     * <p>Gets the maximum of three <code>long</code> values.</p>
     * 
     * @param a  value 1
     * @param b  value 2
     * @param c  value 3
     * @return  the largest of the values
     */
    public static long maximum(long a, long b, long c) {
        if (b > a) {
            a = b;
        }
        if (c > a) {
            a = c;
        }
        return a;
    }

    /**
     * <p>Gets the maximum of three <code>int</code> values.</p>
     * 
     * @param a  value 1
     * @param b  value 2
     * @param c  value 3
     * @return  the largest of the values
     */
    public static int maximum(int a, int b, int c) {
        if (b > a) {
            a = b;
        }
        if (c > a) {
            a = c;
        }
        return a;
    }

    //--------------------------------------------------------------------
    
    /**
     * <p>Compares two <code>doubles</code> for order.</p>
     *
     * <p>This method is more comprehensive than the standard Java greater
     * than, less than and equals operators.</p>
     * <ul>
     *  <li>It returns <code>-1</code> if the first value is less than the second.
     *  <li>It returns <code>+1</code> if the first value is greater than the second.
     *  <li>It returns <code>0</code> if the values are equal.
     * </ul>
     *
     * <p>
     * The ordering is as follows, largest to smallest:
     * <ul>
     *  <li>NaN
     *  <li>Positive infinity
     *  <li>Maximum double
     *  <li>Normal positive numbers
     *  <li>+0.0
     *  <li>-0.0
     *  <li>Normal negative numbers
     *  <li>Minimum double (-Double.MAX_VALUE)
     *  <li>Negative infinity
     * </ul>
     * </p>
     *
     * <p>Comparing <code>NaN</code> with <code>NaN</code> will
     * return <code>0</code>.</p>
     * 
     * @param lhs  the first <code>double</code>
     * @param rhs  the second <code>double</code>
     * @return <code>-1</code> if lhs is less, <code>+1</code> if greater,
     *  <code>0</code> if equal to rhs
     */
    public static int compare(double lhs, double rhs) {
        if (lhs < rhs) {
            return -1;
        }
        if (lhs > rhs) {
            return +1;
        }
        // Need to compare bits to handle 0.0 == -0.0 being true
        // compare should put -0.0 < +0.0
        // Two NaNs are also == for compare purposes
        // where NaN == NaN is false
        long lhsBits = Double.doubleToLongBits(lhs);
        long rhsBits = Double.doubleToLongBits(rhs);
        if (lhsBits == rhsBits) {
            return 0;
        }
        // Something exotic! A comparison to NaN or 0.0 vs -0.0
        // Fortunately NaN's long is > than everything else
        // Also negzeros bits < poszero
        // NAN: 9221120237041090560
        // MAX: 9218868437227405311
        // NEGZERO: -9223372036854775808
        if (lhsBits < rhsBits) {
            return -1;
        } else {
            return +1;
        }
    }
    
    /**
     * <p>Compares two floats for order.</p>
     *
     * <p>This method is more comprehensive than the standard Java greater than,
     * less than and equals operators.</p>
     * <ul>
     *  <li>It returns <code>-1</code> if the first value is less than the second.
     *  <li>It returns <code>+1</code> if the first value is greater than the second.
     *  <li>It returns <code>0</code> if the values are equal.
     * </ul>
     *
     * <p> The ordering is as follows, largest to smallest:
     * <ul>
     * <li>NaN
     * <li>Positive infinity
     * <li>Maximum float
     * <li>Normal positive numbers
     * <li>+0.0
     * <li>-0.0
     * <li>Normal negative numbers
     * <li>Minimum float (-Float.MAX_VALUE)
     * <li>Negative infinity
     * </ul>
     *
     * <p>Comparing <code>NaN</code> with <code>NaN</code> will return
     * <code>0</code>.</p>
     * 
     * @param lhs  the first <code>float</code>
     * @param rhs  the second <code>float</code>
     * @return <code>-1</code> if lhs is less, <code>+1</code> if greater,
     *  <code>0</code> if equal to rhs
     */
    public static int compare(float lhs, float rhs) {
        if (lhs < rhs) {
            return -1;
        }
        if (lhs > rhs) {
            return +1;
        }
        //Need to compare bits to handle 0.0 == -0.0 being true
        // compare should put -0.0 < +0.0
        // Two NaNs are also == for compare purposes
        // where NaN == NaN is false
        int lhsBits = Float.floatToIntBits(lhs);
        int rhsBits = Float.floatToIntBits(rhs);
        if (lhsBits == rhsBits) {
            return 0;
        }
        //Something exotic! A comparison to NaN or 0.0 vs -0.0
        //Fortunately NaN's int is > than everything else
        //Also negzeros bits < poszero
        //NAN: 2143289344
        //MAX: 2139095039
        //NEGZERO: -2147483648
        if (lhsBits < rhsBits) {
            return -1;
        } else {
            return +1;
        }
    }
    
    //--------------------------------------------------------------------
    
    /**
     * <p>Checks whether the <code>String</code> contains only
     * digit characters.</p>
     *
     * <p><code>Null</code> and empty String will return
     * <code>false</code>.</p>
     *
     * @param str  the <code>String</code> to check
     * @return <code>true</code> if str contains only unicode numeric
     */
    public static boolean isDigits(String str) {
        if ((str == null) || (str.length() == 0)) {
            return false;
        }
        for (int i = 0; i < str.length(); i++) {
            if (!Character.isDigit(str.charAt(i))) {
                return false;
            }
        }
        return true;
    }

    /**
     * <p>Checks whether the String a valid Java number.</p>
     *
     * <p>Valid numbers include hexadecimal marked with the <code>0x</code>
     * qualifier, scientific notation and numbers marked with a type
     * qualifier (e.g. 123L).</p>
     *
     * <p><code>Null</code> and empty String will return
     * <code>false</code>.</p>
     *
     * @param str  the <code>String</code> to check
     * @return <code>true</code> if the string is a correctly formatted number
     */
    public static boolean isNumber(String str) {
        if (StringUtils.isEmpty(str)) {
            return false;
        }
        char[] chars = str.toCharArray();
        int sz = chars.length;
        boolean hasExp = false;
        boolean hasDecPoint = false;
        boolean allowSigns = false;
        boolean foundDigit = false;
        // deal with any possible sign up front
        int start = (chars[0] == '-') ? 1 : 0;
        if (sz > start + 1) {
            if (chars[start] == '0' && chars[start + 1] == 'x') {
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
        }
        sz--; // don't want to loop to the last char, check it afterwords
              // for type qualifiers
        int i = start;
        // loop to the next to last char or to the last char if we need another digit to
        // make a valid number (e.g. chars[0..5] = "1234E")
        while (i < sz || (i < sz + 1 && allowSigns && !foundDigit)) {
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
            if (!allowSigns
                && (chars[i] == 'd'
                    || chars[i] == 'D'
                    || chars[i] == 'f'
                    || chars[i] == 'F')) {
                return foundDigit;
            }
            if (chars[i] == 'l'
                || chars[i] == 'L') {
                // not allowing L with an exponent
                return foundDigit && !hasExp;
            }
            // last character is illegal
            return false;
        }
        // allowSigns is true iff the val ends in 'E'
        // found digit it to make sure weird stuff like '.' and '1E-' doesn't pass
        return !allowSigns && foundDigit;
    }
}
